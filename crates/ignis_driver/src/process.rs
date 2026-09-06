//! Bounded execution of a child process.
//!
//! `std::process::Command::output` waits forever, so one hung test would hang
//! the whole run. This module runs a command with a deadline and reports the
//! kill as an ordinary outcome instead of a hang.

use std::io::Read;
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};

/// Interval between liveness checks while waiting for the child.
const POLL_INTERVAL: Duration = Duration::from_millis(5);

/// Exit code reported for a process the runner had to kill.
const TIMEOUT_EXIT_CODE: i32 = 124;

/// Upper bound on how long a pipe-drain thread gets once the child (and its
/// process group) is known to be gone.
///
/// A grandchild that inherited the pipes (for example a fixture that shells
/// out to `sleep`) can keep a pipe's write end open after the direct child
/// has already exited or been killed, so the drain thread's blocking
/// `read_to_end` would otherwise never see EOF. Killing the whole process
/// group closes every inherited copy of the write end, so this bound only
/// has to absorb the time between sending the signal and the kernel
/// delivering it.
const DRAIN_JOIN_BOUND: Duration = Duration::from_secs(5);

/// What a bounded child process produced.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ProcessOutcome {
  pub success: bool,
  pub exit_code: i32,
  pub stdout: String,
  pub stderr: String,
  pub timed_out: bool,
}

/// Runs `command` to completion, killing it once `timeout` elapses.
///
/// The child is spawned in its own process group (unix only) so the runner
/// can kill every descendant it forked, not just the direct child: a fixture
/// that shells out to a long-running grandchild would otherwise leave that
/// grandchild alive (and holding the stdout/stderr pipes open) after the
/// direct child exits or is killed.
///
/// Both streams are drained on dedicated threads: a child that fills a pipe
/// buffer would otherwise block on its own output and never reach the
/// deadline check. On the timeout path the whole group is killed before the
/// direct child is; on the normal-exit path the group is left alone, since
/// `try_wait`/`wait` have already reaped the child by then and its pid (and
/// therefore its group id) can be recycled by the OS for an unrelated
/// process on a busy machine, so signaling it after the fact risks hitting
/// that unrelated process instead. Either way, the drain join is bounded, so
/// an inherited pipe a stray descendant still holds open cannot hang the
/// runner forever even when the group was never signaled.
pub(crate) fn run_with_timeout(
  command: &mut Command,
  timeout: Duration,
) -> Result<ProcessOutcome, String> {
  place_in_its_own_process_group(command);

  let mut child = command
    .stdout(Stdio::piped())
    .stderr(Stdio::piped())
    .spawn()
    .map_err(|error| error.to_string())?;

  let stdout_pipe = child.stdout.take();
  let stderr_pipe = child.stderr.take();

  let stdout_reader = spawn_drain(stdout_pipe);
  let stderr_reader = spawn_drain(stderr_pipe);

  let started = Instant::now();
  let mut timed_out = false;

  let status = loop {
    match child.try_wait().map_err(|error| error.to_string())? {
      Some(status) => break status,
      None => {
        if started.elapsed() >= timeout {
          timed_out = true;
          // The child (and its group) is still unreaped here, so its pid is
          // still its own: signal the group first to reach any descendant
          // that inherited the pipes, then kill the direct child itself.
          kill_process_group(&child);
          let _ = child.kill();
          break child.wait().map_err(|error| error.to_string())?;
        }

        std::thread::sleep(POLL_INTERVAL);
      },
    }
  };

  // Both streams share one deadline instead of a fresh `DRAIN_JOIN_BOUND`
  // each, so a stray descendant holding both pipes open costs the runner
  // `DRAIN_JOIN_BOUND` once, not twice.
  let drain_deadline = Instant::now() + DRAIN_JOIN_BOUND;
  let stdout = stdout_reader
    .recv_timeout(drain_deadline.saturating_duration_since(Instant::now()))
    .unwrap_or_default();
  let stderr = stderr_reader
    .recv_timeout(drain_deadline.saturating_duration_since(Instant::now()))
    .unwrap_or_default();

  let exit_code = if timed_out {
    TIMEOUT_EXIT_CODE
  } else {
    status.code().unwrap_or(1)
  };

  Ok(ProcessOutcome {
    success: !timed_out && status.success(),
    exit_code,
    stdout,
    stderr,
    timed_out,
  })
}

/// Puts the child in a new process group headed by itself (unix only).
///
/// Without this, the child shares the runner's own process group, and
/// signaling "the child's group" would signal the runner too.
#[cfg(unix)]
fn place_in_its_own_process_group(command: &mut Command) {
  use std::os::unix::process::CommandExt;

  command.process_group(0);
}

#[cfg(not(unix))]
fn place_in_its_own_process_group(_command: &mut Command) {}

/// Sends `SIGKILL` to every process in the child's process group (unix only).
///
/// Callers must only reach this while `child` is known unreaped: once
/// `try_wait`/`wait` returns a status, the pid is freed back to the OS and
/// can be recycled for an unrelated process before this runs, and `child.id()`
/// would still name that pid, so `kill(-pid, ...)` could signal a process
/// (and its group) that has nothing to do with this run.
///
/// Best-effort otherwise: the group (or its remaining members) may already
/// be gone, which `kill` reports as an ordinary error this function ignores.
#[cfg(unix)]
fn kill_process_group(child: &Child) {
  let pid = child.id() as i32;

  // A negative pid tells `kill` to signal the whole process group instead of
  // one process; the group id equals the direct child's own pid because it
  // was placed in a new group headed by itself before it was spawned.
  unsafe {
    sys::kill(-pid, sys::SIGKILL);
  }
}

#[cfg(not(unix))]
fn kill_process_group(_child: &Child) {}

#[cfg(unix)]
mod sys {
  pub const SIGKILL: i32 = 9;

  unsafe extern "C" {
    pub fn kill(
      pid: i32,
      sig: i32,
    ) -> i32;
  }
}

/// Drains `pipe` to completion on a dedicated thread and returns a receiver
/// for the result, so a caller can bound how long it waits for the thread
/// instead of joining it unconditionally.
fn spawn_drain<R>(pipe: Option<R>) -> std::sync::mpsc::Receiver<String>
where
  R: Read + Send + 'static,
{
  let (sender, receiver) = std::sync::mpsc::channel();

  std::thread::spawn(move || {
    let _ = sender.send(drain(pipe));
  });

  receiver
}

fn drain<R: Read>(pipe: Option<R>) -> String {
  let Some(mut pipe) = pipe else {
    return String::new();
  };

  let mut buffer = Vec::new();
  let _ = pipe.read_to_end(&mut buffer);

  String::from_utf8_lossy(&buffer).to_string()
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn a_fast_command_reports_its_own_streams_and_status() {
    let mut command = Command::new("sh");
    command.arg("-c").arg("printf out; printf err 1>&2; exit 3");

    let outcome = run_with_timeout(&mut command, Duration::from_secs(30)).expect("spawn");

    assert_eq!(outcome.exit_code, 3);
    assert_eq!(outcome.stdout, "out");
    assert_eq!(outcome.stderr, "err");
    assert!(!outcome.success);
    assert!(!outcome.timed_out);
  }

  #[test]
  fn a_hanging_command_is_killed_at_the_deadline() {
    let mut command = Command::new("sh");
    command.arg("-c").arg("sleep 30");

    let outcome = run_with_timeout(&mut command, Duration::from_millis(100)).expect("spawn");

    assert!(outcome.timed_out);
    assert!(!outcome.success);
    assert_eq!(outcome.exit_code, TIMEOUT_EXIT_CODE);
  }

  #[cfg(unix)]
  #[test]
  fn a_grandchild_holding_the_pipes_open_does_not_hang_the_runner() {
    let mut command = Command::new("sh");
    // The direct child backgrounds a long sleep that inherits its stdio and
    // exits almost immediately itself, so the process exits normally well
    // before `timeout`, but a naive unconditional join on the drain threads
    // would still block for the sleep's whole duration: the grandchild keeps
    // the pipes' write ends open long after the direct child is gone.
    command.arg("-c").arg("sleep 600 & exit 0");

    let started = Instant::now();
    let outcome = run_with_timeout(&mut command, Duration::from_secs(10)).expect("spawn");

    assert!(
      started.elapsed() < Duration::from_secs(10),
      "expected the runner to reap the grandchild instead of waiting for it, took {:?}",
      started.elapsed()
    );
    assert!(!outcome.timed_out);
    assert!(outcome.success);
  }
}
