//! Bounded execution of a child process.
//!
//! `std::process::Command::output` waits forever, so one hung test would hang
//! the whole run. This module runs a command with a deadline and reports the
//! kill as an ordinary outcome instead of a hang.

use std::io::Read;
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

/// Interval between liveness checks while waiting for the child.
const POLL_INTERVAL: Duration = Duration::from_millis(5);

/// Exit code reported for a process the runner had to kill.
const TIMEOUT_EXIT_CODE: i32 = 124;

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
/// Both streams are drained on dedicated threads: a child that fills a pipe
/// buffer would otherwise block on its own output and never reach the deadline
/// check.
pub(crate) fn run_with_timeout(
  command: &mut Command,
  timeout: Duration,
) -> Result<ProcessOutcome, String> {
  let mut child = command
    .stdout(Stdio::piped())
    .stderr(Stdio::piped())
    .spawn()
    .map_err(|error| error.to_string())?;

  let stdout_pipe = child.stdout.take();
  let stderr_pipe = child.stderr.take();

  let stdout_reader = std::thread::spawn(move || drain(stdout_pipe));
  let stderr_reader = std::thread::spawn(move || drain(stderr_pipe));

  let started = Instant::now();
  let mut timed_out = false;

  let status = loop {
    match child.try_wait().map_err(|error| error.to_string())? {
      Some(status) => break status,
      None => {
        if started.elapsed() >= timeout {
          timed_out = true;
          let _ = child.kill();
          break child.wait().map_err(|error| error.to_string())?;
        }

        std::thread::sleep(POLL_INTERVAL);
      },
    }
  };

  let stdout = stdout_reader.join().unwrap_or_default();
  let stderr = stderr_reader.join().unwrap_or_default();

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
}
