//! Bounded worker pool for the parts of the build that spawn external
//! processes: per-translation-unit `gcc -c` invocations and per-test harness
//! runs.
//!
//! Both workloads are embarrassingly parallel but must stay reportable in a
//! fixed order, so [`map_parallel`] always returns results in input order
//! regardless of completion order.

use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Mutex, OnceLock};

/// Environment variable consulted when the CLI supplied no explicit `--jobs`.
const JOBS_ENV: &str = "IGNIS_TEST_JOBS";

static JOB_LIMIT: OnceLock<usize> = OnceLock::new();

/// Records the process-wide worker limit selected on the command line.
///
/// Only the first call has an effect, so the limit stays stable across the
/// several driver entry points a single command may run in sequence (for
/// example `ignis test`, which also builds the standard library).
pub fn set_job_limit(jobs: Option<usize>) {
  if let Some(jobs) = jobs {
    let _ = JOB_LIMIT.set(jobs.max(1));
  }
}

/// Number of concurrent workers to use for process-spawning work.
///
/// Resolution order is the `--jobs` command line value, then `IGNIS_TEST_JOBS`,
/// then the available parallelism reported by the host.
pub fn job_limit() -> usize {
  *JOB_LIMIT.get_or_init(default_job_limit)
}

fn default_job_limit() -> usize {
  if let Ok(raw) = std::env::var(JOBS_ENV)
    && let Ok(jobs) = raw.trim().parse::<usize>()
    && jobs > 0
  {
    return jobs;
  }

  std::thread::available_parallelism()
    .map(|value| value.get())
    .unwrap_or(1)
}

/// Applies `work` to every item using at most `jobs` concurrent workers.
///
/// Results are returned in input order, so completion order never influences
/// what a caller reports. `jobs <= 1` runs everything on the calling thread,
/// which keeps single-threaded builds free of thread setup and keeps panics
/// propagating exactly as they did before.
pub fn map_parallel<T, R, F>(
  items: &[T],
  jobs: usize,
  work: F,
) -> Vec<R>
where
  T: Sync,
  R: Send,
  F: Fn(&T) -> R + Sync,
{
  if items.is_empty() {
    return Vec::new();
  }

  let workers = jobs.max(1).min(items.len());

  if workers == 1 {
    return items.iter().map(&work).collect();
  }

  let next_index = AtomicUsize::new(0);
  let collected: Mutex<Vec<(usize, R)>> = Mutex::new(Vec::with_capacity(items.len()));

  std::thread::scope(|scope| {
    for _ in 0..workers {
      scope.spawn(|| {
        loop {
          let index = next_index.fetch_add(1, Ordering::Relaxed);

          let Some(item) = items.get(index) else {
            break;
          };

          let result = work(item);

          let mut guard = collected.lock().unwrap_or_else(|poisoned| poisoned.into_inner());
          guard.push((index, result));
        }
      });
    }
  });

  let mut results = collected.into_inner().unwrap_or_else(|poisoned| poisoned.into_inner());
  results.sort_by_key(|(index, _)| *index);

  results.into_iter().map(|(_, result)| result).collect()
}

#[cfg(test)]
mod tests {
  use super::*;

  use std::time::Duration;

  #[test]
  fn results_follow_input_order_when_work_finishes_out_of_order() {
    let items: Vec<u64> = (0..8).collect();

    // Earlier items sleep longer, so completion order is the reverse of input
    // order and a naive push-on-completion collector would scramble the report.
    let results = map_parallel(&items, 8, |item| {
      std::thread::sleep(Duration::from_millis((8 - item) * 20));
      *item
    });

    assert_eq!(results, items);
  }

  #[test]
  fn single_job_runs_every_item_in_order() {
    let items: Vec<u64> = (0..16).collect();

    let observed = Mutex::new(Vec::new());
    let results = map_parallel(&items, 1, |item| {
      observed.lock().expect("observation lock").push(*item);
      item * 2
    });

    assert_eq!(results, items.iter().map(|item| item * 2).collect::<Vec<_>>());
    assert_eq!(observed.into_inner().expect("observations"), items);
  }

  #[test]
  fn failures_are_reported_against_the_item_that_produced_them() {
    let items: Vec<u64> = (0..12).collect();

    let results: Vec<Result<u64, String>> = map_parallel(&items, 4, |item| {
      if item % 5 == 3 {
        return Err(format!("item {} failed", item));
      }

      Ok(*item)
    });

    for (index, result) in results.iter().enumerate() {
      let item = index as u64;

      if item % 5 == 3 {
        assert_eq!(
          result.as_ref().err().map(String::as_str),
          Some(format!("item {} failed", item).as_str())
        );
      } else {
        assert_eq!(result.as_ref().ok(), Some(&item));
      }
    }
  }

  #[test]
  fn an_empty_workload_produces_no_results() {
    let items: Vec<u64> = Vec::new();
    let results = map_parallel(&items, 4, |item| *item);

    assert!(results.is_empty());
  }

  #[test]
  fn more_jobs_than_items_still_runs_each_item_exactly_once() {
    let items: Vec<u64> = (0..3).collect();
    let runs = AtomicUsize::new(0);

    let results = map_parallel(&items, 32, |item| {
      runs.fetch_add(1, Ordering::Relaxed);
      *item
    });

    assert_eq!(results, items);
    assert_eq!(runs.load(Ordering::Relaxed), items.len());
  }
}
