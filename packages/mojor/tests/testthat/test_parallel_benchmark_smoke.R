library(testthat)

.parallel_bench_script_path <- function() {
  root <- dirname(dirname(find_mojor()))
  normalizePath(file.path(root, "tools", "benchmark_parallel_smoke.R"), mustWork = TRUE)
}

.parallel_bench_load_env <- function() {
  env <- new.env(parent = baseenv())
  sys.source(.parallel_bench_script_path(), envir = env)
  env
}

test_that("parallel benchmark smoke enforces Linux in-process mode when required", {  env <- .parallel_bench_load_env()
  env$.mojor_parallel_is_linux <- function() TRUE
  env$.mojor_parallel_ensure_backend <- function() list(ok = FALSE, reason = "stub backend fail")

  expect_error(
    env$mojor_parallel_benchmark_smoke(
      n = 1024L,
      runs = 1L,
      warmup = 0L,
      enforce = TRUE,
      allow_skip = TRUE,
      require_inprocess_linux = TRUE
    ),
    "Linux runner requires in-process compiled mode"
  )
})

test_that("parallel benchmark smoke allows skip when Linux requirement is disabled", {  env <- .parallel_bench_load_env()
  env$.mojor_parallel_is_linux <- function() TRUE
  env$.mojor_parallel_ensure_backend <- function() list(ok = FALSE, reason = "stub backend fail")

  res <- env$mojor_parallel_benchmark_smoke(
    n = 1024L,
    runs = 1L,
    warmup = 0L,
    enforce = TRUE,
    allow_skip = TRUE,
    require_inprocess_linux = FALSE
  )
  expect_identical(res$status, "skipped")
  expect_identical(res$reason, "stub backend fail")
})

test_that("parallel benchmark smoke does not hard-fail on non-Linux", {  env <- .parallel_bench_load_env()
  env$.mojor_parallel_is_linux <- function() FALSE
  env$.mojor_parallel_ensure_backend <- function() list(ok = FALSE, reason = "stub backend fail")

  res <- env$mojor_parallel_benchmark_smoke(
    n = 1024L,
    runs = 1L,
    warmup = 0L,
    enforce = TRUE,
    allow_skip = TRUE,
    require_inprocess_linux = TRUE
  )
  expect_identical(res$status, "skipped")
  expect_identical(res$reason, "stub backend fail")
})
