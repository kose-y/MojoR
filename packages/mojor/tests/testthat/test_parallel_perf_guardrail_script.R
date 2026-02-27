library(testthat)

.parallel_guardrail_script_path <- function() {
  cur <- dirname(dirname(find_mojor()))
  for (i in 0:8) {
    cand <- file.path(cur, "scripts", "check_parallel_perf_guardrail.sh")
    if (file.exists(cand)) return(normalizePath(cand, mustWork = TRUE))
    cur <- dirname(cur)
  }
  stop("could not locate scripts/check_parallel_perf_guardrail.sh from test root")
}

.parallel_guardrail_status <- function(output) {
  status <- attr(output, "status")
  if (is.null(status)) return(0L)
  as.integer(status)
}

test_that("parallel perf guardrail script passes on valid speedup", {  script <- .parallel_guardrail_script_path()
  baseline <- tempfile("baseline_", fileext = ".csv")
  current <- tempfile("current_", fileext = ".csv")

  writeLines(c("name,ms", "mojor_parallel_speedup,1.05"), baseline)
  writeLines(c("name,ms", "mojor_parallel_speedup,1.20"), current)

  out <- system2("bash", c(script, "--baseline", baseline, "--current", current), stdout = TRUE, stderr = TRUE)
  expect_identical(.parallel_guardrail_status(out), 0L)
  expect_true(any(grepl("Parallel performance guardrail: clean", out, fixed = TRUE)))
})

test_that("parallel perf guardrail script fails on regression", {  script <- .parallel_guardrail_script_path()
  baseline <- tempfile("baseline_", fileext = ".csv")
  current <- tempfile("current_", fileext = ".csv")

  writeLines(c("name,ms", "mojor_parallel_speedup,1.05"), baseline)
  writeLines(c("name,ms", "mojor_parallel_speedup,0.95"), current)

  out <- suppressWarnings(system2(
    "bash",
    c(
      script,
      "--baseline", baseline,
      "--current", current,
      "--min-speedup-ratio", "0.90",
      "--absolute-min-speedup", "1.01"
    ),
    stdout = TRUE,
    stderr = TRUE
  ))
  expect_identical(.parallel_guardrail_status(out), 1L)
  expect_true(any(grepl("REGRESSION", out, fixed = TRUE)))
})

test_that("parallel perf guardrail script returns input error when metric missing", {  script <- .parallel_guardrail_script_path()
  baseline <- tempfile("baseline_", fileext = ".csv")
  current <- tempfile("current_", fileext = ".csv")

  writeLines(c("name,ms", "mojor_parallel_speedup,1.05"), baseline)
  writeLines(c("name,ms", "mojor_seq_ms,100"), current)

  out <- suppressWarnings(system2(
    "bash",
    c(script, "--baseline", baseline, "--current", current),
    stdout = TRUE,
    stderr = TRUE
  ))
  expect_identical(.parallel_guardrail_status(out), 2L)
  expect_true(any(grepl("input error", out, fixed = TRUE)))
})
