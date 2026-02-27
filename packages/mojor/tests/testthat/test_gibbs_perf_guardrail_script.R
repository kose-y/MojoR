library(testthat)

.gibbs_script_path <- function(name) {
  cur <- dirname(dirname(find_mojor()))
  for (i in 0:8) {
    cand <- file.path(cur, "scripts", name)
    if (file.exists(cand)) return(normalizePath(cand, mustWork = TRUE))
    cur <- dirname(cur)
  }
  stop(sprintf("could not locate scripts/%s from test root", name))
}

.gibbs_script_status <- function(output) {
  status <- attr(output, "status")
  if (is.null(status)) return(0L)
  as.integer(status)
}

test_that("parse_gibbs_benchmark_log extracts required and bulk metrics", {  parser <- .gibbs_script_path("parse_gibbs_benchmark_log.R")
  log_file <- tempfile("gibbs_log_", fileext = ".log")
  csv_file <- tempfile("gibbs_metrics_", fileext = ".csv")

  writeLines(c(
    "Summary (sec)",
    "mean=0.120 median=0.119 p95=0.130",
    "MojoR build time: 1.200 sec",
    "MojoR Summary (sec)",
    "mean=0.040 median=0.039 p95=0.050",
    "MojoR fast build time: 0.400 sec",
    "MojoR fast Summary (sec)",
    "mean=0.020 median=0.019 p95=0.030",
    "MojoR bulk build time: 0.500 sec",
    "MojoR bulk Summary (sec)",
    "mean=0.010 median=0.010 p95=0.011"
  ), log_file)

  out <- system2(
    "Rscript",
    c(parser, paste0("--input=", log_file), paste0("--output=", csv_file)),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_identical(.gibbs_script_status(out), 0L)
  expect_true(file.exists(csv_file))

  parsed <- read.csv(csv_file, stringsAsFactors = FALSE)
  expect_true(all(c(
    "gibbs_r_mean_ms",
    "gibbs_mojor_build_ms",
    "gibbs_mojor_mean_ms",
    "gibbs_mojor_fast_build_ms",
    "gibbs_mojor_fast_mean_ms",
    "gibbs_bulk_build_ms",
    "gibbs_bulk_mean_ms"
  ) %in% parsed$name))
})

test_that("gibbs perf guardrail passes within thresholds", {  script <- .gibbs_script_path("check_gibbs_perf_guardrail.sh")
  baseline <- tempfile("gibbs_base_", fileext = ".csv")
  current <- tempfile("gibbs_cur_", fileext = ".csv")

  writeLines(c(
    "name,ms",
    "gibbs_r_mean_ms,100",
    "gibbs_mojor_build_ms,20",
    "gibbs_mojor_mean_ms,40",
    "gibbs_mojor_fast_build_ms,15",
    "gibbs_mojor_fast_mean_ms,30"
  ), baseline)
  writeLines(c(
    "name,ms",
    "gibbs_r_mean_ms,110",
    "gibbs_mojor_build_ms,24",
    "gibbs_mojor_mean_ms,44",
    "gibbs_mojor_fast_build_ms,18",
    "gibbs_mojor_fast_mean_ms,34"
  ), current)

  out <- system2(
    "bash",
    c(script, "--baseline", baseline, "--current", current),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_identical(.gibbs_script_status(out), 0L)
  expect_true(any(grepl("Gibbs performance guardrail: clean", out, fixed = TRUE)))
})

test_that("gibbs perf guardrail fails on runtime regression", {  script <- .gibbs_script_path("check_gibbs_perf_guardrail.sh")
  baseline <- tempfile("gibbs_base_", fileext = ".csv")
  current <- tempfile("gibbs_cur_", fileext = ".csv")

  writeLines(c(
    "name,ms",
    "gibbs_r_mean_ms,120",
    "gibbs_mojor_build_ms,20",
    "gibbs_mojor_mean_ms,40",
    "gibbs_mojor_fast_build_ms,15",
    "gibbs_mojor_fast_mean_ms,30"
  ), baseline)
  writeLines(c(
    "name,ms",
    "gibbs_r_mean_ms,130",
    "gibbs_mojor_build_ms,24",
    "gibbs_mojor_mean_ms,52",
    "gibbs_mojor_fast_build_ms,18",
    "gibbs_mojor_fast_mean_ms,38"
  ), current)

  out <- suppressWarnings(system2(
    "bash",
    c(script, "--baseline", baseline, "--current", current),
    stdout = TRUE,
    stderr = TRUE
  ))
  expect_identical(.gibbs_script_status(out), 1L)
  expect_true(any(grepl("REGRESSION", out, fixed = TRUE)))
})

test_that("gibbs perf guardrail returns input error for malformed metrics", {  script <- .gibbs_script_path("check_gibbs_perf_guardrail.sh")
  baseline <- tempfile("gibbs_base_", fileext = ".csv")
  current <- tempfile("gibbs_cur_", fileext = ".csv")

  writeLines(c(
    "name,ms",
    "gibbs_r_mean_ms,120",
    "gibbs_mojor_build_ms,20",
    "gibbs_mojor_mean_ms,40",
    "gibbs_mojor_fast_build_ms,15",
    "gibbs_mojor_fast_mean_ms,30"
  ), baseline)
  writeLines(c(
    "name,ms",
    "gibbs_r_mean_ms,130",
    "gibbs_mojor_build_ms,24"
  ), current)

  out <- suppressWarnings(system2(
    "bash",
    c(script, "--baseline", baseline, "--current", current),
    stdout = TRUE,
    stderr = TRUE
  ))
  expect_identical(.gibbs_script_status(out), 2L)
  expect_true(any(grepl("input error", out, fixed = TRUE)))
})
