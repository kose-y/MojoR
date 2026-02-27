library(testthat)

.perf_tool_path <- function() {
  pkg_root <- dirname(dirname(find_mojor()))
  file.path(pkg_root, "tools", "benchmark_ir_pipeline.R")
}

.perf_env <- function() {
  env <- new.env(parent = globalenv())
  sys.source(.perf_tool_path(), envir = env)
  env
}

.perf_script_status <- function(output) {
  status <- attr(output, "status")
  if (is.null(status)) return(0L)
  as.integer(status)
}

.perf_rscript <- function() {
  rscript <- file.path(R.home("bin"), "Rscript")
  if (file.exists(rscript)) {
    return(rscript)
  }
  "Rscript"
}

test_that("IR perf benchmark harness emits finite pipeline metrics", {
  perf_env <- .perf_env()

  bench <- perf_env$mojor_ir_benchmark_pipeline(runs = 2L, warmup = 1L, include_e2e = TRUE)
  metrics <- bench$metrics

  expect_true(is.data.frame(metrics))
  expect_true(all(c("metric", "median_sec", "mean_sec", "sd_sec", "p95_sec", "cv_pct") %in% names(metrics)))
  expect_true(all(c("parse_lossless", "verify_ssa", "lower_backend", "prepare_backend_e2e") %in% metrics$metric))
  expect_true(all(is.finite(metrics$median_sec)))
  expect_true(all(is.finite(metrics$mean_sec)))
  expect_true(all(is.finite(metrics$sd_sec)))
  expect_true(all(is.finite(metrics$p95_sec)))
  expect_true(all(metrics$median_sec >= 0))
  expect_true(any(metrics$median_sec > 0))
})

test_that("IR perf benchmark CLI smoke runs in a clean Rscript process", {
  out <- system2(
    .perf_rscript(),
    c(.perf_tool_path(), "--runs=1", "--warmup=0"),
    stdout = TRUE,
    stderr = TRUE
  )

  expect_identical(.perf_script_status(out), 0L)
  expect_true(any(grepl("parse_lossless", out, fixed = TRUE)))
  expect_true(any(grepl("verify_ssa", out, fixed = TRUE)))
  expect_true(any(grepl("lower_backend", out, fixed = TRUE)))
})

test_that("IR perf benchmark evidence mode emits workload/profile matrix and ablations", {
  perf_env <- .perf_env()

  bench <- perf_env$mojor_ir_benchmark_evidence(
    runs = 1L,
    warmup = 0L,
    include_e2e = FALSE,
    workload_set = "core",
    ablation = TRUE
  )

  metrics <- bench$metrics
  ablation <- bench$ablation

  expect_true(is.data.frame(metrics))
  expect_true(all(c(
    "workload", "profile", "metric", "median_sec", "mean_sec",
    "sd_sec", "p05_sec", "p95_sec", "min_sec", "max_sec", "cv_pct"
  ) %in% names(metrics)))
  expect_true(all(c("branch_if_vec") %in% metrics$workload))
  expect_true(all(c("opt2_full", "no_hir_opt", "no_ssa_passes", "no_fusion_analysis") %in% metrics$profile))

  expect_true(is.data.frame(ablation))
  expect_true(all(c(
    "workload", "profile", "metric", "current_median_sec",
    "baseline_median_sec", "delta_sec", "delta_ratio", "delta_pct"
  ) %in% names(ablation)))
  expect_true(all(ablation$profile != "opt2_full"))
})

test_that("IR perf benchmark evidence CLI smoke runs and prints ablations", {
  out <- system2(
    .perf_rscript(),
    c(
      .perf_tool_path(),
      "--mode=evidence",
      "--runs=1",
      "--warmup=0",
      "--workload-set=core",
      "--ablation=1"
    ),
    stdout = TRUE,
    stderr = TRUE
  )

  expect_identical(.perf_script_status(out), 0L)
  expect_true(any(grepl("branch_if_vec", out, fixed = TRUE)))
  expect_true(any(grepl("opt2_full", out, fixed = TRUE)))
  expect_true(any(grepl("Ablation deltas", out, fixed = TRUE)))
})

test_that("IR perf benchmark gate passes when current metrics are within thresholds", {
  perf_env <- .perf_env()

  current <- data.frame(
    metric = c("parse_lossless", "verify_ssa", "lower_backend", "prepare_backend_e2e"),
    median_sec = c(1.05, 1.08, 1.12, 1.18),
    stringsAsFactors = FALSE
  )
  baseline <- data.frame(
    metric = c("parse_lossless", "verify_ssa", "lower_backend", "prepare_backend_e2e"),
    median_sec = c(1.00, 1.00, 1.00, 1.00),
    stringsAsFactors = FALSE
  )

  gate <- perf_env$mojor_ir_perf_gate(current, baseline)
  expect_true(isTRUE(gate$ok))
  expect_true(nrow(gate$checks) >= 1L)
  expect_true(all(gate$checks$pass))
})

test_that("IR perf benchmark gate reports threshold regressions", {
  perf_env <- .perf_env()

  current <- data.frame(
    metric = c("parse_lossless", "verify_ssa", "lower_backend", "prepare_backend_e2e"),
    median_sec = c(2.0, 2.0, 2.0, 2.0),
    stringsAsFactors = FALSE
  )
  baseline <- data.frame(
    metric = c("parse_lossless", "verify_ssa", "lower_backend", "prepare_backend_e2e"),
    median_sec = c(1.0, 1.0, 1.0, 1.0),
    stringsAsFactors = FALSE
  )

  gate <- perf_env$mojor_ir_perf_gate(current, baseline)
  expect_false(isTRUE(gate$ok))
  expect_true(nrow(gate$checks) >= 1L)
  expect_true(any(!gate$checks$pass))
})

test_that("IR perf benchmark gate handles zero baselines with floor comparison", {
  perf_env <- .perf_env()

  current <- data.frame(
    metric = c("parse_lossless", "verify_ssa", "lower_backend", "prepare_backend_e2e"),
    median_sec = c(0.00105, 0.00100, 0.00112, 0.00110),
    stringsAsFactors = FALSE
  )
  baseline <- data.frame(
    metric = c("parse_lossless", "verify_ssa", "lower_backend", "prepare_backend_e2e"),
    median_sec = c(0.0, 0.0, 0.0, 0.0),
    stringsAsFactors = FALSE
  )

  gate <- perf_env$mojor_ir_perf_gate(current, baseline, baseline_floor_sec = 0.001)
  expect_true(isTRUE(gate$ok))
  expect_true(all(gate$checks$comparison_mode == "floor"))
  expect_true(all(gate$checks$compare_baseline_sec == 0.001))
})

test_that("IR perf benchmark gate fails when zero-baseline metric exceeds floor budget", {
  perf_env <- .perf_env()

  current <- data.frame(
    metric = c("parse_lossless", "verify_ssa", "lower_backend", "prepare_backend_e2e"),
    median_sec = c(0.00130, 0.00130, 0.00130, 0.00130),
    stringsAsFactors = FALSE
  )
  baseline <- data.frame(
    metric = c("parse_lossless", "verify_ssa", "lower_backend", "prepare_backend_e2e"),
    median_sec = c(0.0, 0.0, 0.0, 0.0),
    stringsAsFactors = FALSE
  )

  gate <- perf_env$mojor_ir_perf_gate(current, baseline, baseline_floor_sec = 0.001)
  expect_false(isTRUE(gate$ok))
  expect_true(any(!gate$checks$pass))
})
