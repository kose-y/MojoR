library(testthat)

.gpu_perf_tool_path <- function() {
  mojor_path <- find_mojor()
  root <- dirname(dirname(mojor_path))
  candidates <- c(
    file.path(root, "tools", "benchmark_gpuarray_core.R"),
    file.path(dirname(dirname(root)), "prototype", "tools", "benchmark_gpuarray_core.R")
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0L) {
    stop("could not locate benchmark_gpuarray_core.R")
  }
  normalizePath(hit[[1L]], winslash = "/", mustWork = TRUE)
}

.mojor_skip_gpu_runtime_file_unless_opt_in()

test_that("GPUArray perf benchmark harness emits finite core metrics", {  skip_if_no_mojo()
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not available")
  }
  if (!mojor_has_gpu()) {
    skip("GPU not available")
  }

  perf_env <- new.env(parent = globalenv())
  sys.source(.gpu_perf_tool_path(), envir = perf_env)

  bench <- perf_env$mojor_gpuarray_benchmark_core(runs = 2L, warmup = 1L, n = 512L, m = 16L, k = 16L, p = 16L)
  metrics <- bench$metrics

  expect_true(is.data.frame(metrics))
  expect_true(all(c("metric", "median_sec", "mean_sec", "route") %in% names(metrics)))
  expect_true(all(c("gpu_cast", "gpu_broadcast", "gpu_reduce", "gpu_matmul") %in% metrics$metric))
  expect_true(all(is.finite(metrics$median_sec)))
  expect_true(all(is.finite(metrics$mean_sec)))
  expect_true(all(metrics$median_sec >= 0))
  expect_true(any(metrics$median_sec > 0))
  expect_true(all(is.character(metrics$route)))
  expect_true(all(nzchar(metrics$route)))
})

test_that("GPUArray perf benchmark harness can include indexing smoke metrics", {  skip_if_no_mojo()
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not available")
  }
  if (!mojor_has_gpu()) {
    skip("GPU not available")
  }

  perf_env <- new.env(parent = globalenv())
  sys.source(.gpu_perf_tool_path(), envir = perf_env)

  bench <- perf_env$mojor_gpuarray_benchmark_core(
    runs = 1L,
    warmup = 0L,
    n = 256L,
    m = 16L,
    k = 16L,
    p = 16L,
    include_indexing = TRUE
  )
  metrics <- bench$metrics

  expect_true(all(c("gpu_index_gather", "gpu_index_assign") %in% metrics$metric))
  expect_true(all(is.finite(metrics$median_sec)))
  expect_true(all(metrics$median_sec >= 0))
})

test_that("GPUArray perf benchmark gate passes when current metrics are within thresholds", {  perf_env <- new.env(parent = globalenv())
  sys.source(.gpu_perf_tool_path(), envir = perf_env)

  current <- data.frame(
    metric = c("gpu_cast", "gpu_broadcast", "gpu_reduce", "gpu_matmul"),
    median_sec = c(1.05, 1.10, 1.15, 1.20),
    stringsAsFactors = FALSE
  )
  baseline <- data.frame(
    metric = c("gpu_cast", "gpu_broadcast", "gpu_reduce", "gpu_matmul"),
    median_sec = c(1.00, 1.00, 1.00, 1.00),
    stringsAsFactors = FALSE
  )

  gate <- perf_env$mojor_gpuarray_perf_gate(current, baseline)
  expect_true(isTRUE(gate$ok))
  expect_true(nrow(gate$checks) >= 1L)
  expect_true(all(gate$checks$pass))
})

test_that("GPUArray perf benchmark gate reports threshold regressions", {  perf_env <- new.env(parent = globalenv())
  sys.source(.gpu_perf_tool_path(), envir = perf_env)

  current <- data.frame(
    metric = c("gpu_cast", "gpu_broadcast", "gpu_reduce", "gpu_matmul"),
    median_sec = c(2.0, 2.0, 2.0, 2.0),
    stringsAsFactors = FALSE
  )
  baseline <- data.frame(
    metric = c("gpu_cast", "gpu_broadcast", "gpu_reduce", "gpu_matmul"),
    median_sec = c(1.0, 1.0, 1.0, 1.0),
    stringsAsFactors = FALSE
  )

  gate <- perf_env$mojor_gpuarray_perf_gate(current, baseline)
  expect_false(isTRUE(gate$ok))
  expect_true(nrow(gate$checks) >= 1L)
  expect_true(any(!gate$checks$pass))
})

test_that("GPUArray perf benchmark gate handles zero baselines with floor comparison", {  perf_env <- new.env(parent = globalenv())
  sys.source(.gpu_perf_tool_path(), envir = perf_env)

  current <- data.frame(
    metric = c("gpu_cast", "gpu_broadcast", "gpu_reduce", "gpu_matmul"),
    median_sec = c(0.00011, 0.00011, 0.00012, 0.00013),
    stringsAsFactors = FALSE
  )
  baseline <- data.frame(
    metric = c("gpu_cast", "gpu_broadcast", "gpu_reduce", "gpu_matmul"),
    median_sec = c(0.0, 0.0, 0.0, 0.0),
    stringsAsFactors = FALSE
  )

  gate <- perf_env$mojor_gpuarray_perf_gate(current, baseline, baseline_floor_sec = 1e-4)
  expect_true(isTRUE(gate$ok))
  expect_true(all(gate$checks$comparison_mode == "floor"))
  expect_true(all(gate$checks$compare_baseline_sec == 1e-4))
})

test_that("GPUArray perf benchmark gate fails when floor budget is exceeded", {  perf_env <- new.env(parent = globalenv())
  sys.source(.gpu_perf_tool_path(), envir = perf_env)

  current <- data.frame(
    metric = c("gpu_cast", "gpu_broadcast", "gpu_reduce", "gpu_matmul"),
    median_sec = c(0.00020, 0.00020, 0.00020, 0.00020),
    stringsAsFactors = FALSE
  )
  baseline <- data.frame(
    metric = c("gpu_cast", "gpu_broadcast", "gpu_reduce", "gpu_matmul"),
    median_sec = c(0.0, 0.0, 0.0, 0.0),
    stringsAsFactors = FALSE
  )

  gate <- perf_env$mojor_gpuarray_perf_gate(current, baseline, baseline_floor_sec = 1e-4)
  expect_false(isTRUE(gate$ok))
  expect_true(any(!gate$checks$pass))
})
