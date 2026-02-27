library(testthat)

.gpu_index_audit_tool_path <- function() {
  mojor_path <- find_mojor()
  root <- dirname(dirname(mojor_path))
  candidates <- c(
    file.path(root, "tools", "audit_gpuarray_index_routes.R"),
    file.path(dirname(dirname(root)), "prototype", "tools", "audit_gpuarray_index_routes.R")
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0L) {
    stop("could not locate audit_gpuarray_index_routes.R")
  }
  normalizePath(hit[[1L]], winslash = "/", mustWork = TRUE)
}

.mojor_skip_gpu_runtime_file_unless_opt_in()

test_that("GPUArray index route gate passes/fails by configured thresholds", {  audit_env <- new.env(parent = globalenv())
  sys.source(.gpu_index_audit_tool_path(), envir = audit_env)

  baseline <- data.frame(
    route = c("cpu_gather", "cpu_scatter", "cpu_slice", "cpu_slice_assign"),
    count = c(20L, 20L, 20L, 20L),
    stringsAsFactors = FALSE
  )
  current_ok <- data.frame(
    route = c("cpu_gather", "cpu_scatter", "cpu_slice", "cpu_slice_assign"),
    count = c(15L, 17L, 20L, 10L),
    stringsAsFactors = FALSE
  )
  current_bad <- data.frame(
    route = c("cpu_gather", "cpu_scatter", "cpu_slice", "cpu_slice_assign"),
    count = c(16L, 18L, 21L, 11L),
    stringsAsFactors = FALSE
  )

  gate_ok <- audit_env$mojor_gpuarray_index_route_gate(current_ok, baseline)
  expect_true(isTRUE(gate_ok$ok))
  expect_true(all(gate_ok$checks$pass))

  gate_bad <- audit_env$mojor_gpuarray_index_route_gate(current_bad, baseline)
  expect_false(isTRUE(gate_bad$ok))
  expect_true(any(!gate_bad$checks$pass))
})

test_that("GPUArray index route audit emits required route rows", {  skip_if_no_mojo()
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not available")
  }
  if (!mojor_has_gpu()) {
    skip("GPU not available")
  }

  audit_env <- new.env(parent = globalenv())
  sys.source(.gpu_index_audit_tool_path(), envir = audit_env)

  routes <- audit_env$mojor_gpuarray_audit_index_routes(repeats = 2L)
  expect_true(is.data.frame(routes))
  expect_true(all(c("route", "count") %in% names(routes)))
  expect_true(all(c(
    "gpu_gather", "cpu_gather",
    "gpu_scatter", "cpu_scatter",
    "gpu_slice", "cpu_slice",
    "cpu_slice_assign"
  ) %in% routes$route))
  expect_true(all(is.finite(routes$count)))
  expect_true(all(routes$count >= 0))
})
