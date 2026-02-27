library(testthat)

.gpu_cast_broadcast_route_audit_tool_path <- function() {
  mojor_path <- find_mojor()
  root <- dirname(dirname(mojor_path))
  candidates <- c(
    file.path(root, "tools", "audit_gpuarray_cast_broadcast_routes.R"),
    file.path(dirname(dirname(root)), "prototype", "tools", "audit_gpuarray_cast_broadcast_routes.R")
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0L) {
    stop("could not locate audit_gpuarray_cast_broadcast_routes.R")
  }
  normalizePath(hit[[1L]], winslash = "/", mustWork = TRUE)
}

.mojor_skip_gpu_runtime_file_unless_opt_in()

test_that("GPUArray cast/broadcast route gate passes/fails by configured thresholds", {  audit_env <- new.env(parent = globalenv())
  sys.source(.gpu_cast_broadcast_route_audit_tool_path(), envir = audit_env)

  baseline <- data.frame(
    route = c("cpu_cast", "cpu_broadcast"),
    count = c(20L, 20L),
    stringsAsFactors = FALSE
  )
  current_ok <- data.frame(
    route = c("cpu_cast", "cpu_broadcast"),
    count = c(20L, 20L),
    stringsAsFactors = FALSE
  )
  current_bad <- data.frame(
    route = c("cpu_cast", "cpu_broadcast"),
    count = c(21L, 20L),
    stringsAsFactors = FALSE
  )
  current_drop_ok <- data.frame(
    route = c("cpu_cast", "cpu_broadcast"),
    count = c(17L, 17L),
    stringsAsFactors = FALSE
  )
  current_drop_bad <- data.frame(
    route = c("cpu_cast", "cpu_broadcast"),
    count = c(19L, 17L),
    stringsAsFactors = FALSE
  )

  gate_ok <- audit_env$mojor_gpuarray_cast_broadcast_route_gate(current_ok, baseline)
  expect_true(isTRUE(gate_ok$ok))
  expect_true(all(gate_ok$checks$pass))
  expect_match(gate_ok$note, "no-growth", fixed = FALSE)

  gate_bad <- audit_env$mojor_gpuarray_cast_broadcast_route_gate(current_bad, baseline)
  expect_false(isTRUE(gate_bad$ok))
  expect_true(any(!gate_bad$checks$pass))

  gate_drop_ok <- audit_env$mojor_gpuarray_cast_broadcast_route_gate(
    current_drop_ok,
    baseline,
    thresholds = c(cpu_cast = 0.90, cpu_broadcast = 0.90)
  )
  expect_true(isTRUE(gate_drop_ok$ok))
  expect_match(gate_drop_ok$note, "targeted-drop", fixed = FALSE)

  gate_drop_bad <- audit_env$mojor_gpuarray_cast_broadcast_route_gate(
    current_drop_bad,
    baseline,
    thresholds = c(cpu_cast = 0.90, cpu_broadcast = 0.90)
  )
  expect_false(isTRUE(gate_drop_bad$ok))
  expect_true(any(!gate_drop_bad$checks$pass))
})

test_that("GPUArray cast/broadcast route audit emits required route rows", {  skip_if_no_mojo()
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not available")
  }
  if (!mojor_has_gpu()) {
    skip("GPU not available")
  }

  audit_env <- new.env(parent = globalenv())
  sys.source(.gpu_cast_broadcast_route_audit_tool_path(), envir = audit_env)

  routes <- audit_env$mojor_gpuarray_audit_cast_broadcast_routes(repeats = 2L)
  expect_true(is.data.frame(routes))
  expect_true(all(c("route", "count") %in% names(routes)))
  expect_true(all(c("gpu_cast", "cpu_cast", "gpu_broadcast", "cpu_broadcast") %in% routes$route))
  expect_true(all(is.finite(routes$count)))
  expect_true(all(routes$count >= 0))
})
