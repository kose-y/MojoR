library(testthat)

.gpu_i32_route_audit_tool_path <- function() {
  mojor_path <- find_mojor()
  root <- dirname(dirname(mojor_path))
  candidates <- c(
    file.path(root, "tools", "audit_gpuarray_i32_routes.R"),
    file.path(dirname(dirname(root)), "prototype", "tools", "audit_gpuarray_i32_routes.R")
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0L) {
    stop("could not locate audit_gpuarray_i32_routes.R")
  }
  normalizePath(hit[[1L]], winslash = "/", mustWork = TRUE)
}

.mojor_skip_gpu_runtime_file_unless_opt_in()

test_that("GPUArray i32 route gate passes/fails by configured thresholds", {  audit_env <- new.env(parent = globalenv())
  sys.source(.gpu_i32_route_audit_tool_path(), envir = audit_env)

  baseline <- data.frame(
    route = c("cpu_matmul", "cpu_crossprod", "cpu_tcrossprod", "cpu_reduce"),
    count = c(20L, 10L, 10L, 30L),
    stringsAsFactors = FALSE
  )
  current_ok <- data.frame(
    route = c("cpu_matmul", "cpu_crossprod", "cpu_tcrossprod", "cpu_reduce"),
    count = c(20L, 10L, 10L, 30L),
    stringsAsFactors = FALSE
  )
  current_bad <- data.frame(
    route = c("cpu_matmul", "cpu_crossprod", "cpu_tcrossprod", "cpu_reduce"),
    count = c(21L, 10L, 10L, 30L),
    stringsAsFactors = FALSE
  )
  current_drop_ok <- data.frame(
    route = c("cpu_matmul", "cpu_crossprod", "cpu_tcrossprod", "cpu_reduce"),
    count = c(19L, 9L, 9L, 27L),
    stringsAsFactors = FALSE
  )
  current_drop_bad <- data.frame(
    route = c("cpu_matmul", "cpu_crossprod", "cpu_tcrossprod", "cpu_reduce"),
    count = c(20L, 10L, 10L, 28L),
    stringsAsFactors = FALSE
  )

  gate_ok <- audit_env$mojor_gpuarray_i32_route_gate(current_ok, baseline)
  expect_true(isTRUE(gate_ok$ok))
  expect_true(all(gate_ok$checks$pass))
  expect_match(gate_ok$note, "no-growth", fixed = FALSE)

  gate_bad <- audit_env$mojor_gpuarray_i32_route_gate(current_bad, baseline)
  expect_false(isTRUE(gate_bad$ok))
  expect_true(any(!gate_bad$checks$pass))

  gate_drop_ok <- audit_env$mojor_gpuarray_i32_route_gate(
    current_drop_ok,
    baseline,
    thresholds = c(
      cpu_matmul = 0.95,
      cpu_crossprod = 0.90,
      cpu_tcrossprod = 0.90,
      cpu_reduce = 0.90
    )
  )
  expect_true(isTRUE(gate_drop_ok$ok))
  expect_match(gate_drop_ok$note, "targeted-drop", fixed = FALSE)

  gate_drop_bad <- audit_env$mojor_gpuarray_i32_route_gate(
    current_drop_bad,
    baseline,
    thresholds = c(
      cpu_matmul = 0.95,
      cpu_crossprod = 0.90,
      cpu_tcrossprod = 0.90,
      cpu_reduce = 0.90
    )
  )
  expect_false(isTRUE(gate_drop_bad$ok))
  expect_true(any(!gate_drop_bad$checks$pass))
})

test_that("GPUArray i32 route audit emits required route rows", {  skip_if_no_mojo()
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not available")
  }
  if (!mojor_has_gpu()) {
    skip("GPU not available")
  }

  audit_env <- new.env(parent = globalenv())
  sys.source(.gpu_i32_route_audit_tool_path(), envir = audit_env)

  routes <- audit_env$mojor_gpuarray_audit_i32_routes(repeats = 2L)
  expect_true(is.data.frame(routes))
  expect_true(all(c("route", "count") %in% names(routes)))
  expect_true(all(c(
    "gpu_matmul", "cpu_matmul",
    "gpu_crossprod", "cpu_crossprod",
    "gpu_tcrossprod", "cpu_tcrossprod",
    "gpu_reduce", "cpu_reduce"
  ) %in% routes$route))
  expect_true(all(is.finite(routes$count)))
  expect_true(all(routes$count >= 0))
})
