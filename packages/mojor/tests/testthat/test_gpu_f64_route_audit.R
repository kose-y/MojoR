library(testthat)

.gpu_f64_route_audit_tool_path <- function() {
  mojor_path <- find_mojor()
  root <- dirname(dirname(mojor_path))
  candidates <- c(
    file.path(root, "tools", "audit_gpuarray_f64_routes.R"),
    file.path(dirname(dirname(root)), "prototype", "tools", "audit_gpuarray_f64_routes.R")
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0L) {
    stop("could not locate audit_gpuarray_f64_routes.R")
  }
  normalizePath(hit[[1L]], winslash = "/", mustWork = TRUE)
}

.mojor_skip_gpu_runtime_file_unless_opt_in()

test_that("GPUArray f64 route gate passes/fails by configured thresholds", {  audit_env <- new.env(parent = globalenv())
  sys.source(.gpu_f64_route_audit_tool_path(), envir = audit_env)

  baseline <- data.frame(
    route = c("cpu_matmul", "cpu_reduce"),
    count = c(20L, 20L),
    stringsAsFactors = FALSE
  )
  current_ok <- data.frame(
    route = c("cpu_matmul", "cpu_reduce"),
    count = c(20L, 17L),
    stringsAsFactors = FALSE
  )
  current_bad <- data.frame(
    route = c("cpu_matmul", "cpu_reduce"),
    count = c(21L, 18L),
    stringsAsFactors = FALSE
  )
  current_drop_ok <- data.frame(
    route = c("cpu_matmul", "cpu_reduce"),
    count = c(20L, 17L),
    stringsAsFactors = FALSE
  )
  current_drop_bad <- data.frame(
    route = c("cpu_matmul", "cpu_reduce"),
    count = c(20L, 18L),
    stringsAsFactors = FALSE
  )

  gate_ok <- audit_env$mojor_gpuarray_f64_route_gate(current_ok, baseline)
  expect_true(isTRUE(gate_ok$ok))
  expect_true(all(gate_ok$checks$pass))
  expect_match(gate_ok$note, "no-growth", fixed = FALSE)

  gate_bad <- audit_env$mojor_gpuarray_f64_route_gate(current_bad, baseline)
  expect_false(isTRUE(gate_bad$ok))
  expect_true(any(!gate_bad$checks$pass))

  gate_drop_ok <- audit_env$mojor_gpuarray_f64_route_gate(
    current_drop_ok,
    baseline,
    thresholds = c(cpu_matmul = 1.00, cpu_reduce = 0.85)
  )
  expect_true(isTRUE(gate_drop_ok$ok))
  expect_match(gate_drop_ok$note, "targeted-drop", fixed = FALSE)

  gate_drop_bad <- audit_env$mojor_gpuarray_f64_route_gate(
    current_drop_bad,
    baseline,
    thresholds = c(cpu_matmul = 1.00, cpu_reduce = 0.85)
  )
  expect_false(isTRUE(gate_drop_bad$ok))
  expect_true(any(!gate_drop_bad$checks$pass))
})

test_that("GPUArray f64 route audit lock evicts stale lockdir", {  audit_env <- new.env(parent = globalenv())
  sys.source(.gpu_f64_route_audit_tool_path(), envir = audit_env)

  old_lock_dir_fn <- get(".mojor_gpuarray_f64_audit_lock_dir", envir = audit_env, inherits = FALSE)
  lock_root <- file.path(tempdir(), paste0("mojor-f64-audit-lock-", Sys.getpid()))
  lock_dir <- file.path(lock_root, "lockdir")
  dir.create(lock_root, recursive = TRUE, showWarnings = FALSE)
  on.exit({
    assign(".mojor_gpuarray_f64_audit_lock_dir", old_lock_dir_fn, envir = audit_env)
    unlink(lock_root, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  assign(".mojor_gpuarray_f64_audit_lock_dir", function() lock_dir, envir = audit_env)
  dir.create(lock_dir, recursive = TRUE, showWarnings = FALSE)
  Sys.setFileTime(lock_dir, Sys.time() - 3600)

  out <- audit_env$.mojor_gpuarray_with_f64_audit_lock(
    42L,
    wait_seconds = 1,
    stale_seconds = 1
  )
  expect_identical(out, 42L)
  expect_false(dir.exists(lock_dir))
})

test_that("GPUArray f64 route audit lock releases on error", {  audit_env <- new.env(parent = globalenv())
  sys.source(.gpu_f64_route_audit_tool_path(), envir = audit_env)

  old_lock_dir_fn <- get(".mojor_gpuarray_f64_audit_lock_dir", envir = audit_env, inherits = FALSE)
  lock_root <- file.path(tempdir(), paste0("mojor-f64-audit-lock-error-", Sys.getpid()))
  lock_dir <- file.path(lock_root, "lockdir")
  dir.create(lock_root, recursive = TRUE, showWarnings = FALSE)
  on.exit({
    assign(".mojor_gpuarray_f64_audit_lock_dir", old_lock_dir_fn, envir = audit_env)
    unlink(lock_root, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  assign(".mojor_gpuarray_f64_audit_lock_dir", function() lock_dir, envir = audit_env)
  expect_error(
    audit_env$.mojor_gpuarray_with_f64_audit_lock(
      stop("forced lock failure"),
      wait_seconds = 1,
      stale_seconds = 60
    ),
    "forced lock failure"
  )
  expect_false(dir.exists(lock_dir))
})

test_that("GPUArray f64 route audit emits required route rows", {  skip_if_no_mojo()
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not available")
  }
  if (!mojor_has_gpu()) {
    skip("GPU not available")
  }

  audit_env <- new.env(parent = globalenv())
  sys.source(.gpu_f64_route_audit_tool_path(), envir = audit_env)

  routes <- audit_env$mojor_gpuarray_audit_f64_routes(repeats = 2L)
  expect_true(is.data.frame(routes))
  expect_true(all(c("route", "count") %in% names(routes)))
  expect_true(all(c("gpu_matmul", "cpu_matmul", "gpu_reduce", "cpu_reduce") %in% routes$route))
  expect_true(all(is.finite(routes$count)))
  expect_true(all(routes$count >= 0))
})
