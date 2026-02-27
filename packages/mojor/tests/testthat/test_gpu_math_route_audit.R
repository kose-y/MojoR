library(testthat)

.gpu_math_route_audit_tool_path <- function() {
  mojor_path <- find_mojor()
  root <- dirname(dirname(mojor_path))
  path <- file.path(root, "tools", "audit_gpuarray_math_routes.R")
  if (!file.exists(path)) {
    stop("could not locate audit_gpuarray_math_routes.R")
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.mojor_skip_gpu_runtime_file_unless_opt_in()

test_that("GPUArray math route gate passes/fails by configured thresholds", {
  audit_env <- new.env(parent = globalenv())
  sys.source(.gpu_math_route_audit_tool_path(), envir = audit_env)

  baseline <- data.frame(
    op = c("__all__", "__all__", "__all__"),
    route = c("cpu_arith", "cpu_kernel_host", "cpu_reduce"),
    count = c(20L, 20L, 0L),
    stringsAsFactors = FALSE
  )
  current_ok <- data.frame(
    op = c("__all__", "__all__", "__all__"),
    route = c("cpu_arith", "cpu_kernel_host", "cpu_reduce"),
    count = c(20L, 20L, 0L),
    stringsAsFactors = FALSE
  )
  current_bad <- data.frame(
    op = c("__all__", "__all__", "__all__"),
    route = c("cpu_arith", "cpu_kernel_host", "cpu_reduce"),
    count = c(21L, 20L, 0L),
    stringsAsFactors = FALSE
  )
  current_drop_ok <- data.frame(
    op = c("__all__", "__all__", "__all__"),
    route = c("cpu_arith", "cpu_kernel_host", "cpu_reduce"),
    count = c(17L, 17L, 0L),
    stringsAsFactors = FALSE
  )
  current_drop_bad <- data.frame(
    op = c("__all__", "__all__", "__all__"),
    route = c("cpu_arith", "cpu_kernel_host", "cpu_reduce"),
    count = c(18L, 17L, 0L),
    stringsAsFactors = FALSE
  )

  gate_ok <- audit_env$mojor_gpuarray_math_route_gate(current_ok, baseline)
  expect_true(isTRUE(gate_ok$ok))
  expect_true(all(gate_ok$checks$pass))
  expect_true(all(c("op", "route", "baseline_count", "current_count") %in% names(gate_ok$checks)))
  expect_match(gate_ok$note, "no-growth", fixed = FALSE)

  gate_legacy <- audit_env$mojor_gpuarray_math_route_gate(
    current_ok[, c("route", "count")],
    baseline[, c("route", "count")]
  )
  expect_true(isTRUE(gate_legacy$ok))

  gate_bad <- audit_env$mojor_gpuarray_math_route_gate(current_bad, baseline)
  expect_false(isTRUE(gate_bad$ok))
  expect_true(any(!gate_bad$checks$pass))

  gate_drop_ok <- audit_env$mojor_gpuarray_math_route_gate(
    current_drop_ok,
    baseline,
    thresholds = c(cpu_arith = 0.85, cpu_kernel_host = 0.85, cpu_reduce = 0.85)
  )
  expect_true(isTRUE(gate_drop_ok$ok))
  expect_match(gate_drop_ok$note, "targeted-drop", fixed = FALSE)

  gate_drop_bad <- audit_env$mojor_gpuarray_math_route_gate(
    current_drop_bad,
    baseline,
    thresholds = c(cpu_arith = 0.85, cpu_kernel_host = 0.85, cpu_reduce = 0.85)
  )
  expect_false(isTRUE(gate_drop_bad$ok))
  expect_true(any(!gate_drop_bad$checks$pass))

  baseline_per_op <- data.frame(
    op = c("__all__", "__all__", "gpu_minimum", "gpu_minimum"),
    route = c("cpu_arith", "cpu_kernel_host", "cpu_arith", "cpu_kernel_host"),
    count = c(20L, 20L, 10L, 0L),
    stringsAsFactors = FALSE
  )
  current_per_op_regress <- data.frame(
    op = c("__all__", "__all__", "gpu_minimum", "gpu_minimum"),
    route = c("cpu_arith", "cpu_kernel_host", "cpu_arith", "cpu_kernel_host"),
    count = c(20L, 20L, 11L, 0L),
    stringsAsFactors = FALSE
  )
  thresholds_per_op <- data.frame(
    op = c("__all__", "__all__", "__all__", "gpu_minimum"),
    route = c("cpu_arith", "cpu_kernel_host", "cpu_reduce", "cpu_arith"),
    threshold = c(1.00, 1.00, 1.00, 1.00),
    stringsAsFactors = FALSE
  )
  gate_per_op_bad <- audit_env$mojor_gpuarray_math_route_gate(
    current_per_op_regress,
    baseline_per_op,
    thresholds = thresholds_per_op
  )
  expect_false(isTRUE(gate_per_op_bad$ok))
  expect_true(any(gate_per_op_bad$checks$op == "gpu_minimum" & !gate_per_op_bad$checks$pass))

  baseline_reduce <- data.frame(
    op = c("__all__", "gpu_prod"),
    route = c("cpu_reduce", "cpu_reduce"),
    count = c(0L, 0L),
    stringsAsFactors = FALSE
  )
  current_reduce_regress <- data.frame(
    op = c("__all__", "gpu_prod"),
    route = c("cpu_reduce", "cpu_reduce"),
    count = c(1L, 1L),
    stringsAsFactors = FALSE
  )
  thresholds_reduce <- data.frame(
    op = c("__all__", "gpu_prod"),
    route = c("cpu_reduce", "cpu_reduce"),
    threshold = c(1.00, 1.00),
    stringsAsFactors = FALSE
  )
  gate_reduce_bad <- audit_env$mojor_gpuarray_math_route_gate(
    current_reduce_regress,
    baseline_reduce,
    thresholds = thresholds_reduce
  )
  expect_false(isTRUE(gate_reduce_bad$ok))
  expect_true(any(gate_reduce_bad$checks$op == "gpu_prod" & !gate_reduce_bad$checks$pass))

  reason_baseline <- data.frame(
    op = c("__all__", "__all__", "__all__"),
    route = c("cpu_arith", "cpu_arith", "cpu_kernel_host"),
    reason = c("kernel_dispatch_failed", "round_digits_host_fallback", "<none>"),
    count = c(4L, 2L, 0L),
    stringsAsFactors = FALSE
  )
  reason_current_ok <- data.frame(
    op = c("__all__", "__all__", "__all__"),
    route = c("cpu_arith", "cpu_arith", "cpu_kernel_host"),
    reason = c("kernel_dispatch_failed", "round_digits_host_fallback", "<none>"),
    count = c(4L, 2L, 0L),
    stringsAsFactors = FALSE
  )
  reason_current_bad <- data.frame(
    op = c("__all__", "__all__", "__all__"),
    route = c("cpu_arith", "cpu_arith", "cpu_kernel_host"),
    reason = c("kernel_dispatch_failed", "round_digits_host_fallback", "<none>"),
    count = c(5L, 2L, 0L),
    stringsAsFactors = FALSE
  )

  reason_gate_ok <- audit_env$mojor_gpuarray_math_reason_gate(
    reason_current_ok,
    reason_baseline
  )
  expect_true(isTRUE(reason_gate_ok$ok))
  expect_true(all(reason_gate_ok$checks$pass))
  expect_true(all(c("op", "route", "reason", "baseline_count", "current_count") %in% names(reason_gate_ok$checks)))

  reason_gate_bad <- audit_env$mojor_gpuarray_math_reason_gate(
    reason_current_bad,
    reason_baseline
  )
  expect_false(isTRUE(reason_gate_bad$ok))
  expect_true(any(!reason_gate_bad$checks$pass))
})

test_that("GPUArray math route audit helper parses bool flags and muffles fallback warnings", {
  audit_env <- new.env(parent = globalenv())
  sys.source(.gpu_math_route_audit_tool_path(), envir = audit_env)

  args_true <- c("--quiet-fallback-warnings=true")
  args_false <- c("--quiet-fallback-warnings=false")
  expect_true(isTRUE(audit_env$.mojor_parse_bool_arg(args_true, "quiet-fallback-warnings", FALSE)))
  expect_false(isTRUE(audit_env$.mojor_parse_bool_arg(args_false, "quiet-fallback-warnings", TRUE)))

  expect_warning(
    audit_env$.mojor_gpuarray_eval_math_op(
      {
        warning("mojor gpu fallback to CPU via cpu_arith [x]: y")
        NULL
      },
      quiet_fallback_warnings = FALSE
    ),
    "mojor gpu fallback to CPU via"
  )

  expect_silent(
    audit_env$.mojor_gpuarray_eval_math_op(
      {
        warning("mojor gpu fallback to CPU via cpu_arith [x]: y")
        NULL
      },
      quiet_fallback_warnings = TRUE
    )
  )

  zero_rows <- audit_env$.mojor_gpuarray_math_routes_df(
    route_vec = character(),
    op_vec = character(),
    known_ops = c("log10", "gpu_atan2")
  )
  expect_true(all(c("log10", "gpu_atan2") %in% zero_rows$op))
  expect_true(all(zero_rows$count[zero_rows$op %in% c("log10", "gpu_atan2")] == 0L))

  reason_rows <- audit_env$.mojor_gpuarray_math_reason_counts_df(
    route_vec = c("cpu_arith", "cpu_arith"),
    reason_vec = c("kernel_dispatch_failed", ""),
    op_vec = c("gpu_atan2", "round_digits2"),
    known_ops = c("gpu_atan2", "round_digits2", "gpu_sum")
  )
  expect_true(any(reason_rows$op == "gpu_sum"))
  expect_true(any(reason_rows$reason == "<none>"))
})

test_that("GPUArray math route audit profiles expose ci-fast and nightly defaults", {
  audit_env <- new.env(parent = globalenv())
  sys.source(.gpu_math_route_audit_tool_path(), envir = audit_env)

  fast <- audit_env$mojor_gpuarray_math_route_thresholds(profile = "ci-fast")
  expect_true(all(fast$op == "__all__"))
  expect_true(all(fast$threshold == 1.00))

  nightly <- audit_env$mojor_gpuarray_math_route_thresholds(profile = "nightly")
  expect_true(any(nightly$op == "gpu_prod"))
  expect_true(all(nightly$threshold == 1.00))

  defaults_fast <- audit_env$.mojor_gpuarray_run_profile_defaults("ci-fast")
  expect_identical(defaults_fast$repeats, 2L)
  expect_identical(defaults_fast$profile, "ci-fast")

  defaults_nightly <- audit_env$.mojor_gpuarray_run_profile_defaults("nightly")
  expect_identical(defaults_nightly$repeats, 8L)
  expect_identical(defaults_nightly$profile, "nightly")
  expect_identical(defaults_nightly$progress_every, 2L)
})

test_that("GPUArray math route audit emits required route rows", {
  skip_if_no_mojo()
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not available")
  }
  if (!mojor_has_gpu()) {
    skip("GPU not available")
  }

  audit_env <- new.env(parent = globalenv())
  sys.source(.gpu_math_route_audit_tool_path(), envir = audit_env)

  routes <- audit_env$mojor_gpuarray_audit_math_routes(repeats = 2L)
  expect_true(is.data.frame(routes))
  expect_true(all(c("op", "route", "count") %in% names(routes)))
  expect_true("cpu_arith" %in% routes$route[routes$op == "__all__"])
  expect_true(any(routes$op == "log10"))
  expect_true(any(routes$op == "gpu_atan2"))
  expect_true(any(routes$op == "gpu_prod"))
  expect_true(all(is.finite(routes$count)))
  expect_true(all(routes$count >= 0))

  detail <- audit_env$mojor_gpuarray_audit_math_routes(repeats = 1L, return_details = TRUE)
  expect_true(is.list(detail))
  expect_true(all(c("routes", "reasons") %in% names(detail)))
  expect_true(is.data.frame(detail$routes))
  expect_true(is.data.frame(detail$reasons))
  expect_true(all(c("op", "route", "reason", "count") %in% names(detail$reasons)))
})
