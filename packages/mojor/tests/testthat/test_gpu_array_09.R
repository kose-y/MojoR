# Split from test_gpu_array_06.R.
# Shared utilities are in helper-gpu-array.R.

.mojor_skip_gpu_runtime_file_unless_opt_in()

test_that("GPUArray binary math kernel-fail lane tags kernel reason code across dispatch patterns", {  .skip_if_no_gpu_backend()

  dispatch_env <- environment(.mojor_gpu_binary_math_apply)
  orig_call_kernel <- get(
    ".mojor_gpu_call_kernel",
    envir = dispatch_env,
    inherits = TRUE
  )
  assign(
    ".mojor_gpu_call_kernel",
    function(...) stop("forced binary kernel failure"),
    envir = dispatch_env
  )
  on.exit(
    assign(
      ".mojor_gpu_call_kernel",
      orig_call_kernel,
      envir = dispatch_env
    ),
    add = TRUE
  )

  x <- matrix(c(-2.0, -0.5, 0.0, 1.0), nrow = 2L, ncol = 2L)
  y <- matrix(c(-1.5, 0.5, 0.2, 2.0), nrow = 2L, ncol = 2L)
  gx <- GPUArray(x, dtype = "f32")
  gy <- GPUArray(y, dtype = "f32")

  out_arr_arr <- gpu_maximum(gx, gy)
  out_arr_sca <- gpu_maximum(gx, 0.25)
  out_sca_arr <- gpu_maximum(0.25, gy)

  expect_equal(mojor_gpu_array_read(out_arr_arr), pmax(x, y), tolerance = 1e-5)
  expect_equal(mojor_gpu_array_read(out_arr_sca), pmax(x, 0.25), tolerance = 1e-5)
  expect_equal(mojor_gpu_array_read(out_sca_arr), pmax(0.25, y), tolerance = 1e-5)
  expect_null(dim(out_sca_arr))

  for (out in list(out_arr_arr, out_arr_sca, out_sca_arr)) {
    expect_identical(attr(out, "gpu_fallback", exact = TRUE), "cpu_arith")
    expect_identical(
      attr(out, "gpu_fallback_reason_code", exact = TRUE),
      "kernel_dispatch_failed"
    )
  }

  .mojor_gpu_array_free_all(out_sca_arr, out_arr_sca, out_arr_arr, gy, gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray backend-aware route contract table is stable for unary/binary/reduce lanes", {  .skip_if_no_gpu_backend()

  x_pos <- matrix(c(0.25, 0.5, 1.25, 2.5, 3.75, 4.25), nrow = 2L, ncol = 3L)
  x_mix <- matrix(c(-2.2, -1.1, 0.0, 1.1, 2.2, 3.3), nrow = 2L, ncol = 3L)
  y <- matrix(c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), nrow = 2L, ncol = 3L)
  y_shape <- matrix(c(0.5, 1.5, 2.5), nrow = 1L, ncol = 3L)
  x_na <- matrix(c(NA_real_, 1, NA_real_, 3), nrow = 2L, ncol = 2L)
  y_na <- matrix(c(2, NA_real_, 4, NA_real_), nrow = 2L, ncol = 2L)

  gx <- GPUArray(x_pos, dtype = "f32")
  gx_mix <- GPUArray(x_mix, dtype = "f32")
  gy <- GPUArray(y, dtype = "f32")
  gy_shape <- GPUArray(y_shape, dtype = "f32")
  gx_na <- GPUArray(x_na, dtype = "f32")
  gy_na <- GPUArray(y_na, dtype = "f32")
  backend <- .mojor_gpu_array_api(gx)
  produced <- list()

  on.exit({
    do.call(.mojor_gpu_array_free_all, c(produced, list(gy_na, gx_na, gy_shape, gy, gx_mix, gx)))
  }, add = TRUE)

  lane_contract <- list(
    unary_log = list(
      expr = quote(log(gx)),
      routes = list(
        metal = c(NA_character_, "cpu_arith"),
        default = c(NA_character_, "cpu_arith", "cpu_kernel_host")
      ),
      reasons = list(
        cpu_arith = c("kernel_dispatch_failed", "raw_kernel_unavailable"),
        cpu_kernel_host = c(NA_character_, "kernel_wrapper_dispatch_failed", "kernel_wrapper_unavailable")
      )
    ),
    unary_round_digits2 = list(
      expr = quote(round(gx_mix, digits = 2L)),
      routes = list(
        metal = c("cpu_arith"),
        default = c("cpu_arith")
      ),
      reasons = list(
        cpu_arith = c("round_digits_host_fallback")
      )
    ),
    binary_atan2 = list(
      expr = quote(gpu_atan2(gx, gy)),
      routes = list(
        metal = c(NA_character_, "cpu_arith"),
        default = c(NA_character_, "cpu_arith")
      ),
      reasons = list(
        cpu_arith = c("kernel_dispatch_failed")
      )
    ),
    binary_na_rm = list(
      expr = quote(gpu_minimum(gx_na, gy_na, na.rm = TRUE)),
      routes = list(
        metal = c("cpu_arith"),
        default = c("cpu_arith")
      ),
      reasons = list(
        cpu_arith = c("na_rm_host_parity")
      )
    ),
    binary_shape_mismatch = list(
      expr = quote(gpu_minimum(gx, gy_shape)),
      routes = list(
        metal = c("cpu_arith"),
        default = c("cpu_arith")
      ),
      reasons = list(
        cpu_arith = c("shape_mismatch_host_parity")
      )
    ),
    reduce_sum = list(
      expr = quote(gpu_sum(gx)),
      routes = list(
        metal = c("gpu_reduce", "cpu_reduce"),
        default = c("gpu_reduce", "cpu_reduce")
      ),
      reasons = list()
    )
  )

  run_lane <- function(lane_name, lane_spec) {
    out <- suppressWarnings(eval(lane_spec$expr, envir = environment()))
    produced[[length(produced) + 1L]] <<- out
    route <- attr(out, "gpu_fallback", exact = TRUE)
    reason <- attr(out, "gpu_fallback_reason_code", exact = TRUE)
    route_norm <- if (is.null(route)) NA_character_ else as.character(route)
    reason_norm <- if (is.null(reason)) NA_character_ else as.character(reason)

    expected_routes <- lane_spec$routes$default
    if (identical(backend, "metal")) {
      expected_routes <- lane_spec$routes$metal
    }
    expect_true(
      route_norm %in% expected_routes,
      info = paste0(lane_name, ": route=", route_norm, ", backend=", backend)
    )
    if (!is.na(route_norm)) {
      allowed_reasons <- lane_spec$reasons[[route_norm]]
      if (!is.null(allowed_reasons)) {
        expect_true(
          reason_norm %in% allowed_reasons,
          info = paste0(lane_name, ": reason=", reason_norm, ", route=", route_norm)
        )
      }
    }
  }

  for (lane_name in names(lane_contract)) {
    run_lane(lane_name, lane_contract[[lane_name]])
  }

  do.call(.mojor_gpu_array_free_all, c(produced, list(gy_na, gx_na, gy_shape, gy, gx_mix, gx)))
  produced <- list()
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray unary math raw-kernel unavailability routes through wrapper lane", {  .skip_if_no_gpu_backend()

  dispatch_env <- environment(.mojor_gpu_unary_math_dispatch)
  orig_get_kernel <- get(
    ".mojor_gpu_get_unary_math_kernel",
    envir = dispatch_env,
    inherits = FALSE
  )
  assign(
    ".mojor_gpu_get_unary_math_kernel",
    function(...) {
      list(
        gpu_func_raw = NULL,
        gpu_func = function(x) {
          .mojor_gpu_wrap_host_op_result(
            tan(mojor_gpu_array_read(x)),
            api = .mojor_gpu_pick_api(x),
            route = "cpu_kernel_host",
            reason = "forced unary wrapper fallback",
            reason_code = "forced_wrapper_fallback"
          )
        }
      )
    },
    envir = dispatch_env
  )
  on.exit(
    assign(
      ".mojor_gpu_get_unary_math_kernel",
      orig_get_kernel,
      envir = dispatch_env
    ),
    add = TRUE
  )

  x <- matrix(c(0.25, 0.5, 1.25, 2.5), nrow = 2L, ncol = 2L)
  gx <- GPUArray(x, dtype = "f32")

  out <- tan(gx)
  expect_true(inherits(out, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(out), tan(x), tolerance = 1e-5)
  expect_identical(attr(out, "gpu_fallback", exact = TRUE), "cpu_kernel_host")
  expect_identical(
    attr(out, "gpu_fallback_reason_code", exact = TRUE),
    "forced_wrapper_fallback"
  )

  .mojor_gpu_array_free_all(out, gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray math wrappers do not grow live buffers under repeated calls", {  .skip_if_no_gpu_backend()

  x <- matrix(c(0.25, 0.5, 1.25, 2.5, 3.75, 4.25), nrow = 2L, ncol = 3L)
  y <- matrix(c(-1.5, 0.5, 0.2, 2.0, 1.5, 3.5), nrow = 2L, ncol = 3L)
  xi <- matrix(as.integer(1:6), nrow = 2L, ncol = 3L)
  gx <- GPUArray(x, dtype = "f32")
  gy <- GPUArray(y, dtype = "f32")
  gi <- GPUArray(xi, dtype = "i32")
  gc()
  base_live <- mojor_gpu_buf_f32_live_count()
  expect_route_lane <- function(out, lane, allowed_routes, allowed_reasons = NULL) {
    route <- attr(out, "gpu_fallback", exact = TRUE)
    reason <- attr(out, "gpu_fallback_reason_code", exact = TRUE)
    route_norm <- if (is.null(route)) NA_character_ else as.character(route)
    reason_norm <- if (is.null(reason)) NA_character_ else as.character(reason)
    expect_true(route_norm %in% allowed_routes, info = lane)
    if (!is.null(allowed_reasons) && !is.na(route_norm) && route_norm %in% names(allowed_reasons)) {
      expect_true(reason_norm %in% allowed_reasons[[route_norm]], info = lane)
    }
  }

  for (iter in seq_len(4L)) {
    out_log <- suppressWarnings(log(gx))
    out_round <- suppressWarnings(round(gx, digits = 2L))
    out_pmin <- suppressWarnings(gpu_minimum(gx, gy, na.rm = TRUE))
    out_pmax <- suppressWarnings(gpu_maximum(gx, gy, na.rm = TRUE))
    out_log_i32 <- suppressWarnings(log(gi))
    idx_min <- gpu_which_min(gx)
    idx_max <- gpu_which_max(gy)

    expect_route_lane(
      out_log,
      "log",
      c(NA_character_, "cpu_arith", "cpu_kernel_host"),
      list(
        cpu_arith = c("kernel_dispatch_failed", "raw_kernel_unavailable"),
        cpu_kernel_host = c(NA_character_, "kernel_wrapper_dispatch_failed", "kernel_wrapper_unavailable")
      )
    )
    expect_route_lane(
      out_round,
      "round_digits2",
      c("cpu_arith"),
      list(cpu_arith = c("round_digits_host_fallback"))
    )
    expect_route_lane(
      out_pmin,
      "pmin_na_rm",
      c("cpu_arith"),
      list(cpu_arith = c("na_rm_host_parity"))
    )
    expect_route_lane(
      out_pmax,
      "pmax_na_rm",
      c("cpu_arith"),
      list(cpu_arith = c("na_rm_host_parity"))
    )
    expect_route_lane(
      out_log_i32,
      "log_i32",
      c(NA_character_, "cpu_arith", "cpu_kernel_host"),
      list(
        cpu_arith = c("kernel_dispatch_failed", "raw_kernel_unavailable"),
        cpu_kernel_host = c(NA_character_, "kernel_wrapper_dispatch_failed", "kernel_wrapper_unavailable")
      )
    )

    expect_true(is.numeric(idx_min) || is.integer(idx_min))
    expect_true(is.numeric(idx_max) || is.integer(idx_max))

    .mojor_gpu_array_free_all(
      out_log_i32, out_pmax, out_pmin, out_round, out_log
    )
    gc()
    expect_equal(mojor_gpu_buf_f32_live_count(), base_live)
  }

  .mojor_gpu_array_free_all(gi, gy, gx)
  gc()
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})
