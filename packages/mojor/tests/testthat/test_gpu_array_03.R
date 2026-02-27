# Split from test_gpu_array.R (chunk 03).
# Shared utilities are in helper-gpu-array.R.

.mojor_skip_gpu_runtime_file_unless_opt_in()

test_that("f64 matmul probe includes multiple shapes per mode", {  cfg_mm <- .mojor_gpu_f64_matmul_probe_configs(mode = "matmul")
  cfg_gemv <- .mojor_gpu_f64_matmul_probe_configs(mode = "gemv")
  cfg_gevm <- .mojor_gpu_f64_matmul_probe_configs(mode = "gevm")

  expect_true(length(cfg_mm) >= 2L)
  expect_true(length(cfg_gemv) >= 2L)
  expect_true(length(cfg_gevm) >= 2L)
  expect_true(all(vapply(cfg_mm, function(cfg) identical(cfg$mode, "matmul"), FUN.VALUE = logical(1L))))
  expect_true(all(vapply(cfg_gemv, function(cfg) identical(as.integer(cfg$n), 1L), FUN.VALUE = logical(1L))))
  expect_true(all(vapply(cfg_gevm, function(cfg) identical(as.integer(cfg$m), 1L), FUN.VALUE = logical(1L))))
})

test_that("f64 matmul probe max-shape cap parsing is deterministic", {  old_max_shapes <- Sys.getenv("MOJOR_GPU_F64_PROBE_MAX_SHAPES", unset = NA_character_)
  on.exit({
    if (is.na(old_max_shapes)) {
      Sys.unsetenv("MOJOR_GPU_F64_PROBE_MAX_SHAPES")
    } else {
      Sys.setenv(MOJOR_GPU_F64_PROBE_MAX_SHAPES = old_max_shapes)
    }
  }, add = TRUE)

  Sys.unsetenv("MOJOR_GPU_F64_PROBE_MAX_SHAPES")
  expect_identical(.mojor_gpu_f64_probe_max_shapes(), 1L)

  Sys.setenv(MOJOR_GPU_F64_PROBE_MAX_SHAPES = "2")
  expect_identical(.mojor_gpu_f64_probe_max_shapes(), 2L)

  Sys.setenv(MOJOR_GPU_F64_PROBE_MAX_SHAPES = "0")
  expect_identical(.mojor_gpu_f64_probe_max_shapes(), 1L)

  Sys.setenv(MOJOR_GPU_F64_PROBE_MAX_SHAPES = "abc")
  expect_identical(.mojor_gpu_f64_probe_max_shapes(), 1L)
})

test_that("f64 matmul probe caps repeated shape attempts", {  cap_env <- environment(.mojor_gpu_probe_f64_matmul)
  old_bridge <- get(".mojor_call_bridge", envir = cap_env, inherits = TRUE)
  old_max_shapes <- Sys.getenv("MOJOR_GPU_F64_PROBE_MAX_SHAPES", unset = NA_character_)
  on.exit({
    assign(".mojor_call_bridge", old_bridge, envir = cap_env)
    if (is.na(old_max_shapes)) {
      Sys.unsetenv("MOJOR_GPU_F64_PROBE_MAX_SHAPES")
    } else {
      Sys.setenv(MOJOR_GPU_F64_PROBE_MAX_SHAPES = old_max_shapes)
    }
  }, add = TRUE)

  matmul_calls <- 0L
  assign(
    ".mojor_call_bridge",
    function(bridge, ...) {
      if (identical(bridge, "mojor_gpu_buf_f64_alloc")) {
        return(structure(list(token = tempfile()), class = "mojor_gpu_buf_f64"))
      }
      if (identical(bridge, "mojor_gpu_buf_f64_write")) {
        return(invisible(TRUE))
      }
      if (identical(bridge, "mojor_gpu_buf_f64_matmul")) {
        matmul_calls <<- matmul_calls + 1L
        stop(sprintf("mock probe matmul failure %d", matmul_calls), call. = FALSE)
      }
      if (identical(bridge, "mojor_gpu_buf_f64_free")) {
        return(invisible(TRUE))
      }
      stop(sprintf("unexpected bridge call in test: %s", bridge), call. = FALSE)
    },
    envir = cap_env
  )

  fake_ctx <- structure(list(token = 42L), class = "mojor_gpu_ctx")

  Sys.unsetenv("MOJOR_GPU_F64_PROBE_MAX_SHAPES")
  res_one <- .mojor_gpu_probe_f64_matmul(fake_ctx, mode = "matmul")
  if (
    identical(res_one$code, "probe_context_missing") ||
      (is.character(res_one$reason) &&
        length(res_one$reason) == 1L &&
        identical(res_one$reason, "f64 matmul probe requires GPU context"))
  ) {
    skip("f64 matmul probe requires live GPU context in this runtime")
  }
  expect_false(res_one$available)
  expect_equal(matmul_calls, 1L)
  expect_match(res_one$reason, "across 1/2 shapes", fixed = TRUE)

  matmul_calls <- 0L
  Sys.setenv(MOJOR_GPU_F64_PROBE_MAX_SHAPES = "2")
  res_two <- .mojor_gpu_probe_f64_matmul(fake_ctx, mode = "matmul")
  expect_false(res_two$available)
  expect_equal(matmul_calls, 2L)
  expect_match(res_two$reason, "across 2/2 shapes", fixed = TRUE)
})

test_that("f64 reduce capability probe is stable", {  skip_if_no_gpu_f64()

  cap_scalar_1 <- .mojor_gpu_f64_reduce_capable(dims_mode = "scalar")
  cap_scalar_2 <- .mojor_gpu_f64_reduce_capable(dims_mode = "scalar")
  expect_type(cap_scalar_1, "logical")
  expect_length(cap_scalar_1, 1L)
  expect_true(identical(cap_scalar_1, cap_scalar_2))

  cap_dims_1 <- .mojor_gpu_f64_reduce_capable(dims_mode = "dims")
  cap_dims_2 <- .mojor_gpu_f64_reduce_capable(dims_mode = "dims")
  expect_type(cap_dims_1, "logical")
  expect_length(cap_dims_1, 1L)
  expect_true(identical(cap_dims_1, cap_dims_2))

  cap_arg_scalar_1 <- .mojor_gpu_f64_reduce_capable(dims_mode = "scalar", op_class = "arg")
  cap_arg_scalar_2 <- .mojor_gpu_f64_reduce_capable(dims_mode = "scalar", op_class = "arg")
  expect_type(cap_arg_scalar_1, "logical")
  expect_length(cap_arg_scalar_1, 1L)
  expect_true(identical(cap_arg_scalar_1, cap_arg_scalar_2))

  cap_arg_dims_1 <- .mojor_gpu_f64_reduce_capable(dims_mode = "dims", op_class = "arg")
  cap_arg_dims_2 <- .mojor_gpu_f64_reduce_capable(dims_mode = "dims", op_class = "arg")
  expect_type(cap_arg_dims_1, "logical")
  expect_length(cap_arg_dims_1, 1L)
  expect_true(identical(cap_arg_dims_1, cap_arg_dims_2))
})

test_that("f64 matmul capability mode mapping is deterministic", {  expect_identical(.mojor_gpu_f64_matmul_mode(4L, 3L), "matmul")
  expect_identical(.mojor_gpu_f64_matmul_mode(4L, 1L), "gemv")
  expect_identical(.mojor_gpu_f64_matmul_mode(1L, 4L), "gevm")
})

test_that("gpu matmul works for f64", {  skip_if_no_gpu_f64()

  m <- 4L
  k <- 5L
  n <- 3L
  a <- matrix(runif(m * k), nrow = m)
  b <- matrix(runif(k * n), nrow = k)
  ga <- GPUArray(a, dtype = "f64")
  gb <- GPUArray(b, dtype = "f64")

  gc <- gpu_matmul(ga, gb)
  expect_true(inherits(gc, "mojor_gpu_array"))
  host <- mojor_gpu_array_read(gc)
  expect_equal(dim(host), c(m, n))
  expect_equal(host, a %*% b, tolerance = 1e-8)
  expect_true(attr(gc, "gpu_fallback") %in% c("cpu_matmul", "gpu_matmul"))

  .mojor_gpu_array_free_all(gc, gb, ga)
})

test_that("gpu matmul can write into existing f64 array", {  skip_if_no_gpu_f64()

  m <- 4L
  k <- 5L
  n <- 3L
  a <- matrix(runif(m * k), nrow = m)
  b <- matrix(runif(k * n), nrow = k)
  ga <- GPUArray(a, dtype = "f64")
  gb <- GPUArray(b, dtype = "f64")
  out <- GPUArray(n = m * n, dtype = "f64")
  out$dim <- c(m, n)
  out$strides <- .mojor_dim_strides(out$dim)

  out2 <- gpu_matmul_into(out, ga, gb)
  expect_true(identical(.mojor_gpu_array_handle(out2), .mojor_gpu_array_handle(out)))
  host <- mojor_gpu_array_read(out)
  expect_equal(dim(host), c(m, n))
  expect_equal(host, a %*% b, tolerance = 1e-8)
  expect_true(attr(out2, "gpu_fallback") %in% c("cpu_matmul", "gpu_matmul"))

  .mojor_gpu_array_free_all(out, gb, ga)
})

test_that("gpu crossprod and tcrossprod support f64", {  skip_if_no_gpu_f64()

  a <- matrix(runif(12L), nrow = 3L)
  ga <- GPUArray(a, dtype = "f64")

  cp_method <- get0("crossprod.GPUArray", mode = "function", inherits = TRUE)
  tcp_method <- get0("tcrossprod.GPUArray", mode = "function", inherits = TRUE)
  expect_true(is.function(cp_method))
  expect_true(is.function(tcp_method))

  cp <- cp_method(ga)
  expect_true(inherits(cp, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(cp), crossprod(a), tolerance = 1e-8)
  expect_true(attr(cp, "gpu_fallback") %in% c("cpu_crossprod", "gpu_crossprod"))

  tcp <- tcp_method(ga)
  expect_true(inherits(tcp, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(tcp), tcrossprod(a), tolerance = 1e-8)
  expect_true(attr(tcp, "gpu_fallback") %in% c("cpu_tcrossprod", "gpu_tcrossprod"))

  .mojor_gpu_array_free_all(tcp, cp, ga)
})

test_that("vector crossprod/tcrossprod attempt gpu matmul before host fallback", {  .skip_if_no_gpu_backend(require_float = TRUE)

  cp_method <- get0("crossprod.GPUArray", mode = "function", inherits = TRUE)
  tcp_method <- get0("tcrossprod.GPUArray", mode = "function", inherits = TRUE)
  expect_true(is.function(cp_method))
  expect_true(is.function(tcp_method))

  x <- as.numeric(1:6)
  gx <- GPUArray(x, dtype = "f32")
  called <- 0L
  mm_env <- environment(.mojor_gpu_crossprod_dispatch)
  old_mm <- get(".mojor_gpu_matmul", envir = mm_env, inherits = TRUE)
  assign(
    ".mojor_gpu_matmul",
    function(...) {
      called <<- called + 1L
      stop("forced matmul failure", call. = FALSE)
    },
    envir = mm_env
  )
  on.exit(assign(".mojor_gpu_matmul", old_mm, envir = mm_env), add = TRUE)

  cp <- cp_method(gx)
  expect_equal(called, 1L)
  expect_equal(mojor_gpu_array_read(cp), crossprod(x))
  expect_equal(attr(cp, "gpu_fallback"), "cpu_crossprod")

  tcp <- tcp_method(gx)
  expect_equal(called, 2L)
  expect_equal(mojor_gpu_array_read(tcp), tcrossprod(x))
  expect_equal(attr(tcp, "gpu_fallback"), "cpu_tcrossprod")

  .mojor_gpu_array_free_all(tcp, cp, gx)
})

test_that("crossprod/tcrossprod preserve cpu fallback route from matmul path", {  .skip_if_no_gpu_backend(require_float = TRUE)

  cp_method <- get0("crossprod.GPUArray", mode = "function", inherits = TRUE)
  tcp_method <- get0("tcrossprod.GPUArray", mode = "function", inherits = TRUE)
  expect_true(is.function(cp_method))
  expect_true(is.function(tcp_method))

  x <- matrix(as.numeric(1:6), nrow = 2L, ncol = 3L)
  gx <- GPUArray(x, dtype = "f32")
  cp <- NULL
  tcp <- NULL
  on.exit(.mojor_gpu_array_free_all(tcp, cp, gx), add = TRUE)

  called <- 0L
  mm_env <- environment(.mojor_gpu_crossprod_dispatch)
  old_mm <- get(".mojor_gpu_matmul", envir = mm_env, inherits = TRUE)
  assign(
    ".mojor_gpu_matmul",
    function(x, y, transpose_a = FALSE, transpose_b = FALSE, out = NULL) {
      called <<- called + 1L
      host_x <- mojor_gpu_array_read(x)
      host_y <- mojor_gpu_array_read(y)
      if (isTRUE(transpose_a)) host_x <- t(host_x)
      if (isTRUE(transpose_b)) host_y <- t(host_y)
      out_arr <- GPUArray(host_x %*% host_y, dtype = "f32")
      attr(out_arr, "gpu_fallback") <- "cpu_matmul"
      out_arr
    },
    envir = mm_env
  )
  on.exit(assign(".mojor_gpu_matmul", old_mm, envir = mm_env), add = TRUE)

  cp <- cp_method(gx)
  expect_equal(called, 1L)
  expect_equal(mojor_gpu_array_read(cp), crossprod(x), tolerance = 1e-6)
  expect_equal(attr(cp, "gpu_fallback"), "cpu_crossprod")

  tcp <- tcp_method(gx)
  expect_equal(called, 2L)
  expect_equal(mojor_gpu_array_read(tcp), tcrossprod(x), tolerance = 1e-6)
  expect_equal(attr(tcp, "gpu_fallback"), "cpu_tcrossprod")
})

test_that("vector %*% attempts gpu matmul before host fallback", {  .skip_if_no_gpu_backend(require_float = TRUE)

  mm_method <- getS3method("%*%", "GPUArray", optional = TRUE)
  expect_true(is.function(mm_method))

  vx <- as.numeric(1:3)
  vy <- as.numeric(c(4, 5, 6))
  my <- matrix(as.numeric(1:6), nrow = 3L, ncol = 2L)
  mx <- matrix(as.numeric(1:6), nrow = 2L, ncol = 3L)

  gvx <- GPUArray(vx, dtype = "f32")
  gvy <- GPUArray(vy, dtype = "f32")
  gmy <- GPUArray(my, dtype = "f32")
  gmx <- GPUArray(mx, dtype = "f32")

  called <- 0L
  mm_env <- environment(.mojor_gpu_matmul_dispatch)
  old_mm <- get(".mojor_gpu_matmul", envir = mm_env, inherits = TRUE)
  assign(
    ".mojor_gpu_matmul",
    function(...) {
      called <<- called + 1L
      stop("forced matmul failure", call. = FALSE)
    },
    envir = mm_env
  )
  on.exit(assign(".mojor_gpu_matmul", old_mm, envir = mm_env), add = TRUE)

  out_vv <- mm_method(gvx, gvy)
  expect_equal(called, 1L)
  expect_equal(mojor_gpu_array_read(out_vv), vx %*% vy, tolerance = 1e-6)
  expect_equal(attr(out_vv, "gpu_fallback"), "cpu_matmul")

  out_vm <- mm_method(gvx, gmy)
  expect_equal(called, 2L)
  expect_equal(mojor_gpu_array_read(out_vm), vx %*% my, tolerance = 1e-6)
  expect_equal(attr(out_vm, "gpu_fallback"), "cpu_matmul")

  out_mv <- mm_method(gmx, gvy)
  expect_equal(called, 3L)
  expect_equal(mojor_gpu_array_read(out_mv), mx %*% vy, tolerance = 1e-6)
  expect_equal(attr(out_mv, "gpu_fallback"), "cpu_matmul")

  .mojor_gpu_array_free_all(out_mv, out_vm, out_vv, gmx, gmy, gvy, gvx)
})

test_that("host rhs fallback avoids redundant readback in linalg dispatch", {  .skip_if_no_gpu_backend(require_float = TRUE)

  mm_method <- getS3method("%*%", "GPUArray", optional = TRUE)
  cp_method <- get0("crossprod.GPUArray", mode = "function", inherits = TRUE)
  tcp_method <- get0("tcrossprod.GPUArray", mode = "function", inherits = TRUE)
  expect_true(is.function(mm_method))
  expect_true(is.function(cp_method))
  expect_true(is.function(tcp_method))

  x <- matrix(as.numeric(1:6), nrow = 2L, ncol = 3L)
  rhs_mm <- matrix(as.numeric(7:12), nrow = 3L, ncol = 2L)
  rhs_cp <- matrix(as.numeric(13:18), nrow = 2L, ncol = 3L)
  gx <- GPUArray(x, dtype = "f32")
  out_mm <- NULL
  out_cp <- NULL
  out_tcp <- NULL
  on.exit(.mojor_gpu_array_free_all(out_tcp, out_cp, out_mm, gx), add = TRUE)

  dispatch_env <- environment(.mojor_gpu_matmul_dispatch)
  old_mm <- get(".mojor_gpu_matmul", envir = dispatch_env, inherits = TRUE)
  old_read <- get("mojor_gpu_array_read", envir = dispatch_env, inherits = TRUE)
  read_calls <- 0L
  assign(
    ".mojor_gpu_matmul",
    function(...) stop("forced matmul failure", call. = FALSE),
    envir = dispatch_env
  )
  assign(
    "mojor_gpu_array_read",
    function(x, ...) {
      read_calls <<- read_calls + 1L
      old_read(x, ...)
    },
    envir = dispatch_env
  )
  on.exit({
    assign(".mojor_gpu_matmul", old_mm, envir = dispatch_env)
    assign("mojor_gpu_array_read", old_read, envir = dispatch_env)
  }, add = TRUE)

  out_mm <- mm_method(gx, rhs_mm)
  expect_equal(read_calls, 1L)
  expect_equal(old_read(out_mm), x %*% rhs_mm, tolerance = 1e-6)
  expect_equal(attr(out_mm, "gpu_fallback"), "cpu_matmul")

  out_cp <- cp_method(gx, rhs_cp)
  expect_equal(read_calls, 2L)
  expect_equal(old_read(out_cp), crossprod(x, rhs_cp), tolerance = 1e-6)
  expect_equal(attr(out_cp, "gpu_fallback"), "cpu_crossprod")

  out_tcp <- tcp_method(gx, rhs_cp)
  expect_equal(read_calls, 3L)
  expect_equal(old_read(out_tcp), tcrossprod(x, rhs_cp), tolerance = 1e-6)
  expect_equal(attr(out_tcp, "gpu_fallback"), "cpu_tcrossprod")
})

test_that("f64 host rhs matmul fallback avoids temporary rhs GPU conversion", {  skip_if_no_gpu_f64()

  mm_method <- getS3method("%*%", "GPUArray", optional = TRUE)
  expect_true(is.function(mm_method))

  x <- matrix(as.numeric(1:6), nrow = 2L, ncol = 3L)
  rhs <- matrix(as.numeric(7:12), nrow = 3L, ncol = 2L)
  gx <- GPUArray(x, dtype = "f64")
  out <- NULL
  on.exit(.mojor_gpu_array_free_all(out, gx), add = TRUE)

  dispatch_env <- environment(.mojor_gpu_matmul_dispatch)
  old_cap <- get(".mojor_gpu_f64_matmul_capable", envir = dispatch_env, inherits = TRUE)
  old_mm <- get(".mojor_gpu_matmul", envir = dispatch_env, inherits = TRUE)
  old_arr <- get("mojor_gpu_array", envir = dispatch_env, inherits = TRUE)
  cap_modes <- character(0L)
  mm_calls <- 0L
  rhs_conv_calls <- 0L
  assign(
    ".mojor_gpu_f64_matmul_capable",
    function(force_refresh = FALSE, mode = c("matmul", "gemv", "gevm")) {
      mode <- match.arg(mode, c("matmul", "gemv", "gevm"))
      cap_modes <<- c(cap_modes, mode)
      FALSE
    },
    envir = dispatch_env
  )
  assign(
    ".mojor_gpu_matmul",
    function(...) {
      mm_calls <<- mm_calls + 1L
      stop("forced matmul failure", call. = FALSE)
    },
    envir = dispatch_env
  )
  assign(
    "mojor_gpu_array",
    function(x, ..., api = c("auto", "metal", "cuda", "amd"), dtype = c("auto", "f32", "f64", "i32")) {
      if (is.numeric(x) && isTRUE(identical(dim(x), dim(rhs))) && isTRUE(all.equal(as.numeric(x), as.numeric(rhs)))) {
        rhs_conv_calls <<- rhs_conv_calls + 1L
      }
      old_arr(x, ..., api = api, dtype = dtype)
    },
    envir = dispatch_env
  )
  on.exit({
    assign(".mojor_gpu_f64_matmul_capable", old_cap, envir = dispatch_env)
    assign(".mojor_gpu_matmul", old_mm, envir = dispatch_env)
    assign("mojor_gpu_array", old_arr, envir = dispatch_env)
  }, add = TRUE)

  out <- mm_method(gx, rhs)
  expect_equal(attr(out, "gpu_fallback"), "cpu_matmul")
  expect_equal(mojor_gpu_array_read(out), x %*% rhs, tolerance = 1e-8)
  expect_equal(cap_modes, "matmul")
  expect_equal(mm_calls, 0L)
  expect_equal(rhs_conv_calls, 0L)
})

test_that("f64 host rhs crossprod/tcrossprod fallback avoids temporary rhs GPU conversion", {  skip_if_no_gpu_f64()

  cp_method <- get0("crossprod.GPUArray", mode = "function", inherits = TRUE)
  tcp_method <- get0("tcrossprod.GPUArray", mode = "function", inherits = TRUE)
  expect_true(is.function(cp_method))
  expect_true(is.function(tcp_method))

  x <- matrix(as.numeric(1:6), nrow = 2L, ncol = 3L)
  rhs <- matrix(as.numeric(7:12), nrow = 2L, ncol = 3L)
  gx <- GPUArray(x, dtype = "f64")
  out_cp <- NULL
  out_tcp <- NULL
  on.exit(.mojor_gpu_array_free_all(out_tcp, out_cp, gx), add = TRUE)

  cp_env <- environment(.mojor_gpu_crossprod_dispatch)
  tcp_env <- environment(.mojor_gpu_tcrossprod_dispatch)
  old_cap_cp <- get(".mojor_gpu_f64_matmul_capable", envir = cp_env, inherits = TRUE)
  old_mm_cp <- get(".mojor_gpu_matmul", envir = cp_env, inherits = TRUE)
  old_arr_cp <- get("mojor_gpu_array", envir = cp_env, inherits = TRUE)
  old_cap_tcp <- get(".mojor_gpu_f64_matmul_capable", envir = tcp_env, inherits = TRUE)
  old_mm_tcp <- get(".mojor_gpu_matmul", envir = tcp_env, inherits = TRUE)
  old_arr_tcp <- get("mojor_gpu_array", envir = tcp_env, inherits = TRUE)

  cap_modes <- character(0L)
  mm_calls <- 0L
  rhs_conv_calls <- 0L
  cap_fn <- function(force_refresh = FALSE, mode = c("matmul", "gemv", "gevm")) {
    mode <- match.arg(mode, c("matmul", "gemv", "gevm"))
    cap_modes <<- c(cap_modes, mode)
    FALSE
  }
  mm_fn <- function(...) {
    mm_calls <<- mm_calls + 1L
    stop("forced matmul failure", call. = FALSE)
  }
  arr_fn <- function(x, ..., api = c("auto", "metal", "cuda", "amd"), dtype = c("auto", "f32", "f64", "i32")) {
    if (is.numeric(x) && isTRUE(identical(dim(x), dim(rhs))) && isTRUE(all.equal(as.numeric(x), as.numeric(rhs)))) {
      rhs_conv_calls <<- rhs_conv_calls + 1L
    }
    old_arr_cp(x, ..., api = api, dtype = dtype)
  }
  assign(".mojor_gpu_f64_matmul_capable", cap_fn, envir = cp_env)
  assign(".mojor_gpu_matmul", mm_fn, envir = cp_env)
  assign("mojor_gpu_array", arr_fn, envir = cp_env)
  assign(".mojor_gpu_f64_matmul_capable", cap_fn, envir = tcp_env)
  assign(".mojor_gpu_matmul", mm_fn, envir = tcp_env)
  assign("mojor_gpu_array", arr_fn, envir = tcp_env)
  on.exit({
    assign(".mojor_gpu_f64_matmul_capable", old_cap_cp, envir = cp_env)
    assign(".mojor_gpu_matmul", old_mm_cp, envir = cp_env)
    assign("mojor_gpu_array", old_arr_cp, envir = cp_env)
    assign(".mojor_gpu_f64_matmul_capable", old_cap_tcp, envir = tcp_env)
    assign(".mojor_gpu_matmul", old_mm_tcp, envir = tcp_env)
    assign("mojor_gpu_array", old_arr_tcp, envir = tcp_env)
  }, add = TRUE)

  out_cp <- cp_method(gx, rhs)
  expect_equal(attr(out_cp, "gpu_fallback"), "cpu_crossprod")
  expect_equal(mojor_gpu_array_read(out_cp), crossprod(x, rhs), tolerance = 1e-8)

  out_tcp <- tcp_method(gx, rhs)
  expect_equal(attr(out_tcp, "gpu_fallback"), "cpu_tcrossprod")
  expect_equal(mojor_gpu_array_read(out_tcp), tcrossprod(x, rhs), tolerance = 1e-8)

  expect_equal(cap_modes, c("matmul", "matmul"))
  expect_equal(mm_calls, 0L)
  expect_equal(rhs_conv_calls, 0L)
})

test_that("host rhs cast is deferred when linalg GPU path succeeds", {  .skip_if_no_gpu_backend()

  mm_method <- getS3method("%*%", "GPUArray", optional = TRUE)
  cp_method <- get0("crossprod.GPUArray", mode = "function", inherits = TRUE)
  tcp_method <- get0("tcrossprod.GPUArray", mode = "function", inherits = TRUE)
  expect_true(is.function(mm_method))
  expect_true(is.function(cp_method))
  expect_true(is.function(tcp_method))

  x_num <- matrix(as.numeric(1:6), nrow = 2L, ncol = 3L)
  rhs_mm_num <- matrix(as.numeric(7:12), nrow = 3L, ncol = 2L)
  rhs_cp_num <- matrix(as.numeric(13:18), nrow = 2L, ncol = 3L)
  gx <- GPUArray(x_num, dtype = "f32")
  rhs_mm <- rhs_mm_num
  rhs_cp <- rhs_cp_num
  out_mm <- NULL
  out_cp <- NULL
  out_tcp <- NULL
  on.exit(.mojor_gpu_array_free_all(out_tcp, out_cp, out_mm, gx), add = TRUE)

  env_mm <- environment(.mojor_gpu_matmul_dispatch)
  env_cp <- environment(.mojor_gpu_crossprod_dispatch)
  env_tcp <- environment(.mojor_gpu_tcrossprod_dispatch)
  old_host_rhs_mm <- get(".mojor_gpu_linalg_host_rhs_values", envir = env_mm, inherits = TRUE)
  old_matmul_mm <- get(".mojor_gpu_matmul", envir = env_mm, inherits = TRUE)
  old_host_rhs_cp <- get(".mojor_gpu_linalg_host_rhs_values", envir = env_cp, inherits = TRUE)
  old_matmul_cp <- get(".mojor_gpu_matmul", envir = env_cp, inherits = TRUE)
  old_host_rhs_tcp <- get(".mojor_gpu_linalg_host_rhs_values", envir = env_tcp, inherits = TRUE)
  old_matmul_tcp <- get(".mojor_gpu_matmul", envir = env_tcp, inherits = TRUE)

  host_rhs_calls <- 0L
  mm_calls <- 0L
  host_rhs_fn <- function(y, dtype) {
    host_rhs_calls <<- host_rhs_calls + 1L
    old_host_rhs_mm(y, dtype)
  }
  mm_fn <- function(x, y, transpose_a = FALSE, transpose_b = FALSE, out = NULL) {
    mm_calls <<- mm_calls + 1L
    hx <- mojor_gpu_array_read(x)
    hy <- mojor_gpu_array_read(y)
    if (isTRUE(transpose_a)) {
      hx <- t(hx)
    }
    if (isTRUE(transpose_b)) {
      hy <- t(hy)
    }
    out_arr <- GPUArray(hx %*% hy, dtype = .mojor_gpu_array_dtype(x))
    attr(out_arr, "gpu_fallback") <- "gpu_matmul"
    out_arr
  }
  assign(".mojor_gpu_linalg_host_rhs_values", host_rhs_fn, envir = env_mm)
  assign(".mojor_gpu_matmul", mm_fn, envir = env_mm)
  assign(".mojor_gpu_linalg_host_rhs_values", host_rhs_fn, envir = env_cp)
  assign(".mojor_gpu_matmul", mm_fn, envir = env_cp)
  assign(".mojor_gpu_linalg_host_rhs_values", host_rhs_fn, envir = env_tcp)
  assign(".mojor_gpu_matmul", mm_fn, envir = env_tcp)
  on.exit({
    assign(".mojor_gpu_linalg_host_rhs_values", old_host_rhs_mm, envir = env_mm)
    assign(".mojor_gpu_matmul", old_matmul_mm, envir = env_mm)
    assign(".mojor_gpu_linalg_host_rhs_values", old_host_rhs_cp, envir = env_cp)
    assign(".mojor_gpu_matmul", old_matmul_cp, envir = env_cp)
    assign(".mojor_gpu_linalg_host_rhs_values", old_host_rhs_tcp, envir = env_tcp)
    assign(".mojor_gpu_matmul", old_matmul_tcp, envir = env_tcp)
  }, add = TRUE)

  out_mm <- mm_method(gx, rhs_mm)
  expect_equal(mojor_gpu_array_read(out_mm), x_num %*% rhs_mm_num, tolerance = 1e-5)

  out_cp <- cp_method(gx, rhs_cp)
  expect_equal(mojor_gpu_array_read(out_cp), crossprod(x_num, rhs_cp_num), tolerance = 1e-5)

  out_tcp <- tcp_method(gx, rhs_cp)
  expect_equal(mojor_gpu_array_read(out_tcp), tcrossprod(x_num, rhs_cp_num), tolerance = 1e-5)

  expect_equal(mm_calls, 3L)
  expect_equal(host_rhs_calls, 0L)
})

test_that("mac metal f64 linalg/reduce keep explicit cpu fallback contract", {  skip_if(tolower(Sys.info()[["sysname"]]) != "darwin")
  skip_if_no_gpu_f64()

  x <- matrix(as.double(c(1, 2, 3, 4)), nrow = 2L, ncol = 2L)
  y <- matrix(as.double(c(2, 0, 1, 5)), nrow = 2L, ncol = 2L)
  gx <- GPUArray(x, dtype = "f64", api = "metal")
  gy <- GPUArray(y, dtype = "f64", api = "metal")
  out_mm <- NULL
  out_red <- NULL
  on.exit(.mojor_gpu_array_free_all(out_red, out_mm, gy, gx), add = TRUE)

  mm_env <- environment(.mojor_gpu_matmul_dispatch)
  red_env <- environment(.mojor_gpu_reduce)
  old_cap_mm <- get(".mojor_gpu_f64_matmul_capable", envir = mm_env, inherits = TRUE)
  old_cap_red <- get(".mojor_gpu_f64_reduce_capable", envir = red_env, inherits = TRUE)
  on.exit({
    assign(".mojor_gpu_f64_matmul_capable", old_cap_mm, envir = mm_env)
    assign(".mojor_gpu_f64_reduce_capable", old_cap_red, envir = red_env)
  }, add = TRUE)
  assign(".mojor_gpu_f64_matmul_capable", function(...) TRUE, envir = mm_env)
  assign(".mojor_gpu_f64_reduce_capable", function(...) TRUE, envir = red_env)

  out_mm <- gx %*% gy
  out_red <- gpu_sum(gx)
  expect_identical(attr(out_mm, "gpu_fallback"), "cpu_matmul")
  expect_identical(attr(out_red, "gpu_fallback"), "cpu_reduce")
})

test_that("gpu_slice supports vector and matrix slices via host fallback", {  .skip_if_no_gpu_backend(require_float = TRUE)

  x <- matrix(as.numeric(1:12), nrow = 3L, ncol = 4L)
  gx <- GPUArray(x, dtype = "f32")
  sx <- gpu_slice(gx, starts = c(1L, 2L), ends = c(3L, 4L))
  expect_true(inherits(sx, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(sx), x[, 2:4, drop = FALSE])
  expect_true(attr(sx, "gpu_fallback") %in% c("cpu_slice", "gpu_slice"))

  v <- as.numeric(1:8)
  gv <- GPUArray(v, dtype = "f32")
  sv <- gpu_slice(gv, starts = 2L, ends = 6L)
  expect_equal(mojor_gpu_array_read(sv), v[2:6])
  expect_null(dim(sv))
  expect_true(attr(sv, "gpu_fallback") %in% c("cpu_slice", "gpu_slice"))

  sx_step <- gpu_slice(gx, starts = c(1L, 1L), ends = c(3L, 4L), strides = c(1L, 2L))
  expect_true(inherits(sx_step, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(sx_step), x[, c(1L, 3L), drop = FALSE])
  expect_true(attr(sx_step, "gpu_fallback") %in% c("cpu_slice", "gpu_slice"))

  sv_step <- gpu_slice(gv, starts = 1L, ends = 8L, strides = 2L)
  expect_equal(mojor_gpu_array_read(sv_step), v[seq.int(1L, 8L, by = 2L)])
  expect_null(dim(sv_step))
  expect_true(attr(sv_step, "gpu_fallback") %in% c("cpu_slice", "gpu_slice"))

  .mojor_gpu_array_free_all(sv_step, sx_step, sv, gv, sx, gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("gpu_slice validates bounds, rank, and stride constraints", {  .skip_if_no_gpu_backend(require_float = TRUE)

  gx <- GPUArray(matrix(as.numeric(1:12), nrow = 3L, ncol = 4L), dtype = "f32")
  expect_error(gpu_slice(gx, starts = c(1L, 1L), ends = c(3L, 4L), strides = c(1L, 0L)), "strides must be non-zero")
  expect_error(gpu_slice(gx, starts = 1L, ends = 2L), "starts/ends must match rank")
  expect_error(gpu_slice(gx, starts = c(0L, 1L), ends = c(2L, 2L)), "invalid slice bounds")
  empty_neg <- gpu_slice(gx, starts = c(1L, 1L), ends = c(3L, 4L), strides = c(-1L, 1L))
  expect_equal(dim(empty_neg), c(0L, 4L))
  expect_equal(mojor_gpu_array_read(empty_neg), matrix(numeric(0), nrow = 0L, ncol = 4L))
  expect_true(attr(empty_neg, "gpu_fallback") %in% c("gpu_slice", "cpu_slice"))
  empty_pos <- gpu_slice(gx, starts = c(3L, 1L), ends = c(1L, 4L), strides = c(1L, 1L))
  expect_equal(dim(empty_pos), c(0L, 4L))
  expect_equal(mojor_gpu_array_read(empty_pos), matrix(numeric(0), nrow = 0L, ncol = 4L))
  expect_true(attr(empty_pos, "gpu_fallback") %in% c("gpu_slice", "cpu_slice"))

  gv <- GPUArray(as.numeric(1:8), dtype = "f32")
  empty_vec <- gpu_slice(gv, starts = 6L, ends = 2L, strides = 1L)
  expect_null(dim(empty_vec))
  expect_equal(mojor_gpu_array_read(empty_vec), numeric(0))
  expect_true(attr(empty_vec, "gpu_fallback") %in% c("gpu_slice", "cpu_slice"))

  .mojor_gpu_array_free_all(empty_vec, gv, empty_pos, empty_neg, gx)
})

test_that("gpu_slice supports non-zero forward and reverse strides", {  .skip_if_no_gpu_backend(require_float = TRUE)

  gx <- GPUArray(matrix(as.numeric(1:12), nrow = 3L, ncol = 4L), dtype = "f32")
  s <- gpu_slice(gx, starts = c(1L, 1L), ends = c(3L, 4L), strides = c(1L, 2L))
  expect_equal(mojor_gpu_array_read(s), matrix(c(1, 2, 3, 7, 8, 9), nrow = 3L, ncol = 2L))
  expect_true(attr(s, "gpu_fallback") %in% c("gpu_slice", "cpu_slice"))
  s_rev <- gpu_slice(gx, starts = c(3L, 4L), ends = c(1L, 2L), strides = c(-1L, -2L))
  expect_equal(
    mojor_gpu_array_read(s_rev),
    matrix(c(12, 11, 10, 6, 5, 4), nrow = 3L, ncol = 2L)
  )
  expect_true(attr(s_rev, "gpu_fallback") %in% c("gpu_slice", "cpu_slice"))

  gv <- GPUArray(as.numeric(1:10), dtype = "f32")
  sv <- gpu_slice(gv, starts = 1L, ends = 10L, strides = 3L)
  expect_equal(mojor_gpu_array_read(sv), c(1, 4, 7, 10))
  expect_true(attr(sv, "gpu_fallback") %in% c("gpu_slice", "cpu_slice"))
  sv_rev <- gpu_slice(gv, starts = 10L, ends = 2L, strides = -3L)
  expect_equal(mojor_gpu_array_read(sv_rev), c(10, 7, 4))
  expect_true(attr(sv_rev, "gpu_fallback") %in% c("gpu_slice", "cpu_slice"))

  .mojor_gpu_array_free_all(sv_rev, sv, gv, s_rev, s, gx)
})

test_that("gpu_slice non-unit stride stays GPU-routed when plan cache lookup is unavailable", {  .skip_if_no_gpu_backend(require_float = TRUE)

  x <- matrix(as.numeric(1:12), nrow = 3L, ncol = 4L)
  gx <- GPUArray(x, dtype = "f32")

  gpu_env <- environment(.mojor_gpu_try_gather)
  orig_plan_cached <- get(".mojor_gpu_index_plan_cached", envir = gpu_env, inherits = FALSE)
  assign(".mojor_gpu_index_plan_cached", function(...) NULL, envir = gpu_env)
  on.exit(assign(".mojor_gpu_index_plan_cached", orig_plan_cached, envir = gpu_env), add = TRUE)

  s <- gpu_slice(gx, starts = c(1L, 1L), ends = c(3L, 4L), strides = c(1L, 2L))
  expect_equal(mojor_gpu_array_read(s), x[, c(1L, 3L), drop = FALSE])
  expect_equal(attr(s, "gpu_fallback"), "gpu_slice")

  .mojor_gpu_array_free_all(s, gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

