# Split from test_gpu_array.R (chunk 01).
# Shared utilities are in helper-gpu-array.R.

.mojor_skip_gpu_runtime_file_unless_opt_in()

test_that("GPU route tagging keeps compatibility attrs and deterministic reason payload", {
  obj <- structure(list(), class = "mojor_gpu_array")

  tagged <- .mojor_gpu_route_tag(
    obj,
    "cpu_matmul",
    reason = "probe unavailable",
    reason_code = "probe_unavailable"
  )
  expect_identical(attr(tagged, "gpu_route"), "cpu_matmul")
  expect_identical(attr(tagged, "gpu_fallback"), "cpu_matmul")
  expect_identical(attr(tagged, "gpu_route_reason"), "probe unavailable")
  expect_identical(attr(tagged, "gpu_fallback_reason"), "probe unavailable")
  expect_identical(attr(tagged, "gpu_route_reason_code"), "probe_unavailable")
  expect_identical(attr(tagged, "gpu_fallback_reason_code"), "probe_unavailable")
  expect_identical(.mojor_gpu_route_reason_value(tagged), "probe unavailable")
  expect_identical(.mojor_gpu_route_reason_code_value(tagged), "probe_unavailable")

  tagged2 <- .mojor_gpu_route_tag(tagged, "gpu_matmul")
  expect_identical(attr(tagged2, "gpu_route"), "gpu_matmul")
  expect_identical(attr(tagged2, "gpu_fallback"), "gpu_matmul")
  expect_null(attr(tagged2, "gpu_route_reason", exact = TRUE))
  expect_null(attr(tagged2, "gpu_fallback_reason", exact = TRUE))
  expect_null(attr(tagged2, "gpu_route_reason_code", exact = TRUE))
  expect_null(attr(tagged2, "gpu_fallback_reason_code", exact = TRUE))
  expect_null(.mojor_gpu_route_reason_value(tagged2))
  expect_null(.mojor_gpu_route_reason_code_value(tagged2))
})

test_that("GPU route tagging can reject cpu fallbacks by option", {
  .mojor_test_local_options(gpu_reject_fallback = TRUE)
  obj <- structure(list(), class = "mojor_gpu_array")

  expect_error(
    .mojor_gpu_route_tag(
      obj,
      "cpu_matmul",
      reason = "probe unavailable",
      reason_code = "probe_unavailable"
    ),
    "mojor gpu fallback rejected for route cpu_matmul"
  )

  tagged <- .mojor_gpu_route_tag(obj, "gpu_matmul")
  expect_identical(attr(tagged, "gpu_fallback"), "gpu_matmul")
})

test_that(".mojor_call_bridge retries once on invalid GPU context and invalidates cache", {
  cap_env <- environment(.mojor_call_bridge)
  old_dispatch <- get(".mojor_runtime_dispatch_call", envir = cap_env, inherits = TRUE)
  old_pkg_for_symbol <- get(".mojor_bridge_pkg_for_symbol", envir = cap_env, inherits = TRUE)
  old_ctx <- .mojor_state$gpu_ctx
  old_epoch <- .mojor_state$gpu_ctx_epoch
  old_cache <- .mojor_state$gpu_capability_cache
  on.exit({
    assign(".mojor_runtime_dispatch_call", old_dispatch, envir = cap_env)
    assign(".mojor_bridge_pkg_for_symbol", old_pkg_for_symbol, envir = cap_env)
    .mojor_state$gpu_ctx <- old_ctx
    .mojor_state$gpu_ctx_epoch <- old_epoch
    .mojor_state$gpu_capability_cache <- old_cache
  }, add = TRUE)

  calls <- 0L
  assign(".mojor_bridge_pkg_for_symbol", function(symbol) "mojor", envir = cap_env)
  assign(
    ".mojor_runtime_dispatch_call",
    function(symbol, pkg, deterministic = TRUE, ...) {
      calls <<- calls + 1L
      if (calls == 1L) {
        stop("mojor_gpu_ctx: invalid context", call. = FALSE)
      }
      list(ok = TRUE, symbol = symbol, pkg = pkg)
    },
    envir = cap_env
  )

  .mojor_state$gpu_ctx <- structure(list(token = 1L), class = "mojor_gpu_ctx")
  .mojor_state$gpu_ctx_epoch <- 9L
  .mojor_state$gpu_capability_cache <- list(f64 = TRUE)

  out <- .mojor_call_bridge("mojor_gpu_buf_f32_alloc", 4L)
  expect_equal(calls, 2L)
  expect_true(is.list(out))
  expect_identical(out$symbol, "mojor_gpu_buf_f32_alloc")
  expect_null(.mojor_state$gpu_ctx)
  expect_identical(.mojor_state$gpu_capability_cache, list())
  expect_gte(.mojor_state$gpu_ctx_epoch, 10L)
})

test_that(".mojor_call_bridge retries invalid GPU context at most once", {
  cap_env <- environment(.mojor_call_bridge)
  old_dispatch <- get(".mojor_runtime_dispatch_call", envir = cap_env, inherits = TRUE)
  old_pkg_for_symbol <- get(".mojor_bridge_pkg_for_symbol", envir = cap_env, inherits = TRUE)
  old_ctx <- .mojor_state$gpu_ctx
  old_epoch <- .mojor_state$gpu_ctx_epoch
  old_cache <- .mojor_state$gpu_capability_cache
  on.exit({
    assign(".mojor_runtime_dispatch_call", old_dispatch, envir = cap_env)
    assign(".mojor_bridge_pkg_for_symbol", old_pkg_for_symbol, envir = cap_env)
    .mojor_state$gpu_ctx <- old_ctx
    .mojor_state$gpu_ctx_epoch <- old_epoch
    .mojor_state$gpu_capability_cache <- old_cache
  }, add = TRUE)

  calls <- 0L
  assign(".mojor_bridge_pkg_for_symbol", function(symbol) "mojor", envir = cap_env)
  assign(
    ".mojor_runtime_dispatch_call",
    function(symbol, pkg, deterministic = TRUE, ...) {
      calls <<- calls + 1L
      stop("mojor_gpu_ctx: invalid context", call. = FALSE)
    },
    envir = cap_env
  )

  .mojor_state$gpu_ctx <- structure(list(token = 2L), class = "mojor_gpu_ctx")
  .mojor_state$gpu_ctx_epoch <- 3L
  .mojor_state$gpu_capability_cache <- list(i32 = TRUE)

  expect_error(
    .mojor_call_bridge("mojor_gpu_buf_f32_alloc", 4L),
    "mojor_gpu_ctx: invalid context"
  )
  expect_equal(calls, 2L)
  expect_null(.mojor_state$gpu_ctx)
  expect_identical(.mojor_state$gpu_capability_cache, list())
})

test_that(".mojor_call_bridge does not retry mojor_gpu_ctx_create on invalid context errors", {
  cap_env <- environment(.mojor_call_bridge)
  old_dispatch <- get(".mojor_runtime_dispatch_call", envir = cap_env, inherits = TRUE)
  old_pkg_for_symbol <- get(".mojor_bridge_pkg_for_symbol", envir = cap_env, inherits = TRUE)
  on.exit({
    assign(".mojor_runtime_dispatch_call", old_dispatch, envir = cap_env)
    assign(".mojor_bridge_pkg_for_symbol", old_pkg_for_symbol, envir = cap_env)
  }, add = TRUE)

  calls <- 0L
  assign(".mojor_bridge_pkg_for_symbol", function(symbol) "mojor", envir = cap_env)
  assign(
    ".mojor_runtime_dispatch_call",
    function(symbol, pkg, deterministic = TRUE, ...) {
      calls <<- calls + 1L
      stop("mojor_gpu_ctx: invalid context", call. = FALSE)
    },
    envir = cap_env
  )

  expect_error(
    .mojor_call_bridge("mojor_gpu_ctx_create"),
    "mojor_gpu_ctx: invalid context"
  )
  expect_equal(calls, 1L)
})

test_that("mojor_gpu_array wrappers work", {  .skip_if_no_gpu_backend(require_float = TRUE)

  x <- runif(1024)
  buf <- mojor_gpu_array(x, dtype = "f32")
  expect_true(inherits(buf, "mojor_gpu_array"))
  expect_equal(buf$n, length(x))

  mat <- matrix(runif(12), nrow = 3)
  bufm <- mojor_gpu_array(mat, dtype = "f32")
  expect_equal(bufm$dim, dim(mat))
  expect_equal(bufm$strides, c(1L, 3L))
  hostm <- mojor_gpu_array_read(bufm)
  expect_true(is.matrix(hostm))
  expect_equal(dim(hostm), dim(mat))
  mojor_gpu_array_free(bufm)

  buf2 <- mojor_gpu_chain_array(buf, iters = 2L, scale = 1.1, bias = 0.05)
  expect_true(inherits(buf2, "mojor_gpu_array"))
  expect_length(mojor_gpu_array_read(buf2), length(x))

  total <- mojor_gpu_sum(buf, iters = 2L, scale = 1.1, bias = 0.05, post_scale = 0.9, post_bias = -0.02, post_iters = 1L)
  expect_true(is.numeric(total))

  .mojor_gpu_array_free_all(buf2, buf)
  gc()
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray constructor and accessors work", {  .skip_if_no_gpu_backend(require_float = TRUE)

  x <- float::fl(runif(64))
  buf <- GPUArray(x)
  expect_true(inherits(buf, "GPUArray"))
  expect_equal(length(buf), length(x))
  if (!is.null(dim(buf))) {
    expect_true(is.integer(dim(buf)))
  }

  mat <- matrix(runif(12), nrow = 3)
  bufm <- GPUArray(mat, dtype = "f32")
  expect_equal(dim(bufm), dim(mat))
  expect_equal(dimnames(bufm), dimnames(mat))
  hostm <- as.array(bufm)
  expect_true(is.matrix(hostm))
  expect_equal(dim(hostm), dim(mat))

  buf2 <- as.GPUArray(buf)
  expect_true(inherits(buf2, "GPUArray"))

  buf3 <- gpu_array(n = 16, dtype = "f32")
  expect_true(inherits(buf3, "GPUArray"))

  buf4 <- gpu_zeros(dim = c(3L, 4L), dtype = "f32")
  expect_true(inherits(buf4, "GPUArray"))
  host4 <- mojor_gpu_array_read(buf4)
  expect_equal(dim(host4), c(3L, 4L))
  expect_true(max(abs(host4)) == 0)

  buf5 <- gpu_ones(dim = c(2L, 3L), dtype = "f32")
  expect_true(inherits(buf5, "GPUArray"))
  host5 <- mojor_gpu_array_read(buf5)
  expect_equal(dim(host5), c(2L, 3L))
  expect_true(max(abs(host5 - 1)) == 0)

  buf6 <- gpu_full(3.5, dim = c(2L, 2L), dtype = "f32")
  expect_true(inherits(buf6, "GPUArray"))
  host6 <- mojor_gpu_array_read(buf6)
  expect_equal(dim(host6), c(2L, 2L))
  expect_true(max(abs(host6 - 3.5)) < 1e-5)

  buf7 <- gpu_empty(dim = c(2L, 3L), dtype = "f32")
  expect_true(inherits(buf7, "GPUArray"))
  expect_equal(dim(buf7), c(2L, 3L))
  vals7 <- matrix(as.numeric(101:106), nrow = 2L, ncol = 3L)
  mojor_gpu_array_write(buf7, vals7)
  expect_equal(mojor_gpu_array_read(buf7), vals7)

  buf8 <- gpu_empty(n = 5L, dtype = "f32")
  expect_true(inherits(buf8, "GPUArray"))
  expect_equal(.mojor_gpu_array_length(buf8), 5L)
  expect_null(dim(buf8))
  vals8 <- c(-1, -2, -3, -4, -5)
  mojor_gpu_array_write(buf8, vals8)
  expect_equal(mojor_gpu_array_read(buf8), vals8)

  set.seed(123)
  buf9 <- gpu_rand(dim = c(2L, 3L), min = -0.25, max = 0.75, dtype = "f32")
  expect_true(inherits(buf9, "GPUArray"))
  host9 <- mojor_gpu_array_read(buf9)
  expect_equal(dim(host9), c(2L, 3L))
  expect_true(all(host9 >= -0.25))
  expect_true(all(host9 <= 0.75))

  set.seed(456)
  buf10 <- gpu_rand(n = 5L, min = -2, max = -1, dtype = "f32")
  expect_true(inherits(buf10, "GPUArray"))
  host10 <- mojor_gpu_array_read(buf10)
  expect_length(host10, 5L)
  expect_true(all(host10 >= -2))
  expect_true(all(host10 <= -1))

  set.seed(789)
  exp11 <- array(stats::rnorm(6L, mean = 1.25, sd = 0.5), dim = c(2L, 3L))
  set.seed(789)
  buf11 <- gpu_randn(dim = c(2L, 3L), mean = 1.25, sd = 0.5, dtype = "f32")
  expect_true(inherits(buf11, "GPUArray"))
  host11 <- mojor_gpu_array_read(buf11)
  expect_equal(dim(host11), c(2L, 3L))
  expect_equal(host11, exp11, tolerance = 1e-5)

  set.seed(790)
  exp12 <- stats::rnorm(5L, mean = -0.5, sd = 2.0)
  set.seed(790)
  buf12 <- gpu_randn(n = 5L, mean = -0.5, sd = 2.0, dtype = "f32")
  expect_true(inherits(buf12, "GPUArray"))
  host12 <- mojor_gpu_array_read(buf12)
  expect_length(host12, 5L)
  expect_equal(host12, exp12, tolerance = 1e-5)

  set.seed(791)
  exp13 <- array(sample.int(7L, size = 6L, replace = TRUE) + 3L - 1L, dim = c(2L, 3L))
  set.seed(791)
  buf13 <- gpu_randi(dim = c(2L, 3L), low = 3L, high = 10L, dtype = "f32")
  expect_true(inherits(buf13, "GPUArray"))
  host13 <- mojor_gpu_array_read(buf13)
  expect_equal(dim(host13), c(2L, 3L))
  expect_equal(host13, exp13, tolerance = 1e-6)
  expect_true(all(host13 >= 3))
  expect_true(all(host13 < 10))

  set.seed(792)
  exp14 <- sample.int(4L, size = 5L, replace = TRUE) + (-2L) - 1L
  set.seed(792)
  buf14 <- gpu_randi(n = 5L, low = -2L, high = 2L, dtype = "f32")
  expect_true(inherits(buf14, "GPUArray"))
  host14 <- mojor_gpu_array_read(buf14)
  expect_length(host14, 5L)
  expect_equal(host14, exp14, tolerance = 1e-6)
  expect_true(all(host14 >= -2))
  expect_true(all(host14 < 2))

  expect_error(gpu_empty(), "provide dim or n")
  expect_error(gpu_empty(dim = integer(0)), "dim must be non-empty")
  expect_error(gpu_empty(n = 0L), "n must be positive")
  expect_error(gpu_rand(), "provide dim or n")
  expect_error(gpu_rand(dim = integer(0)), "dim must be non-empty")
  expect_error(gpu_rand(n = 0L), "n must be positive")
  expect_error(gpu_rand(n = 4L, min = NA_real_), "min must be a non-NA numeric scalar")
  expect_error(gpu_rand(n = 4L, max = NA_real_), "max must be a non-NA numeric scalar")
  expect_error(gpu_rand(n = 4L, min = 1, max = 0), "max must be >= min")
  expect_error(gpu_randn(), "provide dim or n")
  expect_error(gpu_randn(dim = integer(0)), "dim must be non-empty")
  expect_error(gpu_randn(n = 0L), "n must be positive")
  expect_error(gpu_randn(n = 4L, mean = NA_real_), "mean must be a non-NA numeric scalar")
  expect_error(gpu_randn(n = 4L, sd = NA_real_), "sd must be a non-NA numeric scalar")
  expect_error(gpu_randn(n = 4L, sd = -1), "sd must be >= 0")
  expect_error(gpu_randi(), "provide dim or n")
  expect_error(gpu_randi(dim = integer(0)), "dim must be non-empty")
  expect_error(gpu_randi(n = 0L), "n must be positive")
  expect_error(gpu_randi(n = 4L, low = NA_real_), "low must be a finite integer scalar")
  expect_error(gpu_randi(n = 4L, high = NA_real_), "high must be a finite integer scalar")
  expect_error(gpu_randi(n = 4L, low = 1.5, high = 4), "low must be a finite integer scalar")
  expect_error(gpu_randi(n = 4L, low = 1, high = 1), "high must be > low")
  expect_error(gpu_randi(n = 4L, low = 2, high = 1), "high must be > low")

  .mojor_gpu_array_free_all(buf14, buf13, buf12, buf11, buf10, buf9, buf8, buf7, buf3, buf4, buf5, buf6, bufm, buf)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray i32 dtype supports allocation, write/read, and integer random constructors", {  .skip_if_no_gpu_backend()

  gi0 <- gpu_zeros(dim = c(2L, 3L), dtype = "i32")
  expect_equal(.mojor_gpu_array_dtype(gi0), "i32")
  expect_true(inherits(.mojor_gpu_array_handle(gi0), "mojor_gpu_buf_i32"))
  host0 <- mojor_gpu_array_read(gi0)
  expect_true(is.integer(host0))
  expect_equal(host0, matrix(0L, nrow = 2L, ncol = 3L))

  gi1 <- gpu_full(5, n = 4L, dtype = "i32")
  host1 <- mojor_gpu_array_read(gi1)
  expect_true(is.integer(host1))
  expect_equal(host1, rep.int(5L, 4L))

  gi2 <- mojor_gpu_array(1:5, dtype = "i32")
  mojor_gpu_array_write(gi2, c(10.9, 11.1, -2.2, 0.0, 3.8))
  host2 <- mojor_gpu_array_read(gi2)
  expect_true(is.integer(host2))
  expect_equal(host2, as.integer(c(10.9, 11.1, -2.2, 0.0, 3.8)))

  set.seed(820)
  ri <- gpu_randi(n = 256L, low = -3L, high = 4L, dtype = "i32")
  host_ri <- mojor_gpu_array_read(ri)
  expect_true(is.integer(host_ri))
  expect_true(all(host_ri >= -3L))
  expect_true(all(host_ri < 4L))

  src <- gpu_ones(dim = c(3L, 2L), dtype = "i32")
  set.seed(821)
  ri_like <- gpu_randi_like(src, low = 20L, high = 24L)
  host_like <- mojor_gpu_array_read(ri_like)
  expect_equal(.mojor_gpu_array_dtype(ri_like), "i32")
  expect_true(is.integer(host_like))
  expect_equal(dim(host_like), c(3L, 2L))
  expect_true(all(host_like >= 20L))
  expect_true(all(host_like < 24L))

  ia <- gpu_ones(dim = c(2L, 3L), dtype = "i32")
  ib <- gpu_ones(dim = c(3L, 2L), dtype = "i32")
  iout <- gpu_zeros(dim = c(2L, 2L), dtype = "i32")

  rs <- gpu_reduce(src, "sum")
  expect_equal(.mojor_gpu_array_dtype(rs), "i32")
  expect_equal(as.integer(mojor_gpu_array_read(rs)), 6L)
  expect_true(attr(rs, "gpu_fallback") %in% c("gpu_reduce", "cpu_reduce"))

  rs2 <- gpu_sum(src)
  expect_equal(.mojor_gpu_array_dtype(rs2), "i32")
  expect_equal(as.integer(mojor_gpu_array_read(rs2)), 6L)
  expect_true(attr(rs2, "gpu_fallback") %in% c("gpu_reduce", "cpu_reduce"))

  mm <- gpu_matmul(ia, ib)
  expect_equal(.mojor_gpu_array_dtype(mm), "i32")
  expect_equal(mojor_gpu_array_read(mm), matrix(3L, nrow = 2L, ncol = 2L))
  expect_true(attr(mm, "gpu_fallback") %in% c("gpu_matmul", "cpu_matmul"))

  mm_into <- gpu_matmul_into(iout, ia, ib)
  expect_identical(mm_into, iout)
  expect_equal(.mojor_gpu_array_dtype(mm_into), "i32")
  expect_equal(mojor_gpu_array_read(mm_into), matrix(3L, nrow = 2L, ncol = 2L))
  expect_true(attr(mm_into, "gpu_fallback") %in% c("gpu_matmul", "cpu_matmul"))

  mm_op <- ia %*% ib
  expect_equal(.mojor_gpu_array_dtype(mm_op), "i32")
  expect_equal(mojor_gpu_array_read(mm_op), matrix(3L, nrow = 2L, ncol = 2L))
  expect_true(attr(mm_op, "gpu_fallback") %in% c("gpu_matmul", "cpu_matmul"))

  cp <- crossprod.mojor_gpu_array(ia)
  expect_equal(.mojor_gpu_array_dtype(cp), "i32")
  expect_equal(mojor_gpu_array_read(cp), matrix(2L, nrow = 3L, ncol = 3L))
  expect_true(attr(cp, "gpu_fallback") %in% c("gpu_crossprod", "cpu_crossprod"))

  tcp <- tcrossprod.mojor_gpu_array(ia)
  expect_equal(.mojor_gpu_array_dtype(tcp), "i32")
  expect_equal(mojor_gpu_array_read(tcp), matrix(3L, nrow = 2L, ncol = 2L))
  expect_true(attr(tcp, "gpu_fallback") %in% c("gpu_tcrossprod", "cpu_tcrossprod"))

  .mojor_gpu_array_free_all(tcp, cp, mm_op, mm, rs2, rs, iout, ib, ia, ri_like, src, ri, gi2, gi1, gi0)
})

test_that("GPUArray host-rhs linalg promotes fractional operands instead of forcing i32 rejection", {  .skip_if_no_gpu_backend()

  lhs_mm <- GPUArray(matrix(as.integer(c(1L, 2L, 3L)), nrow = 1L, ncol = 3L), dtype = "i32")
  lhs_cp <- GPUArray(matrix(as.integer(c(1L, 2L, 3L)), nrow = 3L, ncol = 1L), dtype = "i32")
  lhs_tcp <- GPUArray(matrix(as.integer(c(1L, 2L, 3L)), nrow = 1L, ncol = 3L), dtype = "i32")

  bad_mm <- matrix(c(1, 1.5, 1), nrow = 3L, ncol = 1L)
  bad_cp <- matrix(c(1, 1.5, 1), nrow = 3L, ncol = 1L)
  bad_tcp <- matrix(c(1, 1.5, 1), nrow = 1L, ncol = 3L)

  out_mm <- lhs_mm %*% bad_mm
  expect_equal(.mojor_gpu_array_dtype(out_mm), "f64")
  expect_equal(mojor_gpu_array_read(out_mm), matrix(as.numeric(c(1L, 2L, 3L)), nrow = 1L, ncol = 3L) %*% bad_mm)
  expect_true(attr(out_mm, "gpu_fallback") %in% c("gpu_matmul", "cpu_matmul"))

  out_cp <- crossprod.mojor_gpu_array(lhs_cp, bad_cp)
  expect_equal(.mojor_gpu_array_dtype(out_cp), "f64")
  expect_equal(
    mojor_gpu_array_read(out_cp),
    crossprod(matrix(as.numeric(c(1L, 2L, 3L)), nrow = 3L, ncol = 1L), bad_cp)
  )
  expect_true(attr(out_cp, "gpu_fallback") %in% c("gpu_crossprod", "cpu_crossprod"))

  out_tcp <- tcrossprod.mojor_gpu_array(lhs_tcp, bad_tcp)
  expect_equal(.mojor_gpu_array_dtype(out_tcp), "f64")
  expect_equal(
    mojor_gpu_array_read(out_tcp),
    tcrossprod(matrix(as.numeric(c(1L, 2L, 3L)), nrow = 1L, ncol = 3L), bad_tcp)
  )
  expect_true(attr(out_tcp, "gpu_fallback") %in% c("gpu_tcrossprod", "cpu_tcrossprod"))

  int_like_mm <- matrix(c(1, 2, 1), nrow = 3L, ncol = 1L)
  out_int_like <- lhs_mm %*% int_like_mm
  expect_equal(.mojor_gpu_array_dtype(out_int_like), "i32")
  expect_equal(as.integer(mojor_gpu_array_read(out_int_like)), as.integer(c(8L)))

  .mojor_gpu_array_free_all(out_int_like, out_tcp, out_cp, out_mm, lhs_tcp, lhs_cp, lhs_mm)
})

test_that("GPUArray host-lhs matmul promotes mixed dtype without manual casts", {  .skip_if_no_gpu_backend()

  rhs_gpu <- GPUArray(matrix(as.integer(c(1L, 1L, 1L)), nrow = 3L, ncol = 1L), dtype = "i32")
  lhs_frac <- matrix(c(1, 1.5, 1), nrow = 1L, ncol = 3L)
  out_frac <- lhs_frac %*% rhs_gpu
  expect_equal(.mojor_gpu_array_dtype(out_frac), "f64")
  expect_equal(
    mojor_gpu_array_read(out_frac),
    lhs_frac %*% matrix(as.numeric(c(1, 1, 1)), nrow = 3L, ncol = 1L)
  )
  expect_true(attr(out_frac, "gpu_fallback") %in% c("gpu_matmul", "cpu_matmul"))

  lhs_int_like <- matrix(c(1, 2, 1), nrow = 1L, ncol = 3L)
  out_int_like <- lhs_int_like %*% rhs_gpu
  expect_equal(.mojor_gpu_array_dtype(out_int_like), "i32")
  expect_equal(as.integer(mojor_gpu_array_read(out_int_like)), as.integer(4L))
  expect_true(attr(out_int_like, "gpu_fallback") %in% c("gpu_matmul", "cpu_matmul"))

  .mojor_gpu_array_free_all(out_int_like, out_frac, rhs_gpu)
})

test_that("GPUArray i32 linalg/reduce overflow edge is deterministic at int32 max + 1", {  .skip_if_no_gpu_backend()

  count0 <- mojor_gpu_buf_f32_live_count()
  imax <- .Machine$integer.max
  wrap_i32 <- function(x) {
    as.integer(((as.double(x) + 2147483648) %% 4294967296) - 2147483648)
  }
  wrap_val <- wrap_i32(as.double(imax) + 1)

  expect_overflow_value <- function(route, value, wrapped, allowed_routes) {
    expect_true(route %in% allowed_routes)
    if (startsWith(route, "gpu_")) {
      expect_equal(as.integer(value), as.integer(wrapped))
    } else {
      expect_true(all(is.na(value)))
    }
  }

  lhs_mm <- GPUArray(matrix(c(imax, 1L), nrow = 1L, ncol = 2L), dtype = "i32")
  rhs_mm <- GPUArray(matrix(c(1L, 1L), nrow = 2L, ncol = 1L), dtype = "i32")

  out_mm <- suppressWarnings(gpu_matmul(lhs_mm, rhs_mm))
  out_op <- suppressWarnings(lhs_mm %*% rhs_mm)
  route_mm <- attr(out_mm, "gpu_fallback")
  route_op <- attr(out_op, "gpu_fallback")
  expect_overflow_value(route_mm, mojor_gpu_array_read(out_mm)[1L, 1L], wrap_val, c("gpu_matmul", "cpu_matmul"))
  expect_overflow_value(route_op, mojor_gpu_array_read(out_op)[1L, 1L], wrap_val, c("gpu_matmul", "cpu_matmul"))

  lhs_cp <- GPUArray(matrix(c(imax, 1L), nrow = 2L, ncol = 1L), dtype = "i32")
  rhs_cp <- GPUArray(matrix(c(1L, 1L), nrow = 2L, ncol = 1L), dtype = "i32")
  out_cp <- suppressWarnings(crossprod.mojor_gpu_array(lhs_cp, rhs_cp))
  route_cp <- attr(out_cp, "gpu_fallback")
  cp_host <- mojor_gpu_array_read(out_cp)
  expect_equal(dim(cp_host), c(1L, 1L))
  expect_overflow_value(route_cp, cp_host[1L, 1L], wrap_val, c("gpu_crossprod", "cpu_crossprod"))

  lhs_tcp <- GPUArray(matrix(c(imax, 1L), nrow = 1L, ncol = 2L), dtype = "i32")
  rhs_tcp <- GPUArray(matrix(c(1L, 1L), nrow = 1L, ncol = 2L), dtype = "i32")
  out_tcp <- suppressWarnings(tcrossprod.mojor_gpu_array(lhs_tcp, rhs_tcp))
  route_tcp <- attr(out_tcp, "gpu_fallback")
  tcp_host <- mojor_gpu_array_read(out_tcp)
  expect_equal(dim(tcp_host), c(1L, 1L))
  expect_overflow_value(route_tcp, tcp_host[1L, 1L], wrap_val, c("gpu_tcrossprod", "cpu_tcrossprod"))

  red_vec <- GPUArray(c(imax, 1L), dtype = "i32")
  red_scalar <- suppressWarnings(gpu_reduce(red_vec, "sum", dims = NULL, keepdims = FALSE))
  red_dims <- suppressWarnings(gpu_reduce(red_vec, "sum", dims = 1L, keepdims = FALSE))
  route_red_scalar <- attr(red_scalar, "gpu_fallback")
  route_red_dims <- attr(red_dims, "gpu_fallback")
  expect_overflow_value(route_red_scalar, mojor_gpu_array_read(red_scalar)[1L], wrap_val, c("gpu_reduce", "cpu_reduce"))
  expect_overflow_value(route_red_dims, mojor_gpu_array_read(red_dims)[1L], wrap_val, c("gpu_reduce", "cpu_reduce"))

  .mojor_gpu_array_free_all(red_dims, red_scalar, red_vec, out_tcp, rhs_tcp, lhs_tcp, out_cp, rhs_cp, lhs_cp, out_op, out_mm, rhs_mm, lhs_mm)

  expect_equal(mojor_gpu_buf_f32_live_count(), count0)
})

test_that("GPUArray i32 overflow lock-down covers matmul_into host-rhs and multi-term sum", {  .skip_if_no_gpu_backend()

  count0 <- mojor_gpu_buf_f32_live_count()
  imax <- .Machine$integer.max
  wrap_val <- as.integer(-2147483647L)

  expect_overflow_value <- function(route, value, wrapped) {
    expect_true(route %in% c("gpu_matmul", "cpu_matmul", "gpu_crossprod", "cpu_crossprod",
      "gpu_tcrossprod", "cpu_tcrossprod", "gpu_reduce", "cpu_reduce"))
    if (startsWith(route, "gpu_")) {
      expect_equal(as.integer(value), as.integer(wrapped))
    } else {
      expect_true(all(is.na(value)))
    }
  }

  lhs <- GPUArray(matrix(c(imax, 1L, 1L), nrow = 1L, ncol = 3L), dtype = "i32")
  rhs_gpu <- GPUArray(matrix(c(1L, 1L, 1L), nrow = 3L, ncol = 1L), dtype = "i32")
  out_into <- gpu_zeros(dim = c(1L, 1L), dtype = "i32")
  out_into_res <- suppressWarnings(gpu_matmul_into(out_into, lhs, rhs_gpu))
  expect_identical(out_into_res, out_into)
  route_out_into <- attr(out_into_res, "gpu_fallback")
  expect_true(route_out_into %in% c("gpu_matmul", "cpu_matmul"))
  expect_overflow_value(route_out_into, mojor_gpu_array_read(out_into_res)[1L, 1L], wrap_val)

  rhs_host <- matrix(as.integer(c(1L, 1L, 1L)), nrow = 3L, ncol = 1L)
  out_host_mm <- suppressWarnings(lhs %*% rhs_host)
  route_host_mm <- attr(out_host_mm, "gpu_fallback")
  expect_true(route_host_mm %in% c("gpu_matmul", "cpu_matmul"))
  expect_overflow_value(route_host_mm, mojor_gpu_array_read(out_host_mm)[1L, 1L], wrap_val)

  lhs_cp <- GPUArray(matrix(c(imax, 1L, 1L), nrow = 3L, ncol = 1L), dtype = "i32")
  rhs_cp_host <- matrix(as.integer(c(1L, 1L, 1L)), nrow = 3L, ncol = 1L)
  out_cp <- suppressWarnings(crossprod.mojor_gpu_array(lhs_cp, rhs_cp_host))
  route_cp <- attr(out_cp, "gpu_fallback")
  expect_true(route_cp %in% c("gpu_crossprod", "cpu_crossprod"))
  expect_overflow_value(route_cp, mojor_gpu_array_read(out_cp)[1L, 1L], wrap_val)

  lhs_tcp <- GPUArray(matrix(c(imax, 1L, 1L), nrow = 1L, ncol = 3L), dtype = "i32")
  rhs_tcp_host <- matrix(as.integer(c(1L, 1L, 1L)), nrow = 1L, ncol = 3L)
  out_tcp <- suppressWarnings(tcrossprod.mojor_gpu_array(lhs_tcp, rhs_tcp_host))
  route_tcp <- attr(out_tcp, "gpu_fallback")
  expect_true(route_tcp %in% c("gpu_tcrossprod", "cpu_tcrossprod"))
  expect_overflow_value(route_tcp, mojor_gpu_array_read(out_tcp)[1L, 1L], wrap_val)

  red_vec <- GPUArray(c(imax, 1L, 1L), dtype = "i32")
  red_scalar <- suppressWarnings(gpu_reduce(red_vec, "sum", dims = NULL, keepdims = FALSE))
  route_red_scalar <- attr(red_scalar, "gpu_fallback")
  expect_true(route_red_scalar %in% c("gpu_reduce", "cpu_reduce"))
  expect_overflow_value(route_red_scalar, mojor_gpu_array_read(red_scalar)[1L], wrap_val)

  red_mat <- GPUArray(
    matrix(
      c(imax, 1L, 1L, imax, 1L, 1L),
      nrow = 2L,
      ncol = 3L,
      byrow = TRUE
    ),
    dtype = "i32"
  )
  red_rows <- suppressWarnings(gpu_reduce(red_mat, "sum", dims = 2L, keepdims = FALSE))
  route_red_rows <- attr(red_rows, "gpu_fallback")
  expect_true(route_red_rows %in% c("gpu_reduce", "cpu_reduce"))
  expect_overflow_value(route_red_rows, mojor_gpu_array_read(red_rows), c(wrap_val, wrap_val))

  .mojor_gpu_array_free_all(red_rows, red_mat, red_scalar, red_vec, out_tcp, lhs_tcp, out_cp, lhs_cp, out_host_mm, out_into, rhs_gpu, lhs)

  expect_equal(mojor_gpu_buf_f32_live_count(), count0)
})

test_that("GPUArray i32 overflow lock-down covers longer accumulation and keepdims reductions", {  .skip_if_no_gpu_backend()

  count0 <- mojor_gpu_buf_f32_live_count()
  imax <- .Machine$integer.max
  wrap_i32 <- function(x) {
    as.integer(((as.double(x) + 2147483648) %% 4294967296) - 2147483648)
  }

  expect_overflow_value <- function(route, value, wrapped, allowed_routes) {
    expect_true(route %in% allowed_routes)
    if (startsWith(route, "gpu_")) {
      expect_equal(as.integer(value), as.integer(wrapped))
    } else {
      expect_true(all(is.na(value)))
    }
  }

  wrap_plus_3 <- wrap_i32(as.double(imax) + 3)
  wrap_plus_1 <- wrap_i32(as.double(imax) + 1)

  lhs_mm <- GPUArray(matrix(c(imax, 1L, 1L, 1L), nrow = 1L, ncol = 4L), dtype = "i32")
  rhs_mm <- GPUArray(matrix(as.integer(c(1L, 1L, 1L, 1L)), nrow = 4L, ncol = 1L), dtype = "i32")
  out_mm <- suppressWarnings(gpu_matmul(lhs_mm, rhs_mm))
  route_mm <- attr(out_mm, "gpu_fallback")
  expect_overflow_value(route_mm, mojor_gpu_array_read(out_mm)[1L, 1L], wrap_plus_3, c("gpu_matmul", "cpu_matmul"))

  lhs_cp <- GPUArray(matrix(c(imax, 1L, 1L, 1L), nrow = 4L, ncol = 1L), dtype = "i32")
  rhs_cp_host <- matrix(as.integer(c(1L, 1L, 1L, 1L)), nrow = 4L, ncol = 1L)
  out_cp <- suppressWarnings(crossprod.mojor_gpu_array(lhs_cp, rhs_cp_host))
  route_cp <- attr(out_cp, "gpu_fallback")
  expect_overflow_value(route_cp, mojor_gpu_array_read(out_cp)[1L, 1L], wrap_plus_3, c("gpu_crossprod", "cpu_crossprod"))

  lhs_tcp <- GPUArray(matrix(c(imax, 1L, 1L, 1L), nrow = 1L, ncol = 4L), dtype = "i32")
  rhs_tcp_host <- matrix(as.integer(c(1L, 1L, 1L, 1L)), nrow = 1L, ncol = 4L)
  out_tcp <- suppressWarnings(tcrossprod.mojor_gpu_array(lhs_tcp, rhs_tcp_host))
  route_tcp <- attr(out_tcp, "gpu_fallback")
  expect_overflow_value(route_tcp, mojor_gpu_array_read(out_tcp)[1L, 1L], wrap_plus_3, c("gpu_tcrossprod", "cpu_tcrossprod"))

  red_rows_src <- GPUArray(
    matrix(
      c(imax, 1L, 1L, 1L, imax, 1L, 1L, 1L),
      nrow = 2L,
      ncol = 4L,
      byrow = TRUE
    ),
    dtype = "i32"
  )
  red_rows <- suppressWarnings(gpu_reduce(red_rows_src, "sum", dims = 2L, keepdims = TRUE))
  route_red_rows <- attr(red_rows, "gpu_fallback")
  expect_overflow_value(
    route_red_rows,
    as.vector(mojor_gpu_array_read(red_rows)),
    c(wrap_plus_3, wrap_plus_3),
    c("gpu_reduce", "cpu_reduce")
  )

  red_cols_src <- GPUArray(
    matrix(c(imax, imax, 1L, 1L), nrow = 2L, ncol = 2L, byrow = TRUE),
    dtype = "i32"
  )
  red_cols <- suppressWarnings(gpu_reduce(red_cols_src, "sum", dims = 1L, keepdims = FALSE))
  route_red_cols <- attr(red_cols, "gpu_fallback")
  expect_overflow_value(
    route_red_cols,
    mojor_gpu_array_read(red_cols),
    c(wrap_plus_1, wrap_plus_1),
    c("gpu_reduce", "cpu_reduce")
  )

  .mojor_gpu_array_free_all(red_cols, red_cols_src, red_rows, red_rows_src, out_tcp, lhs_tcp, out_cp, lhs_cp, out_mm, rhs_mm, lhs_mm)

  expect_equal(mojor_gpu_buf_f32_live_count(), count0)
})

test_that("GPUArray i32 overflow lock-down covers matrix outputs and full-axis keepdims", {  .skip_if_no_gpu_backend()

  count0 <- mojor_gpu_buf_f32_live_count()
  imax <- .Machine$integer.max
  wrap_i32 <- function(x) {
    as.integer(((as.double(x) + 2147483648) %% 4294967296) - 2147483648)
  }

  expect_overflow_value <- function(route, value, wrapped, allowed_routes) {
    expect_true(route %in% allowed_routes)
    if (startsWith(route, "gpu_")) {
      expect_equal(as.integer(value), as.integer(wrapped))
    } else {
      expect_true(all(is.na(value)))
    }
  }

  wrap_plus_2 <- wrap_i32(as.double(imax) + 2)
  wrap_full_sum <- wrap_i32(2 * (as.double(imax) + 2))

  lhs_mm <- GPUArray(
    matrix(c(imax, 1L, 1L, imax, 1L, 1L), nrow = 2L, ncol = 3L, byrow = TRUE),
    dtype = "i32"
  )
  rhs_mm <- GPUArray(
    matrix(as.integer(rep(1L, 6L)), nrow = 3L, ncol = 2L),
    dtype = "i32"
  )
  out_mm <- suppressWarnings(gpu_matmul(lhs_mm, rhs_mm))
  route_mm <- attr(out_mm, "gpu_fallback")
  expect_overflow_value(
    route_mm,
    as.vector(mojor_gpu_array_read(out_mm)),
    rep(wrap_plus_2, 4L),
    c("gpu_matmul", "cpu_matmul")
  )

  red_all_keep <- suppressWarnings(gpu_reduce(lhs_mm, "sum", dims = c(1L, 2L), keepdims = TRUE))
  route_red_all_keep <- attr(red_all_keep, "gpu_fallback")
  expect_overflow_value(
    route_red_all_keep,
    as.vector(mojor_gpu_array_read(red_all_keep)),
    wrap_full_sum,
    c("gpu_reduce", "cpu_reduce")
  )

  red_all_drop <- suppressWarnings(gpu_reduce(lhs_mm, "sum", dims = c(1L, 2L), keepdims = FALSE))
  route_red_all_drop <- attr(red_all_drop, "gpu_fallback")
  expect_overflow_value(
    route_red_all_drop,
    as.vector(mojor_gpu_array_read(red_all_drop)),
    wrap_full_sum,
    c("gpu_reduce", "cpu_reduce")
  )

  .mojor_gpu_array_free_all(red_all_drop, red_all_keep, out_mm, rhs_mm, lhs_mm)

  expect_equal(mojor_gpu_buf_f32_live_count(), count0)
})

test_that("GPUArray i32 overflow lock-down covers matmul_into matrix output and mixed dims keepdims permutations", {  .skip_if_no_gpu_backend()

  count0 <- mojor_gpu_buf_f32_live_count()
  imax <- .Machine$integer.max
  wrap_i32 <- function(x) {
    as.integer(((as.double(x) + 2147483648) %% 4294967296) - 2147483648)
  }

  expect_overflow_value <- function(route, value, wrapped, allowed_routes) {
    expect_true(route %in% allowed_routes)
    if (startsWith(route, "gpu_")) {
      expect_equal(as.integer(value), as.integer(wrapped))
    } else {
      expect_true(all(is.na(value)))
    }
  }

  wrap_plus_1 <- wrap_i32(as.double(imax) + 1)
  wrap_plus_2 <- wrap_i32(as.double(imax) + 2)
  wrap_full <- wrap_i32((2 * as.double(imax)) + 2)

  lhs_into <- GPUArray(
    matrix(c(imax, 1L, 1L, imax, 1L, 1L), nrow = 2L, ncol = 3L, byrow = TRUE),
    dtype = "i32"
  )
  rhs_into <- GPUArray(
    matrix(as.integer(rep(1L, 6L)), nrow = 3L, ncol = 2L),
    dtype = "i32"
  )
  out_into <- gpu_zeros(dim = c(2L, 2L), dtype = "i32")
  out_into_res <- suppressWarnings(gpu_matmul_into(out_into, lhs_into, rhs_into))
  expect_identical(out_into_res, out_into)
  route_into <- attr(out_into_res, "gpu_fallback")
  expect_overflow_value(
    route_into,
    as.vector(mojor_gpu_array_read(out_into_res)),
    rep(wrap_plus_2, 4L),
    c("gpu_matmul", "cpu_matmul")
  )

  red_src <- GPUArray(
    matrix(c(imax, 1L, 1L, imax), nrow = 2L, ncol = 2L, byrow = TRUE),
    dtype = "i32"
  )

  red_dims1_keep <- suppressWarnings(gpu_reduce(red_src, "sum", dims = 1L, keepdims = TRUE))
  route_dims1_keep <- attr(red_dims1_keep, "gpu_fallback")
  expect_overflow_value(
    route_dims1_keep,
    as.vector(mojor_gpu_array_read(red_dims1_keep)),
    c(wrap_plus_1, wrap_plus_1),
    c("gpu_reduce", "cpu_reduce")
  )

  red_dims1_drop <- suppressWarnings(gpu_reduce(red_src, "sum", dims = 1L, keepdims = FALSE))
  route_dims1_drop <- attr(red_dims1_drop, "gpu_fallback")
  expect_overflow_value(
    route_dims1_drop,
    as.vector(mojor_gpu_array_read(red_dims1_drop)),
    c(wrap_plus_1, wrap_plus_1),
    c("gpu_reduce", "cpu_reduce")
  )

  red_dims2_keep <- suppressWarnings(gpu_reduce(red_src, "sum", dims = 2L, keepdims = TRUE))
  route_dims2_keep <- attr(red_dims2_keep, "gpu_fallback")
  expect_overflow_value(
    route_dims2_keep,
    as.vector(mojor_gpu_array_read(red_dims2_keep)),
    c(wrap_plus_1, wrap_plus_1),
    c("gpu_reduce", "cpu_reduce")
  )

  red_dims2_drop <- suppressWarnings(gpu_reduce(red_src, "sum", dims = 2L, keepdims = FALSE))
  route_dims2_drop <- attr(red_dims2_drop, "gpu_fallback")
  expect_overflow_value(
    route_dims2_drop,
    as.vector(mojor_gpu_array_read(red_dims2_drop)),
    c(wrap_plus_1, wrap_plus_1),
    c("gpu_reduce", "cpu_reduce")
  )

  red_full_keep <- suppressWarnings(gpu_reduce(red_src, "sum", dims = c(2L, 1L), keepdims = TRUE))
  route_full_keep <- attr(red_full_keep, "gpu_fallback")
  expect_overflow_value(
    route_full_keep,
    as.vector(mojor_gpu_array_read(red_full_keep)),
    wrap_full,
    c("gpu_reduce", "cpu_reduce")
  )

  red_full_drop <- suppressWarnings(gpu_reduce(red_src, "sum", dims = c(1L, 2L), keepdims = FALSE))
  route_full_drop <- attr(red_full_drop, "gpu_fallback")
  expect_overflow_value(
    route_full_drop,
    as.vector(mojor_gpu_array_read(red_full_drop)),
    wrap_full,
    c("gpu_reduce", "cpu_reduce")
  )

  .mojor_gpu_array_free_all(red_full_drop, red_full_keep, red_dims2_drop, red_dims2_keep, red_dims1_drop, red_dims1_keep, red_src, out_into, rhs_into, lhs_into)

  expect_equal(mojor_gpu_buf_f32_live_count(), count0)
})

test_that("GPUArray i32 overflow lock-down covers transpose matmul paths", {  .skip_if_no_gpu_backend()

  count0 <- mojor_gpu_buf_f32_live_count()
  imax <- .Machine$integer.max
  wrap_i32 <- function(x) {
    as.integer(((as.double(x) + 2147483648) %% 4294967296) - 2147483648)
  }

  expect_overflow_value <- function(route, value, wrapped, allowed_routes) {
    expect_true(route %in% allowed_routes)
    if (startsWith(route, "gpu_")) {
      expect_equal(as.integer(value), as.integer(wrapped))
    } else {
      expect_true(all(is.na(value)))
    }
  }

  wrap_plus_2 <- wrap_i32(as.double(imax) + 2)

  lhs_tb <- GPUArray(
    matrix(c(imax, 1L, 1L, imax, 1L, 1L), nrow = 2L, ncol = 3L, byrow = TRUE),
    dtype = "i32"
  )
  rhs_tb <- GPUArray(
    matrix(as.integer(rep(1L, 6L)), nrow = 2L, ncol = 3L, byrow = TRUE),
    dtype = "i32"
  )
  out_tb <- suppressWarnings(gpu_matmul(lhs_tb, rhs_tb, transpose_b = TRUE))
  route_tb <- attr(out_tb, "gpu_fallback")
  expect_overflow_value(
    route_tb,
    as.vector(mojor_gpu_array_read(out_tb)),
    rep(wrap_plus_2, 4L),
    c("gpu_matmul", "cpu_matmul")
  )

  out_tb_into <- gpu_zeros(dim = c(2L, 2L), dtype = "i32")
  out_tb_into_res <- suppressWarnings(gpu_matmul_into(out_tb_into, lhs_tb, rhs_tb, transpose_b = TRUE))
  expect_identical(out_tb_into_res, out_tb_into)
  route_tb_into <- attr(out_tb_into_res, "gpu_fallback")
  expect_overflow_value(
    route_tb_into,
    as.vector(mojor_gpu_array_read(out_tb_into_res)),
    rep(wrap_plus_2, 4L),
    c("gpu_matmul", "cpu_matmul")
  )

  lhs_ta <- GPUArray(
    matrix(c(imax, imax, 1L, 1L, 1L, 1L), nrow = 3L, ncol = 2L, byrow = TRUE),
    dtype = "i32"
  )
  rhs_ta <- GPUArray(
    matrix(as.integer(rep(1L, 6L)), nrow = 3L, ncol = 2L),
    dtype = "i32"
  )
  out_ta <- suppressWarnings(gpu_matmul(lhs_ta, rhs_ta, transpose_a = TRUE))
  route_ta <- attr(out_ta, "gpu_fallback")
  expect_overflow_value(
    route_ta,
    as.vector(mojor_gpu_array_read(out_ta)),
    rep(wrap_plus_2, 4L),
    c("gpu_matmul", "cpu_matmul")
  )

  .mojor_gpu_array_free_all(out_ta, rhs_ta, lhs_ta, out_tb_into, out_tb, rhs_tb, lhs_tb)

  expect_equal(mojor_gpu_buf_f32_live_count(), count0)
})

test_that("GPUArray i32 overflow lock-down covers rank-3 reduction dims permutations", {  .skip_if_no_gpu_backend()

  count0 <- mojor_gpu_buf_f32_live_count()
  imax <- .Machine$integer.max
  wrap_i32 <- function(x) {
    as.integer(((as.double(x) + 2147483648) %% 4294967296) - 2147483648)
  }

  expect_uniform_reduce <- function(route, host_value, reduced_elems, allowed_routes) {
    expect_true(route %in% allowed_routes)
    if (startsWith(route, "gpu_")) {
      expected <- wrap_i32(as.double(imax) * as.double(reduced_elems))
      expect_equal(as.integer(host_value), rep.int(as.integer(expected), length(host_value)))
    } else {
      expect_true(all(is.na(host_value)))
    }
  }

  src <- GPUArray(array(rep(as.integer(imax), 8L), dim = c(2L, 2L, 2L)), dtype = "i32")

  out_d1_drop <- suppressWarnings(gpu_reduce(src, "sum", dims = 1L, keepdims = FALSE))
  route_d1_drop <- attr(out_d1_drop, "gpu_fallback")
  host_d1_drop <- as.vector(mojor_gpu_array_read(out_d1_drop))
  expect_uniform_reduce(route_d1_drop, host_d1_drop, reduced_elems = 2L, c("gpu_reduce", "cpu_reduce"))

  out_d1_keep <- suppressWarnings(gpu_reduce(src, "sum", dims = 1L, keepdims = TRUE))
  route_d1_keep <- attr(out_d1_keep, "gpu_fallback")
  host_d1_keep <- as.vector(mojor_gpu_array_read(out_d1_keep))
  expect_uniform_reduce(route_d1_keep, host_d1_keep, reduced_elems = 2L, c("gpu_reduce", "cpu_reduce"))

  out_d23_drop <- suppressWarnings(gpu_reduce(src, "sum", dims = c(2L, 3L), keepdims = FALSE))
  route_d23_drop <- attr(out_d23_drop, "gpu_fallback")
  host_d23_drop <- as.vector(mojor_gpu_array_read(out_d23_drop))
  expect_uniform_reduce(route_d23_drop, host_d23_drop, reduced_elems = 4L, c("gpu_reduce", "cpu_reduce"))

  out_d23_keep <- suppressWarnings(gpu_reduce(src, "sum", dims = c(2L, 3L), keepdims = TRUE))
  route_d23_keep <- attr(out_d23_keep, "gpu_fallback")
  host_d23_keep <- as.vector(mojor_gpu_array_read(out_d23_keep))
  expect_uniform_reduce(route_d23_keep, host_d23_keep, reduced_elems = 4L, c("gpu_reduce", "cpu_reduce"))

  out_full_drop <- suppressWarnings(gpu_reduce(src, "sum", dims = c(1L, 2L, 3L), keepdims = FALSE))
  route_full_drop <- attr(out_full_drop, "gpu_fallback")
  host_full_drop <- as.vector(mojor_gpu_array_read(out_full_drop))
  expect_uniform_reduce(route_full_drop, host_full_drop, reduced_elems = 8L, c("gpu_reduce", "cpu_reduce"))

  out_full_keep <- suppressWarnings(gpu_reduce(src, "sum", dims = c(3L, 1L, 2L), keepdims = TRUE))
  route_full_keep <- attr(out_full_keep, "gpu_fallback")
  host_full_keep <- as.vector(mojor_gpu_array_read(out_full_keep))
  expect_uniform_reduce(route_full_keep, host_full_keep, reduced_elems = 8L, c("gpu_reduce", "cpu_reduce"))

  .mojor_gpu_array_free_all(out_full_keep, out_full_drop, out_d23_keep, out_d23_drop, out_d1_keep, out_d1_drop, src)

  expect_equal(mojor_gpu_buf_f32_live_count(), count0)
})

test_that("GPUArray i32 indexing uses GPU slice/gather with safe scatter fallback", {  .skip_if_no_gpu_backend()

  count0 <- mojor_gpu_buf_f32_live_count()
  src <- matrix(as.integer(1:12), nrow = 3L, ncol = 4L)
  gx <- GPUArray(src, dtype = "i32")

  s <- gx[1:2, 2:3, drop = FALSE]
  expect_equal(attr(s, "gpu_fallback"), "gpu_slice")
  expect_equal(mojor_gpu_array_read(s), src[1:2, 2:3, drop = FALSE])

  g <- gx[c(3L, 1L), c(4L, 2L), drop = FALSE]
  expect_true(attr(g, "gpu_fallback") %in% c("gpu_gather", "cpu_gather"))
  expect_equal(mojor_gpu_array_read(g), src[c(3L, 1L), c(4L, 2L), drop = FALSE])

  vals <- matrix(as.integer(c(101, 102, 103, 104)), nrow = 2L, ncol = 2L)
  gx[c(3L, 1L), c(4L, 2L)] <- vals
  expect_true(attr(gx, "gpu_fallback") %in% c("gpu_scatter", "cpu_scatter"))
  src_ref <- src
  src_ref[c(3L, 1L), c(4L, 2L)] <- vals
  expect_equal(mojor_gpu_array_read(gx), src_ref)

  .mojor_gpu_array_free_all(g, s, gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), count0)
})

test_that("GPUArray like constructors preserve metadata for GPU and host sources", {  .skip_if_no_gpu_backend(require_float = TRUE)

  count0 <- mojor_gpu_buf_f32_live_count()
  src <- .mojor_gpu_src_matrix()
  gx <- GPUArray(src, dtype = "f32")

  gpu_cases <- list(
    list(name = "zeros", make = function() gpu_zeros_like(gx), expect = array(0, dim = dim(src), dimnames = dimnames(src))),
    list(name = "ones", make = function() gpu_ones_like(gx), expect = array(1, dim = dim(src), dimnames = dimnames(src))),
    list(name = "full", make = function() gpu_full_like(gx, 2.5), expect = array(2.5, dim = dim(src), dimnames = dimnames(src))),
    list(name = "empty", make = function() gpu_empty_like(gx), expect = matrix(as.numeric(801:812), nrow = 3L, ncol = 4L, dimnames = dimnames(src)))
  )
  host_cases <- list(
    list(name = "zeros", make = function() gpu_zeros_like(src, dtype = "f32"), expect = array(0, dim = dim(src), dimnames = dimnames(src))),
    list(name = "ones", make = function() gpu_ones_like(src, dtype = "f32"), expect = array(1, dim = dim(src), dimnames = dimnames(src))),
    list(name = "full", make = function() gpu_full_like(src, 7.25, dtype = "f32"), expect = array(7.25, dim = dim(src), dimnames = dimnames(src))),
    list(name = "empty", make = function() gpu_empty_like(src, dtype = "f32"), expect = matrix(as.numeric(901:912), nrow = 3L, ncol = 4L, dimnames = dimnames(src)))
  )

  made <- list()
  for (case in gpu_cases) {
    obj <- case$make()
    made[[paste0("gpu_", case$name)]] <- obj
    .mojor_expect_gpu_like_meta(
      obj,
      gx,
      expect_dtype = .mojor_gpu_array_dtype(gx),
      expect_api = .mojor_gpu_array_api(gx)
    )
    if (identical(case$name, "empty")) {
      mojor_gpu_array_write(obj, case$expect)
    }
    expect_equal(mojor_gpu_array_read(obj), case$expect)
  }
  for (case in host_cases) {
    obj <- case$make()
    made[[paste0("host_", case$name)]] <- obj
    .mojor_expect_gpu_like_meta(obj, src, expect_dtype = "f32")
    if (identical(case$name, "empty")) {
      mojor_gpu_array_write(obj, case$expect)
    }
    expect_equal(mojor_gpu_array_read(obj), case$expect)
  }

  gv_full <- gpu_full_like(as.numeric(1:5), -3, dtype = "f32")
  gv_empty <- gpu_empty_like(as.numeric(1:5), dtype = "f32")
  mojor_gpu_array_write(gv_empty, c(-1, -2, -3, -4, -5))
  expect_equal(.mojor_gpu_array_dtype(gv_full), "f32")
  expect_equal(.mojor_gpu_array_dtype(gv_empty), "f32")
  expect_equal(mojor_gpu_array_read(gv_full), rep(-3, 5L))
  expect_equal(mojor_gpu_array_read(gv_empty), c(-1, -2, -3, -4, -5))
  expect_null(dim(gv_full))
  expect_null(dim(gv_empty))

  .mojor_gpu_array_free_all(gv_empty, gv_full, made$host_empty, made$host_full, made$host_ones, made$host_zeros,
    made$gpu_empty, made$gpu_full, made$gpu_ones, made$gpu_zeros, gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), count0)
})

test_that("GPUArray random like constructors preserve metadata and deterministic outputs", {  .skip_if_no_gpu_backend(require_float = TRUE)

  count0 <- mojor_gpu_buf_f32_live_count()
  src <- .mojor_gpu_src_matrix()
  gx <- GPUArray(src, dtype = "f32")

  set.seed(101)
  gr <- gpu_rand_like(gx, min = 10, max = 20)
  .mojor_expect_gpu_like_meta(gr, gx, expect_dtype = .mojor_gpu_array_dtype(gx), expect_api = .mojor_gpu_array_api(gx))
  host_gr <- mojor_gpu_array_read(gr)
  expect_true(all(host_gr >= 10))
  expect_true(all(host_gr <= 20))

  set.seed(202)
  gh <- gpu_rand_like(src, min = -1, max = 1, dtype = "f32")
  .mojor_expect_gpu_like_meta(gh, src, expect_dtype = "f32")
  host_gh <- mojor_gpu_array_read(gh)
  expect_true(all(host_gh >= -1))
  expect_true(all(host_gh <= 1))

  gv <- gpu_rand_like(as.numeric(1:5), min = 2, max = 3, dtype = "f32")
  host_gv <- mojor_gpu_array_read(gv)
  expect_length(host_gv, 5L)
  expect_null(dim(gv))
  expect_true(all(host_gv >= 2))
  expect_true(all(host_gv <= 3))

  set.seed(303)
  exp_grn <- array(stats::rnorm(12L, mean = 2.0, sd = 0.25), dim = dim(src), dimnames = dimnames(src))
  set.seed(303)
  grn <- gpu_randn_like(gx, mean = 2.0, sd = 0.25)
  .mojor_expect_gpu_like_meta(grn, gx, expect_dtype = .mojor_gpu_array_dtype(gx), expect_api = .mojor_gpu_array_api(gx))
  expect_equal(mojor_gpu_array_read(grn), exp_grn, tolerance = 1e-5)

  set.seed(404)
  exp_ghn <- array(stats::rnorm(12L, mean = -1.0, sd = 1.5), dim = dim(src), dimnames = dimnames(src))
  set.seed(404)
  ghn <- gpu_randn_like(src, mean = -1.0, sd = 1.5, dtype = "f32")
  .mojor_expect_gpu_like_meta(ghn, src, expect_dtype = "f32")
  expect_equal(mojor_gpu_array_read(ghn), exp_ghn, tolerance = 1e-5)

  set.seed(505)
  exp_gvn <- stats::rnorm(5L, mean = 0, sd = 0)
  set.seed(505)
  gvn <- gpu_randn_like(as.numeric(1:5), mean = 0, sd = 0, dtype = "f32")
  expect_length(mojor_gpu_array_read(gvn), 5L)
  expect_null(dim(gvn))
  expect_equal(mojor_gpu_array_read(gvn), exp_gvn, tolerance = 1e-6)

  set.seed(606)
  exp_gri <- array(sample.int(4L, size = 12L, replace = TRUE) + 5L - 1L, dim = dim(src), dimnames = dimnames(src))
  set.seed(606)
  gri <- gpu_randi_like(gx, low = 5L, high = 9L)
  .mojor_expect_gpu_like_meta(gri, gx, expect_dtype = .mojor_gpu_array_dtype(gx), expect_api = .mojor_gpu_array_api(gx))
  host_gri <- mojor_gpu_array_read(gri)
  expect_equal(host_gri, exp_gri, tolerance = 1e-6)
  expect_true(all(host_gri >= 5))
  expect_true(all(host_gri < 9))

  set.seed(707)
  exp_ghi <- array(sample.int(6L, size = 12L, replace = TRUE) + (-3L) - 1L, dim = dim(src), dimnames = dimnames(src))
  set.seed(707)
  ghi <- gpu_randi_like(src, low = -3L, high = 3L, dtype = "f32")
  .mojor_expect_gpu_like_meta(ghi, src, expect_dtype = "f32")
  host_ghi <- mojor_gpu_array_read(ghi)
  expect_equal(host_ghi, exp_ghi, tolerance = 1e-6)
  expect_true(all(host_ghi >= -3))
  expect_true(all(host_ghi < 3))

  set.seed(808)
  exp_gvi <- sample.int(3L, size = 5L, replace = TRUE) + 0L - 1L
  set.seed(808)
  gvi <- gpu_randi_like(as.numeric(1:5), low = 0L, high = 3L, dtype = "f32")
  host_gvi <- mojor_gpu_array_read(gvi)
  expect_length(host_gvi, 5L)
  expect_null(dim(gvi))
  expect_equal(host_gvi, exp_gvi, tolerance = 1e-6)
  expect_true(all(host_gvi >= 0))
  expect_true(all(host_gvi < 3))

  .mojor_gpu_array_free_all(gvi, ghi, gri, gvn, ghn, grn, gv, gh, gr, gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), count0)
})
