# Split from test_gpu_array.R (chunk 07).
# Shared utilities are in helper-gpu-array.R.

.mojor_skip_gpu_runtime_file_unless_opt_in()

test_that("f64 reduce capability short-circuits to explicit cpu fallback", {  skip_if_no_gpu_f64()

  x <- matrix(as.numeric(1:12), nrow = 3L, ncol = 4L)
  gx <- GPUArray(x, dtype = "f64")
  out_all <- NULL
  out_dims <- NULL
  on.exit(.mojor_gpu_array_free_all(out_dims, out_all, gx), add = TRUE)

  reduce_env <- environment(.mojor_gpu_reduce)
  old_cap <- get(".mojor_gpu_f64_reduce_capable", envir = reduce_env, inherits = TRUE)
  old_reduce_call <- get(".mojor_gpu_reduce_call", envir = reduce_env, inherits = TRUE)
  called <- 0L
  modes <- character(0L)
  assign(
    ".mojor_gpu_f64_reduce_capable",
    function(force_refresh = FALSE, dims_mode = c("scalar", "dims")) {
      dims_mode <- match.arg(dims_mode, c("scalar", "dims"))
      modes <<- c(modes, dims_mode)
      FALSE
    },
    envir = reduce_env
  )
  assign(
    ".mojor_gpu_reduce_call",
    function(...) {
      called <<- called + 1L
      stop("forced reduce call failure", call. = FALSE)
    },
    envir = reduce_env
  )
  on.exit({
    assign(".mojor_gpu_f64_reduce_capable", old_cap, envir = reduce_env)
    assign(".mojor_gpu_reduce_call", old_reduce_call, envir = reduce_env)
  }, add = TRUE)

  out_all <- gpu_reduce(gx, "sum")
  expect_equal(attr(out_all, "gpu_fallback"), "cpu_reduce")
  expect_equal(mojor_gpu_array_read(out_all), sum(x))

  out_dims <- gpu_reduce(gx, "mean", dims = 1L, keepdims = FALSE)
  expect_equal(attr(out_dims, "gpu_fallback"), "cpu_reduce")
  expect_equal(mojor_gpu_array_read(out_dims), colMeans(x))

  expect_equal(called, 0L)
  expect_equal(modes, c("scalar", "dims"))
})

test_that("gpu reduction convenience wrappers dispatch through gpu_reduce", {  .skip_if_no_gpu_backend(require_float = TRUE)

  x <- matrix(1:12, nrow = 3L, ncol = 4L)
  gx <- GPUArray(x, dtype = "f32")

  cols <- gpu_sum(gx, dims = 1L, keepdims = FALSE)
  expect_equal(mojor_gpu_array_read(cols), colSums(x))

  rows <- gpu_mean(gx, dims = 2L, keepdims = TRUE)
  expect_equal(dim(mojor_gpu_array_read(rows)), c(3L, 1L))
  expect_equal(mojor_gpu_array_read(rows), array(rowMeans(x), dim = c(3L, 1L)))

  prod_all <- gpu_prod(gx, dims = c(1L, 2L), keepdims = FALSE)
  expect_equal(mojor_gpu_array_read(prod_all), prod(x))
  expect_true(attr(prod_all, "gpu_fallback") %in% c("gpu_reduce", "cpu_reduce"))
  if (identical(attr(prod_all, "gpu_fallback"), "cpu_reduce")) {
    expect_identical(
      attr(prod_all, "gpu_fallback_reason_code"),
      "reduce_kernel_call_failed_scalar"
    )
  }

  prod_cols <- gpu_prod(gx, dims = 1L, keepdims = FALSE)
  expect_equal(mojor_gpu_array_read(prod_cols), apply(x, 2L, prod))
  expect_true(attr(prod_cols, "gpu_fallback") %in% c("gpu_reduce", "cpu_reduce"))
  if (identical(attr(prod_cols, "gpu_fallback"), "cpu_reduce")) {
    expect_identical(
      attr(prod_cols, "gpu_fallback_reason_code"),
      "reduce_kernel_call_failed_dims"
    )
  }

  min_all <- gpu_min(gx, dims = c(1L, 2L), keepdims = FALSE)
  expect_equal(mojor_gpu_array_read(min_all), min(x))
  pmin_all <- gpu_pmin(gx, dims = c(1L, 2L), keepdims = FALSE)
  expect_equal(mojor_gpu_array_read(pmin_all), min(x))

  max_keep <- gpu_max(gx, dims = c(1L, 2L), keepdims = TRUE)
  expect_equal(dim(mojor_gpu_array_read(max_keep)), c(1L, 1L))
  expect_equal(mojor_gpu_array_read(max_keep), array(max(x), dim = c(1L, 1L)))
  pmax_keep <- gpu_pmax(gx, dims = c(1L, 2L), keepdims = TRUE)
  expect_equal(dim(mojor_gpu_array_read(pmax_keep)), c(1L, 1L))
  expect_equal(mojor_gpu_array_read(pmax_keep), array(max(x), dim = c(1L, 1L)))

  argmin_all <- gpu_argmin(gx)
  argmax_all <- gpu_argmax(gx)
  expect_equal(mojor_gpu_array_read(argmin_all), 1)
  expect_equal(mojor_gpu_array_read(argmax_all), 12)

  argmin_cols <- gpu_argmin(gx, dims = 1L)
  argmax_rows <- gpu_argmax(gx, dims = 2L)
  expect_equal(mojor_gpu_array_read(argmin_cols), c(1, 1, 1, 1))
  expect_equal(mojor_gpu_array_read(argmax_rows), c(4, 4, 4))

  .mojor_gpu_array_free_all(
    argmax_rows, argmin_cols, argmax_all, argmin_all, pmax_keep, max_keep,
    pmin_all, min_all, prod_cols, prod_all, rows, cols, gx
  )
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("gpu_reduce hardened parity covers edge and randomized lanes", {  .skip_if_no_gpu_backend()

  expect_reduce_parity <- function(
    host, dtype, op, dims = NULL, keepdims = FALSE, tol = 1e-5, force_cpu = FALSE
  ) {
    gx <- GPUArray(host, dtype = dtype)
    out <- NULL
    reduce_env <- environment(.mojor_gpu_reduce)
    old_reduce_call <- NULL
    if (isTRUE(force_cpu)) {
      old_reduce_call <- get(".mojor_gpu_reduce_call", envir = reduce_env, inherits = FALSE)
      assign(
        ".mojor_gpu_reduce_call",
        function(...) stop("forced reduce host parity fallback", call. = FALSE),
        envir = reduce_env
      )
      on.exit(assign(".mojor_gpu_reduce_call", old_reduce_call, envir = reduce_env), add = TRUE)
    }
    on.exit(.mojor_gpu_array_free_all(out, gx), add = TRUE)

    out <- suppressWarnings(gpu_reduce(gx, op, dims = dims, keepdims = keepdims))
    expect_true(attr(out, "gpu_fallback", exact = TRUE) %in% c("gpu_reduce", "cpu_reduce"))
    if (isTRUE(force_cpu)) {
      expect_identical(attr(out, "gpu_fallback", exact = TRUE), "cpu_reduce")
    }

    expected <- suppressWarnings(
      .mojor_gpu_reduce_host(host, op, dims = dims, keepdims = keepdims)
    )
    actual <- suppressWarnings(mojor_gpu_array_read(out))

    if (op %in% c("argmin", "argmax")) {
      expect_equal(as.integer(actual), as.integer(expected))
      return(invisible(NULL))
    }
    if (identical(dtype, "i32")) {
      expected_i32 <- suppressWarnings(as.integer(expected))
      if (!is.null(dim(expected))) {
        dim(expected_i32) <- dim(expected)
      }
      expect_equal(as.integer(actual), expected_i32, tolerance = tol)
      return(invisible(NULL))
    }
    expect_equal(actual, expected, tolerance = tol)
  }

  dims_cases <- list(NULL, 1L, 2L, c(1L, 2L))

  for (seed in seq_len(3L)) {
    set.seed(1000L + seed)
    host_f32 <- matrix(runif(12L, min = -3, max = 4), nrow = 3L, ncol = 4L)
    for (op in c("sum", "mean", "prod", "min", "max")) {
      for (dims in dims_cases) {
        expect_reduce_parity(host_f32, "f32", op, dims = dims, keepdims = FALSE, tol = 1e-4)
        expect_reduce_parity(host_f32, "f32", op, dims = dims, keepdims = TRUE, tol = 1e-4)
      }
    }
  }

  host_arg <- matrix(as.numeric(seq_len(12L)) + 0.25, nrow = 3L, ncol = 4L)
  for (op in c("argmin", "argmax")) {
    for (dims in dims_cases) {
      expect_reduce_parity(host_arg, "f32", op, dims = dims, keepdims = FALSE)
    }
  }

  host_na <- matrix(c(NA_real_, -1, 2, NA_real_, 4, 5), nrow = 2L, ncol = 3L)
  for (op in c("sum", "mean", "prod", "min", "max")) {
    for (dims in dims_cases) {
      expect_reduce_parity(
        host_na, "f32", op, dims = dims, keepdims = FALSE, tol = 1e-5,
        force_cpu = TRUE
      )
    }
  }

  host_zero <- numeric(0)
  for (op in c("sum", "mean", "prod", "min", "max")) {
    expect_reduce_parity(host_zero, "f32", op, dims = NULL, keepdims = FALSE, tol = 1e-5)
  }

  host_i32 <- matrix(as.integer(c(2147483640L, 7L, -1L, 2L)), nrow = 2L, ncol = 2L)
  for (op in c("sum", "mean", "prod", "min", "max")) {
    for (dims in dims_cases) {
      expect_reduce_parity(
        host_i32, "i32", op, dims = dims, keepdims = FALSE, tol = 1e-5,
        force_cpu = TRUE
      )
    }
  }

  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("gpu_cast, gpu_promote, and gpu_broadcast are available", {  .skip_if_no_gpu_backend(require_float = TRUE)

  count0 <- mojor_gpu_buf_f32_live_count()

  src_host <- matrix(as.numeric(1:6), nrow = 2L, ncol = 3L)
  gx <- GPUArray(src_host, dtype = "f32")

  g64 <- gpu_cast(gx, "f64")
  expect_equal(.mojor_gpu_array_dtype(g64), "f64")
  expect_equal(mojor_gpu_array_read(g64), src_host)
  expect_true(attr(g64, "gpu_fallback") %in% c("gpu_cast", "cpu_cast"))

  gi <- gpu_cast(gx, "i32")
  expect_equal(.mojor_gpu_array_dtype(gi), "i32")
  expect_equal(mojor_gpu_array_read(gi), matrix(as.integer(1:6), nrow = 2L, ncol = 3L))
  expect_true(attr(gi, "gpu_fallback") %in% c("gpu_cast", "cpu_cast"))

  g32 <- gpu_cast(g64, "f32")
  expect_equal(.mojor_gpu_array_dtype(g32), "f32")
  expect_equal(mojor_gpu_array_read(g32), src_host)
  expect_true(attr(g32, "gpu_fallback") %in% c("gpu_cast", "cpu_cast"))

  gi_from_f64 <- gpu_cast(g64, "i32")
  expect_equal(.mojor_gpu_array_dtype(gi_from_f64), "i32")
  expect_equal(mojor_gpu_array_read(gi_from_f64), matrix(as.integer(1:6), nrow = 2L, ncol = 3L))
  expect_true(attr(gi_from_f64, "gpu_fallback") %in% c("gpu_cast", "cpu_cast"))

  gf64_from_i32 <- gpu_cast(gi, "f64")
  expect_equal(.mojor_gpu_array_dtype(gf64_from_i32), "f64")
  expect_equal(mojor_gpu_array_read(gf64_from_i32), src_host)
  expect_true(attr(gf64_from_i32, "gpu_fallback") %in% c("gpu_cast", "cpu_cast"))

  gf32_from_i32 <- gpu_cast(gi, "f32")
  expect_equal(.mojor_gpu_array_dtype(gf32_from_i32), "f32")
  expect_equal(mojor_gpu_array_read(gf32_from_i32), src_host)
  expect_true(attr(gf32_from_i32, "gpu_fallback") %in% c("gpu_cast", "cpu_cast"))

  g32_same <- gpu_cast(gx, "f32")
  expect_equal(.mojor_gpu_array_dtype(g32_same), "f32")
  expect_equal(mojor_gpu_array_read(g32_same), src_host)
  expect_identical(attr(g32_same, "gpu_fallback"), "gpu_cast")

  p <- gpu_promote(gx, g64)
  expect_true(is.list(p))
  expect_true(inherits(p$x, "GPUArray"))
  expect_true(inherits(p$y, "GPUArray"))
  expect_equal(p$dtype, "f64")
  expect_equal(.mojor_gpu_array_dtype(p$x), "f64")
  expect_equal(.mojor_gpu_array_dtype(p$y), "f64")

  src_b <- GPUArray(c(1, 2, 3), dtype = "f32")
  b <- gpu_broadcast(src_b, c(2L, 3L))
  expect_true(inherits(b, "GPUArray"))
  expect_equal(
    mojor_gpu_array_read(b),
    .mojor_gpu_broadcast_host(as.numeric(c(1, 2, 3)), c(2L, 3L))
  )
  expect_true(attr(b, "gpu_fallback") %in% c("gpu_broadcast", "cpu_broadcast"))

  src_b2 <- GPUArray(matrix(as.integer(c(1L, 2L)), nrow = 2L, ncol = 1L), dtype = "i32")
  b2 <- gpu_broadcast(src_b2, c(2L, 4L))
  expect_true(inherits(b2, "GPUArray"))
  expect_equal(
    mojor_gpu_array_read(b2),
    .mojor_gpu_broadcast_host(
      matrix(as.integer(c(1L, 2L)), nrow = 2L, ncol = 1L),
      c(2L, 4L)
    )
  )
  expect_true(attr(b2, "gpu_fallback") %in% c("gpu_broadcast", "cpu_broadcast"))

  src_b3_host <- array(as.numeric(c(10, 20)), dim = c(1L, 2L, 1L))
  src_b3 <- GPUArray(src_b3_host, dtype = "f32")
  b3 <- gpu_broadcast(src_b3, c(3L, 2L, 4L))
  expect_true(inherits(b3, "GPUArray"))
  expect_equal(
    mojor_gpu_array_read(b3),
    .mojor_gpu_broadcast_host(src_b3_host, c(3L, 2L, 4L))
  )
  expect_true(attr(b3, "gpu_fallback") %in% c("gpu_broadcast", "cpu_broadcast"))

  b_same <- gpu_broadcast(gx, c(2L, 3L))
  expect_equal(mojor_gpu_array_read(b_same), src_host)
  expect_true(attr(b_same, "gpu_fallback") %in% c("gpu_broadcast", "cpu_broadcast"))

  host_cast <- gpu_cast(as.numeric(c(1, 2, 3)), "f64")
  expect_identical(attr(host_cast, "gpu_fallback"), "host_cast")
  expect_equal(.mojor_gpu_array_dtype(host_cast), "f64")

  host_broadcast <- gpu_broadcast(as.numeric(c(1, 2)), c(2L, 2L))
  expect_identical(attr(host_broadcast, "gpu_fallback"), "host_broadcast")
  expect_equal(
    mojor_gpu_array_read(host_broadcast),
    .mojor_gpu_broadcast_host(as.numeric(c(1, 2)), c(2L, 2L))
  )

  expect_true(any(c(
    attr(g64, "gpu_fallback"),
    attr(gi, "gpu_fallback"),
    attr(g32, "gpu_fallback"),
    attr(gi_from_f64, "gpu_fallback"),
    attr(gf64_from_i32, "gpu_fallback"),
    attr(gf32_from_i32, "gpu_fallback"),
    attr(g32_same, "gpu_fallback")
  ) == "gpu_cast"))
  expect_true(any(c(
    attr(b, "gpu_fallback"),
    attr(b2, "gpu_fallback"),
    attr(b3, "gpu_fallback"),
    attr(b_same, "gpu_fallback")
  ) == "gpu_broadcast"))
  expect_error(gpu_broadcast(gx, c(4L, 4L)), "not broadcast-compatible")

  .mojor_gpu_array_free_all(host_broadcast, host_cast, b_same, b3, src_b3, b2, src_b2, b, src_b, g32_same, gf32_from_i32, gf64_from_i32, gi_from_f64, g32, gi, p$x, g64, gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), count0)
})

test_that("transpiled gpu_argmin/gpu_argmax compile and run via gpu reduce helper", {  .skip_if_no_gpu_backend(require_mojo = TRUE)
  if (!mojor_is_loaded()) {
    skip("Mojo backend not loaded")
  }

  f_max <- function(x) {
    out <- 0.0
    for (i in seq_len(1L)) {
      out <- gpu_argmax(x)
    }
    out
  }
  f_min <- function(x) {
    out <- 0.0
    for (i in seq_len(1L)) {
      out <- gpu_argmin(x)
    }
    out
  }

  built_max <- mojor_build(f_max, x = "f64[]", name = "t_gpu_argmax_rt", cache = FALSE, load = TRUE)
  built_min <- mojor_build(f_min, x = "f64[]", name = "t_gpu_argmin_rt", cache = FALSE, load = TRUE)

  x <- c(2, 9, 4, 7)
  max_idx <- built_max$func(x)
  min_idx <- built_min$func(x)
  expect_true(is.numeric(max_idx))
  expect_true(is.numeric(min_idx))
  expect_equal(length(max_idx), 1L)
  expect_equal(length(min_idx), 1L)
  expect_true(max_idx >= 1 && max_idx <= length(x))
  expect_true(min_idx >= 1 && min_idx <= length(x))
})

test_that("gpu elementwise supports gpu array inputs", {  .skip_if_no_gpu_backend()
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2 + 1
    }
    out
  }
  n <- 256L
  kernel <- gpu_kernel(
    f,
    x = "f32[]",
    name = "t_gpu_array_ew",
    elementwise_size = n,
    load = TRUE,
    cache = TRUE
  )
  expect_true(is.function(kernel))
  x <- runif(n)
  buf <- mojor_gpu_array(x, dtype = "f32")
  out <- kernel(buf)
  expect_true(inherits(out, "mojor_gpu_array"))
  out_host <- mojor_gpu_array_read(out)
  expect_length(out_host, length(x))
  expect_true(max(abs(out_host - (x * 2 + 1))) < 1e-3)
  .mojor_gpu_array_free_all(out, buf)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("gpu elementwise supports broadcast_nd dims", {  .skip_if_no_gpu_backend(require_float = TRUE)
  f <- function(x, y) {
    out <- float::float32(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  n <- 6L
  build <- mojor_build(
    f,
    x = "f32[]",
    y = "f32[]",
    name = "t_gpu_array_ew_bcast",
    elementwise = TRUE,
    elementwise_target = "gpu",
    elementwise_size = n,
    broadcast = "broadcast_nd",
    load = TRUE,
    cache = FALSE
  )
  expect_true(is.function(build$gpu_func))
  x_host <- matrix(runif(6), nrow = 2)
  y_host <- matrix(c(1, 2, 3), nrow = 1)
  buf_x <- mojor_gpu_array(x_host, dtype = "f32")
  buf_y <- mojor_gpu_array(y_host, dtype = "f32")
  out <- build$gpu_func_raw(buf_x, buf_y)
  expect_true(inherits(out, "mojor_gpu_array"))
  out_host <- mojor_gpu_array_read(out)
  expected_mat <- x_host + matrix(rep(as.vector(y_host), each = nrow(x_host)), nrow = nrow(x_host))
  expected <- as.vector(expected_mat)
  expect_length(out_host, length(expected))
  expect_true(max(abs(out_host - expected)) < 1e-5)

  buf_x64 <- mojor_gpu_array(x_host, dtype = "f64")
  out_mixed <- build$gpu_func_raw(buf_x64, buf_y)
  expect_true(inherits(out_mixed, "mojor_gpu_array"))
  expect_equal(.mojor_gpu_array_dtype(out_mixed), "f32")
  expect_equal(mojor_gpu_array_read(out_mixed), expected, tolerance = 1e-5)

  .mojor_gpu_array_free_all(out_mixed, buf_x64, out, buf_y, buf_x)
})

test_that("gpu f32 inputs round-trip on GPU", {  .skip_if_no_gpu_backend(require_float = TRUE)
  x <- float::fl(runif(16))
  out <- mojor_sigmoid_f32_gpu(x)
  expect_true(is.numeric(out) || float::is.float(out))
  ref32 <- as.numeric(1 / (1 + exp(-as.numeric(x))))
  expect_true(max(abs(out - ref32)) < 1e-5)

  buf <- mojor_gpu_array(x, dtype = "f32")
  on.exit(mojor_gpu_array_free(buf), add = TRUE)
  roundtrip <- mojor_gpu_array_read(buf)
  expect_true(max(abs(roundtrip - as.numeric(x))) < 1e-6)
})

test_that("mojor_gpu_check_release flags GPU buffer leaks", {  .skip_if_no_gpu_backend(require_float = TRUE)
  x <- float::fl(runif(1024))
  expect_silent(mojor_gpu_check_release(mojor_gpu_chain, x, iters = 1L, scale = 1.1, bias = 0.05))
})

test_that("gpu context smoke test (optional)", {  if (!mojor_is_loaded()) {
    skip("Mojo backend not loaded")
  }
  if (!mojor_has_gpu()) {
    skip("GPU not available")
  }
  if (!requireNamespace("testthat", quietly = TRUE)) {
    skip("testthat not available")
  }
  if (tolower(Sys.getenv("MOJOR_TEST_GPU_CTX_SMOKE", unset = "")) %in% c("1", "true", "yes", "y", "on")) {
    res <- mojor_gpu_ctx_smoke(strict = TRUE)
    expect_true(isTRUE(res$ok))
  } else {
    skip("set MOJOR_TEST_GPU_CTX_SMOKE=1 to run")
  }
})

test_that("elementwise preserves dim for arrays", {  if (!mojor_is_loaded()) {
  skip("Mojo backend not loaded")
}
  .mojor_test_local_options(ir_only = FALSE)
  mat <- matrix(runif(100), nrow = 10)
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2 + 1
    }
    out
  }
  built <- .mojor_build_or_skip_expr_subset(
    mojor_build(
      f,
      x = "f64[]",
      name = "t_ew_dim"
    )
  )
  out <- built$func(mat)
  expect_true(is.matrix(out))
  expect_equal(dim(out), dim(mat))
})

test_that("broadcast scalar length-1 arrays when enabled", {  if (!mojor_is_loaded()) {
  skip("Mojo backend not loaded")
}
  .mojor_test_local_options(ir_only = FALSE)
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  x <- runif(100)
  y <- 2
  built <- .mojor_build_or_skip_expr_subset(
    mojor_build(
      f,
      x = "f64[]",
      y = "f64[]",
      broadcast = "scalar",
      name = "t_ew_broadcast"
    )
  )
  out <- built$func(x, y)
  expect_equal(out, x + y)
})

test_that("broadcast recycle supports shorter vectors", {  if (!mojor_is_loaded()) {
  skip("Mojo backend not loaded")
}
  .mojor_test_local_options(ir_only = FALSE)
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  x <- runif(12)
  y <- c(1, 2, 3)
  built <- .mojor_build_or_skip_expr_subset(
    mojor_build(
      f,
      x = "f64[]",
      y = "f64[]",
      broadcast = "recycle",
      name = "t_ew_recycle"
    )
  )
  out <- built$func(x, y)
  expect_equal(out, x + rep(y, length.out = length(x)))
})

test_that("broadcast recycle_warn emits warning", {  if (!mojor_is_loaded()) {
  skip("Mojo backend not loaded")
}
  .mojor_test_local_options(ir_only = FALSE)
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  x <- runif(10)
  y <- c(1, 2)
  built <- .mojor_build_or_skip_expr_subset(
    mojor_build(
      f,
      x = "f64[]",
      y = "f64[]",
      broadcast = "recycle_warn",
      name = "t_ew_recycle_warn"
    )
  )
  expect_warning(
    built$func(x, y),
    "recycling"
  )
})

test_that("GPUArray Ops work for basic arithmetic", {  .skip_if_no_gpu_backend(require_float = TRUE)
  x <- runif(64)
  y <- runif(64)
  ax <- mojor_gpu_array(x, dtype = "f32")
  ay <- mojor_gpu_array(y, dtype = "f32")
  az <- ax + ay
  expect_true(inherits(az, "GPUArray"))
  host <- mojor_gpu_array_read(az)
  expect_true(max(abs(host - (x + y))) < 1e-5)
  bz <- ax * 2
  host2 <- mojor_gpu_array_read(bz)
  expect_true(max(abs(host2 - (x * 2))) < 1e-5)
  pz <- ax ^ 2
  host3 <- mojor_gpu_array_read(pz)
  expect_true(max(abs(host3 - (x ^ 2))) < 1e-4)

  iz <- +ax
  host4 <- mojor_gpu_array_read(iz)
  expect_true(max(abs(host4 - x)) < 1e-5)

  xi <- as.numeric(0:15)
  ai <- mojor_gpu_array(xi, dtype = "f32")
  mz <- ai %% 3
  host5 <- mojor_gpu_array_read(mz)
  expect_equal(host5, xi %% 3, tolerance = 1e-5)
  dz <- ai %/% 3
  host6 <- mojor_gpu_array_read(dz)
  expect_equal(host6, xi %/% 3, tolerance = 1e-5)

  .mojor_gpu_array_free_all(dz, mz, ai, iz, pz, bz, az, ay, ax)
})

test_that("GPUArray Ops support comparison and logical operators", {  .skip_if_no_gpu_backend(require_float = TRUE)
  x <- matrix(as.numeric(c(-1, 0, 2, 3, 4, 5)), nrow = 2L, ncol = 3L)
  y <- matrix(as.numeric(c(0, 0, 1, 4, 1, 5)), nrow = 2L, ncol = 3L)
  ax <- GPUArray(x, dtype = "f32")
  ay <- GPUArray(y, dtype = "f32")

  lt <- ax < ay
  expect_true(inherits(lt, "mojor_gpu_array"))
  expect_equal(.mojor_gpu_array_dtype(lt), "i32")
  expect_equal(attr(lt, "gpu_fallback"), "cpu_compare")
  expect_equal(mojor_gpu_array_read(lt), array(as.integer(x < y), dim = dim(x)))

  ge <- ax >= 2
  expect_true(inherits(ge, "mojor_gpu_array"))
  expect_equal(.mojor_gpu_array_dtype(ge), "i32")
  expect_equal(attr(ge, "gpu_fallback"), "cpu_compare")
  expect_equal(mojor_gpu_array_read(ge), array(as.integer(x >= 2), dim = dim(x)))

  eq <- ax == ay
  expect_true(inherits(eq, "mojor_gpu_array"))
  expect_equal(.mojor_gpu_array_dtype(eq), "i32")
  expect_equal(attr(eq, "gpu_fallback"), "cpu_compare")
  expect_equal(mojor_gpu_array_read(eq), array(as.integer(x == y), dim = dim(x)))

  not_x <- !ax
  expect_true(inherits(not_x, "mojor_gpu_array"))
  expect_equal(.mojor_gpu_array_dtype(not_x), "i32")
  expect_equal(attr(not_x, "gpu_fallback"), "cpu_logic")
  expect_equal(dim(not_x), dim(x))
  expect_equal(mojor_gpu_array_read(not_x), array(as.integer(!x), dim = dim(x)))

  and_xy <- ax & ay
  expect_true(inherits(and_xy, "mojor_gpu_array"))
  expect_equal(.mojor_gpu_array_dtype(and_xy), "i32")
  expect_equal(attr(and_xy, "gpu_fallback"), "cpu_logic")
  expect_equal(dim(and_xy), dim(x))
  expect_equal(mojor_gpu_array_read(and_xy), array(as.integer(x & y), dim = dim(x)))

  or_x0 <- ax | 0
  expect_true(inherits(or_x0, "mojor_gpu_array"))
  expect_equal(.mojor_gpu_array_dtype(or_x0), "i32")
  expect_equal(attr(or_x0, "gpu_fallback"), "cpu_logic")
  expect_equal(dim(or_x0), dim(x))
  expect_equal(mojor_gpu_array_read(or_x0), array(as.integer(x | 0), dim = dim(x)))

  .mojor_gpu_array_free_all(or_x0, and_xy, not_x, eq, ge, lt, ay, ax)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray Ops comparison/logical keep host parity for recyclable shape mismatch", {  .skip_if_no_gpu_backend(require_float = TRUE)
  mx <- matrix(as.numeric(1:6), nrow = 2L, ncol = 3L)
  my <- as.numeric(c(0, 2, 10))
  mz <- as.numeric(c(1, 2, 3, 4))
  ax <- GPUArray(mx, dtype = "f32")
  ay <- GPUArray(my, dtype = "f32")
  az <- GPUArray(mz, dtype = "f32")

  cmp <- ax < ay
  expect_equal(mojor_gpu_array_read(cmp), array(as.integer(mx < my), dim = dim(mx)))
  expect_equal(attr(cmp, "gpu_fallback"), "cpu_compare")

  expect_warning(
    cmp_warn <- ax >= az,
    "longer object length is not a multiple"
  )
  expect_equal(mojor_gpu_array_read(cmp_warn), array(as.integer(mx >= mz), dim = dim(mx)))
  expect_equal(attr(cmp_warn, "gpu_fallback"), "cpu_compare")

  lg <- ax & ay
  expect_equal(mojor_gpu_array_read(lg), array(as.integer(mx & my), dim = dim(mx)))
  expect_equal(attr(lg, "gpu_fallback"), "cpu_logic")

  expect_warning(
    lg_warn <- ax | az,
    "longer object length is not a multiple"
  )
  expect_equal(mojor_gpu_array_read(lg_warn), array(as.integer(mx | mz), dim = dim(mx)))
  expect_equal(attr(lg_warn, "gpu_fallback"), "cpu_logic")

  .mojor_gpu_array_free_all(lg_warn, lg, cmp_warn, cmp, az, ay, ax)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray Ops arithmetic falls back to host parity for recyclable shape mismatch", {  .skip_if_no_gpu_backend(require_float = TRUE)
  mx <- matrix(as.numeric(1:6), nrow = 2L, ncol = 3L)
  my <- as.numeric(c(10, 20, 30))
  ax <- GPUArray(mx, dtype = "f32")
  ay <- GPUArray(my, dtype = "f32")
  out <- ax + ay
  expect_equal(mojor_gpu_array_read(out), mx + my)
  expect_equal(attr(out, "gpu_fallback"), "cpu_arith")

  mz <- as.numeric(c(1, 2, 3, 4))
  az <- GPUArray(mz, dtype = "f32")
  expect_warning(
    out_warn <- ax + az,
    "longer object length is not a multiple"
  )
  expect_equal(mojor_gpu_array_read(out_warn), mx + mz)
  expect_equal(attr(out_warn, "gpu_fallback"), "cpu_arith")

  .mojor_gpu_array_free_all(out_warn, out, az, ay, ax)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("mojor_gpu_kernel builds a simple GPU kernel", {  .skip_if_no_gpu_backend(require_float = TRUE)
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + 1
    }
    out
  }
  k <- gpu_kernel(f, x = "f32[]", name = "t_gpu_kernel_add1")
  x <- runif(128)
  ax <- mojor_gpu_array(x, dtype = "f32")
  out <- k(ax)
  expect_true(inherits(out, "GPUArray"))
  host <- mojor_gpu_array_read(out)
  expect_true(max(abs(host - (x + 1))) < 1e-5)
  .mojor_gpu_array_free_all(out, ax)
})

test_that("mojor_gpu_kernel supports canonical matrix2d binary elementwise loops", {  .skip_if_no_gpu_backend(require_float = TRUE)
  f <- function(x, y) {
    out <- matrix(0, nrow = nrow(x), ncol = ncol(x))
    for (i in seq_len(nrow(x))) {
      for (j in seq_len(ncol(x))) {
        out[i, j] <- x[i, j] + y[i, j] * 2
      }
    }
    out
  }
  x <- matrix(runif(12), nrow = 3, ncol = 4)
  y <- matrix(runif(12), nrow = 3, ncol = 4)
  k <- gpu_kernel(
    f,
    x = "f32[,]",
    y = "f32[,]",
    name = "t_gpu_kernel_matrix2d_bin",
    cache = FALSE,
    elementwise_size = length(x)
  )
  ax <- mojor_gpu_array(x, dtype = "f32")
  ay <- mojor_gpu_array(y, dtype = "f32")
  out <- k(ax, ay)
  expect_true(inherits(out, "GPUArray"))
  host <- mojor_gpu_array_read(out)
  expect_equal(dim(host), dim(x))
  expect_true(max(abs(as.numeric(host) - as.numeric(x + y * 2))) < 1e-5)
  .mojor_gpu_array_free_all(out, ay, ax)
})

