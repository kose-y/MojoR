# Split from test_gpu_array_06.R.
# Shared utilities are in helper-gpu-array.R.

.mojor_skip_gpu_runtime_file_unless_opt_in()

test_that("GPUArray Summary/mean/which methods provide scalar parity", {  .skip_if_no_gpu_backend()

  summary_method <- getS3method("Summary", "GPUArray", optional = TRUE)
  mean_method <- getS3method("mean", "GPUArray", optional = TRUE)
  which_min_method <- get0("which.min.GPUArray", mode = "function", inherits = TRUE)
  which_max_method <- get0("which.max.GPUArray", mode = "function", inherits = TRUE)
  gpu_which_min_fn <- get0("gpu_which_min", mode = "function", inherits = TRUE)
  gpu_which_max_fn <- get0("gpu_which_max", mode = "function", inherits = TRUE)
  expect_true(is.function(summary_method))
  expect_true(is.function(mean_method))
  expect_true(is.function(which_min_method))
  expect_true(is.function(which_max_method))
  expect_true(is.function(gpu_which_min_fn))
  expect_true(is.function(gpu_which_max_fn))

  x <- matrix(c(-1.5, 2.0, 3.5, 4.0, -2.5, 6.0), nrow = 2L, ncol = 3L)
  gx <- GPUArray(x, dtype = "f32")

  expect_equal(sum(gx), sum(x), tolerance = 1e-5)
  expect_equal(prod(gx), prod(x), tolerance = 1e-4)
  expect_equal(min(gx), min(x), tolerance = 1e-5)
  expect_equal(max(gx), max(x), tolerance = 1e-5)
  expect_equal(sum(gx, 10), sum(x, 10), tolerance = 1e-5)
  expect_equal(mean(gx), mean(x), tolerance = 1e-5)
  expect_equal(mean(gx, trim = 0.25), mean(x, trim = 0.25), tolerance = 1e-5)
  expect_equal(gpu_which_min(gx), which.min(x))
  expect_equal(gpu_which_max(gx), which.max(x))
  expect_error(range(gx), "unsupported Summary operator")

  x_na <- c(NA_real_, 5, NA_real_, 2, 9)
  gx_na <- GPUArray(x_na, dtype = "f32")
  expect_equal(sum(gx_na, na.rm = TRUE), sum(x_na, na.rm = TRUE), tolerance = 1e-5)
  expect_equal(min(gx_na, na.rm = TRUE), min(x_na, na.rm = TRUE), tolerance = 1e-5)
  expect_equal(max(gx_na, na.rm = TRUE), max(x_na, na.rm = TRUE), tolerance = 1e-5)
  expect_equal(mean(gx_na, na.rm = TRUE), mean(x_na, na.rm = TRUE), tolerance = 1e-5)
  expect_equal(gpu_which_min(gx_na), which.min(x_na))
  expect_equal(gpu_which_max(gx_na), which.max(x_na))

  x_zero <- numeric(0)
  gx_zero <- GPUArray(x_zero, dtype = "f32")
  expect_equal(sum(gx_zero), sum(x_zero))
  expect_equal(prod(gx_zero), prod(x_zero))
  expect_equal(suppressWarnings(min(gx_zero)), suppressWarnings(min(x_zero)))
  expect_equal(suppressWarnings(max(gx_zero)), suppressWarnings(max(x_zero)))
  expect_equal(suppressWarnings(mean(gx_zero)), suppressWarnings(mean(x_zero)))

  x_all_na <- c(NA_real_, NA_real_, NA_real_)
  gx_all_na <- GPUArray(x_all_na, dtype = "f32")
  x_all_na_host <- mojor_gpu_array_read(gx_all_na)
  expect_equal(sum(gx_all_na), sum(x_all_na_host))
  expect_equal(sum(gx_all_na, na.rm = TRUE), sum(x_all_na_host, na.rm = TRUE))
  expect_equal(suppressWarnings(min(gx_all_na)), suppressWarnings(min(x_all_na_host)))
  expect_equal(suppressWarnings(max(gx_all_na)), suppressWarnings(max(x_all_na_host)))
  expect_equal(
    suppressWarnings(min(gx_all_na, na.rm = TRUE)),
    suppressWarnings(min(x_all_na_host, na.rm = TRUE))
  )
  expect_equal(
    suppressWarnings(max(gx_all_na, na.rm = TRUE)),
    suppressWarnings(max(x_all_na_host, na.rm = TRUE))
  )
  expect_equal(mean(gx_all_na), mean(x_all_na_host))
  expect_equal(mean(gx_all_na, na.rm = TRUE), mean(x_all_na_host, na.rm = TRUE))

  x_trim_na <- c(NA_real_, -3, -1, 2, 9, NA_real_)
  gx_trim_na <- GPUArray(x_trim_na, dtype = "f32")
  x_trim_na_host <- mojor_gpu_array_read(gx_trim_na)
  expect_equal(
    mean(gx_trim_na, trim = 0.25, na.rm = TRUE),
    mean(x_trim_na_host, trim = 0.25, na.rm = TRUE),
    tolerance = 1e-5
  )
  expect_equal(
    mean(gx_trim_na, trim = 0.25, na.rm = FALSE),
    mean(x_trim_na_host, trim = 0.25, na.rm = FALSE),
    tolerance = 1e-5
  )

  x_i32_edge <- as.integer(c(2147483640L, 7L))
  gx_i32_edge <- GPUArray(x_i32_edge, dtype = "i32")
  expect_equal(sum(gx_i32_edge), sum(x_i32_edge))
  expect_equal(prod(gx_i32_edge), prod(x_i32_edge))
  expect_equal(mean(gx_i32_edge), mean(x_i32_edge))

  .mojor_gpu_array_free_all(gx_i32_edge, gx_trim_na, gx_all_na, gx_zero, gx_na, gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray mixed-dtype arithmetic auto-promotes operands", {  .skip_if_no_gpu_backend(require_float = TRUE)

  x <- matrix(as.numeric(1:6), nrow = 2L)
  y <- matrix(as.integer(11:16), nrow = 2L)
  gx <- GPUArray(x, dtype = "f32")
  gy <- GPUArray(y, dtype = "i32")
  promoted <- gpu_promote(gx, gy)

  out <- gx + gy
  expect_true(inherits(out, "mojor_gpu_array"))
  expect_equal(.mojor_gpu_array_dtype(out), promoted$dtype)
  expect_equal(mojor_gpu_array_read(out), x + as.numeric(y))

  gi <- GPUArray(as.integer(c(1L, 2L, 3L, 4L)), dtype = "i32")
  out_host_rhs <- gi + 0.5
  expect_true(.mojor_gpu_array_dtype(out_host_rhs) %in% c("f32", "f64"))
  expect_equal(mojor_gpu_array_read(out_host_rhs), as.numeric(c(1L, 2L, 3L, 4L)) + 0.5)

  out_host_vec <- gi + as.integer(c(10L, 20L, 30L, 40L))
  expect_equal(as.integer(mojor_gpu_array_read(out_host_vec)), as.integer(c(11L, 22L, 33L, 44L)))

  gf <- GPUArray(as.numeric(c(1, 2, 3, 4)), dtype = "f32")
  out_host_lhs <- 1L + gf
  expect_equal(.mojor_gpu_array_dtype(out_host_lhs), "f32")
  expect_equal(
    mojor_gpu_array_read(out_host_lhs),
    as.numeric(1L) + as.numeric(c(1, 2, 3, 4)),
    tolerance = 1e-5
  )

  .mojor_gpu_array_free_all(
    out_host_lhs, gf, out_host_vec, out_host_rhs, gi, out, promoted$x,
    promoted$y, gy, gx
  )
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray mixed-dtype linalg auto-promotes operands", {  .skip_if_no_gpu_backend(require_float = TRUE)

  mm_method <- getS3method("%*%", "GPUArray", optional = TRUE)
  cp_method <- get0("crossprod.GPUArray", mode = "function", inherits = TRUE)
  tcp_method <- get0("tcrossprod.GPUArray", mode = "function", inherits = TRUE)
  expect_true(is.function(mm_method))
  expect_true(is.function(cp_method))
  expect_true(is.function(tcp_method))

  x <- matrix(as.numeric(1:6), nrow = 2L, ncol = 3L)
  y <- matrix(as.integer(c(2L, 1L, 0L, 3L, 4L, 5L)), nrow = 3L, ncol = 2L)
  gx <- GPUArray(x, dtype = "f32")
  gy <- GPUArray(y, dtype = "i32")

  mm <- mm_method(gx, gy)
  expect_equal(.mojor_gpu_array_dtype(mm), "f32")
  expect_equal(mojor_gpu_array_read(mm), x %*% matrix(as.numeric(y), nrow = 3L, ncol = 2L), tolerance = 1e-5)

  x_host_i32 <- matrix(as.integer(1:6), nrow = 2L, ncol = 3L)
  y_host_i32 <- matrix(as.integer(c(2L, 1L, 0L, 3L, 4L, 5L)), nrow = 3L, ncol = 2L)
  mm_host <- gpu_matmul(x_host_i32, y_host_i32)
  expect_equal(.mojor_gpu_array_dtype(mm_host), "i32")
  expect_equal(
    as.integer(mojor_gpu_array_read(mm_host)),
    as.integer(
      matrix(as.integer(x_host_i32), nrow = 2L, ncol = 3L) %*%
        matrix(as.integer(y_host_i32), nrow = 3L, ncol = 2L)
    )
  )

  mm_direct <- gpu_matmul(gx, gy)
  expect_equal(.mojor_gpu_array_dtype(mm_direct), "f32")
  expect_equal(mojor_gpu_array_read(mm_direct), x %*% matrix(as.numeric(y), nrow = 3L, ncol = 2L), tolerance = 1e-5)

  out_i32 <- gpu_zeros(dim = c(2L, 2L), dtype = "i32")
  mm_into <- gpu_matmul_into(out_i32, gx, gy)
  expect_identical(mm_into, out_i32)
  expect_equal(.mojor_gpu_array_dtype(mm_into), "i32")
  expect_equal(
    as.integer(mojor_gpu_array_read(mm_into)),
    as.integer(
      matrix(as.integer(x), nrow = 2L, ncol = 3L) %*%
        matrix(as.integer(y), nrow = 3L, ncol = 2L)
    )
  )
  expect_true(attr(mm_into, "gpu_fallback") %in% c("cpu_matmul", "gpu_matmul"))
  mm_into_host <- gpu_matmul_into(out_i32, x, y)
  expect_identical(mm_into_host, out_i32)
  expect_equal(.mojor_gpu_array_dtype(mm_into_host), "i32")
  expect_equal(
    as.integer(mojor_gpu_array_read(mm_into_host)),
    as.integer(
      matrix(as.integer(x), nrow = 2L, ncol = 3L) %*%
        matrix(as.integer(y), nrow = 3L, ncol = 2L)
    )
  )
  expect_true(attr(mm_into_host, "gpu_fallback") %in% c("cpu_matmul", "gpu_matmul"))

  x_cp <- matrix(as.numeric(c(1, 2, 3, 4, 5, 6)), nrow = 3L, ncol = 2L)
  y_cp <- matrix(as.integer(c(6L, 5L, 4L, 3L, 2L, 1L)), nrow = 3L, ncol = 2L)
  gx_cp <- GPUArray(x_cp, dtype = "f32")
  gy_cp <- GPUArray(y_cp, dtype = "i32")

  cp <- cp_method(gx_cp, gy_cp)
  expect_equal(.mojor_gpu_array_dtype(cp), "f32")
  expect_equal(mojor_gpu_array_read(cp), crossprod(x_cp, matrix(as.numeric(y_cp), nrow = 3L, ncol = 2L)), tolerance = 1e-5)
  expect_true(attr(cp, "gpu_fallback") %in% c("cpu_crossprod", "gpu_crossprod"))

  tcp <- tcp_method(gx_cp, gy_cp)
  expect_equal(.mojor_gpu_array_dtype(tcp), "f32")
  expect_equal(mojor_gpu_array_read(tcp), tcrossprod(x_cp, matrix(as.numeric(y_cp), nrow = 3L, ncol = 2L)), tolerance = 1e-5)
  expect_true(attr(tcp, "gpu_fallback") %in% c("cpu_tcrossprod", "gpu_tcrossprod"))

  .mojor_gpu_array_free_all(tcp, cp, gy_cp, gx_cp, mm_host, mm_into, out_i32, mm_direct, mm, gy, gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray has explicit shape and conversion methods", {  .skip_if_no_gpu_backend(require_float = TRUE)

  len_method <- getS3method("length", "GPUArray", optional = TRUE)
  dim_method <- getS3method("dim", "GPUArray", optional = TRUE)
  dimnames_method <- getS3method("dimnames", "GPUArray", optional = TRUE)
  arr_method <- get0("as.array.GPUArray", mode = "function", inherits = TRUE)
  mat_method <- get0("as.matrix.GPUArray", mode = "function", inherits = TRUE)
  num_method <- get0("as.numeric.GPUArray", mode = "function", inherits = TRUE)
  expect_true(is.function(len_method))
  expect_true(is.function(dim_method))
  expect_true(is.function(dimnames_method))
  expect_true(is.function(arr_method))
  expect_true(is.function(mat_method))
  expect_true(is.function(num_method))

  m <- matrix(as.numeric(1:6), nrow = 2L, dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
  gx <- GPUArray(m, dtype = "f32")
  expect_equal(len_method(gx), 6L)
  expect_equal(dim_method(gx), c(2L, 3L))
  expect_equal(dimnames_method(gx), dimnames(m))
  expect_equal(arr_method(gx), m)
  expect_equal(mat_method(gx), m)
  expect_equal(num_method(gx), as.numeric(m))

  mojor_gpu_array_free(gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray has explicit print method", {  .skip_if_no_gpu_backend(require_float = TRUE)

  print_method <- getS3method("print", "GPUArray", optional = TRUE)
  expect_true(is.function(print_method))

  gx <- GPUArray(matrix(as.numeric(1:6), nrow = 2L, ncol = 3L), dtype = "f32")
  out <- capture.output(print_method(gx))
  expect_true(any(grepl("^mojor_gpu_array<", out)))
  expect_true(any(grepl(", f32\\]", out)))
  expect_true(any(grepl("dim=2x3", out, fixed = TRUE)))

  mojor_gpu_array_free(gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("gpu_reduce supports dims via host fallback", {  .skip_if_no_gpu_backend(require_float = TRUE)

  x <- matrix(1:12, nrow = 3L, ncol = 4L)
  gx <- GPUArray(x, dtype = "f32")

  cols <- gpu_reduce(gx, "sum", dims = 1L, keepdims = FALSE)
  expect_equal(mojor_gpu_array_read(cols), colSums(x))
  expect_true(attr(cols, "gpu_fallback") %in% c("gpu_reduce", "cpu_reduce"))

  cols_keep <- gpu_reduce(gx, "sum", dims = 1L, keepdims = TRUE)
  expect_equal(dim(mojor_gpu_array_read(cols_keep)), c(1L, 4L))
  expect_equal(mojor_gpu_array_read(cols_keep), array(colSums(x), dim = c(1L, 4L)))

  rows <- gpu_reduce(gx, "mean", dims = 2L, keepdims = TRUE)
  expect_equal(dim(mojor_gpu_array_read(rows)), c(3L, 1L))
  expect_equal(mojor_gpu_array_read(rows), array(rowMeans(x), dim = c(3L, 1L)))

  all <- gpu_reduce(gx, "min", dims = c(1L, 2L), keepdims = FALSE)
  expect_equal(mojor_gpu_array_read(all), min(x))
  expect_true(attr(all, "gpu_fallback") %in% c("gpu_reduce", "cpu_reduce"))

  scalar_keep <- gpu_reduce(gx, "mean", dims = c(1L, 2L), keepdims = TRUE)
  expect_equal(dim(mojor_gpu_array_read(scalar_keep)), c(1L, 1L))
  expect_equal(mojor_gpu_array_read(scalar_keep), array(mean(x), dim = c(1L, 1L)))

  gv <- GPUArray(1:12, dtype = "f32")
  vout <- gpu_reduce(gv, "sum", dims = 1L, keepdims = FALSE)
  expect_equal(mojor_gpu_array_read(vout), sum(1:12))
  expect_equal(vout$dim, NULL)
  expect_equal(vout$n, 1L)
  expect_true(attr(vout, "gpu_fallback") %in% c("gpu_reduce", "cpu_reduce"))
  .mojor_gpu_array_free_all(vout, gv)

  expect_error(gpu_reduce(gx, "sum", dims = 0L), "invalid dims")
  expect_error(gpu_reduce(gx, "sum", dims = 3L), "invalid dims")

  .mojor_gpu_array_free_all(scalar_keep, all, rows, cols_keep, cols, gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("gpu_reduce routes full-axis dims through scalar backend path", {  .skip_if_no_gpu_backend()

  x <- matrix(as.numeric(1:6), nrow = 2L, ncol = 3L)
  gx <- GPUArray(x, dtype = "f32")

  reduce_env <- environment(.mojor_gpu_reduce)
  orig_reduce_call <- get(".mojor_gpu_reduce_call", envir = reduce_env, inherits = FALSE)
  captured_dims <- list()
  assign(
    ".mojor_gpu_reduce_call",
    function(dtype, handle, op_code, dims = NULL, keepdims_i = 0L, require_label = NULL) {
      captured_dims[length(captured_dims) + 1L] <<- list(dims)
      stop("forced gpu reduce call failure")
    },
    envir = reduce_env
  )
  on.exit(assign(".mojor_gpu_reduce_call", orig_reduce_call, envir = reduce_env), add = TRUE)

  all_out <- gpu_reduce(gx, "sum", dims = c(2L, 1L), keepdims = FALSE)
  part_out <- gpu_reduce(gx, "sum", dims = 1L, keepdims = FALSE)

  expect_equal(attr(all_out, "gpu_fallback"), "cpu_reduce")
  expect_equal(attr(part_out, "gpu_fallback"), "cpu_reduce")
  expect_equal(mojor_gpu_array_read(all_out), sum(x))
  expect_equal(mojor_gpu_array_read(part_out), colSums(x))

  expect_null(captured_dims[[1L]])
  expect_true(is.integer(captured_dims[[2L]]))
  expect_true(any(captured_dims[[2L]] < 0L))

  .mojor_gpu_array_free_all(part_out, all_out, gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

