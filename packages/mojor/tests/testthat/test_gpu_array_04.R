# Split from test_gpu_array.R (chunk 04).
# Shared utilities are in helper-gpu-array.R.

.mojor_skip_gpu_runtime_file_unless_opt_in()

test_that("GPUArray bracket indexing routes through gpu_slice for rank-1 and rank-2", {  .skip_if_no_gpu_backend(require_float = TRUE)

  gv <- GPUArray(as.numeric(1:10), dtype = "f32")
  v_slice <- gv[3:7]
  expect_true(inherits(v_slice, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(v_slice), as.numeric(3:7))
  expect_true(attr(v_slice, "gpu_fallback") %in% c("cpu_slice", "gpu_slice"))

  gx <- GPUArray(matrix(as.numeric(1:12), nrow = 3L, ncol = 4L), dtype = "f32")
  block <- gx[2:3, 2:4]
  expect_true(inherits(block, "mojor_gpu_array"))
  expect_equal(dim(block), c(2L, 3L))
  expect_equal(mojor_gpu_array_read(block), matrix(c(5, 6, 8, 9, 11, 12), nrow = 2L))

  row_v <- gx[2, 2:4]
  expect_true(inherits(row_v, "mojor_gpu_array"))
  expect_null(dim(row_v))
  expect_equal(mojor_gpu_array_read(row_v), c(5, 8, 11))

  scalar <- gx[3, 4]
  expect_true(is.numeric(scalar))
  expect_equal(as.numeric(scalar), 12)
  scalar_drop_int <- gx[3, 4, drop = 1L]
  expect_true(is.numeric(scalar_drop_int))
  expect_equal(as.numeric(scalar_drop_int), 12)
  scalar_drop_char <- gx[3, 4, drop = "TRUE"]
  expect_true(is.numeric(scalar_drop_char))
  expect_equal(as.numeric(scalar_drop_char), 12)
  scalar_keep_zero <- gx[3, 4, drop = 0L]
  expect_true(inherits(scalar_keep_zero, "mojor_gpu_array"))
  expect_equal(dim(scalar_keep_zero), c(1L, 1L))
  expect_equal(mojor_gpu_array_read(scalar_keep_zero), matrix(12, nrow = 1L, ncol = 1L))
  expect_error(gx[1, 1, drop = c(TRUE, FALSE)], "drop must")
  expect_error(gx[1, 1, drop = NA], "drop must")
  expect_error(gx[1, 1, drop = "not_bool"], "drop must")

  .mojor_gpu_array_free_all(scalar_keep_zero, row_v, block, gx, v_slice, gv)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray bracket indexing enforces strict supported forms", {  .skip_if_no_gpu_backend(require_float = TRUE)

  gx <- GPUArray(matrix(as.numeric(1:12), nrow = 3L, ncol = 4L), dtype = "f32")
  empty_col <- gx[integer(0), 1]
  expect_true(inherits(empty_col, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(empty_col), numeric(0))
  expect_equal(attr(empty_col, "gpu_fallback"), "gpu_gather")
  empty_col_nd <- gx[integer(0), 1, drop = FALSE]
  expect_true(inherits(empty_col_nd, "mojor_gpu_array"))
  expect_equal(dim(empty_col_nd), c(0L, 1L))
  expect_equal(
    mojor_gpu_array_read(empty_col_nd),
    matrix(numeric(0), nrow = 0L, ncol = 1L)
  )
  expect_equal(attr(empty_col_nd, "gpu_fallback"), "gpu_gather")
  zero_col <- gx[0, 1]
  expect_true(inherits(zero_col, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(zero_col), numeric(0))
  expect_equal(attr(zero_col, "gpu_fallback"), "gpu_gather")
  zero_col_nd <- gx[0, 1, drop = FALSE]
  expect_true(inherits(zero_col_nd, "mojor_gpu_array"))
  expect_equal(dim(zero_col_nd), c(0L, 1L))
  expect_equal(
    mojor_gpu_array_read(zero_col_nd),
    matrix(numeric(0), nrow = 0L, ncol = 1L)
  )
  expect_equal(attr(zero_col_nd, "gpu_fallback"), "gpu_gather")
  scalar_trailing <- gx[2L, 3L, ]
  expect_true(is.numeric(scalar_trailing))
  expect_equal(as.numeric(scalar_trailing), 8)
  scalar_extra_one <- gx[1L, 1L, 1L]
  expect_true(is.numeric(scalar_extra_one))
  expect_equal(as.numeric(scalar_extra_one), 1)
  scalar_extra_true <- gx[1L, 1L, TRUE]
  expect_true(is.numeric(scalar_extra_true))
  expect_equal(as.numeric(scalar_extra_true), 1)
  expect_error(gx[matrix(c(TRUE, FALSE, TRUE, FALSE), nrow = 2L), 2])
  scalar_extra_two <- gx[1L, 1L, 2L]
  expect_true(is.numeric(scalar_extra_two))
  expect_equal(as.numeric(scalar_extra_two), 1)

  gv <- GPUArray(as.numeric(1:8), dtype = "f32")
  gv_rank1_j <- gv[1, 1]
  expect_true(inherits(gv_rank1_j, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(gv_rank1_j), 1)
  expect_true(attr(gv_rank1_j, "gpu_fallback") %in% c("gpu_slice", "cpu_slice"))
  gv_rank1_j_two <- gv[1, 2]
  expect_true(inherits(gv_rank1_j_two, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(gv_rank1_j_two), 1)
  expect_true(attr(gv_rank1_j_two, "gpu_fallback") %in% c("gpu_slice", "cpu_slice"))
  gv_rank1_j_trailing <- gv[2, 1, ]
  expect_true(inherits(gv_rank1_j_trailing, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(gv_rank1_j_trailing), 2)
  expect_true(attr(gv_rank1_j_trailing, "gpu_fallback") %in% c("gpu_slice", "cpu_slice"))
  gv_rank1_j_extra <- gv[3, 1, 1]
  expect_true(inherits(gv_rank1_j_extra, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(gv_rank1_j_extra), 3)
  expect_true(attr(gv_rank1_j_extra, "gpu_fallback") %in% c("gpu_slice", "cpu_slice"))
  gv_rank1_j_null <- gv[4, NULL]
  expect_true(inherits(gv_rank1_j_null, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(gv_rank1_j_null), 4)
  expect_true(attr(gv_rank1_j_null, "gpu_fallback") %in% c("gpu_slice", "cpu_slice"))
  gv_rank1_j_empty <- gv[5, integer(0)]
  expect_true(inherits(gv_rank1_j_empty, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(gv_rank1_j_empty), 5)
  expect_true(attr(gv_rank1_j_empty, "gpu_fallback") %in% c("gpu_slice", "cpu_slice"))
  gv_rank1_j_extra_null <- gv[6, 1, NULL]
  expect_true(inherits(gv_rank1_j_extra_null, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(gv_rank1_j_extra_null), 6)
  expect_true(attr(gv_rank1_j_extra_null, "gpu_fallback") %in% c("gpu_slice", "cpu_slice"))
  gv_rank1_j_extra_empty <- gv[7, 1, integer(0)]
  expect_true(inherits(gv_rank1_j_extra_empty, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(gv_rank1_j_extra_empty), 7)
  expect_true(attr(gv_rank1_j_extra_empty, "gpu_fallback") %in% c("gpu_slice", "cpu_slice"))
  gv_rank1_j_true_vec <- gv[2, c(TRUE, TRUE)]
  expect_true(inherits(gv_rank1_j_true_vec, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(gv_rank1_j_true_vec), 2)
  expect_true(attr(gv_rank1_j_true_vec, "gpu_fallback") %in% c("gpu_slice", "cpu_slice"))
  gv_rank1_j_one_vec <- gv[3, c(1L, 1L, 1L)]
  expect_true(inherits(gv_rank1_j_one_vec, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(gv_rank1_j_one_vec), 3)
  expect_true(attr(gv_rank1_j_one_vec, "gpu_fallback") %in% c("gpu_slice", "cpu_slice"))
  gv_rank1_j_extra_true_vec <- gv[4, 1, c(TRUE, TRUE)]
  expect_true(inherits(gv_rank1_j_extra_true_vec, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(gv_rank1_j_extra_true_vec), 4)
  expect_true(attr(gv_rank1_j_extra_true_vec, "gpu_fallback") %in% c("gpu_slice", "cpu_slice"))
  gv_rank1_j_zero <- gv[3, 0]
  expect_true(inherits(gv_rank1_j_zero, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(gv_rank1_j_zero), numeric(0))
  expect_true(attr(gv_rank1_j_zero, "gpu_fallback") %in% c("gpu_gather", "cpu_gather"))
  gv_rank1_j_zero_vec <- gv[3, c(0L, 0L)]
  expect_true(inherits(gv_rank1_j_zero_vec, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(gv_rank1_j_zero_vec), numeric(0))
  expect_true(attr(gv_rank1_j_zero_vec, "gpu_fallback") %in% c("gpu_gather", "cpu_gather"))
  gv_rank1_j_false <- gv[3, FALSE]
  expect_true(inherits(gv_rank1_j_false, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(gv_rank1_j_false), numeric(0))
  expect_true(attr(gv_rank1_j_false, "gpu_fallback") %in% c("gpu_gather", "cpu_gather"))
  gv_rank1_j_false_vec <- gv[3, c(FALSE, FALSE)]
  expect_true(inherits(gv_rank1_j_false_vec, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(gv_rank1_j_false_vec), numeric(0))
  expect_true(attr(gv_rank1_j_false_vec, "gpu_fallback") %in% c("gpu_gather", "cpu_gather"))
  gv_rank1_j_extra_zero <- gv[3, 1, 0]
  expect_true(inherits(gv_rank1_j_extra_zero, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(gv_rank1_j_extra_zero), numeric(0))
  expect_true(attr(gv_rank1_j_extra_zero, "gpu_fallback") %in% c("gpu_gather", "cpu_gather"))
  gv_rank1_j_extra_false_vec <- gv[4, 1, c(FALSE, FALSE)]
  expect_true(inherits(gv_rank1_j_extra_false_vec, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(gv_rank1_j_extra_false_vec), numeric(0))
  expect_true(attr(gv_rank1_j_extra_false_vec, "gpu_fallback") %in% c("gpu_gather", "cpu_gather"))
  gv_rank1_j_mixed_bool <- gv[1, c(TRUE, FALSE)]
  expect_true(inherits(gv_rank1_j_mixed_bool, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(gv_rank1_j_mixed_bool), numeric(0))
  expect_true(attr(gv_rank1_j_mixed_bool, "gpu_fallback") %in% c("gpu_gather", "cpu_gather"))
  gv_rank1_j_extra_mixed <- gv[1, 1, c(TRUE, FALSE)]
  expect_true(inherits(gv_rank1_j_extra_mixed, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(gv_rank1_j_extra_mixed), numeric(0))
  expect_true(attr(gv_rank1_j_extra_mixed, "gpu_fallback") %in% c("gpu_gather", "cpu_gather"))
  scalar_extra_mixed_empty <- gx[1L, 1L, c(1L, 0L)]
  expect_true(inherits(scalar_extra_mixed_empty, "mojor_gpu_array"))
  expect_length(mojor_gpu_array_read(scalar_extra_mixed_empty), 0L)
  expect_true(attr(scalar_extra_mixed_empty, "gpu_fallback") %in% c("gpu_gather", "cpu_gather"))
  gv_rank1_j_extra_two <- gv[1, 1, 2]
  expect_true(inherits(gv_rank1_j_extra_two, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(gv_rank1_j_extra_two), 1)
  expect_true(attr(gv_rank1_j_extra_two, "gpu_fallback") %in% c("gpu_slice", "cpu_slice"))
  expect_error(gv[c(-1L, 2L)], "cannot mix positive and negative indices")

  .mojor_gpu_array_free_all(
    gv_rank1_j_extra_two, scalar_extra_two, scalar_extra_mixed_empty,
    gv_rank1_j_extra_mixed, gv_rank1_j_mixed_bool,
    gv_rank1_j_extra_false_vec, gv_rank1_j_extra_zero, gv_rank1_j_false_vec,
    gv_rank1_j_false, gv_rank1_j_zero_vec, gv_rank1_j_zero, gv_rank1_j_extra_true_vec,
    gv_rank1_j_one_vec, gv_rank1_j_true_vec, gv_rank1_j_extra_empty,
    gv_rank1_j_extra_null, gv_rank1_j_empty, gv_rank1_j_null, gv_rank1_j_extra,
    gv_rank1_j_trailing, gv_rank1_j_two, gv_rank1_j, zero_col_nd, zero_col, empty_col_nd,
    empty_col, gv, gx
  )
})

test_that("GPUArray bracket indexing supports zero, negative, and reverse strided integer subscripts", {  .skip_if_no_gpu_backend(require_float = TRUE)

  v <- as.numeric(1:10)
  gv <- GPUArray(v, dtype = "f32")

  out_zero <- gv[c(0L, 3L, 0L, 5L)]
  expect_equal(mojor_gpu_array_read(out_zero), v[c(0L, 3L, 0L, 5L)])
  expect_true(attr(out_zero, "gpu_fallback") %in% c("gpu_slice", "cpu_slice", "gpu_gather", "cpu_gather"))

  out_neg <- gv[-c(1L, 4L, 8L)]
  expect_equal(mojor_gpu_array_read(out_neg), v[-c(1L, 4L, 8L)])
  expect_true(attr(out_neg, "gpu_fallback") %in% c("gpu_slice", "cpu_slice", "gpu_gather", "cpu_gather"))

  out_rev <- gv[9:3]
  expect_equal(mojor_gpu_array_read(out_rev), v[9:3])
  expect_true(attr(out_rev, "gpu_fallback") %in% c("gpu_slice", "cpu_slice"))

  m <- matrix(as.numeric(1:12), nrow = 3L, ncol = 4L)
  gx <- GPUArray(m, dtype = "f32")

  out_m_neg <- gx[-1L, -c(2L, 4L), drop = FALSE]
  expect_equal(mojor_gpu_array_read(out_m_neg), m[-1L, -c(2L, 4L), drop = FALSE])
  expect_true(attr(out_m_neg, "gpu_fallback") %in% c("gpu_slice", "cpu_slice", "gpu_gather", "cpu_gather"))

  out_m_rev <- gx[3:1, 4:2, drop = FALSE]
  expect_equal(mojor_gpu_array_read(out_m_rev), m[3:1, 4:2, drop = FALSE])
  expect_true(attr(out_m_rev, "gpu_fallback") %in% c("gpu_slice", "cpu_slice"))

  .mojor_gpu_array_free_all(out_m_rev, out_m_neg, gx, out_rev, out_neg, out_zero, gv)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray bracket indexing supports matrix and logical-matrix selectors via cpu gather parity", {  .skip_if_no_gpu_backend(require_float = TRUE)

  host <- matrix(as.numeric(1:12), nrow = 3L, ncol = 4L)
  gx <- GPUArray(host, dtype = "f32")

  idx_num <- rbind(c(1L, 1L), c(3L, 4L), c(2L, 2L))
  out_num <- gx[idx_num]
  expect_true(inherits(out_num, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(out_num), host[idx_num])
  expect_equal(attr(out_num, "gpu_fallback"), "cpu_gather")

  idx_lgl <- matrix(rep(c(TRUE, FALSE, FALSE), 4L), nrow = 3L, ncol = 4L)
  out_lgl <- gx[idx_lgl]
  expect_true(inherits(out_lgl, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(out_lgl), host[idx_lgl])
  expect_equal(attr(out_lgl, "gpu_fallback"), "cpu_gather")

  .mojor_gpu_array_free_all(out_lgl, out_num, gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray bracket indexing falls back to host parity for factor and NA integer selectors", {  .skip_if_no_gpu_backend(require_float = TRUE)

  host <- matrix(as.numeric(1:12), nrow = 3L, ncol = 4L)
  gx <- GPUArray(host, dtype = "f32")

  idx_factor <- factor(c(3L, 1L), levels = c(1L, 2L, 3L))
  out_factor <- gx[idx_factor, c(4L, 2L), drop = FALSE]
  expect_true(inherits(out_factor, "mojor_gpu_array"))
  expect_equal(
    mojor_gpu_array_read(out_factor),
    host[idx_factor, c(4L, 2L), drop = FALSE]
  )
  expect_equal(attr(out_factor, "gpu_fallback"), "cpu_gather")

  out_na <- gx[c(1L, NA_integer_, 3L), 2L]
  expect_true(inherits(out_na, "mojor_gpu_array"))
  expect_equal(
    mojor_gpu_array_read(out_na),
    host[c(1L, NA_integer_, 3L), 2L]
  )
  expect_equal(attr(out_na, "gpu_fallback"), "cpu_gather")

  .mojor_gpu_array_free_all(out_na, out_factor, gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray bracket indexing falls back to host parity for ND NULL selectors", {  .skip_if_no_gpu_backend(require_float = TRUE)

  host <- array(
    as.numeric(1:24),
    dim = c(2L, 3L, 4L),
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"), c("z1", "z2", "z3", "z4"))
  )
  ga <- GPUArray(host, dtype = "f32")

  out_a <- ga[c(TRUE, FALSE), NULL, c(TRUE, FALSE)]
  expect_true(inherits(out_a, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(out_a), host[c(TRUE, FALSE), NULL, c(TRUE, FALSE)])
  expect_equal(attr(out_a, "gpu_fallback"), "cpu_gather")

  out_b <- ga[1L, c(0L, 2L), NULL]
  expect_true(inherits(out_b, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(out_b), host[1L, c(0L, 2L), NULL])
  expect_equal(attr(out_b, "gpu_fallback"), "cpu_gather")

  out_c <- ga[NULL, -1L, c(TRUE, FALSE, TRUE, FALSE)]
  expect_true(inherits(out_c, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(out_c), host[NULL, -1L, c(TRUE, FALSE, TRUE, FALSE)])
  expect_equal(attr(out_c, "gpu_fallback"), "cpu_gather")

  .mojor_gpu_array_free_all(out_c, out_b, out_a, ga)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray supports logical mask extraction with single index", {  .skip_if_no_gpu_backend(require_float = TRUE)

  host <- matrix(as.numeric(1:12), nrow = 3L, ncol = 4L)
  gx <- GPUArray(host, dtype = "f32")
  mask <- rep(c(TRUE, FALSE), length.out = length(host))
  out <- gx[mask]
  expect_true(inherits(out, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(out), as.vector(host)[mask])
  expect_true(attr(out, "gpu_fallback") %in% c("gpu_gather", "cpu_gather"))
  expect_error(gx[c(TRUE, NA)], "must not contain NA")
  expect_error(gx[c(TRUE, FALSE)], "mask length must equal length\\(x\\)")

  .mojor_gpu_array_free_all(out, gx)
})

test_that("GPUArray supports strict per-axis logical mask extraction", {  .skip_if_no_gpu_backend(require_float = TRUE)

  host_m <- matrix(as.numeric(1:20), nrow = 4L, ncol = 5L)
  gx <- GPUArray(host_m, dtype = "f32")

  g1 <- gx[c(TRUE, FALSE, TRUE, FALSE), c(1L, 3L, 5L)]
  expect_equal(mojor_gpu_array_read(g1), host_m[c(TRUE, FALSE, TRUE, FALSE), c(1L, 3L, 5L), drop = TRUE])
  expect_true(attr(g1, "gpu_fallback") %in% c("gpu_gather", "cpu_gather"))

  g2 <- gx[c(1L, 4L), c(TRUE, FALSE, TRUE, FALSE, TRUE)]
  expect_equal(mojor_gpu_array_read(g2), host_m[c(1L, 4L), c(TRUE, FALSE, TRUE, FALSE, TRUE), drop = TRUE])
  expect_true(attr(g2, "gpu_fallback") %in% c("gpu_gather", "cpu_gather"))

  host_a <- array(as.numeric(1:60), dim = c(3L, 4L, 5L))
  ga <- GPUArray(host_a, dtype = "f32")
  g3 <- ga[c(TRUE, FALSE, TRUE), c(1L, 4L), c(TRUE, FALSE, TRUE, FALSE, TRUE)]
  expect_equal(
    mojor_gpu_array_read(g3),
    host_a[c(TRUE, FALSE, TRUE), c(1L, 4L), c(TRUE, FALSE, TRUE, FALSE, TRUE), drop = TRUE]
  )
  expect_true(attr(g3, "gpu_fallback") %in% c("gpu_gather", "cpu_gather"))

  expect_error(gx[c(TRUE, NA, FALSE, TRUE), c(1L, 3L)], "logical mask must not contain NA")
  expect_error(gx[c(TRUE, FALSE), c(1L, 3L)], "logical mask length must equal extent")
  g_empty <- gx[c(FALSE, FALSE, FALSE, FALSE), c(1L, 3L)]
  expect_true(inherits(g_empty, "mojor_gpu_array"))
  expect_equal(
    mojor_gpu_array_read(g_empty),
    host_m[c(FALSE, FALSE, FALSE, FALSE), c(1L, 3L), drop = TRUE]
  )
  expect_equal(attr(g_empty, "gpu_fallback"), "gpu_gather")
  g_matrix_mask <- gx[matrix(c(TRUE, FALSE, TRUE, FALSE), nrow = 2L), c(1L, 3L)]
  expect_true(inherits(g_matrix_mask, "mojor_gpu_array"))
  expect_equal(
    mojor_gpu_array_read(g_matrix_mask),
    host_m[matrix(c(TRUE, FALSE, TRUE, FALSE), nrow = 2L), c(1L, 3L), drop = TRUE]
  )
  expect_equal(attr(g_matrix_mask, "gpu_fallback"), "cpu_gather")

  .mojor_gpu_array_free_all(g_matrix_mask, g_empty, g3, ga, g2, g1, gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray logical mask compatibility modes are available", {  .skip_if_no_gpu_backend(require_float = TRUE)

  old_mode <- getOption("mojor.gpu.logical_mask_mode")
  on.exit({
    if (is.null(old_mode)) {
      options(mojor.gpu.logical_mask_mode = NULL)
    } else {
      options(mojor.gpu.logical_mask_mode = old_mode)
    }
  }, add = TRUE)

  host <- matrix(as.numeric(1:12), nrow = 3L, ncol = 4L)
  gx <- GPUArray(host, dtype = "f32")

  options(mojor.gpu.logical_mask_mode = "compat")

  mask_recycle <- c(TRUE, FALSE, TRUE)
  mask_recycle_norm <- rep_len(mask_recycle, length(host))
  out_recycle <- gx[mask_recycle]
  expect_equal(mojor_gpu_array_read(out_recycle), as.vector(host)[mask_recycle_norm])
  expect_true(attr(out_recycle, "gpu_fallback") %in% c("gpu_gather", "cpu_gather"))

  mask_na <- c(TRUE, NA, FALSE, TRUE)
  mask_na_norm <- rep_len(mask_na, length(host))
  mask_na_norm[is.na(mask_na_norm)] <- FALSE
  out_na <- gx[mask_na]
  expect_equal(mojor_gpu_array_read(out_na), as.vector(host)[mask_na_norm])
  expect_true(attr(out_na, "gpu_fallback") %in% c("gpu_gather", "cpu_gather"))

  row_mask <- c(TRUE, FALSE)
  row_mask_norm <- rep_len(row_mask, nrow(host))
  col_mask <- c(TRUE, NA, FALSE)
  col_mask_norm <- rep_len(col_mask, ncol(host))
  col_mask_norm[is.na(col_mask_norm)] <- FALSE
  out_axis <- gx[row_mask, col_mask]
  expect_equal(
    mojor_gpu_array_read(out_axis),
    host[row_mask_norm, col_mask_norm, drop = TRUE]
  )
  expect_true(attr(out_axis, "gpu_fallback") %in% c("gpu_gather", "cpu_gather"))

  options(mojor.gpu.logical_mask_mode = "compat_warn")
  expect_warning(
    out_warn_recycle <- gx[c(TRUE, FALSE)],
    "logical mask length recycled"
  )
  expect_warning(
    out_warn_na <- gx[replace(rep(TRUE, length(host)), 2L, NA)],
    "logical mask NA treated as FALSE"
  )

  .mojor_gpu_array_free_all(out_warn_na, out_warn_recycle, out_axis, out_na, out_recycle, gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray supports strict per-axis logical mask extraction for rank-4/5", {  .skip_if_no_gpu_backend(require_float = TRUE)

  host4 <- array(as.numeric(1:120), dim = c(2L, 3L, 4L, 5L))
  g4 <- GPUArray(host4, dtype = "f32")
  out4 <- g4[c(TRUE, FALSE), c(1L, 3L), c(TRUE, FALSE, TRUE, FALSE), c(2L, 5L)]
  expect_equal(
    mojor_gpu_array_read(out4),
    host4[c(TRUE, FALSE), c(1L, 3L), c(TRUE, FALSE, TRUE, FALSE), c(2L, 5L), drop = TRUE]
  )
  expect_true(attr(out4, "gpu_fallback") %in% c("gpu_gather", "cpu_gather"))

  host5 <- array(as.numeric(1:144), dim = c(2L, 3L, 2L, 4L, 3L))
  g5 <- GPUArray(host5, dtype = "f32")
  out5 <- g5[c(TRUE, FALSE), c(1L, 3L), c(TRUE, FALSE), c(2L, 4L), c(TRUE, FALSE, TRUE)]
  expect_equal(
    mojor_gpu_array_read(out5),
    host5[c(TRUE, FALSE), c(1L, 3L), c(TRUE, FALSE), c(2L, 4L), c(TRUE, FALSE, TRUE), drop = TRUE]
  )
  expect_true(attr(out5, "gpu_fallback") %in% c("gpu_gather", "cpu_gather"))

  expect_error(
    g4[c(TRUE, NA), c(1L, 3L), c(1L, 2L), c(1L, 2L)],
    "logical mask must not contain NA"
  )
  expect_error(
    g4[c(TRUE, FALSE, TRUE), c(1L, 3L), c(1L, 2L), c(1L, 2L)],
    "logical mask length must equal extent"
  )
  out_empty <- g4[c(FALSE, FALSE), c(1L, 3L), c(1L, 2L), c(1L, 2L)]
  expect_true(inherits(out_empty, "mojor_gpu_array"))
  expect_equal(
    mojor_gpu_array_read(out_empty),
    host4[c(FALSE, FALSE), c(1L, 3L), c(1L, 2L), c(1L, 2L), drop = TRUE]
  )
  expect_equal(attr(out_empty, "gpu_fallback"), "gpu_gather")
  out_mixed_matrix <- g4[matrix(c(TRUE, FALSE), nrow = 1L), c(1L, 3L), c(1L, 2L), c(1L, 2L)]
  expect_true(inherits(out_mixed_matrix, "mojor_gpu_array"))
  expect_equal(
    mojor_gpu_array_read(out_mixed_matrix),
    host4[matrix(c(TRUE, FALSE), nrow = 1L), c(1L, 3L), c(1L, 2L), c(1L, 2L)]
  )
  expect_equal(attr(out_mixed_matrix, "gpu_fallback"), "cpu_gather")

  .mojor_gpu_array_free_all(out_mixed_matrix, out_empty, out5, g5, out4, g4)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray bracket indexing supports character dimname selectors", {  .skip_if_no_gpu_backend(require_float = TRUE)

  host <- matrix(
    as.numeric(1:12),
    nrow = 3L,
    ncol = 4L,
    dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3", "c4"))
  )
  gx <- GPUArray(host, dtype = "f32")

  out_char <- gx[c("r3", "r1"), c("c4", "c2"), drop = FALSE]
  expect_true(inherits(out_char, "mojor_gpu_array"))
  expect_equal(dim(out_char), c(2L, 2L))
  expect_equal(
    unname(mojor_gpu_array_read(out_char)),
    unname(host[c("r3", "r1"), c("c4", "c2"), drop = FALSE])
  )
  expect_true(attr(out_char, "gpu_fallback") %in% c("gpu_slice", "cpu_slice", "gpu_gather", "cpu_gather"))

  scalar_char <- gx["r2", "c3"]
  expect_true(is.numeric(scalar_char))
  expect_equal(as.numeric(scalar_char), host["r2", "c3"])

  expect_error(
    gx[c("r1", "r_missing"), "c2"],
    "contains unknown dimname labels"
  )

  gx_no_names <- GPUArray(unname(host), dtype = "f32")
  expect_error(
    gx_no_names["r1", 1L],
    "character indices require dimnames"
  )

  host_partial <- matrix(
    as.numeric(1:12),
    nrow = 3L,
    ncol = 4L,
    dimnames = list(c("r_alpha", "r_beta", "r_gamma"), c("c_alpha", "c_beta", "c_gamma", "c_delta"))
  )
  gx_partial <- GPUArray(host_partial, dtype = "f32")
  out_partial <- gx_partial[c("r_g", "r_a"), c("c_d", "c_b"), drop = FALSE]
  expect_true(inherits(out_partial, "mojor_gpu_array"))
  expect_equal(
    unname(mojor_gpu_array_read(out_partial)),
    unname(host_partial[c("r_gamma", "r_alpha"), c("c_delta", "c_beta"), drop = FALSE])
  )
  expect_error(
    gx_partial["r", "c_b"],
    "ambiguous partial dimname label"
  )
  expect_error(
    gx_partial["r_missing", "c_b"],
    "contains unknown dimname labels"
  )

  .mojor_gpu_array_free_all(out_partial, gx_partial, gx_no_names, out_char, gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray bracket indexing supports n-d shorthand with contiguous slice fast path", {  .skip_if_no_gpu_backend(require_float = TRUE)

  a_ref <- array(as.numeric(1:24), dim = c(2L, 3L, 4L))
  ga <- GPUArray(a_ref, dtype = "f32")

  s1 <- ga[1, , ]
  expect_true(inherits(s1, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(s1), a_ref[1, , ])
  expect_true(attr(s1, "gpu_fallback") %in% c("cpu_slice", "gpu_slice"))

  s2 <- ga[, , 2]
  expect_true(inherits(s2, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(s2), a_ref[, , 2])
  expect_true(attr(s2, "gpu_fallback") %in% c("cpu_slice", "gpu_slice"))

  s3 <- ga[1, , , drop = FALSE]
  expect_true(inherits(s3, "mojor_gpu_array"))
  expect_equal(dim(s3), c(1L, 3L, 4L))
  expect_equal(mojor_gpu_array_read(s3), a_ref[1, , , drop = FALSE])

  b_ref <- array(as.numeric(1:16), dim = c(2L, 2L, 2L, 2L))
  gb <- GPUArray(b_ref, dtype = "f32")
  t1 <- gb[1, , , 2]
  expect_true(inherits(t1, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(t1), b_ref[1, , , 2])
  expect_true(attr(t1, "gpu_fallback") %in% c("cpu_slice", "gpu_slice"))

  t2 <- gb[, 2, , ]
  expect_true(inherits(t2, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(t2), b_ref[, 2, , ])
  expect_true(attr(t2, "gpu_fallback") %in% c("cpu_slice", "gpu_slice"))

  g1 <- ga[1, c(1L, 3L), ]
  expect_true(inherits(g1, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(g1), a_ref[1, c(1L, 3L), ])
  expect_true(attr(g1, "gpu_fallback") %in% c("cpu_gather", "gpu_gather"))

  .mojor_gpu_array_free_all(g1, t2, t1, gb, s3, s2, s1, ga)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray bracket indexing supports non-contiguous gather fallback", {  .skip_if_no_gpu_backend(require_float = TRUE)

  v <- as.numeric(1:10)
  gv <- GPUArray(v, dtype = "f32")
  vg <- gv[c(1L, 3L, 5L)]
  expect_true(inherits(vg, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(vg), v[c(1L, 3L, 5L)])
  expect_true(attr(vg, "gpu_fallback") %in% c("cpu_gather", "gpu_gather"))

  m <- matrix(as.numeric(1:12), nrow = 3L, ncol = 4L)
  gx <- GPUArray(m, dtype = "f32")
  count0 <- mojor_gpu_buf_f32_live_count()
  mg <- gx[c(1L, 3L), c(2L, 4L)]
  expect_true(inherits(mg, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(mg), m[c(1L, 3L), c(2L, 4L), drop = TRUE])
  expect_true(attr(mg, "gpu_fallback") %in% c("cpu_gather", "gpu_gather"))
  route_mg <- attr(mg, "gpu_fallback")
  mojor_gpu_array_free(mg)
  count1 <- mojor_gpu_buf_f32_live_count()

  mg2 <- gx[c(1L, 3L), c(2L, 4L)]
  expect_equal(mojor_gpu_array_read(mg2), m[c(1L, 3L), c(2L, 4L), drop = TRUE])
  route_mg2 <- attr(mg2, "gpu_fallback")
  mojor_gpu_array_free(mg2)
  count2 <- mojor_gpu_buf_f32_live_count()

  mu <- gx[c(3L, 1L, 3L), c(4L, 2L)]
  expect_equal(mojor_gpu_array_read(mu), m[c(3L, 1L, 3L), c(4L, 2L), drop = TRUE])
  expect_true(attr(mu, "gpu_fallback") %in% c("cpu_gather", "gpu_gather"))
  route_mu <- attr(mu, "gpu_fallback")
  mojor_gpu_array_free(mu)
  count3 <- mojor_gpu_buf_f32_live_count()

  if (identical(route_mg, "gpu_gather")) {
    expect_equal(count1, count0 + 1L)
  }
  if (identical(route_mg, "gpu_gather") && identical(route_mg2, "gpu_gather")) {
    expect_equal(count2, count1)
  }
  if (identical(route_mu, "gpu_gather")) {
    expect_equal(count3, count2 + 1L)
  }

  .mojor_gpu_array_free_all(gx, vg, gv)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray gather plan cache invalidates on handle swap", {  .skip_if_no_gpu_backend(require_float = TRUE)

  m1 <- matrix(as.numeric(1:12), nrow = 3L, ncol = 4L)
  m2 <- matrix(as.numeric(101:112), nrow = 3L, ncol = 4L)
  gx <- GPUArray(m1, dtype = "f32")

  g1 <- gx[c(1L, 3L), c(2L, 4L)]
  expect_equal(mojor_gpu_array_read(g1), m1[c(1L, 3L), c(2L, 4L), drop = TRUE])
  mojor_gpu_array_free(g1)

  new_handle <- mojor_gpu_buf_f32(as.numeric(m2))
  gx <- .mojor_gpu_array_set_handle(gx, new_handle, free_old = TRUE, invalidate_plans = FALSE)
  gx <- .mojor_gpu_object_set(gx, "n", as.integer(length(m2)))
  gx <- .mojor_gpu_object_set(gx, "dim", as.integer(dim(m2)))
  gx <- .mojor_gpu_object_set(gx, "dimnames", NULL)
  gx <- .mojor_gpu_object_set(gx, "strides", .mojor_dim_strides(as.integer(dim(m2))))

  g2 <- gx[c(1L, 3L), c(2L, 4L)]
  expect_equal(mojor_gpu_array_read(g2), m2[c(1L, 3L), c(2L, 4L), drop = TRUE])
  expect_true(attr(g2, "gpu_fallback") %in% c("cpu_gather", "gpu_gather"))

  .mojor_gpu_array_free_all(g2, gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray gather plan cache evicts beyond capacity", {  .skip_if_no_gpu_backend(require_float = TRUE)

  m <- matrix(as.numeric(1:20), nrow = 4L, ncol = 5L)
  gx <- GPUArray(m, dtype = "f32")
  count_start <- mojor_gpu_buf_f32_live_count()
  gpu_hits <- 0L

  patterns <- list(
    list(rows = c(1L, 3L), cols = c(2L, 5L)),
    list(rows = c(2L, 4L), cols = c(1L, 4L)),
    list(rows = c(4L, 1L), cols = c(3L, 5L)),
    list(rows = c(1L, 4L), cols = c(2L, 3L, 5L)),
    list(rows = c(3L, 1L, 4L), cols = c(5L, 2L)),
    list(rows = c(2L, 1L, 4L), cols = c(4L, 2L)),
    list(rows = c(4L, 2L, 1L), cols = c(1L, 3L)),
    list(rows = c(1L, 3L, 4L), cols = c(5L, 3L)),
    list(rows = c(2L, 4L, 3L), cols = c(2L, 5L)),
    list(rows = c(3L, 1L, 2L), cols = c(4L, 1L))
  )

  for (p in patterns) {
    g <- gx[p$rows, p$cols]
    expect_equal(mojor_gpu_array_read(g), m[p$rows, p$cols, drop = TRUE])
    route <- attr(g, "gpu_fallback")
    expect_true(route %in% c("cpu_gather", "gpu_gather"))
    if (identical(route, "gpu_gather")) {
      gpu_hits <- gpu_hits + 1L
    }
    mojor_gpu_array_free(g)
  }

  plans <- .mojor_gpu_object_get(gx, "index_plans", default = NULL)
  plan_n <- if (is.environment(plans)) length(ls(plans, all.names = TRUE)) else 0L

  if (gpu_hits > 0L) {
    expect_true(is.environment(plans))
    expect_gte(plan_n, 1L)
    expect_lte(plan_n, 8L)
    expect_equal(mojor_gpu_buf_f32_live_count(), count_start + plan_n)
  } else {
    expect_equal(plan_n, 0L)
    expect_equal(mojor_gpu_buf_f32_live_count(), count_start)
  }

  mojor_gpu_array_free(gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), count_start - 1L)
})

test_that("GPUArray bracket assignment supports contiguous vector and matrix writes", {  .skip_if_no_gpu_backend(require_float = TRUE)

  v_ref <- as.numeric(1:8)
  gv <- GPUArray(v_ref, dtype = "f32")
  gv[3:5] <- c(30, 40, 50)
  v_ref[3:5] <- c(30, 40, 50)
  gv[1] <- 99
  v_ref[1] <- 99
  expect_equal(mojor_gpu_array_read(gv), v_ref)
  expect_true(attr(gv, "gpu_fallback") %in% c("gpu_scatter", "cpu_slice_assign"))

  m_ref <- matrix(as.numeric(1:12), nrow = 3L, ncol = 4L)
  gx <- GPUArray(m_ref, dtype = "f32")
  gx[2, 3] <- 777
  m_ref[2, 3] <- 777
  gx[1:2, 2:3] <- matrix(c(-1, -2, -3, -4), nrow = 2L)
  m_ref[1:2, 2:3] <- matrix(c(-1, -2, -3, -4), nrow = 2L)
  gx[, 4] <- c(70, 80, 90)
  m_ref[, 4] <- c(70, 80, 90)
  expect_equal(mojor_gpu_array_read(gx), m_ref)
  expect_true(attr(gx, "gpu_fallback") %in% c("gpu_scatter", "cpu_slice_assign"))

  .mojor_gpu_array_free_all(gx, gv)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray i32 contiguous assignment prefers GPU route", {  .skip_if_no_gpu_backend(require_float = TRUE)

  v_ref <- as.integer(1:12)
  gv <- GPUArray(v_ref, dtype = "i32")
  gv[3:7] <- as.integer(101:105)
  v_ref[3:7] <- as.integer(101:105)
  expect_equal(as.integer(mojor_gpu_array_read(gv)), v_ref)
  expect_true(attr(gv, "gpu_fallback") %in% c("gpu_scatter", "cpu_slice_assign"))

  m_ref <- matrix(as.integer(1:20), nrow = 4L, ncol = 5L)
  gx <- GPUArray(m_ref, dtype = "i32")
  gx[2:4, 2:5] <- matrix(as.integer(201:212), nrow = 3L, ncol = 4L)
  m_ref[2:4, 2:5] <- matrix(as.integer(201:212), nrow = 3L, ncol = 4L)
  expect_equal(as.integer(mojor_gpu_array_read(gx)), as.integer(m_ref))
  expect_true(attr(gx, "gpu_fallback") %in% c("gpu_scatter", "cpu_slice_assign"))

  .mojor_gpu_array_free_all(gx, gv)
})

test_that("GPUArray i32 non-contiguous assignment is capability-gated to scatter", {  .skip_if_no_gpu_backend(require_float = TRUE)

  cap <- .mojor_gpu_i32_scatter_capable(api = "metal", force_refresh = TRUE)

  v_ref <- as.integer(1:12)
  gv <- GPUArray(v_ref, dtype = "i32")
  gv[c(1L, 3L, 7L, 9L)] <- as.integer(c(101L, 103L, 107L, 109L))
  v_ref[c(1L, 3L, 7L, 9L)] <- as.integer(c(101L, 103L, 107L, 109L))
  route_v <- attr(gv, "gpu_fallback")
  expect_equal(as.integer(mojor_gpu_array_read(gv)), v_ref)
  expect_true(route_v %in% c("cpu_scatter", "gpu_scatter"))
  if (isTRUE(cap)) {
    expect_identical(route_v, "gpu_scatter")
  }

  m_ref <- matrix(as.integer(1:20), nrow = 4L, ncol = 5L)
  gx <- GPUArray(m_ref, dtype = "i32")
  vals <- matrix(as.integer(c(901L, 902L, 903L, 904L)), nrow = 2L, ncol = 2L)
  gx[c(1L, 4L), c(2L, 5L)] <- vals
  m_ref[c(1L, 4L), c(2L, 5L)] <- vals
  route_m <- attr(gx, "gpu_fallback")
  expect_equal(as.integer(mojor_gpu_array_read(gx)), as.integer(m_ref))
  expect_true(route_m %in% c("cpu_scatter", "gpu_scatter"))
  if (isTRUE(cap)) {
    expect_identical(route_m, "gpu_scatter")
  }

  .mojor_gpu_array_free_all(gx, gv)
})

test_that("GPUArray bracket assignment supports non-contiguous scatter fallback", {  .skip_if_no_gpu_backend(require_float = TRUE)

  v_ref <- as.numeric(1:8)
  gv <- GPUArray(v_ref, dtype = "f32")
  count_v0 <- mojor_gpu_buf_f32_live_count()
  gv[c(1L, 3L, 5L)] <- c(10, 30, 50)
  v_ref[c(1L, 3L, 5L)] <- c(10, 30, 50)
  route_v1 <- attr(gv, "gpu_fallback")
  count_v1 <- mojor_gpu_buf_f32_live_count()
  expect_equal(mojor_gpu_array_read(gv), v_ref)
  expect_true(route_v1 %in% c("cpu_scatter", "gpu_scatter"))

  gv[c(1L, 3L, 5L)] <- c(11, 31, 51)
  v_ref[c(1L, 3L, 5L)] <- c(11, 31, 51)
  route_v2 <- attr(gv, "gpu_fallback")
  count_v2 <- mojor_gpu_buf_f32_live_count()
  expect_equal(mojor_gpu_array_read(gv), v_ref)
  expect_true(route_v2 %in% c("cpu_scatter", "gpu_scatter"))
  if (identical(route_v1, "gpu_scatter")) {
    expect_equal(count_v1, count_v0 + 1L)
  }
  if (identical(route_v1, "gpu_scatter") && identical(route_v2, "gpu_scatter")) {
    expect_equal(count_v2, count_v1)
  }

  m_ref <- matrix(as.numeric(1:12), nrow = 3L, ncol = 4L)
  gx <- GPUArray(m_ref, dtype = "f32")
  count_m0 <- mojor_gpu_buf_f32_live_count()
  gx[c(1L, 3L), c(2L, 4L)] <- matrix(c(100, 200, 300, 400), nrow = 2L)
  m_ref[c(1L, 3L), c(2L, 4L)] <- matrix(c(100, 200, 300, 400), nrow = 2L)
  route_m1 <- attr(gx, "gpu_fallback")
  count_m1 <- mojor_gpu_buf_f32_live_count()
  expect_true(route_m1 %in% c("cpu_scatter", "gpu_scatter"))
  gx[c(1L, 3L), c(2L, 4L)] <- matrix(c(110, 210, 310, 410), nrow = 2L)
  m_ref[c(1L, 3L), c(2L, 4L)] <- matrix(c(110, 210, 310, 410), nrow = 2L)
  route_m2 <- attr(gx, "gpu_fallback")
  count_m2 <- mojor_gpu_buf_f32_live_count()
  expect_equal(mojor_gpu_array_read(gx), m_ref)
  expect_true(route_m2 %in% c("cpu_scatter", "gpu_scatter"))
  if (identical(route_m1, "gpu_scatter")) {
    expect_equal(count_m1, count_m0 + 1L)
  }
  if (identical(route_m1, "gpu_scatter") && identical(route_m2, "gpu_scatter")) {
    expect_equal(count_m2, count_m1)
  }

  .mojor_gpu_array_free_all(gx, gv)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray rank-1 duplicate assignment supports deduped scatter path", {  .skip_if_no_gpu_backend(require_float = TRUE)

  v_ref <- as.numeric(seq_len(12L))
  gv <- GPUArray(v_ref, dtype = "f32")

  idx1 <- c(2L, 5L, 2L, 9L, 5L)
  vals1 <- as.numeric(c(100, 200, 300, 400, 500))
  gv[idx1] <- vals1
  route1 <- attr(gv, "gpu_fallback")
  expect_true(route1 %in% c("cpu_scatter", "gpu_scatter"))
  v_ref[idx1] <- vals1
  expect_equal(mojor_gpu_array_read(gv), v_ref)

  idx2 <- c(1L, 1L, 3L, 3L, 12L)
  vals2 <- as.numeric(c(7, 8, 9, 10, 11))
  gv[idx2] <- vals2
  route2 <- attr(gv, "gpu_fallback")
  expect_true(route2 %in% c("cpu_scatter", "gpu_scatter"))
  v_ref[idx2] <- vals2
  expect_equal(mojor_gpu_array_read(gv), v_ref)

  mojor_gpu_array_free(gv)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

