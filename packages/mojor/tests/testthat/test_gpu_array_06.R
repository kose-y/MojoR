# Split from test_gpu_array.R (chunk 06).
# Shared utilities are in helper-gpu-array.R.

.mojor_skip_gpu_runtime_file_unless_opt_in()

test_that("GPUArray copy-on-modify option isolates aliases for assignment-style mutation", {  .skip_if_no_gpu_backend(require_float = TRUE)
  old_copy_mode <- getOption("mojor.gpu.copy_on_modify")
  on.exit(options(mojor.gpu.copy_on_modify = old_copy_mode), add = TRUE)

  ref <- matrix(
    as.numeric(1:6),
    nrow = 2L,
    ncol = 3L,
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))
  )

  options(mojor.gpu.copy_on_modify = FALSE)
  g0 <- GPUArray(ref, dtype = "f32")
  g0_alias <- g0
  g0[1, 1] <- 99
  expect_equal(mojor_gpu_array_read(g0_alias), mojor_gpu_array_read(g0))

  options(mojor.gpu.copy_on_modify = TRUE)
  g1 <- GPUArray(ref, dtype = "f32")
  g1_alias <- g1
  g1[1, 1] <- 101
  ref_g1 <- ref
  ref_g1[1, 1] <- 101
  expect_equal(mojor_gpu_array_read(g1), ref_g1)
  expect_equal(mojor_gpu_array_read(g1_alias), ref)

  g2 <- GPUArray(ref, dtype = "f32")
  g2_alias <- g2
  g2[[c("r2", "c3")]] <- 77
  ref_g2 <- ref
  ref_g2["r2", "c3"] <- 77
  expect_equal(mojor_gpu_array_read(g2), ref_g2)
  expect_equal(mojor_gpu_array_read(g2_alias), ref)

  g3 <- GPUArray(ref, dtype = "f32")
  g3_alias <- g3
  g3$handle_epoch <- 42L
  expect_equal(g3$handle_epoch, 42L)
  expect_equal(g3_alias$handle_epoch, 0L)

  .mojor_gpu_array_free_all(g3_alias, g3, g2_alias, g2, g1_alias, g1, g0_alias, g0)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray bracket assignment auto-aligns GPUArray replacement dtypes", {  .skip_if_no_gpu_backend(require_float = TRUE)

  ref_f32 <- matrix(as.numeric(1:9), nrow = 3L, ncol = 3L)
  gx_f32 <- GPUArray(ref_f32, dtype = "f32")
  repl_i32_host <- matrix(as.integer(c(41L, 42L, 43L, 44L)), nrow = 2L, ncol = 2L)
  repl_i32 <- GPUArray(repl_i32_host, dtype = "i32")
  gx_f32[c(1L, 3L), c(1L, 2L)] <- repl_i32
  ref_f32[c(1L, 3L), c(1L, 2L)] <- as.numeric(repl_i32_host)
  expect_equal(mojor_gpu_array_read(gx_f32), ref_f32)
  expect_true(attr(gx_f32, "gpu_fallback") %in% c("gpu_scatter", "cpu_scatter", "cpu_slice_assign"))

  ref_i32 <- matrix(as.integer(1:9), nrow = 3L, ncol = 3L)
  gx_i32 <- GPUArray(ref_i32, dtype = "i32")
  repl_f32_host <- matrix(as.numeric(c(3.9, 4.1, 5.7, 6.2)), nrow = 2L, ncol = 2L)
  repl_f32 <- GPUArray(repl_f32_host, dtype = "f32")
  gx_i32[1:2, 2:3] <- repl_f32
  ref_i32[1:2, 2:3] <- matrix(as.integer(repl_f32_host), nrow = 2L, ncol = 2L)
  expect_equal(as.integer(mojor_gpu_array_read(gx_i32)), as.integer(ref_i32))
  expect_true(attr(gx_i32, "gpu_fallback") %in% c("gpu_scatter", "cpu_scatter", "cpu_slice_assign"))

  .mojor_gpu_array_free_all(repl_f32, gx_i32, repl_i32, gx_f32)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray has explicit bracket S3 methods", {  .skip_if_no_gpu_backend(require_float = TRUE)

  get_method <- getS3method("[", "GPUArray", optional = TRUE)
  set_method <- getS3method("[<-", "GPUArray", optional = TRUE)
  expect_true(is.function(get_method))
  expect_true(is.function(set_method))

  gx <- GPUArray(matrix(as.numeric(1:6), nrow = 2L, ncol = 3L), dtype = "f32")
  out <- get_method(gx, 1:2, 2:3, drop = FALSE)
  expect_true(inherits(out, "mojor_gpu_array"))
  expect_equal(dim(out), c(2L, 2L))
  expect_equal(mojor_gpu_array_read(out), matrix(c(3, 4, 5, 6), nrow = 2L))
  mojor_gpu_array_free(out)

  gx2 <- set_method(gx, 1L, 3L, value = 99)
  expect_true(inherits(gx2, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(gx2)[1, 3], 99)

  mojor_gpu_array_free(gx2)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray has explicit [[ and $ S3 methods", {  .skip_if_no_gpu_backend(require_float = TRUE)

  get2_method <- getS3method("[[", "GPUArray", optional = TRUE)
  dollar_method <- getS3method("$", "GPUArray", optional = TRUE)
  expect_true(is.function(get2_method))
  expect_true(is.function(dollar_method))

  x <- as.numeric(c(1, 3, 5, 7))
  gx <- GPUArray(x, dtype = "f32")

  expect_equal(get2_method(gx, 2L), 3)
  expect_equal(gx[[4L]], 7)
  expect_error(gx[[c(1L, 2L)]], "index must select exactly one element")

  expect_equal(gx$dtype, "f32")
  expect_true(gx$api %in% c("metal", "cuda", "amd"))
  expect_true(is.null(gx$does_not_exist))

  m <- matrix(
    as.numeric(1:12),
    nrow = 3L,
    ncol = 4L,
    dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3", "c4"))
  )
  gm <- GPUArray(m, dtype = "f32")
  expect_equal(gm[[1L]], m[[1L]])
  expect_equal(gm[[5L]], m[[5L]])
  expect_equal(gm[[TRUE]], m[[TRUE]])
  expect_error(gm[[13L]], "subscript out of bounds")
  expect_equal(gm[[2L, 3L]], m[2L, 3L])
  expect_equal(gm[[list(2L, 3L)]], m[2L, 3L])
  expect_equal(gm[[c(2L, 3L)]], m[2L, 3L])
  expect_equal(gm[[c("r2", "c3")]], m["r2", "c3"])
  expect_equal(gm[[list("r2", "c3")]], m["r2", "c3"])
  expect_error(gm[[c(2L, 3L, 1L)]], "tuple index length must match rank")
  expect_error(gm[[c("r2", "c3", "x")]], "tuple index length must match rank")

  col_c2 <- gm$c2
  expect_true(inherits(col_c2, "mojor_gpu_array"))
  expect_equal(unname(mojor_gpu_array_read(col_c2)), unname(m[, "c2", drop = TRUE]))
  expect_true(attr(col_c2, "gpu_fallback") %in% c("gpu_slice", "cpu_slice", "gpu_gather", "cpu_gather"))

  row_r3 <- gm$r3
  expect_true(inherits(row_r3, "mojor_gpu_array"))
  expect_equal(unname(mojor_gpu_array_read(row_r3)), unname(m["r3", , drop = TRUE]))
  expect_true(attr(row_r3, "gpu_fallback") %in% c("gpu_slice", "cpu_slice", "gpu_gather", "cpu_gather"))
  col_c2_get2 <- gm[["c2"]]
  expect_true(inherits(col_c2_get2, "mojor_gpu_array"))
  expect_equal(unname(mojor_gpu_array_read(col_c2_get2)), unname(m[, "c2", drop = TRUE]))
  expect_true(attr(col_c2_get2, "gpu_fallback") %in% c("gpu_slice", "cpu_slice", "gpu_gather", "cpu_gather"))

  m_amb <- m
  rownames(m_amb)[1L] <- "c2"
  gm_amb <- GPUArray(m_amb, dtype = "f32")
  expect_true(is.null(gm_amb$c2))
  expect_true(is.null(gm_amb[["c2"]]))

  m_pref <- m
  rownames(m_pref)[1L] <- "c2prefix"
  gm_pref <- GPUArray(m_pref, dtype = "f32")
  pref_col <- gm_pref$c2
  expect_true(inherits(pref_col, "mojor_gpu_array"))
  expect_equal(unname(mojor_gpu_array_read(pref_col)), unname(m_pref[, "c2", drop = TRUE]))
  pref_col_get2 <- gm_pref[["c2"]]
  expect_true(inherits(pref_col_get2, "mojor_gpu_array"))
  expect_equal(unname(mojor_gpu_array_read(pref_col_get2)), unname(m_pref[, "c2", drop = TRUE]))
  expect_warning(
    warn_partial <- gm_pref[["c2p", exact = NA]],
    "partial match"
  )
  expect_true(inherits(warn_partial, "mojor_gpu_array"))
  expect_equal(unname(mojor_gpu_array_read(warn_partial)), unname(m_pref["c2prefix", , drop = TRUE]))
  expect_error(
    gm_pref[[c("c2p", "c2"), exact = TRUE]],
    "unknown dimname labels"
  )
  expect_warning(
    tuple_warn_partial <- gm_pref[[c("c2p", "c2"), exact = NA]],
    "partial match"
  )
  expect_equal(tuple_warn_partial, m_pref["c2prefix", "c2"])

  m_partial_amb <- m
  rownames(m_partial_amb) <- c("c2left", "c2right", "r3")
  colnames(m_partial_amb) <- c("k1", "k2", "k3", "k4")
  gm_partial_amb <- GPUArray(m_partial_amb, dtype = "f32")
  expect_true(is.null(gm_partial_amb$c2))
  expect_true(is.null(gm_partial_amb[["c2", exact = FALSE]]))

  .mojor_gpu_array_free_all(gm_partial_amb, warn_partial, pref_col_get2, pref_col, gm_pref, gm_amb, col_c2_get2, row_r3, col_c2, gm, gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray dollar partial mode controls $ and replacement partial matching", {  .skip_if_no_gpu_backend(require_float = TRUE)
  old_partial_mode <- getOption("mojor.gpu.dollar_partial")
  on.exit(options(mojor.gpu.dollar_partial = old_partial_mode), add = TRUE)

  m <- matrix(
    as.numeric(1:12),
    nrow = 3L,
    ncol = 4L,
    dimnames = list(c("c2prefix", "r2", "r3"), c("c1", "c2", "c3", "c4"))
  )
  gm <- GPUArray(m, dtype = "f32")

  options(mojor.gpu.dollar_partial = "warn")
  expect_warning(
    row_partial <- gm$c2p,
    "partial match"
  )
  expect_true(inherits(row_partial, "mojor_gpu_array"))
  expect_equal(unname(mojor_gpu_array_read(row_partial)), unname(m["c2prefix", , drop = TRUE]))
  repl <- GPUArray(as.numeric(c(901, 902, 903, 904)), dtype = "f32")
  expect_warning({
    gm$c2p <- repl
  }, "partial match")
  m["c2prefix", ] <- as.numeric(c(901, 902, 903, 904))
  expect_equal(mojor_gpu_array_read(gm), m)

  gv <- GPUArray(as.numeric(1:4), dtype = "f32")
  expect_warning({
    gv$ap <- "metal"
  }, "partial match")
  expect_equal(gv$api, "metal")

  options(mojor.gpu.dollar_partial = "exact")
  expect_true(is.null(gm$c2p))
  expect_error({
    gm$c2p <- repl
  }, "unknown field or dimname axis")
  expect_true(is.null(gv$ap))
  expect_error({
    gv$ap <- "metal"
  }, "unknown field or dimname axis")

  .mojor_gpu_array_free_all(gv, repl, row_partial, gm)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray supports [[<- and $<- replacement routes", {  .skip_if_no_gpu_backend(require_float = TRUE)

  set2_method <- getS3method("[[<-", "GPUArray", optional = TRUE)
  set_dollar_method <- getS3method("$<-", "GPUArray", optional = TRUE)
  expect_true(is.function(set2_method))
  expect_true(is.function(set_dollar_method))

  v_ref <- as.integer(c(10L, 20L, 30L, 40L))
  gv <- GPUArray(v_ref, dtype = "i32")
  repl_scalar <- GPUArray(as.numeric(7.9), dtype = "f32")
  gv[[2L]] <- repl_scalar
  v_ref[[2L]] <- as.integer(7.9)
  expect_equal(as.integer(mojor_gpu_array_read(gv)), v_ref)
  expect_error({
    gv[[c(1L, 2L)]] <- 1
  }, "index must select exactly one element")
  gv[["handle_epoch"]] <- 77L
  expect_equal(gv$handle_epoch, 77L)
  expect_error({
    gv[["does_not_exist"]] <- 1
  }, "unknown field or dimname axis")
  expect_error({
    gv[["d"]] <- 1
  }, "ambiguous partial field name")

  m_ref <- matrix(
    as.numeric(1:12),
    nrow = 3L,
    ncol = 4L,
    dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3", "c4"))
  )
  gm <- GPUArray(m_ref, dtype = "f32")
  gm[[1L]] <- 66
  m_ref[[1L]] <- 66
  expect_equal(mojor_gpu_array_read(gm), m_ref)
  gm[[5L]] <- 55
  m_ref[[5L]] <- 55
  expect_equal(mojor_gpu_array_read(gm), m_ref)
  gm[[TRUE]] <- 44
  m_ref[[TRUE]] <- 44
  expect_equal(mojor_gpu_array_read(gm), m_ref)
  gm[[2L, 3L]] <- 77
  m_ref[2L, 3L] <- 77
  expect_equal(mojor_gpu_array_read(gm), m_ref)
  gm[[c("r1", "c4")]] <- 88
  m_ref["r1", "c4"] <- 88
  expect_equal(mojor_gpu_array_read(gm), m_ref)

  col_repl_1 <- GPUArray(as.integer(c(11L, 12L, 13L)), dtype = "i32")
  gm[["c1"]] <- col_repl_1
  m_ref[, "c1"] <- as.numeric(c(11, 12, 13))
  expect_equal(mojor_gpu_array_read(gm), m_ref)
  expect_true(attr(gm, "gpu_fallback") %in% c("gpu_scatter", "cpu_scatter", "cpu_slice_assign"))

  col_repl <- GPUArray(as.integer(c(91L, 92L, 93L)), dtype = "i32")
  gm$c2 <- col_repl
  m_ref[, "c2"] <- as.numeric(c(91, 92, 93))
  expect_equal(mojor_gpu_array_read(gm), m_ref)
  expect_true(attr(gm, "gpu_fallback") %in% c("gpu_scatter", "cpu_scatter", "cpu_slice_assign"))

  row_repl <- GPUArray(as.integer(c(301L, 302L, 303L, 304L)), dtype = "i32")
  gm$r3 <- row_repl
  m_ref["r3", ] <- as.numeric(c(301, 302, 303, 304))
  expect_equal(mojor_gpu_array_read(gm), m_ref)
  expect_true(attr(gm, "gpu_fallback") %in% c("gpu_scatter", "cpu_scatter", "cpu_slice_assign"))

  m_pref <- m_ref
  rownames(m_pref)[1L] <- "c2prefix"
  gm_pref <- GPUArray(m_pref, dtype = "f32")
  pref_col_vals <- GPUArray(as.integer(c(601L, 602L, 603L)), dtype = "i32")
  gm_pref$c2 <- pref_col_vals
  m_pref[, "c2"] <- as.numeric(c(601, 602, 603))
  expect_equal(mojor_gpu_array_read(gm_pref), m_pref)
  expect_true(attr(gm_pref, "gpu_fallback") %in% c("gpu_scatter", "cpu_scatter", "cpu_slice_assign"))
  pref_col_vals_2 <- GPUArray(as.integer(c(701L, 702L, 703L)), dtype = "i32")
  gm_pref[["c2"]] <- pref_col_vals_2
  m_pref[, "c2"] <- as.numeric(c(701, 702, 703))
  expect_equal(mojor_gpu_array_read(gm_pref), m_pref)
  expect_true(attr(gm_pref, "gpu_fallback") %in% c("gpu_scatter", "cpu_scatter", "cpu_slice_assign"))

  m_axis_amb <- m_ref
  rownames(m_axis_amb)[1L] <- "c2"
  gm_axis_amb <- GPUArray(m_axis_amb, dtype = "f32")
  expect_error({
    gm_axis_amb[["c2"]] <- 1
  }, "matches multiple axes")
  expect_error({
    gm_axis_amb$c2 <- 1
  }, "matches multiple axes")

  m_partial_amb <- m_ref
  rownames(m_partial_amb) <- c("c2left", "c2right", "r3")
  colnames(m_partial_amb) <- c("k1", "k2", "k3", "k4")
  gm_partial_amb <- GPUArray(m_partial_amb, dtype = "f32")
  expect_error({
    gm_partial_amb[["c2"]] <- 1
  }, "ambiguous partial dimname label")
  expect_error({
    gm_partial_amb$c2 <- 1
  }, "ambiguous partial dimname label")

  gm$handle_epoch <- 42L
  expect_equal(gm$handle_epoch, 42L)
  expect_error({
    gm$does_not_exist <- 1
  }, "unknown field or dimname axis")
  expect_error({
    gv$d <- 1
  }, "ambiguous partial field name")

  .mojor_gpu_array_free_all(gm_partial_amb, gm_axis_amb, pref_col_vals_2, pref_col_vals, gm_pref, row_repl, col_repl, col_repl_1, repl_scalar, gm, gv)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray has explicit Ops and linear algebra S3 methods", {  .skip_if_no_gpu_backend(require_float = TRUE)

  ops_method <- getS3method("Ops", "GPUArray", optional = TRUE)
  mm_method <- getS3method("%*%", "GPUArray", optional = TRUE)
  cp_method <- get0("crossprod.GPUArray", mode = "function", inherits = TRUE)
  tcp_method <- get0("tcrossprod.GPUArray", mode = "function", inherits = TRUE)
  expect_true(is.function(ops_method))
  expect_true(is.function(mm_method))
  expect_true(is.function(cp_method))
  expect_true(is.function(tcp_method))

  x <- as.numeric(1:6)
  y <- as.numeric(11:16)
  gx <- GPUArray(x, dtype = "f32")
  gy <- GPUArray(y, dtype = "f32")
  sum_out <- gx + gy
  expect_true(inherits(sum_out, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(sum_out), x + y)
  mojor_gpu_array_free(sum_out)

  a <- matrix(as.numeric(1:6), nrow = 2L)
  b <- matrix(as.numeric(1:9), nrow = 3L)
  ga <- GPUArray(a, dtype = "f32")
  gb <- GPUArray(b, dtype = "f32")
  mm <- mm_method(ga, gb)
  expect_true(inherits(mm, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(mm), a %*% b, tolerance = 1e-4)

  cp <- cp_method(ga)
  expect_true(inherits(cp, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(cp), crossprod(a))
  expect_true(attr(cp, "gpu_fallback") %in% c("cpu_crossprod", "gpu_crossprod"))

  tcp <- tcp_method(ga)
  expect_true(inherits(tcp, "mojor_gpu_array"))
  expect_equal(mojor_gpu_array_read(tcp), tcrossprod(a))
  expect_true(attr(tcp, "gpu_fallback") %in% c("cpu_tcrossprod", "gpu_tcrossprod"))

  .mojor_gpu_array_free_all(tcp, cp, mm, gb, ga, gy, gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray has direct Math and round ufunc methods", {  .skip_if_no_gpu_backend()

  math_method <- getS3method("Math", "GPUArray", optional = TRUE)
  round_method <- getS3method("round", "GPUArray", optional = TRUE)
  expect_true(is.function(math_method))
  expect_true(is.function(round_method))

  check_unary <- function(out, expected, tol = 1e-5) {
    expect_true(inherits(out, "mojor_gpu_array"))
    expect_equal(mojor_gpu_array_read(out), expected, tolerance = tol)
    route <- attr(out, "gpu_fallback", exact = TRUE)
    if (!is.null(route)) {
      expect_true(route %in% c("cpu_arith", "cpu_kernel_host"))
      expect_false(
        identical(
          attr(out, "gpu_fallback_reason_code", exact = TRUE),
          "raw_kernel_unavailable"
        )
      )
    }
  }

  x_pos <- matrix(c(0.25, 0.5, 1.25, 2.5, 3.75, 4.25), nrow = 2L, ncol = 3L)
  x_mix <- matrix(c(-2.2, -1.1, 0.0, 1.1, 2.2, 3.3), nrow = 2L, ncol = 3L)
  g_pos <- GPUArray(x_pos, dtype = "f32")
  g_mix <- GPUArray(x_mix, dtype = "f32")

  out_sin <- sin(g_pos)
  check_unary(out_sin, sin(x_pos))

  out_cos <- cos(g_pos)
  check_unary(out_cos, cos(x_pos))

  out_tan <- tan(g_pos)
  check_unary(out_tan, tan(x_pos))
  expect_false(identical(attr(out_tan, "gpu_fallback", exact = TRUE), "cpu_kernel_host"))

  out_exp <- exp(g_pos)
  check_unary(out_exp, exp(x_pos), tol = 1e-4)

  out_log <- log(g_pos)
  check_unary(out_log, log(x_pos))

  out_log10 <- log10(g_pos)
  check_unary(out_log10, log10(x_pos))

  out_log2 <- log2(g_pos)
  check_unary(out_log2, log2(x_pos))

  out_log1p <- log1p(g_pos)
  check_unary(out_log1p, log1p(x_pos))

  out_expm1 <- expm1(g_pos)
  check_unary(out_expm1, expm1(x_pos), tol = 1e-4)
  expect_false(identical(attr(out_expm1, "gpu_fallback", exact = TRUE), "cpu_kernel_host"))

  out_sqrt <- sqrt(g_pos)
  check_unary(out_sqrt, sqrt(x_pos))

  out_abs <- abs(g_mix)
  check_unary(out_abs, abs(x_mix))

  out_sign <- sign(g_mix)
  check_unary(out_sign, sign(x_mix))

  for (out_unary in list(out_log, out_log10, out_log2, out_log1p, out_sign)) {
    route <- attr(out_unary, "gpu_fallback", exact = TRUE)
    if (!is.null(route)) {
      expect_true(route %in% c("cpu_arith", "cpu_kernel_host"))
      reason_code <- attr(out_unary, "gpu_fallback_reason_code", exact = TRUE)
      expect_true(
        is.null(reason_code) ||
          reason_code %in% c(
            "kernel_dispatch_failed",
            "raw_kernel_unavailable",
            "kernel_wrapper_dispatch_failed",
            "kernel_wrapper_unavailable"
          )
      )
    }
  }

  out_trunc <- trunc(g_mix)
  check_unary(out_trunc, trunc(x_mix))

  out_floor <- floor(g_mix)
  check_unary(out_floor, floor(x_mix))

  out_ceil <- ceiling(g_mix)
  check_unary(out_ceil, ceiling(x_mix))

  out_round0 <- round(g_mix, digits = 0L)
  check_unary(out_round0, round(x_mix, digits = 0L))
  expect_false(identical(attr(out_round0, "gpu_fallback", exact = TRUE), "cpu_kernel_host"))

  out_round <- round(g_mix, digits = 2L)
  check_unary(out_round, round(x_mix, digits = 2L))
  expect_identical(attr(out_round, "gpu_fallback", exact = TRUE), "cpu_arith")
  expect_identical(
    attr(out_round, "gpu_fallback_reason_code", exact = TRUE),
    "round_digits_host_fallback"
  )

  .mojor_gpu_array_free_all(
    out_round, out_round0, out_ceil, out_floor, out_trunc, out_sign, out_abs,
    out_sqrt, out_expm1, out_log1p, out_log2, out_log10, out_log, out_exp, out_tan, out_cos, out_sin,
    g_mix, g_pos
  )
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray binary math functions support gpu_atan2/gpu_minimum/gpu_maximum", {  .skip_if_no_gpu_backend()

  check_binary <- function(out, expected, tol = 1e-5) {
    expect_true(inherits(out, "mojor_gpu_array"))
    expect_equal(mojor_gpu_array_read(out), expected, tolerance = tol)
  }

  expect_kernel_optional_route <- function(out, lane, allow_cpu_kernel_host = FALSE) {
    route <- attr(out, "gpu_fallback", exact = TRUE)
    if (is.null(route)) {
      return(invisible(NULL))
    }
    allowed <- if (isTRUE(allow_cpu_kernel_host)) {
      c("cpu_arith", "cpu_kernel_host")
    } else {
      "cpu_arith"
    }
    expect_true(route %in% allowed, info = lane)
    if (identical(route, "cpu_arith")) {
      expect_identical(
        attr(out, "gpu_fallback_reason_code", exact = TRUE),
        "kernel_dispatch_failed",
        info = lane
      )
    }
  }

  expect_host_parity_reason <- function(out, reason_code, lane) {
    expect_identical(attr(out, "gpu_fallback", exact = TRUE), "cpu_arith", info = lane)
    expect_identical(
      attr(out, "gpu_fallback_reason_code", exact = TRUE),
      reason_code,
      info = lane
    )
  }

  x <- matrix(c(-2.0, -0.5, 0.0, 1.0, 2.0, 4.0), nrow = 2L, ncol = 3L)
  y <- matrix(c(-1.5, 0.5, 0.2, 2.0, 1.5, 3.5), nrow = 2L, ncol = 3L)
  gx <- GPUArray(x, dtype = "f32")
  gy <- GPUArray(y, dtype = "f32")

  out_pmin <- gpu_minimum(gx, gy)
  check_binary(out_pmin, pmin(x, y))
  expect_kernel_optional_route(out_pmin, "arr_arr")

  out_pmin3 <- gpu_minimum(gx, gy, -0.25)
  check_binary(out_pmin3, pmin(pmin(x, y), -0.25))
  expect_kernel_optional_route(out_pmin3, "variadic_fold")

  out_pmin_arr_sca <- gpu_minimum(gx, 0.75)
  check_binary(out_pmin_arr_sca, pmin(x, 0.75))
  expect_kernel_optional_route(out_pmin_arr_sca, "arr_sca")
  expect_null(attr(out_pmin_arr_sca, "gpu_fallback", exact = TRUE))

  out_pmin_sca_arr <- gpu_minimum(0.75, gy)
  check_binary(out_pmin_sca_arr, pmin(0.75, y))
  expect_null(dim(out_pmin_sca_arr))
  expect_kernel_optional_route(out_pmin_sca_arr, "sca_arr")
  expect_null(attr(out_pmin_sca_arr, "gpu_fallback", exact = TRUE))

  out_pmax <- gpu_maximum(gx, gy)
  check_binary(out_pmax, pmax(x, y))
  expect_kernel_optional_route(out_pmax, "arr_arr")

  out_pmax4 <- gpu_maximum(gx, gy, 0.75, -0.5)
  check_binary(out_pmax4, pmax(pmax(pmax(x, y), 0.75), -0.5))
  expect_kernel_optional_route(out_pmax4, "variadic_fold")

  out_pmax3 <- gpu_maximum(out_pmax, 0.75)
  check_binary(out_pmax3, pmax(pmax(x, y), 0.75))
  expect_kernel_optional_route(out_pmax3, "variadic_fold")

  out_pmax_host_lhs <- gpu_maximum(0.5, gy)
  check_binary(out_pmax_host_lhs, pmax(0.5, y))
  expect_null(dim(out_pmax_host_lhs))
  expect_kernel_optional_route(out_pmax_host_lhs, "sca_arr")
  expect_null(attr(out_pmax_host_lhs, "gpu_fallback", exact = TRUE))

  out_pmax_gpu_rhs <- gpu_maximum(gx, 0.75)
  check_binary(out_pmax_gpu_rhs, pmax(x, 0.75))
  expect_kernel_optional_route(out_pmax_gpu_rhs, "arr_sca")
  expect_null(attr(out_pmax_gpu_rhs, "gpu_fallback", exact = TRUE))

  out_atan2 <- gpu_atan2(gx, gy)
  check_binary(out_atan2, atan2(x, y), tol = 1e-4)
  expect_kernel_optional_route(out_atan2, "atan2_arr_arr")

  x_special <- matrix(c(Inf, -Inf, NaN, NA_real_, 0, -0), nrow = 2L, ncol = 3L)
  y_special <- matrix(c(1, 2, -Inf, NA_real_, NaN, Inf), nrow = 2L, ncol = 3L)
  gx_special <- GPUArray(x_special, dtype = "f32")
  gy_special <- GPUArray(y_special, dtype = "f32")

  out_pmin_special <- gpu_minimum(gx_special, gy_special)
  check_binary(out_pmin_special, pmin(x_special, y_special))
  expect_kernel_optional_route(out_pmin_special, "arr_arr_special")
  expect_null(attr(out_pmin_special, "gpu_fallback", exact = TRUE))

  out_pmax_special <- gpu_maximum(gx_special, gy_special)
  check_binary(out_pmax_special, pmax(x_special, y_special))
  expect_kernel_optional_route(out_pmax_special, "arr_arr_special")
  expect_null(attr(out_pmax_special, "gpu_fallback", exact = TRUE))

  out_atan2_special <- gpu_atan2(gx_special, gy_special)
  check_binary(out_atan2_special, atan2(x_special, y_special), tol = 1e-4)
  expect_kernel_optional_route(out_atan2_special, "atan2_arr_arr_special")

  x_na <- matrix(c(NA_real_, 1, NA_real_, 3), nrow = 2L, ncol = 2L)
  y_na <- matrix(c(2, NA_real_, 4, NA_real_), nrow = 2L, ncol = 2L)
  gx_na <- GPUArray(x_na, dtype = "f32")
  gy_na <- GPUArray(y_na, dtype = "f32")
  out_pmin_na <- gpu_minimum(gx_na, gy_na, na.rm = TRUE)
  check_binary(out_pmin_na, pmin(x_na, y_na, na.rm = TRUE))
  expect_host_parity_reason(out_pmin_na, "na_rm_host_parity", "na_rm_arr_arr")

  out_pmax_na <- gpu_maximum(gx_na, gy_na, na.rm = TRUE)
  check_binary(out_pmax_na, pmax(x_na, y_na, na.rm = TRUE))
  expect_host_parity_reason(out_pmax_na, "na_rm_host_parity", "na_rm_arr_arr")

  out_pmin_na_sca <- gpu_minimum(gx_na, 0, na.rm = TRUE)
  check_binary(out_pmin_na_sca, pmin(x_na, 0, na.rm = TRUE))
  expect_host_parity_reason(out_pmin_na_sca, "na_rm_host_parity", "na_rm_arr_sca")

  y_shape <- matrix(c(0.5, 1.5, 2.5), nrow = 1L, ncol = 3L)
  gy_shape <- GPUArray(y_shape, dtype = "f32")
  out_pmin_shape <- gpu_minimum(gx, gy_shape)
  check_binary(out_pmin_shape, pmin(x, y_shape))
  expect_host_parity_reason(out_pmin_shape, "shape_mismatch_host_parity", "shape_mismatch_arr_arr")

  y_recycle <- matrix(c(0.5, 1.5, 2.5, 3.5), nrow = 2L, ncol = 2L)
  gy_recycle <- GPUArray(y_recycle, dtype = "f32")
  collect_warnings <- function(expr) {
    warns <- character()
    value <- withCallingHandlers(
      expr,
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    list(value = value, warnings = warns)
  }
  host_recycle <- collect_warnings(pmin(x, y_recycle))
  gpu_recycle <- collect_warnings(gpu_minimum(gx, gy_recycle))
  expected_recycle <- host_recycle$value
  out_pmin_recycle <- gpu_recycle$value
  check_binary(out_pmin_recycle, expected_recycle)
  expect_host_parity_reason(out_pmin_recycle, "shape_mismatch_host_parity", "shape_mismatch_recycle")
  expect_true(length(host_recycle$warnings) >= 1L)
  expect_true(any(host_recycle$warnings[[1L]] == gpu_recycle$warnings))

  expect_identical(gpu_minimum(gx), gx)
  expect_identical(gpu_maximum(gy), gy)
  expect_error(gpu_minimum(), "at least one argument is required")
  expect_error(gpu_maximum(), "at least one argument is required")

  .mojor_gpu_array_free_all(
    out_pmin_recycle, gy_recycle, out_pmin_shape, gy_shape,
    out_pmin_na_sca, out_pmax_na, out_pmin_na, gy_na, gx_na,
    out_atan2_special, out_pmax_special, out_pmin_special, gy_special, gx_special,
    out_atan2, out_pmax_gpu_rhs, out_pmax_host_lhs, out_pmax3, out_pmax4, out_pmax,
    out_pmin_sca_arr, out_pmin_arr_sca, out_pmin3, out_pmin,
    gy, gx
  )
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})
