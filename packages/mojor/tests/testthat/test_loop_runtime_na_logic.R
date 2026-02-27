library(testthat)

test_that("any produces correct results at runtime", {  skip_if_no_mojo()
  f <- function(x) {
    any(x > 0)
  }
  built <- mojor_build(f, x = "f64[]", name = "t_any_rt", cache = FALSE, load = TRUE)
  expect_equal(built$func(as.double(c(-3, -1, 0))), f(c(-3, -1, 0)))
  expect_equal(built$func(as.double(c(-3, 1, 0))), f(c(-3, 1, 0)))
  expect_equal(built$func(as.double(numeric(0))), f(numeric(0)))
})

test_that("all produces correct results at runtime", {  skip_if_no_mojo()
  f <- function(x) {
    all(x > 0)
  }
  built <- mojor_build(f, x = "f64[]", name = "t_all_rt", cache = FALSE, load = TRUE)
  expect_equal(built$func(as.double(c(1, 2, 3))), f(c(1, 2, 3)))
  expect_equal(built$func(as.double(c(1, -1, 3))), f(c(1, -1, 3)))
  expect_equal(built$func(as.double(numeric(0))), f(numeric(0)))
})

test_that("any with complex conditions produces correct results", {  skip_if_no_mojo()
  f <- function(x, y) {
    any(x > 0 & y < 0)
  }
  built <- mojor_build(f, x = "f64[]", y = "f64[]", name = "t_any_complex_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(1, 2, 3))
  y <- as.double(c(-1, 2, -3))
  expect_equal(built$func(x, y), f(x, y))
})

test_that("all with complex conditions produces correct results", {  skip_if_no_mojo()
  f <- function(x, y) {
    all(x > 0 | y > 0)
  }
  built <- mojor_build(f, x = "f64[]", y = "f64[]", name = "t_all_complex_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(1, -2, 3))
  y <- as.double(c(-1, 2, -3))
  expect_equal(built$func(x, y), f(x, y))
})

test_that("any/all wrappers support multiple positional arguments at runtime", {  skip_if_no_mojo()
  f_any <- function(x, y) {
    any(x > 0, y > 0)
  }
  f_all <- function(x, y) {
    all(x > 0, y > 0)
  }
  built_any <- mojor_build(f_any, x = "f64[]", y = "f64[]", name = "t_any_multi_rt", cache = FALSE, load = TRUE)
  built_all <- mojor_build(f_all, x = "f64[]", y = "f64[]", name = "t_all_multi_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(-1, 2, -3))
  y <- as.double(c(-4, -5, 6))
  expect_equal(built_any$func(x, y), f_any(x, y))
  expect_equal(built_all$func(x, y), f_all(x, y))
})

test_that("any/all wrappers accept named ... arguments at runtime", {  skip_if_no_mojo()
  f_any <- function(x, y) {
    any(lhs = x > 0, rhs = y > 0, na.rm = FALSE)
  }
  f_all <- function(x, y) {
    all(lhs = x > 0, rhs = y > 0, na.rm = FALSE)
  }
  built_any <- mojor_build(f_any, x = "f64[]", y = "f64[]", name = "t_any_named_rt", cache = FALSE, load = TRUE)
  built_all <- mojor_build(f_all, x = "f64[]", y = "f64[]", name = "t_all_named_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(-1, 2, -3))
  y <- as.double(c(-4, -5, 6))
  expect_equal(built_any$func(x, y), f_any(x, y))
  expect_equal(built_all$func(x, y), f_all(x, y))
})

test_that("any/all wrappers support scalar-only inputs at runtime", {  skip_if_no_mojo()
  f_any <- function(a, b) {
    any(lhs = a > 0, rhs = b > 0, na.rm = FALSE)
  }
  f_all <- function(a, b) {
    all(lhs = a > 0, rhs = b > 0, na.rm = FALSE)
  }
  built_any <- mojor_build(f_any, a = "f64", b = "f64", name = "t_any_scalar_only_rt", cache = FALSE, load = TRUE)
  built_all <- mojor_build(f_all, a = "f64", b = "f64", name = "t_all_scalar_only_rt", cache = FALSE, load = TRUE)
  expect_equal(built_any$func(-1, 2), f_any(-1, 2))
  expect_equal(built_any$func(-1, -2), f_any(-1, -2))
  expect_equal(built_all$func(3, 4), f_all(3, 4))
  expect_equal(built_all$func(3, -4), f_all(3, -4))
})

test_that("any/all wrappers support zero-operand form with na.rm = FALSE at runtime", {  skip_if_no_mojo()
  f_any <- function(a) {
    any(na.rm = FALSE)
  }
  f_all <- function(a) {
    all(na.rm = FALSE)
  }
  built_any <- mojor_build(f_any, a = "f64", name = "t_any_zero_rt", cache = FALSE, load = TRUE)
  built_all <- mojor_build(f_all, a = "f64", name = "t_all_zero_rt", cache = FALSE, load = TRUE)
  expect_equal(built_any$func(1.25), f_any(1.25))
  expect_equal(built_any$func(-2.5), f_any(-2.5))
  expect_equal(built_all$func(1.25), f_all(1.25))
  expect_equal(built_all$func(-2.5), f_all(-2.5))
})

test_that("any/all wrappers support zero-operand form with na.rm = TRUE at runtime", {  skip_if_no_mojo()
  f_any <- function(a) {
    any(na.rm = TRUE)
  }
  f_all <- function(a) {
    all(na.rm = TRUE)
  }
  built_any <- mojor_build(f_any, a = "f64", name = "t_any_zero_rm_rt", cache = FALSE, load = TRUE, na_mode = "na_rm")
  built_all <- mojor_build(f_all, a = "f64", name = "t_all_zero_rm_rt", cache = FALSE, load = TRUE, na_mode = "na_rm")
  expect_equal(built_any$func(1.25), f_any(1.25))
  expect_equal(built_any$func(-2.5), f_any(-2.5))
  expect_equal(built_all$func(1.25), f_all(1.25))
  expect_equal(built_all$func(-2.5), f_all(-2.5))
})

test_that("any/all wrappers fold literal-only scalar forms at runtime", {  skip_if_no_mojo()
  f_any <- function(a) {
    any(TRUE, na.rm = FALSE)
  }
  f_all <- function(a) {
    all(FALSE, na.rm = FALSE)
  }
  built_any <- mojor_build(f_any, a = "f64", name = "t_any_true_literal_rt", cache = FALSE, load = TRUE)
  built_all <- mojor_build(f_all, a = "f64", name = "t_all_false_literal_rt", cache = FALSE, load = TRUE)
  expect_equal(built_any$func(1.25), f_any(1.25))
  expect_equal(built_any$func(-2.5), f_any(-2.5))
  expect_equal(built_all$func(1.25), f_all(1.25))
  expect_equal(built_all$func(-2.5), f_all(-2.5))
})

test_that("any/all wrappers support na.rm = TRUE at runtime", {  skip_if_no_mojo()
  f_any <- function(x) {
    any(x > 0, na.rm = TRUE)
  }
  f_all <- function(x) {
    all(x > 0, na.rm = TRUE)
  }
  built_any <- mojor_build(f_any, x = "f64[]", name = "t_any_na_rm_true_rt", cache = FALSE, load = TRUE, na_mode = "na_rm")
  built_all <- mojor_build(f_all, x = "f64[]", name = "t_all_na_rm_true_rt", cache = FALSE, load = TRUE, na_mode = "na_rm")

  x_false <- as.double(c(NaN, -2, -1))
  x_true <- as.double(c(NaN, 2, 3))
  x_all_na <- as.double(c(NaN, NaN))

  expect_equal(built_any$func(x_false), f_any(x_false))
  expect_equal(built_any$func(x_true), f_any(x_true))
  expect_equal(built_any$func(x_all_na), f_any(x_all_na))
  expect_equal(built_all$func(x_false), f_all(x_false))
  expect_equal(built_all$func(x_true), f_all(x_true))
  expect_equal(built_all$func(x_all_na), f_all(x_all_na))
})

test_that("sum/mean with na.rm = TRUE match R at runtime", {  skip_if_no_mojo()
  f_sum <- function(x) {
    sum(x, na.rm = TRUE)
  }
  f_mean <- function(x) {
    mean(x, na.rm = TRUE)
  }
  built_sum <- mojor_build(f_sum, x = "f64[]", name = "t_sum_na_rm_true_rt", cache = FALSE, load = TRUE, na_mode = "na_rm")
  built_mean <- mojor_build(f_mean, x = "f64[]", name = "t_mean_na_rm_true_rt", cache = FALSE, load = TRUE, na_mode = "na_rm")

  x <- as.double(c(1, NaN, 3, NaN))
  x_all_na <- as.double(c(NaN, NaN))
  x_empty <- as.double(numeric(0))

  expect_equal(built_sum$func(x), f_sum(x))
  expect_equal(built_mean$func(x), f_mean(x))
  expect_equal(built_sum$func(x_all_na), f_sum(x_all_na))
  expect_true(is.nan(built_mean$func(x_all_na)))
  expect_equal(built_sum$func(x_empty), f_sum(x_empty))
  expect_true(is.nan(built_mean$func(x_empty)))
})

test_that("var/sd with na.rm = TRUE match R at runtime", {  skip_if_no_mojo()
  f_var <- function(x) {
    var(x, na.rm = TRUE)
  }
  f_sd <- function(x) {
    sd(x, na.rm = TRUE)
  }
  built_var <- mojor_build(f_var, x = "f64[]", name = "t_var_na_rm_true_rt", cache = FALSE, load = TRUE, na_mode = "na_rm")
  built_sd <- mojor_build(f_sd, x = "f64[]", name = "t_sd_na_rm_true_rt", cache = FALSE, load = TRUE, na_mode = "na_rm")

  x <- as.double(c(1, NaN, 3, NaN, 5))
  x_all_na <- as.double(c(NaN, NaN))
  x_one <- as.double(c(7, NaN))

  expect_equal(built_var$func(x), f_var(x))
  expect_equal(built_sd$func(x), f_sd(x))

  var_all_na <- built_var$func(x_all_na)
  sd_all_na <- built_sd$func(x_all_na)
  var_one <- built_var$func(x_one)
  sd_one <- built_sd$func(x_one)

  expect_true(is.na(var_all_na) && is.na(f_var(x_all_na)))
  expect_true(is.na(sd_all_na) && is.na(f_sd(x_all_na)))
  expect_true(is.na(var_one) && is.na(f_var(x_one)))
  expect_true(is.na(sd_one) && is.na(f_sd(x_one)))
})

test_that("na_mode='propagate' preserves NA vs NaN in runtime arithmetic", {  skip_if_no_mojo()
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- (x[i] / y[i]) + 1.0
    }
    out
  }
  built <- mojor_build(
    f,
    x = "f64[]",
    y = "f64[]",
    name = "t_na_propagate_arith_rt",
    cache = FALSE,
    load = TRUE,
    na_mode = "propagate"
  )
  x <- as.double(c(1, NA_real_, NaN, 3))
  y <- as.double(c(1, 2, 2, 0))
  res <- built$func(x, y)
  ref <- f(x, y)
  na_only_res <- is.na(res) & !is.nan(res)
  na_only_ref <- is.na(ref) & !is.nan(ref)
  nan_only_res <- is.nan(res)
  nan_only_ref <- is.nan(ref)

  expect_equal(na_only_res, na_only_ref)
  expect_equal(nan_only_res, nan_only_ref)

  finite_idx <- !(is.na(res) | is.nan(res))
  expect_equal(res[finite_idx], ref[finite_idx])
})

test_that("na_mode='propagate' preserves logical and ifelse NA semantics at runtime", {  skip_if_no_mojo()
  f_cmp <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] > 0
    }
    out
  }
  f_ifelse <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- ifelse(x[i] > 0, 1.0, -1.0)
    }
    out
  }
  built_cmp <- mojor_build(f_cmp, x = "f64[]", name = "t_na_propagate_cmp_rt", cache = FALSE, load = TRUE, na_mode = "propagate")
  built_ifelse <- mojor_build(f_ifelse, x = "f64[]", name = "t_na_propagate_ifelse_rt", cache = FALSE, load = TRUE, na_mode = "propagate")

  x <- as.double(c(1, NaN, -1))
  cmp_res <- built_cmp$func(x)
  cmp_ref <- f_cmp(x)
  expect_equal(cmp_res, cmp_ref)

  ifelse_res <- built_ifelse$func(x)
  ifelse_ref <- f_ifelse(x)
  expect_equal(is.na(ifelse_res), is.na(ifelse_ref))
  expect_equal(ifelse_res[!is.na(ifelse_res)], ifelse_ref[!is.na(ifelse_ref)])
})

test_that("any/all support explicit selector index arrays at runtime", {  skip_if_no_mojo()
  f_any <- function(x, idx) {
    any(x[idx] > 0)
  }
  f_all <- function(x, idx) {
    all(x[idx] > 0)
  }
  built_any <- mojor_build(f_any, x = "f64[]", idx = "i32[]", name = "t_any_sel_rt", cache = FALSE, load = TRUE)
  built_all <- mojor_build(f_all, x = "f64[]", idx = "i32[]", name = "t_all_sel_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(-2, 5, -1, 3))
  idx_true <- as.integer(c(2, 4))
  idx_false <- as.integer(c(1, 3))
  expect_equal(built_any$func(x, idx_true), f_any(x, idx_true))
  expect_equal(built_all$func(x, idx_true), f_all(x, idx_true))
  expect_equal(built_any$func(x, idx_false), f_any(x, idx_false))
  expect_equal(built_all$func(x, idx_false), f_all(x, idx_false))
})

test_that("any/all support selector index expressions at runtime", {  skip_if_no_mojo()
  f_any <- function(x, idx) {
    any(x[idx + 1L] > 0)
  }
  f_all <- function(x, idx) {
    all(x[idx + 1L] > 0)
  }
  built_any <- mojor_build(f_any, x = "f64[]", idx = "i32[]", name = "t_any_sel_shift_rt", cache = FALSE, load = TRUE)
  built_all <- mojor_build(f_all, x = "f64[]", idx = "i32[]", name = "t_all_sel_shift_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(-10, 2, 3, -4, 5))
  idx <- as.integer(c(1, 2, 4))  # selects positions 2, 3, 5
  expect_equal(built_any$func(x, idx), f_any(x, idx))
  expect_equal(built_all$func(x, idx), f_all(x, idx))
})

test_that("any/all support selector expressions with scalar offsets at runtime", {  skip_if_no_mojo()
  f_any <- function(x, idx, off) {
    any(x[idx + off] > 0)
  }
  f_all <- function(x, idx, off) {
    all(x[idx + off] > 0)
  }
  built_any <- mojor_build(f_any, x = "f64[]", idx = "i32[]", off = "i32", name = "t_any_sel_shift_var_rt", cache = FALSE, load = TRUE)
  built_all <- mojor_build(f_all, x = "f64[]", idx = "i32[]", off = "i32", name = "t_all_sel_shift_var_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(-5, 2, 3, -4, 5, 6))
  idx <- as.integer(c(1, 2, 4))  # with off = 1 selects positions 2, 3, 5
  off <- 1L
  expect_equal(built_any$func(x, idx, off), f_any(x, idx, off))
  expect_equal(built_all$func(x, idx, off), f_all(x, idx, off))
})

test_that("any/all selector expressions fold zero offsets at runtime", {  skip_if_no_mojo()
  f_any <- function(x, idx) {
    any(x[idx + 0L] > 0)
  }
  f_all <- function(x, idx) {
    all(x[idx + (1L - 1L)] > 0)
  }
  built_any <- mojor_build(f_any, x = "f64[]", idx = "i32[]", name = "t_any_sel_plus_zero_rt", cache = FALSE, load = TRUE)
  built_all <- mojor_build(f_all, x = "f64[]", idx = "i32[]", name = "t_all_sel_plus_fold_zero_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(-10, 2, 3, -4, 5))
  idx <- as.integer(c(2, 3, 5))
  expect_equal(built_any$func(x, idx), f_any(x, idx))
  expect_equal(built_all$func(x, idx), f_all(x, idx))
})

test_that("any/all selector expressions support 0L + idx and idx - 0L at runtime", {  skip_if_no_mojo()
  f_any <- function(x, idx) {
    any(x[0L + idx] > 0)
  }
  f_all <- function(x, idx) {
    all(x[idx - 0L] > 0)
  }
  built_any <- mojor_build(f_any, x = "f64[]", idx = "i32[]", name = "t_any_sel_zero_lhs_rt", cache = FALSE, load = TRUE)
  built_all <- mojor_build(f_all, x = "f64[]", idx = "i32[]", name = "t_all_sel_minus_zero_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(-10, 2, 3, -4, 5))
  idx <- as.integer(c(2, 4, 5))
  expect_equal(built_any$func(x, idx), f_any(x, idx))
  expect_equal(built_all$func(x, idx), f_all(x, idx))
})

test_that("any/all selector expressions fold nested canceling literal terms at runtime", {  skip_if_no_mojo()
  f_any <- function(x, idx) {
    any(x[(idx + 1L) - 1L] > 0)
  }
  f_all <- function(x, idx) {
    all(x[(idx - 1L) + 1L] > 0)
  }
  built_any <- mojor_build(f_any, x = "f64[]", idx = "i32[]", name = "t_any_sel_nested_cancel_rt", cache = FALSE, load = TRUE)
  built_all <- mojor_build(f_all, x = "f64[]", idx = "i32[]", name = "t_all_sel_nested_cancel_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(-10, 2, 3, -4, 5))
  idx <- as.integer(c(2, 3, 5))
  expect_equal(built_any$func(x, idx), f_any(x, idx))
  expect_equal(built_all$func(x, idx), f_all(x, idx))
})

test_that("any/all selector expressions support 0L - idx at runtime", {  skip_if_no_mojo()
  f_any <- function(x, idx) {
    any(x[0L - idx] > 0)
  }
  f_all <- function(x, idx) {
    all(x[0L - idx] > 0)
  }
  built_any <- mojor_build(f_any, x = "f64[]", idx = "i32[]", name = "t_any_sel_zero_minus_idx_rt", cache = FALSE, load = TRUE)
  built_all <- mojor_build(f_all, x = "f64[]", idx = "i32[]", name = "t_all_sel_zero_minus_idx_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(-10, 2, 3, -4, 5))
  idx <- as.integer(c(-2, -4, -5))  # 0 - idx selects positions 2, 4, 5
  expect_equal(built_any$func(x, idx), f_any(x, idx))
  expect_equal(built_all$func(x, idx), f_all(x, idx))
})

test_that("any/all support selector decrement expressions at runtime", {  skip_if_no_mojo()
  f_any <- function(x, idx) {
    any(x[idx - 1L] > 0)
  }
  f_all <- function(x, idx) {
    all(x[idx - 1L] > 0)
  }
  built_any <- mojor_build(f_any, x = "f64[]", idx = "i32[]", name = "t_any_sel_shift_down_rt", cache = FALSE, load = TRUE)
  built_all <- mojor_build(f_all, x = "f64[]", idx = "i32[]", name = "t_all_sel_shift_down_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(-10, 2, 3, -4, 5, 6))
  idx <- as.integer(c(2, 3, 6))  # selects positions 1, 2, 5
  expect_equal(built_any$func(x, idx), f_any(x, idx))
  expect_equal(built_all$func(x, idx), f_all(x, idx))
})

test_that("any/all support multi-dimensional selector indices at runtime", {  skip_if_no_mojo()
  f_any_row <- function(x, ridx, j) {
    any(x[ridx, j] > 0)
  }
  f_all_col <- function(x, i, cidx) {
    all(x[i, cidx] > 0)
  }
  f_any_row_shift <- function(x, ridx, j) {
    any(x[ridx + 1L, j] > 0)
  }

  built_any_row <- mojor_build(f_any_row, x = "f64[,]", ridx = "i32[]", j = "i32", name = "t_any_sel_nd_row_rt", cache = FALSE, load = TRUE)
  built_all_col <- mojor_build(f_all_col, x = "f64[,]", i = "i32", cidx = "i32[]", name = "t_all_sel_nd_col_rt", cache = FALSE, load = TRUE)
  built_any_row_shift <- mojor_build(f_any_row_shift, x = "f64[,]", ridx = "i32[]", j = "i32", name = "t_any_sel_nd_row_shift_rt", cache = FALSE, load = TRUE)

  x <- matrix(as.double(c(
    -5, 2, 3,
    4, -1, 6,
    7, 8, -2,
    -3, 5, 9
  )), nrow = 4, byrow = TRUE)
  ridx <- as.integer(c(1, 3, 4))
  ridx_shift <- as.integer(c(1, 2, 3))
  cidx <- as.integer(c(1, 2, 3))
  j <- 2L
  i <- 3L

  expect_equal(built_any_row$func(x, ridx, j), f_any_row(x, ridx, j))
  expect_equal(built_all_col$func(x, i, cidx), f_all_col(x, i, cidx))
  expect_equal(built_any_row_shift$func(x, ridx_shift, j), f_any_row_shift(x, ridx_shift, j))
})

test_that("which.min and which.max produce correct results", {  skip_if_no_mojo()
  f_min <- function(x) {
    which.min(x)
  }
  f_max <- function(x) {
    which.max(x)
  }
  built_min <- mojor_build(f_min, x = "f64[]", name = "t_whichmin_rt", cache = FALSE, load = TRUE)
  built_max <- mojor_build(f_max, x = "f64[]", name = "t_whichmax_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(3, 1, 4, 1, 5))
  expect_equal(built_min$func(x), f_min(x))
  expect_equal(built_max$func(x), f_max(x))
})

test_that("which.min/which.max empty-path wrapper unprotect count is balanced", {  skip_if_no_mojo()
  f_min <- function(x) {
    which.min(x)
  }
  f_max <- function(x) {
    which.max(x)
  }

  built_min <- mojor_build(f_min, x = "f64[]", name = "t_whichmin_empty_unprotect_rt", cache = FALSE, load = TRUE)
  built_max <- mojor_build(f_max, x = "f64[]", name = "t_whichmax_empty_unprotect_rt", cache = FALSE, load = TRUE)

  empty_unprotect_pat <- "if \\(out_ptr\\[0\\] == 0\\) \\{\\s+SEXP empty = PROTECT\\(allocVector\\(INTSXP, 0\\)\\);\\s+UNPROTECT\\(2\\);"
  expect_wrapper_has(built_min, empty_unprotect_pat, fixed = FALSE, perl = TRUE)
  expect_wrapper_has(built_max, empty_unprotect_pat, fixed = FALSE, perl = TRUE)

  expect_warning(min_empty <- built_min$func(numeric(0)), NA)
  expect_warning(max_empty <- built_max$func(numeric(0)), NA)
  expect_identical(min_empty, integer(0))
  expect_identical(max_empty, integer(0))
})

test_that("which.min/which.max support selector zero-affine expressions at runtime", {  skip_if_no_mojo()
  f_min_plus0 <- function(x, idx) {
    which.min(x[idx + 0L])
  }
  f_max_0plus <- function(x, idx) {
    which.max(x[0L + idx])
  }
  f_min_minus0 <- function(x, idx) {
    which.min(x[idx - 0L])
  }
  f_max_0minus <- function(x, idx) {
    which.max(x[0L - idx])
  }

  built_min_plus0 <- mojor_build(f_min_plus0, x = "f64[]", idx = "i32[]", name = "t_whichmin_sel_plus0_rt", cache = FALSE, load = TRUE)
  built_max_0plus <- mojor_build(f_max_0plus, x = "f64[]", idx = "i32[]", name = "t_whichmax_sel_0plus_rt", cache = FALSE, load = TRUE)
  built_min_minus0 <- mojor_build(f_min_minus0, x = "f64[]", idx = "i32[]", name = "t_whichmin_sel_minus0_rt", cache = FALSE, load = TRUE)
  built_max_0minus <- mojor_build(f_max_0minus, x = "f64[]", idx = "i32[]", name = "t_whichmax_sel_0minus_rt", cache = FALSE, load = TRUE)

  x <- as.double(c(8, 1, 5, 9, 2))
  idx <- as.integer(c(1, 5, 4))
  idx_mixed <- as.integer(c(5, 9, 1))  # x[idx] => c(2, NA, 8)
  idx_oob <- as.integer(c(7, 8))       # x[idx] => c(NA, NA)
  idx_zero <- as.integer(c(0, 0))      # x[idx] => numeric(0)
  idx_neg <- as.integer(c(-2, -5, -1))  # 0 - idx selects positions 2, 5, 1

  expect_equal(built_min_plus0$func(x, idx), f_min_plus0(x, idx))
  expect_equal(built_max_0plus$func(x, idx), f_max_0plus(x, idx))
  expect_equal(built_min_minus0$func(x, idx), f_min_minus0(x, idx))
  expect_equal(built_max_0plus$func(x, idx_mixed), f_max_0plus(x, idx_mixed))
  expect_identical(built_min_plus0$func(x, idx_oob), f_min_plus0(x, idx_oob))
  expect_identical(built_max_0plus$func(x, idx_zero), f_max_0plus(x, idx_zero))
  expect_equal(built_max_0minus$func(x, idx_neg), f_max_0minus(x, idx_neg))
})

test_that("which.min/which.max support selector nested canceling literal terms at runtime", {  skip_if_no_mojo()
  f_min_nested <- function(x, idx) {
    which.min(x[(idx + 1L) - 1L])
  }
  f_max_nested <- function(x, idx) {
    which.max(x[(idx - 1L) + 1L])
  }

  built_min_nested <- mojor_build(f_min_nested, x = "f64[]", idx = "i32[]", name = "t_whichmin_nested_cancel_rt", cache = FALSE, load = TRUE)
  built_max_nested <- mojor_build(f_max_nested, x = "f64[]", idx = "i32[]", name = "t_whichmax_nested_cancel_rt", cache = FALSE, load = TRUE)

  x <- as.double(c(8, 1, 5, 9, 2))
  idx <- as.integer(c(1, 5, 4))
  idx_oob <- as.integer(c(7, 8))

  expect_equal(built_min_nested$func(x, idx), f_min_nested(x, idx))
  expect_equal(built_max_nested$func(x, idx), f_max_nested(x, idx))
  expect_identical(built_min_nested$func(x, idx_oob), f_min_nested(x, idx_oob))
  expect_identical(built_max_nested$func(x, idx_oob), f_max_nested(x, idx_oob))
})

# ============================================================================
# logical operators runtime tests (paired with test_transpile_logical_ops.R)
# ============================================================================

test_that("not/or/and operators produce correct results", {  skip_if_no_mojo()
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      if (!(x[i] > 0) || y[i] == 0) {
        out[i] <- -x[i]
      } else {
        out[i] <- x[i] + y[i]
      }
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", y = "f64[]", name = "t_logical_ops_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(1, -2, 3, -4, 5))
  y <- as.double(c(0, 1, 2, 0, 3))
  expect_equal(built$func(x, y), f(x, y))
})

test_that("nested short-circuit &&/|| in conditions produce correct results", {  skip_if_no_mojo()
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      if ((x[i] > 0 && y[i] > 0) || (x[i] < 0 && y[i] < 0)) {
        out[i] <- x[i] + y[i]
      } else if (x[i] == 0 || y[i] == 0) {
        out[i] <- 0
      } else {
        out[i] <- x[i] - y[i]
      }
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", y = "f64[]", name = "t_logical_shortcircuit_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(1, -2, 0, 3, -4))
  y <- as.double(c(2, -3, 1, -1, 0))
  expect_equal(built$func(x, y), f(x, y))
})

# ============================================================================
# is.nan / is.finite runtime tests (paired with test_transpile_isnan_isfinite.R)
# ============================================================================

test_that("is.na produces correct results at runtime", {  skip_if_no_mojo()
  f <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) {
      out[i] <- is.na(x[i])
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", name = "t_isna_f64_rt", cache = FALSE, load = TRUE, na_mode = "unsafe")
  x <- as.double(c(1, NaN, 3, NaN, 5))
  expect_equal(built$func(x), f(x))
})

test_that("is.nan produces correct results at runtime", {  skip_if_no_mojo()
  f <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) {
      out[i] <- is.nan(x[i])
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", name = "t_isnan_rt", cache = FALSE, load = TRUE, na_mode = "unsafe")
  x <- as.double(c(1, NA_real_, NaN, 5))
  expect_equal(built$func(x), f(x))
})

test_that("is.nan distinguishes NA vs NaN for f32[] at runtime", {  skip_if_no_mojo()
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not installed")
  }
  f <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) {
      out[i] <- is.nan(x[i])
    }
    out
  }
  built <- mojor_build(f, x = "f32[]", name = "t_isnan_f32_rt", cache = FALSE, load = TRUE, na_mode = "unsafe")
  x <- float::fl(c(1, NA_real_, NaN, Inf, -Inf, 0))
  expect_equal(built$func(x), f(x))
})

test_that("is.finite produces correct results at runtime", {  skip_if_no_mojo()
  f <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) {
      out[i] <- is.finite(x[i])
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", name = "t_isfinite_rt", cache = FALSE, load = TRUE, na_mode = "unsafe")
  x <- as.double(c(1, Inf, 3, -Inf, 5, NaN))
  expect_equal(built$func(x), f(x))
})

test_that("is.finite produces correct results for f32[] at runtime", {  skip_if_no_mojo()
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not installed")
  }
  f <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) {
      out[i] <- is.finite(x[i])
    }
    out
  }
  built <- mojor_build(f, x = "f32[]", name = "t_isfinite_f32_rt", cache = FALSE, load = TRUE, na_mode = "unsafe")
  x <- float::fl(c(1, NA_real_, NaN, Inf, -Inf, 0))
  expect_equal(built$func(x), f(x))
})

test_that("is.infinite produces correct results at runtime", {  skip_if_no_mojo()
  f <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) {
      out[i] <- is.infinite(x[i])
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", name = "t_isinfinite_rt", cache = FALSE, load = TRUE, na_mode = "unsafe")
  x <- as.double(c(1, Inf, 3, -Inf, 5, NaN))
  expect_equal(built$func(x), f(x))
})

test_that("is.infinite produces correct results for f32[] at runtime", {  skip_if_no_mojo()
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not installed")
  }
  f <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) {
      out[i] <- is.infinite(x[i])
    }
    out
  }
  built <- mojor_build(f, x = "f32[]", name = "t_isinfinite_f32_rt", cache = FALSE, load = TRUE, na_mode = "unsafe")
  x <- float::fl(c(1, NA_real_, NaN, Inf, -Inf, 0))
  expect_equal(built$func(x), f(x))
})

# ============================================================================
# type casts runtime tests (paired with test_transpile_casts.R)
# ============================================================================

test_that("as.integer produces correct results at runtime", {  skip_if_no_mojo()
  f <- function(x) {
    out <- integer(length(x))
    for (i in seq_along(x)) {
      out[i] <- as.integer(x[i])
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", name = "t_asinteger_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(1.7, 2.3, -3.9, 4.0))
  expect_equal(built$func(x), f(x))
})

test_that("as.double produces correct results at runtime", {  skip_if_no_mojo()
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- as.double(x[i])
    }
    out
  }
  built <- mojor_build(f, x = "i32[]", name = "t_asdouble_rt", cache = FALSE, load = TRUE)
  x <- as.integer(c(1, 2, 3, 4))
  expect_equal(built$func(x), f(x))
})

# ============================================================================
# return() runtime tests (paired with test_transpile_return.R)
# ============================================================================
