library(testthat)


test_that("is.na transpiles for floats and integers", {  f_na_f <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) {
      out[i] <- is.na(x[i])
    }
    out
  }
  f_na_i <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) {
      out[i] <- is.na(x[i])
    }
    out
  }

  res_f <- mojor_transpile(f_na_f, x = "f64[]", name = "mojor_isna_f")
  res_i <- mojor_transpile(f_na_i, x = "i32[]", name = "mojor_isna_i")
  expect_match(res_f$mojo, " != ", fixed = TRUE)  # NaN check: x != x
  expect_match(res_i$mojo, "== -2147483648", fixed = TRUE)  # integer/logical NA sentinel
})


test_that("is.nan and is.finite transpile for floats", {  f_nan <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) {
      out[i] <- is.nan(x[i])
    }
    out
  }
  f_fin <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) {
      out[i] <- is.finite(x[i])
    }
    out
  }
  f_inf <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) {
      out[i] <- is.infinite(x[i])
    }
    out
  }

  res_nan <- mojor_transpile(f_nan, x = "f64[]", name = "mojor_isnan")
  res_nan_f32 <- mojor_transpile(f_nan, x = "f32[]", name = "mojor_isnan_f32")
  res_fin <- mojor_transpile(f_fin, x = "f64[]", name = "mojor_isfinite")
  res_fin_f32 <- mojor_transpile(f_fin, x = "f32[]", name = "mojor_isfinite_f32")
  res_inf <- mojor_transpile(f_inf, x = "f64[]", name = "mojor_isinf")
  res_inf_f32 <- mojor_transpile(f_inf, x = "f32[]", name = "mojor_isinf_f32")
  expect_match(res_nan$mojo, "_mojor_is_nan_f64(", fixed = TRUE)
  expect_match(res_nan_f32$mojo, "_mojor_is_nan_f32(", fixed = TRUE)
  expect_match(res_fin$mojo, "_MOJOR_INF", fixed = TRUE)
  expect_match(res_fin$mojo, "_MOJOR_NINF", fixed = TRUE)
  expect_match(res_fin_f32$mojo, "_MOJOR_INF_F32", fixed = TRUE)
  expect_match(res_fin_f32$mojo, "_MOJOR_NINF_F32", fixed = TRUE)
  expect_match(res_inf$mojo, "_MOJOR_INF", fixed = TRUE)
  expect_match(res_inf$mojo, "_MOJOR_NINF", fixed = TRUE)
  expect_match(res_inf_f32$mojo, "_MOJOR_INF_F32", fixed = TRUE)
  expect_match(res_inf_f32$mojo, "_MOJOR_NINF_F32", fixed = TRUE)
})


test_that("is.nan/is.finite/is.infinite for integer are constants", {  f_nan <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) {
      out[i] <- is.nan(x[i])
    }
    out
  }
  f_fin <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) {
      out[i] <- is.finite(x[i])
    }
    out
  }
  f_inf <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) {
      out[i] <- is.infinite(x[i])
    }
    out
  }

  res_nan <- mojor_transpile(f_nan, x = "i32[]", name = "mojor_isnan_i")
  res_fin <- mojor_transpile(f_fin, x = "i32[]", name = "mojor_isfinite_i")
  res_inf <- mojor_transpile(f_inf, x = "i32[]", name = "mojor_isinf_i")
  expect_match(res_nan$mojo, "False", fixed = TRUE)
  expect_match(res_fin$mojo, "True", fixed = TRUE)
  expect_match(res_inf$mojo, "False", fixed = TRUE)
})
