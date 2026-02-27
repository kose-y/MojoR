library(testthat)

if (!mojor_is_loaded()) {
  try(mojor_load(file.path(getwd(), "prototype", "build")), silent = TRUE)
}
if (!mojor_is_loaded()) {
  skip("Mojo backend not loaded")
}

test_that("na_mode='forbid' rejects NA/NaN inputs", {  old <- mojor_options(na_mode = "forbid")
  on.exit(mojor_options(na_mode = old$na_mode), add = TRUE)

  x <- c(1, NA_real_, 2)
  expect_error(mojor_sum_f64(x), "NA/NaN not supported")
  expect_error(mojor_min_f64(x), "NA/NaN not supported")
  expect_error(mojor_max_f64(x), "NA/NaN not supported")
})

test_that("na_mode='unsafe' skips NA checks", {  old <- mojor_options(na_mode = "unsafe")
  on.exit(mojor_options(na_mode = old$na_mode), add = TRUE)

  x <- c(1, NA_real_, 2)
  expect_silent(mojor_sum_f64(x))
  expect_silent(mojor_min_f64(x))
  expect_silent(mojor_max_f64(x))
})

test_that("reduction helpers preserve empty semantics", {  x_empty <- numeric(0)

  expect_warning(min_empty <- mojor_min_f64(x_empty), "no non-missing arguments to min; returning Inf", fixed = TRUE)
  expect_warning(max_empty <- mojor_max_f64(x_empty), "no non-missing arguments to max; returning -Inf", fixed = TRUE)
  expect_identical(min_empty, Inf)
  expect_identical(max_empty, -Inf)
  expect_warning(mean_empty <- mojor_mean_f64(x_empty), NA)
  expect_true(is.nan(mean_empty))
  expect_warning(prod_empty <- mojor_prod_f64(x_empty), NA)
  expect_identical(prod_empty, 1)
  expect_warning(sd_empty <- mojor_sd_f64(x_empty), NA)
  expect_true(is.na(sd_empty))
  expect_warning(var_empty <- mojor_var_f64(x_empty), NA)
  expect_true(is.na(var_empty))
  expect_warning(wmin_empty <- mojor_which_min_f64(x_empty), NA)
  expect_warning(wmax_empty <- mojor_which_max_f64(x_empty), NA)
  expect_identical(wmin_empty, integer(0))
  expect_identical(wmax_empty, integer(0))

  if (requireNamespace("float", quietly = TRUE)) {
    x_empty_f32 <- float::fl(numeric(0))
    expect_warning(min_empty_f32 <- mojor_min_f32(x_empty_f32), "no non-missing arguments to min; returning Inf", fixed = TRUE)
    expect_warning(max_empty_f32 <- mojor_max_f32(x_empty_f32), "no non-missing arguments to max; returning -Inf", fixed = TRUE)
    expect_identical(min_empty_f32, Inf)
    expect_identical(max_empty_f32, -Inf)
    expect_warning(mean_empty_f32 <- mojor_mean_f32(x_empty_f32), NA)
    expect_true(is.nan(mean_empty_f32))
    expect_warning(prod_empty_f32 <- mojor_prod_f32(x_empty_f32), NA)
    expect_identical(prod_empty_f32, 1)
    expect_warning(sd_empty_f32 <- mojor_sd_f32(x_empty_f32), NA)
    expect_true(is.na(sd_empty_f32))
    expect_warning(var_empty_f32 <- mojor_var_f32(x_empty_f32), NA)
    expect_true(is.na(var_empty_f32))
    expect_warning(wmin_empty_f32 <- mojor_which_min_f32(x_empty_f32), NA)
    expect_warning(wmax_empty_f32 <- mojor_which_max_f32(x_empty_f32), NA)
    expect_identical(wmin_empty_f32, integer(0))
    expect_identical(wmax_empty_f32, integer(0))
  }
})
