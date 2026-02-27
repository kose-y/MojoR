library(testthat)

if (!requireNamespace("float", quietly = TRUE)) {
  skip("float package not installed")
}

if (!mojor_is_loaded()) {
  try(mojor_load(file.path(getwd(), "prototype", "build")), silent = TRUE)
}
if (!mojor_is_loaded()) {
  skip("Mojo backend not loaded")
}

f32_sum_ref <- function(x) {
  acc <- float::fl(0)
  vals <- float::dbl(float::fl(x))
  for (v in vals) {
    acc <- acc + float::fl(v)
  }
  as.numeric(acc)
}

f32_mean_ref <- function(x) {
  n <- length(x)
  if (n == 0) return(NaN)
  f32_sum_ref(x) / n
}

f32_prod_ref <- function(x) {
  acc <- float::fl(1)
  vals <- float::dbl(float::fl(x))
  for (v in vals) {
    acc <- acc * float::fl(v)
  }
  as.numeric(acc)
}

rel_err <- function(a, b) {
  if (is.nan(a) && is.nan(b)) return(0)
  denom <- max(1, abs(b))
  abs(a - b) / denom
}

test_that("f32 reductions are within expected precision", {  set.seed(42)
  x <- runif(10000, -10, 10)

  sum_ref <- f32_sum_ref(x)
  mean_ref <- f32_mean_ref(x)
  x_prod <- runif(512, 0.9, 1.1)
  prod_ref <- f32_prod_ref(x_prod)
  min_ref <- min(float::dbl(float::fl(x)))
  max_ref <- max(float::dbl(float::fl(x)))

  expect_true(rel_err(mojor_sum_f32(x), sum_ref) < 1e-5)
  expect_true(rel_err(mojor_mean_f32(x), mean_ref) < 1e-5)
  expect_true(rel_err(mojor_prod_f32(x_prod), prod_ref) < 1e-5)
  expect_equal(mojor_min_f32(x), min_ref)
  expect_equal(mojor_max_f32(x), max_ref)
})
