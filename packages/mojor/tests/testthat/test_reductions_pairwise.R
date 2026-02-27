test_that("pairwise sum matches base sum for f64/f32", {  if (!mojor_is_loaded()) {
    skip("Mojo backend not loaded")
  }
  set.seed(123)
  x <- runif(10000)
  if (!is.loaded("mojor_sum_f64_pairwise")) {
    skip("mojor_sum_f64_pairwise not loaded")
  }
  expect_equal(mojor_sum_f64_pairwise(x), sum(x), tolerance = 1e-8)

  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not available")
  }
  xf <- float::fl(x)
  if (!is.loaded("mojor_sum_f32_pairwise_int")) {
    skip("mojor_sum_f32_pairwise_int not loaded")
  }
  expect_equal(mojor_sum_f32_pairwise(xf), sum(x), tolerance = 1e-4)
})
