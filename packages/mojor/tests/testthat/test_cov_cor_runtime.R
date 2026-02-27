# Test covariance and correlation runtime (PR-B7.2)

skip_if_no_mojo <- function() {
  if (!nzchar(Sys.which("mojo"))) {
    skip("Mojo not available")
  }
}

test_that("cov(x, y) works correctly", {  skip_if_no_mojo()

  f <- function(x, y) cov(x, y)
  built <- mojor_build(f, x = "f64[]", y = "f64[]")

  x <- c(1, 2, 3, 4, 5)
  y <- c(2, 4, 6, 8, 10)
  expected <- cov(x, y)
  result <- built$func(x, y)

  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("cov(x, y) with different data works", {  skip_if_no_mojo()

  f <- function(x, y) cov(x, y)
  built <- mojor_build(f, x = "f64[]", y = "f64[]")

  x <- c(2.5, 3.7, 1.2, 4.8, 3.1, 2.9)
  y <- c(1.1, 2.3, 0.9, 3.5, 2.0, 1.8)
  expected <- cov(x, y)
  result <- built$func(x, y)

  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("cov(x, y) with negative correlation works", {  skip_if_no_mojo()

  f <- function(x, y) cov(x, y)
  built <- mojor_build(f, x = "f64[]", y = "f64[]")

  x <- c(1, 2, 3, 4, 5)
  y <- c(5, 4, 3, 2, 1)
  expected <- cov(x, y)
  result <- built$func(x, y)

  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("cor(x, y) with perfect positive correlation works", {  skip_if_no_mojo()

  f <- function(x, y) cor(x, y)
  built <- mojor_build(f, x = "f64[]", y = "f64[]")

  x <- c(1, 2, 3, 4, 5)
  y <- c(2, 4, 6, 8, 10)
  expected <- cor(x, y)
  result <- built$func(x, y)

  expect_equal(result, expected, tolerance = 1e-10)
  expect_equal(result, 1.0, tolerance = 1e-10)
})

test_that("cor(x, y) with perfect negative correlation works", {  skip_if_no_mojo()

  f <- function(x, y) cor(x, y)
  built <- mojor_build(f, x = "f64[]", y = "f64[]")

  x <- c(1, 2, 3, 4, 5)
  y <- c(5, 4, 3, 2, 1)
  expected <- cor(x, y)
  result <- built$func(x, y)

  expect_equal(result, expected, tolerance = 1e-10)
  expect_equal(result, -1.0, tolerance = 1e-10)
})

test_that("cor(x, y) with partial correlation works", {  skip_if_no_mojo()

  f <- function(x, y) cor(x, y)
  built <- mojor_build(f, x = "f64[]", y = "f64[]")

  x <- c(2.5, 3.7, 1.2, 4.8, 3.1, 2.9)
  y <- c(1.1, 2.3, 0.9, 3.5, 2.0, 1.8)
  expected <- cor(x, y)
  result <- built$func(x, y)

  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("cor(x, y) with zero correlation works", {  skip_if_no_mojo()

  f <- function(x, y) cor(x, y)
  built <- mojor_build(f, x = "f64[]", y = "f64[]")

  # Create approximately uncorrelated data
  x <- c(1, 2, 3, 4, 5)
  y <- c(3, 1, 4, 2, 5)
  expected <- cor(x, y)
  result <- built$func(x, y)

  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("cov/cor preserve numerical precision", {  skip_if_no_mojo()

  f_cov <- function(x, y) cov(x, y)
  f_cor <- function(x, y) cor(x, y)

  built_cov <- mojor_build(f_cov, x = "f64[]", y = "f64[]")
  built_cor <- mojor_build(f_cor, x = "f64[]", y = "f64[]")

  # Use values that test floating-point precision
  x <- c(1.123456789, 2.987654321, 3.456789012, 4.567890123)
  y <- c(5.678901234, 6.789012345, 7.890123456, 8.901234567)

  expected_cov <- cov(x, y)
  expected_cor <- cor(x, y)

  result_cov <- built_cov$func(x, y)
  result_cor <- built_cor$func(x, y)

  expect_equal(result_cov, expected_cov, tolerance = 1e-12)
  expect_equal(result_cor, expected_cor, tolerance = 1e-12)
})

test_that("cov/cor handle larger vectors", {  skip_if_no_mojo()

  f_cov <- function(x, y) cov(x, y)
  f_cor <- function(x, y) cor(x, y)

  built_cov <- mojor_build(f_cov, x = "f64[]", y = "f64[]")
  built_cor <- mojor_build(f_cor, x = "f64[]", y = "f64[]")

  x <- rnorm(100)
  y <- rnorm(100)

  expected_cov <- cov(x, y)
  expected_cor <- cor(x, y)

  result_cov <- built_cov$func(x, y)
  result_cor <- built_cor$func(x, y)

  expect_equal(result_cov, expected_cov, tolerance = 1e-10)
  expect_equal(result_cor, expected_cor, tolerance = 1e-10)
})

test_that("cov(X) matrix lane matches base R", {  skip_if_no_mojo()

  f <- function(X) cov(X)
  built <- mojor_build(f, X = "f64[,]")

  X <- matrix(
    c(
      1.0, 3.0, 5.0,
      2.0, 4.0, 6.0,
      3.0, 5.0, 7.0,
      4.0, 6.0, 8.0
    ),
    nrow = 4,
    ncol = 3
  )
  expected <- cov(X)
  result <- built$func(X)

  expect_equal(result, expected, tolerance = 1e-10)
  expect_equal(dim(result), dim(expected))
})

test_that("cor(X) matrix lane matches base R", {  skip_if_no_mojo()

  f <- function(X) cor(X)
  built <- mojor_build(f, X = "f64[,]")

  X <- matrix(
    c(
      1.0, 2.5, 0.5,
      2.0, 3.5, 1.5,
      4.0, 5.5, 2.0,
      7.0, 8.5, 3.0,
      11.0, 12.5, 4.5
    ),
    nrow = 5,
    ncol = 3
  )
  expected <- cor(X)
  result <- built$func(X)

  expect_equal(result, expected, tolerance = 1e-10)
  expect_equal(dim(result), dim(expected))
})

test_that("cov(X, Y) matrix lane matches base R", {  skip_if_no_mojo()

  f <- function(X, Y) cov(X, Y)
  built <- mojor_build(f, X = "f64[,]", Y = "f64[,]")

  X <- matrix(
    c(
      1.0, 2.0, 4.0, 8.0,
      1.5, 2.7, 4.2, 8.8,
      2.0, 3.6, 4.5, 9.6
    ),
    nrow = 4,
    ncol = 3
  )
  Y <- matrix(
    c(
      10.0, 9.0, 7.0, 4.0,
      2.0, 3.0, 6.0, 9.0
    ),
    nrow = 4,
    ncol = 2
  )
  expected <- cov(X, Y)
  result <- built$func(X, Y)

  expect_equal(result, expected, tolerance = 1e-10)
  expect_equal(dim(result), dim(expected))
})

test_that("cor(X, Y) matrix lane matches base R", {  skip_if_no_mojo()

  f <- function(X, Y) cor(X, Y)
  built <- mojor_build(f, X = "f64[,]", Y = "f64[,]")

  X <- matrix(
    c(
      2.0, 3.0, 5.0, 7.0, 11.0,
      1.0, 4.0, 9.0, 16.0, 25.0
    ),
    nrow = 5,
    ncol = 2
  )
  Y <- matrix(
    c(
      5.0, 4.0, 3.0, 2.0, 1.0,
      1.1, 1.6, 2.7, 4.3, 6.8,
      8.0, 13.0, 21.0, 34.0, 55.0
    ),
    nrow = 5,
    ncol = 3
  )
  expected <- cor(X, Y)
  result <- built$func(X, Y)

  expect_equal(result, expected, tolerance = 1e-10)
  expect_equal(dim(result), dim(expected))
})

test_that("cov/cor matrix lanes reject row-count mismatch", {  skip_if_no_mojo()

  f_cov <- function(X, Y) cov(X, Y)
  f_cor <- function(X, Y) cor(X, Y)
  built_cov <- mojor_build(f_cov, X = "f64[,]", Y = "f64[,]")
  built_cor <- mojor_build(f_cor, X = "f64[,]", Y = "f64[,]")

  X <- matrix(as.double(1:12), nrow = 4, ncol = 3)
  Y <- matrix(as.double(1:6), nrow = 3, ncol = 2)

  expect_error(
    built_cov$func(X, Y),
    "matrix inputs must have same number of rows"
  )
  expect_error(
    built_cor$func(X, Y),
    "matrix inputs must have same number of rows"
  )
})
