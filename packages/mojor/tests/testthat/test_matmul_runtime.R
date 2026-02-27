# Test matrix multiplication runtime (PR-B6.1)

skip_if_no_mojo <- function() {
  if (!nzchar(Sys.which("mojo"))) {
    skip("Mojo not available")
  }
}

test_that("matmul A[2×3] %*% B[3×2] works correctly", {  skip_if_no_mojo()

  f <- function(A, B) A %*% B
  built <- mojor_build(f, A = "f64[,]", B = "f64[,]")

  A <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
  B <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)

  expected <- A %*% B
  result <- built$func(A, B)

  expect_equal(result, expected)
  expect_equal(dim(result), c(2, 2))
})

test_that("matmul A[3×2] %*% B[2×4] works correctly", {  skip_if_no_mojo()

  f <- function(A, B) A %*% B
  built <- mojor_build(f, A = "f64[,]", B = "f64[,]")

  A <- matrix(as.double(1:6), nrow = 3, ncol = 2)
  B <- matrix(as.double(1:8), nrow = 2, ncol = 4)

  expected <- A %*% B
  result <- built$func(A, B)

  expect_equal(result, expected)
  expect_equal(dim(result), c(3, 4))
})

test_that("matmul with square matrices works", {  skip_if_no_mojo()

  f <- function(A, B) A %*% B
  built <- mojor_build(f, A = "f64[,]", B = "f64[,]")

  A <- matrix(as.double(1:9), nrow = 3, ncol = 3)
  B <- matrix(as.double(9:1), nrow = 3, ncol = 3)

  expected <- A %*% B
  result <- built$func(A, B)

  expect_equal(result, expected)
  expect_equal(dim(result), c(3, 3))
})

test_that("crossprod(A) works correctly", {  skip_if_no_mojo()

  f <- function(A) crossprod(A)
  built <- mojor_build(f, A = "f64[,]")

  A <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
  expected <- crossprod(A)
  result <- built$func(A)

  expect_equal(result, expected)
  expect_equal(dim(result), c(3, 3))
})

test_that("crossprod(A) with different dimensions works", {  skip_if_no_mojo()

  f <- function(A) crossprod(A)
  built <- mojor_build(f, A = "f64[,]")

  A <- matrix(as.double(1:12), nrow = 4, ncol = 3)
  expected <- crossprod(A)
  result <- built$func(A)

  expect_equal(result, expected)
  expect_equal(dim(result), c(3, 3))
})

test_that("crossprod(A, B) works correctly", {  skip_if_no_mojo()

  f <- function(A, B) crossprod(A, B)
  built <- mojor_build(f, A = "f64[,]", B = "f64[,]")

  A <- matrix(as.double(1:6), nrow = 2, ncol = 3)
  B <- matrix(as.double(1:8), nrow = 2, ncol = 4)

  expected <- crossprod(A, B)
  result <- built$func(A, B)

  expect_equal(result, expected)
  expect_equal(dim(result), c(3, 4))
})

test_that("crossprod(A, B) with same B works", {  skip_if_no_mojo()

  f <- function(A, B) crossprod(A, B)
  built <- mojor_build(f, A = "f64[,]", B = "f64[,]")

  A <- matrix(as.double(1:12), nrow = 3, ncol = 4)
  B <- matrix(as.double(1:12), nrow = 3, ncol = 4)

  expected <- crossprod(A, B)
  result <- built$func(A, B)

  expect_equal(result, expected)
  expect_equal(dim(result), c(4, 4))
})

test_that("tcrossprod(A) works correctly", {  skip_if_no_mojo()

  f <- function(A) tcrossprod(A)
  built <- mojor_build(f, A = "f64[,]")

  A <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
  expected <- tcrossprod(A)
  result <- built$func(A)

  expect_equal(result, expected)
  expect_equal(dim(result), c(2, 2))
})

test_that("tcrossprod(A) with different dimensions works", {  skip_if_no_mojo()

  f <- function(A) tcrossprod(A)
  built <- mojor_build(f, A = "f64[,]")

  A <- matrix(as.double(1:12), nrow = 4, ncol = 3)
  expected <- tcrossprod(A)
  result <- built$func(A)

  expect_equal(result, expected)
  expect_equal(dim(result), c(4, 4))
})

test_that("tcrossprod(A, B) works correctly", {  skip_if_no_mojo()

  f <- function(A, B) tcrossprod(A, B)
  built <- mojor_build(f, A = "f64[,]", B = "f64[,]")

  A <- matrix(as.double(1:6), nrow = 2, ncol = 3)
  B <- matrix(as.double(1:12), nrow = 4, ncol = 3)

  expected <- tcrossprod(A, B)
  result <- built$func(A, B)

  expect_equal(result, expected)
  expect_equal(dim(result), c(2, 4))
})

test_that("tcrossprod(A, B) with square matrices works", {  skip_if_no_mojo()

  f <- function(A, B) tcrossprod(A, B)
  built <- mojor_build(f, A = "f64[,]", B = "f64[,]")

  A <- matrix(as.double(1:9), nrow = 3, ncol = 3)
  B <- matrix(as.double(9:1), nrow = 3, ncol = 3)

  expected <- tcrossprod(A, B)
  result <- built$func(A, B)

  expect_equal(result, expected)
  expect_equal(dim(result), c(3, 3))
})

test_that("matrix multiplication with larger matrices works", {  skip_if_no_mojo()

  f <- function(A, B) A %*% B
  built <- mojor_build(f, A = "f64[,]", B = "f64[,]")

  A <- matrix(rnorm(50), nrow = 10, ncol = 5)
  B <- matrix(rnorm(40), nrow = 5, ncol = 8)

  expected <- A %*% B
  result <- built$func(A, B)

  expect_equal(result, expected, tolerance = 1e-10)
  expect_equal(dim(result), c(10, 8))
})

test_that("crossprod with larger matrices works", {  skip_if_no_mojo()

  f <- function(A) crossprod(A)
  built <- mojor_build(f, A = "f64[,]")

  A <- matrix(rnorm(100), nrow = 20, ncol = 5)

  expected <- crossprod(A)
  result <- built$func(A)

  expect_equal(result, expected, tolerance = 1e-10)
  expect_equal(dim(result), c(5, 5))
})

test_that("tcrossprod with larger matrices works", {  skip_if_no_mojo()

  f <- function(A) tcrossprod(A)
  built <- mojor_build(f, A = "f64[,]")

  A <- matrix(rnorm(100), nrow = 20, ncol = 5)

  expected <- tcrossprod(A)
  result <- built$func(A)

  expect_equal(result, expected, tolerance = 1e-10)
  expect_equal(dim(result), c(20, 20))
})

test_that("matrix multiplication preserves numerical precision", {  skip_if_no_mojo()

  f <- function(A, B) A %*% B
  built <- mojor_build(f, A = "f64[,]", B = "f64[,]")

  # Use values that test floating-point precision
  A <- matrix(c(1.123456789, 2.987654321, 3.456789012), nrow = 1, ncol = 3)
  B <- matrix(c(4.567890123, 5.678901234, 6.789012345), nrow = 3, ncol = 1)

  expected <- A %*% B
  result <- built$func(A, B)

  expect_equal(result, expected, tolerance = 1e-12)
})

test_that("matrix operations handle negative values correctly", {  skip_if_no_mojo()

  f <- function(A, B) A %*% B
  built <- mojor_build(f, A = "f64[,]", B = "f64[,]")

  A <- matrix(c(-1, 2, -3, 4, -5, 6), nrow = 2, ncol = 3)
  B <- matrix(c(1, -2, 3, -4, 5, -6), nrow = 3, ncol = 2)

  expected <- A %*% B
  result <- built$func(A, B)

  expect_equal(result, expected)
})

test_that("matrix operations handle zero matrices correctly", {  skip_if_no_mojo()

  f <- function(A, B) A %*% B
  built <- mojor_build(f, A = "f64[,]", B = "f64[,]")

  A <- matrix(0, nrow = 2, ncol = 3)
  B <- matrix(0, nrow = 3, ncol = 2)

  expected <- A %*% B
  result <- built$func(A, B)

  expect_equal(result, expected)
})
