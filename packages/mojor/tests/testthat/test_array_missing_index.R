library(testthat)

# Tests for missing indices in 3D+ arrays
#
# These tests verify that missing index patterns work for N-dimensional arrays (N >= 3).
# The IR slice assignment implementation handles missing indices via the `n_idx >= 3` case
# which uses dimension pointers to get the size of any missing dimension.
#
# Supported patterns:
# - arr[, j, k]  (missing first dimension)
# - arr[i, , k]  (missing second dimension)
# - arr[i, j, ]  (missing third dimension)
# - arr[, , k]   (multiple missing dimensions)
#
# This complements the 2D matrix tests in test_missing_row_index.R and
# test_matrix_slice_constructor.R.

test_that("3D array with missing first index transpiles", {  f <- function(x, n) {
    arr <- array(0, dim = c(2, 3, n))
    for (k in 1:n) {
      arr[, 1, k] <- c(x[k], x[k] * 2)
    }
    arr
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))

  # Should have loop for missing first dimension
  expect_match(result$mojo, "for __mojor_i1 in")
})

test_that("3D array with missing second index transpiles", {  f <- function(x, n) {
    arr <- array(0, dim = c(2, n, 3))
    for (j in 1:n) {
      arr[1, , 1] <- x[j]
    }
    arr
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))

  # Should have loop for missing second dimension
  expect_match(result$mojo, "for __mojor_i2 in")
})

test_that("3D array with missing third index transpiles", {  f <- function(x, n) {
    arr <- array(0, dim = c(2, 3, n))
    for (i in 1:2) {
      for (j in 1:3) {
        arr[i, j, ] <- x[i]
      }
    }
    arr
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))

  # Should have loop for missing third dimension
  expect_match(result$mojo, "for __mojor_i3 in")
})

test_that("3D array with multiple missing indices transpiles", {  f <- function(x, n) {
    arr <- array(0, dim = c(2, 3, n))
    for (k in 1:n) {
      arr[, , k] <- x[k]
    }
    arr
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))

  # Should have nested loops for both missing dimensions
  expect_match(result$mojo, "for __mojor_i1 in")
  expect_match(result$mojo, "for __mojor_i2 in")
})

# Runtime tests
test_that("3D array with missing index runtime", {  skip_if_no_mojo()

  f <- function(x) {
    arr <- array(0, dim = c(2, 3, length(x)))
    for (k in seq_along(x)) {
      arr[, 1, k] <- c(x[k], x[k] + 1)
    }
    arr
  }

  built <- mojor_build(f, x = "f64[]",
                       name = "test_3d_missing",
                       cache = FALSE, load = TRUE)

  x <- c(1.0, 2.0, 3.0)
  result <- built$func(x)
  expected <- f(x)

  expect_equal(result, expected)
  expect_equal(dim(result), c(2, 3, 3))

  # Check specific values
  expect_equal(result[1, 1, 1], 1.0)
  expect_equal(result[2, 1, 1], 2.0)
  expect_equal(result[1, 1, 2], 2.0)
  expect_equal(result[2, 1, 2], 3.0)
})
