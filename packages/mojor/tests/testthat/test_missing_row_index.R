library(testthat)

# Test missing row index pattern: mat[, j]

test_that("missing row index transpiles", {  f <- function(x, n) {
    mat <- matrix(0, 2, n)
    for (j in 1:n) {
      mat[, j] <- c(x[j], x[j] * 2)
    }
    mat
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))

  # Should have slice assignment loop
  expect_match(result$mojo, "for __mojor_i1 in range")
})

test_that("missing row index with scalar RHS", {  f <- function(x, n) {
    mat <- matrix(0, 2, n)
    for (j in 1:n) {
      mat[, j] <- x[j]
    }
    mat
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "for __mojor_i1 in range")
})

test_that("missing row index with constant RHS", {  f <- function(n) {
    mat <- matrix(0, 3, n)
    for (j in 1:n) {
      mat[, j] <- 42.0
    }
    mat
  }

  result <- mojor_transpile(f, n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "for __mojor_i1 in range")
  expect_match(result$mojo, "42")
})

test_that("missing row index generates correct indexing", {  f <- function(x, n) {
    mat <- matrix(0, 2, n)
    for (j in 1:n) {
      mat[, j] <- c(x[j], x[j] * 2)
    }
    mat
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))

  # Should iterate over rows (first dimension)
  expect_match(result$mojo, "for __mojor_i1 in range\\(0, Int\\(Int\\(nrow_mat\\)\\)\\)")

  # Should use column-major indexing: mat[row + col * nrow]
  expect_match(result$mojo, "mat_tensor\\[__mojor_i1, \\(j - 1\\)\\]")
})

# =============================================================================
# Runtime Tests
# =============================================================================

test_that("missing row index with c() constructor runtime", {  skip_if_no_mojo()

  f <- function(x) {
    mat <- matrix(0, 2, length(x))
    for (j in seq_along(x)) {
      mat[, j] <- c(x[j], x[j] * 2)
    }
    mat
  }

  built <- mojor_build(f, x = "f64[]",
                       name = "test_missing_row_c",
                       cache = FALSE, load = TRUE)

  x <- c(1.0, 2.0, 3.0)
  result <- built$func(x)
  expected <- f(x)

  expect_equal(result, expected)
  expect_equal(dim(result), c(2, 3))
  expect_equal(result[1, ], x)
  expect_equal(result[2, ], x * 2)
})

test_that("missing row index with scalar RHS runtime", {  skip_if_no_mojo()

  f <- function(x) {
    mat <- matrix(0, 3, length(x))
    for (j in seq_along(x)) {
      mat[, j] <- x[j]
    }
    mat
  }

  built <- mojor_build(f, x = "f64[]",
                       name = "test_missing_row_scalar",
                       cache = FALSE, load = TRUE)

  x <- c(10.0, 20.0, 30.0, 40.0)
  result <- built$func(x)
  expected <- f(x)

  expect_equal(result, expected)
  expect_equal(dim(result), c(3, 4))
  # Each column should have identical values
  for (j in seq_along(x)) {
    expect_true(all(result[, j] == x[j]))
  }
})

test_that("missing row index with expression RHS runtime", {  skip_if_no_mojo()

  f <- function(x, y) {
    mat <- matrix(0, 2, length(x))
    for (j in seq_along(x)) {
      mat[, j] <- c(x[j] + y[j], x[j] - y[j])
    }
    mat
  }

  built <- mojor_build(f, x = "f64[]", y = "f64[]",
                       name = "test_missing_row_expr",
                       cache = FALSE, load = TRUE)

  x <- c(5.0, 10.0, 15.0)
  y <- c(1.0, 2.0, 3.0)
  result <- built$func(x, y)
  expected <- f(x, y)

  expect_equal(result, expected)
  expect_equal(result[1, ], x + y)
  expect_equal(result[2, ], x - y)
})

test_that("missing row index with many columns runtime", {  skip_if_no_mojo()

  f <- function(x) {
    mat <- matrix(0, 2, length(x))
    for (j in seq_along(x)) {
      mat[, j] <- c(x[j], x[j] * x[j])
    }
    mat
  }

  built <- mojor_build(f, x = "f64[]",
                       name = "test_missing_row_large",
                       cache = FALSE, load = TRUE)

  x <- seq(1, 10, by = 1)
  result <- built$func(x)
  expected <- f(x)

  expect_equal(result, expected)
  expect_equal(dim(result), c(2, 10))
  expect_equal(result[1, ], x)
  expect_equal(result[2, ], x * x)
})
