library(testthat)

# Tests for matrix slice assignment with c() constructor and missing indices
#
# These tests verify the fixes for:
# 1. Missing index handling (mat[i, ] pattern)
# 2. c() constructor boundary conditions
# 3. LayoutTensor comptime layout constants

# =============================================================================
# Transpilation Tests - Missing Index Handling
# =============================================================================

test_that("matrix slice with missing column index transpiles", {  f <- function(x, y, n) {
    mat <- matrix(0, n, 2)
    for (i in 1:n) {
      mat[i, ] <- c(x[i], y[i])
    }
    mat
  }

  result <- mojor_transpile(f, x = "f64[]", y = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))

  # Should have slice assignment loop (variable name is __mojor_i2 for 2nd dimension)
  expect_match(result$mojo, "for __mojor_i2 in")

  # Should use comptime layout constant
  expect_match(result$mojo, "_MOJOR_MATRIX_LAYOUT")
})

test_that("matrix slice with missing row index transpiles", {  f <- function(x, n) {
    mat <- matrix(0, 2, n)
    for (j in 1:n) {
      mat[, j] <- c(x[j], x[j] * 2)
    }
    mat
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))

  # Should have slice assignment loop (variable name is __mojor_i1 for 1st dimension)
  expect_match(result$mojo, "for __mojor_i1 in")
})

test_that("matrix slice assignment emits comptime layout constant", {  f <- function(x, y, n) {
    mat <- matrix(0, n, 2)
    for (i in 1:n) {
      mat[i, ] <- c(x[i], y[i])
    }
    mat
  }

  result <- mojor_transpile(f, x = "f64[]", y = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))

  # Check for comptime layout constant definition
  expect_match(result$mojo, "comptime _MOJOR_MATRIX_LAYOUT = Layout\\.col_major\\(IndexList\\[2\\]\\(0, 0\\)\\)")

  # Check for RuntimeLayout using the constant
  expect_match(result$mojo, "RuntimeLayout\\[_MOJOR_MATRIX_LAYOUT\\]")
})

# =============================================================================
# Transpilation Tests - c() Constructor Boundaries
# =============================================================================

test_that("c() constructor with two elements has correct boundaries", {  f <- function(x, y, n) {
    mat <- matrix(0, n, 2)
    for (i in 1:n) {
      mat[i, ] <- c(x[i], y[i])
    }
    mat
  }

  result <- mojor_transpile(f, x = "f64[]", y = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))

  # Should have ternary with boundary condition.
  # Parenthesization around __mojor_i2 may vary across optimizations.
  expect_match(result$mojo, "% 2\\) < 1")
})

test_that("c() constructor with three elements has correct boundaries", {  f <- function(x, y, z, n) {
    mat <- matrix(0, n, 3)
    for (i in 1:n) {
      mat[i, ] <- c(x[i], y[i], z[i])
    }
    mat
  }

  result <- mojor_transpile(f, x = "f64[]", y = "f64[]", z = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))

  # Should have nested ternaries with boundaries
  # Pattern: __mojor_i2 <= N for selecting between parts
  expect_match(result$mojo, "__mojor_i2")
})

test_that("c() constructor with scalar and array elements", {  f <- function(x, n) {
    mat <- matrix(0, n, 2)
    for (i in 1:n) {
      mat[i, ] <- c(42.0, x[i])
    }
    mat
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))

  # Should have scalar 42.0 in first branch
  expect_match(result$mojo, "42")
})

# =============================================================================
# Runtime Tests - Correctness
# =============================================================================

test_that("matrix slice c() assigns distinct values", {  skip_if_no_mojo()

  f <- function(x, y) {
    mat <- matrix(0, length(x), 2)
    for (i in seq_along(x)) {
      mat[i, ] <- c(x[i], y[i])
    }
    mat
  }

  built <- mojor_build(f, x = "f64[]", y = "f64[]",
                       name = "test_slice_c_distinct",
                       cache = FALSE, load = TRUE)

  x <- c(1.0, 2.0, 3.0)
  y <- c(10.0, 20.0, 30.0)
  result <- built$func(x, y)
  expected <- f(x, y)

  expect_equal(result, expected)

  # Verify values are distinct (not both equal)
  expect_equal(result[1, 1], 1.0)
  expect_equal(result[1, 2], 10.0)
  expect_equal(result[2, 1], 2.0)
  expect_equal(result[2, 2], 20.0)
})

test_that("matrix slice c() with three elements", {  skip_if_no_mojo()

  f <- function(x, y, z) {
    mat <- matrix(0, length(x), 3)
    for (i in seq_along(x)) {
      mat[i, ] <- c(x[i], y[i], z[i])
    }
    mat
  }

  built <- mojor_build(f, x = "f64[]", y = "f64[]", z = "f64[]",
                       name = "test_slice_c_three",
                       cache = FALSE, load = TRUE)

  x <- c(1.0, 4.0)
  y <- c(2.0, 5.0)
  z <- c(3.0, 6.0)
  result <- built$func(x, y, z)
  expected <- f(x, y, z)

  expect_equal(result, expected)
})

test_that("matrix slice c() with scalar and array", {  skip_if_no_mojo()

  f <- function(x) {
    mat <- matrix(0, length(x), 2)
    for (i in seq_along(x)) {
      mat[i, ] <- c(100.0, x[i])
    }
    mat
  }

  built <- mojor_build(f, x = "f64[]",
                       name = "test_slice_c_scalar",
                       cache = FALSE, load = TRUE)

  x <- c(1.0, 2.0, 3.0)
  result <- built$func(x)
  expected <- f(x)

  expect_equal(result, expected)

  # First column should all be 100
  expect_true(all(result[, 1] == 100.0))
})

test_that("matrix slice c() with expressions", {  skip_if_no_mojo()

  f <- function(x, y) {
    mat <- matrix(0, length(x), 2)
    for (i in seq_along(x)) {
      mat[i, ] <- c(x[i] + y[i], x[i] * y[i])
    }
    mat
  }

  built <- mojor_build(f, x = "f64[]", y = "f64[]",
                       name = "test_slice_c_expr",
                       cache = FALSE, load = TRUE)

  x <- c(1.0, 2.0, 3.0)
  y <- c(4.0, 5.0, 6.0)
  result <- built$func(x, y)
  expected <- f(x, y)

  expect_equal(result, expected)
})

# =============================================================================
# Runtime Tests - RNG with Matrix Output
# =============================================================================

test_that("rnorm with matrix slice assignment", {  skip_if_no_mojo()

  f <- function(n) {
    mat <- matrix(0, n, 2)
    x <- 0.0
    y <- 0.0
    for (i in 1:n) {
      x <- rnorm(1, 0, 1)
      y <- rnorm(1, 0, 1)
      mat[i, ] <- c(x, y)
    }
    mat
  }

  built <- mojor_build(f, n = "i32",
                       name = "test_rnorm_mat_slice",
                       cache = FALSE, load = TRUE)

  # Seed for reproducibility
  mojor_rng_seed(42L)
  result <- built$func(5L)

  # Check dimensions
  expect_equal(dim(result), c(5, 2))

  # Check values are distinct (not all zeros, not all same)
  expect_true(any(result[, 1] != result[1, 1]))
  expect_true(any(result[, 2] != result[1, 2]))

  # Check first and second column are different
  expect_false(all(result[, 1] == result[, 2]))
})

test_that("rgamma with matrix slice assignment", {  skip_if_no_mojo()

  f <- function(n) {
    mat <- matrix(0, n, 2)
    x <- 0.0
    y <- 0.0
    for (i in 1:n) {
      x <- rgamma(1, 2, 1)
      y <- rgamma(1, 3, 1)
      mat[i, ] <- c(x, y)
    }
    mat
  }

  built <- mojor_build(f, n = "i32",
                       name = "test_rgamma_mat_slice",
                       cache = FALSE, load = TRUE)

  mojor_rng_seed(42L)
  result <- built$func(5L)

  # Check dimensions
  expect_equal(dim(result), c(5, 2))

  # Check values are positive (gamma distribution)
  expect_true(all(result > 0))

  # Check values are distinct
  expect_true(any(result[, 1] != result[1, 1]))
})

test_that("nested loops with RNG and matrix output (mini Gibbs)", {  skip_if_no_mojo()

  f <- function(n, thin) {
    mat <- matrix(0, n, 2)
    x <- 1.0
    y <- 0.5
    for (i in 1:n) {
      for (j in 1:thin) {
        x <- rgamma(1, 3, y * y + 4)
        y <- rnorm(1, 1 / (x + 1), 1 / sqrt(2 * (x + 1)))
      }
      mat[i, ] <- c(x, y)
    }
    mat
  }

  built <- mojor_build(f, n = "i32", thin = "i32",
                       name = "test_mini_gibbs",
                       cache = FALSE, load = TRUE)

  mojor_rng_seed(123L)
  result <- built$func(10L, 5L)

  # Check dimensions
  expect_equal(dim(result), c(10, 2))

  # Check x values are positive
  expect_true(all(result[, 1] > 0))

  # Check values vary across iterations
  expect_true(length(unique(result[, 1])) > 1)
  expect_true(length(unique(result[, 2])) > 1)

  # Check first and second column are different
  expect_false(isTRUE(all.equal(result[, 1], result[, 2])))
})

# =============================================================================
# Edge Cases
# =============================================================================

test_that("matrix slice c() with single row", {  skip_if_no_mojo()

  f <- function(x, y) {
    mat <- matrix(0, 1, 2)
    for (i in 1:1) {
      mat[i, ] <- c(x[i], y[i])
    }
    mat
  }

  built <- mojor_build(f, x = "f64[]", y = "f64[]",
                       name = "test_slice_c_single",
                       cache = FALSE, load = TRUE)

  x <- c(42.0)
  y <- c(99.0)
  result <- built$func(x, y)
  expected <- f(x, y)

  expect_equal(result, expected)
})

test_that("matrix slice c() with larger matrices", {  skip_if_no_mojo()

  f <- function(x, y) {
    # Use length(x) directly, not via local variable
    mat <- matrix(0, length(x), 2)
    for (i in seq_along(x)) {
      mat[i, ] <- c(x[i], y[i])
    }
    mat
  }

  built <- mojor_build(f, x = "f64[]", y = "f64[]",
                       name = "test_slice_c_large",
                       cache = FALSE, load = TRUE)

  x <- seq(1, 100, by = 1)
  y <- seq(100, 1, by = -1)
  result <- built$func(x, y)
  expected <- f(x, y)

  expect_equal(result, expected)
  expect_equal(dim(result), c(100, 2))
})
