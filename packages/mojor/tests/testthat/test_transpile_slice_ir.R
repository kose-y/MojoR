library(testthat)

# Slice IR Support Tests

# =============================================================================
# Basic Vector Slicing
# =============================================================================

test_that("IR handles vector slice assignment 1:n inside loop", {  f <- function(x, n, m) {
    out <- numeric(n)
    for (k in 1:m) {
      out[1:n] <- x[k]
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32", m = "i32")

  # Should generate nested loops: outer k loop, inner slice loop
  expect_true(grepl("for k in range", res$mojo))
  expect_true(grepl("for __mojor_i1 in range", res$mojo))
  expect_true(grepl("out\\[__mojor_i1\\]", res$mojo))
})

test_that("IR handles vector slice with seq_len inside loop", {  f <- function(x, n, m) {
    out <- numeric(n)
    for (k in 1:m) {
      out[seq_len(n)] <- k
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32", m = "i32")

  # seq_len(n) should become 1:n slice
  expect_true(grepl("for __mojor_i1 in range", res$mojo))
  expect_true(grepl("out\\[__mojor_i1\\] = (Float64\\(k\\)|k)", res$mojo))
})

test_that("IR handles slice with expression RHS", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[1:n] <- x[i] * 2.0
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  expect_true(grepl("for __mojor_i1 in range", res$mojo))
  expect_true(grepl("_mojor_read_f64\\(x", res$mojo))
  expect_true(grepl("\\* (Float64\\(2\\)|2\\.0)", res$mojo))
})

test_that("IR handles slice starting at different index", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:10) {
      out[2:n] <- i
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # 2:n should start at index 1 (2-1)
  expect_true(grepl("for __mojor_i1 in range\\(1,", res$mojo))
})

test_that("IR handles slice with end expression", {  f <- function(n) {
    out <- numeric(n)
    for (i in 1:3) {
      out[2:(n - 1)] <- i
    }
    out
  }

  res <- mojor_transpile(f, n = "i32")

  expect_true(grepl("n - 1", res$mojo))
})

test_that("IR slice assignment without outer loop not yet supported", {  skip("Standalone slice assignment requires different validation logic")
  f <- function(x, n) {
    out <- numeric(n)
    out[1:n] <- 0
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")
  expect_true(grepl("for __mojor_i1", res$mojo))
})

# =============================================================================
# Matrix Row Slicing
# =============================================================================

test_that("IR handles matrix row slice with missing column index", {  f <- function(x, y, n, m) {
    out <- matrix(0, n, m)
    for (i in 1:n) {
      out[i, ] <- i
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", y = "f64[]", n = "i32", m = "i32")

  # Should generate loop for missing column index
  expect_true(grepl("for __mojor_i2 in range", res$mojo))
  # Uses tensor indexing for matrix output
  expect_true(grepl("__mojor_tensor_out\\[", res$mojo) || grepl("out\\[", res$mojo))
})

test_that("IR handles matrix row assignment with expression", {  f <- function(x, y, n, m) {
    out <- matrix(0, n, m)
    for (i in 1:n) {
      out[i, ] <- x[i] + y[i]
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", y = "f64[]", n = "i32", m = "i32")

  # Should have loop and use both x and y arrays
  expect_true(grepl("for __mojor_i2 in range", res$mojo))
  expect_true(grepl("x", res$mojo) && grepl("y", res$mojo))
})

# =============================================================================
# Array Slice Assignments (3D+)
# =============================================================================

test_that("IR handles array slice assignment with scalar RHS", {  f <- function(n) {
    out <- array(0, dim = c(n, n, n))
    for (i in 1:n) {
      out[1:n, 1, 2] <- i
    }
    out
  }

  res <- mojor_transpile(f, n = "i32", ir_only = TRUE)

  expect_true(grepl("for __mojor_i1 in range", res$mojo))
  expect_true(grepl("__mojor_tensor_out\\[", res$mojo) || grepl("out\\[", res$mojo))
})
