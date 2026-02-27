# Runtime tests for Phase 6.1 Stage B: Memory Safety
# These tests require Mojo compiler + backend to be available.

test_that("memory_check=TRUE builds and runs correctly (f64)", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2.0
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", memory_check = TRUE)
  result <- built$func(c(1.0, 2.0, 3.0))
  expect_equal(result, c(2.0, 4.0, 6.0))
})

test_that("memory_check=TRUE builds and runs correctly (i32)", {  skip_if_no_mojo()

  f <- function(x) {
    out <- integer(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + 1L
    }
    out
  }

  built <- mojor_build(f, x = "i32[]", memory_check = TRUE)
  result <- built$func(c(1L, 2L, 3L))
  expect_equal(result, c(2L, 3L, 4L))
})

test_that("memory_check=TRUE with two array args", {  skip_if_no_mojo()

  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i]
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", y = "f64[]", memory_check = TRUE)
  result <- built$func(c(1.0, 2.0), c(10.0, 20.0))
  expect_equal(result, c(11.0, 22.0))
})

test_that("memory_check=TRUE with scalar arg", {  skip_if_no_mojo()

  f <- function(x, alpha) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * alpha
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", alpha = "f64", memory_check = TRUE)
  result <- built$func(c(1.0, 2.0, 3.0), 10.0)
  expect_equal(result, c(10.0, 20.0, 30.0))
})

test_that("memory_check=TRUE C wrapper rejects NULL input", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2.0
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", memory_check = TRUE)
  expect_error(built$func(NULL), "received NULL")
})

test_that("memory_check=TRUE C wrapper rejects zero-length array", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2.0
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", memory_check = TRUE)
  expect_error(built$func(numeric(0)), "zero-length")
})

test_that("memory_check=TRUE rejects NA scalar", {  skip_if_no_mojo()

  f <- function(x, alpha) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * alpha
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", alpha = "f64", memory_check = TRUE)
  # NA is caught either by R-level na_mode='forbid' guard or C wrapper memory_check
  expect_error(built$func(c(1.0, 2.0), NA_real_), "NA|NaN")
})

test_that("memory_check=TRUE rejects NA integer scalar", {  skip_if_no_mojo()

  f <- function(x, k) {
    out <- integer(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + k
    }
    out
  }

  built <- mojor_build(f, x = "i32[]", k = "i32", memory_check = TRUE)
  # NA is caught either by R-level na_mode='forbid' guard or C wrapper memory_check
  expect_error(built$func(c(1L, 2L), NA_integer_), "NA")
})

test_that("memory_check=FALSE does NOT reject zero-length array", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2.0
    }
    out
  }

  # Without memory_check, zero-length should return empty (normal behavior)
  built <- mojor_build(f, x = "f64[]", memory_check = FALSE)
  result <- built$func(numeric(0))
  expect_length(result, 0)
})

test_that("memory_check=TRUE produces same results as production", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * x[i] + 1.0
    }
    out
  }

  x <- c(1.0, 2.0, 3.0, 4.0, 5.0)

  built_prod <- mojor_build(f, x = "f64[]", memory_check = FALSE)
  built_safe <- mojor_build(f, x = "f64[]", memory_check = TRUE)

  result_prod <- built_prod$func(x)
  result_safe <- built_safe$func(x)

  expect_equal(result_safe, result_prod)
  expect_equal(result_safe, x * x + 1.0)
})

test_that("memory_check=TRUE with larger array", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2.0 + 1.0
    }
    out
  }

  x <- seq(1.0, 100.0, by = 1.0)
  built <- mojor_build(f, x = "f64[]", memory_check = TRUE)
  result <- built$func(x)
  expect_equal(result, x * 2.0 + 1.0)
})
