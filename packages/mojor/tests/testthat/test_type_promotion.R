library(testthat)

test_that("compiled subset: Automatic i32 to f64 promotion in arithmetic", {  skip_if_no_mojo()

  # The pattern from PHASE_5_PLAN.md that should now work without explicit cast
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * y[i] - 0.5  # Should auto-promote x[i] to f64
    }
    out
  }

  built <- mojor_build(
    f,
    x = "i32[]",  # Integer input
    y = "f64[]",  # Float input
    name = "t_auto_promote",
    cache = FALSE,
    load = TRUE
  )

  x <- as.integer(1:5)
  y <- as.double(c(1.5, 2, -1, 0.25, 3))
  expect_equal(built$func(x, y), as.double(x) * y - 0.5)
})

test_that("Type promotion: i32 + f64 → f64", {  skip_if_no_mojo()

  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i]
    }
    out
  }

  built <- mojor_build(
    f,
    x = "i32[]",
    y = "f64[]",
    name = "t_i32_f64_add",
    cache = FALSE,
    load = TRUE
  )

  x <- as.integer(c(1, 2, 3))
  y <- c(1.5, 2.5, 3.5)
  expect_equal(built$func(x, y), as.double(x) + y)
})

test_that("Type promotion: f64 - i32 → f64", {  skip_if_no_mojo()

  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] - y[i]
    }
    out
  }

  built <- mojor_build(
    f,
    x = "f64[]",
    y = "i32[]",
    name = "t_f64_i32_sub",
    cache = FALSE,
    load = TRUE
  )

  x <- c(10.5, 20.5, 30.5)
  y <- as.integer(c(1, 2, 3))
  expect_equal(built$func(x, y), x - as.double(y))
})

test_that("Type promotion: i32 * i32 * f64 → f64", {  skip_if_no_mojo()

  f <- function(x, y, z) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * y[i] * z[i]
    }
    out
  }

  built <- mojor_build(
    f,
    x = "i32[]",
    y = "i32[]",
    z = "f64[]",
    name = "t_multi_promote",
    cache = FALSE,
    load = TRUE
  )

  x <- as.integer(c(2, 3, 4))
  y <- as.integer(c(5, 6, 7))
  z <- c(0.5, 1.5, 2.5)
  expect_equal(built$func(x, y, z), as.double(x) * as.double(y) * z)
})

test_that("Type promotion: i32 / i32 → i32 (integer division)", {  skip_if_no_mojo()

  f <- function(x, y) {
    out <- integer(length(x))
    for (i in seq_along(x)) {
      out[i] <- as.integer(x[i] / y[i])  # Need explicit cast for integer output
    }
    out
  }

  built <- mojor_build(
    f,
    x = "i32[]",
    y = "i32[]",
    name = "t_i32_div_i32",
    cache = FALSE,
    load = TRUE
  )

  x <- as.integer(c(10, 20, 30))
  y <- as.integer(c(3, 4, 7))
  expect_equal(built$func(x, y), as.integer(x / y))
})

test_that("Type promotion: complex expression with mixed types", {  skip_if_no_mojo()

  f <- function(w, x, y, z) {
    out <- numeric(length(w))
    for (i in seq_along(w)) {
      out[i] <- (w[i] + x[i]) * y[i] - z[i]
    }
    out
  }

  built <- mojor_build(
    f,
    w = "i32[]",
    x = "f64[]",
    y = "i32[]",
    z = "f64[]",
    name = "t_complex_promote",
    cache = FALSE,
    load = TRUE
  )

  w <- as.integer(c(1, 2, 3))
  x <- c(0.5, 1.5, 2.5)
  y <- as.integer(c(2, 3, 4))
  z <- c(0.1, 0.2, 0.3)
  expect_equal(
    built$func(w, x, y, z),
    (as.double(w) + x) * as.double(y) - z
  )
})

test_that("Type promotion: comparison operators i32 vs f64", {  skip_if_no_mojo()

  f <- function(x, threshold) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- ifelse(x[i] > threshold[i], 1.0, 0.0)
    }
    out
  }

  built <- mojor_build(
    f,
    x = "i32[]",
    threshold = "f64[]",
    name = "t_comp_promote",
    cache = FALSE,
    load = TRUE
  )

  x <- as.integer(c(1, 5, 10))
  threshold <- c(2.5, 4.5, 9.5)
  expect_equal(
    built$func(x, threshold),
    ifelse(as.double(x) > threshold, 1.0, 0.0)
  )
})

test_that("Type promotion: bool to i32 in arithmetic context", {  skip_if_no_mojo()

  f <- function(x) {
    out <- integer(length(x))
    for (i in seq_along(x)) {
      out[i] <- as.integer(x[i] > 0) * 2L
    }
    out
  }

  built <- mojor_build(
    f,
    x = "i32[]",
    name = "t_bool_to_i32",
    cache = FALSE,
    load = TRUE
  )

  x <- as.integer(c(-2, 0, 3, -1, 5))
  expect_equal(built$func(x), as.integer(x > 0) * 2L)
})

test_that("Type promotion works in nested expressions", {  skip_if_no_mojo()

  f <- function(x, y, a) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- (x[i] * 2L) / (y[i] + a[i])
    }
    out
  }

  built <- mojor_build(
    f,
    x = "i32[]",
    y = "f64[]",
    a = "f64[]",
    name = "t_nested_promote",
    cache = FALSE,
    load = TRUE
  )

  x <- as.integer(c(10, 20, 30))
  y <- c(1.5, 2.5, 3.5)
  a <- c(0.5, 1.5, 2.5)
  expect_equal(
    built$func(x, y, a),
    (as.double(x) * 2.0) / (y + a)
  )
})

test_that("Type promotion: scalar constants with mixed types", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + 10L + 0.5  # i32 constant + f64 constant
    }
    out
  }

  built <- mojor_build(
    f,
    x = "i32[]",
    name = "t_const_promote",
    cache = FALSE,
    load = TRUE
  )

  x <- as.integer(c(1, 2, 3))
  expect_equal(built$func(x), as.double(x) + 10.0 + 0.5)
})
