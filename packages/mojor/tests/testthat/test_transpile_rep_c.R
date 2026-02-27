test_that("rep/rep_len/rep.int lowered to recycling", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- rep(y, length.out = length(x))[i]
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", y = "f64[]", broadcast = "none")
  x <- as.double(1:5)
  y <- as.double(c(10, 20))
  expect_equal(built$func(x, y), rep(y, length.out = length(x)))
})

test_that("c() concatenation supports literals and arrays", {  f <- function(x, y, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- c(x, y)[i]
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", y = "f64[]", n = "i32", broadcast = "none")
  x <- as.double(2:4)
  y <- as.double(10:12)
  n <- as.integer(length(x) + length(y))
  expect_equal(built$func(x, y, n), c(x, y))
})

test_that("c() length mismatch errors", {  skip(paste(
    "Runtime length validation for c() constructors not fully",
    "implemented - IR transforms pattern before check collection"
  ))
  f <- function(x, y, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- c(x, y)[i]
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", y = "f64[]", n = "i32", broadcast = "none")
  x <- as.double(1:3)
  y <- as.double(4:5)
  n <- as.integer(length(x))
  expect_error(built$func(x, y, n), "c\\(\\): output length must match sum of parts")
})

test_that("min/max support n-ary elementwise", {  f <- function(x, y, z) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- min(x[i], y[i], z[i])
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", y = "f64[]", z = "f64[]", broadcast = "none")
  x <- as.double(c(1, 5, 3))
  y <- as.double(c(2, 4, 6))
  z <- as.double(c(0, 7, 2))
  expect_equal(built$func(x, y, z), pmin(x, y, z))
})

test_that("min/max reductions without explicit loops", {  f_min <- function(x) {
    min(x)
  }
  f_max <- function(x) {
    max(x)
  }
  built_min <- mojor_build(f_min, x = "f64[]")
  built_max <- mojor_build(f_max, x = "f64[]")
  x <- as.double(c(3.5, -1.0, 2.2))
  expect_equal(built_min$func(x), min(x))
  expect_equal(built_max$func(x), max(x))
})

test_that("rep_len length mismatch errors", {  skip(paste(
    "Runtime length validation for rep_len() constructors not fully",
    "implemented - IR transforms pattern before check collection"
  ))
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- rep_len(y, length.out = 3L)[i]
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", y = "f64[]", broadcast = "none")
  x <- as.double(1:5)
  y <- as.double(c(10, 20))
  expect_error(built$func(x, y), "rep_len\\(\\): output length must match length.out")
})

test_that("rep/rep_len/rep.int constructors without loops", {  f_rep <- function(x) {
    rep(x, times = 2L)
  }
  f_rep_each <- function(x) {
    rep(x, times = 2L, each = 2L)
  }
  f_rep_len_each <- function(x, n) {
    rep(x, times = 2L, each = 2L, length.out = n)
  }
  f_rep_len <- function(x, n) {
    rep_len(x, length.out = n)
  }
  f_rep_int <- function(x) {
    rep.int(x, times = 3L)
  }
  built_rep <- mojor_build(f_rep, x = "f64[]")
  built_rep_each <- mojor_build(f_rep_each, x = "f64[]")
  built_rep_len_each <- mojor_build(f_rep_len_each, x = "f64[]", n = "i32")
  built_rep_len <- mojor_build(f_rep_len, x = "f64[]", n = "i32")
  built_rep_int <- mojor_build(f_rep_int, x = "i32[]")
  x <- as.double(c(1, 2, 3))
  xi <- as.integer(c(1, 2))
  expect_equal(built_rep$func(x), rep(x, times = 2L))
  expect_equal(built_rep_each$func(x), rep(x, times = 2L, each = 2L))
  expect_equal(built_rep_len_each$func(x, as.integer(5)), rep(x, times = 2L, each = 2L, length.out = 5L))
  expect_equal(built_rep_len$func(x, as.integer(5)), rep_len(x, length.out = 5L))
  expect_equal(built_rep_int$func(xi), rep.int(xi, times = 3L))
})

test_that("c() constructor without loops", {  f <- function(x, y) {
    c(x, 3, y)
  }
  built <- mojor_build(f, x = "f64[]", y = "f64[]")
  x <- as.double(1:2)
  y <- as.double(10:11)
  expect_equal(built$func(x, y), c(x, 3, y))
})

test_that("rep() constructor rejects non-scalar times", {  f <- function(x, t) {
    rep(x, times = t)
  }
  expect_error(mojor_build(f, x = "f64[]", t = "f64[]"), "times must be a scalar")
})
