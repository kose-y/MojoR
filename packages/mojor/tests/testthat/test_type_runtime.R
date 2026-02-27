library(testthat)

test_that("i32 loop arithmetic yields integer output", {  skip_if_no_mojo()
  
  f <- function(x) {
    out <- integer(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2L + 1L
    }
    out
  }
  
  built <- mojor_build(
    f,
    x = "i32[]",
    name = "t_i32_arith",
    cache = FALSE,
    load = TRUE
  )
  
  x <- as.integer(c(-3, 0, 4, 7))
  expect_equal(built$func(x), x * 2L + 1L)
})

test_that("mixed i32 and f64 inputs work with explicit cast", {  skip_if_no_mojo()
  
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- as.double(x[i]) * y[i] - 0.5
    }
    out
  }
  
  built <- mojor_build(
    f,
    x = "i32[]",
    y = "f64[]",
    name = "t_mix_i32_f64",
    cache = FALSE,
    load = TRUE
  )
  
  x <- as.integer(1:5)
  y <- as.double(c(1.5, 2, -1, 0.25, 3))
  expect_equal(built$func(x, y), as.double(x) * y - 0.5)
})

test_that("mixed elementwise saxpy-style ops work with explicit casts", {  skip_if_no_mojo()

  f <- function(x, y, a, b) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- a * as.double(y[i]) + x[i] - as.double(b)
    }
    out
  }

  built <- mojor_build(
    f,
    x = "f64[]",
    y = "i32[]",
    a = "f64",
    b = "i32",
    name = "t_mix_saxpy",
    cache = FALSE,
    load = TRUE
  )

  x <- as.double(c(1.5, -2, 3, 0.25))
  y <- as.integer(c(2, -4, 1, 8))
  expect_equal(built$func(x, y, 0.5, 3L), 0.5 * as.double(y) + x - 3)
})

test_that("f32 loop arithmetic matches expected values", {  skip_if_no_mojo()
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not installed")
  }
  
  f <- function(x) {
    out <- float::float32(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 1.5 + 0.25
    }
    out
  }
  
  built <- mojor_build(
    f,
    x = "f32[]",
    name = "t_f32_arith",
    cache = FALSE,
    load = TRUE
  )
  
  x <- float::fl(c(1.25, -2.5, 3.75, 0))
  res <- built$func(x)
  expect_equal(
    float::dbl(res),
    float::dbl(x) * 1.5 + 0.25,
    tolerance = 1e-6
  )
})

test_that("f64 output accepts f32 inputs with explicit cast", {  skip_if_no_mojo()
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not installed")
  }

  f <- function(x, y) {
    out <- numeric(length(y))
    for (i in seq_along(y)) {
      out[i] <- as.double(x[i]) + y[i]
    }
    out
  }

  built <- mojor_build(
    f,
    x = "f32[]",
    y = "f64[]",
    name = "t_f32_f64_cast",
    cache = FALSE,
    load = TRUE
  )

  x <- float::fl(c(1.25, -2.5, 3.75, 0))
  y <- as.double(c(0.5, 2, -1.5, 4))
  res <- built$func(x, y)
  expect_equal(
    res,
    float::dbl(x) + y,
    tolerance = 1e-9
  )
})

test_that("f32 mixed ops with two arrays are correct", {  skip_if_no_mojo()
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not installed")
  }
  
  f <- function(x, y) {
    out <- float::float32(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * y[i] + x[i] - y[i]
    }
    out
  }
  
  built <- mojor_build(
    f,
    x = "f32[]",
    y = "f32[]",
    name = "t_f32_mixed_ops",
    cache = FALSE,
    load = TRUE
  )
  
  x <- float::fl(c(1.25, -2.5, 3.75, 0))
  y <- float::fl(c(0.5, 2, -1.5, 4))
  res <- built$func(x, y)
  expect_equal(
    float::dbl(res),
    float::dbl(x) * float::dbl(y) + float::dbl(x) - float::dbl(y),
    tolerance = 1e-6
  )
})

test_that("f32 reductions produce correct results", {  skip_if_no_mojo()
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not installed")
  }
  
  f_sum <- function(x) {
    acc <- 0.0
    for (i in seq_along(x)) {
      acc <- acc + as.double(x[i])
    }
    acc
  }
  f_min <- function(x) {
    acc <- 0.0
    for (i in seq_along(x)) {
      val <- as.double(x[i])
      if (i == 1) {
        acc <- val
      } else if (val < acc) {
        acc <- val
      }
    }
    acc
  }
  f_max <- function(x) {
    acc <- 0.0
    for (i in seq_along(x)) {
      val <- as.double(x[i])
      if (i == 1) {
        acc <- val
      } else if (val > acc) {
        acc <- val
      }
    }
    acc
  }
  
  built_sum <- mojor_build(f_sum, x = "f32[]", name = "t_f32_sum", cache = FALSE, load = TRUE)
  built_min <- mojor_build(f_min, x = "f32[]", name = "t_f32_min", cache = FALSE, load = TRUE)
  built_max <- mojor_build(f_max, x = "f32[]", name = "t_f32_max", cache = FALSE, load = TRUE)
  
  x <- float::fl(c(1.25, -2.5, 3.75, 0.5))
  expect_equal(as.double(built_sum$func(x)), sum(float::dbl(x)), tolerance = 1e-6)
  expect_equal(as.double(built_min$func(x)), min(float::dbl(x)))
  expect_equal(as.double(built_max$func(x)), max(float::dbl(x)))
})

test_that("f32 + i32 mixed ops with explicit cast are correct", {  skip_if_no_mojo()
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not installed")
  }
  
  f <- function(x, y) {
    out <- float::float32(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + as.single(as.double(y[i]) * 0.5)
    }
    out
  }
  
  built <- mojor_build(
    f,
    x = "f32[]",
    y = "i32[]",
    name = "t_f32_i32_mixed",
    cache = FALSE,
    load = TRUE
  )
  
  x <- float::fl(c(1.0, -2.0, 3.5, 0.25))
  y <- as.integer(c(2, -4, 1, 8))
  res <- built$func(x, y)
  expect_equal(
    float::dbl(res),
    float::dbl(x) + as.double(y) * 0.5,
    tolerance = 1e-6
  )
})

test_that("f32 output works with f64 + i32 mixed inputs via as.single", {  skip_if_no_mojo()
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not installed")
  }

  f <- function(x, y) {
    out <- float::float32(length(x))
    for (i in seq_along(x)) {
      out[i] <- as.single(x[i] * 0.25 + as.double(y[i]))
    }
    out
  }

  built <- mojor_build(
    f,
    x = "f64[]",
    y = "i32[]",
    name = "t_f32_from_mixed",
    cache = FALSE,
    load = TRUE
  )

  x <- as.double(c(1.5, -2, 3, 0.25))
  y <- as.integer(c(2, -4, 1, 8))
  res <- built$func(x, y)
  expect_equal(
    float::dbl(res),
    x * 0.25 + as.double(y),
    tolerance = 1e-6
  )
})
