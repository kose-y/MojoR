library(testthat)

# ---- Loop Unrolling Tests ----

test_that("unroll parameter is validated", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }
  
  # Valid unroll values
  expect_error(mojor_transpile(f, x = "f64[]", unroll = 1), NA)
  expect_error(mojor_transpile(f, x = "f64[]", unroll = 4), NA)
  expect_error(mojor_transpile(f, x = "f64[]", unroll = 16), NA)
  
  # Invalid unroll values
  expect_error(mojor_transpile(f, x = "f64[]", unroll = 0), "unroll must be an integer between 1 and 16")
  expect_error(mojor_transpile(f, x = "f64[]", unroll = 17), "unroll must be an integer between 1 and 16")
  expect_error(mojor_transpile(f, x = "f64[]", unroll = -1), "unroll must be an integer between 1 and 16")
  expect_error(mojor_transpile(f, x = "f64[]", unroll = "auto"), NULL)  # May error or may be coerced to NA
})

test_that("unroll parameter is returned in transpile output", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }
  
  trans <- mojor_transpile(f, x = "f64[]", unroll = 4)
  expect_equal(trans$unroll, 4)
  
  trans_default <- mojor_transpile(f, x = "f64[]")
  expect_null(trans_default$unroll)
})

test_that("unroll generates unrolled loop code", {  skip_if_no_mojo()
  
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }
  
  # Build with unroll=4
  built <- mojor_build(f, x = "f64[]", name = "t_unroll_4", unroll = 4, cache = FALSE, load = TRUE)
  
  # Verify correctness
  x <- as.double(1:20)
  expect_equal(built$func(x), f(x))
  
  # Test with non-multiple of unroll factor
  x2 <- as.double(1:22)  # 22 not divisible by 4
  expect_equal(built$func(x2), f(x2))
  
  # Test empty vector
  x3 <- numeric(0)
  expect_equal(built$func(x3), f(x3))
})

test_that("unroll=1 generates standard loop", {  skip_if_no_mojo()
  
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + 1
    }
    out
  }
  
  built <- mojor_build(f, x = "f64[]", name = "t_unroll_1", unroll = 1, cache = FALSE, load = TRUE)
  
  x <- as.double(1:10)
  expect_equal(built$func(x), f(x))
})

test_that("unroll factors preserve correctness", {  skip_if_no_mojo()
  
  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[i] * 3 - 1
    }
    out
  }
  
  x <- as.double(seq_len(37))
  n <- as.integer(length(x))
  
  for (factor in c(2, 4, 8, 16)) {
    built <- mojor_build(
      f,
      x = "f64[]",
      n = "i32",
      name = paste0("t_unroll_factor_", factor),
      unroll = factor,
      cache = FALSE,
      load = TRUE
    )
    expect_equal(built$func(x, n), f(x, n), info = paste("unroll factor", factor))
  }
})

# ---- Edge Cases ----

test_that("unroll handles loops with arithmetic body", {  skip_if_no_mojo()
  
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      # Complex arithmetic expression
      out[i] <- x[i] * x[i] + x[i] / 2
    }
    out
  }
  
  built <- mojor_build(f, x = "f64[]", name = "t_unroll_arith", unroll = 2, cache = FALSE, load = TRUE)
  
  x <- as.double(c(1, 4, 2, 9, 3, 16))
  expect_equal(built$func(x), f(x))
})

test_that("unroll is ignored for while loops", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    i <- 1L
    while (i <= length(x)) {
      out[i] <- x[i] * 2
      i <- i + 1L
    }
    out
  }
  
  # unroll parameter should be accepted but won't be applied to while loops
  # Just verify transpilation works
  trans <- mojor_transpile(f, x = "f64[]", name = "t_unroll_while", unroll = 4)
  expect_equal(trans$unroll, 4)
})
