library(testthat)

test_that("scalar & in loop conditions matches R at runtime", {  skip_if_no_mojo()
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      if ((x[i] > 0) & (y[i] > 0)) {
        out[i] <- x[i] + y[i]
      } else {
        out[i] <- 0
      }
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", y = "f64[]", name = "t_logic_amp_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(1, -2, 3, -4))
  y <- as.double(c(5, 6, -7, -8))
  expect_equal(built$func(x, y), f(x, y))
})

test_that("scalar | in loop conditions matches R at runtime", {  skip_if_no_mojo()
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      if ((x[i] > 0) | (y[i] > 0)) {
        out[i] <- x[i] - y[i]
      } else {
        out[i] <- 0
      }
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", y = "f64[]", name = "t_logic_pipe_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(1, -2, 3, -4))
  y <- as.double(c(5, 6, -7, -8))
  expect_equal(built$func(x, y), f(x, y))
})

test_that("xor() in loop conditions matches R at runtime", {  skip_if_no_mojo()
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      if (xor(x[i] > 0, y[i] > 0)) {
        out[i] <- 1
      } else {
        out[i] <- 0
      }
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", y = "f64[]", name = "t_logic_xor_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(1, -2, 3, -4))
  y <- as.double(c(5, 6, -7, -8))
  expect_equal(built$func(x, y), f(x, y))
})
