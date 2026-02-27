library(testthat)


test_that("logical operators and unary ops transpile", {  logic_fn <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      if (!(x[i] > 0) || y[i] == 0) {
        out[i] <- -x[i]
      } else {
        out[i] <- x[i] + y[i]
      }
    }
    return(out)
  }

  res <- mojor_transpile(logic_fn, x = "f64[]", y = "f64[]", name = "mojor_logic")
  expect_match(res$mojo, "not")
  expect_match(res$mojo, "or")
  expect_match(res$mojo, "\\(")
  expect_match(res$mojo, "-_mojor_read_f64\\(x")
})

test_that("nested short-circuit &&/|| in conditions transpile", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      if ((x[i] > 0 && y[i] > 0) || (x[i] < 0 && y[i] < 0)) {
        out[i] <- x[i] + y[i]
      } else if (x[i] == 0 || y[i] == 0) {
        out[i] <- 0
      } else {
        out[i] <- x[i] - y[i]
      }
    }
    out
  }
  res <- mojor_transpile(f, x = "f64[]", y = "f64[]", name = "mojor_logic_nested")
  expect_match(res$mojo, "and")
  expect_match(res$mojo, "or")
})

test_that("scalar & in loop conditions lowers through IR strict path", {  f <- function(x, y) {
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

  res <- mojor_transpile(f, x = "f64[]", y = "f64[]", name = "mojor_logic_amp", ir_only = TRUE)
  expect_match(res$mojo, "and")
})

test_that("scalar | in loop conditions lowers through IR strict path", {  f <- function(x, y) {
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

  res <- mojor_transpile(f, x = "f64[]", y = "f64[]", name = "mojor_logic_pipe", ir_only = TRUE)
  expect_match(res$mojo, "or")
})

test_that("xor() in loop conditions lowers through IR strict path", {  f <- function(x, y) {
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

  res <- mojor_transpile(f, x = "f64[]", y = "f64[]", name = "mojor_logic_xor", ir_only = TRUE)
  expect_match(res$mojo, " != ")
})
