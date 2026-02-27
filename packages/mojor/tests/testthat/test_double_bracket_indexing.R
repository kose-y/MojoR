source("helper-mojo.R")

test_that("[[ indexing transpiles in loop read/write", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[[i]] <- x[[i]] + 1.0
    }
    out
  }

  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_double_bracket_transpile")
  expect_true(length(trans$mojo) > 0)
})

test_that("[[ indexing runtime matches R for vector transform", {  skip_if_no_mojo()

  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[[i]] <- x[[i]] * 2.0 + 1.0
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", n = "i32", name = "t_double_bracket_runtime_vec")

  x <- runif(32)
  expect_equal(built$func(x, 32L), f(x, 32L), tolerance = 1e-10)
})

test_that("[[ indexing runtime matches R for scalar reduction", {  skip_if_no_mojo()

  f <- function(x, n) {
    acc <- 0.0
    for (i in seq_len(n)) {
      acc <- acc + x[[i]]
    }
    acc
  }

  built <- mojor_build(f, x = "f64[]", n = "i32", name = "t_double_bracket_runtime_sum")

  x <- runif(64)
  expect_equal(built$func(x, 64L), f(x, 64L), tolerance = 1e-10)
})
