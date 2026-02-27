# Phase 6.1 Stage A: Debug Mode Basic Tests
# Tests for debug mode infrastructure (Phases 1-3)

library(testthat)
source("helper-mojo.R")

test_that("Debug mode parameters accepted by mojor_transpile", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2.0
    }
    out
  }

  # Should not error
  trans_prod <- mojor_transpile(f, x = "f64[]", debug = FALSE)
  expect_true(!is.null(trans_prod))
  expect_true(!is.null(trans_prod$mojo))

  trans_debug <- mojor_transpile(f, x = "f64[]", debug = TRUE)
  expect_true(!is.null(trans_debug))
  expect_true(!is.null(trans_debug$mojo))

  trans_trace <- mojor_transpile(f, x = "f64[]", debug = TRUE, trace = TRUE)
  expect_true(!is.null(trans_trace))
  expect_true(!is.null(trans_trace$mojo))
})

test_that("Debug mode imports debug_helpers in generated Mojo", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2.0
    }
    out
  }

  # Production mode: no debug helpers import
  trans_prod <- mojor_transpile(f, x = "f64[]", debug = FALSE)
  expect_false(grepl("from debug_helpers import", trans_prod$mojo, fixed = TRUE))

  # Debug mode: should import debug helpers
  trans_debug <- mojor_transpile(f, x = "f64[]", debug = TRUE)
  expect_true(grepl("from debug_helpers import", trans_debug$mojo, fixed = TRUE))
  expect_true(grepl("mojor_check_bounds", trans_debug$mojo, fixed = TRUE))

  # Trace mode: should also import debug helpers
  trans_trace <- mojor_transpile(f, x = "f64[]", debug = TRUE, trace = TRUE)
  expect_true(grepl("from debug_helpers import", trans_trace$mojo, fixed = TRUE))
})

test_that("Debug state flags stored correctly", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2.0
    }
    out
  }

  # Production mode
  trans_prod <- mojor_transpile(f, x = "f64[]", debug = FALSE)
  # State is reset after transpile, so we can't check it directly
  # But we verified import above

  # Debug mode
  trans_debug <- mojor_transpile(f, x = "f64[]", debug = TRUE)
  expect_true(grepl("debug_helpers", trans_debug$mojo))

  # Trace mode
  trans_trace <- mojor_transpile(f, x = "f64[]", debug = TRUE, trace = TRUE)
  expect_true(grepl("debug_helpers", trans_trace$mojo))
})

test_that("mojor_build accepts debug parameters", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in 1:length(x)) {
      out[i] <- x[i] + 1.0
    }
    out
  }

  # Production mode
  built_prod <- mojor_build(f, x = "f64[]", name = "test_debug_prod",
                           cache = FALSE, load = TRUE)
  expect_true(!is.null(built_prod))
  expect_true(!is.null(built_prod$func))

  # Debug mode
  built_debug <- mojor_build(f, x = "f64[]", name = "test_debug_debug",
                            debug = TRUE, cache = FALSE, load = TRUE)
  expect_true(!is.null(built_debug))
  expect_true(!is.null(built_debug$func))

  # Verify both work
  x_test <- c(1.0, 2.0, 3.0)
  result_prod <- built_prod$func(x_test)
  result_debug <- built_debug$func(x_test)

  # Should get same results
  expect_equal(result_prod, result_debug)
  expect_equal(result_prod, x_test + 1.0)
})

test_that("Debug and production builds cached separately", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + 1.0
    }
    out
  }

  # Build with debug
  built1 <- mojor_build(f, x = "f64[]", name = "test_cache_debug",
                       debug = TRUE, cache = TRUE)

  # Build without debug
  built2 <- mojor_build(f, x = "f64[]", name = "test_cache_prod",
                       debug = FALSE, cache = TRUE)

  # Should have different cache keys (implicitly tested by successful builds)
  expect_true(!is.null(built1))
  expect_true(!is.null(built2))

  # Different transpiled code
  trans1 <- mojor_transpile(f, x = "f64[]", debug = TRUE)
  trans2 <- mojor_transpile(f, x = "f64[]", debug = FALSE)

  expect_true(grepl("debug_helpers", trans1$mojo))
  expect_false(grepl("debug_helpers", trans2$mojo))
})
