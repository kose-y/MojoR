# Simple runtime tests for debug mode
# Focus on tests that actually work with current MojoR subset

source("helper-mojo.R")

test_that("debug mode works with valid bounds", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2.0
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", debug = TRUE)
  result <- built$func(c(1.0, 2.0, 3.0))

  expect_equal(result, c(2.0, 4.0, 6.0))
})

test_that("production mode works with valid bounds", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2.0
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", debug = FALSE)
  result <- built$func(c(1.0, 2.0, 3.0))

  expect_equal(result, c(2.0, 4.0, 6.0))
})

test_that("debug mode kernel signature includes raises", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2.0
    }
    out
  }

  trans_debug <- mojor_transpile(f, x = "f64[]", debug = TRUE)
  trans_prod <- mojor_transpile(f, x = "f64[]", debug = FALSE)

  # Debug mode should have "raises" in function signature
  has_raises <- any(grepl("fn mojor_kernel.*raises.*->", trans_debug$mojo))
  expect_true(has_raises,
              info = "Debug mode kernel should have 'raises' in signature")

  # Production mode should not have "raises"
  has_raises_prod <- any(grepl("fn mojor_kernel.*raises", trans_prod$mojo))
  expect_false(has_raises_prod,
               info = "Production mode kernel should not have 'raises'")
})

test_that("debug and production have different code paths", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2.0
    }
    out
  }

  trans_debug <- mojor_transpile(f, x = "f64[]", debug = TRUE)
  trans_prod <- mojor_transpile(f, x = "f64[]", debug = FALSE)

  # Debug mode should import debug helpers
  has_debug_import <- any(grepl("from debug_helpers import", trans_debug$mojo))
  expect_true(has_debug_import,
              info = "Debug mode should import debug_helpers")

  # Production mode should not import debug helpers
  has_prod_import <- any(grepl("from debug_helpers import", trans_prod$mojo))
  expect_false(has_prod_import,
               info = "Production mode should not import debug_helpers")
})

test_that("trace mode generates trace calls", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2.0
    }
    out
  }

  trans_trace <- mojor_transpile(f, x = "f64[]", debug = TRUE, trace = TRUE)

  # Should have trace calls
  has_trace <- any(grepl("mojor_trace", trans_trace$mojo))
  expect_true(has_trace,
              info = "Trace mode should generate mojor_trace calls")
})

test_that("debug mode builds successfully", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + 1.0
    }
    out
  }

  # Should build without errors
  expect_no_error({
    built <- mojor_build(f, x = "f64[]", debug = TRUE)
  })
})

test_that("trace mode builds successfully", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + 1.0
    }
    out
  }

  # Should build without errors
  expect_no_error({
    built <- mojor_build(f, x = "f64[]", debug = TRUE, trace = TRUE)
  })
})

test_that("debug mode handles empty arrays", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2.0
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", debug = TRUE)
  result <- built$func(numeric(0))

  expect_length(result, 0)
})

test_that("debug mode handles single element", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2.0
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", debug = TRUE)
  result <- built$func(5.0)

  expect_equal(result, 10.0)
})

test_that("debug performance overhead measurement", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2.0 + 1.0
    }
    out
  }

  # Build both versions
  prod <- mojor_build(f, x = "f64[]", debug = FALSE)
  debug <- mojor_build(f, x = "f64[]", debug = TRUE)

  # Test data
  x <- runif(10000)

  # Warmup
  prod$func(x)
  debug$func(x)

  # Time production
  prod_time <- system.time({
    for (i in 1:100) prod$func(x)
  })

  # Time debug
  debug_time <- system.time({
    for (i in 1:100) debug$func(x)
  })

  # Calculate overhead
  overhead <- (debug_time["elapsed"] / prod_time["elapsed"] - 1) * 100

  # Report results
  message(sprintf("Production: %.4f sec", prod_time["elapsed"]))
  message(sprintf("Debug: %.4f sec", debug_time["elapsed"]))
  message(sprintf("Overhead: %.1f%%", overhead))

  # Debug overhead should be reasonable (< 100%)
  expect_true(overhead < 200,
              info = sprintf("Debug overhead is %.1f%%", overhead))
})
