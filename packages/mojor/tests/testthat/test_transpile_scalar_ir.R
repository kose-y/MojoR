library(testthat)

# Scalar Output IR Support Tests

# =============================================================================
# Basic Scalar Outputs
# =============================================================================

test_that("IR handles scalar sum accumulator", {  f <- function(x) {
    acc <- 0
    for (i in seq_along(x)) {
      acc <- acc + x[i]
    }
    acc
  }

  res <- mojor_transpile(f, x = "f64[]")

  # Check for scalar output pattern
  expect_equal(res$out_kind, "scalar")
  expect_true(grepl("var acc: Float64 = 0", res$mojo))
  expect_true(grepl("acc_out\\[0\\] = acc", res$mojo))
})

test_that("IR handles scalar product accumulator", {  f <- function(x) {
    prod <- 1
    for (i in seq_along(x)) {
      prod <- prod * x[i]
    }
    prod
  }

  res <- mojor_transpile(f, x = "f64[]")

  expect_equal(res$out_kind, "scalar")
  expect_true(grepl("var prod: Float64 = 1", res$mojo))
  expect_true(grepl("prod_out\\[0\\] = prod", res$mojo))
})

test_that("IR handles scalar with integer type", {  f <- function(x) {
    count <- 0
    for (i in seq_along(x)) {
      if (x[i] > 0) {
        count <- count + 1
      }
    }
    count
  }

  res <- mojor_transpile(f, x = "f64[]")

  expect_equal(res$out_kind, "scalar")
  expect_true(grepl("var count.*=.*0", res$mojo))
})

# =============================================================================
# Scalar with Return Expressions
# =============================================================================

test_that("IR handles return with division expression (mean)", {  f <- function(x) {
    acc <- 0
    for (i in seq_along(x)) {
      acc <- acc + x[i]
    }
    return(acc / length(x))
  }

  res <- mojor_transpile(f, x = "f64[]")

  expect_equal(res$out_kind, "scalar")
  # Check for division before writeback
  expect_true(grepl("acc = acc / Float64", res$mojo))
  expect_true(grepl("acc_out\\[0\\] = acc", res$mojo))
})

test_that("IR handles explicit return of scalar", {  f <- function(x) {
    acc <- 0
    for (i in seq_along(x)) {
      acc <- acc + x[i]
    }
    return(acc)
  }

  res <- mojor_transpile(f, x = "f64[]")

  expect_equal(res$out_kind, "scalar")
  expect_true(grepl("acc_out\\[0\\] = acc", res$mojo))
})

# =============================================================================
# Multiple Accumulators
# =============================================================================

test_that("IR handles multiple scalar variables", {  f <- function(x) {
    sum_val <- 0
    count <- 0
    for (i in seq_along(x)) {
      sum_val <- sum_val + x[i]
      count <- count + 1
    }
    sum_val / count
  }

  res <- mojor_transpile(f, x = "f64[]")

  expect_equal(res$out_kind, "scalar")
  expect_true(grepl("var sum_val", res$mojo))
  expect_true(grepl("var count", res$mojo))
})

# =============================================================================
# Scalar with While Loops
# =============================================================================

test_that("IR handles scalar with while loop", {  f <- function(x, threshold) {
    acc <- 0
    i <- 1
    while (i <= length(x) && acc < threshold) {
      acc <- acc + x[i]
      i <- i + 1
    }
    acc
  }

  res <- mojor_transpile(f, x = "f64[]", threshold = "f64")

  expect_equal(res$out_kind, "scalar")
  expect_true(grepl("while", res$mojo))
  expect_true(grepl("acc_out\\[0\\] = acc", res$mojo))
})

# =============================================================================
# Scalar with Pre-loop Initialization
# =============================================================================

test_that("IR handles scalar with pre-loop assignment", {  f <- function(x) {
    acc <- x[1]  # Initialize from first element
    for (i in 2:length(x)) {
      acc <- acc + x[i]
    }
    acc
  }

  res <- mojor_transpile(f, x = "f64[]")

  expect_equal(res$out_kind, "scalar")
  # Should have pre-loop initialization (in var declaration)
  expect_true(grepl("var acc.*=.*x\\[0\\]", res$mojo))
})

test_that("IR handles scalar with multiple pre-loop scalars", {  f <- function(x) {
    acc <- x[1]
    temp <- 0
    for (i in 2:length(x)) {
      temp <- x[i]
      acc <- acc + temp
    }
    acc
  }

  res <- mojor_transpile(f, x = "f64[]")

  expect_equal(res$out_kind, "scalar")
  expect_true(grepl("var temp", res$mojo))
})

# =============================================================================
# Edge Cases
# =============================================================================

test_that("IR handles scalar with no loop (constant)", {  f <- function() {
    acc <- 42
    acc
  }

  expect_error(
    mojor_transpile(f),
    "provide type annotations|while loops require at least one array argument"
  )
})

test_that("IR handles scalar with nested loops", {  f <- function(x, n) {
    acc <- 0
    for (i in 1:n) {
      for (j in 1:n) {
        acc <- acc + x[i] * x[j]
      }
    }
    acc
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  expect_equal(res$out_kind, "scalar")
  expect_true(grepl("acc_out\\[0\\] = acc", res$mojo))
})

# =============================================================================
# Limitations (Documented)
# =============================================================================

test_that("early return in loop is supported in IR-only", {  f <- function(x) {
    acc <- 0
    for (i in seq_along(x)) {
      if (x[i] < 0) {
        return(acc)  # Early return
      }
      acc <- acc + x[i]
    }
    acc
  }

  # Should work with legacy fallback
  res <- mojor_transpile(f, x = "f64[]")
  expect_equal(res$out_kind, "scalar")
  expect_true(grepl("return", res$mojo))

  # With ir_only
  res_ir <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
  expect_true(grepl("acc_out\\[0\\] = acc", res_ir$mojo))
  expect_true(grepl("return", res_ir$mojo))
})
