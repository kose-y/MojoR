library(testthat)

# Inline Reduction IR Support Tests
#
# This test file proves that inline reduction patterns ALREADY WORK in IR.
# No new codegen needed - these patterns are simple loop+assignment that
# the IR layer handles naturally.
#
# Two types of reductions in MojoR:
# 1. Direct calls (mojor_sum_f64) - NOT transpilation, just .Call() to C kernel
# 2. Inline patterns (acc <- acc + x[i]) - THESE are transpiled to Mojo loops
#
# This file tests type 2 only.

# =============================================================================
# Transpilation Tests - Sum Pattern
# =============================================================================

test_that("inline sum pattern transpiles correctly in IR", {  f <- function(x) {
    acc <- 0
    for (i in seq_along(x)) {
      acc <- acc + x[i]
    }
    acc
  }

  result <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "acc = \\(?acc \\+")
  expect_match(result$mojo, "for i in range")
})

test_that("inline sum with complex expression transpiles", {  f <- function(x, y) {
    acc <- 0
    for (i in seq_along(x)) {
      acc <- acc + (x[i] * y[i])
    }
    acc
  }

  result <- mojor_transpile(f, x = "f64[]", y = "f64[]", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "acc = \\(?acc \\+")
  expect_match(result$mojo, "\\*")
})

test_that("inline sum with commutative pattern transpiles", {  f <- function(x) {
    acc <- 0
    for (i in seq_along(x)) {
      acc <- x[i] + acc  # reversed order
    }
    acc
  }

  result <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  # Should still generate valid code (commutativity of +)
  expect_match(result$mojo, "for i in range")
})

# =============================================================================
# Transpilation Tests - Product Pattern
# =============================================================================

test_that("inline product pattern transpiles correctly", {  f <- function(x) {
    acc <- 1
    for (i in seq_along(x)) {
      acc <- acc * x[i]
    }
    acc
  }

  result <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "acc = \\(?acc \\*")
  expect_match(result$mojo, "for i in range")
})

test_that("inline product with expression transpiles", {  f <- function(x) {
    acc <- 1
    for (i in seq_along(x)) {
      acc <- acc * (x[i] + 1.0)
    }
    acc
  }

  result <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "acc = \\(?acc \\*")
})

# =============================================================================
# Transpilation Tests - Min Pattern
# =============================================================================

test_that("inline min pattern transpiles correctly", {  f <- function(x) {
    acc <- Inf
    for (i in seq_along(x)) {
      acc <- min(acc, x[i])
    }
    acc
  }

  result <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "min\\(")
  expect_match(result$mojo, "for i in range")
})

test_that("inline min with expression transpiles", {  f <- function(x, y) {
    acc <- Inf
    for (i in seq_along(x)) {
      acc <- min(acc, x[i] + y[i])
    }
    acc
  }

  result <- mojor_transpile(f, x = "f64[]", y = "f64[]", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "min\\(")
})

test_that("inline min with reversed args transpiles", {  f <- function(x) {
    acc <- Inf
    for (i in seq_along(x)) {
      acc <- min(x[i], acc)  # reversed order
    }
    acc
  }

  result <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "min\\(")
})

# =============================================================================
# Transpilation Tests - Max Pattern
# =============================================================================

test_that("inline max pattern transpiles correctly", {  f <- function(x) {
    acc <- -Inf
    for (i in seq_along(x)) {
      acc <- max(acc, x[i])
    }
    acc
  }

  result <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "max\\(")
  expect_match(result$mojo, "for i in range")
})

test_that("inline max with expression transpiles", {  f <- function(x) {
    acc <- -Inf
    for (i in seq_along(x)) {
      acc <- max(acc, x[i] * 2.0)
    }
    acc
  }

  result <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "max\\(")
})

test_that("inline max with reversed args transpiles", {  f <- function(x) {
    acc <- -Inf
    for (i in seq_along(x)) {
      acc <- max(x[i], acc)  # reversed order
    }
    acc
  }

  result <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "max\\(")
})

# =============================================================================
# Transpilation Tests - Edge Cases
# =============================================================================

test_that("inline reduction with nested loop transpiles", {  f <- function(x, y) {
    acc <- 0
    for (i in seq_along(x)) {
      temp <- 0
      for (j in seq_along(y)) {
        temp <- temp + y[j]
      }
      acc <- acc + (x[i] * temp)
    }
    acc
  }

  result <- mojor_transpile(f, x = "f64[]", y = "f64[]", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  # Both loops should have reduction patterns
  expect_match(result$mojo, "acc = \\(?acc \\+")
  expect_match(result$mojo, "temp = .*\\+", perl = TRUE)
})

test_that("inline reduction with 1:n range works", {  f <- function(x, n) {
    acc <- 0
    for (i in 1:n) {
      acc <- acc + x[i]
    }
    acc
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "acc = \\(?acc \\+")
})

test_that("inline reduction with pre-loop init transpiles", {  f <- function(x) {
    acc <- x[1]  # init with first element
    for (i in 2:length(x)) {
      acc <- acc + x[i]
    }
    acc
  }

  result <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
  expect_true(!is.null(result$mojo))
  expect_match(result$mojo, "acc = \\(?acc \\+")
  # Should have pre-loop assignment for acc
  expect_match(result$mojo, "var acc")
})

# =============================================================================
# Runtime Tests - Sum
# =============================================================================

test_that("inline sum produces correct results", {  skip_if_no_mojo()

  f <- function(x) {
    acc <- 0
    for (i in seq_along(x)) {
      acc <- acc + x[i]
    }
    acc
  }

  built <- mojor_build(f, x = "f64[]", name = "inline_sum_test",
                       cache = FALSE, load = TRUE, ir_only = TRUE)
  x <- as.double(1:10)
  expect_equal(built$func(x), sum(x))

  # Test with different input
  x2 <- as.double(c(1.5, 2.5, 3.5, 4.5))
  expect_equal(built$func(x2), sum(x2))
})

test_that("inline sum with expression produces correct results", {  skip_if_no_mojo()

  f <- function(x, y) {
    acc <- 0
    for (i in seq_along(x)) {
      acc <- acc + (x[i] * y[i])
    }
    acc
  }

  built <- mojor_build(f, x = "f64[]", y = "f64[]", name = "inline_sum_expr_test",
                       cache = FALSE, load = TRUE, ir_only = TRUE)
  x <- as.double(1:5)
  y <- as.double(2:6)
  expect_equal(built$func(x, y), sum(x * y))
})

# =============================================================================
# Runtime Tests - Product
# =============================================================================

test_that("inline product produces correct results", {  skip_if_no_mojo()

  f <- function(x) {
    acc <- 1
    for (i in seq_along(x)) {
      acc <- acc * x[i]
    }
    acc
  }

  built <- mojor_build(f, x = "f64[]", name = "inline_prod_test",
                       cache = FALSE, load = TRUE, ir_only = TRUE)
  x <- as.double(1:5)
  expect_equal(built$func(x), prod(x))

  # Test with different input
  x2 <- as.double(c(2, 3, 4))
  expect_equal(built$func(x2), prod(x2))
})

# =============================================================================
# Runtime Tests - Min
# =============================================================================

test_that("inline min produces correct results", {  skip_if_no_mojo()

  f <- function(x) {
    acc <- Inf
    for (i in seq_along(x)) {
      acc <- min(acc, x[i])
    }
    acc
  }

  built <- mojor_build(f, x = "f64[]", name = "inline_min_test",
                       cache = FALSE, load = TRUE, ir_only = TRUE)
  x <- as.double(c(5, 2, 8, 1, 9))
  expect_equal(built$func(x), min(x))

  # Test with different input
  x2 <- as.double(c(10.5, 3.2, 7.8, 0.5))
  expect_equal(built$func(x2), min(x2))
})

test_that("inline min with init from first element works", {  skip_if_no_mojo()

  f <- function(x) {
    acc <- x[1]
    for (i in 2:length(x)) {
      acc <- min(acc, x[i])
    }
    acc
  }

  built <- mojor_build(f, x = "f64[]", name = "inline_min_init_test",
                       cache = FALSE, load = TRUE, ir_only = TRUE)
  x <- as.double(c(5, 2, 8, 1, 9))
  expect_equal(built$func(x), min(x))
})

# =============================================================================
# Runtime Tests - Max
# =============================================================================

test_that("inline max produces correct results", {  skip_if_no_mojo()

  f <- function(x) {
    acc <- -Inf
    for (i in seq_along(x)) {
      acc <- max(acc, x[i])
    }
    acc
  }

  built <- mojor_build(f, x = "f64[]", name = "inline_max_test",
                       cache = FALSE, load = TRUE, ir_only = TRUE)
  x <- as.double(c(5, 2, 8, 1, 9))
  expect_equal(built$func(x), max(x))

  # Test with different input
  x2 <- as.double(c(10.5, 3.2, 7.8, 20.5))
  expect_equal(built$func(x2), max(x2))
})

test_that("inline max with expression works", {  skip_if_no_mojo()

  f <- function(x) {
    acc <- -Inf
    for (i in seq_along(x)) {
      acc <- max(acc, x[i] * 2.0)
    }
    acc
  }

  built <- mojor_build(f, x = "f64[]", name = "inline_max_expr_test",
                       cache = FALSE, load = TRUE, ir_only = TRUE)
  x <- as.double(c(5, 2, 8, 1, 9))
  expect_equal(built$func(x), max(x * 2.0))
})

# =============================================================================
# Runtime Tests - Edge Cases
# =============================================================================

test_that("inline reduction with nested loops works", {  skip_if_no_mojo()

  f <- function(x, y) {
    acc <- 0
    for (i in seq_along(x)) {
      temp <- 0
      for (j in seq_along(y)) {
        temp <- temp + y[j]
      }
      acc <- acc + (x[i] * temp)
    }
    acc
  }

  built <- mojor_build(f, x = "f64[]", y = "f64[]", name = "inline_nested_test",
                       cache = FALSE, load = TRUE, ir_only = TRUE)
  x <- as.double(1:3)
  y <- as.double(1:4)

  # Expected: each x[i] multiplied by sum(y)
  expected <- sum(x * sum(y))
  expect_equal(built$func(x, y), expected)
})

test_that("inline reduction with single element works", {  skip_if_no_mojo()

  f <- function(x) {
    acc <- 0
    for (i in seq_along(x)) {
      acc <- acc + x[i]
    }
    acc
  }

  built <- mojor_build(f, x = "f64[]", name = "inline_single_test",
                       cache = FALSE, load = TRUE, ir_only = TRUE)
  x <- as.double(42)
  expect_equal(built$func(x), 42)
})

# =============================================================================
# Runtime Tests - Mixed Types
# =============================================================================

test_that("inline sum works with f32 arrays", {  skip_if_no_mojo()

  f <- function(x) {
    acc <- as.single(0)
    for (i in seq_along(x)) {
      acc <- acc + x[i]
    }
    acc
  }

  built <- mojor_build(f, x = "f32[]", name = "inline_sum_f32_test",
                       cache = FALSE, load = TRUE, ir_only = TRUE)
  x <- float::fl(1:5)
  result <- float::dbl(built$func(x))
  expected <- sum(float::dbl(x))
  expect_equal(result, expected, tolerance = 1e-6)
})
