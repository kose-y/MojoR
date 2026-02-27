test_that("optimization passes are integrated into transpiler", {  skip_if_no_mojo()

  # Simple test to verify optimization doesn't break compilation
  f <- function(x = numeric()) {
    out <- numeric(10)
    for (i in 1:10) {
      out[i] <- x[i] * 2
    }
    out
  }

  # Build and run
  built <- mojor_build(f, x = "f64[]", cache = FALSE, load = TRUE)
  expect_true(built$success)

  # Test correctness
  x <- 1:10
  result <- built$func(x)
  expected <- x * 2

  expect_equal(result, expected)
})

test_that("constant folding works in transpiler", {  skip_if_no_mojo()

  # Constant expression should be folded
  f <- function(x = numeric()) {
    out <- numeric(5)
    for (i in 1:5) {
      out[i] <- x[i] * (2 + 3)  # Should fold to x[i] * 5
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", cache = FALSE, load = TRUE)
  expect_true(built$success)

  x <- 1:5
  result <- built$func(x)
  expected <- x * 5

  expect_equal(result, expected)
})

test_that("optimization preserves correctness", {  skip_if_no_mojo()

  # Multiple optimization opportunities
  f <- function(x = numeric()) {
    out <- numeric(10)
    for (i in 1:10) {
      # CSE opportunity: (i + 1) appears twice
      out[i] <- (i + 1) * (i + 1) + x[i]
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", cache = FALSE, load = TRUE)
  expect_true(built$success)

  x <- 1:10
  result <- built$func(x)

  expected <- numeric(10)
  for (i in 1:10) {
    expected[i] <- (i + 1) * (i + 1) + x[i]
  }

  expect_equal(result, expected)
})

test_that("CSE and constant folding interaction", {  skip_if_no_mojo()

  # CSE should extract common subexpr, then fold it
  f <- function(x = numeric()) {
    out <- numeric(10)
    for (i in 1:10) {
      # (2 + 3) appears twice, CSE extracts it, fold evaluates it
      out[i] <- x[i] * (2 + 3) + (2 + 3)
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", cache = FALSE, load = TRUE)
  expect_true(built$success)

  x <- 1:10
  result <- built$func(x)
  expected <- x * 5 + 5

  expect_equal(result, expected)
})

test_that("LICM with math functions", {  skip_if_no_mojo()

  # sin(3.14159) is loop-invariant, should be hoisted
  f <- function(x = numeric()) {
    out <- numeric(10)
    for (i in 1:10) {
      out[i] <- x[i] + sin(3.14159)
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", cache = FALSE, load = TRUE)
  expect_true(built$success)

  x <- 1:10
  result <- built$func(x)
  expected <- x + sin(3.14159)

  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("nested loops with multiple optimizations", {  skip_if_no_mojo()

  # Constant folding in inner loop
  f <- function(x = numeric()) {
    out <- numeric(25)
    k <- 1
    for (i in 1:5) {
      for (j in 1:5) {
        out[k] <- x[k] * (i + 2) * (1 + 1)
        k <- k + 1
      }
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", cache = FALSE, load = TRUE)
  expect_true(built$success)

  x <- 1:25
  result <- built$func(x)

  expected <- numeric(25)
  k <- 1
  for (i in 1:5) {
    for (j in 1:5) {
      expected[k] <- x[k] * (i + 2) * 2
      k <- k + 1
    }
  }

  expect_equal(result, expected)
})

test_that("optimization with special values", {  skip_if_no_mojo()

  # Test that Inf constant folding works
  f <- function(x = numeric()) {
    out <- numeric(5)
    for (i in 1:5) {
      # Should fold Inf * 2 to Inf
      out[i] <- x[i] + 1e308 * 2
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", cache = FALSE, load = TRUE)
  expect_true(built$success)

  x <- 1:5
  result <- built$func(x)
  expected <- x + Inf

  expect_equal(result, expected)
})

test_that("CSE with array indexing", {  skip_if_no_mojo()

  # Repeated array accesses should be CSE'd
  f <- function(x = numeric()) {
    out <- numeric(10)
    for (i in 1:10) {
      # x[i] * 2 appears implicitly twice
      out[i] <- (x[i] + x[i]) * 3
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", cache = FALSE, load = TRUE)
  expect_true(built$success)

  x <- 1:10
  result <- built$func(x)
  expected <- (x + x) * 3

  expect_equal(result, expected)
})

test_that("full optimization pipeline stress test", {  skip_if_no_mojo()

  # Combines CSE, constant folding, LICM, and DCE opportunities
  f <- function(x = numeric()) {
    out <- numeric(10)
    for (i in 1:10) {
      # Multiple opportunities:
      # - (1 + 2) * (1 + 2) -> CSE + fold -> 9
      # - sin(0) -> fold -> 0
      # - Result doesn't use sin, so DCE might remove it
      tmp1 <- (1 + 2) * (1 + 2)
      tmp2 <- sin(0)
      out[i] <- x[i] * tmp1
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", cache = FALSE, load = TRUE)
  expect_true(built$success)

  x <- 1:10
  result <- built$func(x)
  expected <- x * 9

  expect_equal(result, expected)
})
