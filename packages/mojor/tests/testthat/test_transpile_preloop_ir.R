library(testthat)

# Pre-loop Assignment IR Support Tests

# =============================================================================
# Scalar Pre-loop Assignments
# =============================================================================

test_that("IR handles scalar pre-loop assignment", {  f <- function(x, n) {
    out <- numeric(n)
    acc <- 0  # Pre-loop scalar
    for (i in 1:n) {
      acc <- acc + x[i]
      out[i] <- acc
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for pre-loop scalar declaration
  expect_true(grepl("var acc.*=.*0", res$mojo))
  expect_true(grepl("for i in range", res$mojo))
})

test_that("IR handles multiple scalar pre-loop assignments", {  f <- function(x, n) {
    out <- numeric(n)
    sum_val <- 0
    count <- 0
    for (i in 1:n) {
      sum_val <- sum_val + x[i]
      count <- count + 1
      out[i] <- sum_val / count
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for both pre-loop scalars
  expect_true(grepl("var sum_val.*=.*0", res$mojo))
  expect_true(grepl("var count.*=.*0", res$mojo))
})

# =============================================================================
# Indexed Pre-loop Assignments
# =============================================================================

test_that("IR handles indexed pre-loop assignment with const", {  f <- function(x, n) {
    out <- numeric(n)
    out[1] <- 999  # Pre-loop indexed with const
    for (i in 2:n) {
      out[i] <- x[i] * 2
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for pre-loop indexed assignment
  expect_mojor_any_match(res$mojo, "out\\[Int\\((\\(1 - 1\\)|0)\\)\\].*=.*999")
  expect_true(grepl("for i in range\\(2", res$mojo))
})

test_that("IR handles multiple indexed pre-loop assignments", {  f <- function(x, n) {
    out <- numeric(n)
    out[1] <- 100
    out[2] <- 200
    for (i in 3:n) {
      out[i] <- x[i]
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for both pre-loop assignments
  expect_mojor_any_match(res$mojo, "out\\[Int\\((\\(1 - 1\\)|0)\\)\\].*=.*100")
  expect_mojor_any_match(res$mojo, "out\\[Int\\((\\(2 - 1\\)|1)\\)\\].*=.*200")
})

# =============================================================================
# Pre-loop If/Else
# =============================================================================

test_that("IR handles pre-loop if statement", {  f <- function(x, n, init) {
    out <- numeric(n)
    if (init) {
      out[1] <- 999
    }
    for (i in 2:n) {
      out[i] <- x[i]
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32", init = "bool")

  # Check for pre-loop if
  expect_true(grepl("if.*init", res$mojo))
  expect_mojor_any_match(res$mojo, "out\\[Int\\((\\(1 - 1\\)|0)\\)\\].*=.*999")
})

test_that("IR handles pre-loop if/else", {  f <- function(x, n, init_first) {
    out <- numeric(n)
    if (init_first) {
      out[1] <- 100
    } else {
      out[1] <- 200
    }
    for (i in 2:n) {
      out[i] <- x[i]
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32", init_first = "bool")

  # Check for pre-loop if/else
  expect_true(grepl("if.*init_first", res$mojo))
  expect_true(grepl("else:", res$mojo))
  expect_true(grepl("100", res$mojo))
  expect_true(grepl("200", res$mojo))
})

test_that("IR handles nested pre-loop if/else", {  f <- function(x, n, mode) {
    out <- numeric(n)
    if (mode > 0) {
      if (mode > 10) {
        out[1] <- 1000
      } else {
        out[1] <- 100
      }
    } else {
      out[1] <- 0
    }
    for (i in 2:n) {
      out[i] <- x[i]
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32", mode = "i32")

  # Check for nested if/else
  expect_true(grepl("if.*mode.*>.*0", res$mojo))
  expect_true(grepl("if.*mode.*>.*10", res$mojo))
})

# =============================================================================
# Pre-loop with While Loops
# =============================================================================

test_that("IR handles pre-loop assignments before while", {  f <- function(x, n) {
    out <- numeric(n)
    out[1] <- 999  # Pre-loop indexed
    i <- 2         # Pre-loop scalar
    while (i <= n) {
      out[i] <- x[i] * 2
      i <- i + 1
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for pre-loop assignments
  expect_mojor_any_match(res$mojo, "out\\[Int\\((\\(1 - 1\\)|0)\\)\\].*=.*999")
  expect_true(grepl("var i.*=.*2", res$mojo))
  expect_true(grepl("while.*i.*<=.*n", res$mojo))
})

# =============================================================================
# Mixed Pre-loop Patterns
# =============================================================================

test_that("IR handles mixed pre-loop scalar and indexed", {  f <- function(x, n) {
    out <- numeric(n)
    acc <- 0
    out[1] <- acc
    for (i in 2:n) {
      acc <- acc + x[i]
      out[i] <- acc
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for both pre-loop patterns
  expect_true(grepl("var acc.*=.*0", res$mojo))
  expect_mojor_any_match(res$mojo, "out\\[Int\\((\\(1 - 1\\)|0)\\)\\].*=.*acc")
})

test_that("IR handles pre-loop assignment with expression", {  f <- function(x, n, init_val) {
    out <- numeric(n)
    out[1] <- init_val * 2
    for (i in 2:n) {
      out[i] <- x[i]
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32", init_val = "f64")

  # Check for pre-loop assignment with expression
  expect_mojor_any_match(res$mojo, "out\\[Int\\((\\(1 - 1\\)|0)\\)\\].*=.*init_val.*\\*")
})

# =============================================================================
# Edge Cases
# =============================================================================

test_that("IR handles function with only pre-loop assignments (no loop)", {  # This pattern is unusual but valid R
  f <- function(n) {
    out <- numeric(n)
    out[1] <- 100
    out[2] <- 200
    out
  }

  res <- mojor_transpile(f, n = "i32")
  expect_true(is.character(res$mojo) && nzchar(res$mojo))
  expect_true(grepl("out\\[", res$mojo))
})

test_that("IR handles pre-loop with multiple statements before loop", {  f <- function(x, n) {
    out <- numeric(n)
    temp1 <- 0
    temp2 <- 0
    out[1] <- 999
    for (i in 2:n) {
      out[i] <- x[i]
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check all pre-loop statements are emitted
  expect_true(grepl("var temp1", res$mojo))
  expect_true(grepl("var temp2", res$mojo))
  expect_mojor_any_match(res$mojo, "out\\[Int\\((\\(1 - 1\\)|0)\\)\\]")
})
