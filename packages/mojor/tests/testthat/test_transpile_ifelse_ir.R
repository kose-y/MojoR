library(testthat)

# Phase 6: ifelse IR Support Tests

test_that("IR emits simple ifelse as ternary", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- ifelse(x[i] > 0, x[i] * 2, 0)
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for ternary expression pattern: (yes if cond else no)
  expect_true(grepl("\\(.* if .* else .*\\)", res$mojo))
  # Should NOT have if/else statement blocks
  expect_false(grepl("if \\(.*\\):\\s*\n.*else:", res$mojo))
})

test_that("IR handles ifelse with mixed types", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- ifelse(x[i] > 0, x[i], 1)
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for ternary expression
  expect_true(grepl("if .* else", res$mojo))
  expect_mojor_any_match(res$mojo, c("Float64\\(1\\)", "1\\.0"))
})

test_that("IR handles nested ifelse", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- ifelse(x[i] > 0,
                       ifelse(x[i] > 10, 20, 10),
                       0)
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for nested ternary - should have parentheses wrapping inner ifelse
  expect_true(grepl("\\(\\(.* if .* else .*\\) if .* else .*\\)", res$mojo))
})

test_that("IR handles ifelse in condition", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      if (ifelse(x[i] > 0, TRUE, FALSE)) {
        out[i] <- 1
      }
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check that ifelse is emitted as ternary even in condition
  expect_true(grepl("if .* else", res$mojo))
})

test_that("IR handles ifelse with logical result", {  f <- function(x, n) {
    out <- logical(n)
    for (i in 1:n) {
      out[i] <- ifelse(x[i] > 0, TRUE, FALSE)
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for ternary expression
  expect_true(grepl("if .* else", res$mojo))
  expect_mojor_any_match(res$mojo, "Int32\\(")
})

test_that("IR handles ifelse with integer branches", {  f <- function(x, n) {
    out <- integer(n)
    for (i in 1:n) {
      out[i] <- ifelse(x[i] > 0, 10L, 0L)
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for ternary expression
  expect_true(grepl("if .* else", res$mojo))
  # Should use Int32 type
  expect_true(grepl("Int32", res$mojo))
})
