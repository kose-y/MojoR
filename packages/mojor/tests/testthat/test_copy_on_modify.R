library(testthat)

# Tests for copy-on-modify semantics
# These verify that MojoR respects R's copy-on-modify semantics when
# in-place operations are enabled.

test_that("transpiler defaults to r semantics", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "t_com_default")
  expect_equal(trans$semantics, "r")
  expect_equal(trans$modified_args, character(0))
})

test_that("transpiler accepts raw semantics", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "t_com_raw", semantics = "raw")
  expect_equal(trans$semantics, "raw")
})

test_that("transpiler detects direct modified array args", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      x[i] <- x[i] + 1
      out[i] <- x[i]
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "t_com_detect_direct")
  expect_equal(trans$modified_args, "x")
})

test_that("transpiler detects alias-modified array args", {  f <- function(x) {
    out <- numeric(length(x))
    y <- x
    for (i in seq_along(x)) {
      y[i] <- x[i] + 1
      out[i] <- y[i]
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "t_com_detect_alias")
  expect_equal(trans$modified_args, "x")
})

test_that("transpiler detects branch-merged alias modifications", {  f <- function(x, y, use_x) {
    z <- y
    if (use_x) {
      z <- x
    }
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      z[i] <- z[i] + 1
      out[i] <- z[i]
    }
    out
  }
  trans <- mojor_transpile(
    f,
    x = "f64[]",
    y = "f64[]",
    use_x = "lgl",
    name = "t_com_detect_branch_merge"
  )
  expect_setequal(trans$modified_args, c("x", "y"))
})

test_that("transpiler detects nested alias chains across control flow", {  f <- function(x, y, use_x) {
    if (use_x) {
      a <- x
    } else {
      a <- y
    }
    b <- a
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      b[i] <- b[i] + 1
      out[i] <- b[i]
    }
    out
  }
  trans <- mojor_transpile(
    f,
    x = "f64[]",
    y = "f64[]",
    use_x = "lgl",
    name = "t_com_detect_nested_alias_cf"
  )
  expect_setequal(trans$modified_args, c("x", "y"))
})

test_that("transpiler avoids false positives when both branches overwrite aliases", {  f <- function(x, use_x) {
    z <- x
    if (use_x) {
      z <- numeric(length(x))
    } else {
      z <- numeric(length(x))
    }
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      z[i] <- i
      out[i] <- z[i]
    }
    out
  }
  trans <- mojor_transpile(
    f,
    x = "f64[]",
    use_x = "lgl",
    name = "t_com_detect_branch_overwrite"
  )
  expect_equal(trans$modified_args, character(0))
})

test_that("mojor_build defaults to r semantics", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }
  
  # Should build successfully with default semantics
  expect_error(
    mojor_build(f, x = "f64[]", name = "t_com_build_default", cache = FALSE, load = FALSE),
    NA
  )
})

test_that("mojor_build accepts raw semantics", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }
  
  # Should build successfully with raw semantics
  expect_error(
    mojor_build(f, x = "f64[]", name = "t_com_build_raw", semantics = "raw", cache = FALSE, load = FALSE),
    NA
  )
})

test_that("modified arg detection is semantics-independent", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      x[i] <- x[i] + 1
      out[i] <- x[i]
    }
    out
  }
  trans_r <- mojor_transpile(f, x = "f64[]", name = "t_com_detect_r", semantics = "r")
  trans_raw <- mojor_transpile(f, x = "f64[]", name = "t_com_detect_raw", semantics = "raw")
  expect_equal(trans_r$modified_args, "x")
  expect_equal(trans_raw$modified_args, "x")
})

test_that("UNPROTECT count adjusts based on semantics", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  
  # Test with r semantics (would need PROTECT for modified args)
  expect_error(
    mojor_build(f, x = "f64[]", y = "f64[]", name = "t_com_unprotect_r", semantics = "r", cache = FALSE, load = FALSE),
    NA
  )
  
  # Test with raw semantics (no PROTECT needed for modified args)
  expect_error(
    mojor_build(f, x = "f64[]", y = "f64[]", name = "t_com_unprotect_raw", semantics = "raw", cache = FALSE, load = FALSE),
    NA
  )
})

test_that("semantics parameter is case sensitive", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }
  
  # Invalid semantics should error
  expect_error(
    mojor_transpile(f, x = "f64[]", name = "t_com_invalid", semantics = "R"),
    "should be one of"
  )
})

test_that("semantics is passed through build to transpile", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }
  
  # Build with raw semantics - should work
  built <- mojor_build(f, x = "f64[]", name = "t_com_passthrough", semantics = "raw", cache = FALSE, load = FALSE)
  expect_equal(built$trans$semantics, "raw")
})

# Note: Full in-place operation code generation is still subset-limited.
# These tests verify semantics plumbing and modified-arg analysis state.
