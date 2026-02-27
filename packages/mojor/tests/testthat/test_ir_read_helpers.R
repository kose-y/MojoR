library(testthat)

# Test IR-driven array read helpers

test_that("IR uses _mojor_read_f64 for f64 arrays", {  f <- function(x) {
    out <- 0.0
    for (i in 1:length(x)) {
      out <- out + x[i]
    }
    out
  }
  
  trans <- mojor_transpile(f, x = "f64[]", emit_ir = TRUE)
  expect_true(grepl("_mojor_read_f64", trans$mojo, fixed = TRUE))
  expect_false(grepl("_mojor_read_lgl", trans$mojo, fixed = TRUE))
})

test_that("IR uses _mojor_read_i32 for i32 arrays", {  f <- function(x) {
    out <- 0L
    for (i in 1:length(x)) {
      out <- out + x[i]
    }
    out
  }
  
  trans <- mojor_transpile(f, x = "i32[]", emit_ir = TRUE)
  expect_true(grepl("_mojor_read_i32", trans$mojo, fixed = TRUE))
  expect_false(grepl("_mojor_read_f64", trans$mojo, fixed = TRUE))
})

test_that("IR uses _mojor_read_f32 for f32 arrays", {  f <- function(x) {
    out <- 0.0
    for (i in 1:length(x)) {
      out <- out + x[i]
    }
    out
  }
  
  trans <- mojor_transpile(f, x = "f32[]", emit_ir = TRUE)
  expect_true(grepl("_mojor_read_f32", trans$mojo, fixed = TRUE))
  expect_false(grepl("_mojor_read_f64", trans$mojo, fixed = TRUE))
})

test_that("IR uses _mojor_read_lgl for logical arrays", {  f <- function(mask, x) {
    out <- 0.0
    for (i in 1:length(mask)) {
      if (mask[i]) {
        out <- out + x[i]
      }
    }
    out
  }
  
  trans <- mojor_transpile(f, mask = "lgl[]", x = "f64[]", emit_ir = TRUE)
  expect_true(grepl("_mojor_read_lgl", trans$mojo, fixed = TRUE))
  expect_true(grepl("_mojor_read_f64", trans$mojo, fixed = TRUE))
})

test_that("IR does not use read helpers for 2D indexing", {  f <- function(x, n) {
    out <- matrix(0.0, n, n)
    for (i in 1:n) {
      for (j in 1:n) {
        out[i, j] <- x[i, j]
      }
    }
    out
  }
  
  trans <- mojor_transpile(f, x = "f64[]", n = "i32", emit_ir = TRUE)
  # Should use LayoutTensor, not read helpers
  expect_false(grepl("_mojor_read_f64.*,.*,", trans$mojo))
})
