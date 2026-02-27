# PR-B2: Index Arithmetic Expansion Tests
# Tests for extended index expressions beyond i +/- literal

test_that("index with scalar variable offset transpiles", {  f <- function(x, offset) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i + offset]
    }
    out
  }
  
  trans <- mojor_transpile(f, x = "f64[]", offset = "i32", name = "t_idx_scalar_offset")
  
  # Check that transpile succeeded
  expect_true(length(trans$mojo) > 0)
  
  # Check that offset appears in generated code (as a parameter)
  mojo_code <- paste(trans$mojo, collapse = "\n")
  expect_true(grepl("offset", mojo_code))
})

test_that("index with multiplication by scalar transpiles", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[i * 2]
    }
    out
  }
  
  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_idx_mul")
  
  # Check that transpile succeeded
  expect_true(length(trans$mojo) > 0)
  
  # Check that multiplication appears in generated code
  mojo_code <- paste(trans$mojo, collapse = "\n")
  expect_true(grepl("\\* 2|\\*2", mojo_code))
})

test_that("index with division by scalar transpiles", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i / 2]
    }
    out
  }
  
  trans <- mojor_transpile(f, x = "f64[]", name = "t_idx_div")
  
  # Check that transpile succeeded
  expect_true(length(trans$mojo) > 0)
  
  # Check that division appears in generated code
  mojo_code <- paste(trans$mojo, collapse = "\n")
  expect_true(grepl("/ 2|/2", mojo_code))
})

test_that("index with scalar + loop var order works", {  f <- function(x, offset) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[offset + i]
    }
    out
  }
  
  trans <- mojor_transpile(f, x = "f64[]", offset = "i32", name = "t_idx_offset_plus_i")
  expect_true(length(trans$mojo) > 0)
})

test_that("index with scalar - loop var works", {  f <- function(x, offset) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[offset - i]
    }
    out
  }
  
  trans <- mojor_transpile(f, x = "f64[]", offset = "i32", name = "t_idx_offset_minus_i")
  expect_true(length(trans$mojo) > 0)
})

test_that("ir_only mode accepts new index patterns", {  old <- mojor_options()$ir_only
  on.exit(mojor_options(ir_only = old), add = TRUE)
  mojor_options(ir_only = TRUE)
  
  f <- function(x, offset) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i + offset]
    }
    out
  }
  
  # Should not fall back with ir_only = TRUE
  trans <- mojor_transpile(f, x = "f64[]", offset = "i32", name = "t_idx_ir_only")
  expect_true(length(trans$mojo) > 0)
})
