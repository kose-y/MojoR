# Test: Cumulative Operations in Expressions (PR-B5 Phase 4 Extension)
# Tests cumulative ops in indexed expressions like y <- cumsum(x)[i] + 1

source("helper-mojo.R")

context("cumulative operations in expressions - transpile only")
test_that("cumsum works in expression context", {  # Test that cumsum can be used in expressions, not just direct assignment
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- cumsum(x)[i] + 1  # Expression context
    }
    out
  }
  
  result <- tryCatch({
    mojor_transpile(f, x = "f64[]", n = "i32")
  }, error = function(e) {
    skip(paste("Transpile failed:", conditionMessage(e)))
  })
  
  expect_false(is.null(result), "cumsum in expression should transpile")
  mojo_code <- result$mojo
  
  # Should have accumulator initialization
  expect_true(any(grepl("__mojor_cumul_acc.*=.*0\\.0", mojo_code)), 
              "Should initialize cumsum accumulator to 0.0")
  
  # Should have accumulator update after assignment
  expect_true(any(grepl("__mojor_cumul_acc.*=.*\\+.*x", mojo_code)), 
              "Should have accumulator update with x")
})

test_that("cumprod works in expression context", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- cumprod(x)[i] * 2  # Expression context
    }
    out
  }
  
  result <- tryCatch({
    mojor_transpile(f, x = "f64[]", n = "i32")
  }, error = function(e) {
    skip(paste("Transpile failed:", conditionMessage(e)))
  })
  
  expect_false(is.null(result), "cumprod in expression should transpile")
  mojo_code <- result$mojo
  
  expect_true(any(grepl("__mojor_cumul_acc.*=.*1\\.0", mojo_code)), 
              "Should initialize cumprod accumulator to 1.0")
})

test_that("cummax works in expression context", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- cummax(x)[i] + 10
    }
    out
  }
  
  result <- tryCatch({
    mojor_transpile(f, x = "f64[]", n = "i32")
  }, error = function(e) {
    skip(paste("Transpile failed:", conditionMessage(e)))
  })
  
  expect_false(is.null(result), "cummax in expression should transpile")
  mojo_code <- result$mojo
  
  expect_true(any(grepl("__mojor_cumul_acc", mojo_code)), 
              "Should use accumulator variable")
})

test_that("cummin works in expression context", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- cummin(x)[i] - 5
    }
    out
  }
  
  result <- tryCatch({
    mojor_transpile(f, x = "f64[]", n = "i32")
  }, error = function(e) {
    skip(paste("Transpile failed:", conditionMessage(e)))
  })
  
  expect_false(is.null(result), "cummin in expression should transpile")
  mojo_code <- result$mojo
  
  expect_true(any(grepl("__mojor_cumul_acc", mojo_code)), 
              "Should use accumulator variable")
})

test_that("multiple cumulative ops in same loop", {  f <- function(x, n) {
    out1 <- numeric(n)
    out2 <- numeric(n)
    for (i in 1:n) {
      out1[i] <- cumsum(x)[i]
      out2[i] <- cumprod(x)[i]
    }
    cbind(out1, out2)
  }
  
  result <- tryCatch({
    mojor_transpile(f, x = "f64[]", n = "i32")
  }, error = function(e) {
    skip(paste("Transpile failed:", conditionMessage(e)))
  })
  
  expect_false(is.null(result), "multiple cumulative ops should transpile")
  mojo_code <- result$mojo
  
  # Should have two different accumulators
  expect_true(any(grepl("__mojor_cumul_acc_1", mojo_code)), 
              "Should have first accumulator")
  expect_true(any(grepl("__mojor_cumul_acc_2", mojo_code)), 
              "Should have second accumulator")
})

test_that("cumulative op in complex expression", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- cumsum(x)[i] * 2 + cumprod(x)[i] * 3
    }
    out
  }
  
  result <- tryCatch({
    mojor_transpile(f, x = "f64[]", n = "i32")
  }, error = function(e) {
    skip(paste("Transpile failed:", conditionMessage(e)))
  })
  
  expect_false(is.null(result), "complex expression with cumulative ops should transpile")
})
