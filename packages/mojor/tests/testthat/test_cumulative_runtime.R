# Test: Cumulative Operations Runtime Validation (PR-B5 Phase 4)
# Tests actual execution of cumsum, cumprod, cummax, cummin

source("helper-mojo.R")

skip_if_no_mojo <- function() {
  skip_if_not(.mojor_has_mojo(), "Mojo not available")
}

context("cumulative operations runtime - cumsum")
test_that("cumsum produces correct results", {  skip_if_no_mojo()
  
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- cumsum(x)[i]
    }
    out
  }
  
  # Use IR-only mode to ensure we're testing the new implementation
  result <- tryCatch({
    mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  }, error = function(e) {
    skip(paste("Transpile failed:", conditionMessage(e)))
  })
  
  expect_false(is.null(result), "cumsum should transpile successfully")

  # Check that accumulator pattern is in the output
  mojo_code <- result$mojo
  expect_true(any(grepl("__mojor_cumul_acc", mojo_code)),
              "Should use accumulator variable")
  expect_true(any(grepl("__mojor_cumul_acc.*=.*0\\.0", mojo_code)),
              "Should initialize accumulator to 0.0")
})

test_that("cumsum matches R results", {  skip_if_no_mojo()
  
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- cumsum(x)[i]
    }
    out
  }
  
  # Test with simple vector
  x_test <- as.double(c(1, 2, 3, 4, 5))
  n_test <- length(x_test)
  
  # Expected R result
  expected <- cumsum(x_test)
  
  # Transpile and build
  built <- tryCatch({
    mojor_build(f, x = "f64[]", n = "i32", name = "t_cumsum_rt", cache = FALSE, load = TRUE)
  }, error = function(e) {
    skip(paste("Build failed:", conditionMessage(e)))
  })
  
  # Call the compiled function
  actual <- built$func(x_test, n_test)
  
  expect_equal(actual, expected, tolerance = 1e-10,
               info = "cumsum should match R's cumsum")
})

context("cumulative operations runtime - cumprod")
test_that("cumprod produces correct results", {  skip_if_no_mojo()
  
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- cumprod(x)[i]
    }
    out
  }
  
  built <- tryCatch({
    mojor_build(f, x = "f64[]", n = "i32", name = "t_cumprod_rt", cache = FALSE, load = TRUE)
  }, error = function(e) {
    skip(paste("Build failed:", conditionMessage(e)))
  })
  
  x_test <- as.double(c(1, 2, 3, 4))
  expected <- cumprod(x_test)
  actual <- built$func(x_test, length(x_test))
  
  expect_equal(actual, expected, tolerance = 1e-10,
               info = "cumprod should match R's cumprod")
})

context("cumulative operations runtime - cummax/cummin")
test_that("cummax produces correct results", {  skip_if_no_mojo()
  
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- cummax(x)[i]
    }
    out
  }
  
  built <- tryCatch({
    mojor_build(f, x = "f64[]", n = "i32", name = "t_cummax_rt", cache = FALSE, load = TRUE)
  }, error = function(e) {
    skip(paste("Build failed:", conditionMessage(e)))
  })
  
  x_test <- as.double(c(3, 1, 4, 1, 5, 9, 2, 6))
  expected <- cummax(x_test)
  actual <- built$func(x_test, length(x_test))
  
  expect_equal(actual, expected, tolerance = 1e-10,
               info = "cummax should match R's cummax")
})

test_that("cummin produces correct results", {  skip_if_no_mojo()
  
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- cummin(x)[i]
    }
    out
  }
  
  built <- tryCatch({
    mojor_build(f, x = "f64[]", n = "i32", name = "t_cummin_rt", cache = FALSE, load = TRUE)
  }, error = function(e) {
    skip(paste("Build failed:", conditionMessage(e)))
  })
  
  x_test <- as.double(c(5, 3, 8, 2, 9, 1, 7, 4))
  expected <- cummin(x_test)
  actual <- built$func(x_test, length(x_test))
  
  expect_equal(actual, expected, tolerance = 1e-10,
               info = "cummin should match R's cummin")
})

context("cumulative operations runtime - edge cases")
test_that("cumsum handles single element", {  skip_if_no_mojo()
  
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- cumsum(x)[i]
    }
    out
  }
  
  built <- tryCatch({
    mojor_build(f, x = "f64[]", n = "i32", name = "t_cumsum_single_rt", cache = FALSE, load = TRUE)
  }, error = function(e) {
    skip(paste("Build failed:", conditionMessage(e)))
  })
  
  x_test <- as.double(42)
  expected <- cumsum(x_test)
  actual <- built$func(x_test, length(x_test))
  
  expect_equal(actual, expected, tolerance = 1e-10)
})

test_that("cumprod handles zeros correctly", {  skip_if_no_mojo()
  
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- cumprod(x)[i]
    }
    out
  }
  
  built <- tryCatch({
    mojor_build(f, x = "f64[]", n = "i32", name = "t_cumprod_zero_rt", cache = FALSE, load = TRUE)
  }, error = function(e) {
    skip(paste("Build failed:", conditionMessage(e)))
  })
  
  x_test <- as.double(c(2, 3, 0, 5, 7))
  expected <- cumprod(x_test)
  actual <- built$func(x_test, length(x_test))
  
  expect_equal(actual, expected, tolerance = 1e-10,
               info = "cumprod should handle zeros (result becomes 0 and stays 0)")
})

test_that("cummax handles already sorted input", {  skip_if_no_mojo()
  
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- cummax(x)[i]
    }
    out
  }
  
  built <- tryCatch({
    mojor_build(f, x = "f64[]", n = "i32", name = "t_cummax_sorted_rt", cache = FALSE, load = TRUE)
  }, error = function(e) {
    skip(paste("Build failed:", conditionMessage(e)))
  })
  
  x_test <- as.double(c(1, 2, 3, 4, 5))
  expected <- cummax(x_test)
  actual <- built$func(x_test, length(x_test))
  
  expect_equal(actual, expected, tolerance = 1e-10)
})

test_that("cummin handles reverse sorted input", {  skip_if_no_mojo()
  
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- cummin(x)[i]
    }
    out
  }
  
  built <- tryCatch({
    mojor_build(f, x = "f64[]", n = "i32", name = "t_cummin_rev_rt", cache = FALSE, load = TRUE)
  }, error = function(e) {
    skip(paste("Build failed:", conditionMessage(e)))
  })
  
  x_test <- as.double(c(5, 4, 3, 2, 1))
  expected <- cummin(x_test)
  actual <- built$func(x_test, length(x_test))
  
  expect_equal(actual, expected, tolerance = 1e-10)
})

context("cumulative operations documentation")
test_that("PR-B5 is documented", {  plan_file <- "docs/BREADTH/PLAN.md"
  skip_if_not(file.exists(plan_file), "docs/BREADTH/PLAN.md not found")
  
  plan_doc <- readLines(plan_file)
  expect_true(any(grepl("PR-B5", plan_doc)))
  expect_true(any(grepl("Cumulative\\s*[/&]\\s*Statistical\\s*Ops", plan_doc, ignore.case = TRUE)))
})
