library(testthat)

# Tests for mojor_bincount (C_BinCount equivalent)

test_that("mojor_bincount basic functionality works", {  skip_if_no_mojo()
  
  x <- c(1, 2, 3, 5, 6, 7, 10, 11, 12)
  breaks <- c(0, 5, 10, 15)
  
  result <- mojor_bincount(x, breaks, right = FALSE, include.lowest = TRUE)
  
  expect_equal(length(result), 3)
  expect_equal(result, c(3L, 3L, 3L))
})

test_that("mojor_bincount matches R hist with same parameters", {  skip_if_no_mojo()
  
  x <- runif(100)
  breaks <- seq(0, 1, by = 0.1)
  
  result_mojo <- mojor_bincount(x, breaks, right = FALSE, include.lowest = TRUE)
  result_r <- hist(x, breaks = breaks, right = FALSE, plot = FALSE)$counts
  
  expect_equal(result_mojo, as.integer(result_r))
})

test_that("mojor_bincount with right=TRUE uses (a,b] intervals", {  skip_if_no_mojo()
  
  x <- c(0, 5, 10, 15)
  breaks <- c(0, 5, 10, 15)
  
  result <- mojor_bincount(x, breaks, right = TRUE, include.lowest = TRUE)
  expected <- hist(x, breaks = breaks, right = TRUE, plot = FALSE)$counts
  
  # Should match R's hist behavior
  expect_equal(result, as.integer(expected))
})

test_that("mojor_bincount handles empty input", {  skip_if_no_mojo()
  
  x <- numeric(0)
  breaks <- c(0, 5, 10)
  
  result <- mojor_bincount(x, breaks)
  
  expect_equal(result, c(0L, 0L))
})

test_that("mojor_bincount handles values outside breaks range", {  skip_if_no_mojo()
  
  x <- c(-5, 25, 3, 7, 12)  # -5 and 25 outside [0, 15]
  breaks <- c(0, 5, 10, 15)
  
  result <- mojor_bincount(x, breaks, right = FALSE, include.lowest = TRUE)
  
  # Only 3, 7, 12 should be counted
  expect_equal(sum(result), 3)
})

test_that("mojor_bincount performance is comparable to R findInterval", {  skip_if_no_mojo()
  
  x <- runif(10000)
  breaks <- seq(0, 1, by = 0.01)
  
  # Time MojoR
  start <- Sys.time()
  result_mojo <- mojor_bincount(x, breaks, right = FALSE, include.lowest = TRUE)
  mojo_time <- as.numeric(Sys.time() - start, units = "secs")
  
  # Time R
  start <- Sys.time()
  result_r <- tabulate(findInterval(x, breaks), length(breaks) - 1)
  r_time <- as.numeric(Sys.time() - start, units = "secs")
  
  # MojoR should not be more than 5x slower (some overhead expected)
  expect_true(mojo_time < r_time * 5)
  
  # Results should match
  expect_equal(result_mojo, as.integer(result_r))
})
