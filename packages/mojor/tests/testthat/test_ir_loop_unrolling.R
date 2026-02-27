library(testthat)

# Test IR loop unrolling for reductions

test_that("Loop unrolling generates correct code", {  f <- function(x) {
    s <- 0.0
    for (i in 1:length(x)) {
      s <- s + x[i]
    }
    s
  }
  
  trans <- mojor_transpile(f, x = "f64[]", unroll = 4, emit_ir = TRUE)
  
  # Check for unrolling markers
  expect_true(grepl("_mojor_unroll_tail_start", trans$mojo, fixed = TRUE))
  expect_true(grepl("for _mojor_i in range", trans$mojo, fixed = TRUE))
  
  # Check that reduction comment is present
  expect_true(grepl("# IR reduction: sum", trans$mojo, fixed = TRUE))
})

test_that("Unrolled loop produces correct results", {  skip_if_no_mojo()
  
  f <- function(x) {
    s <- 0.0
    for (i in 1:length(x)) {
      s <- s + x[i]
    }
    s
  }
  
  # Build with unrolling
  built <- mojor_build(f, x = "f64[]", unroll = 4, cache = FALSE)
  
  # Test with various sizes (skip n=0 due to 1:0 edge case)
  test_sizes <- c(1, 3, 4, 5, 7, 8, 15, 16, 17, 100, 1000)
  
  for (n in test_sizes) {
    x <- runif(n)
    result <- built$func(x)
    expected <- sum(x)
    expect_equal(result, expected, tolerance = 1e-10,
                 info = paste("Failed for n =", n))
  }
})

test_that("Unrolling works with different unroll factors", {  skip_if_no_mojo()
  
  f <- function(x) {
    s <- 0.0
    for (i in 1:length(x)) {
      s <- s + x[i]
    }
    s
  }
  
  x <- runif(100)
  expected <- sum(x)
  
  for (unroll_factor in c(2, 4, 8)) {
    built <- mojor_build(f, x = "f64[]", unroll = unroll_factor, cache = FALSE)
    result <- built$func(x)
    expect_equal(result, expected, tolerance = 1e-10,
                 info = paste("Failed for unroll =", unroll_factor))
  }
})

test_that("Unrolling works with product reduction", {  skip_if_no_mojo()
  
  f <- function(x) {
    p <- 1.0
    for (i in 1:length(x)) {
      p <- p * x[i]
    }
    p
  }
  
  built <- mojor_build(f, x = "f64[]", unroll = 4, cache = FALSE)
  
  x <- runif(10, 0.5, 1.5)
  result <- built$func(x)
  expected <- prod(x)
  expect_equal(result, expected, tolerance = 1e-10)
})
