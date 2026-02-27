library(testthat)

# ---- Combined Feature Tests ----

test_that("unroll and bounds_check work together", {  skip_if_no_mojo()
  
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * x[i]
    }
    out
  }
  
  built <- mojor_build(f, x = "f64[]", name = "t_combo", unroll = 4, bounds_check = FALSE, cache = FALSE, load = TRUE)
  
  x <- as.double(1:20)
  expect_equal(built$func(x), x^2)
})

test_that("all three parameters pass through mojor_build to mojor_transpile", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i]
    }
    out
  }
  
  built <- mojor_build(f, x = "f64[]", name = "t_params", 
                       unroll = 2, reduction = "auto", bounds_check = FALSE,
                       cache = FALSE, load = FALSE)
  
  expect_equal(built$trans$unroll, 2)
  expect_equal(built$trans$reduction, "auto")
  expect_false(built$trans$bounds_check)
})
