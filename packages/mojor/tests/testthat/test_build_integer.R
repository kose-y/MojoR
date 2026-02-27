library(testthat)


test_that("integer scalar reduction builds and runs", {  skip_if_no_mojo()

  sum_int <- function(x) {
    total <- 0L
    for (i in seq_along(x)) {
      total <- total + x[i]
    }
    return(total)
  }

  built <- mojor_build(sum_int, x = "i32[]", name = "mojor_sum_i32_test", verbose = FALSE)
  x <- as.integer(c(1, 2, 3, 4, 5))
  expect_equal(built$func(x), sum_int(x))
})
