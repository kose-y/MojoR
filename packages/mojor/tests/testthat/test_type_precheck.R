library(testthat)


test_that("mojor_build prechecks types when arguments are in scope", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + 1
    }
    out
  }

  x <- 1:5  # integer, but we will claim f64[]
  expect_error(
    mojor_build(f, x = "f64[]", name = "t_precheck"),
    "must be double"
  )
})
