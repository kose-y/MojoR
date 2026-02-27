library(testthat)

test_that("broadcast recycle works on CPU with warning", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  built <- mojor_build(
    f,
    x = "f64[]",
    y = "f64[]",
    name = "t_recycle_cpu",
    broadcast = "recycle_warn",
    cache = FALSE,
    load = TRUE
  )
  x <- as.double(1:4)
  y <- as.double(c(10, 20))
  expect_warning(res <- built$func(x, y), "recycling length")
  expect_equal(res, x + rep(y, length.out = length(x)))
})
