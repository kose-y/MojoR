library(testthat)

test_that("array output uses declared dim, not input dim", {  f <- function(x) {
    out <- array(0, dim = c(2, 2, 2))
    for (i in seq_along(x)) {
      out[1:2, 1, 2] <- x
    }
    out
  }
  built <- mojor_build(
    f,
    x = "f64[]",
    name = "t_array_out_dim",
    cache = FALSE,
    load = TRUE
  )
  x <- array(as.double(1:12), dim = c(3, 4))
  res <- built$func(x)
  expect_equal(dim(res), c(2L, 2L, 2L))
})
