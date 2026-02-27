library(testthat)


test_that("integer division and modulo transpile", {  f <- function(x, y) {
    out <- integer(length(x))
    for (i in seq_along(x)) {
      out[i] <- (x[i] %/% y[i]) + (x[i] %% y[i])
    }
    out
  }

  res <- mojor_transpile(f, x = "i32[]", y = "i32[]", name = "mojor_mod")
  expect_equal(res$out_type, "i32[]")
  expect_match(res$mojo, "//", fixed = TRUE)
  expect_match(res$mojo, " % ", fixed = TRUE)
})
