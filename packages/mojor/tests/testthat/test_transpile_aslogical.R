library(testthat)

test_that("as.logical emits Int32 boolean cast", {  f <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) {
      out[i] <- as.logical(x[i])
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", name = "mojor_aslogical")
  expect_match(res$mojo, "Int32(", fixed = TRUE)
  expect_match(res$mojo, "!= 0", fixed = TRUE)
})
