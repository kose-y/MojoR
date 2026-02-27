library(testthat)


test_that("integer vector output inferred", {  add_one_int <- function(x) {
    out <- integer(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + 1L
    }
    return(out)
  }

  res <- mojor_transpile(add_one_int, x = "i32[]", name = "mojor_add_one_i32")
  expect_equal(res$out_kind, "vector")
  expect_equal(res$out_type, "i32[]")
})
