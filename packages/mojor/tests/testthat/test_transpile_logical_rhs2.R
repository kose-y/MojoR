library(testthat)

test_that("logical rhs compression for mask-to-mask assignment", {  f <- function(x, mask1, mask2) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[mask1] <- x[mask2]
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", mask1 = "lgl[]", mask2 = "lgl[]", name = "t_rhs_mask2")
  expect_false(grepl("compressed RHS recycled", trans$mojo, fixed = TRUE))
  expect_mojor_any_match(trans$mojo, "_mojor_read_lgl\\(mask1")
  expect_mojor_any_match(trans$mojo, "_mojor_read_lgl\\(mask2")
})
