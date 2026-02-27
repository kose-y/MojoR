library(testthat)

test_that("logical rhs compression for slice writes", {  f <- function(x, mask, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[1:n] <- x[mask]
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", mask = "lgl[]", n = "i32", name = "t_rhs_mask")
  expect_true(grepl("__mojor_rhs_scan", trans$mojo, fixed = TRUE))
  expect_false(grepl("compressed RHS recycled", trans$mojo, fixed = TRUE))
})

test_that("slice write clamps end to output length", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[1:n] <- x[1:n]
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_slice_clamp")
  expect_mojor_any_match(
    trans$mojo,
    c("__mojor_end", "for __mojor_i1 in range\\(0, Int\\(n\\)\\)"),
    fixed = FALSE
  )
})
