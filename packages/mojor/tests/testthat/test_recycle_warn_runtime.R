library(testthat)

test_that("runtime warning for compressed RHS recycle (slice)", {  old <- mojor_options()$warn_recycle
  on.exit(mojor_options(warn_recycle = old), add = TRUE)
  mojor_options(warn_recycle = TRUE)
  f <- function(x, mask, nlen) {
    out <- numeric(nlen)
    for (i in seq_len(nlen)) {
      out[1:nlen] <- x[mask]
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", mask = "lgl[]", nlen = "i32", name = "t_warn_slice", cache = FALSE, load = TRUE)
  x <- as.double(1:5)
  mask <- c(TRUE, FALSE, TRUE, FALSE, FALSE)
  expect_warning(built$func(x, mask, as.integer(5)), "compressed RHS recycled")
})

test_that("runtime warning for compressed RHS recycle (mask-mismatch)", {  old <- mojor_options()$warn_recycle
  on.exit(mojor_options(warn_recycle = old), add = TRUE)
  mojor_options(warn_recycle = TRUE)
  f <- function(x, mask1, mask2) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[mask1] <- x[mask2]
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", mask1 = "lgl[]", mask2 = "lgl[]", name = "t_warn_mask", cache = FALSE, load = TRUE)
  x <- as.double(1:6)
  mask1 <- c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)
  mask2 <- c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE)
  expect_warning(built$func(x, mask1, mask2), "compressed RHS recycled")
})
