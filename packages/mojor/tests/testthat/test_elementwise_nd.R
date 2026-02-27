library(testthat)

test_that("n-d elementwise via explicit loops", {  f <- function(x, n) {
    out <- array(0, dim = c(n, n, n))
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        for (k in seq_len(n)) {
          out[i, j, k] <- x[i, j, k] + 1
        }
      }
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_ew_nd")
  expect_mojor_any_match(trans$mojo, c("for i in range", "for __mojor_i1 in range"), fixed = TRUE)
  expect_mojor_any_match(trans$mojo, c("out[", "__mojor_tensor_out["), fixed = TRUE)
  expect_mojor_any_match(trans$mojo, c("_mojor_read_f64(", "__mojor_tensor_x["), fixed = TRUE)
})
