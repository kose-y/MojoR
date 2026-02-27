library(testthat)

test_that("Rgibbs-style nested RNG kernel transpiles with RNG + row-slice assignment", {  rgibbs <- function(N, thin) {
    mat <- matrix(0, nrow = N, ncol = 2)
    x <- y <- 0
    for (i in 1:N) {
      for (j in 1:thin) {
        x <- rgamma(1, 3, y * y + 4)
        y <- rnorm(1, 1 / (x + 1), 1 / sqrt(2 * (x + 1)))
      }
      mat[i, ] <- c(x, y)
    }
    mat
  }

  out <- mojor_transpile(
    rgibbs,
    N = "i32",
    thin = "i32",
    name = "t_rgibbs_transpile",
    emit_ir = TRUE
  )

  expect_type(out, "list")
  expect_true(isTRUE(out$rng_needed))
  expect_match(out$mojo, "_random_standard_normal", fixed = TRUE)
  expect_match(out$mojo, "_random_standard_gamma", fixed = TRUE)
  expect_match(out$mojo, "for j in range", fixed = TRUE)
})
