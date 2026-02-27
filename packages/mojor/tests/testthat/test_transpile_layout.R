library(testthat)

test_that("array_layout = row_major updates layout constants and linearization", {  f <- function(x, y, n) {
    mat <- matrix(0, n, 2)
    for (i in 1:n) mat[i, ] <- c(x[i], y[i])
    mat
  }
  trans <- mojor_transpile(
    f,
    x = "f64[]",
    y = "f64[]",
    n = "i32",
    array_layout = "row_major",
    ir_only = TRUE
  )
  expect_true(grepl("Layout.row_major", trans$mojo, fixed = TRUE))
  expect_true(
    grepl("* ncol_out_i", trans$mojo, fixed = TRUE) ||
      (grepl("RuntimeLayout[_MOJOR_MATRIX_LAYOUT].row_major", trans$mojo, fixed = TRUE) &&
         grepl("nrow_out_i", trans$mojo, fixed = TRUE) &&
         grepl("ncol_out_i", trans$mojo, fixed = TRUE))
  )
})
