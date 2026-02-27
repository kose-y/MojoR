library(testthat)

test_that("matrix indexing uses nrow param and linearizes index", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        out[i] <- x[i, j]
      }
    }
    out
  }

  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_matrix_index")
  expect_true(any(grepl("__mojor_nrow_x: Int32", trans$signature, fixed = TRUE)))
  expect_true(grepl("nrow_x_i", trans$mojo, fixed = TRUE))
  expect_true(grepl("LayoutTensor", trans$mojo, fixed = TRUE))
  expect_mojor_any_match(
    trans$mojo,
    c(
      "__mojor_tensor_x\\[\\(i - 1\\), \\(j - 1\\)\\]",
      "__mojor_tensor_x\\[i, j\\]",
      "__mojor_tensor_x\\[Int\\(\\(i - 1\\)\\), Int\\(\\(j - 1\\)\\)\\]"
    )
  )
})
