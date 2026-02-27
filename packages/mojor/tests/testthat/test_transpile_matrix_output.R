library(testthat)

test_that("matrix output assignment linearizes with out nrow", {  f <- function(x, n) {
    out <- matrix(0, nrow = n, ncol = n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        out[i, j] <- x[i, j]
      }
    }
    out
  }

  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_out_mat")
  expect_true(isTRUE(trans$out_matrix))
  expect_true(any(grepl("__mojor_out_nrow: Int32", trans$signature, fixed = TRUE)))
  expect_true(grepl("nrow_out_i", trans$mojo, fixed = TRUE))
  expect_true(grepl("LayoutTensor", trans$mojo, fixed = TRUE))
  expect_mojor_any_match(
    trans$mojo,
    c(
      "(__mojor_tensor_out|out_tensor)\\[\\(i - 1\\), \\(j - 1\\)\\]",
      "(__mojor_tensor_out|out_tensor)\\[i, j\\]",
      "(__mojor_tensor_out|out_tensor)\\[Int\\(\\(i - 1\\)\\), Int\\(\\(j - 1\\)\\)\\]"
    )
  )
})

test_that("matrix output allows index expressions", {  f <- function(x, nr, nc) {
    out <- matrix(0, nr, nc)
    for (i in 1:(nr - 1)) {
      for (j in 1:(nc - 1)) {
        out[i + 1, j + 1] <- x[i, j]
      }
    }
    out
  }

  trans <- mojor_transpile(f, x = "f64[]", nr = "i32", nc = "i32", name = "t_out_mat_expr")
  expect_true(isTRUE(trans$out_matrix))
  expect_true(grepl("Int\\(\\(\\(i \\+ 1\\) - 1\\)\\)", trans$mojo))
  expect_true(grepl("Int\\(\\(\\(j \\+ 1\\) - 1\\)\\)", trans$mojo))
})

test_that("IR-only supports matrix output indexing assignments", {  f <- function(x, n) {
    out <- matrix(0, nrow = n, ncol = n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        out[i, j] <- x[i, j]
      }
    }
    out
  }

  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_out_mat_ir", ir_only = TRUE)
  expect_true(isTRUE(trans$out_matrix))
  expect_mojor_any_match(trans$mojo, "__mojor_tensor_out|out_tensor")
})
