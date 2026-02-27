library(testthat)

test_that("n-dim indexing uses dim pointer", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        for (k in seq_len(n)) {
          out[i] <- x[i, j, k]
        }
      }
    }
    out
  }

  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_nd_index")
  expect_true(any(grepl("__mojor_dim_x_ptr: ImmutOpaqueAny", trans$signature, fixed = TRUE)))
  expect_true(any(grepl("__mojor_ndim_x: Int32", trans$signature, fixed = TRUE)))
  expect_true(grepl("dim_x_ptr\\[0\\]", trans$mojo))
  expect_true(grepl("dim_x_ptr\\[1\\]", trans$mojo))
})

test_that("n-dim indexing uses LayoutTensor for 3D arrays", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        for (k in seq_len(n)) {
          out[i] <- x[i, j, k]
        }
      }
    }
    out
  }

  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_nd_layouttensor")
  expect_true(grepl("LayoutTensor", trans$mojo, fixed = TRUE))
  expect_true(grepl("_MOJOR_LAYOUT_x = Layout.col_major\\(IndexList\\[3\\]\\(0, 0, 0\\)\\)", trans$mojo))
  # Accept multiple index formats: (i - 1), i, or Int((i - 1))
  expect_true(grepl("__mojor_tensor_x\\[\\(i - 1\\), \\(j - 1\\), \\(k - 1\\)\\]", trans$mojo) ||
                grepl("__mojor_tensor_x\\[i, j, k\\]", trans$mojo) ||
                grepl("__mojor_tensor_x\\[Int\\(\\(i - 1\\)\\), Int\\(\\(j - 1\\)\\), Int\\(\\(k - 1\\)\\)\\]", trans$mojo))
})

