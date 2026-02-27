library(testthat)

test_that("array output assignment linearizes with dim pointer", {  f <- function(x, n) {
    out <- array(0, dim = c(n, n, n))
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        for (k in seq_len(n)) {
          out[i, j, k] <- x[i, j, k]
        }
      }
    }
    out
  }

  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_out_array")
  expect_true(isTRUE(trans$out_array))
  expect_true(any(grepl("__mojor_out_dim_ptr: ImmutOpaqueAny", trans$signature, fixed = TRUE)))
  expect_true(any(grepl("__mojor_out_ndim: Int32", trans$signature, fixed = TRUE)))
  expect_true(grepl("dim_out_ptr\\[0\\]", trans$mojo))
  expect_true(grepl("dim_out_ptr\\[1\\]", trans$mojo))
})

test_that("array output supports dim name argument", {  f <- function(x, dim, n) {
    out <- array(0, dim = dim)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        for (k in seq_len(n)) {
          out[i, j, k] <- x[i, j, k]
        }
      }
    }
    out
  }

  trans <- mojor_transpile(f, x = "f64[]", dim = "i32[]", n = "i32", name = "t_out_array_dim")
  expect_true(isTRUE(trans$out_array))
  expect_true(any(grepl("__mojor_out_dim_ptr: ImmutOpaqueAny", trans$signature, fixed = TRUE)))
  expect_true(any(grepl("__mojor_out_ndim: Int32", trans$signature, fixed = TRUE)))
})

test_that("array output allows index expressions", {  f <- function(x, n) {
    out <- array(0, dim = c(n, n, n))
    for (i in 1:(n - 1)) {
      for (j in 1:(n - 1)) {
        for (k in 1:n) {
          out[i + 1, j + 1, k] <- x[i, j, k]
        }
      }
    }
    out
  }

  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_out_array_expr")
  expect_true(isTRUE(trans$out_array))
  expect_true(grepl("Int\\(\\(\\(i \\+ 1\\) - 1\\)\\)", trans$mojo))
  expect_true(grepl("Int\\(\\(\\(j \\+ 1\\) - 1\\)\\)", trans$mojo))
})

test_that("IR-only supports array output indexing assignments", {  f <- function(x, n) {
    out <- array(0, dim = c(n, n, n))
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        for (k in seq_len(n)) {
          out[i, j, k] <- x[i, j, k]
        }
      }
    }
    out
  }

  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_out_array_ir", ir_only = TRUE)
  expect_true(isTRUE(trans$out_array))
  expect_true(grepl("__mojor_tensor_out", trans$mojo))
})

test_that("index bounds helper emits guard when enabled", {  skip_if(isTRUE(getOption("mojor.ir_only", FALSE)), "IR bounds checking not yet implemented")
  
  old <- mojor_options()$index_bounds
  on.exit(mojor_options(index_bounds = old), add = TRUE)
  mojor_options(index_bounds = TRUE)
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i]
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "t_bounds")
  expect_mojor_any_match(
    trans$mojo,
    c("_mojor_oob", "_mojor_read_f64"),
    fixed = TRUE
  )
})

test_that("index bounds guard works with N-dim indexing", {  old <- mojor_options()$index_bounds
  on.exit(mojor_options(index_bounds = old), add = TRUE)
  mojor_options(index_bounds = TRUE)
  f <- function(x, n) {
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
  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_bounds_nd")
  expect_true(
    grepl("_mojor_oob", trans$mojo, fixed = TRUE) ||
      grepl("__mojor_na_flag[0] = Int32(2)", trans$mojo, fixed = TRUE)
  )
})

test_that("index bounds guard uses per-array length when available", {  old <- mojor_options()$index_bounds
  on.exit(mojor_options(index_bounds = old), add = TRUE)
  mojor_options(index_bounds = TRUE)
  f <- function(y, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- y[i]
    }
    out
  }
  trans <- mojor_transpile(f, y = "f64[]", n = "i32", name = "t_bounds_len")
  expect_true(grepl("n_i", trans$mojo, fixed = TRUE))
})
