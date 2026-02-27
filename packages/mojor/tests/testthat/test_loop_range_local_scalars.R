library(testthat)

test_that("seq_len accepts local i32 scalar bounds", {
  f_alias <- function(x, n_arg) {
    n <- n_arg
    out <- numeric(length(x))
    for (i in seq_len(n)) {
      out[i] <- x[i]
    }
    out
  }

  f_literal <- function(x) {
    n <- 4L
    out <- numeric(length(x))
    for (i in seq_len(n)) {
      out[i] <- x[i]
    }
    out
  }

  expect_error(mojor_transpile(f_alias, x = "f64[]", n_arg = "i32", ir_only = TRUE), NA)
  expect_error(mojor_transpile(f_literal, x = "f64[]", ir_only = TRUE), NA)
})

test_that("parallel loop helpers accept local i32 scalar bounds", {
  f_parallel_len <- function(x, n_arg) {
    n <- n_arg
    out <- numeric(n)
    for (i in seq_parallel_len(n)) {
      out[i] <- x[i] * 2
    }
    out
  }

  f_prange <- function(x, n_arg) {
    n <- n_arg
    out <- numeric(n)
    for (i in mojor_prange(n)) {
      out[i] <- x[i] * 3
    }
    out
  }

  trans_parallel_len <- mojor_transpile(f_parallel_len, x = "f64[]", n_arg = "i32")
  trans_prange <- mojor_transpile(f_prange, x = "f64[]", n_arg = "i32")

  expect_true(grepl("parallelize", trans_parallel_len$mojo, fixed = TRUE))
  expect_true(grepl("parallelize", trans_prange$mojo, fixed = TRUE))
})

test_that("seq and seq.int-style bounds accept local i32 scalars", {
  f_seq_single <- function(x, n_arg) {
    n <- n_arg
    out <- numeric(n)
    for (i in seq(n)) {
      out[i] <- x[i]
    }
    out
  }

  f_seq_end <- function(x, n_arg) {
    n <- n_arg
    out <- numeric(n)
    for (i in seq(1L, n)) {
      out[i] <- x[i]
    }
    out
  }

  expect_error(mojor_transpile(f_seq_single, x = "f64[]", n_arg = "i32", ir_only = TRUE), NA)
  expect_error(mojor_transpile(f_seq_end, x = "f64[]", n_arg = "i32", ir_only = TRUE), NA)
})

test_that("scalar arithmetic locals are accepted as loop bounds", {
  f_seq_end_expr <- function(x, n_arg) {
    n <- n_arg
    m <- n - 1L
    out <- numeric(m)
    for (i in seq(1L, m)) {
      out[i] <- x[i]
    }
    out
  }

  f_seq_len_expr <- function(x, n_arg) {
    n <- n_arg + 1L
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[i]
    }
    out
  }

  f_colon_max <- function(x, n_arg) {
    n <- n_arg
    out <- numeric(max(1L, n))
    for (i in 1:max(1L, n)) {
      out[i] <- x[i]
    }
    out
  }

  expect_error(mojor_transpile(f_seq_end_expr, x = "f64[]", n_arg = "i32", ir_only = TRUE), NA)
  expect_error(mojor_transpile(f_seq_len_expr, x = "f64[]", n_arg = "i32", ir_only = TRUE), NA)
  expect_error(mojor_transpile(f_colon_max, x = "f64[]", n_arg = "i32", ir_only = TRUE), NA)
})

test_that("local non-i32 scalar bounds stay rejected", {
  f_bad <- function(x, n_arg) {
    n <- as.double(n_arg)
    out <- numeric(length(x))
    for (i in seq_len(n)) {
      out[i] <- x[i]
    }
    out
  }

  expect_error(
    mojor_transpile(f_bad, x = "f64[]", n_arg = "i32", ir_only = TRUE),
    "seq_len\\(\\) arg must be a function argument|seq_len\\(\\) scalar must be i32"
  )
})

test_that("length(local array alias) works in loop ranges", {
  f_seq_len <- function(x) {
    y <- x
    out <- numeric(length(x))
    for (i in seq_len(length(y))) {
      out[i] <- y[i]
    }
    out
  }

  f_colon <- function(x) {
    y <- x
    out <- numeric(length(x))
    for (i in 1:length(y)) {
      out[i] <- y[i]
    }
    out
  }

  expect_error(mojor_transpile(f_seq_len, x = "f64[]", ir_only = TRUE), NA)
  expect_error(mojor_transpile(f_colon, x = "f64[]", ir_only = TRUE), NA)
})

test_that("parallel length(local array alias) variants are accepted", {
  f_parallel_len <- function(x) {
    y <- x
    out <- numeric(length(x))
    for (i in seq_parallel_len(length(y))) {
      out[i] <- y[i] * 2
    }
    out
  }

  f_prange <- function(x) {
    y <- x
    out <- numeric(length(x))
    for (i in mojor_prange(length(y))) {
      out[i] <- y[i] * 3
    }
    out
  }

  trans_parallel_len <- mojor_transpile(f_parallel_len, x = "f64[]")
  trans_prange <- mojor_transpile(f_prange, x = "f64[]")

  expect_true(grepl("parallelize", trans_parallel_len$mojo, fixed = TRUE))
  expect_true(grepl("parallelize", trans_prange$mojo, fixed = TRUE))
})

test_that("parallel helpers accept local i32 scalar expressions", {
  f_parallel_len_expr <- function(x, n_arg) {
    n <- n_arg
    out_n <- max(1L, n)
    out <- numeric(out_n)
    for (i in seq_parallel_len(max(1L, n))) {
      out[i] <- x[i] * 2
    }
    out
  }

  f_prange_expr <- function(x, n_arg) {
    n <- n_arg
    out_n <- as.integer(n + 1L)
    out <- numeric(out_n)
    for (i in mojor_prange(as.integer(n + 1L))) {
      out[i] <- x[i] * 3
    }
    out
  }

  trans_parallel_len <- mojor_transpile(f_parallel_len_expr, x = "f64[]", n_arg = "i32")
  trans_prange <- mojor_transpile(f_prange_expr, x = "f64[]", n_arg = "i32")

  expect_true(grepl("parallelize", trans_parallel_len$mojo, fixed = TRUE))
  expect_true(grepl("parallelize", trans_prange$mojo, fixed = TRUE))
})

test_that("loop ranges accept abs() i32 scalar expressions", {
  f_abs <- function(x, n_arg) {
    out <- numeric(abs(n_arg))
    for (i in seq_len(abs(n_arg))) {
      out[i] <- x[i]
    }
    out
  }

  expect_error(mojor_transpile(f_abs, x = "f64[]", n_arg = "i32", ir_only = TRUE), NA)
})

test_that("parallel helpers keep non-i32 scalar args rejected", {
  f_bad_prange <- function(x, n_bad) {
    out <- numeric(length(x))
    for (i in mojor_prange(n_bad)) {
      out[i] <- x[i]
    }
    out
  }

  expect_error(
    mojor_transpile(f_bad_prange, x = "f64[]", n_bad = "f64", ir_only = TRUE),
    "mojor_prange\\(\\) must take an i32 scalar expression|mojor_prange\\(\\) scalar must be i32"
  )
})

test_that("local alias length captured into scalar init works", {
  f <- function(x) {
    y <- x
    n <- length(y)
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- y[i]
    }
    out
  }

  expect_error(mojor_transpile(f, x = "f64[]", ir_only = TRUE), NA)
})

test_that("matrix alias bounds are accepted for nrow/dim forms", {
  f_nrow <- function(m) {
    mm <- m
    s <- 0.0
    for (i in 1:nrow(mm)) {
      s <- s + mm[i, 1L]
    }
    s
  }

  f_dim <- function(m) {
    mm <- m
    s <- 0.0
    for (i in 1:dim(mm)[1]) {
      s <- s + mm[i, 1L]
    }
    s
  }

  expect_error(mojor_transpile(f_nrow, m = "f64[,]", ir_only = TRUE), NA)
  expect_error(mojor_transpile(f_dim, m = "f64[,]", ir_only = TRUE), NA)
})

test_that("loop ranges accept min/max reductions over nrow/ncol expressions", {
  f_max <- function(m) {
    acc <- 0.0
    for (i in seq_len(max(nrow(m), ncol(m)))) {
      acc <- acc + as.double(i)
    }
    acc
  }

  f_min <- function(m) {
    acc <- 0.0
    for (i in seq_len(min(nrow(m), ncol(m)))) {
      acc <- acc + m[i, 1L]
    }
    acc
  }

  skip_if_no_mojo()
  built_max <- mojor_build(f_max, m = "f64[,]", name = "loop_dim_max_reduce")
  built_min <- mojor_build(f_min, m = "f64[,]", name = "loop_dim_min_reduce")
  m <- matrix(as.numeric(1:12), nrow = 3, ncol = 4)

  expect_equal(built_max$func(m), f_max(m))
  expect_equal(built_min$func(m), f_min(m))
})

test_that("loop ranges accept max(dim(x)) scalar reducers for matrix and ND arrays", {
  f_mat <- function(m) {
    n <- max(dim(m))
    acc <- 0.0
    for (i in 1:n) {
      acc <- acc + as.double(i)
    }
    acc
  }

  f_arr <- function(a) {
    n <- max(dim(a))
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- a[1L, 1L, 1L] + as.double(i)
    }
    out
  }

  expect_error(mojor_transpile(f_mat, m = "f64[,]", ir_only = TRUE), NA)
  expect_error(mojor_transpile(f_arr, a = "f64[3d]", ir_only = TRUE), NA)

  skip_if_no_mojo()
  built_mat <- mojor_build(
    f_mat, m = "f64[,]", name = "loop_dim_max_of_dim_matrix",
    cache = FALSE, load = TRUE
  )
  built_arr <- mojor_build(
    f_arr, a = "f64[3d]", name = "loop_dim_max_of_dim_array3d",
    cache = FALSE, load = TRUE
  )

  m <- matrix(as.numeric(1:12), nrow = 3, ncol = 4)
  a <- array(as.numeric(1:24), dim = c(2, 3, 4))

  expect_equal(built_mat$func(m), f_mat(m))
  expect_equal(built_arr$func(a), f_arr(a))
})

test_that("matrix type spec whitespace variants normalize identically", {
  f <- function(m) {
    mm <- m
    s <- 0.0
    for (i in 1:nrow(mm)) {
      s <- s + mm[i, 1L]
    }
    s
  }

  trans_compact <- mojor_transpile(f, m = "f64[,]", ir_only = TRUE)
  trans_spaced <- mojor_transpile(f, m = "f64[, ]", ir_only = TRUE)

  expect_identical(trans_compact$mojo, trans_spaced$mojo)
  expect_identical(trans_compact$types$m, "f64[,]")
  expect_identical(trans_spaced$types$m, "f64[,]")
})

test_that("loop ranges support c() literals beyond 8 entries", {
  f <- function(z) {
    acc <- as.integer(z)
    for (v in c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L)) {
      acc <- acc + v
    }
    acc
  }

  expect_error(mojor_transpile(f, z = "i32", ir_only = TRUE), NA)

  skip_if_no_mojo()
  built <- mojor_build(f, z = "i32", cache = FALSE, load = TRUE)
  expect_equal(built$func(5L), f(5L))
})

test_that("':' loop ranges accept starts below 1", {
  f <- function(z) {
    acc <- as.integer(z)
    for (i in -2:2) {
      acc <- acc + i
    }
    acc
  }

  expect_error(mojor_transpile(f, z = "i32", ir_only = TRUE), NA)

  skip_if_no_mojo()
  built <- mojor_build(f, z = "i32", cache = FALSE, load = TRUE)
  expect_equal(built$func(5L), f(5L))
})
