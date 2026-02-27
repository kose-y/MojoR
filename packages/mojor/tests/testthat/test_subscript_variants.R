# PR-B2 Phase 2: Subscript Assignment Variants
# Test coverage for matrix/array assignment patterns

# -----------------------------------------------------------------------------
# Column Assignment (mat[, j] <- v)
# -----------------------------------------------------------------------------

test_that("matrix column assignment transpiles", {  f <- function(x, n) {
    mat <- matrix(0, 2, n)
    for (j in seq_len(n)) {
      mat[, j] <- c(x[j], x[j] * 2)
    }
    mat
  }
  
  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_mat_col_assign")
  expect_true(length(trans$mojo) > 0)
  
  # Should have slice assignment loop for first dimension
  expect_match(trans$mojo, "for __mojor_i1 in")
})

test_that("matrix column assignment with scalar value transpiles", {  f <- function(n) {
    mat <- matrix(0, 3, n)
    for (j in seq_len(n)) {
      mat[, j] <- 5.0  # scalar fill
    }
    mat
  }
  
  trans <- mojor_transpile(f, n = "i32", name = "t_mat_col_scalar")
  expect_true(length(trans$mojo) > 0)
})

# -----------------------------------------------------------------------------
# Array Slice Assignment
# -----------------------------------------------------------------------------

test_that("3D array slice assignment transpiles", {  f <- function(x, n) {
    arr <- array(0, dim = c(2, 2, n))
    for (k in seq_len(n)) {
      arr[, , k] <- c(x[k], x[k]*2, x[k]*3, x[k]*4)
    }
    arr
  }
  
  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_arr_3d_slice")
  expect_true(length(trans$mojo) > 0)
})

test_that("3D array partial slice assignment transpiles", {  f <- function(x, n) {
    arr <- array(0, dim = c(2, 3, n))
    for (i in seq_len(n)) {
      arr[i, , ] <- c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
    }
    arr
  }
  
  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_arr_3d_partial")
  expect_true(length(trans$mojo) > 0)
})

# -----------------------------------------------------------------------------
# Complex Index Assignment
# -----------------------------------------------------------------------------

test_that("matrix assignment with computed index transpiles", {  f <- function(x, offset, n) {
    mat <- matrix(0, n, 2)
    for (i in seq_len(n - offset)) {
      mat[i + offset, ] <- c(x[i], x[i] * 2)
    }
    mat
  }
  
  trans <- mojor_transpile(f, x = "f64[]", offset = "i32", n = "i32", 
                           name = "t_mat_offset_assign")
  expect_true(length(trans$mojo) > 0)
})

# -----------------------------------------------------------------------------
# ND Slice Runtime Parity
# -----------------------------------------------------------------------------

test_that("3D array first-dimension slice runtime parity", {  skip_if_no_mojo()
  f <- function(n) {
    arr <- array(0, dim = c(n, 2, 2))
    for (i in seq_len(n)) {
      arr[i, , ] <- c(1, 2, 3, 4)
    }
    arr
  }

  built <- mojor_build(f, n = "i32", ir_only = TRUE, cache = FALSE, load = TRUE, name = "t_arr_3d_firstdim_runtime")
  expect_equal(built$func(3L), f(3L))
})

test_that("3D array middle-dimension slice runtime parity", {  skip_if_no_mojo()
  f <- function(n) {
    arr <- array(0, dim = c(2, n, 2))
    for (j in seq_len(n)) {
      arr[, j, ] <- c(1, 2, 3, 4)
    }
    arr
  }

  built <- mojor_build(f, n = "i32", ir_only = TRUE, cache = FALSE, load = TRUE, name = "t_arr_3d_middim_runtime")
  expect_equal(built$func(3L), f(3L))
})

test_that("4D array trailing-dimension slice runtime parity", {  skip_if_no_mojo()
  f <- function(n) {
    arr <- array(0, dim = c(2, 2, 2, n))
    for (k in seq_len(n)) {
      arr[, , , k] <- c(1, 2, 3, 4, 5, 6, 7, 8)
    }
    arr
  }

  built <- mojor_build(f, n = "i32", ir_only = TRUE, cache = FALSE, load = TRUE, name = "t_arr_4d_trailing_runtime")
  expect_equal(built$func(2L), f(2L))
})

test_that("3D array slice constructor recycling runtime parity", {  skip_if_no_mojo()
  f <- function(n) {
    arr <- array(0, dim = c(4, 3, n))
    for (k in seq_len(n)) {
      arr[, , k] <- c(1, 2, 3, 4, 5, 6)
    }
    arr
  }

  built <- mojor_build(f, n = "i32", ir_only = TRUE, cache = FALSE, load = TRUE, name = "t_arr_3d_recycle_runtime")
  expect_equal(built$func(2L), f(2L))
})

# -----------------------------------------------------------------------------
# Subset-Limited Forms
# -----------------------------------------------------------------------------

test_that("submatrix assignment with integer vector indices is supported", {  skip_if_no_mojo()
  f <- function(m, rows, cols) {
    mat <- matrix(0, 5, 5)
    mat[rows, cols] <- m
    mat
  }

  built <- mojor_build(
    f,
    m = "f64[]",
    rows = "i32[]",
    cols = "i32[]",
    ir_only = TRUE,
    cache = FALSE,
    load = TRUE,
    name = "t_submatrix_assign_vec_idx"
  )

  m <- as.double(c(10, 20, 30, 40))
  rows <- c(1L, 3L)
  cols <- c(2L, 5L)
  expect_equal(built$func(m, rows, cols), f(m, rows, cols))
})

test_that("submatrix assignment with named index args is supported", {  skip_if_no_mojo()
  f <- function(m, rows, cols) {
    mat <- matrix(0, 5, 5)
    mat[rows = rows, cols = cols] <- m
    mat
  }

  built <- mojor_build(
    f,
    m = "f64[]",
    rows = "i32[]",
    cols = "i32[]",
    ir_only = TRUE,
    cache = FALSE,
    load = TRUE,
    name = "t_submatrix_assign_named_vec_idx"
  )

  m <- as.double(c(10, 20, 30, 40))
  rows <- c(1L, 3L)
  cols <- c(2L, 5L)
  expect_equal(built$func(m, rows, cols), f(m, rows, cols))
})

test_that("submatrix assignment with integer vector indices and scalar RHS is supported", {  skip_if_no_mojo()
  f <- function(rows, cols) {
    mat <- matrix(0, 5, 5)
    mat[rows, cols] <- 7.0
    mat
  }

  built <- mojor_build(
    f,
    rows = "i32[]",
    cols = "i32[]",
    ir_only = TRUE,
    cache = FALSE,
    load = TRUE,
    name = "t_submatrix_assign_scalar_vec_idx"
  )

  rows <- c(2L, 4L)
  cols <- c(1L, 5L)
  expect_equal(built$func(rows, cols), f(rows, cols))
})

test_that("submatrix assignment with computed integer vector index expressions is supported", {  skip_if_no_mojo()
  f <- function(m) {
    mat <- matrix(0, 5, 5)
    mat[rep_len(c(1L, 3L), 2), c(2L, 5L)] <- m
    mat
  }

  built <- mojor_build(
    f,
    m = "f64[]",
    ir_only = TRUE,
    cache = FALSE,
    load = TRUE,
    name = "t_submatrix_assign_expr_vec_idx"
  )

  m <- as.double(c(10, 20, 30, 40))
  expect_equal(built$func(m), f(m))
})

test_that("submatrix assignment supports i32[] arithmetic index expressions", {  skip_if_no_mojo()
  f <- function(m, rows, cols, off) {
    mat <- matrix(0, 6, 6)
    mat[rows + off, cols] <- m
    mat
  }

  built <- mojor_build(
    f,
    m = "f64[]",
    rows = "i32[]",
    cols = "i32[]",
    off = "i32",
    ir_only = TRUE,
    cache = FALSE,
    load = TRUE,
    name = "t_submatrix_assign_expr_vec_idx_plus"
  )

  m <- as.double(c(10, 20, 30, 40))
  rows <- c(1L, 3L)
  cols <- c(2L, 5L)
  off <- 1L
  expect_equal(built$func(m, rows, cols, off), f(m, rows, cols, off))
})

test_that("subarray assignment supports ifelse i32[] index expressions", {  skip_if_no_mojo()
  f <- function(vals, rows_a, rows_b, cols, layers, mask) {
    arr <- array(0, dim = c(4, 4, 3))
    arr[ifelse(mask, rows_a, rows_b), cols, layers] <- vals
    arr
  }

  built <- mojor_build(
    f,
    vals = "f64[]",
    rows_a = "i32[]",
    rows_b = "i32[]",
    cols = "i32[]",
    layers = "i32[]",
    mask = "lgl[]",
    ir_only = TRUE,
    cache = FALSE,
    load = TRUE,
    name = "t_subarray_assign_ifelse_vec_idx"
  )

  vals <- as.double(1:8)
  rows_a <- c(1L, 3L)
  rows_b <- c(2L, 4L)
  cols <- c(1L, 4L)
  layers <- c(1L, 3L)
  mask <- c(TRUE, FALSE)
  expect_equal(built$func(vals, rows_a, rows_b, cols, layers, mask), f(vals, rows_a, rows_b, cols, layers, mask))
})

test_that("subarray assignment supports pmin/pmax i32[] index expressions", {  skip_if_no_mojo()
  f <- function(vals, rows, cols, layers, row_cap, layer_floor) {
    arr <- array(0, dim = c(4, 4, 3))
    arr[pmin(rows + 1L, row_cap), pmax(cols - 1L, 1L), pmax(layers - 1L, layer_floor)] <- vals
    arr
  }

  built <- mojor_build(
    f,
    vals = "f64[]",
    rows = "i32[]",
    cols = "i32[]",
    layers = "i32[]",
    row_cap = "i32",
    layer_floor = "i32",
    ir_only = TRUE,
    cache = FALSE,
    load = TRUE,
    name = "t_subarray_assign_pmin_pmax_vec_idx"
  )

  vals <- as.double(1:8)
  rows <- c(1L, 3L)
  cols <- c(1L, 4L)
  layers <- c(1L, 3L)
  row_cap <- 4L
  layer_floor <- 1L
  expect_equal(
    built$func(vals, rows, cols, layers, row_cap, layer_floor),
    f(vals, rows, cols, layers, row_cap, layer_floor)
  )
})

test_that("submatrix assignment with local integer vector vars is supported", {  skip_if_no_mojo()
  f <- function(m) {
    mat <- matrix(0, 5, 5)
    rows <- c(1L, 3L)
    cols <- c(2L, 5L)
    mat[rows, cols] <- m
    mat
  }

  built <- mojor_build(
    f,
    m = "f64[]",
    ir_only = TRUE,
    cache = FALSE,
    load = TRUE,
    name = "t_submatrix_assign_local_vec_idx"
  )

  m <- as.double(c(10, 20, 30, 40))
  expect_equal(built$func(m), f(m))
})

test_that("slice assignment supports indexed matrix RHS slices", {  skip_if_no_mojo()
  f <- function(rhs) {
    out <- matrix(0, 3, 3)
    out[1:2, 2:3] <- rhs[1:2, 1:2]
    out
  }

  built <- mojor_build(
    f,
    rhs = "f64[,]",
    ir_only = TRUE,
    cache = FALSE,
    load = TRUE,
    name = "t_slice_assign_rhs_matrix_slice"
  )

  rhs <- matrix(as.double(1:9), nrow = 3, ncol = 3)
  expect_equal(built$func(rhs), f(rhs))
})

test_that("slice assignment supports indexed ND RHS slices", {  skip_if_no_mojo()
  f <- function(rhs) {
    out <- array(0, dim = c(2, 2, 2))
    out[1:2, 1:2, 2] <- rhs[1:2, 1:2, 1]
    out
  }

  built <- mojor_build(
    f,
    rhs = "f64[3d]",
    ir_only = TRUE,
    cache = FALSE,
    load = TRUE,
    name = "t_slice_assign_rhs_array_slice"
  )

  rhs <- array(as.double(1:8), dim = c(2, 2, 2))
  expect_equal(built$func(rhs), f(rhs))
})

test_that("matrix slice assignment supports direct array RHS vars", {  skip_if_no_mojo()
  f <- function(vals, n) {
    m <- matrix(0, n, 2)
    for (i in seq_len(n)) {
      m[i, ] <- vals
    }
    m
  }

  built <- mojor_build(
    f,
    vals = "f64[]",
    n = "i32",
    ir_only = TRUE,
    cache = FALSE,
    load = TRUE,
    name = "t_matrix_assign_array_rhs_var"
  )

  vals <- as.double(c(5, 6))
  expect_equal(built$func(vals, 4L), f(vals, 4L))

  vals_short <- as.double(9)
  expect_equal(built$func(vals_short, 3L), f(vals_short, 3L))
})

test_that("matrix slice assignment supports computed array RHS expressions", {  skip_if_no_mojo()
  f <- function(vals, offs, scale, n) {
    m <- matrix(0, n, 2)
    for (i in seq_len(n)) {
      m[i, ] <- (vals * scale) + offs
    }
    m
  }

  built <- mojor_build(
    f,
    vals = "f64[]",
    offs = "f64[]",
    scale = "f64",
    n = "i32",
    ir_only = TRUE,
    cache = FALSE,
    load = TRUE,
    name = "t_matrix_assign_array_rhs_expr"
  )

  vals <- as.double(c(2, 4))
  offs <- as.double(1)
  expect_equal(built$func(vals, offs, 0.5, 4L), f(vals, offs, 0.5, 4L))
})

test_that("ND slice assignment supports computed array RHS expressions", {  skip_if_no_mojo()
  f <- function(rhs, offs, scale, n) {
    arr <- array(0, dim = c(2, 2, n))
    for (k in seq_len(n)) {
      arr[, , k] <- (rhs + offs) * scale
    }
    arr
  }

  built <- mojor_build(
    f,
    rhs = "f64[]",
    offs = "f64[]",
    scale = "f64",
    n = "i32",
    ir_only = TRUE,
    cache = FALSE,
    load = TRUE,
    name = "t_nd_assign_array_rhs_expr"
  )

  rhs <- as.double(c(3, 5))
  offs <- as.double(2)
  expect_equal(built$func(rhs, offs, 1.5, 2L), f(rhs, offs, 1.5, 2L))
})
