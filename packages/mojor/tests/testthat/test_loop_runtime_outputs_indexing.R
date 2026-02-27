library(testthat)

test_that("return with scalar accumulator produces correct results", {  skip_if_no_mojo()
  f <- function(x, threshold) {
    acc <- 0.0
    for (i in seq_along(x)) {
      if (x[i] > threshold) {
        acc <- i
        return(acc)
      }
    }
    return(acc)
  }
  built <- mojor_build(f, x = "f64[]", threshold = "f64", name = "t_return_scalar_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(1, 2, 6, 7, 3))
  expect_equal(built$func(x, 5.0), f(x, 5.0))
  expect_equal(built$func(x, 10.0), f(x, 10.0))
})

# ============================================================================
# matrix output runtime tests (paired with test_transpile_matrix_output.R)
# ============================================================================

test_that("matrix output with multiple assignments produces correct results", {  skip_if_no_mojo()
  f <- function(x, n) {
    out <- matrix(0, n, n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        out[i, j] <- x[i] + x[j]
      }
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "t_matrix_out_assign_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(1, 2, 3))
  n <- 3L
  expect_equal(built$func(x, n), f(x, n))
})

test_that("matrix output with dimnames produces correct results", {  skip_if_no_mojo()
  f <- function(x, n) {
    out <- matrix(0, n, n, dimnames = list(c("r1", "r2"), c("c1", "c2")))
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        out[i, j] <- i * 10 + j
      }
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "t_matrix_out_dimnames_rt", cache = FALSE, load = TRUE)
  x <- as.double(1:2)
  n <- 2L
  expect_equal(built$func(x, n), f(x, n))
})


# ============================================================================
# slice indexing runtime tests (paired with test_transpile_slice_index.R)
# ============================================================================

test_that("basic indexing produces correct results", {  skip_if_no_mojo()
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i]
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", name = "t_basic_idx_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(10, 20, 30, 40))
  expect_equal(built$func(x), f(x))
})

test_that("offset indexing produces correct results with bounds", {  skip_if_no_mojo()
  old <- mojor_options()$index_bounds
  on.exit(mojor_options(index_bounds = old), add = TRUE)
  mojor_options(index_bounds = TRUE)
  f <- function(x) {
    n <- as.integer(length(x))
    acc <- 0.0
    for (i in 2:n) {
      acc <- acc + x[i - 1] + x[i]
    }
    acc
  }
  built <- mojor_build(f, x = "f64[]", name = "t_offset_idx_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(1, 2, 3, 4, 5))
  expect_equal(built$func(x), f(x))
})

# ============================================================================
# array output runtime tests (paired with test_transpile_array_output.R)
# ============================================================================

test_that("3D array output produces correct results", {  skip_if_no_mojo()
  f <- function(x, n) {
    out <- array(0, dim = c(n, n, n))
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        for (k in seq_len(n)) {
          out[i, j, k] <- i + 10 * j + 100 * k + x[1]
        }
      }
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "t_array_3d_out_rt", cache = FALSE, load = TRUE)
  x <- as.double(7)
  n <- 2L
  expect_equal(built$func(x, n), f(x, n))
})

test_that("matrix output from vector produces correct results", {  skip_if_no_mojo()
  f <- function(x, nr, nc) {
    out <- matrix(0, nr, nc)
    for (i in seq_len(nr)) {
      for (j in seq_len(nc)) {
        out[i, j] <- x[(i - 1) * nc + j]
      }
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", nr = "i32", nc = "i32", name = "t_mat_out_from_vec_rt", cache = FALSE, load = TRUE)
  x <- as.double(1:6)
  expect_equal(built$func(x, 2L, 3L), f(x, 2L, 3L))
})


# ============================================================================
# as.logical runtime tests (paired with test_transpile_aslogical.R)
# ============================================================================

test_that("as.logical produces correct results at runtime", {  skip_if_no_mojo()
  f <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) {
      out[i] <- as.logical(x[i])
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", name = "t_aslogical_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(0, 1, -1, 0.5, 0, 3.14))
  expect_equal(built$func(x), f(x))
})

# ============================================================================
# integer vector output runtime tests (paired with test_transpile_integer.R)
# ============================================================================

test_that("integer vector output produces correct results at runtime", {  skip_if_no_mojo()
  f <- function(x) {
    out <- integer(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + 1L
    }
    out
  }
  built <- mojor_build(f, x = "i32[]", name = "t_integer_output_rt", cache = FALSE, load = TRUE)
  x <- as.integer(c(1, 5, -3, 0, 100))
  expect_equal(built$func(x), f(x))
})

# ============================================================================
# slice write runtime tests (paired with test_transpile_slice_write.R)
# ============================================================================

test_that("slice write out[1:n] <- x[1:n] produces correct results at runtime", {  skip_if_no_mojo()
  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[1:n] <- x[1:n]
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "t_slice_write_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(10, 20, 30, 40, 50))
  expect_equal(built$func(x, 5L), f(x, 5L))
})

test_that("in-loop canonical slice allocation y <- x[1:k] works in strict and non-strict lanes", {  skip_if_no_mojo()
  f <- function(x, k, m) {
    out <- numeric(m)
    for (i in 1:m) {
      y <- x[1:k]
      out[i] <- y[i]
    }
    out
  }

  x <- as.double(c(2, 4, 6, 8, 10))
  k <- 4L
  m <- 4L
  ref <- f(x, k, m)

  old_ir_only <- mojor_options()$ir_only
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)

  mojor_options(ir_only = FALSE)
  built_non_strict <- mojor_build(
    f,
    x = "f64[]",
    k = "i32",
    m = "i32",
    name = "t_loop_slice_alloc_non_strict",
    cache = FALSE,
    load = TRUE
  )
  expect_equal(built_non_strict$func(x, k, m), ref)

  mojor_options(ir_only = TRUE)
  built_strict <- mojor_build(
    f,
    x = "f64[]",
    k = "i32",
    m = "i32",
    name = "t_loop_slice_alloc_strict",
    cache = FALSE,
    load = TRUE
  )
  expect_equal(built_strict$func(x, k, m), ref)
})

test_that("logical index assignment out[mask] <- x[i] produces correct results at runtime", {  skip_if_no_mojo()
  f <- function(x, mask) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[mask] <- x[i]
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", mask = "lgl[]", name = "t_logical_write_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(10, 20, 30, 40, 50))
  mask <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
  expect_equal(built$func(x, mask), f(x, mask))
})

test_that("logical index assignment skips NA mask entries (runtime)", {  skip_if_no_mojo()
  f <- function(x, mask) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[mask] <- x[i]
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", mask = "lgl[]", name = "t_logical_write_na_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(10, 20, 30, 40, 50))
  mask <- c(TRUE, NA, FALSE, TRUE, NA)
  expect_equal(built$func(x, mask), f(x, mask))
})

test_that("na_mode variants run elementwise arithmetic with NaN inputs", {  skip_if_no_mojo()
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      z <- x[i] / y[i]
      out[i] <- z + 1
    }
    out
  }
  x <- as.double(c(0, 1, 2))
  y <- as.double(c(0, 1, 2))
  ref <- f(x, y)
  for (mode in c("assign", "unsafe")) {
    built <- mojor_build(f, x = "f64[]", y = "f64[]", name = paste0("t_na_elemwise_", mode), cache = FALSE, load = TRUE, na_mode = mode)
    res <- built$func(x, y)
    expect_equal(is.nan(res), is.nan(ref))
    expect_equal(res[!is.nan(res)], ref[!is.nan(ref)])
  }
  built_forbid <- mojor_build(f, x = "f64[]", y = "f64[]", name = "t_na_elemwise_forbid", cache = FALSE, load = TRUE, na_mode = "forbid")
  expect_error(built_forbid$func(x, y), "NA detected", fixed = TRUE)
})

# ============================================================================
# Slice IR runtime tests (paired with test_transpile_slice_ir.R)
# ============================================================================

test_that("vector slice with scalar RHS produces correct results", {  skip_if_no_mojo()
  f <- function(x, n, m) {
    out <- numeric(n)
    for (k in 1:m) {
      out[1:n] <- x[k]
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", n = "i32", m = "i32",
                       name = "t_vec_slice_scalar_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(10, 20, 30))
  expect_equal(built$func(x, 5L, 3L), f(x, 5L, 3L))
})

test_that("vector slice with seq_len produces correct results", {  skip_if_no_mojo()
  f <- function(n, m) {
    out <- numeric(n)
    for (k in 1:m) {
      out[seq_len(n)] <- k
    }
    out
  }
  built <- mojor_build(f, n = "i32", m = "i32",
                       name = "t_vec_slice_seqlen_rt", cache = FALSE, load = TRUE)
  expect_equal(built$func(5L, 3L), f(5L, 3L))
})

test_that("vector slice with expression RHS produces correct results", {  skip_if_no_mojo()
  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[1:n] <- x[i] * 2.0
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", n = "i32",
                       name = "t_vec_slice_expr_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(1, 2, 3, 4, 5))
  expect_equal(built$func(x, 5L), f(x, 5L))
})

test_that("vector slice with offset start produces correct results", {  skip_if_no_mojo()
  f <- function(n) {
    out <- numeric(n)
    for (i in 1:10) {
      out[2:n] <- i
    }
    out
  }
  built <- mojor_build(f, n = "i32",
                       name = "t_vec_slice_offset_rt", cache = FALSE, load = TRUE)
  expect_equal(built$func(5L), f(5L))
})

test_that("vector slice with end expression produces correct results", {  skip_if_no_mojo()
  f <- function(n) {
    out <- numeric(n)
    for (i in 1:3) {
      out[2:(n - 1)] <- i
    }
    out
  }
  built <- mojor_build(f, n = "i32",
                       name = "t_vec_slice_endexpr_rt", cache = FALSE, load = TRUE)
  expect_equal(built$func(5L), f(5L))
})

test_that("matrix row slice with scalar RHS produces correct results", {  skip_if_no_mojo()
  f <- function(n, m) {
    out <- matrix(0, n, m)
    for (i in 1:n) {
      out[i, ] <- i
    }
    out
  }
  built <- mojor_build(f, n = "i32", m = "i32",
                       name = "t_mat_slice_scalar_rt", cache = FALSE, load = TRUE)
  expect_equal(built$func(3L, 4L), f(3L, 4L))
})

test_that("matrix row slice with expression RHS produces correct results", {  skip_if_no_mojo()
  f <- function(x, y, n, m) {
    out <- matrix(0, n, m)
    for (i in 1:n) {
      out[i, ] <- x[i] + y[i]
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", y = "f64[]", n = "i32", m = "i32",
                       name = "t_mat_slice_expr_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(1, 2, 3))
  y <- as.double(c(10, 20, 30))
  expect_equal(built$func(x, y, 3L, 4L), f(x, y, 3L, 4L))
})

# ============================================================================
# loop fusion runtime tests (paired with test_transpile_fusion.R)
# ============================================================================


# ============================================================================
# migrated from legacy monolithic runtime test file (loop/output/indexing coverage)
# ============================================================================
test_that("loop variable value semantics match R (runtime)", {  skip_if_no_mojo()
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- i
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", name = "t_loop_var_value_rt", cache = FALSE, load = TRUE)
  x <- as.double(1:5)
  expect_equal(built$func(x), f(x))
})

test_that("triple nested loops produce correct 3D output", {  skip_if_no_mojo()
  f <- function(x, n) {
    out <- array(0, dim = c(n, n, n))
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        for (k in seq_len(n)) {
          out[i, j, k] <- i + 10 * j + 100 * k
        }
      }
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "t_loop_3d_rt", cache = FALSE, load = TRUE)
  n <- 3L
  x <- as.double(1:n)
  expect_equal(built$func(x, n), f(x, n))
})

test_that("nested loops with different ranges match R (scalar reduction)", {  skip_if_no_mojo()
  f <- function(x, y) {
    acc <- 0
    for (i in seq_along(x)) {
      for (j in seq_along(y)) {
        acc <- acc + x[i] * y[j]
      }
    }
    acc
  }
  built <- mojor_build(f, x = "f64[]", y = "f64[]", name = "t_loop_mixed_scalar_rt", cache = FALSE, load = TRUE)
  x <- as.double(1:4)
  y <- as.double(1:4)
  expect_equal(built$func(x, y), f(x, y))
})

test_that("nested loops with different ranges fill matrix output", {  skip_if_no_mojo()
  f <- function(x, y) {
    out <- matrix(0, length(x), length(y))
    for (i in seq_along(x)) {
      for (j in seq_along(y)) {
        out[i, j] <- x[i] + y[j]
      }
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", y = "f64[]", name = "t_loop_mixed_mat_rt", cache = FALSE, load = TRUE)
  x <- as.double(1:3)
  y <- as.double(1:5)
  expect_equal(built$func(x, y), f(x, y))
})

test_that("mismatched loop ranges are allowed when arrays use distinct loop vars", {  skip_if_no_mojo()
  f <- function(x, y) {
    acc <- 0
    for (i in seq_along(x)) {
      for (j in seq_along(y)) {
        acc <- acc + x[i] * y[j]
      }
    }
    acc
  }
  built <- mojor_build(f, x = "f64[]", y = "f64[]", name = "t_loop_mixed_len_ok", cache = FALSE, load = TRUE)
  x <- as.double(1:4)
  y <- as.double(1:3)
  expect_equal(built$func(x, y), f(x, y))
})

test_that("length mismatches still error when arrays share the loop var", {  skip_if_no_mojo()
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", y = "f64[]", name = "t_loop_len_mismatch_err", cache = FALSE, load = TRUE)
  x <- as.double(1:4)
  y <- as.double(1:3)
  expect_error(built$func(x, y), "length mismatch")
})

test_that("complex index expressions allow mismatched lengths with bounds helper", {  skip_if_no_mojo()
  f <- function(x, y, shift) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- y[i + shift[i]]
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", y = "f64[]", shift = "i32[]", name = "t_loop_complex_idx", cache = FALSE, load = TRUE)
  x <- as.double(1:4)
  y <- as.double(10:14)
  shift <- rep(1L, length(x))
  expect_equal(built$func(x, y, shift), f(x, y, shift))
})

test_that("complex 2D index expressions are bounds-checked", {  skip_if_no_mojo()
  f <- function(x, shift, nr, nc) {
    out <- matrix(0, nr, nc)
    for (i in 1:nr) {
      for (j in 1:nc) {
        out[i, j] <- x[i, j + shift[i]]
      }
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", shift = "i32[]", nr = "i32", nc = "i32", name = "t_loop_complex_idx_mat", cache = FALSE, load = TRUE)
  mat <- matrix(as.double(1:12), nrow = 3, ncol = 4)
  shift <- rep(0L, nrow(mat))
  expect_equal(built$func(mat, shift, nrow(mat), ncol(mat)), f(mat, shift, nrow(mat), ncol(mat)))
})

test_that("complex N-D index expressions are bounds-checked", {  skip_if_no_mojo()
  f <- function(x, shift, nr, nc, nd) {
    out <- array(0, dim = c(nr, nc, nd))
    for (i in 1:nr) {
      for (j in 1:nc) {
        for (k in 1:nd) {
          out[i, j, k] <- x[i, j + shift[i], k]
        }
      }
    }
    out
  }
  built <- mojor_build(
    f,
    x = "f64[]",
    shift = "i32[]",
    nr = "i32",
    nc = "i32",
    nd = "i32",
    name = "t_loop_complex_idx_nd",
    cache = FALSE,
    load = TRUE
  )
  nr <- 2L
  nc <- 3L
  nd <- 2L
  arr <- array(as.double(1:(nr * nc * nd)), dim = c(nr, nc, nd))
  shift <- rep(0L, nr)
  expect_equal(built$func(arr, shift, nr, nc, nd), f(arr, shift, nr, nc, nd))
})

test_that("local scalar temporaries inside loops are supported", {  skip_if_no_mojo()
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      tmp <- x[i] * 2
      out[i] <- tmp + 1
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", name = "t_loop_local_scalar", cache = FALSE, load = TRUE)
  x <- as.double(1:6)
  expect_equal(built$func(x), f(x))
})

test_that("pre-loop output assignments are preserved", {  skip_if_no_mojo()
  f <- function(x) {
    out <- numeric(length(x))
    out[1] <- x[1] + 10
    for (i in 2:length(x)) {
      out[i] <- x[i] * 2
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", name = "t_loop_pre_out_assign", cache = FALSE, load = TRUE)
  x <- as.double(1:5)
  expect_equal(built$func(x), f(x))
})

test_that("pre-loop if/else output assignments are preserved", {  skip_if_no_mojo()
  f <- function(n, flag, v) {
    out <- numeric(n)
    if (flag > 0) {
      out[1] <- v + 1
    } else {
      out[1] <- v + 2
    }
    for (i in 2:n) {
      out[i] <- v + i
    }
    out
  }
  built <- mojor_build(f, n = "i32", flag = "i32", v = "f64", name = "t_loop_pre_out_if", cache = FALSE, load = TRUE)
  expect_equal(built$func(5L, 1L, 3.5), f(5L, 1L, 3.5))
  expect_equal(built$func(5L, 0L, 3.5), f(5L, 0L, 3.5))
})

test_that("pre-loop nested if/else output assignments are preserved", {  skip_if_no_mojo()
  f <- function(n, a, b) {
    out <- numeric(n)
    if (a > 0) {
      if (b > 0) {
        out[1] <- 1
      } else {
        out[1] <- 2
      }
    } else {
      out[1] <- 3
    }
    for (i in 2:n) {
      out[i] <- i
    }
    out
  }
  built <- mojor_build(f, n = "i32", a = "i32", b = "i32", name = "t_loop_pre_out_nested_if", cache = FALSE, load = TRUE)
  expect_equal(built$func(5L, 1L, 1L), f(5L, 1L, 1L))
  expect_equal(built$func(5L, 1L, 0L), f(5L, 1L, 0L))
  expect_equal(built$func(5L, 0L, 1L), f(5L, 0L, 1L))
})

test_that("vector assignments accept index expressions", {  skip_if_no_mojo()
  f <- function(x, k) {
    out <- numeric(length(x))
    for (i in 1:(length(x) - k)) {
      out[i + k] <- x[i] * 2
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", k = "i32", name = "t_loop_idx_expr_out", cache = FALSE, load = TRUE)
  x <- as.double(1:6)
  k <- 1L
  expect_equal(built$func(x, k), f(x, k))
})

test_that("matrix assignments accept index expressions", {  skip_if_no_mojo()
  f <- function(nr, nc) {
    out <- matrix(0, nr, nc)
    for (i in 1:(nr - 1)) {
      for (j in 1:(nc - 1)) {
        out[i + 1, j + 1] <- i + j
      }
    }
    out
  }
  built <- mojor_build(f, nr = "i32", nc = "i32", name = "t_loop_mat_idx_expr", cache = FALSE, load = TRUE)
  expect_equal(built$func(4L, 5L), f(4L, 5L))
})

test_that("array assignments accept index expressions", {  skip_if_no_mojo()
  f <- function(n) {
    out <- array(0, dim = c(n, n, n))
    for (i in 1:(n - 1)) {
      for (j in 1:(n - 1)) {
        for (k in 1:n) {
          out[i + 1, j + 1, k] <- i + j + k
        }
      }
    }
    out
  }
  built <- mojor_build(f, n = "i32", name = "t_loop_arr_idx_expr", cache = FALSE, load = TRUE)
  expect_equal(built$func(4L), f(4L))
})

test_that("pre-loop matrix/array assignments with scalar indices are preserved", {  skip_if_no_mojo()
  f_mat <- function(nr, nc, v) {
    out <- matrix(0, nr, nc)
    r <- 1L
    c <- 2L
    out[r, c] <- v + 1
    for (i in 1:nr) {
      for (j in 1:nc) {
        if (i != r || j != c) {
          out[i, j] <- v + i + j
        }
      }
    }
    out
  }
  built_mat <- mojor_build(f_mat, nr = "i32", nc = "i32", v = "f64", name = "t_loop_pre_out_mat", cache = FALSE, load = TRUE)
  expect_equal(built_mat$func(3L, 4L, 2.0), f_mat(3L, 4L, 2.0))

  f_arr <- function(nr, nc, nd, v) {
    out <- array(0, dim = c(nr, nc, nd))
    r <- 1L
    c <- 2L
    d <- 1L
    out[r, c, d] <- v + 1
    for (i in 1:nr) {
      for (j in 1:nc) {
        for (k in 1:nd) {
          if (i != r || j != c || k != d) {
            out[i, j, k] <- v + i + j + k
          }
        }
      }
    }
    out
  }
  built_arr <- mojor_build(
    f_arr,
    nr = "i32",
    nc = "i32",
    nd = "i32",
    v = "f64",
    name = "t_loop_pre_out_arr",
    cache = FALSE,
    load = TRUE
  )
  expect_equal(built_arr$func(2L, 3L, 2L, 1.5), f_arr(2L, 3L, 2L, 1.5))
})
