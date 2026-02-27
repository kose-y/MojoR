library(testthat)

test_that("edge syntax matrix: loop/read/write wrappers are equivalent across modes", {
  make_loop_fn <- function(seq_expr) {
    eval(substitute(function(x, n) {
      out <- numeric(n)
      for (i in SEQ_EXPR) {
        out[i] <- x[i] + 1.0
      }
      out
    }, list(SEQ_EXPR = seq_expr)))
  }
  make_read_fn <- function(sel_expr) {
    eval(substitute(function(x, n) {
      out <- numeric(n)
      out[1:n] <- x[SEL_EXPR]
      out
    }, list(SEL_EXPR = sel_expr)))
  }
  make_write_fn <- function(sel_expr) {
    eval(substitute(function(x, n) {
      out <- numeric(n)
      out[SEL_EXPR] <- x[1:n]
      out
    }, list(SEL_EXPR = sel_expr)))
  }

  forms <- list(
    base = quote(1:n),
    paren = quote((1:n)),
    plus = quote(+(1:n)),
    seq_len = quote(seq_len(n)),
    seq_len_paren = quote((seq_len(n))),
    seqint = quote(seq.int(1L, n)),
    seqint_paren = quote((seq.int(1L, n)))
  )
  modes <- list(strict = TRUE, non_strict = FALSE)
  x <- c(2.0, 4.0, 6.0)
  n <- 3L
  results <- list()

  skip_if_no_mojo()
  for (mode_name in names(modes)) {
    .mojor_test_local_options(ir_only = modes[[mode_name]])

    loop_base <- mojor_build(
      make_loop_fn(forms$base),
      x = "f64[]",
      n = "i32",
      name = paste0("idx_syn_matrix_loop_base_", mode_name)
    )
    loop_ref <- loop_base$func(x, n)
    for (form_name in names(forms)) {
      built <- mojor_build(
        make_loop_fn(forms[[form_name]]),
        x = "f64[]",
        n = "i32",
        name = paste0("idx_syn_matrix_loop_", form_name, "_", mode_name)
      )
      out <- built$func(x, n)
      expect_equal(out, loop_ref)
      results[[paste("loop", form_name, mode_name, sep = "_")]] <- out
    }

    read_base <- mojor_build(
      make_read_fn(forms$base),
      x = "f64[]",
      n = "i32",
      name = paste0("idx_syn_matrix_read_base_", mode_name)
    )
    read_ref <- read_base$func(x, n)
    for (form_name in names(forms)) {
      built <- mojor_build(
        make_read_fn(forms[[form_name]]),
        x = "f64[]",
        n = "i32",
        name = paste0("idx_syn_matrix_read_", form_name, "_", mode_name)
      )
      out <- built$func(x, n)
      expect_equal(out, read_ref)
      results[[paste("read", form_name, mode_name, sep = "_")]] <- out
    }

    write_base <- mojor_build(
      make_write_fn(forms$base),
      x = "f64[]",
      n = "i32",
      name = paste0("idx_syn_matrix_write_base_", mode_name)
    )
    write_ref <- write_base$func(x, n)
    for (form_name in names(forms)) {
      built <- mojor_build(
        make_write_fn(forms[[form_name]]),
        x = "f64[]",
        n = "i32",
        name = paste0("idx_syn_matrix_write_", form_name, "_", mode_name)
      )
      out <- built$func(x, n)
      expect_equal(out, write_ref)
      results[[paste("write", form_name, mode_name, sep = "_")]] <- out
    }
  }

  for (form_name in names(forms)) {
    expect_equal(results[[paste("loop", form_name, "strict", sep = "_")]],
                 results[[paste("loop", form_name, "non_strict", sep = "_")]])
    expect_equal(results[[paste("read", form_name, "strict", sep = "_")]],
                 results[[paste("read", form_name, "non_strict", sep = "_")]])
    expect_equal(results[[paste("write", form_name, "strict", sep = "_")]],
                 results[[paste("write", form_name, "non_strict", sep = "_")]])
  }
})

test_that("edge syntax matrix: scalar index sugar wrappers are equivalent across modes", {
  make_read_fn <- function(sel_expr) {
    eval(substitute(function(x, n) {
      out <- numeric(n)
      for (i in seq_len(n)) {
        out[i] <- x[SEL_EXPR]
      }
      out
    }, list(SEL_EXPR = sel_expr)))
  }
  make_write_fn <- function(sel_expr) {
    eval(substitute(function(x, n) {
      out <- numeric(n)
      for (i in seq_len(n)) {
        out[SEL_EXPR] <- x[i]
      }
      out
    }, list(SEL_EXPR = sel_expr)))
  }

  forms <- list(
    base = quote(i),
    paren = quote((i)),
    plus = quote(+(i))
  )
  modes <- list(strict = TRUE, non_strict = FALSE)
  x <- c(2.0, 4.0, 6.0, 8.0)
  n <- 4L
  results <- list()

  skip_if_no_mojo()
  for (mode_name in names(modes)) {
    .mojor_test_local_options(ir_only = modes[[mode_name]])

    read_ref <- mojor_build(
      make_read_fn(forms$base),
      x = "f64[]",
      n = "i32",
      name = paste0("idx_syn_scalar_read_base_", mode_name)
    )$func(x, n)
    write_ref <- mojor_build(
      make_write_fn(forms$base),
      x = "f64[]",
      n = "i32",
      name = paste0("idx_syn_scalar_write_base_", mode_name)
    )$func(x, n)

    for (form_name in names(forms)) {
      read_out <- mojor_build(
        make_read_fn(forms[[form_name]]),
        x = "f64[]",
        n = "i32",
        name = paste0("idx_syn_scalar_read_", form_name, "_", mode_name)
      )$func(x, n)
      write_out <- mojor_build(
        make_write_fn(forms[[form_name]]),
        x = "f64[]",
        n = "i32",
        name = paste0("idx_syn_scalar_write_", form_name, "_", mode_name)
      )$func(x, n)
      expect_equal(read_out, read_ref)
      expect_equal(write_out, write_ref)
      results[[paste("read", form_name, mode_name, sep = "_")]] <- read_out
      results[[paste("write", form_name, mode_name, sep = "_")]] <- write_out
    }
  }

  for (form_name in names(forms)) {
    expect_equal(
      results[[paste("read", form_name, "strict", sep = "_")]],
      results[[paste("read", form_name, "non_strict", sep = "_")]]
    )
    expect_equal(
      results[[paste("write", form_name, "strict", sep = "_")]],
      results[[paste("write", form_name, "non_strict", sep = "_")]]
    )
  }
})

test_that("edge syntax matrix: negative selector wrappers are equivalent across modes", {
  make_neg_scalar_fn <- function(idx_expr) {
    eval(substitute(function(x, n) {
      out <- numeric(n)
      for (i in seq_len(n)) {
        out[i] <- x[IDX_EXPR]
      }
      out
    }, list(IDX_EXPR = idx_expr)))
  }
  make_neg_vector_fn <- function(sel_expr) {
    eval(substitute(function(x) {
      out <- numeric(1)
      out[1] <- x[SEL_EXPR]
      out
    }, list(SEL_EXPR = sel_expr)))
  }

  scalar_forms <- list(
    base = quote(-i),
    paren = quote(-(i)),
    plus = quote(-(+i))
  )
  vector_forms <- list(
    base = quote(-(1:2)),
    paren = quote(-((1:2))),
    plus = quote(-(+(1:2))),
    seqint = quote(-(seq.int(1L, 2L)))
  )
  modes <- list(strict = TRUE, non_strict = FALSE)
  x_scalar <- c(10.0, 20.0, 30.0, 40.0)
  x_vector <- c(1.0, 2.0, 3.0, 4.0, 5.0)
  n <- 3L
  results <- list()

  skip_if_no_mojo()
  for (mode_name in names(modes)) {
    .mojor_test_local_options(ir_only = modes[[mode_name]])

    scalar_ref <- mojor_build(
      make_neg_scalar_fn(scalar_forms$base),
      x = "f64[]",
      n = "i32",
      name = paste0("idx_syn_matrix_neg_scalar_base_", mode_name)
    )$func(x_scalar, n)
    for (form_name in names(scalar_forms)) {
      out <- mojor_build(
        make_neg_scalar_fn(scalar_forms[[form_name]]),
        x = "f64[]",
        n = "i32",
        name = paste0("idx_syn_matrix_neg_scalar_", form_name, "_", mode_name)
      )$func(x_scalar, n)
      expect_equal(out, scalar_ref)
      results[[paste("neg_scalar", form_name, mode_name, sep = "_")]] <- out
    }

    vector_ref <- mojor_build(
      make_neg_vector_fn(vector_forms$base),
      x = "f64[]",
      name = paste0("idx_syn_matrix_neg_vec_base_", mode_name)
    )$func(x_vector)
    for (form_name in names(vector_forms)) {
      out <- mojor_build(
        make_neg_vector_fn(vector_forms[[form_name]]),
        x = "f64[]",
        name = paste0("idx_syn_matrix_neg_vec_", form_name, "_", mode_name)
      )$func(x_vector)
      expect_equal(out, vector_ref)
      results[[paste("neg_vec", form_name, mode_name, sep = "_")]] <- out
    }
  }

  for (form_name in names(scalar_forms)) {
    expect_equal(results[[paste("neg_scalar", form_name, "strict", sep = "_")]],
                 results[[paste("neg_scalar", form_name, "non_strict", sep = "_")]])
  }
  for (form_name in names(vector_forms)) {
    expect_equal(results[[paste("neg_vec", form_name, "strict", sep = "_")]],
                 results[[paste("neg_vec", form_name, "non_strict", sep = "_")]])
  }
})

test_that("loop syntax: seq.int range is supported", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq.int(1L, n)) {
      out[i] <- x[i] + 2.0
    }
    out
  }

  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_syn_seq_int")
  expect_equal(built$func(c(1.0, 2.0, 3.0), 3L), c(3.0, 4.0, 5.0))
})

test_that("loop syntax: seq_len with unary plus bound is supported", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(+n)) {
      out[i] <- x[i] + 3.0
    }
    out
  }

  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_syn_seq_len_plus")
  expect_equal(built$func(c(1.0, 2.0, 3.0), 3L), c(4.0, 5.0, 6.0))
})

test_that("loop syntax: parenthesized 1:n range is canonicalized", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in (1:n)) {
      out[i] <- x[i]
    }
    out
  }

  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_syn_paren_loop_range")
  expect_equal(built$func(c(4.0, 5.0, 6.0), 3L), c(4.0, 5.0, 6.0))
})

test_that("loop syntax: unary plus wrapped 1:n range is canonicalized", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in +(1:n)) {
      out[i] <- x[i]
    }
    out
  }

  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_syn_plus_loop_range")
  expect_equal(built$func(c(7.0, 8.0, 9.0), 3L), c(7.0, 8.0, 9.0))
})

test_that("loop syntax: parenthesized seq.int range is canonicalized", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in (seq.int(1L, n))) {
      out[i] <- x[i]
    }
    out
  }

  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_syn_paren_seqint_loop_range")
  expect_equal(built$func(c(11.0, 12.0, 13.0), 3L), c(11.0, 12.0, 13.0))
})

test_that("index syntax: unary plus index is supported", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[+i]
    }
    out
  }

  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_syn_unary_plus_index")
  expect_equal(built$func(c(5.0, 6.0, 7.0), 3L), c(5.0, 6.0, 7.0))
})

test_that("index syntax: parenthesized arithmetic index is supported", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[(i + 0L)]
    }
    out
  }

  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_syn_paren_arith_index")
  expect_equal(built$func(c(5.0, 6.0, 7.0), 3L), c(5.0, 6.0, 7.0))
})

test_that("index syntax: parenthesized dynamic negative exclusion is supported", {
  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[-(i)]
    }
    out
  }

  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_syn_neg_paren_dynamic")
  expect_equal(built$func(c(10.0, 20.0, 30.0, 40.0), 3L), c(20.0, 10.0, 10.0))
})

test_that("index syntax: dynamic negative scalar wrappers are equivalent", {
  f_base <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[-i]
    }
    out
  }
  f_paren <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[-(i)]
    }
    out
  }
  f_plus <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[-(+i)]
    }
    out
  }

  skip_if_no_mojo()
  b_base <- mojor_build(f_base, x = "f64[]", n = "i32", name = "idx_syn_neg_dyn_base")
  b_paren <- mojor_build(f_paren, x = "f64[]", n = "i32", name = "idx_syn_neg_dyn_paren")
  b_plus <- mojor_build(f_plus, x = "f64[]", n = "i32", name = "idx_syn_neg_dyn_plus")
  x <- c(10.0, 20.0, 30.0, 40.0)
  expect_equal(b_base$func(x, 3L), b_paren$func(x, 3L))
  expect_equal(b_base$func(x, 3L), b_plus$func(x, 3L))
})

test_that("index syntax: negative vector range wrappers are equivalent", {
  f_base <- function(x) {
    out <- numeric(1)
    out[1] <- x[-(1:2)]
    out
  }
  f_paren <- function(x) {
    out <- numeric(1)
    out[1] <- x[-((1:2))]
    out
  }
  f_plus <- function(x) {
    out <- numeric(1)
    out[1] <- x[-(+(1:2))]
    out
  }
  f_seqint <- function(x) {
    out <- numeric(1)
    out[1] <- x[-(seq.int(1L, 2L))]
    out
  }

  skip_if_no_mojo()
  b_base <- mojor_build(f_base, x = "f64[]", name = "idx_syn_neg_vec_base")
  b_paren <- mojor_build(f_paren, x = "f64[]", name = "idx_syn_neg_vec_paren")
  b_plus <- mojor_build(f_plus, x = "f64[]", name = "idx_syn_neg_vec_plus")
  b_seqint <- mojor_build(f_seqint, x = "f64[]", name = "idx_syn_neg_vec_seqint")
  x <- c(1.0, 2.0, 3.0, 4.0, 5.0)
  expect_equal(b_base$func(x), b_paren$func(x))
  expect_equal(b_base$func(x), b_plus$func(x))
  expect_equal(b_base$func(x), b_seqint$func(x))
})

test_that("index syntax: negative wrapper forms match strict and non-strict", {
  f <- function(x) {
    out <- numeric(1)
    out[1] <- x[-(+(1:2))]
    out
  }

  skip_if_no_mojo()
  b_strict <- mojor_build(f, x = "f64[]", name = "idx_syn_neg_wrap_strict")
  .mojor_test_local_options(ir_only = FALSE)
  b_non_strict <- mojor_build(f, x = "f64[]", name = "idx_syn_neg_wrap_non_strict")

  x <- c(3.0, 4.0, 5.0, 6.0)
  expect_equal(b_strict$func(x), b_non_strict$func(x))
})

test_that("index syntax: parenthesized matrix indices are supported", {
  f <- function(mat, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- mat[(i), (2L)]
    }
    out
  }

  skip_if_no_mojo()
  built <- mojor_build(f, mat = "f64[,]", n = "i32", name = "idx_syn_matrix_paren")
  mat <- matrix(c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), nrow = 3, ncol = 2)
  expect_equal(built$func(mat, 3L), c(4.0, 5.0, 6.0))
})

test_that("index syntax: scalar [[ ]] read on vector is supported", {
  f <- function(x) {
    out <- numeric(1)
    out[1] <- x[[1]]
    out
  }

  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", name = "idx_syn_double_bracket_scalar")
  expect_equal(built$func(c(1.0, 2.0, 3.0)), 1.0)
})

test_that("index syntax: parenthesized 1:n selector is canonicalized", {
  f <- function(x, n) {
    out <- numeric(n)
    out[1:n] <- x[(1:n)]
    out
  }

  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_syn_paren_rhs_slice")
  expect_equal(built$func(c(2.0, 4.0, 6.0), 3L), c(2.0, 4.0, 6.0))
})

test_that("index syntax: unary plus wrapped 1:n selector is canonicalized", {
  f <- function(x, n) {
    out <- numeric(n)
    out[1:n] <- x[+(1:n)]
    out
  }

  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_syn_plus_rhs_slice")
  expect_equal(built$func(c(3.0, 6.0, 9.0), 3L), c(3.0, 6.0, 9.0))
})

test_that("index syntax: parenthesized seq.int selector is canonicalized", {
  f <- function(x, n) {
    out <- numeric(n)
    out[1:n] <- x[(seq.int(1L, n))]
    out
  }

  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_syn_paren_seqint_rhs_slice")
  expect_equal(built$func(c(1.5, 2.5, 3.5), 3L), c(1.5, 2.5, 3.5))
})

test_that("index syntax: parenthesized LHS slice selector is canonicalized", {
  f <- function(x, n) {
    out <- numeric(n)
    out[(1:n)] <- x[1:n]
    out
  }

  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_syn_paren_lhs_slice")
  expect_equal(built$func(c(8.0, 9.0, 10.0), 3L), c(8.0, 9.0, 10.0))
})

test_that("index syntax: parenthesized seq.int LHS selector is canonicalized", {
  f <- function(x, n) {
    out <- numeric(n)
    out[(seq.int(1L, n))] <- x[seq.int(1L, n)]
    out
  }

  skip_if_no_mojo()
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "idx_syn_paren_seqint_lhs_slice")
  expect_equal(built$func(c(0.5, 1.5, 2.5), 3L), c(0.5, 1.5, 2.5))
})

test_that("index syntax: wrapped selector paths avoid raw-node strict failures", {
  f <- function(x, n) {
    out <- numeric(n)
    out[1:n] <- x[(1:n)]
    out
  }

  skip_if_no_mojo()
  expect_error(
    mojor_build(f, x = "f64[]", n = "i32", name = "idx_syn_no_raw_verify_wrapped"),
    NA
  )
})

test_that("index syntax: full selectors x[] and x[] <- y are lowered (strict + non-strict)", {
  f_read <- function(x) {
    out <- x[]
    out
  }
  f_write <- function(x, y) {
    out <- x
    out[] <- y
    out
  }

  modes <- list(strict = TRUE, non_strict = FALSE)

  skip_if_no_mojo()
  for (mode_name in names(modes)) {
    .mojor_test_local_options(ir_only = modes[[mode_name]])

    read_built <- mojor_build(
      f_read,
      x = "f64[]",
      name = paste0("idx_syn_full_selector_read_", mode_name)
    )
    expect_equal(read_built$func(c(1.0, 2.0, 3.0)), c(1.0, 2.0, 3.0))

    write_built <- mojor_build(
      f_write,
      x = "f64[]",
      y = "f64[]",
      name = paste0("idx_syn_full_selector_write_", mode_name)
    )
    expect_equal(
      write_built$func(c(1.0, 2.0, 3.0, 4.0), c(9.0, 8.0)),
      c(9.0, 8.0, 9.0, 8.0)
    )
  }
})

test_that("index syntax: RHS missing-dimension slice assignment matches strict/non-strict + recycling", {
  f_mat <- function(mat, n, j) {
    out <- numeric(n)
    out[1:n] <- mat[, j]
    out
  }
  f_arr <- function(arr, n, i, k) {
    out <- numeric(n)
    out[1:n] <- arr[i, , k]
    out
  }

  modes <- list(strict = TRUE, non_strict = FALSE)

  skip_if_no_mojo()
  mat <- matrix(1:9, nrow = 3, ncol = 3)
  arr <- array(1:24, dim = c(2, 3, 4))
  n_cases <- c(3L, 5L)

  for (mode_name in names(modes)) {
    .mojor_test_local_options(ir_only = modes[[mode_name]])

    built_mat <- mojor_build(
      f_mat,
      mat = "f64[,]",
      n = "i32",
      j = "i32",
      name = paste0("idx_syn_missing_row_rhs_slice_", mode_name)
    )
    built_arr <- mojor_build(
      f_arr,
      arr = "f64[3d]",
      n = "i32",
      i = "i32",
      k = "i32",
      name = paste0("idx_syn_3d_missing_middle_rhs_slice_", mode_name)
    )

    for (n in n_cases) {
      expected_mat <- suppressWarnings({
        out <- numeric(n)
        out[1:n] <- mat[, 2L]
        out
      })
      expect_equal(
        built_mat$func(mat, n, 2L),
        expected_mat
      )
      expected_arr <- suppressWarnings({
        out <- numeric(n)
        out[1:n] <- arr[1L, , 2L]
        out
      })
      expect_equal(
        built_arr$func(arr, n, 1L, 2L),
        expected_arr
      )
    }
  }
})

test_that("index syntax: shifted LHS slice keeps RHS missing-index alignment (strict + non-strict)", {
  f_mat <- function(mat, n, j) {
    out <- numeric(n + 1L)
    out[2:(n + 1L)] <- mat[, j]
    out
  }
  f_arr <- function(arr, n, i, k) {
    out <- numeric(n + 1L)
    out[2:(n + 1L)] <- arr[i, , k]
    out
  }

  modes <- list(strict = TRUE, non_strict = FALSE)

  skip_if_no_mojo()
  for (mode_name in names(modes)) {
    .mojor_test_local_options(ir_only = modes[[mode_name]])

    built_mat <- mojor_build(
      f_mat,
      mat = "f64[,]",
      n = "i32",
      j = "i32",
      name = paste0("idx_syn_shift_missing_rhs_mat_", mode_name)
    )
    mat <- matrix(1:9, nrow = 3, ncol = 3)
    expected_mat <- numeric(4L)
    expected_mat[2:4] <- mat[, 2L]
    expect_equal(built_mat$func(mat, 3L, 2L), expected_mat)

    built_arr <- mojor_build(
      f_arr,
      arr = "f64[3d]",
      n = "i32",
      i = "i32",
      k = "i32",
      name = paste0("idx_syn_shift_missing_rhs_arr_", mode_name)
    )
    arr <- array(1:24, dim = c(2, 3, 4))
    expected_arr <- numeric(4L)
    expected_arr[2:4] <- arr[1L, , 2L]
    expect_equal(built_arr$func(arr, 3L, 1L, 2L), expected_arr)
  }
})

test_that("index syntax: RHS missing-dimension assignment supports LHS vec selectors", {
  f_mat <- function(mat, n, j) {
    out <- numeric(n)
    for (iter in 1:1L) {
      out[c(5L, 2L, 4L, 1L)] <- mat[, j]
    }
    out
  }
  f_arr <- function(arr, n, i, k) {
    out <- numeric(n)
    for (iter in 1:1L) {
      out[c(5L, 2L, 4L, 1L)] <- arr[i, , k]
    }
    out
  }

  modes <- list(strict = TRUE, non_strict = FALSE)

  skip_if_no_mojo()
  mat <- matrix(1:9, nrow = 3, ncol = 3)
  arr <- array(1:24, dim = c(2, 3, 4))
  n <- 5L

  for (mode_name in names(modes)) {
    .mojor_test_local_options(ir_only = modes[[mode_name]])

    built_mat <- mojor_build(
      f_mat,
      mat = "f64[,]",
      n = "i32",
      j = "i32",
      name = paste0("idx_syn_missing_rhs_vec_lhs_mat_", mode_name)
    )
    built_arr <- mojor_build(
      f_arr,
      arr = "f64[3d]",
      n = "i32",
      i = "i32",
      k = "i32",
      name = paste0("idx_syn_missing_rhs_vec_lhs_arr_", mode_name)
    )

    expect_equal(
      built_mat$func(mat, n, 2L),
      suppressWarnings({
        out <- numeric(n)
        out[c(5L, 2L, 4L, 1L)] <- mat[, 2L]
        out
      })
    )
    expect_equal(
      built_arr$func(arr, n, 1L, 2L),
      suppressWarnings({
        out <- numeric(n)
        out[c(5L, 2L, 4L, 1L)] <- arr[1L, , 2L]
        out
      })
    )
  }
})

test_that("index syntax: direct nested negative selectors are supported and missing is explicit-error", {
  f_vec <- function(x, idx) {
    out <- x[idx][-c(1L, 3L)]
    out
  }
  f_mat <- function(mat, row, cols) {
    out <- numeric(length(cols))
    out[1:length(cols)] <- mat[row, ][cols]
    out
  }

  modes <- list(strict = TRUE, non_strict = FALSE)

  skip_if_no_mojo()
  x <- c(10.0, 20.0, 30.0, 40.0, 50.0, 60.0)
  idx <- c(6L, 5L, 4L, 3L, 2L, 1L)
  mat <- matrix(1:12, nrow = 3, ncol = 4)
  cols <- c(4L, 2L, 1L)

  for (mode_name in names(modes)) {
    .mojor_test_local_options(ir_only = modes[[mode_name]])

    built_vec <- mojor_build(
      f_vec,
      x = "f64[]",
      idx = "i32[]",
      name = paste0("idx_syn_nested_neg_vec_", mode_name)
    )
    expect_equal(built_vec$func(x, idx), f_vec(x, idx))
    expect_error(
      mojor_build(
        f_mat,
        mat = "f64[,]",
        row = "i32",
        cols = "i32[]",
        name = paste0("idx_syn_nested_missing_mat_", mode_name)
      ),
      "direct nested selector with missing index is not supported"
    )
  }
})

test_that("selector safety matrix: invalid scalar and mixed-sign selectors fail across modes", {
  cases <- list(
    list(
      name = "zero_scalar_read",
      fn = function(x) {
        out <- numeric(1)
        out[1] <- x[0]
        out
      },
      pattern = "index 0 is unsupported in scalar selector"
    ),
    list(
      name = "mixed_sign_read",
      fn = function(x) {
        out <- numeric(1)
        out[1] <- x[c(1, -2, 3)]
        out
      },
      pattern = "mixed positive and negative selector values are not supported"
    ),
    list(
      name = "mixed_sign_write",
      fn = function(x) {
        out <- x
        out[c(1, -2, 3)] <- 1.0
        out
      },
      pattern = "mixed positive and negative selector values are not supported"
    ),
    list(
      name = "na_scalar_read",
      fn = function(x) {
        out <- numeric(1)
        out[1] <- x[NA_real_]
        out
      },
      pattern = "NA selector values are not supported"
    ),
    list(
      name = "nonfinite_scalar_read",
      fn = function(x) {
        out <- numeric(1)
        out[1] <- x[Inf]
        out
      },
      pattern = "non-finite selector values \\(Inf/NaN\\) are not supported"
    ),
    list(
      name = "na_vector_selector",
      fn = function(x) {
        out <- numeric(1)
        out[1] <- x[c(1, NA_real_)]
        out
      },
      pattern = "NA selector values are not supported"
    )
  )
  modes <- list(strict = TRUE, non_strict = FALSE)

  skip_if_no_mojo()
  for (mode_name in names(modes)) {
    .mojor_test_local_options(ir_only = modes[[mode_name]])
    for (case in cases) {
      expect_error(
        mojor_build(case$fn, x = "f64[]", name = paste0("idx_syn_safety_", case$name, "_", mode_name)),
        case$pattern
      )
    }
  }
})

test_that("selector safety matrix: literal logical selector mismatch on static extent fails across modes", {
  f <- function() {
    out <- numeric(1)
    out[1] <- c(10.0, 20.0, 30.0)[c(TRUE, FALSE)]
    out
  }
  modes <- list(strict = TRUE, non_strict = FALSE)

  skip_if_no_mojo()
  for (mode_name in names(modes)) {
    .mojor_test_local_options(ir_only = modes[[mode_name]])
    expect_error(
      mojor_build(f, name = paste0("idx_syn_safety_lgl_len_", mode_name)),
      "logical selector length mismatch for statically known extent"
    )
  }
})

test_that("selector safety runtime: dynamic i32[] selector emits guard checks", {
  f <- function(x, idx) {
    out <- x[idx]
    out
  }

  skip_if_no_mojo()
  trans <- mojor_transpile(
    f,
    x = "f64[]",
    idx = "i32[]",
    ir_only = TRUE,
    name = "idx_syn_dyn_i32_guard_emit"
  )

  expect_match(trans$mojo, "var __mojor_idx_raw\\d*: Int32 = _mojor_read_i32\\(idx, __mojor_pos_j\\d*")
  expect_match(trans$mojo, "if __mojor_idx_raw\\d* == -2147483648 or __mojor_idx_raw\\d* <= 0:")
  expect_match(trans$mojo, "_mojor_oob\\(\\)")
})

test_that("selector safety runtime: dynamic i32[] selector raises in strict bounds mode", {
  f <- function(x, idx) {
    out <- x[idx]
    out
  }

  skip_if_no_mojo()
  .mojor_test_local_options(ir_only = FALSE)
  old_index_bounds <- mojor_options()$index_bounds
  on.exit(mojor_options(index_bounds = old_index_bounds), add = TRUE)
  mojor_options(index_bounds = TRUE)
  built <- mojor_build(
    f,
    x = "f64[]",
    idx = "i32[]",
    bounds_check = TRUE,
    na_mode = "unsafe",
    name = "idx_syn_dyn_i32_strict_runtime_guard"
  )

  x <- c(10.0, 20.0, 30.0)
  expect_error(built$func(x, c(1L, NA_integer_)), "Index out of bounds")
  expect_error(built$func(x, c(1L, 0L)), "Index out of bounds")
  expect_equal(built$func(x, c(1L, 2L)), c(10.0, 20.0))
})

test_that("selector safety runtime: dynamic i32[] selector preserves legacy non-strict lane", {
  f <- function(x, idx) {
    out <- x[idx]
    out
  }

  skip_if_no_mojo()
  .mojor_test_local_options(ir_only = FALSE)
  old_index_bounds <- mojor_options()$index_bounds
  on.exit(mojor_options(index_bounds = old_index_bounds), add = TRUE)
  mojor_options(index_bounds = FALSE)
  built <- mojor_build(
    f,
    x = "f64[]",
    idx = "i32[]",
    bounds_check = FALSE,
    na_mode = "unsafe",
    name = "idx_syn_dyn_i32_legacy_guard"
  )

  x <- c(10.0, 20.0, 30.0)
  na_res <- built$func(x, c(1L, NA_integer_))
  zero_res <- built$func(x, c(1L, 0L))

  expect_equal(na_res[1], 10.0)
  expect_true(is.nan(na_res[2]))
  expect_equal(zero_res[1], 10.0)
  expect_true(is.nan(zero_res[2]))
})

test_that("canonicalization invariants: loop/index AST wrappers are normalized once", {
  loop_ast <- .mojor_ir_canonicalize_loop_index_ast(
    quote(for (i in +(1:n)) i)
  )
  idx_ast <- .mojor_ir_canonicalize_loop_index_ast(
    quote(x[+(seq.int(1L, n))])
  )

  expect_true(is.call(loop_ast[[3]]))
  expect_equal(as.character(loop_ast[[3]][[1]]), ":")
  expect_true(is.call(idx_ast[[3]]))
  expect_equal(as.character(idx_ast[[3]][[1]]), "seq.int")
})

test_that("canonicalization invariants: wrapped selector IR emits no raw selector nodes", {
  has_raw_kind <- function(node) {
    if (is.null(node)) {
      return(FALSE)
    }
    if (is.list(node) && !is.null(node$kind) && identical(node$kind, "raw")) {
      return(TRUE)
    }
    if (is.list(node)) {
      for (v in node) {
        if (has_raw_kind(v)) {
          return(TRUE)
        }
      }
      return(FALSE)
    }
    if (is.call(node) || is.pairlist(node)) {
      for (v in as.list(node)) {
        if (has_raw_kind(v)) {
          return(TRUE)
        }
      }
      return(FALSE)
    }
    FALSE
  }

  ir <- .mojor_ir_build_stmt(quote(out[(1:n)] <- x[+(seq.int(1L, n))]))
  ir <- .mojor_ir_normalize(ir)

  expect_equal(ir$kind, "assign")
  expect_false(has_raw_kind(ir$lhs))
  expect_false(has_raw_kind(ir$rhs))
  expect_equal(ir$lhs$kind, "subscript")
  expect_equal(ir$lhs$indices[[1]]$kind, "slice_index")
  expect_equal(ir$rhs$kind, "index")
  expect_equal(ir$rhs$indices[[1]]$kind, "slice_index")
})

test_that("canonicalization invariants: selector verification rejects raw nodes and unresolved markers", {
  bad_raw <- .mojor_ir_index(
    .mojor_ir_var("x"),
    list(.mojor_ir_raw(quote(i))),
    index_base = "one_based"
  )
  bad_marker <- .mojor_ir_index(
    .mojor_ir_var("x"),
    list(list(kind = "var", name = "__pos_vec_sel__")),
    index_base = "one_based"
  )

  verify_ctx <- list(
    ir_only = FALSE,
    type_env = list(x = "f64[]", i = "i32")
  )

  expect_error(
    .mojor_ir_verify(bad_raw, ctx = verify_ctx),
    "selector raw fallback node is not allowed after canonicalization"
  )
  expect_error(
    .mojor_ir_verify(bad_marker, ctx = verify_ctx),
    "unresolved selector marker leaked after canonicalization"
  )
})

test_that("negative write wrappers: returned-arg targets stay equivalent across modes", {
  make_arg_write_fn <- function(sel_expr) {
    eval(substitute(function(x) {
      x[SEL_EXPR] <- 9.0
      x
    }, list(SEL_EXPR = sel_expr)))
  }

  selectors <- list(
    scalar_base = quote(-1),
    scalar_paren = quote(-(1L)),
    scalar_plus = quote(-(+1L)),
    vec_base = quote(-c(1L, 3L)),
    vec_paren = quote(-((1:2))),
    vec_plus = quote(-(+(1:2))),
    vec_seqint = quote(-(seq.int(1L, 2L)))
  )
  modes <- list(strict = TRUE, non_strict = FALSE)
  cases <- list(
    scalar_base = c(1, 9, 9, 9, 9),
    scalar_paren = c(1, 9, 9, 9, 9),
    scalar_plus = c(1, 9, 9, 9, 9),
    vec_base = c(1, 9, 3, 9, 9),
    vec_paren = c(1, 2, 9, 9, 9),
    vec_plus = c(1, 2, 9, 9, 9),
    vec_seqint = c(1, 2, 9, 9, 9)
  )
  input <- c(1, 2, 3, 4, 5)
  mode_results <- list()

  skip_if_no_mojo()
  for (mode_name in names(modes)) {
    .mojor_test_local_options(ir_only = modes[[mode_name]])
    for (sel_name in names(selectors)) {
      built <- mojor_build(
        make_arg_write_fn(selectors[[sel_name]]),
        x = "f64[]",
        name = paste0("idx_syn_neg_return_arg_write_", sel_name, "_", mode_name)
      )
      out <- built$func(input)
      expect_equal(out, cases[[sel_name]])
      mode_results[[paste(sel_name, mode_name, sep = "_")]] <- out
    }
  }

  for (sel_name in names(selectors)) {
    expect_equal(
      mode_results[[paste(sel_name, "strict", sep = "_")]],
      mode_results[[paste(sel_name, "non_strict", sep = "_")]]
    )
  }
})

test_that("negative write wrappers: output-base writes stay equivalent across modes", {
  make_write_fn <- function(sel_expr) {
    eval(substitute(function(n) {
      out <- numeric(n)
      out[SEL_EXPR] <- 5.0
      out
    }, list(SEL_EXPR = sel_expr)))
  }

  cases <- list(
    scalar_base = list(expr = quote(-2), expect = c(5, 0, 5, 5, 5)),
    scalar_paren = list(expr = quote(-(2L)), expect = c(5, 0, 5, 5, 5)),
    scalar_plus = list(expr = quote(-(+2L)), expect = c(5, 0, 5, 5, 5)),
    vec_base = list(expr = quote(-c(2L, 4L)), expect = c(5, 0, 5, 0, 5)),
    vec_paren = list(expr = quote(-((1:2))), expect = c(0, 0, 5, 5, 5)),
    vec_plus = list(expr = quote(-(+(1:2))), expect = c(0, 0, 5, 5, 5)),
    vec_seqint = list(expr = quote(-(seq.int(1L, 2L))), expect = c(0, 0, 5, 5, 5))
  )
  modes <- list(strict = TRUE, non_strict = FALSE)
  n <- 5L
  mode_results <- list()

  skip_if_no_mojo()
  for (mode_name in names(modes)) {
    .mojor_test_local_options(ir_only = modes[[mode_name]])
    for (case_name in names(cases)) {
      built <- mojor_build(
        make_write_fn(cases[[case_name]]$expr),
        n = "i32",
        name = paste0("idx_syn_neg_output_write_", case_name, "_", mode_name)
      )
      out <- built$func(n)
      expect_equal(out, cases[[case_name]]$expect)
      mode_results[[paste(case_name, mode_name, sep = "_")]] <- out
    }
  }

  for (case_name in names(cases)) {
    expect_equal(
      mode_results[[paste(case_name, "strict", sep = "_")]],
      mode_results[[paste(case_name, "non_strict", sep = "_")]]
    )
  }
})

test_that("v1 freeze diagnostics: negative indexed writes on non-output base fail explicitly", {
  f <- function(x, n) {
    out <- numeric(n)
    tmp <- x
    tmp[-1L] <- 0.0
    out[1] <- tmp[1]
    out
  }
  modes <- list(strict = TRUE, non_strict = FALSE)

  skip_if_no_mojo()
  for (mode_name in names(modes)) {
    .mojor_test_local_options(ir_only = modes[[mode_name]])
    expect_error(
      mojor_build(
        f,
        x = "f64[]",
        n = "i32",
        name = paste0("idx_syn_neg_write_non_output_", mode_name)
      ),
      "negative indexed writes are supported only on the output base variable"
    )
  }
})

test_that("slice overlap writes: self-referential RHS uses snapshot semantics", {
  make_overlap_fn <- function(lhs_sel, rhs_sel) {
    eval(substitute(function(x) {
      out <- x
      out[LHS_SEL] <- out[RHS_SEL]
      out
    }, list(LHS_SEL = lhs_sel, RHS_SEL = rhs_sel)))
  }

  selector_pairs <- list(
    base = list(lhs = quote(2:4), rhs = quote(1:3)),
    paren = list(lhs = quote((2:4)), rhs = quote((1:3))),
    plus = list(lhs = quote(+(2:4)), rhs = quote(+(1:3))),
    seqint = list(lhs = quote(seq.int(2L, 4L)), rhs = quote(seq.int(1L, 3L))),
    seqint_paren = list(lhs = quote((seq.int(2L, 4L))), rhs = quote((seq.int(1L, 3L))))
  )
  modes <- list(strict = TRUE, non_strict = FALSE)
  x <- c(1.0, 2.0, 3.0, 4.0, 5.0)
  ref <- c(1.0, 1.0, 2.0, 3.0, 5.0)
  results <- list()

  skip_if_no_mojo()
  for (mode_name in names(modes)) {
    .mojor_test_local_options(ir_only = modes[[mode_name]])
    for (pair_name in names(selector_pairs)) {
      pair <- selector_pairs[[pair_name]]
      built <- mojor_build(
        make_overlap_fn(pair$lhs, pair$rhs),
        x = "f64[]",
        name = paste0("idx_syn_overlap_", pair_name, "_", mode_name)
      )
      out <- built$func(x)
      expect_equal(out, ref)
      results[[paste(pair_name, mode_name, sep = "_")]] <- out
    }
  }

  for (pair_name in names(selector_pairs)) {
    expect_equal(
      results[[paste(pair_name, "strict", sep = "_")]],
      results[[paste(pair_name, "non_strict", sep = "_")]]
    )
  }
})

test_that("no-loop selector normalization avoids scalar temp collisions on vector index returns", {
  f_neg_last <- function(x) {
    y <- x[-length(x)]
    y
  }
  f_empty_idx <- function(x) {
    idx <- integer(0)
    x[idx]
  }

  skip_if_no_mojo()
  built_neg_last <- mojor_build(f_neg_last, x = "f64[]", name = "idx_syn_no_loop_neg_last")
  built_empty <- mojor_build(f_empty_idx, x = "f64[]", name = "idx_syn_no_loop_empty_idx")
  x <- c(10.0, 20.0, 30.0)
  expect_equal(built_neg_last$func(x), c(10.0, 20.0))
  expect_equal(built_empty$func(x), numeric(0))
})
