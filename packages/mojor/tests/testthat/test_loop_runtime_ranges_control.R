library(testthat)

loop_rt_build <- function(f, name, type_sig) {
  build_args <- c(list(f), type_sig, list(name = name, cache = FALSE, load = TRUE))
  do.call(mojor_build, build_args)
}

loop_rt_expect_match <- function(f, name, type_sig, call_args, tolerance = NULL) {
  built <- loop_rt_build(f, name = name, type_sig = type_sig)
  actual <- do.call(built$func, call_args)
  expected <- do.call(f, call_args)
  if (is.null(tolerance)) {
    expect_equal(actual, expected)
  } else {
    expect_equal(actual, expected, tolerance = tolerance)
  }
}

test_that("loop range scalar expressions match R", {  skip_if_no_mojo()
  f_seq_len <- function(x, n, k) {
    out <- numeric(length(x))
    for (i in seq_len(n + k)) {
      out[i] <- x[i]
    }
    out
  }
  f_colon <- function(x, n, k) {
    out <- numeric(length(x))
    for (j in 2:(n + k)) {
      out[j] <- x[j] + j
    }
    out
  }
  f_seqint <- function(x, n, k, step_i) {
    out <- numeric(length(x))
    for (m in seq.int(from = 1L, to = n + k, by = step_i)) {
      out[m] <- out[m] + 1
    }
    out
  }
  n <- 6L
  k <- 2L
  step_i <- 2L
  x <- as.double(1:(n + k))
  loop_rt_expect_match(f_seq_len, "t_loop_range_expr_seq_len_rt", list(x = "f64[]", n = "i32", k = "i32"), list(x = x, n = n, k = k))
  loop_rt_expect_match(f_colon, "t_loop_range_expr_colon_rt", list(x = "f64[]", n = "i32", k = "i32"), list(x = x, n = n, k = k))
  loop_rt_expect_match(f_seqint, "t_loop_range_expr_seqint_rt", list(x = "f64[]", n = "i32", k = "i32", step_i = "i32"), list(x = x, n = n, k = k, step_i = step_i))
})

test_that("loop range scalar expressions support min/max and %/%", {  skip_if_no_mojo()
  f_min <- function(x, n, k) {
    out <- numeric(min(n, k))
    for (i in seq_len(min(n, k))) {
      out[i] <- x[i]
    }
    out
  }
  f_div <- function(x, n) {
    out <- numeric(n %/% 2L)
    for (i in seq_len(n %/% 2L)) {
      out[i] <- x[i]
    }
    out
  }
  n <- 10L
  k <- 6L
  x_min <- as.double(1:min(n, k))
  x_div <- as.double(1:(n %/% 2L))
  loop_rt_expect_match(f_min, "t_loop_range_expr_min_rt", list(x = "f64[]", n = "i32", k = "i32"), list(x = x_min, n = n, k = k))
  loop_rt_expect_match(f_div, "t_loop_range_expr_div_rt", list(x = "f64[]", n = "i32"), list(x = x_div, n = n))
})

test_that("loop range conditional scalar expressions match R", {  skip_if_no_mojo()
  f_ifelse <- function(x, n, k, flag) {
    out <- numeric(length(x))
    for (i in seq.int(from = 1L, to = ifelse(flag, n, k))) {
      out[i] <- x[i]
    }
    out
  }
  f_if <- function(x, n, k, flag) {
    out <- numeric(length(x))
    for (i in 1:(if (flag) n else k)) {
      out[i] <- x[i] * 2
    }
    out
  }

  x <- as.double(1:10)
  n <- 6L
  k <- 4L

  loop_rt_expect_match(
    f_ifelse, "t_loop_range_expr_ifelse_true_rt",
    list(x = "f64[]", n = "i32", k = "i32", flag = "lgl"),
    list(x = x, n = n, k = k, flag = TRUE)
  )
  loop_rt_expect_match(
    f_ifelse, "t_loop_range_expr_ifelse_false_rt",
    list(x = "f64[]", n = "i32", k = "i32", flag = "lgl"),
    list(x = x, n = n, k = k, flag = FALSE)
  )

  loop_rt_expect_match(
    f_if, "t_loop_range_expr_if_true_rt",
    list(x = "f64[]", n = "i32", k = "i32", flag = "lgl"),
    list(x = x, n = n, k = k, flag = TRUE)
  )
  loop_rt_expect_match(
    f_if, "t_loop_range_expr_if_false_rt",
    list(x = "f64[]", n = "i32", k = "i32", flag = "lgl"),
    list(x = x, n = n, k = k, flag = FALSE)
  )
})

test_that("loop range casted scalar expressions with unary plus match R", {  skip_if_no_mojo()
  f_cast <- function(n_f64, bump_f64) {
    acc <- 0L
    for (i in seq_len(as.integer(+n_f64 + as.double(bump_f64)))) {
      acc <- acc + i
    }
    acc
  }

  n_f64 <- 5.0
  bump_f64 <- 2.0
  loop_rt_expect_match(
    f_cast,
    "t_loop_range_expr_cast_unary_plus_rt",
    list(n_f64 = "f64", bump_f64 = "f64"),
    list(n_f64 = n_f64, bump_f64 = bump_f64)
  )
})

test_that("affine loop ranges match R", {  skip_if_no_mojo()
  f_offset <- function(x, n) {
    acc <- 0L
    for (i in seq_len(n) + 1L) {
      acc <- acc + i
    }
    acc
  }
  f_scale <- function(x, n) {
    acc <- 0L
    for (i in 2L * seq_len(n)) {
      acc <- acc + i
    }
    acc
  }
  f_rev <- function(x, n) {
    acc <- 0L
    for (i in rev(seq_len(n))) {
      acc <- acc * 10L + i
    }
    acc
  }
  f_div <- function(x, n) {
    acc <- 0
    for (i in seq_len(n) / 2) {
      acc <- acc + i
    }
    acc + x[1] - x[1]
  }
  f_div_lhs <- function(x, n) {
    acc <- 0
    for (i in 10 / seq_len(n)) {
      acc <- acc + i
    }
    acc + x[1] - x[1]
  }
  n <- 4L
  x_dummy <- as.double(1:n)
  loop_rt_expect_match(f_offset, "t_loop_range_expr_offset_rt", list(x = "f64[]", n = "i32"), list(x = x_dummy, n = n))
  loop_rt_expect_match(f_scale, "t_loop_range_expr_scale_rt", list(x = "f64[]", n = "i32"), list(x = x_dummy, n = n))
  loop_rt_expect_match(f_rev, "t_loop_range_expr_rev_rt", list(x = "f64[]", n = "i32"), list(x = x_dummy, n = n))
  loop_rt_expect_match(f_div, "t_loop_range_expr_div2_rt", list(x = "f64[]", n = "i32"), list(x = x_dummy, n = n))
  loop_rt_expect_match(f_div_lhs, "t_loop_range_expr_div_lhs_rt", list(x = "f64[]", n = "i32"), list(x = x_dummy, n = n))
})

test_that("zero-length and descending ranges match R", {  skip_if_no_mojo()
  f_zero <- function(x, n) {
    out <- integer(n)
    for (i in seq_len(n)) {
      out[i] <- i
    }
    out
  }
  f_desc <- function(x, n) {
    out <- integer(n)
    for (i in n:1) {
      out[i] <- i
    }
    out
  }
  x_zero <- numeric(0)
  x_desc <- as.double(1:5)
  loop_rt_expect_match(f_zero, "t_loop_zero_len_rt", list(x = "f64[]", n = "i32"), list(x = x_zero, n = 0L))
  loop_rt_expect_match(f_desc, "t_loop_desc_seqint_rt", list(x = "f64[]", n = "i32"), list(x = x_desc, n = 5L))
})

test_that("descending seq.int and float seq() value loops match R", {  skip_if_no_mojo()
  f_seqint <- function(x, n) {
    acc <- 0L
    for (i in seq.int(from = n, to = 1L, by = -2L)) {
      acc <- acc + i
    }
    acc
  }
  f_seqint_auto <- function(x, n) {
    acc <- 0L
    for (i in seq.int(from = n, to = 1L)) {
      acc <- acc + i
    }
    acc
  }
  f_seqint_auto_vec <- function(x, n) {
    out <- numeric(length(x))
    for (i in seq.int(from = n, to = 1L)) {
      out[i] <- x[i]
    }
    out
  }
  f_seqint_auto_range <- function(from_val, to_val) {
    acc <- 0L
    for (i in seq.int(from = from_val, to = to_val)) {
      acc <- acc + i
    }
    acc
  }
  f_float <- function(x, from_val, to_val, step) {
    acc <- 0.0
    for (t in seq(from = from_val, to = to_val, by = step)) {
      acc <- acc + t
    }
    acc
  }
  x_dummy_seqint <- as.double(seq_len(9))
  loop_rt_expect_match(f_seqint, "t_loop_desc_seqint_step_rt", list(x = "f64[]", n = "i32"), list(x = x_dummy_seqint, n = 9L))
  loop_rt_expect_match(f_seqint_auto, "t_loop_desc_seqint_auto_rt", list(x = "f64[]", n = "i32"), list(x = x_dummy_seqint, n = 9L))
  loop_rt_expect_match(f_seqint_auto_vec, "t_loop_desc_seqint_auto_vec_rt", list(x = "f64[]", n = "i32"), list(x = x_dummy_seqint, n = 9L))
  loop_rt_expect_match(f_seqint_auto_range, "t_loop_desc_seqint_auto_range_rt", list(from_val = "i32", to_val = "i32"), list(from_val = 9L, to_val = 4L))
  loop_rt_expect_match(f_seqint_auto_range, "t_loop_desc_seqint_auto_range_rt", list(from_val = "i32", to_val = "i32"), list(from_val = 2L, to_val = 5L))
  loop_rt_expect_match(
    f_float,
    "t_loop_desc_float_seq_rt",
    list(x = "f64[]", from_val = "f64", to_val = "f64", step = "f64"),
    list(x = as.double(seq_len(21)), from_val = 1.0, to_val = -1.0, step = -0.1),
    tolerance = 1e-9
  )
})

test_that("nested repeat loops with break/next match R", {  skip_if_no_mojo()
  f <- function(x) {
    out <- numeric(length(x))
    i <- 1L
    repeat {
      if (i > length(x)) break
      j <- 1L
      acc <- 0
      repeat {
        if (j > i) break
        if (x[j] < 0) {
          j <- j + 1L
          next
        }
        acc <- acc + x[j]
        j <- j + 1L
      }
      out[i] <- acc
      i <- i + 1L
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", name = "t_loop_repeat_nested_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(1, -2, 3, 4, -5, 6))
  expect_equal(built$func(x), f(x))
})


# ============================================================================
# ifelse() runtime correctness tests (paired with test_transpile_ifelse.R)
# ============================================================================

test_that("ifelse produces correct results at runtime", {  skip_if_no_mojo()
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- ifelse(x[i] > 0, x[i], -x[i])
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", name = "t_ifelse_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(-3, -1, 0, 2, 5))
  expect_equal(built$func(x), f(x))
})

test_that("ifelse with logical output produces correct results", {  skip_if_no_mojo()
  f <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) {
      out[i] <- ifelse(x[i] > 0, TRUE, FALSE)
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", name = "t_ifelse_lgl_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(-3, -1, 0, 2, 5))
  expect_equal(built$func(x), f(x))
})

test_that("nested ifelse produces correct results", {  skip_if_no_mojo()
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- ifelse(x[i] > 0, ifelse(y[i] > 0, x[i] + y[i], x[i]), -x[i])
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", y = "f64[]", name = "t_ifelse_nested_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(-3, 1, -2, 4))
  y <- as.double(c(2, -1, 3, 5))
  expect_equal(built$func(x, y), f(x, y))
})

test_that("ifelse in if condition produces correct results", {  skip_if_no_mojo()
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      if (ifelse(x[i] > 0, TRUE, FALSE)) {
        out[i] <- x[i] * 2
      } else {
        out[i] <- -x[i]
      }
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", name = "t_ifelse_ifcond_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(-3, -1, 0, 2, 5))
  expect_equal(built$func(x), f(x))
})

test_that("multi-statement if/else bodies produce correct results", {  skip_if_no_mojo()
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      if (x[i] > 0) {
        out[i] <- x[i]
        out[i] <- out[i] + 1
      } else {
        out[i] <- -x[i]
        out[i] <- out[i] - 1
      }
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", name = "t_ifelse_multistmt_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(-3, -1, 0, 2, 5))
  expect_equal(built$func(x), f(x))
})

test_that("else-if chains with multi-statement bodies produce correct results", {  skip_if_no_mojo()
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      if (x[i] > 1) {
        out[i] <- x[i]
        out[i] <- out[i] + 1
      } else if (x[i] > 0) {
        out[i] <- x[i] * 2
        out[i] <- out[i] + 2
      } else {
        out[i] <- -x[i]
        out[i] <- out[i] - 1
      }
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", name = "t_ifelse_chain_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(-2, 0.5, 1.5, 3))
  expect_equal(built$func(x), f(x))
})


# ============================================================================
# any() / all() runtime correctness tests (paired with test_transpile_any_all.R)
# ============================================================================

test_that("any/all named selector index arguments match R runtime", {  skip_if_no_mojo()
  any_named <- function(x, idx) {
    any(x[idx = idx] > 0)
  }
  all_named <- function(x, idx) {
    all(x[idx = idx] > 0)
  }

  built_any <- mojor_build(any_named, x = "f64[]", idx = "i32[]", name = "t_any_named_sel_rt", cache = FALSE, load = TRUE)
  built_all <- mojor_build(all_named, x = "f64[]", idx = "i32[]", name = "t_all_named_sel_rt", cache = FALSE, load = TRUE)

  x <- as.double(c(-2, 5, 3, -1))
  idx <- as.integer(c(2, 3))
  expect_equal(built_any$func(x, idx), any_named(x, idx))
  expect_equal(built_all$func(x, idx), all_named(x, idx))
})


# ============================================================================
# migrated from legacy monolithic runtime test file (range/control coverage)
# ============================================================================
test_that("dynamic colon ranges follow R direction at runtime", {  skip_if_no_mojo()
  f <- function(x, n) {
    acc <- 0
    for (i in n:1) {
      acc <- acc * 10 + i
    }
    acc + x[1] - x[1]
  }
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "t_loop_colon_dyn_rt", cache = FALSE, load = TRUE)
  n <- 3L
  x <- as.double(1:n)
  expect_equal(built$func(x, n), f(x, n))
})

test_that("colon ranges with scalar endpoints match R (a:b)", {  skip_if_no_mojo()
  f <- function(x, a, b) {
    out <- numeric(length(x))
    for (i in a:b) {
      out[i] <- x[i] + i
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", a = "i32", b = "i32", name = "t_loop_colon_names_rt", cache = FALSE, load = TRUE)
  x <- as.double(1:6)
  expect_equal(built$func(x, 2L, 6L), f(x, 2L, 6L))
  expect_equal(built$func(x, 6L, 2L), f(x, 6L, 2L))
})

test_that("colon ranges with local scalar aliases match R", {  skip_if_no_mojo()
  f_arg <- function(x, n) {
    m <- n
    out <- numeric(length(x))
    for (i in m:1) {
      out[i] <- x[i]
    }
    out
  }
  f_len <- function(x) {
    n <- length(x)
    out <- numeric(length(x))
    for (i in n:1) {
      out[i] <- x[i]
    }
    out
  }
  built_arg <- mojor_build(f_arg, x = "f64[]", n = "i32", name = "t_loop_colon_local_arg", cache = FALSE, load = TRUE)
  built_len <- mojor_build(f_len, x = "f64[]", name = "t_loop_colon_local_len", cache = FALSE, load = TRUE)
  x <- as.double(1:6)
  expect_equal(built_arg$func(x, 6L), f_arg(x, 6L))
  expect_equal(built_len$func(x), f_len(x))
})

test_that("constructor sequence loop ranges match R (c/rep/rep_len)", {  skip_if_no_mojo()
  f1 <- function(x) {
    acc <- 0
    for (i in c(1L, 3L, 2L)) {
      acc <- acc + i
    }
    acc + x[1] - x[1]
  }
  b1 <- mojor_build(f1, x = "f64[]", name = "t_loop_seq_ctor_c", cache = FALSE, load = TRUE)
  x <- as.double(1:3)
  expect_equal(b1$func(x), f1(x))

  f2 <- function(x, n) {
    acc <- 0
    for (i in rep(2L, times = n)) {
      acc <- acc + i
    }
    acc + x[1] - x[1]
  }
  b2 <- mojor_build(f2, x = "f64[]", n = "i32", name = "t_loop_seq_ctor_rep", cache = FALSE, load = TRUE)
  n <- 4L
  x <- as.double(1:n)
  expect_equal(b2$func(x, n), f2(x, n))

  f3 <- function(x, n) {
    acc <- 0
    for (i in rep_len(c(1L, 2L), length.out = n)) {
      acc <- acc + i
    }
    acc + x[1] - x[1]
  }
  b3 <- mojor_build(f3, x = "f64[]", n = "i32", name = "t_loop_seq_ctor_rep_len", cache = FALSE, load = TRUE)
  n <- 5L
  x <- as.double(1:n)
  expect_equal(b3$func(x, n), f3(x, n))
})

test_that("constructor RHS recycling matches R", {  skip_if_no_mojo()
  f <- function(n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- c(1, 2)
    }
    out
  }
  built <- mojor_build(f, n = "i32", name = "t_ctor_rhs_recycle_rt", cache = FALSE, load = TRUE)
  expect_equal(built$func(5L), f(5L))
})

test_that("constructor RHS rep_len matches R on non-multiple lengths", {  skip_if_no_mojo()
  f <- function(n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- rep_len(c(1L, 2L, 3L), length.out = n)[i]
    }
    out
  }
  built <- mojor_build(f, n = "i32", name = "t_ctor_rhs_rep_len_rt", cache = FALSE, load = TRUE)
  expect_equal(built$func(7L), f(7L))
})

test_that("constructor RHS c() supports mixed types and arrays", {  skip_if_no_mojo()
  f <- function(x) {
    n <- length(x) + 2L
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- c(1L, x, 2.5)[i]
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", name = "t_ctor_rhs_c_mixed_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(10, 20, 30))
  expect_equal(built$func(x), f(x))
})

test_that("constructor RHS rep supports times/each with length.out truncation", {  skip_if_no_mojo()
  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- rep(x, times = 2L, each = 2L, length.out = n)[i]
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "t_ctor_rhs_rep_len_out_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(1, 2, 3))
  expect_equal(built$func(x, 7L), f(x, 7L))
})

test_that("loop ranges can use c() with array values", {  skip_if_no_mojo()
  f <- function(x) {
    acc <- 0
    for (v in c(x, 2)) {
      acc <- acc + v
    }
    acc
  }
  built <- mojor_build(f, x = "f64[]", name = "t_loop_seq_ctor_array", cache = FALSE, load = TRUE)
  x <- as.double(1:4)
  expect_equal(built$func(x), f(x))
})

test_that("constructor sequences support rev and affine transforms", {  skip_if_no_mojo()
  f1 <- function(x) {
    acc <- 0
    for (v in rev(c(1L, 2L, 3L))) {
      acc <- acc * 10 + v
    }
    acc + x[1] - x[1]
  }
  b1 <- mojor_build(f1, x = "f64[]", name = "t_loop_seq_ctor_rev", cache = FALSE, load = TRUE)
  x <- as.double(1:3)
  expect_equal(b1$func(x), f1(x))

  f2 <- function(x) {
    acc <- 0
    for (v in c(1L, 2L) + 1L) {
      acc <- acc + v
    }
    acc + x[1] - x[1]
  }
  b2 <- mojor_build(f2, x = "f64[]", name = "t_loop_seq_ctor_plus", cache = FALSE, load = TRUE)
  x2 <- as.double(1:2)
  expect_equal(b2$func(x2), f2(x2))

  f3 <- function(x) {
    acc <- 0
    for (v in 2L * c(1L, 3L)) {
      acc <- acc + v
    }
    acc + x[1] - x[1]
  }
  b3 <- mojor_build(f3, x = "f64[]", name = "t_loop_seq_ctor_mul", cache = FALSE, load = TRUE)
  expect_equal(b3$func(x2), f3(x2))
})

test_that("constructor sequences support nested constructors and division transforms", {  skip_if_no_mojo()
  f <- function(x, n) {
    acc <- 0
    for (v in c(rep_len(c(2L, 4L), length.out = n), 8L) / 2) {
      acc <- acc + v
    }
    acc + x[1] - x[1]
  }
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "t_loop_seq_ctor_nested_div", cache = FALSE, load = TRUE)
  n <- 4L
  x <- as.double(1:(n + 1L))
  expect_equal(built$func(x, n), f(x, n))
})

test_that("nested while loops match R", {  skip_if_no_mojo()
  f <- function(x) {
    out <- numeric(length(x))
    i <- 1L
    while (i <= length(x)) {
      j <- 1L
      acc <- 0
      while (j <= length(x)) {
        acc <- acc + x[j]
        j <- j + 1L
      }
      out[i] <- acc
      i <- i + 1L
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", name = "t_loop_while_nested_rt", cache = FALSE, load = TRUE)
  x <- as.double(1:5)
  expect_equal(built$func(x), f(x))
})

test_that("while condition can use length() of non-primary array", {  skip_if_no_mojo()
  f <- function(x, y) {
    out <- numeric(length(x))
    i <- 1L
    while (i <= length(y)) {  # y is non-primary (different from x)
      out[i] <- x[i] + y[i]
      i <- i + 1L
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", y = "f64[]", name = "t_while_nonprimary_len_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(10, 20, 30, 40, 50))
  y <- as.double(c(1, 2, 3, 4, 5))
  expect_equal(built$func(x, y), f(x, y))
})

test_that("break/next semantics in while loops match R", {  skip_if_no_mojo()
  f <- function(x) {
    out <- numeric(length(x))
    i <- 1L
    while (i <= length(x)) {
      if (x[i] < 0) {
        i <- i + 1L
        next
      }
      if (x[i] == 0) break
      out[i] <- x[i] * 2
      i <- i + 1L
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", name = "t_loop_while_break_next_rt", cache = FALSE, load = TRUE)
  x <- as.double(c(1, -1, 2, 0, 3))
  expect_equal(built$func(x), f(x))
})

test_that("mixed for + while nesting matches R", {  skip_if_no_mojo()
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      j <- 1L
      acc <- 0
      while (j <= i) {
        acc <- acc + x[j]
        j <- j + 1L
      }
      out[i] <- acc
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", name = "t_loop_for_while_rt", cache = FALSE, load = TRUE)
  x <- as.double(1:6)
  expect_equal(built$func(x), f(x))
})

test_that("seq.int by-step + descending colon loops match R", {  skip_if_no_mojo()
  f <- function(x, n) {
    out <- numeric(length(x))
    for (i in seq.int(1L, n, by = 2L)) {
      out[i] <- i
    }
    for (j in n:1) {
      out[j] <- out[j] + j
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", n = "i32", name = "t_loop_seqint_colon_rt", cache = FALSE, load = TRUE)
  n <- 6L
  x <- as.double(1:n)
  expect_equal(built$func(x, n), f(x, n))
})

test_that("seq(by) and seq.int(length.out) loops match R", {  skip_if_no_mojo()
  f_seq_by <- function(x, n) {
    out <- numeric(n)
    for (i in seq(1, n, by = 2L)) {
      out[i] <- x[i] + i
    }
    out
  }
  f_seqint_len <- function(x, n) {
    out <- numeric(n)
    for (i in seq.int(length.out = n)) {
      out[i] <- x[i]
    }
    out
  }
  n <- 8L
  x <- as.double(1:n)
  loop_rt_expect_match(f_seq_by, "t_loop_seq_by_int_rt", list(x = "f64[]", n = "i32"), list(x = x, n = n))
  loop_rt_expect_match(f_seqint_len, "t_loop_seqint_len_rt", list(x = "f64[]", n = "i32"), list(x = x, n = n))
})
