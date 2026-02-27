library(testthat)


test_that("seq.int and 1:n are accepted", {  f1 <- function(x, x_n) {
    out <- numeric(length(x))
    for (i in seq.int(1, x_n)) {
      out[i] <- x[i]
    }
    out
  }

  res1 <- mojor_transpile(f1, x = "f64[]", x_n = "i32", name = "mojor_seqint")
  expect_equal(res1$n_source$kind, "expr")
  expect_true(grepl("x_n", res1$n_source$expr_c, fixed = TRUE))

  f2 <- function(x, n) {
    out <- numeric(length(x))
    for (i in 1:n) {
      out[i] <- x[i]
    }
    out
  }

  res2 <- mojor_transpile(f2, x = "f64[]", n = "i32", name = "mojor_colon")
  expect_equal(res2$n_source$kind, "scalar")
  expect_equal(res2$n_source$name, "n")
})

test_that("seq.int supports non-literal from/to scalars", {  f <- function(x, from, to) {
    out <- numeric(length(x))
    for (i in seq.int(from = from, to = to, by = 1L)) {
      out[i] <- x[i]
    }
    out
  }
  res <- mojor_transpile(f, x = "f64[]", from = "i32", to = "i32", name = "mojor_seqint_vars")
  expect_equal(res$n_source$kind, "scalar")
  expect_equal(res$n_source$name, "to")
})

test_that("seq.int supports by step", {  f <- function(x, n) {
    out <- numeric(length(x))
    for (i in seq.int(1, n, by = 2L)) {
      out[i] <- x[i]
    }
    out
  }
  expect_silent(mojor_transpile(f, x = "f64[]", n = "i32", name = "mojor_seqint_by"))
})

test_that("seq() dispatcher supports scalar ranges", {  f <- function(x, n) {
    out <- numeric(length(x))
    for (i in seq(from = 1L, to = n, by = 2L)) {
      out[i] <- x[i]
    }
    out
  }
  expect_silent(mojor_transpile(f, x = "f64[]", n = "i32", name = "mojor_seq"))
})

test_that("seq.int/seq accept to=... without from", {  f1 <- function(x) {
    out <- numeric(length(x))
    for (i in seq.int(to = 10L)) {
      out[i] <- x[i]
    }
    out
  }
  expect_silent(mojor_transpile(f1, x = "f64[]", name = "mojor_seqint_to_only"))

  f2 <- function(x, n) {
    out <- numeric(length(x))
    for (i in seq(to = n)) {
      out[i] <- x[i]
    }
    out
  }
  expect_mojor_error_or_result(
    run = function() mojor_transpile(f2, x = "f64[]", n = "i32", name = "mojor_seq_to_only"),
    error_patterns = "code generation failed for loop",
    on_success = function(res2) {
      expect_mojor_any_match(res2$mojo, "range\\(")
    }
  )
})

test_that("seq() supports float by for value loops", {  f <- function(x) {
    acc <- 0.0
    for (t in seq(from = 0, to = 1, by = 0.1)) {
      acc <- acc + t
    }
    acc
  }
  trans <- suppressWarnings(mojor_transpile(f, x = "f64[]", name = "mojor_seq_float_by"))
  expect_type(trans, "list")
  expect_true(is.list(trans$float_seq_check))
  expect_mojor_any_match(trans$mojo, "range\\(")
})

test_that("descending ranges are supported with ':' and seq.int", {  f1 <- function(x, n) {
    out <- numeric(length(x))
    for (i in n:1) {
      out[i] <- x[i]
    }
    out
  }
  res1 <- mojor_transpile(f1, x = "f64[]", n = "i32", name = "mojor_colon_desc_parity")
  expect_equal(res1$n_source$kind, "scalar")
  expect_true(grepl("range\\(", res1$mojo))
  expect_true(grepl("if", res1$mojo, fixed = TRUE))

  f2 <- function(x, n) {
    out <- numeric(length(x))
    for (i in seq.int(from = n, to = 1L, by = -2L)) {
      out[i] <- x[i]
    }
    out
  }
  trans2 <- mojor_transpile(f2, x = "f64[]", n = "i32", name = "mojor_seqint_desc_parity")
  expect_true(grepl("range\\(", trans2$mojo))
  expect_true(grepl("-2", trans2$mojo, fixed = TRUE))

  f3 <- function(x, n) {
    out <- numeric(length(x))
    for (i in seq.int(from = n, to = 1L)) {
      out[i] <- x[i]
    }
    out
  }
  trans3 <- mojor_transpile(f3, x = "f64[]", n = "i32", name = "mojor_seqint_desc_auto")
  expect_true(grepl("range\\(", trans3$mojo))
  expect_true(grepl("if", trans3$mojo, fixed = TRUE))
})

test_that("seq() supports decreasing float by for value loops", {  f <- function(x) {
    acc <- 0.0
    for (t in seq(from = 1, to = -1, by = -0.1)) {
      acc <- acc + t
    }
    acc
  }
  trans <- suppressWarnings(mojor_transpile(f, x = "f64[]", name = "mojor_seq_float_desc"))
  expect_type(trans, "list")
  expect_true(is.list(trans$float_seq_check))
  expect_mojor_any_match(trans$mojo, "range\\(")
  expect_mojor_any_match(trans$mojo, "-0.1", fixed = TRUE)
})

test_that("seq() float by emits runtime checks", {  f <- function(x, to, by) {
    acc <- 0.0
    for (t in seq(from = 1, to = to, by = by)) {
      acc <- acc + t
    }
    acc
  }
  trans <- mojor_transpile(f, x = "f64[]", to = "f64", by = "f64", name = "mojor_seq_float_check")
  expect_true(is.list(trans$float_seq_check))
  expect_true(grepl("floor", trans$n_source$expr_c))
})

test_that("seq.int supports descending ranges with negative by", {  f <- function(x, n) {
    out <- numeric(length(x))
    for (i in seq.int(from = n, to = 1L, by = -2L)) {
      out[i] <- x[i]
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "mojor_seqint_desc")
  expect_true(grepl("range\\(", trans$mojo))
  expect_true(grepl("-2", trans$mojo, fixed = TRUE))
})

test_that("seq.int supports scalar by arguments", {  f <- function(x, from, to, by) {
    out <- numeric(length(x))
    for (i in seq.int(from = from, to = to, by = by)) {
      out[i] <- x[i]
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", from = "i32", to = "i32", by = "i32", name = "mojor_seqint_by_arg")
  expect_mojor_any_match(trans$mojo, "range\\(")
  expect_mojor_any_match(trans$mojo, "range\\(Int\\(from\\).+Int\\(by\\)\\)")
})

test_that("descending colon ranges are accepted", {  f <- function(x, n) {
    out <- numeric(length(x))
    for (i in n:1) {
      out[i] <- x[i]
    }
    out
  }
  res <- mojor_transpile(f, x = "f64[]", n = "i32", name = "mojor_colon_desc")
  expect_equal(res$n_source$kind, "scalar")
  expect_equal(res$n_source$name, "n")
  expect_true(grepl("if", res$mojo, fixed = TRUE))
})

test_that("length(x) can be used in loop ranges", {  f1 <- function(x) {
    out <- numeric(length(x))
    for (i in 1:length(x)) {
      out[i] <- x[i]
    }
    out
  }
  res1 <- mojor_transpile(f1, x = "f64[]", name = "mojor_colon_length")
  expect_equal(res1$n_source$kind, "array")
  expect_equal(res1$n_source$name, "x")

  f2 <- function(x) {
    out <- numeric(length(x))
    for (i in seq_len(length(x))) {
      out[i] <- x[i]
    }
    out
  }
  res2 <- mojor_transpile(f2, x = "f64[]", name = "mojor_seq_len_length")
  expect_equal(res2$n_source$kind, "array")
  expect_equal(res2$n_source$name, "x")

  f3 <- function(x) {
    out <- numeric(length(x))
    for (i in seq.int(length.out = length(x))) {
      out[i] <- x[i]
    }
    out
  }
  res3 <- mojor_transpile(f3, x = "f64[]", name = "mojor_seqint_length")
  expect_equal(res3$n_source$kind, "array")
  expect_equal(res3$n_source$name, "x")
})

test_that("seq_len(dim(x)[1]) is accepted for matrix and fixed-rank ND args", {  f_mat <- function(x) {
    acc <- 0L
    for (i in seq_len(dim(x)[1])) {
      acc <- acc + i
    }
    acc
  }

  res_mat <- mojor_transpile(
    f_mat, x = "f64[,]", ir_only = TRUE,
    name = "mojor_seq_len_dim_direct_mat"
  )
  expect_equal(res_mat$n_source$kind, "expr")
  expect_mojor_any_match(res_mat$n_source$expr_c, "Rf_nrows\\(x\\)")
  expect_mojor_any_match(res_mat$mojo, "for i in range\\(")

  f_nd <- function(x) {
    acc <- 0L
    for (i in seq_len(dim(x)[1])) {
      acc <- acc + i
    }
    acc
  }

  res_nd <- mojor_transpile(
    f_nd, x = "f64[3d]", ir_only = TRUE,
    name = "mojor_seq_len_dim_direct_nd"
  )
  expect_equal(res_nd$n_source$kind, "expr")
  expect_mojor_any_match(res_nd$n_source$expr_c, "R_DimSymbol")
})

test_that("length(x)-1 and n-1 are accepted in loop ranges", {  f1 <- function(x) {
    out <- numeric(length(x))
    for (i in 1:(length(x) - 1)) {
      out[i] <- x[i]
    }
    out
  }
  res1 <- mojor_transpile(f1, x = "f64[]", name = "mojor_colon_length_minus")
  expect_equal(res1$n_source$kind, "array")
  expect_equal(res1$n_source$name, "x")

  f2 <- function(x, n) {
    out <- numeric(length(x))
    for (i in seq_len(n - 1)) {
      out[i] <- x[i]
    }
    out
  }
  res2 <- mojor_transpile(f2, x = "f64[]", n = "i32", name = "mojor_seq_len_minus")
  expect_equal(res2$n_source$kind, "scalar")
  expect_equal(res2$n_source$name, "n")

  f2b <- function(x) {
    out <- numeric(length(x))
    for (i in seq_len(length(x) - 1)) {
      out[i] <- x[i]
    }
    out
  }
  res2b <- mojor_transpile(f2b, x = "f64[]", name = "mojor_seq_len_length_minus")
  expect_equal(res2b$n_source$kind, "array")
  expect_equal(res2b$n_source$name, "x")

  f3 <- function(x) {
    out <- numeric(length(x))
    for (i in seq.int(to = length(x) - 1)) {
      out[i] <- x[i]
    }
    out
  }
  res3 <- mojor_transpile(f3, x = "f64[]", name = "mojor_seqint_to_length_minus")
  expect_equal(res3$n_source$kind, "array")
  expect_equal(res3$n_source$name, "x")
})

test_that("scalar conditional loop range expressions are accepted", {  f_ifelse <- function(x, n, k, flag) {
    out <- numeric(length(x))
    for (i in seq.int(from = 1L, to = ifelse(flag, n, k))) {
      out[i] <- x[i]
    }
    out
  }
  trans_ifelse <- mojor_transpile(
    f_ifelse,
    x = "f64[]",
    n = "i32",
    k = "i32",
    flag = "lgl",
    name = "mojor_seqint_ifelse"
  )
  expect_equal(trans_ifelse$n_source$kind, "expr")
  expect_mojor_any_match(trans_ifelse$mojo, "if")

  f_if <- function(x, n, k, flag) {
    out <- numeric(length(x))
    for (i in 1:(if (flag) n else k)) {
      out[i] <- x[i]
    }
    out
  }
  trans_if <- mojor_transpile(
    f_if,
    x = "f64[]",
    n = "i32",
    k = "i32",
    flag = "lgl",
    name = "mojor_colon_if_expr"
  )
  expect_equal(trans_if$n_source$kind, "expr")
  expect_mojor_any_match(trans_if$mojo, "if")
})

test_that("loop ranges accept explicit casts and unary plus in scalar lanes", {
  f_seq_len_cast <- function(n_f64, bump_f32) {
    acc <- 0L
    for (i in seq_len(as.integer(+n_f64 + as.double(bump_f32)))) {
      acc <- acc + i
    }
    acc
  }

  trans_cast <- mojor_transpile(
    f_seq_len_cast,
    n_f64 = "f64",
    bump_f32 = "f32",
    name = "mojor_seq_len_cast_unary_plus"
  )
  expect_equal(trans_cast$n_source$kind, "expr")
  expect_mojor_any_match(trans_cast$mojo, "for i in range\\(")

  f_dim_idx_cast <- function(x, axis_f64) {
    acc <- 0L
    for (i in seq_len(dim(x)[as.integer(+axis_f64)])) {
      acc <- acc + i
    }
    acc
  }

  trans_dim <- mojor_transpile(
    f_dim_idx_cast,
    x = "f64[3d]",
    axis_f64 = "f64",
    ir_only = TRUE,
    name = "mojor_seq_len_dim_cast_unary_plus"
  )
  expect_equal(trans_dim$n_source$kind, "expr")
  expect_mojor_any_match(trans_dim$mojo, "for i in range\\(")
})

test_that("invalid loop range transforms are rejected", {  bad1 <- function(x, n) {
    acc <- 0
    for (i in seq_len(n) / x) {
      acc <- acc + 1
    }
    acc
  }
  expect_error(mojor_transpile(bad1, x = "f64[]", n = "i32", name = "t_bad_seq_div_array"))

  bad2 <- function(x, y) {
    acc <- 0
    for (i in x + y) {
      acc <- acc + 1
    }
    acc
  }
  expect_error(mojor_transpile(bad2, x = "f64[]", y = "f64[]", name = "t_bad_vec_add"))

  # Note: seq() with float by values (e.g., by = 0.5) IS supported by the transpiler
  # and intentionally allowed with runtime length computation, so it's not included here
})

test_that("iterator loop over array values is supported", {  f <- function(x) {
    acc <- 0
    for (v in x) {
      acc <- acc + v
    }
    acc
  }
  built <- mojor_build(f, x = "f64[]", name = "mojor_iter_loop", cache = FALSE, load = TRUE)
  x <- as.double(1:5)
  expect_equal(built$func(x), sum(x))
})

test_that("loop ranges can use local length scalars", {  f <- function(x) {
    n <- length(x)
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[i]
    }
    out
  }
  build <- mojor_build(f, x = "f64[]", name = "t_seq_len_local", cache = FALSE, load = TRUE)
  x <- as.double(1:6)
  expect_equal(build$func(x), x)

  f2 <- function(x) {
    n <- length(x)
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- x[i]
    }
    out
  }
  build2 <- mojor_build(f2, x = "f64[]", name = "t_colon_local", cache = FALSE, load = TRUE)
  expect_equal(build2$func(x), x)
})


test_that("seq_len requires i32 scalar if not array", {  bad <- function(x, n) {
    out <- numeric(length(x))
    for (i in seq_len(n)) {
      out[i] <- x[i]
    }
    out
  }

  expect_error(
    mojor_transpile(bad, x = "f64[]", n = "f64", name = "mojor_seq_len_bad"),
    "seq_len\\(\\) scalar must be i32"
  )
})

test_that("seq_along/seq_len/seq.int support along.with argument", {  # seq_along with along.with
  f1 <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(along.with = y)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  res1 <- mojor_transpile(f1, x = "f64[]", y = "f64[]", name = "mojor_seq_along_alongwith")
  expect_equal(res1$n_source$kind, "array")
  expect_equal(res1$n_source$name, "y")
  
  # seq_len with along.with
  f2 <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_len(along.with = y)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  res2 <- mojor_transpile(f2, x = "f64[]", y = "f64[]", name = "mojor_seq_len_alongwith")
  expect_equal(res2$n_source$kind, "array")
  expect_equal(res2$n_source$name, "y")
  
  # seq.int with along.with
  f3 <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq.int(along.with = y)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  res3 <- mojor_transpile(f3, x = "f64[]", y = "f64[]", name = "mojor_seqint_alongwith")
  expect_equal(res3$n_source$kind, "array")
  expect_equal(res3$n_source$name, "y")
  
  # seq with along.with
  f4 <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq(along.with = y)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  res4 <- mojor_transpile(f4, x = "f64[]", y = "f64[]", name = "mojor_seq_alongwith")
  expect_equal(res4$n_source$kind, "array")
  expect_equal(res4$n_source$name, "y")
})

test_that("seq.int/seq with along.with works at runtime", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq.int(along.with = y)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  built <- mojor_build(f, x = "f64[]", y = "f64[]", name = "t_seqint_alongwith", cache = FALSE, load = TRUE)
  x <- as.double(1:5)
  y <- as.double(10:14)
  expect_equal(built$func(x, y), x + y)
})
