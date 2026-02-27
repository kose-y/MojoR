test_that("transpile: loop range supports named-order rep_len arguments", {  f <- function(x, n) {
    acc <- 0
    for (v in rep_len(length.out = n, x = c(1L, 2L))) {
      acc <- acc + v
    }
    acc + x[1] - x[1]
  }

  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_ctor_named_seq_transpile")
  expect_equal(trans$n_source$kind, "expr")
  expect_true(is.call(trans$n_source$iter_expr_ast))
  expect_equal(as.character(trans$n_source$iter_expr_ast[[1]]), "rep_len")
})

test_that("runtime: loop range supports named-order rep_len arguments", {  skip_if_no_mojo()

  f <- function(x, n) {
    acc <- 0
    for (v in rep_len(length.out = n, x = c(1L, 2L))) {
      acc <- acc + v
    }
    acc + x[1] - x[1]
  }

  built <- mojor_build(
    f,
    x = "f64[]",
    n = "i32",
    name = "t_ctor_named_seq_runtime",
    cache = FALSE,
    load = TRUE
  )

  n <- 7L
  x <- as.double(seq_len(n))
  expect_equal(built$func(x, n), f(x, n))
})

test_that("transpile: constructor-only and rhs rep_len accept named-order args", {  f_top <- function(x, n) {
    rep_len(length.out = n, x = x)
  }

  expect_silent(mojor_transpile(f_top, x = "f64[]", n = "i32", name = "t_ctor_named_top_transpile"))

  f_rhs <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- rep_len(length.out = n, x = x)[i]
    }
    out
  }

  expect_silent(mojor_transpile(f_rhs, x = "f64[]", n = "i32", name = "t_ctor_named_rhs_transpile"))
})

test_that("runtime: constructor-only and rhs rep_len accept named-order args", {  skip_if_no_mojo()

  f_top <- function(x, n) {
    rep_len(length.out = n, x = x)
  }

  built_top <- mojor_build(
    f_top,
    x = "f64[]",
    n = "i32",
    name = "t_ctor_named_top_runtime",
    cache = FALSE,
    load = TRUE
  )

  x <- as.double(c(1.5, 2.5, 3.5))
  n <- 8L
  expect_equal(built_top$func(x, n), f_top(x, n))

  f_rhs <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- rep_len(length.out = n, x = x)[i]
    }
    out
  }

  built_rhs <- mojor_build(
    f_rhs,
    x = "f64[]",
    n = "i32",
    name = "t_ctor_named_rhs_runtime",
    cache = FALSE,
    load = TRUE
  )

  expect_equal(built_rhs$func(x, n), f_rhs(x, n))
})
