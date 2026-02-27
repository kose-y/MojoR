library(testthat)


test_that("subset v1: loop ranges", {  f1 <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i]
    out
  }
  f2 <- function(x, n) {
    out <- numeric(length(x))
    for (i in seq_len(n)) out[i] <- x[i]
    out
  }
  f3 <- function(x, n) {
    out <- numeric(length(x))
    for (i in seq.int(1, n)) out[i] <- x[i]
    out
  }
  f4 <- function(x, n) {
    out <- numeric(length(x))
    for (i in 1:n) out[i] <- x[i]
    out
  }

  expect_silent(mojor_transpile(f1, x = "f64[]", name = "t_seq_along"))
  expect_silent(mojor_transpile(f2, x = "f64[]", n = "i32", name = "t_seq_len"))
  expect_silent(mojor_transpile(f3, x = "f64[]", n = "i32", name = "t_seq_int"))
  expect_silent(mojor_transpile(f4, x = "f64[]", n = "i32", name = "t_colon"))
})


test_that("subset v1: arithmetic and comparisons", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- (x[i] + y[i]) * 2 - (x[i] / y[i])
    }
    out
  }
  res <- mojor_transpile(f, x = "f64[]", y = "f64[]", name = "t_arith")
  expect_match(res$mojo, "+", fixed = TRUE)
  expect_match(res$mojo, "*", fixed = TRUE)
  expect_match(res$mojo, "/", fixed = TRUE)
})


test_that("subset v1: modulo ops", {  f <- function(x, y) {
    out <- integer(length(x))
    for (i in seq_along(x)) {
      out[i] <- (x[i] %/% y[i]) + (x[i] %% y[i])
    }
    out
  }
  res <- mojor_transpile(f, x = "i32[]", y = "i32[]", name = "t_mod")
  expect_match(res$mojo, "//", fixed = TRUE)
  expect_match(res$mojo, " % ", fixed = TRUE)
})


test_that("subset v1: casts", {  f <- function(x) {
    out <- integer(length(x))
    for (i in seq_along(x)) out[i] <- as.integer(x[i])
    out
  }
  g <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- as.double(x[i])
    out
  }
  h_f32 <- function(x) {
    out <- float::float32(length(x))
    for (i in seq_along(x)) out[i] <- as.single(x[i])
    out
  }
  h_lgl <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) out[i] <- as.logical(x[i])
    out
  }

  expect_match(mojor_transpile(f, x = "f64[]", name = "t_cast_i")$mojo, "Int32", fixed = TRUE)
  expect_match(mojor_transpile(g, x = "i32[]", name = "t_cast_f")$mojo, "Float64", fixed = TRUE)
  expect_match(mojor_transpile(h_f32, x = "f64[]", name = "t_cast_f32")$mojo, "Float32", fixed = TRUE)
  cast_l_mojo <- mojor_transpile(h_lgl, x = "f64[]", name = "t_cast_l")$mojo
  expect_true(grepl("_mojor_read_f64\\(x, Int\\(\\(i - 1\\)\\), Int\\(n_i\\)\\)", cast_l_mojo))
  expect_true(grepl("!= 0", cast_l_mojo, fixed = TRUE))
  expect_true(grepl("out[Int(", cast_l_mojo, fixed = TRUE))
})


test_that("subset v1: any/all wrappers", {  any_fn <- function(x) any(x > 0)
  all_fn <- function(x, y) all(x & y)
  res_any <- mojor_transpile(any_fn, x = "f64[]", name = "t_any")
  res_all <- mojor_transpile(all_fn, x = "lgl[]", y = "lgl[]", name = "t_all")
  expect_match(res_any$mojo, "break", fixed = TRUE)
  expect_match(res_all$mojo, "break", fixed = TRUE)
})

test_that("subset v1: multi-statement loop bodies", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i]
      out[i] <- out[i] + 1
    }
    out
  }
  res <- mojor_transpile(f, x = "f64[]", name = "t_multistmt")
  expect_true(grepl("out\\[.*\\] =", res$mojo))
})

test_that("subset v1: sequential loops", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i]
    }
    for (i in seq_along(x)) {
      out[i] <- out[i] + 1
    }
    out
  }
  res <- mojor_transpile(f, x = "f64[]", name = "t_two_loops")
  expect_match(res$mojo, "for i in range", fixed = TRUE)
})

test_that("subset v1: mixed ranges allowed for scalar outputs", {  f <- function(x, y) {
    acc <- 0
    for (i in seq_along(x)) {
      acc <- acc + x[i]
    }
    for (j in seq_along(y)) {
      acc <- acc + y[j]
    }
    acc
  }
  suppressWarnings(mojor_transpile(f, x = "f64[]", y = "f64[]", name = "t_scalar_mixed_ranges"))
})

test_that("subset v1: nested loops (same range source)", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      for (j in seq_along(x)) {
        out[i] <- out[i] + x[j]
      }
    }
    out
  }
  res <- mojor_transpile(f, x = "f64[]", name = "t_nested")
  expect_match(res$mojo, "for j in range", fixed = TRUE)
})

test_that("subset v1: is.na/is.nan/is.finite/is.infinite", {  f <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) {
      out[i] <- is.finite(x[i])
    }
    out
  }
  res <- mojor_transpile(f, x = "f64[]", name = "t_finite")
  expect_match(res$mojo, "_MOJOR_INF", fixed = TRUE)
})
