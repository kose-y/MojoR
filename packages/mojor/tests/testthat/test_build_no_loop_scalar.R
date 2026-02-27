library(testthat)


test_that("scalar-only any/all wrappers transpile with n_source none", {  any_scalar <- function(a, b) {
    any(lhs = a > 0, rhs = b > 0, na.rm = FALSE)
  }
  all_scalar <- function(a, b) {
    all(lhs = a > 0, rhs = b > 0, na.rm = FALSE)
  }
  any_zero <- function(a) {
    any(na.rm = FALSE)
  }
  all_false <- function(a) {
    all(FALSE, na.rm = FALSE)
  }

  tr_any_scalar <- mojor_transpile(any_scalar, a = "f64", b = "f64", name = "mojor_any_scalar_nnone", ir_only = TRUE)
  tr_all_scalar <- mojor_transpile(all_scalar, a = "f64", b = "f64", name = "mojor_all_scalar_nnone", ir_only = TRUE)
  tr_any_zero <- mojor_transpile(any_zero, a = "f64", name = "mojor_any_zero_nnone", ir_only = TRUE)
  tr_all_false <- mojor_transpile(all_false, a = "f64", name = "mojor_all_false_nnone", ir_only = TRUE)

  expect_equal(tr_any_scalar$n_source$kind, "none")
  expect_equal(tr_all_scalar$n_source$kind, "none")
  expect_equal(tr_any_zero$n_source$kind, "none")
  expect_equal(tr_all_false$n_source$kind, "none")
  expect_no_match(tr_any_scalar$mojo, "for i in range\\(")
  expect_no_match(tr_all_scalar$mojo, "for i in range\\(")
  expect_no_match(tr_any_zero$mojo, "for i in range\\(")
  expect_no_match(tr_all_false$mojo, "for i in range\\(")
})

test_that("mojor_build supports non-constant scalar-only any/all wrappers", {  skip_if_no_mojo()

  any_scalar <- function(a, b) {
    any(lhs = a > 0, rhs = b > 0, na.rm = FALSE)
  }
  all_scalar <- function(a, b) {
    all(lhs = a > 0, rhs = b > 0, na.rm = FALSE)
  }

  built_any <- mojor_build(any_scalar, a = "f64", b = "f64", name = "t_any_scalar_nnone_rt", cache = FALSE, load = TRUE)
  built_all <- mojor_build(all_scalar, a = "f64", b = "f64", name = "t_all_scalar_nnone_rt", cache = FALSE, load = TRUE)

  expect_equal(built_any$func(-1, 2), any_scalar(-1, 2))
  expect_equal(built_any$func(-1, -2), any_scalar(-1, -2))
  expect_equal(built_all$func(3, 4), all_scalar(3, 4))
  expect_equal(built_all$func(3, -4), all_scalar(3, -4))
})

test_that("mojor_build supports constant scalar-only any/all wrappers", {  skip_if_no_mojo()

  any_zero <- function(a) {
    any(na.rm = FALSE)
  }
  all_false <- function(a) {
    all(FALSE, na.rm = FALSE)
  }

  built_any_zero <- mojor_build(any_zero, a = "f64", name = "t_any_zero_nnone_rt", cache = FALSE, load = TRUE)
  built_all_false <- mojor_build(all_false, a = "f64", name = "t_all_false_nnone_rt", cache = FALSE, load = TRUE)

  expect_equal(built_any_zero$func(1.25), any_zero(1.25))
  expect_equal(built_any_zero$func(-2.5), any_zero(-2.5))
  expect_equal(built_all_false$func(1.25), all_false(1.25))
  expect_equal(built_all_false$func(-2.5), all_false(-2.5))
})
