library(testthat)

source("helper-mojo.R")

test_that("pair: any/all na.rm=TRUE transpile on strict path", {  f_any <- function(x) {
    any(x > 0, na.rm = TRUE)
  }
  f_all <- function(x) {
    all(x > 0, na.rm = TRUE)
  }

  t_any <- mojor_transpile(
    f_any,
    x = "f64[]",
    name = "pair_any_na_rm_ir",
    ir_only = TRUE,
    na_mode = "na_rm"
  )
  t_all <- mojor_transpile(
    f_all,
    x = "f64[]",
    name = "pair_all_na_rm_ir",
    ir_only = TRUE,
    na_mode = "na_rm"
  )

  expect_equal(t_any$out_type, "lgl")
  expect_equal(t_all$out_type, "lgl")
  expect_match(t_any$mojo, "for i in range", fixed = TRUE)
  expect_match(t_all$mojo, "for i in range", fixed = TRUE)
})

test_that("pair: any/all na.rm=TRUE runtime parity", {  skip_if_no_mojo()

  f_any <- function(x) {
    any(x > 0, na.rm = TRUE)
  }
  f_all <- function(x) {
    all(x > 0, na.rm = TRUE)
  }

  built_any <- mojor_build(
    f_any,
    x = "f64[]",
    name = "pair_any_na_rm_rt",
    cache = FALSE,
    load = TRUE,
    na_mode = "na_rm"
  )
  built_all <- mojor_build(
    f_all,
    x = "f64[]",
    name = "pair_all_na_rm_rt",
    cache = FALSE,
    load = TRUE,
    na_mode = "na_rm"
  )

  cases <- list(
    as.double(c(NaN, -2, -1)),
    as.double(c(NaN, 2, 3)),
    as.double(c(NaN, NaN)),
    as.double(c(-3, -2, -1)),
    as.double(c(1, 2, 3)),
    as.double(numeric(0))
  )

  for (x in cases) {
    expect_equal(built_any$func(x), f_any(x))
    expect_equal(built_all$func(x), f_all(x))
  }
})

