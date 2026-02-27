# apply(): end-to-end transpile/runtime coverage.

skip_if_no_mojo <- function() {
  if (!nzchar(Sys.which("mojo"))) {
    skip("Mojo not available")
  }
}

test_that("apply() pre-loop assignment transpiles through IR", {  skip_if_no_mojo()

  f <- function(A, nr) {
    tmp <- apply(A, 1, sum)
    out <- numeric(nr)
    for (i in 1:nr) {
      out[i] <- tmp[i]
    }
    out
  }

  tr <- mojor_transpile(f, A = "f64[,]", nr = "i32", ir_only = TRUE)
  expect_true(is.list(tr))
  expect_true(!is.null(tr$mojo))
  expect_match(tr$mojo, "var tmp = alloc\\[Float64\\]\\(Int\\(")
  expect_match(tr$mojo, "_mojor_read_f64\\(A,")
  expect_match(tr$mojo, "tmp\\[Int\\(")
})

test_that("apply() direct return no-loop transpiles through IR", {  skip_if_no_mojo()

  f <- function(A) {
    apply(A, 1, sum)
  }

  tr <- mojor_transpile(f, A = "f64[,]", ir_only = TRUE)
  expect_true(is.list(tr))
  expect_equal(tr$out_kind, "vector")
  expect_equal(tr$out_name, "__mojor_apply_out")
  expect_true(!is.null(tr$out_len_source))
  expect_equal(tr$out_len_source$kind, "expr")
  expect_match(deparse(tr$out_len_source$expr), "nrow\\(A\\)")
  expect_match(tr$mojo, "_mojor_read_f64\\(A,")
  expect_match(tr$mojo, "__mojor_apply_out\\[Int\\(")
})

test_that("apply() row margin sum matches R", {  skip_if_no_mojo()

  f <- function(A, nr) {
    tmp <- apply(A, 1, sum)
    out <- numeric(nr)
    for (i in 1:nr) {
      out[i] <- tmp[i]
    }
    out
  }

  built <- mojor_build(f, A = "f64[,]", nr = "i32", name = "t_apply_e2e_row_sum", cache = FALSE, load = TRUE)
  A <- matrix(as.double(1:12), nrow = 3, ncol = 4)
  expected <- apply(A, 1, sum)
  result <- built$func(A, nrow(A))
  expect_equal(result, expected)
})

test_that("apply() row margin sum with na.rm=TRUE matches R", {  skip_if_no_mojo()

  f <- function(A, nr) {
    tmp <- apply(A, 1, sum, na.rm = TRUE)
    out <- numeric(nr)
    for (i in 1:nr) {
      out[i] <- tmp[i]
    }
    out
  }

  built <- mojor_build(f, A = "f64[,]", nr = "i32", name = "t_apply_e2e_row_sum_na_rm", cache = FALSE, load = TRUE)
  A <- matrix(c(1, 2, NA, 4, 5, 6), nrow = 2, ncol = 3)
  expected <- apply(A, 1, sum, na.rm = TRUE)
  result <- built$func(A, nrow(A))
  expect_equal(result, expected)
})

test_that("apply() inline lambda row sum with internal na.rm matches R", {  skip_if_no_mojo()

  f <- function(A, nr) {
    tmp <- apply(A, 1, function(v) sum(v, na.rm = TRUE))
    out <- numeric(nr)
    for (i in 1:nr) {
      out[i] <- tmp[i]
    }
    out
  }

  built <- mojor_build(f, A = "f64[,]", nr = "i32", name = "t_apply_e2e_lambda_sum_na_rm", cache = FALSE, load = TRUE)
  A <- matrix(c(1, NA, 3, 4, 5, NA), nrow = 2, ncol = 3)
  expected <- apply(A, 1, function(v) sum(v, na.rm = TRUE))
  result <- built$func(A, nrow(A))
  expect_equal(result, expected)
})

test_that("apply() direct return no-loop matches R", {  skip_if_no_mojo()

  f <- function(A) {
    apply(A, 1, sum)
  }

  built <- mojor_build(f, A = "f64[,]", name = "t_apply_e2e_direct_row_sum", cache = FALSE, load = TRUE)
  A <- matrix(as.double(1:15), nrow = 3, ncol = 5)
  expected <- apply(A, 1, sum)
  result <- built$func(A)
  expect_equal(result, expected)
})

test_that("apply() alias return no-loop matches R", {  skip_if_no_mojo()

  f <- function(A) {
    out <- apply(A, 2, function(v) mean(v, na.rm = TRUE))
    out
  }

  built <- mojor_build(f, A = "f64[,]", name = "t_apply_e2e_alias_col_mean", cache = FALSE, load = TRUE)
  A <- matrix(c(1, 2, NA, 4, 5, 6, 7, NA, 9), nrow = 3, ncol = 3)
  expected <- apply(A, 2, function(v) mean(v, na.rm = TRUE))
  result <- built$func(A)
  expect_equal(result, expected)
})
