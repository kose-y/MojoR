library(testthat)

test_that("expression-only supports full-vector dim() for matrix args", {  f <- function(x) {
    dim(x)
  }

  res <- mojor_transpile(f, x = "f64[,]", ir_only = TRUE)
  expect_true(isTRUE(res$is_expression_kernel))
  expect_true(isTRUE(res$is_vector_output))
  expect_equal(res$out_type, "i32[]")
  expect_identical(res$transpile_route$route, "loop")
  expect_identical(res$transpile_route$reason, "normalized_dim_metadata_vector_return")
})

test_that("expression-only supports full-vector dim() for fixed-rank ND args", {  f <- function(x) {
    dim(x)
  }

  res <- mojor_transpile(f, x = "f64[3d]", ir_only = TRUE)
  expect_true(isTRUE(res$is_expression_kernel))
  expect_true(isTRUE(res$is_vector_output))
  expect_equal(res$out_type, "i32[]")
  expect_identical(res$transpile_route$route, "loop")
  expect_identical(res$transpile_route$reason, "normalized_dim_metadata_vector_return")
})

test_that("expression-only supports full-vector dim() via no-loop let-binding", {
  f <- function(x) {
    d <- dim(x)
    d
  }

  res <- mojor_transpile(f, x = "f64[,]", ir_only = TRUE)
  expect_true(isTRUE(res$is_expression_kernel))
  expect_true(isTRUE(res$is_vector_output))
  expect_true(isTRUE(res$dim_builtin))
  expect_identical(res$dim_builtin_mode, "vector")
  expect_equal(res$out_type, "i32[]")
  expect_equal(as.integer(res$vector_len_const), 2L)
})

test_that("mojor_fn strict no-loop dim() let-binding compiles without fallback", {  skip_if_no_mojo()
  f <- function(x) {
    d <- dim(x)
    d
  }

  compiled <- mojor_fn(
    f,
    x = "f64[,]",
    object_mode = "off",
    ir_only = TRUE,
    cache = FALSE,
    load = TRUE
  )

  x <- matrix(as.double(seq_len(12L)), nrow = 3L, ncol = 4L)
  expect_equal(as.integer(compiled(x)), as.integer(dim(x)))
})

test_that("expression-only supports scalar dim(x)[k] metadata queries", {
  f_lit <- function(x) {
    dim(x)[1L]
  }
  f_arg <- function(x, k) {
    dim(x)[k]
  }

  lit_res <- mojor_transpile(f_lit, x = "f64[,]", ir_only = TRUE)
  expect_true(isTRUE(lit_res$is_expression_kernel))
  expect_identical(lit_res$out_type, "i32")
  expect_identical(lit_res$transpile_route$route, "loop")
  expect_identical(lit_res$transpile_route$reason, "normalized_dim_metadata_scalar_return")

  arg_res <- mojor_transpile(f_arg, x = "f64[,]", k = "i32", ir_only = TRUE)
  expect_true(isTRUE(arg_res$is_expression_kernel))
  expect_identical(arg_res$out_type, "i32")
  expect_identical(arg_res$transpile_route$route, "loop")
  expect_identical(arg_res$transpile_route$reason, "normalized_dim_metadata_scalar_return")

  arg_res_f64 <- mojor_transpile(f_arg, x = "f64[,]", k = "f64", ir_only = TRUE)
  expect_true(isTRUE(arg_res_f64$is_expression_kernel))
  expect_identical(arg_res_f64$out_type, "i32")
  expect_identical(arg_res_f64$transpile_route$route, "loop")
  expect_identical(arg_res_f64$transpile_route$reason, "normalized_dim_metadata_scalar_return")

  arg_res_lgl <- mojor_transpile(f_arg, x = "f64[,]", k = "lgl", ir_only = TRUE)
  expect_true(isTRUE(arg_res_lgl$is_expression_kernel))
  expect_identical(arg_res_lgl$out_type, "i32")
  expect_identical(arg_res_lgl$transpile_route$route, "loop")
  expect_identical(arg_res_lgl$transpile_route$reason, "normalized_dim_metadata_scalar_return")
})

test_that("expression-only dim(x)[k] wrappers accept scalar f64/lgl index args", {  skip_if_no_mojo()
  f <- function(x, k) {
    dim(x)[k]
  }

  built_f64 <- mojor_build(
    f,
    x = "f64[,]",
    k = "f64",
    name = "t_dim_index_expr_only_f64_arg",
    ir_only = TRUE,
    cache = FALSE,
    load = TRUE
  )

  built_lgl <- mojor_build(
    f,
    x = "f64[,]",
    k = "lgl",
    name = "t_dim_index_expr_only_lgl_arg",
    ir_only = TRUE,
    cache = FALSE,
    load = TRUE
  )

  x <- matrix(as.double(seq_len(6L)), nrow = 2L, ncol = 3L)
  expect_equal(built_f64$func(x, 1), nrow(x))
  expect_equal(built_f64$func(x, 2), ncol(x))
  expect_equal(built_lgl$func(x, TRUE), nrow(x))
  expect_equal(built_lgl$func(x, FALSE), 0L)
})

test_that("expression-only supports computed scalar dim index expressions", {
  f <- function(x, k) {
    dim(x)[k + 1L]
  }

  trans <- mojor_transpile(f, x = "f64[3d]", k = "i32", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_identical(trans$transpile_route$route, "loop")
  expect_identical(trans$transpile_route$reason, "normalized_dim_metadata_scalar_return")

  trans_f64 <- mojor_transpile(f, x = "f64[3d]", k = "f64", ir_only = TRUE)
  expect_true(isTRUE(trans_f64$is_expression_kernel))
  expect_identical(trans_f64$transpile_route$route, "loop")
  expect_identical(trans_f64$transpile_route$reason, "normalized_dim_metadata_scalar_return")

  built <- mojor_build(
    f,
    x = "f64[3d]",
    k = "i32",
    name = "t_dim_index_expr_only_build",
    ir_only = TRUE,
    cache = FALSE,
    load = TRUE
  )

  x <- array(as.double(seq_len(24L)), dim = c(2L, 3L, 4L))
  expect_equal(built$func(x, 1L), dim(x)[2L])
  expect_equal(built$func(x, 2L), dim(x)[3L])
  expect_equal(built$func(x, 3L), 0L)

  built_f64 <- mojor_build(
    f,
    x = "f64[3d]",
    k = "f64",
    name = "t_dim_index_expr_only_build_f64",
    ir_only = TRUE,
    cache = FALSE,
    load = TRUE
  )

  expect_equal(built_f64$func(x, 1), dim(x)[2L])
  expect_equal(built_f64$func(x, 2), dim(x)[3L])
  expect_equal(built_f64$func(x, 3), 0L)
})

test_that("expression-only supports scalar length(dim(x)) metadata queries", {
  f <- function(x) {
    length(dim(x))
  }

  res <- mojor_transpile(f, x = "f64[3d]", ir_only = TRUE)
  expect_true(isTRUE(res$is_expression_kernel))
  expect_identical(res$out_type, "i32")
  expect_identical(res$transpile_route$route, "loop")
  expect_identical(res$transpile_route$reason, "normalized_dim_metadata_scalar_return")
})

test_that("dim(x)[1] works in loop mode without dim helper", {  f <- function(x) {
    n <- dim(x)[1]
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- x[i]
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[,]", ir_only = TRUE)
  expect_true(!is.null(res$mojo))
  expect_match(res$mojo, "for i in range\\(")
})

test_that("loop mode accepts scalar dim index vars in range bounds", {
  f <- function(x, k) {
    out <- numeric(dim(x)[k])
    for (i in seq_len(dim(x)[k])) {
      out[i] <- 1.0
    }
    out
  }

  expect_error(mojor_transpile(f, x = "f64[,]", k = "i32", ir_only = TRUE), NA)
  expect_error(mojor_transpile(f, x = "f64[,]", k = "f64", ir_only = TRUE), NA)
  expect_error(mojor_transpile(f, x = "f64[,]", k = "lgl", ir_only = TRUE), NA)
})

test_that("dim(alias)[k] lowers through canonical matrix dim helper paths", {
  old_aliases <- .mojor_state$current_array_aliases
  on.exit(.mojor_state$current_array_aliases <- old_aliases, add = TRUE)
  .mojor_state$current_array_aliases <- list(mm = "m")

  args <- c("m")
  specs <- list(m = "f64[,]")

  c_nrow <- .mojor_dim_expr_to_c(quote(dim(mm)[1]), args, specs)$c_expr
  c_ncol <- .mojor_dim_expr_to_c(quote(dim(mm)[2]), args, specs)$c_expr
  mojo_nrow <- .mojor_dim_expr_to_mojo(quote(dim(mm)[1]), args, specs)$mojo_expr
  mojo_ncol <- .mojor_dim_expr_to_mojo(quote(dim(mm)[2]), args, specs)$mojo_expr

  expect_identical(c_nrow, "Rf_nrows(m)")
  expect_identical(c_ncol, "Rf_ncols(m)")
  expect_identical(mojo_nrow, .mojor_nrow_var_name("m"))
  expect_identical(mojo_ncol, .mojor_ncol_var_name("m"))

  args_dyn <- c("m", "k")
  specs_dyn <- list(m = "f64[,]", k = "i32")
  c_dyn <- .mojor_dim_expr_to_c(quote(dim(mm)[k]), args_dyn, specs_dyn)$c_expr
  mojo_dyn <- .mojor_dim_expr_to_mojo(quote(dim(mm)[k]), args_dyn, specs_dyn)$mojo_expr
  expect_true(grepl("k_val", c_dyn, fixed = TRUE))
  expect_true(grepl("nrow_m", mojo_dyn, fixed = TRUE) || grepl("ncol_m", mojo_dyn, fixed = TRUE))
})

test_that("float expression lowering supports nrow/ncol/dim(alias)[k]", {
  old_aliases <- .mojor_state$current_array_aliases
  on.exit(.mojor_state$current_array_aliases <- old_aliases, add = TRUE)
  .mojor_state$current_array_aliases <- list(mm = "m")

  args <- c("m")
  specs <- list(m = "f64[,]")

  expr <- quote((nrow(mm) + dim(mm)[1]) / ncol(mm))
  lowered <- .mojor_float_expr_to_c(expr, args, specs)

  expect_true(grepl("Rf_nrows\\(m\\)", lowered))
  expect_true(grepl("Rf_ncols\\(m\\)", lowered))
})

test_that("float expression lowering supports dim(alias)[k] for fixed-rank ND", {
  old_aliases <- .mojor_state$current_array_aliases
  on.exit(.mojor_state$current_array_aliases <- old_aliases, add = TRUE)
  .mojor_state$current_array_aliases <- list(aa = "a")

  args <- c("a")
  specs <- list(a = "f64[3d]")

  expr <- quote(dim(aa)[3] / 2)
  lowered <- .mojor_float_expr_to_c(expr, args, specs)

  expect_true(grepl("INTEGER\\(Rf_getAttrib\\(a, R_DimSymbol\\)\\)\\[2\\]", lowered))
})

test_that("type normalization preserves explicit ND rank for concrete arrays", {
  x3 <- array(as.double(seq_len(24L)), dim = c(2L, 3L, 4L))
  x2 <- matrix(as.double(seq_len(6L)), nrow = 2L, ncol = 3L)

  expect_identical(.mojor_normalize_type_spec(x3), "f64[3d]")
  expect_identical(.mojor_normalize_type_spec(x2), "f64[,]")
})
