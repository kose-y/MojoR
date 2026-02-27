library(testthat)

.with_expr_emit_shape_state <- function(code) {
  fields <- c(
    "current_len_var_map",
    "current_const_array_vars",
    "current_n_source_name",
    "current_nrow_var_map",
    "current_ncol_var_map",
    "current_dim_var_map",
    "current_ndim_var_map",
    "current_local_matrix_dims",
    "current_local_array_dims",
    "current_out_name",
    "current_out_nrow_var",
    "current_out_ncol_var",
    "current_out_dim_var",
    "current_out_ndim_var",
    "current_constructor_mode"
  )
  prev <- setNames(vector("list", length(fields)), fields)
  for (nm in fields) {
    prev[[nm]] <- if (exists(nm, envir = .mojor_state, inherits = FALSE)) {
      get(nm, envir = .mojor_state, inherits = FALSE)
    } else {
      NULL
    }
  }
  on.exit({
    for (nm in fields) {
      assign(nm, prev[[nm]], envir = .mojor_state)
    }
  }, add = TRUE)
  force(code)
}

test_that("set/match expression emit prefers matrix shape metadata over generic n_i", {
  .with_expr_emit_shape_state({
    .mojor_state$current_len_var_map <- list()
    .mojor_state$current_const_array_vars <- list()
    .mojor_state$current_n_source_name <- NULL
    .mojor_state$current_nrow_var_map <- list(x = "nrow_x_i", table = "nrow_t_i")
    .mojor_state$current_ncol_var_map <- list(x = "ncol_x_i", table = "ncol_t_i")
    .mojor_state$current_dim_var_map <- list()
    .mojor_state$current_ndim_var_map <- list()
    .mojor_state$current_local_matrix_dims <- list()
    .mojor_state$current_local_array_dims <- list()
    .mojor_state$current_out_name <- NULL
    .mojor_state$current_out_nrow_var <- NULL
    .mojor_state$current_out_ncol_var <- NULL
    .mojor_state$current_out_dim_var <- NULL
    .mojor_state$current_out_ndim_var <- NULL

    node <- list(
      kind = "match",
      x = list(kind = "var", name = "x"),
      table = list(kind = "var", name = "table")
    )

    emit <- .mojor_ir_expr_emit(node, type_env = list(x = "f64[,]", table = "f64[,]"))
    expect_match(emit, "\\(nrow_x_i \\* ncol_x_i\\)")
    expect_match(emit, "\\(nrow_t_i \\* ncol_t_i\\)")
    expect_false(grepl(", n_i,", emit, fixed = TRUE))
  })
})

test_that("quantile expression emit uses local static array dims before generic n_i", {
  .with_expr_emit_shape_state({
    .mojor_state$current_len_var_map <- list()
    .mojor_state$current_const_array_vars <- list()
    .mojor_state$current_n_source_name <- NULL
    .mojor_state$current_nrow_var_map <- list()
    .mojor_state$current_ncol_var_map <- list()
    .mojor_state$current_dim_var_map <- list()
    .mojor_state$current_ndim_var_map <- list()
    .mojor_state$current_local_matrix_dims <- list()
    .mojor_state$current_local_array_dims <- list(
      x = list(dim = c("n_x0", "n_x1"), type = "f64[2d]"),
      p = list(dim = c("n_p"), type = "f64[]")
    )
    .mojor_state$current_out_name <- NULL
    .mojor_state$current_out_nrow_var <- NULL
    .mojor_state$current_out_ncol_var <- NULL
    .mojor_state$current_out_dim_var <- NULL
    .mojor_state$current_out_ndim_var <- NULL

    node <- list(
      kind = "quantile",
      x = list(kind = "var", name = "x"),
      probs = list(kind = "var", name = "p"),
      na_rm = list(kind = "const", value = "False"),
      type = list(kind = "const", value = "7")
    )

    emit <- .mojor_ir_expr_emit(node, type_env = list(x = "f64[2d]", p = "f64[]"))
    expect_match(emit, "\\(n_x0 \\* n_x1\\)")
    expect_match(emit, ", n_p,")
    expect_false(grepl(", n_i,", emit, fixed = TRUE))
  })
})

test_that("constructor linearization uses matrix output nrow stride before generic n_i", {
  .with_expr_emit_shape_state({
    .mojor_state$current_len_var_map <- list()
    .mojor_state$current_const_array_vars <- list()
    .mojor_state$current_n_source_name <- NULL
    .mojor_state$current_nrow_var_map <- list()
    .mojor_state$current_ncol_var_map <- list()
    .mojor_state$current_dim_var_map <- list()
    .mojor_state$current_ndim_var_map <- list()
    .mojor_state$current_local_matrix_dims <- list()
    .mojor_state$current_local_array_dims <- list()
    .mojor_state$current_out_name <- "out"
    .mojor_state$current_out_nrow_var <- "nrow_out_i"
    .mojor_state$current_out_ncol_var <- "ncol_out_i"
    .mojor_state$current_out_dim_var <- NULL
    .mojor_state$current_out_ndim_var <- NULL
    .mojor_state$current_constructor_mode <- TRUE

    node <- list(
      kind = "c",
      parts = list(
        list(kind = "const", value = "1"),
        list(kind = "const", value = "2")
      ),
      "__mojor_slice_loop_vars" = c("__mojor_i1", "__mojor_i2")
    )

    emit <- .mojor_ir_expr_emit(
      node,
      loop_vars = c("__mojor_i1", "__mojor_i2"),
      type_env = list()
    )
    expect_true(grepl("nrow_out_i", emit, fixed = TRUE))
    expect_false(grepl(" * n_i", emit, fixed = TRUE))
  })
})
