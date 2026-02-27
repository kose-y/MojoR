context("compiled subset data.frame column access")
test_that("mojor_transpile rewrites df column reads to hidden primitive args", {  old_opts <- options(
    mojor.tier8_complete = FALSE,
    mojor.tier9_force = TRUE
  )
  on.exit(options(old_opts), add = TRUE)

  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = FALSE)

  f <- function(df, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- df$val[i] + 1.0
    }
    out
  }

  hidden <- .mojor_df_hidden_arg_name("df", "val")
  trans <- mojor_transpile(
    f,
    df = "df",
    n = "i32",
    df_schema = list(df = c(val = "f64[]")),
    name = "t_tier9_df_transpile"
  )

  expect_true(isTRUE(trans$df_rewritten))
  expect_true(hidden %in% names(trans$types))
  expect_false("df" %in% names(trans$types))
  expect_true(grepl(hidden, trans$mojo, fixed = TRUE))
})

test_that("compiled subset accepts f32[] data.frame columns for rewrite/runtime extraction", {  old_opts <- options(
    mojor.tier8_complete = FALSE,
    mojor.tier9_force = TRUE
  )
  on.exit(options(old_opts), add = TRUE)

  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = FALSE)

  f <- function(df, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- df$val[i]
    }
    out
  }

  hidden <- .mojor_df_hidden_arg_name("df", "val")
  trans <- mojor_transpile(
    f,
    df = "df",
    n = "i32",
    df_schema = list(df = c(val = "f32[]")),
    name = "t_tier9_df_f32_transpile"
  )
  expect_true(isTRUE(trans$df_rewritten))
  expect_identical(trans$types[[hidden]], "f32[]")

  hidden_map <- list(df = c(val = hidden))
  out <- .mojor_df_prepare_compiled_args(
    resolved_args = list(df = data.frame(val = c(1, 2, 3)), n = 3L),
    arg_specs = list(df = "df", n = "i32"),
    df_schema = list(df = c(val = "f32[]")),
    hidden_map = hidden_map,
    context = "test"
  )
  expect_true(hidden %in% names(out))
  expect_type(out[[hidden]], "double")

  expect_error(
    .mojor_df_prepare_compiled_args(
      resolved_args = list(df = data.frame(val = c(TRUE, FALSE, TRUE)), n = 3L),
      arg_specs = list(df = "df", n = "i32"),
      df_schema = list(df = c(val = "f32[]")),
      hidden_map = hidden_map,
      context = "test"
    ),
    "must be numeric for spec f32\\[\\]"
  )
})

test_that("compiled subset accepts chr[] data.frame columns for rewrite/runtime extraction", {  old_opts <- options(
    mojor.tier8_complete = FALSE,
    mojor.tier9_force = TRUE
  )
  on.exit(options(old_opts), add = TRUE)

  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = FALSE)

  f <- function(df) nchar(df$txt)

  hidden <- .mojor_df_hidden_arg_name("df", "txt")
  trans <- mojor_transpile(
    f,
    df = "df",
    df_schema = list(df = c(txt = "chr[]")),
    name = "t_tier9_df_chr_transpile"
  )
  expect_true(isTRUE(trans$df_rewritten))
  expect_identical(trans$types[[hidden]], "chr[]")
  expect_true(isTRUE(trans$string_builtin))

  built <- mojor_build(
    f,
    df = "df",
    df_schema = list(df = c(txt = "chr[]")),
    name = "t_tier9_df_chr_build"
  )
  x <- data.frame(txt = c("a", "abcd", ""), stringsAsFactors = FALSE)
  expect_equal(unname(built$func(x)), unname(nchar(x$txt)))

  hidden_map <- list(df = c(txt = hidden))
  out <- .mojor_df_prepare_compiled_args(
    resolved_args = list(df = x),
    arg_specs = list(df = "df"),
    df_schema = list(df = c(txt = "chr[]")),
    hidden_map = hidden_map,
    context = "test"
  )
  expect_true(hidden %in% names(out))
  expect_type(out[[hidden]], "character")

  bad_chr_df <- data.frame(txt = factor(c("x", "y", "z")))
  expect_error(
    .mojor_df_prepare_compiled_args(
      resolved_args = list(df = bad_chr_df),
      arg_specs = list(df = "df"),
      df_schema = list(df = c(txt = "chr[]")),
      hidden_map = hidden_map,
      context = "test"
    ),
    "must be character for spec chr\\[\\]"
  )
})

test_that("compiled subset supports canonical data.frame bracket scalar reads", {  old_opts <- options(
    mojor.tier8_complete = FALSE,
    mojor.tier9_force = TRUE
  )
  on.exit(options(old_opts), add = TRUE)

  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = FALSE)

  f_dynamic <- function(df, col, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- df[[col]][i]
    }
    out
  }

  expect_error(
    mojor_transpile(
      f_dynamic,
      df = "df",
      col = "i32",
      n = "i32",
      df_schema = list(df = c(val = "f64[]")),
      name = "t_tier9_df_dynamic"
    ),
    "requires a literal string column name"
  )

  f_bracket_pos <- function(df, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- df[i, 1]
    }
    out
  }

  trans_pos <- mojor_transpile(
    f_bracket_pos,
    df = "df",
    n = "i32",
    df_schema = list(df = c(val = "f64[]")),
    name = "t_tier9_df_bracket_pos"
  )
  expect_true(isTRUE(trans_pos$df_rewritten))
  expect_true(grepl(.mojor_df_hidden_arg_name("df", "val"), trans_pos$mojo, fixed = TRUE))

  f_bracket_name <- function(df, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- df[i, "off"] + df[i, 1]
    }
    out
  }
  trans_name <- mojor_transpile(
    f_bracket_name,
    df = "df",
    n = "i32",
    df_schema = list(df = c(val = "f64[]", off = "f64[]")),
    name = "t_tier9_df_bracket_name"
  )
  expect_true(isTRUE(trans_name$df_rewritten))
  expect_true(grepl(.mojor_df_hidden_arg_name("df", "off"), trans_name$mojo, fixed = TRUE))
  expect_true(grepl(.mojor_df_hidden_arg_name("df", "val"), trans_name$mojo, fixed = TRUE))

  f_symbol_col <- function(df, col, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- df[i, col]
    }
    out
  }
  trans_symbol <- mojor_transpile(
    f_symbol_col,
    df = "df",
    col = "i32",
    n = "i32",
    df_schema = list(df = c(val = "f64[]", off = "f64[]")),
    name = "t_tier9_df_symbol_col"
  )
  expect_true(isTRUE(trans_symbol$df_rewritten))
  expect_identical(
    trans_symbol$types[[.mojor_df_dynamic_arg_name("df", "col")]],
    "f64[]"
  )

  built_symbol <- mojor_build(
    f_symbol_col,
    df = "df",
    col = "i32",
    n = "i32",
    df_schema = list(df = c(val = "f64[]", off = "f64[]")),
    name = "t_tier9_df_symbol_col_build"
  )
  df_num <- data.frame(val = runif(16), off = runif(16), stringsAsFactors = FALSE)
  expect_equal(built_symbol$func(df_num, 1L, 16L), f_symbol_col(df_num, 1L, 16L), tolerance = 1e-10)
  expect_equal(built_symbol$func(df_num, 2L, 16L), f_symbol_col(df_num, 2L, 16L), tolerance = 1e-10)
  expect_error(
    built_symbol$func(df_num, 3L, 16L),
    "dynamic selector argument 'col' is out of bounds"
  )

  expect_error(
    mojor_transpile(
      f_symbol_col,
      df = "df",
      col = "i32",
      n = "i32",
      df_schema = list(df = c(val = "f64[]", idx = "i32[]")),
      name = "t_tier9_df_symbol_col_hetero"
    ),
    "require homogeneous df_schema"
  )
})

test_that("compiled build executes df$col[i] via runtime extraction wrapper", {  skip_if_no_mojo()

  old_opts <- options(
    mojor.tier8_complete = FALSE,
    mojor.tier9_force = TRUE
  )
  on.exit(options(old_opts), add = TRUE)

  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = FALSE)

  f <- function(df, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- df$val[i] + df$off[i]
    }
    out
  }

  built <- mojor_build(
    f,
    df = "df",
    n = "i32",
    df_schema = list(df = c(val = "f64[]", off = "f64[]")),
    name = "t_tier9_df_build"
  )

  df <- data.frame(val = runif(24), off = runif(24))
  expect_equal(built$func(df, 24L), f(df, 24L), tolerance = 1e-10)
  expect_true(isTRUE(built$df_rewritten))
  expect_null(built$gpu_func)
  expect_null(built$gpu_func_raw)
})

test_that("compiled subset runtime extraction validates column specs", {  df_schema <- list(df = c(i = "i32[]", b = "lgl[]", f = "f32[]", ch = "chr[]"))
  hidden_map <- list(df = c(
    i = .mojor_df_hidden_arg_name("df", "i"),
    b = .mojor_df_hidden_arg_name("df", "b"),
    f = .mojor_df_hidden_arg_name("df", "f"),
    ch = .mojor_df_hidden_arg_name("df", "ch")
  ))

  ok_df <- data.frame(i = as.integer(c(1, 2, 3)), b = c(TRUE, FALSE, TRUE), f = c(1, 2, 3), ch = c("a", "b", "c"), stringsAsFactors = FALSE)
  out <- .mojor_df_prepare_compiled_args(
    resolved_args = list(df = ok_df, n = 3L),
    arg_specs = list(df = "df", n = "i32"),
    df_schema = df_schema,
    hidden_map = hidden_map,
    context = "test"
  )
  expect_true(hidden_map$df[["i"]] %in% names(out))
  expect_true(hidden_map$df[["b"]] %in% names(out))
  expect_true(hidden_map$df[["f"]] %in% names(out))
  expect_true(hidden_map$df[["ch"]] %in% names(out))

  bad_df <- data.frame(i = c(1, 2, 3), b = c(TRUE, FALSE, TRUE), f = c(1, 2, 3), ch = c("a", "b", "c"), stringsAsFactors = FALSE)
  expect_error(
    .mojor_df_prepare_compiled_args(
      resolved_args = list(df = bad_df, n = 3L),
      arg_specs = list(df = "df", n = "i32"),
      df_schema = df_schema,
      hidden_map = hidden_map,
      context = "test"
    ),
    "must be integer"
  )

  bad_f32_df <- data.frame(i = as.integer(c(1, 2, 3)), b = c(TRUE, FALSE, TRUE), f = c("x", "y", "z"), ch = c("a", "b", "c"), stringsAsFactors = FALSE)
  expect_error(
    .mojor_df_prepare_compiled_args(
      resolved_args = list(df = bad_f32_df, n = 3L),
      arg_specs = list(df = "df", n = "i32"),
      df_schema = df_schema,
      hidden_map = hidden_map,
      context = "test"
    ),
    "must be numeric for spec f32\\[\\]"
  )

  bad_chr_df <- data.frame(i = as.integer(c(1, 2, 3)), b = c(TRUE, FALSE, TRUE), f = c(1, 2, 3), ch = factor(c("x", "y", "z")))
  expect_error(
    .mojor_df_prepare_compiled_args(
      resolved_args = list(df = bad_chr_df, n = 3L),
      arg_specs = list(df = "df", n = "i32"),
      df_schema = df_schema,
      hidden_map = hidden_map,
      context = "test"
    ),
    "must be character for spec chr\\[\\]"
  )
})
