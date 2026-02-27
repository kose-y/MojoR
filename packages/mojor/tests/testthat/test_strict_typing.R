library(testthat)

test_that("type inference handles metadata query/predicate node types", {  type_env <- list(x = "f64[]")

  node_query <- .mojor_ir_type_query("typeof", .mojor_ir_var("x"))
  node_pred <- .mojor_ir_type_predicate("is.vector", .mojor_ir_var("x"))

  expect_identical(.mojor_ir_infer_type(node_query, type_env), "chr[]")
  expect_identical(.mojor_ir_infer_type(node_pred, type_env), "bool")
})

test_that("strict type checker rejects unknown compiled subset result types on assignment", {  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = TRUE)

  rhs <- .mojor_ir_df_col_read(.mojor_ir_var("df"), .mojor_ir_var("col"))
  stmt <- .mojor_ir_assign(.mojor_ir_var("y"), rhs)

  expect_error(
    .mojor_ir_type_check_stmt(stmt, list(y = "f64[]", df = "df", col = "chr[]")),
    "compiled subset node 'df_col_read' type contract failed in assignment RHS: IR verify \\[df_col_read\\]: result type must be .*\\(got 'unknown'\\)"
  )

  mojor_options(ir_only = FALSE)
  expect_silent(.mojor_ir_type_check_stmt(
    stmt,
    list(y = "f64[]", df = "df", col = "chr[]")
  ))
})

test_that("strict type checker accepts known compiled subset regex result types", {  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = TRUE)

  rhs <- .mojor_ir_regex_grepl(
    pattern = .mojor_ir_var("pattern"),
    x = .mojor_ir_var("x")
  )
  stmt <- .mojor_ir_assign(.mojor_ir_var("out"), rhs)

  expect_silent(.mojor_ir_type_check_stmt(
    stmt,
    list(out = "lgl[]", pattern = "chr", x = "chr[]")
  ))
})

test_that("strict verifier enforces compiled subset regex argument typing", {  node <- .mojor_ir_regex_grep(
    pattern = .mojor_ir_var("pattern"),
    x = .mojor_ir_var("x"),
    value = FALSE
  )

  expect_silent(.mojor_ir_verify(
    node,
    ctx = list(ir_only = TRUE, type_env = list(pattern = "chr", x = "chr[]"))
  ))

  expect_error(
    .mojor_ir_verify(
      node,
      ctx = list(ir_only = TRUE, type_env = list(pattern = "chr", x = "f64[]"))
    ),
    "IR verify \\[regex_grep\\]: x must have type 'chr\\[\\]' in strict mode"
  )
})

test_that("strict verifier rejects compiled subset regex vector pattern vars", {  node <- .mojor_ir_regex_grepl(
    pattern = .mojor_ir_var("pattern"),
    x = .mojor_ir_var("x")
  )

  expect_error(
    .mojor_ir_verify(
      node,
      ctx = list(ir_only = TRUE, type_env = list(pattern = "chr[]", x = "chr[]"))
    ),
    "IR verify \\[regex_grepl\\]: pattern must have type 'chr' in strict mode"
  )
})

test_that("strict verifier enforces compiled subset row/col matrix typing", {  node_row <- .mojor_ir_row_matrix(.mojor_ir_var("x"))
  node_col <- .mojor_ir_col_matrix(.mojor_ir_var("x"))

  expect_silent(.mojor_ir_verify(
    node_row,
    ctx = list(ir_only = TRUE, type_env = list(x = "f64[2d]"))
  ))
  expect_silent(.mojor_ir_verify(
    node_col,
    ctx = list(ir_only = TRUE, type_env = list(x = "i32[,]"))
  ))

  expect_error(
    .mojor_ir_verify(
      node_row,
      ctx = list(ir_only = TRUE, type_env = list(x = "f64[]"))
    ),
    "IR verify \\[row_matrix\\]: x must have type"
  )
  expect_error(
    .mojor_ir_verify(
      node_row,
      ctx = list(ir_only = TRUE, type_env = list(x = "chr[,]"))
    ),
    "IR verify \\[row_matrix\\]: x must have type"
  )
})

test_that("strict verifier enforces compiled subset expand_grid vector typing", {  node <- .mojor_ir_expand_grid(list(.mojor_ir_var("x"), .mojor_ir_var("y")))

  expect_silent(.mojor_ir_verify(
    node,
    ctx = list(ir_only = TRUE, type_env = list(x = "f64[]", y = "lgl[]"))
  ))

  expect_error(
    .mojor_ir_verify(
      node,
      ctx = list(ir_only = TRUE, type_env = list(x = "f64[]", y = "chr[]"))
    ),
    "IR verify \\[expand_grid\\]: args\\[\\[2\\]\\] must have type 'f64\\[\\]' or 'i32\\[\\]' or 'lgl\\[\\]' in strict mode"
  )
})

test_that("strict type checker enforces compiled subset regex argument typing on assignment RHS", {  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = TRUE)

  rhs <- .mojor_ir_regex_grep(
    pattern = .mojor_ir_var("pattern"),
    x = .mojor_ir_var("x"),
    value = FALSE
  )
  stmt <- .mojor_ir_assign(.mojor_ir_var("out"), rhs)

  expect_error(
    .mojor_ir_type_check_stmt(
      stmt,
      list(out = "i32[]", pattern = "chr", x = "f64[]")
    ),
    "compiled subset node 'regex_grep' type contract failed in assignment RHS: IR verify \\[regex_grep\\]: x must have type 'chr\\[\\]' in strict mode"
  )
})

test_that("strict type checker accepts compiled subset weighted sampling prob typing", {  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = TRUE)

  rhs <- .mojor_ir_sample_int(
    n = .mojor_ir_var("n"),
    size = .mojor_ir_var("size"),
    replace = TRUE,
    prob = .mojor_ir_var("prob")
  )
  stmt <- .mojor_ir_assign(.mojor_ir_var("out"), rhs)

  expect_silent(
    .mojor_ir_type_check_stmt(
      stmt,
      list(out = "i32[]", n = "i32", size = "i32", prob = "f64[]")
    )
  )

  rhs_bad_replace <- .mojor_ir_sample_int(
    n = .mojor_ir_var("n"),
    size = .mojor_ir_var("size"),
    replace = FALSE,
    prob = .mojor_ir_var("prob")
  )
  stmt_bad_replace <- .mojor_ir_assign(.mojor_ir_var("out"), rhs_bad_replace)
  expect_silent(
    .mojor_ir_type_check_stmt(
      stmt_bad_replace,
      list(out = "i32[]", n = "i32", size = "i32", prob = "f64[]")
    )
  )
})

test_that("strict type checker enforces compiled subset return argument typing", {  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = TRUE)

  stmt <- .mojor_ir_return(
    .mojor_ir_regex_sub(
      pattern = .mojor_ir_var("pattern"),
      replacement = .mojor_ir_var("replacement"),
      x = .mojor_ir_var("x")
    )
  )

  expect_error(
    .mojor_ir_type_check_stmt(
      stmt,
      list(pattern = "chr", replacement = "f64[]", x = "chr[]")
    ),
    "compiled subset node 'regex_sub' type contract failed in return: IR verify \\[regex_sub\\]: replacement must have type 'chr' in strict mode"
  )
})

test_that("IR expression builder parses only strict compiled subset regex/table forms", {  node_grepl <- .mojor_ir_expr_build(quote(grepl(pattern, x)))
  node_sub <- .mojor_ir_expr_build(quote(sub("a", replacement, x)))
  node_row <- .mojor_ir_expr_build(quote(row(x)))
  node_expand <- .mojor_ir_expr_build(quote(expand.grid(x, y)))

  expect_identical(node_grepl$kind, "regex_grepl")
  expect_identical(node_sub$kind, "regex_sub")
  expect_identical(node_row$kind, "row_matrix")
  expect_identical(node_expand$kind, "expand_grid")
  expect_null(.mojor_ir_expr_build(quote(grep(pattern, x, value = TRUE))))
  expect_null(.mojor_ir_expr_build(quote(expand.grid(a = x, b = y))))
  expect_null(.mojor_ir_expr_build(quote(row(x + 1L))))
})

test_that("compat type checker keeps compiled subset regex mismatches non-fatal", {  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = FALSE)

  stmt <- .mojor_ir_assign(
    .mojor_ir_var("out"),
    .mojor_ir_regex_grep(
      pattern = .mojor_ir_var("pattern"),
      x = .mojor_ir_var("x"),
      value = FALSE
    )
  )

  expect_silent(.mojor_ir_type_check_stmt(
    stmt,
    list(out = "i32[]", pattern = "chr", x = "f64[]")
  ))
})
