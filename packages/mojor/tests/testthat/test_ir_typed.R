library(testthat)

# =============================================================================
# Type Promotion Tests
# =============================================================================

test_that("type promotion handles same types", {  expect_equal(.mojor_type_promote("i32", "i32"), "i32")
  expect_equal(.mojor_type_promote("f64", "f64"), "f64")
  expect_equal(.mojor_type_promote("f32", "f32"), "f32")
  expect_equal(.mojor_type_promote("bool", "bool"), "i32")  # bool promotes to i32
})

test_that("type promotion follows lattice", {  # i32 < f32 < f64
  expect_equal(.mojor_type_promote("i32", "f64"), "f64")
  expect_equal(.mojor_type_promote("f64", "i32"), "f64")
  expect_equal(.mojor_type_promote("i32", "f32"), "f32")
  expect_equal(.mojor_type_promote("f32", "i32"), "f32")
  expect_equal(.mojor_type_promote("f32", "f64"), "f64")
  expect_equal(.mojor_type_promote("f64", "f32"), "f64")
})

test_that("type promotion handles bool", {  expect_equal(.mojor_type_promote("bool", "i32"), "i32")
  expect_equal(.mojor_type_promote("i32", "bool"), "i32")
  expect_equal(.mojor_type_promote("bool", "f64"), "f64")
  expect_equal(.mojor_type_promote("f64", "bool"), "f64")
})

test_that("type promotion handles unknown", {  expect_equal(.mojor_type_promote("unknown", "f64"), "f64")
  expect_equal(.mojor_type_promote("f64", "unknown"), "f64")
  expect_equal(.mojor_type_promote("unknown", "unknown"), "unknown")
})

# =============================================================================
# Type Inference Tests
# =============================================================================

test_that("type inference on constants", {  type_env <- list()

  # Integer constant
  node <- .mojor_ir_const("42")
  expect_equal(.mojor_ir_infer_type(node, type_env), "i32")

  # Float constant
  node <- .mojor_ir_const("3.14")
  expect_equal(.mojor_ir_infer_type(node, type_env), "f64")

  # Special values
  node <- .mojor_ir_const("inf")
  expect_equal(.mojor_ir_infer_type(node, type_env), "f64")

  node <- .mojor_ir_const("nan")
  expect_equal(.mojor_ir_infer_type(node, type_env), "f64")
})

test_that("type inference on variables", {  type_env <- list(x = "f64[]", y = "i32[]", z = "f32[]")

  # Array type
  node <- .mojor_ir_var("x")
  expect_equal(.mojor_ir_infer_type(node, type_env), "f64[]")

  node <- .mojor_ir_var("y")
  expect_equal(.mojor_ir_infer_type(node, type_env), "i32[]")

  node <- .mojor_ir_var("z")
  expect_equal(.mojor_ir_infer_type(node, type_env), "f32[]")

  # Unknown variable
  node <- .mojor_ir_var("unknown")
  expect_equal(.mojor_ir_infer_type(node, type_env), "unknown")
})

test_that("type inference on index", {  type_env <- list(x = "f64[]", y = "i32[]", i = "i32")

  # x[i] should be f64
  node <- .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
  expect_equal(.mojor_ir_infer_type(node, type_env), "f64")

  # y[i] should be i32
  node <- .mojor_ir_index(.mojor_ir_var("y"), list(.mojor_ir_var("i")))
  expect_equal(.mojor_ir_infer_type(node, type_env), "i32")
})

test_that("type inference on binop", {  type_env <- list(x = "f64[]", y = "i32[]", i = "i32")

  # f64 + f64 = f64
  lhs <- .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
  rhs <- .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
  node <- .mojor_ir_binop("+", lhs, rhs)
  expect_equal(.mojor_ir_infer_type(node, type_env), "f64")

  # i32 + i32 = i32
  lhs <- .mojor_ir_index(.mojor_ir_var("y"), list(.mojor_ir_var("i")))
  rhs <- .mojor_ir_index(.mojor_ir_var("y"), list(.mojor_ir_var("i")))
  node <- .mojor_ir_binop("+", lhs, rhs)
  expect_equal(.mojor_ir_infer_type(node, type_env), "i32")

  # f64 + i32 = f64 (promotion)
  lhs <- .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
  rhs <- .mojor_ir_index(.mojor_ir_var("y"), list(.mojor_ir_var("i")))
  node <- .mojor_ir_binop("+", lhs, rhs)
  expect_equal(.mojor_ir_infer_type(node, type_env), "f64")
})

test_that("type inference on comparison", {  type_env <- list(x = "f64[]", i = "i32")

  # x[i] > 0 should be bool
  lhs <- .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
  rhs <- .mojor_ir_const("0")
  node <- .mojor_ir_binop(">", lhs, rhs)
  expect_equal(.mojor_ir_infer_type(node, type_env), "bool")
})

test_that("type inference on cast", {  type_env <- list()

  # Cast i32 to f64
  node <- .mojor_ir_cast("f64", .mojor_ir_const("42"))
  expect_equal(.mojor_ir_infer_type(node, type_env), "f64")

  # Cast f64 to i32
  node <- .mojor_ir_cast("i32", .mojor_ir_const("3.14"))
  expect_equal(.mojor_ir_infer_type(node, type_env), "i32")
})

test_that("type inference on cov/cor descriptors", {  # vector lanes -> scalar f64
  vec_env <- list(x = "f64[]", y = "f64[]")
  expect_equal(.mojor_ir_infer_type(.mojor_ir_cov(.mojor_ir_var("x")), vec_env), "f64")
  expect_equal(.mojor_ir_infer_type(.mojor_ir_cov(.mojor_ir_var("x"), .mojor_ir_var("y")), vec_env), "f64")
  expect_equal(.mojor_ir_infer_type(.mojor_ir_cor(.mojor_ir_var("x")), vec_env), "f64")
  expect_equal(.mojor_ir_infer_type(.mojor_ir_cor(.mojor_ir_var("x"), .mojor_ir_var("y")), vec_env), "f64")

  # matrix lanes -> matrix f64 with representation preserved when possible
  mat_env <- list(x = "f32[,]", y = "f32[,]")
  expect_equal(.mojor_ir_infer_type(.mojor_ir_cov(.mojor_ir_var("x")), mat_env), "f64[,]")
  expect_equal(.mojor_ir_infer_type(.mojor_ir_cov(.mojor_ir_var("x"), .mojor_ir_var("y")), mat_env), "f64[,]")
  expect_equal(.mojor_ir_infer_type(.mojor_ir_cor(.mojor_ir_var("x")), mat_env), "f64[,]")
  expect_equal(.mojor_ir_infer_type(.mojor_ir_cor(.mojor_ir_var("x"), .mojor_ir_var("y")), mat_env), "f64[,]")

  # mixed rank / mixed dtype stay unknown
  mixed_rank_env <- list(x = "f64[]", y = "f64[,]")
  mixed_dtype_env <- list(x = "f32[]", y = "f64[]")
  expect_equal(.mojor_ir_infer_type(.mojor_ir_cov(.mojor_ir_var("x"), .mojor_ir_var("y")), mixed_rank_env), "unknown")
  expect_equal(.mojor_ir_infer_type(.mojor_ir_cor(.mojor_ir_var("x"), .mojor_ir_var("y")), mixed_rank_env), "unknown")
  expect_equal(.mojor_ir_infer_type(.mojor_ir_cov(.mojor_ir_var("x"), .mojor_ir_var("y")), mixed_dtype_env), "unknown")
  expect_equal(.mojor_ir_infer_type(.mojor_ir_cor(.mojor_ir_var("x"), .mojor_ir_var("y")), mixed_dtype_env), "unknown")
})

test_that("type inference on dim metadata descriptor", {  mat_env <- list(x = "f64[,]", y = "i32[3d]")
  vec_env <- list(x = "f64[]")
  scalar_env <- list(x = "f64")

  expect_equal(.mojor_ir_infer_type(.mojor_ir_dim(.mojor_ir_var("x")), mat_env), "i32[]")
  expect_equal(.mojor_ir_infer_type(.mojor_ir_dim(.mojor_ir_var("y")), mat_env), "i32[]")
  expect_equal(.mojor_ir_infer_type(.mojor_ir_dim(.mojor_ir_var("x")), vec_env), "unknown")
  expect_equal(.mojor_ir_infer_type(.mojor_ir_dim(.mojor_ir_var("x")), scalar_env), "unknown")
})

test_that("type inference on vexpr descriptor", {  env <- list(
    n = "i32",
    x = "f64",
    y = "i32",
    b = "bool",
    l = "lgl"
  )

  expect_equal(.mojor_ir_infer_type(.mojor_ir_vexpr(.mojor_ir_var("n"), .mojor_ir_var("x")), env), "f64[]")
  expect_equal(.mojor_ir_infer_type(.mojor_ir_vexpr(.mojor_ir_var("n"), .mojor_ir_var("y")), env), "i32[]")
  expect_equal(.mojor_ir_infer_type(.mojor_ir_vexpr(.mojor_ir_var("n"), .mojor_ir_var("b")), env), "bool[]")
  expect_equal(.mojor_ir_infer_type(.mojor_ir_vexpr(.mojor_ir_var("n"), .mojor_ir_var("l")), env), "lgl[]")

  expect_equal(.mojor_ir_infer_type(.mojor_ir_vexpr(.mojor_ir_const("3"), .mojor_ir_const("1.0")), list()), "f64[]")
  expect_equal(.mojor_ir_infer_type(.mojor_ir_vexpr(.mojor_ir_var("x"), .mojor_ir_var("y")), env), "unknown")
  expect_equal(.mojor_ir_infer_type(.mojor_ir_vexpr(.mojor_ir_var("n"), .mojor_ir_var("arr")), list(n = "i32", arr = "f64[]")), "unknown")
})

# =============================================================================
# Type Annotation Tests
# =============================================================================

test_that("type annotation adds type field", {  type_env <- list(x = "f64[]", i = "i32")

  # Annotate a constant
  node <- .mojor_ir_const("42")
  annotated <- .mojor_ir_annotate_type(node, type_env)
  expect_equal(annotated$type, "i32")

  # Annotate a variable
  node <- .mojor_ir_var("x")
  annotated <- .mojor_ir_annotate_type(node, type_env)
  expect_equal(annotated$type, "f64[]")
})

test_that("type annotation is recursive", {  type_env <- list(x = "f64[]", y = "i32[]", i = "i32")

  # x[i] + y[i]
  lhs <- .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
  rhs <- .mojor_ir_index(.mojor_ir_var("y"), list(.mojor_ir_var("i")))
  node <- .mojor_ir_binop("+", lhs, rhs)

  annotated <- .mojor_ir_annotate_type(node, type_env)

  # Root should be f64 (promotion)
  expect_equal(annotated$type, "f64")

  # LHS should be f64
  expect_equal(annotated$lhs$type, "f64")
  expect_equal(annotated$lhs$base$type, "f64[]")
  expect_equal(annotated$lhs$indices[[1]]$type, "i32")

  # RHS should be i32
  expect_equal(annotated$rhs$type, "i32")
  expect_equal(annotated$rhs$base$type, "i32[]")
  expect_equal(annotated$rhs$indices[[1]]$type, "i32")
})

# =============================================================================
# Cast Insertion Tests
# =============================================================================

test_that("cast insertion for binop promotion", {  type_env <- list(x = "f64[]", y = "i32[]", i = "i32")

  # x[i] + y[i] (f64 + i32 should insert cast on y[i])
  lhs <- .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
  rhs <- .mojor_ir_index(.mojor_ir_var("y"), list(.mojor_ir_var("i")))
  node <- .mojor_ir_binop("+", lhs, rhs)

  casted <- .mojor_ir_insert_casts(node, type_env)

  # Root should be binop
  expect_equal(casted$kind, "binop")
  expect_equal(casted$op, "+")

  # LHS should be unchanged
  expect_equal(casted$lhs$kind, "index")

  # RHS should be wrapped in cast
  expect_equal(casted$rhs$kind, "cast")
  expect_equal(casted$rhs$to, "f64")
  expect_equal(casted$rhs$expr$kind, "index")
})

test_that("cast insertion for comparison with different types", {  type_env <- list(x = "f64[]", y = "i32[]", i = "i32")

  # x[i] > y[i] (f64 > i32 should insert cast on y[i])
  lhs <- .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
  rhs <- .mojor_ir_index(.mojor_ir_var("y"), list(.mojor_ir_var("i")))
  node <- .mojor_ir_binop(">", lhs, rhs)

  casted <- .mojor_ir_insert_casts(node, type_env)

  # Root should be comparison
  expect_equal(casted$kind, "binop")
  expect_equal(casted$op, ">")

  # RHS should be wrapped in cast
  expect_equal(casted$rhs$kind, "cast")
  expect_equal(casted$rhs$to, "f64")
})

test_that("cast insertion for logical operators", {  type_env <- list(x = "f64[]", y = "i32[]", i = "i32")

  # (x[i] > 0) && (y[i] > 0) should work without casts (both sides are bool)
  lhs <- .mojor_ir_binop(">",
                         .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i"))),
                         .mojor_ir_const("0"))
  rhs <- .mojor_ir_binop(">",
                         .mojor_ir_index(.mojor_ir_var("y"), list(.mojor_ir_var("i"))),
                         .mojor_ir_const("0"))
  node <- .mojor_ir_binop("&&", lhs, rhs)

  casted <- .mojor_ir_insert_casts(node, type_env)

  # Root should be logical operator
  expect_equal(casted$kind, "binop")
  expect_equal(casted$op, "&&")

  # Both sides should be comparisons (no casts needed)
  expect_equal(casted$lhs$kind, "binop")
  expect_equal(casted$rhs$kind, "binop")
})

# =============================================================================
# Type Check Stmt Tests
# =============================================================================

test_that("type check handles simple assignment", {  type_env <- list(out = "f64[]", i = "i32", x = "f64[]")

  # out[i] <- x[i]
  lhs <- .mojor_ir_index(.mojor_ir_var("out"), list(.mojor_ir_var("i")))
  rhs <- .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
  stmt <- .mojor_ir_assign(lhs, rhs)

  checked <- .mojor_ir_type_check_stmt(stmt, type_env)

  # Should be unchanged (types match)
  expect_equal(checked$kind, "assign")
  expect_equal(checked$rhs$kind, "index")
})

test_that("type check inserts cast in assignment", {  type_env <- list(out = "f64[]", i = "i32", x = "i32[]")

  # out[i] <- x[i] (f64 <- i32 should insert cast)
  lhs <- .mojor_ir_index(.mojor_ir_var("out"), list(.mojor_ir_var("i")))
  rhs <- .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
  stmt <- .mojor_ir_assign(lhs, rhs)

  checked <- .mojor_ir_type_check_stmt(stmt, type_env)

  # RHS should be wrapped in cast
  expect_equal(checked$rhs$kind, "cast")
  expect_equal(checked$rhs$to, "f64")
  expect_equal(checked$rhs$expr$kind, "index")
})

test_that("type check handles if statement", {  type_env <- list(out = "f64[]", i = "i32", x = "f64[]")

  # if (x[i] > 0) { out[i] <- x[i] }
  cond <- .mojor_ir_binop(">",
                          .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i"))),
                          .mojor_ir_const("0"))
  then_lhs <- .mojor_ir_index(.mojor_ir_var("out"), list(.mojor_ir_var("i")))
  then_rhs <- .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
  then_stmt <- .mojor_ir_assign(then_lhs, then_rhs)
  then_block <- .mojor_ir_block(list(then_stmt))

  stmt <- .mojor_ir_if(cond, then_block, NULL)

  checked <- .mojor_ir_type_check_stmt(stmt, type_env)

  # Should have if structure
  expect_equal(checked$kind, "if")
  expect_equal(checked$cond$kind, "binop")
  expect_equal(checked$then$kind, "block")
})

test_that("type check handles block", {  type_env <- list(out = "f64[]", i = "i32", x = "i32[]", y = "f64[]")

  # Block with two assignments
  # out[i] <- x[i]  # needs cast
  # out[i] <- out[i] + y[i]

  lhs1 <- .mojor_ir_index(.mojor_ir_var("out"), list(.mojor_ir_var("i")))
  rhs1 <- .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
  stmt1 <- .mojor_ir_assign(lhs1, rhs1)

  lhs2 <- .mojor_ir_index(.mojor_ir_var("out"), list(.mojor_ir_var("i")))
  rhs2 <- .mojor_ir_binop("+",
                          .mojor_ir_index(.mojor_ir_var("out"), list(.mojor_ir_var("i"))),
                          .mojor_ir_index(.mojor_ir_var("y"), list(.mojor_ir_var("i"))))
  stmt2 <- .mojor_ir_assign(lhs2, rhs2)

  block <- .mojor_ir_block(list(stmt1, stmt2))
  checked <- .mojor_ir_type_check_stmt(block, type_env)

  # Should have block structure
  expect_equal(checked$kind, "block")
  expect_equal(length(checked$stmts), 2)

  # First statement should have cast on RHS
  expect_equal(checked$stmts[[1]]$rhs$kind, "cast")
  expect_equal(checked$stmts[[1]]$rhs$to, "f64")

  # Second statement should be unchanged
  expect_equal(checked$stmts[[2]]$rhs$kind, "binop")
})

test_that("type check handles mixed arithmetic in assignment", {  type_env <- list(out = "f64[]", i = "i32", x = "f64[]")

  # out[i] <- i + x[i] (i32 + f64 should promote to f64)
  lhs <- .mojor_ir_index(.mojor_ir_var("out"), list(.mojor_ir_var("i")))
  rhs <- .mojor_ir_binop("+",
                         .mojor_ir_var("i"),
                         .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i"))))
  stmt <- .mojor_ir_assign(lhs, rhs)
  checked <- .mojor_ir_type_check_stmt(stmt, type_env)

  # RHS should have cast on i
  expect_equal(checked$rhs$kind, "binop")
  expect_equal(checked$rhs$lhs$kind, "cast")
  expect_equal(checked$rhs$lhs$to, "f64")
})

# =============================================================================
# Phase 4.4: Guard Optimization Tests
# =============================================================================

test_that("bounds guard optimization skips loop variable", {  type_env <- list(x = "f64[]", i = "i32")

  # Build IR for: x[i] (index access)
  node <- .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))

  # Without optimization (no loop_var): should emit bounds check
  result <- .mojor_ir_emit_bounds_guards(node, "    ", NULL, bounds_check = TRUE, loop_var = NULL)
  guard_lines <- result$lines
  expect_true(length(guard_lines) > 0)
  expect_true(any(grepl("if i < 1 or i > Int\\(len\\(x\\)\\)", guard_lines)))

  # With optimization (loop_var = "i"): should skip bounds check
  result_opt <- .mojor_ir_emit_bounds_guards(node, "    ", NULL, bounds_check = TRUE, loop_var = "i")
  expect_equal(length(result_opt$lines), 0)
})

test_that("bounds guard optimization does not skip non-loop variables", {  type_env <- list(x = "f64[]", i = "i32", j = "i32")

  # Build IR for: x[j] (where loop_var is "i", not "j")
  node <- .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("j")))

  # Should still emit bounds check for j even when loop_var = "i"
  result <- .mojor_ir_emit_bounds_guards(node, "    ", NULL, bounds_check = TRUE, loop_var = "i")
  guard_lines <- result$lines
  expect_true(length(guard_lines) > 0)
  expect_true(any(grepl("if j < 1 or j > Int\\(len\\(x\\)\\)", guard_lines)))
})

test_that("bounds guard optimization handles multiple accesses", {  type_env <- list(x = "f64[]", y = "f64[]", i = "i32", j = "i32")

  # Build IR for: x[i] + y[j] (where loop_var is "i")
  lhs <- .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
  rhs <- .mojor_ir_index(.mojor_ir_var("y"), list(.mojor_ir_var("j")))
  node <- .mojor_ir_binop("+", lhs, rhs)

  # Should skip bounds check for x[i] but emit for y[j]
  result <- .mojor_ir_emit_bounds_guards(node, "    ", NULL, bounds_check = TRUE, loop_var = "i")
  guard_lines <- result$lines
  expect_true(length(guard_lines) > 0)
  expect_false(any(grepl("if i < 1 or i > Int\\(len\\(x\\)\\)", guard_lines)))
  expect_true(any(grepl("if j < 1 or j > Int\\(len\\(y\\)\\)", guard_lines)))
})

test_that("bounds guard optimization respects bounds_check flag", {  type_env <- list(x = "f64[]", i = "i32")

  # Build IR for: x[i]
  node <- .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))

  # With bounds_check = FALSE, should not emit any guards regardless of loop_var
  result <- .mojor_ir_emit_bounds_guards(node, "    ", NULL, bounds_check = FALSE, loop_var = "i")
  expect_equal(length(result$lines), 0)

  result_no_opt <- .mojor_ir_emit_bounds_guards(node, "    ", NULL, bounds_check = FALSE, loop_var = NULL)
  expect_equal(length(result_no_opt$lines), 0)
})

test_that("stmt emit wraps indexed writes with OOB guard when bounds_check=TRUE", {
  old_len_map <- .mojor_state$current_len_var_map
  old_n_source <- .mojor_state$current_n_source_name
  on.exit({
    .mojor_state$current_len_var_map <- old_len_map
    .mojor_state$current_n_source_name <- old_n_source
  }, add = TRUE)

  .mojor_state$current_len_var_map <- list(out = "n_out_i")
  .mojor_state$current_n_source_name <- NULL

  node <- .mojor_ir_assign(
    .mojor_ir_index(.mojor_ir_var("out"), list(.mojor_ir_var("idx"))),
    .mojor_ir_const("1.0")
  )
  lines <- .mojor_ir_stmt_emit(
    node,
    indent = "    ",
    zero_based_vars = NULL,
    out_name = "out",
    bounds_check = TRUE,
    type_env = list(out = "f64[]", idx = "i32")
  )

  expect_true(any(grepl("if idx < 1 or idx > Int\\(n_out_i\\):", lines)))
  expect_true(any(grepl("_mojor_oob\\(\\)", lines)))
  expect_true(any(grepl("^\\s*else:$", lines)))
  expect_true(any(grepl(" = 1\\.0", lines)))
})

test_that("slice assign emit wraps writes with OOB guard only in bounds mode", {
  old_len_map <- .mojor_state$current_len_var_map
  old_n_source <- .mojor_state$current_n_source_name
  on.exit({
    .mojor_state$current_len_var_map <- old_len_map
    .mojor_state$current_n_source_name <- old_n_source
  }, add = TRUE)

  .mojor_state$current_len_var_map <- list(out = "n_out_i")
  .mojor_state$current_n_source_name <- NULL

  lhs <- list(
    kind = "subscript",
    var = "out",
    indices = list(.mojor_ir_slice_index(.mojor_ir_const("1"), .mojor_ir_var("end_idx")))
  )
  node <- list(
    kind = "assign",
    lhs = lhs,
    rhs = .mojor_ir_const("9.0"),
    src = NULL
  )

  lines_bounds <- .mojor_ir_slice_assign_emit(
    node,
    indent = "    ",
    zero_based_vars = NULL,
    out_name = "out",
    type_env = list(out = "f64[]", end_idx = "i32"),
    bounds_check = TRUE
  )
  expect_true(any(grepl("_mojor_oob\\(\\)", lines_bounds)))
  expect_true(any(grepl("^\\s*else:$", lines_bounds)))
  expect_true(any(grepl(" = 9\\.0", lines_bounds)))

  lines_nobounds <- .mojor_ir_slice_assign_emit(
    node,
    indent = "    ",
    zero_based_vars = NULL,
    out_name = "out",
    type_env = list(out = "f64[]", end_idx = "i32"),
    bounds_check = FALSE
  )
  expect_false(any(grepl("_mojor_oob\\(\\)", lines_nobounds)))
})

test_that("nd exclusion write emit wraps scalar-axis writes with OOB guard in bounds mode", {
  old_nrow_map <- .mojor_state$current_nrow_var_map
  old_ncol_map <- .mojor_state$current_ncol_var_map
  old_dim_map <- .mojor_state$current_dim_var_map
  old_out_name <- .mojor_state$current_out_name
  old_out_nrow <- .mojor_state$current_out_nrow_var
  old_out_ncol <- .mojor_state$current_out_ncol_var
  old_out_dim <- .mojor_state$current_out_dim_var
  old_tensor_map <- .mojor_state$current_tensor_map
  old_index_bounds <- .mojor_state$options$index_bounds
  on.exit({
    .mojor_state$current_nrow_var_map <- old_nrow_map
    .mojor_state$current_ncol_var_map <- old_ncol_map
    .mojor_state$current_dim_var_map <- old_dim_map
    .mojor_state$current_out_name <- old_out_name
    .mojor_state$current_out_nrow_var <- old_out_nrow
    .mojor_state$current_out_ncol_var <- old_out_ncol
    .mojor_state$current_out_dim_var <- old_out_dim
    .mojor_state$current_tensor_map <- old_tensor_map
    .mojor_state$options$index_bounds <- old_index_bounds
  }, add = TRUE)

  .mojor_state$current_nrow_var_map <- list(out = "nrow_out_i")
  .mojor_state$current_ncol_var_map <- list(out = "ncol_out_i")
  .mojor_state$current_dim_var_map <- NULL
  .mojor_state$current_out_name <- NULL
  .mojor_state$current_out_nrow_var <- NULL
  .mojor_state$current_out_ncol_var <- NULL
  .mojor_state$current_out_dim_var <- NULL
  .mojor_state$current_tensor_map <- list()

  node <- list(
    kind = "assign",
    lhs = list(
      kind = "subscript",
      var = "out",
      indices = list(
        list(kind = "const", value = -1, neg_exclusion = "0"),
        .mojor_ir_var("j")
      )
    ),
    rhs = .mojor_ir_const("9.0"),
    src = NULL
  )

  lines_bounds <- .mojor_ir_nd_exclusion_write_emit(
    node,
    indent = "    ",
    zero_based_vars = NULL,
    type_env = list(out = "f64[,]", j = "i32"),
    loop_var = NULL,
    bounds_check = TRUE
  )
  expect_true(any(grepl("_mojor_oob\\(\\)", lines_bounds)))
  expect_true(any(grepl("^\\s*else:$", lines_bounds)))
  expect_true(any(grepl("ncol_out_i", lines_bounds, fixed = TRUE)))
  expect_true(any(grepl("\\[Int\\(", lines_bounds)))

  .mojor_state$options$index_bounds <- TRUE
  lines_bounds_strict <- .mojor_ir_nd_exclusion_write_emit(
    node,
    indent = "    ",
    zero_based_vars = NULL,
    type_env = list(out = "f64[,]", j = "i32"),
    loop_var = NULL,
    bounds_check = TRUE
  )
  expect_true(any(grepl("__mojor_na_flag\\[0\\]\\s*=\\s*Int32\\(2\\)", lines_bounds_strict)))
  expect_false(any(grepl("_mojor_oob\\(\\)", lines_bounds_strict)))

  lines_nobounds <- .mojor_ir_nd_exclusion_write_emit(
    node,
    indent = "    ",
    zero_based_vars = NULL,
    type_env = list(out = "f64[,]", j = "i32"),
    loop_var = NULL,
    bounds_check = FALSE
  )
  expect_false(any(grepl("_mojor_oob\\(\\)", lines_nobounds)))
})

test_that("nd exclusion subset emit wraps scalar-axis reads with OOB guard in bounds mode", {
  old_nrow_map <- .mojor_state$current_nrow_var_map
  old_ncol_map <- .mojor_state$current_ncol_var_map
  old_dim_map <- .mojor_state$current_dim_var_map
  old_out_name <- .mojor_state$current_out_name
  old_out_nrow <- .mojor_state$current_out_nrow_var
  old_out_ncol <- .mojor_state$current_out_ncol_var
  old_out_dim <- .mojor_state$current_out_dim_var
  old_index_bounds <- .mojor_state$options$index_bounds
  on.exit({
    .mojor_state$current_nrow_var_map <- old_nrow_map
    .mojor_state$current_ncol_var_map <- old_ncol_map
    .mojor_state$current_dim_var_map <- old_dim_map
    .mojor_state$current_out_name <- old_out_name
    .mojor_state$current_out_nrow_var <- old_out_nrow
    .mojor_state$current_out_ncol_var <- old_out_ncol
    .mojor_state$current_out_dim_var <- old_out_dim
    .mojor_state$options$index_bounds <- old_index_bounds
  }, add = TRUE)

  .mojor_state$current_nrow_var_map <- list(mat = "nrow_mat_i")
  .mojor_state$current_ncol_var_map <- list(mat = "ncol_mat_i")
  .mojor_state$current_dim_var_map <- NULL
  .mojor_state$current_out_name <- NULL
  .mojor_state$current_out_nrow_var <- NULL
  .mojor_state$current_out_ncol_var <- NULL
  .mojor_state$current_out_dim_var <- NULL

  node <- list(
    kind = "assign",
    lhs = .mojor_ir_var("out"),
    rhs = .mojor_ir_index(
      .mojor_ir_var("mat"),
      list(
        list(kind = "const", value = -1, neg_exclusion = "0"),
        .mojor_ir_var("j")
      )
    ),
    src = NULL
  )

  lines_bounds <- .mojor_ir_nd_exclusion_subset_emit(
    node,
    indent = "    ",
    zero_based_vars = NULL,
    type_env = list(out = "f64[]", mat = "f64[,]", j = "i32"),
    loop_var = NULL,
    bounds_check = TRUE
  )
  expect_true(any(grepl("_mojor_oob\\(\\)", lines_bounds)))
  expect_true(any(grepl("^\\s*else:$", lines_bounds)))
  expect_true(any(grepl("ncol_mat_i", lines_bounds, fixed = TRUE)))
  expect_true(any(grepl("out\\[", lines_bounds)))

  .mojor_state$options$index_bounds <- TRUE
  lines_bounds_strict <- .mojor_ir_nd_exclusion_subset_emit(
    node,
    indent = "    ",
    zero_based_vars = NULL,
    type_env = list(out = "f64[]", mat = "f64[,]", j = "i32"),
    loop_var = NULL,
    bounds_check = TRUE
  )
  expect_true(any(grepl("__mojor_na_flag\\[0\\]\\s*=\\s*Int32\\(2\\)", lines_bounds_strict)))
  expect_false(any(grepl("_mojor_oob\\(\\)", lines_bounds_strict)))

  lines_nobounds <- .mojor_ir_nd_exclusion_subset_emit(
    node,
    indent = "    ",
    zero_based_vars = NULL,
    type_env = list(out = "f64[]", mat = "f64[,]", j = "i32"),
    loop_var = NULL,
    bounds_check = FALSE
  )
  expect_false(any(grepl("_mojor_oob\\(\\)", lines_nobounds)))
})
