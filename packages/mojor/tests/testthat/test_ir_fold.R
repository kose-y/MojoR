test_that("all_const detects constant expressions", {  # Simple const
  expr <- .mojor_ir_const("1.0")
  expect_true(.mojor_ir_all_const(expr))

  # Var (not const)
  expr <- .mojor_ir_var("x")
  expect_false(.mojor_ir_all_const(expr))

  # Binop with two consts
  expr <- .mojor_ir_binop("+", .mojor_ir_const("1.0"), .mojor_ir_const("2.0"))
  expect_true(.mojor_ir_all_const(expr))

  # Binop with var and const
  expr <- .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_const("2.0"))
  expect_false(.mojor_ir_all_const(expr))
})

test_that("all_const handles nested expressions", {  # (1 + 2) * 3 - all const
  expr <- .mojor_ir_binop(
    "*",
    .mojor_ir_binop("+", .mojor_ir_const("1.0"), .mojor_ir_const("2.0")),
    .mojor_ir_const("3.0")
  )
  expect_true(.mojor_ir_all_const(expr))

  # (x + 2) * 3 - has var
  expr <- .mojor_ir_binop(
    "*",
    .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_const("2.0")),
    .mojor_ir_const("3.0")
  )
  expect_false(.mojor_ir_all_const(expr))
})

test_that("eval_const evaluates arithmetic operations", {  # 1 + 2
  expr <- .mojor_ir_binop("+", .mojor_ir_const("1.0"), .mojor_ir_const("2.0"))
  result <- .mojor_ir_eval_const(expr)
  expect_equal(result, 3.0)

  # 5 - 3
  expr <- .mojor_ir_binop("-", .mojor_ir_const("5.0"), .mojor_ir_const("3.0"))
  result <- .mojor_ir_eval_const(expr)
  expect_equal(result, 2.0)

  # 4 * 3
  expr <- .mojor_ir_binop("*", .mojor_ir_const("4.0"), .mojor_ir_const("3.0"))
  result <- .mojor_ir_eval_const(expr)
  expect_equal(result, 12.0)

  # 10 / 2
  expr <- .mojor_ir_binop("/", .mojor_ir_const("10.0"), .mojor_ir_const("2.0"))
  result <- .mojor_ir_eval_const(expr)
  expect_equal(result, 5.0)
})

test_that("eval_const evaluates comparison operations", {  # 5 > 3
  expr <- .mojor_ir_binop(">", .mojor_ir_const("5.0"), .mojor_ir_const("3.0"))
  result <- .mojor_ir_eval_const(expr)
  expect_type(result, "double")
  expect_equal(result, 1.0)  # TRUE as numeric

  # 2 < 1
  expr <- .mojor_ir_binop("<", .mojor_ir_const("2.0"), .mojor_ir_const("1.0"))
  result <- .mojor_ir_eval_const(expr)
  expect_type(result, "double")
  expect_equal(result, 0.0)  # FALSE as numeric

  # 5 == 5
  expr <- .mojor_ir_binop("==", .mojor_ir_const("5.0"), .mojor_ir_const("5.0"))
  result <- .mojor_ir_eval_const(expr)
  expect_type(result, "double")
  expect_equal(result, 1.0)  # TRUE as numeric
})

test_that("eval_const evaluates unary operations", {  # -5
  expr <- .mojor_ir_unop("-", .mojor_ir_const("5.0"))
  result <- .mojor_ir_eval_const(expr)
  expect_equal(result, -5.0)

  # !0
  expr <- .mojor_ir_unop("!", .mojor_ir_const("0.0"))
  result <- .mojor_ir_eval_const(expr)
  expect_type(result, "double")
  expect_equal(result, 1.0)  # TRUE as numeric

  # !1
  expr <- .mojor_ir_unop("!", .mojor_ir_const("1.0"))
  result <- .mojor_ir_eval_const(expr)
  expect_type(result, "double")
  expect_equal(result, 0.0)  # FALSE as numeric
})

test_that("eval_const evaluates math functions", {  # sin(0)
  expr <- .mojor_ir_call("sin", list(.mojor_ir_const("0.0")))
  result <- .mojor_ir_eval_const(expr)
  expect_equal(result, sin(0), tolerance = 1e-10)

  # log(1)
  expr <- .mojor_ir_call("log", list(.mojor_ir_const("1.0")))
  result <- .mojor_ir_eval_const(expr)
  expect_equal(result, 0.0, tolerance = 1e-10)

  # sqrt(4)
  expr <- .mojor_ir_call("sqrt", list(.mojor_ir_const("4.0")))
  result <- .mojor_ir_eval_const(expr)
  expect_equal(result, 2.0)

  # exp(0)
  expr <- .mojor_ir_call("exp", list(.mojor_ir_const("0.0")))
  result <- .mojor_ir_eval_const(expr)
  expect_equal(result, 1.0, tolerance = 1e-10)
})

test_that("eval_const handles nested arithmetic", {  # (1 + 2) * 3 = 9
  expr <- .mojor_ir_binop(
    "*",
    .mojor_ir_binop("+", .mojor_ir_const("1.0"), .mojor_ir_const("2.0")),
    .mojor_ir_const("3.0")
  )
  result <- .mojor_ir_eval_const(expr)
  expect_equal(result, 9.0)

  # 2 * (3 + 4) = 14
  expr <- .mojor_ir_binop(
    "*",
    .mojor_ir_const("2.0"),
    .mojor_ir_binop("+", .mojor_ir_const("3.0"), .mojor_ir_const("4.0"))
  )
  result <- .mojor_ir_eval_const(expr)
  expect_equal(result, 14.0)
})

test_that("fold_expr folds constant arithmetic", {  # 1 + 2 should fold to 3.0
  expr <- .mojor_ir_binop("+", .mojor_ir_const("1.0"), .mojor_ir_const("2.0"))
  result <- .mojor_ir_fold_expr(expr)

  expect_equal(result$kind, "const")
  expect_equal(as.numeric(result$value), 3.0)
})

test_that("fold_expr folds nested constants", {  # (1 + 2) * 3 should fold to 9.0
  expr <- .mojor_ir_binop(
    "*",
    .mojor_ir_binop("+", .mojor_ir_const("1.0"), .mojor_ir_const("2.0")),
    .mojor_ir_const("3.0")
  )
  result <- .mojor_ir_fold_expr(expr)

  expect_equal(result$kind, "const")
  expect_equal(as.numeric(result$value), 9.0)
})

test_that("fold_expr does not fold expressions with variables", {  # x + 2 should not fold
  expr <- .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_const("2.0"))
  result <- .mojor_ir_fold_expr(expr)

  expect_equal(result$kind, "binop")
  expect_equal(result$op, "+")
})

test_that("fold_expr folds math function calls", {  # sin(0) should fold to 0.0
  expr <- .mojor_ir_call("sin", list(.mojor_ir_const("0.0")))
  result <- .mojor_ir_fold_expr(expr)

  expect_equal(result$kind, "const")
  expect_equal(as.numeric(result$value), 0.0, tolerance = 1e-10)
})

test_that("fold_expr partially folds mixed expressions", {  # x + (1 + 2) should fold to x + 3
  expr <- .mojor_ir_binop(
    "+",
    .mojor_ir_var("x"),
    .mojor_ir_binop("+", .mojor_ir_const("1.0"), .mojor_ir_const("2.0"))
  )
  result <- .mojor_ir_fold_expr(expr)

  expect_equal(result$kind, "binop")
  expect_equal(result$op, "+")
  expect_equal(result$lhs$kind, "var")
  expect_equal(result$rhs$kind, "const")
  expect_equal(as.numeric(result$rhs$value), 3.0)
})

test_that("fold_stmt folds assignment RHS", {  # out <- 1 + 2
  stmt <- .mojor_ir_assign(
    .mojor_ir_var("out"),
    .mojor_ir_binop("+", .mojor_ir_const("1.0"), .mojor_ir_const("2.0"))
  )

  result <- .mojor_ir_fold_stmt(stmt)

  expect_equal(result$kind, "assign")
  expect_equal(result$rhs$kind, "const")
  expect_equal(as.numeric(result$rhs$value), 3.0)
})

test_that("fold_stmt folds if conditions", {  # if (1 > 0) { ... }
  yes_block <- .mojor_ir_block(list())
  stmt <- .mojor_ir_if(
    .mojor_ir_binop(">", .mojor_ir_const("1.0"), .mojor_ir_const("0.0")),
    yes_block,
    NULL
  )

  result <- .mojor_ir_fold_stmt(stmt)

  expect_equal(result$kind, "if")
  expect_equal(result$cond$kind, "const")
  expect_equal(as.numeric(result$cond$value), 1.0, tolerance = 1e-10)  # TRUE
})

test_that("fold_block folds all statements", {  # Block with multiple const expressions
  block <- .mojor_ir_block(list(
    .mojor_ir_assign(
      .mojor_ir_var("tmp1"),
      .mojor_ir_binop("+", .mojor_ir_const("1.0"), .mojor_ir_const("2.0"))
    ),
    .mojor_ir_assign(
      .mojor_ir_var("tmp2"),
      .mojor_ir_binop("*", .mojor_ir_const("3.0"), .mojor_ir_const("4.0"))
    )
  ))

  result <- .mojor_ir_fold_block(block)

  expect_equal(result$kind, "block")
  expect_length(result$stmts, 2)

  # First statement folded to 3.0
  expect_equal(result$stmts[[1]]$rhs$kind, "const")
  expect_equal(as.numeric(result$stmts[[1]]$rhs$value), 3.0)

  # Second statement folded to 12.0
  expect_equal(result$stmts[[2]]$rhs$kind, "const")
  expect_equal(as.numeric(result$stmts[[2]]$rhs$value), 12.0)
})

test_that("fold_block recursively folds if statements", {  # if (1 > 0) { out <- 2 + 3 }
  yes_block <- .mojor_ir_block(list(
    .mojor_ir_assign(
      .mojor_ir_var("out"),
      .mojor_ir_binop("+", .mojor_ir_const("2.0"), .mojor_ir_const("3.0"))
    )
  ))

  stmt <- .mojor_ir_if(
    .mojor_ir_binop(">", .mojor_ir_const("1.0"), .mojor_ir_const("0.0")),
    yes_block,
    NULL
  )

  block <- .mojor_ir_block(list(stmt))
  result <- .mojor_ir_fold_block(block)

  # Condition should be folded
  expect_equal(result$stmts[[1]]$cond$kind, "const")
  expect_equal(as.numeric(result$stmts[[1]]$cond$value), 1.0, tolerance = 1e-10)

  # Then block RHS should be folded
  expect_equal(result$stmts[[1]]$then$stmts[[1]]$rhs$kind, "const")
  expect_equal(as.numeric(result$stmts[[1]]$then$stmts[[1]]$rhs$value), 5.0)
})

test_that("fold_block recursively folds loops", {  # for (i in 1:10) { out[i] <- 2 * 3 }
  body <- .mojor_ir_block(list(
    .mojor_ir_assign(
      .mojor_ir_subscript("out", list(.mojor_ir_var("i"))),
      .mojor_ir_binop("*", .mojor_ir_const("2.0"), .mojor_ir_const("3.0"))
    )
  ))

  range <- .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10"), NULL, "inclusive")
  loop <- .mojor_ir_loop("i", range, body)

  block <- .mojor_ir_block(list(loop))
  result <- .mojor_ir_fold_block(block)

  # Loop body RHS should be folded
  loop_result <- result$stmts[[1]]
  expect_equal(loop_result$body$stmts[[1]]$rhs$kind, "const")
  expect_equal(as.numeric(loop_result$body$stmts[[1]]$rhs$value), 6.0)
})

test_that("value_to_const handles Inf", {  const <- .mojor_ir_value_to_const(Inf)
  expect_equal(const$kind, "const")
  expect_equal(const$value, "Inf")
})

test_that("value_to_const handles -Inf", {  const <- .mojor_ir_value_to_const(-Inf)
  expect_equal(const$kind, "const")
  expect_equal(const$value, "-Inf")
})

test_that("value_to_const handles NaN", {  const <- .mojor_ir_value_to_const(NaN)
  expect_equal(const$kind, "const")
  expect_equal(const$value, "NaN")
})

test_that("value_to_const handles regular numbers", {  const <- .mojor_ir_value_to_const(3.14159)
  expect_equal(const$kind, "const")
  expect_equal(as.numeric(const$value), 3.14159, tolerance = 1e-5)
})

test_that("folding preserves non-pure expressions", {  # rnorm(1) + 2 should not fold (RNG effect)
  expr <- .mojor_ir_binop(
    "+",
    .mojor_ir_call("rnorm", list(.mojor_ir_const("1"))),
    .mojor_ir_const("2.0")
  )
  result <- .mojor_ir_fold_expr(expr)

  # Should remain a binop (not folded)
  expect_equal(result$kind, "binop")
})

test_that("folding handles empty blocks", {  block <- .mojor_ir_block(list())
  result <- .mojor_ir_fold_block(block)
  expect_equal(result$kind, "block")
  expect_length(result$stmts, 0)
})

test_that("folding handles NULL gracefully", {  result <- .mojor_ir_fold_block(NULL)
  expect_null(result)
})

test_that("eval_const handles Inf and NaN", {  # Inf
  expr <- .mojor_ir_const("Inf")
  result <- .mojor_ir_eval_const(expr)
  expect_true(is.infinite(result) && result > 0)

  # -Inf
  expr <- .mojor_ir_const("-Inf")
  result <- .mojor_ir_eval_const(expr)
  expect_true(is.infinite(result) && result < 0)

  # NaN
  expr <- .mojor_ir_const("NaN")
  result <- .mojor_ir_eval_const(expr)
  expect_true(is.nan(result))
})

test_that("folding eliminates redundant computation", {  # out <- (1 + 2) + (3 + 4) should fold to out <- 10
  expr <- .mojor_ir_binop(
    "+",
    .mojor_ir_binop("+", .mojor_ir_const("1.0"), .mojor_ir_const("2.0")),
    .mojor_ir_binop("+", .mojor_ir_const("3.0"), .mojor_ir_const("4.0"))
  )

  stmt <- .mojor_ir_assign(.mojor_ir_var("out"), expr)
  result <- .mojor_ir_fold_stmt(stmt)

  expect_equal(result$rhs$kind, "const")
  expect_equal(as.numeric(result$rhs$value), 10.0)
})

test_that("folding works with unary minus on constants", {  # out <- -(1 + 2)
  expr <- .mojor_ir_unop(
    "-",
    .mojor_ir_binop("+", .mojor_ir_const("1.0"), .mojor_ir_const("2.0"))
  )

  result <- .mojor_ir_fold_expr(expr)

  expect_equal(result$kind, "const")
  expect_equal(as.numeric(result$value), -3.0)
})

test_that("folding with CSE creates opportunities", {  # After CSE: tmp <- x + y; out <- tmp * tmp
  # After fold (no change expected, not all const)
  # But if we had: tmp <- 2 + 3; out <- tmp * tmp
  # Should fold to: tmp <- 5; out <- tmp * tmp

  stmt1 <- .mojor_ir_assign(
    .mojor_ir_var("tmp"),
    .mojor_ir_binop("+", .mojor_ir_const("2.0"), .mojor_ir_const("3.0"))
  )
  stmt2 <- .mojor_ir_assign(
    .mojor_ir_var("out"),
    .mojor_ir_binop("*", .mojor_ir_var("tmp"), .mojor_ir_var("tmp"))
  )

  block <- .mojor_ir_block(list(stmt1, stmt2))
  result <- .mojor_ir_fold_block(block)

  # First statement should be folded
  expect_equal(result$stmts[[1]]$rhs$kind, "const")
  expect_equal(as.numeric(result$stmts[[1]]$rhs$value), 5.0)

  # Second statement not folded (uses variable)
  expect_equal(result$stmts[[2]]$rhs$kind, "binop")
})
