# =============================================================================
# Test IR CSE (Common Subexpression Elimination)
# =============================================================================

test_that("expression key generation works", {  # Constants
  expect_equal(.mojor_ir_expr_key(.mojor_ir_const("42")), "const:42")
  expect_equal(.mojor_ir_expr_key(.mojor_ir_const("3.14")), "const:3.14")

  # Variables
  expect_equal(.mojor_ir_expr_key(.mojor_ir_var("x")), "var:x")
  expect_equal(.mojor_ir_expr_key(.mojor_ir_var("y")), "var:y")

  # Binop
  x <- .mojor_ir_var("x")
  y <- .mojor_ir_var("y")
  expr <- .mojor_ir_binop("+", x, y)
  expect_equal(.mojor_ir_expr_key(expr), "binop:+(var:x,var:y)")

  # Unop
  expr <- .mojor_ir_unop("-", x)
  expect_equal(.mojor_ir_expr_key(expr), "unop:-(var:x)")

  # Cast
  expr <- .mojor_ir_cast("f64", x)
  expect_equal(.mojor_ir_expr_key(expr), "cast:f64(var:x)")

  # Call
  expr <- .mojor_ir_call("sin", list(x))
  expect_equal(.mojor_ir_expr_key(expr), "call:sin(var:x)")

  # Complex nested expression
  expr <- .mojor_ir_binop("*",
    .mojor_ir_binop("+", x, y),
    .mojor_ir_binop("+", x, y)
  )
  key <- .mojor_ir_expr_key(expr)
  expect_true(grepl("binop:\\*\\(binop:\\+\\(var:x,var:y\\),binop:\\+\\(var:x,var:y\\)\\)", key))
})

test_that("trivial expression check works", {  expect_true(.mojor_ir_is_trivial(.mojor_ir_const("42")))
  expect_true(.mojor_ir_is_trivial(.mojor_ir_var("x")))
  expect_false(.mojor_ir_is_trivial(.mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_var("y"))))
  expect_false(.mojor_ir_is_trivial(.mojor_ir_unop("-", .mojor_ir_var("x"))))
})

test_that("subexpression collection works", {  x <- .mojor_ir_var("x")
  y <- .mojor_ir_var("y")

  # Simple binop: should collect just the binop, not the vars
  expr <- .mojor_ir_binop("+", x, y)
  subexprs <- .mojor_ir_collect_subexprs(expr)
  expect_equal(length(subexprs), 1)
  expect_equal(subexprs[[1]]$kind, "binop")

  # Nested expression: (x + y) * (x + y)
  add_expr <- .mojor_ir_binop("+", x, y)
  mul_expr <- .mojor_ir_binop("*", add_expr, add_expr)
  subexprs <- .mojor_ir_collect_subexprs(mul_expr)
  # Should collect: mul_expr (top), add_expr (left), add_expr (right)
  expect_equal(length(subexprs), 3)
  expect_equal(subexprs[[1]]$kind, "binop")
  expect_equal(subexprs[[1]]$op, "*")
  expect_equal(subexprs[[2]]$kind, "binop")
  expect_equal(subexprs[[2]]$op, "+")
  expect_equal(subexprs[[3]]$kind, "binop")
  expect_equal(subexprs[[3]]$op, "+")

  # Unop
  expr <- .mojor_ir_unop("-", .mojor_ir_binop("+", x, y))
  subexprs <- .mojor_ir_collect_subexprs(expr)
  expect_equal(length(subexprs), 2) # unop and binop

  # Call with multiple args
  expr <- .mojor_ir_call("foo", list(
    .mojor_ir_binop("+", x, y),
    .mojor_ir_binop("*", x, y)
  ))
  subexprs <- .mojor_ir_collect_subexprs(expr)
  expect_equal(length(subexprs), 3) # call, add, mul
})

test_that("expression replacement works", {  x <- .mojor_ir_var("x")
  y <- .mojor_ir_var("y")

  # Replace binop with variable
  add_expr <- .mojor_ir_binop("+", x, y)
  key <- .mojor_ir_expr_key(add_expr)

  # Top-level replacement
  replaced <- .mojor_ir_replace_expr(add_expr, key, "tmp")
  expect_equal(replaced$kind, "var")
  expect_equal(replaced$name, "tmp")

  # Nested replacement: (x + y) * (x + y) -> tmp * tmp
  mul_expr <- .mojor_ir_binop("*", add_expr, add_expr)
  replaced <- .mojor_ir_replace_expr(mul_expr, key, "tmp")
  expect_equal(replaced$kind, "binop")
  expect_equal(replaced$op, "*")
  expect_equal(replaced$lhs$kind, "var")
  expect_equal(replaced$lhs$name, "tmp")
  expect_equal(replaced$rhs$kind, "var")
  expect_equal(replaced$rhs$name, "tmp")
})

test_that("CSE eliminates duplicate pure subexpressions in assignment", {  x <- .mojor_ir_var("x")
  y <- .mojor_ir_var("y")
  out <- .mojor_ir_var("out")

  # out <- (x + y) * (x + y)
  add_expr1 <- .mojor_ir_binop("+", x, y)
  add_expr2 <- .mojor_ir_binop("+", x, y)
  mul_expr <- .mojor_ir_binop("*", add_expr1, add_expr2)
  stmt <- .mojor_ir_assign(out, mul_expr)

  result <- .mojor_ir_cse_stmt(stmt)

  # Should generate: __cse_tmp1 <- x + y; out <- __cse_tmp1 * __cse_tmp1
  expect_equal(length(result$stmts), 2)
  expect_equal(result$stmts[[1]]$kind, "assign")
  expect_equal(result$stmts[[1]]$lhs$name, "__cse_tmp1")
  expect_equal(result$stmts[[1]]$rhs$kind, "binop")
  expect_equal(result$stmts[[1]]$rhs$op, "+")

  expect_equal(result$stmts[[2]]$kind, "assign")
  expect_equal(result$stmts[[2]]$lhs$name, "out")
  expect_equal(result$stmts[[2]]$rhs$kind, "binop")
  expect_equal(result$stmts[[2]]$rhs$op, "*")
  expect_equal(result$stmts[[2]]$rhs$lhs$kind, "var")
  expect_equal(result$stmts[[2]]$rhs$lhs$name, "__cse_tmp1")
  expect_equal(result$stmts[[2]]$rhs$rhs$kind, "var")
  expect_equal(result$stmts[[2]]$rhs$rhs$name, "__cse_tmp1")
})

test_that("CSE does not eliminate non-duplicate expressions", {  x <- .mojor_ir_var("x")
  y <- .mojor_ir_var("y")
  z <- .mojor_ir_var("z")
  out <- .mojor_ir_var("out")

  # out <- (x + y) * (x + z)  -- different expressions
  add1 <- .mojor_ir_binop("+", x, y)
  add2 <- .mojor_ir_binop("+", x, z)
  mul_expr <- .mojor_ir_binop("*", add1, add2)
  stmt <- .mojor_ir_assign(out, mul_expr)

  result <- .mojor_ir_cse_stmt(stmt)

  # Should not transform (no duplicates)
  expect_equal(length(result$stmts), 1)
  expect_equal(result$stmts[[1]], stmt)
})

test_that("CSE does not eliminate trivial expressions", {  x <- .mojor_ir_var("x")
  out <- .mojor_ir_var("out")

  # out <- x * x  -- x appears twice but is trivial
  mul_expr <- .mojor_ir_binop("*", x, x)
  stmt <- .mojor_ir_assign(out, mul_expr)

  result <- .mojor_ir_cse_stmt(stmt)

  # Should not transform (x is trivial)
  expect_equal(length(result$stmts), 1)
  expect_equal(result$stmts[[1]], stmt)
})

test_that("CSE does not eliminate impure expressions", {  x <- .mojor_ir_var("x")
  i <- .mojor_ir_var("i")
  out <- .mojor_ir_var("out")

  # out <- x[i] * x[i]  -- not pure (ReadsMem)
  idx1 <- .mojor_ir_index(x, list(i))
  idx2 <- .mojor_ir_index(x, list(i))
  mul_expr <- .mojor_ir_binop("*", idx1, idx2)
  stmt <- .mojor_ir_assign(out, mul_expr)

  result <- .mojor_ir_cse_stmt(stmt)

  # Should not transform (x[i] is not pure)
  expect_equal(length(result$stmts), 1)
  expect_equal(result$stmts[[1]], stmt)
})

test_that("CSE does not eliminate RNG expressions", {  out <- .mojor_ir_var("out")
  n <- .mojor_ir_var("n")

  # out <- rnorm(n) + rnorm(n)  -- RNG, must not CSE
  rng1 <- .mojor_ir_call("rnorm", list(n))
  rng2 <- .mojor_ir_call("rnorm", list(n))
  add_expr <- .mojor_ir_binop("+", rng1, rng2)
  stmt <- .mojor_ir_assign(out, add_expr)

  result <- .mojor_ir_cse_stmt(stmt)

  # Should not transform (rnorm is RNG)
  expect_equal(length(result$stmts), 1)
  expect_equal(result$stmts[[1]], stmt)
})

test_that("CSE eliminates math function duplicates", {  x <- .mojor_ir_var("x")
  out <- .mojor_ir_var("out")

  # out <- sin(x) * sin(x)
  sin1 <- .mojor_ir_call("sin", list(x))
  sin2 <- .mojor_ir_call("sin", list(x))
  mul_expr <- .mojor_ir_binop("*", sin1, sin2)
  stmt <- .mojor_ir_assign(out, mul_expr)

  result <- .mojor_ir_cse_stmt(stmt)

  # Should generate: __cse_tmp1 <- sin(x); out <- __cse_tmp1 * __cse_tmp1
  expect_equal(length(result$stmts), 2)
  expect_equal(result$stmts[[1]]$lhs$name, "__cse_tmp1")
  expect_equal(result$stmts[[1]]$rhs$fn, "sin")

  expect_equal(result$stmts[[2]]$lhs$name, "out")
  expect_equal(result$stmts[[2]]$rhs$lhs$name, "__cse_tmp1")
  expect_equal(result$stmts[[2]]$rhs$rhs$name, "__cse_tmp1")
})

test_that("CSE handles multiple duplicates", {  x <- .mojor_ir_var("x")
  y <- .mojor_ir_var("y")
  out <- .mojor_ir_var("out")

  # out <- (x + y) * (x + y) + (x * 2) * (x * 2)
  add1 <- .mojor_ir_binop("+", x, y)
  add2 <- .mojor_ir_binop("+", x, y)
  two <- .mojor_ir_const("2")
  mul1 <- .mojor_ir_binop("*", x, two)
  mul2 <- .mojor_ir_binop("*", x, two)

  mul_add <- .mojor_ir_binop("*", add1, add2)
  mul_mul <- .mojor_ir_binop("*", mul1, mul2)
  add_final <- .mojor_ir_binop("+", mul_add, mul_mul)

  stmt <- .mojor_ir_assign(out, add_final)

  result <- .mojor_ir_cse_stmt(stmt)

  # Should generate 2 temp variables for the two different duplicates
  expect_equal(length(result$stmts), 3) # 2 temps + 1 final assign
  expect_equal(result$stmts[[1]]$lhs$name, "__cse_tmp1")
  expect_equal(result$stmts[[2]]$lhs$name, "__cse_tmp2")
  expect_equal(result$stmts[[3]]$lhs$name, "out")
})

test_that("CSE works on blocks", {  x <- .mojor_ir_var("x")
  y <- .mojor_ir_var("y")
  out1 <- .mojor_ir_var("out1")
  out2 <- .mojor_ir_var("out2")

  # out1 <- (x + y) * (x + y)
  # out2 <- (x + y) + 1
  add1 <- .mojor_ir_binop("+", x, y)
  add2 <- .mojor_ir_binop("+", x, y)
  mul_expr <- .mojor_ir_binop("*", add1, add2)
  stmt1 <- .mojor_ir_assign(out1, mul_expr)

  add3 <- .mojor_ir_binop("+", x, y)
  one <- .mojor_ir_const("1")
  add_expr <- .mojor_ir_binop("+", add3, one)
  stmt2 <- .mojor_ir_assign(out2, add_expr)

  block <- .mojor_ir_block(list(stmt1, stmt2))

  result <- .mojor_ir_cse_block(block)

  # First statement should introduce __cse_tmp1 for (x + y)
  # Second statement has no duplicates within itself, so no new temp
  expect_equal(length(result$block$stmts), 3) # tmp1, out1, out2

  # Verify first statement was transformed
  expect_equal(result$block$stmts[[1]]$lhs$name, "__cse_tmp1")
  expect_equal(result$block$stmts[[2]]$lhs$name, "out1")
  expect_equal(result$block$stmts[[3]]$lhs$name, "out2")
})

test_that("CSE works recursively on if statements", {  x <- .mojor_ir_var("x")
  y <- .mojor_ir_var("y")
  cond <- .mojor_ir_var("cond")
  out <- .mojor_ir_var("out")

  # if (cond) { out <- (x + y) * (x + y) }
  add1 <- .mojor_ir_binop("+", x, y)
  add2 <- .mojor_ir_binop("+", x, y)
  mul_expr <- .mojor_ir_binop("*", add1, add2)
  assign_stmt <- .mojor_ir_assign(out, mul_expr)
  then_block <- .mojor_ir_block(list(assign_stmt))

  if_stmt <- .mojor_ir_if(cond, then_block)

  result <- .mojor_ir_cse_stmt(if_stmt)

  # CSE should transform the then block
  expect_equal(length(result$stmts), 1)
  expect_equal(result$stmts[[1]]$kind, "if")
  expect_equal(length(result$stmts[[1]]$then$stmts), 2) # tmp + out
})

test_that("CSE works recursively on loops", {  i <- .mojor_ir_var("i")
  x <- .mojor_ir_var("x")
  y <- .mojor_ir_var("y")
  out <- .mojor_ir_var("out")

  # for (i in 1:10) { out[i] <- (x + y) * (x + y) }
  add1 <- .mojor_ir_binop("+", x, y)
  add2 <- .mojor_ir_binop("+", x, y)
  mul_expr <- .mojor_ir_binop("*", add1, add2)

  lhs <- .mojor_ir_index(out, list(i))
  assign_stmt <- .mojor_ir_assign(lhs, mul_expr)
  body <- .mojor_ir_block(list(assign_stmt))

  range <- .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10"))
  loop_stmt <- .mojor_ir_loop("i", range, body)

  result <- .mojor_ir_cse_stmt(loop_stmt)

  # CSE should transform the loop body
  expect_equal(length(result$stmts), 1)
  expect_equal(result$stmts[[1]]$kind, "loop")
  expect_equal(length(result$stmts[[1]]$body$stmts), 2) # tmp + out[i]
})

test_that("CSE handles cast expressions correctly", {  x <- .mojor_ir_var("x")
  out <- .mojor_ir_var("out")

  # out <- as.double(x) + as.double(x)
  cast1 <- .mojor_ir_cast("f64", x)
  cast2 <- .mojor_ir_cast("f64", x)
  add_expr <- .mojor_ir_binop("+", cast1, cast2)
  stmt <- .mojor_ir_assign(out, add_expr)

  result <- .mojor_ir_cse_stmt(stmt)

  # Should eliminate duplicate cast
  expect_equal(length(result$stmts), 2)
  expect_equal(result$stmts[[1]]$lhs$name, "__cse_tmp1")
  expect_equal(result$stmts[[1]]$rhs$kind, "cast")
  expect_equal(result$stmts[[2]]$rhs$lhs$name, "__cse_tmp1")
  expect_equal(result$stmts[[2]]$rhs$rhs$name, "__cse_tmp1")
})

test_that("CSE handles ifelse expressions correctly", {  cond <- .mojor_ir_var("cond")
  x <- .mojor_ir_var("x")
  y <- .mojor_ir_var("y")
  out <- .mojor_ir_var("out")

  # out <- ifelse(cond, x, y) + ifelse(cond, x, y)
  ifelse1 <- .mojor_ir_ifelse(cond, x, y)
  ifelse2 <- .mojor_ir_ifelse(cond, x, y)
  add_expr <- .mojor_ir_binop("+", ifelse1, ifelse2)
  stmt <- .mojor_ir_assign(out, add_expr)

  result <- .mojor_ir_cse_stmt(stmt)

  # Should eliminate duplicate ifelse
  expect_equal(length(result$stmts), 2)
  expect_equal(result$stmts[[1]]$lhs$name, "__cse_tmp1")
  expect_equal(result$stmts[[1]]$rhs$kind, "ifelse")
  expect_equal(result$stmts[[2]]$rhs$lhs$name, "__cse_tmp1")
  expect_equal(result$stmts[[2]]$rhs$rhs$name, "__cse_tmp1")
})

test_that("CSE handles nested duplicates correctly", {  x <- .mojor_ir_var("x")
  y <- .mojor_ir_var("y")
  out <- .mojor_ir_var("out")

  # out <- sin(x + y) * sin(x + y) + (x + y)
  # Should extract both sin(x + y) and (x + y)
  add1 <- .mojor_ir_binop("+", x, y)
  add2 <- .mojor_ir_binop("+", x, y)
  add3 <- .mojor_ir_binop("+", x, y)

  sin1 <- .mojor_ir_call("sin", list(add1))
  sin2 <- .mojor_ir_call("sin", list(add2))

  mul_expr <- .mojor_ir_binop("*", sin1, sin2)
  add_final <- .mojor_ir_binop("+", mul_expr, add3)

  stmt <- .mojor_ir_assign(out, add_final)

  result <- .mojor_ir_cse_stmt(stmt)

  # Should generate temps for both (x + y) and sin(x + y)
  expect_gt(length(result$stmts), 1)
  expect_equal(result$stmts[[length(result$stmts)]]$lhs$name, "out")
})

test_that("CSE with empty block", {  block <- .mojor_ir_block(list())
  result <- .mojor_ir_cse_block(block)
  expect_equal(length(result$block$stmts), 0)
})

test_that("CSE with NULL expressions", {  expect_equal(.mojor_ir_expr_key(NULL), "")
  expect_true(.mojor_ir_is_trivial(NULL))
  expect_equal(length(.mojor_ir_collect_subexprs(NULL)), 0)
  expect_null(.mojor_ir_replace_expr(NULL, "key", "var"))
})

test_that("CSE preserves statement order", {  x <- .mojor_ir_var("x")
  y <- .mojor_ir_var("y")
  out1 <- .mojor_ir_var("out1")
  out2 <- .mojor_ir_var("out2")

  # out1 <- x + y
  # out2 <- (x + y) * (x + y)
  stmt1 <- .mojor_ir_assign(out1, .mojor_ir_binop("+", x, y))

  add1 <- .mojor_ir_binop("+", x, y)
  add2 <- .mojor_ir_binop("+", x, y)
  mul_expr <- .mojor_ir_binop("*", add1, add2)
  stmt2 <- .mojor_ir_assign(out2, mul_expr)

  block <- .mojor_ir_block(list(stmt1, stmt2))
  result <- .mojor_ir_cse_block(block)

  # First statement should be unchanged
  expect_equal(result$block$stmts[[1]], stmt1)
  # Second statement should have temp inserted before it
  expect_true(length(result$block$stmts) >= 3)
})
