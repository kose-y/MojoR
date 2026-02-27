# =============================================================================
# Test IR LICM (Loop-Invariant Code Motion)
# =============================================================================

test_that("variable reference detection works", {  x <- .mojor_ir_var("x")
  y <- .mojor_ir_var("y")
  i <- .mojor_ir_var("i")

  # Simple variable reference
  expect_true(.mojor_ir_references_var(i, "i"))
  expect_false(.mojor_ir_references_var(x, "i"))

  # Binop
  expr <- .mojor_ir_binop("+", x, i)
  expect_true(.mojor_ir_references_var(expr, "i"))
  expect_false(.mojor_ir_references_var(expr, "j"))

  # Nested
  expr <- .mojor_ir_binop("*", .mojor_ir_binop("+", x, y), i)
  expect_true(.mojor_ir_references_var(expr, "i"))
  expect_false(.mojor_ir_references_var(expr, "z"))
})

test_that("loop-invariant detection for pure expressions", {  x <- .mojor_ir_var("x")
  y <- .mojor_ir_var("y")
  i <- .mojor_ir_var("i")

  # x + y is loop-invariant (doesn't reference i, is pure)
  expr <- .mojor_ir_binop("+", x, y)
  expect_true(.mojor_ir_is_loop_invariant(expr, "i"))

  # i + 1 is NOT loop-invariant (references i)
  expr <- .mojor_ir_binop("+", i, .mojor_ir_const("1"))
  expect_false(.mojor_ir_is_loop_invariant(expr, "i"))

  # sin(x) is loop-invariant
  expr <- .mojor_ir_call("sin", list(x))
  expect_true(.mojor_ir_is_loop_invariant(expr, "i"))

  # sin(i) is NOT loop-invariant
  expr <- .mojor_ir_call("sin", list(i))
  expect_false(.mojor_ir_is_loop_invariant(expr, "i"))
})

test_that("loop-invariant detection for ReadsMem expressions", {  x <- .mojor_ir_var("x")
  y <- .mojor_ir_var("y")
  i <- .mojor_ir_var("i")

  # x[1] reads memory, NOT loop-invariant by default
  expr <- .mojor_ir_index(x, list(.mojor_ir_const("1")))
  expect_false(.mojor_ir_is_loop_invariant(expr, "i", allow_reads = FALSE))

  # x[1] is loop-invariant when allow_reads = TRUE
  expect_true(.mojor_ir_is_loop_invariant(expr, "i", allow_reads = TRUE))

  # x[i] references i, NOT loop-invariant even with allow_reads
  expr <- .mojor_ir_index(x, list(i))
  expect_false(.mojor_ir_is_loop_invariant(expr, "i", allow_reads = TRUE))
})

test_that("loop-invariant detection for RNG expressions", {  n <- .mojor_ir_var("n")
  i <- .mojor_ir_var("i")

  # rnorm(n) is RNG, NOT loop-invariant (even though doesn't reference i)
  expr <- .mojor_ir_call("rnorm", list(n))
  expect_false(.mojor_ir_is_loop_invariant(expr, "i"))
  expect_false(.mojor_ir_is_loop_invariant(expr, "i", allow_reads = TRUE))
})

test_that("LICM hoists loop-invariant expressions", {  x <- .mojor_ir_var("x")
  y <- .mojor_ir_var("y")
  i <- .mojor_ir_var("i")
  out <- .mojor_ir_var("out")

  # for (i in 1:10) { out[i] <- (x + y) * (x + y) }
  add1 <- .mojor_ir_binop("+", x, y)
  add2 <- .mojor_ir_binop("+", x, y)
  mul_expr <- .mojor_ir_binop("*", add1, add2)

  lhs <- .mojor_ir_index(out, list(i))
  assign_stmt <- .mojor_ir_assign(lhs, mul_expr)
  body <- .mojor_ir_block(list(assign_stmt))

  range <- .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10"))
  loop <- .mojor_ir_loop("i", range, body)

  result <- .mojor_ir_licm_loop(loop)

  # Should hoist (x + y) - appears twice, and possibly the multiplication too
  expect_gt(length(result$stmts), 0)
  expect_equal(result$stmts[[1]]$kind, "assign")
  expect_equal(result$stmts[[1]]$lhs$name, "__licm_tmp1")

  # Loop body should be transformed (RHS uses hoisted temps)
  # The entire expression is loop-invariant, so it might be fully hoisted
  expect_true(result$loop$body$stmts[[1]]$rhs$kind %in% c("var", "binop"))
})

test_that("LICM does not hoist expressions referencing loop var", {  x <- .mojor_ir_var("x")
  i <- .mojor_ir_var("i")
  out <- .mojor_ir_var("out")

  # for (i in 1:10) { out[i] <- x + i }
  # x + i should NOT be hoisted (references i)
  add_expr <- .mojor_ir_binop("+", x, i)

  lhs <- .mojor_ir_index(out, list(i))
  assign_stmt <- .mojor_ir_assign(lhs, add_expr)
  body <- .mojor_ir_block(list(assign_stmt))

  range <- .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10"))
  loop <- .mojor_ir_loop("i", range, body)

  result <- .mojor_ir_licm_loop(loop)

  # Should NOT hoist anything
  expect_equal(length(result$stmts), 0)
})

test_that("LICM does not hoist RNG expressions", {  n <- .mojor_ir_var("n")
  i <- .mojor_ir_var("i")
  out <- .mojor_ir_var("out")

  # for (i in 1:10) { out[i] <- rnorm(n) }
  # rnorm(n) should NOT be hoisted (RNG must evaluate fresh)
  rng_expr <- .mojor_ir_call("rnorm", list(n))

  lhs <- .mojor_ir_index(out, list(i))
  assign_stmt <- .mojor_ir_assign(lhs, rng_expr)
  body <- .mojor_ir_block(list(assign_stmt))

  range <- .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10"))
  loop <- .mojor_ir_loop("i", range, body)

  result <- .mojor_ir_licm_loop(loop)

  # Should NOT hoist RNG
  expect_equal(length(result$stmts), 0)
})

test_that("LICM hoists pure invariants even when loop also contains RNG", {  x <- .mojor_ir_var("x")
  n <- .mojor_ir_var("n")
  i <- .mojor_ir_var("i")
  tmp <- .mojor_ir_var("tmp")
  out <- .mojor_ir_var("out")

  # for (i in 1:10) {
  #   tmp <- sin(x)               # invariant pure expression
  #   out[i] <- tmp + rnorm(n)    # RNG must stay in-loop
  # }
  stmt1 <- .mojor_ir_assign(tmp, .mojor_ir_call("sin", list(x)))
  stmt2 <- .mojor_ir_assign(
    .mojor_ir_index(out, list(i)),
    .mojor_ir_binop("+", tmp, .mojor_ir_call("rnorm", list(n)))
  )

  loop <- .mojor_ir_loop(
    "i",
    .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10")),
    .mojor_ir_block(list(stmt1, stmt2))
  )

  result <- .mojor_ir_licm_loop(loop)

  # sin(x) should be hoisted, RNG call should remain in-loop.
  expect_gte(length(result$stmts), 1)
  expect_equal(result$stmts[[1]]$kind, "assign")
  expect_true(grepl("^__licm_tmp", result$stmts[[1]]$lhs$name))
  loop_rng <- result$loop$body$stmts[[2]]$rhs$rhs
  expect_equal(loop_rng$kind, "call")
  expect_equal(loop_rng$fn, "rnorm")
})

test_that("LICM hoists math functions", {  a <- .mojor_ir_var("a")
  b <- .mojor_ir_var("b")
  i <- .mojor_ir_var("i")
  out <- .mojor_ir_var("out")

  # for (i in 1:10) { out[i] <- sin(a) * cos(b) }
  sin_expr <- .mojor_ir_call("sin", list(a))
  cos_expr <- .mojor_ir_call("cos", list(b))
  mul_expr <- .mojor_ir_binop("*", sin_expr, cos_expr)

  lhs <- .mojor_ir_index(out, list(i))
  assign_stmt <- .mojor_ir_assign(lhs, mul_expr)
  body <- .mojor_ir_block(list(assign_stmt))

  range <- .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10"))
  loop <- .mojor_ir_loop("i", range, body)

  result <- .mojor_ir_licm_loop(loop)

  # Should hoist sin(a), cos(b), and possibly their multiplication (all loop-invariant)
  expect_gte(length(result$stmts), 2)  # At least sin(a) and cos(b)
  expect_equal(result$stmts[[1]]$lhs$name, "__licm_tmp1")
  expect_equal(result$stmts[[2]]$lhs$name, "__licm_tmp2")
})

test_that("LICM works on blocks with multiple loops", {  x <- .mojor_ir_var("x")
  y <- .mojor_ir_var("y")
  i <- .mojor_ir_var("i")
  out1 <- .mojor_ir_var("out1")
  out2 <- .mojor_ir_var("out2")

  # for (i in 1:10) { out1[i] <- sin(x) }
  # for (i in 1:10) { out2[i] <- cos(y) }

  sin_expr <- .mojor_ir_call("sin", list(x))
  lhs1 <- .mojor_ir_index(out1, list(i))
  assign1 <- .mojor_ir_assign(lhs1, sin_expr)
  body1 <- .mojor_ir_block(list(assign1))
  range1 <- .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10"))
  loop1 <- .mojor_ir_loop("i", range1, body1)

  cos_expr <- .mojor_ir_call("cos", list(y))
  lhs2 <- .mojor_ir_index(out2, list(i))
  assign2 <- .mojor_ir_assign(lhs2, cos_expr)
  body2 <- .mojor_ir_block(list(assign2))
  range2 <- .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10"))
  loop2 <- .mojor_ir_loop("i", range2, body2)

  block <- .mojor_ir_block(list(loop1, loop2))

  result <- .mojor_ir_licm_block(block)

  # Should have 4 statements: tmp1, loop1, tmp2, loop2
  expect_equal(length(result$block$stmts), 4)
  expect_equal(result$block$stmts[[1]]$kind, "assign")  # tmp1
  expect_equal(result$block$stmts[[2]]$kind, "loop")    # loop1
  expect_equal(result$block$stmts[[3]]$kind, "assign")  # tmp2
  expect_equal(result$block$stmts[[4]]$kind, "loop")    # loop2
})

test_that("LICM preserves non-loop statements", {  x <- .mojor_ir_var("x")
  y <- .mojor_ir_var("y")
  i <- .mojor_ir_var("i")
  out <- .mojor_ir_var("out")
  a <- .mojor_ir_var("a")

  # a <- x + y
  # for (i in 1:10) { out[i] <- sin(x) }

  assign_a <- .mojor_ir_assign(a, .mojor_ir_binop("+", x, y))

  sin_expr <- .mojor_ir_call("sin", list(x))
  lhs <- .mojor_ir_index(out, list(i))
  assign_loop <- .mojor_ir_assign(lhs, sin_expr)
  body <- .mojor_ir_block(list(assign_loop))
  range <- .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10"))
  loop <- .mojor_ir_loop("i", range, body)

  block <- .mojor_ir_block(list(assign_a, loop))

  result <- .mojor_ir_licm_block(block)

  # Should have 3 statements: assign_a, tmp1, loop
  expect_equal(length(result$block$stmts), 3)
  expect_equal(result$block$stmts[[1]], assign_a)  # Original assignment
  expect_equal(result$block$stmts[[2]]$kind, "assign")  # Hoisted tmp
  expect_equal(result$block$stmts[[3]]$kind, "loop")    # Loop
})

test_that("LICM handles nested loops", {  x <- .mojor_ir_var("x")
  i <- .mojor_ir_var("i")
  j <- .mojor_ir_var("j")
  out <- .mojor_ir_var("out")

  # for (i in 1:10) {
  #   for (j in 1:10) {
  #     out[j] <- sin(x)  # sin(x) invariant to both i and j
  #   }
  # }

  sin_expr <- .mojor_ir_call("sin", list(x))
  lhs <- .mojor_ir_index(out, list(j))
  assign_inner <- .mojor_ir_assign(lhs, sin_expr)
  inner_body <- .mojor_ir_block(list(assign_inner))
  inner_range <- .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10"))
  inner_loop <- .mojor_ir_loop("j", inner_range, inner_body)

  outer_body <- .mojor_ir_block(list(inner_loop))
  outer_range <- .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10"))
  outer_loop <- .mojor_ir_loop("i", outer_range, outer_body)

  block <- .mojor_ir_block(list(outer_loop))

  result <- .mojor_ir_licm_block(block)

  # LICM processes loops independently, so sin(x) is hoisted from inner loop
  # (to before inner loop, but still inside outer loop)
  # To hoist all the way out requires iterative LICM
  expect_equal(length(result$block$stmts), 1)  # outer_loop
  expect_equal(result$block$stmts[[1]]$kind, "loop")

  # Inner loop should have hoisted temp before it (inside outer loop body)
  outer_body_stmts <- result$block$stmts[[1]]$body$stmts
  expect_gte(length(outer_body_stmts), 1)  # At least the inner loop
})

test_that("LICM handles if statements recursively", {  x <- .mojor_ir_var("x")
  cond <- .mojor_ir_var("cond")
  i <- .mojor_ir_var("i")
  out <- .mojor_ir_var("out")

  # if (cond) {
  #   for (i in 1:10) { out[i] <- sin(x) }
  # }

  sin_expr <- .mojor_ir_call("sin", list(x))
  lhs <- .mojor_ir_index(out, list(i))
  assign_stmt <- .mojor_ir_assign(lhs, sin_expr)
  body <- .mojor_ir_block(list(assign_stmt))
  range <- .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10"))
  loop <- .mojor_ir_loop("i", range, body)

  then_block <- .mojor_ir_block(list(loop))
  if_stmt <- .mojor_ir_if(cond, then_block)

  block <- .mojor_ir_block(list(if_stmt))

  result <- .mojor_ir_licm_block(block)

  # Should hoist inside the if branch
  expect_equal(length(result$block$stmts), 1)
  expect_equal(result$block$stmts[[1]]$kind, "if")
  # Then branch should have: tmp1, loop
  expect_equal(length(result$block$stmts[[1]]$then$stmts), 2)
})

test_that("LICM with allow_reads = FALSE (default)", {  x <- .mojor_ir_var("x")
  i <- .mojor_ir_var("i")
  out <- .mojor_ir_var("out")

  # for (i in 1:10) { out[i] <- x[1] }
  # x[1] should NOT be hoisted (ReadsMem, allow_reads = FALSE)

  index_expr <- .mojor_ir_index(x, list(.mojor_ir_const("1")))
  lhs <- .mojor_ir_index(out, list(i))
  assign_stmt <- .mojor_ir_assign(lhs, index_expr)
  body <- .mojor_ir_block(list(assign_stmt))
  range <- .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10"))
  loop <- .mojor_ir_loop("i", range, body)

  result <- .mojor_ir_licm_loop(loop, temp_counter = 0, allow_reads = FALSE)

  # Should NOT hoist (ReadsMem)
  expect_equal(length(result$stmts), 0)
})

test_that("LICM with allow_reads = TRUE", {  x <- .mojor_ir_var("x")
  i <- .mojor_ir_var("i")
  out <- .mojor_ir_var("out")

  # for (i in 1:10) { out[i] <- x[1] }
  # x[1] SHOULD be hoisted with allow_reads = TRUE

  index_expr <- .mojor_ir_index(x, list(.mojor_ir_const("1")))
  lhs <- .mojor_ir_index(out, list(i))
  assign_stmt <- .mojor_ir_assign(lhs, index_expr)
  body <- .mojor_ir_block(list(assign_stmt))
  range <- .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10"))
  loop <- .mojor_ir_loop("i", range, body)

  result <- .mojor_ir_licm_loop(loop, temp_counter = 0, allow_reads = TRUE)

  # Should hoist x[1]
  expect_equal(length(result$stmts), 1)
  expect_equal(result$stmts[[1]]$lhs$name, "__licm_tmp1")
  expect_equal(result$stmts[[1]]$rhs$kind, "index")
})

test_that("LICM handles while loops", {  x <- .mojor_ir_var("x")
  cond <- .mojor_ir_var("cond")
  i <- .mojor_ir_var("i")

  # while (cond) { i <- sin(x) }
  # sin(x) should NOT be hoisted from while loop (loop var unclear)

  sin_expr <- .mojor_ir_call("sin", list(x))
  assign_stmt <- .mojor_ir_assign(i, sin_expr)
  body <- .mojor_ir_block(list(assign_stmt))
  while_loop <- .mojor_ir_while(cond, body)

  block <- .mojor_ir_block(list(while_loop))

  result <- .mojor_ir_licm_block(block)

  # While loop doesn't have explicit loop var, so LICM doesn't apply
  # (pass through unchanged)
  expect_equal(length(result$block$stmts), 1)
  expect_equal(result$block$stmts[[1]]$kind, "while")
})

test_that("LICM with empty loop body", {  i <- .mojor_ir_var("i")

  # for (i in 1:10) { }
  body <- .mojor_ir_block(list())
  range <- .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10"))
  loop <- .mojor_ir_loop("i", range, body)

  result <- .mojor_ir_licm_loop(loop)

  # No statements to hoist
  expect_equal(length(result$stmts), 0)
})

test_that("LICM with NULL expressions", {  expect_false(.mojor_ir_references_var(NULL, "i"))
  expect_false(.mojor_ir_is_loop_invariant(NULL, "i"))
})

test_that("LICM combines with CSE", {  x <- .mojor_ir_var("x")
  y <- .mojor_ir_var("y")
  i <- .mojor_ir_var("i")
  out <- .mojor_ir_var("out")

  # for (i in 1:10) { out[i] <- (x + y) * (x + y) }
  # CSE first: extract (x + y) to tmp
  # LICM second: hoist tmp outside loop

  add1 <- .mojor_ir_binop("+", x, y)
  add2 <- .mojor_ir_binop("+", x, y)
  mul_expr <- .mojor_ir_binop("*", add1, add2)

  lhs <- .mojor_ir_index(out, list(i))
  assign_stmt <- .mojor_ir_assign(lhs, mul_expr)
  body <- .mojor_ir_block(list(assign_stmt))
  range <- .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10"))
  loop <- .mojor_ir_loop("i", range, body)

  block <- .mojor_ir_block(list(loop))

  # Apply CSE first
  cse_result <- .mojor_ir_cse_block(block)

  # Then apply LICM
  licm_result <- .mojor_ir_licm_block(cse_result$block)

  # Should have CSE temp + LICM hoist
  # Final: licm_tmp, loop with cse_tmp inside
  expect_gt(length(licm_result$block$stmts), 1)
})
