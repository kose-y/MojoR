test_that("collect var refs finds all variable references in expressions", {  # Simple var
  expr <- .mojor_ir_var("x")
  refs <- .mojor_ir_collect_var_refs(expr)
  expect_true("x" %in% refs)

  # Const (no refs)
  expr <- .mojor_ir_const("1.0")
  refs <- .mojor_ir_collect_var_refs(expr)
  expect_length(refs, 0)

  # Binop
  expr <- .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_var("y"))
  refs <- .mojor_ir_collect_var_refs(expr)
  expect_true("x" %in% refs)
  expect_true("y" %in% refs)

  # Nested
  expr <- .mojor_ir_binop(
    "*",
    .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_var("y")),
    .mojor_ir_var("z")
  )
  refs <- .mojor_ir_collect_var_refs(expr)
  expect_true("x" %in% refs)
  expect_true("y" %in% refs)
  expect_true("z" %in% refs)
})

test_that("collect var refs finds references in call nodes", {  expr <- .mojor_ir_call("sin", list(.mojor_ir_var("x")))
  refs <- .mojor_ir_collect_var_refs(expr)
  expect_true("x" %in% refs)

  # Multiple args
  expr <- .mojor_ir_call("atan2", list(.mojor_ir_var("y"), .mojor_ir_var("x")))
  refs <- .mojor_ir_collect_var_refs(expr)
  expect_true("x" %in% refs)
  expect_true("y" %in% refs)
})

test_that("collect var refs finds references in index nodes", {  expr <- .mojor_ir_index(
    .mojor_ir_var("x"),
    list(.mojor_ir_var("i"))
  )
  refs <- .mojor_ir_collect_var_refs(expr)
  expect_true("x" %in% refs)
  expect_true("i" %in% refs)

  # Matrix index
  expr <- .mojor_ir_index(
    .mojor_ir_var("mat"),
    list(.mojor_ir_var("i"), .mojor_ir_var("j"))
  )
  refs <- .mojor_ir_collect_var_refs(expr)
  expect_true("mat" %in% refs)
  expect_true("i" %in% refs)
  expect_true("j" %in% refs)
})

test_that("collect stmt refs finds references in assign statements", {  # Simple assign
  stmt <- .mojor_ir_assign(
    .mojor_ir_var("out"),
    .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_var("y"))
  )
  refs <- .mojor_ir_collect_stmt_refs(stmt)
  expect_true("x" %in% refs)
  expect_true("y" %in% refs)
  # LHS should NOT be in refs (we're looking for uses, not definitions)
  expect_false("out" %in% refs)
})

test_that("collect stmt refs finds references in subscript_assign statements", {  # out[i] <- x + y
  stmt <- .mojor_ir_assign(
    .mojor_ir_subscript("out", list(.mojor_ir_var("i"))),
    .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_var("y"))
  )
  refs <- .mojor_ir_collect_stmt_refs(stmt)
  expect_true("i" %in% refs)  # Index is a use
  expect_true("x" %in% refs)
  expect_true("y" %in% refs)
  expect_true("out" %in% refs)  # Base of subscript is a use
})

test_that("collect block refs finds all references in a block", {  # Block with multiple statements
  block <- .mojor_ir_block(list(
    .mojor_ir_assign(.mojor_ir_var("tmp"), .mojor_ir_var("x")),
    .mojor_ir_assign(.mojor_ir_var("out"), .mojor_ir_var("tmp"))
  ))
  refs <- .mojor_ir_collect_block_refs(block)
  expect_true("x" %in% refs)
  expect_true("tmp" %in% refs)
})

test_that("is_dead_stmt identifies unused pure assignments", {  # Dead: tmp <- x + y, tmp not used
  stmt <- .mojor_ir_assign(
    .mojor_ir_var("tmp"),
    .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_var("y"))
  )
  expect_true(.mojor_ir_is_dead_stmt(stmt, c("x", "y")))

  # Live: tmp is used
  expect_false(.mojor_ir_is_dead_stmt(stmt, c("x", "y", "tmp")))
})

test_that("is_dead_stmt keeps subscript_assign statements", {  # out[i] <- x + y is never dead (WritesMem)
  stmt <- .mojor_ir_assign(
    .mojor_ir_subscript("out", list(.mojor_ir_var("i"))),
    .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_var("y"))
  )
  expect_false(.mojor_ir_is_dead_stmt(stmt, c("x", "y")))
})

test_that("is_dead_stmt keeps RNG assignments", {  # tmp <- rnorm(1) is never dead (RNG effect)
  stmt <- .mojor_ir_assign(
    .mojor_ir_var("tmp"),
    .mojor_ir_call("rnorm", list(.mojor_ir_const("1")))
  )
  expect_false(.mojor_ir_is_dead_stmt(stmt, character()))
})

test_that("is_dead_stmt keeps ReadsMem assignments", {  # tmp <- x[i] is never dead without alias analysis
  stmt <- .mojor_ir_assign(
    .mojor_ir_var("tmp"),
    .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
  )
  expect_false(.mojor_ir_is_dead_stmt(stmt, c("x", "i")))
})

test_that("DCE removes unused pure assignments", {  # Block:
  #   tmp <- x + y
  #   out <- z
  # tmp is not used, should be removed
  block <- .mojor_ir_block(list(
    .mojor_ir_assign(.mojor_ir_var("tmp"), .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_var("y"))),
    .mojor_ir_assign(.mojor_ir_var("out"), .mojor_ir_var("z"))
  ))

  result <- .mojor_ir_dce_block(block, live_after = c("out"))
  expect_length(result$stmts, 1)
  expect_equal(result$stmts[[1]]$lhs$name, "out")
})

test_that("DCE keeps used assignments", {  # Block:
  #   tmp <- x + y
  #   out <- tmp * 2
  # tmp is used, should be kept
  block <- .mojor_ir_block(list(
    .mojor_ir_assign(.mojor_ir_var("tmp"), .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_var("y"))),
    .mojor_ir_assign(.mojor_ir_var("out"), .mojor_ir_binop("*", .mojor_ir_var("tmp"), .mojor_ir_const("2")))
  ))

  result <- .mojor_ir_dce_block(block)
  expect_length(result$stmts, 2)
  expect_equal(result$stmts[[1]]$lhs$name, "tmp")
  expect_equal(result$stmts[[2]]$lhs$name, "out")
})

test_that("DCE removes multiple dead assignments", {  # Block:
  #   tmp1 <- x + y
  #   tmp2 <- a * b
  #   out <- z
  # tmp1 and tmp2 are not used
  block <- .mojor_ir_block(list(
    .mojor_ir_assign(.mojor_ir_var("tmp1"), .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_var("y"))),
    .mojor_ir_assign(.mojor_ir_var("tmp2"), .mojor_ir_binop("*", .mojor_ir_var("a"), .mojor_ir_var("b"))),
    .mojor_ir_assign(.mojor_ir_var("out"), .mojor_ir_var("z"))
  ))

  result <- .mojor_ir_dce_block(block, live_after = c("out"))
  expect_length(result$stmts, 1)
  expect_equal(result$stmts[[1]]$lhs$name, "out")
})

test_that("DCE keeps RNG assignments even if unused", {  # Block:
  #   tmp <- rnorm(1)
  #   out <- x
  # tmp has RNG effect, must keep
  block <- .mojor_ir_block(list(
    .mojor_ir_assign(.mojor_ir_var("tmp"), .mojor_ir_call("rnorm", list(.mojor_ir_const("1")))),
    .mojor_ir_assign(.mojor_ir_var("out"), .mojor_ir_var("x"))
  ))

  result <- .mojor_ir_dce_block(block)
  expect_length(result$stmts, 2)  # Both kept
})

test_that("DCE keeps ReadsMem assignments even if unused", {  # Block:
  #   tmp <- x[i]
  #   out <- y
  # tmp has ReadsMem effect, must keep
  block <- .mojor_ir_block(list(
    .mojor_ir_assign(.mojor_ir_var("tmp"), .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))),
    .mojor_ir_assign(.mojor_ir_var("out"), .mojor_ir_var("y"))
  ))

  result <- .mojor_ir_dce_block(block)
  expect_length(result$stmts, 2)  # Both kept
})

test_that("DCE works recursively on if statements", {  # if (cond) {
  #   tmp <- x + y
  #   out <- z
  # }
  # tmp is unused in yes branch
  yes_block <- .mojor_ir_block(list(
    .mojor_ir_assign(.mojor_ir_var("tmp"), .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_var("y"))),
    .mojor_ir_assign(.mojor_ir_var("out"), .mojor_ir_var("z"))
  ))
  stmt <- .mojor_ir_if(.mojor_ir_var("cond"), yes_block, NULL)

  # Wrap in block
  block <- .mojor_ir_block(list(stmt))
  result <- .mojor_ir_dce_block(block, live_after = c("out"))

  # Should have removed tmp from then branch
  then_result <- result$stmts[[1]]$then
  expect_length(then_result$stmts, 1)
  expect_equal(then_result$stmts[[1]]$lhs$name, "out")
})

test_that("DCE works on if/else statements", {  # if (cond) {
  #   tmp1 <- x
  #   out <- tmp1
  # } else {
  #   tmp2 <- y
  #   out <- z
  # }
  # tmp1 is used, tmp2 is not
  yes_block <- .mojor_ir_block(list(
    .mojor_ir_assign(.mojor_ir_var("tmp1"), .mojor_ir_var("x")),
    .mojor_ir_assign(.mojor_ir_var("out"), .mojor_ir_var("tmp1"))
  ))
  no_block <- .mojor_ir_block(list(
    .mojor_ir_assign(.mojor_ir_var("tmp2"), .mojor_ir_var("y")),
    .mojor_ir_assign(.mojor_ir_var("out"), .mojor_ir_var("z"))
  ))
  stmt <- .mojor_ir_if(.mojor_ir_var("cond"), yes_block, no_block)

  block <- .mojor_ir_block(list(stmt))
  result <- .mojor_ir_dce_block(block, live_after = c("out"))

  # Then branch should keep tmp1
  then_result <- result$stmts[[1]]$then
  expect_length(then_result$stmts, 2)

  # Else branch should remove tmp2
  else_result <- result$stmts[[1]]$else_block
  expect_length(else_result$stmts, 1)
  expect_equal(else_result$stmts[[1]]$lhs$name, "out")
})

test_that("DCE works recursively on loops", {  # for (i in 1:10) {
  #   tmp <- x + y
  #   out[i] <- z
  # }
  # tmp is unused
  body <- .mojor_ir_block(list(
    .mojor_ir_assign(.mojor_ir_var("tmp"), .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_var("y"))),
    .mojor_ir_assign(
      .mojor_ir_subscript("out", list(.mojor_ir_var("i"))),
      .mojor_ir_var("z")
    )
  ))
  range <- .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10"), NULL, "inclusive")
  loop <- .mojor_ir_loop("i", range, body)

  block <- .mojor_ir_block(list(loop))
  result <- .mojor_ir_dce_block(block)

  # Loop body must retain the array write even if temporary assignments remain.
  body_result <- result$stmts[[1]]$body
  expect_true(any(vapply(body_result$stmts, function(stmt) {
    identical(stmt$kind, "assign") &&
      !is.null(stmt$lhs$kind) &&
      identical(stmt$lhs$kind, "subscript")
  }, logical(1))))
})

test_that("DCE works on while loops", {  # while (cond) {
  #   tmp <- x
  #   y <- y + 1
  # }
  # tmp is unused
  body <- .mojor_ir_block(list(
    .mojor_ir_assign(.mojor_ir_var("tmp"), .mojor_ir_var("x")),
    .mojor_ir_assign(.mojor_ir_var("y"), .mojor_ir_binop("+", .mojor_ir_var("y"), .mojor_ir_const("1")))
  ))
  stmt <- .mojor_ir_while(.mojor_ir_var("cond"), body)

  block <- .mojor_ir_block(list(stmt))
  result <- .mojor_ir_dce_block(block, live_after = c("y"))

  # While body must preserve the y update.
  body_result <- result$stmts[[1]]$body
  expect_true(any(vapply(body_result$stmts, function(stmt) {
    identical(stmt$kind, "assign") &&
      !is.null(stmt$lhs$name) &&
      identical(stmt$lhs$name, "y")
  }, logical(1))))
})

test_that("DCE handles empty blocks", {  block <- .mojor_ir_block(list())
  result <- .mojor_ir_dce_block(block)
  expect_length(result$stmts, 0)
})

test_that("DCE handles NULL gracefully", {  result <- .mojor_ir_dce_block(NULL)
  expect_null(result)
})

test_that("DCE handles chain of dependencies", {  # tmp1 <- x
  # tmp2 <- tmp1 + y
  # tmp3 <- tmp2 * 2
  # out <- tmp3
  # All should be kept (chain of uses)
  block <- .mojor_ir_block(list(
    .mojor_ir_assign(.mojor_ir_var("tmp1"), .mojor_ir_var("x")),
    .mojor_ir_assign(.mojor_ir_var("tmp2"), .mojor_ir_binop("+", .mojor_ir_var("tmp1"), .mojor_ir_var("y"))),
    .mojor_ir_assign(.mojor_ir_var("tmp3"), .mojor_ir_binop("*", .mojor_ir_var("tmp2"), .mojor_ir_const("2"))),
    .mojor_ir_assign(.mojor_ir_var("out"), .mojor_ir_var("tmp3"))
  ))

  result <- .mojor_ir_dce_block(block)
  expect_length(result$stmts, 4)  # All kept
})

test_that("DCE handles partially used chain", {  # tmp1 <- x
  # tmp2 <- tmp1 + y
  # tmp3 <- a * b  (unused)
  # out <- tmp2
  # tmp1 and tmp2 used, tmp3 dead
  block <- .mojor_ir_block(list(
    .mojor_ir_assign(.mojor_ir_var("tmp1"), .mojor_ir_var("x")),
    .mojor_ir_assign(.mojor_ir_var("tmp2"), .mojor_ir_binop("+", .mojor_ir_var("tmp1"), .mojor_ir_var("y"))),
    .mojor_ir_assign(.mojor_ir_var("tmp3"), .mojor_ir_binop("*", .mojor_ir_var("a"), .mojor_ir_var("b"))),
    .mojor_ir_assign(.mojor_ir_var("out"), .mojor_ir_var("tmp2"))
  ))

  result <- .mojor_ir_dce_block(block, live_after = c("out"))
  expect_length(result$stmts, 3)  # tmp3 removed
  expect_equal(result$stmts[[1]]$lhs$name, "tmp1")
  expect_equal(result$stmts[[2]]$lhs$name, "tmp2")
  expect_equal(result$stmts[[3]]$lhs$name, "out")
})

test_that("DCE works with nested loops", {  # for (i in 1:10) {
  #   tmp1 <- x
  #   for (j in 1:10) {
  #     tmp2 <- y
  #     out[i, j] <- z
  #   }
  # }
  # Both tmp1 and tmp2 are unused
  inner_body <- .mojor_ir_block(list(
    .mojor_ir_assign(.mojor_ir_var("tmp2"), .mojor_ir_var("y")),
    .mojor_ir_assign(
      .mojor_ir_subscript("out", list(.mojor_ir_var("i"), .mojor_ir_var("j"))),
      .mojor_ir_var("z")
    )
  ))
  inner_range <- .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10"), NULL, "inclusive")
  inner_loop <- .mojor_ir_loop("j", inner_range, inner_body)

  outer_body <- .mojor_ir_block(list(
    .mojor_ir_assign(.mojor_ir_var("tmp1"), .mojor_ir_var("x")),
    inner_loop
  ))
  outer_range <- .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10"), NULL, "inclusive")
  outer_loop <- .mojor_ir_loop("i", outer_range, outer_body)

  block <- .mojor_ir_block(list(outer_loop))
  result <- .mojor_ir_dce_block(block)

  # Outer body should still contain the nested loop.
  outer_body_result <- result$stmts[[1]]$body
  outer_loop_idx <- which(vapply(outer_body_result$stmts, function(stmt) identical(stmt$kind, "loop"), logical(1)))
  expect_true(length(outer_loop_idx) >= 1)

  # Inner body must retain the matrix write.
  inner_body_result <- outer_body_result$stmts[[outer_loop_idx[[1]]]]$body
  expect_true(any(vapply(inner_body_result$stmts, function(stmt) {
    identical(stmt$kind, "assign") &&
      !is.null(stmt$lhs$kind) &&
      identical(stmt$lhs$kind, "subscript")
  }, logical(1))))
})

test_that("DCE removes all dead code when entire block is dead", {  # tmp1 <- x + y
  # tmp2 <- sin(a)
  # tmp3 <- b * c
  # out <- z
  # Only out assignment is live
  block <- .mojor_ir_block(list(
    .mojor_ir_assign(.mojor_ir_var("tmp1"), .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_var("y"))),
    .mojor_ir_assign(.mojor_ir_var("tmp2"), .mojor_ir_call("sin", list(.mojor_ir_var("a")))),
    .mojor_ir_assign(.mojor_ir_var("tmp3"), .mojor_ir_binop("*", .mojor_ir_var("b"), .mojor_ir_var("c"))),
    .mojor_ir_assign(.mojor_ir_var("out"), .mojor_ir_var("z"))
  ))

  result <- .mojor_ir_dce_block(block, live_after = c("out"))
  expect_length(result$stmts, 1)
  expect_equal(result$stmts[[1]]$lhs$name, "out")
})

test_that("DCE preserves order of live statements", {  # tmp1 <- x
  # tmp2 <- y
  # out1 <- tmp1
  # out2 <- tmp2
  # All should be kept in order
  block <- .mojor_ir_block(list(
    .mojor_ir_assign(.mojor_ir_var("tmp1"), .mojor_ir_var("x")),
    .mojor_ir_assign(.mojor_ir_var("tmp2"), .mojor_ir_var("y")),
    .mojor_ir_assign(.mojor_ir_var("out1"), .mojor_ir_var("tmp1")),
    .mojor_ir_assign(.mojor_ir_var("out2"), .mojor_ir_var("tmp2"))
  ))

  result <- .mojor_ir_dce_block(block)
  expect_length(result$stmts, 4)
  expect_equal(result$stmts[[1]]$lhs$name, "tmp1")
  expect_equal(result$stmts[[2]]$lhs$name, "tmp2")
  expect_equal(result$stmts[[3]]$lhs$name, "out1")
  expect_equal(result$stmts[[4]]$lhs$name, "out2")
})
