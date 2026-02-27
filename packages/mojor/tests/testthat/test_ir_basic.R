library(testthat)

test_that("IR can build and emit simple assignment", {  # Simple scalar assignment: x <- 5
  stmt <- quote(x <- 5)
  ir <- .mojor_ir_build_stmt(stmt)

  expect_equal(ir$kind, "assign")
  expect_equal(ir$lhs$kind, "var")
  expect_equal(ir$lhs$name, "x")
  expect_equal(ir$rhs$kind, "const")
  expect_equal(ir$rhs$value, "5.0")

  # Emit it
  mojo <- .mojor_ir_stmt_emit(ir, indent = "    ")
  expect_equal(mojo, "    x = 5.0")
})

test_that("IR can build and emit indexed assignment", {  # Indexed assignment: out[i] <- x[i] + 1
  stmt <- quote(out[i] <- x[i] + 1)
  ir <- .mojor_ir_build_stmt(stmt)

  expect_equal(ir$kind, "assign")
  expect_equal(ir$lhs$kind, "index")
  expect_equal(ir$lhs$base$name, "out")
  expect_equal(ir$rhs$kind, "binop")
  expect_equal(ir$rhs$op, "+")

  # Emit with zero-based loop var
  mojo <- .mojor_ir_stmt_emit(ir, indent = "    ", zero_based_vars = "i")
  expect_true(any(grepl("out\\[", mojo)))  # Has out index
  expect_true(any(grepl("x\\[", mojo)))     # Has x index
  expect_false(any(grepl("i - 1", mojo)))  # Verify no -1 when i is zero-based
})

test_that("IR normalizes 1-based indices to 0-based", {  # Assignment: out[i] <- x[i]
  stmt <- quote(out[i] <- x[i])
  ir <- .mojor_ir_build_stmt(stmt)

  # Emit WITHOUT zero_based_vars - should add -1 and Int()
  mojo <- .mojor_ir_stmt_emit(ir, indent = "    ", zero_based_vars = NULL)
  expect_true(grepl("Int\\(\\(i - 1\\)\\)", mojo))
})

test_that("IR can build block of statements", {  # Block: { x <- 1; y <- 2 }
  block <- quote({ x <- 1; y <- 2 })
  ir <- .mojor_ir_build_block(block)

  expect_equal(ir$kind, "block")
  expect_equal(length(ir$stmts), 2)
  expect_equal(ir$stmts[[1]]$kind, "assign")
  expect_equal(ir$stmts[[2]]$kind, "assign")

  # Emit it
  mojo_lines <- .mojor_ir_block_emit(ir, indent = "    ")
  expect_equal(length(mojo_lines), 2)
  expect_equal(mojo_lines[1], "    x = 1.0")
  expect_equal(mojo_lines[2], "    y = 2.0")
})

test_that("IR handles arithmetic on RHS", {  # Assignment: out[i] <- x[i] * 2 + y[i]
  stmt <- quote(out[i] <- x[i] * 2 + y[i])
  ir <- .mojor_ir_build_stmt(stmt)

  expect_equal(ir$kind, "assign")
  expect_equal(ir$rhs$kind, "binop")
  expect_equal(ir$rhs$op, "+")

  # Emit with zero-based vars
  mojo <- .mojor_ir_stmt_emit(ir, indent = "    ", zero_based_vars = "i")
  expect_true(any(grepl("\\*\\s*2", mojo)))  # Flexible spacing around *
})

test_that("IR can build and emit simple seq_len loop", {  # Loop: for (i in seq_len(n)) { out[i] <- i }
  stmt <- quote(for (i in seq_len(n)) { out[i] <- i })
  ir <- .mojor_ir_build_stmt(stmt)

  expect_equal(ir$kind, "loop")
  expect_equal(ir$var, "i")
  # seq_len(n) with variable n is canonicalized to a structured range node
  expect_true(ir$range$kind %in% c("range_expr", "range"))

  # Emit loop
  mojo_lines <- .mojor_ir_stmt_emit(ir, indent = "    ")
  expect_false(is.null(mojo_lines))
  expect_true(length(mojo_lines) >= 2)  # At least header + one body line
  expect_true(grepl("for i in range", mojo_lines[1]))
  expect_true(grepl("seq_len", mojo_lines[1]) || grepl("range\\(1,", mojo_lines[1]))
})

test_that("IR can build and emit simple 1:n loop", {  # Loop: for (i in 1:10) { x <- i }
  stmt <- quote(for (i in 1:10) { x <- i })
  ir <- .mojor_ir_build_stmt(stmt)

  expect_equal(ir$kind, "loop")
  expect_equal(ir$var, "i")

  # Emit loop
  mojo_lines <- .mojor_ir_stmt_emit(ir, indent = "    ")
  expect_false(is.null(mojo_lines))
  expect_true(grepl("for i in range\\(1,", mojo_lines[1]))  # Check loop header
  # Canonicalized 1:10 emits Int(10) + 1 or 11 or 10 + 1
  expect_true(grepl("10.*1", mojo_lines[1]) || grepl("11", mojo_lines[1]))
  expect_true(grepl("x = i", mojo_lines[2]))
})

test_that("IR can emit loop with indexed assignment", {  # Loop: for (i in seq_len(n)) { out[i] <- x[i] + 1 }
  stmt <- quote(for (i in seq_len(n)) { out[i] <- x[i] + 1 })
  ir <- .mojor_ir_build_stmt(stmt)

  expect_equal(ir$kind, "loop")

  # Emit loop - loop var 'i' is 1-based, so indices should have (i - 1)
  mojo_lines <- .mojor_ir_stmt_emit(ir, indent = "    ")
  expect_false(is.null(mojo_lines))
  # Check loop header
  expect_true(grepl("for i in range", mojo_lines[1]))
  # Check body has indexed assignment
  body_line <- paste(mojo_lines[-1], collapse = "\n")
  expect_true(grepl("out\\[", body_line))  # Has indexed assignment
  expect_true(grepl("i - 1", body_line))    # Has i-1 for 1-based loop var
})

test_that("IR can emit seq_along loop", {  # Loop: for (i in seq_along(x)) { out[i] <- x[i] }
  stmt <- quote(for (i in seq_along(x)) { out[i] <- x[i] })
  ir <- .mojor_ir_build_stmt(stmt)

  expect_equal(ir$kind, "loop")

  prev_n_source_name <- .mojor_state$current_n_source_name
  prev_len_var_map <- .mojor_state$current_len_var_map
  .mojor_state$current_n_source_name <- "x"
  .mojor_state$current_len_var_map <- list()
  on.exit({
    .mojor_state$current_n_source_name <- prev_n_source_name
    .mojor_state$current_len_var_map <- prev_len_var_map
  }, add = TRUE)

  # Emit loop
  mojo_lines <- .mojor_ir_stmt_emit(ir, indent = "    ")
  expect_false(is.null(mojo_lines))
  expect_true(grepl("for i in range\\(1, \\(n_i \\+ 1\\)\\)", mojo_lines[1]))
})

test_that("IR can build and emit simple if statement with comparison", {  # If statement: if (x > 0) { y <- 1 } else { y <- 0 }
  stmt <- quote(if (x > 0) { y <- 1 } else { y <- 0 })
  ir <- .mojor_ir_build_stmt(stmt)

  expect_equal(ir$kind, "if")
  expect_equal(ir$cond$kind, "binop")
  expect_equal(ir$cond$op, ">")

  # Emit if statement
  mojo_lines <- .mojor_ir_stmt_emit(ir, indent = "    ")
  expect_false(is.null(mojo_lines))
  expect_true(grepl("if.*x.*>.*0.*:", mojo_lines[1]))  # Mojo format, flexible spacing
  expect_true(grepl("y = 1.0", mojo_lines[2]))
  expect_true(grepl("else:", mojo_lines[3]))
  expect_true(grepl("y = 0.0", mojo_lines[4]))
})

test_that("IR can emit if statement without else", {  # If statement: if (x > 0) { y <- 1 }
  stmt <- quote(if (x > 0) { y <- 1 })
  ir <- .mojor_ir_build_stmt(stmt)

  expect_equal(ir$kind, "if")
  expect_null(ir$else_block)

  # Emit if statement
  mojo_lines <- .mojor_ir_stmt_emit(ir, indent = "    ")
  expect_false(is.null(mojo_lines))
  expect_true(grepl("if.*x.*>.*0.*:", mojo_lines[1]))
  expect_true(grepl("y = 1.0", mojo_lines[2]))
  expect_equal(length(mojo_lines), 2)  # No else block
})

test_that("IR adds boolean coercion for non-comparison conditions", {  # If statement: if (x) { y <- 1 }
  stmt <- quote(if (x) { y <- 1 })
  ir <- .mojor_ir_build_stmt(stmt)

  expect_equal(ir$kind, "if")
  expect_equal(ir$cond$kind, "var")

  # Emit if statement - should add != 0 coercion
  mojo_lines <- .mojor_ir_stmt_emit(ir, indent = "    ")
  expect_false(is.null(mojo_lines))
  expect_true(grepl("if \\(x != 0\\):", mojo_lines[1]))  # Coercion added
})

test_that("IR can emit if with logical operators", {  # If statement: if (x > 0 && y > 0) { z <- 1 }
  stmt <- quote(if (x > 0 && y > 0) { z <- 1 })
  ir <- .mojor_ir_build_stmt(stmt)

  expect_equal(ir$kind, "if")
  expect_equal(ir$cond$kind, "binop")
  expect_equal(ir$cond$op, "&&")

  # Emit if statement
  mojo_lines <- .mojor_ir_stmt_emit(ir, indent = "    ")
  expect_false(is.null(mojo_lines))
  expect_true(grepl("if.*x.*>.*0.*and.*y.*>.*0.*:", mojo_lines[1]))  # Mojo format
})

test_that("IR can emit if with indexed assignments", {  # If statement: if (i > 5) { out[i] <- x[i] }
  stmt <- quote(if (i > 5) { out[i] <- x[i] })
  ir <- .mojor_ir_build_stmt(stmt)

  expect_equal(ir$kind, "if")

  # Emit if statement - indices should use (i - 1) for 1-based loop var
  mojo_lines <- .mojor_ir_stmt_emit(ir, indent = "    ")
  expect_false(is.null(mojo_lines))
  expect_true(grepl("if.*i.*>.*5.*:", mojo_lines[1]))
  expect_true(grepl("out\\[Int\\(\\(i - 1\\)\\)\\]", mojo_lines[2]))
})

# =============================================================================
# Reduction Pattern Detection Tests (inline reductions)
# =============================================================================

test_that("detect_reduction_pattern identifies sum pattern (acc <- acc + expr)", {  # for (i in seq_along(x)) { acc <- acc + x[i] }
  loop <- .mojor_ir_loop(
    var = "i",
    range = .mojor_ir_range_expr(quote(seq_along(x))),
    body = .mojor_ir_block(list(
      .mojor_ir_assign(
        .mojor_ir_var("acc"),
        .mojor_ir_binop("+",
          .mojor_ir_var("acc"),
          .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
        )
      )
    ))
  )

  info <- .mojor_ir_detect_reduction_pattern(loop)
  expect_false(is.null(info))
  expect_equal(info$kind, "sum")
  expect_equal(info$acc, "acc")
  expect_equal(info$op, "+")
  expect_equal(info$rhs$kind, "index")
})

test_that("detect_reduction_pattern identifies sum pattern reversed (acc <- expr + acc)", {  # for (i in seq_along(x)) { acc <- x[i] + acc }  (commutative)
  loop <- .mojor_ir_loop(
    var = "i",
    range = .mojor_ir_range_expr(quote(seq_along(x))),
    body = .mojor_ir_block(list(
      .mojor_ir_assign(
        .mojor_ir_var("acc"),
        .mojor_ir_binop("+",
          .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i"))),
          .mojor_ir_var("acc")
        )
      )
    ))
  )

  info <- .mojor_ir_detect_reduction_pattern(loop)
  expect_false(is.null(info))
  expect_equal(info$kind, "sum")
  expect_equal(info$acc, "acc")
  expect_equal(info$op, "+")
  expect_equal(info$rhs$kind, "index")
})

test_that("detect_reduction_pattern identifies product pattern", {  # for (i in seq_along(x)) { acc <- acc * x[i] }
  loop <- .mojor_ir_loop(
    var = "i",
    range = .mojor_ir_range_expr(quote(seq_along(x))),
    body = .mojor_ir_block(list(
      .mojor_ir_assign(
        .mojor_ir_var("acc"),
        .mojor_ir_binop("*",
          .mojor_ir_var("acc"),
          .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
        )
      )
    ))
  )

  info <- .mojor_ir_detect_reduction_pattern(loop)
  expect_false(is.null(info))
  expect_equal(info$kind, "product")
  expect_equal(info$acc, "acc")
  expect_equal(info$op, "*")
})

test_that("detect_reduction_pattern identifies product pattern reversed", {  # for (i in seq_along(x)) { acc <- x[i] * acc }
  loop <- .mojor_ir_loop(
    var = "i",
    range = .mojor_ir_range_expr(quote(seq_along(x))),
    body = .mojor_ir_block(list(
      .mojor_ir_assign(
        .mojor_ir_var("acc"),
        .mojor_ir_binop("*",
          .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i"))),
          .mojor_ir_var("acc")
        )
      )
    ))
  )

  info <- .mojor_ir_detect_reduction_pattern(loop)
  expect_false(is.null(info))
  expect_equal(info$kind, "product")
  expect_equal(info$acc, "acc")
  expect_equal(info$op, "*")
})

test_that("detect_reduction_pattern identifies min pattern (acc first)", {  # for (i in seq_along(x)) { acc <- min(acc, x[i]) }
  loop <- .mojor_ir_loop(
    var = "i",
    range = .mojor_ir_range_expr(quote(seq_along(x))),
    body = .mojor_ir_block(list(
      .mojor_ir_assign(
        .mojor_ir_var("acc"),
        .mojor_ir_call("min", list(
          .mojor_ir_var("acc"),
          .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
        ))
      )
    ))
  )

  info <- .mojor_ir_detect_reduction_pattern(loop)
  expect_false(is.null(info))
  expect_equal(info$kind, "min")
  expect_equal(info$acc, "acc")
  expect_equal(info$op, "min")
  expect_equal(info$rhs$kind, "index")
})

test_that("detect_reduction_pattern identifies min pattern (acc second)", {  # for (i in seq_along(x)) { acc <- min(x[i], acc) }
  loop <- .mojor_ir_loop(
    var = "i",
    range = .mojor_ir_range_expr(quote(seq_along(x))),
    body = .mojor_ir_block(list(
      .mojor_ir_assign(
        .mojor_ir_var("acc"),
        .mojor_ir_call("min", list(
          .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i"))),
          .mojor_ir_var("acc")
        ))
      )
    ))
  )

  info <- .mojor_ir_detect_reduction_pattern(loop)
  expect_false(is.null(info))
  expect_equal(info$kind, "min")
  expect_equal(info$acc, "acc")
  expect_equal(info$op, "min")
  expect_equal(info$rhs$kind, "index")
})

test_that("detect_reduction_pattern identifies max pattern (acc first)", {  # for (i in seq_along(x)) { acc <- max(acc, x[i]) }
  loop <- .mojor_ir_loop(
    var = "i",
    range = .mojor_ir_range_expr(quote(seq_along(x))),
    body = .mojor_ir_block(list(
      .mojor_ir_assign(
        .mojor_ir_var("acc"),
        .mojor_ir_call("max", list(
          .mojor_ir_var("acc"),
          .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
        ))
      )
    ))
  )

  info <- .mojor_ir_detect_reduction_pattern(loop)
  expect_false(is.null(info))
  expect_equal(info$kind, "max")
  expect_equal(info$acc, "acc")
  expect_equal(info$op, "max")
})

test_that("detect_reduction_pattern identifies max pattern (acc second)", {  # for (i in seq_along(x)) { acc <- max(x[i], acc) }
  loop <- .mojor_ir_loop(
    var = "i",
    range = .mojor_ir_range_expr(quote(seq_along(x))),
    body = .mojor_ir_block(list(
      .mojor_ir_assign(
        .mojor_ir_var("acc"),
        .mojor_ir_call("max", list(
          .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i"))),
          .mojor_ir_var("acc")
        ))
      )
    ))
  )

  info <- .mojor_ir_detect_reduction_pattern(loop)
  expect_false(is.null(info))
  expect_equal(info$kind, "max")
  expect_equal(info$acc, "acc")
  expect_equal(info$op, "max")
})

test_that("detect_reduction_pattern returns NULL for multi-statement body", {  # for (i in seq_along(x)) { tmp <- x[i]; acc <- acc + tmp }
  # Not a reduction pattern (multiple statements)
  loop <- .mojor_ir_loop(
    var = "i",
    range = .mojor_ir_range_expr(quote(seq_along(x))),
    body = .mojor_ir_block(list(
      .mojor_ir_assign(
        .mojor_ir_var("tmp"),
        .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
      ),
      .mojor_ir_assign(
        .mojor_ir_var("acc"),
        .mojor_ir_binop("+", .mojor_ir_var("acc"), .mojor_ir_var("tmp"))
      )
    ))
  )

  info <- .mojor_ir_detect_reduction_pattern(loop)
  expect_null(info)
})

test_that("detect_reduction_pattern returns NULL for non-loop node", {  # If statement, not a loop
  node <- .mojor_ir_if(
    .mojor_ir_binop(">", .mojor_ir_var("x"), .mojor_ir_const("0")),
    .mojor_ir_block(list(.mojor_ir_assign(.mojor_ir_var("y"), .mojor_ir_const("1"))))
  )

  info <- .mojor_ir_detect_reduction_pattern(node)
  expect_null(info)
})

test_that("detect_reduction_pattern returns NULL for NULL input", {  info <- .mojor_ir_detect_reduction_pattern(NULL)
  expect_null(info)
})

test_that("detect_reduction_pattern returns NULL for wrong assignment pattern", {  # for (i in seq_along(x)) { acc <- x[i] + y[i] }
  # Not a reduction (accumulator not used on RHS)
  loop <- .mojor_ir_loop(
    var = "i",
    range = .mojor_ir_range_expr(quote(seq_along(x))),
    body = .mojor_ir_block(list(
      .mojor_ir_assign(
        .mojor_ir_var("acc"),
        .mojor_ir_binop("+",
          .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i"))),
          .mojor_ir_index(.mojor_ir_var("y"), list(.mojor_ir_var("i")))
        )
      )
    ))
  )

  info <- .mojor_ir_detect_reduction_pattern(loop)
  expect_null(info)
})

test_that("detect_reduction_pattern returns NULL for non-reduction call", {  # for (i in seq_along(x)) { acc <- abs(acc) }
  # Not a reduction pattern (abs is not min/max)
  loop <- .mojor_ir_loop(
    var = "i",
    range = .mojor_ir_range_expr(quote(seq_along(x))),
    body = .mojor_ir_block(list(
      .mojor_ir_assign(
        .mojor_ir_var("acc"),
        .mojor_ir_call("abs", list(.mojor_ir_var("acc")))
      )
    ))
  )

  info <- .mojor_ir_detect_reduction_pattern(loop)
  expect_null(info)
})

test_that("detect_reduction_pattern handles complex expression in reduction", {  # for (i in seq_along(x)) { acc <- acc + (x[i] * y[i]) }
  loop <- .mojor_ir_loop(
    var = "i",
    range = .mojor_ir_range_expr(quote(seq_along(x))),
    body = .mojor_ir_block(list(
      .mojor_ir_assign(
        .mojor_ir_var("acc"),
        .mojor_ir_binop("+",
          .mojor_ir_var("acc"),
          .mojor_ir_binop("*",
            .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i"))),
            .mojor_ir_index(.mojor_ir_var("y"), list(.mojor_ir_var("i")))
          )
        )
      )
    ))
  )

  info <- .mojor_ir_detect_reduction_pattern(loop)
  expect_false(is.null(info))
  expect_equal(info$kind, "sum")
  expect_equal(info$rhs$kind, "binop")
  expect_equal(info$rhs$op, "*")
})

test_that("mojor_ir_is_var_node helper works correctly", {  # Positive case
  var_node <- .mojor_ir_var("x")
  expect_true(.mojor_ir_is_var_node(var_node, "x"))

  # Negative cases
  expect_false(.mojor_ir_is_var_node(var_node, "y"))  # Wrong name
  expect_false(.mojor_ir_is_var_node(.mojor_ir_const("5"), "x"))  # Wrong kind
  expect_false(.mojor_ir_is_var_node(NULL, "x"))  # NULL node
})
