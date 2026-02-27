library(testthat)

test_that("loop_recovery_analysis recovers natural loop from canonical SSA", {  ir <- .mojor_ir_block(list(
    .mojor_ir_assign(.mojor_ir_var("acc"), .mojor_ir_const("0")),
    .mojor_ir_loop(
      var = "i",
      range = .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_var("n")),
      body = .mojor_ir_block(list(
        .mojor_ir_assign(
          .mojor_ir_var("acc"),
          .mojor_ir_binop("+", .mojor_ir_var("acc"), .mojor_ir_const("1"))
        )
      ))
    ),
    .mojor_ir_return(.mojor_ir_var("acc"))
  ))

  ssa <- .mojor_ir_to_ssa(ir)
  expect_silent(.mojor_ir_verify_ssa(ssa))

  rec <- .mojor_loop_recovery_analysis(ssa)
  expect_identical(rec$status, "Recovered")
  expect_true(isTRUE(rec$boundary_verifier_enabled))
  expect_true(length(rec$loops) >= 1L)

  first_id <- names(rec$loops)[[1]]
  first <- rec$loops[[first_id]]
  expect_true(first$header %in% names(ssa$blocks))
  expect_true(first$continue_dest$block %in% names(ssa$blocks))
  expect_true(first$break_dest$block %in% names(ssa$blocks))
})

test_that("loop_recovery_analysis preserves intent attrs on loop anchor", {  ssa <- list(
    kind = "ssa_fn",
    entry = "entry",
    blocks = list(
      entry = list(
        kind = "ssa_block",
        name = "entry",
        params = list(list(id = "%arg1", var = "c")),
        stmts = list(),
        term = list(kind = "br", target = "header", args = c("%arg1"))
      ),
      header = list(
        kind = "ssa_block",
        name = "header",
        attrs = list(
          loop_id = "L1",
          loop_kind = "for",
          loop_role = "header",
          loop_continue_dest = "header",
          loop_break_dest = "exit",
          loop_anchor = TRUE,
          `schedule.unroll` = 8L
        ),
        params = list(list(id = "%v1", var = "c")),
        stmts = list(),
        term = list(kind = "condbr", cond = "%v1", then = "body", "else" = "exit", then_args = character(0), else_args = character(0))
      ),
      body = list(
        kind = "ssa_block",
        name = "body",
        params = list(),
        stmts = list(),
        term = list(kind = "br", target = "header", args = c("%v1"))
      ),
      exit = list(
        kind = "ssa_block",
        name = "exit",
        params = list(),
        stmts = list(),
        term = list(kind = "ret", value = NULL)
      )
    )
  )

  expect_silent(.mojor_ir_verify_ssa(ssa))
  rec <- .mojor_loop_recovery_analysis(ssa)
  expect_identical(rec$status, "Recovered")
  expect_true("L1" %in% names(rec$loops))
  expect_identical(rec$loops$L1$intent_attrs$`schedule.unroll`, 8L)
})

test_that("loop_recovery_analysis returns Unknown for irreducible entry form", {  ssa <- list(
    kind = "ssa_fn",
    entry = "entry",
    blocks = list(
      entry = list(
        kind = "ssa_block",
        name = "entry",
        params = list(),
        stmts = list(
          list(kind = "ssa_stmt", id = "%v1", op = "const", args = character(0))
        ),
        term = list(kind = "condbr", cond = "%v1", then = "A", "else" = "X", then_args = character(0), else_args = character(0))
      ),
      A = list(
        kind = "ssa_block",
        name = "A",
        params = list(),
        stmts = list(
          list(kind = "ssa_stmt", id = "%v2", op = "const", args = character(0))
        ),
        term = list(kind = "condbr", cond = "%v2", then = "B", "else" = "exit", then_args = character(0), else_args = character(0))
      ),
      X = list(
        kind = "ssa_block",
        name = "X",
        params = list(),
        stmts = list(),
        term = list(kind = "br", target = "B", args = character(0))
      ),
      B = list(
        kind = "ssa_block",
        name = "B",
        params = list(),
        stmts = list(),
        term = list(kind = "br", target = "A", args = character(0))
      ),
      exit = list(
        kind = "ssa_block",
        name = "exit",
        params = list(),
        stmts = list(),
        term = list(kind = "ret", value = NULL)
      )
    )
  )

  expect_silent(.mojor_ir_verify_ssa(ssa))
  rec <- .mojor_loop_recovery_analysis(ssa)
  expect_identical(rec$status, "Unknown")
  expect_false(isTRUE(rec$boundary_verifier_enabled))
  expect_true(any(vapply(rec$diagnostics, function(d) identical(d$code, "LOOP_RECOVERY_UNKNOWN_NOT_REDUCIBLE"), logical(1))))
})
