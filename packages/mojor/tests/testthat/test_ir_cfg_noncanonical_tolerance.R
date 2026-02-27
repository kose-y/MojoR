library(testthat)

mk_noncanonical_cfg_ssa <- function() {
  list(
    kind = "ssa_fn",
    entry = "entry",
    blocks = list(
      entry = list(
        kind = "ssa_block",
        name = "entry",
        params = list(list(id = "%arg1", var = "c")),
        stmts = list(),
        term = list(kind = "br", target = "h", args = c("%arg1"))
      ),
      h = list(
        kind = "ssa_block",
        name = "h",
        params = list(list(id = "%v1", var = "c")),
        stmts = list(),
        term = list(kind = "condbr", cond = "%v1", then = "b", "else" = "e", then_args = character(0), else_args = character(0))
      ),
      b = list(
        kind = "ssa_block",
        name = "b",
        params = list(),
        stmts = list(),
        term = list(kind = "br", target = "h", args = c("%v1"))
      ),
      e = list(
        kind = "ssa_block",
        name = "e",
        params = list(),
        stmts = list(),
        term = list(kind = "ret", value = NULL)
      )
    )
  )
}

mk_unknown_recovery_cfg_ssa <- function() {
  list(
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
}

test_that("core verifier accepts valid non-canonical CFG", {  ssa <- mk_noncanonical_cfg_ssa()
  expect_silent(.mojor_ir_verify_ssa(ssa))
  core <- .mojor_verify_semantic_ir(ssa, mode = "core", boundary = "none")
  expect_true(isTRUE(core$ok))
})

test_that("post_recanonicalize boundary is disabled when recovery is Unknown", {  ssa <- mk_unknown_recovery_cfg_ssa()
  expect_silent(.mojor_ir_verify_ssa(ssa))

  recovery <- .mojor_loop_recovery_analysis(ssa)
  expect_identical(recovery$status, "Unknown")

  res <- .mojor_verify_semantic_ir(
    ssa,
    mode = "boundary",
    boundary = "post_recanonicalize",
    opts = list(loop_recovery = recovery)
  )
  expect_true(isTRUE(res$ok))
  expect_true(any(vapply(res$diagnostics, function(d) identical(d$code, "RECANONICALIZE_BOUNDARY_DISABLED_RECOVERY_UNKNOWN"), logical(1))))
})
