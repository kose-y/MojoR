library(testthat)

mk_simple_loop_ssa <- function() {
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

mk_irreducible_loop_ssa <- function() {
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

test_that("recanonicalize_loop_cfg restores loop boundary metadata/attrs", {  ssa <- mk_simple_loop_ssa()
  expect_silent(.mojor_ir_verify_ssa(ssa))

  recan <- .mojor_recanonicalize_loop_cfg(ssa, verify = TRUE)
  expect_identical(recan$status, "Recanonicalized")
  expect_true(isTRUE(recan$boundary_verifier_enabled))
  expect_true(length(recan$ssa$loop_metadata) >= 1L)

  loop_id <- names(recan$ssa$loop_metadata)[[1]]
  anchor <- recan$ssa$loop_metadata[[loop_id]]$anchor_block
  expect_true(anchor %in% names(recan$ssa$blocks))
  expect_identical(recan$ssa$blocks[[anchor]]$attrs$loop_id, loop_id)
  expect_identical(recan$ssa$blocks[[anchor]]$attrs$loop_role, "header")

  post <- .mojor_verify_semantic_ir(recan$ssa, mode = "boundary", boundary = "post_recanonicalize")
  expect_true(isTRUE(post$ok))
})

test_that("recanonicalize_loop_cfg propagates Unknown from recovery", {  ssa <- mk_irreducible_loop_ssa()
  expect_silent(.mojor_ir_verify_ssa(ssa))

  rec <- .mojor_loop_recovery_analysis(ssa)
  expect_identical(rec$status, "Unknown")

  recan <- .mojor_recanonicalize_loop_cfg(ssa, recovery = rec, verify = TRUE)
  expect_identical(recan$status, "Unknown")
  expect_false(isTRUE(recan$boundary_verifier_enabled))
  expect_identical(.mojor_ir_ssa_format(recan$ssa), .mojor_ir_ssa_format(ssa))
})
