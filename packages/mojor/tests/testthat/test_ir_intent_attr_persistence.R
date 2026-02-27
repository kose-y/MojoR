library(testthat)

test_that("canonicalization preserves intent attrs on blocks and statements", {  ssa <- list(
    kind = "ssa_fn",
    entry = "loop_header1",
    blocks = list(
      loop_header1 = list(
        kind = "ssa_block",
        name = "loop_header1",
        attrs = list(`schedule.unroll` = 4L),
        params = list(list(id = "%v1", var = "i"), list(id = "%v2", var = "acc")),
        stmts = list(
          list(
            kind = "ssa_stmt",
            id = "%v3",
            op = "binop:+",
            args = c("%v2", "%v1"),
            attrs = list(`schedule.vectorize` = 8L)
          )
        ),
        term = list(kind = "br", target = "loop_exit1", args = c("%v1", "%v3"))
      ),
      loop_exit1 = list(
        kind = "ssa_block",
        name = "loop_exit1",
        params = list(list(id = "%v4", var = "i"), list(id = "%v5", var = "acc")),
        stmts = list(),
        term = list(kind = "ret", value = "%v5")
      )
    )
  )

  can <- .mojor_ssa_canonicalize(ssa)
  expect_identical(can$blocks$loop_header1$attrs$`schedule.unroll`, 4L)
  expect_identical(can$blocks$loop_header1$stmts[[1]]$attrs$`schedule.vectorize`, 8L)
})

test_that("loop destination metadata seed detects logical continue/break destinations", {  ssa <- list(
    kind = "ssa_fn",
    entry = "loop_header1",
    blocks = list(
      loop_header1 = list(
        kind = "ssa_block",
        name = "loop_header1",
        params = list(),
        stmts = list(),
        term = list(kind = "br", target = "loop_exit1", args = character(0))
      ),
      loop_exit1 = list(
        kind = "ssa_block",
        name = "loop_exit1",
        params = list(),
        stmts = list(),
        term = list(kind = "ret", value = NULL)
      )
    )
  )

  meta <- .mojor_ssa_loop_destination_metadata(ssa)
  expect_true("for_1" %in% names(meta))
  expect_identical(meta$for_1$continue_dest$block, "loop_header1")
  expect_identical(meta$for_1$break_dest$block, "loop_exit1")

  with_meta <- .mojor_ssa_attach_loop_destination_metadata(ssa)
  expect_true(!is.null(with_meta$loop_metadata$for_1))
})
