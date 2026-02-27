library(testthat)

structured_golden_path <- function(name) {
  .mojor_test_golden_path(name)
}

read_structured_golden <- function(name) {
  .mojor_test_read_golden(name)
}

test_that("structured lowering is deterministic and preserves loop intent attrs", {  ir <- .mojor_ir_block(list(
    .mojor_ir_assign(.mojor_ir_var("acc"), .mojor_ir_const("0")),
    .mojor_ir_s_for(
      var = "i",
      range = .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_var("n")),
      body = .mojor_ir_block(list(
        .mojor_ir_assign(
          .mojor_ir_var("acc"),
          .mojor_ir_binop("+", .mojor_ir_var("acc"), .mojor_ir_const("1"))
        )
      )),
      loop_id = "L_main",
      attrs = list(`schedule.unroll` = 4L, `intent.vectorize` = TRUE)
    ),
    .mojor_ir_return(.mojor_ir_var("acc"))
  ))

  ssa1 <- .mojor_ir_structured_lower_to_ssa(ir, verify_ir = TRUE)
  ssa2 <- .mojor_ir_structured_lower_to_ssa(ir, verify_ir = TRUE)

  expect_identical(.mojor_ir_ssa_format(ssa1), .mojor_ir_ssa_format(ssa2))
  expect_silent(.mojor_ir_verify_ssa(ssa1))
  expect_identical(.mojor_ir_ssa_format(ssa1), read_structured_golden("structured_to_ssa_for.ssa"))

  post <- .mojor_verify_semantic_ir(ssa1, mode = "boundary", boundary = "post_structured_lowering")
  expect_true(isTRUE(post$ok))

  expect_true("L_main" %in% names(ssa1$loop_metadata))
  anchor <- ssa1$loop_metadata$L_main$anchor_block
  expect_true(!is.null(anchor) && nzchar(anchor))
  expect_identical(ssa1$loop_metadata$L_main$continue_dest$block, anchor)
  expect_identical(ssa1$blocks[[anchor]]$attrs$loop_id, "L_main")
  expect_identical(ssa1$blocks[[anchor]]$attrs$`schedule.unroll`, 4L)
  expect_identical(ssa1$blocks[[anchor]]$attrs$`intent.vectorize`, TRUE)
})

test_that("post_structured_lowering boundary rejects remaining structured op spellings", {  ssa <- list(
    kind = "ssa_fn",
    entry = "bb1",
    blocks = list(
      bb1 = list(
        kind = "ssa_block",
        name = "bb1",
        params = list(),
        stmts = list(
          list(kind = "ssa_effect", op = "s_if", args = character(0))
        ),
        term = list(kind = "ret", value = NULL)
      )
    )
  )

  res <- .mojor_verify_semantic_ir(ssa, mode = "boundary", boundary = "post_structured_lowering")
  expect_false(isTRUE(res$ok))
  expect_true(any(vapply(res$diagnostics, function(d) identical(d$code, "STRUCTURED_OP_REMAINING"), logical(1))))
})

test_that("post_structured_lowering boundary requires complete loop metadata", {  ssa <- list(
    kind = "ssa_fn",
    entry = "loop_header1",
    blocks = list(
      loop_header1 = list(
        kind = "ssa_block",
        name = "loop_header1",
        attrs = list(loop_id = "L_missing", loop_kind = "for", loop_role = "header"),
        params = list(),
        stmts = list(),
        term = list(kind = "ret", value = NULL)
      )
    )
  )

  res <- .mojor_verify_semantic_ir(ssa, mode = "boundary", boundary = "post_structured_lowering")
  expect_false(isTRUE(res$ok))
  expect_true(any(vapply(res$diagnostics, function(d) identical(d$code, "LOOP_METADATA_INCOMPLETE"), logical(1))))
})
