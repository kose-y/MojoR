library(testthat)

test_that("IR elementwise fusion merges adjacent loops", {  range <- .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10"))

  loop1 <- .mojor_ir_loop(
    "i",
    range,
    .mojor_ir_block(list(
      .mojor_ir_assign(
        .mojor_ir_index(.mojor_ir_var("out"), list(.mojor_ir_var("i"))),
        .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
      )
    ))
  )
  loop1$metadata <- list(elementwise = list(enabled = TRUE, target = "cpu"))

  loop2 <- .mojor_ir_loop(
    "i",
    range,
    .mojor_ir_block(list(
      .mojor_ir_assign(
        .mojor_ir_index(.mojor_ir_var("out"), list(.mojor_ir_var("i"))),
        .mojor_ir_binop(
          "+",
          .mojor_ir_index(.mojor_ir_var("out"), list(.mojor_ir_var("i"))),
          .mojor_ir_const("1")
        )
      )
    ))
  )
  loop2$metadata <- list(elementwise = list(enabled = TRUE, target = "cpu"))

  block <- .mojor_ir_block(list(loop1, loop2))
  optimized <- .mojor_ir_fuse_elementwise_stmt(block)

  expect_equal(length(optimized$stmts), 1)
  fused <- optimized$stmts[[1]]
  expect_true(isTRUE(fused$metadata$elementwise$fused))
  expect_equal(fused$metadata$elementwise$fused_count, 2L)
  expect_equal(length(fused$body$stmts), 2)
})

test_that("IR elementwise fusion skips target mismatch", {  range <- .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10"))

  loop1 <- .mojor_ir_loop(
    "i",
    range,
    .mojor_ir_block(list(
      .mojor_ir_assign(
        .mojor_ir_index(.mojor_ir_var("out"), list(.mojor_ir_var("i"))),
        .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
      )
    ))
  )
  loop1$metadata <- list(elementwise = list(enabled = TRUE, target = "cpu"))

  loop2 <- .mojor_ir_loop(
    "i",
    range,
    .mojor_ir_block(list(
      .mojor_ir_assign(
        .mojor_ir_index(.mojor_ir_var("out"), list(.mojor_ir_var("i"))),
        .mojor_ir_binop(
          "+",
          .mojor_ir_index(.mojor_ir_var("out"), list(.mojor_ir_var("i"))),
          .mojor_ir_const("1")
        )
      )
    ))
  )
  loop2$metadata <- list(elementwise = list(enabled = TRUE, target = "gpu"))

  block <- .mojor_ir_block(list(loop1, loop2))
  optimized <- .mojor_ir_fuse_elementwise_stmt(block)

  expect_equal(length(optimized$stmts), 2)
})
