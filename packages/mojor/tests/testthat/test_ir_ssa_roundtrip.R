library(testthat)

test_that("SSA lossless parser/printer round-trips printer output", {  ir <- .mojor_ir_block(list(
    .mojor_ir_assign(.mojor_ir_var("a"), .mojor_ir_const("1")),
    .mojor_ir_return(.mojor_ir_var("a"))
  ))
  ssa <- .mojor_ir_to_ssa(ir)
  text <- .mojor_ir_ssa_format(ssa)

  raw <- .mojor_ssa_parse_lossless(text)
  out <- .mojor_ssa_print_lossless(raw)

  expect_identical(out, text)
})

test_that("SSA normalized parser canonicalizes trivia", {  messy <- paste(
    "ssa_fn entry=bb1",
    "bb1(  ):",
    "ret",
    sep = "\n"
  )

  norm <- .mojor_ssa_parse_norm(messy)
  text <- .mojor_ssa_print_lossless(norm)

  expect_identical(text, paste(
    "ssa_fn entry=bb1",
    "bb1():",
    "  ret",
    sep = "\n"
  ))
})

test_that("SSA pretty printer is idempotent under parse/lower/canonicalize", {  ir <- .mojor_ir_block(list(
    .mojor_ir_assign(.mojor_ir_var("acc"), .mojor_ir_const("0")),
    .mojor_ir_if(
      cond = .mojor_ir_binop(">", .mojor_ir_var("x"), .mojor_ir_const("0")),
      then = .mojor_ir_block(list(
        .mojor_ir_assign(
          .mojor_ir_var("acc"),
          .mojor_ir_binop("+", .mojor_ir_var("acc"), .mojor_ir_const("1"))
        )
      )),
      else_block = .mojor_ir_block(list(
        .mojor_ir_assign(
          .mojor_ir_var("acc"),
          .mojor_ir_binop("-", .mojor_ir_var("acc"), .mojor_ir_const("1"))
        )
      ))
    ),
    .mojor_ir_return(.mojor_ir_var("acc"))
  ))

  ssa <- .mojor_ir_to_ssa(ir)
  text1 <- .mojor_ssa_print_semantic(ssa, mode = "pretty")

  tree <- .mojor_ssa_parse_norm(text1)
  sem <- .mojor_ssa_lower_to_semantic(tree)
  can <- .mojor_ssa_canonicalize(sem)
  text2 <- .mojor_ssa_print_semantic(can, mode = "pretty")

  expect_identical(text1, text2)
})

test_that("SSA compat and pretty parse back to equivalent semantics", {  ir <- .mojor_ir_block(list(
    .mojor_ir_assign(.mojor_ir_var("a"), .mojor_ir_const("1")),
    .mojor_ir_assign(
      .mojor_ir_var("b"),
      .mojor_ir_binop("+", .mojor_ir_var("a"), .mojor_ir_const("2"))
    ),
    .mojor_ir_return(.mojor_ir_var("b"))
  ))

  ssa <- .mojor_ssa_canonicalize(.mojor_ir_to_ssa(ir))

  pretty <- .mojor_ssa_print_semantic(ssa, mode = "pretty")
  compat <- .mojor_ssa_print_semantic(ssa, mode = "compat")

  from_pretty <- .mojor_ssa_canonicalize(
    .mojor_ssa_lower_to_semantic(.mojor_ssa_parse_norm(pretty))
  )
  from_compat <- .mojor_ssa_canonicalize(
    .mojor_ssa_lower_to_semantic(.mojor_ssa_parse_norm(compat))
  )

  expect_true(.mojor_ssa_semantic_eq(from_pretty, from_compat))
})
