library(testthat)

test_that("RawSyntaxTree lowers to SemanticIR without trivia", {  text <- paste(
    "ssa_fn entry=bb1",
    "bb1(%v10/*x*/):",
    "  %v20 = const()",
    "  %v21 = binop:+(%v10, %v20)",
    "  ret %v21",
    sep = "\n"
  )

  raw <- .mojor_ssa_parse_lossless(text)
  sem <- .mojor_ssa_lower_to_semantic(raw)

  expect_identical(sem$kind, "ssa_fn")
  expect_true(is.null(sem$src_text))
  expect_true(is.null(sem$parse_metadata))
  expect_identical(sem$blocks$bb1$params[[1]]$var, "x")
  expect_silent(.mojor_ir_verify_ssa(sem))
})

test_that("SSA canonicalization prunes unreachable blocks and renumbers values", {  text <- paste(
    "ssa_fn entry=entry",
    "entry(%v9/*x*/):",
    "  %v99 = const()",
    "  br exit(%v99)",
    "dead():",
    "  %v50 = const()",
    "  ret %v50",
    "exit(%v7):",
    "  ret %v7",
    sep = "\n"
  )

  sem <- .mojor_ssa_lower_to_semantic(.mojor_ssa_parse_lossless(text))
  can <- .mojor_ssa_canonicalize(sem)
  fmt <- .mojor_ssa_print_semantic(can, mode = "pretty")

  expect_false(grepl("dead\\(", fmt))
  expect_match(fmt, "entry\\(%arg1/\\*x\\*/\\):")
  expect_match(fmt, "%v1 = const\\(\\)")
  expect_silent(.mojor_ir_verify_ssa(can))
})

test_that("SSA boundary APIs reject wrong representation kinds", {  text <- paste(
    "ssa_fn entry=bb1",
    "bb1():",
    "  ret",
    sep = "\n"
  )

  raw <- .mojor_ssa_parse_lossless(text)
  sem <- .mojor_ssa_lower_to_semantic(raw)

  expect_error(.mojor_ssa_canonicalize(raw), "SemanticIR")
  expect_error(.mojor_ssa_print_lossless(sem), "RawSyntaxTree")
  expect_error(.mojor_ssa_print_semantic(raw), "SemanticIR")
})

test_that("SSA passes reject RawSyntaxTree and require SemanticIR", {  text <- paste(
    "ssa_fn entry=bb1",
    "bb1():",
    "  ret",
    sep = "\n"
  )

  raw <- .mojor_ssa_parse_lossless(text)

  expect_error(.mojor_ir_ssa_prune_unreachable(raw), "RawSyntaxTree")
  expect_error(.mojor_ir_ssa_copy_propagate(raw), "RawSyntaxTree")
  expect_error(.mojor_ir_ssa_prune_dead_stmts(raw), "RawSyntaxTree")
  expect_error(.mojor_ir_ssa_optimize(raw), "RawSyntaxTree")
})
