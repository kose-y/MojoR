library(testthat)

test_that("SSA type normalization supports index-first vocabulary", {  expect_identical(.mojor_ssa_type_normalize("bool"), "i1")
  expect_identical(.mojor_ssa_type_normalize("i32"), "i32")
  expect_identical(.mojor_ssa_type_normalize("index"), "index")
  expect_identical(.mojor_ssa_type_normalize("f64[]"), "ref<f64,1>")
  expect_identical(.mojor_ssa_type_normalize("ref<f32, ?>"), "ref<f32,?>")
})

test_that("typing verifier catches branch argument and condbr type mismatches", {  ssa <- list(
    kind = "ssa_fn",
    entry = "entry",
    blocks = list(
      entry = list(
        kind = "ssa_block",
        name = "entry",
        params = list(),
        stmts = list(
          list(kind = "ssa_stmt", id = "%v1", op = "const", value = "1.0", type = "f64", args = character(0))
        ),
        term = list(kind = "condbr", cond = "%v1", then = "bb_true", "else" = "bb_false", then_args = c("%v1"), else_args = c("%v1"))
      ),
      bb_true = list(
        kind = "ssa_block",
        name = "bb_true",
        params = list(list(id = "%v2", var = "x", type = "i32")),
        stmts = list(),
        term = list(kind = "ret", value = "%v2")
      ),
      bb_false = list(
        kind = "ssa_block",
        name = "bb_false",
        params = list(list(id = "%v3", var = "x", type = "i32")),
        stmts = list(),
        term = list(kind = "ret", value = "%v3")
      )
    )
  )

  res <- .mojor_verify_semantic_ir(
    ssa,
    mode = "boundary",
    boundary = "post_canonicalize",
    opts = list(require_typing = TRUE)
  )

  expect_false(isTRUE(res$ok))
  codes <- vapply(res$diagnostics, function(d) as.character(d$code), character(1))
  expect_true("TYPE_CONDBR_COND_TYPE" %in% codes)
  expect_true("TYPE_BRANCH_ARG_MISMATCH" %in% codes)
})

test_that("typing verifier checks index_cast and typed compare operand classes", {  ssa <- list(
    kind = "ssa_fn",
    entry = "bb1",
    blocks = list(
      bb1 = list(
        kind = "ssa_block",
        name = "bb1",
        params = list(),
        stmts = list(
          list(kind = "ssa_stmt", id = "%v1", op = "const", value = "1.25", type = "f64", args = character(0)),
          list(kind = "ssa_stmt", id = "%v2", op = "const", value = "3", type = "i32", args = character(0)),
          list(kind = "ssa_stmt", id = "%v3", op = "index_cast", type = "index", args = c("%v1")),
          list(kind = "ssa_stmt", id = "%v4", op = "cmpi:eq", type = "i1", args = c("%v1", "%v2")),
          list(kind = "ssa_stmt", id = "%v5", op = "cmpf:lt", type = "i1", args = c("%v2", "%v1"))
        ),
        term = list(kind = "ret", value = "%v2")
      )
    )
  )

  res <- .mojor_verify_semantic_ir(
    ssa,
    mode = "boundary",
    boundary = "post_canonicalize",
    opts = list(require_typing = TRUE)
  )

  expect_false(isTRUE(res$ok))
  codes <- vapply(res$diagnostics, function(d) as.character(d$code), character(1))
  expect_true("TYPE_INDEX_CAST_SOURCE_TYPE" %in% codes)
  expect_true("TYPE_CMPI_OPERAND_TYPE" %in% codes)
  expect_true("TYPE_CMPF_OPERAND_TYPE" %in% codes)
})

test_that("typing verifier accepts canonicalized SSA from existing loop kernels", {  ir <- .mojor_ir_block(list(
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

  ssa <- .mojor_ssa_canonicalize(.mojor_ir_to_ssa(ir))
  res <- .mojor_verify_semantic_ir(
    ssa,
    mode = "boundary",
    boundary = "post_canonicalize",
    opts = list(require_typing = TRUE)
  )

  expect_true(isTRUE(res$ok))
  expect_identical(length(res$diagnostics), 0L)
})
