library(testthat)

test_that("semantic_hash_alpha is stable under SSA renumbering", {  ssa_a <- list(
    kind = "ssa_fn",
    entry = "bb1",
    blocks = list(
      bb1 = list(
        kind = "ssa_block",
        name = "bb1",
        params = list(list(id = "%v10", var = "x"), list(id = "%v20", var = "y")),
        stmts = list(
          list(kind = "ssa_stmt", id = "%v30", op = "binop:+", args = c("%v10", "%v20"))
        ),
        term = list(kind = "ret", value = "%v30")
      )
    )
  )

  ssa_b <- list(
    kind = "ssa_fn",
    entry = "bb1",
    blocks = list(
      bb1 = list(
        kind = "ssa_block",
        name = "bb1",
        params = list(list(id = "%v1", var = "x"), list(id = "%v2", var = "y")),
        stmts = list(
          list(kind = "ssa_stmt", id = "%v3", op = "binop:+", args = c("%v1", "%v2"))
        ),
        term = list(kind = "ret", value = "%v3")
      )
    )
  )

  expect_false(identical(.mojor_ssa_semantic_hash_fn(ssa_a), .mojor_ssa_semantic_hash_fn(ssa_b)))
  expect_identical(.mojor_ssa_semantic_hash_alpha_fn(ssa_a), .mojor_ssa_semantic_hash_alpha_fn(ssa_b))
})

test_that("semantic_hash_alpha canonicalizes op aliases", {  legacy <- list(
    kind = "ssa_fn",
    entry = "bb1",
    blocks = list(
      bb1 = list(
        kind = "ssa_block",
        name = "bb1",
        params = list(list(id = "%v1", var = "x"), list(id = "%v2", var = "y")),
        stmts = list(
          list(kind = "ssa_stmt", id = "%v3", op = "binop:+", args = c("%v1", "%v2"))
        ),
        term = list(kind = "ret", value = "%v3")
      )
    )
  )

  canonical <- list(
    kind = "ssa_fn",
    entry = "bb1",
    blocks = list(
      bb1 = list(
        kind = "ssa_block",
        name = "bb1",
        params = list(list(id = "%v11", var = "x"), list(id = "%v12", var = "y")),
        stmts = list(
          list(kind = "ssa_stmt", id = "%v13", op = "mojor.arith.add", args = c("%v11", "%v12"))
        ),
        term = list(kind = "ret", value = "%v13")
      )
    )
  )

  expect_identical(
    .mojor_ssa_semantic_hash_alpha_fn(legacy),
    .mojor_ssa_semantic_hash_alpha_fn(canonical)
  )
})
