library(testthat)

test_that("memory canonicalization rewrites legacy memory ops and preserves attrs", {  ssa <- list(
    kind = "ssa_fn",
    entry = "bb1",
    blocks = list(
      bb1 = list(
        kind = "ssa_block",
        name = "bb1",
        attrs = list(`schedule.unroll` = 2L),
        params = list(
          list(id = "%arg1", var = "x"),
          list(id = "%arg2", var = "out"),
          list(id = "%arg3", var = "mat"),
          list(id = "%arg4", var = "i"),
          list(id = "%arg5", var = "rhs")
        ),
        stmts = list(
          list(kind = "ssa_stmt", id = "%v1", op = "index", args = c("%arg1", "%arg4"), attrs = list(`schedule.vectorize` = 4L)),
          list(kind = "ssa_effect", op = "store_index", args = c("%arg2", "%arg4", "%v1"), index_kinds = "scalar"),
          list(
            kind = "ssa_effect",
            op = "store_subscript",
            target = "mat",
            args = c("%arg3", "%arg4", "%arg5"),
            index_kinds = c("scalar", "missing")
          )
        ),
        term = list(kind = "ret", value = "%v1")
      )
    )
  )

  out <- .mojor_ssa_memory_canonicalize(ssa)
  ops <- vapply(out$blocks$bb1$stmts, function(st) as.character(st$op), character(1))

  expect_identical(ops[[1]], "mojor.mem.load")
  expect_identical(ops[[2]], "mojor.mem.store")
  expect_identical(ops[[3]], "mojor.mem.store")
  expect_identical(out$blocks$bb1$stmts[[2]]$legacy_memory_form, "store_index")
  expect_identical(out$blocks$bb1$stmts[[3]]$legacy_memory_form, "store_subscript")
  expect_identical(out$blocks$bb1$stmts[[3]]$target, "mat")
  expect_identical(out$blocks$bb1$stmts[[3]]$index_kinds, c("scalar", "missing"))
  expect_identical(out$blocks$bb1$attrs$`schedule.unroll`, 2L)
  expect_identical(out$blocks$bb1$stmts[[1]]$attrs$`schedule.vectorize`, 4L)
})

test_that("boundary verifier rejects legacy memory spellings after memory canonicalization boundary", {  legacy <- list(
    kind = "ssa_fn",
    entry = "bb1",
    blocks = list(
      bb1 = list(
        kind = "ssa_block",
        name = "bb1",
        params = list(
          list(id = "%arg1", var = "x"),
          list(id = "%arg2", var = "out"),
          list(id = "%arg3", var = "i")
        ),
        stmts = list(
          list(kind = "ssa_stmt", id = "%v1", op = "index", args = c("%arg1", "%arg3")),
          list(kind = "ssa_effect", op = "store_index", args = c("%arg2", "%arg3", "%v1"), index_kinds = "scalar")
        ),
        term = list(kind = "ret", value = "%v1")
      )
    )
  )

  core <- .mojor_verify_semantic_ir(legacy, mode = "core", boundary = "none")
  expect_true(isTRUE(core$ok))

  post_mem <- .mojor_verify_semantic_ir(legacy, mode = "boundary", boundary = "post_memory_canonicalize")
  expect_false(isTRUE(post_mem$ok))
  expect_true(any(vapply(post_mem$diagnostics, function(d) identical(d$code, "LEGACY_MEMORY_SPELLING"), logical(1))))

  canonical <- .mojor_ssa_memory_canonicalize(legacy)
  post_mem_can <- .mojor_verify_semantic_ir(canonical, mode = "boundary", boundary = "post_memory_canonicalize")
  expect_true(isTRUE(post_mem_can$ok))
})

test_that("pretty semantic printer emits canonical mojor.mem load/store spellings", {  text <- paste(
    "ssa_fn entry=bb1",
    "bb1(%arg1/*x*/, %arg2/*out*/, %arg3/*i*/):",
    "  %v1 = index(%arg1, %arg3)",
    "  store_index(%arg2, %arg3, %v1)",
    "  ret %v1",
    sep = "\n"
  )

  sem <- .mojor_ssa_lower_to_semantic(.mojor_ssa_parse_lossless(text))
  pretty <- .mojor_ssa_print_semantic(sem, mode = "pretty")

  expect_match(pretty, "mojor\\.mem\\.load\\(")
  expect_match(pretty, "mojor\\.mem\\.store\\(")
  expect_false(grepl("store_index\\(", pretty))
})

test_that("backend selection accepts canonical mojor.mem.load/store spellings", {  load_inst <- .mojor_ir_backend_select_stmt(
    list(kind = "ssa_stmt", id = "%v1", op = "mojor.mem.load", args = c("%arg1", "%arg2"))
  )
  expect_identical(load_inst$opcode, "load_index")

  store_index_inst <- .mojor_ir_backend_select_stmt(
    list(
      kind = "ssa_effect",
      op = "mojor.mem.store",
      args = c("%arg3", "%arg2", "%v1"),
      index_kinds = "scalar",
      legacy_memory_form = "store_index"
    )
  )
  expect_identical(store_index_inst$opcode, "mem_store_index_scalar")

  store_subscript_inst <- .mojor_ir_backend_select_stmt(
    list(
      kind = "ssa_effect",
      op = "mojor.mem.store",
      target = "mat",
      args = c("%arg4", "%arg2", "%v1"),
      index_kinds = c("scalar", "missing"),
      legacy_memory_form = "store_subscript"
    )
  )
  expect_identical(store_subscript_inst$opcode, "mem_store_subscript_missing")
})
