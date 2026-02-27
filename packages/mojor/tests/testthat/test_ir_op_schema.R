library(testthat)

test_that("op canonicalization maps legacy spellings to namespaced ops", {  expect_identical(.mojor_ir_op_canonicalize("const"), "mojor.arith.const")
  expect_identical(.mojor_ir_op_canonicalize("binop:+"), "mojor.arith.add")
  expect_identical(.mojor_ir_op_canonicalize("unop:-"), "mojor.arith.neg")
  expect_identical(.mojor_ir_op_canonicalize("index_cast"), "mojor.arith.index_cast")
  expect_identical(.mojor_ir_op_canonicalize("cmpi:eq"), "mojor.arith.cmpi.eq")
  expect_identical(.mojor_ir_op_canonicalize("cmpf:lt"), "mojor.arith.cmpf.lt")
  expect_identical(.mojor_ir_op_canonicalize("cast:i32"), "mojor.arith.cast.i32")
  expect_identical(.mojor_ir_op_canonicalize("call:sin"), "mojor.misc.call.sin")
  expect_identical(.mojor_ir_op_canonicalize("load"), "mojor.mem.load")
  expect_identical(.mojor_ir_op_canonicalize("load:surface"), "mojor.mem.load.surface")
  expect_identical(.mojor_ir_op_canonicalize("store:foo"), "mojor.mem.store.foo")
  expect_identical(.mojor_ir_op_canonicalize("scheduled_reduce"), "mojor.misc.stmt.scheduled_reduce")
  expect_identical(.mojor_ir_op_canonicalize("scalar_reduce"), "mojor.misc.stmt.scalar_reduce")
  expect_identical(.mojor_ir_op_canonicalize("s_if"), "mojor.misc.stmt.if")
  expect_identical(.mojor_ir_op_canonicalize("s_for"), "mojor.misc.stmt.for")
  expect_identical(.mojor_ir_op_canonicalize("s_while"), "mojor.misc.stmt.while")
  expect_identical(.mojor_ir_op_canonicalize("s_repeat"), "mojor.misc.stmt.repeat")
})

test_that("schema table includes canonical memory load/store ops", {  load_schema <- .mojor_ir_op_schema_get("mojor.mem.load", canonicalize = FALSE)
  store_schema <- .mojor_ir_op_schema_get("mojor.mem.store", canonicalize = FALSE)
  index_cast_schema <- .mojor_ir_op_schema_get("mojor.arith.index_cast", canonicalize = FALSE)
  cmpi_schema <- .mojor_ir_op_schema_get("mojor.arith.cmpi.eq", canonicalize = FALSE)
  cmpf_schema <- .mojor_ir_op_schema_get("mojor.arith.cmpf.lt", canonicalize = FALSE)
  load_surface_schema <- .mojor_ir_op_schema_get("mojor.mem.load.surface", canonicalize = FALSE)
  store_surface_schema <- .mojor_ir_op_schema_get("mojor.mem.store.surface", canonicalize = FALSE)
  stmt_if_schema <- .mojor_ir_op_schema_get("mojor.misc.stmt.if", canonicalize = FALSE)

  expect_false(is.null(load_schema))
  expect_false(is.null(store_schema))
  expect_false(is.null(index_cast_schema))
  expect_false(is.null(cmpi_schema))
  expect_false(is.null(cmpf_schema))
  expect_false(is.null(load_surface_schema))
  expect_false(is.null(store_surface_schema))
  expect_false(is.null(stmt_if_schema))
  expect_true("ReadsMem" %in% load_schema$interfaces)
  expect_true("WritesMem" %in% store_schema$interfaces)
  expect_true("Pure" %in% index_cast_schema$interfaces)
  expect_true("Pure" %in% cmpi_schema$interfaces)
  expect_true("Pure" %in% cmpf_schema$interfaces)
  expect_true("ReadsMem" %in% load_surface_schema$interfaces)
  expect_true("WritesMem" %in% store_surface_schema$interfaces)
  expect_true("Effect" %in% stmt_if_schema$interfaces)
})

test_that("SSA op canonicalization rewrites statement/effect ops", {  ssa <- list(
    kind = "ssa_fn",
    entry = "bb1",
    blocks = list(
      bb1 = list(
        kind = "ssa_block",
        name = "bb1",
        params = list(),
        stmts = list(
          list(kind = "ssa_stmt", id = "%v1", op = "const", args = character(0)),
          list(kind = "ssa_stmt", id = "%v2", op = "binop:+", args = c("%v1", "%v1")),
          list(kind = "ssa_effect", op = "store_index", args = c("out", "%v1", "%v2"))
        ),
        term = list(kind = "ret", value = "%v2")
      )
    )
  )

  can <- .mojor_ir_ssa_canonicalize_ops(ssa)
  ops <- vapply(can$blocks$bb1$stmts, function(st) st$op, character(1))
  expect_identical(ops[[1]], "mojor.arith.const")
  expect_identical(ops[[2]], "mojor.arith.add")
  expect_identical(ops[[3]], "mojor.mem.store_index")
})

test_that("op schema validation accepts known ops and reports unknowns", {  good <- list(
    kind = "ssa_fn",
    entry = "bb1",
    blocks = list(
      bb1 = list(
        kind = "ssa_block",
        name = "bb1",
        params = list(),
        stmts = list(
          list(kind = "ssa_stmt", id = "%v1", op = "const", args = character(0)),
          list(kind = "ssa_stmt", id = "%v2", op = "binop:+", args = c("%v1", "%v1")),
          list(kind = "ssa_stmt", id = "%v3", op = "load:surface", args = c("x", "%v1"))
        ),
        term = list(kind = "ret", value = "%v3")
      )
    )
  )
  bad <- good
  bad$blocks$bb1$stmts[[2]]$op <- "mystery:thing"

  ok <- .mojor_ir_validate_op_schema(good)
  expect_true(isTRUE(ok$ok))
  expect_identical(length(ok$diagnostics), 0L)

  bad_res <- .mojor_ir_validate_op_schema(bad)
  expect_false(isTRUE(bad_res$ok))
  expect_true(length(bad_res$diagnostics) >= 1L)
  expect_identical(bad_res$diagnostics[[1]]$code, "OP_SCHEMA_UNKNOWN")
  expect_error(.mojor_ir_validate_op_schema(bad, strict = TRUE), "OP_SCHEMA_UNKNOWN")
})

test_that("op schema validation enforces stmt/effect kind contracts", {  missing_id <- list(
    kind = "ssa_fn",
    entry = "bb1",
    blocks = list(
      bb1 = list(
        kind = "ssa_block",
        name = "bb1",
        params = list(),
        stmts = list(
          list(kind = "ssa_stmt", op = "const", args = character(0))
        ),
        term = list(kind = "ret", value = NULL)
      )
    )
  )
  bad_effect_kind <- list(
    kind = "ssa_fn",
    entry = "bb1",
    blocks = list(
      bb1 = list(
        kind = "ssa_block",
        name = "bb1",
        params = list(),
        stmts = list(
          list(kind = "ssa_effect", op = "const", args = character(0))
        ),
        term = list(kind = "ret", value = NULL)
      )
    )
  )
  bad_stmt_kind <- list(
    kind = "ssa_fn",
    entry = "bb1",
    blocks = list(
      bb1 = list(
        kind = "ssa_block",
        name = "bb1",
        params = list(),
        stmts = list(
          list(kind = "ssa_stmt", id = "%v1", op = "store_index", args = c("out", "%i", "%v"))
        ),
        term = list(kind = "ret", value = NULL)
      )
    )
  )

  missing_res <- .mojor_ir_validate_op_schema(missing_id)
  expect_false(isTRUE(missing_res$ok))
  expect_true(any(vapply(missing_res$diagnostics, function(d) identical(d$code, "OP_SCHEMA_ID_REQUIRED"), logical(1))))

  effect_res <- .mojor_ir_validate_op_schema(bad_effect_kind)
  expect_false(isTRUE(effect_res$ok))
  expect_true(any(vapply(effect_res$diagnostics, function(d) identical(d$code, "OP_SCHEMA_KIND_MISMATCH"), logical(1))))

  stmt_res <- .mojor_ir_validate_op_schema(bad_stmt_kind)
  expect_false(isTRUE(stmt_res$ok))
  expect_true(any(vapply(stmt_res$diagnostics, function(d) identical(d$code, "OP_SCHEMA_KIND_MISMATCH"), logical(1))))

  expect_error(.mojor_ir_validate_op_schema(missing_id, strict = TRUE), "OP_SCHEMA_ID_REQUIRED")
  expect_error(.mojor_ir_validate_op_schema(bad_effect_kind, strict = TRUE), "OP_SCHEMA_KIND_MISMATCH")
  expect_error(.mojor_ir_validate_op_schema(bad_stmt_kind, strict = TRUE), "OP_SCHEMA_KIND_MISMATCH")
})

test_that("op schema validation enforces store arity in canonical store namespace", {  bad_store <- list(
    kind = "ssa_fn",
    entry = "bb1",
    blocks = list(
      bb1 = list(
        kind = "ssa_block",
        name = "bb1",
        params = list(),
        stmts = list(
          list(kind = "ssa_effect", op = "store:foo", args = c("%v1", "%v2"))
        ),
        term = list(kind = "ret", value = NULL)
      )
    )
  )

  res <- .mojor_ir_validate_op_schema(bad_store)
  expect_false(isTRUE(res$ok))
  expect_true(any(vapply(res$diagnostics, function(d) identical(d$code, "OP_SCHEMA_ARITY"), logical(1))))
  expect_error(.mojor_ir_validate_op_schema(bad_store, strict = TRUE), "OP_SCHEMA_ARITY")
})

test_that("op schema validation catches call/effect mismatch and unknown stmt kinds", {  bad_call_effect <- list(
    kind = "ssa_fn",
    entry = "bb1",
    blocks = list(
      bb1 = list(
        kind = "ssa_block",
        name = "bb1",
        params = list(),
        stmts = list(
          list(kind = "ssa_effect", op = "call:sin", args = c("%v1"))
        ),
        term = list(kind = "ret", value = NULL)
      )
    )
  )
  bad_kind <- list(
    kind = "ssa_fn",
    entry = "bb1",
    blocks = list(
      bb1 = list(
        kind = "ssa_block",
        name = "bb1",
        params = list(),
        stmts = list(
          list(kind = "ssa_weird", op = "const", args = character(0))
        ),
        term = list(kind = "ret", value = NULL)
      )
    )
  )

  call_res <- .mojor_ir_validate_op_schema(bad_call_effect)
  expect_false(isTRUE(call_res$ok))
  expect_true(any(vapply(call_res$diagnostics, function(d) identical(d$code, "OP_SCHEMA_KIND_MISMATCH"), logical(1))))
  expect_error(.mojor_ir_validate_op_schema(bad_call_effect, strict = TRUE), "OP_SCHEMA_KIND_MISMATCH")

  kind_res <- .mojor_ir_validate_op_schema(bad_kind)
  expect_false(isTRUE(kind_res$ok))
  expect_true(any(vapply(kind_res$diagnostics, function(d) identical(d$code, "OP_SCHEMA_STMT_KIND_UNKNOWN"), logical(1))))
  expect_error(.mojor_ir_validate_op_schema(bad_kind, strict = TRUE), "OP_SCHEMA_STMT_KIND_UNKNOWN")
})

test_that("SSA verifier schema checks are gated by strict mode", {  bad <- list(
    kind = "ssa_fn",
    entry = "bb1",
    blocks = list(
      bb1 = list(
        kind = "ssa_block",
        name = "bb1",
        params = list(),
        stmts = list(
          list(kind = "ssa_stmt", id = "%v1", op = "mystery:thing", args = character(0))
        ),
        term = list(kind = "ret", value = "%v1")
      )
    )
  )

  expect_silent(.mojor_ir_verify_ssa(bad))
  expect_error(.mojor_ir_verify_ssa(bad, strict_schema = TRUE), "OP_SCHEMA_UNKNOWN")

  old <- options(mojor.ssa_schema_strict = TRUE)
  on.exit(options(old), add = TRUE)
  expect_error(.mojor_ir_verify_ssa(bad), "OP_SCHEMA_UNKNOWN")
  expect_silent(.mojor_ir_verify_ssa(bad, strict_schema = FALSE))
})

test_that("SSA verifier strict schema catches stmt/effect kind mismatches", {  bad <- list(
    kind = "ssa_fn",
    entry = "bb1",
    blocks = list(
      bb1 = list(
        kind = "ssa_block",
        name = "bb1",
        params = list(),
        stmts = list(
          list(kind = "ssa_effect", op = "const", args = character(0))
        ),
        term = list(kind = "ret", value = NULL)
      )
    )
  )

  expect_error(.mojor_ir_verify_ssa(bad, strict_schema = TRUE), "OP_SCHEMA_KIND_MISMATCH")
  expect_silent(.mojor_ir_verify_ssa(bad, strict_schema = FALSE))
})

test_that("strict schema accepts reduction effect ops via canonical compatibility mapping", {  ssa <- list(
    kind = "ssa_fn",
    entry = "bb1",
    blocks = list(
      bb1 = list(
        kind = "ssa_block",
        name = "bb1",
        params = list(),
        stmts = list(
          list(
            kind = "ssa_effect",
            op = "scheduled_reduce",
            args = c("acc", "arg", "n_i"),
            reduce_mode = "tree",
            reduce_op = "sum",
            reduce_acc = "acc",
            reduce_arg = "arg",
            n_var = "n_i"
          ),
          list(
            kind = "ssa_effect",
            op = "scalar_reduce",
            args = c("acc", "arg"),
            reduce_mode = "linear",
            reduce_op = "sum",
            reduce_acc = "acc",
            reduce_arg = "arg",
            n_var = "n_i"
          )
        ),
        term = list(kind = "ret", value = NULL)
      )
    )
  )

  expect_silent(.mojor_ir_verify_ssa(ssa, strict_schema = TRUE))
})

test_that("SSA canonicalize rewrites op spellings and enforces schema in strict mode", {  legacy <- list(
    kind = "ssa_fn",
    entry = "bb1",
    blocks = list(
      bb1 = list(
        kind = "ssa_block",
        name = "bb1",
        params = list(),
        stmts = list(
          list(kind = "ssa_stmt", id = "%v1", op = "const", args = character(0)),
          list(kind = "ssa_stmt", id = "%v2", op = "binop:+", args = c("%v1", "%v1"))
        ),
        term = list(kind = "ret", value = "%v2")
      )
    )
  )
  strict_can <- .mojor_ssa_canonicalize(legacy, strict_schema = TRUE)
  strict_ops <- vapply(strict_can$blocks$bb1$stmts, function(st) st$op, character(1))
  expect_identical(strict_ops[[1]], "mojor.arith.const")
  expect_identical(strict_ops[[2]], "mojor.arith.add")

  bad <- legacy
  bad$blocks$bb1$stmts[[2]]$op <- "mystery:thing"
  expect_error(.mojor_ssa_canonicalize(bad, strict_schema = TRUE), "OP_SCHEMA_UNKNOWN")
  expect_silent(.mojor_ssa_canonicalize(bad, strict_schema = FALSE))
})

test_that("SSA backend prep strict mode enforces op schema diagnostics", {  ir <- .mojor_ir_return(
    .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_const(1))
  )

  orig_to_ssa <- .mojor_ir_to_ssa
  on.exit({
    .mojor_ir_to_ssa <<- orig_to_ssa
  }, add = TRUE)

  .mojor_ir_to_ssa <<- function(node) {
    ssa <- orig_to_ssa(node)
    entry <- ssa$entry
    blk <- ssa$blocks[[entry]]
    extra <- list(kind = "ssa_stmt", id = "%v9999", op = "mystery:thing", args = character(0))
    if (is.null(blk$stmts)) {
      blk$stmts <- list(extra)
    } else {
      blk$stmts <- c(blk$stmts, list(extra))
    }
    ssa$blocks[[entry]] <- blk
    ssa
  }

  expect_silent(
    .mojor_ir_prepare_ssa_backend(
      ir,
      layout_ctx = list(n_var = "n_i", type_env = list(x = "f64")),
      opt_level = 0,
      strict_schema = FALSE,
      verify = TRUE
    )
  )
  expect_error(
    .mojor_ir_prepare_ssa_backend(
      ir,
      layout_ctx = list(n_var = "n_i", type_env = list(x = "f64")),
      opt_level = 0,
      strict_schema = TRUE,
      verify = TRUE
    ),
    "OP_SCHEMA_UNKNOWN"
  )
})
