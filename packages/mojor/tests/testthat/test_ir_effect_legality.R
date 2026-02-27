library(testthat)

test_that("expression effect/resource summary reports read resources with stable ids", {  expr <- .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
  info <- .mojor_ir_expr_effect_resource_summary(expr)

  expect_true("Read" %in% info$effect_classes)
  expect_true(length(info$resources) >= 1L)
  expect_identical(info$resources[[1]]$resource_kind, "Ref")
  expect_identical(info$resources[[1]]$resource_id, "var:x")
  expect_identical(info$resources[[1]]$access, "Read")
})

test_that("LICM read hoisting is blocked when loop writes same resource", {  loop <- .mojor_ir_loop(
    var = "i",
    range = .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_var("n")),
    body = .mojor_ir_block(list(
      .mojor_ir_assign(
        .mojor_ir_var("tmp"),
        .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_const("1")))
      ),
      .mojor_ir_assign(
        .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i"))),
        .mojor_ir_var("tmp")
      )
    ))
  )

  result <- .mojor_ir_licm_loop(loop, temp_counter = 0, allow_reads = TRUE)
  expect_identical(length(result$stmts), 0L)
  expect_identical(result$loop$body$stmts[[1]]$rhs$kind, "index")
})

test_that("SSA effect/resource summary derives resource_id from operand identity", {  read_stmt <- list(kind = "ssa_stmt", id = "%v1", op = "mojor.mem.load", args = c("%arg2", "%arg3"))
  read_info <- .mojor_ssa_stmt_effect_resource_summary(read_stmt)
  expect_identical(read_info$effect_class, "Read")
  expect_identical(read_info$resources[[1]]$resource_kind, "Ref")
  expect_identical(read_info$resources[[1]]$resource_id, "arg_slot:2")

  write_stmt <- list(kind = "ssa_effect", op = "mojor.mem.store", args = c("%v9", "%arg3", "%v1"))
  write_info <- .mojor_ssa_stmt_effect_resource_summary(write_stmt)
  expect_identical(write_info$effect_class, "Write")
  expect_identical(write_info$resources[[1]]$resource_kind, "Local")
  expect_identical(write_info$resources[[1]]$resource_id, "ssa_value:9")
})

test_that("SSA effect/resource annotation persists on statements", {  ssa <- list(
    kind = "ssa_fn",
    entry = "bb1",
    blocks = list(
      bb1 = list(
        kind = "ssa_block",
        name = "bb1",
        params = list(),
        stmts = list(
          list(kind = "ssa_stmt", id = "%v1", op = "mojor.mem.load", args = c("%arg1", "%arg2")),
          list(kind = "ssa_stmt", id = "%v2", op = "mojor.arith.const", args = character(0))
        ),
        term = list(kind = "ret", value = "%v2")
      )
    )
  )

  out <- .mojor_ssa_annotate_effects_resources(ssa)
  expect_identical(out$blocks$bb1$stmts[[1]]$effect_class, "Read")
  expect_identical(out$blocks$bb1$stmts[[1]]$resource_id, "arg_slot:1")
  expect_identical(out$blocks$bb1$stmts[[2]]$effect_class, "None")
})

test_that("SSA prune_dead_stmts keeps unused ReadsMem values", {  ssa <- list(
    kind = "ssa_fn",
    entry = "entry",
    blocks = list(
      entry = list(
        kind = "ssa_block",
        name = "entry",
        params = list(
          list(id = "%arg1", var = "x"),
          list(id = "%arg2", var = "i")
        ),
        stmts = list(
          list(kind = "ssa_stmt", id = "%v1", op = "mojor.mem.load", args = c("%arg1", "%arg2")),
          list(kind = "ssa_stmt", id = "%v2", op = "mojor.arith.const", args = character(0))
        ),
        term = list(kind = "ret", value = "%v2")
      )
    )
  )

  opt <- .mojor_ir_ssa_optimize(ssa, passes = c("prune_dead_stmts"), verify = TRUE)
  ids <- vapply(opt$blocks$entry$stmts, function(st) as.character(st$id), character(1))
  expect_true("%v1" %in% ids)
  expect_true("%v2" %in% ids)
})

test_that("RNG effect/resource summary uses stable RNG state resource", {  expr1 <- .mojor_ir_call("rnorm", list(.mojor_ir_var("n")))
  expr2 <- .mojor_ir_call("rgamma", list(.mojor_ir_var("n"), .mojor_ir_const("2.0"), .mojor_ir_const("1.0")))

  info1 <- .mojor_ir_expr_effect_resource_summary(expr1)
  info2 <- .mojor_ir_expr_effect_resource_summary(expr2)

  expect_true("RNG" %in% info1$effect_classes)
  expect_true("RNG" %in% info2$effect_classes)

  rng1 <- Filter(function(r) identical(as.character(r$access), "RNG"), info1$resources)
  rng2 <- Filter(function(r) identical(as.character(r$access), "RNG"), info2$resources)
  expect_true(length(rng1) >= 1L)
  expect_true(length(rng2) >= 1L)
  expect_identical(rng1[[1]]$resource_kind, "Ref")
  expect_identical(rng1[[1]]$resource_id, "rng:state")
  expect_identical(rng2[[1]]$resource_id, "rng:state")
})

test_that("SSA RNG effect/resource summary uses shared RNG state resource", {  stmt1 <- list(kind = "ssa_stmt", id = "%v1", op = "call:rnorm", args = c("%arg1"))
  stmt2 <- list(kind = "ssa_stmt", id = "%v2", op = "mojor.misc.call.rpois", args = c("%arg2"))

  info1 <- .mojor_ssa_stmt_effect_resource_summary(stmt1)
  info2 <- .mojor_ssa_stmt_effect_resource_summary(stmt2)

  expect_identical(info1$effect_class, "RNG")
  expect_identical(info2$effect_class, "RNG")
  expect_identical(info1$resources[[1]]$resource_kind, "Ref")
  expect_identical(info1$resources[[1]]$resource_id, "rng:state")
  expect_identical(info2$resources[[1]]$resource_id, "rng:state")
})

test_that("SSA recognizes full RNG distribution set in both legacy and canonical call ops", {  rng_fns <- .mojor_ssa_rng_call_fns()
  expect_true(length(rng_fns) >= 19)

  for (fn in rng_fns) {
    legacy <- list(kind = "ssa_stmt", id = "%v1", op = paste0("call:", fn), args = c("%arg1"))
    canonical <- list(kind = "ssa_stmt", id = "%v2", op = paste0("mojor.misc.call.", fn), args = c("%arg1"))

    info_legacy <- .mojor_ssa_stmt_effect_resource_summary(legacy)
    info_canonical <- .mojor_ssa_stmt_effect_resource_summary(canonical)

    expect_identical(info_legacy$effect_class, "RNG", label = paste0("legacy:", fn))
    expect_identical(info_canonical$effect_class, "RNG", label = paste0("canonical:", fn))
    expect_identical(info_legacy$resources[[1]]$resource_id, "rng:state", label = paste0("legacy:", fn))
    expect_identical(info_canonical$resources[[1]]$resource_id, "rng:state", label = paste0("canonical:", fn))
  }
})
