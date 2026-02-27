library(testthat)

has_effect_op <- function(ssa, op) {
  for (bn in names(ssa$blocks)) {
    blk <- ssa$blocks[[bn]]
    if (is.null(blk$stmts) || length(blk$stmts) == 0) next
    for (st in blk$stmts) {
      if (is.null(st$kind) || !identical(st$kind, "ssa_effect")) next
      if (identical(st$op, op)) return(TRUE)
    }
  }
  FALSE
}

test_that("SSA CFG lowering uses explicit branch edges for break in for-loop", {  f <- function(x, n) {
    acc <- 0
    for (i in seq_len(n)) {
      if (x[i] < 0) break
      acc <- acc + x[i]
    }
    acc
  }

  prep <- .mojor_ir_prepare_ssa_backend(
    f,
    layout_ctx = list(n_var = "n_i", type_env = list(x = "f64[]", n = "i32")),
    opt_level = 0
  )

  ssa <- prep$ssa$raw
  fmt <- .mojor_ir_ssa_format(ssa)
  expect_false(has_effect_op(ssa, "stmt:break"))
  expect_match(fmt, "br loop_exit", fixed = TRUE)
  expect_silent(.mojor_ir_verify_ssa(ssa))
})

test_that("SSA CFG lowering uses explicit loop-header backedge for next in for-loop", {  f <- function(x, n) {
    acc <- 0
    for (i in seq_len(n)) {
      if (x[i] < 0) next
      acc <- acc + x[i]
    }
    acc
  }

  prep <- .mojor_ir_prepare_ssa_backend(
    f,
    layout_ctx = list(n_var = "n_i", type_env = list(x = "f64[]", n = "i32")),
    opt_level = 0
  )

  ssa <- prep$ssa$raw
  fmt <- .mojor_ir_ssa_format(ssa)
  expect_false(has_effect_op(ssa, "stmt:next"))
  expect_match(fmt, "loop_next", fixed = TRUE)
  expect_match(fmt, "br loop_header", fixed = TRUE)
  expect_silent(.mojor_ir_verify_ssa(ssa))
})

test_that("SSA CFG lowering uses explicit branch edges for break in while-loop", {  f <- function(x, n) {
    i <- 1
    acc <- 0
    while (i <= n) {
      if (x[i] < 0) break
      acc <- acc + x[i]
      i <- i + 1
    }
    acc
  }

  prep <- .mojor_ir_prepare_ssa_backend(
    f,
    layout_ctx = list(n_var = "n_i", type_env = list(x = "f64[]", n = "i32")),
    opt_level = 0
  )

  ssa <- prep$ssa$raw
  fmt <- .mojor_ir_ssa_format(ssa)
  expect_false(has_effect_op(ssa, "stmt:break"))
  expect_match(fmt, "br while_exit", fixed = TRUE)
  expect_silent(.mojor_ir_verify_ssa(ssa))
})

test_that("SSA CFG lowering remains verifier-clean when both if arms terminate in loop body", {  f <- function(x, n) {
    acc <- 0
    for (i in seq_len(n)) {
      if (x[i] < 0) break else next
    }
    acc
  }

  prep <- .mojor_ir_prepare_ssa_backend(
    f,
    layout_ctx = list(n_var = "n_i", type_env = list(x = "f64[]", n = "i32")),
    opt_level = 0
  )

  ssa <- prep$ssa$raw
  expect_false(has_effect_op(ssa, "stmt:break"))
  expect_false(has_effect_op(ssa, "stmt:next"))
  expect_silent(.mojor_ir_verify_ssa(ssa))
})
