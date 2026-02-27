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

mk_break_next_stress_fn <- function(seed) {
  stopifnot(is.numeric(seed), length(seed) == 1)
  seed <- as.integer(seed)
  stopifnot(seed >= 1L)

  if ((seed %% 2L) == 1L) {
    lo <- (seed %% 3L) + 1L
    hi <- (seed %% 5L) + 4L
    txt <- sprintf(
      paste(
        "function(x, n) {",
        "  acc <- 0",
        "  for (i in seq_len(n)) {",
        "    if (x[i] < -%d) next",
        "    if (x[i] > %d) break",
        "    if (x[i] < 0) {",
        "      acc <- acc + 1",
        "      next",
        "    }",
        "    if (x[i] > %d) break",
        "    acc <- acc + x[i]",
        "  }",
        "  acc",
        "}",
        sep = "\n"
      ),
      lo,
      hi,
      lo
    )
  } else {
    lo <- (seed %% 4L) + 1L
    hi <- (seed %% 6L) + 5L
    txt <- sprintf(
      paste(
        "function(x, n) {",
        "  i <- 1",
        "  acc <- 0",
        "  while (i <= n) {",
        "    if (x[i] < -%d) {",
        "      i <- i + 1",
        "      next",
        "    }",
        "    if (x[i] > %d) break",
        "    if (x[i] < 0) {",
        "      acc <- acc + 1",
        "      i <- i + 1",
        "      next",
        "    }",
        "    if (x[i] > %d) break",
        "    acc <- acc + x[i]",
        "    i <- i + 1",
        "  }",
        "  acc",
        "}",
        sep = "\n"
      ),
      lo,
      hi,
      lo
    )
  }

  eval(parse(text = txt), envir = baseenv())
}

test_that("CFG lowering remains break/next-clean across stress corpus", {  seeds <- 1:24
  for (seed in seeds) {
    fn <- mk_break_next_stress_fn(seed)
    prep <- .mojor_ir_prepare_ssa_backend(
      fn,
      layout_ctx = list(n_var = "n_i", type_env = list(x = "f64[]", n = "i32")),
      opt_level = 0
    )

    ssa <- prep$ssa$raw
    expect_false(has_effect_op(ssa, "stmt:break"), info = paste0("seed=", seed))
    expect_false(has_effect_op(ssa, "stmt:next"), info = paste0("seed=", seed))
    expect_silent(.mojor_ir_verify_ssa(ssa))

    boundary <- .mojor_verify_semantic_ir(
      ssa,
      mode = "boundary",
      boundary = "post_structured_lowering"
    )
    expect_true(isTRUE(boundary$ok), info = paste0("seed=", seed))
  }
})

