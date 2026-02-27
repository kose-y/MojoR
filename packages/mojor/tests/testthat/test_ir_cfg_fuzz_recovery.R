library(testthat)

mk_reducible_cfg_fuzz_ssa <- function(seed, max_body = 6L) {
  stopifnot(is.numeric(seed), length(seed) == 1)
  seed <- as.integer(seed)
  stopifnot(seed >= 1L)
  stopifnot(is.numeric(max_body), length(max_body) == 1)
  max_body <- as.integer(max_body)
  stopifnot(max_body >= 2L)

  next_value_id <- local({
    idx <- 0L
    function() {
      idx <<- idx + 1L
      paste0("%v", idx)
    }
  })

  body_n <- (seed %% max_body) + 1L
  blocks <- list()

  blocks$entry <- list(
    kind = "ssa_block",
    name = "entry",
    params = list(list(id = "%arg1", var = "c")),
    stmts = list(),
    term = list(kind = "br", target = "header", args = c("%arg1"))
  )

  header_iv <- next_value_id()
  header_cond <- next_value_id()
  blocks$header <- list(
    kind = "ssa_block",
    name = "header",
    params = list(list(id = header_iv, var = "c")),
    stmts = list(
      list(kind = "ssa_stmt", id = header_cond, op = "const", args = character(0))
    ),
    term = list(
      kind = "condbr",
      cond = header_cond,
      then = "body_1",
      "else" = "exit",
      then_args = character(0),
      else_args = character(0)
    )
  )

  for (i in seq_len(body_n)) {
    bname <- paste0("body_", i)
    next_main <- if (i < body_n) paste0("body_", i + 1L) else "latch"
    skip_target <- next_main
    use_side <- ((seed + i) %% 2L) == 0L

    if (isTRUE(use_side)) {
      side_name <- paste0("side_", i)
      cond_id <- next_value_id()
      blocks[[bname]] <- list(
        kind = "ssa_block",
        name = bname,
        params = list(),
        stmts = list(
          list(kind = "ssa_stmt", id = cond_id, op = "const", args = character(0))
        ),
        term = list(
          kind = "condbr",
          cond = cond_id,
          then = next_main,
          "else" = side_name,
          then_args = character(0),
          else_args = character(0)
        )
      )
      blocks[[side_name]] <- list(
        kind = "ssa_block",
        name = side_name,
        params = list(),
        stmts = list(),
        term = list(kind = "br", target = skip_target, args = character(0))
      )
    } else {
      blocks[[bname]] <- list(
        kind = "ssa_block",
        name = bname,
        params = list(),
        stmts = list(),
        term = list(kind = "br", target = skip_target, args = character(0))
      )
    }
  }

  latch_arg <- next_value_id()
  blocks$latch <- list(
    kind = "ssa_block",
    name = "latch",
    params = list(),
    stmts = list(
      list(kind = "ssa_stmt", id = latch_arg, op = "const", args = character(0))
    ),
    term = list(
      kind = "br",
      target = "header",
      args = c(latch_arg)
    )
  )

  blocks$exit <- list(
    kind = "ssa_block",
    name = "exit",
    params = list(),
    stmts = list(),
    term = list(kind = "ret", value = NULL)
  )

  list(kind = "ssa_fn", entry = "entry", blocks = blocks)
}

test_that("loop recovery/recanonicalize remain stable on reducible CFG fuzz corpus", {  seeds <- 1:60
  for (seed in seeds) {
    ssa <- mk_reducible_cfg_fuzz_ssa(seed)
    expect_silent(.mojor_ir_verify_ssa(ssa))

    rec <- .mojor_loop_recovery_analysis(
      ssa,
      require_reducible = TRUE,
      require_single_exit = TRUE
    )
    expect_identical(rec$status, "Recovered", info = paste0("seed=", seed))
    expect_true(isTRUE(rec$boundary_verifier_enabled), info = paste0("seed=", seed))
    expect_true(length(rec$loops) >= 1L, info = paste0("seed=", seed))

    recan <- .mojor_recanonicalize_loop_cfg(
      ssa,
      recovery = rec,
      verify = TRUE
    )
    expect_identical(recan$status, "Recanonicalized", info = paste0("seed=", seed))
    expect_true(isTRUE(recan$boundary_verifier_enabled), info = paste0("seed=", seed))
    expect_silent(.mojor_ir_verify_ssa(recan$ssa))

    post <- .mojor_verify_semantic_ir(
      recan$ssa,
      mode = "boundary",
      boundary = "post_recanonicalize",
      opts = list(loop_recovery = recan$recovery)
    )
    expect_true(isTRUE(post$ok), info = paste0("seed=", seed))
  }
})
