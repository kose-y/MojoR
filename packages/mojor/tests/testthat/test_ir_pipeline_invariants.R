library(testthat)

test_that("Stage 5c pipeline includes boundary verification and typing after recanonicalize", {  ir <- .mojor_ir_build_stmt(quote(x <- y + 1))

  prep <- .mojor_ir_prepare_ssa_backend(
    ir,
    layout_ctx = list(n_var = "n_i", type_env = list(y = "f64")),
    opt_level = 0,
    verify = TRUE,
    enforce_typing_boundary = TRUE
  )

  p <- prep$pipeline
  expect_true("verify_boundary_post_structured_lowering" %in% p)
  expect_true("verify_boundary_post_memory_canonicalize" %in% p)
  expect_true("typing" %in% p)
  expect_true("verify_boundary_post_typing" %in% p)
  expect_true(match("to_ssa", p) < match("memory_canonicalize", p))
  expect_true(match("memory_canonicalize", p) < match("annotate_effects_resources", p))
  expect_true(match("annotate_effects_resources", p) < match("recanonicalize_loop_cfg", p))
  expect_true(match("recanonicalize_loop_cfg", p) < match("typing", p))
  expect_true(match("fusion_candidate_analysis", p) < match("typing", p))
  expect_true(match("typing", p) < match("backend_cfg", p))

  expect_identical(prep$ssa$memory_canonical$kind, "ssa_fn")
  expect_identical(prep$ssa$effect_annotated$kind, "ssa_fn")
  expect_identical(prep$ssa$typed$kind, "ssa_fn")
  expect_true(is.list(prep$verifier$boundary))
  expect_true(isTRUE(.mojor_ir_check_ssa_backend_invariants(prep)))
})

test_that("Stage 5c pipeline allows recanonicalize disable while preserving typing boundary", {  ir <- .mojor_ir_build_stmt(quote(x <- y + 1))

  prep <- .mojor_ir_prepare_ssa_backend(
    ir,
    layout_ctx = list(n_var = "n_i", type_env = list(y = "f64")),
    opt_level = 0,
    verify = TRUE,
    recanonicalize_loops = FALSE,
    enforce_typing_boundary = TRUE
  )

  p <- prep$pipeline
  expect_false("recanonicalize_loop_cfg" %in% p)
  expect_true("typing" %in% p)
  expect_true("verify_boundary_post_typing" %in% p)
  expect_true(match("annotate_effects_resources", p) < match("typing", p))
  expect_true(isTRUE(.mojor_ir_check_ssa_backend_invariants(prep)))
})

test_that("core and boundary verifier behavior is deterministic for memory boundary", {  legacy <- list(
    kind = "ssa_fn",
    entry = "bb1",
    blocks = list(
      bb1 = list(
        kind = "ssa_block",
        name = "bb1",
        params = list(),
        stmts = list(
          list(kind = "ssa_stmt", id = "%v1", op = "index", args = c("x", "%i"))
        ),
        term = list(kind = "ret", value = "%v1")
      )
    )
  )

  core <- .mojor_verify_semantic_ir(legacy, mode = "core", boundary = "none")
  boundary <- .mojor_verify_semantic_ir(legacy, mode = "boundary", boundary = "post_memory_canonicalize")

  expect_true(isTRUE(core$ok))
  expect_false(isTRUE(boundary$ok))
  expect_true(any(vapply(boundary$diagnostics, function(d) identical(d$code, "LEGACY_MEMORY_SPELLING"), logical(1))))
})
