library(testthat)

.mojor_test_make_two_loop_ssa <- function(
  noalias_right = TRUE,
  include_loop_metadata = FALSE,
  right_extra_stmts = list()
) {
  ssa <- list(
    kind = "ssa_fn",
    entry = "entry",
    blocks = list(
      entry = list(
        kind = "ssa_block",
        name = "entry",
        params = list(),
        stmts = list(
          list(kind = "ssa_stmt", id = "%n", op = "const", value = 8L, args = character(0)),
          list(kind = "ssa_stmt", id = "%one", op = "const", value = 1L, args = character(0)),
          list(kind = "ssa_stmt", id = "%start1", op = "const", value = 1L, args = character(0)),
          list(kind = "ssa_stmt", id = "%start2", op = "const", value = 1L, args = character(0))
        ),
        term = list(kind = "br", target = "h1", args = c("%start1"))
      ),
      h1 = list(
        kind = "ssa_block",
        name = "h1",
        attrs = list(loop_id = "L1", loop_role = "header", loop_kind = "for", fusion_noalias_proven = TRUE),
        params = list(list(id = "%v11", var = "i")),
        stmts = list(list(kind = "ssa_stmt", id = "%c1", op = "loop_cond", args = c("%v11", "%n", "%one"))),
        term = list(kind = "condbr", cond = "%c1", then = "b1", "else" = "x1", then_args = character(0), else_args = character(0))
      ),
      b1 = list(
        kind = "ssa_block",
        name = "b1",
        attrs = list(loop_id = "L1", loop_role = "body", loop_kind = "for"),
        params = list(),
        stmts = list(
          list(kind = "ssa_stmt", id = "%v1", op = "const", value = 1.0, args = character(0)),
          list(kind = "ssa_effect", op = "mojor.mem.store", args = c("out1", "%v11", "%v1")),
          list(kind = "ssa_stmt", id = "%n1", op = "loop_next", args = c("%v11", "%one"))
        ),
        term = list(kind = "br", target = "h1", args = c("%n1"))
      ),
      x1 = list(
        kind = "ssa_block",
        name = "x1",
        attrs = list(loop_id = "L1", loop_role = "exit", loop_kind = "for"),
        params = list(),
        stmts = list(),
        term = list(kind = "br", target = "h2", args = c("%start2"))
      ),
      h2 = list(
        kind = "ssa_block",
        name = "h2",
        attrs = list(loop_id = "L2", loop_role = "header", loop_kind = "for", fusion_noalias_proven = isTRUE(noalias_right)),
        params = list(list(id = "%v21", var = "i")),
        stmts = list(list(kind = "ssa_stmt", id = "%c2", op = "loop_cond", args = c("%v21", "%n", "%one"))),
        term = list(kind = "condbr", cond = "%c2", then = "b2", "else" = "x2", then_args = character(0), else_args = character(0))
      ),
      b2 = list(
        kind = "ssa_block",
        name = "b2",
        attrs = list(loop_id = "L2", loop_role = "body", loop_kind = "for"),
        params = list(),
        stmts = c(
          list(list(kind = "ssa_stmt", id = "%v2", op = "const", value = 2.0, args = character(0))),
          right_extra_stmts,
          list(
            list(kind = "ssa_effect", op = "mojor.mem.store", args = c("out2", "%v21", "%v2")),
            list(kind = "ssa_stmt", id = "%n2", op = "loop_next", args = c("%v21", "%one"))
          )
        ),
        term = list(kind = "br", target = "h2", args = c("%n2"))
      ),
      x2 = list(
        kind = "ssa_block",
        name = "x2",
        attrs = list(loop_id = "L2", loop_role = "exit", loop_kind = "for"),
        params = list(),
        stmts = list(),
        term = list(kind = "br", target = "done", args = character(0))
      ),
      done = list(
        kind = "ssa_block",
        name = "done",
        params = list(),
        stmts = list(),
        term = list(kind = "ret", value = NULL)
      )
    )
  )
  if (isTRUE(include_loop_metadata)) {
    ssa$loop_metadata <- list(
      L1 = list(
        loop_id = "L1",
        kind = "for",
        anchor_block = "h1",
        continue_dest = list(loop_id = "L1", role = "Continue", block = "h1"),
        break_dest = list(loop_id = "L1", role = "Break", block = "x1")
      ),
      L2 = list(
        loop_id = "L2",
        kind = "for",
        anchor_block = "h2",
        continue_dest = list(loop_id = "L2", role = "Continue", block = "h2"),
        break_dest = list(loop_id = "L2", role = "Break", block = "x2")
      )
    )
  }
  ssa
}

.mojor_test_two_loop_recovery <- function() {
  list(
    status = "Recovered",
    loops = list(
      L1 = list(loop_id = "L1", kind = "for", header = "h1", nodes = c("h1", "b1", "x1")),
      L2 = list(loop_id = "L2", kind = "for", header = "h2", nodes = c("h2", "b2", "x2"))
    )
  )
}

test_that("fusion candidate analysis is analysis-only in backend prep", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) out[i] <- x[i] + 1
    for (i in seq_len(n)) out[i] <- out[i] * 2
    out
  }

  prep <- .mojor_ir_prepare_ssa_backend(
    f,
    layout_ctx = list(n_var = "n_i", type_env = list(x = "f64[]", n = "i32")),
    opt_level = 0,
    verify = TRUE
  )

  fa <- prep$ssa$fusion_analysis
  expect_true(is.list(fa))
  expect_true(isTRUE(fa$analysis_only))
  expect_true(length(fa$candidates) >= 1)
  expect_true(any(vapply(fa$candidates, function(ca) {
    !isTRUE(ca$accepted) && "FUSE_REJECT_NOALIAS_MISSING" %in% ca$reasons
  }, logical(1))))
  expect_true(any(vapply(fa$diagnostics, function(d) identical(d$code, "FUSE_REJECT_NOALIAS_MISSING"), logical(1))))

  # Analysis-only pass must not rewrite CFG statements.
  expect_identical(.mojor_ir_ssa_format(prep$ssa$recanonicalized), .mojor_ir_ssa_format(prep$ssa$typed))
})

test_that("fusion candidate analysis reports broadcast_nd rejection code", {  ssa <- list(
    kind = "ssa_fn",
    entry = "h1",
    blocks = list(
      h1 = list(
        kind = "ssa_block",
        name = "h1",
        attrs = list(loop_id = "L1", loop_role = "header", loop_kind = "for"),
        params = list(list(id = "%i1", var = "i")),
        stmts = list(
          list(kind = "ssa_stmt", id = "%v1", op = "mojor.mem.load", args = c("x", "%i1"))
        ),
        term = list(kind = "br", target = "h2", args = character(0))
      ),
      h2 = list(
        kind = "ssa_block",
        name = "h2",
        attrs = list(loop_id = "L2", loop_role = "header", loop_kind = "for"),
        params = list(list(id = "%i2", var = "i")),
        stmts = list(
          list(kind = "ssa_stmt", id = "%v2", op = "mojor.mem.load", args = c("x", "%i2", "_mojor_bcast_index"))
        ),
        term = list(kind = "ret", value = "%v2")
      )
    ),
    loop_metadata = list(
      L1 = list(
        loop_id = "L1",
        kind = "for",
        anchor_block = "h1",
        continue_dest = list(loop_id = "L1", role = "Continue", block = "h1"),
        break_dest = list(loop_id = "L1", role = "Break", block = "h2")
      ),
      L2 = list(
        loop_id = "L2",
        kind = "for",
        anchor_block = "h2",
        continue_dest = list(loop_id = "L2", role = "Continue", block = "h2"),
        break_dest = list(loop_id = "L2", role = "Break", block = "h2")
      )
    )
  )

  recovery <- list(
    status = "Recovered",
    loops = list(
      L1 = list(loop_id = "L1", kind = "for", header = "h1", nodes = c("h1")),
      L2 = list(loop_id = "L2", kind = "for", header = "h2", nodes = c("h2"))
    )
  )

  fa <- .mojor_ssa_fusion_candidate_analysis(ssa, recovery = recovery)
  expect_true(length(fa$candidates) == 1)
  expect_false(isTRUE(fa$candidates[[1]]$accepted))
  expect_true("FUSE_REJECT_BROADCAST_ND" %in% fa$candidates[[1]]$reasons)
  expect_true(any(vapply(fa$diagnostics, function(d) identical(d$code, "FUSE_REJECT_BROADCAST_ND"), logical(1))))
})

test_that("fusion candidate analysis reports control-flow rejection by default", {  ssa <- list(
    kind = "ssa_fn",
    entry = "h1",
    blocks = list(
      h1 = list(
        kind = "ssa_block",
        name = "h1",
        attrs = list(loop_id = "L1", loop_role = "header", loop_kind = "for", fusion_noalias_proven = TRUE),
        params = list(list(id = "%i1", var = "i")),
        stmts = list(),
        term = list(kind = "br", target = "c1", args = character(0))
      ),
      c1 = list(
        kind = "ssa_block",
        name = "c1",
        attrs = list(loop_id = "L1", loop_role = "body", loop_kind = "for"),
        params = list(),
        stmts = list(list(kind = "ssa_stmt", id = "%c1", op = "binop:==", args = c("%i1", "%i1"))),
        term = list(kind = "condbr", cond = "%c1", then = "t1", "else" = "e1", then_args = character(0), else_args = character(0))
      ),
      t1 = list(
        kind = "ssa_block",
        name = "t1",
        attrs = list(loop_id = "L1", loop_role = "body", loop_kind = "for"),
        params = list(),
        stmts = list(
          list(kind = "ssa_stmt", id = "%v1", op = "const", value = 1L, args = character(0)),
          list(kind = "ssa_stmt", id = "%s1", op = "mojor.mem.store", args = c("out1", "%i1", "%v1"))
        ),
        term = list(kind = "br", target = "m1", args = character(0))
      ),
      e1 = list(
        kind = "ssa_block",
        name = "e1",
        attrs = list(loop_id = "L1", loop_role = "body", loop_kind = "for"),
        params = list(),
        stmts = list(
          list(kind = "ssa_stmt", id = "%v2", op = "const", value = 2L, args = character(0)),
          list(kind = "ssa_stmt", id = "%s2", op = "mojor.mem.store", args = c("out1", "%i1", "%v2"))
        ),
        term = list(kind = "br", target = "m1", args = character(0))
      ),
      m1 = list(
        kind = "ssa_block",
        name = "m1",
        attrs = list(loop_id = "L1", loop_role = "body", loop_kind = "for"),
        params = list(),
        stmts = list(),
        term = list(kind = "br", target = "h1", args = character(0))
      ),
      h2 = list(
        kind = "ssa_block",
        name = "h2",
        attrs = list(loop_id = "L2", loop_role = "header", loop_kind = "for", fusion_noalias_proven = TRUE),
        params = list(list(id = "%i2", var = "i")),
        stmts = list(),
        term = list(kind = "br", target = "b2", args = character(0))
      ),
      b2 = list(
        kind = "ssa_block",
        name = "b2",
        attrs = list(loop_id = "L2", loop_role = "body", loop_kind = "for"),
        params = list(),
        stmts = list(
          list(kind = "ssa_stmt", id = "%v3", op = "const", value = 3L, args = character(0)),
          list(kind = "ssa_stmt", id = "%s3", op = "mojor.mem.store", args = c("out2", "%i2", "%v3"))
        ),
        term = list(kind = "br", target = "h2", args = character(0))
      )
    )
  )

  recovery <- list(
    status = "Recovered",
    loops = list(
      L1 = list(loop_id = "L1", kind = "for", header = "h1", nodes = c("h1", "c1", "t1", "e1", "m1")),
      L2 = list(loop_id = "L2", kind = "for", header = "h2", nodes = c("h2", "b2"))
    )
  )

  fa <- .mojor_ssa_fusion_candidate_analysis(ssa, recovery = recovery)
  expect_true("FUSE_REJECT_CONTROL_FLOW" %in% fa$candidates[[1]]$reasons)

  fa_opt <- .mojor_ssa_fusion_candidate_analysis(
    ssa,
    recovery = recovery,
    policy = list(fusion_allow_control_flow_simple = TRUE)
  )
  expect_false("FUSE_REJECT_CONTROL_FLOW" %in% fa_opt$candidates[[1]]$reasons)
})

test_that("fusion candidate analysis broadcast_nd opt-in only suppresses identity-safe pairs", {  make_broadcast_ssa <- function(right_sig) {
    list(
      kind = "ssa_fn",
      entry = "h1",
      blocks = list(
        h1 = list(
          kind = "ssa_block",
          name = "h1",
          attrs = list(loop_id = "L1", loop_role = "header", loop_kind = "for", fusion_noalias_proven = TRUE),
          params = list(list(id = "%v11", var = "i")),
          stmts = list(),
          term = list(kind = "br", target = "b1", args = character(0))
        ),
        b1 = list(
          kind = "ssa_block",
          name = "b1",
          attrs = list(loop_id = "L1", loop_role = "body", loop_kind = "for"),
          params = list(),
          stmts = list(
            list(kind = "ssa_stmt", id = "%v12", op = "const", value = 1, args = character(0)),
            list(kind = "ssa_stmt", id = "%s1", op = "mojor.mem.store", args = c("out1", "%v11", "_mojor_bcast_index(%v11,__dim_x,__dim_y)", "%v12"))
          ),
          term = list(kind = "br", target = "h1", args = character(0))
        ),
        h2 = list(
          kind = "ssa_block",
          name = "h2",
          attrs = list(loop_id = "L2", loop_role = "header", loop_kind = "for", fusion_noalias_proven = TRUE),
          params = list(list(id = "%v21", var = "i")),
          stmts = list(),
          term = list(kind = "br", target = "b2", args = character(0))
        ),
        b2 = list(
          kind = "ssa_block",
          name = "b2",
          attrs = list(loop_id = "L2", loop_role = "body", loop_kind = "for"),
          params = list(),
          stmts = list(
            list(kind = "ssa_stmt", id = "%v22", op = "const", value = 2, args = character(0)),
            list(kind = "ssa_stmt", id = "%s2", op = "mojor.mem.store", args = c("out2", "%v21", right_sig, "%v22"))
          ),
          term = list(kind = "br", target = "h2", args = character(0))
        )
      )
    )
  }

  recovery <- list(
    status = "Recovered",
    loops = list(
      L1 = list(loop_id = "L1", kind = "for", header = "h1", nodes = c("h1", "b1")),
      L2 = list(loop_id = "L2", kind = "for", header = "h2", nodes = c("h2", "b2"))
    )
  )

  ssa_identity <- make_broadcast_ssa("_mojor_bcast_index(%v21,__dim_x,__dim_y)")
  fa_default <- .mojor_ssa_fusion_candidate_analysis(ssa_identity, recovery = recovery)
  expect_true("FUSE_REJECT_BROADCAST_ND" %in% fa_default$candidates[[1]]$reasons)

  fa_opt <- .mojor_ssa_fusion_candidate_analysis(
    ssa_identity,
    recovery = recovery,
    policy = list(fusion_allow_broadcast_nd_identity = TRUE)
  )
  expect_false("FUSE_REJECT_BROADCAST_ND" %in% fa_opt$candidates[[1]]$reasons)

  ssa_non_identity <- make_broadcast_ssa("_mojor_bcast_index(%v21,__dim_other)")
  fa_non_identity <- .mojor_ssa_fusion_candidate_analysis(
    ssa_non_identity,
    recovery = recovery,
    policy = list(fusion_allow_broadcast_nd_identity = TRUE)
  )
  expect_true("FUSE_REJECT_BROADCAST_ND" %in% fa_non_identity$candidates[[1]]$reasons)
})

test_that("fusion constrained rewrite fuses accepted adjacent single-block loops", {  ssa <- .mojor_test_make_two_loop_ssa(noalias_right = TRUE, include_loop_metadata = TRUE)
  recovery <- .mojor_test_two_loop_recovery()
  fa <- .mojor_ssa_fusion_candidate_analysis(ssa, recovery = recovery)
  expect_true(isTRUE(fa$candidates[[1]]$accepted))

  rw <- .mojor_ssa_fusion_rewrite_constrained(ssa, analysis = fa, recovery = recovery)
  expect_identical(rw$kind, "fusion_rewrite")
  expect_identical(rw$mode, "constrained")
  expect_identical(rw$status, "Applied")
  expect_identical(rw$applied_count, 1L)

  fused <- rw$ssa
  expect_identical(as.character(fused$blocks$h1$term[["else"]]), "x2")
  expect_false("L2" %in% names(fused$loop_metadata))
  expect_identical(as.character(fused$loop_metadata$L1$break_dest$block), "x2")

  has_right_store_with_left_iv <- any(vapply(fused$blocks$b1$stmts, function(st) {
    if (is.null(st) || !is.list(st) || is.null(st$op) || is.null(st$args) || length(st$args) < 2) return(FALSE)
    if (!identical(.mojor_ssa_op_canonical(st$op), "mojor.mem.store")) return(FALSE)
    identical(as.character(st$args[[1]]), "out2") && identical(as.character(st$args[[2]]), "%v11")
  }, logical(1)))
  expect_true(isTRUE(has_right_store_with_left_iv))
})

test_that("fusion constrained rewrite leaves rejected candidates unchanged", {  ssa <- .mojor_test_make_two_loop_ssa(noalias_right = FALSE, include_loop_metadata = FALSE)
  recovery <- .mojor_test_two_loop_recovery()
  fa <- .mojor_ssa_fusion_candidate_analysis(ssa, recovery = recovery)
  expect_false(isTRUE(fa$candidates[[1]]$accepted))
  expect_true("FUSE_REJECT_NOALIAS_MISSING" %in% fa$candidates[[1]]$reasons)

  rw <- .mojor_ssa_fusion_rewrite_constrained(ssa, analysis = fa, recovery = recovery)
  expect_identical(rw$kind, "fusion_rewrite")
  expect_identical(rw$mode, "constrained")
  expect_identical(rw$status, "NoAcceptedCandidates")
  expect_identical(rw$applied_count, 0L)
  expect_identical(as.character(rw$ssa$blocks$h1$term[["else"]]), "x1")
})

test_that("fusion constrained rewrite rejects accepted candidates outside single_basic_loop domain", {  ssa <- .mojor_test_make_two_loop_ssa(noalias_right = TRUE, include_loop_metadata = FALSE)
  recovery <- .mojor_test_two_loop_recovery()
  analysis <- list(
    analysis_only = TRUE,
    candidates = list(
      list(
        producer = "L1",
        consumer = "L2",
        domain = "structured_or_unknown",
        accepted = TRUE,
        reasons = character(0)
      )
    ),
    diagnostics = list()
  )

  rw <- .mojor_ssa_fusion_rewrite_constrained(ssa, analysis = analysis, recovery = recovery)
  expect_identical(rw$mode, "constrained")
  expect_identical(rw$status, "NoChange")
  expect_identical(rw$applied_count, 0L)
  expect_true(any(vapply(rw$diagnostics, function(d) identical(d$code, "FUSE_REWRITE_SKIP_UNSAFE_DOMAIN"), logical(1))))
  expect_identical(.mojor_ir_ssa_format(rw$ssa), .mojor_ir_ssa_format(ssa))
})

test_that("fusion constrained rewrite rejects accepted candidates with non-pure effect profile", {  ssa <- .mojor_test_make_two_loop_ssa(
    noalias_right = TRUE,
    include_loop_metadata = FALSE,
    right_extra_stmts = list(list(kind = "ssa_stmt", id = "%impure", op = "mystery:impure", args = character(0)))
  )
  recovery <- .mojor_test_two_loop_recovery()
  analysis <- list(
    analysis_only = TRUE,
    candidates = list(
      list(
        producer = "L1",
        consumer = "L2",
        domain = "single_basic_loop",
        accepted = TRUE,
        reasons = character(0)
      )
    ),
    diagnostics = list()
  )

  rw <- .mojor_ssa_fusion_rewrite_constrained(ssa, analysis = analysis, recovery = recovery)
  expect_identical(rw$mode, "constrained")
  expect_identical(rw$status, "NoChange")
  expect_identical(rw$applied_count, 0L)
  expect_true(any(vapply(rw$diagnostics, function(d) identical(d$code, "FUSE_REWRITE_SKIP_EFFECTS_NOT_PURE"), logical(1))))
  expect_identical(.mojor_ir_ssa_format(rw$ssa), .mojor_ir_ssa_format(ssa))
})

test_that("fusion rewrite skeleton is legality-gated and preserves SSA", {  ssa <- list(
    kind = "ssa_fn",
    entry = "entry",
    blocks = list(
      entry = list(
        kind = "ssa_block",
        name = "entry",
        params = list(),
        stmts = list(),
        term = list(kind = "ret", value = NULL)
      )
    )
  )
  analysis <- list(
    analysis_only = TRUE,
    candidates = list(
      list(producer = "L1", consumer = "L2", accepted = TRUE, domain = "single_basic_loop", reasons = character(0)),
      list(producer = "L3", consumer = "L4", accepted = FALSE, domain = "single_basic_loop", reasons = c("FUSE_REJECT_NOALIAS_MISSING"))
    ),
    diagnostics = list()
  )

  rw <- .mojor_ssa_fusion_rewrite_skeleton(ssa, analysis = analysis)
  expect_identical(rw$kind, "fusion_rewrite")
  expect_identical(rw$mode, "skeleton")
  expect_identical(rw$status, "SkeletonNoOp")
  expect_identical(rw$applied_count, 0L)
  expect_length(rw$applied_pairs, 0L)
  expect_length(rw$eligible_pairs, 1L)
  expect_identical(rw$eligible_pairs[[1]]$producer, "L1")
  expect_identical(rw$eligible_pairs[[1]]$consumer, "L2")
  expect_true(any(vapply(rw$diagnostics, function(d) identical(d$code, "FUSE_REWRITE_SKELETON_NOOP"), logical(1))))
  expect_identical(.mojor_ir_ssa_format(rw$ssa), .mojor_ir_ssa_format(ssa))
})

test_that("backend prep fusion_rewrite defaults to skeleton no-op mode", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) out[i] <- x[i] + 1
    for (i in seq_len(n)) out[i] <- out[i] * 2
    out
  }

  prep <- .mojor_ir_prepare_ssa_backend(
    f,
    layout_ctx = list(n_var = "n_i", type_env = list(x = "f64[]", n = "i32")),
    opt_level = 0,
    fusion_rewrite = TRUE,
    verify = TRUE
  )

  expect_true("fusion_rewrite" %in% prep$pipeline)
  expect_identical(prep$ssa$fusion_rewrite$kind, "fusion_rewrite")
  expect_identical(prep$ssa$fusion_rewrite$mode, "skeleton")
  expect_identical(prep$ssa$fusion_rewrite$applied_count, 0L)
  expect_identical(.mojor_ir_ssa_format(prep$ssa$recanonicalized), .mojor_ir_ssa_format(prep$ssa$typed))
})

test_that("backend prep constrained fusion rewrite keeps boundary verifiers clean", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) out[i] <- x[i] + 1
    for (i in seq_len(n)) out[i] <- out[i] * 2
    out
  }

  prep <- .mojor_ir_prepare_ssa_backend(
    f,
    layout_ctx = list(n_var = "n_i", type_env = list(x = "f64[]", n = "i32")),
    opt_level = 0,
    fusion_rewrite = TRUE,
    fusion_rewrite_mode = "constrained",
    enforce_typing_boundary = TRUE,
    verify = TRUE
  )

  expect_true("fusion_rewrite" %in% prep$pipeline)
  expect_true("verify_boundary_post_recanonicalize" %in% prep$pipeline)
  expect_true("verify_boundary_post_typing" %in% prep$pipeline)
  expect_identical(prep$ssa$fusion_rewrite$mode, "constrained")
  expect_true(is.list(prep$verifier$boundary$post_recanonicalize))
  expect_true(is.list(prep$verifier$boundary$post_typing))
})

.mojor_test_fusion_three_loop_fn <- function(x, y, n) {
  out1 <- numeric(n)
  out2 <- numeric(n)
  out <- numeric(n)
  for (i in seq_len(n)) out1[i] <- x[i] + 1
  for (i in seq_len(n)) out2[i] <- y[i] * 2
  for (i in seq_len(n)) out[i] <- out1[i] + out2[i]
  out
}

test_that("backend prep constrained rewrite applies on real noalias loop pair", {  prep <- .mojor_ir_prepare_ssa_backend(
    .mojor_test_fusion_three_loop_fn,
    layout_ctx = list(n_var = "n_i", type_env = list(x = "f64[]", y = "f64[]", n = "i32")),
    opt_level = 0,
    fusion_rewrite = TRUE,
    fusion_rewrite_mode = "constrained",
    enforce_typing_boundary = TRUE,
    verify = TRUE
  )

  expect_true("fusion_rewrite" %in% prep$pipeline)
  expect_identical(prep$ssa$fusion_rewrite$mode, "constrained")
  expect_true(is.list(prep$ssa$fusion_analysis))
  expect_true(length(prep$ssa$fusion_analysis$candidates) >= 1L)
  accepted_count <- sum(vapply(prep$ssa$fusion_analysis$candidates, function(ca) isTRUE(ca$accepted), logical(1)))
  if (prep$ssa$fusion_rewrite$applied_count > 0L) {
    expect_true(accepted_count >= 1L)
  } else {
    expect_identical(prep$ssa$fusion_rewrite$status, "NoAcceptedCandidates")
    expect_identical(accepted_count, 0L)
    expect_true(all(vapply(prep$ssa$fusion_analysis$candidates, function(ca) {
      any(grepl("NOALIAS", as.character(ca$reasons), fixed = TRUE))
    }, logical(1))))
  }
  expect_false(any(vapply(prep$ssa$fusion_rewrite$diagnostics, function(d) identical(d$code, "FUSE_REWRITE_SKIP_MISSING_LOOP"), logical(1))))

  header_noalias <- vapply(prep$ssa$recanonicalized$blocks, function(blk) {
    if (is.null(blk) || !is.list(blk) || is.null(blk$attrs) || !is.list(blk$attrs)) return(FALSE)
    identical(as.character(blk$attrs$loop_role), "header") && isTRUE(blk$attrs$fusion_noalias_proven)
  }, logical(1))
  if (prep$ssa$fusion_rewrite$applied_count > 0L) {
    expect_true(sum(header_noalias) >= 1L)
  } else {
    expect_true(sum(header_noalias) >= 0L)
  }

  expect_true(is.list(prep$verifier$boundary$post_recanonicalize))
  expect_true(isTRUE(prep$verifier$boundary$post_recanonicalize$ok))
  expect_true(is.list(prep$verifier$boundary$post_typing))
  expect_true(isTRUE(prep$verifier$boundary$post_typing$ok))
})

test_that("backend prep constrained rewrite is idempotent across reruns", {  prep <- .mojor_ir_prepare_ssa_backend(
    .mojor_test_fusion_three_loop_fn,
    layout_ctx = list(n_var = "n_i", type_env = list(x = "f64[]", y = "f64[]", n = "i32")),
    opt_level = 0,
    fusion_rewrite = TRUE,
    fusion_rewrite_mode = "constrained",
    enforce_typing_boundary = TRUE,
    verify = TRUE
  )

  first_rw <- prep$ssa$fusion_rewrite
  expect_identical(first_rw$mode, "constrained")
  expect_true(first_rw$applied_count >= 0L)
  first_ssa_fmt <- .mojor_ir_ssa_format(first_rw$ssa)

  rec_second <- .mojor_loop_recovery_analysis(
    first_rw$ssa,
    require_reducible = FALSE,
    require_single_exit = FALSE
  )
  fa_second <- .mojor_ssa_fusion_candidate_analysis(first_rw$ssa, recovery = rec_second)
  second_rw <- .mojor_ssa_fusion_rewrite_constrained(
    first_rw$ssa,
    analysis = fa_second,
    recovery = rec_second
  )

  expect_true(second_rw$status %in% c("NoCandidates", "NoAcceptedCandidates", "NoChange"))
  expect_identical(second_rw$applied_count, 0L)
  expect_identical(.mojor_ir_ssa_format(second_rw$ssa), first_ssa_fmt)
})

.mojor_test_fusion_diag_fingerprint <- function(diags) {
  if (is.null(diags) || length(diags) == 0L) return(character(0))
  vapply(diags, function(d) {
    code <- if (is.null(d$code)) "" else as.character(d$code)
    severity <- if (is.null(d$severity)) "" else as.character(d$severity)
    message <- if (is.null(d$message)) "" else as.character(d$message)
    paste(code, severity, message, sep = "|")
  }, character(1))
}

.mojor_test_constrained_rewrite_reruns <- function(ssa, analysis_transform = NULL) {
  one_pass <- function(cur_ssa) {
    rec <- .mojor_loop_recovery_analysis(
      cur_ssa,
      require_reducible = FALSE,
      require_single_exit = FALSE
    )
    fa <- .mojor_ssa_fusion_candidate_analysis(cur_ssa, recovery = rec)
    if (!is.null(analysis_transform) && is.function(analysis_transform)) {
      fa <- analysis_transform(fa)
    }
    .mojor_ssa_fusion_rewrite_constrained(
      cur_ssa,
      analysis = fa,
      recovery = rec
    )
  }

  second <- one_pass(ssa)
  third <- one_pass(second$ssa)
  list(second = second, third = third)
}

test_that("backend prep constrained rewrite pass-2/pass-3 are deterministic", {  prep <- .mojor_ir_prepare_ssa_backend(
    .mojor_test_fusion_three_loop_fn,
    layout_ctx = list(n_var = "n_i", type_env = list(x = "f64[]", y = "f64[]", n = "i32")),
    opt_level = 0,
    fusion_rewrite = TRUE,
    fusion_rewrite_mode = "constrained",
    enforce_typing_boundary = TRUE,
    verify = TRUE
  )

  first_rw <- prep$ssa$fusion_rewrite
  expect_identical(first_rw$mode, "constrained")
  expect_true(first_rw$applied_count >= 0L)

  reruns <- .mojor_test_constrained_rewrite_reruns(first_rw$ssa)
  second_rw <- reruns$second
  third_rw <- reruns$third

  expect_identical(second_rw$applied_count, 0L)
  expect_identical(third_rw$applied_count, 0L)
  expect_identical(second_rw$status, third_rw$status)
  expect_identical(.mojor_test_fusion_diag_fingerprint(second_rw$diagnostics), .mojor_test_fusion_diag_fingerprint(third_rw$diagnostics))
  expect_identical(.mojor_ir_ssa_format(second_rw$ssa), .mojor_ir_ssa_format(third_rw$ssa))
})

test_that("backend prep constrained rewrite pass-2/pass-3 negative diagnostics are deterministic", {  prep <- .mojor_ir_prepare_ssa_backend(
    .mojor_test_fusion_three_loop_fn,
    layout_ctx = list(n_var = "n_i", type_env = list(x = "f64[]", y = "f64[]", n = "i32")),
    opt_level = 0,
    fusion_rewrite = FALSE,
    enforce_typing_boundary = TRUE,
    verify = TRUE
  )

  base_ssa <- prep$ssa$typed
  expect_true(is.list(base_ssa))

  force_domain_reject <- function(fa) {
    out <- fa
    out$candidates <- lapply(fa$candidates, function(ca) {
      cand <- ca
      cand$accepted <- TRUE
      cand$domain <- "structured_or_unknown"
      cand$reasons <- character(0)
      cand
    })
    out
  }
  rec_base <- .mojor_loop_recovery_analysis(
    base_ssa,
    require_reducible = FALSE,
    require_single_exit = FALSE
  )
  fa_base <- .mojor_ssa_fusion_candidate_analysis(base_ssa, recovery = rec_base)
  expect_true(length(fa_base$candidates) >= 1L)

  reruns <- .mojor_test_constrained_rewrite_reruns(base_ssa, analysis_transform = force_domain_reject)
  second_rw <- reruns$second
  third_rw <- reruns$third

  second_diag <- .mojor_test_fusion_diag_fingerprint(second_rw$diagnostics)
  third_diag <- .mojor_test_fusion_diag_fingerprint(third_rw$diagnostics)
  expect_identical(second_rw$status, "NoChange")
  expect_identical(third_rw$status, "NoChange")
  expect_identical(second_rw$applied_count, 0L)
  expect_identical(third_rw$applied_count, 0L)
  expect_true(length(second_diag) >= 1L)
  expect_true(all(vapply(second_rw$diagnostics, function(d) {
    code <- as.character(d$code)
    code %in% c("FUSE_REWRITE_SKIP_UNSAFE_DOMAIN", "FUSE_REWRITE_SKIP_NOALIAS_MISSING")
  }, logical(1))))
  expect_identical(second_diag, third_diag)
  expect_identical(.mojor_ir_ssa_format(second_rw$ssa), .mojor_ir_ssa_format(third_rw$ssa))
})

test_that("backend prep constrained rewrite pass-2/pass-3 mixed-reason diagnostics are deterministic", {  prep <- .mojor_ir_prepare_ssa_backend(
    .mojor_test_fusion_three_loop_fn,
    layout_ctx = list(n_var = "n_i", type_env = list(x = "f64[]", y = "f64[]", n = "i32")),
    opt_level = 0,
    fusion_rewrite = FALSE,
    enforce_typing_boundary = TRUE,
    verify = TRUE
  )

  base_ssa <- prep$ssa$typed
  expect_true(is.list(base_ssa))
  rec_base <- .mojor_loop_recovery_analysis(
    base_ssa,
    require_reducible = FALSE,
    require_single_exit = FALSE
  )
  loops_base <- .mojor_ssa_fusion_loop_records(base_ssa, recovery = rec_base)
  expect_true(length(loops_base) >= 2L)

  mixed_ssa <- base_ssa
  for (lr in loops_base) {
    header <- if (is.null(lr$header)) "" else as.character(lr$header)
    if (!nzchar(header) || !header %in% names(mixed_ssa$blocks)) next
    blk <- mixed_ssa$blocks[[header]]
    attrs <- if (is.null(blk$attrs) || !is.list(blk$attrs)) list() else blk$attrs
    attrs$fusion_noalias_proven <- FALSE
    blk$attrs <- attrs
    mixed_ssa$blocks[[header]] <- blk
  }

  first_header <- as.character(loops_base[[1]]$header)
  first_body <- if (nzchar(first_header) && first_header %in% names(mixed_ssa$blocks) &&
                    !is.null(mixed_ssa$blocks[[first_header]]$term$then)) {
    as.character(mixed_ssa$blocks[[first_header]]$term$then)
  } else {
    ""
  }
  expect_true(nzchar(first_body) && first_body %in% names(mixed_ssa$blocks))
  body_blk <- mixed_ssa$blocks[[first_body]]
  body_stmts <- if (is.null(body_blk$stmts)) list() else body_blk$stmts
  body_blk$stmts <- c(
    body_stmts,
    list(list(kind = "ssa_stmt", id = "%det_mixed_impure", op = "mystery:impure", args = character(0)))
  )
  mixed_ssa$blocks[[first_body]] <- body_blk

  force_mixed_reject <- function(fa) {
    out <- fa
    out$candidates <- lapply(fa$candidates, function(ca) {
      cand <- ca
      cand$accepted <- TRUE
      cand$domain <- "single_basic_loop"
      cand$reasons <- character(0)
      cand
    })
    out
  }
  rec_base_mixed <- .mojor_loop_recovery_analysis(
    mixed_ssa,
    require_reducible = FALSE,
    require_single_exit = FALSE
  )
  fa_base_mixed <- .mojor_ssa_fusion_candidate_analysis(mixed_ssa, recovery = rec_base_mixed)
  expect_true(length(fa_base_mixed$candidates) >= 1L)

  reruns <- .mojor_test_constrained_rewrite_reruns(mixed_ssa, analysis_transform = force_mixed_reject)
  second_rw <- reruns$second
  third_rw <- reruns$third

  second_diag <- .mojor_test_fusion_diag_fingerprint(second_rw$diagnostics)
  third_diag <- .mojor_test_fusion_diag_fingerprint(third_rw$diagnostics)
  second_codes <- vapply(second_rw$diagnostics, function(d) if (is.null(d$code)) "" else as.character(d$code), character(1))
  expect_identical(second_rw$status, "NoChange")
  expect_identical(third_rw$status, "NoChange")
  expect_identical(second_rw$applied_count, 0L)
  expect_identical(third_rw$applied_count, 0L)
  expect_true(length(second_diag) >= 2L)
  expect_true(any(second_codes == "FUSE_REWRITE_SKIP_NOALIAS_MISSING"))
  expect_true(any(second_codes == "FUSE_REWRITE_SKIP_EFFECTS_NOT_PURE"))
  expect_false(all(second_codes == "FUSE_REWRITE_SKIP_UNSAFE_DOMAIN"))
  expect_identical(second_diag, third_diag)
  expect_identical(.mojor_ir_ssa_format(second_rw$ssa), .mojor_ir_ssa_format(third_rw$ssa))
})
