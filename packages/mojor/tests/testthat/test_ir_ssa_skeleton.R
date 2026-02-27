library(testthat)

ssa_golden_path <- function(name) {
  .mojor_test_golden_path(name)
}

read_ssa_golden <- function(name) {
  .mojor_test_read_golden(name)
}

find_ssa_effect <- function(ssa, op) {
  for (blk_name in names(ssa$blocks)) {
    blk <- ssa$blocks[[blk_name]]
    if (is.null(blk$stmts) || length(blk$stmts) == 0) next
    for (st in blk$stmts) {
      if (is.null(st$kind) || st$kind != "ssa_effect") next
      if (identical(st$op, op)) return(st)
    }
  }
  NULL
}

ssa_stmt <- function(id, op, args = character(0), ...) {
  c(list(kind = "ssa_stmt", id = id, op = op, args = args), list(...))
}

ssa_effect <- function(op, args = character(0), ...) {
  c(list(kind = "ssa_effect", op = op, args = args), list(...))
}

ssa_ret <- function(value = NULL) {
  list(kind = "ret", value = value)
}

ssa_br <- function(target, args = character(0)) {
  list(kind = "br", target = target, args = args)
}

ssa_condbr <- function(cond, then, else_target, then_args = character(0), else_args = character(0)) {
  list(
    kind = "condbr",
    cond = cond,
    then = then,
    "else" = else_target,
    then_args = then_args,
    else_args = else_args
  )
}

ssa_block <- function(name, stmts = list(), term = ssa_ret(NULL), params = list()) {
  list(
    kind = "ssa_block",
    name = name,
    params = params,
    stmts = stmts,
    term = term
  )
}

ssa_fn <- function(entry, blocks, cfg_version = NULL) {
  out <- list(kind = "ssa_fn", entry = entry, blocks = blocks)
  if (!is.null(cfg_version)) out$cfg_version <- as.integer(cfg_version)
  out
}

ssa_stmt_ids <- function(ssa, block = "entry") {
  vapply(ssa$blocks[[block]]$stmts, function(st) st$id, character(1))
}

ssa_one_block_fn <- function(stmts = list(), term = ssa_ret(NULL), entry = "entry") {
  ssa_fn(entry, stats::setNames(list(ssa_block(entry, stmts = stmts, term = term)), entry))
}

ssa_optimize_checked <- function(ssa, passes = NULL) {
  expect_silent(.mojor_ir_verify_ssa(ssa))
  opt <- if (is.null(passes)) {
    .mojor_ir_ssa_optimize(ssa)
  } else {
    .mojor_ir_ssa_optimize(ssa, passes = passes)
  }
  expect_silent(.mojor_ir_verify_ssa(opt))
  opt
}

ssa_from_ir <- function(ir) {
  ssa <- .mojor_ir_to_ssa(ir)
  expect_silent(.mojor_ir_verify_ssa(ssa))
  ssa
}

ssa_has_param_var <- function(ssa, var) {
  any(vapply(ssa$blocks, function(blk) {
    if (is.null(blk$params) || length(blk$params) == 0L) return(FALSE)
    any(vapply(blk$params, function(param) identical(param$var, var), logical(1)))
  }, logical(1)))
}

ssa_prepare_backend <- function(ir, type_env, ..., n_var = "n_i") {
  .mojor_ir_prepare_ssa_backend(
    ir,
    layout_ctx = list(n_var = n_var, type_env = type_env),
    opt_level = 0,
    ...
  )
}

ssa_entry_block <- function(selected) {
  selected$blocks[[selected$entry]]
}

ssa_block_ops <- function(block) {
  vapply(block$instructions, function(inst) inst$opcode, character(1))
}

test_that("SSA skeleton lowers straight-line IR", {  ir <- .mojor_ir_block(list(
    .mojor_ir_assign(.mojor_ir_var("a"), .mojor_ir_const("1")),
    .mojor_ir_assign(
      .mojor_ir_var("b"),
      .mojor_ir_binop("+", .mojor_ir_var("a"), .mojor_ir_const("2"))
    ),
    .mojor_ir_return(.mojor_ir_var("b"))
  ))

  ssa <- ssa_from_ir(ir)
  expect_equal(ssa$kind, "ssa_fn")
  expect_true(ssa$entry %in% names(ssa$blocks))
})

test_that("SSA skeleton models block params for carried values", {  cases <- list(
    list(
      ir = .mojor_ir_block(list(
        .mojor_ir_if(
          cond = .mojor_ir_binop(">", .mojor_ir_var("x"), .mojor_ir_const("0")),
          then = .mojor_ir_block(list(
            .mojor_ir_assign(.mojor_ir_var("acc"), .mojor_ir_const("1"))
          )),
          else_block = .mojor_ir_block(list(
            .mojor_ir_assign(.mojor_ir_var("acc"), .mojor_ir_const("2"))
          ))
        ),
        .mojor_ir_return(.mojor_ir_var("acc"))
      )),
      param = "acc"
    ),
    list(
      ir = .mojor_ir_block(list(
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
      )),
      param = "acc"
    )
  )
  for (case in cases) {
    ssa <- ssa_from_ir(case$ir)
    expect_true(ssa_has_param_var(ssa, case$param))
  }
})

test_that("SSA skeleton lowers loop forms to explicit header blocks", {  cases <- list(
    list(
      ir = .mojor_ir_block(list(
        .mojor_ir_assign(.mojor_ir_var("i"), .mojor_ir_const("1")),
        .mojor_ir_assign(.mojor_ir_var("acc"), .mojor_ir_const("0")),
        .mojor_ir_while(
          cond = .mojor_ir_binop("<=", .mojor_ir_var("i"), .mojor_ir_var("n")),
          body = .mojor_ir_block(list(
            .mojor_ir_assign(
              .mojor_ir_var("acc"),
              .mojor_ir_binop("+", .mojor_ir_var("acc"), .mojor_ir_var("i"))
            ),
            .mojor_ir_assign(
              .mojor_ir_var("i"),
              .mojor_ir_binop("+", .mojor_ir_var("i"), .mojor_ir_const("1"))
            )
          ))
        ),
        .mojor_ir_return(.mojor_ir_var("acc"))
      )),
      prefix = "^while_header"
    ),
    list(
      ir = .mojor_ir_block(list(
        .mojor_ir_assign(.mojor_ir_var("acc"), .mojor_ir_const("0")),
        .mojor_ir_repeat(
          body = .mojor_ir_block(list(
            .mojor_ir_assign(
              .mojor_ir_var("acc"),
              .mojor_ir_binop("+", .mojor_ir_var("acc"), .mojor_ir_const("1"))
            )
          ))
        ),
        .mojor_ir_return(.mojor_ir_var("acc"))
      )),
      prefix = "^repeat_header"
    )
  )
  for (case in cases) {
    ssa <- ssa_from_ir(case$ir)
    expect_true(any(grepl(case$prefix, names(ssa$blocks))))
  }
})

test_that("SSA skeleton lowers indexed LHS assignment to store_index", {  ir <- .mojor_ir_block(list(
    .mojor_ir_assign(
      .mojor_ir_index(.mojor_ir_var("out"), list(.mojor_ir_var("i"))),
      .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
    ),
    .mojor_ir_return(.mojor_ir_const("0"))
  ))

  ssa <- ssa_from_ir(ir)

  st <- find_ssa_effect(ssa, "store_index")
  expect_false(is.null(st))
  expect_identical(st$index_kinds, "scalar")
  expect_true(length(st$args) >= 3)
})

test_that("SSA skeleton lowers subscript LHS helpers to store_subscript", {  ir <- .mojor_ir_block(list(
    .mojor_ir_assign(
      .mojor_ir_subscript("mat", list(
        .mojor_ir_scalar_index(.mojor_ir_var("i")),
        .mojor_ir_missing_index()
      )),
      .mojor_ir_var("v")
    ),
    .mojor_ir_assign(
      .mojor_ir_subscript("out", list(
        .mojor_ir_slice_index(.mojor_ir_const("1"), .mojor_ir_var("n"))
      )),
      .mojor_ir_var("rhs")
    ),
    .mojor_ir_return(.mojor_ir_const("0"))
  ))

  ssa <- ssa_from_ir(ir)

  formatted <- .mojor_ir_ssa_format(ssa)
  expect_match(formatted, "idx_missing\\(")
  expect_match(formatted, "idx_slice\\(")
  expect_match(formatted, "store_subscript\\(")

  st <- find_ssa_effect(ssa, "store_subscript")
  expect_false(is.null(st))
  expect_identical(st$target, "mat")
  expect_identical(st$index_kinds, c("scalar", "missing"))
})

test_that("SSA verifier catches use-before-def value references", {  bad_cases <- list(
    ssa_one_block_fn(
      stmts = list(
        ssa_stmt("%v1", "binop:+", args = c("%v9", "%v9"))
      ),
      term = ssa_ret("%v1"),
      entry = "bb1"
    ),
    ssa_one_block_fn(
      stmts = list(
        ssa_stmt("%v2", "binop:+", args = c("%v1", "%v1")),
        ssa_stmt("%v1", "const")
      ),
      term = ssa_ret("%v2"),
      entry = "bb1"
    )
  )
  for (bad in bad_cases) {
    expect_error(.mojor_ir_verify_ssa(bad), "use-before-def")
  }
})

test_that("SSA verifier allows dominating cross-block value uses", {  good <- ssa_fn(
    entry = "entry",
    blocks = list(
      entry = ssa_block(
        "entry",
        stmts = list(ssa_stmt("%v1", "const")),
        term = ssa_br("bb2")
      ),
      bb2 = ssa_block(
        "bb2",
        stmts = list(ssa_stmt("%v2", "binop:+", args = c("%v1", "%v1"))),
        term = ssa_ret("%v2")
      )
    )
  )
  expect_silent(.mojor_ir_verify_ssa(good))
})

test_that("SSA verifier rejects non-dominating cross-block value uses", {  bad <- ssa_fn(
    entry = "entry",
    blocks = list(
      entry = ssa_block(
        "entry",
        stmts = list(ssa_stmt("%v1", "const")),
        term = ssa_condbr("%v1", then = "then1", else_target = "else1")
      ),
      then1 = ssa_block(
        "then1",
        stmts = list(ssa_stmt("%v2", "const")),
        term = ssa_br("merge1")
      ),
      else1 = ssa_block(
        "else1",
        term = ssa_br("merge1")
      ),
      merge1 = ssa_block(
        "merge1",
        stmts = list(ssa_stmt("%v3", "binop:+", args = c("%v2", "%v1"))),
        term = ssa_ret("%v3")
      )
    )
  )
  expect_error(.mojor_ir_verify_ssa(bad), "does not dominate")
})

test_that("SSA verifier rejects stmt:break/stmt:next effects in CFG IR", {  bad_cases <- list(
    list(op = "stmt:break", err = "forbids 'stmt:break'"),
    list(op = "stmt:next", err = "forbids 'stmt:next'"),
    list(op = "mojor.misc.stmt.break", err = "break/next must be lowered to edges"),
    list(op = "mojor.misc.stmt.next", err = "break/next must be lowered to edges")
  )
  for (case in bad_cases) {
    bad <- ssa_one_block_fn(
      stmts = list(ssa_effect(case$op)),
      term = ssa_ret(NULL),
      entry = "bb1"
    )
    expect_error(.mojor_ir_verify_ssa(bad), case$err)
  }
})

test_that("SSA verifier dominator cache tracks hit/miss and cfg_version invalidation", {  .mojor_ssa_dom_cache_reset()
  cached_ssa <- ssa_fn(
    entry = "entry",
    cfg_version = 1L,
    blocks = list(
      entry = ssa_block(
        "entry",
        stmts = list(ssa_stmt("%v1", "const")),
        term = ssa_br("exit")
      ),
      exit = ssa_block(
        "exit",
        stmts = list(ssa_stmt("%v2", "unop:+", args = c("%v1"))),
        term = ssa_ret("%v2")
      )
    )
  )
  expect_silent(.mojor_ir_verify_ssa(cached_ssa))
  stats <- .mojor_ssa_dom_cache_stats()
  expect_identical(stats$misses, 1L)
  expect_identical(stats$hits, 0L)
  expect_identical(stats$entries, 1L)

  expect_silent(.mojor_ir_verify_ssa(cached_ssa))
  stats <- .mojor_ssa_dom_cache_stats()
  expect_identical(stats$misses, 1L)
  expect_identical(stats$hits, 1L)
  expect_identical(stats$entries, 1L)

  invalidating_ssa <- ssa_fn(
    entry = "entry",
    cfg_version = 1L,
    blocks = list(
      entry = ssa_block(
        "entry",
        stmts = list(ssa_stmt("%v1", "const")),
        term = ssa_ret("%v1")
      )
    )
  )
  expect_silent(.mojor_ir_verify_ssa(invalidating_ssa))
  expect_silent(.mojor_ir_verify_ssa(invalidating_ssa))
  invalidating_ssa_v2 <- invalidating_ssa
  invalidating_ssa_v2$cfg_version <- 2L
  expect_silent(.mojor_ir_verify_ssa(invalidating_ssa_v2))

  stats <- .mojor_ssa_dom_cache_stats()
  expect_identical(stats$misses, 3L)
  expect_identical(stats$hits, 2L)
  expect_identical(stats$entries, 3L)
})

test_that("golden SSA format: simple loop", {  code <- quote({
    acc <- 0
    for (i in 1:n) {
      acc <- acc + x[i]
    }
    acc
  })

  ir <- .mojor_ir_build_block(code)
  ir <- .mojor_ir_normalize(ir)
  ssa <- ssa_from_ir(ir)

  formatted <- .mojor_ir_ssa_format(ssa)
  expect_identical(formatted, read_ssa_golden("ssa_simple_loop.ssa"))
})

test_that("golden SSA format: subscript store helpers", {  ir <- .mojor_ir_block(list(
    .mojor_ir_assign(
      .mojor_ir_subscript("mat", list(
        .mojor_ir_scalar_index(.mojor_ir_var("i")),
        .mojor_ir_missing_index()
      )),
      .mojor_ir_var("v")
    ),
    .mojor_ir_assign(
      .mojor_ir_subscript("out", list(
        .mojor_ir_slice_index(.mojor_ir_const("1"), .mojor_ir_var("n"))
      )),
      .mojor_ir_var("rhs")
    ),
    .mojor_ir_return(.mojor_ir_const("0"))
  ))
  ir <- .mojor_ir_normalize(ir)
  ssa <- ssa_from_ir(ir)

  formatted <- .mojor_ir_ssa_format(ssa)
  expect_identical(formatted, read_ssa_golden("ssa_store_subscript.ssa"))
})

test_that("SSA optimize prune_unreachable removes dead blocks", {  ssa <- ssa_fn(
    entry = "entry",
    blocks = list(
      entry = ssa_block(
        "entry",
        stmts = list(ssa_stmt("%v1", "const")),
        term = ssa_br("live")
      ),
      live = ssa_block(
        "live",
        stmts = list(ssa_stmt("%v2", "binop:+", args = c("%v1", "%v1"))),
        term = ssa_ret("%v2")
      ),
      dead = ssa_block(
        "dead",
        stmts = list(ssa_stmt("%v3", "const")),
        term = ssa_ret("%v3")
      )
    )
  )
  opt <- ssa_optimize_checked(ssa)
  expect_true("entry" %in% names(opt$blocks))
  expect_true("live" %in% names(opt$blocks))
  expect_false("dead" %in% names(opt$blocks))
})

test_that("SSA optimize prune_dead_stmts preserves effectful statements by schema", {  cases <- list(
    list(
      name = "removes unused pure values",
      ssa = ssa_one_block_fn(
        stmts = list(
          ssa_stmt("%v1", "const"),
          ssa_stmt("%v2", "const"),
          ssa_stmt("%v3", "binop:+", args = c("%v1", "%v2")),
          ssa_stmt("%v4", "const")
        ),
        term = ssa_ret("%v3")
      ),
      keep = c("%v1", "%v2", "%v3"),
      drop = c("%v4")
    ),
    list(
      name = "keeps impure call statements",
      ssa = ssa_one_block_fn(
        stmts = list(
          ssa_stmt("%v1", "call:side_effect_fn"),
          ssa_stmt("%v2", "const")
        ),
        term = ssa_ret("%v2")
      ),
      keep = c("%v1", "%v2"),
      drop = character(0)
    ),
    list(
      name = "uses namespaced schema interfaces",
      ssa = ssa_one_block_fn(
        stmts = list(
          ssa_stmt("%v1", "mojor.arith.const"),
          ssa_stmt("%v2", "mojor.arith.add", args = c("%v1", "%v1")),
          ssa_stmt("%v3", "mojor.misc.call.side_effect_fn")
        ),
        term = ssa_ret("%v1")
      ),
      keep = c("%v1", "%v3"),
      drop = c("%v2")
    )
  )

  for (case in cases) {
    opt <- ssa_optimize_checked(case$ssa, passes = "prune_dead_stmts")
    ids <- ssa_stmt_ids(opt)
    for (id in case$keep) expect_true(id %in% ids, info = case$name)
    for (id in case$drop) expect_false(id %in% ids, info = case$name)
  }
})

test_that("SSA optimize copy_propagate alias rewriting is stable", {  cases <- list(
    list(
      name = "forwards trivial local aliases",
      ssa = ssa_one_block_fn(
        stmts = list(
          ssa_stmt("%v1", "const"),
          ssa_stmt("%v2", "unop:+", args = c("%v1")),
          ssa_stmt("%v3", "select", args = c("%v1", "%v2", "%v2"))
        ),
        term = ssa_ret("%v3")
      ),
      check = function(opt) {
        ids <- ssa_stmt_ids(opt)
        expect_true("%v1" %in% ids)
        expect_false("%v2" %in% ids)
        expect_false("%v3" %in% ids)
        expect_identical(opt$blocks$entry$term$value, "%v1")
      }
    ),
    list(
      name = "removes nested identical casts",
      ssa = ssa_one_block_fn(
        stmts = list(
          ssa_stmt("%v1", "const"),
          ssa_stmt("%v2", "cast:i32", args = c("%v1")),
          ssa_stmt("%v3", "cast:i32", args = c("%v2"))
        ),
        term = ssa_ret("%v3")
      ),
      check = function(opt) {
        ids <- ssa_stmt_ids(opt)
        expect_true("%v2" %in% ids)
        expect_false("%v3" %in% ids)
        expect_identical(opt$blocks$entry$term$value, "%v2")
      }
    ),
    list(
      name = "handles namespaced alias ops",
      ssa = ssa_one_block_fn(
        stmts = list(
          ssa_stmt("%v1", "mojor.arith.const"),
          ssa_stmt("%v2", "mojor.arith.pos", args = c("%v1")),
          ssa_stmt("%v3", "mojor.arith.select", args = c("%v1", "%v2", "%v2")),
          ssa_stmt("%v4", "mojor.arith.cast.i32", args = c("%v1")),
          ssa_stmt("%v5", "mojor.arith.cast.i32", args = c("%v4"))
        ),
        term = ssa_ret("%v3")
      ),
      check = function(opt) {
        ids <- ssa_stmt_ids(opt)
        expect_true("%v1" %in% ids)
        expect_false("%v2" %in% ids)
        expect_false("%v3" %in% ids)
        expect_true("%v4" %in% ids)
        expect_false("%v5" %in% ids)
        expect_identical(opt$blocks$entry$term$value, "%v1")
      }
    ),
    list(
      name = "keeps aliases with cross-block uses",
      ssa = ssa_fn(
        entry = "entry",
        blocks = list(
          entry = ssa_block(
            "entry",
            stmts = list(
              ssa_stmt("%v1", "const"),
              ssa_stmt("%v2", "unop:+", args = c("%v1"))
            ),
            term = ssa_br("live")
          ),
          live = ssa_block(
            "live",
            stmts = list(ssa_stmt("%v3", "binop:+", args = c("%v2", "%v2"))),
            term = ssa_ret("%v3")
          )
        )
      ),
      check = function(opt) {
        expect_true("%v2" %in% ssa_stmt_ids(opt, block = "entry"))
      }
    )
  )
  for (case in cases) {
    opt <- ssa_optimize_checked(case$ssa, passes = "copy_propagate")
    case$check(opt)
  }
})

test_that("SSA optimize default pipeline applies unreachable + copy + dead-stmt pruning", {  ssa <- ssa_fn(
    entry = "entry",
    blocks = list(
      entry = ssa_block(
        "entry",
        stmts = list(
          ssa_stmt("%v1", "const"),
          ssa_stmt("%v2", "unop:+", args = c("%v1")),
          ssa_stmt("%v5", "const"),
          ssa_stmt("%v6", "unop:+", args = c("%v1"))
        ),
        term = ssa_br("live")
      ),
      live = ssa_block(
        "live",
        stmts = list(ssa_stmt("%v3", "binop:+", args = c("%v2", "%v2"))),
        term = ssa_ret("%v3")
      ),
      dead = ssa_block(
        "dead",
        stmts = list(ssa_stmt("%v4", "const")),
        term = ssa_ret("%v4")
      )
    )
  )

  opt <- ssa_optimize_checked(ssa)

  expect_false("dead" %in% names(opt$blocks))
  entry_ids <- ssa_stmt_ids(opt, block = "entry")
  expect_true("%v1" %in% entry_ids)
  expect_true("%v2" %in% entry_ids)
  expect_false("%v5" %in% entry_ids)
  expect_false("%v6" %in% entry_ids)
})

test_that("SSA optimize reports unknown pass names", {  ssa <- ssa_one_block_fn()
  expect_error(.mojor_ir_ssa_optimize(ssa, passes = c("does_not_exist")), "unknown pass")
})

test_that("SSA backend prep pipeline order is deterministic", {  ir <- .mojor_ir_build_stmt(quote(x <- y + 1))
  prep <- ssa_prepare_backend(ir, type_env = list(y = "f64"))

  expect_identical(
    prep$pipeline,
    c(
      "normalize", "verify_ir",
      "optimize_ir", "verify_ir",
      "lower_ir", "verify_ir",
      "schedule_ir", "verify_ir",
      "to_ssa", "verify_ssa", "verify_boundary_post_structured_lowering",
      "memory_canonicalize", "verify_boundary_post_memory_canonicalize",
      "annotate_effects_resources", "verify_ssa",
      "loop_recovery", "recanonicalize_loop_cfg", "verify_boundary_post_recanonicalize",
      "fusion_candidate_analysis",
      "typing", "verify_ssa",
      "optimize_ssa", "verify_ssa",
      "backend_cfg"
    )
  )
  expect_identical(prep$backend_cfg$kind, "ssa_backend_cfg")
})

test_that("SSA backend prep applies schedule before SSA conversion", {  ir <- .mojor_ir_scalar_reduce(
    op = "sum",
    acc = "acc",
    arg = "x",
    associative = TRUE,
    commutative = TRUE
  )

  prep <- ssa_prepare_backend(
    ir,
    type_env = list(x = "f64[]"),
    schedule = list(reduction = "tree", n_var = "n_i", type_env = list(x = "f64[]"))
  )

  sched_ir <- prep$ir$scheduled
  if (identical(sched_ir$kind, "block")) {
    expect_true(length(sched_ir$stmts) >= 1)
    sched_ir <- sched_ir$stmts[[1]]
  }
  expect_identical(sched_ir$kind, "scheduled_reduce")
  expect_identical(sched_ir$mode, "tree")
  expect_true(match("schedule_ir", prep$pipeline) < match("to_ssa", prep$pipeline))
})

test_that("SSA backend cfg exposes explicit block successors", {  ir <- .mojor_ir_build_stmt(quote(if (x > 0) { y <- 1 } else { y <- 2 }))
  prep <- ssa_prepare_backend(ir, type_env = list(x = "f64"))

  cfg <- prep$backend_cfg
  entry_blk <- cfg$blocks[[cfg$entry]]
  expect_identical(entry_blk$term$kind, "condbr")
  expect_equal(length(entry_blk$successors), 2L)
  expect_true(all(entry_blk$successors %in% names(cfg$blocks)))
})

test_that("SSA backend cfg strict_schema gate controls schema verification", {  bad <- ssa_fn(
    entry = "entry",
    blocks = list(
      entry = ssa_block(
        "entry",
        stmts = list(ssa_stmt("%v1", "mystery:thing")),
        term = ssa_ret("%v1")
      )
    )
  )

  expect_silent(.mojor_ir_ssa_backend_cfg(bad, verify = TRUE, strict_schema = FALSE))
  expect_error(
    .mojor_ir_ssa_backend_cfg(bad, verify = TRUE, strict_schema = TRUE),
    "OP_SCHEMA_UNKNOWN"
  )
})

test_that("SSA backend prep threads strict_schema and overrides global option", {  ir <- .mojor_ir_return(
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

  old_opt <- options(mojor.ssa_schema_strict = TRUE)
  on.exit(options(old_opt), add = TRUE)

  expect_error(
    ssa_prepare_backend(
      ir,
      type_env = list(x = "f64"),
      strict_schema = NULL,
      verify = TRUE
    ),
    "OP_SCHEMA_UNKNOWN"
  )

  expect_silent(
    ssa_prepare_backend(
      ir,
      type_env = list(x = "f64"),
      strict_schema = FALSE,
      verify = TRUE
    )
  )

  expect_error(
    ssa_prepare_backend(
      ir,
      type_env = list(x = "f64"),
      strict_schema = TRUE,
      verify = TRUE
    ),
    "OP_SCHEMA_UNKNOWN"
  )
})

test_that("SSA backend invariant checker validates stage boundaries", {  ir <- .mojor_ir_build_stmt(quote(x <- y + 1))
  prep <- ssa_prepare_backend(ir, type_env = list(y = "f64"))

  expect_true(isTRUE(.mojor_ir_check_ssa_backend_invariants(prep)))

  bad_order <- prep
  bad_order$pipeline <- c(
    "normalize",
    "to_ssa",
    "memory_canonicalize",
    "annotate_effects_resources",
    "typing",
    "schedule_ir",
    "backend_cfg"
  )
  expect_error(
    .mojor_ir_check_ssa_backend_invariants(bad_order),
    "expected stage order schedule_ir -> to_ssa -> backend_cfg"
  )

  bad_ssa <- prep
  bad_ssa$ssa$raw$kind <- "not_ssa"
  expect_error(
    .mojor_ir_check_ssa_backend_invariants(bad_ssa),
    "to_ssa must produce an ssa_fn"
  )

  bad_cfg <- prep
  bad_cfg$backend_cfg$entry <- "missing_block"
  expect_error(
    .mojor_ir_check_ssa_backend_invariants(bad_cfg),
    "entry must reference an existing block"
  )
})

test_that("SSA backend prep records SSA pass instrumentation", {  ir <- .mojor_ir_build_stmt(quote(x <- y + 1))
  prep <- ssa_prepare_backend(ir, type_env = list(y = "f64"))

  trace <- prep$ssa$pass_trace
  expect_true(is.list(trace))
  expect_identical(
    vapply(trace, function(step) step$pass, character(1)),
    c("prune_unreachable", "copy_propagate", "prune_dead_stmts")
  )

  for (step in trace) {
    expect_true(is.list(step$before))
    expect_true(is.list(step$after))
    expect_true(is.integer(step$before$block_count))
    expect_true(is.integer(step$after$stmt_count))
    expect_true(is.logical(step$changed) && length(step$changed) == 1)
  }
})

test_that("SSA backend lowering consumes prep CFG and is deterministic", {  ir <- .mojor_ir_build_stmt(quote(if (x > 0) { y <- 1 } else { y <- 2 }))
  prep <- ssa_prepare_backend(ir, type_env = list(x = "f64"), backend_lower = TRUE)

  expect_true(match("backend_cfg", prep$pipeline) < match("lower_backend", prep$pipeline))

  lowered <- prep$backend_lowered
  expect_identical(lowered$kind, "ssa_backend_lowered")
  expect_identical(lowered$entry, prep$backend_cfg$entry)
  expect_identical(lowered$block_order, prep$backend_cfg$block_order)
  expect_true(length(lowered$instructions) >= length(lowered$block_order))

  lowered_from_cfg <- .mojor_ir_ssa_backend_lower(prep$backend_cfg)
  expect_identical(lowered_from_cfg$instructions, lowered$instructions)

  fmt <- .mojor_ir_ssa_backend_format(prep)
  expect_match(fmt, "ssa_backend entry=", fixed = TRUE)
  expect_match(fmt, "succ:", fixed = TRUE)
})

test_that("SSA backend selection maps straight-line arithmetic to backend opcodes", {  ir <- .mojor_ir_return(
    .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_const(1))
  )
  prep <- ssa_prepare_backend(ir, type_env = list(x = "f64"), backend_select = TRUE)

  expect_true(match("lower_backend", prep$pipeline) < match("select_backend", prep$pipeline))
  selected <- prep$backend_selected
  expect_identical(selected$kind, "ssa_backend_selected")

  entry_blk <- ssa_entry_block(selected)
  ops <- ssa_block_ops(entry_blk)
  expect_true("mov_const" %in% ops)
  expect_true("add" %in% ops)
  expect_identical(entry_blk$terminator$opcode, "ret")
})

test_that("SSA backend selection preserves conditional branch structure", {  ir <- .mojor_ir_build_stmt(quote(if (x > 0) { y <- 1 } else { y <- 2 }))
  prep <- ssa_prepare_backend(ir, type_env = list(x = "f64"), backend_select = TRUE)

  selected <- prep$backend_selected
  entry_blk <- ssa_entry_block(selected)
  expect_identical(entry_blk$terminator$opcode, "condbr")
  expect_true(length(entry_blk$successors) == 2)

  fmt <- .mojor_ir_ssa_backend_selected_format(selected)
  expect_match(fmt, "ssa_backend_selected entry=", fixed = TRUE)
  expect_match(fmt, "condbr", fixed = TRUE)
})

test_that("SSA backend selection retains loop branch arguments", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) out[i] <- x[i] + 1
    out
  }

  prep <- ssa_prepare_backend(
    f,
    type_env = list(x = "f64[]", n = "i32"),
    backend_select = TRUE
  )

  selected <- prep$backend_selected
  has_br_with_args <- any(vapply(selected$blocks, function(blk) {
    term <- blk$terminator
    identical(term$opcode, "br") && !is.null(term$args) && length(term$args) > 0
  }, logical(1)))
  expect_true(has_br_with_args)
})

test_that("SSA backend selection maps call and typed cast ops", {  ir <- .mojor_ir_return(
    .mojor_ir_cast("Int32", .mojor_ir_call("sin", list(.mojor_ir_var("x"))))
  )
  prep <- ssa_prepare_backend(ir, type_env = list(x = "f64"), backend_select = TRUE)

  entry_blk <- ssa_entry_block(prep$backend_selected)
  ops <- ssa_block_ops(entry_blk)
  expect_true("call" %in% ops)
  expect_true("cast_i32" %in% ops)
  call_inst <- entry_blk$instructions[[which(ops == "call")[1]]]
  cast_inst <- entry_blk$instructions[[which(ops == "cast_i32")[1]]]
  expect_identical(call_inst$callee, "sin")
  expect_identical(cast_inst$cast_to, "i32")
})

test_that("SSA backend selection accepts canonical namespaced ops in lowered SSA", {  lowered <- list(
    kind = "ssa_backend_lowered",
    entry = "entry",
    block_order = c("entry"),
    blocks = list(
      entry = list(
        name = "entry",
        params = character(0),
        param_repr = character(0),
        successors = character(0),
        ssa_stmts = list(
          list(kind = "ssa_stmt", id = "%v1", op = "mojor.arith.const", args = character(0), value = "1"),
          list(kind = "ssa_stmt", id = "%v2", op = "mojor.arith.add", args = c("%v1", "%v1")),
          list(kind = "ssa_stmt", id = "%v3", op = "mojor.misc.call.sin", args = c("%v2")),
          list(kind = "ssa_stmt", id = "%v4", op = "mojor.arith.cast.i32", args = c("%v3")),
          list(kind = "ssa_stmt", id = "%v5", op = "mojor.mem.index", args = c("x", "%v1", "n_i")),
          list(kind = "ssa_effect", op = "mojor.mem.store_index", args = c("out", "%v1", "%v2"), index_kinds = "scalar"),
          list(kind = "ssa_effect", op = "mojor.misc.stmt.raw", args = c("%v2"))
        ),
        ssa_term = list(kind = "ret", value = "%v4")
      )
    )
  )

  selected <- .mojor_ir_ssa_backend_select(lowered)
  entry_blk <- ssa_entry_block(selected)
  ops <- ssa_block_ops(entry_blk)

  expect_true("mov_const" %in% ops)
  expect_true("add" %in% ops)
  expect_true("call" %in% ops)
  expect_true("cast_i32" %in% ops)
  expect_true("load_index" %in% ops)
  expect_true("mem_store_index_scalar" %in% ops)
  expect_true("effect_stmt_raw" %in% ops)

  call_inst <- entry_blk$instructions[[which(ops == "call")[1]]]
  cast_inst <- entry_blk$instructions[[which(ops == "cast_i32")[1]]]
  expect_identical(call_inst$callee, "sin")
  expect_identical(cast_inst$cast_to, "i32")
})

test_that("SSA backend selection maps subscript missing stores to mem_store_subscript_missing", {  lowered <- list(
    kind = "ssa_backend_lowered",
    entry = "entry",
    block_order = c("entry"),
    blocks = list(
      entry = list(
        name = "entry",
        params = character(0),
        param_repr = character(0),
        successors = character(0),
        ssa_stmts = list(
          list(kind = "ssa_stmt", id = "%idx_missing", op = "mojor.misc.idx_missing", args = character(0)),
          list(
            kind = "ssa_effect",
            op = "mojor.mem.store_subscript",
            target = "out",
            args = c("%i", "%idx_missing", "%rhs"),
            index_kinds = c("scalar", "missing")
          )
        ),
        ssa_term = list(kind = "ret", value = "%rhs")
      )
    )
  )

  selected <- .mojor_ir_ssa_backend_select(lowered)
  all_instr <- unlist(
    lapply(selected$blocks, function(blk) blk$instructions),
    recursive = FALSE
  )
  store_instr <- Filter(function(inst) {
    !is.null(inst$opcode) && grepl("^mem_store_(subscript|index)_", inst$opcode)
  }, all_instr)
  expect_true(length(store_instr) > 0)
  expect_true(any(vapply(store_instr, function(inst) {
    "missing" %in% as.character(inst$index_kinds)
  }, logical(1))))
})

test_that("SSA backend selection maps mask assignment to mem_store_index_scalar", {  f <- function(x) {
    out <- numeric(length(x))
    mask <- x > 0
    out[mask] <- x[mask] + 1
    out
  }
  prep <- ssa_prepare_backend(f, type_env = list(x = "f64[]"), backend_select = TRUE)

  entry_blk <- ssa_entry_block(prep$backend_selected)
  ops <- ssa_block_ops(entry_blk)
  expect_true("load_index" %in% ops)
  expect_true(any(c("mem_store_index_scalar", "mem_store_subscript_scalar") %in% ops))
})

test_that("SSA backend selection maps tree sum reduction to reduce opcode bundle", {  ir <- .mojor_ir_scalar_reduce(
    op = "sum",
    acc = "acc",
    arg = "x",
    associative = TRUE,
    commutative = TRUE
  )
  prep <- ssa_prepare_backend(
    ir,
    type_env = list(x = "f64[]"),
    schedule = list(reduction = "tree", n_var = "n_i", type_env = list(x = "f64[]")),
    backend_select = TRUE
  )

  entry_blk <- ssa_entry_block(prep$backend_selected)
  ops <- ssa_block_ops(entry_blk)
  expect_identical(ops, c("reduce_init", "reduce_step", "reduce_merge", "reduce_finalize"))
  expect_true(all(vapply(entry_blk$instructions, function(inst) identical(inst$reduce_mode, "tree"), logical(1))))
  expect_true(all(vapply(entry_blk$instructions, function(inst) identical(inst$reduce_op, "sum"), logical(1))))
})

test_that("SSA backend selection maps simd which.max arg-reduction with tie metadata", {  ir <- .mojor_ir_scalar_reduce(
    op = "which.max",
    acc = "idx",
    arg = "x",
    associative = TRUE,
    commutative = TRUE
  )
  prep <- ssa_prepare_backend(
    ir,
    type_env = list(x = "f64[]"),
    schedule = list(reduction = "simd", n_var = "n_i", type_env = list(x = "f64[]")),
    backend_select = TRUE
  )

  entry_blk <- ssa_entry_block(prep$backend_selected)
  ops <- ssa_block_ops(entry_blk)
  expect_identical(ops, c("reduce_init", "reduce_step", "reduce_merge", "reduce_finalize"))
  reduce_modes <- unique(vapply(entry_blk$instructions, function(inst) as.character(inst$reduce_mode), character(1)))
  expect_identical(length(reduce_modes), 1L)
  expect_true(reduce_modes[[1]] %in% c("simd", "linear"))
  expect_true(all(vapply(entry_blk$instructions, function(inst) identical(inst$reduce_op, "which.max"), logical(1))))
  expect_true(all(vapply(entry_blk$instructions, function(inst) identical(inst$tie_break, "first"), logical(1))))
  expected_merge <- if (identical(reduce_modes[[1]], "simd")) "simd_lane" else "linear_fold"
  expect_identical(entry_blk$instructions[[3]]$merge_kind, expected_merge)
})

test_that("SSA backend codegen lowers tree sum reduction bundle to CPU target opcodes", {  ir <- .mojor_ir_scalar_reduce(
    op = "sum",
    acc = "acc",
    arg = "x",
    associative = TRUE,
    commutative = TRUE
  )
  prep <- ssa_prepare_backend(
    ir,
    type_env = list(x = "f64[]"),
    schedule = list(reduction = "tree", n_var = "n_i", type_env = list(x = "f64[]")),
    backend_codegen = TRUE,
    backend_target = "mojo_cpu"
  )

  expect_true(match("select_backend", prep$pipeline) < match("codegen_backend", prep$pipeline))
  codegen <- prep$backend_codegen
  expect_identical(codegen$kind, "ssa_backend_codegen")
  expect_identical(codegen$target, "mojo_cpu")

  entry_blk <- codegen$blocks[[codegen$entry]]
  ops <- ssa_block_ops(entry_blk)
  expect_identical(ops, c(
    "cpu_reduce_init_tree_sum",
    "cpu_reduce_step_tree_sum",
    "cpu_reduce_merge_tree_sum_tree_pairwise",
    "cpu_reduce_finalize_tree_sum"
  ))

  fmt <- .mojor_ir_ssa_backend_codegen_format(codegen)
  expect_match(fmt, "ssa_backend_codegen target=mojo_cpu entry=", fixed = TRUE)
})

test_that("SSA backend codegen normalizes canonical namespaced selected opcodes", {  selected <- list(
    kind = "ssa_backend_selected",
    entry = "entry",
    block_order = c("entry"),
    blocks = list(
      entry = list(
        name = "entry",
        params = character(0),
        param_repr = character(0),
        successors = character(0),
        instructions = list(
          list(kind = "inst", opcode = "mojor.arith.const", dst = "%v1", args = character(0), value = "1"),
          list(kind = "inst", opcode = "mojor.arith.add", dst = "%v2", args = c("%v1", "%v1")),
          list(kind = "inst", opcode = "mojor.misc.call.sin", dst = "%v3", args = c("%v2")),
          list(kind = "inst", opcode = "mojor.arith.cast.i32", dst = "%v4", args = c("%v3")),
          list(kind = "inst", opcode = "mojor.mem.index", dst = "%v5", args = c("x", "%v1", "n_i")),
          list(kind = "inst", opcode = "mojor.mem.store_index", args = c("out", "%v1", "%v2"), index_kinds = "scalar"),
          list(kind = "inst", opcode = "mojor.misc.stmt.raw", args = c("%v2"))
        ),
        terminator = list(kind = "term", opcode = "ret", value = "%v4")
      )
    )
  )

  codegen <- .mojor_ir_ssa_backend_codegen(selected, target = "mojo_cpu")
  entry_blk <- codegen$blocks[[codegen$entry]]
  ops <- ssa_block_ops(entry_blk)

  expect_true("cpu_mov_const" %in% ops)
  expect_true("cpu_add" %in% ops)
  expect_true("cpu_call" %in% ops)
  expect_true("cpu_cast_i32" %in% ops)
  expect_true("cpu_load_index" %in% ops)
  expect_true("cpu_mem_store_index_scalar" %in% ops)
  expect_true("cpu_effect_stmt_raw" %in% ops)

  call_inst <- entry_blk$instructions[[which(ops == "cpu_call")[1]]]
  cast_inst <- entry_blk$instructions[[which(ops == "cpu_cast_i32")[1]]]
  expect_identical(call_inst$callee, "sin")
  expect_identical(cast_inst$cast_to, "i32")
})

test_that("SSA backend codegen preserves simd arg-reduction tie metadata", {  ir <- .mojor_ir_scalar_reduce(
    op = "which.max",
    acc = "idx",
    arg = "x",
    associative = TRUE,
    commutative = TRUE
  )
  prep <- ssa_prepare_backend(
    ir,
    type_env = list(x = "f64[]"),
    schedule = list(reduction = "simd", n_var = "n_i", type_env = list(x = "f64[]")),
    backend_codegen = TRUE,
    backend_target = "mojo_cpu"
  )

  entry_blk <- prep$backend_codegen$blocks[[prep$backend_codegen$entry]]
  ops <- ssa_block_ops(entry_blk)
  expect_identical(length(ops), 4L)
  expect_match(ops[[1]], "^cpu_reduce_init_(simd|linear)_which_max$")
  mode <- sub("^cpu_reduce_init_([a-z]+)_which_max$", "\\1", ops[[1]])
  expect_identical(ops[[2]], paste0("cpu_reduce_step_", mode, "_which_max"))
  merge_kind <- if (identical(mode, "simd")) "simd_lane" else "linear_fold"
  expect_identical(ops[[3]], paste0("cpu_reduce_merge_", mode, "_which_max_", merge_kind))
  expect_identical(ops[[4]], paste0("cpu_reduce_finalize_", mode, "_which_max"))
  expect_true(all(vapply(entry_blk$instructions, function(inst) identical(inst$tie_break, "first"), logical(1))))
})
