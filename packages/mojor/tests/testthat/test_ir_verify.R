library(testthat)

# Tests for .mojor_ir_verify() — IR tree structure validation
#
# "Must pass" tests: valid IR nodes return TRUE invisibly.
# "Must fail" tests: invalid IR nodes stop with an error.

# =============================================================================
# Helpers
# =============================================================================

verify_ok <- function(node) {
  expect_true(isTRUE(.mojor_ir_verify(node)))
}

verify_fail <- function(node, pattern = NULL) {
  if (is.null(pattern)) {
    expect_error(.mojor_ir_verify(node))
  } else {
    expect_error(.mojor_ir_verify(node), regexp = pattern)
  }
}

verify_ctx <- function(...) {
  modifyList(
    list(
      loop_vars = character(),
      defined_vars = NULL,
      check_scope = FALSE,
      ir_only = FALSE
    ),
    list(...)
  )
}

has_call_fn <- function(node, fns) {
  if (is.null(node) || !is.list(node)) return(FALSE)
  if (!is.null(node$kind) && identical(node$kind, "call") &&
      !is.null(node$fn) && node$fn %in% fns) {
    return(TRUE)
  }
  for (i in seq_along(node)) {
    if (has_call_fn(node[[i]], fns)) return(TRUE)
  }
  FALSE
}

has_scalar_index <- function(node) {
  if (!is.list(node) || is.null(node$kind)) return(FALSE)
  if (identical(node$kind, "scalar_index")) return(TRUE)
  any(vapply(node, function(child) {
    if (is.list(child)) has_scalar_index(child) else FALSE
  }, logical(1)))
}

# =============================================================================
# Must-pass: expression nodes
# =============================================================================

test_that("verify: core expression/indexing/statement nodes", {  ok_cases <- list(
    .mojor_ir_const("1.0"),
    .mojor_ir_const(TRUE),
    .mojor_ir_var("x"),
    .mojor_ir_unop("-", .mojor_ir_var("x")),
    .mojor_ir_unop("!", .mojor_ir_var("flag")),
    .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_const("1.0")),
    .mojor_ir_binop("%%", .mojor_ir_var("i"), .mojor_ir_var("n")),
    .mojor_ir_binop("<=", .mojor_ir_var("a"), .mojor_ir_var("b")),
    .mojor_ir_cast("f64", .mojor_ir_var("x")),
    .mojor_ir_cast("i32", .mojor_ir_var("n")),
    .mojor_ir_call("length", list(.mojor_ir_var("x"))),
    .mojor_ir_call("sin", list(.mojor_ir_var("theta"))),
    .mojor_ir_gpu_reduce("sum", .mojor_ir_var("x"), dims = NULL, keepdims = FALSE),
    .mojor_ir_gpu_reduce("prod", .mojor_ir_var("x"), dims = NULL, keepdims = FALSE),
    .mojor_ir_assign(
      .mojor_ir_var("out"),
      .mojor_ir_gpu_matmul(.mojor_ir_var("a"), .mojor_ir_var("b"), transpose_a = FALSE, transpose_b = TRUE)
    ),
    .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i"))),
    .mojor_ir_index(.mojor_ir_var("mat"), list(.mojor_ir_var("i"), .mojor_ir_var("j"))),
    .mojor_ir_ifelse(
      .mojor_ir_binop(">", .mojor_ir_var("x"), .mojor_ir_const("0")),
      .mojor_ir_var("x"),
      .mojor_ir_unop("-", .mojor_ir_var("x"))
    ),
    .mojor_ir_scalar_index(.mojor_ir_var("i")),
    .mojor_ir_slice_index(.mojor_ir_const("1"), .mojor_ir_var("n")),
    .mojor_ir_missing_index(),
    .mojor_ir_subscript("mat", list(.mojor_ir_scalar_index(.mojor_ir_var("i")), .mojor_ir_missing_index())),
    .mojor_ir_c(list(.mojor_ir_var("x"), .mojor_ir_var("y"))),
    .mojor_ir_rep(.mojor_ir_var("x"), times = .mojor_ir_const("3")),
    .mojor_ir_rep_len(.mojor_ir_var("x"), .mojor_ir_var("n")),
    .mojor_ir_assign(.mojor_ir_var("out"), .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_const("1.0"))),
    .mojor_ir_assign(.mojor_ir_index(.mojor_ir_var("out"), list(.mojor_ir_var("i"))), .mojor_ir_var("x")),
    .mojor_ir_if(
      .mojor_ir_binop(">", .mojor_ir_var("n"), .mojor_ir_const("0")),
      .mojor_ir_block(list(.mojor_ir_assign(.mojor_ir_var("x"), .mojor_ir_const("1.0"))))
    ),
    .mojor_ir_loop(
      "i",
      .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_var("n")),
      .mojor_ir_block(list(
        .mojor_ir_assign(
          .mojor_ir_index(.mojor_ir_var("out"), list(.mojor_ir_var("i"))),
          .mojor_ir_var("x")
        )
      ))
    ),
    .mojor_ir_loop(
      "i",
      .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_var("n")),
      .mojor_ir_block(list(
        .mojor_ir_loop(
          "j",
          .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_var("m")),
          .mojor_ir_block(list())
        )
      ))
    ),
    .mojor_ir_while(.mojor_ir_binop("<", .mojor_ir_var("i"), .mojor_ir_var("n")), .mojor_ir_block(list())),
    .mojor_ir_return(.mojor_ir_var("out")),
    .mojor_ir_return(),
    .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_var("n")),
    .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_var("n"), .mojor_ir_const("2")),
    .mojor_ir_range_expr(quote(1:n)),
    .mojor_ir_raw(quote(x + y))
  )
  for (node in ok_cases) verify_ok(node)
})

test_that("verify: break and next", {  # Break and next are only valid inside loops
  expect_true(isTRUE(.mojor_ir_verify(.mojor_ir_break(), list(in_loop = TRUE))))
  expect_true(isTRUE(.mojor_ir_verify(.mojor_ir_next(), list(in_loop = TRUE))))

  # Break/next outside loop should error
  expect_error(
    .mojor_ir_verify(.mojor_ir_break(), list(in_loop = FALSE)),
    "break statement outside loop"
  )
  expect_error(
    .mojor_ir_verify(.mojor_ir_next(), list(in_loop = FALSE)),
    "next statement outside loop"
  )
})

test_that("verify: rng_vec supports all shared RNG distributions", {  meta <- .mojor_ir_rng_metadata()
  rng_dists <- .mojor_ir_rng_call_fns()
  expect_true(length(rng_dists) >= 19)
  for (dist in rng_dists) {
    n_param <- as.integer(meta[[dist]]$min_params[[1]])
    params <- if (n_param > 0) replicate(n_param, .mojor_ir_const("1"), simplify = FALSE) else list()
    verify_ok(.mojor_ir_rng_vec(dist, .mojor_ir_const("1"), params))
  }
})

test_that("verify fails: rng_vec rejects unsupported distributions", {  verify_fail(
    .mojor_ir_rng_vec("r_not_supported", .mojor_ir_const("1"), list()),
    "unsupported distribution"
  )
})

test_that("verify fails: rng_vec enforces RNG parameter arity", {  meta <- .mojor_ir_rng_metadata()
  for (dist in names(meta)) {
    min_params <- as.integer(meta[[dist]]$min_params[[1]])
    max_params <- as.integer(meta[[dist]]$max_params[[1]])

    if (min_params > 0L) {
      too_few <- replicate(min_params - 1L, .mojor_ir_const("1"), simplify = FALSE)
      verify_fail(
        .mojor_ir_rng_vec(dist, .mojor_ir_const("1"), too_few),
        "expects .* params"
      )
    }

    too_many <- replicate(max_params + 1L, .mojor_ir_const("1"), simplify = FALSE)
    verify_fail(
      .mojor_ir_rng_vec(dist, .mojor_ir_const("1"), too_many),
      "expects .* params"
    )
  }
})

test_that("verify fails: scalar RNG call arity is checked from metadata", {  verify_ok(.mojor_ir_call("rpois", list(.mojor_ir_const("1"), .mojor_ir_const("2.0"))))
  verify_fail(
    .mojor_ir_call("rpois", list(.mojor_ir_const("1"))),
    "expects .* arguments"
  )
  verify_fail(
    .mojor_ir_call("rpois", list(.mojor_ir_const("1"), .mojor_ir_const("2.0"), .mojor_ir_const("3.0"))),
    "expects .* arguments"
  )
})

test_that("expr build uses shared RNG distribution set", {  # runif/rnorm are lowered to rng_vec
  for (dist in c("runif", "rnorm")) {
    expr <- as.call(list(as.name(dist), as.name("n")))
    node <- .mojor_ir_expr_build(expr)
    expect_false(is.null(node), label = dist)
    expect_identical(node$kind, "rng_vec", label = dist)
    expect_identical(node$dist, dist, label = dist)
  }

  # Remaining RNG calls build as scalar call nodes.
  for (dist in setdiff(.mojor_ir_rng_call_fns(), c("runif", "rnorm"))) {
    expr <- as.call(list(as.name(dist), as.name("n")))
    node <- .mojor_ir_expr_build(expr)
    expect_false(is.null(node), label = dist)
    expect_identical(node$kind, "call", label = dist)
    expect_identical(node$fn, dist, label = dist)
  }
})

test_that("expr build: gpu reductions lower to gpu_reduce IR", {  node <- .mojor_ir_expr_build(quote(gpu_reduce(x, op = "sum", dims = 1L, keepdims = TRUE)))
  expect_false(is.null(node))
  expect_identical(node$kind, "gpu_reduce")
  expect_identical(node$op, "sum")
  expect_identical(node$keepdims, TRUE)
  expect_true(is.list(node$dims))

  node2 <- .mojor_ir_expr_build(quote(gpu_sum(x, dims = 1L)))
  expect_false(is.null(node2))
  expect_identical(node2$kind, "gpu_reduce")
  expect_identical(node2$op, "sum")
  expect_identical(node2$keepdims, FALSE)

  node3 <- .mojor_ir_expr_build(quote(gpu_argmax(x)))
  expect_false(is.null(node3))
  expect_identical(node3$kind, "gpu_reduce")
  expect_identical(node3$op, "argmax")
  expect_identical(node3$keepdims, FALSE)

  node4 <- .mojor_ir_expr_build(quote(gpu_prod(x)))
  expect_false(is.null(node4))
  expect_identical(node4$kind, "gpu_reduce")
  expect_identical(node4$op, "prod")

  node5 <- .mojor_ir_expr_build(quote(gpu_pmin(x)))
  expect_false(is.null(node5))
  expect_identical(node5$kind, "gpu_reduce")
  expect_identical(node5$op, "min")
})

test_that("expr build: CPU reductions share parser helpers", {  sum_node <- .mojor_ir_expr_build(quote(sum(x, na.rm = TRUE)))
  expect_false(is.null(sum_node))
  expect_identical(sum_node$kind, "call")
  expect_identical(sum_node$fn, "sum")
  expect_identical(sum_node$na_rm, TRUE)

  min_node <- .mojor_ir_expr_build(quote(min(x, na.rm = TRUE)))
  expect_false(is.null(min_node))
  expect_identical(min_node$kind, "call")
  expect_identical(min_node$fn, "min")
  expect_identical(min_node$na_rm, TRUE)

  max_node <- .mojor_ir_expr_build(quote(max(x, y, na.rm = TRUE)))
  expect_false(is.null(max_node))
  expect_identical(max_node$kind, "call")
  expect_identical(max_node$fn, "max")
  expect_length(max_node$args, 2L)
})

test_that("lowering: gpu_reduce lowers to core call", {  node <- .mojor_ir_gpu_reduce(
    op = "mean",
    arg = .mojor_ir_var("x"),
    dims = .mojor_ir_var("dims_i"),
    keepdims = TRUE
  )
  lowered <- .mojor_ir_lower(node)
  expect_identical(lowered$kind, "call")
  expect_identical(lowered$fn, "_mojor_gpu_reduce")
  expect_length(lowered$args, 6L)
  expect_identical(lowered$args[[1]]$kind, "var")
  expect_identical(lowered$args[[1]]$name, "x")
  expect_identical(lowered$args[[2]]$kind, "const")
  expect_identical(lowered$args[[2]]$value, "\"mean\"")
  expect_identical(lowered$args[[3]]$kind, "var")
  expect_identical(lowered$args[[3]]$name, "dims_i")
  expect_identical(lowered$args[[4]]$kind, "const")
  expect_identical(lowered$args[[4]]$value, "True")
  expect_identical(lowered$args[[5]]$kind, "const")
  expect_identical(lowered$args[[5]]$value, "False")
  expect_identical(lowered$args[[6]]$kind, "call")
  expect_identical(lowered$args[[6]]$fn, "length")
  expect_identical(lowered$args[[6]]$args[[1]]$kind, "var")
  expect_identical(lowered$args[[6]]$args[[1]]$name, "x")
})

test_that("lowering: unified preview rewrites gpu_reduce without helper calls", {  ctx <- list(
    n_var = "n_i",
    gpu_jit_mode = "unified_preview",
    type_env = list(x = "f64[]")
  )
  cases <- list(
    list(
      node = .mojor_ir_assign(
        .mojor_ir_var("out"),
        .mojor_ir_gpu_reduce("sum", .mojor_ir_var("x"), dims = NULL, keepdims = FALSE)
      ),
      check = function(tail_stmt) {
        expect_identical(tail_stmt$kind, "assign")
        expect_identical(tail_stmt$lhs$name, "out")
        expect_identical(tail_stmt$rhs$kind, "var")
      }
    ),
    list(
      node = .mojor_ir_assign(
        .mojor_ir_var("idx"),
        .mojor_ir_gpu_reduce("argmax", .mojor_ir_var("x"), dims = NULL, keepdims = FALSE)
      ),
      check = function(tail_stmt) {
        expect_identical(tail_stmt$kind, "assign")
        expect_identical(tail_stmt$lhs$name, "idx")
        expect_identical(tail_stmt$rhs$kind, "var")
      }
    ),
    list(
      node = .mojor_ir_assign(
        .mojor_ir_var("out"),
        .mojor_ir_gpu_reduce("prod", .mojor_ir_var("x"), dims = NULL, keepdims = FALSE)
      ),
      check = function(tail_stmt) {
        expect_identical(tail_stmt$kind, "assign")
        expect_identical(tail_stmt$lhs$name, "out")
        expect_identical(tail_stmt$rhs$kind, "var")
      }
    ),
    list(
      node = .mojor_ir_assign(
        .mojor_ir_var("out"),
        .mojor_ir_gpu_reduce("mean", .mojor_ir_var("x"), dims = NULL, keepdims = FALSE)
      ),
      check = function(tail_stmt) {
        expect_identical(tail_stmt$kind, "assign")
        expect_identical(tail_stmt$rhs$kind, "binop")
        expect_identical(tail_stmt$rhs$op, "/")
      }
    ),
    list(
      node = .mojor_ir_assign(
        .mojor_ir_var("out"),
        .mojor_ir_binop(
          "+",
          .mojor_ir_const("1"),
          .mojor_ir_gpu_reduce("sum", .mojor_ir_var("x"), dims = NULL, keepdims = FALSE)
        )
      ),
      check = function(tail_stmt) {
        expect_identical(tail_stmt$kind, "assign")
        expect_identical(tail_stmt$rhs$kind, "binop")
        expect_identical(tail_stmt$rhs$rhs$kind, "var")
      }
    ),
    list(
      node = .mojor_ir_return(
        .mojor_ir_gpu_reduce("sum", .mojor_ir_var("x"), dims = NULL, keepdims = FALSE)
      ),
      check = function(tail_stmt) {
        expect_identical(tail_stmt$kind, "return")
        expect_identical(tail_stmt$value$kind, "var")
      }
    ),
    list(
      node = .mojor_ir_return(
        .mojor_ir_gpu_reduce("mean", .mojor_ir_var("x"), dims = NULL, keepdims = FALSE)
      ),
      check = function(tail_stmt) {
        expect_identical(tail_stmt$kind, "return")
        expect_identical(tail_stmt$value$kind, "binop")
        expect_identical(tail_stmt$value$op, "/")
      }
    )
  )
  for (case in cases) {
    lowered <- .mojor_ir_lower(case$node, ctx = ctx)
    expect_identical(lowered$kind, "block")
    expect_false(has_call_fn(lowered, "_mojor_gpu_reduce"))
    case$check(lowered$stmts[[length(lowered$stmts)]])
  }
})

test_that("lowering: gpu_matmul lowers to core call", {  node <- .mojor_ir_gpu_matmul(
    a = .mojor_ir_var("a"),
    b = .mojor_ir_var("b"),
    transpose_a = TRUE,
    transpose_b = FALSE
  )
  lowered <- .mojor_ir_lower(node)
  expect_identical(lowered$kind, "call")
  expect_identical(lowered$fn, "_mojor_gpu_matmul")
  expect_length(lowered$args, 4L)
  expect_identical(lowered$args[[1]]$kind, "var")
  expect_identical(lowered$args[[1]]$name, "a")
  expect_identical(lowered$args[[2]]$kind, "var")
  expect_identical(lowered$args[[2]]$name, "b")
  expect_identical(lowered$args[[3]]$kind, "const")
  expect_identical(lowered$args[[3]]$value, "True")
  expect_identical(lowered$args[[4]]$kind, "const")
  expect_identical(lowered$args[[4]]$value, "False")
})

test_that("lowering: auto mode rewrites gpu_matmul assign helper-free", {  node <- .mojor_ir_assign(
    .mojor_ir_var("out"),
    .mojor_ir_gpu_matmul(
      a = .mojor_ir_var("a"),
      b = .mojor_ir_var("b"),
      transpose_a = TRUE,
      transpose_b = FALSE
    )
  )
  lowered <- .mojor_ir_lower(
    node,
    ctx = list(
      n_var = "n_i",
      gpu_jit_mode = "auto",
      type_env = list(a = "f64[,]", b = "f64[,]", out = "f64[,]")
    )
  )
  expect_identical(lowered$kind, "block")
  expect_false(has_call_fn(lowered, c("_mojor_gpu_matmul", "_mojor_gpu_matmul_into")))
  tail_stmt <- lowered$stmts[[length(lowered$stmts)]]
  expect_identical(tail_stmt$kind, "if")
  expect_identical(tail_stmt$cond$kind, "binop")
  expect_identical(tail_stmt$cond$op, "&&")
  expect_identical(tail_stmt$then$kind, "block")
  expect_identical(tail_stmt$then$stmts[[1]]$kind, "loop")
})

test_that("lowering: unified preview rewrites gpu_matmul without helper calls", {  helper_fns <- c("_mojor_gpu_matmul", "_mojor_gpu_matmul_into")
  cases <- list(
    list(
      node = .mojor_ir_assign(
        .mojor_ir_var("out"),
        .mojor_ir_gpu_matmul(
          a = .mojor_ir_var("a"),
          b = .mojor_ir_var("b"),
          transpose_a = TRUE,
          transpose_b = FALSE
        )
      ),
      ctx = list(
        n_var = "n_i",
        gpu_jit_mode = "unified_preview",
        type_env = list(a = "f64[,]", b = "f64[,]", out = "f64[,]")
      ),
      check = function(tail_stmt) {
        expect_identical(tail_stmt$kind, "if")
        expect_identical(tail_stmt$cond$kind, "binop")
        expect_identical(tail_stmt$cond$op, "&&")
        expect_identical(tail_stmt$then$kind, "block")
        expect_identical(tail_stmt$then$stmts[[1]]$kind, "loop")
      }
    ),
    list(
      node = .mojor_ir_return(
        .mojor_ir_gpu_matmul(
          a = .mojor_ir_var("a"),
          b = .mojor_ir_var("b"),
          transpose_a = FALSE,
          transpose_b = FALSE
        )
      ),
      ctx = list(
        n_var = "n_i",
        gpu_jit_mode = "unified_preview",
        out_name = "out",
        type_env = list(a = "f64[,]", b = "f64[,]", out = "f64[,]")
      ),
      check = function(tail_stmt) {
        expect_identical(tail_stmt$kind, "return")
        expect_identical(tail_stmt$value$kind, "var")
        expect_identical(tail_stmt$value$name, "out")
      }
    )
  )
  for (case in cases) {
    lowered <- .mojor_ir_lower(case$node, ctx = case$ctx)
    expect_identical(lowered$kind, "block")
    expect_false(has_call_fn(lowered, helper_fns))
    case$check(lowered$stmts[[length(lowered$stmts)]])
  }
})

# =============================================================================
# Must-fail: structural violations
# =============================================================================

test_that("verify fails: structural/type validation rejects invalid nodes", {  fail_cases <- list(
    list(node = NULL, pattern = "NULL node"),
    list(node = "not a node", pattern = "must be a list"),
    list(node = list(value = 1), pattern = "missing valid 'kind'"),
    list(node = list(kind = "banana"), pattern = "unknown node kind"),
    list(node = .mojor_ir_binop("@", .mojor_ir_var("x"), .mojor_ir_var("y")), pattern = "unknown op"),
    list(
      node = .mojor_ir_assign(.mojor_ir_const("1.0"), .mojor_ir_var("x")),
      pattern = "lhs must be var/index/subscript"
    ),
    list(node = list(kind = "rep", x = .mojor_ir_var("x")), pattern = "at least one of times/each/length_out"),
    list(node = list(kind = "c", parts = list()), pattern = "non-empty list"),
    list(node = list(kind = "subscript", var = "mat", indices = list()), pattern = "non-empty list")
  )
  for (case in fail_cases) {
    verify_fail(case$node, case$pattern)
  }
})

test_that("verify fails: gpu_reduce rejects unsupported op", {  verify_fail(
    .mojor_ir_gpu_reduce("median", .mojor_ir_var("x")),
    "unsupported op"
  )
})

test_that("verify: gpu_reduce dims/keepdims are accepted across supported gpu_jit modes", {  node_dims <- .mojor_ir_gpu_reduce(
    "sum",
    .mojor_ir_var("x"),
    dims = .mojor_ir_const("Int(1)")
  )
  node_keep <- .mojor_ir_gpu_reduce("sum", .mojor_ir_var("x"), dims = NULL, keepdims = TRUE)

  expect_no_error(.mojor_ir_verify(node_dims, ctx = list(gpu_jit_mode = "auto")))
  expect_no_error(.mojor_ir_verify(node_dims, ctx = list(gpu_jit_mode = "unified_preview")))
  expect_no_error(.mojor_ir_verify(node_keep, ctx = list(gpu_jit_mode = "auto")))
  expect_no_error(.mojor_ir_verify(node_keep, ctx = list(gpu_jit_mode = "unified_preview")))
})

test_that("verify fails: gpu_matmul expression form is rejected", {  verify_fail(
    .mojor_ir_gpu_matmul(.mojor_ir_var("a"), .mojor_ir_var("b")),
    "expression form (is unsupported|requires gpu_jit_mode='auto' or 'unified_preview')"
  )
})

test_that("verify: gpu_matmul expression/return forms are allowed in supported gpu_jit modes", {  verify_unified_path <- function(node, mode) {
    expect_no_error(.mojor_ir_verify(node, ctx = list(gpu_jit_mode = mode)))
  }

  node <- .mojor_ir_gpu_matmul(.mojor_ir_var("a"), .mojor_ir_var("b"))
  ret <- .mojor_ir_return(.mojor_ir_gpu_matmul(.mojor_ir_var("a"), .mojor_ir_var("b")))
  verify_unified_path(node, "unified_preview")
  verify_unified_path(ret, "unified_preview")
  verify_unified_path(node, "auto")
  verify_unified_path(ret, "auto")
})

test_that("verify fails: gpu_matmul strict mode rejects mixed element types with deterministic diagnostic", {  node <- .mojor_ir_assign(
    .mojor_ir_var("out"),
    .mojor_ir_gpu_matmul(.mojor_ir_var("a"), .mojor_ir_var("b"))
  )

  expect_error(
    .mojor_ir_verify(
      node,
      ctx = list(
        ir_only = TRUE,
        gpu_jit_mode = "unified_preview",
        type_env = list(
          out = "f32[,]",
          a = "f32[,]",
          b = "f64[,]"
        )
      )
    ),
    "a and b must have matching element type in strict mode"
  )
})

test_that("verify: strict mode accepts matmul/crossprod/tcrossprod matrix descriptor nodes", {  strict_ctx <- list(
    ir_only = TRUE,
    type_env = list(
      a = "f64[,]",
      b = "f64[,]"
    )
  )

  expect_no_error(.mojor_ir_verify(.mojor_ir_matmul(.mojor_ir_var("a"), .mojor_ir_var("b")), ctx = strict_ctx))
  expect_no_error(.mojor_ir_verify(.mojor_ir_crossprod(.mojor_ir_var("a")), ctx = strict_ctx))
  expect_no_error(.mojor_ir_verify(.mojor_ir_crossprod(.mojor_ir_var("a"), .mojor_ir_var("b")), ctx = strict_ctx))
  expect_no_error(.mojor_ir_verify(.mojor_ir_tcrossprod(.mojor_ir_var("a")), ctx = strict_ctx))
  expect_no_error(.mojor_ir_verify(.mojor_ir_tcrossprod(.mojor_ir_var("a"), .mojor_ir_var("b")), ctx = strict_ctx))
})

test_that("verify fails: strict mode matrix descriptor nodes enforce numeric 2D matrix operands", {  strict_ctx <- list(
    ir_only = TRUE,
    type_env = list(
      a = "f64[]",
      b = "f64[,]"
    )
  )

  expect_error(
    .mojor_ir_verify(.mojor_ir_matmul(.mojor_ir_var("a"), .mojor_ir_var("b")), ctx = strict_ctx),
    "lhs must have type"
  )
  expect_error(
    .mojor_ir_verify(.mojor_ir_crossprod(.mojor_ir_var("a")), ctx = strict_ctx),
    "lhs must have type"
  )
  expect_error(
    .mojor_ir_verify(.mojor_ir_tcrossprod(.mojor_ir_var("a"), .mojor_ir_var("b")), ctx = strict_ctx),
    "lhs must have type"
  )
})

test_that("verify fails: strict mode matrix descriptor nodes reject mixed element dtypes", {  strict_ctx <- list(
    ir_only = TRUE,
    type_env = list(
      a = "f32[,]",
      b = "f64[,]"
    )
  )

  expect_error(
    .mojor_ir_verify(.mojor_ir_matmul(.mojor_ir_var("a"), .mojor_ir_var("b")), ctx = strict_ctx),
    "matching element type in strict mode"
  )
  expect_error(
    .mojor_ir_verify(.mojor_ir_crossprod(.mojor_ir_var("a"), .mojor_ir_var("b")), ctx = strict_ctx),
    "matching element type in strict mode"
  )
  expect_error(
    .mojor_ir_verify(.mojor_ir_tcrossprod(.mojor_ir_var("a"), .mojor_ir_var("b")), ctx = strict_ctx),
    "matching element type in strict mode"
  )
})

test_that("verify: strict mode accepts cov/cor vector and matrix descriptor nodes", {  vec_ctx <- list(
    ir_only = TRUE,
    type_env = list(
      x = "f64[]",
      y = "f64[]"
    )
  )
  mat_ctx <- list(
    ir_only = TRUE,
    type_env = list(
      x = "f32[,]",
      y = "f32[,]"
    )
  )

  expect_no_error(.mojor_ir_verify(.mojor_ir_cov(.mojor_ir_var("x")), ctx = vec_ctx))
  expect_no_error(.mojor_ir_verify(.mojor_ir_cov(.mojor_ir_var("x"), .mojor_ir_var("y")), ctx = vec_ctx))
  expect_no_error(.mojor_ir_verify(.mojor_ir_cor(.mojor_ir_var("x")), ctx = vec_ctx))
  expect_no_error(.mojor_ir_verify(.mojor_ir_cor(.mojor_ir_var("x"), .mojor_ir_var("y")), ctx = vec_ctx))

  expect_no_error(.mojor_ir_verify(.mojor_ir_cov(.mojor_ir_var("x")), ctx = mat_ctx))
  expect_no_error(.mojor_ir_verify(.mojor_ir_cov(.mojor_ir_var("x"), .mojor_ir_var("y")), ctx = mat_ctx))
  expect_no_error(.mojor_ir_verify(.mojor_ir_cor(.mojor_ir_var("x")), ctx = mat_ctx))
  expect_no_error(.mojor_ir_verify(.mojor_ir_cor(.mojor_ir_var("x"), .mojor_ir_var("y")), ctx = mat_ctx))
})

test_that("verify fails: strict mode cov/cor enforce numeric array rank and element contracts", {  rank_ctx <- list(
    ir_only = TRUE,
    type_env = list(
      x = "f64[]",
      y = "f64[,]"
    )
  )
  dtype_ctx <- list(
    ir_only = TRUE,
    type_env = list(
      x = "f32[]",
      y = "f64[]"
    )
  )
  type_ctx <- list(
    ir_only = TRUE,
    type_env = list(
      x = "lgl[]"
    )
  )

  expect_error(
    .mojor_ir_verify(.mojor_ir_cov(.mojor_ir_var("x"), .mojor_ir_var("y")), ctx = rank_ctx),
    "matching rank in strict mode"
  )
  expect_error(
    .mojor_ir_verify(.mojor_ir_cor(.mojor_ir_var("x"), .mojor_ir_var("y")), ctx = rank_ctx),
    "matching rank in strict mode"
  )
  expect_error(
    .mojor_ir_verify(.mojor_ir_cov(.mojor_ir_var("x"), .mojor_ir_var("y")), ctx = dtype_ctx),
    "matching element type in strict mode"
  )
  expect_error(
    .mojor_ir_verify(.mojor_ir_cor(.mojor_ir_var("x"), .mojor_ir_var("y")), ctx = dtype_ctx),
    "matching element type in strict mode"
  )
  expect_error(
    .mojor_ir_verify(.mojor_ir_cov(.mojor_ir_var("x")), ctx = type_ctx),
    "x must have type"
  )
  expect_error(
    .mojor_ir_verify(.mojor_ir_cor(.mojor_ir_var("x")), ctx = type_ctx),
    "x must have type"
  )
})

test_that("verify: strict mode accepts dim/type metadata descriptor nodes", {  ctx <- list(
    ir_only = TRUE,
    type_env = list(
      x = "f64[,]",
      y = "i32[3d]"
    )
  )

  expect_no_error(.mojor_ir_verify(.mojor_ir_dim(.mojor_ir_var("x")), ctx = ctx))
  expect_no_error(.mojor_ir_verify(.mojor_ir_dim(.mojor_ir_var("y")), ctx = ctx))
  expect_no_error(.mojor_ir_verify(.mojor_ir_type_predicate("is.matrix", .mojor_ir_var("x")), ctx = ctx))
  expect_no_error(.mojor_ir_verify(.mojor_ir_type_query("typeof", .mojor_ir_var("y")), ctx = ctx))
})

test_that("verify fails: strict mode dim/type metadata nodes enforce direct-var and subset contracts", {  dim_ctx <- list(
    ir_only = TRUE,
    type_env = list(
      x = "f64[]"
    )
  )
  meta_ctx <- list(
    ir_only = TRUE,
    type_env = list(
      x = "f64[,]"
    )
  )

  expect_error(
    .mojor_ir_verify(.mojor_ir_dim(.mojor_ir_var("x")), ctx = dim_ctx),
    "array rank >= 2 in strict mode"
  )
  expect_error(
    .mojor_ir_verify(.mojor_ir_dim(.mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_const("1"))), ctx = meta_ctx),
    "x must be a direct variable in strict mode"
  )
  expect_error(
    .mojor_ir_verify(.mojor_ir_type_predicate("is.not_supported", .mojor_ir_var("x")), ctx = meta_ctx),
    "unsupported predicate"
  )
  expect_error(
    .mojor_ir_verify(.mojor_ir_type_query("storage.mode", .mojor_ir_var("x")), ctx = meta_ctx),
    "unsupported query"
  )
  expect_error(
    .mojor_ir_verify(.mojor_ir_type_query("typeof", .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_const("1"))), ctx = meta_ctx),
    "x must be a direct variable in strict mode"
  )
})

test_that("verify: strict mode accepts vexpr with typed scalar body and i32 len", {  ctx <- list(
    ir_only = TRUE,
    type_env = list(
      n = "i32",
      x = "f64"
    )
  )

  expect_no_error(.mojor_ir_verify(.mojor_ir_vexpr(.mojor_ir_var("n"), .mojor_ir_var("x")), ctx = ctx))
  expect_no_error(.mojor_ir_verify(.mojor_ir_vexpr(.mojor_ir_const("4"), .mojor_ir_const("1.0")), ctx = ctx))
})

test_that("verify fails: strict mode vexpr enforces i32 len and scalar body subset", {  len_ctx <- list(
    ir_only = TRUE,
    type_env = list(
      n = "f64",
      x = "f64"
    )
  )
  body_ctx <- list(
    ir_only = TRUE,
    type_env = list(
      n = "i32",
      x = "f64[]"
    )
  )

  expect_error(
    .mojor_ir_verify(.mojor_ir_vexpr(.mojor_ir_var("n"), .mojor_ir_var("x")), ctx = len_ctx),
    "len must have type 'i32'"
  )
  expect_error(
    .mojor_ir_verify(.mojor_ir_vexpr(.mojor_ir_var("n"), .mojor_ir_var("x")), ctx = body_ctx),
    "body must have type"
  )
})

test_that("verify fails: loop var shadows outer loop var", {  verify_fail(
    .mojor_ir_loop(
      "i",
      .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_var("n")),
      .mojor_ir_block(list(
        .mojor_ir_loop(
          "i",   # duplicate!
          .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_var("m")),
          .mojor_ir_block(list())
        )
      ))
    ),
    "shadows outer loop variable"
  )
})

# =============================================================================
# Loop reduce annotation (built-time reduction)
# =============================================================================

test_that("verify: loop with valid reduce annotation", {  # sum reduction: s <- s + x[i]
  loop <- .mojor_ir_loop(
    "i",
    .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_var("n")),
    .mojor_ir_block(list(
      .mojor_ir_assign(
        .mojor_ir_var("s"),
        .mojor_ir_binop("+", .mojor_ir_var("s"),
          .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i"))))
      )
    ))
  )
  # Simulate what builder does
  loop$reduce <- list(
    kind = "sum", acc = "s", op = "+",
    rhs = .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
  )
  verify_ok(loop)
})

test_that("builder reduction annotations match supported patterns", {  cases <- list(
    list(stmt = quote(for (i in 1:n) s <- s + x[i]), kind = "sum", acc = "s"),
    list(stmt = quote(for (i in 1:n) p <- p * x[i]), kind = "product"),
    list(stmt = quote(for (i in 1:n) mn <- min(mn, x[i])), kind = "min"),
    list(stmt = quote(for (i in 1:n) out[i] <- x[i] + 1), kind = NULL)
  )
  for (case in cases) {
    ir <- .mojor_ir_build_stmt(case$stmt)
    expect_true(!is.null(ir))
    if (is.null(case$kind)) {
      expect_true(is.null(ir$reduce))
      next
    }
    expect_identical(ir$kind, "loop")
    expect_true(!is.null(ir$reduce))
    expect_identical(ir$reduce$kind, case$kind)
    if (!is.null(case$acc)) {
      expect_identical(ir$reduce$acc, case$acc)
    }
    verify_ok(ir)
  }
})

test_that("verify fails: malformed loop reduce metadata", {  bad_reduces <- list(
    list(reduce = list(kind = "mean", acc = "s"), pattern = "kind must be one of"),
    list(reduce = list(kind = "sum"), pattern = "acc must be a character")
  )
  for (case in bad_reduces) {
    loop <- .mojor_ir_loop(
      "i",
      .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_var("n")),
      .mojor_ir_block(list())
    )
    loop$reduce <- case$reduce
    verify_fail(loop, case$pattern)
  }
})

# =============================================================================
# Scope tracking (check_scope = TRUE)
# =============================================================================

test_that("verify: check_scope passes when var is defined", {  ctx <- verify_ctx(defined_vars = c("x", "n", "out"), check_scope = TRUE)
  node <- .mojor_ir_loop(
    "i",
    .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_var("n")),
    .mojor_ir_block(list(
      .mojor_ir_assign(
        .mojor_ir_index(.mojor_ir_var("out"), list(.mojor_ir_var("i"))),
        .mojor_ir_var("x")
      )
    ))
  )
  expect_true(isTRUE(.mojor_ir_verify(node, ctx)))
})

test_that("verify: check_scope fails on undefined var", {  ctx <- verify_ctx(defined_vars = c("x"), check_scope = TRUE)
  # "n" is not in defined_vars
  node <- .mojor_ir_var("n")
  expect_error(.mojor_ir_verify(node, ctx), "not in scope")
})

test_that("verify: loop var is in scope inside loop body", {  ctx <- verify_ctx(defined_vars = c("x", "n", "out"), check_scope = TRUE)
  # Loop body uses "i" — should be added by loop handler
  node <- .mojor_ir_loop(
    "i",
    .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_var("n")),
    .mojor_ir_block(list(
      .mojor_ir_assign(
        .mojor_ir_index(.mojor_ir_var("out"), list(.mojor_ir_var("i"))),
        .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
      )
    ))
  )
  expect_true(isTRUE(.mojor_ir_verify(node, ctx)))
})

test_that("verify: block scope accumulates from assigns", {  ctx <- verify_ctx(defined_vars = c("x"), check_scope = TRUE)
  # "acc" is assigned before use
  node <- .mojor_ir_block(list(
    .mojor_ir_assign(.mojor_ir_var("acc"), .mojor_ir_const("0.0")),
    .mojor_ir_assign(
      .mojor_ir_var("acc"),
      .mojor_ir_binop("+", .mojor_ir_var("acc"), .mojor_ir_var("x"))
    )
  ))
  expect_true(isTRUE(.mojor_ir_verify(node, ctx)))
})

test_that("verify: check_scope disabled by default (no error on unknown var)", {  # Without check_scope = TRUE, undefined vars pass silently
  verify_ok(.mojor_ir_var("mystery_var_not_defined"))
})

# =============================================================================
# ir_only mode (raw nodes forbidden)
# =============================================================================

test_that("verify: ir_only enforces raw-node exclusion and preserves typed nodes", {  ctx <- verify_ctx(ir_only = TRUE)
  expect_error(.mojor_ir_verify(.mojor_ir_raw(quote(x + y)), ctx), "raw fallback node not allowed")
  ok_nodes <- list(
    .mojor_ir_const("1.0"),
    .mojor_ir_var("x"),
    .mojor_ir_binop("+", .mojor_ir_var("a"), .mojor_ir_var("b"))
  )
  for (node in ok_nodes) {
    expect_true(isTRUE(.mojor_ir_verify(node, ctx)))
  }
})

test_that("verify: ir_only disabled by default (raw nodes pass)", {  verify_ok(.mojor_ir_raw(quote(anything)))
})

# =============================================================================
# index_base validation
# =============================================================================

test_that("verify: index with valid index_base", {  verify_ok(.mojor_ir_index(
    .mojor_ir_var("x"), list(.mojor_ir_var("i")),
    index_base = "one_based"
  ))
  verify_ok(.mojor_ir_index(
    .mojor_ir_var("x"), list(.mojor_ir_var("i")),
    index_base = "zero_based"
  ))
})

test_that("verify fails: index with invalid index_base", {  node <- .mojor_ir_index(
    .mojor_ir_var("x"), list(.mojor_ir_var("i")),
    index_base = "two_based"
  )
  verify_fail(node, "index_base must be one_based/zero_based")
})

# =============================================================================
# .mojor_ir_normalize() — scalar_index unwrapping
# =============================================================================

test_that("normalize: scalar/subscript index helpers normalize by case", {  nested_inner <- .mojor_ir_binop("+", .mojor_ir_var("i"), .mojor_ir_const("1"))
  cases <- list(
    list(
      node = .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_scalar_index(.mojor_ir_var("i")))),
      check = function(after) {
        expect_equal(after$indices[[1]]$kind, "var")
        expect_equal(after$indices[[1]]$name, "i")
      }
    ),
    list(
      node = .mojor_ir_subscript("mat", list(
        .mojor_ir_scalar_index(.mojor_ir_var("i")),
        .mojor_ir_scalar_index(.mojor_ir_var("j"))
      )),
      check = function(after) {
        expect_equal(after$indices[[1]]$kind, "var")
        expect_equal(after$indices[[1]]$name, "i")
        expect_equal(after$indices[[2]]$kind, "var")
        expect_equal(after$indices[[2]]$name, "j")
      }
    ),
    list(
      node = .mojor_ir_subscript("mat", list(
        .mojor_ir_slice_index(.mojor_ir_const("1"), .mojor_ir_var("n")),
        .mojor_ir_missing_index()
      )),
      check = function(after) {
        expect_equal(after$indices[[1]]$kind, "slice_index")
        expect_equal(after$indices[[2]]$kind, "missing_index")
      }
    ),
    list(
      node = .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_scalar_index(nested_inner))),
      check = function(after) {
        expect_equal(after$indices[[1]]$kind, "binop")
        expect_equal(after$indices[[1]]$op, "+")
      }
    ),
    list(
      node = .mojor_ir_scalar_index(.mojor_ir_var("k")),
      check = function(after) {
        expect_equal(after$kind, "var")
        expect_equal(after$name, "k")
      }
    )
  )
  for (case in cases) {
    case$check(.mojor_ir_normalize(case$node))
  }
})

test_that("normalize: loop body with subscript assignment is unwrapped", {  stmt <- quote(for (i in 1:n) mat[i, j] <- x[i])
  ir <- .mojor_ir_build_stmt(stmt)
  normed <- .mojor_ir_normalize(ir)
  expect_false(has_scalar_index(normed))
})

test_that("normalize: idempotent on already-clean tree", {  ir <- .mojor_ir_loop(
    "i",
    .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_var("n")),
    .mojor_ir_block(list(
      .mojor_ir_assign(
        .mojor_ir_index(.mojor_ir_var("out"), list(.mojor_ir_var("i"))),
        .mojor_ir_var("x")
      )
    ))
  )
  once <- .mojor_ir_normalize(ir)
  twice <- .mojor_ir_normalize(once)
  expect_identical(once, twice)
})

# ---------------------------------------------------------------------------
# Type vocabulary helpers
# ---------------------------------------------------------------------------

test_that("type helpers: array/base/ndim helpers are consistent", {  array_cases <- list(
    list(tag = "f64[]", is_array = TRUE, ndim = 1L),
    list(tag = "f64[1d]", is_array = TRUE, ndim = 1L),  # [1d] alias for []
    list(tag = "f64[,]", is_array = TRUE, ndim = 2L),
    list(tag = "f64[,,]", is_array = TRUE, ndim = 3L),
    list(tag = "f64[2d]", is_array = TRUE, ndim = 2L),
    list(tag = "i32[3d]", is_array = TRUE, ndim = 3L),
    list(tag = "f64", is_array = FALSE, ndim = 1L),
    list(tag = NULL, is_array = FALSE, ndim = 1L)
  )
  for (case in array_cases) {
    expect_identical(.mojor_type_is_array(case$tag), case$is_array)
    expect_identical(.mojor_type_ndim(case$tag), case$ndim)
  }

  base_cases <- list("f64[]", "f64[1d]", "f64[2d]", "f64")
  for (tag in base_cases) {
    expect_equal(.mojor_type_base(tag), "f64")
    expect_equal(.mojor_type_elem(tag), "f64")
  }

  # Test [1d] normalization
  expect_equal(.mojor_type_normalize("f64[1d]"), "f64[]")
  expect_equal(.mojor_type_normalize("i32[1d]"), "i32[]")
  expect_equal(.mojor_type_normalize("f64[]"), "f64[]")  # idempotent
  expect_equal(.mojor_type_normalize("f64[2d]"), "f64[2d]")  # no change for other ranks

  tag_cases <- list(
    list(base = "f64", ndim = 1L, out = "f64[]"),
    list(base = "f64", ndim = 2L, out = "f64[2d]"),
    list(base = "i32", ndim = 3L, out = "i32[3d]"),
    list(base = "f64[]", ndim = 2L, out = "f64[2d]")
  )
  for (case in tag_cases) {
    expect_equal(.mojor_type_tag_ndim(case$base, case$ndim), case$out)
  }
})

# ---------------------------------------------------------------------------
# .mojor_ir_annotate_typed
# ---------------------------------------------------------------------------

test_that("annotate_typed: node typing cases are stable", {  cases <- list(
    list(
      node = .mojor_ir_const("1.0"),
      env = list(),
      check = function(result) expect_equal(result$type, "f64")
    ),
    list(
      node = .mojor_ir_var("x"),
      env = list(x = "f64[]"),
      check = function(result) expect_equal(result$type, "f64[]")
    ),
    list(
      node = .mojor_ir_binop("+", .mojor_ir_var("a"), .mojor_ir_var("b")),
      env = list(a = "f64", b = "f64"),
      check = function(result) {
        expect_equal(result$lhs$type, "f64")
        expect_equal(result$rhs$type, "f64")
        expect_equal(result$type, "f64")
      }
    ),
    list(
      node = .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i"))),
      env = list(x = "f64[]", i = "i32"),
      check = function(result) {
        expect_equal(result$type, "f64")
        expect_null(result$layout)
      }
    ),
    list(
      node = .mojor_ir_index(
        .mojor_ir_var("mat"),
        list(.mojor_ir_var("i"), .mojor_ir_var("j"))
      ),
      env = list(mat = "f64[2d]", i = "i32", j = "i32"),
      check = function(result) {
        expect_equal(result$type, "f64")
        expect_equal(result$layout$kind, "col_major")
        expect_equal(result$layout$ndim, 2L)
      }
    ),
    list(
      node = .mojor_ir_loop(
        "i",
        .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_var("n")),
        .mojor_ir_block(list(
          .mojor_ir_assign(
            .mojor_ir_index(.mojor_ir_var("out"), list(.mojor_ir_var("i"))),
            .mojor_ir_binop("*", .mojor_ir_var("x"), .mojor_ir_const("2.0"))
          )
        ))
      ),
      env = list(x = "f64", out = "f64[]", n = "i32"),
      check = function(result) {
        rhs <- result$body$stmts[[1]]$rhs
        expect_equal(rhs$type, "f64")
        expect_equal(rhs$rhs$type, "f64")
      }
    )
  )
  for (case in cases) {
    case$check(.mojor_ir_annotate_typed(case$node, case$env))
  }
})

test_that("annotate_typed: infer_type uses cached $type", {  node <- .mojor_ir_const("42.0")
  node$type <- "i32"  # pre-set cache
  result <- .mojor_ir_infer_type(node, list())
  expect_equal(result, "i32")  # returns cache, not re-inferred "f64"
})
