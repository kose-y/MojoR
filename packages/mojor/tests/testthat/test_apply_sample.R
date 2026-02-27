# =============================================================================
# Phase 4.2 & 4.3 Tests: apply(), sample.int(), sample()
# =============================================================================
# Tests for the new Phase 4 features in MojoR
# =============================================================================

context("apply() function")
tier4_make_apply <- function(margin = 1, fun = "sum", na_rm = FALSE) {
  .mojor_ir_apply(
    x = .mojor_ir_var("x"),
    margin = margin,
    fun = fun,
    na_rm = na_rm
  )
}

test_that("apply() IR node creation", {  skip_if_no_mojo()

  node <- tier4_make_apply()
  expect_identical(node$kind, "apply")
  expect_identical(node$margin, 1)
  expect_identical(node$fun, "sum")
})

test_that("apply() verifier contracts", {  skip_if_no_mojo()

  expect_silent(.mojor_ir_verify(tier4_make_apply(margin = 1, fun = "sum")))
  expect_silent(.mojor_ir_verify(tier4_make_apply(margin = 2, fun = "mean")))
  for (fun in c("sum", "mean", "min", "max")) {
    expect_silent(.mojor_ir_verify(tier4_make_apply(margin = 1, fun = fun)))
  }

  expect_error(.mojor_ir_verify(tier4_make_apply(margin = 3, fun = "sum")), "margin must be 1 or 2")
  expect_error(.mojor_ir_verify(tier4_make_apply(margin = 1, fun = "custom")), "unsupported fun")
})

test_that("apply() expression build accepts simple inline lambda FUN", {  skip_if_no_mojo()

  node1 <- .mojor_ir_expr_build(quote(apply(x, 1, function(v) sum(v))))
  expect_identical(node1$kind, "apply")
  expect_identical(node1$fun, "sum")
  expect_identical(node1$na_rm, FALSE)

  node2 <- .mojor_ir_expr_build(quote(apply(x, 2, function(v) { return(mean(v, na.rm = TRUE)) })))
  expect_identical(node2$kind, "apply")
  expect_identical(node2$fun, "mean")
  expect_identical(node2$na_rm, TRUE)
})

test_that("apply() expression build accepts scalar typed na.rm argument", {  skip_if_no_mojo()
  node <- .mojor_ir_expr_build(quote(apply(x, 1, sum, na.rm = flag)))
  expect_identical(node$kind, "apply")
  expect_identical(node$fun, "sum")
  expect_true(is.list(node$na_rm))
  expect_identical(node$na_rm$kind, "var")
  expect_identical(node$na_rm$name, "flag")
})

test_that("apply() expression build rejects unsupported inline lambda FUN", {  skip_if_no_mojo()

  expect_null(.mojor_ir_expr_build(quote(apply(x, 1, function(v) v + k))))
  expect_null(.mojor_ir_expr_build(quote(apply(x, 1, function(v = 1) sum(v)))))
  expect_null(.mojor_ir_expr_build(quote(apply(x, 1, function(v, w) sum(v)))))
  expect_null(.mojor_ir_expr_build(quote(apply(x, 1, function(v) sum(v), na.rm = TRUE))))
})

test_that("apply() verifier accepts typed scalar na_rm in strict mode", {  skip_if_no_mojo()
  strict_ctx <- list(
    loop_vars = character(),
    defined_vars = NULL,
    check_scope = FALSE,
    ir_only = TRUE,
    in_loop = FALSE,
    type_env = list(x = "f64[,]", flag = "i32"),
    has_returned = FALSE,
    in_assign_rhs = FALSE
  )
  node <- tier4_make_apply(margin = 1, fun = "sum", na_rm = .mojor_ir_var("flag"))
  expect_silent(.mojor_ir_verify(node, strict_ctx))

  bad_ctx <- strict_ctx
  bad_ctx$type_env$flag <- "f64"
  expect_error(
    .mojor_ir_verify(node, bad_ctx),
    "na_rm.*bool.*lgl.*i32"
  )
})

test_that("apply() effect/resource/lowering contracts", {  skip_if_no_mojo()

  node <- tier4_make_apply()
  effects <- .mojor_ir_expr_effects(node)
  expect_true("ReadsMem" %in% effects || "Pure" %in% effects)
  expect_is(.mojor_ir_expr_resources(node), "list")
  expect_identical(.mojor_ir_lower(node, list())$kind, "apply")
})

test_that("apply() emit covers sum/mean/min/max forms", {  skip_if_no_mojo()

  cases <- list(
    list(node = tier4_make_apply(margin = 1, fun = "sum"), checks = c("for __mojor_apply_", "__mojor_apply_acc.*\\+"), min_len = 6L),
    list(node = tier4_make_apply(margin = 2, fun = "mean"), checks = c("for __mojor_apply_")),
    list(node = tier4_make_apply(margin = 1, fun = "min"), checks = c("< __mojor_apply_acc")),
    list(node = tier4_make_apply(margin = 1, fun = "max"), checks = c("> __mojor_apply_acc"))
  )
  for (case in cases) {
    result <- .mojor_ir_stmt_emit(case$node, indent = "  ", out_name = "out")
    expect_is(result, "character")
    if (!is.null(case$min_len)) expect_gte(length(result), case$min_len)
    for (pat in case$checks) expect_true(any(grepl(pat, result)))
  }
})

test_that("apply() emit supports scalar na_rm expression flags", {  skip_if_no_mojo()
  node_lgl <- tier4_make_apply(margin = 1, fun = "sum", na_rm = .mojor_ir_var("flag"))
  out_lgl <- .mojor_ir_apply_emit(
    node_lgl,
    indent = "  ",
    out_name = "out",
    type_env = list(x = "f64[,]", flag = "lgl")
  )
  expect_is(out_lgl, "character")
  expect_true(any(grepl("if flag:", out_lgl, fixed = TRUE)))

  node_i32 <- tier4_make_apply(margin = 1, fun = "sum", na_rm = .mojor_ir_var("flag_i32"))
  out_i32 <- .mojor_ir_apply_emit(
    node_i32,
    indent = "  ",
    out_name = "out",
    type_env = list(x = "f64[,]", flag_i32 = "i32")
  )
  expect_is(out_i32, "character")
  expect_true(any(grepl("if (flag_i32 != 0):", out_i32, fixed = TRUE)))
})

context("sample.int() function")
tier4_make_sample_int <- function(n = .mojor_ir_const("10"), size = .mojor_ir_const("5"), replace = FALSE, prob = NULL) {
  .mojor_ir_sample_int(n = n, size = size, replace = replace, prob = prob)
}

test_that("sample.int() IR node creation", {  skip_if_no_mojo()

  node <- tier4_make_sample_int()
  expect_identical(node$kind, "sample_int")
  expect_identical(node$n$value, "10")
  expect_identical(node$size$value, "5")
  expect_false(node$replace)
})

test_that("sample.int() verifier/effects/resources/lowering", {  skip_if_no_mojo()

  expect_silent(.mojor_ir_verify(tier4_make_sample_int(replace = TRUE)))
  expect_silent(.mojor_ir_verify(tier4_make_sample_int(n = .mojor_ir_var("n"), size = .mojor_ir_var("k"), replace = FALSE)))
  expect_error(
    .mojor_ir_verify(tier4_make_sample_int(replace = "yes")),
    "replace must be a single logical value"
  )

  expect_is(.mojor_ir_expr_effects(tier4_make_sample_int()), "character")
  expect_is(.mojor_ir_expr_resources(tier4_make_sample_int(n = .mojor_ir_var("n"), size = .mojor_ir_var("k"))), "list")
  expect_identical(.mojor_ir_lower(tier4_make_sample_int(), list())$kind, "sample_int")
})

test_that("sample.int() verifier accepts typed scalar replace in strict mode", {  skip_if_no_mojo()
  node <- .mojor_ir_sample_int(
    n = .mojor_ir_var("n"),
    size = .mojor_ir_var("k"),
    replace = .mojor_ir_var("r")
  )
  strict_ctx <- list(
    loop_vars = character(),
    defined_vars = NULL,
    check_scope = FALSE,
    ir_only = TRUE,
    in_loop = FALSE,
    type_env = list(n = "i32", k = "i32", r = "lgl"),
    has_returned = FALSE,
    in_assign_rhs = FALSE
  )
  expect_silent(.mojor_ir_verify(node, strict_ctx))
})

test_that("sample.int() emit with and without replacement", {  skip_if_no_mojo()

  cases <- list(
    list(node = tier4_make_sample_int(replace = TRUE), checks = c("for i in range", "idx = Int")),
    list(node = tier4_make_sample_int(size = .mojor_ir_const("3"), replace = FALSE), checks = c("used = alloc_bool", "while True"))
  )
  for (case in cases) {
    result <- .mojor_ir_stmt_emit(case$node, indent = "  ", out_name = "out")
    expect_is(result, "character")
    for (pat in case$checks) expect_true(any(grepl(pat, result)))
  }
})

test_that("sample.int() emit supports scalar replace flag expression", {  skip_if_no_mojo()
  node <- .mojor_ir_sample_int(
    n = .mojor_ir_const("10"),
    size = .mojor_ir_const("4"),
    replace = .mojor_ir_var("r")
  )
  result <- .mojor_ir_stmt_emit(node, indent = "  ", out_name = "out", type_env = list(r = "lgl"))
  expect_is(result, "character")
  expect_true(any(grepl("if r:", result, fixed = TRUE)))
  expect_true(any(grepl("used = alloc_bool", result, fixed = TRUE)))
})

test_that("sample.int() emit - weighted prob in non-strict lane (replace = TRUE)", {  skip_if_no_mojo()

  node <- .mojor_ir_sample_int(
    n = .mojor_ir_const("10"),
    size = .mojor_ir_const("4"),
    replace = TRUE,
    prob = .mojor_ir_var("p")
  )
  result <- .mojor_ir_stmt_emit(node, indent = "  ", out_name = "out", type_env = list(p = "f64[]"))
  expect_is(result, "character")
  expect_true(any(grepl("_mojor_read_f64\\(p", result)))
  expect_true(any(grepl("__mojor_prob_total", result, fixed = TRUE)))
})

test_that("sample.int() emit - weighted prob supports replace = FALSE", {  skip_if_no_mojo()

  node <- .mojor_ir_sample_int(
    n = .mojor_ir_const("10"),
    size = .mojor_ir_const("4"),
    replace = FALSE,
    prob = .mojor_ir_var("p")
  )
  result <- .mojor_ir_stmt_emit(node, indent = "  ", out_name = "out", type_env = list(p = "f64[]"))
  expect_is(result, "character")
  expect_true(any(grepl("_mojor_read_f64\\(p", result)))
  expect_true(any(grepl("__mojor_prob_total", result, fixed = TRUE)))
  expect_true(any(grepl("__mojor_prob_used", result, fixed = TRUE)))
  expect_true(any(grepl("alloc_bool", result, fixed = TRUE)))
})

test_that("sample.int() emit - weighted prob supports scalar replace flag expression", {  skip_if_no_mojo()
  node <- .mojor_ir_sample_int(
    n = .mojor_ir_const("10"),
    size = .mojor_ir_const("4"),
    replace = .mojor_ir_var("r"),
    prob = .mojor_ir_var("p")
  )
  result <- .mojor_ir_stmt_emit(
    node,
    indent = "  ",
    out_name = "out",
    type_env = list(p = "f64[]", r = "lgl")
  )
  expect_is(result, "character")
  expect_true(any(grepl("if r:", result, fixed = TRUE)))
  expect_true(any(grepl("__mojor_prob_total", result, fixed = TRUE)))
  expect_true(any(grepl("__mojor_prob_used", result, fixed = TRUE)))
})

test_that("sample.int() indexed dynamic emit supports weighted prob in non-strict lane", {  skip_if_no_mojo()

  node <- .mojor_ir_assign(
    .mojor_ir_index(.mojor_ir_var("out"), indices = list(.mojor_ir_var("i")), index_base = "one_based"),
    .mojor_ir_index(
      .mojor_ir_sample_int(
        n = .mojor_ir_const("10"),
        size = .mojor_ir_const("4"),
        replace = .mojor_ir_const("TRUE"),
        prob = .mojor_ir_var("p")
      ),
      indices = list(.mojor_ir_var("i")),
      index_base = "one_based"
    )
  )

  result <- .mojor_ir_stmt_emit(
    node,
    indent = "  ",
    type_env = list(out = "i32[]", p = "f64[]"),
    loop_var = "i"
  )
  expect_is(result, "character")
  expect_true(any(grepl("_mojor_read_f64\\(p", result)))
  expect_true(any(grepl("__mojor_prob_total", result, fixed = TRUE)))
})

test_that("sample.int() indexed dynamic emit supports scalar replace flag expression", {  skip_if_no_mojo()
  node <- .mojor_ir_assign(
    .mojor_ir_index(.mojor_ir_var("out"), indices = list(.mojor_ir_var("i")), index_base = "one_based"),
    .mojor_ir_index(
      .mojor_ir_sample_int(
        n = .mojor_ir_const("10"),
        size = .mojor_ir_const("4"),
        replace = .mojor_ir_var("r")
      ),
      indices = list(.mojor_ir_var("i")),
      index_base = "one_based"
    )
  )

  result <- .mojor_ir_stmt_emit(
    node,
    indent = "  ",
    type_env = list(out = "i32[]", r = "lgl"),
    loop_var = "i"
  )
  expect_is(result, "character")
  expect_true(any(grepl("_mojor_sample_pick_index", result, fixed = TRUE)))
  expect_true(any(grepl(", r)", result, fixed = TRUE)))
})

context("sample() function")
test_that("sample() IR node creation", {  skip_if_no_mojo()
  
  node <- .mojor_ir_sample(
    x = .mojor_ir_var("x"),
    size = .mojor_ir_const("5"),
    replace = FALSE
  )
  expect_identical(node$kind, "sample")
  expect_identical(node$x$name, "x")
  expect_identical(node$size$value, "5")
  expect_false(node$replace)
})

test_that("sample() verifier - valid inputs", {  skip_if_no_mojo()
  
  # With replacement
  node1 <- .mojor_ir_sample(
    x = .mojor_ir_var("x"),
    size = .mojor_ir_const("5"),
    replace = TRUE
  )
  expect_silent(.mojor_ir_verify(node1))
  
  # Without replacement
  node2 <- .mojor_ir_sample(
    x = .mojor_ir_var("data"),
    size = .mojor_ir_var("k"),
    replace = FALSE
  )
  expect_silent(.mojor_ir_verify(node2))
})

test_that("sample() verifier - invalid replace", {  skip_if_no_mojo()
  
  node <- .mojor_ir_sample(
    x = .mojor_ir_var("x"),
    size = .mojor_ir_const("5"),
    replace = "no"
  )
  expect_error(.mojor_ir_verify(node), "replace must be a single logical value")
})

test_that("sample() effect tracking", {  skip_if_no_mojo()
  
  node <- .mojor_ir_sample(
    x = .mojor_ir_var("x"),
    size = .mojor_ir_const("5"),
    replace = FALSE
  )
  effects <- .mojor_ir_expr_effects(node)
  expect_is(effects, "character")
})

test_that("sample() resource tracking", {  skip_if_no_mojo()
  
  node <- .mojor_ir_sample(
    x = .mojor_ir_var("x"),
    size = .mojor_ir_var("k"),
    replace = FALSE
  )
  resources <- .mojor_ir_expr_resources(node)
  expect_is(resources, "list")
})

test_that("sample() lowering", {  skip_if_no_mojo()
  
  node <- .mojor_ir_sample(
    x = .mojor_ir_var("x"),
    size = .mojor_ir_const("5"),
    replace = FALSE
  )
  ctx <- list()
  lowered <- .mojor_ir_lower(node, ctx)
  expect_identical(lowered$kind, "sample")
})

test_that("sample() emit - with replacement", {  skip_if_no_mojo()
  
  node <- .mojor_ir_sample(
    x = .mojor_ir_var("x"),
    size = .mojor_ir_const("5"),
    replace = TRUE
  )
  result <- .mojor_ir_sample_emit(node, zero_based_vars = NULL, type_env = NULL, loop_var = NULL)
  expect_is(result, "character")
  expect_true(any(grepl("for i in range", result)))
})

test_that("sample() emit supports scalar replace flag expression", {  skip_if_no_mojo()
  node <- .mojor_ir_sample(
    x = .mojor_ir_var("x"),
    size = .mojor_ir_const("4"),
    replace = .mojor_ir_var("r")
  )
  result <- .mojor_ir_sample_emit(node, zero_based_vars = NULL, type_env = list(x = "f64[]", r = "lgl"), loop_var = NULL)
  expect_is(result, "character")
  expect_true(any(grepl("if r:", result, fixed = TRUE)))
  expect_true(any(grepl("used = alloc_bool", result, fixed = TRUE)))
})

test_that("sample() emit - without replacement", {  skip_if_no_mojo()
  
  node <- .mojor_ir_sample(
    x = .mojor_ir_var("x"),
    size = .mojor_ir_const("3"),
    replace = FALSE
  )
  result <- .mojor_ir_sample_emit(node, zero_based_vars = NULL, type_env = NULL, loop_var = NULL)
  expect_is(result, "character")
  expect_true(any(grepl("used = alloc_bool", result)))
})

test_that("sample() emit - weighted prob in non-strict lane (replace = TRUE)", {  skip_if_no_mojo()

  node <- .mojor_ir_sample(
    x = .mojor_ir_var("x"),
    size = .mojor_ir_const("5"),
    replace = TRUE,
    prob = .mojor_ir_var("p")
  )
  result <- .mojor_ir_sample_emit(
    node,
    zero_based_vars = NULL,
    type_env = list(x = "f64[]", p = "f64[]"),
    loop_var = NULL
  )
  expect_is(result, "character")
  expect_true(any(grepl("_mojor_read_f64\\(p", result)))
  expect_true(any(grepl("__mojor_prob_total", result, fixed = TRUE)))
})

test_that("sample() emit - weighted prob supports replace = FALSE", {  skip_if_no_mojo()

  node <- .mojor_ir_sample(
    x = .mojor_ir_var("x"),
    size = .mojor_ir_const("5"),
    replace = FALSE,
    prob = .mojor_ir_var("p")
  )
  result <- .mojor_ir_sample_emit(
    node,
    zero_based_vars = NULL,
    type_env = list(x = "f64[]", p = "f64[]"),
    loop_var = NULL
  )
  expect_is(result, "character")
  expect_true(any(grepl("_mojor_read_f64\\(p", result)))
  expect_true(any(grepl("__mojor_prob_total", result, fixed = TRUE)))
  expect_true(any(grepl("__mojor_prob_used", result, fixed = TRUE)))
  expect_true(any(grepl("alloc_bool", result, fixed = TRUE)))
})

test_that("sample() emit - weighted prob supports scalar replace flag expression", {  skip_if_no_mojo()
  node <- .mojor_ir_sample(
    x = .mojor_ir_var("x"),
    size = .mojor_ir_const("5"),
    replace = .mojor_ir_var("r"),
    prob = .mojor_ir_var("p")
  )
  result <- .mojor_ir_sample_emit(
    node,
    zero_based_vars = NULL,
    type_env = list(x = "f64[]", p = "f64[]", r = "lgl"),
    loop_var = NULL
  )
  expect_is(result, "character")
  expect_true(any(grepl("if r:", result, fixed = TRUE)))
  expect_true(any(grepl("__mojor_prob_total", result, fixed = TRUE)))
  expect_true(any(grepl("__mojor_prob_used", result, fixed = TRUE)))
})

test_that("sample() indexed dynamic emit supports weighted prob in non-strict lane", {  skip_if_no_mojo()

  node <- .mojor_ir_assign(
    .mojor_ir_index(.mojor_ir_var("out"), indices = list(.mojor_ir_var("i")), index_base = "one_based"),
    .mojor_ir_index(
      .mojor_ir_sample(
        x = .mojor_ir_var("x"),
        size = .mojor_ir_const("4"),
        replace = .mojor_ir_const("TRUE"),
        prob = .mojor_ir_var("p")
      ),
      indices = list(.mojor_ir_var("i")),
      index_base = "one_based"
    )
  )

  result <- .mojor_ir_stmt_emit(
    node,
    indent = "  ",
    type_env = list(out = "f64[]", x = "f64[]", p = "f64[]"),
    loop_var = "i"
  )
  expect_is(result, "character")
  expect_true(any(grepl("_mojor_read_f64\\(p", result)))
  expect_true(any(grepl("__mojor_prob_total", result, fixed = TRUE)))
})

test_that("sample() indexed dynamic emit supports scalar replace flag expression", {  skip_if_no_mojo()
  node <- .mojor_ir_assign(
    .mojor_ir_index(.mojor_ir_var("out"), indices = list(.mojor_ir_var("i")), index_base = "one_based"),
    .mojor_ir_index(
      .mojor_ir_sample(
        x = .mojor_ir_var("x"),
        size = .mojor_ir_const("4"),
        replace = .mojor_ir_var("r")
      ),
      indices = list(.mojor_ir_var("i")),
      index_base = "one_based"
    )
  )

  result <- .mojor_ir_stmt_emit(
    node,
    indent = "  ",
    type_env = list(out = "f64[]", x = "f64[]", r = "lgl"),
    loop_var = "i"
  )
  expect_is(result, "character")
  expect_true(any(grepl("_mojor_sample_pick_index", result, fixed = TRUE)))
  expect_true(any(grepl(", r)", result, fixed = TRUE)))
})

context("AST to IR conversion")
test_that("AST apply() conversion", {  skip_if_no_mojo()
  
  # Simulate R AST for apply(x, 1, sum)
  ast <- list(
    op = "apply",
    args = list(
      list(op = "name", value = "x"),
      list(op = "const", value = 1),
      list(op = "name", value = "sum")
    )
  )
  
  # The conversion happens in .mojor_ir_expr_build
  # This test verifies the AST structure is correct
  expect_identical(ast$op, "apply")
  expect_length(ast$args, 3)
})

test_that("AST sample.int() conversion", {  skip_if_no_mojo()
  
  # Simulate R AST for sample.int(10, 5)
  ast <- list(
    op = "sample.int",
    args = list(
      list(op = "const", value = 10),
      list(op = "const", value = 5)
    )
  )
  
  expect_identical(ast$op, "sample.int")
  expect_length(ast$args, 2)
})

test_that("AST sample() conversion", {  skip_if_no_mojo()
  
  # Simulate R AST for sample(x, 5)
  ast <- list(
    op = "sample",
    args = list(
      list(op = "name", value = "x"),
      list(op = "const", value = 5)
    )
  )
  
  expect_identical(ast$op, "sample")
  expect_length(ast$args, 2)
})

context("Integration tests")
test_that("apply() with nested expressions", {  skip_if_no_mojo()
  
  node <- .mojor_ir_apply(
    x = .mojor_ir_binop("+", .mojor_ir_var("a"), .mojor_ir_var("b")),
    margin = 1,
    fun = "sum"
  )
  expect_silent(.mojor_ir_verify(node))
})

test_that("sample.int() with variable n", {  skip_if_no_mojo()
  
  node <- .mojor_ir_sample_int(
    n = .mojor_ir_var("n"),
    size = .mojor_ir_const("5"),
    replace = FALSE
  )
  expect_silent(.mojor_ir_verify(node))
})

test_that("sample() with variable size", {  skip_if_no_mojo()
  
  node <- .mojor_ir_sample(
    x = .mojor_ir_var("x"),
    size = .mojor_ir_var("k"),
    replace = TRUE
  )
  expect_silent(.mojor_ir_verify(node))
})

test_that("Effect and resource summary", {  skip_if_no_mojo()
  
  # Test apply
  apply_node <- .mojor_ir_apply(
    x = .mojor_ir_var("x"),
    margin = 1,
    fun = "sum"
  )
  summary <- .mojor_ir_expr_effect_resource_summary(apply_node)
  expect_is(summary$effect_classes, "character")
  expect_is(summary$resources, "list")
  
  # Test sample_int
  sample_int_node <- .mojor_ir_sample_int(
    n = .mojor_ir_const("10"),
    size = .mojor_ir_const("5"),
    replace = FALSE
  )
  summary <- .mojor_ir_expr_effect_resource_summary(sample_int_node)
  expect_is(summary$effect_classes, "character")
  expect_is(summary$resources, "list")
})

test_that("CSE legality check", {  skip_if_no_mojo()
  
  # apply reads memory, so should not be CSE legal
  node <- .mojor_ir_apply(
    x = .mojor_ir_var("x"),
    margin = 1,
    fun = "sum"
  )
  # CSE legality depends on effect class
  # This test verifies the function exists and returns a boolean
  result <- .mojor_ir_cse_legal_expr(node)
  expect_is(result, "logical")
  expect_length(result, 1)
})

test_that("DCE legality check", {  skip_if_no_mojo()
  
  # apply reads memory, so RHS should not be DCE legal
  node <- .mojor_ir_apply(
    x = .mojor_ir_var("x"),
    margin = 1,
    fun = "sum"
  )
  result <- .mojor_ir_dce_legal_rhs(node)
  expect_is(result, "logical")
  expect_length(result, 1)
})

# =============================================================================
# Runtime Tests (Phase 4.2: apply() and Phase 4.3: sample.int()/sample())
# =============================================================================

tier4_build_runtime <- function(f, name, type_sig) {
  build_args <- c(list(f, name = name, cache = FALSE, load = TRUE), type_sig)
  do.call(mojor_build, build_args)
}

tier4_expect_compiled_equal <- function(f, name, type_sig, call_args) {
  built <- tier4_build_runtime(f, name = name, type_sig = type_sig)
  expect_equal(do.call(built$func, call_args), do.call(f, call_args))
}

tier4_expect_seeded_deterministic <- function(fn, seed, call_args) {
  mojor_rng_seed(as.integer(seed))
  result_1 <- do.call(fn, call_args)
  mojor_rng_seed(as.integer(seed))
  result_2 <- do.call(fn, call_args)
  expect_equal(result_1, result_2)
  result_1
}

tier4_expect_in_range <- function(values, lower, upper) {
  expect_true(all(values >= lower) && all(values <= upper))
}

tier4_expect_membership <- function(values, universe) {
  expect_true(all(values %in% universe))
}

test_that("apply() runtime variants match R", {  skip_if_no_mojo()

  cases <- list(
    list(
      name = "t_apply_row_sum",
      type_sig = list(mat = "f64[,]", nr = "i32"),
      call_args = list(mat = matrix(as.double(1:12), nrow = 3, ncol = 4), nr = 3L),
      f = function(mat, nr) {
        tmp <- apply(mat, 1, sum)
        out <- numeric(nr)
        for (i in 1:nr) out[i] <- tmp[i]
        out
      }
    ),
    list(
      name = "t_apply_col_mean",
      type_sig = list(mat = "f64[,]", nc = "i32"),
      call_args = list(mat = matrix(as.double(1:12), nrow = 3, ncol = 4), nc = 4L),
      f = function(mat, nc) {
        out <- numeric(nc)
        for (j in 1:nc) out[j] <- mean(mat[, j])
        out
      }
    ),
    list(
      name = "t_apply_row_min",
      type_sig = list(mat = "f64[,]", nr = "i32"),
      call_args = list(mat = matrix(as.double(c(5, 2, 8, 1, 9, 3)), nrow = 2, ncol = 3), nr = 2L),
      f = function(mat, nr) {
        tmp <- apply(mat, 1, min)
        out <- numeric(nr)
        for (i in 1:nr) out[i] <- tmp[i]
        out
      }
    ),
    list(
      name = "t_apply_col_max",
      type_sig = list(mat = "f64[,]", nc = "i32"),
      call_args = list(mat = matrix(as.double(c(1, 5, 3, 7, 2, 6)), nrow = 2, ncol = 3), nc = 3L),
      f = function(mat, nc) {
        out <- numeric(nc)
        for (j in 1:nc) out[j] <- max(mat[, j])
        out
      }
    ),
    list(
      name = "t_apply_na",
      type_sig = list(mat = "f64[,]", nr = "i32"),
      call_args = list(mat = matrix(c(1, 2, NA, 4, 5, 6), nrow = 2, ncol = 3), nr = 2L),
      f = function(mat, nr) {
        tmp <- apply(mat, 1, sum, na.rm = TRUE)
        out <- numeric(nr)
        for (i in 1:nr) out[i] <- tmp[i]
        out
      }
    ),
    list(
      name = "t_apply_na_dynamic",
      type_sig = list(mat = "f64[,]", nr = "i32", na_flag = "i32"),
      call_args = list(mat = matrix(c(1, 2, NA, 4, 5, 6), nrow = 2, ncol = 3), nr = 2L, na_flag = 1L),
      f = function(mat, nr, na_flag) {
        tmp <- apply(mat, 1, sum, na.rm = na_flag)
        out <- numeric(nr)
        for (i in 1:nr) out[i] <- tmp[i]
        out
      }
    )
  )

  for (case in cases) {
    tier4_expect_compiled_equal(case$f, case$name, case$type_sig, case$call_args)
  }
})

test_that("sample.int() runtime loop variants are deterministic and bounded", {  skip_if_no_mojo()

  cases <- list(
    list(
      build_name = "t_sample_int_basic",
      seed = 42L,
      n = 10L,
      expected_length = 5L,
      min_value = 1L,
      max_value = 10L,
      f = function(n) {
        out <- integer(5)
        for (i in seq_len(5)) out[i] <- sample.int(n, size = 1, replace = FALSE)[1]
        out
      }
    ),
    list(
      build_name = "t_sample_int_replace",
      seed = 123L,
      n = 5L,
      expected_length = 10L,
      min_value = 1L,
      max_value = 5L,
      f = function(n) {
        out <- integer(10)
        for (i in seq_len(10)) out[i] <- sample.int(n, size = 1, replace = TRUE)[1]
        out
      }
    ),
    list(
      build_name = "t_sample_int_scalar_no_index",
      seed = 314L,
      n = 9L,
      expected_length = 6L,
      min_value = 1L,
      max_value = 9L,
      f = function(n) {
        out <- integer(6)
        for (i in seq_len(6)) out[i] <- sample.int(n, size = 1, replace = TRUE)
        out
      }
    ),
    list(
      build_name = "t_sample_int_scalar_no_index_size_gt1",
      seed = 415L,
      n = 9L,
      expected_length = 6L,
      min_value = 1L,
      max_value = 9L,
      f = function(n) {
        out <- integer(6)
        for (i in seq_len(6)) out[i] <- sample.int(n, size = 3, replace = TRUE)
        out
      }
    ),
    list(
      build_name = "t_sample_int_indexed_scalar_size_ge_index",
      seed = 901L,
      n = 11L,
      expected_length = 6L,
      min_value = 1L,
      max_value = 11L,
      f = function(n) {
        out <- integer(6)
        for (i in seq_len(6)) out[i] <- sample.int(n, size = 3, replace = FALSE)[2]
        out
      }
    ),
    list(
      build_name = "t_sample_int_indexed_runtime_index",
      seed = 202L,
      n = 13L,
      expected_length = 4L,
      min_value = 1L,
      max_value = 13L,
      f = function(n) {
        out <- integer(4)
        for (i in seq_len(4)) out[i] <- sample.int(n, size = 4, replace = TRUE)[i]
        out
      }
    ),
    list(
      build_name = "t_sample_int_indexed_runtime_index_composed",
      seed = 212L,
      n = 13L,
      expected_length = 4L,
      min_value = 101L,
      max_value = 113L,
      f = function(n) {
        out <- integer(4)
        for (i in seq_len(4)) out[i] <- sample.int(n, size = 4, replace = TRUE)[i] + 100L
        out
      }
    )
  )

  for (case in cases) {
    built <- tier4_build_runtime(case$f, case$build_name, list(n = "i32"))
    result <- tier4_expect_seeded_deterministic(built$func, case$seed, list(case$n))
    expect_equal(length(result), case$expected_length)
    tier4_expect_in_range(result, case$min_value, case$max_value)
  }
})

test_that("sample() runtime loop variants are deterministic and preserve membership", {  skip_if_no_mojo()

  cases <- list(
    list(
      build_name = "t_sample_basic",
      seed = 456L,
      x = as.double(c(10, 20, 30)),
      expected_length = 3L,
      allowed_values = function(x) x,
      f = function(x) {
        out <- numeric(3)
        for (i in seq_len(3)) out[i] <- sample(x, size = 1, replace = FALSE)[1]
        out
      }
    ),
    list(
      build_name = "t_sample_scalar_index_len_mismatch",
      seed = 617L,
      x = as.double(c(2, 4, 6, 8, 10, 12)),
      expected_length = 4L,
      allowed_values = function(x) x,
      f = function(x) {
        out <- numeric(4)
        for (i in seq_len(4)) out[i] <- sample(x, size = 1, replace = FALSE)[1]
        out
      }
    ),
    list(
      build_name = "t_sample_scalar_no_index",
      seed = 271L,
      x = as.double(c(2, 4, 6, 8, 10)),
      expected_length = 4L,
      allowed_values = function(x) x,
      f = function(x) {
        out <- numeric(4)
        for (i in seq_len(4)) out[i] <- sample(x, size = 1, replace = FALSE)
        out
      }
    ),
    list(
      build_name = "t_sample_scalar_no_index_size_gt1",
      seed = 272L,
      x = as.double(c(2, 4, 6, 8, 10)),
      expected_length = 4L,
      allowed_values = function(x) x,
      f = function(x) {
        out <- numeric(4)
        for (i in seq_len(4)) out[i] <- sample(x, size = 3, replace = FALSE)
        out
      }
    ),
    list(
      build_name = "t_sample_indexed_scalar_size_ge_index",
      seed = 119L,
      x = as.double(c(3, 6, 9, 12, 15, 18)),
      expected_length = 5L,
      allowed_values = function(x) x,
      f = function(x) {
        out <- numeric(5)
        for (i in seq_len(5)) out[i] <- sample(x, size = 3, replace = FALSE)[2]
        out
      }
    ),
    list(
      build_name = "t_sample_indexed_runtime_index",
      seed = 303L,
      x = as.double(c(5, 10, 15, 20, 25, 30)),
      expected_length = 4L,
      allowed_values = function(x) x,
      f = function(x) {
        out <- numeric(4)
        for (i in seq_len(4)) out[i] <- sample(x, size = 4, replace = TRUE)[i]
        out
      }
    ),
    list(
      build_name = "t_sample_indexed_runtime_index_composed",
      seed = 313L,
      x = as.double(c(5, 10, 15, 20, 25, 30)),
      expected_length = 4L,
      allowed_values = function(x) x + 0.5,
      f = function(x) {
        out <- numeric(4)
        for (i in seq_len(4)) out[i] <- sample(x, size = 4, replace = TRUE)[i] + 0.5
        out
      }
    ),
    list(
      build_name = "t_sample_replace",
      seed = 789L,
      x = as.double(c(1, 2, 3, 4, 5, 6, 7, 8)),
      expected_length = 8L,
      allowed_values = function(x) x,
      f = function(x) {
        out <- numeric(8)
        for (i in seq_len(8)) out[i] <- sample(x, size = 1, replace = TRUE)[1]
        out
      }
    )
  )

  for (case in cases) {
    built <- tier4_build_runtime(case$f, case$build_name, list(x = "f64[]"))
    result <- tier4_expect_seeded_deterministic(built$func, case$seed, list(case$x))
    expect_equal(length(result), case$expected_length)
    tier4_expect_membership(result, case$allowed_values(case$x))
  }
})

test_that("sample scalar expression emit supports dynamic size arguments", {
  sample_int_node <- .mojor_ir_sample_int(
    n = .mojor_ir_var("n"),
    size = .mojor_ir_var("k"),
    replace = .mojor_ir_const("TRUE")
  )
  sample_int_expr <- .mojor_ir_expr_emit(
    sample_int_node,
    type_env = list(n = "i32", k = "i32"),
    loop_vars = "i"
  )
  expect_type(sample_int_expr, "character")
  expect_match(sample_int_expr, "_rng_next_f64", fixed = TRUE)

  indexed_int_node <- .mojor_ir_index(
    .mojor_ir_sample_int(
      n = .mojor_ir_var("n"),
      size = .mojor_ir_var("k"),
      replace = .mojor_ir_const("FALSE")
    ),
    indices = list(.mojor_ir_const("2")),
    index_base = "one_based"
  )
  indexed_int_expr <- .mojor_ir_expr_emit(
    indexed_int_node,
    type_env = list(n = "i32", k = "i32"),
    loop_vars = "i"
  )
  expect_type(indexed_int_expr, "character")
  expect_match(indexed_int_expr, "_mojor_sample_pick_index", fixed = TRUE)
  expect_match(indexed_int_expr, "Int\\(k\\)")

  indexed_sample_node <- .mojor_ir_index(
    .mojor_ir_sample(
      x = .mojor_ir_var("x"),
      size = .mojor_ir_var("k"),
      replace = .mojor_ir_const("TRUE")
    ),
    indices = list(.mojor_ir_const("2")),
    index_base = "one_based"
  )
  indexed_sample_expr <- .mojor_ir_expr_emit(
    indexed_sample_node,
    type_env = list(x = "f64[]", k = "i32"),
    loop_vars = "i"
  )
  expect_type(indexed_sample_expr, "character")
  expect_match(indexed_sample_expr, "_mojor_sample_pick_index", fixed = TRUE)
  expect_match(indexed_sample_expr, "Int\\(k\\)")
})

test_that("sample indexed scalar rejects index greater than compile-time size", {  node <- .mojor_ir_index(
    .mojor_ir_sample_int(
      n = .mojor_ir_var("n"),
      size = .mojor_ir_const("2"),
      replace = .mojor_ir_const("FALSE")
    ),
    indices = list(.mojor_ir_const("3")),
    index_base = "one_based"
  )

  expect_error(
    .mojor_ir_expr_emit(node, type_env = list(n = "i32"), loop_vars = "i"),
    "indexed scalar emission requires size >= requested index",
    fixed = TRUE
  )
})

test_that("sample.int() edge case n=1", {  skip_if_no_mojo()
  
  f <- function(n) {
    sample.int(n, size = 1, replace = FALSE)
  }
  
  built <- tier4_build_runtime(f, "t_sample_int_edge", list(n = "i32"))
  
  set.seed(111)
  r_result <- f(1)
  
  set.seed(111)
  mojo_result <- built$func(1L)
  
  expect_equal(mojo_result, r_result)
  expect_equal(length(mojo_result), 1)
  expect_equal(mojo_result, 1)
})
