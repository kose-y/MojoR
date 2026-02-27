# Test suite for IR effect system
# This file validates that .mojor_ir_expr_effects() correctly identifies side effects

library(testthat)

# =============================================================================
# Pure Expression Tests
# =============================================================================

test_that("constants are pure", {  nodes <- list(
    .mojor_ir_const("42"),
    .mojor_ir_const("3.14"),
    .mojor_ir_const("TRUE")
  )

  for (node in nodes) {
    expect_equal(.mojor_ir_expr_effects(node), "Pure")
    expect_true(.mojor_ir_is_pure(node))
  }
})

test_that("variables are pure", {  node <- .mojor_ir_var("x")
  expect_equal(.mojor_ir_expr_effects(node), "Pure")
  expect_true(.mojor_ir_is_pure(node))
})

test_that("arithmetic is pure", {  nodes <- list(
    # Unary
    .mojor_ir_unop("-", .mojor_ir_var("x")),
    .mojor_ir_unop("!", .mojor_ir_var("x")),
    # Binary
    .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_var("y")),
    .mojor_ir_binop("-", .mojor_ir_var("x"), .mojor_ir_const("1")),
    .mojor_ir_binop("*", .mojor_ir_const("2"), .mojor_ir_var("y")),
    .mojor_ir_binop("/", .mojor_ir_var("x"), .mojor_ir_var("y"))
  )

  for (node in nodes) {
    expect_equal(.mojor_ir_expr_effects(node), "Pure")
    expect_true(.mojor_ir_is_pure(node))
  }
})

test_that("comparisons are pure", {  nodes <- list(
    .mojor_ir_binop(">", .mojor_ir_var("x"), .mojor_ir_const("0")),
    .mojor_ir_binop("<", .mojor_ir_var("x"), .mojor_ir_var("y")),
    .mojor_ir_binop("==", .mojor_ir_var("x"), .mojor_ir_var("y")),
    .mojor_ir_binop("!=", .mojor_ir_var("x"), .mojor_ir_const("0"))
  )

  for (node in nodes) {
    expect_equal(.mojor_ir_expr_effects(node), "Pure")
    expect_true(.mojor_ir_is_pure(node))
  }
})

test_that("casts are pure (inherit from operand)", {  node <- .mojor_ir_cast("f64", .mojor_ir_var("x"))
  expect_equal(.mojor_ir_expr_effects(node), "Pure")
  expect_true(.mojor_ir_is_pure(node))
})

test_that("math functions are pure", {  math_fns <- c("sin", "cos", "tan", "asin", "acos", "atan",
                "log", "log10", "log1p", "log2", "exp", "expm1",
                "sqrt", "abs", "floor", "ceiling", "trunc", "round")

  for (fn in math_fns) {
    node <- .mojor_ir_call(fn, list(.mojor_ir_var("x")))
    expect_equal(.mojor_ir_expr_effects(node), "Pure", label = fn)
    expect_true(.mojor_ir_is_pure(node), label = fn)
  }
})

test_that("min/max are pure", {  nodes <- list(
    .mojor_ir_call("min", list(.mojor_ir_var("x"), .mojor_ir_var("y"))),
    .mojor_ir_call("max", list(.mojor_ir_var("x"), .mojor_ir_var("y")))
  )

  for (node in nodes) {
    expect_equal(.mojor_ir_expr_effects(node), "Pure")
    expect_true(.mojor_ir_is_pure(node))
  }
})

test_that("is.* predicates are pure", {  predicates <- c("is.na", "is.nan", "is.finite", "is.infinite")

  for (pred in predicates) {
    node <- .mojor_ir_call(pred, list(.mojor_ir_var("x")))
    expect_equal(.mojor_ir_expr_effects(node), "Pure", label = pred)
    expect_true(.mojor_ir_is_pure(node), label = pred)
  }
})

test_that("length() on variables is pure", {  node <- .mojor_ir_call("length", list(.mojor_ir_var("x")))
  expect_equal(.mojor_ir_expr_effects(node), "Pure")
  expect_true(.mojor_ir_is_pure(node))
})

# =============================================================================
# ReadsMem Effect Tests
# =============================================================================

test_that("array indexing reads memory", {  node <- .mojor_ir_index(
    .mojor_ir_var("x"),
    list(.mojor_ir_var("i"))
  )

  effects <- .mojor_ir_expr_effects(node)
  expect_true("ReadsMem" %in% effects)
  expect_false(.mojor_ir_is_pure(node))
  expect_true(.mojor_ir_reads_mem(node))
})

test_that("multi-dimensional indexing reads memory", {  node <- .mojor_ir_index(
    .mojor_ir_var("mat"),
    list(.mojor_ir_var("i"), .mojor_ir_var("j"))
  )

  effects <- .mojor_ir_expr_effects(node)
  expect_true("ReadsMem" %in% effects)
  expect_true(.mojor_ir_reads_mem(node))
})

test_that("gpu reduction and matmul are marked as ReadsMem", {  red <- .mojor_ir_gpu_reduce(
    "sum",
    .mojor_ir_var("x"),
    dims = .mojor_ir_const("1"),
    keepdims = FALSE
  )
  mm <- .mojor_ir_gpu_matmul(
    .mojor_ir_var("a"),
    .mojor_ir_var("b"),
    transpose_a = FALSE,
    transpose_b = FALSE
  )

  red_eff <- .mojor_ir_expr_effects(red)
  mm_eff <- .mojor_ir_expr_effects(mm)

  expect_true("ReadsMem" %in% red_eff)
  expect_true("ReadsMem" %in% mm_eff)
  expect_false(.mojor_ir_is_pure(red))
  expect_false(.mojor_ir_is_pure(mm))
})

test_that("indexing with expressions reads memory", {  # x[i + 1]
  node <- .mojor_ir_index(
    .mojor_ir_var("x"),
    list(.mojor_ir_binop("+", .mojor_ir_var("i"), .mojor_ir_const("1")))
  )

  effects <- .mojor_ir_expr_effects(node)
  expect_true("ReadsMem" %in% effects)
})

# =============================================================================
# RNG Effect Tests
# =============================================================================

test_that("RNG functions have RNG effect", {  rng_fns <- .mojor_ir_rng_call_fns()
  expect_true(length(rng_fns) >= 19)

  for (fn in rng_fns) {
    node <- .mojor_ir_call(fn, list(.mojor_ir_var("n")))
    effects <- .mojor_ir_expr_effects(node)
    expect_equal(effects, "RNG", label = fn)
    expect_true(.mojor_ir_has_rng(node), label = fn)
    expect_false(.mojor_ir_is_pure(node), label = fn)
  }
})

test_that("sampling nodes carry RNG effect", {  s_int <- .mojor_ir_sample_int(
    n = .mojor_ir_var("n"),
    size = .mojor_ir_const("10"),
    replace = .mojor_ir_const("True")
  )
  s_vec <- .mojor_ir_sample(
    x = .mojor_ir_var("x"),
    size = .mojor_ir_const("10"),
    replace = .mojor_ir_const("False")
  )

  eff_int <- .mojor_ir_expr_effects(s_int)
  eff_vec <- .mojor_ir_expr_effects(s_vec)
  expect_true("RNG" %in% eff_int)
  expect_true("RNG" %in% eff_vec)
  expect_false(.mojor_ir_is_pure(s_int))
  expect_false(.mojor_ir_is_pure(s_vec))
})

# =============================================================================
# Effect Propagation Tests
# =============================================================================

test_that("effects propagate through unary ops", {  # -x[i] should have ReadsMem
  node <- .mojor_ir_unop(
    "-",
    .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
  )

  effects <- .mojor_ir_expr_effects(node)
  expect_true("ReadsMem" %in% effects)
})

test_that("effects propagate through binary ops", {  # x[i] + y[j] should have ReadsMem
  node <- .mojor_ir_binop(
    "+",
    .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i"))),
    .mojor_ir_index(.mojor_ir_var("y"), list(.mojor_ir_var("j")))
  )

  effects <- .mojor_ir_expr_effects(node)
  expect_true("ReadsMem" %in% effects)
})

test_that("mixed effects combine", {  # x[i] + rnorm(1) should have both ReadsMem and RNG
  node <- .mojor_ir_binop(
    "+",
    .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i"))),
    .mojor_ir_call("rnorm", list(.mojor_ir_const("1")))
  )

  effects <- .mojor_ir_expr_effects(node)
  expect_true("ReadsMem" %in% effects)
  expect_true("RNG" %in% effects)
  expect_true(.mojor_ir_reads_mem(node))
  expect_true(.mojor_ir_has_rng(node))
})

test_that("effects propagate through casts", {  # as.double(x[i]) should have ReadsMem
  node <- .mojor_ir_cast(
    "f64",
    .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
  )

  effects <- .mojor_ir_expr_effects(node)
  expect_true("ReadsMem" %in% effects)
})

test_that("effects propagate through math calls", {  # sin(x[i]) should have ReadsMem
  node <- .mojor_ir_call(
    "sin",
    list(.mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i"))))
  )

  effects <- .mojor_ir_expr_effects(node)
  expect_true("ReadsMem" %in% effects)
  expect_false(.mojor_ir_is_pure(node))
})

test_that("effects propagate through ifelse", {  # ifelse(cond, x[i], y[j]) should have ReadsMem
  node <- .mojor_ir_ifelse(
    .mojor_ir_var("cond"),
    .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i"))),
    .mojor_ir_index(.mojor_ir_var("y"), list(.mojor_ir_var("j")))
  )

  effects <- .mojor_ir_expr_effects(node)
  expect_true("ReadsMem" %in% effects)
})

test_that("ifelse with RNG branch has RNG effect", {  # ifelse(cond, rnorm(1), 0)
  node <- .mojor_ir_ifelse(
    .mojor_ir_var("cond"),
    .mojor_ir_call("rnorm", list(.mojor_ir_const("1"))),
    .mojor_ir_const("0")
  )

  effects <- .mojor_ir_expr_effects(node)
  expect_true("RNG" %in% effects)
})

# =============================================================================
# Complex Expression Tests
# =============================================================================

test_that("complex pure expression is pure", {  # sin(x) * cos(y) + sqrt(z)
  node <- .mojor_ir_binop(
    "+",
    .mojor_ir_binop(
      "*",
      .mojor_ir_call("sin", list(.mojor_ir_var("x"))),
      .mojor_ir_call("cos", list(.mojor_ir_var("y")))
    ),
    .mojor_ir_call("sqrt", list(.mojor_ir_var("z")))
  )

  expect_equal(.mojor_ir_expr_effects(node), "Pure")
  expect_true(.mojor_ir_is_pure(node))
})

test_that("complex expression with memory read is not pure", {  # sin(x[i]) * cos(y) + sqrt(z)
  node <- .mojor_ir_binop(
    "+",
    .mojor_ir_binop(
      "*",
      .mojor_ir_call(
        "sin",
        list(.mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i"))))
      ),
      .mojor_ir_call("cos", list(.mojor_ir_var("y")))
    ),
    .mojor_ir_call("sqrt", list(.mojor_ir_var("z")))
  )

  effects <- .mojor_ir_expr_effects(node)
  expect_true("ReadsMem" %in% effects)
  expect_false(.mojor_ir_is_pure(node))
})

# =============================================================================
# Helper Function Tests
# =============================================================================

test_that(".mojor_ir_is_pure helper works correctly", {  pure_nodes <- list(
    .mojor_ir_const("42"),
    .mojor_ir_var("x"),
    .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_var("y"))
  )

  impure_nodes <- list(
    .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i"))),
    .mojor_ir_call("rnorm", list(.mojor_ir_const("1")))
  )

  for (node in pure_nodes) {
    expect_true(.mojor_ir_is_pure(node))
  }

  for (node in impure_nodes) {
    expect_false(.mojor_ir_is_pure(node))
  }
})

test_that(".mojor_ir_has_rng helper works correctly", {  rng_nodes <- list(
    .mojor_ir_call("rnorm", list(.mojor_ir_const("1"))),
    .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_call("runif", list(.mojor_ir_const("1"))))
  )

  non_rng_nodes <- list(
    .mojor_ir_const("42"),
    .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i"))),
    .mojor_ir_call("sin", list(.mojor_ir_var("x")))
  )

  for (node in rng_nodes) {
    expect_true(.mojor_ir_has_rng(node))
  }

  for (node in non_rng_nodes) {
    expect_false(.mojor_ir_has_rng(node))
  }
})

test_that(".mojor_ir_reads_mem helper works correctly", {  reads_mem_nodes <- list(
    .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i"))),
    .mojor_ir_binop("+", .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i"))), .mojor_ir_var("y"))
  )

  no_reads_mem_nodes <- list(
    .mojor_ir_const("42"),
    .mojor_ir_var("x"),
    .mojor_ir_call("sin", list(.mojor_ir_var("x"))),
    .mojor_ir_call("rnorm", list(.mojor_ir_const("1")))
  )

  for (node in reads_mem_nodes) {
    expect_true(.mojor_ir_reads_mem(node))
  }

  for (node in no_reads_mem_nodes) {
    expect_false(.mojor_ir_reads_mem(node))
  }
})

# =============================================================================
# Edge Cases
# =============================================================================

test_that("NULL node returns Unknown", {  expect_equal(.mojor_ir_expr_effects(NULL), "Unknown")
})

test_that("non-list node returns Unknown", {  expect_equal(.mojor_ir_expr_effects("not a node"), "Unknown")
})

test_that("node without kind returns Unknown", {  node <- list(foo = "bar")
  expect_equal(.mojor_ir_expr_effects(node), "Unknown")
})

test_that("unknown function call returns Unknown", {  node <- .mojor_ir_call("custom_function", list(.mojor_ir_var("x")))
  expect_equal(.mojor_ir_expr_effects(node), "Unknown")
})

test_that("raw node returns Unknown", {  node <- .mojor_ir_raw(quote(x + y))
  expect_equal(.mojor_ir_expr_effects(node), "Unknown")
})

# =============================================================================
# Constructor Effects
# =============================================================================

test_that("rep with pure arguments is pure", {  node <- .mojor_ir_rep(
    .mojor_ir_var("x"),
    times = .mojor_ir_const("3")
  )

  expect_equal(.mojor_ir_expr_effects(node), "Pure")
})

test_that("rep with impure arguments has effects", {  # rep(x[i], times=3)
  node <- .mojor_ir_rep(
    .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i"))),
    times = .mojor_ir_const("3")
  )

  effects <- .mojor_ir_expr_effects(node)
  expect_true("ReadsMem" %in% effects)
})

test_that("c() with pure parts is pure", {  node <- .mojor_ir_c(list(
    .mojor_ir_var("x"),
    .mojor_ir_var("y"),
    .mojor_ir_const("1")
  ))

  expect_equal(.mojor_ir_expr_effects(node), "Pure")
})

test_that("c() with impure parts has effects", {  # c(x[i], y[j])
  node <- .mojor_ir_c(list(
    .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i"))),
    .mojor_ir_index(.mojor_ir_var("y"), list(.mojor_ir_var("j")))
  ))

  effects <- .mojor_ir_expr_effects(node)
  expect_true("ReadsMem" %in% effects)
})

# =============================================================================
# Optimization Implications
# =============================================================================

test_that("pure expressions can be CSE'd", {  # x + y (appears twice) - both are pure, can be CSE'd
  expr1 <- .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_var("y"))
  expr2 <- .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_var("y"))

  expect_true(.mojor_ir_is_pure(expr1))
  expect_true(.mojor_ir_is_pure(expr2))
  # In a real optimizer, these would be detected as identical and CSE'd
})

test_that("memory-reading expressions cannot be freely CSE'd", {  # x[i] (appears twice) - both read memory, CSE requires alias analysis
  expr1 <- .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
  expr2 <- .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))

  expect_false(.mojor_ir_is_pure(expr1))
  expect_false(.mojor_ir_is_pure(expr2))
  # CSE would require proving that x[i] doesn't change between uses
})

test_that("RNG expressions must never be CSE'd", {  # rnorm(1) (appears twice) - RNG, must evaluate separately
  expr1 <- .mojor_ir_call("rnorm", list(.mojor_ir_const("1")))
  expr2 <- .mojor_ir_call("rnorm", list(.mojor_ir_const("1")))

  expect_true(.mojor_ir_has_rng(expr1))
  expect_true(.mojor_ir_has_rng(expr2))
  # CSE would break semantics - each call must produce different random number
})
