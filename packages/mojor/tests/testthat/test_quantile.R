# =============================================================================
# Quantiles & Robust Stats Tests
# =============================================================================

source("helper-mojo.R")

# --- Verifier tests ---

test_that("verifier accepts median node", {  node <- list(kind = "median", x = list(kind = "var", name = "x"))
  expect_true(.mojor_ir_verify(node))
})

test_that("verifier accepts quantile node", {  node <- list(kind = "quantile",
               x = list(kind = "var", name = "x"),
               probs = list(kind = "var", name = "p"))
  expect_true(.mojor_ir_verify(node))
})

test_that("verifier accepts iqr node", {  node <- list(kind = "iqr", x = list(kind = "var", name = "x"))
  expect_true(.mojor_ir_verify(node))
})

test_that("verifier accepts mad node", {  node <- list(kind = "mad", x = list(kind = "var", name = "x"))
  expect_true(.mojor_ir_verify(node))
})

test_that("verifier rejects quantile node without probs", {  node <- list(kind = "quantile", x = list(kind = "var", name = "x"))
  expect_error(.mojor_ir_verify(node), "required field.*probs")
})

# --- Type inference tests ---

test_that("median returns f64", {  type_env <- list(x = "f64[]")
  node <- list(kind = "median", x = list(kind = "var", name = "x"))
  expect_equal(.mojor_ir_infer_type(node, type_env), "f64")
})

test_that("quantile returns f64[]", {  type_env <- list(x = "f64[]", p = "f64[]")
  node <- list(kind = "quantile",
               x = list(kind = "var", name = "x"),
               probs = list(kind = "var", name = "p"))
  expect_equal(.mojor_ir_infer_type(node, type_env), "f64[]")
})

test_that("iqr returns f64", {  type_env <- list(x = "f64[]")
  node <- list(kind = "iqr", x = list(kind = "var", name = "x"))
  expect_equal(.mojor_ir_infer_type(node, type_env), "f64")
})

test_that("mad returns f64", {  type_env <- list(x = "f64[]")
  node <- list(kind = "mad", x = list(kind = "var", name = "x"))
  expect_equal(.mojor_ir_infer_type(node, type_env), "f64")
})

# --- IR Node constructor tests ---

test_that("IR node constructors create correct structure", {  m <- .mojor_ir_median(list(kind = "var", name = "x"))
  expect_equal(m$kind, "median")
  expect_equal(m$x$name, "x")

  q <- .mojor_ir_quantile(list(kind = "var", name = "x"), list(kind = "var", name = "p"))
  expect_equal(q$kind, "quantile")
  expect_equal(q$probs$name, "p")
  expect_equal(q$na_rm, FALSE)
  expect_equal(q$type, 7L)

  iq <- .mojor_ir_iqr(list(kind = "var", name = "x"))
  expect_equal(iq$kind, "iqr")

  md <- .mojor_ir_mad(list(kind = "var", name = "x"))
  expect_equal(md$kind, "mad")
})

test_that("quantile parser supports na.rm and type arguments", {  q_node <- .mojor_ir_expr_build(quote(quantile(x, probs = p, na.rm = TRUE, type = 5)))
  expect_true(is.list(q_node) && q_node$kind == "quantile")
  expect_equal(q_node$na_rm$kind, "const")
  expect_equal(q_node$na_rm$value, "True")
  expect_equal(q_node$type$kind, "const")
  expect_equal(as.numeric(q_node$type$value), 5)
})

test_that("quantile parser supports positional na.rm and type arguments", {  q_node <- .mojor_ir_expr_build(quote(quantile(x, p, TRUE, 6)))
  expect_true(is.list(q_node) && q_node$kind == "quantile")
  expect_equal(q_node$na_rm$kind, "const")
  expect_equal(q_node$na_rm$value, "True")
  expect_equal(q_node$type$kind, "const")
  expect_equal(as.numeric(q_node$type$value), 6)
})

test_that("quantile verifier rejects invalid na.rm", {  node <- list(
    kind = "quantile",
    x = list(kind = "var", name = "x"),
    probs = list(kind = "var", name = "p"),
    na_rm = list(kind = "const", value = "2"),
    type = list(kind = "const", value = "7")
  )
  expect_error(.mojor_ir_verify(node), "na_rm must be a logical value")
})

test_that("quantile verifier rejects invalid type", {  node <- list(
    kind = "quantile",
    x = list(kind = "var", name = "x"),
    probs = list(kind = "var", name = "p"),
    na_rm = list(kind = "const", value = "False"),
    type = list(kind = "const", value = "9")
  )
  expect_error(.mojor_ir_verify(node), "type must be an integer")
})

# --- Expression-only transpile tests ---

test_that("expression-only quantile/robust ops transpile with helper imports", {  trans_median <- mojor_transpile(function(x) median(x), x = "f64[]")
  expect_true(isTRUE(trans_median$is_expression_kernel))
  expect_match(trans_median$mojo, "from quantile_helpers import mojor_median_f64", fixed = TRUE)
  expect_true(isTRUE(trans_median$quantile_needed))

  trans_iqr <- mojor_transpile(function(x) IQR(x), x = "f64[]")
  expect_true(isTRUE(trans_iqr$is_expression_kernel))
  expect_match(trans_iqr$mojo, "from quantile_helpers import mojor_iqr_f64", fixed = TRUE)

  trans_mad <- mojor_transpile(function(x) mad(x), x = "f64[]")
  expect_true(isTRUE(trans_mad$is_expression_kernel))
  expect_match(trans_mad$mojo, "from quantile_helpers import mojor_mad_f64", fixed = TRUE)
  expect_match(trans_mad$mojo, "mojor_median_f64", fixed = TRUE)

  trans_quantile <- mojor_transpile(function(x, p) quantile(x, p), x = "f64[]", p = "f64[]")
  expect_true(isTRUE(trans_quantile$is_expression_kernel))
  expect_true(isTRUE(trans_quantile$is_vector_output))
  expect_equal(trans_quantile$out_type, "f64[]")
  expect_match(trans_quantile$mojo, "from quantile_helpers import mojor_quantile_f64", fixed = TRUE)
  expect_true(isTRUE(trans_quantile$quantile_needed))
})

test_that("expression-only quantile supports literal probs vectors", {  trans_quantile_lit <- mojor_transpile(
    function(x) quantile(x, c(0.25, 0.5, 0.75)),
    x = "f64[]"
  )
  expect_true(isTRUE(trans_quantile_lit$is_expression_kernel))
  expect_true(isTRUE(trans_quantile_lit$is_vector_output))
  expect_equal(trans_quantile_lit$out_type, "f64[]")
  expect_identical(trans_quantile_lit$transpile_route$route, "loop")
  expect_identical(
    trans_quantile_lit$transpile_route$reason,
    "normalized_ir_whole_vector_call_quantile"
  )
  expect_match(trans_quantile_lit$mojo, "__mojor_norm_probs", fixed = TRUE)
})

test_that("expression-only quantile supports computed compile-time probs expressions", {  trans_quantile_expr <- mojor_transpile(
    function(x) quantile(x, c(0.10 + 0.15, 0.5, 1 - 0.25)),
    x = "f64[]"
  )
  expect_true(isTRUE(trans_quantile_expr$is_expression_kernel))
  expect_identical(trans_quantile_expr$transpile_route$route, "loop")
  expect_identical(
    trans_quantile_expr$transpile_route$reason,
    "normalized_ir_whole_vector_call_quantile"
  )
  expect_match(trans_quantile_expr$mojo, "__mojor_norm_probs", fixed = TRUE)

  trans_quantile_seq <- mojor_transpile(
    function(x) quantile(x, seq(0.2, 0.8, by = 0.3)),
    x = "f64[]"
  )
  expect_true(isTRUE(trans_quantile_seq$is_expression_kernel))
  expect_identical(trans_quantile_seq$transpile_route$route, "loop")
  expect_identical(
    trans_quantile_seq$transpile_route$reason,
    "normalized_ir_whole_vector_call_quantile"
  )
  expect_match(trans_quantile_seq$mojo, "__mojor_norm_probs", fixed = TRUE)
})

# --- Runtime tests ---

test_that("runtime: expression-only median/iqr/mad match base R", {  skip_if_no_mojo()

  x_med <- as.double(c(1, 2, 3, 4, 5))
  x_iqr <- as.double(1:10)

  built_median <- mojor_build(
    function(x) median(x),
    x = "f64[]",
    object_mode = "off",
    cache = FALSE,
    load = TRUE,
    name = "t74_median_rt"
  )
  expect_equal(unname(built_median$func(x_med)), unname(median(x_med)), tolerance = 1e-10)

  built_iqr <- mojor_build(
    function(x) IQR(x),
    x = "f64[]",
    object_mode = "off",
    cache = FALSE,
    load = TRUE,
    name = "t74_iqr_rt"
  )
  expect_equal(unname(built_iqr$func(x_iqr)), unname(IQR(x_iqr)), tolerance = 1e-10)

  built_mad <- mojor_build(
    function(x) mad(x),
    x = "f64[]",
    object_mode = "off",
    cache = FALSE,
    load = TRUE,
    name = "t74_mad_rt"
  )
  expect_equal(unname(built_mad$func(x_med)), unname(mad(x_med)), tolerance = 1e-10)
})

test_that("runtime: expression-only quantile(x, probs) matches base R", {  skip_if_no_mojo()

  x <- as.double(1:10)
  probs <- c(0.25, 0.5, 0.75)

  built_quantile <- mojor_build(
    function(x, p) quantile(x, p),
    x = "f64[]",
    p = "f64[]",
    object_mode = "off",
    cache = FALSE,
    load = TRUE,
    name = "t74_quantile_rt"
  )

  expect_equal(
    unname(built_quantile$func(x, probs)),
    unname(quantile(x, probs)),
    tolerance = 1e-10
  )
})

test_that("runtime: expression-only quantile(x, c(...)) matches base R", {  skip_if_no_mojo()

  x <- as.double(c(4, 1, 7, 9, 3, 6, 2, 8, 5))
  probs <- c(0.2, 0.5, 0.8)

  built_quantile_lit <- mojor_build(
    function(x) quantile(x, c(0.2, 0.5, 0.8)),
    x = "f64[]",
    object_mode = "off",
    cache = FALSE,
    load = TRUE,
    name = "t74_quantile_lit_rt"
  )

  expect_equal(
    unname(built_quantile_lit$func(x)),
    unname(quantile(x, probs)),
    tolerance = 1e-10
  )
})

test_that("runtime: expression-only quantile(x, computed probs expression) matches base R", {  skip_if_no_mojo()

  x <- as.double(c(4, 1, 7, 9, 3, 6, 2, 8, 5))
  probs <- c(0.25, 0.5, 0.75)

  built_quantile_expr <- mojor_build(
    function(x) quantile(x, c(0.10 + 0.15, 0.5, 1 - 0.25)),
    x = "f64[]",
    object_mode = "off",
    cache = FALSE,
    load = TRUE,
    name = "t74_quantile_expr_rt"
  )

  expect_equal(
    unname(built_quantile_expr$func(x)),
    unname(quantile(x, probs)),
    tolerance = 1e-10
  )
})
