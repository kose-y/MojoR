# =============================================================================
# Set/Match Primitives Tests
# =============================================================================

# --- Verifier tests ---

test_that("verifier accepts unique node", {  node <- list(kind = "unique", x = list(kind = "var", name = "x"))
  expect_true(.mojor_ir_verify(node))
})

test_that("verifier accepts duplicated node", {  node <- list(kind = "duplicated", x = list(kind = "var", name = "x"))
  expect_true(.mojor_ir_verify(node))
})

test_that("verifier accepts any_duplicated node", {  node <- list(kind = "any_duplicated", x = list(kind = "var", name = "x"))
  expect_true(.mojor_ir_verify(node))
})

test_that("verifier accepts match node", {  node <- list(kind = "match",
               x = list(kind = "var", name = "x"),
               table = list(kind = "var", name = "y"))
  expect_true(.mojor_ir_verify(node))
})

test_that("verifier accepts in node", {  node <- list(kind = "in",
               x = list(kind = "var", name = "x"),
               table = list(kind = "var", name = "y"))
  expect_true(.mojor_ir_verify(node))
})

test_that("verifier rejects match node without table", {  node <- list(kind = "match", x = list(kind = "var", name = "x"))
  expect_error(.mojor_ir_verify(node), "required field.*table")
})

# --- Type inference tests ---

test_that("unique type inferred from input", {  type_env <- list(x = "f64[]")
  node <- list(kind = "unique", x = list(kind = "var", name = "x"))
  expect_equal(.mojor_ir_infer_type(node, type_env), "f64[]")
})

test_that("duplicated returns lgl[]", {  type_env <- list(x = "f64[]")
  node <- list(kind = "duplicated", x = list(kind = "var", name = "x"))
  expect_equal(.mojor_ir_infer_type(node, type_env), "lgl[]")
})

test_that("any_duplicated returns i32", {  type_env <- list(x = "f64[]")
  node <- list(kind = "any_duplicated", x = list(kind = "var", name = "x"))
  expect_equal(.mojor_ir_infer_type(node, type_env), "i32")
})

test_that("match returns i32[]", {  type_env <- list(x = "f64[]", y = "f64[]")
  node <- list(kind = "match",
               x = list(kind = "var", name = "x"),
               table = list(kind = "var", name = "y"))
  expect_equal(.mojor_ir_infer_type(node, type_env), "i32[]")
})

test_that("in returns lgl[]", {  type_env <- list(x = "f64[]", y = "f64[]")
  node <- list(kind = "in",
               x = list(kind = "var", name = "x"),
               table = list(kind = "var", name = "y"))
  expect_equal(.mojor_ir_infer_type(node, type_env), "lgl[]")
})

# --- IR Node constructor tests ---

test_that("IR node constructors create correct structure", {  u <- .mojor_ir_unique(list(kind = "var", name = "x"))
  expect_equal(u$kind, "unique")
  expect_equal(u$x$name, "x")

  d <- .mojor_ir_duplicated(list(kind = "var", name = "x"))
  expect_equal(d$kind, "duplicated")

  ad <- .mojor_ir_any_duplicated(list(kind = "var", name = "x"))
  expect_equal(ad$kind, "any_duplicated")

  m <- .mojor_ir_match(list(kind = "var", name = "x"), list(kind = "var", name = "y"))
  expect_equal(m$kind, "match")
  expect_equal(m$table$name, "y")

  in_node <- .mojor_ir_in(list(kind = "var", name = "x"), list(kind = "var", name = "y"))
  expect_equal(in_node$kind, "in")
})
