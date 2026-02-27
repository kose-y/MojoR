# Unit tests for type-based constant folding (Phase 3)

test_that("type annotation parsing works", {  # Test f64[]
  info <- .mojor_parse_type_annotation("f64[]")
  expect_equal(info$kind, "vector")
  expect_equal(info$elem_type, "f64")

  # Test i32[]
  info <- .mojor_parse_type_annotation("i32[]")
  expect_equal(info$kind, "vector")
  expect_equal(info$elem_type, "i32")

  # Test f64[,]
  info <- .mojor_parse_type_annotation("f64[,]")
  expect_equal(info$kind, "matrix")
  expect_equal(info$elem_type, "f64")

  # Test logical[]
  info <- .mojor_parse_type_annotation("logical[]")
  expect_equal(info$kind, "vector")
  expect_equal(info$elem_type, "logical")
})

test_that("type predicate evaluation works", {  # is.vector on vector
  info <- .mojor_parse_type_annotation("f64[]")
  expect_true(.mojor_eval_type_predicate("is.vector", info))
  expect_false(.mojor_eval_type_predicate("is.matrix", info))

  # is.matrix on matrix
  info <- .mojor_parse_type_annotation("f64[,]")
  expect_false(.mojor_eval_type_predicate("is.vector", info))
  expect_true(.mojor_eval_type_predicate("is.matrix", info))

  # is.numeric on different types
  info_f64 <- .mojor_parse_type_annotation("f64[]")
  info_i32 <- .mojor_parse_type_annotation("i32[]")
  info_lgl <- .mojor_parse_type_annotation("logical[]")

  expect_true(.mojor_eval_type_predicate("is.numeric", info_f64))
  expect_true(.mojor_eval_type_predicate("is.numeric", info_i32))
  expect_false(.mojor_eval_type_predicate("is.numeric", info_lgl))

  # is.integer/is.double
  expect_false(.mojor_eval_type_predicate("is.integer", info_f64))
  expect_true(.mojor_eval_type_predicate("is.double", info_f64))
  expect_true(.mojor_eval_type_predicate("is.integer", info_i32))
  expect_false(.mojor_eval_type_predicate("is.double", info_i32))
})

test_that("type query evaluation works", {  # typeof
  info_f64 <- .mojor_parse_type_annotation("f64[]")
  info_i32 <- .mojor_parse_type_annotation("i32[]")

  expect_equal(.mojor_eval_type_query("typeof", info_f64), "double")
  expect_equal(.mojor_eval_type_query("typeof", info_i32), "integer")

  # mode
  expect_equal(.mojor_eval_type_query("mode", info_f64), "numeric")
  expect_equal(.mojor_eval_type_query("mode", info_i32), "numeric")

  # class
  info_vec <- .mojor_parse_type_annotation("f64[]")
  info_mat <- .mojor_parse_type_annotation("f64[,]")

  expect_equal(.mojor_eval_type_query("class", info_vec), "numeric")
  expect_equal(.mojor_eval_type_query("class", info_mat), "matrix")
})

test_that("type folding on IR nodes works", {  # Create a simple type_predicate IR node
  x_var <- .mojor_ir_var("x")
  pred_node <- .mojor_ir_type_predicate("is.vector", x_var)

  # Fold it with type map
  type_map <- list(x = "f64[]")
  folded <- .mojor_ir_fold_type_expr(pred_node, type_map)

  # Should be folded to TRUE constant
  expect_equal(folded$kind, "const")
  expect_equal(folded$value, "True")
})

test_that("type folding on type_query nodes works", {  # Create a type_query IR node
  x_var <- .mojor_ir_var("x")
  query_node <- .mojor_ir_type_query("typeof", x_var)

  # Fold it with type map
  type_map <- list(x = "f64[]")
  folded <- .mojor_ir_fold_type_expr(query_node, type_map)

  # Should be folded to "double" string constant
  expect_equal(folded$kind, "const")
  expect_equal(folded$value, '"double"')
})
