# Unit tests for dim() lowering (Phase 3)

test_that("dim_map building works", {  # Build a test dim_map
  nrow_map <- c(x = "nrow_x", y = "nrow_y")
  ncol_map <- c(x = "ncol_x")
  dim_map_raw <- c(z = "dim_z")
  ndim_map <- c(z = "ndim_z")

  .build_dim_map <- function(nrow_map, ncol_map, dim_map, ndim_map) {
    result <- list()
    for (nm in names(nrow_map)) {
      result[[nm]] <- list(
        kind = "matrix",
        nrow = nrow_map[[nm]],
        ncol = if (nm %in% names(ncol_map)) ncol_map[[nm]] else NULL
      )
    }
    for (nm in names(dim_map)) {
      if (!is.null(result[[nm]])) next
      result[[nm]] <- list(
        kind = "array",
        dim = dim_map[[nm]],
        ndim = if (nm %in% names(ndim_map)) ndim_map[[nm]] else NULL
      )
    }
    result
  }

  dim_map <- .build_dim_map(nrow_map, ncol_map, dim_map_raw, ndim_map)

  # Check matrix entries
  expect_equal(dim_map$x$kind, "matrix")
  expect_equal(dim_map$x$nrow, "nrow_x")
  expect_equal(dim_map$x$ncol, "ncol_x")

  expect_equal(dim_map$y$kind, "matrix")
  expect_equal(dim_map$y$nrow, "nrow_y")
  expect_null(dim_map$y$ncol)

  # Check array entries
  expect_equal(dim_map$z$kind, "array")
  expect_equal(dim_map$z$dim, "dim_z")
  expect_equal(dim_map$z$ndim, "ndim_z")
})

test_that("dim() lowering for matrix dim(x)[1]", {  # Create dim(x)[1] pattern
  x_var <- .mojor_ir_var("x")
  dim_node <- .mojor_ir_dim(x_var)
  idx_node <- .mojor_ir_index(dim_node, list(.mojor_ir_const("1")))

  # Build dim_map
  dim_map <- list(
    x = list(kind = "matrix", nrow = "nrow_x", ncol = "ncol_x")
  )

  # Lower it
  lowered <- .mojor_ir_lower_dim_expr(idx_node, dim_map)

  # Should be cast(Int32, var(nrow_x))
  expect_equal(lowered$kind, "cast")
  expect_equal(lowered$to, "Int32")
  expect_equal(lowered$expr$kind, "var")
  expect_equal(lowered$expr$name, "nrow_x")
})

test_that("dim() lowering for matrix dim(x)[2]", {  x_var <- .mojor_ir_var("x")
  dim_node <- .mojor_ir_dim(x_var)
  idx_node <- .mojor_ir_index(dim_node, list(.mojor_ir_const("2")))

  dim_map <- list(
    x = list(kind = "matrix", nrow = "nrow_x", ncol = "ncol_x")
  )

  lowered <- .mojor_ir_lower_dim_expr(idx_node, dim_map)

  # Should be cast(Int32, var(ncol_x))
  expect_equal(lowered$kind, "cast")
  expect_equal(lowered$to, "Int32")
  expect_equal(lowered$expr$kind, "var")
  expect_equal(lowered$expr$name, "ncol_x")
})

test_that("dim() lowering for array dim(x)[i]", {  x_var <- .mojor_ir_var("x")
  dim_node <- .mojor_ir_dim(x_var)
  idx_node <- .mojor_ir_index(dim_node, list(.mojor_ir_const("3")))

  dim_map <- list(
    x = list(kind = "array", dim = "dim_x", ndim = "ndim_x")
  )

  lowered <- .mojor_ir_lower_dim_expr(idx_node, dim_map)

  # Should be cast(Int32, index(var(dim_x), [const("2")], zero_based))
  expect_equal(lowered$kind, "cast")
  expect_equal(lowered$to, "Int32")
  expect_equal(lowered$expr$kind, "index")
  expect_equal(lowered$expr$base$name, "dim_x")
  expect_equal(lowered$expr$indices[[1]]$value, "2")  # 3-1 = 2
  expect_equal(lowered$expr$index_base, "zero_based")
})

test_that("length(dim(x)) lowering for matrix", {  x_var <- .mojor_ir_var("x")
  dim_node <- .mojor_ir_dim(x_var)
  len_node <- .mojor_ir_call("length", list(dim_node))

  dim_map <- list(
    x = list(kind = "matrix", nrow = "nrow_x", ncol = "ncol_x")
  )

  lowered <- .mojor_ir_lower_dim_expr(len_node, dim_map)

  # Should be const("2")
  expect_equal(lowered$kind, "const")
  expect_equal(lowered$value, "2")
})

test_that("length(dim(x)) lowering for array", {  x_var <- .mojor_ir_var("x")
  dim_node <- .mojor_ir_dim(x_var)
  len_node <- .mojor_ir_call("length", list(dim_node))

  dim_map <- list(
    x = list(kind = "array", dim = "dim_x", ndim = "ndim_x")
  )

  lowered <- .mojor_ir_lower_dim_expr(len_node, dim_map)

  # Should be var("ndim_x")
  expect_equal(lowered$kind, "var")
  expect_equal(lowered$name, "ndim_x")
})

test_that("dim() lowering with non-constant index is not lowered", {  # dim(x)[i] where i is a variable
  x_var <- .mojor_ir_var("x")
  i_var <- .mojor_ir_var("i")
  dim_node <- .mojor_ir_dim(x_var)
  idx_node <- .mojor_ir_index(dim_node, list(i_var))

  dim_map <- list(
    x = list(kind = "matrix", nrow = "nrow_x", ncol = "ncol_x")
  )

  lowered <- .mojor_ir_lower_dim_expr(idx_node, dim_map)

  # Should NOT be lowered (remains as index(dim(x), [var(i)]))
  expect_equal(lowered$kind, "index")
  expect_equal(lowered$base$kind, "dim")
})

test_that("dim() lowering in nested expressions", {  # x + dim(y)[1]
  x_var <- .mojor_ir_var("x")
  y_var <- .mojor_ir_var("y")
  dim_node <- .mojor_ir_dim(y_var)
  idx_node <- .mojor_ir_index(dim_node, list(.mojor_ir_const("1")))
  binop <- .mojor_ir_binop("+", x_var, idx_node)

  dim_map <- list(
    y = list(kind = "matrix", nrow = "nrow_y", ncol = "ncol_y")
  )

  lowered <- .mojor_ir_lower_dim_expr(binop, dim_map)

  # Should lower the RHS
  expect_equal(lowered$kind, "binop")
  expect_equal(lowered$lhs$name, "x")
  expect_equal(lowered$rhs$kind, "cast")
  expect_equal(lowered$rhs$to, "Int32")
  expect_equal(lowered$rhs$expr$name, "nrow_y")
})

test_that("dim() lowering in assignment", {  # n <- dim(x)[1]
  x_var <- .mojor_ir_var("x")
  n_var <- .mojor_ir_var("n")
  dim_node <- .mojor_ir_dim(x_var)
  idx_node <- .mojor_ir_index(dim_node, list(.mojor_ir_const("1")))
  assign <- .mojor_ir_assign(n_var, idx_node)

  dim_map <- list(
    x = list(kind = "matrix", nrow = "nrow_x", ncol = "ncol_x")
  )

  lowered <- .mojor_ir_lower_dim_stmt(assign, dim_map)

  # Should lower the RHS
  expect_equal(lowered$kind, "assign")
  expect_equal(lowered$rhs$kind, "cast")
  expect_equal(lowered$rhs$to, "Int32")
  expect_equal(lowered$rhs$expr$name, "nrow_x")
})

# =============================================================================
# Tests for dim() as a standalone function (returns integer vector)
# =============================================================================

test_that("dim() IR node is created correctly", {  x_var <- .mojor_ir_var("x")
  dim_node <- .mojor_ir_dim(x_var)
  
  expect_equal(dim_node$kind, "dim")
  expect_equal(dim_node$x$kind, "var")
  expect_equal(dim_node$x$name, "x")
})

test_that("dim() emission for matrix x", {  # Create dim(x) node for a matrix
  x_var <- .mojor_ir_var("x")
  dim_node <- .mojor_ir_dim(x_var)
  
  # Set up state for output matrix
  .mojor_test_local_state("current_out_name", "x")
  .mojor_test_local_state("current_out_dim_var", "out_dim")
  .mojor_test_local_state("current_out_ndim_var", "out_ndim")
  
  # Emit
  result <- .mojor_ir_expr_emit(dim_node, type_env = list(x = "f64[2d]"))
  
  # IR emit returns the resolved runtime dim pointer variable.
  expect_identical(result, "out_dim")
  
})

test_that("dim() emission for array x", {  # Create dim(x) node for an array
  x_var <- .mojor_ir_var("x")
  dim_node <- .mojor_ir_dim(x_var)
  
  # Set up state for input array
  .mojor_test_local_state("current_dim_var_map", list(x = "dim_x"))
  .mojor_test_local_state("current_ndim_var_map", list(x = "ndim_x"))
  
  # Emit
  result <- .mojor_ir_expr_emit(dim_node, type_env = list(x = "f64[3d]"))
  
  # IR emit returns the resolved runtime dim pointer variable.
  expect_identical(result, "dim_x")
  
})

test_that("dim() in expression context", {  # x + dim(y)[1] should work alongside dim(y) as vector
  x_var <- .mojor_ir_var("x")
  y_var <- .mojor_ir_var("y")
  dim_y <- .mojor_ir_dim(y_var)
  
  # Set up state
  .mojor_test_local_state("current_dim_var_map", list(y = "dim_y"))
  .mojor_test_local_state("current_ndim_var_map", list(y = "ndim_y"))
  
  # Emit dim(y)
  result <- .mojor_ir_expr_emit(dim_y, type_env = list(y = "f64[2d]"))
  
  # IR emit returns the resolved runtime dim pointer variable.
  expect_identical(result, "dim_y")
  
})
