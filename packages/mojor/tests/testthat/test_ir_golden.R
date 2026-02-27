library(testthat)

# Golden snapshots are canonicalized at default optimization level.
mojor_options(opt_level = 2L)

# Golden tests for MojoR IR
#
# Each test pins down:
#   1. The IR tree structure (via .mojor_ir_format() patterns)
#   2. That the built IR passes .mojor_ir_verify()
#   3. The expected Mojo output patterns (where applicable)
#
# These serve as regression anchors: any change that alters IR structure
# or Mojo output for these canonical inputs will fail here.

# =============================================================================
# Helpers
# =============================================================================

ir_from <- function(stmt) {
  ir <- .mojor_ir_build_stmt(stmt)
  if (!is.null(ir)) .mojor_ir_verify(ir)
  ir
}

ir_tree <- function(ir) {
  paste(.mojor_ir_format(ir), collapse = "\n")
}

transpile_ir <- function(f, ...) {
  mojor_transpile(f, ..., ir_only = TRUE)$mojo
}

# =============================================================================
# 1. Scalar sum reduction
# =============================================================================

test_that("golden: scalar sum reduction", {  ir <- ir_from(quote(for (i in 1:n) s <- s + x[i]))

  expect_equal(ir$kind, "loop")
  expect_equal(ir$var, "i")
  expect_equal(ir$reduce$kind, "sum")
  expect_equal(ir$reduce$acc, "s")

  # 1:n with variable n needs dynamic sign detection — stays as range_expr
  expect_equal(ir$range$kind, "range_expr")

  tree <- ir_tree(ir)
  expect_match(tree, "loop i")
  expect_match(tree, "binop \\+")
  # reduce annotation is on the node directly, not in format tree
})

# =============================================================================
# 2. Scalar product reduction
# =============================================================================

test_that("golden: scalar product reduction", {  ir <- ir_from(quote(for (i in 1:n) p <- p * x[i]))

  expect_equal(ir$reduce$kind, "product")
  expect_equal(ir$reduce$acc, "p")
})

# =============================================================================
# 3. Scalar min reduction
# =============================================================================

test_that("golden: scalar min reduction", {  ir <- ir_from(quote(for (i in 1:n) mn <- min(mn, x[i])))

  expect_equal(ir$reduce$kind, "min")
  expect_equal(ir$reduce$acc, "mn")
})

# =============================================================================
# 4. Elementwise array loop
# =============================================================================

test_that("golden: elementwise array assignment", {  ir <- ir_from(quote(for (i in 1:n) out[i] <- x[i] + y[i]))

  expect_equal(ir$kind, "loop")
  expect_true(is.null(ir$reduce))  # not a reduction

  # 1:n with variable n stays as range_expr (needs dynamic sign detection)
  expect_equal(ir$range$kind, "range_expr")

  tree <- ir_tree(ir)
  expect_match(tree, "loop i")
  expect_match(tree, "binop \\+")
  expect_match(tree, "index")
})

# =============================================================================
# 4b. seq_len canonicalization
# =============================================================================

test_that("golden: seq_len range canonicalization", {  ir <- ir_from(quote(for (i in seq_len(n)) out[i] <- x[i]))

  expect_equal(ir$range$kind, "range")
  expect_equal(ir$range$start$kind, "const")
  expect_equal(ir$range$start$value, "1")
  expect_equal(ir$range$end$kind, "var")
  expect_equal(ir$range$end$name, "n")
})

# =============================================================================
# 5. Elementwise transpilation — Mojo output
# =============================================================================

test_that("golden: elementwise loop transpiles to Mojo", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + y[i]
    out
  }
  mojo <- transpile_ir(f, x = "f64[]", y = "f64[]")
  expect_match(mojo, "for.*in range")
  expect_match(mojo, "\\+")
})

# =============================================================================
# 5b. SIMD vectorization (aligned chunk loop)
# =============================================================================

test_that("golden: vectorized elementwise loop emits optimized chunk loop", {  f <- function(x, y, n) {
    out <- numeric(n)
    for (i in seq_len(n)) out[i] <- x[i] + y[i]
    out
  }
  mojo <- mojor_transpile(
    f,
    x = "f64[]",
    y = "f64[]",
    n = "i32",
    assume_aligned = 32L,
    simd_mode = "explicit",
    na_mode = "unsafe",
    bounds_check = FALSE,
    ir_only = TRUE
  )$mojo
  if (grepl("var __mojor_simd_end", mojo, fixed = TRUE)) {
    expect_match(mojo, "var __mojor_simd_end = .* - \\(.* % _MOJOR_ALIGN_F64\\)")
    expect_match(mojo, "for __mojor_simd_i in range\\(0, __mojor_simd_end, _MOJOR_ALIGN_F64\\)")
    expect_true(grepl("x.load[width=_MOJOR_ALIGN_F64, alignment=_MOJOR_ALIGN](__mojor_simd_i)", mojo, fixed = TRUE))
    expect_true(grepl("y.load[width=_MOJOR_ALIGN_F64, alignment=_MOJOR_ALIGN](__mojor_simd_i)", mojo, fixed = TRUE))
    expect_true(grepl("out.store[width=_MOJOR_ALIGN_F64, alignment=_MOJOR_ALIGN](__mojor_simd_i, out_chunk)", mojo, fixed = TRUE))
    expect_match(mojo, "for i in range\\(__mojor_simd_end \\+ 1, .*\\+ 1\\)")
  } else {
    expect_true(grepl("var _mojor_unroll_main_count = ", mojo, fixed = TRUE))
    expect_true(grepl("for _mojor_i in range(", mojo, fixed = TRUE))
    expect_true(grepl("for i in range(_mojor_unroll_tail_start, ", mojo, fixed = TRUE))
  }
})

# =============================================================================
# 6. ifelse expression
# =============================================================================

test_that("golden: ifelse expression IR", {  # ifelse lowers to ifelse node; children may not appear in tree top-level
  ir <- ir_from(quote(for (i in 1:n) out[i] <- ifelse(x[i] > 0, x[i], 0)))

  tree <- ir_tree(ir)
  expect_match(tree, "ifelse")
  # cond is binop > — verify on node structure directly
  body_rhs <- ir$body$stmts[[1]]$rhs
  expect_equal(body_rhs$kind, "ifelse")
  expect_equal(body_rhs$cond$kind, "binop")
  expect_equal(body_rhs$cond$op, ">")
})

# =============================================================================
# 7. While loop
# =============================================================================

test_that("golden: while loop IR", {  ir <- ir_from(quote(while (i <= n) {
    s <- s + x[i]
    i <- i + 1
  }))

  expect_equal(ir$kind, "while")
  tree <- ir_tree(ir)
  expect_match(tree, "^while")
  expect_equal(ir$cond$kind, "binop")
  expect_equal(ir$cond$op, "<=")
})

# =============================================================================
# 8. Break and next inside loop
# =============================================================================

test_that("golden: break inside loop IR", {  ir <- ir_from(quote(for (i in 1:n) {
    if (x[i] < 0) break
    out[i] <- x[i]
  }))

  expect_equal(ir$kind, "loop")
  tree <- ir_tree(ir)
  expect_match(tree, "break")
  expect_match(tree, "if")
})

# =============================================================================
# 9. Missing index (mat[i, ] pattern)
# =============================================================================

test_that("golden: missing index assignment IR", {  ir <- ir_from(quote(mat[i, ] <- c(x[i], y[i])))

  expect_equal(ir$kind, "assign")
  expect_equal(ir$lhs$kind, "subscript")
  expect_equal(ir$lhs$indices[[2]]$kind, "missing_index")
  expect_equal(ir$rhs$kind, "c")
  expect_equal(length(ir$rhs$parts), 2)
})

# =============================================================================
# 10. Missing index transpilation
# =============================================================================

test_that("golden: missing column index transpiles", {  f <- function(x, y, n) {
    mat <- matrix(0, n, 2)
    for (i in 1:n) mat[i, ] <- c(x[i], y[i])
    mat
  }
  mojo <- transpile_ir(f, x = "f64[]", y = "f64[]", n = "i32")
  expect_match(mojo, "for __mojor_i2 in")
  expect_match(mojo, "_MOJOR_MATRIX_LAYOUT")
})

# =============================================================================
# 11. c() constructor parts
# =============================================================================

test_that("golden: c() constructor with three parts", {  ir <- ir_from(quote(mat[i, ] <- c(x[i], y[i], z[i])))

  expect_equal(ir$rhs$kind, "c")
  expect_equal(length(ir$rhs$parts), 3)
  # Each part is an index into a different array
  expect_equal(ir$rhs$parts[[1]]$kind, "index")
  expect_equal(ir$rhs$parts[[2]]$kind, "index")
  expect_equal(ir$rhs$parts[[3]]$kind, "index")
})

# =============================================================================
# 12. rep() constructor
# =============================================================================

test_that("golden: rep() constructor IR", {  ir <- ir_from(quote(for (i in 1:n) out[i] <- rep(x[i], 3)))

  tree <- ir_tree(ir)
  expect_match(tree, "loop i")
  # rep node present in tree
  expect_match(tree, "rep")
})

# =============================================================================
# 13. Type cast (as.integer)
# =============================================================================

test_that("golden: type cast IR", {  ir <- ir_from(quote(for (i in 1:n) out[i] <- as.integer(x[i])))

  tree <- ir_tree(ir)
  # as.integer maps to Mojo "Int" cast
  expect_match(tree, "cast Int")
  rhs <- ir$body$stmts[[1]]$rhs
  expect_equal(rhs$kind, "cast")
  expect_equal(rhs$to, "Int")
})

# =============================================================================
# 14. Math function
# =============================================================================

test_that("golden: math function (log) IR", {  ir <- ir_from(quote(for (i in 1:n) out[i] <- log(x[i])))

  tree <- ir_tree(ir)
  expect_match(tree, "call log")
})

# =============================================================================
# 15. Math transpilation — log appears in Mojo
# =============================================================================

test_that("golden: math function transpiles to Mojo", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- log(x[i])
    out
  }
  mojo <- transpile_ir(f, x = "f64[]")
  expect_match(mojo, "log\\(")
})

# =============================================================================
# 16. Nested loops
# =============================================================================

test_that("golden: nested loops IR", {  ir <- ir_from(quote(
    for (i in 1:n) {
      for (j in 1:m) {
        out[i] <- out[i] + mat[i] * x[j]
      }
    }
  ))

  expect_equal(ir$kind, "loop")
  expect_equal(ir$var, "i")
  inner <- ir$body$stmts[[1]]
  expect_equal(inner$kind, "loop")
  expect_equal(inner$var, "j")
})

# =============================================================================
# 17. If-else statement
# =============================================================================

test_that("golden: if-else statement IR", {  ir <- ir_from(quote(
    if (n > 0) {
      out <- x[1]
    } else {
      out <- 0
    }
  ))

  expect_equal(ir$kind, "if")
  expect_equal(ir$cond$kind, "binop")
  expect_equal(ir$cond$op, ">")
  expect_false(is.null(ir$else_block))
})

# =============================================================================
# 18. Return statement
# =============================================================================

test_that("golden: return statement IR", {  ir <- ir_from(quote(return(x + y)))

  expect_equal(ir$kind, "return")
  expect_equal(ir$value$kind, "binop")
  expect_equal(ir$value$op, "+")
})

# =============================================================================
# 19. 2D matrix index on RHS
# =============================================================================

test_that("golden: 2D matrix index on RHS", {  ir <- ir_from(quote(for (i in 1:n) out[i] <- mat[i, j]))

  tree <- ir_tree(ir)
  expect_match(tree, "loop i")
  # mat[i, j] builds as an index node with two indices (not subscript)
  rhs <- ir$body$stmts[[1]]$rhs
  expect_equal(rhs$kind, "index")
  expect_equal(rhs$base$name, "mat")
  expect_equal(length(rhs$indices), 2)
})

# =============================================================================
# 20. Logical binop chain
# =============================================================================

test_that("golden: logical binop chain IR", {  ir <- ir_from(quote(for (i in 1:n) out[i] <- (x[i] > 0 && y[i] < 1)))

  tree <- ir_tree(ir)
  expect_match(tree, "binop &&")
  expect_match(tree, "binop >")
  expect_match(tree, "binop <")
})

# =============================================================================
# Verifier integration: builder output always passes verify
# =============================================================================

test_that("golden: all builder outputs pass verifier", {  stmts <- list(
    quote(for (i in 1:n) s <- s + x[i]),
    quote(for (i in 1:n) out[i] <- x[i] * 2.0),
    quote(for (i in 1:n) out[i] <- ifelse(x[i] > 0, x[i], -x[i])),
    quote(while (i < n) i <- i + 1),
    quote(if (n > 0) out <- x[1]),
    quote(mat[i, ] <- c(x[i], y[i])),
    quote(return(x + y))
  )
  for (stmt in stmts) {
    ir <- .mojor_ir_build_stmt(stmt)
    if (!is.null(ir)) {
      expect_true(isTRUE(.mojor_ir_verify(ir)),
                  info = paste("failed for:", deparse(stmt)))
    }
  }
})
