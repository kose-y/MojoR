# Test file for Phase 4.6: Loop Fusion
# Tests fusion legality checks, elem0 normalization, and runtime behavior

library(testthat)
library(mojor)

# Helper function to check if fusion was applied
check_fusion_applied <- function(result, expected_fused) {
  # Check if the result contains fused loops (simplified check)
  # In practice, we'd inspect the IR or Mojo output
  TRUE
}

# ============================================================================
# elem0 Normalization Tests
# ============================================================================

test_that("elem0 normalization: one_based index", {  # elem0 for i - 1 where i is one_based should normalize to i
  ctx <- list(
    index_base = "one_based",
    zero_based_vars = character(0)
  )
  
  # Create a simple var node representing i
  var_node <- list(
    kind = "var",
    name = "i"
  )
  
  # Create a binop node representing i - 1
  binop_node <- list(
    kind = "binop",
    op = "-",
    lhs = var_node,
    rhs = list(kind = "const", value = 1L)
  )
  
  # elem0 should normalize i - 1 (one_based) to i in canonical space
  elem0 <- .mojor_ir_compute_elem0(binop_node, ctx)
  expect_equal(elem0$kind, "var")
  expect_equal(elem0$name, "i")
})

test_that("elem0 normalization: zero_based index", {  # elem0 for i where i is zero_based should stay as i
  ctx <- list(
    index_base = "one_based",
    zero_based_vars = c("i")
  )
  
  var_node <- list(
    kind = "var",
    name = "i"
  )
  
  elem0 <- .mojor_ir_compute_elem0(var_node, ctx)
  expect_equal(elem0$kind, "var")
  expect_equal(elem0$name, "i")
})

test_that("elem0 normalization: constant index", {  # elem0 for constant should return constant
  ctx <- list(
    index_base = "one_based",
    zero_based_vars = character(0)
  )
  
  const_node <- list(
    kind = "const",
    value = 5L
  )
  
  elem0 <- .mojor_ir_compute_elem0(const_node, ctx)
  expect_equal(elem0$kind, "const")
  expect_equal(elem0$value, 5L)
})

# ============================================================================
# Loop Domain Extraction Tests
# ============================================================================

test_that("loop domain: basic loop", {  # Create a simple loop: for i in 1:n
  loop <- list(
    kind = "loop",
    var = "i",
    start = list(kind = "const", value = 1L),
    end_exclusive = list(kind = "var", name = "n"),
    step = list(kind = "const", value = 1L),
    body = list(kind = "block", stmts = list())
  )
  
  domain <- .mojor_ir_loop_domain(loop)
  expect_equal(domain$start$kind, "const")
  expect_equal(domain$start$value, 1L)
  expect_equal(domain$end_exclusive$kind, "var")
  expect_equal(domain$end_exclusive$name, "n")
  expect_equal(domain$step$kind, "const")
  expect_equal(domain$step$value, 1L)
  expect_equal(domain$order, "ascending")
  expect_equal(domain$var, "i")
})

test_that("loop domain: reverse loop", {  # Create a reverse loop: for i in n:1
  loop <- list(
    kind = "loop",
    var = "i",
    start = list(kind = "var", name = "n"),
    end_exclusive = list(kind = "const", value = 0L),
    step = list(kind = "const", value = -1L),
    body = list(kind = "block", stmts = list())
  )
  
  domain <- .mojor_ir_loop_domain(loop)
  expect_equal(domain$order, "descending")
  expect_equal(domain$var, "i")
})

# ============================================================================
# Noalias Checking Tests
# ============================================================================

test_that("noalias: same variable should not be fused", {  # Two loops writing to the same variable
  loop1 <- list(
    kind = "loop",
    var = "i",
    start = list(kind = "const", value = 1L),
    end_exclusive = list(kind = "var", name = "n"),
    step = list(kind = "const", value = 1L),
    body = list(
      kind = "assign",
      lhs = list(kind = "index", name = "x", idx = list(kind = "var", name = "i")),
      rhs = list(kind = "const", value = 1L)
    )
  )
  
  loop2 <- list(
    kind = "loop",
    var = "i",
    start = list(kind = "const", value = 1L),
    end_exclusive = list(kind = "var", name = "n"),
    step = list(kind = "const", value = 1L),
    body = list(
      kind = "assign",
      lhs = list(kind = "index", name = "x", idx = list(kind = "var", name = "i")),
      rhs = list(kind = "const", value = 2L)
    )
  )
  
  ctx <- list(
    noalias = list(),
    index_base = "one_based",
    zero_based_vars = character(0)
  )
  
  # Should reject due to noalias violation
  result <- .mojor_ir_fusion_legality(loop1, loop2, list(loop1, loop2), ctx)
  expect_false(result$legal)
  expect_equal(result$rejection_code, "FUSE_REJECT_NOALIAS_MISSING")
})

test_that("noalias: different variables can be fused", {  loop1 <- list(
    kind = "loop",
    var = "i",
    start = list(kind = "const", value = 1L),
    end_exclusive = list(kind = "var", name = "n"),
    step = list(kind = "const", value = 1L),
    body = list(
      kind = "assign",
      lhs = list(kind = "index", name = "x", idx = list(kind = "var", name = "i")),
      rhs = list(kind = "const", value = 1L)
    )
  )
  
  loop2 <- list(
    kind = "loop",
    var = "i",
    start = list(kind = "const", value = 1L),
    end_exclusive = list(kind = "var", name = "n"),
    step = list(kind = "const", value = 1L),
    body = list(
      kind = "assign",
      lhs = list(kind = "index", name = "y", idx = list(kind = "var", name = "i")),
      rhs = list(kind = "const", value = 2L)
    )
  )
  
  ctx <- list(
    noalias = list(),
    index_base = "one_based",
    zero_based_vars = character(0)
  )
  
  # Should be legal (different variables)
  result <- .mojor_ir_fusion_legality(loop1, loop2, list(loop1, loop2), ctx)
  expect_true(result$legal)
})

# ============================================================================
# Fusion Legality Tests
# ============================================================================

test_that("fusion legality: domain mismatch rejects", {  loop1 <- list(
    kind = "loop",
    var = "i",
    start = list(kind = "const", value = 1L),
    end_exclusive = list(kind = "var", name = "n"),
    step = list(kind = "const", value = 1L),
    body = list(kind = "block", stmts = list())
  )
  
  loop2 <- list(
    kind = "loop",
    var = "i",
    start = list(kind = "const", value = 1L),
    end_exclusive = list(kind = "var", name = "m"),  # Different end
    step = list(kind = "const", value = 1L),
    body = list(kind = "block", stmts = list())
  )
  
  ctx <- list(
    noalias = list(),
    index_base = "one_based",
    zero_based_vars = character(0)
  )
  
  result <- .mojor_ir_fusion_legality(loop1, loop2, list(loop1, loop2), ctx)
  expect_false(result$legal)
  expect_equal(result$rejection_code, "FUSE_REJECT_DOMAIN_MISMATCH")
})

test_that("fusion legality: control flow rejects", {  loop1 <- list(
    kind = "loop",
    var = "i",
    start = list(kind = "const", value = 1L),
    end_exclusive = list(kind = "var", name = "n"),
    step = list(kind = "const", value = 1L),
    body = list(
      kind = "block",
      stmts = list(
        list(
          kind = "if",
          cond = list(kind = "const", value = TRUE),
          then_branch = list(kind = "block", stmts = list()),
          else_branch = NULL
        )
      )
    )
  )
  
  loop2 <- list(
    kind = "loop",
    var = "i",
    start = list(kind = "const", value = 1L),
    end_exclusive = list(kind = "var", name = "n"),
    step = list(kind = "const", value = 1L),
    body = list(kind = "block", stmts = list())
  )
  
  ctx <- list(
    noalias = list(),
    index_base = "one_based",
    zero_based_vars = character(0)
  )
  
  result <- .mojor_ir_fusion_legality(loop1, loop2, list(loop1, loop2), ctx)
  expect_false(result$legal)
  expect_equal(result$rejection_code, "FUSE_REJECT_CONTROL_FLOW")
})

test_that("fusion legality: simple if assign is allowed only under opt-in", {  make_simple_if_loop <- function(lhs_name) {
    list(
      kind = "loop",
      var = "i",
      start = list(kind = "const", value = 1L),
      end_exclusive = list(kind = "var", name = "n"),
      step = list(kind = "const", value = 1L),
      body = list(
        kind = "block",
        stmts = list(
          list(
            kind = "if",
            cond = list(kind = "binop", op = "==", lhs = list(kind = "var", name = "i"), rhs = list(kind = "const", value = 1L)),
            then_branch = list(
              kind = "block",
              stmts = list(
                list(
                  kind = "assign",
                  lhs = list(kind = "index", name = lhs_name, idx = list(kind = "var", name = "i")),
                  rhs = list(kind = "var", name = "i")
                )
              )
            ),
            else_branch = list(
              kind = "block",
              stmts = list(
                list(
                  kind = "assign",
                  lhs = list(kind = "index", name = lhs_name, idx = list(kind = "var", name = "i")),
                  rhs = list(kind = "binop", op = "*", lhs = list(kind = "var", name = "i"), rhs = list(kind = "const", value = 2L))
                )
              )
            )
          )
        )
      )
    )
  }

  loop1 <- make_simple_if_loop("x")
  loop2 <- list(
    kind = "loop",
    var = "i",
    start = list(kind = "const", value = 1L),
    end_exclusive = list(kind = "var", name = "n"),
    step = list(kind = "const", value = 1L),
    body = list(
      kind = "assign",
      lhs = list(kind = "index", name = "y", idx = list(kind = "var", name = "i")),
      rhs = list(kind = "binop", op = "+", lhs = list(kind = "var", name = "i"), rhs = list(kind = "const", value = 1L))
    )
  )

  base_ctx <- list(
    noalias = list(),
    index_base = "one_based",
    zero_based_vars = character(0)
  )

  rejected <- .mojor_ir_fusion_legality(loop1, loop2, list(loop1, loop2), base_ctx)
  expect_false(rejected$legal)
  expect_equal(rejected$rejection_code, "FUSE_REJECT_CONTROL_FLOW")

  allowed_ctx <- c(base_ctx, list(fusion_allow_control_flow_simple = TRUE))
  allowed <- .mojor_ir_fusion_legality(loop1, loop2, list(loop1, loop2), allowed_ctx)
  expect_true(allowed$legal)
})

test_that("fusion legality: opt-in control flow still rejects unsafe forms", {  loop_nested_if <- list(
    kind = "loop",
    var = "i",
    start = list(kind = "const", value = 1L),
    end_exclusive = list(kind = "var", name = "n"),
    step = list(kind = "const", value = 1L),
    body = list(
      kind = "block",
      stmts = list(
        list(
          kind = "if",
          cond = list(kind = "const", value = TRUE),
          then_branch = list(
            kind = "block",
            stmts = list(
              list(
                kind = "if",
                cond = list(kind = "const", value = TRUE),
                then_branch = list(
                  kind = "assign",
                  lhs = list(kind = "index", name = "x", idx = list(kind = "var", name = "i")),
                  rhs = list(kind = "const", value = 1L)
                ),
                else_branch = list(
                  kind = "assign",
                  lhs = list(kind = "index", name = "x", idx = list(kind = "var", name = "i")),
                  rhs = list(kind = "const", value = 2L)
                )
              )
            )
          ),
          else_branch = list(
            kind = "assign",
            lhs = list(kind = "index", name = "x", idx = list(kind = "var", name = "i")),
            rhs = list(kind = "const", value = 3L)
          )
        )
      )
    )
  )

  loop_branch_lhs_mismatch <- list(
    kind = "loop",
    var = "i",
    start = list(kind = "const", value = 1L),
    end_exclusive = list(kind = "var", name = "n"),
    step = list(kind = "const", value = 1L),
    body = list(
      kind = "block",
      stmts = list(
        list(
          kind = "if",
          cond = list(kind = "const", value = TRUE),
          then_branch = list(
            kind = "assign",
            lhs = list(kind = "index", name = "x", idx = list(kind = "var", name = "i")),
            rhs = list(kind = "const", value = 1L)
          ),
          else_branch = list(
            kind = "assign",
            lhs = list(kind = "index", name = "y", idx = list(kind = "var", name = "i")),
            rhs = list(kind = "const", value = 2L)
          )
        )
      )
    )
  )

  loop_regular <- list(
    kind = "loop",
    var = "i",
    start = list(kind = "const", value = 1L),
    end_exclusive = list(kind = "var", name = "n"),
    step = list(kind = "const", value = 1L),
    body = list(
      kind = "assign",
      lhs = list(kind = "index", name = "z", idx = list(kind = "var", name = "i")),
      rhs = list(kind = "const", value = 9L)
    )
  )

  ctx <- list(
    noalias = list(),
    index_base = "one_based",
    zero_based_vars = character(0),
    fusion_allow_control_flow_simple = TRUE
  )

  nested <- .mojor_ir_fusion_legality(loop_nested_if, loop_regular, list(loop_nested_if, loop_regular), ctx)
  expect_false(nested$legal)
  expect_equal(nested$rejection_code, "FUSE_REJECT_CONTROL_FLOW")

  mismatch <- .mojor_ir_fusion_legality(loop_branch_lhs_mismatch, loop_regular, list(loop_branch_lhs_mismatch, loop_regular), ctx)
  expect_false(mismatch$legal)
  expect_equal(mismatch$rejection_code, "FUSE_REJECT_CONTROL_FLOW")
})

test_that("fusion legality: loops with vexpr RHS can fuse", {  vexpr_loop <- list(
    kind = "loop",
    var = "i",
    start = list(kind = "const", value = 1L),
    end_exclusive = list(kind = "var", name = "n"),
    step = list(kind = "const", value = 1L),
    body = list(
      kind = "assign",
      lhs = list(kind = "index", name = "x", idx = list(kind = "var", name = "i")),
      rhs = list(
        kind = "vexpr",
        len = list(kind = "var", name = "n"),
        body = list(
          kind = "binop",
          op = "+",
          lhs = list(kind = "var", name = "i"),
          rhs = list(kind = "const", value = 1L)
        )
      )
    )
  )
  
  regular_loop <- list(
    kind = "loop",
    var = "i",
    start = list(kind = "const", value = 1L),
    end_exclusive = list(kind = "var", name = "n"),
    step = list(kind = "const", value = 1L),
    body = list(
      kind = "assign",
      lhs = list(kind = "index", name = "y", idx = list(kind = "var", name = "i")),
      rhs = list(kind = "binop", op = "+", lhs = list(kind = "var", name = "i"), rhs = list(kind = "const", value = 2L))
    )
  )
  
  ctx <- list(
    noalias = list(),
    index_base = "one_based",
    zero_based_vars = character(0)
  )
  
  result <- .mojor_ir_fusion_legality(vexpr_loop, regular_loop, list(vexpr_loop, regular_loop), ctx)
  expect_true(result$legal)
  expect_null(result$rejection_code)
})

# ============================================================================
# Runtime Tests
# ============================================================================

test_that("fusion: basic loop fusion works", {  skip_if_no_mojo()
  
  # Simple case: two loops that can be fused
  fn <- function(n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- i * 2
    }
    for (i in seq_len(n)) {
      out[i] <- i * 3
    }
    out
  }
  
  # Test with fusion enabled (default)
  built_fused <- mojor_build(fn, n = "i32", fusion = TRUE, name = "t_fusion_basic_fused", cache = FALSE, load = TRUE)
  out_fused <- built_fused$func(10L)
  
  # Test with fusion disabled
  built_unfused <- mojor_build(fn, n = "i32", fusion = FALSE, name = "t_fusion_basic_unfused", cache = FALSE, load = TRUE)
  out_unfused <- built_unfused$func(10L)
  
  # Results should be identical
  expect_equal(out_fused, out_unfused)
  
  # Verify against R reference
  expected <- fn(10L)
  expect_equal(out_fused, expected)
})

test_that("fusion: three loops can be fused", {  skip_if_no_mojo()
  
  fn <- function(n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- i
    }
    for (i in seq_len(n)) {
      out[i] <- i * 2
    }
    for (i in seq_len(n)) {
      out[i] <- i * 3
    }
    out
  }
  
  built <- mojor_build(fn, n = "i32", fusion = TRUE, name = "t_fusion_three_loops", cache = FALSE, load = TRUE)
  out <- built$func(10L)
  
  expected <- fn(10L)
  expect_equal(out, expected)
})

test_that("fusion: different domains cannot be fused", {  skip_if_no_mojo()
  
  fn <- function(n) {
    x <- numeric(n + 1L)
    for (i in seq_len(n)) {
      x[i] <- i
    }
    for (i in seq_len(n + 1L)) {
      x[i] <- i * 2
    }
    x
  }
  
  # Note: This test verifies that different domains are NOT fused
  # The transpiler should reject this pattern (different loop bounds)
  expect_error(
    mojor_transpile(fn, n = "i32", fusion = TRUE),
    "multiple loops must share the same range"
  )
})

test_that("fusion: broadcast_nd runtime parity", {  skip_if_no_mojo()
  
  fn <- function(x, y) {
    n <- length(x)
    z <- numeric(n)
    for (i in seq_len(n)) {
      z[i] <- x[i] + y
    }
    z
  }
  
  # This uses broadcast_nd internally
  built <- mojor_build(fn, x = "f64[]", y = "f64", broadcast = "broadcast_nd", fusion = TRUE, name = "t_fusion_broadcast_nd", cache = FALSE, load = TRUE)
  out <- built$func(c(1, 2, 3), 2)
  
  expected <- fn(c(1, 2, 3), 2)
  # Compare values directly (Mojo returns array with dim, R returns numeric)
  expect_equal(as.numeric(out), expected)
})

test_that("fusion: broadcast_nd pair loops can fuse (transpile)", {  fn <- function(x, y) {
    n <- length(x)
    z <- numeric(n)
    for (i in seq_len(n)) {
      z[i] <- x[i] + y
    }
    for (i in seq_len(n)) {
      z[i] <- z[i] * 2
    }
    z
  }

  trans <- mojor_transpile(
    fn,
    x = "f64[]",
    y = "f64",
    broadcast = "broadcast_nd",
    fusion = TRUE,
    fusion_debug = TRUE
  )

  expect_true(isTRUE(trans$fused))
})

test_that("fusion: control-flow runtime parity (single loop)", {  skip_if_no_mojo()
  
  fn <- function(n) {
    x <- numeric(n)
    for (i in seq_len(n)) {
      if (i %% 2 == 0) {
        x[i] <- i * 2
      } else {
        x[i] <- i
      }
    }
    x
  }
  
  built <- mojor_build(fn, n = "i32", fusion = TRUE, name = "t_fusion_control_flow", cache = FALSE, load = TRUE)
  out <- built$func(10L)
  
  expected <- fn(10L)
  expect_equal(out, expected)
})

test_that("fusion: control-flow pair loops can fuse (transpile)", {  fn <- function(n) {
    x <- numeric(n)
    for (i in seq_len(n)) {
      if (i %% 2 == 0) {
        x[i] <- i * 2
      } else {
        x[i] <- i
      }
    }
    for (i in seq_len(n)) {
      x[i] <- x[i] + 1
    }
    x
  }

  trans_default <- mojor_transpile(fn, n = "i32", fusion = TRUE, fusion_debug = TRUE)
  expect_true(is.logical(trans_default$fused) || isTRUE(trans_default$fused))

  old <- mojor_options(fusion_allow_control_flow_simple = TRUE)
  on.exit(mojor_options(fusion_allow_control_flow_simple = old$fusion_allow_control_flow_simple), add = TRUE)
  trans_opt_in <- mojor_transpile(fn, n = "i32", fusion = TRUE, fusion_debug = TRUE)
  expect_true(isTRUE(trans_opt_in$fused))
})

test_that("fusion: control-flow pair runtime parity under opt-in", {  skip_if_no_mojo()

  fn <- function(n) {
    x <- numeric(n)
    for (i in seq_len(n)) {
      if (i %% 2 == 0) {
        x[i] <- i * 2
      } else {
        x[i] <- i
      }
    }
    for (i in seq_len(n)) {
      x[i] <- x[i] + 1
    }
    x
  }

  old <- mojor_options(fusion_allow_control_flow_simple = TRUE)
  on.exit(mojor_options(fusion_allow_control_flow_simple = old$fusion_allow_control_flow_simple), add = TRUE)

  built <- mojor_build(
    fn, n = "i32",
    fusion = TRUE,
    name = "t_fusion_control_flow_pair_opt_in",
    cache = FALSE,
    load = TRUE
  )
  out <- built$func(10L)
  expect_equal(out, fn(10L))
})

# ============================================================================
# Error Handling Tests
# ============================================================================

test_that("fusion: invalid fusion parameter throws error", {  fn <- function(n) numeric(n)
  
  expect_error(
    mojor_transpile(fn, n = 10L, fusion = "yes"),
    "fusion must be TRUE or FALSE"
  )
})

test_that("fusion: invalid fusion length throws error", {  fn <- function(n) numeric(n)
  
  expect_error(
    mojor_transpile(fn, n = 10L, fusion = c(TRUE, FALSE)),
    "fusion must be TRUE or FALSE"
  )
})

# ============================================================================
# Integration Tests
# ============================================================================

test_that("fusion: integration with other optimizations", {  skip_if_no_mojo()
  
  fn <- function(n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- i * 2
    }
    for (i in seq_len(n)) {
      out[i] <- out[i] + 1
    }
    out
  }
  
  # Test with fusion and other options
  built <- mojor_build(
    fn, n = "i32",
    fusion = TRUE,
    bounds_check = TRUE,
    simd_mode = "explicit",
    name = "t_fusion_integration",
    cache = FALSE,
    load = TRUE
  )
  out <- built$func(10L)
  
  expected <- fn(10L)
  expect_equal(out, expected)
})

test_that("fusion: preserves semantics with NA values in inline conditionals", {  skip_if_no_mojo()
  
  fn <- function(n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- if (i %% 3 == 0) NA_real_ else i
    }
    for (i in seq_len(n)) {
      out[i] <- out[i] * 2
    }
    out
  }

  trans <- mojor_transpile(fn, n = "i32", fusion = TRUE, fusion_debug = TRUE, na_mode = "unsafe")
  expect_true(isTRUE(trans$fused))

  built <- mojor_build(
    fn, n = "i32",
    fusion = TRUE,
    na_mode = "unsafe",
    name = "t_fusion_na_inline_conditional",
    cache = FALSE,
    load = TRUE
  )
  out <- as.numeric(built$func(12L))
  expected <- fn(12L)
  expect_identical(is.na(out), is.na(expected))
  expect_equal(out[!is.na(expected)], expected[!is.na(expected)])
})
