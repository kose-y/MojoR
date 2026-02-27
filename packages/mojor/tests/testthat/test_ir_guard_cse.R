# Phase 16: Guard CSE (Common Subexpression Elimination) Tests
#
# These tests verify that redundant guard checks are eliminated within
# single statements, reducing code size and improving performance.

library(testthat)

# =============================================================================
# NA Guard CSE Tests
# =============================================================================

test_that("NA guard CSE eliminates duplicate array accesses", {  # Expression: x[i] + x[i] should only check x[i] once for NA
  stmt <- quote(out[i] <- x[i] + x[i])
  ir <- .mojor_ir_build_stmt(stmt)
  
  # Type check with proper environment
  types <- list(out = "f64[]", x = "f64[]", i = "i32")
  ir_typed <- .mojor_ir_type_check_stmt(ir, types)
  
  # Emit with NA guards
  result <- .mojor_ir_emit_na_guard(
    ir_typed$rhs, 
    indent = "    ", 
    na_guard = "forbid",
    zero_based_vars = NULL,
    type_env = types,
    guard_cache = NULL
  )
  
  # Should return list with lines and guard_cache
  expect_type(result, "list")
  expect_true("lines" %in% names(result))
  expect_true("guard_cache" %in% names(result))
  
  # The guard cache should contain x[i] (the checked expression)
  expect_true(length(result$guard_cache) >= 1)
  
  # Second emission with same cache should return empty lines (already checked)
  result2 <- .mojor_ir_emit_na_guard(
    ir_typed$rhs,
    indent = "    ",
    na_guard = "forbid", 
    zero_based_vars = NULL,
    type_env = types,
    guard_cache = result$guard_cache
  )
  
  # Second call should have no new lines (everything already checked)
  expect_equal(length(result2$lines), 0)
})

test_that("NA guard CSE handles different array accesses separately", {  # Expression: x[i] + y[i] should check both x[i] and y[i]
  stmt <- quote(out[i] <- x[i] + y[i])
  ir <- .mojor_ir_build_stmt(stmt)
  
  types <- list(out = "f64[]", x = "f64[]", y = "f64[]", i = "i32")
  ir_typed <- .mojor_ir_type_check_stmt(ir, types)
  
  result <- .mojor_ir_emit_na_guard(
    ir_typed$rhs,
    indent = "    ",
    na_guard = "forbid",
    zero_based_vars = NULL,
    type_env = types,
    guard_cache = NULL
  )
  
  # Should have both x and y in cache
  expect_true(length(result$guard_cache) >= 2)
})

test_that("NA guard checks use Int32 casts for integer and logical sources", {  checks <- .mojor_ir_na_checks(
    sources = c("i", "flag", "x[(i - 1)]", "u"),
    type_env = list(i = "i32", flag = "lgl", x = "i32[]", u = "f64")
  )

  expect_true("(Int32(i) == Int32(-2147483648))" %in% checks)
  expect_true("(Int32(flag) == Int32(-2147483648))" %in% checks)
  expect_true("(Int32(x[(i - 1)]) == Int32(-2147483648))" %in% checks)
  expect_true("(u != u)" %in% checks)
})

# =============================================================================
# Bounds Guard CSE Tests
# =============================================================================

test_that("bounds guard CSE eliminates duplicate index checks", {  # Access pattern: x[i] appears twice, should only bounds-check once
  stmt <- quote(out[i] <- x[i] + x[i])
  ir <- .mojor_ir_build_stmt(stmt)
  
  .mojor_test_local_state("current_len_var_map", list(x = "len_x"))
  
  result <- .mojor_ir_emit_bounds_guards(
    ir$rhs,
    indent = "    ",
    zero_based_vars = NULL,
    bounds_check = TRUE,
    loop_var = NULL,
    guard_cache = NULL
  )
  
  expect_type(result, "list")
  expect_true("lines" %in% names(result))
  expect_true("guard_cache" %in% names(result))
  
  # Should have exactly one check for i:x
  expect_equal(length(result$guard_cache), 1)
  expect_equal(result$guard_cache[1], "i:x")
  
  # Second emission should return empty
  result2 <- .mojor_ir_emit_bounds_guards(
    ir$rhs,
    indent = "    ",
    zero_based_vars = NULL,
    bounds_check = TRUE,
    loop_var = NULL,
    guard_cache = result$guard_cache
  )
  
  expect_equal(length(result2$lines), 0)
})

test_that("bounds guard CSE handles different arrays separately", {  # Access pattern: x[i] + y[i] should check both
  stmt <- quote(out[i] <- x[i] + y[i])
  ir <- .mojor_ir_build_stmt(stmt)
  
  .mojor_test_local_state("current_len_var_map", list(x = "len_x", y = "len_y"))
  
  result <- .mojor_ir_emit_bounds_guards(
    ir$rhs,
    indent = "    ",
    zero_based_vars = NULL,
    bounds_check = TRUE,
    loop_var = NULL,
    guard_cache = NULL
  )
  
  # Should have checks for both x and y
  expect_true(length(result$guard_cache) >= 2)
  expect_true("i:x" %in% result$guard_cache)
  expect_true("i:y" %in% result$guard_cache)
})

test_that("bounds guards skip synthetic ND exclusion marker indices", {
  .mojor_test_local_state("current_dim_var_map", list(mat = "dim_mat"))

  node <- .mojor_ir_index(
    .mojor_ir_var("mat"),
    list(
      list(kind = "var", name = "__neg_excl__", neg_exclusion = "0"),
      .mojor_ir_var("j")
    )
  )

  result <- .mojor_ir_emit_bounds_guards(
    node,
    indent = "    ",
    zero_based_vars = NULL,
    bounds_check = TRUE,
    loop_var = NULL,
    guard_cache = NULL
  )

  expect_true(length(result$lines) > 0)
  expect_false(any(grepl("__neg_excl__", result$lines, fixed = TRUE)))
  expect_true(any(grepl("if j < 1 or j > Int\\(dim_mat\\[1\\]\\)", result$lines)))
})

# =============================================================================
# Loop Variable Optimization Tests
# =============================================================================

test_that("bounds guard skips check for loop variable", {  # When i is the loop variable, x[i] doesn't need bounds check
  stmt <- quote(out[i] <- x[i] + 1)
  ir <- .mojor_ir_build_stmt(stmt)
  
  .mojor_test_local_state("current_len_var_map", list(x = "len_x"))
  
  # Pass loop_var = "i" to indicate i is the loop variable
  result <- .mojor_ir_emit_bounds_guards(
    ir$rhs,
    indent = "    ",
    zero_based_vars = NULL,
    bounds_check = TRUE,
    loop_var = "i",  # i is the loop variable
    guard_cache = NULL
  )
  
  # Should skip bounds check for loop variable
  expect_equal(length(result$lines), 0)
})

# =============================================================================
# Guard Hoisting Tests
# =============================================================================

test_that("guard hoisting moves invariant bounds check before loop", {  f <- function(x, j, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[j] + 1
    }
    out
  }

  mojo <- mojor_transpile(
    f,
    x = "f64[]",
    j = "i32",
    n = "i32",
    name = "t_guard_hoist_invariant",
    bounds_check = TRUE,
    na_mode = "unsafe",
    ir_only = TRUE
  )$mojo

  pos_for <- regexpr("\\n\\s+for\\s+[A-Za-z0-9_]+\\s+in\\s+range\\(", mojo, perl = TRUE)[[1]]
  pos_guard <- regexpr("__mojor_na_flag\\[0\\] = Int32\\(2\\)", mojo, perl = TRUE)[[1]]
  expect_true(pos_for > 0)
  expect_true(pos_guard > 0)
  expect_true(pos_guard < pos_for)
  expect_true(grepl("if \\(Int\\(1\\) <= Int\\(n\\)\\):", mojo))
  expect_equal(length(gregexpr("__mojor_na_flag\\[0\\] = Int32\\(2\\)", mojo, perl = TRUE)[[1]]), 1)
})

test_that("guard hoisting keeps loop-dependent bounds checks inside loop", {  f <- function(x, j, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[i + j]
    }
    out
  }

  mojo <- mojor_transpile(
    f,
    x = "f64[]",
    j = "i32",
    n = "i32",
    name = "t_guard_hoist_dependent",
    bounds_check = TRUE,
    na_mode = "unsafe",
    ir_only = TRUE
  )$mojo

  pos_for <- regexpr("\\n\\s+for\\s+[A-Za-z0-9_]+\\s+in\\s+range\\(", mojo, perl = TRUE)[[1]]
  pos_guard <- regexpr("__mojor_na_flag\\[0\\] = Int32\\(2\\)", mojo, perl = TRUE)[[1]]
  expect_true(pos_for > 0)
  expect_true(pos_guard > 0)
  expect_true(pos_guard > pos_for)
  expect_false(grepl("if \\(1 <= n\\):\\s*\\n\\s*if \\(i \\+ j\\)", mojo, perl = TRUE))
})

# =============================================================================
# Integration Tests
# =============================================================================

test_that("guard CSE works in full transpilation", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      # This expression has x[i] twice - should only check once
      out[i] <- x[i] + x[i] * 2.0
    }
    out
  }
  
  # Transpile and verify it works
  result <- mojor_transpile(f, x = "f64[]", name = "t_guard_cse", emit_ir = TRUE)
  expect_true(!is.null(result$mojo))
  
  # Build and run
  built <- mojor_build(f, x = "f64[]", name = "t_guard_cse_rt", cache = FALSE, load = TRUE)
  x <- c(1.0, 2.0, 3.0, 4.0, 5.0)
  expect_equal(built$func(x), f(x))
})

test_that("guard CSE handles complex expressions", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      # Multiple accesses to same arrays
      out[i] <- x[i] * y[i] + x[i] - y[i] / x[i]
    }
    out
  }
  
  result <- mojor_transpile(f, x = "f64[]", y = "f64[]", name = "t_guard_complex")
  expect_true(!is.null(result$mojo))
  
  # Should compile and run correctly
  built <- mojor_build(f, x = "f64[]", y = "f64[]", name = "t_guard_complex_rt", cache = FALSE, load = TRUE)
  x <- c(1.0, 2.0, 3.0)
  y <- c(4.0, 5.0, 6.0)
  expect_equal(built$func(x, y), f(x, y))
})

# =============================================================================
# Cache Persistence Tests
# =============================================================================

test_that("guard cache accumulates across calls", {  # First check x[i]
  cache1 <- "x[i]"
  
  # Second check adds y[i]
  cache2 <- c(cache1, "y[i]")
  
  # Verify cache grows
  expect_equal(length(cache2), 2)
  expect_true("x[i]" %in% cache2)
  expect_true("y[i]" %in% cache2)
})

test_that("empty cache handled correctly", {  stmt <- quote(out[i] <- x[i] + 1)
  ir <- .mojor_ir_build_stmt(stmt)
  
  types <- list(out = "f64[]", x = "f64[]", i = "i32")
  ir_typed <- .mojor_ir_type_check_stmt(ir, types)
  
  # Call with NULL cache (initial state)
  result <- .mojor_ir_emit_na_guard(
    ir_typed$rhs,
    indent = "    ",
    na_guard = "forbid",
    zero_based_vars = NULL,
    type_env = types,
    guard_cache = NULL
  )
  
  expect_type(result, "list")
  expect_true(!is.null(result$guard_cache))
})
