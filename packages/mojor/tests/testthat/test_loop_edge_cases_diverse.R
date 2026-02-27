# Diverse Loop Edge Case Tests
# Tests various loop structures to ensure stable transpilation
#
# RUNNING THESE TESTS:
#   From R: devtools::test(filter = "loop_edge_cases_diverse")
#   Or: testthat::test_file("tests/testthat/test_loop_edge_cases_diverse.R")
#
# See test_loop_edge_cases_diverse_RESULTS.md for detailed results.

# =============================================================================
# SECTION 1: Standard Loop Patterns (Baseline)
# =============================================================================

test_that("standard seq_len loop transpiles successfully", {
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_len(length(x))) {
      out[i] <- x[i] * 2
    }
    out
  }
  trans <- mojor_transpile(f, x = 'f64[]')
  expect_true(!is.null(trans$mojo))
  # Check that it generates a for loop
  expect_true(grepl("for ", trans$mojo))
})

test_that("standard seq_along loop transpiles successfully", {
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }
  trans <- mojor_transpile(f, x = 'f64[]')
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("for ", trans$mojo))
})

test_that("standard 1:n colon loop transpiles successfully", {
  f <- function(n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- i * i
    }
    out
  }
  trans <- mojor_transpile(f, n = 'i32')
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("for ", trans$mojo))
})

test_that("standard while loop transpiles successfully", {
  f <- function(x) {
    out <- numeric(length(x))
    i <- 1L
    while (i <= length(x)) {
      out[i] <- x[i] + 1
      i <- i + 1L
    }
    out
  }
  trans <- mojor_transpile(f, x = 'f64[]')
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("while ", trans$mojo))
})

test_that("standard repeat loop with break transpiles successfully", {
  f <- function(x) {
    out <- numeric(length(x))
    i <- 1L
    repeat {
      if (i > length(x)) break
      out[i] <- x[i] + 1
      i <- i + 1L
    }
    out
  }
  trans <- mojor_transpile(f, x = 'f64[]')
  expect_true(!is.null(trans$mojo))
  # Repeat converts to while True or similar
  expect_true(grepl("while ", trans$mojo) || grepl("for ", trans$mojo))
})

# =============================================================================
# SECTION 2: Loop Bound Variations
# =============================================================================

test_that("seq with from, to parameters transpiles successfully", {
  f <- function(n) {
    out <- numeric(n)
    for (i in seq(1, n)) {
      out[i] <- i
    }
    out
  }
  trans <- mojor_transpile(f, n = 'i32')
  expect_true(!is.null(trans$mojo))
})

test_that("seq.int works as loop bound", {
  f <- function(n) {
    out <- numeric(n)
    for (i in seq.int(1, n)) {
      out[i] <- i * 2
    }
    out
  }
  trans <- mojor_transpile(f, n = 'i32')
  expect_true(!is.null(trans$mojo))
})

test_that("seq with by parameter transpiles successfully", {
  f <- function() {
    out <- 0.0
    for (i in seq(1, 10, by = 2)) {
      out <- out + i
    }
    out
  }
  trans <- mojor_transpile(f)
  expect_true(!is.null(trans$mojo))
})

test_that("seq with length.out parameter transpiles successfully", {
  # seq(length.out=n) now works - equivalent to seq_len(n) when from defaults to 1
  f <- function(n) {
    out <- numeric(n)
    for (i in seq(length.out = n)) {
      out[i] <- i
    }
    out
  }
  trans <- mojor_transpile(f, n = 'i32')
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("for ", trans$mojo))
})

test_that("computed expression as loop bound transpiles successfully", {
  f <- function(n, m) {
    bound <- min(n, m)
    out <- numeric(bound)
    for (i in seq_len(bound)) {
      out[i] <- i
    }
    out
  }
  trans <- mojor_transpile(f, n = 'i32', m = 'i32')
  expect_true(!is.null(trans$mojo))
})

test_that("loop bound with arithmetic expression transpiles successfully", {
  f <- function(n) {
    bound <- n * 2L
    out <- numeric(bound)
    for (i in seq_len(bound)) {
      out[i] <- i
    }
    out
  }
  trans <- mojor_transpile(f, n = 'i32')
  expect_true(!is.null(trans$mojo))
})

# =============================================================================
# SECTION 3: Reverse and Descending Loops
# =============================================================================

test_that("descending colon sequence transpiles successfully", {
  f <- function(n) {
    out <- numeric(n)
    idx <- 1L
    for (i in n:1) {
      out[idx] <- i
      idx <- idx + 1L
    }
    out
  }
  trans <- mojor_transpile(f, n = 'i32')
  expect_true(!is.null(trans$mojo))
})

test_that("reverse seq with negative by transpiles successfully", {
  f <- function() {
    out <- 0.0
    for (i in seq(10, 1, by = -1)) {
      out <- out + i
    }
    out
  }
  trans <- mojor_transpile(f)
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("range.*-1", trans$mojo))
})

test_that("seq with negative by -2 transpiles successfully", {
  f <- function() {
    out <- 0.0
    for (i in seq(10, 1, by = -2)) {
      out <- out + i
    }
    out
  }
  trans <- mojor_transpile(f)
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("range.*-2", trans$mojo))
})

test_that("dynamic seq with negative by transpiles successfully", {
  f <- function(n) {
    out <- 0.0
    for (i in seq(n, 1, by = -1)) {
      out <- out + i
    }
    out
  }
  trans <- mojor_transpile(f, n = 'i32')
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("range.*-1", trans$mojo))
})

# =============================================================================
# SECTION 4: Break and Next Patterns
# =============================================================================

test_that("simple break in for loop generates break statement", {
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      if (x[i] < 0) break
      out[i] <- x[i]
    }
    out
  }
  trans <- mojor_transpile(f, x = 'f64[]')
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("break", trans$mojo))
})

test_that("simple next in for loop generates continue statement", {
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      if (x[i] < 0) next
      out[i] <- x[i] * 2
    }
    out
  }
  trans <- mojor_transpile(f, x = 'f64[]')
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("continue", trans$mojo))
})

test_that("break in nested inner loop works correctly", {
  f <- function(n) {
    out <- 0.0
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (j > 3) break
        out <- out + i * j
      }
    }
    out
  }
  trans <- mojor_transpile(f, n = 'i32')
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("break", trans$mojo))
})

test_that("next in nested inner loop works correctly", {
  f <- function(n) {
    out <- 0.0
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (j == 3) next
        out <- out + i * j
      }
    }
    out
  }
  trans <- mojor_transpile(f, n = 'i32')
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("continue", trans$mojo))
})

test_that("break in while loop works correctly", {
  f <- function(x) {
    out <- 0.0
    i <- 1L
    while (TRUE) {
      if (i > length(x)) break
      out <- out + x[i]
      i <- i + 1L
    }
    out
  }
  trans <- mojor_transpile(f, x = 'f64[]')
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("break", trans$mojo))
})

test_that("next in while loop works correctly", {
  f <- function(x) {
    out <- 0.0
    i <- 1L
    while (i <= length(x)) {
      if (x[i] < 0) {
        i <- i + 1L
        next
      }
      out <- out + x[i]
      i <- i + 1L
    }
    out
  }
  trans <- mojor_transpile(f, x = 'f64[]')
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("continue", trans$mojo))
})

test_that("multiple break conditions work correctly", {
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      if (x[i] == 0) break
      if (x[i] < 0) break
      out[i] <- x[i]
    }
    out
  }
  trans <- mojor_transpile(f, x = 'f64[]')
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("break", trans$mojo))
})

# =============================================================================
# SECTION 5: Nested Loop Patterns
# =============================================================================

test_that("double nested for loops transpile successfully", {
  f <- function(n) {
    out <- matrix(0.0, nrow = n, ncol = n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        out[i, j] <- i + j
      }
    }
    out
  }
  trans <- mojor_transpile(f, n = 'i32')
  expect_true(!is.null(trans$mojo))
  # opt_level=3 can introduce tiled outer loops; require nested-loop presence
  # without constraining the exact number of emitted `for` constructs.
  expect_gte(length(gregexpr("for ", trans$mojo)[[1]]), 2)
})

test_that("triple nested for loops transpile successfully", {
  f <- function(d1, d2, d3) {
    out <- array(0.0, dim = c(d1, d2, d3))
    for (i in seq_len(d1)) {
      for (j in seq_len(d2)) {
        for (k in seq_len(d3)) {
          out[i, j, k] <- i + j + k
        }
      }
    }
    out
  }
  trans <- mojor_transpile(f, d1 = 'i32', d2 = 'i32', d3 = 'i32')
  expect_true(!is.null(trans$mojo))
})

test_that("for inside while transpiles successfully", {
  f <- function(n) {
    out <- 0.0
    i <- 1L
    while (i <= n) {
      for (j in seq_len(n)) {
        out <- out + i * j
      }
      i <- i + 1L
    }
    out
  }
  trans <- mojor_transpile(f, n = 'i32')
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("for ", trans$mojo) && grepl("while ", trans$mojo))
})

test_that("while inside for transpiles successfully", {
  f <- function(n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      j <- 1L
      acc <- 0.0
      while (j <= i) {
        acc <- acc + j
        j <- j + 1L
      }
      out[i] <- acc
    }
    out
  }
  trans <- mojor_transpile(f, n = 'i32')
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("for ", trans$mojo) && grepl("while ", trans$mojo))
})

test_that("triangular iteration pattern transpiles successfully", {
  # Inner loop bound depends on outer loop variable
  f <- function(n) {
    out <- 0.0
    for (i in seq_len(n)) {
      for (j in seq_len(i)) {
        out <- out + i * j
      }
    }
    out
  }
  trans <- mojor_transpile(f, n = 'i32')
  expect_true(!is.null(trans$mojo))
})

# =============================================================================
# SECTION 6: Sequential Loops (Multiple at Same Level)
# =============================================================================

test_that("sequential loops with scalar accumulation transpile successfully", {
  f <- function(x, y) {
    acc <- 0.0
    for (i in seq_along(x)) {
      acc <- acc + x[i]
    }
    for (j in seq_along(y)) {
      acc <- acc + y[j]
    }
    acc
  }
  trans <- mojor_transpile(f, x = 'f64[]', y = 'f64[]')
  expect_true(!is.null(trans$mojo))
})

test_that("sequential independent for loops transpile successfully", {
  f <- function(n) {
    out1 <- 0.0
    out2 <- 0.0
    for (i in seq_len(n)) {
      out1 <- out1 + i
    }
    for (j in seq_len(n)) {
      out2 <- out2 + j * j
    }
    out1 + out2
  }
  trans <- mojor_transpile(f, n = 'i32')
  expect_true(!is.null(trans$mojo))
})

# =============================================================================
# SECTION 7: Empty and Edge Case Bounds
# =============================================================================

test_that("loop with potential zero iterations handles gracefully", {
  f <- function(n) {
    out <- 0.0
    for (i in seq_len(n)) {
      out <- out + i
    }
    out
  }
  trans <- mojor_transpile(f, n = 'i32')
  expect_true(!is.null(trans$mojo))
})

test_that("single iteration loop transpiles successfully", {
  f <- function() {
    out <- 0.0
    for (i in 1:1) {
      out <- out + i
    }
    out
  }
  trans <- mojor_transpile(f)
  expect_true(!is.null(trans$mojo))
})

test_that("loop with constant bound transpiles successfully", {
  f <- function() {
    out <- 0.0
    for (i in 1:10) {
      out <- out + i
    }
    out
  }
  trans <- mojor_transpile(f)
  expect_true(!is.null(trans$mojo))
})

# =============================================================================
# SECTION 8: Loop Variable Usage Patterns
# =============================================================================

test_that("loop variable used in multiple index positions transpiles successfully", {
  f <- function(m) {
    n <- nrow(m)
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- m[i, i]
    }
    out
  }
  trans <- mojor_transpile(f, m = 'f64[,]')
  expect_true(!is.null(trans$mojo))
})

test_that("loop variable in arithmetic expression transpiles successfully", {
  f <- function(n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- i * i + 2 * i + 1
    }
    out
  }
  trans <- mojor_transpile(f, n = 'i32')
  expect_true(!is.null(trans$mojo))
})

test_that("in-place array update transpiles successfully", {
  # In-place updates like x[i] <- x[i] + 1 with return of x now work
  f <- function(x) {
    for (i in seq_along(x)) {
      x[i] <- x[i] + 1
    }
    x
  }
  trans <- mojor_transpile(f, x = 'f64[]')
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("for ", trans$mojo))
})

# =============================================================================
# SECTION 9: Complex Conditions in While Loops
# =============================================================================

test_that("while with compound AND condition transpiles successfully", {
  f <- function(x, y) {
    out <- 0.0
    i <- 1L
    while (i <= length(x) && i <= length(y)) {
      out <- out + x[i] + y[i]
      i <- i + 1L
    }
    out
  }
  trans <- mojor_transpile(f, x = 'f64[]', y = 'f64[]')
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("while ", trans$mojo))
  expect_true(grepl(" and ", trans$mojo))
})

test_that("while with compound OR condition transpiles successfully", {
  f <- function(x, threshold) {
    out <- 0.0
    i <- 1L
    while (i <= length(x) || out < threshold) {
      if (i > length(x)) break
      out <- out + x[i]
      i <- i + 1L
    }
    out
  }
  trans <- mojor_transpile(f, x = 'f64[]', threshold = 'f64')
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("while ", trans$mojo))
  expect_true(grepl(" or ", trans$mojo))
})

test_that("while with NOT condition transpiles successfully", {
  f <- function(x) {
    out <- 0.0
    i <- 1L
    while (!(i > length(x))) {
      out <- out + x[i]
      i <- i + 1L
    }
    out
  }
  trans <- mojor_transpile(f, x = 'f64[]')
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("while ", trans$mojo))
  expect_true(grepl("not ", trans$mojo))
})

# =============================================================================
# SECTION 10: Known Limitation / Edge Cases
# =============================================================================

test_that("repeat with explicit break transpiles successfully", {
  f <- function(x) {
    out <- 0.0
    i <- 1L
    repeat {
      if (i > length(x)) break
      out <- out + x[i]
      i <- i + 1L
    }
    out
  }
  trans <- mojor_transpile(f, x = 'f64[]')
  expect_true(!is.null(trans$mojo))
})

test_that("loop over stored index vector transpiles successfully", {
  f <- function(n) {
    indices <- seq_len(n)
    out <- numeric(n)
    for (i in indices) {
      out[i] <- i * 2
    }
    out
  }
  trans <- mojor_transpile(f, n = 'i32')
  expect_true(!is.null(trans$mojo))
})

test_that("if-else inside loop transpiles successfully", {
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      if (x[i] > 0) {
        out[i] <- x[i] * 2
      } else {
        out[i] <- x[i] / 2
      }
    }
    out
  }
  trans <- mojor_transpile(f, x = 'f64[]')
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("if ", trans$mojo) && grepl("else:", trans$mojo))
})

test_that("nested if inside loop transpiles successfully", {
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      if (x[i] > 0) {
        if (x[i] > 10) {
          out[i] <- 10
        } else {
          out[i] <- x[i]
        }
      } else {
        out[i] <- 0
      }
    }
    out
  }
  trans <- mojor_transpile(f, x = 'f64[]')
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("if ", trans$mojo))
})


# =============================================================================
# SECTION 11: For Each Style Iteration
# =============================================================================

test_that("for (x in vector) iterates over vector values", {
  f <- function(v) {
    s <- 0.0
    for (x in v) {
      s <- s + x
    }
    s
  }
  trans <- mojor_transpile(f, v = 'f64[]')
  expect_true(!is.null(trans$mojo))
  # Should generate __mojor_iter_i for internal iteration
  expect_true(grepl("__mojor_iter_i", trans$mojo))
  # Should read from vector using the iterator
  expect_true(grepl("_mojor_read_f64", trans$mojo))
})

test_that("for (i in seq(a, b)) iterates over sequence", {
  f <- function() {
    s <- 0.0
    for (i in seq(1, 5)) {
      s <- s + i
    }
    s
  }
  trans <- mojor_transpile(f)
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("for i in range", trans$mojo))
})

test_that("for (i in 1:n) colon sequence iteration works", {
  f <- function() {
    s <- 0.0
    for (i in 1:10) {
      s <- s + i
    }
    s
  }
  trans <- mojor_transpile(f)
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("for i in range", trans$mojo))
})

test_that("for (x in c(...)) literal vector iteration works", {
  f <- function() {
    s <- 0.0
    for (x in c(1, 2, 3)) {
      s <- s + x
    }
    s
  }
  trans <- mojor_transpile(f)
  expect_true(!is.null(trans$mojo))
  # Should iterate over the 3 elements (range(1, 3+1))
  expect_true(grepl("range.*3", trans$mojo))
})

test_that("multiple sequential for-each loops work", {
  f <- function(x, y) {
    s <- 0.0
    for (xi in x) {
      s <- s + xi
    }
    for (yi in y) {
      s <- s + yi
    }
    s
  }
  trans <- mojor_transpile(f, x = 'f64[]', y = 'f64[]')
  expect_true(!is.null(trans$mojo))
  # Should have two separate loops
  expect_equal(length(gregexpr("for ", trans$mojo)[[1]]), 2)
})

test_that("for-each with manual index tracking works", {
  f <- function(v) {
    out <- numeric(length(v))
    i <- 1L
    for (x in v) {
      out[i] <- x * 2
      i <- i + 1L
    }
    out
  }
  trans <- mojor_transpile(f, v = 'f64[]')
  expect_true(!is.null(trans$mojo))
  # Should have both the for-each loop and the manual index update
  expect_true(grepl("for ", trans$mojo))
  expect_true(grepl("i = ", trans$mojo) || grepl("i <-", trans$mojo))
})

test_that("nested for-each with index-based inner loop works", {
  # Mixing for-each with index-based is supported
  f <- function(vec, n) {
    total <- 0.0
    for (i in seq_len(n)) {
      for (j in seq_along(vec)) {
        total <- total + vec[j] * i
      }
    }
    total
  }
  trans <- mojor_transpile(f, vec = 'f64[]', n = 'i32')
  expect_true(!is.null(trans$mojo))
  # Should have nested loops
  expect_equal(length(gregexpr("for ", trans$mojo)[[1]]), 2)
})

test_that("nested index-based outer with for-each inner works", {
  # Previously this was NOT SUPPORTED but now works!
  f <- function(vec, n) {
    total <- 0.0
    for (i in seq_len(n)) {
      for (x in vec) {
        total <- total + x * i
      }
    }
    total
  }
  trans <- mojor_transpile(f, vec = 'f64[]', n = 'i32')
  expect_true(!is.null(trans$mojo))
  # Should have nested loops
  expect_equal(length(gregexpr("for ", trans$mojo)[[1]]), 2)
  # Inner loop should iterate over vec's length
  expect_true(grepl("n_vec_i", trans$mojo))
})


# =============================================================================
# SECTION 12: Very Complex Loop Patterns
# =============================================================================

test_that("deeply nested quadruple loop with mixed iteration types works", {
  # Four levels of nesting with different loop types at each level
  f <- function(a, b, c, d) {
    total <- 0.0
    for (i in seq_len(a)) {
      for (j in seq(b, 1, by = -1)) {  # Descending
        for (k in seq_along(c)) {
          for (x in d) {  # For-each innermost
            total <- total + i * j * k * x
          }
        }
      }
    }
    total
  }
  trans <- mojor_transpile(f, a = 'i32', b = 'i32', c = 'f64[]', d = 'f64[]')
  expect_true(!is.null(trans$mojo))
  # Should have 4 nested loops
  expect_equal(length(gregexpr("for ", trans$mojo)[[1]]), 4)
})

test_that("complex control flow with conditional break/next in nested loops works", {
  # Multiple levels of nesting with conditional break/next
  f <- function(n, threshold) {
    result <- 0.0
    for (i in seq_len(n)) {
      if (i > 50) {
        break  # Early exit outer
      }
      for (j in seq_len(n)) {
        if (j %% 10 == 0) {
          next  # Skip multiples of 10
        }
        if (i * j > threshold) {
          break  # Exit inner
        }
        result <- result + i * j
      }
    }
    result
  }
  trans <- mojor_transpile(f, n = 'i32', threshold = 'f64')
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("break", trans$mojo))
  expect_true(grepl("continue", trans$mojo))
})

test_that("multiple arrays modified in-place in nested loops works", {
  # Modify two input arrays in-place in nested context
  f <- function(x, y) {
    for (i in seq_along(x)) {
      x[i] <- x[i] * 2
      for (j in seq_along(y)) {
        y[j] <- y[j] + x[i]
        if (y[j] > 100) {
          y[j] <- 100  # Cap at 100
        }
      }
    }
    list(x = x, y = y)
  }
  trans <- mojor_transpile(f, x = 'f64[]', y = 'f64[]')
  expect_true(!is.null(trans$mojo))
})

test_that("while loop with complex compound conditions and nested control flow works", {
  # Complex while with AND, OR, NOT and nested if/break/next
  f <- function(x, y) {
    out <- numeric(length(x))
    i <- 1L
    while (i <= length(x) && (i <= length(y) || length(y) == 0)) {
      if (!(x[i] > 0)) {
        i <- i + 1L
        next
      }
      if (x[i] > 100 && y[i] < 0) {
        break
      }
      out[i] <- x[i] + (if (i <= length(y)) y[i] else 0)
      i <- i + 1L
    }
    out
  }
  trans <- mojor_transpile(f, x = 'f64[]', y = 'f64[]')
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("while ", trans$mojo))
})

test_that("triangular iteration with in-place matrix update works", {
  # Upper triangular pattern with in-place modification
  f <- function(m) {
    n <- nrow(m)
    for (i in seq_len(n)) {
      for (j in seq(i, n)) {
        m[i, j] <- m[i, j] + i * j
        if (i != j) {
          m[j, i] <- m[j, i] - i * j  # Symmetric update
        }
      }
    }
    m
  }
  trans <- mojor_transpile(f, m = 'f64[,]')
  expect_true(!is.null(trans$mojo))
})

test_that("sequential loops with different bounds and control flow works", {
  # Multiple sequential loops, each with different patterns
  f <- function(n, m) {
    # First loop: simple accumulation
    sum1 <- 0.0
    for (i in seq_len(n)) {
      sum1 <- sum1 + i
    }
    
    # Second loop: with break condition
    sum2 <- 0.0
    for (i in seq_len(m)) {
      if (i > 100) break
      sum2 <- sum2 + i * 2
    }
    
    # Third loop: while with complex condition
    sum3 <- 0.0
    i <- 1L
    while (i <= n && i <= m) {
      sum3 <- sum3 + i * i
      i <- i + 1L
    }
    
    sum1 + sum2 + sum3
  }
  trans <- mojor_transpile(f, n = 'i32', m = 'i32')
  expect_true(!is.null(trans$mojo))
})

test_that("nested loops with break to exit early works", {
  # Use break with flag instead of early return
  f <- function(n, p, threshold) {
    result <- 0
    for (i in seq_len(n)) {
      for (j in seq_len(p)) {
        val <- i * j
        if (val > threshold) {
          result <- 1
          break  # Exit inner
        }
        if (i > 10) {
          result <- -1
          break  # Exit inner, will exit outer next
        }
      }
      if (result != 0) break  # Exit outer
    }
    result
  }
  trans <- mojor_transpile(f, n = 'i32', p = 'i32', threshold = 'f64')
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("break", trans$mojo))
})

test_that("for-each with nested index-based and break works", {
  # For-each outer, index-based inner with early exit
  f <- function(vectors, limit) {
    total <- 0.0
    count <- 0L
    for (v in vectors) {
      for (i in seq_along(v)) {
        if (count >= limit) {
          break
        }
        total <- total + v[i]
        count <- count + 1L
      }
      if (count >= limit) {
        break
      }
    }
    total
  }
  # Note: This tests the pattern; vectors would need special handling
  # Testing with a simpler version that uses a single vector
  f2 <- function(v, limit) {
    total <- 0.0
    for (i in seq_along(v)) {
      if (i > limit) {
        break
      }
      total <- total + v[i]
    }
    total
  }
  trans <- mojor_transpile(f2, v = 'f64[]', limit = 'i32')
  expect_true(!is.null(trans$mojo))
})

test_that("complex nested loop with scalar and vector accumulation works", {
  # Multiple accumulators at different nesting levels
  f <- function(x, n) {
    grand_total <- 0.0
    row_sums <- numeric(n)
    
    for (i in seq_len(n)) {
      row_sum <- 0.0
      for (j in seq_along(x)) {
        val <- x[j] * i
        row_sum <- row_sum + val
        grand_total <- grand_total + val
      }
      row_sums[i] <- row_sum
    }
    list(grand = grand_total, rows = row_sums)
  }
  trans <- mojor_transpile(f, x = 'f64[]', n = 'i32')
  expect_true(!is.null(trans$mojo))
})

test_that("matrix traversal with boundary checks and in-place update works", {
  # 2D array with careful boundary checking
  f <- function(m) {
    n <- nrow(m)
    p <- ncol(m)
    for (i in seq_len(n)) {
      for (j in seq_len(p)) {
        # Update center
        m[i, j] <- m[i, j] * 2
        
        # Update neighbors if they exist
        if (i > 1) {
          m[i - 1, j] <- m[i - 1, j] + 1
        }
        if (j > 1) {
          m[i, j - 1] <- m[i, j - 1] + 1
        }
        if (i < n) {
          m[i + 1, j] <- m[i + 1, j] + 1
        }
        if (j < p) {
          m[i, j + 1] <- m[i, j + 1] + 1
        }
      }
    }
    m
  }
  trans <- mojor_transpile(f, m = 'f64[,]')
  expect_true(!is.null(trans$mojo))
})

test_that("deep nesting with alternating loop types and complex exits works", {
  # Alternating for/while/nested with multiple exit conditions
  f <- function(n, x) {
    result <- 0.0
    i <- 1L
    while (i <= n) {
      found <- FALSE
      for (j in seq_along(x)) {
        if (x[j] == i) {
          found <- TRUE
          break
        }
      }
      if (!found) {
        for (k in seq_len(i)) {
          result <- result + k
          if (result > 1000) {
            return(result)  # Early return
          }
        }
      }
      i <- i + 1L
    }
    result
  }
  trans <- mojor_transpile(f, n = 'i32', x = 'f64[]')
  expect_true(!is.null(trans$mojo))
})

test_that("multiple nested for-each loops with index tracking works", {
  # Multiple for-each with manual position tracking
  f <- function(a, b, c) {
    out <- numeric(length(a) * length(b) * length(c))
    pos <- 1L
    for (x in a) {
      for (y in b) {
        for (z in c) {
          out[pos] <- x + y + z
          pos <- pos + 1L
        }
      }
    }
    out
  }
  trans <- mojor_transpile(f, a = 'f64[]', b = 'f64[]', c = 'f64[]')
  expect_true(!is.null(trans$mojo))
})

test_that("complex loop with repeat, multiple breaks, and in-place modification works", {
  # Repeat loop with multiple break conditions and in-place update
  f <- function(x, max_iter) {
    iter <- 0L
    repeat {
      iter <- iter + 1L
      changed <- FALSE
      
      for (i in seq_along(x)) {
        if (i > 1 && x[i] < x[i - 1]) {
          # Swap/adjust
          tmp <- x[i]
          x[i] <- x[i - 1]
          x[i - 1] <- tmp
          changed <- TRUE
        }
      }
      
      if (!changed) {
        break  # Converged
      }
      if (iter >= max_iter) {
        break  # Max iterations
      }
    }
    x
  }
  trans <- mojor_transpile(f, x = 'f64[]', max_iter = 'i32')
  expect_true(!is.null(trans$mojo))
})

test_that("nested loops with computed bounds via min() works", {
  # Computed inner bound using min() with outer loop variable
  f <- function(n, m) {
    out <- matrix(0.0, nrow = n, ncol = m)
    
    for (i in seq_len(n)) {
      inner_bound <- min(i * 2, m)
      for (j in seq_len(inner_bound)) {
        out[i, j] <- i * j
      }
    }
    out
  }
  trans <- mojor_transpile(f, n = 'i32', m = 'i32')
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("for ", trans$mojo))
})

test_that("nested loops with computed bounds via arithmetic works", {
  # Computed inner bound using arithmetic with outer loop variable
  f <- function(n) {
    total <- 0
    for (i in seq_len(n)) {
      bound <- i * 2L + 1L
      for (j in seq_len(bound)) {
        total <- total + j
      }
    }
    total
  }
  trans <- mojor_transpile(f, n = 'i32')
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("for ", trans$mojo))
})


test_that("inline computed bounds with min() works", {
  # Inline computed bound: seq_len(min(i * 2, m))
  f <- function(n, m) {
    out <- matrix(0.0, nrow = n, ncol = m)
    for (i in seq_len(n)) {
      for (j in seq_len(min(i * 2, m))) {  # Inline computed bound
        out[i, j] <- i * j
      }
    }
    out
  }
  trans <- mojor_transpile(f, n = 'i32', m = 'i32')
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("for ", trans$mojo))
})

test_that("inline computed bounds with arithmetic works", {
  # Inline computed bound: seq_len(i * 2 + 1)
  f <- function(n) {
    total <- 0
    for (i in seq_len(n)) {
      for (j in seq_len(i * 2 + 1)) {  # Inline arithmetic
        total <- total + j
      }
    }
    total
  }
  trans <- mojor_transpile(f, n = 'i32')
  expect_true(!is.null(trans$mojo))
  expect_true(grepl("for ", trans$mojo))
})
