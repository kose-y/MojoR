library(testthat)

# Control Flow IR Support Tests

# =============================================================================
# While Loop Tests
# =============================================================================

test_that("IR emits simple while loop with comparison", {  f <- function(x, n) {
    out <- numeric(n)
    i <- 1
    while (i <= n) {
      out[i] <- x[i] * 2
      i <- i + 1
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for while header
  expect_true(grepl("while.*i.*<=.*n", res$mojo))
  expect_true(grepl("while.*:", res$mojo))
})

test_that("IR supports standalone while loops with scalar i32 n source", {  f <- function(n) {
    i <- 1L
    acc <- 0.0
    while (i <= n) {
      acc <- acc + i
      i <- i + 1L
    }
    acc
  }

  res <- mojor_transpile(f, n = "i32")

  expect_equal(res$n_source$kind, "scalar")
  expect_equal(res$n_source$name, "n")
  expect_equal(res$out_kind, "scalar")
  expect_true(grepl("while.*i.*<=.*n", res$mojo))
})

test_that("IR supports standalone while loops with scalar f64 n source", {  f <- function(n) {
    i <- 1L
    acc <- 0.0
    while (i <= n) {
      acc <- acc + i
      i <- i + 1L
    }
    acc
  }

  res <- mojor_transpile(f, n = "f64")

  expect_equal(res$n_source$kind, "scalar")
  expect_equal(res$n_source$name, "n")
  expect_equal(res$out_kind, "scalar")
  expect_true(grepl("while.*i.*<=.*n", res$mojo))
})

test_that("IR supports standalone while loops with local scalar i32 n source and no formals", {  f <- function() {
    n <- 6L
    i <- 1L
    acc <- 0.0
    while (i <= n) {
      acc <- acc + i
      i <- i + 1L
    }
    acc
  }

  res <- mojor_transpile(f)

  expect_equal(res$n_source$kind, "scalar")
  expect_equal(res$n_source$name, "n")
  expect_equal(res$out_kind, "scalar")
  expect_true(grepl("while.*i.*<=.*n", res$mojo))
})

test_that("IR supports standalone repeat loops with scalar i32 n source", {  f <- function(n) {
    i <- 1L
    acc <- 0.0
    repeat {
      if (i > n) break
      acc <- acc + i
      i <- i + 1L
    }
    acc
  }

  res <- mojor_transpile(f, n = "i32")

  expect_equal(res$n_source$kind, "scalar")
  expect_equal(res$n_source$name, "n")
  expect_equal(res$out_kind, "scalar")
  expect_true(grepl("while True", res$mojo))
})

test_that("IR supports standalone repeat loops with scalar f64 n source", {  f <- function(n) {
    i <- 1L
    acc <- 0.0
    repeat {
      if (i > n) break
      acc <- acc + i
      i <- i + 1L
    }
    acc
  }

  res <- mojor_transpile(f, n = "f64")

  expect_equal(res$n_source$kind, "scalar")
  expect_equal(res$n_source$name, "n")
  expect_equal(res$out_kind, "scalar")
  expect_true(grepl("while True", res$mojo))
})

test_that("IR supports standalone repeat loops with local scalar i32 n source and no formals", {  f <- function() {
    n <- 5L
    i <- 1L
    acc <- 0.0
    repeat {
      if (i > n) break
      acc <- acc + i
      i <- i + 1L
    }
    acc
  }

  res <- mojor_transpile(f)

  expect_equal(res$n_source$kind, "scalar")
  expect_equal(res$n_source$name, "n")
  expect_equal(res$out_kind, "scalar")
  expect_true(grepl("while True", res$mojo))
})

test_that("IR supports standalone repeat loops with local scalar f64 n source and no formals", {  f <- function() {
    n <- 5.0
    i <- 1L
    acc <- 0.0
    repeat {
      if (i > n) break
      acc <- acc + i
      i <- i + 1L
    }
    acc
  }

  res <- mojor_transpile(f)

  expect_equal(res$n_source$kind, "scalar")
  expect_equal(res$n_source$name, "n")
  expect_equal(res$out_kind, "scalar")
  expect_true(grepl("while True", res$mojo))
})

test_that("repeat scalar n-source prefers referenced i32 arg", {  f <- function(n, m) {
    i <- 1L
    acc <- 0.0
    repeat {
      if (i > m) break
      acc <- acc + i
      i <- i + 1L
    }
    acc
  }

  res <- mojor_transpile(f, n = "i32", m = "i32")

  expect_equal(res$n_source$kind, "scalar")
  expect_equal(res$n_source$name, "m")
})

test_that("IR emits while loop with scalar condition (needs coercion)", {  f <- function(x, n) {
    out <- numeric(n)
    i <- 1
    flag <- 5
    while (flag) {
      out[i] <- x[i]
      i <- i + 1
      flag <- flag - 1
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Non-boolean condition should get coercion
  expect_true(grepl("while.*!=.*0", res$mojo))
})

test_that("IR emits while loop with compound condition", {  f <- function(x, n) {
    out <- numeric(n)
    i <- 1
    while (i <= n && x[i] > 0) {
      out[i] <- x[i]
      i <- i + 1
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for compound condition with and
  expect_true(grepl("while", res$mojo))
  expect_true(grepl("and", res$mojo))
})

test_that("IR handles nested while loops", {  f <- function(x, n, m) {
    out <- numeric(n)
    i <- 1
    while (i <= n) {
      j <- 1
      while (j <= m) {
        out[i] <- out[i] + j
        j <- j + 1
      }
      i <- i + 1
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32", m = "i32")

  # Check for nested while loops
  expect_true(grepl("while.*i.*<=.*n", res$mojo))
  expect_true(grepl("while.*j.*<=.*m", res$mojo))
})

test_that("IR emits while with boolean condition (no coercion)", {  f <- function(x, n) {
    out <- numeric(n)
    i <- 1
    cont <- TRUE
    while (cont) {
      out[i] <- x[i]
      i <- i + 1
      cont <- (i <= n)
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Boolean condition should not need coercion
  expect_true(grepl("while", res$mojo))
})

# =============================================================================
# Break Statement Tests
# =============================================================================

test_that("IR emits break in while loop", {  f <- function(x, n) {
    out <- numeric(n)
    i <- 1
    while (i <= n) {
      if (x[i] < 0) break
      out[i] <- x[i]
      i <- i + 1
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for break statement
  expect_true(grepl("break", res$mojo))
})

test_that("IR emits break in for loop", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      if (x[i] < 0) break
      out[i] <- x[i]
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for break statement
  expect_true(grepl("break", res$mojo))
})

test_that("IR emits break in nested if within loop", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      if (x[i] < 0) {
        if (x[i] < -10) {
          break
        }
      }
      out[i] <- x[i]
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for break statement in nested context
  expect_true(grepl("break", res$mojo))
})

# =============================================================================
# Next Statement Tests
# =============================================================================

test_that("IR emits next (continue) in while loop", {  f <- function(x, n) {
    out <- numeric(n)
    i <- 1
    while (i <= n) {
      if (x[i] < 0) {
        i <- i + 1
        next
      }
      out[i] <- x[i] * 2
      i <- i + 1
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for continue statement (R next → Mojo continue)
  expect_true(grepl("continue", res$mojo))
})

test_that("IR emits next (continue) in for loop", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      if (x[i] < 0) next
      out[i] <- x[i] * 2
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for continue statement
  expect_true(grepl("continue", res$mojo))
})

test_that("IR emits next in nested if within loop", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      if (x[i] < 0) {
        if (x[i] < -10) {
          next
        }
      }
      out[i] <- x[i]
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for continue statement
  expect_true(grepl("continue", res$mojo))
})

# =============================================================================
# Mixed Control Flow Tests
# =============================================================================

test_that("IR handles for loop with while inside", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      j <- 1
      while (j <= 3) {
        out[i] <- out[i] + j
        j <- j + 1
      }
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for both for and while
  expect_true(grepl("for.*i.*in.*range", res$mojo))
  expect_true(grepl("while.*j.*<=", res$mojo))
})

test_that("IR handles while loop with for inside", {  f <- function(x, n) {
    out <- numeric(n)
    i <- 1
    while (i <= n) {
      for (j in 1:3) {
        out[i] <- out[i] + j
      }
      i <- i + 1
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for both while and for
  expect_true(grepl("while.*i.*<=", res$mojo))
  expect_true(grepl("for.*j.*in.*range", res$mojo))
})

test_that("IR handles while with if/else containing break", {  f <- function(x, n) {
    out <- numeric(n)
    i <- 1
    while (i <= n) {
      if (x[i] < 0) {
        break
      } else {
        out[i] <- x[i]
      }
      i <- i + 1
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for while, if/else, and break
  expect_true(grepl("while", res$mojo))
  expect_true(grepl("if", res$mojo))
  expect_true(grepl("else", res$mojo))
  expect_true(grepl("break", res$mojo))
})

test_that("IR handles while with if/else containing next", {  f <- function(x, n) {
    out <- numeric(n)
    i <- 1
    while (i <= n) {
      if (x[i] < 0) {
        i <- i + 1
        next
      } else {
        out[i] <- x[i]
        i <- i + 1
      }
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for while, if/else, and continue
  expect_true(grepl("while", res$mojo))
  expect_true(grepl("if", res$mojo))
  expect_true(grepl("else", res$mojo))
  expect_true(grepl("continue", res$mojo))
})

# =============================================================================
# Edge Cases
# =============================================================================

test_that("IR handles while with complex multi-variable condition", {  f <- function(x, y, n) {
    out <- numeric(n)
    i <- 1
    while (i <= n && x[i] > y[i]) {
      out[i] <- x[i] - y[i]
      i <- i + 1
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", y = "f64[]", n = "i32")

  # Check for complex condition
  expect_true(grepl("while", res$mojo))
  expect_true(grepl("and", res$mojo))
})

test_that("IR handles while with OR condition", {  f <- function(x, n) {
    out <- numeric(n)
    i <- 1
    while (i <= n || x[1] > 0) {
      out[i] <- x[i]
      i <- i + 1
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for OR condition
  expect_true(grepl("while", res$mojo))
  expect_true(grepl("or", res$mojo))
})

test_that("IR handles while with negated condition", {  f <- function(x, n) {
    out <- numeric(n)
    i <- 1
    done <- FALSE
    while (!done) {
      out[i] <- x[i]
      i <- i + 1
      done <- (i > n)
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", n = "i32")

  # Check for while with negation
  expect_true(grepl("while", res$mojo))
  expect_true(grepl("not", res$mojo))
})
