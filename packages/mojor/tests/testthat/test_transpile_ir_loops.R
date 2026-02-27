library(testthat)

test_that("IR emits simple seq_len loop", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[i] + 1
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32")
  mojo <- result$mojo

  # IR may unroll loops; ensure an i-loop range is emitted.
  expect_true(grepl("for i in range", mojo))
  # Body should have proper index conversion (writes use direct index, reads use helper)
  expect_true(grepl("out\\[Int\\(\\(i - 1\\)\\)\\]", mojo))
  expect_true(grepl("Int\\(\\(i - 1\\)\\)", mojo))  # index conversion present
})

test_that("IR emits simple 1:n loop", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:10) {
      out[i] <- x[i]
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32")
  mojo <- result$mojo

  expect_true(grepl("for i in range", mojo))
})

# NOTE: seq_along in main loops is preprocessed by .mojor_parse_loop_seq() into length expressions
# before IR sees it, so it won't match IR patterns. Test nested seq_along instead:
test_that("IR emits nested seq_along loop", {  f <- function(x, n, m) {
    out <- numeric(n)
    total <- 0.0
    for (i in 1:n) {
      for (j in seq_along(x)) {
        total <- total + x[j]
      }
      out[i] <- total
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", m = "i32")
  mojo <- result$mojo

  # seq_along uses len_var_map → emits range(1, (n_x_i + 1)) or similar
  expect_true(grepl("for j in range\\(1,", mojo))
})

test_that("IR emits if/else inside loop", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      if (x[i] > 0) {
        out[i] <- x[i] * 2
      } else {
        out[i] <- 0
      }
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32")
  mojo <- result$mojo

  # Reads use _mojor_read_f64 helper; check condition and else presence
  expect_true(grepl("if.*>.*(Float64\\(0\\)|0)", mojo))
  expect_true(grepl("else:", mojo))
})

test_that("IR emits if without else", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      if (i > 5) {
        out[i] <- x[i] + 10
      }
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32")
  mojo <- result$mojo

  # Check for if with i > 5 condition (typed IR may add Float64 cast)
  expect_true(grepl("if.*i.*>.*5", mojo))
  # Should NOT have else block
  expect_false(grepl("else:", mojo))
})

test_that("IR handles dynamic range (a:b with sign check)", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- x[i]
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32")
  mojo <- result$mojo

  # Dynamic 1:n still must emit an i-loop range.
  expect_true(grepl("for i in range", mojo))
})

test_that("IR emits nested loop", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      for (j in 1:3) {
        out[i] <- out[i] + x[i] * j
      }
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32")
  mojo <- result$mojo

  # Check for both loop variables
  expect_true(grepl("for i in range", mojo))
  expect_true(grepl("for j in range", mojo))
})

test_that("IR-only emits seq.int loop with by", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq.int(1, n, by = 2)) {
      out[i] <- x[i]
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  mojo <- result$mojo

  expect_true(grepl("for i in range", mojo))
  has_step <- grepl(", Int\\(2\\.?0?\\)", mojo) || grepl(", 2\\)", mojo)
  expect_true(has_step)
})

test_that("IR-only emits seq loop with by", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq(1, n, by = 2)) {
      out[i] <- x[i]
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  mojo <- result$mojo

  expect_true(grepl("for i in range", mojo))
  has_step <- grepl(", Int\\(2\\.?0?\\)", mojo) || grepl(", 2\\)", mojo)
  expect_true(has_step)
})

test_that("IR-only emits seq.int length.out loop", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq.int(length.out = n)) {
      out[i] <- x[i]
    }
    out
  }

  result <- mojor_transpile(f, x = "f64[]", n = "i32", ir_only = TRUE)
  mojo <- result$mojo

  expect_true(grepl("for i in range", mojo))
})
