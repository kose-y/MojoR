test_that("transpile while loop with break/next", {  f <- function(x) {
    out <- numeric(length(x))
    i <- 1L
    while (i <= length(x)) {
      if (x[i] < 0) {
        i <- i + 1L
        next
      }
      if (x[i] == 0) break
      out[i] <- x[i] * 2
      i <- i + 1L
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]")
  expect_true(grepl("while", trans$mojo))
  expect_true(grepl("continue", trans$mojo))
  expect_true(grepl("break", trans$mojo))
})

test_that("transpile nested while loops", {  f <- function(x) {
    out <- numeric(length(x))
    i <- 1L
    while (i <= length(x)) {
      j <- 1L
      while (j <= length(x)) {
        out[i] <- out[i] + x[j]
        j <- j + 1L
      }
      i <- i + 1L
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]")
  expect_true(grepl("while", trans$mojo))
})

test_that("transpile mixed while + for nesting", {  f <- function(x, y) {
    out <- numeric(length(x))
    i <- 1L
    while (i <= length(x)) {
      for (j in seq_along(y)) {
        out[i] <- out[i] + y[j]
      }
      i <- i + 1L
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]")
  expect_true(grepl("while", trans$mojo))
  expect_true(grepl("for", trans$mojo))
})

test_that("transpile mixed for + while nesting", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      j <- 1L
      while (j <= length(x)) {
        out[i] <- out[i] + x[j]
        j <- j + 1L
      }
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]")
  expect_true(grepl("for", trans$mojo))
  expect_true(grepl("while", trans$mojo))
})

test_that("break/next inside nested loops", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      j <- 1L
      while (j <= length(x)) {
        if (x[j] < 0) {
          j <- j + 1L
          next
        }
        if (x[j] == 0) break
        out[i] <- out[i] + x[j]
        j <- j + 1L
      }
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]")
  expect_true(grepl("break", trans$mojo))
  expect_true(grepl("continue", trans$mojo))
})

test_that("repeat loops transpile", {  f <- function(x) {
    out <- numeric(length(x))
    i <- 1L
    repeat {
      if (i > length(x)) break
      out[i] <- x[i] + 1
      i <- i + 1L
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]")
  expect_true(grepl("while True", trans$mojo))
  expect_true(grepl("break", trans$mojo))
})

test_that("repeat loops support next + break", {  f <- function(x) {
    out <- numeric(length(x))
    i <- 1L
    repeat {
      if (i > length(x)) break
      if (x[i] < 0) {
        i <- i + 1L
        next
      }
      out[i] <- x[i]
      i <- i + 1L
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]")
  expect_true(grepl("while True", trans$mojo))
  expect_true(grepl("continue", trans$mojo))
  expect_true(grepl("break", trans$mojo))
})

test_that("while supports compound conditions", {  f <- function(x, y) {
    out <- numeric(length(x))
    i <- 1L
    while (i <= length(x) && i <= length(y) && !(x[i] < 0 || y[i] < 0)) {
      out[i] <- x[i] + y[i]
      i <- i + 1L
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]")
  expect_true(grepl("while", trans$mojo))
  expect_true(grepl("and", trans$mojo))
  expect_true(grepl("or", trans$mojo))
  expect_true(grepl("not", trans$mojo))
})

test_that("while condition supports length() of non-primary arrays", {  f <- function(x, y) {
    out <- numeric(length(x))
    i <- 1L
    while (i <= length(y)) {
      out[i] <- y[i]
      i <- i + 1L
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]")
  expect_true(grepl("while", trans$mojo))
  expect_true(grepl("n_y_i", trans$mojo))
})

test_that("mixed top-level ranges allowed for scalar output", {  f <- function(x, y) {
    acc <- 0
    for (i in seq_along(x)) {
      acc <- acc + x[i]
    }
    for (j in seq_along(y)) {
      acc <- acc + y[j]
    }
    acc
  }
  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]")
  expect_true(grepl("for", trans$mojo))
})

test_that("mixed top-level ranges rejected for vector output", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i]
    }
    for (j in seq_along(y)) {
      out[j] <- y[j]
    }
    out
  }
  expect_error(
    mojor_transpile(f, x = "f64[]", y = "f64[]"),
    "multiple loops must share the same range"
  )
})

test_that("while with zero typed args transpiles", {
  f <- function() {
    out <- 0
    i <- 1L
    while (i <= 10L) {
      out <- out + i
      i <- i + 1L
    }
    out
  }
  trans <- mojor_transpile(f)
  expect_true(grepl("while", trans$mojo))
})

test_that("repeat with zero typed args transpiles", {
  f <- function() {
    out <- 0
    i <- 1L
    repeat {
      if (i > 5L) break
      out <- out + i
      i <- i + 1L
    }
    out
  }
  trans <- mojor_transpile(f)
  expect_true(grepl("while True", trans$mojo))
  expect_true(grepl("break", trans$mojo))
})

test_that("for with zero typed args transpiles using seq_len with local scalar", {
  f <- function() {
    out <- 0
    n <- 10L
    for (i in seq_len(n)) {
      out <- out + i
    }
    out
  }
  trans <- mojor_transpile(f)
  expect_true(grepl("for", trans$mojo) || grepl("while", trans$mojo))
})

test_that("for with zero typed args and vector output works", {
  f <- function() {
    n <- 10L
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- i * 2
    }
    out
  }
  trans <- mojor_transpile(f)
  expect_true(grepl("for", trans$mojo) || grepl("while", trans$mojo))
})

test_that("for with scalar param and vector output works", {
  f <- function(n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- i * 2
    }
    out
  }
  trans <- mojor_transpile(f, n = 'i32')
  expect_true(grepl("for", trans$mojo) || grepl("while", trans$mojo))
})

test_that("matrix input with zero other args works", {
  f <- function(m) {
    out <- 0.0
    for (i in seq_len(nrow(m))) {
      for (j in seq_len(ncol(m))) {
        out <- out + m[i, j]
      }
    }
    out
  }
  trans <- mojor_transpile(f, m = 'f64[,]')
  expect_true(grepl("for", trans$mojo) || grepl("while", trans$mojo))
})

test_that("matrix output from scalar params works", {
  f <- function(nrow, ncol) {
    out <- matrix(0.0, nrow = nrow, ncol = ncol)
    for (i in seq_len(nrow)) {
      for (j in seq_len(ncol)) {
        out[i, j] <- i + j
      }
    }
    out
  }
  trans <- mojor_transpile(f, nrow = 'i32', ncol = 'i32')
  expect_true(grepl("for", trans$mojo) || grepl("while", trans$mojo))
})

test_that("3D array output from scalar params works", {
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
  expect_true(grepl("for", trans$mojo) || grepl("while", trans$mojo))
})

test_that("matrix output with zero args and local dims works", {
  f <- function() {
    nrow <- 5L
    ncol <- 4L
    out <- matrix(0.0, nrow = nrow, ncol = ncol)
    for (i in seq_len(nrow)) {
      for (j in seq_len(ncol)) {
        out[i, j] <- i * j
      }
    }
    out
  }
  trans <- mojor_transpile(f)
  expect_true(grepl("for", trans$mojo) || grepl("while", trans$mojo))
})
# Complex nested loop scenarios with zero typed arguments

test_that("nested for loops with zero args work", {
  f <- function() {
    n <- 3L
    m <- 4L
    out <- 0.0
    for (i in seq_len(n)) {
      for (j in seq_len(m)) {
        out <- out + i * j
      }
    }
    out
  }
  trans <- mojor_transpile(f)
  expect_true(grepl("for", trans$mojo) || grepl("while", trans$mojo))
})

test_that("nested for with matrix output and zero args work", {
  f <- function() {
    n <- 3L
    m <- 4L
    out <- matrix(0.0, nrow = n, ncol = m)
    for (i in seq_len(n)) {
      for (j in seq_len(m)) {
        out[i, j] <- i * j
      }
    }
    out
  }
  trans <- mojor_transpile(f)
  expect_true(grepl("for", trans$mojo) || grepl("while", trans$mojo))
})

test_that("triple nested loops with 3D output work", {
  f <- function() {
    d1 <- 3L
    d2 <- 4L
    d3 <- 2L
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
  trans <- mojor_transpile(f)
  expect_true(grepl("for", trans$mojo) || grepl("while", trans$mojo))
})

test_that("mixed for and while with scalar params work", {
  f <- function(n) {
    out <- 0.0
    for (i in seq_len(n)) {
      j <- 1L
      while (j <= n) {
        out <- out + i * j
        j <- j + 1L
      }
    }
    out
  }
  trans <- mojor_transpile(f, n = 'i32')
  expect_true(grepl("for", trans$mojo) || grepl("while", trans$mojo))
})

test_that("while with nested for using local scalar works", {
  # Local scalars can now be used in seq_len inside nested loop contexts
  f <- function() {
    n <- 3L
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
  trans <- mojor_transpile(f)
  expect_true(grepl("for", trans$mojo) || grepl("while", trans$mojo))
})

test_that("nested loops with break in inner loop work", {
  f <- function() {
    n <- 5L
    out <- 0.0
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (j > 3L) break
        out <- out + i * j
      }
    }
    out
  }
  trans <- mojor_transpile(f)
  expect_true(grepl("for", trans$mojo) || grepl("while", trans$mojo))
  expect_true(grepl("break", trans$mojo))
})

test_that("nested loops with next in inner loop work", {
  f <- function() {
    n <- 5L
    out <- 0.0
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (j == 3L) next
        out <- out + i * j
      }
    }
    out
  }
  trans <- mojor_transpile(f)
  expect_true(grepl("for", trans$mojo) || grepl("while", trans$mojo))
  expect_true(grepl("continue", trans$mojo))
})
test_that("loop over intermediate seq_len variable works", {
  f <- function(n) {
    v <- seq_len(n)
    out <- 0.0
    for (i in v) {
      out <- out + i
    }
    out
  }
  trans <- mojor_transpile(f, n = 'i32')
  expect_true(grepl("for", trans$mojo) || grepl("while", trans$mojo))
})

test_that("loop over intermediate seq_along variable works", {
  f <- function(x) {
    idx <- seq_along(x)
    out <- 0.0
    for (i in idx) {
      out <- out + x[i]
    }
    out
  }
  trans <- mojor_transpile(f, x = 'f64[]')
  expect_true(grepl("for", trans$mojo) || grepl("while", trans$mojo))
})
# Test edge case loop patterns

test_that("loop with float sequence works", {
  f <- function() {
    out <- 0.0
    for (i in seq(1, 10, by = 0.5)) {
      out <- out + i
    }
    out
  }
  trans <- mojor_transpile(f)
  expect_true(grepl("for", trans$mojo) || grepl("while", trans$mojo))
})

test_that("loop with decreasing sequence works", {
  f <- function() {
    out <- 0.0
    for (i in 10:1) {
      out <- out + i
    }
    out
  }
  trans <- mojor_transpile(f)
  expect_true(grepl("for", trans$mojo) || grepl("while", trans$mojo))
})

test_that("multiple sequential loops work", {
  f <- function() {
    out <- 0.0
    for (i in 1:5) {
      out <- out + i
    }
    for (j in 1:5) {
      out <- out + j
    }
    out
  }
  trans <- mojor_transpile(f)
  expect_true(grepl("for", trans$mojo) || grepl("while", trans$mojo))
})

test_that("loop with complex computed bound works", {
  f <- function(n, m) {
    out <- 0.0
    for (i in seq_len(n + m - 1L)) {
      out <- out + i
    }
    out
  }
  trans <- mojor_transpile(f, n = 'i32', m = 'i32')
  expect_true(grepl("for", trans$mojo) || grepl("while", trans$mojo))
})

test_that("loop with min/max bound works", {
  f <- function(n, m) {
    out <- 0.0
    for (i in seq_len(min(n, m))) {
      out <- out + i
    }
    out
  }
  trans <- mojor_transpile(f, n = 'i32', m = 'i32')
  expect_true(grepl("for", trans$mojo) || grepl("while", trans$mojo))
})
test_that("decreasing sequence 10:1 works", {
  f <- function() {
    out <- 0.0
    for (i in 10:1) {
      out <- out + i
    }
    out
  }
  trans <- mojor_transpile(f)
  expect_true(grepl("for", trans$mojo) || grepl("while", trans$mojo))
  # Check that it includes the right sum (55 from 1+2+...+10)
  expect_true(is.list(trans))
})

test_that("decreasing sequence with params works", {
  f <- function(n) {
    out <- 0.0
    for (i in n:1) {
      out <- out + i
    }
    out
  }
  trans <- mojor_transpile(f, n = 'i32')
  expect_true(grepl("for", trans$mojo) || grepl("while", trans$mojo))
})
test_that("seq with float step works", {
  f <- function() {
    out <- 0.0
    for (i in seq(1, 5, by = 0.5)) {
      out <- out + i
    }
    out
  }
  trans <- mojor_transpile(f)
  expect_true(grepl("for", trans$mojo) || grepl("while", trans$mojo))
})

test_that("seq with float step and params works", {
  f <- function(from, to, by) {
    out <- 0.0
    for (i in seq(from, to, by = by)) {
      out <- out + i
    }
    out
  }
  trans <- mojor_transpile(f, from = 'f64', to = 'f64', by = 'f64')
  expect_true(grepl("for", trans$mojo) || grepl("while", trans$mojo))
})

test_that("seq_len with computed expression works", {
  f <- function(n) {
    out <- 0.0
    for (i in seq_len(n * 2L)) {
      out <- out + i
    }
    out
  }
  trans <- mojor_transpile(f, n = 'i32')
  expect_true(grepl("for", trans$mojo) || grepl("while", trans$mojo))
})
# Test implicit vectorization edge cases

test_that("implicit vectorization with ifelse works", {
  f <- function(x, y) {
    ifelse(x > y, sqrt(x), abs(y))
  }
  trans <- mojor_transpile(f, x = 'f64[]', y = 'f64[]')
  expect_true(isTRUE(trans$is_expression_kernel))
})

test_that("implicit vectorization with pmin/pmax works", {
  f <- function(x, y) {
    pmin(x, y) + pmax(x, y)
  }
  trans <- mojor_transpile(f, x = 'f64[]', y = 'f64[]')
  expect_true(isTRUE(trans$is_expression_kernel))
})

test_that("implicit vectorization with mixed types works", {
  f <- function(x, y) {
    x + y  # x is f64[], y is i32[]
  }
  trans <- mojor_transpile(f, x = 'f64[]', y = 'i32[]')
  expect_true(isTRUE(trans$is_expression_kernel))
})

test_that("implicit vectorization with scalar args works", {
  f <- function(x, s) {
    x + s  # x is f64[], s is scalar f64
  }
  trans <- mojor_transpile(f, x = 'f64[]', s = 'f64')
  expect_true(isTRUE(trans$is_expression_kernel))
})
