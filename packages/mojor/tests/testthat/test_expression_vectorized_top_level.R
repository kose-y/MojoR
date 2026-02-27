library(testthat)

test_that("expression-only supports implicit top-level binary vector arithmetic", {
  f <- function(x, y) {
    x + y * 2
  }

  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_true(isTRUE(trans$is_vector_output))
  expect_identical(trans$out_type, "f64[]")
})

test_that("runtime: implicit top-level binary vector arithmetic matches R", {
  skip_if_no_mojo()

  f <- function(x, y) {
    x + y * 2
  }

  built <- mojor_build(f, x = "f64[]", y = "f64[]", cache = FALSE, load = TRUE)
  x <- as.double(c(1, 2, 3, 4))
  y <- as.double(c(5, 6, 7, 8))
  expect_equal(built$func(x, y), f(x, y))
})

test_that("expression-only supports implicit top-level arity-4 vector arithmetic", {
  f <- function(a, b, c, d) {
    a + b - c + d * 0.5
  }

  trans <- mojor_transpile(f, a = "f64[]", b = "f64[]", c = "f64[]", d = "f64[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_true(isTRUE(trans$is_vector_output))
  expect_identical(trans$out_type, "f64[]")
})

test_that("runtime: implicit top-level arity-4 vector arithmetic matches R", {
  skip_if_no_mojo()

  f <- function(a, b, c, d) {
    a + b - c + d * 0.5
  }

  built <- mojor_build(f, a = "f64[]", b = "f64[]", c = "f64[]", d = "f64[]", cache = FALSE, load = TRUE)
  a <- as.double(c(1, 2, 3, 4))
  b <- as.double(c(5, 6, 7, 8))
  c <- as.double(c(2, 1, 2, 1))
  d <- as.double(c(10, 20, 30, 40))
  expect_equal(built$func(a, b, c, d), f(a, b, c, d))
})

test_that("expression-only supports implicit unary vector arithmetic", {
  f <- function(x) {
    (x * 2) + 1
  }

  trans <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_true(isTRUE(trans$is_vector_output))
  expect_identical(trans$out_type, "f64[]")
})

# --- Scalar arg support ---

test_that("expression-only supports f64[] with f64 scalar arg", {
  f <- function(x, k) x * k

  trans <- mojor_transpile(f, x = "f64[]", k = "f64", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_true(isTRUE(trans$is_vector_output))
  expect_identical(trans$out_type, "f64[]")
})

test_that("runtime: f64[] with f64 scalar arg matches R", {
  skip_if_no_mojo()

  f <- function(x, k) x * k

  built <- mojor_build(f, x = "f64[]", k = "f64", cache = FALSE, load = TRUE)
  x <- as.double(c(1, 2, 3, 4))
  expect_equal(built$func(x, 2.5), f(x, 2.5))
})

test_that("expression-only supports i32[] with i32 scalar arg", {
  f <- function(x, k) x + k

  trans <- mojor_transpile(f, x = "i32[]", k = "i32", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_identical(trans$out_type, "i32[]")
})

test_that("expression-only supports f64[] with i32 scalar arg (promotion)", {
  f <- function(x, k) x * k

  trans <- mojor_transpile(f, x = "f64[]", k = "i32", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_identical(trans$out_type, "f64[]")
})

test_that("expression-only supports comparison with scalar threshold", {
  f <- function(x, k) x > k

  trans <- mojor_transpile(f, x = "f64[]", k = "f64", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_identical(trans$out_type, "lgl[]")
})

test_that("runtime: comparison with scalar threshold matches R", {
  skip_if_no_mojo()

  f <- function(x, k) x > k

  built <- mojor_build(f, x = "f64[]", k = "f64", cache = FALSE, load = TRUE)
  x <- as.double(c(1, 5, 3, 8))
  expect_equal(built$func(x, 4.0), f(x, 4.0))
})

test_that("expression-only supports multi-vector + scalar", {
  f <- function(x, k, y) x * k + y

  trans <- mojor_transpile(f, x = "f64[]", k = "f64", y = "f64[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_identical(trans$out_type, "f64[]")
})

test_that("runtime: multi-vector + scalar matches R", {
  skip_if_no_mojo()

  f <- function(x, k, y) x * k + y

  built <- mojor_build(f, x = "f64[]", k = "f64", y = "f64[]", cache = FALSE, load = TRUE)
  x <- as.double(c(1, 2, 3))
  y <- as.double(c(10, 20, 30))
  expect_equal(built$func(x, 2.0, y), f(x, 2.0, y))
})

# --- i32[] support ---

test_that("expression-only supports i32[] implicit vectorized arithmetic", {
  f <- function(x, y) x + y * 2L

  trans <- mojor_transpile(f, x = "i32[]", y = "i32[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_true(isTRUE(trans$is_vector_output))
  expect_identical(trans$out_type, "i32[]")
})

test_that("runtime: i32[] implicit vectorized arithmetic matches R", {
  skip_if_no_mojo()

  f <- function(x, y) x + y * 2L

  built <- mojor_build(f, x = "i32[]", y = "i32[]", cache = FALSE, load = TRUE)
  x <- as.integer(c(1, 2, 3, 4))
  y <- as.integer(c(5, 6, 7, 8))
  expect_equal(built$func(x, y), f(x, y))
})

# --- lgl[] support ---

test_that("expression-only supports lgl[] implicit vectorized logical ops", {
  f <- function(x, y) x & y

  trans <- mojor_transpile(f, x = "lgl[]", y = "lgl[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_true(isTRUE(trans$is_vector_output))
  expect_identical(trans$out_type, "lgl[]")
})

test_that("runtime: lgl[] implicit vectorized logical ops match R", {
  skip_if_no_mojo()

  f <- function(x, y) x & y

  built <- mojor_build(f, x = "lgl[]", y = "lgl[]", cache = FALSE, load = TRUE)
  x <- c(TRUE, FALSE, TRUE, FALSE)
  y <- c(TRUE, TRUE, FALSE, FALSE)
  expect_equal(built$func(x, y), f(x, y))
})

test_that("expression-only supports unary ! on lgl[]", {
  f <- function(x) !x

  trans <- mojor_transpile(f, x = "lgl[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_true(isTRUE(trans$is_vector_output))
  expect_identical(trans$out_type, "lgl[]")
})

# --- Mixed-type support ---

test_that("expression-only supports mixed f64[]/i32[] with promotion", {
  f <- function(x, y) x + y * 2.5

  trans <- mojor_transpile(f, x = "f64[]", y = "i32[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_true(isTRUE(trans$is_vector_output))
  expect_identical(trans$out_type, "f64[]")
})

test_that("runtime: mixed f64[]/i32[] promotion matches R", {
  skip_if_no_mojo()

  f <- function(x, y) x + y * 2.5

  built <- mojor_build(f, x = "f64[]", y = "i32[]", cache = FALSE, load = TRUE)
  x <- as.double(c(1.5, 2.5, 3.5))
  y <- as.integer(c(5, 6, 7))
  expect_equal(built$func(x, y), f(x, y))
})

test_that("expression-only supports mixed i32[]/lgl[] with promotion", {
  f <- function(x, y) x + y

  trans <- mojor_transpile(f, x = "i32[]", y = "lgl[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_true(isTRUE(trans$is_vector_output))
  expect_identical(trans$out_type, "i32[]")
})

# --- Comparison operators ---

test_that("expression-only supports f64[] comparison returning lgl[]", {
  f <- function(x, y) x > y

  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_true(isTRUE(trans$is_vector_output))
  expect_identical(trans$out_type, "lgl[]")
})

test_that("runtime: f64[] comparison matches R", {
  skip_if_no_mojo()

  f <- function(x, y) x > y

  built <- mojor_build(f, x = "f64[]", y = "f64[]", cache = FALSE, load = TRUE)
  x <- as.double(c(1, 5, 3, 8))
  y <- as.double(c(2, 4, 6, 7))
  expect_equal(built$func(x, y), f(x, y))
})

test_that("expression-only supports all six comparison operators", {
  for (op in c("==", "!=", "<", ">", "<=", ">=")) {
    f <- eval(parse(text = sprintf("function(x, y) x %s y", op)))
    trans <- mojor_transpile(f, x = "f64[]", y = "f64[]", ir_only = TRUE)
    expect_true(isTRUE(trans$is_expression_kernel), info = paste("op:", op))
    expect_identical(trans$out_type, "lgl[]", info = paste("op:", op))
  }
})

test_that("expression-only supports i32[] comparison returning lgl[]", {
  f <- function(x, y) x > y

  trans <- mojor_transpile(f, x = "i32[]", y = "i32[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_identical(trans$out_type, "lgl[]")
})

# --- Complex mixed expressions ---

test_that("expression-only supports mixed comparison and logical", {
  f <- function(x, y, z) (x > y) & (z <= 0.5)

  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]", z = "f64[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_identical(trans$out_type, "lgl[]")
})

test_that("runtime: mixed comparison and logical matches R", {
  skip_if_no_mojo()

  f <- function(x, y, z) (x > y) & (z <= 0.5)

  built <- mojor_build(f, x = "f64[]", y = "f64[]", z = "f64[]", cache = FALSE, load = TRUE)
  x <- as.double(c(1, 5, 3))
  y <- as.double(c(2, 4, 6))
  z <- as.double(c(0.1, 0.6, 0.3))
  expect_equal(built$func(x, y, z), f(x, y, z))
})

# --- ifelse support ---

test_that("expression-only supports basic ifelse", {
  f <- function(x) ifelse(x > 0, x, -x)

  trans <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_true(isTRUE(trans$is_vector_output))
  expect_identical(trans$out_type, "f64[]")
})

test_that("runtime: basic ifelse matches R", {
  skip_if_no_mojo()

  f <- function(x) ifelse(x > 0, x, -x)

  built <- mojor_build(f, x = "f64[]", cache = FALSE, load = TRUE)
  x <- as.double(c(-3, -1, 0, 2, 5))
  expect_equal(built$func(x), f(x))
})

test_that("expression-only supports two-vector ifelse", {
  f <- function(x, y) ifelse(x > y, x, y)

  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_identical(trans$out_type, "f64[]")
})

test_that("expression-only supports ifelse with scalar arg", {
  f <- function(x, k) ifelse(x > k, x, 0)

  trans <- mojor_transpile(f, x = "f64[]", k = "f64", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_identical(trans$out_type, "f64[]")
})

test_that("runtime: ifelse with scalar arg matches R", {
  skip_if_no_mojo()

  f <- function(x, k) ifelse(x > k, x, 0)

  built <- mojor_build(f, x = "f64[]", k = "f64", cache = FALSE, load = TRUE)
  x <- as.double(c(1, 5, 3, 8))
  expect_equal(built$func(x, 4.0), f(x, 4.0))
})

test_that("expression-only supports nested ifelse", {
  f <- function(x) ifelse(x > 0, x, ifelse(x > -1, 0, x))

  trans <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_identical(trans$out_type, "f64[]")
})

test_that("expression-only supports expression-form if/else", {
  f <- function(x) if (x > 0) x else -x

  trans <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_identical(trans$out_type, "f64[]")
})

test_that("expression-only supports ifelse returning lgl[]", {
  f <- function(x) ifelse(x > 0, TRUE, FALSE)

  trans <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_identical(trans$out_type, "lgl[]")
})

# --- Math function support ---

test_that("expression-only supports unary math functions", {
  for (fn in c("abs", "sqrt", "sin", "cos", "exp", "log", "floor", "ceiling")) {
    f <- eval(parse(text = sprintf("function(x) %s(x)", fn)))
    trans <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
    expect_true(isTRUE(trans$is_expression_kernel), info = paste("fn:", fn))
    expect_identical(trans$out_type, "f64[]", info = paste("fn:", fn))
  }
})

test_that("runtime: unary math functions match R", {
  skip_if_no_mojo()

  f <- function(x) sqrt(x) + abs(x)

  built <- mojor_build(f, x = "f64[]", cache = FALSE, load = TRUE)
  x <- as.double(c(1, 4, 9, 16))
  expect_equal(built$func(x), f(x))
})

test_that("expression-only supports composed math expressions", {
  f <- function(x) exp(-x * x / 2) / sqrt(2 * 3.14159265)

  trans <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_identical(trans$out_type, "f64[]")
})

test_that("runtime: composed math expression matches R", {
  skip_if_no_mojo()

  f <- function(x) exp(-x * x / 2) / sqrt(2 * 3.14159265)

  built <- mojor_build(f, x = "f64[]", cache = FALSE, load = TRUE)
  x <- as.double(c(-2, -1, 0, 1, 2))
  expect_equal(built$func(x), f(x))
})

test_that("expression-only supports atan2 binary math function", {
  f <- function(x, y) atan2(x, y)

  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_identical(trans$out_type, "f64[]")
})

test_that("runtime: atan2 matches R", {
  skip_if_no_mojo()

  f <- function(x, y) atan2(x, y)

  built <- mojor_build(f, x = "f64[]", y = "f64[]", cache = FALSE, load = TRUE)
  x <- as.double(c(0, 1, 0, -1))
  y <- as.double(c(1, 0, -1, 0))
  expect_equal(built$func(x, y), f(x, y))
})

test_that("expression-only supports math + comparison + ifelse combo", {
  f <- function(x) ifelse(abs(x) > 1, sqrt(abs(x)), x)

  trans <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_identical(trans$out_type, "f64[]")
})

test_that("runtime: math + comparison + ifelse combo matches R", {
  skip_if_no_mojo()

  f <- function(x) ifelse(abs(x) > 1, sqrt(abs(x)), x)

  built <- mojor_build(f, x = "f64[]", cache = FALSE, load = TRUE)
  x <- as.double(c(-2, -0.5, 0, 0.5, 2))
  expect_equal(built$func(x), f(x))
})

test_that("expression-only supports sign()", {
  f <- function(x) sign(x)

  trans <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_identical(trans$out_type, "f64[]")
})

test_that("runtime: sign() matches R", {
  skip_if_no_mojo()

  f <- function(x) sign(x)

  built <- mojor_build(f, x = "f64[]", cache = FALSE, load = TRUE)
  x <- as.double(c(-3, -0.1, 0, 0.5, 2))
  expect_equal(built$func(x), f(x))
})

# --- Type check predicates ---

test_that("expression-only supports is.na()", {
  f <- function(x) is.na(x)

  trans <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
})

test_that("expression-only supports is.nan()", {
  f <- function(x) is.nan(x)

  trans <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
})

test_that("expression-only supports is.finite()", {
  f <- function(x) is.finite(x)

  trans <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
})

test_that("expression-only supports is.infinite()", {
  f <- function(x) is.infinite(x)

  trans <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
})

test_that("runtime: is.na() matches R", {
  skip_if_no_mojo()

  f <- function(x) is.na(x)

  built <- mojor_build(f, x = "f64[]", cache = FALSE, load = TRUE)
  x <- c(1.0, NaN, 3.0, NA_real_, 5.0)
  expect_equal(built$func(x), f(x))
})

test_that("runtime: is.finite() matches R", {
  skip_if_no_mojo()

  f <- function(x) is.finite(x)

  built <- mojor_build(f, x = "f64[]", cache = FALSE, load = TRUE)
  x <- c(1.0, Inf, -Inf, NaN, 0.0)
  expect_equal(built$func(x), f(x))
})

# --- Type cast functions ---

test_that("expression-only supports as.integer() in expression", {
  f <- function(x) as.integer(x > 0)

  trans <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
})

test_that("expression-only supports as.double()", {
  f <- function(x) as.double(x)

  trans <- mojor_transpile(f, x = "i32[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
})

test_that("expression-only supports as.logical()", {
  f <- function(x) as.logical(x)

  trans <- mojor_transpile(f, x = "i32[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
})

test_that("runtime: as.integer(x > 0) matches R", {
  skip_if_no_mojo()

  f <- function(x) as.integer(x > 0)

  built <- mojor_build(f, x = "f64[]", cache = FALSE, load = TRUE)
  x <- c(-2.0, -0.5, 0.0, 0.5, 3.0)
  expect_equal(built$func(x), f(x))
})

# --- xor ---

test_that("expression-only supports xor()", {
  f <- function(x, y) xor(x > 0, y > 0)

  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
})

test_that("runtime: xor() matches R", {
  skip_if_no_mojo()

  f <- function(x, y) xor(x > 0, y > 0)

  built <- mojor_build(f, x = "f64[]", y = "f64[]", cache = FALSE, load = TRUE)
  x <- c(-1.0, 1.0, -1.0, 1.0)
  y <- c(-1.0, -1.0, 1.0, 1.0)
  expect_equal(built$func(x, y), f(x, y))
})

# --- Composed expressions with new functions ---

test_that("expression-only supports ifelse(is.na(x), replacement, x)", {
  f <- function(x, fill) ifelse(is.na(x), fill, x)

  trans <- mojor_transpile(f, x = "f64[]", fill = "f64", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
})

test_that("runtime: ifelse(is.na(x), 0, x) matches R", {
  skip_if_no_mojo()

  f <- function(x, fill) ifelse(is.na(x), fill, x)

  built <- mojor_build(f, x = "f64[]", fill = "f64", cache = FALSE, load = TRUE)
  x <- c(1.0, NaN, 3.0, NA_real_, 5.0)
  expect_equal(built$func(x, 0.0), f(x, 0.0))
})

test_that("expression-only supports as.double(x) + is.finite(y) combo", {
  f <- function(x, y) as.double(is.finite(y)) * x

  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
})

# --- Multi-statement let-binding bodies ---

test_that("expression-only supports multi-statement let-binding body", {
  f <- function(x, y) {
    tmp <- x + y
    sqrt(tmp)
  }

  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
})

test_that("expression-only supports chained let-bindings", {
  f <- function(x) {
    a <- x * 2
    b <- a + 1
    b
  }

  trans <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
})

test_that("expression-only supports multiple temps in let-bindings", {
  f <- function(x, y) {
    s <- x + y
    d <- x - y
    s * d
  }

  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
})

test_that("expression-only supports let-binding with explicit return", {
  f <- function(x, y) {
    tmp <- x + y
    return(tmp * 2)
  }

  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
})

test_that("expression-only supports let-binding with scalar args", {
  f <- function(x, k) {
    shifted <- x - k
    abs(shifted)
  }

  trans <- mojor_transpile(f, x = "f64[]", k = "f64", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
})

test_that("runtime: multi-statement let-binding matches R", {
  skip_if_no_mojo()

  f <- function(x, y) {
    tmp <- x + y
    sqrt(tmp)
  }

  built <- mojor_build(f, x = "f64[]", y = "f64[]", cache = FALSE, load = TRUE)
  x <- c(1.0, 4.0, 9.0, 16.0)
  y <- c(0.0, 0.0, 0.0, 0.0)
  expect_equal(built$func(x, y), f(x, y))
})

test_that("runtime: chained let-bindings match R", {
  skip_if_no_mojo()

  f <- function(x) {
    a <- x * 2
    b <- a + 1
    b
  }

  built <- mojor_build(f, x = "f64[]", cache = FALSE, load = TRUE)
  x <- c(1.0, 2.0, 3.0, 4.0)
  expect_equal(built$func(x), f(x))
})

test_that("runtime: multiple temps match R", {
  skip_if_no_mojo()

  f <- function(x, y) {
    s <- x + y
    d <- x - y
    s * d
  }

  built <- mojor_build(f, x = "f64[]", y = "f64[]", cache = FALSE, load = TRUE)
  x <- c(5.0, 3.0, 7.0)
  y <- c(2.0, 1.0, 3.0)
  expect_equal(built$func(x, y), f(x, y))
})

# --- abs2 ---

test_that("expression-only supports abs2()", {
  f <- function(x) abs2(x)

  trans <- mojor_transpile(f, x = "f64[]", ir_only = TRUE)
  expect_true(isTRUE(trans$is_expression_kernel))
})

test_that("runtime: abs2() matches x*x", {
  skip_if_no_mojo()

  f <- function(x) abs2(x)
  ref <- function(x) x * x

  built <- mojor_build(f, x = "f64[]", cache = FALSE, load = TRUE)
  x <- c(-3.0, -1.0, 0.0, 2.0, 5.0)
  expect_equal(built$func(x), ref(x))
})
