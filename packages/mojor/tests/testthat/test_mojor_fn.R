library(testthat)

test_that("mojor_fn requires type hints for function inputs", {  f <- function(x, y) {
    x + y
  }

  expect_error(mojor_fn(f, x = "f64[]"), "missing type hints")
  expect_error(mojor_fn(f), "missing type hints")
})

test_that("mojor_fn compiles a simple elementwise loop", {  skip_if_no_mojo()

  fast_double <- mojor_fn(function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }, x = "f64[]", cache = FALSE)

  x <- as.double(1:5)
  expect_equal(fast_double(x), x * 2)
})

test_that("mojor_fn accepts string bodies", {  skip_if_no_mojo()

  fast_add <- mojor_fn(
    "fast_add(x: f64[], y: f64[])",
    "out <- numeric(length(x)); for (i in seq_along(x)) { out[i] <- x[i] + y[i] }; out",
    cache = FALSE
  )

  x <- as.double(1:5)
  y <- as.double(6:10)
  expect_equal(fast_add(x, y), x + y)
})

test_that("mojor_fn rejects conflicting type hints with signature string", {  skip_if_no_mojo()

  expect_error(
    mojor_fn("(x: f64[])", "out <- x; out", x = "i32[]", cache = FALSE),
    "type hints conflict"
  )
})

test_that("mojor_fn forwards build flags", {  skip_if_no_mojo()

  built <- mojor_fn(function(x) {
    acc <- 0
    for (i in seq_along(x)) acc <- acc + x[i]
    acc
  }, x = "f64[]", reduction = "tree", load = FALSE, cache = FALSE)

  expect_identical(built$trans$reduction, "tree")
})

test_that("mojor_fn allows omitted function name", {  skip_if_no_mojo()

  fast_double <- mojor_fn(
    "(x: f64[])",
    "out <- numeric(length(x)); for (i in seq_along(x)) { out[i] <- x[i] * 2 }; out",
    cache = FALSE
  )

  x <- as.double(1:4)
  expect_equal(fast_double(x), x * 2)
})
