test_that("transpile route metadata uses loop route for normalized no-loop expressions", {
  f <- function(x, y) x + y
  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]")

  expect_true(is.list(trans$transpile_route))
  expect_identical(trans$transpile_route$route, "loop")
  expect_true(isTRUE(trans$transpile_route$normalization_applied))
  expect_identical(trans$transpile_route$normalized_kind, "vectorized_expression_loop")
  expect_true(isTRUE(trans$transpile_route$expression_candidate))
})

test_that("transpile route metadata marks loop dispatch for explicit loops", {
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + 1
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]")

  expect_true(is.list(trans$transpile_route))
  expect_identical(trans$transpile_route$route, "loop")
  expect_identical(trans$transpile_route$reason, "loops_present")
})

test_that("transpile route metadata marks no-loop any/all synthesis as loop path", {
  f <- function(x) any(TRUE)
  trans <- mojor_transpile(f, x = "f64[]")

  expect_true(is.list(trans$transpile_route))
  expect_identical(trans$transpile_route$route, "loop")
  expect_identical(trans$transpile_route$reason, "no_loop_any_all_synthesis")
})

test_that("normalized no-loop scalar expressions keep expression payload fields", {
  f <- function(x, k) k + 1L
  trans <- mojor_transpile(f, x = "f64[]", k = "i32")

  expect_true(is.list(trans$transpile_route))
  expect_identical(trans$transpile_route$route, "loop")
  expect_identical(trans$transpile_route$normalized_kind, "ir_scalar_call_binop")
  expect_true(isTRUE(trans$is_expression_kernel))
  expect_identical(trans$return_type, "Int32")
})

test_that("route classifier falls back to unified loop lane when normalization is disabled", {
  route <- .mojor_classify_transpile_route(
    blocks = list(quote(x + y)),
    loops = list(),
    no_loop_scalar_any_all = FALSE,
    gpu_jit_mode = "auto",
    args = c("x", "y"),
    types = list(x = "f64[]", y = "f64[]"),
    normalization_enabled = FALSE
  )

  expect_identical(route$route, "loop")
  expect_identical(route$reason, "no_loop_loop_ir_fallback")
  expect_false(isTRUE(route$normalization$applied))
})
