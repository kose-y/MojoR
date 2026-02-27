context("compiled subset minimal homogeneous list output")
test_that("mojor_transpile rewrites terminal list(...) into per-item kernels", {  old_opts <- options(
    mojor.tier8_complete = FALSE,
    mojor.tier9_force = TRUE
  )
  on.exit(options(old_opts), add = TRUE)

  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = FALSE)

  f <- function(x, n) {
    a <- numeric(n)
    b <- numeric(n)
    for (i in seq_len(n)) {
      a[i] <- x[i]
      b[i] <- x[i] + 5.0
    }
    list(a = a, b = b)
  }

  trans <- mojor_transpile(
    f,
    x = "f64[]",
    n = "i32",
    name = "t_tier9_list_ctor_transpile"
  )

  expect_true(isTRUE(trans$tier9_ctor_rewritten))
  expect_identical(trans$tier9_return_ctor$kind, "list")
  expect_identical(trans$tier9_return_ctor$names, c("a", "b"))
  expect_true(isTRUE(trans$tier9_ctor_runtime_only))
})

test_that("compiled subset build returns homogeneous list", {  old_opts <- options(
    mojor.tier8_complete = FALSE,
    mojor.tier9_force = TRUE
  )
  on.exit(options(old_opts), add = TRUE)

  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = FALSE)

  f <- function(x, n) {
    a <- numeric(n)
    b <- numeric(n)
    for (i in seq_len(n)) {
      a[i] <- x[i] * 2.0
      b[i] <- x[i] + 1.0
    }
    list(a = a, b = b)
  }

  built <- mojor_build(
    f,
    x = "f64[]",
    n = "i32",
    name = "t_tier9_list_ctor_build"
  )

  x <- runif(16)
  got <- built$func(x, 16L)
  ref <- f(x, 16L)
  expect_true(is.list(got))
  expect_equal(got, ref, tolerance = 1e-10)
  expect_true(isTRUE(built$tier9_ctor_rewritten))
  expect_identical(built$tier9_return_ctor$kind, "list")
  expect_identical(built$object_mode_kind, "tier9_ctor_runtime")
  expect_null(built$gpu_func)
  expect_null(built$gpu_func_raw)
})

test_that("compiled subset list assembly rejects heterogeneous element types", {  expect_error(
    .mojor_tier9_runtime_make_list(
      values = list(1:3, c(TRUE, FALSE, TRUE)),
      item_names = c("a", "b"),
      context = "test"
    ),
    "homogeneous atomic vector element types"
  )
})

test_that("compiled subset list assembly accepts homogeneous character vectors", {  out <- .mojor_tier9_runtime_make_list(
    values = list(c("a", "b"), c("c", "d")),
    item_names = c("first", "second"),
    context = "test"
  )
  expect_true(is.list(out))
  expect_identical(names(out), c("first", "second"))
  expect_identical(out$first, c("a", "b"))
  expect_identical(out$second, c("c", "d"))
})

test_that("compiled subset rejects nested list construction", {  old_opts <- options(
    mojor.tier8_complete = FALSE,
    mojor.tier9_force = TRUE
  )
  on.exit(options(old_opts), add = TRUE)

  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = FALSE)

  f <- function(x, n) {
    a <- numeric(n)
    for (i in seq_len(n)) {
      a[i] <- x[i]
    }
    list(a = a, b = list(a))
  }

  expect_error(
    mojor_transpile(
      f,
      x = "f64[]",
      n = "i32",
      name = "t_tier9_list_ctor_nested"
    ),
    "supports only precomputed vector variables"
  )
})
