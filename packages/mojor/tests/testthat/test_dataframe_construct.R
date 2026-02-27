context("compiled subset data.frame return-boundary construction")
test_that("mojor_transpile rewrites terminal data.frame(...) into per-column kernels", {  old_opts <- options(
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
      b[i] <- x[i] * 2.0
    }
    data.frame(a = a, b = b)
  }

  trans <- mojor_transpile(
    f,
    x = "f64[]",
    n = "i32",
    name = "t_tier9_df_ctor_transpile"
  )

  expect_true(isTRUE(trans$tier9_ctor_rewritten))
  expect_identical(trans$tier9_return_ctor$kind, "data.frame")
  expect_identical(trans$tier9_return_ctor$names, c("a", "b"))
  expect_true(isTRUE(trans$tier9_ctor_runtime_only))
})

test_that("compiled subset build returns data.frame with R-parity structure", {  old_opts <- options(
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
      a[i] <- x[i] + 1.0
      b[i] <- x[i] * 3.0
    }
    data.frame(a = a, b = b)
  }

  built <- mojor_build(
    f,
    x = "f64[]",
    n = "i32",
    name = "t_tier9_df_ctor_build"
  )

  x <- runif(20)
  got <- built$func(x, 20L)
  ref <- f(x, 20L)
  expect_s3_class(got, "data.frame")
  expect_equal(got, ref, tolerance = 1e-10)
  expect_true(isTRUE(built$tier9_ctor_rewritten))
  expect_identical(built$tier9_return_ctor$kind, "data.frame")
  expect_identical(built$object_mode_kind, "tier9_ctor_runtime")
  expect_null(built$gpu_func)
  expect_null(built$gpu_func_raw)
})

test_that("compiled subset data.frame assembly enforces equal column lengths", {  expect_error(
    .mojor_tier9_runtime_make_data_frame(
      values = list(1:3, 1:2),
      col_names = c("a", "b"),
      context = "test"
    ),
    "column lengths must match"
  )
})

test_that("compiled subset data.frame assembly accepts chr[] columns", {  got <- .mojor_tier9_runtime_make_data_frame(
    values = list(c("a", "b"), c("x", "y")),
    col_names = c("left", "right"),
    context = "test"
  )
  expect_s3_class(got, "data.frame")
  expect_identical(got$left, c("a", "b"))
  expect_identical(got$right, c("x", "y"))
})

test_that("compiled subset rejects unsupported data.frame constructor arguments", {  old_opts <- options(
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
    data.frame(a = a, stringsAsFactors = TRUE)
  }

  expect_error(
    mojor_transpile(
      f,
      x = "f64[]",
      n = "i32",
      name = "t_tier9_df_ctor_bad_args"
    ),
    "does not support argument"
  )
})
