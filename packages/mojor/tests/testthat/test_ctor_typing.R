context("compiled subset constructor typing")
test_that("compiled subset list constructor rejects heterogeneous known argument types", {  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = FALSE)
  old_opts <- options(
    mojor.tier8_complete = FALSE,
    mojor.tier9_force = TRUE
  )
  on.exit(options(old_opts), add = TRUE)

  f <- function(x, y) {
    list(x = x, y = y)
  }

  expect_error(
    mojor_build(
      f,
      x = "f64[]",
      y = "i32[]",
      name = "t_tier9_ctor_hetero"
    ),
    "homogeneous known argument types"
  )
})

test_that("compiled subset list constructor accepts homogeneous chr[] argument types", {  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = FALSE)
  old_opts <- options(
    mojor.tier8_complete = FALSE,
    mojor.tier9_force = TRUE
  )
  on.exit(options(old_opts), add = TRUE)

  f <- function(x) {
    list(x = x)
  }

  built <- mojor_build(
    f,
    x = "chr[]",
    name = "t_tier9_ctor_chr"
  )

  got <- built$func(c("a", "b"))
  expect_true(is.list(got))
  expect_identical(got$x, c("a", "b"))
})

test_that("compiled subset data.frame constructor typing accepts chr[] argument types", {  ctor <- list(
    kind = "data.frame",
    entries = list(list(expr = quote(x)))
  )
  expect_silent(.mojor_tier9_validate_ctor_arg_specs(
    ctor = ctor,
    arg_specs = list(x = "chr[]"),
    context = "test"
  ))
})

test_that("compiled subset constructor typing accepts f32[] argument types", {  ctor_list <- list(
    kind = "list",
    entries = list(list(expr = quote(x)))
  )
  expect_silent(.mojor_tier9_validate_ctor_arg_specs(
    ctor = ctor_list,
    arg_specs = list(x = "f32[]"),
    context = "test"
  ))

  ctor_df <- list(
    kind = "data.frame",
    entries = list(list(expr = quote(x)))
  )
  expect_silent(.mojor_tier9_validate_ctor_arg_specs(
    ctor = ctor_df,
    arg_specs = list(x = "f32[]"),
    context = "test"
  ))
})

test_that("compiled subset constructor rejects scalar known argument types", {  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = FALSE)
  old_opts <- options(
    mojor.tier8_complete = FALSE,
    mojor.tier9_force = TRUE
  )
  on.exit(options(old_opts), add = TRUE)

  f <- function(x) {
    list(x = x)
  }

  expect_error(
    mojor_build(
      f,
      x = "f64",
      name = "t_tier9_ctor_scalar"
    ),
    "must have a vector type"
  )
})

test_that("compiled subset return constructors are supported in strict ir_only lanes", {  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = TRUE)
  old_opts <- options(
    mojor.tier8_complete = FALSE,
    mojor.tier9_force = TRUE
  )
  on.exit(options(old_opts), add = TRUE)

  f_df <- function(x) {
    data.frame(x = x)
  }
  trans_df <- mojor_transpile(
    f_df,
    x = "f64[]",
    name = "t_tier9_ctor_ir_only_df"
  )
  expect_true(isTRUE(trans_df$tier9_ctor_rewritten))
  expect_true(isTRUE(trans_df$tier9_ctor_runtime_only))
  expect_identical(trans_df$tier9_return_ctor$kind, "data.frame")

  f_list <- function(x) {
    list(x = x)
  }
  trans_list <- mojor_transpile(
    f_list,
    x = "f64[]",
    name = "t_tier9_ctor_ir_only_list"
  )
  expect_true(isTRUE(trans_list$tier9_ctor_rewritten))
  expect_true(isTRUE(trans_list$tier9_ctor_runtime_only))
  expect_identical(trans_list$tier9_return_ctor$kind, "list")

  built_df <- mojor_build(
    f_df,
    x = "f64[]",
    name = "t_tier9_ctor_ir_only_build_df",
    object_mode = "off"
  )
  got_df <- built_df$func(c(1, 2, 3))
  expect_s3_class(got_df, "data.frame")
  expect_identical(got_df$x, c(1, 2, 3))

  built_list <- mojor_build(
    f_list,
    x = "f64[]",
    name = "t_tier9_ctor_ir_only_build_list",
    object_mode = "off"
  )
  got_list <- built_list$func(c(1, 2, 3))
  expect_true(is.list(got_list))
  expect_identical(got_list$x, c(1, 2, 3))
})
