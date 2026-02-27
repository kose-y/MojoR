context("compiled subset hard gate and schema plumbing")
test_that("mojor_transpile and mojor_build expose df_schema argument", {  expect_true("df_schema" %in% names(formals(mojor_transpile)))
  expect_true("df_schema" %in% names(formals(mojor_build)))
})

test_that("compiled subset type requests can be explicitly gated off", {  old_opts <- options(
    mojor.tier8_complete = FALSE,
    mojor.tier9_force = FALSE
  )
  on.exit(options(old_opts), add = TRUE)

  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = FALSE)

  f_df <- function(df) 1L
  expect_error(
    mojor_transpile(f_df, df = "df", df_schema = list(df = c(col = "f64[]"))),
    "currently gated by configuration"
  )
})

test_that("compiled subset requests are enabled by default after closeout", {  old_opts <- options(
    mojor.tier8_complete = getOption("mojor.tier8_complete"),
    mojor.tier9_force = getOption("mojor.tier9_force"),
    mojor.ir_only = getOption("mojor.ir_only")
  )
  on.exit(options(old_opts), add = TRUE)

  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = FALSE)
  options(mojor.tier8_complete = NULL, mojor.tier9_force = NULL, mojor.ir_only = FALSE)

  expect_true(.mojor_tier9_ready())

  f_df <- function(df) 1L
  expect_silent(
    mojor_transpile(f_df, df = "df", df_schema = list(df = c(col = "f64[]")))
  )
})

test_that("df_schema validator enforces named primitive column specs", {  expect_error(
    .mojor_validate_df_schema(c("df"), list(df = "df"), list(df = c("f64[]"))),
    "named list/vector"
  )

  expect_error(
    .mojor_validate_df_schema(c("df"), list(df = "df"), list(df = c(col = "f64"))),
    "must be one of"
  )

  ok <- .mojor_validate_df_schema(
    c("df"),
    list(df = "df"),
    list(df = c(a = "f64[]", b = "f32[]", c = "i32[]", d = "lgl[]", e = "chr[]"))
  )

  expect_true(is.list(ok))
  expect_true("df" %in% names(ok))
  expect_identical(ok$df$a, "f64[]")
  expect_identical(ok$df$b, "f32[]")
  expect_identical(ok$df$e, "chr[]")
})
