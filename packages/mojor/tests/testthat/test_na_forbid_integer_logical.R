# =============================================================================
# NA forbid mode: integer/logical elementwise guards
# =============================================================================

library(testthat)

test_that("na_mode='forbid' emits Int32 sentinel guards for i32/lgl arithmetic", {  f <- function(x, flag) {
    out <- integer(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + flag
    }
    out
  }

  trans <- mojor_transpile(
    f,
    x = "i32[]",
    flag = "lgl",
    name = "t_na_forbid_i32_lgl_codegen",
    na_mode = "forbid",
    ir_only = TRUE
  )
  code <- paste(trans$mojo, collapse = "\n")

  expect_match(code, "Int32(flag) == Int32(-2147483648)", fixed = TRUE)
  expect_match(code, "Int32(_mojor_read_i32(", fixed = TRUE)
  expect_match(code, "== Int32(-2147483648)", fixed = TRUE)
})

test_that("na_mode='forbid' rejects NA for i32[] + lgl scalar arithmetic", {  skip_if_no_mojo()

  f <- function(x, flag) {
    out <- integer(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + flag
    }
    out
  }

  built <- mojor_build(
    f,
    x = "i32[]",
    flag = "lgl",
    name = "t_na_forbid_i32_lgl_rt",
    cache = FALSE,
    load = TRUE,
    na_mode = "forbid"
  )

  x_ok <- c(1L, 2L, 3L)
  expect_equal(built$func(x_ok, TRUE), f(x_ok, TRUE))
  expect_error(built$func(c(1L, NA_integer_, 3L), TRUE), "NA/NaN not supported", fixed = TRUE)
  expect_error(built$func(x_ok, NA), "NA/NaN not supported", fixed = TRUE)
})
