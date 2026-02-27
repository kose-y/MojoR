library(testthat)

test_that("mojor_build ignores ambient same-name symbols for declared type specs", {
  skip_if_no_mojo()

  # These ambient bindings should not participate in validation when x/idx are
  # declared with explicit type strings.
  x <- 1:3
  idx <- c(1L, 2L)
  f <- function(x, idx) {
    out <- numeric(1)
    out[1] <- x[idx]
    out
  }

  expect_no_error(
    mojor_build(
      f,
      x = "f64[]",
      idx = "i32",
      name = "t_preflight_declared_types_ignore_env",
      cache = FALSE,
      load = FALSE
    )
  )
})

test_that("mojor_build still rejects invalid explicit value specs", {
  f <- function(x) {
    out <- numeric(1)
    out[1] <- 1.0
    out
  }

  expect_error(
    mojor_build(
      f,
      x = list(1, 2, 3),
      name = "t_preflight_invalid_explicit_spec",
      cache = FALSE,
      load = FALSE
    ),
    "unsupported type spec"
  )
})
