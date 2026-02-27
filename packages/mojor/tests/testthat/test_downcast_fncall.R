# Test automatic downcasting from function call results
# Related to Phase 5.2 follow-up: integer(length(x)) emission fix

test_that("follow-up: integer(length(x)) with explicit downcast from sqrt()", {  skip_if_no_mojo()

  # Pattern: integer(length(x)) + explicit as.integer() downcast
  code <- function(x) {
    out <- integer(length(x))
    for (i in seq_along(x)) {
      out[i] <- as.integer(sqrt(x[i]))
    }
    out
  }

  built <- mojor_build(
    code,
    x = "f64[]",
    name = "t_downcast_sqrt",
    cache = FALSE,
    load = TRUE
  )

  expect_true(!is.null(built))
  expect_true(built$success)

  # Test correctness
  x <- as.numeric(c(4, 9, 16, 25))
  mojo_result <- built$func(x)
  expect_equal(mojo_result, c(2L, 3L, 4L, 5L))
})

test_that("Implicit downcast from sqrt() to i32 (future feature)", {  skip_if_no_mojo()

  # Test: should this auto-downcast?
  code <- function(x) {
    out <- integer(length(x))
    for (i in seq_along(x)) {
      out[i] <- sqrt(x[i])  # No explicit as.integer()
    }
    out
  }

  # What happens?
  result <- try(mojor_build(code, x = "f64[]", cache = FALSE), silent = TRUE)

  if (inherits(result, "try-error")) {
    # Expected: requires explicit cast
    skip("Implicit downcast not yet supported")
  } else {
    # If it works, test correctness
    x <- as.numeric(c(4, 9, 16, 25))
    expect_equal(result$func(x), c(2L, 3L, 4L, 5L))
  }
})
