library(testthat)

test_that("elementwise in-place update supported", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- out[i] + x[i]
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "t_ew_inplace")
  expect_true(
    grepl("out[(i - 1)]", trans$mojo, fixed = TRUE) ||
      grepl("out[i]", trans$mojo, fixed = TRUE) ||
      grepl("out[Int((i - 1))]", trans$mojo, fixed = TRUE) ||
      grepl("out[Int(i)]", trans$mojo, fixed = TRUE)
  )
  expect_true(grepl("+", trans$mojo, fixed = TRUE))
})

test_that("ifelse() vectorized branches supported", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- ifelse(x[i] > 0, x[i], -x[i])
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "t_ew_ifelse")
  expect_true(grepl("if", trans$mojo))
  expect_true(
    grepl("out[(i - 1)]", trans$mojo, fixed = TRUE) ||
      grepl("out[i]", trans$mojo, fixed = TRUE) ||
      grepl("out[Int((i - 1))]", trans$mojo, fixed = TRUE) ||
      grepl("out[Int(i)]", trans$mojo, fixed = TRUE)
  )
})

test_that("broadcast scalar and recycle supported", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  built <- mojor_build(
    f,
    x = "f64[]",
    y = "f64[]",
    broadcast = "recycle",
    name = "t_ew_broadcast",
    cache = FALSE,
    load = TRUE
  )
  x <- as.double(1:4)
  y <- as.double(c(10, 20))
  res <- built$func(x, y)
  expect_equal(res, x + rep(y, length.out = length(x)))
})
