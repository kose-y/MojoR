library(testthat)

test_that("loop variable used as value is 1-based", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- i
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "mojor_loop_var_value")
  expect_true(
    grepl("out\\[\\(i - 1\\)\\] = (Float64\\()?i\\)?", trans$mojo) ||
      grepl("out\\[i - 1\\] = (Float64\\()?i\\)?", trans$mojo) ||
      grepl("out\\[Int\\(\\(i - 1\\)\\)\\] = (Float64\\()?i\\)?", trans$mojo) ||
      grepl("out\\[Int\\(i - 1\\)\\] = (Float64\\()?i\\)?", trans$mojo)
  )
})

test_that("loop variable in scalar accumulation is 1-based", {  f <- function(x) {
    acc <- 0
    for (i in seq_along(x)) {
      acc <- acc + i
    }
    acc
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "mojor_loop_var_acc")
  expect_true(grepl("acc = .*i", trans$mojo))
})

test_that("loop variable in conditions is 1-based", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      if (i == 1L) {
        out[i] <- x[i]
      } else {
        out[i] <- x[i] + 1
      }
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "mojor_loop_var_cond")
  expect_true(
    grepl("i == 1", trans$mojo, fixed = TRUE) ||
      grepl("Int\\(i\\) == Int\\(1\\)", trans$mojo)
  )
})
