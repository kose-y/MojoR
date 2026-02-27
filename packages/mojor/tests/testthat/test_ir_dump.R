library(testthat)

collect_ir_kinds <- function(node) {
  kinds <- character(0)
  if (is.list(node)) {
    if (!is.null(node$kind)) {
      kinds <- c(kinds, as.character(node$kind))
    }
    for (val in node) {
      kinds <- c(kinds, collect_ir_kinds(val))
    }
  }
  unique(kinds)
}

test_that("mojor_ir_dump returns basic statement IR nodes", {  f <- function(n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- i + 1
    }
    out
  }

  ir <- mojor_ir_dump(f)
  expect_true(is.list(ir))
  expect_equal(ir$kind, "block")
  kinds <- collect_ir_kinds(ir)
  expect_true("loop" %in% kinds)
  expect_true("assign" %in% kinds)
  expect_true("index" %in% kinds)
  expect_true("binop" %in% kinds)
})

test_that("mojor_ir_print formats IR and mojor_transpile returns IR", {  f <- function(n) {
    out <- numeric(n)
    for (i in 1:n) {
      out[i] <- i + 1
    }
    out
  }

  ir_lines <- capture.output(mojor_ir_print(f))
  expect_true(length(ir_lines) > 0)

  trans <- mojor_transpile(f, n = "i32", emit_ir = TRUE, name = "t_ir_emit")
  expect_true(is.list(trans$ir))
  expect_equal(trans$ir$kind, "block")
})
