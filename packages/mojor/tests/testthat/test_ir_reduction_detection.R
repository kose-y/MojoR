library(testthat)

with_len_map <- function(expr) {
  prev_n_source_name <- .mojor_state$current_n_source_name
  prev_len_var_map <- .mojor_state$current_len_var_map
  .mojor_state$current_n_source_name <- "x"
  .mojor_state$current_len_var_map <- list()
  on.exit({
    .mojor_state$current_n_source_name <- prev_n_source_name
    .mojor_state$current_len_var_map <- prev_len_var_map
  }, add = TRUE)
  force(expr)
}

test_that("IR reduction detection: sum", {  stmt <- quote(for (i in seq_along(x)) { acc <- acc + x[i] })
  ir <- .mojor_ir_build_stmt(stmt)

  # Builder annotates $reduce at build time; type-check preserves it
  expect_equal(ir$reduce$kind, "sum")
  expect_equal(ir$reduce$acc, "acc")

  with_len_map({
    ir_typed <- .mojor_ir_type_check_stmt(ir, list(x = "f64[]", acc = "f64"))
    pat <- ir_typed$reduce
    expect_equal(pat$kind, "sum")
    expect_equal(pat$acc, "acc")

    mojo_lines <- .mojor_ir_stmt_emit(ir_typed, indent = "    ", type_env = list(x = "f64[]", acc = "f64"))
    expect_true(grepl("# IR reduction: sum acc <- acc \\+ ", mojo_lines[1]))
  })
})

test_that("IR reduction detection: product", {  stmt <- quote(for (i in seq_along(x)) { acc <- acc * x[i] })
  ir <- .mojor_ir_build_stmt(stmt)

  expect_equal(ir$reduce$kind, "product")
  expect_equal(ir$reduce$acc, "acc")

  with_len_map({
    ir_typed <- .mojor_ir_type_check_stmt(ir, list(x = "f64[]", acc = "f64"))
    expect_equal(ir_typed$reduce$kind, "product")
    expect_equal(ir_typed$reduce$acc, "acc")
  })
})

test_that("IR reduction detection: min/max", {  stmt_min <- quote(for (i in seq_along(x)) { acc <- min(acc, x[i]) })
  ir_min <- .mojor_ir_build_stmt(stmt_min)
  stmt_max <- quote(for (i in seq_along(x)) { acc <- max(x[i], acc) })
  ir_max <- .mojor_ir_build_stmt(stmt_max)

  expect_equal(ir_min$reduce$kind, "min")
  expect_equal(ir_max$reduce$kind, "max")

  with_len_map({
    ir_min_typed <- .mojor_ir_type_check_stmt(ir_min, list(x = "f64[]", acc = "f64"))
    ir_max_typed <- .mojor_ir_type_check_stmt(ir_max, list(x = "f64[]", acc = "f64"))
    expect_equal(ir_min_typed$reduce$kind, "min")
    expect_equal(ir_max_typed$reduce$kind, "max")
  })
})
