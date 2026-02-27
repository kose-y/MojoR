library(testthat)

# IR functions are loaded by testthat helper
# (removed broken source() call)

# Basic IR Slice Tests (IR building only, not full transpilation yet)

test_that("IR builds slice_index node from 1:10", {  # Test slice detection
  expr <- quote(1:10)
  slice <- .mojor_ir_detect_slice(expr)
  expect_false(is.null(slice))
  expect_equal(slice$start, 1)
  expect_equal(slice$end, 10)
})

test_that("IR builds slice_index node from seq_len(n)", {  expr <- quote(seq_len(5))
  slice <- .mojor_ir_detect_slice(expr)
  expect_false(is.null(slice))
  expect_equal(slice$start, 1)
})

test_that("IR builds subscript node for out[1:n]", {  stmt <- quote(out[1:10] <- 0)
  ir <- .mojor_ir_build_stmt(stmt)

  expect_equal(ir$kind, "assign")
  expect_equal(ir$lhs$kind, "subscript")
  expect_equal(ir$lhs$var, "out")
  expect_equal(length(ir$lhs$indices), 1)
  expect_equal(ir$lhs$indices[[1]]$kind, "slice_index")
})

test_that("IR formats slice nodes correctly", {  start_ir <- .mojor_ir_const("1")
  end_ir <- .mojor_ir_var("n")
  slice <- .mojor_ir_slice_index(start_ir, end_ir)

  formatted <- .mojor_ir_format(slice)
  expect_true(any(grepl("slice_index", formatted)))
  expect_true(any(grepl("start:", formatted)))
  expect_true(any(grepl("end:", formatted)))
})
