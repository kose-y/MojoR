library(testthat)


test_that("any-style logical reduction uses short-circuit", {  f <- function(x) {
    found <- FALSE
    for (i in seq_along(x)) {
      if (x[i] > 0) {
        found <- TRUE
      }
    }
    found
  }

  res <- mojor_transpile(f, x = "f64[]", name = "mojor_any")
  expect_equal(res$out_type, "lgl")
  expect_match(res$mojo, "break", fixed = TRUE)
})


test_that("all-style logical reduction uses short-circuit", {  f <- function(x) {
    ok <- TRUE
    for (i in seq_along(x)) {
      if (!(x[i] > 0)) {
        ok <- FALSE
      }
    }
    ok
  }

  res <- mojor_transpile(f, x = "f64[]", name = "mojor_all")
  expect_equal(res$out_type, "lgl")
  expect_match(res$mojo, "break", fixed = TRUE)
})

test_that("any()/all() wrappers compile", {  any_fn <- function(x) {
    any(x)
  }
  all_fn <- function(x) {
    return(all(x))
  }

  res_any <- mojor_transpile(any_fn, x = "lgl[]", name = "mojor_any_wrap")
  res_all <- mojor_transpile(all_fn, x = "lgl[]", name = "mojor_all_wrap")
  expect_equal(res_any$out_type, "lgl")
  expect_equal(res_all$out_type, "lgl")
  expect_match(res_any$mojo, "break", fixed = TRUE)
  expect_match(res_all$mojo, "break", fixed = TRUE)
})

test_that("strict mode forbids legacy short-circuit fallback helper", {  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = TRUE)

  ctx <- new.env(parent = .GlobalEnv)
  ctx$short_ir_fallback <- list(cond = quote(TRUE), set = "1")

  expect_error(
    .mojor_emit_transpile_short_ir_fallback(ctx),
    "strict IR mode forbids legacy short-circuit fallback"
  )
})

test_that("any/all accept numeric and comparisons", {  any_num <- function(x) {
    any(x)
  }
  all_cmp <- function(x) {
    all(x > 0)
  }

  res_any <- mojor_transpile(any_num, x = "f64[]", name = "mojor_any_num")
  res_all <- mojor_transpile(all_cmp, x = "f64[]", name = "mojor_all_cmp")
  expect_equal(res_any$out_type, "lgl")
  expect_equal(res_all$out_type, "lgl")
  expect_true(grepl("!= (Float64\\(0\\)|0)", res_any$mojo))
  expect_true(grepl("> (Float64\\(0\\)|0)", res_all$mojo))
})

test_that("any/all support multiple arrays with & and |", {  any_both <- function(x, y) {
    any(x & y)
  }
  all_either <- function(x, y) {
    all(x | y)
  }

  res_any <- mojor_transpile(any_both, x = "lgl[]", y = "lgl[]", name = "mojor_any_both")
  res_all <- mojor_transpile(all_either, x = "lgl[]", y = "lgl[]", name = "mojor_all_either")
  expect_match(res_any$mojo, "and", fixed = TRUE)
  expect_match(res_all$mojo, "or", fixed = TRUE)
})

test_that("any/all wrappers support multiple positional arguments", {  any_multi <- function(x, y) {
    any(x > 0, y > 0)
  }
  all_multi <- function(x, y) {
    all(x > 0, y > 0)
  }

  res_any <- mojor_transpile(any_multi, x = "f64[]", y = "f64[]", name = "mojor_any_multi", ir_only = TRUE)
  res_all <- mojor_transpile(all_multi, x = "f64[]", y = "f64[]", name = "mojor_all_multi", ir_only = TRUE)
  expect_match(res_any$mojo, "or", fixed = TRUE)
  expect_match(res_all$mojo, "and", fixed = TRUE)
})

test_that("any/all wrappers accept named ... arguments", {  any_named <- function(x, y) {
    any(lhs = x > 0, rhs = y > 0, na.rm = FALSE)
  }
  all_named <- function(x, y) {
    all(lhs = x > 0, rhs = y > 0, na.rm = FALSE)
  }

  res_any <- mojor_transpile(any_named, x = "f64[]", y = "f64[]", name = "mojor_any_named", ir_only = TRUE)
  res_all <- mojor_transpile(all_named, x = "f64[]", y = "f64[]", name = "mojor_all_named", ir_only = TRUE)
  expect_match(res_any$mojo, "or", fixed = TRUE)
  expect_match(res_all$mojo, "and", fixed = TRUE)
})

test_that("any/all wrappers support scalar-only inputs", {  any_scalar <- function(a, b) {
    any(lhs = a > 0, rhs = b > 0, na.rm = FALSE)
  }
  all_scalar <- function(a, b) {
    all(lhs = a > 0, rhs = b > 0, na.rm = FALSE)
  }

  res_any <- mojor_transpile(any_scalar, a = "f64", b = "f64", name = "mojor_any_scalar", ir_only = TRUE)
  res_all <- mojor_transpile(all_scalar, a = "f64", b = "f64", name = "mojor_all_scalar", ir_only = TRUE)
  expect_equal(res_any$out_type, "lgl")
  expect_equal(res_all$out_type, "lgl")
  expect_match(res_any$mojo, "var acc: Int32 = \\(1 if")
  expect_match(res_all$mojo, "var acc: Int32 = \\(1 if")
  expect_match(res_any$mojo, "or", fixed = TRUE)
  expect_match(res_all$mojo, "and", fixed = TRUE)
  expect_no_match(res_any$mojo, "for i in range\\(")
  expect_no_match(res_all$mojo, "for i in range\\(")
  expect_match(res_any$mojo, "acc_out\\[0\\] = acc")
  expect_match(res_all$mojo, "acc_out\\[0\\] = acc")
})

test_that("any/all wrappers support zero-operand form with na.rm = FALSE", {  any_zero <- function(a) {
    any(na.rm = FALSE)
  }
  all_zero <- function(a) {
    all(na.rm = FALSE)
  }

  res_any <- mojor_transpile(any_zero, a = "f64", name = "mojor_any_zero", ir_only = TRUE)
  res_all <- mojor_transpile(all_zero, a = "f64", name = "mojor_all_zero", ir_only = TRUE)
  expect_equal(res_any$out_type, "lgl")
  expect_equal(res_all$out_type, "lgl")
  expect_match(res_any$mojo, "var acc: Int32 = 0", fixed = TRUE)
  expect_match(res_all$mojo, "var acc: Int32 = 1", fixed = TRUE)
  expect_no_match(res_any$mojo, "for i in range\\(")
  expect_no_match(res_all$mojo, "for i in range\\(")
})

test_that("any/all wrappers support zero-operand form with na.rm = TRUE", {  any_zero_rm <- function(a) {
    any(na.rm = TRUE)
  }
  all_zero_rm <- function(a) {
    all(na.rm = TRUE)
  }

  res_any <- mojor_transpile(any_zero_rm, a = "f64", name = "mojor_any_zero_rm", ir_only = TRUE, na_mode = "na_rm")
  res_all <- mojor_transpile(all_zero_rm, a = "f64", name = "mojor_all_zero_rm", ir_only = TRUE, na_mode = "na_rm")
  expect_equal(res_any$out_type, "lgl")
  expect_equal(res_all$out_type, "lgl")
  expect_no_match(res_any$mojo, "for i in range\\(")
  expect_no_match(res_all$mojo, "for i in range\\(")
  expect_match(res_any$mojo, "acc_out\\[0\\] = acc")
  expect_match(res_all$mojo, "acc_out\\[0\\] = acc")
})

test_that("any/all wrappers fold literal-only scalar forms without loop scaffolding", {  any_true <- function(a) {
    any(TRUE, na.rm = FALSE)
  }
  all_false <- function(a) {
    all(FALSE, na.rm = FALSE)
  }

  res_any <- mojor_transpile(any_true, a = "f64", name = "mojor_any_true_literal", ir_only = TRUE)
  res_all <- mojor_transpile(all_false, a = "f64", name = "mojor_all_false_literal", ir_only = TRUE)
  expect_equal(res_any$out_type, "lgl")
  expect_equal(res_all$out_type, "lgl")
  expect_match(res_any$mojo, "var acc: Int32 = 1", fixed = TRUE)
  expect_match(res_all$mojo, "var acc: Int32 = 0", fixed = TRUE)
  expect_no_match(res_any$mojo, "for i in range\\(")
  expect_no_match(res_all$mojo, "for i in range\\(")
})

test_that("any/all wrappers support na.rm = TRUE", {  any_na_rm <- function(x) {
    any(x > 0, na.rm = TRUE)
  }
  all_na_rm <- function(x) {
    all(x > 0, na.rm = TRUE)
  }

  res_any <- mojor_transpile(any_na_rm, x = "f64[]", name = "mojor_any_na_rm", ir_only = TRUE)
  res_all <- mojor_transpile(all_na_rm, x = "f64[]", name = "mojor_all_na_rm", ir_only = TRUE)
  expect_equal(res_any$out_type, "lgl")
  expect_equal(res_all$out_type, "lgl")
  expect_match(res_any$mojo, "for i in range", fixed = TRUE)
  expect_match(res_all$mojo, "for i in range", fixed = TRUE)
})

test_that("any/all wrappers reject non-logical na.rm", {  any_bad <- function(x) {
    any(x > 0, na.rm = 1L)
  }
  all_bad <- function(x) {
    all(x > 0, na.rm = "TRUE")
  }

  expect_error(
    mojor_transpile(any_bad, x = "f64[]", name = "mojor_any_bad_na_rm"),
    "na\\.rm must be TRUE or FALSE"
  )
  expect_error(
    mojor_transpile(all_bad, x = "f64[]", name = "mojor_all_bad_na_rm"),
    "na\\.rm must be TRUE or FALSE"
  )
})

test_that("any/all support xor() logical composition", {  any_xor <- function(x, y) {
    any(xor(x, y))
  }
  all_xor <- function(x, y) {
    all(xor(x, y))
  }

  res_any <- mojor_transpile(any_xor, x = "lgl[]", y = "lgl[]", name = "mojor_any_xor", ir_only = TRUE)
  res_all <- mojor_transpile(all_xor, x = "lgl[]", y = "lgl[]", name = "mojor_all_xor", ir_only = TRUE)
  expect_match(res_any$mojo, "!=")
  expect_match(res_all$mojo, "!=")
})

test_that("any/all support explicit array selector indices", {  any_sel <- function(x, idx) {
    any(x[idx] > 0)
  }
  all_sel <- function(x, idx) {
    all(x[idx] > 0)
  }

  res_any <- mojor_transpile(any_sel, x = "f64[]", idx = "i32[]", name = "mojor_any_sel", ir_only = TRUE)
  res_all <- mojor_transpile(all_sel, x = "f64[]", idx = "i32[]", name = "mojor_all_sel", ir_only = TRUE)
  expect_match(res_any$mojo, "_mojor_read_i32\\(idx")
  expect_match(res_all$mojo, "_mojor_read_i32\\(idx")
  expect_match(res_any$mojo, "_mojor_read_f64\\(x")
  expect_match(res_all$mojo, "_mojor_read_f64\\(x")
})

test_that("any/all support selector index expressions derived from arrays", {  any_shift <- function(x, idx) {
    any(x[idx + 1L] > 0)
  }
  all_shift <- function(x, idx) {
    all(x[idx + 1L] > 0)
  }

  res_any <- mojor_transpile(any_shift, x = "f64[]", idx = "i32[]", name = "mojor_any_sel_shift", ir_only = TRUE)
  res_all <- mojor_transpile(all_shift, x = "f64[]", idx = "i32[]", name = "mojor_all_sel_shift", ir_only = TRUE)
  expect_match(res_any$mojo, "_mojor_read_i32\\(idx")
  expect_match(res_all$mojo, "_mojor_read_i32\\(idx")
  expect_match(res_any$mojo, "\\+ 1")
  expect_match(res_all$mojo, "\\+ 1")
})

test_that("any/all support selector index expressions with scalar offsets", {  any_shift_var <- function(x, idx, off) {
    any(x[idx + off] > 0)
  }
  all_shift_var <- function(x, idx, off) {
    all(x[idx + off] > 0)
  }

  res_any <- mojor_transpile(any_shift_var, x = "f64[]", idx = "i32[]", off = "i32", name = "mojor_any_sel_shift_var", ir_only = TRUE)
  res_all <- mojor_transpile(all_shift_var, x = "f64[]", idx = "i32[]", off = "i32", name = "mojor_all_sel_shift_var", ir_only = TRUE)
  expect_match(res_any$mojo, "_mojor_read_i32\\(idx")
  expect_match(res_all$mojo, "_mojor_read_i32\\(idx")
  expect_match(res_any$mojo, "\\+ off")
  expect_match(res_all$mojo, "\\+ off")
})

test_that("any/all selector index emission folds zero offsets", {  any_plus_zero <- function(x, idx) {
    any(x[idx + 0L] > 0)
  }
  all_plus_zero <- function(x, idx) {
    all(x[idx + (1L - 1L)] > 0)
  }

  res_any <- mojor_transpile(any_plus_zero, x = "f64[]", idx = "i32[]", name = "mojor_any_sel_plus_zero", ir_only = TRUE)
  res_all <- mojor_transpile(all_plus_zero, x = "f64[]", idx = "i32[]", name = "mojor_all_sel_plus_fold_zero", ir_only = TRUE)
  expect_match(res_any$mojo, "_mojor_read_i32\\(idx")
  expect_match(res_all$mojo, "_mojor_read_i32\\(idx")
  expect_no_match(res_any$mojo, "_mojor_read_i32\\(idx[^\\n]*\\+ 0")
  expect_no_match(res_all$mojo, "_mojor_read_i32\\(idx[^\\n]*\\+ 0")
})

test_that("any/all selector index emission folds zero terms for commuted/subtractive forms", {  any_zero_lhs <- function(x, idx) {
    any(x[0L + idx] > 0)
  }
  all_minus_zero <- function(x, idx) {
    all(x[idx - 0L] > 0)
  }

  res_any <- mojor_transpile(any_zero_lhs, x = "f64[]", idx = "i32[]", name = "mojor_any_sel_zero_lhs", ir_only = TRUE)
  res_all <- mojor_transpile(all_minus_zero, x = "f64[]", idx = "i32[]", name = "mojor_all_sel_minus_zero", ir_only = TRUE)
  expect_match(res_any$mojo, "_mojor_read_i32\\(idx")
  expect_match(res_all$mojo, "_mojor_read_i32\\(idx")
  expect_no_match(res_any$mojo, "_mojor_read_i32\\(idx[^\\n]*\\+ 0")
  expect_no_match(res_all$mojo, "_mojor_read_i32\\(idx[^\\n]*- 0")
})

test_that("any/all selector index emission folds nested canceling literal terms", {  any_cancel <- function(x, idx) {
    any(x[(idx + 1L) - 1L] > 0)
  }
  all_cancel <- function(x, idx) {
    all(x[(idx - 1L) + 1L] > 0)
  }

  res_any <- mojor_transpile(any_cancel, x = "f64[]", idx = "i32[]", name = "mojor_any_sel_nested_cancel", ir_only = TRUE)
  res_all <- mojor_transpile(all_cancel, x = "f64[]", idx = "i32[]", name = "mojor_all_sel_nested_cancel", ir_only = TRUE)
  expect_match(res_any$mojo, "_mojor_read_i32\\(idx")
  expect_match(res_all$mojo, "_mojor_read_i32\\(idx")
  expect_no_match(res_any$mojo, "_mojor_read_i32\\(idx[^\\n]*\\+ 1\\) - 1")
  expect_no_match(res_all$mojo, "_mojor_read_i32\\(idx[^\\n]*- 1\\) \\+ 1")
})

test_that("any/all selector index emission preserves 0 - idx form", {  any_zero_minus_idx <- function(x, idx) {
    any(x[0L - idx] > 0)
  }
  all_zero_minus_idx <- function(x, idx) {
    all(x[0L - idx] > 0)
  }

  res_any <- mojor_transpile(any_zero_minus_idx, x = "f64[]", idx = "i32[]", name = "mojor_any_sel_zero_minus_idx", ir_only = TRUE)
  res_all <- mojor_transpile(all_zero_minus_idx, x = "f64[]", idx = "i32[]", name = "mojor_all_sel_zero_minus_idx", ir_only = TRUE)
  expect_match(res_any$mojo, "_mojor_read_i32\\(idx")
  expect_match(res_all$mojo, "_mojor_read_i32\\(idx")
  expect_match(res_any$mojo, "0 - _mojor_read_i32\\(idx")
  expect_match(res_all$mojo, "0 - _mojor_read_i32\\(idx")
})

test_that("any/all support selector index decrement expressions", {  any_shift_down <- function(x, idx) {
    any(x[idx - 1L] > 0)
  }
  all_shift_down <- function(x, idx) {
    all(x[idx - 1L] > 0)
  }

  res_any <- mojor_transpile(any_shift_down, x = "f64[]", idx = "i32[]", name = "mojor_any_sel_shift_down", ir_only = TRUE)
  res_all <- mojor_transpile(all_shift_down, x = "f64[]", idx = "i32[]", name = "mojor_all_sel_shift_down", ir_only = TRUE)
  expect_match(res_any$mojo, "_mojor_read_i32\\(idx")
  expect_match(res_all$mojo, "_mojor_read_i32\\(idx")
  expect_match(res_any$mojo, "- 1\\) - 1", fixed = FALSE)
  expect_match(res_all$mojo, "- 1\\) - 1", fixed = FALSE)
})

test_that("any/all support multi-dimensional selector index expressions", {  any_row_sel <- function(x, ridx, j) {
    any(x[ridx, j] > 0)
  }
  all_col_sel <- function(x, i, cidx) {
    all(x[i, cidx] > 0)
  }
  any_row_shift <- function(x, ridx, j) {
    any(x[ridx + 1L, j] > 0)
  }

  res_any_row <- mojor_transpile(any_row_sel, x = "f64[,]", ridx = "i32[]", j = "i32", name = "mojor_any_row_sel", ir_only = TRUE)
  res_all_col <- mojor_transpile(all_col_sel, x = "f64[,]", i = "i32", cidx = "i32[]", name = "mojor_all_col_sel", ir_only = TRUE)
  res_any_row_shift <- mojor_transpile(any_row_shift, x = "f64[,]", ridx = "i32[]", j = "i32", name = "mojor_any_row_sel_shift", ir_only = TRUE)

  expect_match(res_any_row$mojo, "_mojor_read_i32\\(ridx")
  expect_match(res_all_col$mojo, "_mojor_read_i32\\(cidx")
  expect_match(res_any_row$mojo, "__mojor_tensor_x\\[")
  expect_match(res_all_col$mojo, "__mojor_tensor_x\\[")
  expect_match(res_any_row_shift$mojo, "_mojor_read_i32\\(ridx")
  expect_match(res_any_row_shift$mojo, "\\+ 1")
})

test_that("any/all support metadata-only multi-dimensional selector sources", {  any_meta_only <- function(x, ridx, j) {
    any(x[length(ridx), j] > 0)
  }
  all_meta_only <- function(x, ridx, j) {
    all(x[length(ridx), j] > 0)
  }

  res_any <- mojor_transpile(any_meta_only, x = "f64[,]", ridx = "i32[]", j = "i32", name = "mojor_any_meta_only", ir_only = TRUE)
  res_all <- mojor_transpile(all_meta_only, x = "f64[,]", ridx = "i32[]", j = "i32", name = "mojor_all_meta_only", ir_only = TRUE)
  expect_equal(res_any$out_type, "lgl")
  expect_equal(res_all$out_type, "lgl")
})

test_that("any/all support named selector index arguments", {  any_named_sel <- function(x, idx) {
    any(x[idx = idx] > 0)
  }
  all_named_sel <- function(x, idx) {
    all(x[idx = idx] > 0)
  }

  res_any <- mojor_transpile(any_named_sel, x = "f64[]", idx = "i32[]", name = "mojor_any_named_sel", ir_only = TRUE)
  res_all <- mojor_transpile(all_named_sel, x = "f64[]", idx = "i32[]", name = "mojor_all_named_sel", ir_only = TRUE)
  expect_match(res_any$mojo, "_mojor_read_i32\\(idx")
  expect_match(res_all$mojo, "_mojor_read_i32\\(idx")
  expect_match(res_any$mojo, "_mojor_read_f64\\(x")
  expect_match(res_all$mojo, "_mojor_read_f64\\(x")
})

test_that("any/all support named multi-dimensional selector index arguments", {  any_named_row <- function(x, ridx, j) {
    any(x[row = ridx, col = j] > 0)
  }
  all_named_col <- function(x, i, cidx) {
    all(x[row = i, col = cidx] > 0)
  }

  res_any <- mojor_transpile(any_named_row, x = "f64[,]", ridx = "i32[]", j = "i32", name = "mojor_any_named_row_sel", ir_only = TRUE)
  res_all <- mojor_transpile(all_named_col, x = "f64[,]", i = "i32", cidx = "i32[]", name = "mojor_all_named_col_sel", ir_only = TRUE)
  expect_match(res_any$mojo, "_mojor_read_i32\\(ridx")
  expect_match(res_all$mojo, "_mojor_read_i32\\(cidx")
  expect_match(res_any$mojo, "x\\[")
  expect_match(res_all$mojo, "x\\[")
})
