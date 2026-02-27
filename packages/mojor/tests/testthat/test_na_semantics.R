# =============================================================================
# NA Semantics Tests
# =============================================================================

test_that("na_mode='propagate' emits NA-aware helpers for arithmetic", {  fn <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + 1.0
    }
    return(out)
  }
  old <- mojor_options(na_mode = "propagate")
  on.exit(mojor_options(na_mode = old$na_mode), add = TRUE)
  trans <- mojor_transpile(fn, x = "f64[]")
  expect_true(grepl("_mojor_add_f64_na", trans$mojo, fixed = TRUE))
  expect_true(grepl("__mojor_na_flag", trans$mojo, fixed = TRUE))
  expect_true(grepl("from na_helpers import", trans$mojo, fixed = TRUE))
})

test_that("na_mode='propagate' emits NA-aware helpers for subtraction", {  fn <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] - y[i]
    }
    return(out)
  }
  old <- mojor_options(na_mode = "propagate")
  on.exit(mojor_options(na_mode = old$na_mode), add = TRUE)
  trans <- mojor_transpile(fn, x = "f64[]", y = "f64[]")
  expect_true(grepl("_mojor_sub_f64_na", trans$mojo, fixed = TRUE))
})

test_that("na_mode='forbid' does NOT emit NA propagation helpers", {  fn <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + 1.0
    }
    return(out)
  }
  old <- mojor_options(na_mode = "forbid")
  on.exit(mojor_options(na_mode = old$na_mode), add = TRUE)
  trans <- mojor_transpile(fn, x = "f64[]")
  expect_false(grepl("_mojor_add_f64_na", trans$mojo, fixed = TRUE))
})

test_that("na_mode='propagate' accepted by mojor_options", {  old <- mojor_options(na_mode = "propagate")
  on.exit(mojor_options(na_mode = old$na_mode), add = TRUE)
  expect_equal(mojor_options("na_mode")$na_mode, "propagate")
})

test_that("na_mode='na_rm' accepted by mojor_options", {  old <- mojor_options(na_mode = "na_rm")
  on.exit(mojor_options(na_mode = old$na_mode), add = TRUE)
  expect_equal(mojor_options("na_mode")$na_mode, "na_rm")
})

test_that("na_needed flag set in transpiler output for propagate mode", {  fn <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2.0
    }
    return(out)
  }
  old <- mojor_options(na_mode = "propagate")
  on.exit(mojor_options(na_mode = old$na_mode), add = TRUE)
  trans <- mojor_transpile(fn, x = "f64[]")
  expect_true(isTRUE(trans$na_needed))
})

test_that("na_needed flag set in transpiler output for na_rm mode", {  fn <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2.0
    }
    return(out)
  }
  old <- mojor_options(na_mode = "na_rm")
  on.exit(mojor_options(na_mode = old$na_mode), add = TRUE)
  trans <- mojor_transpile(fn, x = "f64[]")
  expect_true(isTRUE(trans$na_needed))
})

test_that("sum/mean with na.rm=TRUE transpile in na_rm mode", {  sum_fn <- function(x) {
    sum(x, na.rm = TRUE)
  }
  mean_fn <- function(x) {
    mean(x, na.rm = TRUE)
  }

  trans_sum <- mojor_transpile(sum_fn, x = "f64[]", name = "t_sum_na_rm_transpile", na_mode = "na_rm", ir_only = TRUE)
  trans_mean <- mojor_transpile(mean_fn, x = "f64[]", name = "t_mean_na_rm_transpile", na_mode = "na_rm", ir_only = TRUE)

  expect_equal(trans_sum$out_type, "f64")
  expect_equal(trans_mean$out_type, "f64")
  expect_match(trans_mean$mojo, "acc_count", fixed = TRUE)
})

test_that("var/sd with na.rm=TRUE transpile in na_rm mode", {  var_fn <- function(x) {
    var(x, na.rm = TRUE)
  }
  sd_fn <- function(x) {
    sd(x, na.rm = TRUE)
  }
  var_default_fn <- function(x) {
    var(x)
  }
  sd_default_fn <- function(x) {
    sd(x)
  }

  trans_var <- mojor_transpile(var_fn, x = "f64[]", name = "t_var_na_rm_transpile", na_mode = "na_rm", ir_only = TRUE)
  trans_sd <- mojor_transpile(sd_fn, x = "f64[]", name = "t_sd_na_rm_transpile", na_mode = "na_rm", ir_only = TRUE)
  trans_var_default <- mojor_transpile(var_default_fn, x = "f64[]", name = "t_var_default_transpile", na_mode = "na_rm", ir_only = TRUE)
  trans_sd_default <- mojor_transpile(sd_default_fn, x = "f64[]", name = "t_sd_default_transpile", na_mode = "na_rm", ir_only = TRUE)

  na_skip_guard <- "if (not (_mojor_read_f64(x, Int((i - 1)), Int(n_i)) != _mojor_read_f64(x, Int((i - 1)), Int(n_i)))):"

  expect_equal(trans_var$out_type, "f64")
  expect_equal(trans_sd$out_type, "f64")
  expect_match(trans_var$mojo, "acc_count", fixed = TRUE)
  expect_match(trans_var$mojo, "acc = acc / Float64(acc_count - 1)", fixed = TRUE)
  expect_match(trans_sd$mojo, "acc = sqrt(acc / Float64(acc_count - 1))", fixed = TRUE)
  expect_match(trans_var$mojo, na_skip_guard, fixed = TRUE)
  expect_match(trans_sd$mojo, na_skip_guard, fixed = TRUE)
  expect_false(grepl(na_skip_guard, trans_var_default$mojo, fixed = TRUE))
  expect_false(grepl(na_skip_guard, trans_sd_default$mojo, fixed = TRUE))
})

test_that("is.na lowering emits type-appropriate checks", {  f_float <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) {
      out[i] <- is.na(x[i])
    }
    out
  }
  f_int <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) {
      out[i] <- is.na(x[i])
    }
    out
  }

  trans_float <- mojor_transpile(f_float, x = "f64[]", name = "t_isna_f64_transpile", ir_only = TRUE)
  trans_int <- mojor_transpile(f_int, x = "i32[]", name = "t_isna_i32_transpile", ir_only = TRUE)

  expect_match(trans_float$mojo, " != ", fixed = TRUE)
  expect_match(trans_int$mojo, "== -2147483648", fixed = TRUE)
})

test_that("mean parser handles na.rm explicitly", {  mean_na_rm <- .mojor_ir_expr_build(quote(mean(x, na.rm = TRUE)))
  mean_default <- .mojor_ir_expr_build(quote(mean(x)))
  mean_bad_flag <- .mojor_ir_expr_build(quote(mean(x, na.rm = 1L)))
  mean_bad_named <- .mojor_ir_expr_build(quote(mean(x, trim = 0.1)))

  expect_false(is.null(mean_na_rm))
  expect_equal(mean_na_rm$kind, "mean")
  expect_true(isTRUE(mean_na_rm$na_rm))

  expect_false(is.null(mean_default))
  expect_equal(mean_default$kind, "mean")
  expect_false(isTRUE(mean_default$na_rm))

  expect_null(mean_bad_flag)
  expect_null(mean_bad_named)
})

test_that("var/sd parsers handle na.rm explicitly", {  var_na_rm <- .mojor_ir_expr_build(quote(var(x, na.rm = TRUE)))
  var_default <- .mojor_ir_expr_build(quote(var(x)))
  var_bad_flag <- .mojor_ir_expr_build(quote(var(x, na.rm = 1L)))
  var_bad_named <- .mojor_ir_expr_build(quote(var(x, trim = 0.1)))

  sd_na_rm <- .mojor_ir_expr_build(quote(sd(x, na.rm = TRUE)))
  sd_default <- .mojor_ir_expr_build(quote(sd(x)))
  sd_bad_flag <- .mojor_ir_expr_build(quote(sd(x, na.rm = 1L)))
  sd_bad_named <- .mojor_ir_expr_build(quote(sd(x, trim = 0.1)))

  expect_false(is.null(var_na_rm))
  expect_equal(var_na_rm$kind, "var_stat")
  expect_true(isTRUE(var_na_rm$na_rm))

  expect_false(is.null(var_default))
  expect_equal(var_default$kind, "var_stat")
  expect_false(isTRUE(var_default$na_rm))

  expect_false(is.null(sd_na_rm))
  expect_equal(sd_na_rm$kind, "sd")
  expect_true(isTRUE(sd_na_rm$na_rm))

  expect_false(is.null(sd_default))
  expect_equal(sd_default$kind, "sd")
  expect_false(isTRUE(sd_default$na_rm))

  expect_null(var_bad_flag)
  expect_null(var_bad_named)
  expect_null(sd_bad_flag)
  expect_null(sd_bad_named)
})

test_that("var/sd reject unsupported na.rm forms in strict ir_only mode", {  bad_var_flag_fn <- function(x) {
    var(x, na.rm = 1L)
  }
  bad_sd_flag_expr_fn <- function(x, flag) {
    sd(x, na.rm = flag)
  }
  bad_var_multi_arg_fn <- function(x, y) {
    var(x, y, na.rm = TRUE)
  }
  bad_sd_named_arg_fn <- function(x) {
    sd(x, trim = 0.1)
  }

  expect_error(
    mojor_transpile(bad_var_flag_fn, x = "f64[]", name = "t_var_bad_na_rm_flag", na_mode = "na_rm", ir_only = TRUE),
    "na.rm must be TRUE or FALSE \\(or omitted\\)"
  )
  expect_error(
    mojor_transpile(bad_sd_flag_expr_fn, x = "f64[]", flag = "lgl", name = "t_sd_bad_na_rm_symbol", na_mode = "na_rm", ir_only = TRUE),
    "na.rm must be TRUE or FALSE \\(or omitted\\)"
  )
  expect_error(
    mojor_transpile(bad_var_multi_arg_fn, x = "f64[]", y = "f64[]", name = "t_var_bad_multi_arg", na_mode = "na_rm", ir_only = TRUE),
    "reductions only support a single array argument"
  )
  expect_error(
    mojor_transpile(bad_sd_named_arg_fn, x = "f64[]", name = "t_sd_bad_named_arg", na_mode = "na_rm", ir_only = TRUE),
    "reductions only support a single array argument"
  )
})

test_that("na_mode='propagate' emits deterministic ifelse and f32 comparison helpers", {  f_ifelse <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- ifelse(x[i] > 0, 1.0, -1.0)
    }
    out
  }
  f_cmp_f32 <- function(x) {
    out <- logical(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] > 0
    }
    out
  }

  trans_ifelse <- mojor_transpile(f_ifelse, x = "f64[]", name = "t_ifelse_na_prop_transpile", na_mode = "propagate", ir_only = TRUE)
  trans_cmp_f32 <- mojor_transpile(f_cmp_f32, x = "f32[]", name = "t_cmp_f32_na_prop_transpile", na_mode = "propagate", ir_only = TRUE)

  expect_match(trans_ifelse$mojo, "_mojor_r_na_f64() if", fixed = TRUE)
  expect_match(trans_cmp_f32$mojo, "_mojor_gt_f32_na", fixed = TRUE)
})
