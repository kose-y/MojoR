library(testthat)

test_that("explicit return and implicit return are inferred", {  vector_add_ret <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i]
    }
    return(out)
  }

  res <- mojor_transpile(vector_add_ret, x = "f64[]", y = "f64[]", name = "mojor_vec_add_ret")
  expect_equal(res$out_kind, "vector")
  expect_equal(res$out_name, "out")
  expect_equal(res$return_name, "out")

  vector_add_imp <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i]
    }
    out
  }

  res2 <- mojor_transpile(vector_add_imp, x = "f64[]", y = "f64[]", name = "mojor_vec_add_imp")
  expect_equal(res2$out_kind, "vector")
  expect_equal(res2$out_name, "out")
  expect_equal(res2$return_name, "out")
})


test_that("invalid return target is rejected", {  bad_ret <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i]
    }
    return(x)
  }

  expect_error(
    mojor_transpile(bad_ret, x = "f64[]", name = "mojor_bad_ret"),
    "return\\(\\) must return"
  )
})

test_that("return(acc / n) is supported for scalar mean", {  mean_like <- function(x) {
    acc <- 0
    for (i in seq_along(x)) {
      acc <- acc + x[i]
    }
    return(acc / length(x))
  }

  res <- mojor_transpile(mean_like, x = "f64[]", name = "mojor_mean_like")
  expect_equal(res$out_kind, "scalar")
  expect_equal(res$return_name, "acc")
  expect_true(grepl("acc = acc / Float64\\(n_i\\)", res$mojo, fixed = FALSE))
})

test_that("return(as.double(acc) / n) is accepted as mean-like scalar return", {  mean_like_wrapped <- function(x) {
    acc <- 0
    for (i in seq_along(x)) {
      acc <- acc + x[i]
    }
    return(as.double(acc) / length(x))
  }

  res <- mojor_transpile(mean_like_wrapped, x = "f64[]", name = "mojor_mean_like_wrapped", ir_only = TRUE)
  expect_equal(res$out_kind, "scalar")
  expect_equal(res$return_name, "acc")
  expect_true(grepl("acc = acc / Float64\\(n_i\\)", res$mojo, fixed = FALSE))

  skip_if_no_mojo()
  built <- mojor_build(mean_like_wrapped, x = "f64[]", cache = FALSE, load = TRUE)
  x <- c(1, 2, 3, 4)
  expect_equal(built$func(x), mean_like_wrapped(x))
})

test_that("return(acc / n) supports f32 accumulators", {  mean_like_f32 <- function(x) {
    acc <- as.single(0.0)
    for (i in seq_along(x)) {
      acc <- acc + x[i]
    }
    return(acc / length(x))
  }

  res <- mojor_transpile(mean_like_f32, x = "f32[]", name = "mojor_mean_like_f32", ir_only = TRUE)
  expect_equal(res$out_kind, "scalar")
  expect_equal(res$return_name, "acc")
  expect_true(grepl("acc = acc / Float32\\(n_i\\)", res$mojo, fixed = FALSE))

  skip_if_no_mojo()
  skip_if_not_installed("float")
  built <- mojor_build(mean_like_f32, x = "f32[]", cache = FALSE, load = TRUE)
  x <- float::fl(c(1, 2, 3, 4))
  expect_equal(as.numeric(built$func(x)), as.numeric(mean_like_f32(x)), tolerance = 1e-6)
})

test_that("return(as.single(acc) / n) is accepted for f32 accumulators", {  mean_like_f32_wrapped <- function(x) {
    acc <- as.single(0.0)
    for (i in seq_along(x)) {
      acc <- acc + x[i]
    }
    return(as.single(acc) / length(x))
  }

  res <- mojor_transpile(mean_like_f32_wrapped, x = "f32[]", name = "mojor_mean_like_f32_wrapped", ir_only = TRUE)
  expect_equal(res$out_kind, "scalar")
  expect_equal(res$return_name, "acc")
  expect_true(grepl("acc = acc / Float32\\(n_i\\)", res$mojo, fixed = FALSE))

  skip_if_no_mojo()
  skip_if_not_installed("float")
  built <- mojor_build(mean_like_f32_wrapped, x = "f32[]", cache = FALSE, load = TRUE)
  x <- float::fl(c(1, 2, 3, 4))
  expect_equal(as.numeric(built$func(x)), as.numeric(mean_like_f32_wrapped(x)), tolerance = 1e-6)
})

test_that("return(<scalar-expr> / n) is accepted without direct acc numerator", {  mean_like_expr_num <- function(x) {
    acc <- 0
    for (i in seq_along(x)) {
      acc <- acc + x[i]
    }
    return((acc + 1.0) / length(x))
  }

  res <- mojor_transpile(mean_like_expr_num, x = "f64[]", name = "mojor_mean_like_expr_num", ir_only = TRUE)
  expect_equal(res$out_kind, "scalar")
  expect_equal(res$return_name, "acc")
  expect_true(grepl("acc = \\(acc \\+ 1\\.0\\)", res$mojo, fixed = FALSE))
  expect_true(grepl("acc = acc / Float64\\(n_i\\)", res$mojo, fixed = FALSE))

  skip_if_no_mojo()
  built <- mojor_build(mean_like_expr_num, x = "f64[]", cache = FALSE, load = TRUE)
  x <- c(1, 2, 3, 4)
  expect_equal(built$func(x), mean_like_expr_num(x))
})

test_that("return(acc / <i32-expr>) supports non-loop-length denominators", {  mean_like_expr <- function(x) {
    acc <- 0
    for (i in seq_along(x)) {
      acc <- acc + x[i]
    }
    return(acc / as.integer(length(x) - 1L))
  }

  res <- mojor_transpile(mean_like_expr, x = "f64[]", name = "mojor_mean_like_expr", ir_only = TRUE)
  expect_equal(res$out_kind, "scalar")
  expect_equal(res$return_name, "acc")
  expect_true(grepl("acc = acc / Float64\\(", res$mojo, fixed = FALSE))

  skip_if_no_mojo()
  built <- mojor_build(mean_like_expr, x = "f64[]", cache = FALSE, load = TRUE)
  x <- c(1, 2, 3, 4)
  expect_equal(built$func(x), mean_like_expr(x))
})

test_that("return(acc / <numeric-expr>) supports f64 denominator expressions", {  mean_like_f64_den <- function(x) {
    acc <- 0
    for (i in seq_along(x)) {
      acc <- acc + x[i]
    }
    return(acc / as.double(length(x)))
  }

  res <- mojor_transpile(mean_like_f64_den, x = "f64[]", name = "mojor_mean_like_f64_den", ir_only = TRUE)
  expect_equal(res$out_kind, "scalar")
  expect_equal(res$return_name, "acc")
  expect_true(grepl("acc = acc / Float64\\(", res$mojo, fixed = FALSE))

  skip_if_no_mojo()
  built <- mojor_build(mean_like_f64_den, x = "f64[]", cache = FALSE, load = TRUE)
  x <- c(1, 2, 3, 4)
  expect_equal(built$func(x), mean_like_f64_den(x))
})

test_that("return(acc / <expr>) keeps non-numeric denominators rejected", {  bad_mean_like <- function(x) {
    acc <- 0
    for (i in seq_along(x)) {
      acc <- acc + x[i]
    }
    return(acc / (length(x) > 0L))
  }

  expect_error(
    mojor_transpile(bad_mean_like, x = "f64[]", name = "mojor_bad_mean_like", ir_only = TRUE),
    "denominator must be a numeric scalar expression"
  )
})

test_that("return() inside nested control flow is supported", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      if (x[i] > 0) {
        return(out)
      }
      out[i] <- x[i]
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "mojor_ret_nested")
  expect_true(grepl("return", trans$mojo))
})

test_that("return() scalar accumulator inside nested control flow is supported", {  f <- function(x) {
    acc <- 0
    for (i in seq_along(x)) {
      if (x[i] > 0) {
        return(acc)
      }
      acc <- acc + x[i]
    }
    acc
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "mojor_ret_nested_scalar")
  expect_true(grepl("acc_out\\[0\\] = acc", trans$mojo))
  expect_true(grepl("return", trans$mojo))
})

test_that("explicit min/max/product accumulators are supported", {  min_loop <- function(x) {
    acc <- x[1]
    for (i in seq_along(x)) {
      acc <- min(acc, x[i])
    }
    acc
  }
  max_loop <- function(x) {
    acc <- x[1]
    for (i in seq_along(x)) {
      acc <- max(acc, x[i])
    }
    acc
  }
  prod_loop <- function(x) {
    acc <- 1
    for (i in seq_along(x)) {
      acc <- acc * x[i]
    }
    acc
  }

  res_min <- mojor_transpile(min_loop, x = "f64[]", name = "mojor_min_loop")
  res_max <- mojor_transpile(max_loop, x = "f64[]", name = "mojor_max_loop")
  res_prod <- mojor_transpile(prod_loop, x = "f64[]", name = "mojor_prod_loop")

  expect_equal(res_min$out_kind, "scalar")
  expect_equal(res_max$out_kind, "scalar")
  expect_equal(res_prod$out_kind, "scalar")
  expect_true(grepl("min\\(", res_min$mojo))
  expect_true(grepl("max\\(", res_max$mojo))
  expect_true(grepl("\\*", res_prod$mojo))
})

test_that("min/max accept explicit multi-arg reductions", {  min3 <- function(x, y) {
    acc <- x[1]
    for (i in seq_along(x)) {
      acc <- min(acc, x[i], y[i])
    }
    acc
  }
  max3 <- function(x, y) {
    acc <- x[1]
    for (i in seq_along(x)) {
      acc <- max(acc, x[i], y[i])
    }
    acc
  }

  res_min <- mojor_transpile(min3, x = "f64[]", y = "f64[]", name = "mojor_min3")
  res_max <- mojor_transpile(max3, x = "f64[]", y = "f64[]", name = "mojor_max3")
  expect_equal(res_min$out_kind, "scalar")
  expect_equal(res_max$out_kind, "scalar")
  expect_true(grepl("min\\(", res_min$mojo))
  expect_true(grepl("max\\(", res_max$mojo))
})

test_that("which.min/which.max loop patterns transpile", {  wmin <- function(x) {
    acc <- x[1]
    idx <- 1L
    for (i in seq_along(x)) {
      if (x[i] < acc) {
        acc <- x[i]
        idx <- as.integer(i)
      }
    }
    idx
  }
  wmax <- function(x) {
    acc <- x[1]
    idx <- 1L
    for (i in seq_along(x)) {
      if (x[i] > acc) {
        acc <- x[i]
        idx <- as.integer(i)
      }
    }
    idx
  }

  res_min <- mojor_transpile(wmin, x = "f64[]", name = "t_which_min")
  res_max <- mojor_transpile(wmax, x = "f64[]", name = "t_which_max")
  expect_equal(res_min$out_kind, "scalar")
  expect_equal(res_max$out_kind, "scalar")
  expect_equal(res_min$out_name, "idx")
  expect_equal(res_max$out_name, "idx")
  expect_match(res_min$mojo, "var idx: Int32", fixed = TRUE)
  expect_match(res_max$mojo, "var idx: Int32", fixed = TRUE)
})

test_that("which.min/which.max top-level reductions transpile", {  wmin <- function(x) which.min(x)
  wmax <- function(x) which.max(x)

  res_min <- mojor_transpile(wmin, x = "f64[]", name = "t_which_min_top")
  res_max <- mojor_transpile(wmax, x = "f64[]", name = "t_which_max_top")
  expect_equal(res_min$out_kind, "scalar")
  expect_equal(res_max$out_kind, "scalar")
  expect_equal(res_min$out_name, "idx")
  expect_equal(res_max$out_name, "idx")
  expect_match(res_min$mojo, "var idx: Int32", fixed = TRUE)
  expect_match(res_max$mojo, "var idx: Int32", fixed = TRUE)
  expect_true(
    grepl("v < best", res_min$mojo, fixed = TRUE) ||
      grepl("__mojor_right_v < __mojor_left_v", res_min$mojo, fixed = TRUE)
  )
  expect_true(
    grepl("v > best", res_max$mojo, fixed = TRUE) ||
      grepl("__mojor_right_v > __mojor_left_v", res_max$mojo, fixed = TRUE)
  )
})

test_that("strict scalar reduction helper enforces strict verify and non-empty emit", {  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = TRUE)

  old_prepare <- .mojor_prepare_transpile_ir_stmt
  old_pipeline <- .mojor_run_transpile_ir_pipeline
  old_emit <- .mojor_transpile_type_and_emit_stmt
  on.exit(assign(".mojor_prepare_transpile_ir_stmt", old_prepare, envir = .GlobalEnv), add = TRUE)
  on.exit(assign(".mojor_run_transpile_ir_pipeline", old_pipeline, envir = .GlobalEnv), add = TRUE)
  on.exit(assign(".mojor_transpile_type_and_emit_stmt", old_emit, envir = .GlobalEnv), add = TRUE)

  seen <- new.env(parent = emptyenv())
  seen$verify_strict <- NULL
  seen$allow_empty <- NULL

  assign(".mojor_prepare_transpile_ir_stmt", function(ir, local_types, dim_map, verify_strict = TRUE) {
    seen$verify_strict <- isTRUE(verify_strict)
    ir
  }, envir = .GlobalEnv)
  assign(".mojor_run_transpile_ir_pipeline", function(ctx, ir, opt_level, schedule, elementwise_plan = NULL, elementwise_target = NULL, enable_fusion = FALSE) {
    ir
  }, envir = .GlobalEnv)
  assign(".mojor_transpile_type_and_emit_stmt", function(ir, local_types, out_name, na_mode, bounds_check, scalar_name, schedule, loop_var = NULL, unroll = NULL, require_kinds = NULL, indent = "    ", zero_based_vars = character(0), allow_empty = FALSE) {
    seen$allow_empty <- isTRUE(allow_empty)
    list(ir_typed = ir, mojo_lines = c("    # strict scalar reduce"))
  }, envir = .GlobalEnv)

  ctx <- new.env(parent = .GlobalEnv)
  ctx$has_while <- FALSE
  ctx$out_kind <- "scalar"
  ctx$scalar_reduce_ir_node <- .mojor_ir_scalar_reduce("sum", "acc", "x")
  ctx$scalar_reduction_op <- "sum"
  ctx$loop_infos <- list(list(var = "i"))
  ctx$scalar_reduction_arg <- "x"
  ctx$local_types <- list(x = "f64[]")
  ctx$dim_map <- list()
  ctx$opt_level <- 2L
  ctx$schedule <- list()
  ctx$out_name <- "out"
  ctx$na_mode <- "forbid"
  ctx$scalar_init <- "acc"
  ctx$mojo_lines <- character(0)

  expect_silent(.mojor_emit_transpile_scalar_reduce_ir_path(ctx))
  expect_true(isTRUE(seen$verify_strict))
  expect_false(isTRUE(seen$allow_empty))
  expect_true(isTRUE(ctx$custom_reduction_emitted))
})

test_that("strict mode rejects scalar reduction fallback when reduction IR preparation returns NULL", {  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = TRUE)

  old_prepare <- .mojor_prepare_transpile_ir_stmt
  on.exit(assign(".mojor_prepare_transpile_ir_stmt", old_prepare, envir = .GlobalEnv), add = TRUE)
  assign(".mojor_prepare_transpile_ir_stmt", function(ir, local_types, dim_map, verify_strict = TRUE) {
    if (!is.null(ir$kind) && identical(ir$kind, "scalar_reduce")) return(NULL)
    old_prepare(ir, local_types, dim_map, verify_strict = verify_strict)
  }, envir = .GlobalEnv)

  f <- function(x) which.min(x)
  expect_error(
    mojor_transpile(f, x = "f64[]", name = "t_strict_scalar_reduce_null_ir"),
    "strict scalar reduction IR preparation produced no IR"
  )
})

test_that("compat mode keeps which.min loop fallback when reduction IR preparation returns NULL", {  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = FALSE)

  old_prepare <- .mojor_prepare_transpile_ir_stmt
  on.exit(assign(".mojor_prepare_transpile_ir_stmt", old_prepare, envir = .GlobalEnv), add = TRUE)
  assign(".mojor_prepare_transpile_ir_stmt", function(ir, local_types, dim_map, verify_strict = TRUE) {
    if (!is.null(ir$kind) && identical(ir$kind, "scalar_reduce")) return(NULL)
    old_prepare(ir, local_types, dim_map, verify_strict = verify_strict)
  }, envir = .GlobalEnv)

  f <- function(x) which.min(x)
  trans <- mojor_transpile(f, x = "f64[]", name = "t_compat_scalar_reduce_null_ir")
  expect_false(grepl("from nn.argmaxmin import argmax, argmin", trans$mojo, fixed = TRUE))
  expect_false(grepl("argmin(", trans$mojo, fixed = TRUE))
  expect_match(trans$mojo, "__mojor_has_value", fixed = TRUE)
  expect_match(trans$mojo, "__mojor_best", fixed = TRUE)
})

test_that("strict mode forbids argext scalar reduction fallback helper", {  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = TRUE)

  ctx <- new.env(parent = .GlobalEnv)
  ctx$custom_reduction_emitted <- FALSE
  ctx$has_while <- FALSE
  ctx$out_kind <- "scalar"
  ctx$scalar_reduction_op <- "which.min"
  ctx$loop_infos <- list(list(var = "i"))
  ctx$scalar_reduction_arg <- "x"
  ctx$use_argext <- TRUE

  expect_error(
    .mojor_emit_transpile_argext_reduction_path(ctx),
    "strict IR mode forbids argext scalar reduction fallback"
  )
})

test_that("strict mode forbids argext scalar reduction fallback helper even when use_argext is FALSE", {  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = TRUE)

  ctx <- new.env(parent = .GlobalEnv)
  ctx$custom_reduction_emitted <- FALSE
  ctx$has_while <- FALSE
  ctx$out_kind <- "scalar"
  ctx$scalar_reduction_op <- "which.max"
  ctx$loop_infos <- list(list(var = "i"))
  ctx$scalar_reduction_arg <- "x"
  ctx$use_argext <- FALSE

  expect_error(
    .mojor_emit_transpile_argext_reduction_path(ctx),
    "strict IR mode forbids argext scalar reduction fallback"
  )
})

test_that("compat mode keeps argext scalar reduction helper emission", {  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = FALSE)
  had_layout_runtime <- exists("layout_runtime", envir = .GlobalEnv, inherits = FALSE)
  old_layout_runtime <- if (had_layout_runtime) get("layout_runtime", envir = .GlobalEnv, inherits = FALSE) else NULL
  on.exit({
    if (had_layout_runtime) {
      assign("layout_runtime", old_layout_runtime, envir = .GlobalEnv)
    } else if (exists("layout_runtime", envir = .GlobalEnv, inherits = FALSE)) {
      rm("layout_runtime", envir = .GlobalEnv)
    }
  }, add = TRUE)
  assign("layout_runtime", function(layout_name, dims_expr) {
    paste0(layout_name, "(", dims_expr, ")")
  }, envir = .GlobalEnv)

  ctx <- new.env(parent = .GlobalEnv)
  ctx$custom_reduction_emitted <- FALSE
  ctx$has_while <- FALSE
  ctx$out_kind <- "scalar"
  ctx$scalar_reduction_op <- "which.min"
  ctx$loop_infos <- list(list(var = "i"))
  ctx$scalar_reduction_arg <- "x"
  ctx$use_argext <- TRUE
  ctx$arg_specs <- list(x = "f64[]")
  ctx$scalar_init <- "idx"
  ctx$na_guard <- "forbid"
  ctx$mojo_lines <- character(0)

  expect_silent(.mojor_emit_transpile_argext_reduction_path(ctx))
  expect_true(isTRUE(ctx$custom_reduction_emitted))
  expect_true(any(grepl("argmin", ctx$mojo_lines, fixed = TRUE)))
})

test_that("which.min/which.max selector zero-affine reductions transpile", {  wmin_plus0 <- function(x, idx) which.min(x[idx + 0L])
  wmax_0plus <- function(x, idx) which.max(x[0L + idx])
  wmin_minus0 <- function(x, idx) which.min(x[idx - 0L])
  wmax_0minus <- function(x, idx) which.max(x[0L - idx])

  res_min_plus0 <- mojor_transpile(wmin_plus0, x = "f64[]", idx = "i32[]", name = "t_which_min_plus0")
  res_max_0plus <- mojor_transpile(wmax_0plus, x = "f64[]", idx = "i32[]", name = "t_which_max_0plus")
  res_min_minus0 <- mojor_transpile(wmin_minus0, x = "f64[]", idx = "i32[]", name = "t_which_min_minus0")
  res_max_0minus <- mojor_transpile(wmax_0minus, x = "f64[]", idx = "i32[]", name = "t_which_max_0minus")

  expect_equal(res_min_plus0$out_kind, "scalar")
  expect_equal(res_max_0plus$out_kind, "scalar")
  expect_equal(res_min_minus0$out_kind, "scalar")
  expect_equal(res_max_0minus$out_kind, "scalar")

  expect_equal(res_min_plus0$out_name, "__mojor_idx")
  expect_equal(res_max_0plus$out_name, "__mojor_idx")
  expect_equal(res_min_minus0$out_name, "__mojor_idx")
  expect_equal(res_max_0minus$out_name, "__mojor_idx")
  expect_equal(res_min_plus0$scalar_reduction_op, "which.min")
  expect_equal(res_max_0plus$scalar_reduction_op, "which.max")
  expect_equal(res_min_minus0$scalar_reduction_op, "which.min")
  expect_equal(res_max_0minus$scalar_reduction_op, "which.max")

  expect_equal(res_min_plus0$n_source$name, "idx")
  expect_equal(res_max_0plus$n_source$name, "idx")
  expect_equal(res_min_minus0$n_source$name, "idx")
  expect_equal(res_max_0minus$n_source$name, "idx")

  expect_match(res_min_plus0$mojo, "var __mojor_idx: Int32", fixed = TRUE)
  expect_match(res_max_0plus$mojo, "var __mojor_idx: Int32", fixed = TRUE)
  expect_match(res_min_minus0$mojo, "var __mojor_idx: Int32", fixed = TRUE)
  expect_match(res_max_0minus$mojo, "var __mojor_idx: Int32", fixed = TRUE)

  expect_true(grepl("_mojor_read_i32\\(idx, Int\\(\\(i - 1\\)\\), Int\\(n_i\\)\\)", res_min_plus0$mojo))
  expect_true(grepl("_mojor_read_i32\\(idx, Int\\(\\(i - 1\\)\\), Int\\(n_i\\)\\)", res_max_0plus$mojo))
  expect_true(grepl("_mojor_read_i32\\(idx, Int\\(\\(i - 1\\)\\), Int\\(n_i\\)\\)", res_min_minus0$mojo))
  expect_true(grepl("\\(0 - _mojor_read_i32\\(idx, Int\\(\\(i - 1\\)\\), Int\\(n_i\\)\\)\\)", res_max_0minus$mojo))

  expect_false(grepl("idx[Int((i - 1))] + 0", res_min_plus0$mojo, fixed = TRUE))
  expect_false(grepl("idx[Int((i - 1))] - 0", res_min_minus0$mojo, fixed = TRUE))
})

test_that("which.min/which.max selector nested canceling literals transpile", {  wmin_nested <- function(x, idx) which.min(x[(idx + 1L) - 1L])
  wmax_nested <- function(x, idx) which.max(x[(idx - 1L) + 1L])

  res_min_nested <- mojor_transpile(wmin_nested, x = "f64[]", idx = "i32[]", name = "t_which_min_nested_cancel")
  res_max_nested <- mojor_transpile(wmax_nested, x = "f64[]", idx = "i32[]", name = "t_which_max_nested_cancel")

  expect_equal(res_min_nested$out_kind, "scalar")
  expect_equal(res_max_nested$out_kind, "scalar")
  expect_equal(res_min_nested$out_name, "__mojor_idx")
  expect_equal(res_max_nested$out_name, "__mojor_idx")
  expect_equal(res_min_nested$scalar_reduction_op, "which.min")
  expect_equal(res_max_nested$scalar_reduction_op, "which.max")
  expect_equal(res_min_nested$n_source$name, "idx")
  expect_equal(res_max_nested$n_source$name, "idx")

  expect_false(grepl("idx[Int((i - 1))] + 1\\) - 1", res_min_nested$mojo, fixed = FALSE))
  expect_false(grepl("idx[Int((i - 1))] - 1\\) \\+ 1", res_max_nested$mojo, fixed = FALSE))
  expect_true(grepl("_mojor_read_i32\\(idx, Int\\(\\(i - 1\\)\\), Int\\(n_i\\)\\)", res_min_nested$mojo))
  expect_true(grepl("_mojor_read_i32\\(idx, Int\\(\\(i - 1\\)\\), Int\\(n_i\\)\\)", res_max_nested$mojo))
})

# --- Early return from nested loops (formerly known issue) ---

test_that("scalar early return from nested for-for loops works", {
  f <- function(n, m, x) {
    result <- 0.0
    for (i in seq_len(n)) {
      for (j in seq_len(m)) {
        result <- result + x[i] * x[j]
        if (result > 100) {
          return(result)
        }
      }
    }
    result
  }
  trans <- mojor_transpile(f, n = "i32", m = "i32", x = "f64[]")
  expect_true(grepl("result_out\\[0\\] = result", trans$mojo))
  expect_true(grepl("return", trans$mojo))
  # Verify the return is inside the inner loop (indented under if)
  lines <- strsplit(trans$mojo, "\n")[[1]]
  return_lines <- grep("^\\s+return$", lines)
  expect_true(length(return_lines) >= 1)
})

test_that("vector early return from nested for-for loops works", {
  f <- function(n, x) {
    out <- numeric(length(x))
    for (i in seq_len(n)) {
      for (j in seq_along(x)) {
        out[j] <- out[j] + i
        if (out[j] > 100) {
          return(out)
        }
      }
    }
    out
  }
  trans <- mojor_transpile(f, n = "i32", x = "f64[]")
  lines <- strsplit(trans$mojo, "\n")[[1]]
  return_lines <- grep("^\\s+return$", lines)
  expect_true(length(return_lines) >= 1)
})

test_that("scalar early return from while-for nested loops works", {
  f <- function(n, x) {
    acc <- 0.0
    i <- 1L
    while (i <= n) {
      for (j in seq_along(x)) {
        acc <- acc + x[j] * i
        if (acc > 1000) {
          return(acc)
        }
      }
      i <- i + 1L
    }
    acc
  }
  trans <- mojor_transpile(f, n = "i32", x = "f64[]")
  expect_true(grepl("acc_out\\[0\\] = acc", trans$mojo))
  expect_true(grepl("return", trans$mojo))
})

test_that("scalar early return from 3-level nested loops works", {
  f <- function(n, m, k, x) {
    result <- 0.0
    for (i in seq_len(n)) {
      for (j in seq_len(m)) {
        for (l in seq_len(k)) {
          result <- result + x[i]
          if (result > 100) {
            return(result)
          }
        }
      }
    }
    result
  }
  trans <- mojor_transpile(f, n = "i32", m = "i32", k = "i32", x = "f64[]")
  expect_true(grepl("result_out\\[0\\] = result", trans$mojo))
  lines <- strsplit(trans$mojo, "\n")[[1]]
  return_lines <- grep("^\\s+return$", lines)
  expect_true(length(return_lines) >= 1)
})

test_that("early return from nested loop with no trailing expression works", {
  f <- function(n, m, x) {
    result <- 0.0
    for (i in seq_len(n)) {
      for (j in seq_len(m)) {
        result <- result + x[i] * x[j]
        if (result > 100) {
          return(result)
        }
      }
    }
  }
  # Should still determine output kind from scalar_init
  trans <- mojor_transpile(f, n = "i32", m = "i32", x = "f64[]")
  expect_true(grepl("result_out\\[0\\] = result", trans$mojo))
  expect_true(grepl("return", trans$mojo))
})

test_that("multiple early returns from nested loops work", {
  f <- function(n, m, x) {
    result <- 0.0
    for (i in seq_len(n)) {
      if (i > 5L) return(result)
      for (j in seq_len(m)) {
        result <- result + x[i] * x[j]
        if (result > 100) {
          return(result)
        }
      }
    }
    result
  }
  trans <- mojor_transpile(f, n = "i32", m = "i32", x = "f64[]")
  expect_true(grepl("return", trans$mojo))
  lines <- strsplit(trans$mojo, "\n")[[1]]
  return_lines <- grep("^\\s+return$", lines)
  # Should have at least 2 return statements (one in outer if, one in inner if)
  expect_true(length(return_lines) >= 2)
})
