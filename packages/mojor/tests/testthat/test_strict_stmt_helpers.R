library(testthat)

test_that("strict IR preparation enforces verification even when verify_strict is FALSE", {  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = TRUE)

  old_verify <- .mojor_ir_verify
  on.exit(assign(".mojor_ir_verify", old_verify, envir = .GlobalEnv), add = TRUE)
  assign(".mojor_ir_verify", function(ir, ctx = list()) {
    stop("verify boom")
  }, envir = .GlobalEnv)

  expect_error(
    .mojor_prepare_transpile_ir_stmt(
      .mojor_ir_nchar(x = .mojor_ir_var("x")),
      local_types = list(x = "chr[]"),
      dim_map = list(),
      verify_strict = FALSE
    ),
    "verify boom"
  )
})

test_that("strict stmt helper surfaces deterministic build errors", {  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = TRUE)

  old_build <- .mojor_ir_build_stmt
  on.exit(assign(".mojor_ir_build_stmt", old_build, envir = .GlobalEnv), add = TRUE)
  assign(".mojor_ir_build_stmt", function(stmt) {
    stop("stmt boom")
  }, envir = .GlobalEnv)

  expect_error(
    .mojor_stmt_to_mojo(
      stmt = quote(y <- 1L),
      loop_vars = character(0),
      seq_info = list(),
      types = list(y = "i32"),
      out_name = "out",
      out_type = "i32[]"
    ),
    "strict IR build failed: stmt boom"
  )
})

test_that("strict transpile pre-loop path compiles deterministic pre-loop statements", {  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = TRUE)

  old_build <- .mojor_ir_build_stmt
  on.exit(assign(".mojor_ir_build_stmt", old_build, envir = .GlobalEnv), add = TRUE)
  assign(".mojor_ir_build_stmt", function(stmt) {
    is_preloop_out_assign <- is.call(stmt) &&
      identical(as.character(stmt[[1]]), "<-") &&
      is.symbol(stmt[[2]]) &&
      identical(as.character(stmt[[2]]), "out")
    if (isTRUE(is_preloop_out_assign)) {
      stop("preloop boom")
    }
    old_build(stmt)
  }, envir = .GlobalEnv)

  f <- function(x) {
    out <- numeric(length(x))
    out <- x
    for (i in seq_along(x)) {
      out[i] <- out[i] + 1
    }
    return(out)
  }

  res <- mojor_transpile(
    f,
    x = "f64[]",
    name = "t_strict_preloop_stmt_build_fail"
  )
  expect_true(is.character(res$mojo) && nzchar(res$mojo))
  expect_true(grepl("out\\[", res$mojo))
})

test_that("strict loop sequence literal fold errors are deterministic", {  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = TRUE)

  old_eval <- .mojor_eval_seq_literal_expr
  on.exit(assign(".mojor_eval_seq_literal_expr", old_eval, envir = .GlobalEnv), add = TRUE)
  assign(".mojor_eval_seq_literal_expr", function(expr) {
    stop("literal fold boom")
  }, envir = .GlobalEnv)

  expect_error(
    .mojor_parse_loop_seq(
      seq_expr = quote(c(1L, 2L) + 1L),
      arg_specs = list(),
      args = character(0),
      scalar_inits = list()
    ),
    "strict loop sequence literal folding failed: literal fold boom"
  )
})

test_that("compat mode keeps literal fold fallback when static eval fails", {  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = FALSE)

  old_eval <- .mojor_eval_seq_literal_expr
  on.exit(assign(".mojor_eval_seq_literal_expr", old_eval, envir = .GlobalEnv), add = TRUE)
  assign(".mojor_eval_seq_literal_expr", function(expr) {
    stop("literal fold boom")
  }, envir = .GlobalEnv)

  info <- .mojor_parse_loop_seq(
    seq_expr = quote(c(1L, 2L) + 1L),
    arg_specs = list(),
    args = character(0),
    scalar_inits = list()
  )
  expect_false(is.null(info$iter_expr_transform))
  expect_identical(info$iter_expr_transform$op, "+")
})

test_that("strict RNG collector errors are deterministic while compat remains tolerant", {  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)

  old_call_arg <- .mojor_call_arg
  on.exit(assign(".mojor_call_arg", old_call_arg, envir = .GlobalEnv), add = TRUE)
  assign(".mojor_call_arg", function(node, i) {
    stop("rng walk boom")
  }, envir = .GlobalEnv)

  mojor_options(ir_only = TRUE)
  expect_error(
    .mojor_collect_rng_calls(quote(runif(1))),
    "strict RNG call analysis failed: rng walk boom"
  )
  expect_error(
    .mojor_detect_rng_calls(quote(foo(runif(1)))),
    "strict RNG call analysis failed: rng walk boom"
  )

  mojor_options(ir_only = FALSE)
  expect_setequal(
    .mojor_collect_rng_calls(quote(runif(1))),
    "runif"
  )
  expect_false(.mojor_detect_rng_calls(quote(foo(runif(1)))))
})
