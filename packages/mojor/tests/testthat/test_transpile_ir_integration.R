library(testthat)

test_that("IR path generates correct indexed assignment", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[i] + 1
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_ir_simple")
  # IR emits out[Int((i - 1))] on LHS; RHS uses _mojor_read_f64 helper with index Int((i - 1))
  expect_true(grepl("out\\[Int\\(\\(i - 1\\)\\)\\]", trans$mojo))
  expect_true(grepl("Int\\(\\(i - 1\\)\\)", trans$mojo))
})

test_that("IR path handles multiplication", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[i] * 2
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_ir_mul")
  # Should contain multiplication
  has_mul <- grepl("* Float64(2)", trans$mojo, fixed = TRUE) || grepl("* 2", trans$mojo, fixed = TRUE)
  expect_true(has_mul)
})

test_that("IR path handles two-array addition", {  f <- function(x, y, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]", n = "i32", name = "t_ir_add")
  # IR uses _mojor_read_f64 helper; both x and y appear with Int((i - 1)) index
  expect_true(grepl("x,.*Int\\(\\(i - 1\\)\\)", trans$mojo))
  expect_true(grepl("y,.*Int\\(\\(i - 1\\)\\)", trans$mojo))
})

test_that("IR path handles loop variable on RHS", {  f <- function(n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- i
    }
    out
  }
  trans <- mojor_transpile(f, n = "i32", name = "t_ir_loopvar")
  # Should have out[...] = i (with proper index normalization on LHS but not RHS since i is the value)
  expect_true(grepl("= i", trans$mojo))
})

test_that("IR-only mode supports constructor RHS (c())", {  f <- function(n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- c(1, 2)
    }
    out
  }
  trans <- suppressWarnings(mojor_transpile(f, n = "i32", name = "t_ir_ctor_c", ir_only = TRUE))
  expect_true(nzchar(trans$mojo))
})

test_that("IR-only mode supports constructor RHS (rep_len)", {  f <- function(n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- rep_len(c(1, 2), length.out = n)
    }
    out
  }
  trans <- suppressWarnings(mojor_transpile(f, n = "i32", name = "t_ir_ctor_rep_len", ir_only = TRUE))
  expect_true(nzchar(trans$mojo))
})

test_that("IR fallback works when RHS contains ifelse", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- ifelse(x[i] > 0, x[i], 0)
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_ir_ifelse")
  # Should still transpile successfully (fallback to old path)
  expect_true(nzchar(trans$mojo))
})

test_that("IR-only mode succeeds on supported IR path", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- x[i] + 1
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_ir_only_ok", ir_only = TRUE)
  expect_true(nzchar(trans$mojo))
})

test_that("IR-only mode supports while loops", {  f <- function(x) {
    out <- numeric(length(x))
    i <- 1
    while (i <= length(x)) {
      out[i] <- x[i]
      i <- i + 1
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "t_ir_only_ok_while", ir_only = TRUE)
  expect_true(nzchar(trans$mojo))
})

test_that("emit_ssa_backend defaults off", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) out[i] <- x[i] + 1
    out
  }

  trans <- mojor_transpile(f, x = "f64[]", n = "i32", name = "t_ir_ssa_default_off")
  expect_null(trans$ssa_backend)
  expect_null(trans$ssa_backend_error)
})

test_that("mojor_transpile emits backend SSA artifact with schedule->SSA ordering", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) out[i] <- x[i] + 1
    out
  }

  trans <- mojor_transpile(
    f,
    x = "f64[]",
    n = "i32",
    unroll = 4L,
    emit_ssa_backend = TRUE,
    name = "t_ir_emit_ssa_backend"
  )

  expect_true(is.list(trans$ssa_backend))
  expect_null(trans$ssa_backend_error)

  pipeline <- trans$ssa_backend$pipeline
  expect_true("schedule_ir" %in% pipeline)
  expect_true("to_ssa" %in% pipeline)
  expect_true("optimize_ssa" %in% pipeline)
  expect_true("backend_cfg" %in% pipeline)
  expect_true(match("schedule_ir", pipeline) < match("to_ssa", pipeline))
  expect_true(match("to_ssa", pipeline) < match("optimize_ssa", pipeline))
  expect_true(match("optimize_ssa", pipeline) < match("backend_cfg", pipeline))

  sched_ir <- trans$ssa_backend$ir$scheduled
  expect_identical(sched_ir$kind, "block")
  loop_nodes <- Filter(function(st) !is.null(st$kind) && identical(st$kind, "loop"), sched_ir$stmts)
  expect_true(length(loop_nodes) >= 1)
  expect_identical(loop_nodes[[1]]$schedule$unroll, 4L)

  cfg <- trans$ssa_backend$backend_cfg
  expect_identical(cfg$kind, "ssa_backend_cfg")
  expect_true(cfg$entry %in% names(cfg$blocks))
})

test_that("emit_ssa_backend validates logical scalar input", {  f <- function(x) sum(x)
  expect_error(
    mojor_transpile(f, x = "f64[]", emit_ssa_backend = c(TRUE, FALSE)),
    "emit_ssa_backend must be TRUE or FALSE"
  )
})

test_that("emit_ssa_backend retries backend prep at opt_level 0 after failure", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) out[i] <- x[i] + 1
    out
  }

  attempts <- integer(0)
  transpile_env <- environment(mojor_transpile)
  old_prepare <- get(".mojor_ir_prepare_ssa_backend", envir = transpile_env, inherits = TRUE)
  on.exit(assign(".mojor_ir_prepare_ssa_backend", old_prepare, envir = transpile_env), add = TRUE)
  assign(
    ".mojor_ir_prepare_ssa_backend",
    function(
      ir_or_expr,
      layout_ctx = list(n_var = "n_i", type_env = NULL),
      schedule = NULL,
      opt_level = 2,
      ssa_passes = c("prune_unreachable", "copy_propagate", "prune_dead_stmts"),
      verify = TRUE
    ) {
      attempts <<- c(attempts, as.integer(opt_level))
      if (opt_level > 0) stop("forced backend failure at opt>0")
      list(
        pipeline = c("schedule_ir", "to_ssa", "backend_cfg"),
        ir = list(scheduled = list(kind = "block")),
        ssa = list(
          raw = list(kind = "ssa_fn"),
          optimized = list(kind = "ssa_fn")
        ),
        backend_cfg = list(
          kind = "ssa_backend_cfg",
          entry = "entry",
          block_order = "entry",
          blocks = list(entry = list())
        )
      )
    },
    envir = transpile_env
  )

  trans <- mojor_transpile(
    f,
    x = "f64[]",
    n = "i32",
    emit_ssa_backend = TRUE,
    opt_level = 2L,
    name = "t_ir_emit_ssa_backend_retry"
  )

  expect_identical(attempts, c(2L, 0L))
  expect_true(is.list(trans$ssa_backend))
  expect_null(trans$ssa_backend_error)
})

test_that("emit_ssa_backend reports fallback failure message when retry also fails", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) out[i] <- x[i] + 1
    out
  }

  attempts <- integer(0)
  transpile_env <- environment(mojor_transpile)
  old_prepare <- get(".mojor_ir_prepare_ssa_backend", envir = transpile_env, inherits = TRUE)
  on.exit(assign(".mojor_ir_prepare_ssa_backend", old_prepare, envir = transpile_env), add = TRUE)
  assign(
    ".mojor_ir_prepare_ssa_backend",
    function(
      ir_or_expr,
      layout_ctx = list(n_var = "n_i", type_env = NULL),
      schedule = NULL,
      opt_level = 2,
      ssa_passes = c("prune_unreachable", "copy_propagate", "prune_dead_stmts"),
      verify = TRUE
    ) {
      attempts <<- c(attempts, as.integer(opt_level))
      stop(paste0("forced backend failure at opt=", opt_level))
    },
    envir = transpile_env
  )

  trans <- mojor_transpile(
    f,
    x = "f64[]",
    n = "i32",
    emit_ssa_backend = TRUE,
    opt_level = 2L,
    name = "t_ir_emit_ssa_backend_retry_fail"
  )

  expect_identical(attempts, c(2L, 0L))
  expect_null(trans$ssa_backend)
  expect_match(trans$ssa_backend_error, "forced backend failure at opt=0")
})
