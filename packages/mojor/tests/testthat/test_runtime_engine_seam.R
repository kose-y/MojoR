library(testthat)

.with_env_restore <- function(var, value = NULL) {
  old <- Sys.getenv(var, unset = NA_character_)
  if (is.null(value)) {
    Sys.unsetenv(var)
  } else {
    Sys.setenv(value)
  }
  function() {
    if (is.na(old)) {
      Sys.unsetenv(var)
    } else {
      do.call(Sys.setenv, stats::setNames(list(old), var))
    }
  }
}

test_that("runtime v1 symbol candidates support both naming conventions", {
  expect_identical(
    .mojor_runtime_symbol_v1_candidates("mojor_sum_f64"),
    c("mojor_rt_v1_mojor_sum_f64", "mojor_rt_v1_sum_f64")
  )
  expect_identical(
    .mojor_runtime_symbol_v1_candidates("kernel_call"),
    "mojor_rt_v1_kernel_call"
  )
  expect_identical(
    .mojor_runtime_symbol_v1_candidates("mojor_rt_v1_sum_f64"),
    "mojor_rt_v1_sum_f64"
  )
})

test_that("runtime v1 symbol resolution caches misses", {
  old_cache <- .mojor_state$runtime_v1_symbol_cache
  on.exit({
    .mojor_state$runtime_v1_symbol_cache <- old_cache
  }, add = TRUE)
  .mojor_state$runtime_v1_symbol_cache <- list()

  pkg <- "mojor_bridge"
  symbol <- "mojor__runtime_cache_probe_missing__"
  key <- .mojor_runtime_v1_cache_key(pkg, symbol)

  expect_null(.mojor_runtime_v1_resolve_symbol(pkg = pkg, symbol = symbol))
  expect_true(is.na(.mojor_state$runtime_v1_symbol_cache[[key]]))
  expect_null(.mojor_runtime_v1_resolve_symbol(pkg = pkg, symbol = symbol))
})

test_that("binding bridge resets runtime v1 cache", {
  old_cache <- .mojor_state$runtime_v1_symbol_cache
  old_pkg <- .mojor_state$bridge_pkg
  old_path <- .mojor_state$bridge_path
  on.exit({
    .mojor_state$runtime_v1_symbol_cache <- old_cache
    .mojor_state$bridge_pkg <- old_pkg
    .mojor_state$bridge_path <- old_path
  }, add = TRUE)

  .mojor_state$runtime_v1_symbol_cache <- list(foo = "bar")
  .mojor_bind_bridge(pkg = "mojor_bridge", path = NULL)
  expect_identical(length(.mojor_state$runtime_v1_symbol_cache), 0L)
})

test_that("runtime bridge dispatch rejects missing v1 symbols", {
  bridge_pkg <- .mojor_bridge_pkg(required = FALSE)
  if (is.null(bridge_pkg) || !nzchar(bridge_pkg)) {
    skip("bridge package not loaded in this test environment")
  }

  expect_error(
    .mojor_runtime_dispatch_call("mojor__definitely_missing__", pkg = bridge_pkg),
    "runtime v1 dispatch failure"
  )
})

test_that("runtime dispatch preserves direct non-bridge call behavior", {
  err <- tryCatch(
    {
      .mojor_runtime_dispatch_call("mojor__definitely_missing__", pkg = "stats")
      NULL
    },
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_false(grepl("runtime v1 dispatch failure", conditionMessage(err), fixed = TRUE))
})

test_that("unified ir service mode is fixed and typed lir payload uses unified tag", {
  restore_payload <- .with_env_restore("MOJOR_EMIT_TYPED_LIR_PAYLOAD", NULL)
  on.exit(restore_payload(), add = TRUE)

  expect_identical(.mojor_ir_service_mode(), "unified")
  expect_false(.mojor_emit_typed_lir_payload_enabled())

  Sys.setenv(MOJOR_EMIT_TYPED_LIR_PAYLOAD = "1")

  expect_true(.mojor_emit_typed_lir_payload_enabled())

  payload <- .mojor_ir_serialize_typed_lir(list(kind = "assign", lhs = list(kind = "var", name = "x")))
  expect_true(is.list(payload))
  expect_identical(payload$schema_version, 1L)
  expect_identical(payload$format, "dput")
  expect_identical(payload$service, "unified")
  expect_true(is.character(payload$payload) && nzchar(payload$payload))
})

test_that("deprecated ir split env vars are ignored with deterministic warning", {
  restore_service <- .with_env_restore("MOJOR_IR_SERVICE", NULL)
  restore_shadow <- .with_env_restore("MOJOR_IR_EMIT_SHADOW", NULL)
  on.exit(restore_service(), add = TRUE)
  on.exit(restore_shadow(), add = TRUE)
  Sys.setenv(MOJOR_IR_SERVICE = "mojo_pilot", MOJOR_IR_EMIT_SHADOW = "1")

  warned <- character(0)
  warn_msg <- "mojor_transpile: MOJOR_IR_SERVICE and MOJOR_IR_EMIT_SHADOW are deprecated and ignored; unified IR emitter is always used"
  fn <- function(x) {
    x + 1
  }
  tr <- withCallingHandlers(
    mojor_transpile(fn, x = "f64[]"),
    warning = function(w) {
      warned <<- c(warned, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(is.list(tr))
  expect_identical(sum(warned == warn_msg), 1L)
})

test_that("unified ir emitter hard-fails unsupported kinds", {
  assign_node <- list(
    kind = "assign",
    lhs = list(kind = "var", name = "out"),
    rhs = list(kind = "const", value = 1)
  )
  assign_lines <- .mojor_ir_emit_stmt_with_service(
    ir_typed = assign_node,
    indent = "    ",
    zero_based_vars = character(0),
    out_name = "out",
    na_guard = "forbid",
    bounds_check = FALSE,
    loop_var = NULL,
    scalar_name = NULL,
    type_env = list(out = "f64"),
    unroll = NULL,
    schedule = NULL
  )
  expect_true(is.character(assign_lines))
  expect_true(length(assign_lines) >= 1)

  expect_error(
    .mojor_ir_emit_stmt_with_service(
      ir_typed = list(kind = "raw"),
      indent = "    ",
      zero_based_vars = character(0),
      out_name = "out",
      na_guard = "forbid",
      bounds_check = FALSE,
      loop_var = NULL,
      scalar_name = NULL,
      type_env = list(out = "f64"),
      unroll = NULL,
      schedule = NULL
    ),
    "unified IR emitter does not support IR kind 'raw'",
    fixed = TRUE
  )
  expect_error(
    .mojor_ir_emit_stmt_with_service(
      ir_typed = list(kind = "mystery"),
      indent = "    ",
      zero_based_vars = character(0),
      out_name = "out",
      na_guard = "forbid",
      bounds_check = FALSE,
      loop_var = NULL,
      scalar_name = NULL,
      type_env = list(out = "f64"),
      unroll = NULL,
      schedule = NULL
    ),
    "unified IR emitter does not support IR kind 'mystery'",
    fixed = TRUE
  )
})

test_that("transpile output no longer includes ir_service metadata", {
  restore_service <- .with_env_restore("MOJOR_IR_SERVICE", NULL)
  restore_shadow <- .with_env_restore("MOJOR_IR_EMIT_SHADOW", NULL)
  on.exit(restore_service(), add = TRUE)
  on.exit(restore_shadow(), add = TRUE)
  tr <- suppressWarnings(mojor_transpile(function(x) x + 1, x = "f64[]"))
  expect_false("ir_service" %in% names(tr))
})
