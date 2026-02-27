test_that("runif-only kernels do not materialize ziggurat tables", {  f <- function(n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- runif(1)
    }
    out
  }

  trans <- mojor_transpile(f, n = "i32", name = "rng_runif_only")
  rng_import_line <- grep("^from rng_helpers import ", strsplit(trans$mojo, "\n", fixed = TRUE)[[1]], value = TRUE)
  expect_equal(length(rng_import_line), 1)
  expect_false(grepl("from ziggurat_constants import", trans$mojo, fixed = TRUE))
  expect_false(grepl("materialize\\[_KI_DOUBLE\\]", trans$mojo))
  expect_false(grepl("materialize\\[_WI_DOUBLE\\]", trans$mojo))
  expect_false(grepl("materialize\\[_FI_DOUBLE\\]", trans$mojo))
  expect_true(grepl("_rng_next_f64", trans$mojo, fixed = TRUE))
  expect_true(grepl("_rng_next_f64", rng_import_line, fixed = TRUE))
  expect_false(grepl("_random_standard_normal", rng_import_line, fixed = TRUE))
  expect_false(grepl("_random_poisson", rng_import_line, fixed = TRUE))
})

test_that("rnorm kernels still materialize ziggurat tables", {  f <- function(n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- rnorm(1)
    }
    out
  }

  trans <- mojor_transpile(f, n = "i32", name = "rng_rnorm_tables")
  rng_import_line <- grep("^from rng_helpers import ", strsplit(trans$mojo, "\n", fixed = TRUE)[[1]], value = TRUE)
  expect_equal(length(rng_import_line), 1)
  expect_true(grepl("from ziggurat_constants import", trans$mojo, fixed = TRUE))
  expect_true(grepl("materialize\\[_KI_DOUBLE\\]", trans$mojo))
  expect_true(grepl("materialize\\[_WI_DOUBLE\\]", trans$mojo))
  expect_true(grepl("materialize\\[_FI_DOUBLE\\]", trans$mojo))
  expect_true(grepl("_random_standard_normal", rng_import_line, fixed = TRUE))
  expect_false(grepl("_rng_next_f64", rng_import_line, fixed = TRUE))
  expect_false(grepl("_random_poisson", rng_import_line, fixed = TRUE))
})

test_that("mixed RNG kernels import only required RNG helpers", {  f <- function(n, lam) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- rexp(1, 2) + rpois(1, lam)
    }
    out
  }

  trans <- mojor_transpile(f, n = "i32", lam = "f64", name = "rng_rexp_rpois_imports")
  rng_import_line <- grep("^from rng_helpers import ", strsplit(trans$mojo, "\n", fixed = TRUE)[[1]], value = TRUE)
  expect_equal(length(rng_import_line), 1)
  expect_true(grepl("_rng_next_f64", rng_import_line, fixed = TRUE))
  expect_true(grepl("_random_poisson", rng_import_line, fixed = TRUE))
  expect_false(grepl("_random_beta", rng_import_line, fixed = TRUE))
  expect_false(grepl("_random_standard_normal", rng_import_line, fixed = TRUE))
})

test_that("RNG call collector stays in sync with shared RNG helper list", {  rng_fns <- .mojor_ir_rng_call_fns()
  calls <- lapply(rng_fns, function(fn) as.call(list(as.name(fn), 1)))
  found <- unique(unlist(lapply(calls, .mojor_collect_rng_calls)))

  expect_setequal(found, rng_fns)
  expect_true(.mojor_blocks_need_rng(calls))
})

test_that("RNG table detection matches shared table helper list", {  table_rng_fns <- .mojor_ir_rng_table_fns()
  non_table_rng_fns <- setdiff(.mojor_ir_rng_call_fns(), table_rng_fns)

  for (fn in table_rng_fns) {
    expr <- as.call(list(as.name(fn), 1))
    expect_true(.mojor_blocks_need_rng_tables(list(expr)), label = fn)
  }
  for (fn in non_table_rng_fns) {
    expr <- as.call(list(as.name(fn), 1))
    expect_false(.mojor_blocks_need_rng_tables(list(expr)), label = fn)
  }
})

test_that("scalar RNG emitter supports all registered distributions", {  meta <- .mojor_ir_rng_metadata()
  min_params <- vapply(meta, function(x) as.integer(x$min_params[[1]]), integer(1))
  max_params <- vapply(meta, function(x) as.integer(x$max_params[[1]]), integer(1))

  for (fn in names(meta)) {
    n_param <- unname(min_params[[fn]])
    params <- if (n_param > 0) rep("1.0", n_param) else character(0)
    expr <- .mojor_ir_scalar_rng_emit(fn, params)
    expect_false(is.null(expr), label = fn)
    expect_true(nzchar(expr), label = fn)
  }

  for (fn in names(min_params[min_params > 0])) {
    n_param <- unname(min_params[[fn]])
    too_few <- if (n_param > 1L) rep("1.0", n_param - 1L) else character(0)
    expect_null(.mojor_ir_scalar_rng_emit(fn, too_few), label = fn)
  }
  for (fn in names(max_params)) {
    n_param <- unname(max_params[[fn]])
    too_many <- rep("1.0", n_param + 1L)
    expect_null(.mojor_ir_scalar_rng_emit(fn, too_many), label = fn)
  }
  expect_null(.mojor_ir_scalar_rng_emit("r_not_supported", c("1.0")))
})

test_that("RNG helper symbol resolver is precise and strict on unknown calls", {  expect_setequal(
    .mojor_rng_helper_symbols_for_calls(c("runif", "rpois")),
    c("_rng_next_f64", "_random_poisson")
  )
  expect_setequal(
    .mojor_rng_helper_symbols_for_calls(c("rnorm", "rnbinom")),
    c("_random_standard_normal", "_random_standard_gamma", "_random_poisson")
  )
  expect_error(
    .mojor_rng_helper_symbols_for_calls(c("r_not_supported")),
    "unknown RNG call"
  )
})

test_that("RNG metadata stays aligned across call/table/helper views", {  catalog_meta <- .mojor_rng_catalog_metadata()
  meta <- .mojor_ir_rng_metadata()
  map <- .mojor_ir_rng_helper_symbol_map()

  expect_setequal(names(catalog_meta), .mojor_rng_catalog_call_fns())
  expect_setequal(names(catalog_meta), names(meta))
  expect_setequal(names(meta), .mojor_ir_rng_call_fns())
  expect_setequal(names(map), .mojor_ir_rng_call_fns())
  expect_setequal(.mojor_rng_catalog_table_fns(), .mojor_ir_rng_table_fns())
  expect_setequal(.mojor_rng_catalog_helper_symbols(), .mojor_rng_all_helper_symbols())

  table_from_meta <- names(meta)[vapply(meta, function(x) isTRUE(x$needs_tables), logical(1))]
  expect_setequal(table_from_meta, .mojor_ir_rng_table_fns())

  for (fn in .mojor_ir_rng_call_fns()) {
    expect_true(is.numeric(meta[[fn]]$min_params) && length(meta[[fn]]$min_params) == 1L, label = fn)
    expect_true(is.numeric(meta[[fn]]$max_params) && length(meta[[fn]]$max_params) == 1L, label = fn)
    expect_true(as.integer(meta[[fn]]$min_params[[1]]) <= as.integer(meta[[fn]]$max_params[[1]]), label = fn)
    expect_setequal(.mojor_rng_helper_symbols_for_calls(fn), map[[fn]])
  }
})
