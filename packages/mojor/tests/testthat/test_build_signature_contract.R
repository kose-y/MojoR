library(testthat)

test_that("signature param parser extracts stable parameter names", {
  sig <- c(
    "x_ptr: ImmutOpaqueAny",
    "__mojor_out_ptr: MutOpaqueAny",
    "__mojor_n: Int32",
    "__mojor_len_x: Int32"
  )
  expect_equal(
    .mojor_signature_param_names(sig),
    c("x_ptr", "__mojor_out_ptr", "__mojor_n", "__mojor_len_x")
  )
})

test_that("expected signature names include alias-output length parameter", {
  expected <- .mojor_expected_kernel_signature_param_names(
    args = c("x", "n"),
    arg_specs = list(x = "f64[]", n = "i32"),
    out_kind = "vector",
    out_name = "x",
    len_arrays = "x"
  )
  expect_true("__mojor_out_ptr" %in% expected)
  expect_true("__mojor_len_x" %in% expected)
})

test_that("signature validator fails fast on mismatched parameter order", {
  trans <- list(
    signature = c(
      "x_ptr: ImmutOpaqueAny",
      "__mojor_n: Int32",
      "__mojor_out_ptr: MutOpaqueAny",
      "__mojor_len_x: Int32"
    )
  )
  expect_error(
    .mojor_validate_kernel_signature_contract(
      trans = trans,
      args = "x",
      arg_specs = list(x = "f64[]"),
      out_kind = "vector",
      out_name = "x",
      len_arrays = "x"
    ),
    "kernel signature parameter order mismatch",
    fixed = TRUE
  )
})

test_that("transpile alias-output signature satisfies build contract", {
  f_alias <- function(x) {
    x[1] <- x[1] + 1.0
    x
  }

  trans <- mojor_transpile(
    f_alias,
    x = "f64[]",
    name = "sig_contract_alias_output"
  )
  params <- .mojor_signature_param_names(trans$signature)
  expect_true("__mojor_out_ptr" %in% params)
  expect_true("__mojor_len_x" %in% params)
  expect_error(
    .mojor_validate_kernel_signature_contract(
      trans = trans,
      args = names(formals(f_alias)),
      arg_specs = trans$kernel_types,
      out_kind = trans$out_kind,
      out_name = trans$out_name,
      rng_needed = isTRUE(trans$rng_needed),
      len_arrays = trans$len_arrays,
      nrow_arrays = trans$nrow_arrays,
      ncol_arrays = trans$ncol_arrays,
      dim_arrays = trans$dim_arrays,
      out_matrix = isTRUE(trans$out_matrix),
      out_array = isTRUE(trans$out_array),
      broadcast_nd = isTRUE(trans$broadcast_nd),
      na_needed = (trans$na_mode %in% c("forbid", "propagate") || isTRUE(trans$index_bounds))
    ),
    NA
  )
})

test_that("selector index signatures carry selector length parameters", {
  f_selector <- function(x, mask, n) {
    out <- numeric(n)
    out[1:n] <- x[mask]
    out
  }

  trans <- mojor_transpile(
    f_selector,
    x = "f64[]",
    mask = "i32[]",
    n = "i32",
    name = "sig_contract_selector_len"
  )
  params <- .mojor_signature_param_names(trans$signature)
  expect_true("__mojor_len_mask" %in% params)
  expect_error(
    .mojor_validate_kernel_signature_contract(
      trans = trans,
      args = names(formals(f_selector)),
      arg_specs = trans$kernel_types,
      out_kind = trans$out_kind,
      out_name = trans$out_name,
      rng_needed = isTRUE(trans$rng_needed),
      len_arrays = trans$len_arrays,
      nrow_arrays = trans$nrow_arrays,
      ncol_arrays = trans$ncol_arrays,
      dim_arrays = trans$dim_arrays,
      out_matrix = isTRUE(trans$out_matrix),
      out_array = isTRUE(trans$out_array),
      broadcast_nd = isTRUE(trans$broadcast_nd),
      na_needed = (trans$na_mode %in% c("forbid", "propagate") || isTRUE(trans$index_bounds))
    ),
    NA
  )
})
