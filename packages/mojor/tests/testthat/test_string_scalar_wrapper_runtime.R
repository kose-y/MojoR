library(testthat)

.mojor_fn_off_string <- function(fn, ..., .ir_only = NULL, .load = TRUE) {
  args <- c(list(fn, object_mode = "off", cache = FALSE, load = .load), list(...))
  if (!is.null(.ir_only)) args$ir_only <- .ir_only
  do.call(mojor_fn, args)
}

.mojor_fn_to_callable <- function(compiled) {
  if (is.function(compiled)) {
    return(compiled)
  }
  compiled$func
}

test_that("string wrapper transpile metadata supports chr scalar and mixed lanes", {
  t_nchar <- mojor_transpile(
    function(x) nchar(x),
    x = "chr",
    name = "t_string_scalar_nchar"
  )
  expect_true(isTRUE(t_nchar$string_builtin))
  expect_identical(t_nchar$out_type, "i32")
  expect_false(isTRUE(t_nchar$is_vector_output))
  expect_null(t_nchar$vector_len_arg)

  t_paste_mixed <- mojor_transpile(
    function(x, y) paste(x, y, sep = "|"),
    x = "chr",
    y = "chr[]",
    name = "t_string_scalar_paste_mixed"
  )
  expect_true(isTRUE(t_paste_mixed$string_builtin))
  expect_identical(t_paste_mixed$out_type, "chr[]")
  expect_true(isTRUE(t_paste_mixed$is_vector_output))
  expect_identical(t_paste_mixed$vector_len_arg, "y")

  t_paste_collapse <- mojor_transpile(
    function(x, y) paste(x, y, sep = "|", collapse = "::"),
    x = "chr",
    y = "chr[]",
    name = "t_string_scalar_paste_collapse"
  )
  expect_true(isTRUE(t_paste_collapse$string_builtin))
  expect_identical(t_paste_collapse$out_type, "chr")
  expect_false(isTRUE(t_paste_collapse$is_vector_output))
  expect_null(t_paste_collapse$vector_len_arg)
})

test_that("string wrapper runtime supports chr scalar and mixed lanes", {
  x_scalar <- "abcd"
  y_scalar <- "Z"
  x_vec <- c("a", "bc", NA_character_, "")
  y_vec <- c("1", "2", "3", "4")

  built_meta <- .mojor_fn_off_string(function(x) nchar(x), x = "chr", .load = FALSE)
  expect_true(isTRUE(built_meta$is_expression_kernel))

  f_nchar <- .mojor_fn_to_callable(.mojor_fn_off_string(function(x) nchar(x), x = "chr"))
  expect_equal(unname(f_nchar(x_scalar)), unname(nchar(x_scalar)))

  f_nzchar <- .mojor_fn_to_callable(.mojor_fn_off_string(function(x) nzchar(x), x = "chr"))
  expect_equal(unname(f_nzchar(x_scalar)), unname(nzchar(x_scalar)))

  f_substr <- .mojor_fn_to_callable(
    .mojor_fn_off_string(function(x, start, stop) substr(x, start, stop), x = "chr", start = "i32", stop = "i32")
  )
  expect_equal(unname(f_substr(x_scalar, 2L, 3L)), unname(substr(x_scalar, 2L, 3L)))

  f_paste_mixed <- .mojor_fn_to_callable(
    .mojor_fn_off_string(function(x, y) paste(x, y, sep = "|"), x = "chr", y = "chr[]")
  )
  expect_equal(unname(f_paste_mixed(x_scalar, y_vec)), unname(paste(x_scalar, y_vec, sep = "|")))

  f_paste0_mixed <- .mojor_fn_to_callable(
    .mojor_fn_off_string(function(x, y) paste0(x, y), x = "chr[]", y = "chr")
  )
  expect_equal(unname(f_paste0_mixed(x_vec, y_scalar)), unname(paste0(x_vec, y_scalar)))

  f_paste0_scalar <- .mojor_fn_to_callable(
    .mojor_fn_off_string(function(x, y) paste0(x, y), x = "chr", y = "chr")
  )
  expect_equal(unname(f_paste0_scalar(x_scalar, y_scalar)), unname(paste0(x_scalar, y_scalar)))

  f_paste_collapse <- .mojor_fn_to_callable(
    .mojor_fn_off_string(function(x, y) paste(x, y, sep = "|", collapse = "::"), x = "chr", y = "chr[]")
  )
  expect_equal(
    unname(f_paste_collapse(x_scalar, y_vec)),
    unname(paste(x_scalar, y_vec, sep = "|", collapse = "::"))
  )

  f_paste0_collapse <- .mojor_fn_to_callable(
    .mojor_fn_off_string(function(x, y) paste0(x, y, collapse = "::"), x = "chr[]", y = "chr")
  )
  expect_equal(
    unname(f_paste0_collapse(x_vec, y_scalar)),
    unname(paste0(x_vec, y_scalar, collapse = "::"))
  )
})

test_that("string wrapper runtime supports chr scalar lanes under ir_only", {
  x_scalar <- "abcd"
  y_scalar <- "Z"

  f_nchar <- .mojor_fn_to_callable(
    .mojor_fn_off_string(function(x) nchar(x), x = "chr", .ir_only = TRUE)
  )
  expect_equal(unname(f_nchar(x_scalar)), unname(nchar(x_scalar)))

  f_paste0 <- .mojor_fn_to_callable(
    .mojor_fn_off_string(function(x, y) paste0(x, y), x = "chr", y = "chr", .ir_only = TRUE)
  )
  expect_equal(unname(f_paste0(x_scalar, y_scalar)), unname(paste0(x_scalar, y_scalar)))
})
