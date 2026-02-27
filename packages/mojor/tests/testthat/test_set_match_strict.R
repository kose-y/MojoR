# =============================================================================
# Set/Match strict compiled-path tests
# =============================================================================

test_that("set/match expression kernels transpile in strict compiled path", {  t_unique <- mojor_transpile(function(x) unique(x), x = "f64[]", name = "t_unique_strict")
  expect_true(isTRUE(t_unique$is_expression_kernel))
  expect_true(isTRUE(t_unique$is_vector_output))
  expect_equal(t_unique$out_type, "f64[]")
  expect_true(isTRUE(t_unique$set_match_needed))
  expect_match(t_unique$mojo, "from set_match_helpers import mojor_unique_f64", fixed = TRUE)

  t_dup <- mojor_transpile(function(x) duplicated(x), x = "f64[]", name = "t_dup_strict")
  expect_true(isTRUE(t_dup$is_vector_output))
  expect_equal(t_dup$out_type, "lgl[]")
  expect_match(t_dup$mojo, "from set_match_helpers import mojor_duplicated_f64", fixed = TRUE)

  t_any_dup <- mojor_transpile(function(x) anyDuplicated(x), x = "f64[]", name = "t_any_dup_strict")
  expect_false(isTRUE(t_any_dup$is_vector_output))
  expect_equal(t_any_dup$return_type, "Int32")
  expect_match(t_any_dup$mojo, "from set_match_helpers import mojor_any_duplicated_f64", fixed = TRUE)

  t_match <- mojor_transpile(function(x, table) match(x, table), x = "f64[]", table = "f64[]", name = "t_match_strict")
  expect_true(isTRUE(t_match$is_vector_output))
  expect_equal(t_match$out_type, "i32[]")
  expect_match(t_match$mojo, "from set_match_helpers import mojor_match_f64", fixed = TRUE)

  t_in <- mojor_transpile(function(x, table) x %in% table, x = "f64[]", table = "f64[]", name = "t_in_strict")
  expect_true(isTRUE(t_in$is_vector_output))
  expect_equal(t_in$out_type, "lgl[]")
  expect_match(t_in$mojo, "from set_match_helpers import mojor_in_f64", fixed = TRUE)
})

test_that("match/%in% reject mismatched element types in strict compiled path", {  expect_error(
    mojor_transpile(function(x, table) match(x, table), x = "f64[]", table = "i32[]", name = "t_match_type_mismatch"),
    "matching element types"
  )

  expect_error(
    mojor_transpile(function(x, table) x %in% table, x = "i32[]", table = "f64[]", name = "t_in_type_mismatch"),
    "matching element types"
  )
})

test_that("unique/duplicated/anyDuplicated compiled kernels match R", {  skip_if_no_mojo()

  x <- as.double(c(2, NaN, 2, 4, NaN, 4, 5))

  b_unique <- mojor_build(function(x) unique(x), x = "f64[]", name = "t_unique_rt_strict", cache = FALSE, load = TRUE)
  expect_equal(b_unique$func(x), unique(x))

  b_dup <- mojor_build(function(x) duplicated(x), x = "f64[]", name = "t_dup_rt_strict", cache = FALSE, load = TRUE)
  expect_equal(b_dup$func(x), duplicated(x))

  b_any_dup <- mojor_build(function(x) anyDuplicated(x), x = "f64[]", name = "t_any_dup_rt_strict", cache = FALSE, load = TRUE)
  expect_equal(b_any_dup$func(x), anyDuplicated(x))
})

test_that("match/%in% compiled kernels match R for double vectors", {  skip_if_no_mojo()

  x <- as.double(c(2, NaN, 6, NaN, 1))
  table <- as.double(c(1, NaN, 2, 4))

  b_match <- mojor_build(
    function(x, table) match(x, table),
    x = "f64[]", table = "f64[]",
    name = "t_match_rt_strict", cache = FALSE, load = TRUE
  )
  expect_equal(b_match$func(x, table), match(x, table))

  b_in <- mojor_build(
    function(x, table) x %in% table,
    x = "f64[]", table = "f64[]",
    name = "t_in_rt_strict", cache = FALSE, load = TRUE
  )
  got_in <- b_in$func(x, table)
  expect_equal(got_in, x %in% table)
  expect_false(any(is.na(got_in)))
})

test_that("%in% missing unmatched values are FALSE (not NA)", {  skip_if_no_mojo()

  b_in_i32 <- mojor_build(
    function(x, table) x %in% table,
    x = "i32[]", table = "i32[]",
    name = "t_in_i32_na_nomatch", cache = FALSE, load = TRUE
  )

  x <- as.integer(c(1L, NA_integer_, 3L))
  table <- as.integer(c(1L, 3L))
  got <- b_in_i32$func(x, table)
  expect_equal(got, x %in% table)
  expect_false(any(is.na(got)))
})
