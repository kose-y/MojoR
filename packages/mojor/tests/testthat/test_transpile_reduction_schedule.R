test_that("schedule transforms return first-class scheduled_reduce nodes", {  for (op in c("sum", "product", "min")) {
    node <- .mojor_ir_scalar_reduce(
      op = op,
      acc = "acc",
      arg = "x",
      associative = TRUE,
      commutative = TRUE
    )

    tree <- .mojor_ir_schedule_tree_reduce(
      node,
      list(n_var = "n_i", type_env = list(x = "f64[]"))
    )
    simd <- .mojor_ir_schedule_simd_reduce(
      node,
      list(n_var = "n_i", type_env = list(x = "f64[]"))
    )

    expect_identical(tree$kind, "scheduled_reduce")
    expect_identical(tree$mode, "tree")
    expect_identical(simd$kind, "scheduled_reduce")
    expect_identical(simd$mode, "simd")
    expect_silent(.mojor_ir_verify(tree))
    expect_silent(.mojor_ir_verify(simd))
  }
})

test_that("scalar min reduction routes through tree schedule transform", {  f <- function(x) min(x)

  trans <- expect_no_warning(
    mojor_transpile(f, x = "f64[]", reduction = "tree")
  )

  code <- paste(trans$mojo, collapse = "\n")
  expect_match(code, "# Tree reduction for min(x)", fixed = TRUE)
  expect_match(code, "var temp = alloc[Float64](n_i)", fixed = TRUE)
  expect_match(code, "if n_i == Int(0):", fixed = TRUE)
  expect_match(code, "acc = _MOJOR_NAN", fixed = TRUE)
  expect_false(grepl("for __mojor_ri in range\\(2, n_i \\+ 1\\):", code))
})

test_that("scalar min reduction routes through simd schedule transform", {  f <- function(x) min(x)

  trans <- expect_no_warning(
    mojor_transpile(f, x = "f64[]", reduction = "simd")
  )

  code <- paste(trans$mojo, collapse = "\n")
  expect_match(code, "# SIMD reduction for min(x)", fixed = TRUE)
  expect_match(code, "comptime simd_width = simd_width_of[DType.float64]()", fixed = TRUE)
  expect_match(code, "var vec_acc = SIMD[DType.float64, simd_width](_MOJOR_INF)", fixed = TRUE)
  expect_match(code, "if n_i == Int(0):", fixed = TRUE)
  expect_match(code, "acc = _MOJOR_NAN", fixed = TRUE)
  expect_false(grepl("for __mojor_ri in range\\(2, n_i \\+ 1\\):", code))
})

test_that("scalar sum/product reductions route through schedule transforms", {  f_sum <- function(x) sum(x)
  f_prod <- function(x) prod(x)

  trans_sum_tree <- expect_no_warning(mojor_transpile(f_sum, x = "f64[]", reduction = "tree"))
  code_sum_tree <- paste(trans_sum_tree$mojo, collapse = "\n")
  expect_match(code_sum_tree, "# Tree reduction for sum(x)", fixed = TRUE)
  expect_match(code_sum_tree, "acc = 0.0", fixed = TRUE)
  expect_false(grepl("acc = _MOJOR_NAN", code_sum_tree, fixed = TRUE))

  trans_sum_simd <- expect_no_warning(mojor_transpile(f_sum, x = "f64[]", reduction = "simd"))
  code_sum_simd <- paste(trans_sum_simd$mojo, collapse = "\n")
  expect_match(code_sum_simd, "# SIMD reduction for sum(x)", fixed = TRUE)
  expect_match(code_sum_simd, "vec_acc = SIMD[DType.float64, simd_width](0.0)", fixed = TRUE)
  expect_match(code_sum_simd, "acc = 0.0", fixed = TRUE)

  trans_prod_tree <- expect_no_warning(mojor_transpile(f_prod, x = "f64[]", reduction = "tree"))
  code_prod_tree <- paste(trans_prod_tree$mojo, collapse = "\n")
  expect_match(code_prod_tree, "# Tree reduction for product(x)", fixed = TRUE)
  expect_match(code_prod_tree, "acc = 1.0", fixed = TRUE)

  trans_prod_simd <- expect_no_warning(mojor_transpile(f_prod, x = "f64[]", reduction = "simd"))
  code_prod_simd <- paste(trans_prod_simd$mojo, collapse = "\n")
  expect_match(code_prod_simd, "# SIMD reduction for product(x)", fixed = TRUE)
  expect_match(code_prod_simd, "vec_acc = SIMD[DType.float64, simd_width](1.0)", fixed = TRUE)
  expect_match(code_prod_simd, "acc = 1.0", fixed = TRUE)
})

test_that("which.max routes through tie-stable tree schedule transform", {  node <- .mojor_ir_scalar_reduce(
    op = "which.max",
    acc = "idx",
    arg = "x",
    associative = FALSE,
    commutative = FALSE
  )

  tree <- .mojor_ir_schedule_tree_reduce(
    node,
    list(n_var = "n_i", type_env = list(x = "f64[]"))
  )
  simd <- .mojor_ir_schedule_simd_reduce(
    node,
    list(n_var = "n_i", type_env = list(x = "f64[]"))
  )

  expect_identical(tree$kind, "scheduled_reduce")
  expect_identical(tree$mode, "tree")
  expect_identical(tree$op, "which.max")
  expect_identical(tree$empty_val, "Int32(0)")
  expect_identical(simd$kind, "scheduled_reduce")
  expect_identical(simd$mode, "simd")
  expect_identical(simd$op, "which.max")
  expect_silent(.mojor_ir_verify(tree))
  expect_silent(.mojor_ir_verify(simd))

  f <- function(x) which.max(x)
  trans <- expect_no_warning(
    mojor_transpile(f, x = "f64[]", reduction = "tree")
  )
  code <- paste(trans$mojo, collapse = "\n")
  expect_match(code, "# Tree arg-reduction for which.max(x) (tie-stable: first index)", fixed = TRUE)
  expect_match(code, "temp_idx[__mojor_tr_j] = min(__mojor_li, __mojor_ri)", fixed = TRUE)
  expect_match(code, "idx = temp_idx[0]", fixed = TRUE)
  expect_false(grepl("var has_value = False", code, fixed = TRUE))
})

test_that("which.min routes through tie-stable tree schedule transform", {  node <- .mojor_ir_scalar_reduce(
    op = "which.min",
    acc = "idx",
    arg = "x",
    associative = FALSE,
    commutative = FALSE
  )

  tree <- .mojor_ir_schedule_tree_reduce(
    node,
    list(n_var = "n_i", type_env = list(x = "f64[]"))
  )
  simd <- .mojor_ir_schedule_simd_reduce(
    node,
    list(n_var = "n_i", type_env = list(x = "f64[]"))
  )

  expect_identical(tree$kind, "scheduled_reduce")
  expect_identical(tree$mode, "tree")
  expect_identical(tree$op, "which.min")
  expect_identical(tree$empty_val, "Int32(0)")
  expect_identical(simd$kind, "scheduled_reduce")
  expect_identical(simd$mode, "simd")
  expect_identical(simd$op, "which.min")
  expect_silent(.mojor_ir_verify(tree))
  expect_silent(.mojor_ir_verify(simd))

  f <- function(x) which.min(x)
  trans <- expect_no_warning(
    mojor_transpile(f, x = "f64[]", reduction = "tree")
  )
  code <- paste(trans$mojo, collapse = "\n")
  expect_match(code, "# Tree arg-reduction for which.min(x) (tie-stable: first index)", fixed = TRUE)
  expect_match(code, "temp_idx[__mojor_tr_j] = min(__mojor_li, __mojor_ri)", fixed = TRUE)
  expect_match(code, "idx = temp_idx[0]", fixed = TRUE)
  expect_false(grepl("var has_value = False", code, fixed = TRUE))
})

test_that("SIMD reduction schedules i32 sum/product", {  f_sum <- function(x) sum(x)
  f_prod <- function(x) prod(x)

  trans_sum <- expect_no_warning(mojor_transpile(f_sum, x = "i32[]", reduction = "simd"))
  code_sum <- paste(trans_sum$mojo, collapse = "\n")
  expect_match(code_sum, "# SIMD reduction for sum(x)", fixed = TRUE)
  expect_match(code_sum, "comptime simd_width = simd_width_of[DType.int32]()", fixed = TRUE)
  expect_match(code_sum, "vec_acc = SIMD[DType.int32, simd_width](Int32(0))", fixed = TRUE)
  expect_false(grepl("for __mojor_ri in range\\(1, ", code_sum))

  trans_prod <- expect_no_warning(mojor_transpile(f_prod, x = "i32[]", reduction = "simd"))
  code_prod <- paste(trans_prod$mojo, collapse = "\n")
  expect_match(code_prod, "# SIMD reduction for product(x)", fixed = TRUE)
  expect_match(code_prod, "comptime simd_width = simd_width_of[DType.int32]()", fixed = TRUE)
  expect_match(code_prod, "vec_acc = SIMD[DType.int32, simd_width](Int32(1))", fixed = TRUE)
  expect_false(grepl("for __mojor_ri in range\\(1, ", code_prod))
})

test_that("SIMD reduction schedules which.min/which.max", {  f_min <- function(x) which.min(x)
  f_max <- function(x) which.max(x)

  trans_min <- expect_no_warning(mojor_transpile(f_min, x = "f64[]", reduction = "simd"))
  code_min <- paste(trans_min$mojo, collapse = "\n")
  expect_match(code_min, "# SIMD arg-reduction for which.min(x) (tie-stable: first index)", fixed = TRUE)
  expect_match(code_min, "comptime simd_width = simd_width_of[DType.float64]()", fixed = TRUE)
  expect_match(code_min, "var __mojor_best_idx: Int32 = Int32(1)", fixed = TRUE)
  expect_false(grepl("var has_value = False", code_min, fixed = TRUE))

  trans_max <- expect_no_warning(mojor_transpile(f_max, x = "f64[]", reduction = "simd"))
  code_max <- paste(trans_max$mojo, collapse = "\n")
  expect_match(code_max, "# SIMD arg-reduction for which.max(x) (tie-stable: first index)", fixed = TRUE)
  expect_match(code_max, "comptime simd_width = simd_width_of[DType.float64]()", fixed = TRUE)
  expect_match(code_max, "var __mojor_best_idx: Int32 = Int32(1)", fixed = TRUE)
  expect_false(grepl("var has_value = False", code_max, fixed = TRUE))
})

test_that("SIMD arg-reductions emit dtype-specific code for i32[] and f32[]", {  f_min <- function(x) which.min(x)
  f_max <- function(x) which.max(x)

  code_min_i32 <- paste(mojor_transpile(f_min, x = "i32[]", reduction = "simd")$mojo, collapse = "\n")
  code_max_i32 <- paste(mojor_transpile(f_max, x = "i32[]", reduction = "simd")$mojo, collapse = "\n")
  code_min_f32 <- paste(mojor_transpile(f_min, x = "f32[]", reduction = "simd")$mojo, collapse = "\n")
  code_max_f32 <- paste(mojor_transpile(f_max, x = "f32[]", reduction = "simd")$mojo, collapse = "\n")

  expect_match(code_min_i32, "# SIMD arg-reduction for which.min(x) (tie-stable: first index)", fixed = TRUE)
  expect_match(code_max_i32, "# SIMD arg-reduction for which.max(x) (tie-stable: first index)", fixed = TRUE)
  expect_match(code_min_i32, "comptime simd_width = simd_width_of[DType.int32]()", fixed = TRUE)
  expect_match(code_max_i32, "comptime simd_width = simd_width_of[DType.int32]()", fixed = TRUE)
  expect_match(code_min_i32, "var __mojor_best_v: Int32 = x[0]", fixed = TRUE)
  expect_match(code_max_i32, "var __mojor_best_v: Int32 = x[0]", fixed = TRUE)
  expect_false(grepl("var has_value = False", code_min_i32, fixed = TRUE))
  expect_false(grepl("var has_value = False", code_max_i32, fixed = TRUE))

  expect_match(code_min_f32, "# SIMD arg-reduction for which.min(x) (tie-stable: first index)", fixed = TRUE)
  expect_match(code_max_f32, "# SIMD arg-reduction for which.max(x) (tie-stable: first index)", fixed = TRUE)
  expect_match(code_min_f32, "comptime simd_width = simd_width_of[DType.float32]()", fixed = TRUE)
  expect_match(code_max_f32, "comptime simd_width = simd_width_of[DType.float32]()", fixed = TRUE)
  expect_match(code_min_f32, "var __mojor_best_v: Float32 = x[0]", fixed = TRUE)
  expect_match(code_max_f32, "var __mojor_best_v: Float32 = x[0]", fixed = TRUE)
  expect_false(grepl("var has_value = False", code_min_f32, fixed = TRUE))
  expect_false(grepl("var has_value = False", code_max_f32, fixed = TRUE))
})

test_that("which arg-reductions emit explicit tie-break branches for tree/simd schedules", {  f_min <- function(x) which.min(x)
  f_max <- function(x) which.max(x)

  code_tree_min <- paste(mojor_transpile(f_min, x = "f64[]", reduction = "tree")$mojo, collapse = "\n")
  code_tree_max <- paste(mojor_transpile(f_max, x = "f64[]", reduction = "tree")$mojo, collapse = "\n")
  code_simd_min <- paste(mojor_transpile(f_min, x = "f64[]", reduction = "simd")$mojo, collapse = "\n")
  code_simd_max <- paste(mojor_transpile(f_max, x = "f64[]", reduction = "simd")$mojo, collapse = "\n")

  expect_match(code_tree_min, "elif __mojor_rv > __mojor_lv:", fixed = TRUE)
  expect_match(code_tree_max, "elif __mojor_rv < __mojor_lv:", fixed = TRUE)
  expect_match(code_tree_min, "temp_idx[__mojor_tr_j] = min(__mojor_li, __mojor_ri)", fixed = TRUE)
  expect_match(code_tree_max, "temp_idx[__mojor_tr_j] = min(__mojor_li, __mojor_ri)", fixed = TRUE)

  expect_match(code_simd_min, "elif __mojor_lane_v == __mojor_chunk_best_v and __mojor_sr_lane < __mojor_chunk_best_lane:", fixed = TRUE)
  expect_match(code_simd_max, "elif __mojor_lane_v == __mojor_chunk_best_v and __mojor_sr_lane < __mojor_chunk_best_lane:", fixed = TRUE)
  expect_match(code_simd_min, "elif __mojor_chunk_best_v == __mojor_best_v and __mojor_chunk_best_idx < __mojor_best_idx:", fixed = TRUE)
  expect_match(code_simd_max, "elif __mojor_chunk_best_v == __mojor_best_v and __mojor_chunk_best_idx < __mojor_best_idx:", fixed = TRUE)
  expect_match(code_simd_min, "elif __mojor_rem_v == __mojor_best_v and __mojor_rem_idx < __mojor_best_idx:", fixed = TRUE)
  expect_match(code_simd_max, "elif __mojor_rem_v == __mojor_best_v and __mojor_rem_idx < __mojor_best_idx:", fixed = TRUE)
})

test_that("SIMD i32 arg-reductions keep explicit tie-break branches", {  f_min <- function(x) which.min(x)
  f_max <- function(x) which.max(x)

  code_min_i32 <- paste(mojor_transpile(f_min, x = "i32[]", reduction = "simd")$mojo, collapse = "\n")
  code_max_i32 <- paste(mojor_transpile(f_max, x = "i32[]", reduction = "simd")$mojo, collapse = "\n")

  expect_match(code_min_i32, "# SIMD arg-reduction for which.min(x) (tie-stable: first index)", fixed = TRUE)
  expect_match(code_max_i32, "# SIMD arg-reduction for which.max(x) (tie-stable: first index)", fixed = TRUE)
  expect_match(code_min_i32, "comptime simd_width = simd_width_of[DType.int32]()", fixed = TRUE)
  expect_match(code_max_i32, "comptime simd_width = simd_width_of[DType.int32]()", fixed = TRUE)
  expect_match(code_min_i32, "var __mojor_best_v: Int32 = x[0]", fixed = TRUE)
  expect_match(code_max_i32, "var __mojor_best_v: Int32 = x[0]", fixed = TRUE)
  expect_match(code_min_i32, "elif __mojor_lane_v == __mojor_chunk_best_v and __mojor_sr_lane < __mojor_chunk_best_lane:", fixed = TRUE)
  expect_match(code_max_i32, "elif __mojor_lane_v == __mojor_chunk_best_v and __mojor_sr_lane < __mojor_chunk_best_lane:", fixed = TRUE)
  expect_match(code_min_i32, "elif __mojor_chunk_best_v == __mojor_best_v and __mojor_chunk_best_idx < __mojor_best_idx:", fixed = TRUE)
  expect_match(code_max_i32, "elif __mojor_chunk_best_v == __mojor_best_v and __mojor_chunk_best_idx < __mojor_best_idx:", fixed = TRUE)
  expect_match(code_min_i32, "elif __mojor_rem_v == __mojor_best_v and __mojor_rem_idx < __mojor_best_idx:", fixed = TRUE)
  expect_match(code_max_i32, "elif __mojor_rem_v == __mojor_best_v and __mojor_rem_idx < __mojor_best_idx:", fixed = TRUE)
})

test_that("lgl[] tree reductions use Int32 temporaries and tie-stable casts", {  f_sum <- function(x) sum(x)
  f_prod <- function(x) prod(x)
  f_wmin <- function(x) which.min(x)
  f_wmax <- function(x) which.max(x)

  trans_sum <- mojor_transpile(f_sum, x = "lgl[]", reduction = "tree")
  trans_prod <- mojor_transpile(f_prod, x = "lgl[]", reduction = "tree")
  trans_wmin <- mojor_transpile(f_wmin, x = "lgl[]", reduction = "tree")
  trans_wmax <- mojor_transpile(f_wmax, x = "lgl[]", reduction = "tree")
  code_sum <- paste(trans_sum$mojo, collapse = "\n")
  code_prod <- paste(trans_prod$mojo, collapse = "\n")
  code_wmin <- paste(trans_wmin$mojo, collapse = "\n")
  code_wmax <- paste(trans_wmax$mojo, collapse = "\n")

  expect_match(code_sum, "# Tree reduction for sum(x)", fixed = TRUE)
  expect_match(code_sum, "var temp = alloc[Int32](n_i)", fixed = TRUE)
  expect_match(code_sum, "acc = Int32(0)", fixed = TRUE)
  expect_identical(trans_sum$out_type, "i32")

  expect_match(code_prod, "# Tree reduction for product(x)", fixed = TRUE)
  expect_match(code_prod, "var temp = alloc[Int32](n_i)", fixed = TRUE)
  expect_match(code_prod, "acc = Int32(1)", fixed = TRUE)
  expect_identical(trans_prod$out_type, "i32")

  expect_match(code_wmin, "# Tree arg-reduction for which.min(x) (tie-stable: first index)", fixed = TRUE)
  expect_match(code_wmin, "var temp_val = alloc[Int32](n_i)", fixed = TRUE)
  expect_match(code_wmin, "Int32(x[__mojor_left_i])", fixed = TRUE)
  expect_match(code_wmin, "temp_idx[__mojor_tr_j] = min(__mojor_li, __mojor_ri)", fixed = TRUE)

  expect_match(code_wmax, "# Tree arg-reduction for which.max(x) (tie-stable: first index)", fixed = TRUE)
  expect_match(code_wmax, "var temp_val = alloc[Int32](n_i)", fixed = TRUE)
  expect_match(code_wmax, "Int32(x[__mojor_left_i])", fixed = TRUE)
  expect_match(code_wmax, "temp_idx[__mojor_tr_j] = min(__mojor_li, __mojor_ri)", fixed = TRUE)
})

test_that("lgl[] simd request falls back to non-simd scalar reduction path", {  f_sum <- function(x) sum(x)
  code_sum <- paste(mojor_transpile(f_sum, x = "lgl[]", reduction = "simd")$mojo, collapse = "\n")
  expect_false(grepl("# SIMD reduction for sum\\(x\\)", code_sum))
  expect_match(code_sum, "for __mojor_ri in range\\(1, ", perl = TRUE)
})

test_that("nested loop reduction schedule emits SIMD/tree reductions for vector outputs", {  f <- function(x, g) {
    out <- numeric(g)
    for (j in seq_len(g)) {
      acc <- 0
      for (i in seq_along(x)) acc <- acc + x[i]
      out[j] <- acc
    }
    out
  }

  code_simd <- paste(
    mojor_transpile(
      f,
      x = "f64[]",
      g = "i32",
      reduction = "simd",
      na_mode = "unsafe",
      bounds_check = FALSE
    )$mojo,
    collapse = "\n"
  )
  expect_match(code_simd, "for j in range\\(1, Int\\(g\\) \\+ 1\\):", perl = TRUE)
  expect_match(code_simd, "# SIMD reduction for sum\\(x\\)", perl = TRUE)
  expect_match(code_simd, "var __mojor_red_n = Int\\(n_[A-Za-z0-9_]+_i\\)", perl = TRUE)
  expect_false(grepl("for i in range\\(1, n_i \\+ 1\\):", code_simd))

  code_tree <- paste(
    mojor_transpile(
      f,
      x = "f64[]",
      g = "i32",
      reduction = "tree",
      na_mode = "unsafe",
      bounds_check = FALSE
    )$mojo,
    collapse = "\n"
  )
  expect_match(code_tree, "# Tree reduction for sum\\(x\\)", perl = TRUE)
  expect_match(code_tree, "var __mojor_red_n = Int\\(n_[A-Za-z0-9_]+_i\\)", perl = TRUE)
  expect_false(grepl("for i in range\\(1, n_i \\+ 1\\):", code_tree))
})

test_that("nested SIMD sum reduction uses int32 lanes for i32 vector outputs", {  f <- function(x, g) {
    out <- integer(g)
    for (j in seq_len(g)) {
      acc <- 0L
      for (i in seq_along(x)) acc <- acc + x[i]
      out[j] <- acc
    }
    out
  }

  code <- paste(
    mojor_transpile(
      f,
      x = "i32[]",
      g = "i32",
      reduction = "simd",
      na_mode = "unsafe",
      bounds_check = FALSE
    )$mojo,
    collapse = "\n"
  )

  expect_match(code, "# SIMD reduction for sum\\(x\\)", perl = TRUE)
  expect_match(code, "comptime simd_width = simd_width_of\\[DType.int32\\]\\(\\)", perl = TRUE)
  expect_match(code, "SIMD\\[DType.int32, simd_width\\]\\(Int32\\(0\\)\\)", perl = TRUE)
  expect_false(grepl("for i in range\\(1, n_i \\+ 1\\):", code))
})

test_that("nested loop reduction schedule emits SIMD/tree product reductions for vector outputs", {  f <- function(x, g) {
    out <- numeric(g)
    for (j in seq_len(g)) {
      acc <- 1
      for (i in seq_along(x)) acc <- acc * x[i]
      out[j] <- acc
    }
    out
  }

  code_simd <- paste(
    mojor_transpile(
      f,
      x = "f64[]",
      g = "i32",
      reduction = "simd",
      na_mode = "unsafe",
      bounds_check = FALSE
    )$mojo,
    collapse = "\n"
  )
  expect_match(code_simd, "for j in range\\(1, Int\\(g\\) \\+ 1\\):", perl = TRUE)
  expect_match(code_simd, "# SIMD reduction for product\\(x\\)", perl = TRUE)
  expect_match(code_simd, "var __mojor_red_n = Int\\(n_[A-Za-z0-9_]+_i\\)", perl = TRUE)
  expect_false(grepl("for i in range\\(1, n_i \\+ 1\\):", code_simd))

  code_tree <- paste(
    mojor_transpile(
      f,
      x = "f64[]",
      g = "i32",
      reduction = "tree",
      na_mode = "unsafe",
      bounds_check = FALSE
    )$mojo,
    collapse = "\n"
  )
  expect_match(code_tree, "# Tree reduction for product\\(x\\)", perl = TRUE)
  expect_match(code_tree, "var __mojor_red_n = Int\\(n_[A-Za-z0-9_]+_i\\)", perl = TRUE)
  expect_false(grepl("for i in range\\(1, n_i \\+ 1\\):", code_tree))
})
