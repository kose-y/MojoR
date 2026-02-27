library(testthat)

# Golden snapshots are canonicalized at default optimization level.
mojor_options(opt_level = 2L)

golden_path <- function(name) {
  .mojor_test_golden_path(name)
}

read_golden <- function(name) {
  .mojor_test_read_golden(name)
}

expect_golden <- function(actual, name) {
  expect_mojor_golden(actual, name)
}

format_r_dump <- function(x) {
  paste(capture.output(dput(x)), collapse = "\n")
}

extract_simd_chunk <- function(mojo) {
  lines <- strsplit(mojo, "\n", fixed = TRUE)[[1]]
  start <- grep("var __mojor_simd_end", lines, fixed = TRUE)
  end <- grep("for i in range(__mojor_simd_end + 1", lines, fixed = TRUE)
  if (length(start) > 0 && length(end) > 0) {
    start <- start[[1]]
    end <- end[[1]]
    if (end >= start) {
      return(paste(lines[start:end], collapse = "\n"))
    }
  }
  # Current merged-lane codegen may emit unroll chunks instead of explicit SIMD
  # chunk loops; treat that as the canonical optimized vector-add chunk.
  start <- grep("var _mojor_unroll_span", lines, fixed = TRUE)
  end <- grep("for i in range(_mojor_unroll_tail_start, ", lines, fixed = TRUE)
  if (length(start) == 0 || length(end) == 0) return(NULL)
  start <- start[[1]]
  end <- end[[1]]
  if (end < start) return(NULL)
  paste(lines[start:end], collapse = "\n")
}

extract_guard_hoist_chunk <- function(mojo) {
  lines <- strsplit(mojo, "\n", fixed = TRUE)[[1]]
  start <- grep("^\\s*if \\((Int\\(1\\)|1) <= (Int\\(n\\)|n)\\):\\s*$", lines, perl = TRUE)
  if (length(start) == 0) {
    start <- grep("^\\s*if Int\\(1\\) <= Int\\(n\\):\\s*$", lines, perl = TRUE)
  }
  end <- grep("^\\s*for (_mojor_i|i) in range\\(", lines, perl = TRUE)
  if (length(start) == 0 || length(end) == 0) return(NULL)
  start <- start[[1]]
  end <- end[end >= start][[1]]
  if (end < start) return(NULL)
  paste(lines[start:end], collapse = "\n")
}

extract_reduction_chunk <- function(mojo, marker) {
  lines <- strsplit(mojo, "\n", fixed = TRUE)[[1]]
  start <- grep(marker, lines, fixed = TRUE)
  end <- grep("^\\s*[A-Za-z_][A-Za-z0-9_]*_out\\[0\\] = [A-Za-z_][A-Za-z0-9_]*\\s*$", lines)
  if (length(start) == 0 || length(end) == 0) return(NULL)
  start <- start[[1]]
  end <- end[[length(end)]]
  if (end < start) return(NULL)
  paste(lines[start:end], collapse = "\n")
}

extract_zero_based_chunk <- function(mojo) {
  lines <- strsplit(mojo, "\n", fixed = TRUE)[[1]]
  start <- grep("^\\s*for i in range\\(", lines)
  if (length(start) == 0) return(NULL)
  start <- start[[1]]
  if (start + 1 > length(lines)) return(NULL)
  paste(lines[start:(start + 1)], collapse = "\n")
}

extract_row_major_chunk <- function(mojo) {
  lines <- strsplit(mojo, "\n", fixed = TRUE)[[1]]
  keep <- c(
    grep("_MOJOR_LAYOUT_mat = Layout.row_major", lines, fixed = TRUE),
    grep("_MOJOR_MATRIX_LAYOUT = Layout.row_major", lines, fixed = TRUE),
    grep("var mat_layout = RuntimeLayout[_MOJOR_MATRIX_LAYOUT].row_major", lines, fixed = TRUE),
    grep("mat[((i - 1)) * ncol_out_i + __mojor_i2] =", lines, fixed = TRUE),
    grep("mat_tensor[(i - 1), __mojor_i2] =", lines, fixed = TRUE)
  )
  keep <- sort(unique(keep))
  if (length(keep) == 0) return(NULL)
  paste(lines[keep], collapse = "\n")
}

extract_tile_single_loop_chunk <- function(mojo) {
  lines <- strsplit(mojo, "\n", fixed = TRUE)[[1]]
  start <- grep("^\\s*for _mojor_tile_i in range\\(", lines)
  inner <- grep("^\\s*for i in range\\(_mojor_tile_i, ", lines)
  if (length(start) == 0 || length(inner) == 0) return(NULL)
  start <- start[[1]]
  inner <- inner[[1]]
  if (inner < start) return(NULL)
  paste(lines[start:inner], collapse = "\n")
}

extract_tile_nested_loop_chunk <- function(mojo) {
  lines <- strsplit(mojo, "\n", fixed = TRUE)[[1]]
  tile_i <- grep("^\\s*for _mojor_tile_i in range\\(", lines)
  inner_i <- grep("^\\s*for i in range\\(_mojor_tile_i, ", lines)
  tile_j <- grep("^\\s*for _mojor_tile_j in range\\(", lines)
  inner_j <- grep("^\\s*for j in range\\(_mojor_tile_j, ", lines)
  if (length(tile_i) == 0 || length(inner_i) == 0 || length(tile_j) == 0 || length(inner_j) == 0) return(NULL)
  idx <- c(tile_i[[1]], inner_i[[1]], tile_j[[1]], inner_j[[1]])
  paste(lines[idx], collapse = "\n")
}

extract_nested_reduction_chunk <- function(mojo, marker) {
  lines <- strsplit(mojo, "\n", fixed = TRUE)[[1]]
  start <- grep(marker, lines, fixed = TRUE)
  end <- grep("out[Int((j - 1))]", lines, fixed = TRUE)
  if (length(start) == 0 || length(end) == 0) return(NULL)
  start <- start[[1]]
  end <- end[end >= start][[1]]
  if (end < start) return(NULL)
  paste(lines[start:end], collapse = "\n")
}

.mojor_golden_reduction_fn <- function(op) {
  switch(
    op,
    min = function(x) min(x),
    max = function(x) max(x),
    sum = function(x) sum(x),
    prod = function(x) prod(x),
    `which.max` = function(x) which.max(x),
    `which.min` = function(x) which.min(x),
    stop("unsupported reduction op for golden helper: ", op)
  )
}

.mojor_golden_nested_reduce_fn <- function(op, dtype = c("f64", "i32")) {
  dtype <- match.arg(dtype)
  if (identical(op, "sum") && identical(dtype, "i32")) {
    return(function(x, g) {
      out <- integer(g)
      for (j in seq_len(g)) {
        acc <- 0L
        for (i in seq_along(x)) acc <- acc + x[i]
        out[j] <- acc
      }
      out
    })
  }
  if (identical(op, "sum")) {
    return(function(x, g) {
      out <- numeric(g)
      for (j in seq_len(g)) {
        acc <- 0
        for (i in seq_along(x)) acc <- acc + x[i]
        out[j] <- acc
      }
      out
    })
  }
  if (identical(op, "prod")) {
    return(function(x, g) {
      out <- numeric(g)
      for (j in seq_len(g)) {
        acc <- 1
        for (i in seq_along(x)) acc <- acc * x[i]
        out[j] <- acc
      }
      out
    })
  }
  stop("unsupported nested reduction op for golden helper: ", op)
}

.mojor_expect_reduction_golden_chunk <- function(op, reduction, x_type, marker, golden_name) {
  mojo <- mojor_transpile(
    .mojor_golden_reduction_fn(op),
    x = x_type,
    reduction = reduction,
    na_mode = "forbid",
    ir_only = TRUE
  )$mojo
  chunk <- extract_reduction_chunk(mojo, marker)
  expect_true(is.character(chunk) && nzchar(chunk))
  expect_golden(chunk, golden_name)
}

.mojor_expect_nested_reduction_golden_chunk <- function(op, reduction, x_type, marker, golden_name) {
  nested_dtype <- if (identical(x_type, "i32[]")) "i32" else "f64"
  mojo <- mojor_transpile(
    .mojor_golden_nested_reduce_fn(op, nested_dtype),
    x = x_type,
    g = "i32",
    reduction = reduction,
    na_mode = "unsafe",
    bounds_check = FALSE,
    ir_only = TRUE
  )$mojo
  chunk <- extract_nested_reduction_chunk(mojo, marker)
  expect_true(is.character(chunk) && nzchar(chunk))
  expect_golden(chunk, golden_name)
}

test_that("golden file: IR format simple loop", {  code <- quote({ for (i in 1:n) out[i] <- x[i] + 1 })
  ir <- .mojor_ir_build_stmt(code[[2]])
  .mojor_ir_verify(ir)
  formatted <- paste(.mojor_ir_format(ir), collapse = "\n")
  expect_golden(formatted, "simple_loop.ir")
})

test_that("golden file: SIMD vector add chunk", {  f <- function(x, y, n) {
    out <- numeric(n)
    for (i in seq_len(n)) out[i] <- x[i] + y[i]
    out
  }
  mojo <- mojor_transpile(
    f,
    x = "f64[]",
    y = "f64[]",
    n = "i32",
    assume_aligned = 32L,
    simd_mode = "explicit",
    na_mode = "unsafe",
    bounds_check = FALSE,
    ir_only = TRUE
  )$mojo
  chunk <- extract_simd_chunk(mojo)
  expect_true(is.character(chunk) && nzchar(chunk))
  expect_golden(chunk, "simd_vector_add.mojo")
})

test_that("golden file: guard hoist preheader chunk", {  f <- function(x, j, n) {
    out <- numeric(n)
    for (i in seq_len(n)) out[i] <- x[j] + 1
    out
  }
  mojo <- mojor_transpile(
    f,
    x = "f64[]",
    j = "i32",
    n = "i32",
    bounds_check = TRUE,
    na_mode = "unsafe",
    ir_only = TRUE
  )$mojo
  chunk <- extract_guard_hoist_chunk(mojo)
  expect_true(is.character(chunk) && nzchar(chunk))
  expect_golden(chunk, "guard_hoist_invariant.mojo")
})

for (case in list(
  list(name = "tree reduction min", op = "min", reduction = "tree", x_type = "f64[]", marker = "# Tree reduction for min(x)", golden = "tree_reduction_min.mojo"),
  list(name = "simd reduction min", op = "min", reduction = "simd", x_type = "f64[]", marker = "# SIMD reduction for min(x)", golden = "simd_reduction_min.mojo"),
  list(name = "tree reduction sum", op = "sum", reduction = "tree", x_type = "f64[]", marker = "# Tree reduction for sum(x)", golden = "tree_reduction_sum.mojo"),
  list(name = "simd reduction sum", op = "sum", reduction = "simd", x_type = "f64[]", marker = "# SIMD reduction for sum(x)", golden = "simd_reduction_sum.mojo"),
  list(name = "simd reduction i32 sum", op = "sum", reduction = "simd", x_type = "i32[]", marker = "# SIMD reduction for sum(x)", golden = "simd_reduction_sum_i32.mojo"),
  list(name = "simd reduction i32 product", op = "prod", reduction = "simd", x_type = "i32[]", marker = "# SIMD reduction for product(x)", golden = "simd_reduction_product_i32.mojo"),
  list(name = "tree reduction which.max", op = "which.max", reduction = "tree", x_type = "f64[]", marker = "# Tree arg-reduction for which.max(x) (tie-stable: first index)", golden = "tree_reduction_which_max.mojo"),
  list(name = "simd reduction which.max", op = "which.max", reduction = "simd", x_type = "f64[]", marker = "# SIMD arg-reduction for which.max(x) (tie-stable: first index)", golden = "simd_reduction_which_max.mojo"),
  list(name = "simd reduction i32 which.max", op = "which.max", reduction = "simd", x_type = "i32[]", marker = "# SIMD arg-reduction for which.max(x) (tie-stable: first index)", golden = "simd_reduction_which_max_i32.mojo"),
  list(name = "simd reduction which.min", op = "which.min", reduction = "simd", x_type = "f64[]", marker = "# SIMD arg-reduction for which.min(x) (tie-stable: first index)", golden = "simd_reduction_which_min.mojo"),
  list(name = "tree reduction which.min", op = "which.min", reduction = "tree", x_type = "f64[]", marker = "# Tree arg-reduction for which.min(x) (tie-stable: first index)", golden = "tree_reduction_which_min.mojo")
)) {
  local({
    cfg <- case
    test_that(paste0("golden file: ", cfg$name, " chunk"), {
      .mojor_expect_reduction_golden_chunk(
        op = cfg$op,
        reduction = cfg$reduction,
        x_type = cfg$x_type,
        marker = cfg$marker,
        golden_name = cfg$golden
      )
    })
  })
}

test_that("golden file: zero-based index emission chunk", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in 1:n) out[i] <- x[i]
    out
  }
  mojo <- mojor_transpile(
    f,
    x = "f64[]",
    n = "i32",
    index_base = "zero_based",
    na_mode = "unsafe",
    bounds_check = FALSE,
    ir_only = TRUE
  )$mojo
  chunk <- extract_zero_based_chunk(mojo)
  expect_true(is.character(chunk) && nzchar(chunk))
  expect_golden(chunk, "index_base_zero_based.mojo")
})

test_that("golden file: row-major layout emission chunk", {  f <- function(x, y, n) {
    mat <- matrix(0, n, 2)
    for (i in 1:n) mat[i, ] <- c(x[i], y[i])
    mat
  }
  mojo <- mojor_transpile(
    f,
    x = "f64[]",
    y = "f64[]",
    n = "i32",
    array_layout = "row_major",
    na_mode = "unsafe",
    bounds_check = FALSE,
    ir_only = TRUE
  )$mojo
  chunk <- extract_row_major_chunk(mojo)
  expect_true(is.character(chunk) && nzchar(chunk))
  expect_golden(chunk, "layout_row_major.mojo")
})

test_that("golden file: single-loop tiling chunk", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2
    out
  }
  mojo <- mojor_transpile(
    f,
    x = "f64[]",
    tile = 16,
    na_mode = "unsafe",
    bounds_check = FALSE
  )$mojo
  chunk <- extract_tile_single_loop_chunk(mojo)
  expect_true(is.character(chunk) && nzchar(chunk))
  expect_golden(chunk, "tile_single_loop.mojo")
})

test_that("golden file: nested-loop tiling headers", {  f <- function(a, n, m) {
    out <- matrix(0, n, m)
    for (i in seq_len(n)) {
      for (j in seq_len(m)) out[i, j] <- a[i, j] + i - j
    }
    out
  }
  mojo <- mojor_transpile(
    f,
    a = "f64[]",
    n = "i32",
    m = "i32",
    tile = c(16, 64),
    na_mode = "unsafe",
    bounds_check = FALSE,
    ir_only = TRUE
  )$mojo
  chunk <- extract_tile_nested_loop_chunk(mojo)
  expect_true(is.character(chunk) && nzchar(chunk))
  expect_golden(chunk, "tile_nested_loop.mojo")
})

for (case in list(
  list(name = "nested tree reduction sum", op = "sum", reduction = "tree", x_type = "f64[]", marker = "# Tree reduction for sum(x)", golden = "nested_tree_reduction_sum_vector.mojo"),
  list(name = "nested simd reduction sum", op = "sum", reduction = "simd", x_type = "f64[]", marker = "# SIMD reduction for sum(x)", golden = "nested_simd_reduction_sum_vector.mojo"),
  list(name = "nested simd reduction i32 sum", op = "sum", reduction = "simd", x_type = "i32[]", marker = "# SIMD reduction for sum(x)", golden = "nested_simd_reduction_sum_i32_vector.mojo"),
  list(name = "nested tree reduction product", op = "prod", reduction = "tree", x_type = "f64[]", marker = "# Tree reduction for product(x)", golden = "nested_tree_reduction_product_vector.mojo"),
  list(name = "nested simd reduction product", op = "prod", reduction = "simd", x_type = "f64[]", marker = "# SIMD reduction for product(x)", golden = "nested_simd_reduction_product_vector.mojo")
)) {
  local({
    cfg <- case
    test_that(paste0("golden file: ", cfg$name, " chunk"), {
      .mojor_expect_nested_reduction_golden_chunk(
        op = cfg$op,
        reduction = cfg$reduction,
        x_type = cfg$x_type,
        marker = cfg$marker,
        golden_name = cfg$golden
      )
    })
  })
}

test_that("golden file: SSA backend lowered CFG format", {  ir <- .mojor_ir_build_stmt(quote(if (x > 0) { y <- 1 } else { y <- 2 }))
  prep <- .mojor_ir_prepare_ssa_backend(
    ir,
    layout_ctx = list(n_var = "n_i", type_env = list(x = "f64")),
    opt_level = 0,
    backend_lower = TRUE
  )
  formatted <- .mojor_ir_ssa_backend_format(prep$backend_lowered)
  expect_golden(formatted, "ssa_backend_cfg_if_else.backend")
})

test_that("golden file: SSA backend unroll artifacts", {  f <- function(x, n) {
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
    opt_level = 0L,
    name = "t_golden_ssa_backend_unroll"
  )

  expect_true(is.list(trans$ssa_backend))
  expect_null(trans$ssa_backend_error)

  expect_golden(
    format_r_dump(trans$ssa_backend$ir$scheduled),
    "ssa_backend_unroll_scheduled.ir"
  )
  expect_golden(
    format_r_dump(trans$ssa_backend$ssa$optimized),
    "ssa_backend_unroll_optimized.ssa"
  )
  expect_golden(
    format_r_dump(trans$ssa_backend$backend_cfg),
    "ssa_backend_unroll_cfg.cfg"
  )
})

test_that("golden file: scheduled tree reduction node", {  ir <- .mojor_ir_scalar_reduce(
    op = "sum",
    acc = "acc",
    arg = "x",
    associative = TRUE,
    commutative = TRUE
  )

  prep <- .mojor_ir_prepare_ssa_backend(
    ir,
    layout_ctx = list(n_var = "n_i", type_env = list(x = "f64[]")),
    schedule = list(reduction = "tree", n_var = "n_i", type_env = list(x = "f64[]")),
    opt_level = 0L
  )

  expect_golden(format_r_dump(prep$ir$scheduled), "scheduled_reduce_tree_sum.ir")
})

test_that("golden file: scheduled simd arg-reduction node", {  ir <- .mojor_ir_scalar_reduce(
    op = "which.max",
    acc = "idx",
    arg = "x",
    associative = TRUE,
    commutative = TRUE
  )

  prep <- .mojor_ir_prepare_ssa_backend(
    ir,
    layout_ctx = list(n_var = "n_i", type_env = list(x = "f64[]")),
    schedule = list(reduction = "simd", n_var = "n_i", type_env = list(x = "f64[]")),
    opt_level = 0L
  )

  expect_golden(format_r_dump(prep$ir$scheduled), "scheduled_reduce_simd_which_max.ir")
})

test_that("golden file: selected backend straight-line opcodes", {  ir <- .mojor_ir_return(
    .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_const(1))
  )
  prep <- .mojor_ir_prepare_ssa_backend(
    ir,
    layout_ctx = list(n_var = "n_i", type_env = list(x = "f64")),
    opt_level = 0,
    backend_select = TRUE
  )
  formatted <- .mojor_ir_ssa_backend_selected_format(prep$backend_selected)
  expect_golden(formatted, "ssa_backend_selected_straight.backend")
})

test_that("golden file: selected backend if/else cfg", {  ir <- .mojor_ir_build_stmt(quote(if (x > 0) { y <- 1 } else { y <- 2 }))
  prep <- .mojor_ir_prepare_ssa_backend(
    ir,
    layout_ctx = list(n_var = "n_i", type_env = list(x = "f64")),
    opt_level = 0,
    backend_select = TRUE
  )
  formatted <- .mojor_ir_ssa_backend_selected_format(prep$backend_selected)
  expect_golden(formatted, "ssa_backend_selected_if_else.backend")
})

test_that("golden file: selected backend loop cfg", {  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) out[i] <- x[i] + 1
    out
  }
  prep <- .mojor_ir_prepare_ssa_backend(
    f,
    layout_ctx = list(n_var = "n_i", type_env = list(x = "f64[]", n = "i32")),
    opt_level = 0,
    backend_select = TRUE
  )
  formatted <- .mojor_ir_ssa_backend_selected_format(prep$backend_selected)
  expect_golden(formatted, "ssa_backend_selected_loop.backend")
})

test_that("golden file: selected backend call+cast cfg", {  ir <- .mojor_ir_return(
    .mojor_ir_cast("Int32", .mojor_ir_call("sin", list(.mojor_ir_var("x"))))
  )
  prep <- .mojor_ir_prepare_ssa_backend(
    ir,
    layout_ctx = list(n_var = "n_i", type_env = list(x = "f64")),
    opt_level = 0,
    backend_select = TRUE
  )
  formatted <- .mojor_ir_ssa_backend_selected_format(prep$backend_selected)
  expect_golden(formatted, "ssa_backend_selected_call_cast.backend")
})

test_that("golden file: selected backend subscript-missing store cfg", {  ir <- .mojor_ir_assign(
    .mojor_ir_subscript(
      "out",
      list(
        .mojor_ir_scalar_index(.mojor_ir_var("i")),
        .mojor_ir_missing_index()
      )
    ),
    .mojor_ir_var("rhs")
  )
  prep <- .mojor_ir_prepare_ssa_backend(
    ir,
    layout_ctx = list(n_var = "n_i", type_env = list(out = "f64[]", i = "i32", rhs = "f64")),
    opt_level = 0,
    backend_select = TRUE
  )
  formatted <- .mojor_ir_ssa_backend_selected_format(prep$backend_selected)
  expect_golden(formatted, "ssa_backend_selected_subscript_missing.backend")
})

test_that("golden file: selected backend mask-store cfg", {  f <- function(x) {
    out <- numeric(length(x))
    mask <- x > 0
    out[mask] <- x[mask] + 1
    out
  }
  prep <- .mojor_ir_prepare_ssa_backend(
    f,
    layout_ctx = list(n_var = "n_i", type_env = list(x = "f64[]")),
    opt_level = 0,
    backend_select = TRUE
  )
  formatted <- .mojor_ir_ssa_backend_selected_format(prep$backend_selected)
  expect_golden(formatted, "ssa_backend_selected_mask_store.backend")
})

test_that("golden file: selected backend tree sum reduction cfg", {  ir <- .mojor_ir_scalar_reduce(
    op = "sum",
    acc = "acc",
    arg = "x",
    associative = TRUE,
    commutative = TRUE
  )
  prep <- .mojor_ir_prepare_ssa_backend(
    ir,
    layout_ctx = list(n_var = "n_i", type_env = list(x = "f64[]")),
    schedule = list(reduction = "tree", n_var = "n_i", type_env = list(x = "f64[]")),
    opt_level = 0,
    backend_select = TRUE
  )
  formatted <- .mojor_ir_ssa_backend_selected_format(prep$backend_selected)
  expect_golden(formatted, "ssa_backend_selected_reduce_tree_sum.backend")
})

test_that("golden file: selected backend simd sum reduction cfg", {  ir <- .mojor_ir_scalar_reduce(
    op = "sum",
    acc = "acc",
    arg = "x",
    associative = TRUE,
    commutative = TRUE
  )
  prep <- .mojor_ir_prepare_ssa_backend(
    ir,
    layout_ctx = list(n_var = "n_i", type_env = list(x = "f64[]")),
    schedule = list(reduction = "simd", n_var = "n_i", type_env = list(x = "f64[]")),
    opt_level = 0,
    backend_select = TRUE
  )
  formatted <- .mojor_ir_ssa_backend_selected_format(prep$backend_selected)
  expect_golden(formatted, "ssa_backend_selected_reduce_simd_sum.backend")
})

test_that("golden file: selected backend simd which.max arg-reduction cfg", {  ir <- .mojor_ir_scalar_reduce(
    op = "which.max",
    acc = "idx",
    arg = "x",
    associative = TRUE,
    commutative = TRUE
  )
  prep <- .mojor_ir_prepare_ssa_backend(
    ir,
    layout_ctx = list(n_var = "n_i", type_env = list(x = "f64[]")),
    schedule = list(reduction = "simd", n_var = "n_i", type_env = list(x = "f64[]")),
    opt_level = 0,
    backend_select = TRUE
  )
  formatted <- .mojor_ir_ssa_backend_selected_format(prep$backend_selected)
  expect_golden(formatted, "ssa_backend_selected_reduce_simd_which_max.backend")
})

test_that("golden file: codegen backend tree sum reduction cfg", {  ir <- .mojor_ir_scalar_reduce(
    op = "sum",
    acc = "acc",
    arg = "x",
    associative = TRUE,
    commutative = TRUE
  )
  prep <- .mojor_ir_prepare_ssa_backend(
    ir,
    layout_ctx = list(n_var = "n_i", type_env = list(x = "f64[]")),
    schedule = list(reduction = "tree", n_var = "n_i", type_env = list(x = "f64[]")),
    opt_level = 0,
    backend_codegen = TRUE,
    backend_target = "mojo_cpu"
  )
  formatted <- .mojor_ir_ssa_backend_codegen_format(prep$backend_codegen)
  expect_golden(formatted, "ssa_backend_codegen_reduce_tree_sum.backend")
})

test_that("golden file: codegen backend simd which.max arg-reduction cfg", {  ir <- .mojor_ir_scalar_reduce(
    op = "which.max",
    acc = "idx",
    arg = "x",
    associative = TRUE,
    commutative = TRUE
  )
  prep <- .mojor_ir_prepare_ssa_backend(
    ir,
    layout_ctx = list(n_var = "n_i", type_env = list(x = "f64[]")),
    schedule = list(reduction = "simd", n_var = "n_i", type_env = list(x = "f64[]")),
    opt_level = 0,
    backend_codegen = TRUE,
    backend_target = "mojo_cpu"
  )
  formatted <- .mojor_ir_ssa_backend_codegen_format(prep$backend_codegen)
  expect_golden(formatted, "ssa_backend_codegen_reduce_simd_which_max.backend")
})

test_that("golden file: backend prep strict-schema unknown-op diagnostic", {  ir <- .mojor_ir_return(
    .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_const(1))
  )

  orig_to_ssa <- .mojor_ir_to_ssa
  on.exit({
    .mojor_ir_to_ssa <<- orig_to_ssa
  }, add = TRUE)

  .mojor_ir_to_ssa <<- function(node) {
    ssa <- orig_to_ssa(node)
    entry <- ssa$entry
    blk <- ssa$blocks[[entry]]
    extra <- list(kind = "ssa_stmt", id = "%v9999", op = "mystery:thing", args = character(0))
    if (is.null(blk$stmts)) {
      blk$stmts <- list(extra)
    } else {
      blk$stmts <- c(blk$stmts, list(extra))
    }
    ssa$blocks[[entry]] <- blk
    ssa
  }

  msg <- tryCatch(
    {
      .mojor_ir_prepare_ssa_backend(
        ir,
        layout_ctx = list(n_var = "n_i", type_env = list(x = "f64")),
        opt_level = 0,
        strict_schema = TRUE,
        verify = TRUE
      )
      "NO_ERROR"
    },
    error = function(e) conditionMessage(e)
  )

  expect_golden(msg, "ssa_backend_strict_schema_unknown.error")
})

test_that("golden file: SSA cfg lowering for break in for-loop", {  f <- function(x, n) {
    acc <- 0
    for (i in seq_len(n)) {
      if (x[i] < 0) break
      acc <- acc + x[i]
    }
    acc
  }

  prep <- .mojor_ir_prepare_ssa_backend(
    f,
    layout_ctx = list(n_var = "n_i", type_env = list(x = "f64[]", n = "i32")),
    opt_level = 0
  )
  formatted <- .mojor_ir_ssa_format(prep$ssa$raw)
  expect_golden(formatted, "ssa_cfg_break_for.ssa")
})

test_that("golden file: SSA cfg lowering for next in for-loop", {  f <- function(x, n) {
    acc <- 0
    for (i in seq_len(n)) {
      if (x[i] < 0) next
      acc <- acc + x[i]
    }
    acc
  }

  prep <- .mojor_ir_prepare_ssa_backend(
    f,
    layout_ctx = list(n_var = "n_i", type_env = list(x = "f64[]", n = "i32")),
    opt_level = 0
  )
  formatted <- .mojor_ir_ssa_format(prep$ssa$raw)
  expect_golden(formatted, "ssa_cfg_next_for.ssa")
})
