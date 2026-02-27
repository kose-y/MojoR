t # =============================================================================
# MojoR Intermediate Representation (IR)
# =============================================================================
# This file implements the IR layer between R AST and Mojo code generation.
# It provides node constructors, builders, emitters, and transformation passes.
#
# Organization:
# 1. IR Node Constructors (const, var, unop, binop, cast, call, index, etc.)
# 2. IR Builders (AST -> IR conversion)
# 3. IR Dump/Format Utilities
# 4. Slice Detection and Building
# 5. Expression IR (build and emit)
# 6. Statement IR Emitters (assign, loop, if, while, etc.)
# 7. Type Inference and Checking
# 8. Guard Passes (NA guards, bounds guards)
# =============================================================================

# =============================================================================
# Section 1: IR Node Constructors
# =============================================================================

.mojor_ir_const <- function(value, src = NULL) {
  list(kind = "const", value = value, src = src)
}

.mojor_ir_var <- function(name, src = NULL) {
  list(kind = "var", name = name, src = src)
}

.mojor_ir_unop <- function(op, expr, src = NULL) {
  list(kind = "unop", op = op, expr = expr, src = src)
}

.mojor_ir_binop <- function(op, lhs, rhs, src = NULL) {
  list(kind = "binop", op = op, lhs = lhs, rhs = rhs, src = src)
}

.mojor_ir_cast <- function(to, expr, src = NULL) {
  list(kind = "cast", to = to, expr = expr, src = src)
}

.mojor_ir_call <- function(fn, args, src = NULL) {
  list(kind = "call", fn = fn, args = args, src = src)
}

.mojor_ir_index <- function(base, indices, index_base = "one_based", src = NULL) {
  list(kind = "index", base = base, indices = indices, index_base = index_base, src = src)
}

# Step 6.1: ifelse expression node
.mojor_ir_ifelse <- function(cond, yes, no, src = NULL) {
  list(kind = "ifelse", cond = cond, yes = yes, no = no, src = src)
}

.mojor_ir_block <- function(stmts, src = NULL) {
  list(kind = "block", stmts = stmts, src = src)
}

.mojor_ir_assign <- function(lhs, rhs, src = NULL) {
  list(kind = "assign", lhs = lhs, rhs = rhs, src = src)
}

.mojor_ir_if <- function(cond, then, else_block = NULL, src = NULL) {
  list(kind = "if", cond = cond, then = then, else_block = else_block, src = src)
}

.mojor_ir_loop <- function(var, range, body, src = NULL) {
  list(kind = "loop", var = var, range = range, body = body, metadata = NULL, src = src)
}

.mojor_ir_while <- function(cond, body, src = NULL) {
  list(kind = "while", cond = cond, body = body, src = src)
}

.mojor_ir_break <- function(src = NULL) {
  list(kind = "break", src = src)
}

.mojor_ir_next <- function(src = NULL) {
  list(kind = "next", src = src)
}

# Step 8.7: Repeat loop support
.mojor_ir_repeat <- function(body, src = NULL) {
  list(kind = "repeat", body = body, src = src)
}

# Step 8.7: Return statement support
.mojor_ir_return <- function(value = NULL, src = NULL) {
  list(kind = "return", value = value, src = src)
}

# Step 19: Scalar reduction node <U+2014> sum/product/min/max/which.min/which.max over an array
# op: one of "sum", "product", "min", "max", "which.min", "which.max"
# acc: accumulator variable name (string) <U+2014> "acc" for min/max, "idx" for which.*
# arg: array variable name (string)
# init: IR expression node for initial value (NULL = infer from op+type at lowering)
# axis: reduction axis (0 for 1D, future: multi-dimensional)
# associative: whether op is associative (enables tree reduction)
# commutative: whether op is commutative (enables parallel reordering)
# src: optional source R expression
.mojor_ir_scalar_reduce <- function(op, acc, arg, init = NULL, axis = 0L, associative = TRUE, commutative = TRUE, na_rm = FALSE, src = NULL) {
 # Step 23: Extended reduction semantics
 # Step 7.1: Added na_rm parameter for NA-aware reductions
  list(
    kind = "scalar_reduce",
    op = op,
    acc = acc,
    arg = arg,
    init = init,
    axis = axis,
    associative = associative,
    commutative = commutative,
    na_rm = na_rm,
    src = src
  )
}

# Scheduled scalar reduction node (post-schedule, pre-emit)
# mode: "tree" | "simd"
# op: "sum" | "product" | "min" | "max" | "which.min" | "which.max"
# dtype: tree -> Float64/Float32/Int32, simd -> DType.float64/DType.float32
.mojor_ir_scheduled_reduce <- function(mode, op, acc, arg, n_var = "n_i", dtype = NULL, init_val = NULL, empty_val = NULL, nan_val = NULL, value_cast = NULL, src = NULL) {
  list(
    kind = "scheduled_reduce",
    mode = mode,
    op = op,
    acc = acc,
    arg = arg,
    n_var = n_var,
    dtype = dtype,
    init_val = init_val,
    empty_val = empty_val,
    nan_val = nan_val,
    value_cast = value_cast,
    src = src
  )
}

# GPU Array IR nodes (Step 24: GPU Acceleration)
# ------------------------------------------------------------------------------

# GPU Reduction node - parallel reduction on GPU
# op: one of "sum", "mean", "prod", "min", "max", "argmin", "argmax"
# arg: array variable name (string)
# dims: reduction dimensions (list of integers, NULL = all dims)
# keepdims: whether to keep reduced dimensions
# src: optional source R expression
.mojor_ir_gpu_reduce <- function(op, arg, dims = NULL, keepdims = FALSE, src = NULL) {
  list(
    kind = "gpu_reduce",
    op = op,
    arg = arg,
    dims = dims,
    keepdims = keepdims,
    src = src
  )
}

# GPU Matmul node - matrix multiplication on GPU
# a: left matrix variable name (string)
# b: right matrix variable name (string)
# transpose_a: whether to transpose the left matrix
# transpose_b: whether to transpose the right matrix
# src: optional source R expression
.mojor_ir_gpu_matmul <- function(a, b, transpose_a = FALSE, transpose_b = FALSE, src = NULL) {
  list(
    kind = "gpu_matmul",
    a = a,
    b = b,
    transpose_a = transpose_a,
    transpose_b = transpose_b,
    src = src
  )
}

# Step 8.5: Slice support IR nodes
.mojor_ir_slice_index <- function(start, end, src = NULL) {
 # Represents a slice index like 1:n or seq_len(m)
 # start: IR expression node (numeric literal or expression)
 # end: IR expression node (can be variable, call, etc.)
  list(kind = "slice_index", start = start, end = end, src = src)
}

# Step 8.12: Constructor support IR nodes
.mojor_ir_rep <- function(x, times = NULL, each = NULL, length_out = NULL, src = NULL) {
 # Represents rep(x, times=, each=, length.out=)
 # x: IR expression node for the value to repeat
 # times: IR expression node or NULL
 # each: IR expression node or NULL
 # length_out: IR expression node or NULL
  list(kind = "rep", x = x, times = times, each = each, length_out = length_out, src = src)
}

.mojor_ir_c <- function(parts, src = NULL) {
 # Represents c(...) concatenation
 # parts: list of IR expression nodes
  list(kind = "c", parts = parts, src = src)
}

.mojor_ir_rep_len <- function(x, length_out, src = NULL) {
 # Represents rep_len(x, length.out=)
 # x: IR expression node for the value to repeat
 # length_out: IR expression node for output length
  list(kind = "rep_len", x = x, length_out = length_out, src = src)
}

.mojor_ir_seq <- function(from, to, length_out, src = NULL) {
 # Represents seq(from, to, length.out)
 # from: IR expression node for start value
 # to: IR expression node for end value (can be NULL)
 # length_out: IR expression node for number of elements
  list(kind = "seq", from = from, to = to, length_out = length_out, src = src)
}

# PR-B3 Step 3: Array utility function IR nodes
.mojor_ir_transpose <- function(x, src = NULL) {
 # Represents t(x) - matrix transpose
 # x: IR expression node for the matrix to transpose
 # The transpose is implemented via index transformation: t(x)[i,j] = x[j,i]
  list(kind = "transpose", x = x, src = src)
}

.mojor_ir_cbind <- function(args, src = NULL) {
 # Represents cbind(...) - column binding of vectors/matrices
 # args: list of IR expression nodes for the arguments to bind
 # Implemented via position-based indexing across columns
  list(kind = "cbind", args = args, src = src)
}

.mojor_ir_rbind <- function(args, src = NULL) {
 # Represents rbind(...) - row binding of vectors/matrices
 # args: list of IR expression nodes for the arguments to bind
 # Implemented via position-based indexing across rows
  list(kind = "rbind", args = args, src = src)
}

# PR-B4: diag() - matrix diagonal operations
.mojor_ir_diag <- function(x = NULL, n = NULL, src = NULL) {
 # Represents diag(x) - diagonal extraction or creation
 # Three modes:
 # 1. Extraction: x is a matrix -> extract diagonal elements
 # 2. Creation: x is a vector -> create diagonal matrix
 # 3. Identity: n is a scalar -> create n<U+00D7>n identity matrix
 # x: IR expression node (vector or matrix)
 # n: IR expression node for identity matrix size (scalar)
  list(kind = "diag", x = x, n = n, src = src)
}

# =============================================================================
# Higher-Order Functions, Strings, and Sampling
# =============================================================================

# Higher-Order Functions

# vapply() - apply function to each element with known value type
# x: IR expression node for input vector
# FUN: function to apply (inlineable, single-expression)
# FUN.VALUE: expected return type (scalar)
.mojor_ir_vapply <- function(x, fun, fun_value_type, src = NULL) {
  list(kind = "vapply", x = x, fun = fun, fun_value_type = fun_value_type, src = src)
}

# sapply() - apply function to each element, simplify to vector
# x: IR expression node for input vector
# FUN: function to apply (inlineable, single-expression)
.mojor_ir_sapply <- function(x, fun, src = NULL) {
  list(kind = "sapply", x = x, fun = fun, src = src)
}

# lapply() - apply function to each element, return list (deferred to future)
# For MVP: only supported when FUN returns scalar (returned as vector)
# x: IR expression node for input vector
# FUN: function to apply (inlineable, single-expression)
.mojor_ir_lapply <- function(x, fun, src = NULL) {
  list(kind = "lapply", x = x, fun = fun, src = src)
}

# mapply() - multivariate apply (2+ same-length numeric vectors)
# FUN: function to apply
# args: list of IR expression nodes for input vectors
.mojor_ir_mapply <- function(fun, args, src = NULL) {
  list(kind = "mapply", fun = fun, args = args, src = src)
}

# String Basics

# nchar() - number of characters in string(s)
# x: IR expression node for string(s)
.mojor_ir_nchar <- function(x, src = NULL) {
  list(kind = "nchar", x = x, src = src)
}

# nzchar() - non-zero length strings
# x: IR expression node for string(s)
.mojor_ir_nzchar <- function(x, src = NULL) {
  list(kind = "nzchar", x = x, src = src)
}

# substr() / substring() - extract/replace substrings
# x: IR expression node for string(s)
# start: IR expression node for start position
# stop: IR expression node for end position
.mojor_ir_substr <- function(x, start, stop, src = NULL) {
  list(kind = "substr", x = x, start = start, stop = stop, src = src)
}

# paste() / paste0() - string concatenation
# ...: list of IR expression nodes for strings to concatenate
# sep: separator string (single character)
# collapse: collapse vector into single string (NULL for no collapse)
.mojor_ir_paste <- function(args, sep = "\" \"", collapse = NULL, src = NULL) {
  list(kind = "paste", args = args, sep = sep, collapse = collapse, src = src)
}

# paste0() - paste with empty separator
.mojor_ir_paste0 <- function(args, collapse = NULL, src = NULL) {
  list(kind = "paste", args = args, sep = "\"\"", collapse = collapse, src = src)
}

# Sampling & Permutation

# sample.int() - sample from integer sequence 1:n
# n: IR expression node for population size (positive integer)
# size: IR expression node for sample size (non-negative integer)
# replace: IR expression node for sampling with replacement (logical)
# prob: IR expression node for probability weights (NULL for uniform)
.mojor_ir_sample_int <- function(n, size, replace = NULL, prob = NULL, src = NULL) {
  list(kind = "sample_int", n = n, size = size, replace = replace, prob = prob, src = src)
}

# sample() - sample from vector
# x: IR expression node for input vector
# size: IR expression node for sample size
# replace: IR expression node for sampling with replacement (logical)
# prob: IR expression node for probability weights (NULL for uniform)
.mojor_ir_sample <- function(x, size, replace = NULL, prob = NULL, src = NULL) {
  list(kind = "sample", x = x, size = size, replace = replace, prob = prob, src = src)
}

# =============================================================================
# Data Structures, Regex, and Table Utilities
# =============================================================================

# 9.1 Data structures
.mojor_ir_df_col_read <- function(df, col, src = NULL) {
  list(kind = "df_col_read", df = df, col = col, src = src)
}

.mojor_ir_df_col_exists_guard <- function(df, col, src = NULL) {
  list(kind = "df_col_exists_guard", df = df, col = col, src = src)
}

.mojor_ir_df_make <- function(cols, src = NULL) {
  list(kind = "df_make", cols = cols, src = src)
}

.mojor_ir_list_make <- function(items, names = NULL, src = NULL) {
  list(kind = "list_make", items = items, names = names, src = src)
}

# 9.2 Regex/pattern matching
.mojor_ir_regex_grepl <- function(pattern, x, ignore_case = FALSE, perl = TRUE, fixed = FALSE, src = NULL) {
  list(kind = "regex_grepl", pattern = pattern, x = x, ignore_case = ignore_case, perl = perl, fixed = fixed, src = src)
}

.mojor_ir_regex_grep <- function(pattern, x, value = FALSE, ignore_case = FALSE, perl = TRUE, fixed = FALSE, src = NULL) {
  list(kind = "regex_grep", pattern = pattern, x = x, value = value, ignore_case = ignore_case, perl = perl, fixed = fixed, src = src)
}

.mojor_ir_regex_sub <- function(pattern, replacement, x, ignore_case = FALSE, perl = TRUE, fixed = FALSE, global = FALSE, src = NULL) {
  list(kind = "regex_sub", pattern = pattern, replacement = replacement, x = x, ignore_case = ignore_case, perl = perl, fixed = fixed, global = global, src = src)
}

# 9.3 Table utilities
.mojor_ir_row_matrix <- function(x, src = NULL) {
  list(kind = "row_matrix", x = x, src = src)
}

.mojor_ir_col_matrix <- function(x, src = NULL) {
  list(kind = "col_matrix", x = x, src = src)
}

.mojor_ir_expand_grid <- function(args, src = NULL) {
  list(kind = "expand_grid", args = args, src = src)
}

# PR-B6: Matrix multiplication operations
.mojor_ir_matmul <- function(lhs, rhs, src = NULL) {
 # Represents A %*% B - standard matrix multiplication
 # lhs: IR expression node for left matrix A[m<U+00D7>n]
 # rhs: IR expression node for right matrix B[n<U+00D7>p]
 # Output: C[m<U+00D7>p] where C[i,j] = sum(A[i,k] * B[k,j]) for k=1:n
 # Dimensions: inner dimensions must match (lhs_ncol == rhs_nrow)
  list(kind = "matmul", lhs = lhs, rhs = rhs, src = src)
}

.mojor_ir_crossprod <- function(lhs, rhs = NULL, src = NULL) {
 # Represents crossprod(A, B) - optimized t(A) %*% B
 # lhs: IR expression node for left matrix A[m<U+00D7>n]
 # rhs: IR expression node for right matrix B[m<U+00D7>p] (optional)
 # If rhs is NULL: crossprod(A) = t(A) %*% A -> C[n<U+00D7>n]
 # If rhs is provided: crossprod(A, B) = t(A) %*% B -> C[n<U+00D7>p]
 # Output: C[i,j] = sum(A[k,i] * B[k,j]) for k=1:m
 # Dimensions: lhs and rhs must have same number of rows (m)
  list(kind = "crossprod", lhs = lhs, rhs = rhs, src = src)
}

.mojor_ir_tcrossprod <- function(lhs, rhs = NULL, src = NULL) {
 # Represents tcrossprod(A, B) - optimized A %*% t(B)
 # lhs: IR expression node for left matrix A[m<U+00D7>n]
 # rhs: IR expression node for right matrix B[p<U+00D7>n] (optional)
 # If rhs is NULL: tcrossprod(A) = A %*% t(A) -> C[m<U+00D7>m]
 # If rhs is provided: tcrossprod(A, B) = A %*% t(B) -> C[m<U+00D7>p]
 # Output: C[i,j] = sum(A[i,k] * B[j,k]) for k=1:n
 # Dimensions: lhs and rhs must have same number of columns (n)
  list(kind = "tcrossprod", lhs = lhs, rhs = rhs, src = src)
}

# PR-B5: Cumulative and statistical operations
.mojor_ir_cumsum <- function(x, src = NULL) {
 # Represents cumsum(x) - cumulative sum
 # Has loop-carried dependency: out[i] = out[i-1] + x[i]
 # Not parallelizable due to sequential dependency
  list(kind = "cumsum", x = x, src = src)
}

# Vectorized RNG and allocation nodes

# HIR node for vectorized RNG generation
# dist: distribution name ("runif", "rnorm", etc.)
# n: number of samples (scalar expression)
# params: list of parameter expressions (e.g., mean/sd for rnorm)
# src: optional source R expression
.mojor_ir_rng_vec <- function(dist, n, params = NULL, src = NULL) {
  list(kind = "rng_vec", dist = dist, n = n, params = params, src = src)
}

# HIR node for temporary allocation
# len: allocation length (scalar expression)
# dtype: data type ("f64", "f32", "i32", etc.)
# src: optional source R expression
.mojor_ir_alloc <- function(len, dtype, src = NULL) {
  list(kind = "alloc", len = len, dtype = dtype, src = src)
}

.mojor_ir_cumprod <- function(x, src = NULL) {
 # Represents cumprod(x) - cumulative product
 # Has loop-carried dependency: out[i] = out[i-1] * x[i]
  list(kind = "cumprod", x = x, src = src)
}

.mojor_ir_mean <- function(x, na_rm = FALSE, src = NULL) {
 # Represents mean(x, na.rm = <logical>) - arithmetic mean (scalar reduction)
  list(kind = "mean", x = x, na_rm = na_rm, src = src)
}

.mojor_ir_cummax <- function(x, src = NULL) {
 # Represents cummax(x) - cumulative maximum
 # Has loop-carried dependency: out[i] = max(out[i-1], x[i])
  list(kind = "cummax", x = x, src = src)
}

.mojor_ir_cummin <- function(x, src = NULL) {
 # Represents cummin(x) - cumulative minimum
 # Has loop-carried dependency: out[i] = min(out[i-1], x[i])
  list(kind = "cummin", x = x, src = src)
}

.mojor_ir_variance <- function(x, na_rm = FALSE, src = NULL) {
 # Represents var(x, na.rm = <logical>) - sample variance (scalar reduction)
  list(kind = "var_stat", x = x, na_rm = na_rm, src = src)
}

.mojor_ir_sd <- function(x, na_rm = FALSE, src = NULL) {
 # Represents sd(x, na.rm = <logical>) - standard deviation (scalar reduction)
  list(kind = "sd", x = x, na_rm = na_rm, src = src)
}

# Implicit loop expression node
.mojor_ir_vexpr <- function(len, body, src = NULL) {
 # Represents a vectorized expression for implicit loop generation
 # len: IR expression node for output length (scalar)
 # body: IR expression node for the elementwise operation
 # src: optional source R expression
  list(kind = "vexpr", len = len, body = body, src = src)
}

# PR-B7.2: Covariance and correlation operations
.mojor_ir_cov <- function(x, y = NULL, src = NULL) {
 # Represents cov(x, y) - covariance
 # x: IR expression node for first input (vector or matrix)
 # y: IR expression node for second input (optional, vector or matrix)
 # If y is NULL: cov(x) = var(x) for vectors, covariance matrix for matrices
 # If y is provided: covariance between x and y (scalar for vectors, matrix for matrices)
 # For vectors: cov(x, y) = sum((x - mean(x)) * (y - mean(y))) / (n - 1)
 # For matrices: column-wise covariance matrix
  list(kind = "cov", x = x, y = y, src = src)
}

.mojor_ir_cor <- function(x, y = NULL, src = NULL) {
 # Represents cor(x, y) - Pearson correlation coefficient
 # x: IR expression node for first input (vector or matrix)
 # y: IR expression node for second input (optional, vector or matrix)
 # If y is NULL: cor(x) = 1 for vectors, correlation matrix for matrices
 # If y is provided: correlation between x and y (scalar for vectors, matrix for matrices)
 # For vectors: cor(x, y) = cov(x, y) / (sd(x) * sd(y))
 # For matrices: column-wise correlation matrix
  list(kind = "cor", x = x, y = y, src = src)
}

# Metadata expressions
.mojor_ir_dim <- function(x, src = NULL) {
 # Represents dim(x) - dimension query
 # For vectors: returns length as scalar or c(length)
 # For matrices: returns c(nrow, ncol) as integer vector
 # Runtime evaluation required (dimensions not always known at compile time)
  list(kind = "dim", x = x, src = src)
}

.mojor_ir_type_predicate <- function(predicate, x, src = NULL) {
 # Represents type predicates: is.vector(), is.matrix(), is.numeric(), etc.
 # predicate: string like "is.vector", "is.matrix", "is.numeric"
 # x: IR expression node (usually a variable)
 # Can be folded to compile-time constant TRUE/FALSE based on type annotations
  list(kind = "type_predicate", predicate = predicate, x = x, src = src)
}

.mojor_ir_type_query <- function(query, x, src = NULL) {
 # Represents type queries: typeof(), mode(), class()
 # query: string like "typeof", "mode", "class"
 # x: IR expression node (usually a variable)
 # Can be folded to compile-time string constant based on type annotations
  list(kind = "type_query", query = query, x = x, src = src)
}

.mojor_ir_scalar_index <- function(expr, src = NULL) {
 # Represents a scalar index like i or j+1
 # expr: IR expression node
  list(kind = "scalar_index", expr = expr, src = src)
}

.mojor_ir_vec_index <- function(expr, src = NULL, selection = NULL) {
 # Represents a vector selector index (e.g. c(1,3), seq.int(...))
 # expr: IR expression node (or NULL when selection metadata is provided)
 # selection: optional canonical selector metadata used by legacy emit lanes
  list(kind = "vec_index", expr = expr, src = src, pos_vec_selection = selection)
}

.mojor_ir_missing_index <- function(src = NULL) {
 # Represents a missing index mat[i, ] (entire dimension)
  list(kind = "missing_index", src = src)
}

.mojor_ir_subscript <- function(var, indices, src = NULL) {
 # Represents subscripted variable like out[1:n, j]
 # var: variable name (string)
 # indices: list of index nodes (slice_index, scalar_index, missing_index)
  list(kind = "subscript", var = var, indices = indices, src = src)
}

.mojor_ir_range <- function(start, end, step = NULL, src = NULL, end_exclusive = FALSE) {
  list(kind = "range", start = start, end = end, step = step, end_exclusive = end_exclusive, src = src)
}

.mojor_ir_raw <- function(expr, src = NULL) {
  list(kind = "raw", expr = expr, src = src)
}

.mojor_ir_range_expr <- function(expr, src = NULL) {
  list(kind = "range_expr", expr = expr, src = src)
}

# Try to parse a for-loop range expression into a structured range node.
# Handles: 1:n (ascending from 1), seq_len(n), seq(from, to), seq(from, to, by=).
# Returns a range node or NULL (caller falls back to range_expr).
.mojor_ir_try_build_range <- function(expr) {
  expr_src <- expr
  expr <- .mojor_ir_normalize_selector_ast(expr, mode = "loop_seq")
  if (is.call(expr_src) &&
      length(expr_src) >= 1L &&
      as.character(expr_src[[1]]) %in% c("seq_len", "seq_along", "mojor_seq_len")) {
    expr <- expr_src
  }
  if (!is.call(expr)) {
    return(NULL)
  }
  op <- as.character(expr[[1]])

 # Pattern: a:b where BOTH sides are integer constants (e.g. 1:5).
 # Variable-end ranges like 1:n need dynamic sign detection and stay as range_expr.
  if (op == ":" && length(expr) == 3) {
    start <- expr[[2]]
    end <- expr[[3]]
    if (is.numeric(start) && length(start) == 1 && start == floor(start) && !is.na(start) &&
      is.numeric(end) && length(end) == 1 && end == floor(end) && !is.na(end)) {
      # Detect decreasing sequences and add negative step
      if (start > end) {
        return(.mojor_ir_range(
          .mojor_ir_const(as.character(as.integer(start))),
          .mojor_ir_const(as.character(as.integer(end))),
          step = .mojor_ir_const("-1")
        ))
      }
      return(.mojor_ir_range(
        .mojor_ir_const(as.character(as.integer(start))),
        .mojor_ir_const(as.character(as.integer(end)))
      ))
    }
  }

 # Pattern: seq_len(n) where n is a name, integer constant, or any scalar i32 expression
  if (op == "seq_len" && length(expr) == 2) {
    arg <- expr[[2]]
    if (is.name(arg)) {
      return(.mojor_ir_range(
        .mojor_ir_const("1"),
        .mojor_ir_var(as.character(arg))
      ))
    }
    if (is.numeric(arg) && length(arg) == 1 && arg == floor(arg) && !is.na(arg)) {
      return(.mojor_ir_range(
        .mojor_ir_const("1"),
        .mojor_ir_const(as.character(as.integer(arg)))
      ))
    }
 # Handle complex scalar i32 expressions (e.g., seq_len(nr) where nr is local var)
 # Use .mojor_ir_expr_build to handle any valid scalar i32 expression
    arg_ir <- tryCatch(.mojor_ir_expr_build(arg), error = function(e) NULL)
    if (!is.null(arg_ir)) {
      return(.mojor_ir_range(
        .mojor_ir_const("1"),
        arg_ir
      ))
    }
  }

 # Pattern: seq(from, to) or seq(from, to, by=) - PR-B3 Step 2
 # Only handle simple cases with names or constants for range construction
  if (op == "seq" && length(expr) >= 3) {
    parts <- as.list(expr)[-1]
    nms <- names(parts)
    get_arg <- function(nm, pos) {
      if (!is.null(nms) && nm %in% nms) {
        return(parts[[which(nms == nm)[1]]])
      }
      if (length(parts) >= pos && (is.null(nms) || is.null(nms[[pos]]) || nms[[pos]] == "")) {
        return(parts[[pos]])
      }
      NULL
    }

    from_expr <- get_arg("from", 1)
    to_expr <- get_arg("to", 2)
    by_expr <- get_arg("by", 3)
    length_out_expr <- get_arg("length.out", 4)

 # seq() without from defaults to 1
    if (is.null(from_expr) && !is.null(to_expr)) {
      from_expr <- 1
    }

 # Need from and either to or length.out
    if (!is.null(from_expr) && (!is.null(to_expr) || !is.null(length_out_expr))) {
 # Build start IR
      start_ir <- NULL
      if (is.numeric(from_expr) && length(from_expr) == 1 && !is.na(from_expr)) {
        start_ir <- .mojor_ir_const(as.character(as.integer(from_expr)))
      } else if (is.name(from_expr)) {
        start_ir <- .mojor_ir_var(as.character(from_expr))
      }

 # Build end/step IR
      end_ir <- NULL
      step_ir <- NULL

      if (!is.null(by_expr) && !is.null(to_expr)) {
 # seq(from, to, by=) - step mode
        if (is.numeric(to_expr) && length(to_expr) == 1 && !is.na(to_expr)) {
          end_ir <- .mojor_ir_const(as.character(as.integer(to_expr)))
        } else if (is.name(to_expr)) {
          end_ir <- .mojor_ir_var(as.character(to_expr))
        }
        if (is.numeric(by_expr) && length(by_expr) == 1 && !is.na(by_expr)) {
          step_ir <- .mojor_ir_const(as.character(as.integer(by_expr)))
        } else if (is.name(by_expr)) {
          step_ir <- .mojor_ir_var(as.character(by_expr))
        } else if (is.call(by_expr)) {
          # Handle call expressions like -1 (unary minus)
          step_ir <- tryCatch(.mojor_ir_expr_build(by_expr), error = function(e) NULL)
        }
      } else if (!is.null(length_out_expr)) {
 # seq(from, length.out=) - length mode: equivalent to seq_len(length.out) + (from-1)
 # We treat this as range(1, length.out) for iteration, adjust index in body if needed
        if (is.numeric(length_out_expr) && length(length_out_expr) == 1 && !is.na(length_out_expr)) {
          end_ir <- .mojor_ir_const(as.character(as.integer(length_out_expr)))
        } else if (is.name(length_out_expr)) {
          end_ir <- .mojor_ir_var(as.character(length_out_expr))
        }
 # from is the start
        if (is.numeric(from_expr) && length(from_expr) == 1 && !is.na(from_expr) && from_expr == 1) {
 # seq(1, length.out=n) is equivalent to seq_len(n)
 # start is already 1, end is length.out
        }
      } else if (!is.null(to_expr)) {
 # seq(from, to) - simple range
        if (is.numeric(to_expr) && length(to_expr) == 1 && !is.na(to_expr)) {
          end_ir <- .mojor_ir_const(as.character(as.integer(to_expr)))
        } else if (is.name(to_expr)) {
          end_ir <- .mojor_ir_var(as.character(to_expr))
        }
      }

      if (!is.null(start_ir) && !is.null(end_ir)) {
 # For seq() with from != 1, we need special handling
 # Currently only support from=1 for range construction
        if (!is.null(by_expr) || is.null(step_ir)) {
          return(.mojor_ir_range(start_ir, end_ir, step = step_ir))
        } else {
          return(.mojor_ir_range(start_ir, end_ir))
        }
      }
    }
  }

  NULL
}

# =============================================================================
# Effect System
# =============================================================================
#
# .mojor_ir_expr_effects(node)
#
# Returns the effect signature of an expression node. Effects are used by
# optimization passes to determine which operations can be reordered, CSE'd,
# or hoisted out of loops.
#
# Effect lattice:
# Pure <U+2014> No side effects, deterministic (arithmetic, comparisons)
# ReadsMem <U+2014> Reads from memory (array indexing)
# WritesMem <U+2014> Writes to memory (not applicable to expressions)
# RNG <U+2014> Random number generation (non-deterministic)
# Status <U+2014> Modifies status flags (NA flag writes)
# Unknown <U+2014> Unknown or mixed effects
#
# Returns: character vector of effects (may have multiple)
#
.mojor_ir_rng_metadata <- function() {
 # Shared RNG metadata used across IR build/verify/emit and transpile import logic.
  .mojor_rng_catalog_metadata()
}

.mojor_ir_rng_call_fns <- function() {
  names(.mojor_ir_rng_metadata())
}

.mojor_ir_rng_table_fns <- function() {
 # Distributions that depend on ziggurat tables in generated Mojo.
  meta <- .mojor_ir_rng_metadata()
  names(meta)[vapply(meta, function(x) isTRUE(x$needs_tables), logical(1))]
}

.mojor_ir_rng_helper_symbol_map <- function() {
  meta <- .mojor_ir_rng_metadata()
  lapply(meta, function(x) unique(as.character(x$helper_symbols)))
}

.mojor_ir_scalar_rng_emit <- function(fn, params = character(0)) {
  fn <- as.character(fn)
  if (length(fn) != 1 || is.na(fn) || !nzchar(fn)) {
    return(NULL)
  }
  params <- as.character(params)

  if (exists(".mojor_state", inherits = TRUE) && is.environment(.mojor_state)) {
    .mojor_state$needs_mojo_random <- TRUE
  }

  meta <- .mojor_ir_rng_metadata()
  info <- meta[[fn]]
  if (is.null(info)) {
    return(NULL)
  }
  min_params <- info$min_params
  if (is.null(min_params) || !is.numeric(min_params) || length(min_params) != 1) {
    stop(sprintf(".mojor_ir_scalar_rng_emit: invalid min_params metadata for '%s'", fn))
  }
  min_params <- as.integer(min_params[[1]])
  max_params <- info$max_params
  if (is.null(max_params) || !is.numeric(max_params) || length(max_params) != 1) {
    stop(sprintf(".mojor_ir_scalar_rng_emit: invalid max_params metadata for '%s'", fn))
  }
  max_params <- as.integer(max_params[[1]])
  if (max_params < min_params) {
    stop(sprintf(".mojor_ir_scalar_rng_emit: invalid param bounds metadata for '%s'", fn))
  }
  if (length(params) < min_params) {
    return(NULL)
  }
  if (length(params) > max_params) {
    return(NULL)
  }

  switch(fn,
    runif = {
      min_val <- if (length(params) >= 1) params[[1]] else "0.0"
      max_val <- if (length(params) >= 2) params[[2]] else "1.0"
      paste0("(", min_val, " + (", max_val, " - ", min_val, ") * _rng_next_f64(__mojor_rng_state))")
    },
    rnorm = {
      mean_val <- if (length(params) >= 1) params[[1]] else "0.0"
      sd_val <- if (length(params) >= 2) params[[2]] else "1.0"
      paste0("(", mean_val, " + ", sd_val, " * _random_standard_normal(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi))")
    },
    rgamma = {
      shape_val <- params[[1]]
      rate_val <- if (length(params) >= 2) params[[2]] else "1.0"
      paste0("(_random_standard_gamma(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi, ", shape_val, ") / ", rate_val, ")")
    },
    rbinom = {
      paste0("Float64(_random_binomial(__mojor_rng_state, Int(", params[[1]], "), ", params[[2]], "))")
    },
    rexp = {
      rate_val <- if (length(params) >= 1) params[[1]] else "1.0"
      paste0("(-log1p(-_rng_next_f64(__mojor_rng_state)) / ", rate_val, ")")
    },
    rpois = {
      paste0("Float64(_random_poisson(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi, ", params[[1]], "))")
    },
    rlnorm = {
      meanlog_val <- if (length(params) >= 1) params[[1]] else "0.0"
      sdlog_val <- if (length(params) >= 2) params[[2]] else "1.0"
      paste0("exp(", meanlog_val, " + ", sdlog_val, " * _random_standard_normal(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi))")
    },
    rchisq = {
      paste0("_random_chisq(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi, ", params[[1]], ")")
    },
    rt = {
      df_val <- params[[1]]
      paste0("(_random_standard_normal(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi) / sqrt(_random_chisq(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi, ", df_val, ") / ", df_val, "))")
    },
    rf = {
      df1_val <- params[[1]]
      df2_val <- params[[2]]
      paste0("((_random_chisq(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi, ", df1_val, ") / ", df1_val, ") / (_random_chisq(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi, ", df2_val, ") / ", df2_val, "))")
    },
    rbeta = {
      paste0("_random_beta(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi, ", params[[1]], ", ", params[[2]], ")")
    },
    rweibull = {
      scale_val <- if (length(params) >= 2) params[[2]] else "1.0"
      paste0("_random_weibull(__mojor_rng_state, ", params[[1]], ", ", scale_val, ")")
    },
    rlogis = {
      location_val <- if (length(params) >= 1) params[[1]] else "0.0"
      scale_val <- if (length(params) >= 2) params[[2]] else "1.0"
      paste0("_random_logistic(__mojor_rng_state, ", location_val, ", ", scale_val, ")")
    },
    rcauchy = {
      location_val <- if (length(params) >= 1) params[[1]] else "0.0"
      scale_val <- if (length(params) >= 2) params[[2]] else "1.0"
      paste0("_random_cauchy(__mojor_rng_state, ", location_val, ", ", scale_val, ")")
    },
    rgeom = {
      paste0("Float64(_random_geometric(__mojor_rng_state, ", params[[1]], "))")
    },
    rnbinom = {
      size_val <- params[[1]]
      prob_val <- params[[2]]
      paste0("Float64(_random_poisson(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi, (_random_standard_gamma(__mojor_rng_state, __mojor_rng_ki, __mojor_rng_wi, __mojor_rng_fi, ", size_val, ") / (", prob_val, " / (1.0 - ", prob_val, ")))))")
    },
    rhyper = {
      paste0("Float64(_random_hypergeometric(__mojor_rng_state, Int(", params[[1]], "), Int(", params[[2]], "), Int(", params[[3]], ")))")
    },
    rsignrank = {
      paste0("Float64(_random_signrank(__mojor_rng_state, Int(", params[[1]], ")))")
    },
    rwilcox = {
      paste0("Float64(_random_wilcox(__mojor_rng_state, Int(", params[[1]], "), Int(", params[[2]], ")))")
    },
    NULL
  )
}

.mojor_ir_is_rng_call_fn <- function(fn) {
  fn <- as.character(fn)
  length(fn) == 1 && !is.na(fn) && fn %in% .mojor_ir_rng_call_fns()
}

.mojor_ir_rng_resource <- function(access = "RNG") {
  list(resource_kind = "Ref", resource_id = "rng:state", access = as.character(access))
}

.mojor_ir_expr_effects <- function(node) {
  if (is.null(node) || !is.list(node)) {
    return("Unknown")
  }

  k <- node$kind
  if (is.null(k)) {
    return("Unknown")
  }

  switch(k,
 # Pure: no side effects, deterministic
    const = "Pure",
    var = "Pure", # Variable reference is pure (doesn't read memory, just names)

 # Unary/binary ops: inherit effects from operands
    unop = {
      .mojor_ir_expr_effects(node$expr)
    },
    binop = {
      lhs_eff <- .mojor_ir_expr_effects(node$lhs)
      rhs_eff <- .mojor_ir_expr_effects(node$rhs)
      unique(c(lhs_eff, rhs_eff))
    },

 # Cast: inherits effects from operand
    cast = {
      .mojor_ir_expr_effects(node$expr)
    },

 # Index: reads memory
    index = {
      base_eff <- .mojor_ir_expr_effects(node$base)
 # Combine base effects with index effects
      idx_effs <- lapply(node$indices, .mojor_ir_expr_effects)
      all_effs <- c(base_eff, unlist(idx_effs), "ReadsMem")
      unique(all_effs)
    },

 # Call: depends on function
    call = {
      fn <- node$fn
 # RNG functions
      if (.mojor_ir_is_rng_call_fn(fn)) {
        arg_effs <- if (!is.null(node$args)) lapply(node$args, .mojor_ir_expr_effects) else list()
        arg_effs <- setdiff(unique(unlist(arg_effs)), "Pure")
        return(unique(c(arg_effs, "RNG")))
      }
 # Math functions are pure
      if (fn %in% c(
        "sin", "cos", "tan", "asin", "acos", "atan",
        "sinh", "cosh", "tanh", "asinh", "acosh", "atanh",
        "log", "log10", "log1p", "log2", "exp", "expm1",
        "sqrt", "abs", "abs2", "floor", "ceiling", "trunc", "round",
        "sign", "cbrt", "lgamma", "erf", "gamma", "atan2", "hypot", "pow"
      )) {
 # But check arguments for effects
        arg_effs <- lapply(node$args, .mojor_ir_expr_effects)
        return(unique(unlist(arg_effs)))
      }
 # length() is pure on variable names, but if argument has effects, inherit them
      if (fn == "length") {
        arg_effs <- lapply(node$args, .mojor_ir_expr_effects)
        return(unique(unlist(arg_effs)))
      }
 # min/max are pure
      if (fn %in% c("min", "max", "pmin", "pmax")) {
        arg_effs <- lapply(node$args, .mojor_ir_expr_effects)
        return(unique(unlist(arg_effs)))
      }
 # is.na, is.nan, is.finite, is.infinite, as.logical - pure
      if (fn %in% c("is.na", "is.nan", "is.finite", "is.infinite", "as.logical")) {
        arg_effs <- lapply(node$args, .mojor_ir_expr_effects)
        return(unique(unlist(arg_effs)))
      }
 # Unknown function
      "Unknown"
    },

 # ifelse: union of effects from all branches
    ifelse = {
      cond_eff <- .mojor_ir_expr_effects(node$cond)
      yes_eff <- .mojor_ir_expr_effects(node$yes)
      no_eff <- .mojor_ir_expr_effects(node$no)
      unique(c(cond_eff, yes_eff, no_eff))
    },

 # Indexing helpers: depend on wrapped expressions
    scalar_index = {
      .mojor_ir_expr_effects(node$expr)
    },
    slice_index = {
      start_eff <- .mojor_ir_expr_effects(node$start)
      end_eff <- .mojor_ir_expr_effects(node$end)
      unique(c(start_eff, end_eff))
    },
    missing_index = "Pure",

 # Constructors: depend on parts
    rep = {
      x_eff <- .mojor_ir_expr_effects(node$x)
 # times/each/length_out might have effects
      times_eff <- if (!is.null(node$times)) .mojor_ir_expr_effects(node$times) else NULL
      each_eff <- if (!is.null(node$each)) .mojor_ir_expr_effects(node$each) else NULL
      len_eff <- if (!is.null(node$length_out)) .mojor_ir_expr_effects(node$length_out) else NULL
      unique(c(x_eff, times_eff, each_eff, len_eff))
    },
    rep_len = {
      x_eff <- .mojor_ir_expr_effects(node$x)
      len_eff <- .mojor_ir_expr_effects(node$length_out)
      unique(c(x_eff, len_eff))
    },
    c = {
      part_effs <- lapply(node$parts, .mojor_ir_expr_effects)
      unique(unlist(part_effs))
    },

 # PR-B3 Step 3: Array utility constructors
    seq = {
      from_eff <- .mojor_ir_expr_effects(node$from)
      len_eff <- .mojor_ir_expr_effects(node$length_out)
      to_eff <- if (!is.null(node$to)) .mojor_ir_expr_effects(node$to) else NULL
      unique(c(from_eff, len_eff, to_eff))
    },
    transpose = {
 # t(x) effects are same as x (just index transformation)
      .mojor_ir_expr_effects(node$x)
    },
    cbind = {
 # Union of all arg effects
      arg_effs <- lapply(node$args, .mojor_ir_expr_effects)
      unique(unlist(arg_effs))
    },
    rbind = {
 # Union of all arg effects
      arg_effs <- lapply(node$args, .mojor_ir_expr_effects)
      unique(unlist(arg_effs))
    },

 # PR-B4: diag() effects
    diag = {
 # diag(x) or diag(n) - effects from argument
      if (!is.null(node$x)) {
        .mojor_ir_expr_effects(node$x)
      } else if (!is.null(node$n)) {
        .mojor_ir_expr_effects(node$n)
      } else {
        "Pure"
      }
    },

 # PR-B5: Cumulative operation effects
    cumsum = {
 # cumsum(x) - reads memory from x, has loop-carried dependency
      x_eff <- .mojor_ir_expr_effects(node$x)
      if ("ReadsMem" %in% x_eff) "ReadsMem" else "Pure"
    },
    cumprod = {
 # cumprod(x) - reads memory from x, has loop-carried dependency
      x_eff <- .mojor_ir_expr_effects(node$x)
      if ("ReadsMem" %in% x_eff) "ReadsMem" else "Pure"
    },

 # PR-B5: Statistical function effects
    mean = {
 # mean(x) - scalar reduction, reads memory from x
      x_eff <- .mojor_ir_expr_effects(node$x)
      if ("ReadsMem" %in% x_eff) "ReadsMem" else "Pure"
    },

 # PR-B5 Step 3: Additional cumulative operation effects
    cummax = {
 # cummax(x) - reads memory from x, has loop-carried dependency
      x_eff <- .mojor_ir_expr_effects(node$x)
      if ("ReadsMem" %in% x_eff) "ReadsMem" else "Pure"
    },
    cummin = {
 # cummin(x) - reads memory from x, has loop-carried dependency
      x_eff <- .mojor_ir_expr_effects(node$x)
      if ("ReadsMem" %in% x_eff) "ReadsMem" else "Pure"
    },

 # PR-B5 Step 3: Variance and standard deviation effects
    var_stat = {
 # var(x) - scalar reduction, reads memory from x
      x_eff <- .mojor_ir_expr_effects(node$x)
      if ("ReadsMem" %in% x_eff) "ReadsMem" else "Pure"
    },
    sd = {
 # sd(x) - scalar reduction, reads memory from x
      x_eff <- .mojor_ir_expr_effects(node$x)
      if ("ReadsMem" %in% x_eff) "ReadsMem" else "Pure"
    },

 # Range: pure (just describes iteration)
    range = {
      start_eff <- .mojor_ir_expr_effects(node$start)
      end_eff <- .mojor_ir_expr_effects(node$end)
      step_eff <- if (!is.null(node$step)) .mojor_ir_expr_effects(node$step) else NULL
      unique(c(start_eff, end_eff, step_eff))
    },
    range_expr = "Pure", # R expression, effects unknown but treated as pure

 # Vectorized RNG - always has RNG effect
    rng_vec = {
      n_eff <- if (!is.null(node$n)) .mojor_ir_expr_effects(node$n) else "Pure"
      param_effs <- if (!is.null(node$params)) unlist(lapply(node$params, .mojor_ir_expr_effects)) else "Pure"
      upstream <- setdiff(unique(c(n_eff, param_effs)), "Pure")
      unique(c(upstream, "RNG"))
    },

 # Allocation - pure (no side effects, just memory allocation)
    alloc = "Pure",

 # apply() - reads memory from input matrix
    apply = {
      x_eff <- .mojor_ir_expr_effects(node$x)
      if ("ReadsMem" %in% x_eff) "ReadsMem" else "Pure"
    },

 # sample_int() - reads memory from n/size if they're expressions
    sample_int = {
      n_eff <- .mojor_ir_expr_effects(node$n)
      size_eff <- .mojor_ir_expr_effects(node$size)
      replace_eff <- if (!is.null(node$replace)) .mojor_ir_expr_effects(node$replace) else "Pure"
      prob_eff <- if (!is.null(node$prob)) .mojor_ir_expr_effects(node$prob) else "Pure"
      upstream <- setdiff(unique(c(n_eff, size_eff, replace_eff, prob_eff)), "Pure")
      unique(c(upstream, "RNG"))
    },

 # sample() - reads memory from x and size
    sample = {
      x_eff <- .mojor_ir_expr_effects(node$x)
      size_eff <- .mojor_ir_expr_effects(node$size)
      replace_eff <- if (!is.null(node$replace)) .mojor_ir_expr_effects(node$replace) else "Pure"
      prob_eff <- if (!is.null(node$prob)) .mojor_ir_expr_effects(node$prob) else "Pure"
      upstream <- setdiff(unique(c(x_eff, size_eff, replace_eff, prob_eff)), "Pure")
      unique(c(upstream, "RNG"))
    },
    gpu_reduce = {
      arg_eff <- .mojor_ir_expr_effects(node$arg)
      dims_eff <- if (!is.null(node$dims)) .mojor_ir_expr_effects(node$dims) else "Pure"
      unique(c(arg_eff, dims_eff, "ReadsMem"))
    },
    gpu_matmul = {
      a_eff <- .mojor_ir_expr_effects(node$a)
      b_eff <- .mojor_ir_expr_effects(node$b)
      unique(c(a_eff, b_eff, "ReadsMem"))
    },

 # vexpr - pure (no side effects, just describes computation)
    vexpr = {
      len_eff <- .mojor_ir_expr_effects(node$len)
      body_eff <- .mojor_ir_expr_effects(node$body)
      unique(c(len_eff, body_eff))
    },

 # Set/Match primitives - read memory from input(s)
    unique = {
      x_eff <- .mojor_ir_expr_effects(node$x)
      if ("ReadsMem" %in% x_eff) "ReadsMem" else "Pure"
    },
    duplicated = {
      x_eff <- .mojor_ir_expr_effects(node$x)
      if ("ReadsMem" %in% x_eff) "ReadsMem" else "Pure"
    },
    any_duplicated = {
      x_eff <- .mojor_ir_expr_effects(node$x)
      if ("ReadsMem" %in% x_eff) "ReadsMem" else "Pure"
    },
    match = {
      x_eff <- .mojor_ir_expr_effects(node$x)
      table_eff <- .mojor_ir_expr_effects(node$table)
      unique(c(x_eff, table_eff, "ReadsMem"))
    },
    `%in%` = {
      x_eff <- .mojor_ir_expr_effects(node$x)
      table_eff <- .mojor_ir_expr_effects(node$table)
      unique(c(x_eff, table_eff, "ReadsMem"))
    },

 # Quantiles & Robust Stats - read memory from input
    median = {
      x_eff <- .mojor_ir_expr_effects(node$x)
      if ("ReadsMem" %in% x_eff) "ReadsMem" else "Pure"
    },
    quantile = {
      x_eff <- .mojor_ir_expr_effects(node$x)
      if ("ReadsMem" %in% x_eff) "ReadsMem" else "Pure"
    },
    iqr = {
      x_eff <- .mojor_ir_expr_effects(node$x)
      if ("ReadsMem" %in% x_eff) "ReadsMem" else "Pure"
    },
    mad = {
      x_eff <- .mojor_ir_expr_effects(node$x)
      if ("ReadsMem" %in% x_eff) "ReadsMem" else "Pure"
    },

 # Higher-Order Functions - read memory from input(s)
    vapply = {
      x_eff <- .mojor_ir_expr_effects(node$x)
      if ("ReadsMem" %in% x_eff) "ReadsMem" else "Pure"
    },
    sapply = {
      x_eff <- .mojor_ir_expr_effects(node$x)
      if ("ReadsMem" %in% x_eff) "ReadsMem" else "Pure"
    },
    lapply = {
      x_eff <- .mojor_ir_expr_effects(node$x)
      if ("ReadsMem" %in% x_eff) "ReadsMem" else "Pure"
    },
    mapply = {
      arg_effs <- lapply(node$args, .mojor_ir_expr_effects)
      unique(c(unlist(arg_effs), "ReadsMem"))
    },

 # String Basics - read memory from input(s)
    nchar = {
      x_eff <- .mojor_ir_expr_effects(node$x)
      if ("ReadsMem" %in% x_eff) "ReadsMem" else "Pure"
    },
    nzchar = {
      x_eff <- .mojor_ir_expr_effects(node$x)
      if ("ReadsMem" %in% x_eff) "ReadsMem" else "Pure"
    },
    substr = {
      x_eff <- .mojor_ir_expr_effects(node$x)
      start_eff <- .mojor_ir_expr_effects(node$start)
      stop_eff <- .mojor_ir_expr_effects(node$stop)
      unique(c(x_eff, start_eff, stop_eff))
    },
    paste = {
      arg_effs <- lapply(node$args, .mojor_ir_expr_effects)
      unique(c(unlist(arg_effs), "ReadsMem"))
    },

 # Fallback
    raw = "Unknown",
    "Unknown"
  )
}


# Helper: Check if an expression is pure (no side effects)
.mojor_ir_is_pure <- function(node) {
  effs <- .mojor_ir_expr_effects(node)
  all(effs == "Pure")
}

# Helper: Check if an expression has RNG effects
.mojor_ir_has_rng <- function(node) {
  effs <- .mojor_ir_expr_effects(node)
  "RNG" %in% effs
}

# Helper: Check if an expression reads memory
.mojor_ir_reads_mem <- function(node) {
  effs <- .mojor_ir_expr_effects(node)
  "ReadsMem" %in% effs
}

.mojor_ir_map_effect_classes <- function(effects) {
  if (is.null(effects) || length(effects) == 0) {
    return("Unknown")
  }
  out <- character(0)
  for (eff in as.character(effects)) {
    cls <- switch(eff,
      "Pure" = "None",
      "ReadsMem" = "Read",
      "WritesMem" = "Write",
      "RNG" = "RNG",
      "Unknown" = "Unknown",
      "Status" = "Unknown",
      "Unknown"
    )
    out <- c(out, cls)
  }
  unique(out)
}

.mojor_ir_resource_from_name <- function(name, access = "Read") {
  name <- as.character(name)
  if (length(name) != 1 || is.na(name) || !nzchar(name)) {
    return(list(resource_kind = "Unknown", resource_id = "unknown:<empty>", access = as.character(access)))
  }
  list(
    resource_kind = "Ref",
    resource_id = paste0("var:", name),
    access = as.character(access)
  )
}

.mojor_ir_resource_key <- function(res) {
  if (is.null(res) || !is.list(res)) {
    return("")
  }
  kind <- if (is.null(res$resource_kind)) "Unknown" else as.character(res$resource_kind)
  rid <- if (is.null(res$resource_id)) "unknown" else as.character(res$resource_id)
  acc <- if (is.null(res$access)) "Unknown" else as.character(res$access)
  paste(kind, rid, acc, sep = "|")
}

.mojor_ir_unique_resources <- function(resources) {
  if (is.null(resources) || length(resources) == 0) {
    return(list())
  }
  keys <- character(0)
  out <- list()
  for (res in resources) {
    key <- .mojor_ir_resource_key(res)
    if (!nzchar(key) || key %in% keys) next
    keys <- c(keys, key)
    out[[length(out) + 1L]] <- res
  }
  out
}

.mojor_ir_expr_resources <- function(node) {
  if (is.null(node) || !is.list(node) || is.null(node$kind)) {
    return(list())
  }

  k <- node$kind
  switch(k,
    var = {
      list(.mojor_ir_resource_from_name(node$name, access = "Read"))
    },
    index = {
      out <- list()
      if (!is.null(node$base) && is.list(node$base) && identical(node$base$kind, "var")) {
        out[[length(out) + 1L]] <- .mojor_ir_resource_from_name(node$base$name, access = "Read")
      } else {
        out[[length(out) + 1L]] <- list(resource_kind = "Unknown", resource_id = "unknown:index_base", access = "Read")
      }
      out <- c(out, .mojor_ir_expr_resources(node$base))
      if (!is.null(node$indices) && length(node$indices) > 0) {
        for (idx in node$indices) out <- c(out, .mojor_ir_expr_resources(idx))
      }
      .mojor_ir_unique_resources(out)
    },
    unop = .mojor_ir_expr_resources(node$expr),
    cast = .mojor_ir_expr_resources(node$expr),
    binop = .mojor_ir_unique_resources(c(.mojor_ir_expr_resources(node$lhs), .mojor_ir_expr_resources(node$rhs))),
    call = {
      out <- list()
      if (!is.null(node$fn) && .mojor_ir_is_rng_call_fn(node$fn)) {
        out[[length(out) + 1L]] <- .mojor_ir_rng_resource(access = "RNG")
      }
      if (!is.null(node$args) && length(node$args) > 0) {
        for (arg in node$args) out <- c(out, .mojor_ir_expr_resources(arg))
      }
      .mojor_ir_unique_resources(out)
    },
    ifelse = .mojor_ir_unique_resources(c(
      .mojor_ir_expr_resources(node$cond),
      .mojor_ir_expr_resources(node$yes),
      .mojor_ir_expr_resources(node$no)
    )),
    scalar_index = .mojor_ir_expr_resources(node$expr),
    slice_index = .mojor_ir_unique_resources(c(.mojor_ir_expr_resources(node$start), .mojor_ir_expr_resources(node$end))),
    rep = .mojor_ir_unique_resources(c(
      .mojor_ir_expr_resources(node$x),
      if (!is.null(node$times)) .mojor_ir_expr_resources(node$times) else list(),
      if (!is.null(node$each)) .mojor_ir_expr_resources(node$each) else list(),
      if (!is.null(node$length_out)) .mojor_ir_expr_resources(node$length_out) else list()
    )),
    rep_len = .mojor_ir_unique_resources(c(.mojor_ir_expr_resources(node$x), .mojor_ir_expr_resources(node$length_out))),
    c = {
      out <- list()
      if (!is.null(node$parts) && length(node$parts) > 0) {
        for (part in node$parts) out <- c(out, .mojor_ir_expr_resources(part))
      }
      .mojor_ir_unique_resources(out)
    },
    range = .mojor_ir_unique_resources(c(
      .mojor_ir_expr_resources(node$start),
      .mojor_ir_expr_resources(node$end),
      if (!is.null(node$step)) .mojor_ir_expr_resources(node$step) else list()
    )),

 # rng_vec - RNG effect with resource
    rng_vec = {
      out <- list()
      if (!is.null(node$dist) && .mojor_ir_is_rng_call_fn(node$dist)) {
        out[[length(out) + 1L]] <- .mojor_ir_rng_resource(access = "RNG")
      }
 # n and params might have effects
      n_eff <- .mojor_ir_expr_resources(node$n)
      params_effs <- if (!is.null(node$params)) lapply(node$params, .mojor_ir_expr_resources) else list()
      .mojor_ir_unique_resources(c(out, n_eff, unlist(params_effs, recursive = FALSE)))
    },

 # alloc - pure (no resources, just allocation)
    alloc = .mojor_ir_expr_resources(node$len),

 # apply() - reads memory from input matrix
    apply = .mojor_ir_expr_resources(node$x),

 # / sample_int() - reads memory from n, size, replace, prob
    sample_int = .mojor_ir_unique_resources(c(
      .mojor_ir_expr_resources(node$n),
      .mojor_ir_expr_resources(node$size),
      if (!is.null(node$replace)) .mojor_ir_expr_resources(node$replace) else NULL,
      if (!is.null(node$prob)) .mojor_ir_expr_resources(node$prob) else NULL,
      list(.mojor_ir_rng_resource(access = "RNG"))
    )),

 # / sample() - reads memory from x, size, replace, prob
    sample = .mojor_ir_unique_resources(c(
      .mojor_ir_expr_resources(node$x),
      .mojor_ir_expr_resources(node$size),
      if (!is.null(node$replace)) .mojor_ir_expr_resources(node$replace) else NULL,
      if (!is.null(node$prob)) .mojor_ir_expr_resources(node$prob) else NULL,
      list(.mojor_ir_rng_resource(access = "RNG"))
    )),

 # Set/Match Primitives - read memory from input
    unique = .mojor_ir_expr_resources(node$x),
    duplicated = .mojor_ir_expr_resources(node$x),
    any_duplicated = .mojor_ir_expr_resources(node$x),
    match = .mojor_ir_unique_resources(c(
      .mojor_ir_expr_resources(node$x),
      .mojor_ir_expr_resources(node$table)
    )),
    `%in%` = .mojor_ir_unique_resources(c(
      .mojor_ir_expr_resources(node$x),
      .mojor_ir_expr_resources(node$table)
    )),

 # Quantiles & Robust Stats - read memory from input
    median = .mojor_ir_expr_resources(node$x),
    quantile = .mojor_ir_expr_resources(node$x),
    iqr = .mojor_ir_expr_resources(node$x),
    mad = .mojor_ir_expr_resources(node$x),

 # Higher-Order Functions - read memory from input(s)
    vapply = .mojor_ir_expr_resources(node$x),
    sapply = .mojor_ir_expr_resources(node$x),
    lapply = .mojor_ir_expr_resources(node$x),
    mapply = .mojor_ir_unique_resources(unlist(lapply(node$args, .mojor_ir_expr_resources), recursive = FALSE)),

 # String Basics - read memory from input(s)
    nchar = .mojor_ir_expr_resources(node$x),
    nzchar = .mojor_ir_expr_resources(node$x),
    substr = .mojor_ir_unique_resources(c(
      .mojor_ir_expr_resources(node$x),
      .mojor_ir_expr_resources(node$start),
      .mojor_ir_expr_resources(node$stop)
    )),
    paste = .mojor_ir_unique_resources(lapply(node$args, .mojor_ir_expr_resources)),
    gpu_reduce = .mojor_ir_unique_resources(c(
      .mojor_ir_expr_resources(node$arg),
      if (!is.null(node$dims)) .mojor_ir_expr_resources(node$dims) else list()
    )),
    gpu_matmul = .mojor_ir_unique_resources(c(
      .mojor_ir_expr_resources(node$a),
      .mojor_ir_expr_resources(node$b)
    )),
    list()
  )
}

.mojor_ir_expr_effect_resource_summary <- function(node) {
  effects <- .mojor_ir_expr_effects(node)
  resources <- .mojor_ir_expr_resources(node)
  list(
    effect_classes = .mojor_ir_map_effect_classes(effects),
    resources = resources
  )
}

.mojor_ir_cse_legal_expr <- function(expr) {
  # Index-descriptor nodes are structural and cannot be materialized as
  # standalone assignment RHS values by the emitter.
  if (!is.null(expr$kind) && expr$kind %in% c("scalar_index", "slice_index", "missing_index")) {
    return(FALSE)
  }
  info <- .mojor_ir_expr_effect_resource_summary(expr)
  identical(info$effect_classes, "None")
}

.mojor_ir_dce_legal_rhs <- function(expr) {
  info <- .mojor_ir_expr_effect_resource_summary(expr)
  identical(info$effect_classes, "None")
}

.mojor_ir_collect_written_resource_ids_stmt <- function(stmt, ids = character(0)) {
  if (is.null(stmt) || !is.list(stmt) || is.null(stmt$kind)) {
    return(ids)
  }
  kind <- if (exists(".mojor_ir_base_kind", mode = "function")) .mojor_ir_base_kind(stmt$kind) else stmt$kind

  if (identical(kind, "assign")) {
    lhs <- stmt$lhs
    if (!is.null(lhs) && is.list(lhs)) {
      if (identical(lhs$kind, "subscript") && !is.null(lhs$var)) {
        res <- .mojor_ir_resource_from_name(lhs$var, access = "Write")
        ids <- unique(c(ids, res$resource_id))
      } else if (identical(lhs$kind, "index") &&
        !is.null(lhs$base) && is.list(lhs$base) &&
        identical(lhs$base$kind, "var")) {
        res <- .mojor_ir_resource_from_name(lhs$base$name, access = "Write")
        ids <- unique(c(ids, res$resource_id))
      }
    }
    return(ids)
  }

  if (identical(kind, "if")) {
    ids <- .mojor_ir_collect_written_resource_ids_block(stmt$then, ids)
    if (!is.null(stmt$else_block)) ids <- .mojor_ir_collect_written_resource_ids_block(stmt$else_block, ids)
    return(ids)
  }

  if (kind %in% c("loop", "while", "repeat")) {
    return(.mojor_ir_collect_written_resource_ids_block(stmt$body, ids))
  }

  ids
}

.mojor_ir_collect_written_resource_ids_block <- function(block, ids = character(0)) {
  if (is.null(block) || !is.list(block) || !identical(block$kind, "block") ||
    is.null(block$stmts) || length(block$stmts) == 0) {
    return(unique(ids))
  }
  out <- ids
  for (stmt in block$stmts) {
    out <- .mojor_ir_collect_written_resource_ids_stmt(stmt, out)
  }
  unique(out)
}

.mojor_ir_licm_reads_legal <- function(expr, written_resource_ids = character(0)) {
  info <- .mojor_ir_expr_effect_resource_summary(expr)
  effects <- info$effect_classes
  if (!("Read" %in% effects)) {
    return(FALSE)
  }
  if (any(effects %in% c("Write", "RNG", "Unknown"))) {
    return(FALSE)
  }

  read_ids <- character(0)
  if (!is.null(info$resources) && length(info$resources) > 0) {
    for (res in info$resources) {
      if (is.null(res$access) || !identical(as.character(res$access), "Read")) next
      rid <- if (is.null(res$resource_id)) "unknown:missing" else as.character(res$resource_id)
      read_ids <- c(read_ids, rid)
      if (startsWith(rid, "unknown:")) {
        return(FALSE)
      }
    }
  }
  read_ids <- unique(read_ids)
  if (length(read_ids) == 0) {
    return(FALSE)
  }
  length(intersect(read_ids, as.character(written_resource_ids))) == 0
}

# =============================================================================
# CSE (Common Subexpression Elimination)
# =============================================================================

# Generate a canonical string key for an expression
.mojor_ir_expr_key <- function(expr) {
  if (is.null(expr) || !is.list(expr)) {
    return("")
  }

  k <- expr$kind
  if (is.null(k)) {
    return("")
  }

  switch(k,
    const = paste0("const:", expr$value),
    var = paste0("var:", expr$name),
    unop = paste0("unop:", expr$op, "(", .mojor_ir_expr_key(expr$expr), ")"),
    binop = paste0(
      "binop:", expr$op, "(",
      .mojor_ir_expr_key(expr$lhs), ",",
      .mojor_ir_expr_key(expr$rhs), ")"
    ),
    cast = paste0("cast:", expr$to, "(", .mojor_ir_expr_key(expr$expr), ")"),
    call = {
      arg_keys <- sapply(expr$args, .mojor_ir_expr_key)
      paste0("call:", expr$fn, "(", paste(arg_keys, collapse = ","), ")")
    },
    index = {
      idx_keys <- sapply(expr$indices, .mojor_ir_expr_key)
      paste0(
        "index:", .mojor_ir_expr_key(expr$base), "[",
        paste(idx_keys, collapse = ","), "]"
      )
    },
    ifelse = paste0(
      "ifelse(",
      .mojor_ir_expr_key(expr$cond), ",",
      .mojor_ir_expr_key(expr$yes), ",",
      .mojor_ir_expr_key(expr$no), ")"
    ),
 # Higher-Order Functions
    vapply = paste0(
      "vapply(",
      .mojor_ir_expr_key(expr$x), ",",
      "FUN,",
      .mojor_ir_expr_key(expr$fun_value_type), ")"
    ),
    sapply = paste0(
      "sapply(",
      .mojor_ir_expr_key(expr$x), ",",
      "FUN)"
    ),
    lapply = paste0(
      "lapply(",
      .mojor_ir_expr_key(expr$x), ",",
      "FUN)"
    ),
    mapply = paste0(
      "mapply(FUN,",
      paste(sapply(expr$args, .mojor_ir_expr_key), collapse = ","), ")"
    ),
 # String Basics
    nchar = paste0("nchar(", .mojor_ir_expr_key(expr$x), ")"),
    nzchar = paste0("nzchar(", .mojor_ir_expr_key(expr$x), ")"),
    substr = paste0(
      "substr(",
      .mojor_ir_expr_key(expr$x), ",",
      .mojor_ir_expr_key(expr$start), ",",
      .mojor_ir_expr_key(expr$stop), ")"
    ),
    paste = paste0(
      "paste(",
      paste(sapply(expr$args, .mojor_ir_expr_key), collapse = ","), ")"
    ),
    gpu_reduce = paste0(
      "gpu_reduce(",
      .mojor_ir_expr_key(expr$arg), ",",
      expr$op, ",",
      if (is.null(expr$dims)) "NULL" else .mojor_ir_expr_key(expr$dims), ",",
      if (isTRUE(expr$keepdims)) "TRUE" else "FALSE",
      ")"
    ),
    gpu_matmul = paste0(
      "gpu_matmul(",
      .mojor_ir_expr_key(expr$a), ",",
      .mojor_ir_expr_key(expr$b), ",",
      if (isTRUE(expr$transpose_a)) "TRUE" else "FALSE", ",",
      if (isTRUE(expr$transpose_b)) "TRUE" else "FALSE",
      ")"
    ),
 # Sampling & Permutation
    sample_int = paste0(
      "sample_int(",
      .mojor_ir_expr_key(expr$n), ",",
      .mojor_ir_expr_key(expr$size), ",",
      "replace=", .mojor_ir_expr_key(expr$replace), ",",
      "prob=", .mojor_ir_expr_key(expr$prob), ")"
    ),
    sample = paste0(
      "sample(",
      .mojor_ir_expr_key(expr$x), ",",
      .mojor_ir_expr_key(expr$size), ",",
      "replace=", .mojor_ir_expr_key(expr$replace), ",",
      "prob=", .mojor_ir_expr_key(expr$prob), ")"
    ),
 # Default: use deparse as fallback
    paste0(k, ":", paste(deparse(expr), collapse = ""))
  )
}

# Check if expression is trivial (const or var - not worth caching)
.mojor_ir_is_trivial <- function(expr) {
  if (is.null(expr) || !is.list(expr)) {
    return(TRUE)
  }
  expr$kind %in% c("const", "var")
}

# Collect all subexpressions from an expression tree
.mojor_ir_collect_subexprs <- function(expr, subexprs = list()) {
  if (is.null(expr) || !is.list(expr) || is.null(expr$kind)) {
    return(subexprs)
  }

 # Add current expression if non-trivial
  if (!.mojor_ir_is_trivial(expr)) {
    subexprs[[length(subexprs) + 1]] <- expr
  }

 # Recurse on children
  switch(expr$kind,
    unop = {
      subexprs <- .mojor_ir_collect_subexprs(expr$expr, subexprs)
    },
    binop = {
      subexprs <- .mojor_ir_collect_subexprs(expr$lhs, subexprs)
      subexprs <- .mojor_ir_collect_subexprs(expr$rhs, subexprs)
    },
    cast = {
      subexprs <- .mojor_ir_collect_subexprs(expr$expr, subexprs)
    },
    call = {
      for (arg in expr$args) {
        subexprs <- .mojor_ir_collect_subexprs(arg, subexprs)
      }
    },
    index = {
      subexprs <- .mojor_ir_collect_subexprs(expr$base, subexprs)
      for (idx in expr$indices) {
        subexprs <- .mojor_ir_collect_subexprs(idx, subexprs)
      }
    },
    ifelse = {
      subexprs <- .mojor_ir_collect_subexprs(expr$cond, subexprs)
      subexprs <- .mojor_ir_collect_subexprs(expr$yes, subexprs)
      subexprs <- .mojor_ir_collect_subexprs(expr$no, subexprs)
    },
 # Higher-Order Functions
    vapply = {
      subexprs <- .mojor_ir_collect_subexprs(expr$x, subexprs)
    },
    sapply = {
      subexprs <- .mojor_ir_collect_subexprs(expr$x, subexprs)
    },
    lapply = {
      subexprs <- .mojor_ir_collect_subexprs(expr$x, subexprs)
    },
    mapply = {
      for (arg in expr$args) {
        subexprs <- .mojor_ir_collect_subexprs(arg, subexprs)
      }
    },
 # String Basics
    nchar = {
      subexprs <- .mojor_ir_collect_subexprs(expr$x, subexprs)
    },
    nzchar = {
      subexprs <- .mojor_ir_collect_subexprs(expr$x, subexprs)
    },
    substr = {
      subexprs <- .mojor_ir_collect_subexprs(expr$x, subexprs)
      subexprs <- .mojor_ir_collect_subexprs(expr$start, subexprs)
      subexprs <- .mojor_ir_collect_subexprs(expr$stop, subexprs)
    },
    paste = {
      for (arg in expr$args) {
        subexprs <- .mojor_ir_collect_subexprs(arg, subexprs)
      }
    },
 # Sampling & Permutation
    sample_int = {
      subexprs <- .mojor_ir_collect_subexprs(expr$n, subexprs)
      subexprs <- .mojor_ir_collect_subexprs(expr$size, subexprs)
      if (!is.null(expr$replace)) subexprs <- .mojor_ir_collect_subexprs(expr$replace, subexprs)
      if (!is.null(expr$prob)) subexprs <- .mojor_ir_collect_subexprs(expr$prob, subexprs)
    },
    sample = {
      subexprs <- .mojor_ir_collect_subexprs(expr$x, subexprs)
      subexprs <- .mojor_ir_collect_subexprs(expr$size, subexprs)
      if (!is.null(expr$replace)) subexprs <- .mojor_ir_collect_subexprs(expr$replace, subexprs)
      if (!is.null(expr$prob)) subexprs <- .mojor_ir_collect_subexprs(expr$prob, subexprs)
    },
    gpu_reduce = {
      subexprs <- .mojor_ir_collect_subexprs(expr$arg, subexprs)
      if (!is.null(expr$dims)) subexprs <- .mojor_ir_collect_subexprs(expr$dims, subexprs)
    },
    gpu_matmul = {
      subexprs <- .mojor_ir_collect_subexprs(expr$a, subexprs)
      subexprs <- .mojor_ir_collect_subexprs(expr$b, subexprs)
    }
  )

  subexprs
}

# Replace an expression with a variable reference in an expression tree
.mojor_ir_replace_expr <- function(expr, old_expr_key, new_var) {
  if (is.null(expr) || !is.list(expr) || is.null(expr$kind)) {
    return(expr)
  }

 # Check if this expression matches
  if (.mojor_ir_expr_key(expr) == old_expr_key) {
    return(.mojor_ir_var(new_var))
  }

 # Recurse on children
  switch(expr$kind,
    unop = {
      expr$expr <- .mojor_ir_replace_expr(expr$expr, old_expr_key, new_var)
    },
    binop = {
      expr$lhs <- .mojor_ir_replace_expr(expr$lhs, old_expr_key, new_var)
      expr$rhs <- .mojor_ir_replace_expr(expr$rhs, old_expr_key, new_var)
    },
    cast = {
      expr$expr <- .mojor_ir_replace_expr(expr$expr, old_expr_key, new_var)
    },
    call = {
      for (i in seq_along(expr$args)) {
        expr$args[[i]] <- .mojor_ir_replace_expr(expr$args[[i]], old_expr_key, new_var)
      }
    },
    index = {
      expr$base <- .mojor_ir_replace_expr(expr$base, old_expr_key, new_var)
      for (i in seq_along(expr$indices)) {
        expr$indices[[i]] <- .mojor_ir_replace_expr(expr$indices[[i]], old_expr_key, new_var)
      }
    },
    ifelse = {
      expr$cond <- .mojor_ir_replace_expr(expr$cond, old_expr_key, new_var)
      expr$yes <- .mojor_ir_replace_expr(expr$yes, old_expr_key, new_var)
      expr$no <- .mojor_ir_replace_expr(expr$no, old_expr_key, new_var)
    },
 # Higher-Order Functions
    vapply = {
      expr$x <- .mojor_ir_replace_expr(expr$x, old_expr_key, new_var)
    },
    sapply = {
      expr$x <- .mojor_ir_replace_expr(expr$x, old_expr_key, new_var)
    },
    lapply = {
      expr$x <- .mojor_ir_replace_expr(expr$x, old_expr_key, new_var)
    },
    mapply = {
      for (i in seq_along(expr$args)) {
        expr$args[[i]] <- .mojor_ir_replace_expr(expr$args[[i]], old_expr_key, new_var)
      }
    },
 # String Basics
    nchar = {
      expr$x <- .mojor_ir_replace_expr(expr$x, old_expr_key, new_var)
    },
    nzchar = {
      expr$x <- .mojor_ir_replace_expr(expr$x, old_expr_key, new_var)
    },
    substr = {
      expr$x <- .mojor_ir_replace_expr(expr$x, old_expr_key, new_var)
      expr$start <- .mojor_ir_replace_expr(expr$start, old_expr_key, new_var)
      expr$stop <- .mojor_ir_replace_expr(expr$stop, old_expr_key, new_var)
    },
    paste = {
      for (i in seq_along(expr$args)) {
        expr$args[[i]] <- .mojor_ir_replace_expr(expr$args[[i]], old_expr_key, new_var)
      }
    },
 # Sampling & Permutation
    sample_int = {
      expr$n <- .mojor_ir_replace_expr(expr$n, old_expr_key, new_var)
      expr$size <- .mojor_ir_replace_expr(expr$size, old_expr_key, new_var)
      if (!is.null(expr$replace)) expr$replace <- .mojor_ir_replace_expr(expr$replace, old_expr_key, new_var)
      if (!is.null(expr$prob)) expr$prob <- .mojor_ir_replace_expr(expr$prob, old_expr_key, new_var)
    },
    sample = {
      expr$x <- .mojor_ir_replace_expr(expr$x, old_expr_key, new_var)
      expr$size <- .mojor_ir_replace_expr(expr$size, old_expr_key, new_var)
      if (!is.null(expr$replace)) expr$replace <- .mojor_ir_replace_expr(expr$replace, old_expr_key, new_var)
      if (!is.null(expr$prob)) expr$prob <- .mojor_ir_replace_expr(expr$prob, old_expr_key, new_var)
    },
    gpu_reduce = {
      expr$arg <- .mojor_ir_replace_expr(expr$arg, old_expr_key, new_var)
      if (!is.null(expr$dims)) expr$dims <- .mojor_ir_replace_expr(expr$dims, old_expr_key, new_var)
    },
    gpu_matmul = {
      expr$a <- .mojor_ir_replace_expr(expr$a, old_expr_key, new_var)
      expr$b <- .mojor_ir_replace_expr(expr$b, old_expr_key, new_var)
    }
  )

  expr
}

# CSE pass on a single statement
# Returns list(stmts = <list of statements>, temp_counter = <int>)
.mojor_ir_cse_stmt <- function(stmt, temp_counter = 0) {
  if (is.null(stmt) || !is.list(stmt) || is.null(stmt$kind)) {
    return(list(stmts = list(stmt), temp_counter = temp_counter))
  }

 # Handle different statement types
  if (stmt$kind == "assign") {
 # Collect all subexpressions from RHS
    subexprs <- .mojor_ir_collect_subexprs(stmt$rhs)

 # Stage 3: CSE legality is effect/resource based.
    pure_subexprs <- Filter(function(e) .mojor_ir_cse_legal_expr(e), subexprs)

    if (length(pure_subexprs) == 0) {
      return(list(stmts = list(stmt), temp_counter = temp_counter))
    }

 # Build frequency table by expression key
    keys <- sapply(pure_subexprs, .mojor_ir_expr_key)
    key_counts <- table(keys)

 # Find duplicates (appears more than once)
    dup_keys <- names(key_counts[key_counts > 1])

    if (length(dup_keys) == 0) {
      return(list(stmts = list(stmt), temp_counter = temp_counter))
    }

 # Extract duplicates to temp variables
    new_stmts <- list()

    for (dup_key in dup_keys) {
 # Find first occurrence
      idx <- which(keys == dup_key)[1]
      dup_expr <- pure_subexprs[[idx]]

 # Generate temp variable
      temp_counter <- temp_counter + 1
      temp_var <- sprintf("__cse_tmp%d", temp_counter)

 # Create assignment: temp_var <- dup_expr
      temp_assign <- .mojor_ir_assign(
        lhs = .mojor_ir_var(temp_var),
        rhs = dup_expr
      )
      new_stmts[[length(new_stmts) + 1]] <- temp_assign

 # Replace all occurrences in stmt$rhs
      stmt$rhs <- .mojor_ir_replace_expr(stmt$rhs, dup_key, temp_var)
    }

 # Return temp assignments followed by modified stmt
    return(list(stmts = c(new_stmts, list(stmt)), temp_counter = temp_counter))
  } else if (stmt$kind == "if") {
 # Recursively process branches
    then_result <- .mojor_ir_cse_block(stmt$then, temp_counter)
    stmt$then <- then_result$block
    temp_counter <- then_result$temp_counter

    if (!is.null(stmt$else_block)) {
      else_result <- .mojor_ir_cse_block(stmt$else_block, temp_counter)
      stmt$else_block <- else_result$block
      temp_counter <- else_result$temp_counter
    }

    return(list(stmts = list(stmt), temp_counter = temp_counter))
  } else if (stmt$kind == "loop") {
 # Recursively process loop body
    body_result <- .mojor_ir_cse_block(stmt$body, temp_counter)
    stmt$body <- body_result$block
    temp_counter <- body_result$temp_counter

    return(list(stmts = list(stmt), temp_counter = temp_counter))
  } else if (stmt$kind == "while" || stmt$kind == "repeat") {
 # Recursively process loop body
    body_result <- .mojor_ir_cse_block(stmt$body, temp_counter)
    stmt$body <- body_result$block
    temp_counter <- body_result$temp_counter

    return(list(stmts = list(stmt), temp_counter = temp_counter))
  } else {
 # Other statement types pass through unchanged
    return(list(stmts = list(stmt), temp_counter = temp_counter))
  }
}

# CSE pass on a block
# Returns list(block = <block>, temp_counter = <int>)
.mojor_ir_cse_block <- function(block, temp_counter = 0) {
  if (is.null(block) || !is.list(block) || block$kind != "block") {
    return(list(block = block, temp_counter = temp_counter))
  }

  new_stmts <- list()

  for (stmt in block$stmts) {
    result <- .mojor_ir_cse_stmt(stmt, temp_counter)
    new_stmts <- c(new_stmts, result$stmts)
    temp_counter <- result$temp_counter
  }

  list(block = .mojor_ir_block(new_stmts), temp_counter = temp_counter)
}

# =============================================================================
# LICM (Loop-Invariant Code Motion)
# =============================================================================

# Check if an expression references a specific variable
.mojor_ir_references_var <- function(expr, var_name) {
  if (is.null(expr) || !is.list(expr) || is.null(expr$kind)) {
    return(FALSE)
  }

  switch(expr$kind,
    var = {
      expr$name == var_name
    },
    unop = {
      .mojor_ir_references_var(expr$expr, var_name)
    },
    binop = {
      .mojor_ir_references_var(expr$lhs, var_name) ||
        .mojor_ir_references_var(expr$rhs, var_name)
    },
    cast = {
      .mojor_ir_references_var(expr$expr, var_name)
    },
    call = {
      any(sapply(expr$args, function(arg) .mojor_ir_references_var(arg, var_name)))
    },
    index = {
      .mojor_ir_references_var(expr$base, var_name) ||
        any(sapply(expr$indices, function(idx) .mojor_ir_references_var(idx, var_name)))
    },
    ifelse = {
      .mojor_ir_references_var(expr$cond, var_name) ||
        .mojor_ir_references_var(expr$yes, var_name) ||
        .mojor_ir_references_var(expr$no, var_name)
    },
    scalar_index = {
      .mojor_ir_references_var(expr$expr, var_name)
    },
    slice_index = {
      .mojor_ir_references_var(expr$start, var_name) ||
        .mojor_ir_references_var(expr$end, var_name)
    },
    rep = {
      .mojor_ir_references_var(expr$x, var_name) ||
        (!is.null(expr$times) && .mojor_ir_references_var(expr$times, var_name)) ||
        (!is.null(expr$each) && .mojor_ir_references_var(expr$each, var_name)) ||
        (!is.null(expr$length_out) && .mojor_ir_references_var(expr$length_out, var_name))
    },
    rep_len = {
      .mojor_ir_references_var(expr$x, var_name) ||
        .mojor_ir_references_var(expr$length_out, var_name)
    },
    c = {
      any(sapply(expr$parts, function(part) .mojor_ir_references_var(part, var_name)))
    },
    range = {
      .mojor_ir_references_var(expr$start, var_name) ||
        .mojor_ir_references_var(expr$end, var_name) ||
        (!is.null(expr$step) && .mojor_ir_references_var(expr$step, var_name))
    },
 # Higher-Order Functions
    vapply = {
      .mojor_ir_references_var(expr$x, var_name)
    },
    sapply = {
      .mojor_ir_references_var(expr$x, var_name)
    },
    lapply = {
      .mojor_ir_references_var(expr$x, var_name)
    },
    mapply = {
      any(sapply(expr$args, function(arg) .mojor_ir_references_var(arg, var_name)))
    },
 # String Basics
    nchar = {
      .mojor_ir_references_var(expr$x, var_name)
    },
    nzchar = {
      .mojor_ir_references_var(expr$x, var_name)
    },
    substr = {
      .mojor_ir_references_var(expr$x, var_name) ||
        .mojor_ir_references_var(expr$start, var_name) ||
        .mojor_ir_references_var(expr$stop, var_name)
    },
    paste = {
      any(sapply(expr$args, function(arg) .mojor_ir_references_var(arg, var_name)))
    },
 # Sampling & Permutation
    sample_int = {
      .mojor_ir_references_var(expr$n, var_name) ||
        .mojor_ir_references_var(expr$size, var_name) ||
        (!is.null(expr$replace) && .mojor_ir_references_var(expr$replace, var_name)) ||
        (!is.null(expr$prob) && .mojor_ir_references_var(expr$prob, var_name))
    },
    sample = {
      .mojor_ir_references_var(expr$x, var_name) ||
        .mojor_ir_references_var(expr$size, var_name) ||
        (!is.null(expr$replace) && .mojor_ir_references_var(expr$replace, var_name)) ||
        (!is.null(expr$prob) && .mojor_ir_references_var(expr$prob, var_name))
    },
    gpu_reduce = {
      .mojor_ir_references_var(expr$arg, var_name) ||
        (!is.null(expr$dims) && .mojor_ir_references_var(expr$dims, var_name))
    },
    gpu_matmul = {
      .mojor_ir_references_var(expr$a, var_name) ||
        .mojor_ir_references_var(expr$b, var_name)
    },
 # Default: assume no reference
    FALSE
  )
}

# Check if expression is loop-invariant (doesn't reference loop var and is pure/ReadsMem)
.mojor_ir_is_loop_invariant <- function(expr, loop_var, allow_reads = FALSE, written_resource_ids = character(0)) {
  if (is.null(expr) || !is.list(expr)) {
    return(FALSE)
  }

 # Check if expression references loop variable
  if (.mojor_ir_references_var(expr, loop_var)) {
    return(FALSE)
  }

 # Check effects
  effects <- .mojor_ir_expr_effects(expr)

 # Pure expressions are always safe to hoist
  if (.mojor_ir_is_pure(expr)) {
    return(TRUE)
  }

 # ReadsMem expressions can be hoisted if allow_reads = TRUE
 # (requires alias analysis to ensure no writes in loop)
  if (allow_reads && .mojor_ir_licm_reads_legal(expr, written_resource_ids = written_resource_ids)) {
    return(TRUE)
  }

 # Everything else is not loop-invariant
  FALSE
}

# Collect loop-invariant expressions from a statement
.mojor_ir_collect_loop_invariants <- function(stmt, loop_var, subexprs = list(), allow_reads = FALSE, written_resource_ids = character(0)) {
  if (is.null(stmt) || !is.list(stmt) || is.null(stmt$kind)) {
    return(subexprs)
  }

  if (stmt$kind == "assign") {
 # Collect from RHS
    rhs_subexprs <- .mojor_ir_collect_subexprs(stmt$rhs)

 # Filter to loop-invariant non-trivial expressions
    for (expr in rhs_subexprs) {
      if (!.mojor_ir_is_trivial(expr) &&
        .mojor_ir_is_loop_invariant(expr, loop_var, allow_reads, written_resource_ids = written_resource_ids)) {
        subexprs[[length(subexprs) + 1]] <- expr
      }
    }
  }

  subexprs
}

# LICM pass on a loop node
# Returns list(stmts = <hoisted stmts>, loop = <transformed loop>, temp_counter = <int>)
.mojor_ir_licm_loop <- function(loop, temp_counter = 0, allow_reads = FALSE) {
  if (is.null(loop) || !is.list(loop) || loop$kind != "loop") {
    return(list(stmts = list(), loop = loop, temp_counter = temp_counter))
  }

  written_resource_ids <- .mojor_ir_collect_written_resource_ids_block(loop$body)

 # Collect all loop-invariant subexpressions from loop body
  all_invariants <- list()
  for (stmt in loop$body$stmts) {
    stmt_invariants <- .mojor_ir_collect_loop_invariants(
      stmt,
      loop$var,
      list(),
      allow_reads,
      written_resource_ids = written_resource_ids
    )
    all_invariants <- c(all_invariants, stmt_invariants)
  }

  if (length(all_invariants) == 0) {
    return(list(stmts = list(), loop = loop, temp_counter = temp_counter))
  }

 # Build frequency table by expression key
  keys <- sapply(all_invariants, .mojor_ir_expr_key)
  key_counts <- table(keys)

 # Find invariants that appear at least once (all are candidates for hoisting)
  hoist_keys <- names(key_counts[key_counts > 0])

  if (length(hoist_keys) == 0) {
    return(list(stmts = list(), loop = loop, temp_counter = temp_counter))
  }

 # Hoist invariants to temp variables before the loop
  hoisted_stmts <- list()
  key_to_var <- list()

  for (hoist_key in hoist_keys) {
 # Find first occurrence
    idx <- which(keys == hoist_key)[1]
    invariant_expr <- all_invariants[[idx]]

 # Generate temp variable
    temp_counter <- temp_counter + 1
    temp_var <- sprintf("__licm_tmp%d", temp_counter)

 # Create assignment: temp_var <- invariant_expr
    temp_assign <- .mojor_ir_assign(
      lhs = .mojor_ir_var(temp_var),
      rhs = invariant_expr
    )
    hoisted_stmts[[length(hoisted_stmts) + 1]] <- temp_assign

 # Record mapping
    key_to_var[[hoist_key]] <- temp_var
  }

 # Replace invariants in loop body with temp variables
  new_body_stmts <- list()
  for (stmt in loop$body$stmts) {
 # Replace each hoisted expression with its temp variable
    new_stmt <- stmt
    for (hoist_key in hoist_keys) {
      if (hoist_key %in% names(key_to_var)) {
        new_stmt$rhs <- .mojor_ir_replace_expr(new_stmt$rhs, hoist_key, key_to_var[[hoist_key]])
      }
    }
    new_body_stmts[[length(new_body_stmts) + 1]] <- new_stmt
  }

 # Update loop body
  loop$body <- .mojor_ir_block(new_body_stmts)

  list(stmts = hoisted_stmts, loop = loop, temp_counter = temp_counter)
}

# LICM pass on a block
# Returns list(block = <block>, temp_counter = <int>)
.mojor_ir_licm_block <- function(block, temp_counter = 0, allow_reads = FALSE) {
  if (is.null(block) || !is.list(block) || block$kind != "block") {
    return(list(block = block, temp_counter = temp_counter))
  }

  new_stmts <- list()

  for (stmt in block$stmts) {
    if (!is.null(stmt$kind) && stmt$kind == "loop") {
 # Apply LICM to this loop
      result <- .mojor_ir_licm_loop(stmt, temp_counter, allow_reads)

 # Add hoisted statements before loop
      new_stmts <- c(new_stmts, result$stmts)
 # Add transformed loop
      new_stmts[[length(new_stmts) + 1]] <- result$loop

      temp_counter <- result$temp_counter
    } else if (!is.null(stmt$kind) && stmt$kind == "if") {
 # Recursively process branches
      then_result <- .mojor_ir_licm_block(stmt$then, temp_counter, allow_reads)
      stmt$then <- then_result$block
      temp_counter <- then_result$temp_counter

      if (!is.null(stmt$else_block)) {
        else_result <- .mojor_ir_licm_block(stmt$else_block, temp_counter, allow_reads)
        stmt$else_block <- else_result$block
        temp_counter <- else_result$temp_counter
      }

      new_stmts[[length(new_stmts) + 1]] <- stmt
    } else if (!is.null(stmt$kind) && (stmt$kind == "while" || stmt$kind == "repeat")) {
 # While/repeat loops: apply LICM to body
      body_result <- .mojor_ir_licm_block(stmt$body, temp_counter, allow_reads)
      stmt$body <- body_result$block
      temp_counter <- body_result$temp_counter

      new_stmts[[length(new_stmts) + 1]] <- stmt
    } else {
 # Other statements pass through
      new_stmts[[length(new_stmts) + 1]] <- stmt
    }
  }

  list(block = .mojor_ir_block(new_stmts), temp_counter = temp_counter)
}

# =============================================================================
# Constant Folding
# =============================================================================

# Check if expression has only constant operands
.mojor_ir_all_const <- function(expr) {
  if (is.null(expr) || !is.list(expr) || is.null(expr$kind)) {
    return(FALSE)
  }

  switch(expr$kind,
    const = TRUE,
    var = FALSE,
    unop = .mojor_ir_all_const(expr$expr),
    binop = .mojor_ir_all_const(expr$lhs) && .mojor_ir_all_const(expr$rhs),
    cast = .mojor_ir_all_const(expr$expr),
    call = all(sapply(expr$args, .mojor_ir_all_const)),
    ifelse = .mojor_ir_all_const(expr$cond) && .mojor_ir_all_const(expr$yes) && .mojor_ir_all_const(expr$no),
 # Sampling & Permutation - not all const (uses RNG)
    sample_int = FALSE,
    sample = FALSE,
 # Default: not all const
    FALSE
  )
}

# Evaluate a constant expression at compile time
.mojor_ir_value_to_const <- function(val) {
  if (is.logical(val)) {
    .mojor_ir_const(if (val) "TRUE" else "FALSE")
  } else if (is.numeric(val)) {
    if (is.infinite(val)) {
      .mojor_ir_const(if (val > 0) "Inf" else "-Inf")
    } else if (is.nan(val)) {
      .mojor_ir_const("NaN")
    } else {
      .mojor_ir_const(as.character(val))
    }
  } else {
    .mojor_ir_const(as.character(val))
  }
}

# Constant folding on an expression
.mojor_ir_fold_expr <- function(expr) {
  if (is.null(expr) || !is.list(expr) || is.null(expr$kind)) {
    return(expr)
  }
  if (identical(expr$kind, "const")) {
 # Preserve literal spelling (e.g. "0.0" vs "0") so downstream
 # type inference keeps the original scalar intent.
    return(expr)
  }

 # First, recursively fold subexpressions
  expr <- switch(expr$kind,
    unop = {
      expr$expr <- .mojor_ir_fold_expr(expr$expr)
      expr
    },
    binop = {
      expr$lhs <- .mojor_ir_fold_expr(expr$lhs)
      expr$rhs <- .mojor_ir_fold_expr(expr$rhs)
      expr
    },
    cast = {
      expr$expr <- .mojor_ir_fold_expr(expr$expr)
      expr
    },
    call = {
      expr$args <- lapply(expr$args, .mojor_ir_fold_expr)
      expr
    },
    ifelse = {
      expr$cond <- .mojor_ir_fold_expr(expr$cond)
      expr$yes <- .mojor_ir_fold_expr(expr$yes)
      expr$no <- .mojor_ir_fold_expr(expr$no)
      expr
    },
    index = {
      expr$base <- .mojor_ir_fold_expr(expr$base)
      expr$indices <- lapply(expr$indices, .mojor_ir_fold_expr)
      expr
    },
 # Sampling & Permutation - pass through (uses RNG, not foldable)
    sample_int = {
      expr$n <- .mojor_ir_fold_expr(expr$n)
      expr$size <- .mojor_ir_fold_expr(expr$size)
      if (!is.null(expr$replace)) expr$replace <- .mojor_ir_fold_expr(expr$replace)
      if (!is.null(expr$prob)) expr$prob <- .mojor_ir_fold_expr(expr$prob)
      expr
    },
    sample = {
      expr$x <- .mojor_ir_fold_expr(expr$x)
      expr$size <- .mojor_ir_fold_expr(expr$size)
      if (!is.null(expr$replace)) expr$replace <- .mojor_ir_fold_expr(expr$replace)
      if (!is.null(expr$prob)) expr$prob <- .mojor_ir_fold_expr(expr$prob)
      expr
    },
 # Other node types pass through
    expr
  )

 # Now check if expression is pure with all constant operands
  if (.mojor_ir_is_pure(expr) && .mojor_ir_all_const(expr)) {
 # Try to evaluate at compile time
    val <- .mojor_ir_eval_const(expr)
    if (!is.null(val)) {
 # Replace with constant
      return(.mojor_ir_value_to_const(val))
    }
  }

  expr
}

# Constant folding on a statement
.mojor_ir_fold_stmt <- function(stmt) {
  if (is.null(stmt) || !is.list(stmt) || is.null(stmt$kind)) {
    return(stmt)
  }

  switch(stmt$kind,
    assign = {
      stmt$rhs <- .mojor_ir_fold_expr(stmt$rhs)
 # Also fold LHS if it's an index expression
      if (stmt$lhs$kind == "index") {
        stmt$lhs$indices <- lapply(stmt$lhs$indices, .mojor_ir_fold_expr)
      }
      stmt
    },
    `if` = {
      stmt$cond <- .mojor_ir_fold_expr(stmt$cond)
      stmt$then <- .mojor_ir_fold_block(stmt$then)
      if (!is.null(stmt$else_block)) {
        stmt$else_block <- .mojor_ir_fold_block(stmt$else_block)
      }
      stmt
    },
    loop = {
 # Fold range expressions
      if (!is.null(stmt$range) && stmt$range$kind == "range") {
        stmt$range$start <- .mojor_ir_fold_expr(stmt$range$start)
        stmt$range$end <- .mojor_ir_fold_expr(stmt$range$end)
        if (!is.null(stmt$range$step)) {
          stmt$range$step <- .mojor_ir_fold_expr(stmt$range$step)
        }
      }
      stmt$body <- .mojor_ir_fold_block(stmt$body)
      stmt
    },
    `while` = {
      stmt$cond <- .mojor_ir_fold_expr(stmt$cond)
      stmt$body <- .mojor_ir_fold_block(stmt$body)
      stmt
    },
    `repeat` = {
      stmt$body <- .mojor_ir_fold_block(stmt$body)
      stmt
    },
    `return` = {
      if (!is.null(stmt$value)) {
        stmt$value <- .mojor_ir_fold_expr(stmt$value)
      }
      stmt
    },
 # Other statement types pass through
    stmt
  )
}

# Constant folding on a block
.mojor_ir_fold_block <- function(block) {
  if (is.null(block) || !is.list(block) || block$kind != "block") {
    return(block)
  }

  block$stmts <- lapply(block$stmts, .mojor_ir_fold_stmt)
  block
}

# =============================================================================
# Type-Based Constant Folding ()
# =============================================================================

# Helper: Parse type annotation to extract kind and element type
.mojor_parse_type_annotation <- function(type_str) {
 # Returns list(kind = "scalar"|"vector"|"matrix"|"array", elem_type = "f64"|"i32"|"logical")
  if (is.null(type_str) || !is.character(type_str) || length(type_str) != 1) {
    return(NULL)
  }

 # Scalar types: "f64", "i32", "logical"
  if (type_str %in% c("f64", "i32", "logical")) {
    return(list(kind = "scalar", elem_type = type_str))
  }

 # Vector types: "f64[]", "f64[1d]", "i32[]", "logical[]", etc.
  if (grepl("^(f64|i32|logical)(\\[\\]|\\[1d\\])$", type_str)) {
    elem_type <- sub("(\\[\\]|\\[1d\\])$", "", type_str)
    return(list(kind = "vector", elem_type = elem_type))
  }

 # Matrix types: "f64[,]", "i32[,]", "logical[,]"
  if (grepl("^(f64|i32|logical)\\[,\\]$", type_str)) {
    elem_type <- sub("\\[,\\]$", "", type_str)
    return(list(kind = "matrix", elem_type = elem_type))
  }

 # Array types: "f64[,,]", "i32[,,]", etc. (3+ dimensions)
  if (grepl("^(f64|i32|logical)\\[[,]+\\]$", type_str)) {
    elem_type <- sub("\\[[,]+\\]$", "", type_str)
    ndim <- nchar(gsub("[^,]", "", type_str)) + 1
    return(list(kind = "array", elem_type = elem_type, ndim = ndim))
  }

  NULL
}

# Helper: Evaluate type predicate for a given type
.mojor_eval_type_predicate <- function(predicate, type_info) {
  if (is.null(type_info)) {
    return(NULL)
  }

  switch(predicate,
    "is.vector" = type_info$kind == "vector",
    "is.matrix" = type_info$kind == "matrix",
    "is.array" = type_info$kind %in% c("array", "matrix"), # R considers matrices as 2D arrays
    "is.numeric" = type_info$elem_type %in% c("f64", "i32"),
    "is.integer" = type_info$elem_type == "i32",
    "is.double" = type_info$elem_type == "f64",
    "is.logical" = type_info$elem_type == "logical",
    NULL
  )
}

# Helper: Evaluate type query for a given type
.mojor_eval_type_query <- function(query, type_info) {
  if (is.null(type_info)) {
    return(NULL)
  }

  switch(query,
    "typeof" = switch(type_info$elem_type,
      "f64" = "double",
      "i32" = "integer",
      "logical" = "logical",
      NULL
    ),
    "mode" = switch(type_info$elem_type,
      "f64" = "numeric",
      "i32" = "numeric",
      "logical" = "logical",
      NULL
    ),
    "class" = switch(type_info$kind,
      "scalar" = switch(type_info$elem_type,
        "f64" = "numeric",
        "i32" = "integer",
        "logical" = "logical",
        NULL
      ),
      "vector" = switch(type_info$elem_type,
        "f64" = "numeric",
        "i32" = "integer",
        "logical" = "logical",
        NULL
      ),
      "matrix" = "matrix",
      "array" = "array",
      NULL
    ),
    NULL
  )
}

# Type-based constant folding for expressions
.mojor_ir_fold_type_expr <- function(expr, type_map) {
  if (is.null(expr) || !is.list(expr) || is.null(expr$kind)) {
    return(expr)
  }

 # First, recursively fold subexpressions
  expr <- switch(expr$kind,
    unop = {
      expr$expr <- .mojor_ir_fold_type_expr(expr$expr, type_map)
      expr
    },
    binop = {
      expr$lhs <- .mojor_ir_fold_type_expr(expr$lhs, type_map)
      expr$rhs <- .mojor_ir_fold_type_expr(expr$rhs, type_map)
      expr
    },
    cast = {
      expr$expr <- .mojor_ir_fold_type_expr(expr$expr, type_map)
      expr
    },
    call = {
      expr$args <- lapply(expr$args, function(a) .mojor_ir_fold_type_expr(a, type_map))
      expr
    },
    ifelse = {
      expr$cond <- .mojor_ir_fold_type_expr(expr$cond, type_map)
      expr$yes <- .mojor_ir_fold_type_expr(expr$yes, type_map)
      expr$no <- .mojor_ir_fold_type_expr(expr$no, type_map)
      expr
    },
    index = {
      expr$base <- .mojor_ir_fold_type_expr(expr$base, type_map)
      expr$indices <- lapply(expr$indices, function(i) .mojor_ir_fold_type_expr(i, type_map))
      expr
    },
    type_predicate = {
 # Check if argument is a simple variable
      if (!is.null(expr$x) && is.list(expr$x) && expr$x$kind == "var") {
        var_name <- expr$x$name
        if (!is.null(type_map[[var_name]])) {
          type_info <- .mojor_parse_type_annotation(type_map[[var_name]])
          result <- .mojor_eval_type_predicate(expr$predicate, type_info)
          if (!is.null(result)) {
 # Replace with constant TRUE or FALSE
            return(.mojor_ir_const(if (result) "True" else "False"))
          }
        }
      }
 # Can't fold, return as-is (will error in emission if not folded)
      expr
    },
    type_query = {
 # Check if argument is a simple variable
      if (!is.null(expr$x) && is.list(expr$x) && expr$x$kind == "var") {
        var_name <- expr$x$name
        if (!is.null(type_map[[var_name]])) {
          type_info <- .mojor_parse_type_annotation(type_map[[var_name]])
          result <- .mojor_eval_type_query(expr$query, type_info)
          if (!is.null(result) && is.character(result)) {
 # Replace with constant string (as a Mojo string literal)
            return(.mojor_ir_const(paste0('"', result, '"')))
          }
        }
      }
 # Can't fold, return as-is (will error in emission if not folded)
      expr
    },
 # Sampling & Permutation - pass through (uses RNG, not foldable)
    sample_int = {
      expr$n <- .mojor_ir_fold_type_expr(expr$n, type_map)
      expr$size <- .mojor_ir_fold_type_expr(expr$size, type_map)
      if (!is.null(expr$replace)) expr$replace <- .mojor_ir_fold_type_expr(expr$replace, type_map)
      if (!is.null(expr$prob)) expr$prob <- .mojor_ir_fold_type_expr(expr$prob, type_map)
      expr
    },
    sample = {
      expr$x <- .mojor_ir_fold_type_expr(expr$x, type_map)
      expr$size <- .mojor_ir_fold_type_expr(expr$size, type_map)
      if (!is.null(expr$replace)) expr$replace <- .mojor_ir_fold_type_expr(expr$replace, type_map)
      if (!is.null(expr$prob)) expr$prob <- .mojor_ir_fold_type_expr(expr$prob, type_map)
      expr
    },
 # Other node types pass through
    expr
  )

  expr
}

# Type-based constant folding for statements
.mojor_ir_fold_type_stmt <- function(stmt, type_map) {
  if (is.null(stmt) || !is.list(stmt) || is.null(stmt$kind)) {
    return(stmt)
  }

  switch(stmt$kind,
    assign = {
      stmt$rhs <- .mojor_ir_fold_type_expr(stmt$rhs, type_map)
 # Also fold LHS if it's an index expression
      if (stmt$lhs$kind == "index") {
        stmt$lhs$indices <- lapply(stmt$lhs$indices, function(i) .mojor_ir_fold_type_expr(i, type_map))
      }
      stmt
    },
    `if` = {
      stmt$cond <- .mojor_ir_fold_type_expr(stmt$cond, type_map)
      stmt$then <- .mojor_ir_fold_type_block(stmt$then, type_map)
      if (!is.null(stmt$else_block)) {
        stmt$else_block <- .mojor_ir_fold_type_block(stmt$else_block, type_map)
      }
      stmt
    },
    loop = {
 # Fold range expressions
      if (!is.null(stmt$range) && stmt$range$kind == "range") {
        stmt$range$start <- .mojor_ir_fold_type_expr(stmt$range$start, type_map)
        stmt$range$end <- .mojor_ir_fold_type_expr(stmt$range$end, type_map)
        if (!is.null(stmt$range$step)) {
          stmt$range$step <- .mojor_ir_fold_type_expr(stmt$range$step, type_map)
        }
      }
      stmt$body <- .mojor_ir_fold_type_block(stmt$body, type_map)
      stmt
    },
    `while` = {
      stmt$cond <- .mojor_ir_fold_type_expr(stmt$cond, type_map)
      stmt$body <- .mojor_ir_fold_type_block(stmt$body, type_map)
      stmt
    },
    `repeat` = {
      stmt$body <- .mojor_ir_fold_type_block(stmt$body, type_map)
      stmt
    },
    `return` = {
      if (!is.null(stmt$value)) {
        stmt$value <- .mojor_ir_fold_type_expr(stmt$value, type_map)
      }
      stmt
    },
 # Other statement types pass through
    stmt
  )
}

# Type-based constant folding for blocks
.mojor_ir_fold_type_block <- function(block, type_map) {
  if (is.null(block) || !is.list(block) || block$kind != "block") {
    return(block)
  }

  block$stmts <- lapply(block$stmts, function(s) .mojor_ir_fold_type_stmt(s, type_map))
  block
}

# =============================================================================
# Dimension Query Lowering (dim() to dimension variables)
# =============================================================================
# Lowers dim(x)[i] patterns to direct dimension variable references.
# This avoids allocating integer vectors and provides zero-overhead access.
#
# dim_map structure:
# list(
# var_name = list(
# kind = "matrix"|"array",
# nrow = "nrow_var", # for matrices
# ncol = "ncol_var", # for matrices
# dim = "dim_var", # for arrays
# ndim = "ndim_var" # for arrays
# )
# )

.mojor_ir_lower_dim_expr <- function(expr, dim_map) {
  if (is.null(expr) || !is.list(expr) || is.null(expr$kind)) {
    return(expr)
  }

 # First, recursively lower subexpressions
  expr <- switch(expr$kind,
    unop = {
      expr$expr <- .mojor_ir_lower_dim_expr(expr$expr, dim_map)
      expr
    },
    binop = {
      expr$lhs <- .mojor_ir_lower_dim_expr(expr$lhs, dim_map)
      expr$rhs <- .mojor_ir_lower_dim_expr(expr$rhs, dim_map)
      expr
    },
    cast = {
      expr$expr <- .mojor_ir_lower_dim_expr(expr$expr, dim_map)
      expr
    },
    call = {
      expr$args <- lapply(expr$args, function(a) .mojor_ir_lower_dim_expr(a, dim_map))
      expr
    },
    ifelse = {
      expr$cond <- .mojor_ir_lower_dim_expr(expr$cond, dim_map)
      expr$yes <- .mojor_ir_lower_dim_expr(expr$yes, dim_map)
      expr$no <- .mojor_ir_lower_dim_expr(expr$no, dim_map)
      expr
    },
    index = {
 # Check for dim(x)[i] pattern
      if (!is.null(expr$base) && is.list(expr$base) &&
        expr$base$kind == "dim" &&
        !is.null(expr$base$x) && is.list(expr$base$x) &&
        expr$base$x$kind == "var") {
        var_name <- expr$base$x$name
        dim_info <- dim_map[[var_name]]

        if (!is.null(dim_info) && length(expr$indices) == 1) {
          idx_expr <- expr$indices[[1]]

 # Check if index is a constant
          if (idx_expr$kind == "const") {
            dim_idx <- suppressWarnings(as.integer(idx_expr$value))
            if (!is.na(dim_idx) && dim_idx >= 1) {
 # Replace with dimension variable reference
              if (dim_info$kind == "matrix") {
                if (dim_idx == 1 && !is.null(dim_info$nrow)) {
 # dim(x)[1] -> cast(Int32, nrow_x)
                  return(.mojor_ir_cast("Int32", .mojor_ir_var(dim_info$nrow)))
                } else if (dim_idx == 2 && !is.null(dim_info$ncol)) {
 # dim(x)[2] -> cast(Int32, ncol_x)
                  return(.mojor_ir_cast("Int32", .mojor_ir_var(dim_info$ncol)))
                }
              } else if (dim_info$kind == "array" && !is.null(dim_info$dim)) {
 # dim(x)[i] -> cast(Int32, dim_x[i-1])
                zero_idx <- .mojor_ir_const(as.character(dim_idx - 1L))
                dim_access <- .mojor_ir_index(
                  .mojor_ir_var(dim_info$dim),
                  list(zero_idx),
                  index_base = "zero_based"
                )
                return(.mojor_ir_cast("Int32", dim_access))
              }
            }
          }
        }
      }

 # Not a lowerable pattern, recursively lower subexpressions
      expr$base <- .mojor_ir_lower_dim_expr(expr$base, dim_map)
      expr$indices <- lapply(expr$indices, function(i) .mojor_ir_lower_dim_expr(i, dim_map))
      expr
    },
 # Sampling & Permutation - pass through (no dim lowering needed)
    sample_int = {
      expr$n <- .mojor_ir_lower_dim_expr(expr$n, dim_map)
      expr$size <- .mojor_ir_lower_dim_expr(expr$size, dim_map)
      if (!is.null(expr$replace)) expr$replace <- .mojor_ir_lower_dim_expr(expr$replace, dim_map)
      if (!is.null(expr$prob)) expr$prob <- .mojor_ir_lower_dim_expr(expr$prob, dim_map)
      expr
    },
    sample = {
      expr$x <- .mojor_ir_lower_dim_expr(expr$x, dim_map)
      expr$size <- .mojor_ir_lower_dim_expr(expr$size, dim_map)
      if (!is.null(expr$replace)) expr$replace <- .mojor_ir_lower_dim_expr(expr$replace, dim_map)
      if (!is.null(expr$prob)) expr$prob <- .mojor_ir_lower_dim_expr(expr$prob, dim_map)
      expr
    },
 # Pass through other expression types
    expr
  )

 # After lowering subexpressions, check for length(dim(x)) pattern
  if (expr$kind == "call" && expr$fn == "length" && length(expr$args) == 1) {
    arg <- expr$args[[1]]
    if (!is.null(arg) && is.list(arg) && arg$kind == "dim" &&
      !is.null(arg$x) && is.list(arg$x) && arg$x$kind == "var") {
      var_name <- arg$x$name
      dim_info <- dim_map[[var_name]]

      if (!is.null(dim_info)) {
 # length(dim(x)) -> constant 2 for matrices, ndim for arrays
        if (dim_info$kind == "matrix") {
          return(.mojor_ir_const("2"))
        } else if (dim_info$kind == "array" && !is.null(dim_info$ndim)) {
          return(.mojor_ir_var(dim_info$ndim))
        }
      }
    }
  }

  expr
}

.mojor_ir_lower_dim_stmt <- function(stmt, dim_map) {
  if (is.null(stmt) || !is.list(stmt) || is.null(stmt$kind)) {
    return(stmt)
  }

  switch(stmt$kind,
    assign = {
      stmt$rhs <- .mojor_ir_lower_dim_expr(stmt$rhs, dim_map)
 # Also lower LHS if it's an index expression
      if (stmt$lhs$kind == "index") {
        stmt$lhs$indices <- lapply(stmt$lhs$indices, function(i) .mojor_ir_lower_dim_expr(i, dim_map))
      }
      stmt
    },
    `if` = {
      stmt$cond <- .mojor_ir_lower_dim_expr(stmt$cond, dim_map)
      stmt$then <- .mojor_ir_lower_dim_block(stmt$then, dim_map)
      if (!is.null(stmt$else_block)) {
        stmt$else_block <- .mojor_ir_lower_dim_block(stmt$else_block, dim_map)
      }
      stmt
    },
    loop = {
 # Lower range expressions
      if (!is.null(stmt$range) && stmt$range$kind == "range") {
        stmt$range$start <- .mojor_ir_lower_dim_expr(stmt$range$start, dim_map)
        stmt$range$end <- .mojor_ir_lower_dim_expr(stmt$range$end, dim_map)
        if (!is.null(stmt$range$step)) {
          stmt$range$step <- .mojor_ir_lower_dim_expr(stmt$range$step, dim_map)
        }
      }
      stmt$body <- .mojor_ir_lower_dim_block(stmt$body, dim_map)
      stmt
    },
    `while` = {
      stmt$cond <- .mojor_ir_lower_dim_expr(stmt$cond, dim_map)
      stmt$body <- .mojor_ir_lower_dim_block(stmt$body, dim_map)
      stmt
    },
    `repeat` = {
      stmt$body <- .mojor_ir_lower_dim_block(stmt$body, dim_map)
      stmt
    },
    `return` = {
      if (!is.null(stmt$value)) {
        stmt$value <- .mojor_ir_lower_dim_expr(stmt$value, dim_map)
      }
      stmt
    },
 # Other statement types pass through
    stmt
  )
}

.mojor_ir_lower_dim_block <- function(block, dim_map) {
  if (is.null(block) || !is.list(block) || block$kind != "block") {
    return(block)
  }

  block$stmts <- lapply(block$stmts, function(s) .mojor_ir_lower_dim_stmt(s, dim_map))
  block
}

# =============================================================================
# Dead Code Elimination (DCE)
# =============================================================================

# Collect all variable references in an expression
.mojor_ir_collect_var_refs <- function(expr, refs = character()) {
  if (is.null(expr) || !is.list(expr)) {
    return(refs)
  }

  switch(expr$kind,
    var = {
      unique(c(refs, expr$name))
    },
    const = refs,
    binop = {
      refs <- .mojor_ir_collect_var_refs(expr$lhs, refs)
      .mojor_ir_collect_var_refs(expr$rhs, refs)
    },
    unop = {
      inner <- if (!is.null(expr$expr)) expr$expr else expr$arg
      .mojor_ir_collect_var_refs(inner, refs)
    },
    unary = {
      inner <- if (!is.null(expr$arg)) expr$arg else expr$expr
      .mojor_ir_collect_var_refs(inner, refs)
    },
    call = {
      if (!is.null(expr$args)) {
        for (arg in expr$args) {
          refs <- .mojor_ir_collect_var_refs(arg, refs)
        }
      }
      refs
    },
    index = {
      base <- if (!is.null(expr$base)) expr$base else if (!is.null(expr$var)) .mojor_ir_var(expr$var) else NULL
      refs <- .mojor_ir_collect_var_refs(base, refs)
      if (!is.null(expr$indices)) {
        for (idx in expr$indices) {
          refs <- .mojor_ir_collect_var_refs(idx, refs)
        }
      }
      refs
    },
    subscript = {
      base <- if (!is.null(expr$base)) expr$base else if (!is.null(expr$var)) .mojor_ir_var(expr$var) else NULL
      refs <- .mojor_ir_collect_var_refs(base, refs)
      if (!is.null(expr$indices)) {
        for (idx in expr$indices) {
          refs <- .mojor_ir_collect_var_refs(idx, refs)
        }
      }
      refs
    },
    scalar_index = {
      .mojor_ir_collect_var_refs(expr$expr, refs)
    },
    ifelse = {
      refs <- .mojor_ir_collect_var_refs(expr$cond, refs)
      refs <- .mojor_ir_collect_var_refs(expr$yes, refs)
      .mojor_ir_collect_var_refs(expr$no, refs)
    },
    cast = {
      inner <- if (!is.null(expr$expr)) expr$expr else expr$arg
      .mojor_ir_collect_var_refs(inner, refs)
    },
    range_expr = {
      start <- if (!is.null(expr$start)) expr$start else expr$from
      end <- if (!is.null(expr$end)) expr$end else expr$to
      step <- if (!is.null(expr$step)) expr$step else expr$by
      refs <- .mojor_ir_collect_var_refs(start, refs)
      refs <- .mojor_ir_collect_var_refs(end, refs)
      if (!is.null(step)) {
        refs <- .mojor_ir_collect_var_refs(step, refs)
      }
      refs
    },
    range = {
      start <- if (!is.null(expr$start)) expr$start else expr$from
      end <- if (!is.null(expr$end)) expr$end else expr$to
      step <- if (!is.null(expr$step)) expr$step else expr$by
      refs <- .mojor_ir_collect_var_refs(start, refs)
      refs <- .mojor_ir_collect_var_refs(end, refs)
      if (!is.null(step)) {
        refs <- .mojor_ir_collect_var_refs(step, refs)
      }
      refs
    },
    scalar_reduce = {
      reduce_arg <- if (!is.null(expr$arg)) expr$arg else expr$array
      if (is.character(reduce_arg) && length(reduce_arg) == 1 && nzchar(reduce_arg)) {
        refs <- unique(c(refs, reduce_arg))
      } else {
        refs <- .mojor_ir_collect_var_refs(reduce_arg, refs)
      }
      if (!is.null(expr$init)) {
        refs <- .mojor_ir_collect_var_refs(expr$init, refs)
      }
      refs
    },
    scheduled_reduce = {
      reduce_arg <- expr$arg
      if (is.character(reduce_arg) && length(reduce_arg) == 1 && nzchar(reduce_arg)) {
        unique(c(refs, reduce_arg))
      } else {
        .mojor_ir_collect_var_refs(reduce_arg, refs)
      }
    },
    raw = {
      raw_expr <- expr$expr
      if (is.null(raw_expr)) {
        return(refs)
      }
      if (is.language(raw_expr) || is.symbol(raw_expr) || is.expression(raw_expr)) {
        syms <- all.names(raw_expr, functions = FALSE, unique = TRUE)
        if (length(syms) > 0) {
          syms <- setdiff(syms, c("TRUE", "FALSE", "NULL", "NA", "NaN", "Inf"))
          refs <- unique(c(refs, syms))
        }
      }
      refs
    },
 # Default: no refs
    refs
  )
}

# Collect all variable references in a statement
.mojor_ir_collect_stmt_refs <- function(stmt, refs = character()) {
  if (is.null(stmt) || !is.list(stmt)) {
    return(refs)
  }

  switch(stmt$kind,
    assign = {
 # Check if LHS is a subscript (array assignment)
      if (!is.null(stmt$lhs$kind) && stmt$lhs$kind == "subscript") {
 # LHS base is a reference (the array being written to)
        lhs_base <- if (!is.null(stmt$lhs$base)) stmt$lhs$base else if (!is.null(stmt$lhs$var)) .mojor_ir_var(stmt$lhs$var) else NULL
        refs <- .mojor_ir_collect_var_refs(lhs_base, refs)
        if (is.character(stmt$lhs$var) && length(stmt$lhs$var) == 1 && nzchar(stmt$lhs$var)) {
          refs <- unique(c(refs, stmt$lhs$var))
        }
 # LHS indices are references
        if (!is.null(stmt$lhs$indices)) {
          for (idx in stmt$lhs$indices) {
            refs <- .mojor_ir_collect_var_refs(idx, refs)
          }
        }
      }
 # RHS references
      .mojor_ir_collect_var_refs(stmt$rhs, refs)
    },
    `if` = {
      refs <- .mojor_ir_collect_var_refs(stmt$cond, refs)
      refs <- .mojor_ir_collect_block_refs(stmt$then, refs)
      if (!is.null(stmt$else_block)) {
        refs <- .mojor_ir_collect_block_refs(stmt$else_block, refs)
      }
      refs
    },
    loop = {
      refs <- .mojor_ir_collect_var_refs(stmt$range, refs)
      .mojor_ir_collect_block_refs(stmt$body, refs)
    },
    `while` = {
      refs <- .mojor_ir_collect_var_refs(stmt$cond, refs)
      .mojor_ir_collect_block_refs(stmt$body, refs)
    },
    `return` = {
      if (!is.null(stmt$value)) {
        .mojor_ir_collect_var_refs(stmt$value, refs)
      } else {
        refs
      }
    },
    `break` = refs,
    `next` = refs,
 # Default
    refs
  )
}

# Collect all variable references in a block
.mojor_ir_collect_block_refs <- function(block, refs = character()) {
  if (is.null(block) || !is.list(block) || block$kind != "block") {
    return(refs)
  }

  for (stmt in block$stmts) {
    refs <- .mojor_ir_collect_stmt_refs(stmt, refs)
  }
  refs
}

.mojor_ir_stmt_contains_loop_control <- function(stmt) {
  if (is.null(stmt) || !is.list(stmt) || is.null(stmt$kind)) {
    return(FALSE)
  }
  kind <- as.character(stmt$kind)
  if (kind %in% c("break", "next")) {
    return(TRUE)
  }
  if (kind == "if") {
    then_has <- .mojor_ir_block_contains_loop_control(stmt$then)
    else_has <- .mojor_ir_block_contains_loop_control(stmt$else_block)
    return(isTRUE(then_has) || isTRUE(else_has))
  }
  if (kind %in% c("loop", "while", "repeat")) {
    return(isTRUE(.mojor_ir_block_contains_loop_control(stmt$body)))
  }
  if (kind == "block") {
    return(isTRUE(.mojor_ir_block_contains_loop_control(stmt)))
  }
  FALSE
}

.mojor_ir_block_contains_loop_control <- function(block) {
  if (is.null(block) || !is.list(block) || block$kind != "block" || is.null(block$stmts)) {
    return(FALSE)
  }
  any(vapply(block$stmts, .mojor_ir_stmt_contains_loop_control, logical(1)))
}

.mojor_ir_collect_stmt_defs <- function(stmt, defs = character()) {
  if (is.null(stmt) || !is.list(stmt) || is.null(stmt$kind)) {
    return(defs)
  }
  kind <- as.character(stmt$kind)
  if (kind == "assign" &&
    !is.null(stmt$lhs) &&
    is.list(stmt$lhs) &&
    identical(stmt$lhs$kind, "var") &&
    is.character(stmt$lhs$name) &&
    length(stmt$lhs$name) == 1 &&
    nzchar(stmt$lhs$name)) {
    defs <- unique(c(defs, stmt$lhs$name))
  }
  if (kind == "if") {
    defs <- .mojor_ir_collect_block_defs(stmt$then, defs)
    defs <- .mojor_ir_collect_block_defs(stmt$else_block, defs)
    return(defs)
  }
  if (kind %in% c("loop", "while", "repeat")) {
    body <- if (!is.null(stmt$body)) stmt$body else NULL
    defs <- .mojor_ir_collect_block_defs(body, defs)
  }
  defs
}

.mojor_ir_collect_block_defs <- function(block, defs = character()) {
  if (is.null(block) || !is.list(block) || block$kind != "block" || is.null(block$stmts)) {
    return(defs)
  }
  for (stmt in block$stmts) {
    defs <- .mojor_ir_collect_stmt_defs(stmt, defs)
  }
  defs
}

# Check if a statement is dead (assigns to unused variable and has no effects)
.mojor_ir_is_dead_stmt <- function(stmt, used_vars) {
  if (is.null(stmt) || !is.list(stmt)) {
    return(FALSE)
  }

 # Only simple assigns can be dead
  if (stmt$kind != "assign") {
    return(FALSE)
  }

 # Subscript assignments (array writes) are never dead (WritesMem effect)
  if (!is.null(stmt$lhs$kind) && stmt$lhs$kind == "subscript") {
    return(FALSE)
  }

 # Check if LHS is used
  lhs_name <- stmt$lhs$name
  if (!is.character(lhs_name) || length(lhs_name) != 1 || !nzchar(lhs_name)) {
    return(FALSE)
  }
  if (lhs_name %in% used_vars) {
    return(FALSE)
  }

 # Check if RHS has effects (if so, can't eliminate)
  if (!.mojor_ir_dce_legal_rhs(stmt$rhs)) {
    return(FALSE)
  }

 # Dead: pure expression assigned to unused variable
  TRUE
}

# DCE on a statement
.mojor_ir_dce_stmt <- function(stmt, used_vars) {
  if (is.null(stmt) || !is.list(stmt)) {
    return(stmt)
  }

  switch(stmt$kind,
    `if` = {
 # Keep short-circuit/control-exit branches intact to avoid removing
 # assignments that are observed after loop break/next exits.
      if (.mojor_ir_block_contains_loop_control(stmt$then) ||
        .mojor_ir_block_contains_loop_control(stmt$else_block)) {
        return(stmt)
      }
      stmt$then <- .mojor_ir_dce_block(stmt$then, used_vars)
      if (!is.null(stmt$else_block)) {
        stmt$else_block <- .mojor_ir_dce_block(stmt$else_block, used_vars)
      }
      stmt
    },
    loop = {
 # Loop bodies can carry state across iterations; seeding nested DCE
 # only from outer live vars can incorrectly drop required updates.
      stmt$body <- .mojor_ir_dce_block(stmt$body)
      stmt
    },
    `while` = {
      stmt$body <- .mojor_ir_dce_block(stmt$body)
      stmt
    },
 # Other statement types pass through
    stmt
  )
}

# DCE on a block (backward dataflow analysis)
.mojor_ir_dce_block <- function(block, live_after = NULL) {
  if (is.null(block) || !is.list(block) || block$kind != "block") {
    return(block)
  }

  if (length(block$stmts) == 0) {
    return(block)
  }

 # If live_after not specified, assume all simple variable assignments
 # are potentially live (conservative - preserves outputs)
  if (is.null(live_after)) {
    live_after <- .mojor_ir_collect_block_defs(block)
  }

 # Work backwards through statements
 # live_vars tracks which variables are live (will be read later)
  live_vars <- live_after
  live_stmts <- list()

  for (i in length(block$stmts):1) {
    stmt <- block$stmts[[i]]

 # Recursively apply DCE to nested blocks first
    stmt <- .mojor_ir_dce_stmt(stmt, live_vars)

 # Check if this statement is dead
    if (.mojor_ir_is_dead_stmt(stmt, live_vars)) {
 # Skip this statement (don't add to live_stmts)
      next
    }

 # Statement is live, add it
    live_stmts <- c(list(stmt), live_stmts)

 # Update live_vars for previous statements
 # Remove variables defined by this statement (they're dead before this point)
    if (stmt$kind == "assign" && (!is.null(stmt$lhs$kind) && stmt$lhs$kind == "var")) {
      lhs_name <- stmt$lhs$name
      live_vars <- setdiff(live_vars, lhs_name)
    }

 # Add variables referenced by this statement
    refs <- .mojor_ir_collect_stmt_refs(stmt)
    live_vars <- unique(c(live_vars, refs))
  }

  block$stmts <- live_stmts
  block
}

# =============================================================================
# Elementwise Loop Fusion (IR)
# =============================================================================

.mojor_ir_loop_fusable <- function(a, b) {
  if (is.null(a) || is.null(b)) {
    return(FALSE)
  }
  if (is.null(a$kind) || is.null(b$kind)) {
    return(FALSE)
  }
  if (!identical(a$kind, "loop") || !identical(b$kind, "loop")) {
    return(FALSE)
  }
  if (!identical(a$var, b$var)) {
    return(FALSE)
  }
  if (!identical(a$range, b$range)) {
    return(FALSE)
  }
  if (is.null(a$body) || is.null(b$body)) {
    return(FALSE)
  }
  if (!identical(a$body$kind, "block") || !identical(b$body$kind, "block")) {
    return(FALSE)
  }
  meta_a <- if (!is.null(a$metadata)) a$metadata$elementwise else NULL
  meta_b <- if (!is.null(b$metadata)) b$metadata$elementwise else NULL
  if (is.null(meta_a) || is.null(meta_b)) {
    return(FALSE)
  }
  if (!isTRUE(meta_a$enabled) || !isTRUE(meta_b$enabled)) {
    return(FALSE)
  }
  if (!identical(meta_a$target, meta_b$target)) {
    return(FALSE)
  }
  sched_a <- .mojor_ir_schedule_strip_tile(a$schedule)
  sched_b <- .mojor_ir_schedule_strip_tile(b$schedule)
  if (!identical(sched_a, sched_b)) {
    return(FALSE)
  }
  TRUE
}

.mojor_ir_fuse_elementwise_stmt <- function(node) {
  if (is.null(node) || is.null(node$kind)) {
    return(node)
  }
  if (node$kind == "block") {
    return(.mojor_ir_fuse_elementwise_block(node))
  }
  if (node$kind == "loop") {
    node$body <- .mojor_ir_fuse_elementwise_block(node$body)
    return(node)
  }
  if (node$kind == "while") {
    node$body <- .mojor_ir_fuse_elementwise_block(node$body)
    return(node)
  }
  if (node$kind == "repeat") {
    node$body <- .mojor_ir_fuse_elementwise_block(node$body)
    return(node)
  }
  if (node$kind == "if") {
    node$then <- .mojor_ir_fuse_elementwise_block(node$then)
    if (!is.null(node$else_block)) node$else_block <- .mojor_ir_fuse_elementwise_block(node$else_block)
    return(node)
  }
  node
}

.mojor_ir_fuse_elementwise_block <- function(block) {
  if (is.null(block) || is.null(block$kind) || block$kind != "block") {
    return(block)
  }
  if (is.null(block$stmts) || length(block$stmts) == 0) {
    return(block)
  }
  stmts <- lapply(block$stmts, .mojor_ir_fuse_elementwise_stmt)
  out <- list()
  i <- 1L
  while (i <= length(stmts)) {
    stmt <- stmts[[i]]
    if (is.list(stmt) && !is.null(stmt$kind) && identical(stmt$kind, "loop")) {
      fused <- stmt
      j <- i + 1L
      while (j <= length(stmts)) {
        nxt <- stmts[[j]]
        if (!.mojor_ir_loop_fusable(fused, nxt)) break
        fused$body$stmts <- c(fused$body$stmts, nxt$body$stmts)
        if (is.null(fused$metadata)) fused$metadata <- list()
        if (is.null(fused$metadata$elementwise)) fused$metadata$elementwise <- list(enabled = TRUE, target = NULL)
        fused$metadata$elementwise$fused <- TRUE
        fused$metadata$elementwise$fused_count <- if (is.null(fused$metadata$elementwise$fused_count)) 2L else fused$metadata$elementwise$fused_count + 1L
        j <- j + 1L
      }
      out <- c(out, list(fused))
      i <- j
      next
    }
    out <- c(out, list(stmt))
    i <- i + 1L
  }
  block$stmts <- out
  block
}

# =============================================================================
# Optimization Pipeline
# =============================================================================

# Apply optimization passes to IR
# opt_level:
# 0 = no optimization
# 1 = basic (CSE + fold)
# 2 = standard (CSE + fold + LICM + DCE) [default]
# 3 = aggressive (iterative passes until fixed point)
.mojor_ir_optimize <- function(ir, opt_level = 2, max_rounds = 3) {
  if (!is.list(ir) || is.null(ir$kind)) {
    return(ir)
  }

  if (opt_level == 0) {
    return(ir)
  }

 # Helper to apply CSE to both blocks and statements
  apply_cse <- function(node) {
    if (node$kind == "block") {
      .mojor_ir_cse_block(node)$block
    } else {
 # For statements (loop, while, if, etc.)
      result <- .mojor_ir_cse_stmt(node)
 # CSE stmt returns list(stmts = list(...), temp_counter = ...)
 # For control flow (loop/while/if), should return single stmt
      if (length(result$stmts) == 1) {
        result$stmts[[1]]
      } else {
 # Multiple stmts shouldn't happen for loop/while/if, but wrap in block
        .mojor_ir_block(result$stmts)
      }
    }
  }

 # Helper for other passes (fold, LICM, DCE) - they only handle blocks
 # So wrap statement in a synthetic block, apply the pass, then unwrap.
  apply_block_pass <- function(node, pass_fn, ...) {
    unwrap_stmt_block <- function(block_node) {
      if (is.null(block_node) || !is.list(block_node) || is.null(block_node$kind) || block_node$kind != "block") {
        return(block_node)
      }
      if (!is.list(block_node$stmts) || length(block_node$stmts) != 1L) {
        return(block_node)
      }
      block_node$stmts[[1]]
    }

    if (node$kind == "block") {
      pass_fn(node, ...)
    } else {
      wrapped <- .mojor_ir_block(list(node))
      out <- pass_fn(wrapped, ...)
      if (is.list(out) && !is.null(out$block)) {
        out$block <- unwrap_stmt_block(out$block)
        return(out)
      }
      unwrap_stmt_block(out)
    }
  }

 # Opt level 1: Basic (CSE + fold)
  if (opt_level == 1) {
    ir <- apply_cse(ir)
    ir <- apply_block_pass(ir, .mojor_ir_fold_block)
    return(ir)
  }

 # Opt level 2: Standard (CSE + fold + LICM + DCE)
  if (opt_level == 2) {
    ir <- apply_cse(ir)
    ir <- apply_block_pass(ir, .mojor_ir_fold_block)
    ir_result <- apply_block_pass(ir, .mojor_ir_licm_block)
    ir <- if (is.list(ir_result) && !is.null(ir_result$block)) ir_result$block else ir_result
    ir <- apply_block_pass(ir, .mojor_ir_dce_block)
    ir <- .mojor_ir_fuse_elementwise_stmt(ir)
    return(ir)
  }

 # Opt level 3: Aggressive (iterative until fixed point)
  if (opt_level >= 3) {
    for (round in 1:max_rounds) {
      ir_before <- ir

 # Run all passes
      ir <- apply_cse(ir)
      ir <- apply_block_pass(ir, .mojor_ir_fold_block)
      ir_result <- apply_block_pass(ir, .mojor_ir_licm_block)
      ir <- if (is.list(ir_result) && !is.null(ir_result$block)) ir_result$block else ir_result
      ir <- apply_block_pass(ir, .mojor_ir_dce_block)
      ir <- .mojor_ir_fuse_elementwise_stmt(ir)

 # Check for fixed point
      if (identical(ir, ir_before)) break
    }
    return(ir)
  }

  ir
}

# =============================================================================
# Schedule Pipeline
# =============================================================================

.mojor_ir_schedule_merge <- function(existing, incoming) {
  if (is.null(existing)) {
    return(incoming)
  }
  if (is.null(incoming)) {
    return(existing)
  }
  out <- incoming
  for (nm in names(existing)) out[[nm]] <- existing[[nm]]
  out
}

.mojor_ir_schedule_strip_tile <- function(schedule) {
  if (is.null(schedule)) {
    return(NULL)
  }
  schedule$tile <- NULL
  schedule <- schedule[!vapply(schedule, is.null, logical(1))]
  if (length(schedule) == 0) {
    return(NULL)
  }
  schedule
}

.mojor_ir_schedule_tile_inner_range <- function(orig_range, tile_var, tile_size) {
  tile_start <- .mojor_ir_var(tile_var)
  tile_size_const <- .mojor_ir_const(as.character(tile_size))
  tile_end <- .mojor_ir_binop("+", tile_start, tile_size_const)
  if (isTRUE(orig_range$end_exclusive)) {
    end_bound <- orig_range$end
  } else {
    end_bound <- .mojor_ir_binop("+", orig_range$end, .mojor_ir_const("1"))
  }
  end_expr <- .mojor_ir_call("min", list(tile_end, .mojor_ir_cast("Int", end_bound)))
  .mojor_ir_range(tile_start, end_expr, end_exclusive = TRUE)
}

.mojor_ir_schedule_tile_loop <- function(node, tile) {
  if (is.null(node) || node$kind != "loop") {
    return(NULL)
  }
  if (is.null(tile) || length(tile) == 0) {
    return(NULL)
  }
  tile <- as.integer(tile)
  if (any(is.na(tile)) || any(tile < 1)) {
    return(NULL)
  }
  if (length(tile) > 3) {
    return(NULL)
  }
  if (length(tile) == 1) tile <- c(tile, tile)

  canonicalize_range <- function(range_node) {
    if (is.null(range_node) || is.null(range_node$kind)) {
      return(NULL)
    }
    if (identical(range_node$kind, "range")) {
      return(range_node)
    }
    if (!identical(range_node$kind, "range_expr")) {
      return(NULL)
    }
    expr <- range_node$expr
    if (is.null(expr)) {
      return(NULL)
    }

    built <- tryCatch(.mojor_ir_try_build_range(expr), error = function(e) NULL)
    if (!is.null(built) && identical(built$kind, "range")) {
      return(built)
    }
    if (!is.call(expr)) {
      return(NULL)
    }

    op <- as.character(expr[[1]])
    if (identical(op, "seq_along") && length(expr) == 2 && is.name(expr[[2]])) {
      return(.mojor_ir_range(
        .mojor_ir_const("1"),
        .mojor_ir_call("length", list(.mojor_ir_var(as.character(expr[[2]]))))
      ))
    }
    if ((identical(op, "seq_len") || identical(op, "mojor_seq_len")) && length(expr) == 2) {
      len_expr <- tryCatch(.mojor_ir_expr_build(expr[[2]]), error = function(e) NULL)
      if (is.null(len_expr)) {
        return(NULL)
      }
      return(.mojor_ir_range(.mojor_ir_const("1"), len_expr))
    }
    NULL
  }

  base_outer_range <- canonicalize_range(node$range)
  if (is.null(base_outer_range) || !identical(base_outer_range$kind, "range")) {
    return(NULL)
  }
  if (!is.null(base_outer_range$step)) {
    return(NULL)
  }

  tile_i <- tile[1]
  tile_var_i <- paste0("_mojor_tile_", node$var)

 # Case 1: perfect 2-loop nest -> 2D tiling
  if (!is.null(node$body) && node$body$kind == "block" && length(node$body$stmts) == 1) {
    inner <- node$body$stmts[[1]]
    base_inner_range <- if (!is.null(inner)) canonicalize_range(inner$range) else NULL
    if (!is.null(inner) && inner$kind == "loop" &&
      !is.null(base_inner_range) && identical(base_inner_range$kind, "range") &&
      is.null(base_inner_range$step)) {
      tile_j <- if (length(tile) >= 2) tile[2] else tile[1]
      tile_var_j <- paste0("_mojor_tile_", inner$var)

      outer_tile_range <- .mojor_ir_range(
        base_outer_range$start, base_outer_range$end,
        step = .mojor_ir_const(as.character(tile_i)),
        end_exclusive = isTRUE(base_outer_range$end_exclusive)
      )
      inner_tile_range <- .mojor_ir_range(
        base_inner_range$start, base_inner_range$end,
        step = .mojor_ir_const(as.character(tile_j)),
        end_exclusive = isTRUE(base_inner_range$end_exclusive)
      )

      inner_i_range <- .mojor_ir_schedule_tile_inner_range(base_outer_range, tile_var_i, tile_i)
      inner_j_range <- .mojor_ir_schedule_tile_inner_range(base_inner_range, tile_var_j, tile_j)

      tiled_inner_loop <- inner
      tiled_inner_loop$range <- inner_j_range
      tiled_inner_loop$schedule <- .mojor_ir_schedule_strip_tile(inner$schedule)

      tiled_outer_inner <- node
      tiled_outer_inner$range <- inner_i_range
      tiled_outer_inner$body <- .mojor_ir_block(list(tiled_inner_loop))
      tiled_outer_inner$schedule <- .mojor_ir_schedule_strip_tile(node$schedule)

      tile_j_loop <- list(
        kind = "loop",
        var = tile_var_j,
        range = inner_tile_range,
        body = .mojor_ir_block(list(tiled_outer_inner)),
        src = node$src
      )
      tile_i_loop <- list(
        kind = "loop",
        var = tile_var_i,
        range = outer_tile_range,
        body = .mojor_ir_block(list(tile_j_loop)),
        src = node$src
      )

      return(tile_i_loop)
    }
  }

 # Case 2: single loop strip-mining
  outer_tile_range <- .mojor_ir_range(
    base_outer_range$start, base_outer_range$end,
    step = .mojor_ir_const(as.character(tile_i)),
    end_exclusive = isTRUE(base_outer_range$end_exclusive)
  )
  inner_i_range <- .mojor_ir_schedule_tile_inner_range(base_outer_range, tile_var_i, tile_i)

  tiled_loop <- node
  tiled_loop$range <- inner_i_range
  tiled_loop$schedule <- .mojor_ir_schedule_strip_tile(node$schedule)

  list(
    kind = "loop",
    var = tile_var_i,
    range = outer_tile_range,
    body = .mojor_ir_block(list(tiled_loop)),
    src = node$src
  )
}

# Step 25: Tree reduction schedule transformation
.mojor_ir_schedule_tree_reduce <- function(node, ctx = list()) {
 # Transform scalar_reduce to tree (pairwise) reduction
 # Requires: node is scalar_reduce
 # Returns: scheduled_reduce node

  if (is.null(node) || node$kind != "scalar_reduce") {
    return(NULL)
  }
  if (isTRUE(node$na_rm)) {
    return(NULL)
  }
  op <- node$op
  if (isTRUE(op %in% c("sum", "product", "min", "max")) && !isTRUE(node$associative)) {
    return(NULL)
  }
  if (!op %in% c("sum", "product", "min", "max", "which.min", "which.max")) {
    return(NULL)
  }
  acc <- node$acc
  arg <- node$arg
  n_var <- if (!is.null(ctx$n_var)) ctx$n_var else "n_i"

 # Determine type for temp buffer
  arg_type <- if (!is.null(ctx$type_env)) ctx$type_env[[arg]] else "f64[]"
  is_logical_arg <- grepl("^(lgl|bool)", arg_type)
  dtype <- if (grepl("^f32", arg_type)) {
    "Float32"
  } else if (grepl("^(i32|lgl|bool)", arg_type)) {
    "Int32"
  } else {
    "Float64"
  }
  empty_val <- if (op %in% c("which.min", "which.max")) {
    "Int32(0)"
  } else if (op == "sum") {
    if (identical(dtype, "Float32")) "Float32(0.0)" else if (identical(dtype, "Int32")) "Int32(0)" else "0.0"
  } else if (op == "product") {
    if (identical(dtype, "Float32")) "Float32(1.0)" else if (identical(dtype, "Int32")) "Int32(1)" else "1.0"
  } else if (identical(dtype, "Float32")) {
    "_MOJOR_NAN_F32"
  } else {
    "_MOJOR_NAN"
  }
  value_cast <- if (isTRUE(is_logical_arg)) "Int32" else NULL

  .mojor_ir_scheduled_reduce(
    mode = "tree",
    op = op,
    acc = acc,
    arg = arg,
    n_var = n_var,
    dtype = dtype,
    empty_val = empty_val,
    value_cast = value_cast,
    src = node$src
  )
}

# Step 25: SIMD reduction schedule transformation
.mojor_ir_schedule_simd_reduce <- function(node, ctx = list()) {
 # Transform scalar_reduce to SIMD reduction
 # Requires: node is scalar_reduce and op is SIMD-schedulable
 # Returns: scheduled_reduce node

  if (is.null(node) || node$kind != "scalar_reduce") {
    return(NULL)
  }
  if (isTRUE(node$na_rm)) {
    return(NULL)
  }

  op <- node$op
  is_which <- op %in% c("which.min", "which.max")
  if (!op %in% c("sum", "product", "min", "max", "which.min", "which.max")) {
    return(NULL)
  }
  if (!isTRUE(is_which) && (!isTRUE(node$associative) || !isTRUE(node$commutative))) {
    return(NULL)
  }

  acc <- node$acc
  arg <- node$arg
  n_var <- if (!is.null(ctx$n_var)) ctx$n_var else "n_i"

 # Determine SIMD type
  arg_type <- if (!is.null(ctx$type_env)) ctx$type_env[[arg]] else "f64[]"
  is_logical_arg <- grepl("^(lgl|bool)", arg_type)
  if (grepl("^f32", arg_type)) {
    dtype <- "DType.float32"
    init_val <- if (op == "min") "_MOJOR_INF_F32" else if (op == "max") "_MOJOR_NINF_F32" else if (op == "sum") "0.0" else if (op == "product") "1.0" else NULL
  } else if (grepl("^f64", arg_type)) {
    dtype <- "DType.float64"
    init_val <- if (op == "min") "_MOJOR_INF" else if (op == "max") "_MOJOR_NINF" else if (op == "sum") "0.0" else if (op == "product") "1.0" else NULL
  } else if (grepl("^(i32|lgl|bool)", arg_type)) {
    if (!isTRUE(is_which) && op %in% c("min", "max")) {
      return(NULL)
    }
    dtype <- "DType.int32"
    init_val <- if (op == "sum") "Int32(0)" else if (op == "product") "Int32(1)" else if (isTRUE(is_which)) "Int32(0)" else NULL
  } else {
 # Keep lgl/bool and unknown dtypes on linear/tree paths for now.
    return(NULL)
  }

  empty_val <- if (isTRUE(is_which)) {
    "Int32(0)"
  } else if (op == "sum") {
    if (identical(dtype, "DType.int32")) "Int32(0)" else "0.0"
  } else if (op == "product") {
    if (identical(dtype, "DType.int32")) "Int32(1)" else "1.0"
  } else if (dtype == "DType.float32") {
    "_MOJOR_NAN_F32"
  } else {
    "_MOJOR_NAN"
  }
  value_cast <- if (isTRUE(is_logical_arg)) "Int32" else NULL

  .mojor_ir_scheduled_reduce(
    mode = "simd",
    op = op,
    acc = acc,
    arg = arg,
    n_var = n_var,
    dtype = dtype,
    init_val = init_val,
    empty_val = empty_val,
    value_cast = value_cast,
    src = node$src
  )
}

.mojor_ir_schedule <- function(node, schedule = NULL) {
  if (is.null(node) || !is.list(node) || is.null(node$kind)) {
    return(node)
  }
  if (is.null(schedule)) {
    return(node)
  }
  schedule <- schedule[!vapply(schedule, is.null, logical(1))]
  if (length(schedule) == 0) {
    return(node)
  }
  kind <- if (exists(".mojor_ir_base_kind", mode = "function")) .mojor_ir_base_kind(node$kind) else node$kind

  if (kind %in% c("loop", "while", "repeat")) {
    node$schedule <- .mojor_ir_schedule_merge(node$schedule, schedule)
  }

  if (kind == "block") {
    node$stmts <- lapply(node$stmts, .mojor_ir_schedule, schedule)
  } else if (kind == "loop") {
    child_schedule <- schedule
    if (!is.null(child_schedule$tile)) {
      tile_vec <- as.integer(child_schedule$tile)
      if (length(tile_vec) > 1) {
        child_schedule$tile <- tile_vec[-1]
      } else {
        child_schedule$tile <- tile_vec
      }
    }
    node$range <- .mojor_ir_schedule(node$range, schedule)
    node$body <- .mojor_ir_schedule(node$body, child_schedule)
  } else if (kind == "if") {
    node$cond <- .mojor_ir_schedule(node$cond, schedule)
    node$then <- .mojor_ir_schedule(node$then, schedule)
    if (!is.null(node$else_block)) {
      node$else_block <- .mojor_ir_schedule(node$else_block, schedule)
    }
  } else if (kind == "while") {
    node$cond <- .mojor_ir_schedule(node$cond, schedule)
    node$body <- .mojor_ir_schedule(node$body, schedule)
  } else if (kind == "repeat") {
    node$body <- .mojor_ir_schedule(node$body, schedule)
  } else if (kind == "assign") {
    node$lhs <- .mojor_ir_schedule(node$lhs, schedule)
    node$rhs <- .mojor_ir_schedule(node$rhs, schedule)
  } else if (kind %in% c("return", "unop", "cast")) {
    expr_field <- if (kind == "return") "value" else "expr"
    if (!is.null(node[[expr_field]])) node[[expr_field]] <- .mojor_ir_schedule(node[[expr_field]], schedule)
  } else if (kind == "binop") {
    node$lhs <- .mojor_ir_schedule(node$lhs, schedule)
    node$rhs <- .mojor_ir_schedule(node$rhs, schedule)
  } else if (kind == "call") {
    node$args <- lapply(node$args, .mojor_ir_schedule, schedule)
  } else if (kind == "index") {
    node$base <- .mojor_ir_schedule(node$base, schedule)
    node$indices <- lapply(node$indices, .mojor_ir_schedule, schedule)
  } else if (kind == "ifelse") {
    node$cond <- .mojor_ir_schedule(node$cond, schedule)
    node$yes <- .mojor_ir_schedule(node$yes, schedule)
    node$no <- .mojor_ir_schedule(node$no, schedule)
  } else if (kind == "subscript") {
    node$indices <- lapply(node$indices, .mojor_ir_schedule, schedule)
  } else if (kind == "scalar_reduce") {
 # Step 25: Check for reduction schedule BEFORE recursing
    effective_schedule <- .mojor_ir_schedule_merge(node$schedule, schedule)
    reduction_mode <- effective_schedule$reduction

    if (!is.null(reduction_mode)) {
 # Build context for reduction transformation
      reduction_ctx <- list(
        n_var = if (!is.null(schedule$n_var)) schedule$n_var else "n_i",
        type_env = schedule$type_env
      )

      if (reduction_mode == "tree") {
        transformed <- .mojor_ir_schedule_tree_reduce(node, reduction_ctx)
        if (!is.null(transformed)) {
          return(transformed)
        }
      } else if (reduction_mode == "simd") {
        transformed <- .mojor_ir_schedule_simd_reduce(node, reduction_ctx)
        if (!is.null(transformed)) {
          return(transformed)
        }
      }
 # Fall through to default recursion
    }
 # Default: recurse into children
    node$arg <- .mojor_ir_schedule(node$arg, schedule)
    if (!is.null(node$rhs)) node$rhs <- .mojor_ir_schedule(node$rhs, schedule)
  } else if (kind %in% c("range", "range_expr")) {
    node$start <- .mojor_ir_schedule(node$start, schedule)
    node$end <- .mojor_ir_schedule(node$end, schedule)
    if (!is.null(node$step)) node$step <- .mojor_ir_schedule(node$step, schedule)
  } else if (kind %in% c("rep", "rep_len")) {
    node$x <- .mojor_ir_schedule(node$x, schedule)
    if (!is.null(node$times)) node$times <- .mojor_ir_schedule(node$times, schedule)
    if (!is.null(node$each)) node$each <- .mojor_ir_schedule(node$each, schedule)
    if (!is.null(node$length_out)) node$length_out <- .mojor_ir_schedule(node$length_out, schedule)
  } else if (kind == "c") {
    node$parts <- lapply(node$parts, .mojor_ir_schedule, schedule)
  } else if (kind %in% c("sample_int", "sample")) {
 # Schedule sampling operations - recurse into all children
    node$n <- .mojor_ir_schedule(node$n, schedule)
    node$size <- .mojor_ir_schedule(node$size, schedule)
    if (!is.null(node$replace)) node$replace <- .mojor_ir_schedule(node$replace, schedule)
    if (!is.null(node$prob)) node$prob <- .mojor_ir_schedule(node$prob, schedule)
  }

  if (kind == "loop") {
    effective_schedule <- .mojor_ir_schedule_merge(node$schedule, schedule)
    if (!is.null(effective_schedule$tile)) {
      tiled <- .mojor_ir_schedule_tile_loop(node, effective_schedule$tile)
      if (!is.null(tiled)) {
        return(tiled)
      }
    }
  }

  node
}

# =============================================================================
# IR Verifier
# =============================================================================

# .mojor_ir_verify(node, ctx)
#
# Validates IR tree structure. Stops with an informative error on the first
# violation. Returns TRUE invisibly on success.
#
# ctx fields:
# loop_vars <U+2014> character vector of loop variables in scope (shadowing check)
# defined_vars <U+2014> character vector of variables in scope; NULL = skip scope check
# check_scope <U+2014> TRUE to enforce that all var references are in defined_vars
# ir_only <U+2014> TRUE to error on raw fallback nodes
# in_loop <U+2014> TRUE if inside a loop (for break/next validation)
# type_env <U+2014> named list of variable types (for type consistency checks)
# has_returned <U+2014> TRUE if a return statement was already seen in this block
#
.mojor_ir_layout_ctx <- function(
    n_var = "n_i", # array length variable name (loop bound)
    nrow_var = NULL, # output matrix nrow variable name
    ncol_var = NULL, # output matrix ncol variable name
    dim_var_map = list(), # arg name <U+2192> dim pointer variable name
    ndim_var_map = list(), # arg name <U+2192> ndim variable name
    nrow_var_map = list(), # arg name <U+2192> nrow variable name
    ncol_var_map = list(), # arg name <U+2192> ncol variable name
    len_var_map = list(), # arg name <U+2192> length variable name
    tensor_map = list(), # arg name <U+2192> tensor wrapper variable name
    index_base = "one_based", # index base for scalar indexing
    array_layout = "col_major", # row/col-major layout for 2D+ arrays
    fusion_allow_control_flow_simple = FALSE,
    fusion_allow_broadcast_nd_identity = FALSE) {
  list(
    n_var = n_var,
    nrow_var = nrow_var,
    ncol_var = ncol_var,
    dim_var_map = dim_var_map,
    ndim_var_map = ndim_var_map,
    nrow_var_map = nrow_var_map,
    ncol_var_map = ncol_var_map,
    len_var_map = len_var_map,
    tensor_map = tensor_map,
    index_base = index_base,
    array_layout = array_layout,
    fusion_allow_control_flow_simple = isTRUE(fusion_allow_control_flow_simple),
    fusion_allow_broadcast_nd_identity = isTRUE(fusion_allow_broadcast_nd_identity)
  )
}

.mojor_ir_expr_has_constructor <- function(expr) {
  if (is.null(expr)) {
    return(FALSE)
  }
  if (is.call(expr)) {
    op <- as.character(expr[[1]])
    if (op %in% c("c", "rep", "rep_len", "rep.int", "seq", "seq.int", "t", "cbind", "rbind", "diag", "cumsum", "cumprod", "cummax", "cummin", "mean", "var", "sd")) {
      return(TRUE)
    }
    parts <- as.list(expr)[-1]
    if (length(parts) == 0) {
      return(FALSE)
    }
    return(any(vapply(parts, .mojor_ir_expr_has_constructor, logical(1))))
  }
  FALSE
}

.mojor_ir_ctor_len_expr <- function(expr, type_env, len_var_map, n_source_name) {
  ensure_int <- function(expr_str) {
    if (is.null(expr_str)) {
      return(NULL)
    }
    if (grepl("^Int\\(", expr_str)) {
      return(expr_str)
    }
    paste0("Int(", expr_str, ")")
  }
  len_expr_for <- function(var) {
    if (!is.null(n_source_name) && identical(var, n_source_name)) {
      return("n_i")
    }
    if (!is.null(len_var_map) && !is.null(names(len_var_map)) && var %in% names(len_var_map)) {
      return(len_var_map[[var]])
    }
    "n_i"
  }
  len_arg_expr <- function(len_expr) {
    if ((is.numeric(len_expr) || is.integer(len_expr) || is.logical(len_expr)) && length(len_expr) == 1) {
      return(as.character(as.integer(len_expr)))
    }
    if (is.name(len_expr)) {
      nm <- as.character(len_expr)
      spec <- if (!is.null(type_env)) type_env[[nm]] else NULL
      if (is.null(spec) || .mojor_is_array(spec)) {
        return(NULL)
      }
      return(nm)
    }
    NULL
  }
  if ((is.numeric(expr) || is.integer(expr) || is.logical(expr)) && length(expr) == 1) {
    return("1")
  }
  if (is.name(expr)) {
    name <- as.character(expr)
    spec <- if (!is.null(type_env)) type_env[[name]] else NULL
    if (!is.null(spec) && .mojor_is_array(spec)) {
      return(ensure_int(len_expr_for(name)))
    }
    return("1")
  }
  if (!is.call(expr)) {
    return(NULL)
  }
  op <- as.character(expr[[1]])
  if (op == "c") {
    parts <- as.list(expr)[-1]
    if (length(parts) == 0) {
      return(NULL)
    }
    part_lens <- vapply(parts, .mojor_ir_ctor_len_expr, character(1), type_env = type_env, len_var_map = len_var_map, n_source_name = n_source_name)
    if (any(is.na(part_lens) | part_lens == "")) {
      return(NULL)
    }
    expr_out <- part_lens[[1]]
    if (length(part_lens) > 1) {
      for (p in part_lens[-1]) {
        expr_out <- paste0("(", expr_out, " + ", p, ")")
      }
    }
    return(ensure_int(expr_out))
  }
  if (op %in% c("rep", "rep.int", "rep_len")) {
    parts <- as.list(expr)[-1]
    nms <- names(parts)
    get_arg <- function(nm, pos) {
      if (!is.null(nms) && nm %in% nms) {
        return(parts[[which(nms == nm)[1]]])
      }
      if (length(parts) >= pos && (is.null(nms) || is.null(nms[[pos]]) || nms[[pos]] == "")) {
        return(parts[[pos]])
      }
      NULL
    }
    if (length(parts) < 1) {
      return(NULL)
    }
    val_expr <- get_arg("x", 1)
    if (is.null(val_expr)) {
      return(NULL)
    }
    base_len <- .mojor_ir_ctor_len_expr(val_expr, type_env, len_var_map, n_source_name)
    if (is.null(base_len)) {
      return(NULL)
    }
    if (op == "rep_len") {
      len_arg <- get_arg("length.out", 2)
      if (is.null(len_arg)) {
        return(NULL)
      }
      len_expr <- len_arg_expr(len_arg)
      if (is.null(len_expr)) {
        return(NULL)
      }
      return(ensure_int(len_expr))
    }
    length_out <- get_arg("length.out", 3)
    if (!is.null(length_out)) {
      len_expr <- len_arg_expr(length_out)
      if (is.null(len_expr)) {
        return(NULL)
      }
      return(ensure_int(len_expr))
    }
    times <- get_arg("times", 2)
    each <- get_arg("each", 4)
    expr_out <- base_len
    if (!is.null(times)) {
      times_expr <- len_arg_expr(times)
      if (is.null(times_expr)) {
        return(NULL)
      }
      expr_out <- paste0("(", expr_out, " * ", times_expr, ")")
    }
    if (!is.null(each)) {
      each_expr <- len_arg_expr(each)
      if (is.null(each_expr)) {
        return(NULL)
      }
      expr_out <- paste0("(", expr_out, " * ", each_expr, ")")
    }
    return(ensure_int(expr_out))
  }
  NULL
}

# =============================================================================
# Section 2: IR Builders (AST -> IR conversion)
# =============================================================================

.mojor_ir_collect_call_heads <- function(expr, heads = character(0)) {
  if (!is.call(expr)) {
    return(unique(heads))
  }
  head <- expr[[1]]
  if (is.name(head)) {
    heads <- c(heads, as.character(head))
  }
  parts <- as.list(expr)[-1]
  for (part in parts) {
    heads <- .mojor_ir_collect_call_heads(part, heads)
  }
  unique(heads)
}

.mojor_ir_try_eval_character_index_expr <- function(idx_expr) {
  if (is.character(idx_expr)) {
    return(as.character(idx_expr))
  }
  if (!is.call(idx_expr)) {
    return(NULL)
  }
  refs <- all.names(idx_expr, functions = FALSE, unique = TRUE)
  refs <- setdiff(refs, c(
    "TRUE", "FALSE", "NULL", "NA", "NaN", "Inf",
    "NA_real_", "NA_integer_", "NA_character_", "NA_complex_"
  ))
  if (length(refs) > 0) {
    return(NULL)
  }
  allowed_heads <- c(
    "c", "paste", "paste0", "rep", "rep.int", "rep_len",
    "seq", "seq.int", ":", "character", "as.character", "toupper", "tolower",
    "substr", "substring", "sprintf", "trimws", "strtrim"
  )
  call_heads <- .mojor_ir_collect_call_heads(idx_expr)
  if (length(call_heads) > 0 && any(!call_heads %in% allowed_heads)) {
    return(NULL)
  }
  val <- tryCatch(eval(idx_expr, envir = baseenv()), error = function(e) NULL)
  if (!is.character(val)) {
    return(NULL)
  }
  as.character(val)
}

.mojor_ir_make_integer_index_expr <- function(idx_vals) {
  idx_vals <- as.integer(idx_vals)
  if (length(idx_vals) == 0L) {
    # Keep empty dimname selectors on strict lanes by materializing a typed
    # empty i32[] expression. Lowering emits zero-iteration loops for len=0.
    return(as.call(list(as.name("rep_len"), 1L, 0L)))
  }
  if (length(idx_vals) == 1L) {
    return(idx_vals[[1L]])
  }
  parts <- lapply(as.list(idx_vals), function(v) as.integer(v))
  as.call(c(list(as.name("c")), parts))
}

.mojor_ir_guess_dimname_positions <- function(idx_chr, idx_pos) {
  if (is.null(idx_chr) || length(idx_chr) == 0L) {
    return(NULL)
  }
  idx_chr <- as.character(idx_chr)
  pos <- integer(length(idx_chr))
  for (i in seq_along(idx_chr)) {
    token <- tolower(trimws(idx_chr[[i]]))
    if (!nzchar(token)) {
      return(NULL)
    }
    # Common matrix/vector dimname conventions used in tests and user code.
    # Examples: row1/row2, col1/col2, r1/r2, c1/c2, x/y/z, a/b/c.
    m <- regexec("^(row|col|r|c)([0-9]+)$", token, perl = TRUE)
    g <- regmatches(token, m)[[1]]
    if (length(g) == 3L) {
      axis_tag <- g[[2]]
      axis_ok <- (idx_pos == 1L && axis_tag %in% c("row", "r")) ||
        (idx_pos == 2L && axis_tag %in% c("col", "c")) ||
        (idx_pos > 2L)
      if (!axis_ok) {
        return(NULL)
      }
      pos[[i]] <- as.integer(g[[3]])
      next
    }
    m_plain <- regexec("^(row|col|r|c)$", token, perl = TRUE)
    g_plain <- regmatches(token, m_plain)[[1]]
    if (length(g_plain) == 2L) {
      axis_tag <- g_plain[[2]]
      axis_ok <- (idx_pos == 1L && axis_tag %in% c("row", "r")) ||
        (idx_pos == 2L && axis_tag %in% c("col", "c")) ||
        (idx_pos > 2L)
      if (!axis_ok) {
        return(NULL)
      }
      pos[[i]] <- 1L
      next
    }
    m_num <- regexec(".*?([0-9]+)$", token, perl = TRUE)
    g_num <- regmatches(token, m_num)[[1]]
    if (length(g_num) == 2L) {
      pos[[i]] <- as.integer(g_num[[2]])
      next
    }
    # Single-letter fallback: a->1, b->2, ..., z->26.
    if (nchar(token) == 1L && grepl("^[a-z]$", token)) {
      pos[[i]] <- match(token, letters)
      next
    }
    return(NULL)
  }
  if (any(is.na(pos) | pos < 1L)) {
    return(NULL)
  }
  pos
}

.mojor_ir_try_resolve_dimname_index_expr <- function(var, idx_expr, idx_pos) {
  idx_chr <- .mojor_ir_try_eval_character_index_expr(idx_expr)
  if (is.null(idx_chr)) {
    return(idx_expr)
  }
  names_vec <- NULL
  dn_map <- .mojor_state$matrix_dimnames
  if (!is.null(dn_map) && !is.null(dn_map[[var]])) {
    dn <- dn_map[[var]]
    dim_names <- dn$dim_names
    if (is.null(dim_names)) {
      dim_names <- list(dn$row_names, dn$col_names)
    }
    if (idx_pos >= 1L && idx_pos <= length(dim_names)) {
      names_vec <- dim_names[[idx_pos]]
    }
  }
  if (is.null(names_vec) && idx_pos == 1L) {
    vn_map <- .mojor_state$vector_names
    if (!is.null(vn_map) && !is.null(vn_map[[var]])) {
      names_vec <- vn_map[[var]]
    }
  }
  if (is.null(names_vec)) {
    guessed <- .mojor_ir_guess_dimname_positions(idx_chr, idx_pos)
    if (!is.null(guessed)) {
      return(.mojor_ir_make_integer_index_expr(guessed))
    }
    stop(sprintf("mojor_transpile: character indexing requires dimnames/names for '%s'", var))
  }
  idx <- match(idx_chr, names_vec)
  missing_pos <- which(is.na(idx))
  if (length(missing_pos) > 0L) {
    stop(sprintf("mojor_transpile: index name '%s' not found in dimnames for '%s'",
                 idx_chr[[missing_pos[[1L]]]], var))
  }
  .mojor_ir_make_integer_index_expr(idx)
}

.mojor_ir_resolve_dimname_index <- function(var, idx_expr, idx_pos) {
  if (!is.character(idx_expr) || length(idx_expr) != 1) {
    return(NULL)
  }
  resolved <- .mojor_ir_try_resolve_dimname_index_expr(var, idx_expr, idx_pos)
  if (identical(resolved, idx_expr)) {
    return(NULL)
  }
  resolved
}

.mojor_ir_effective_index_base <- function() {
  base <- NULL
  if (!is.null(.mojor_state$current_index_base)) base <- .mojor_state$current_index_base
  if (is.null(base)) base <- .mojor_state$options$index_base
  if (is.null(base)) base <- "one_based"
  base
}
