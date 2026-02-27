.mojor_ir_infer_type <- function(node, type_env = list()) {
 # Bottom-up type inference for IR expression nodes
 # type_env: named list mapping variable names to types

  if (is.null(node) || is.null(node$kind)) {
    return("unknown")
  }

 # Cache hit: return pre-annotated type
  if (!is.null(node$type)) {
    return(node$type)
  }

  if (node$kind == "const") {
 # Infer from value string
    val <- node$value
    if (is.null(val)) {
      return("unknown")
    }

    if (is.character(val) && length(val) == 1 && (grepl("^\".*\"$", val) || grepl("^'.*'$", val))) {
      return("chr")
    }

 # Check for boolean constants (R and Mojo spellings)
    if (val %in% c("TRUE", "FALSE", "True", "False", "true", "false")) {
      return("bool")
    }

 # Special float constants (accept uppercase and lowercase)
    if (tolower(val) %in% c("inf", "-inf", "nan")) {
      return("f64")
    }

 # Check for integer constants (no decimal point)
    if (grepl("^-?[0-9]+$", val)) {
      return("i32")
    }

 # Check for float constants
    if (grepl("^-?[0-9]*\\.[0-9]+([eE][+-]?[0-9]+)?$", val) ||
      grepl("^-?[0-9]+\\.[0-9]*([eE][+-]?[0-9]+)?$", val)) {
      return("f64")
    }

 # Scientific notation without decimal
    if (grepl("[eE]", val)) {
      return("f64")
    }

    return("unknown")
  }

  vector_type_from_elem <- function(elem_type) {
    if (is.null(elem_type) || identical(elem_type, "unknown")) {
      return("unknown")
    }
    if (elem_type %in% c("i32", "lgl", "bool")) {
      return("i32[]")
    }
    if (identical(elem_type, "f32")) {
      return("f32[]")
    }
    if (identical(elem_type, "f64")) {
      return("f64[]")
    }
    if (identical(elem_type, "chr")) {
      return("chr[]")
    }
    "unknown"
  }

  promote_many <- function(types) {
    if (length(types) == 0) {
      return("unknown")
    }
    out <- types[[1]]
    if (length(types) > 1) {
      for (t in types[-1]) {
        out <- .mojor_type_promote(out, t)
      }
    }
    out
  }
  is_i32_or_integral_literal <- function(expr_node, inferred_type) {
    if (identical(inferred_type, "i32")) {
      return(TRUE)
    }
    if (is.null(expr_node) || !is.list(expr_node) || !identical(expr_node$kind, "const")) {
      return(FALSE)
    }
    val <- suppressWarnings(as.numeric(.mojor_ir_eval_const(expr_node)))
    if (length(val) != 1 || !is.finite(val)) {
      return(FALSE)
    }
    if (abs(val - round(val)) > 1e-12) {
      return(FALSE)
    }
    if (val < -2147483648 || val > 2147483647) {
      return(FALSE)
    }
    TRUE
  }
  matrix_type_from_operands <- function(lhs_type, rhs_type) {
    if (is.null(lhs_type) || is.null(rhs_type) ||
        identical(lhs_type, "unknown") || identical(rhs_type, "unknown")) {
      return("unknown")
    }
    if (!.mojor_type_is_array(lhs_type) || !.mojor_type_is_array(rhs_type)) {
      return("unknown")
    }
    if (!identical(.mojor_type_ndim(lhs_type), 2L) || !identical(.mojor_type_ndim(rhs_type), 2L)) {
      return("unknown")
    }
    lhs_elem <- .mojor_type_elem(lhs_type)
    rhs_elem <- .mojor_type_elem(rhs_type)
    if (!identical(lhs_elem, rhs_elem)) {
      return("unknown")
    }
    if (!(lhs_elem %in% c("i32", "f32", "f64"))) {
      return("unknown")
    }
    prefer_comma_matrix <- grepl("\\[,\\]$", lhs_type) || grepl("\\[,\\]$", rhs_type)
    if (isTRUE(prefer_comma_matrix)) {
      return(paste0(lhs_elem, "[,]"))
    }
    paste0(lhs_elem, "[2d]")
  }
  cov_cor_result_type_from_operands <- function(x_type, y_type) {
    if (is.null(x_type) || is.null(y_type) ||
        identical(x_type, "unknown") || identical(y_type, "unknown")) {
      return("unknown")
    }
    if (!.mojor_type_is_array(x_type) || !.mojor_type_is_array(y_type)) {
      return("unknown")
    }
    x_ndim <- .mojor_type_ndim(x_type)
    y_ndim <- .mojor_type_ndim(y_type)
    if (!identical(x_ndim, y_ndim)) {
      return("unknown")
    }
    if (!(x_ndim %in% c(1L, 2L))) {
      return("unknown")
    }
    x_elem <- .mojor_type_elem(x_type)
    y_elem <- .mojor_type_elem(y_type)
    if (!identical(x_elem, y_elem)) {
      return("unknown")
    }
    if (!(x_elem %in% c("i32", "f32", "f64"))) {
      return("unknown")
    }
    if (identical(x_ndim, 1L)) {
      return("f64")
    }
    prefer_comma_matrix <- grepl("\\[,\\]$", x_type) || grepl("\\[,\\]$", y_type)
    if (isTRUE(prefer_comma_matrix)) {
      return("f64[,]")
    }
    "f64[2d]"
  }

  if (node$kind == "var") {
 # Look up in type environment
    var_type <- type_env[[node$name]]
    if (is.null(var_type)) {
      return("unknown")
    }
 # Strip array suffix if present (var access returns array type)
    return(var_type)
  }

  if (node$kind == "cast") {
 # Cast node specifies its result type
    return(node$to)
  }

  if (node$kind == "unop") {
    arg_type <- .mojor_ir_infer_type(node$expr, type_env)
    if (node$op == "!") {
      return("bool")
    }
    if (node$op == "-") {
      return(arg_type)
    } # Preserve numeric type
    return("unknown")
  }

  if (node$kind == "binop") {
    lhs_type <- .mojor_ir_infer_type(node$lhs, type_env)
    rhs_type <- .mojor_ir_infer_type(node$rhs, type_env)

 # Comparison ops always return bool
    if (node$op %in% c(">", "<", ">=", "<=", "==", "!=")) {
      return("bool")
    }

 # Logical ops expect bool and return bool
    if (node$op %in% c("&&", "||")) {
      return("bool")
    }

 # Arithmetic ops: promote to common type
    if (node$op %in% c("+", "-", "*", "/", "%%", "%/%")) {
      return(.mojor_type_promote(lhs_type, rhs_type))
    }

    return("unknown")
  }

  if (node$kind == "index") {
 # Array index returns element type for scalar selectors and vector type for
 # vector/slice selectors (e.g., x[idx], x[1:3], x[, j]).
    base_type <- .mojor_ir_infer_type(node$base, type_env)
    if (is.null(base_type) || base_type == "unknown") {
      return("unknown")
    }

    if (.mojor_type_is_array(base_type)) {
      elem_type <- .mojor_type_elem(base_type)
      base_ndim <- .mojor_type_ndim(base_type)

      # For full-rank ND selectors, preserve output rank after dropping
      # scalar-selected axes (e.g., arr[, , k] -> elem[,]).
      if (!is.na(base_ndim) && base_ndim >= 2L &&
          !is.null(node$indices) && is.list(node$indices) &&
          length(node$indices) == base_ndim) {
        result_ndim <- base_ndim
        for (idx in node$indices) {
          idx_node <- idx
          if (is.list(idx_node) && identical(idx_node$kind, "scalar_index") && !is.null(idx_node$expr)) {
            idx_node <- idx_node$expr
          }
          is_vector_selector <- FALSE
          if (is.list(idx_node)) {
            if (identical(idx_node$kind, "missing_index") ||
                identical(idx_node$kind, "slice_index") ||
                identical(idx_node$kind, "c") ||
                !is.null(idx_node$pos_vec_selection) ||
                !is.null(idx_node$neg_exclusion) ||
                !is.null(idx_node$neg_vec_exclusion)) {
              is_vector_selector <- TRUE
            }
          }
          if (!isTRUE(is_vector_selector)) {
            idx_type <- tryCatch(.mojor_ir_infer_type(idx_node, type_env), error = function(e) "unknown")
            if (!is.null(idx_type) && .mojor_type_is_array(idx_type)) {
              is_vector_selector <- TRUE
            }
          }
          if (!isTRUE(is_vector_selector)) {
            result_ndim <- result_ndim - 1L
          }
        }
        if (result_ndim <= 0L) {
          if (elem_type == "lgl") elem_type <- "bool"
          return(elem_type)
        }
        return(.mojor_type_tag_ndim(elem_type, result_ndim))
      }

      has_vector_selector <- FALSE
      if (!is.null(node$indices) && is.list(node$indices) && length(node$indices) > 0) {
        for (idx in node$indices) {
          idx_node <- idx
          if (is.list(idx_node) && identical(idx_node$kind, "scalar_index") && !is.null(idx_node$expr)) {
            idx_node <- idx_node$expr
          }
          if (is.list(idx_node) &&
            (identical(idx_node$kind, "slice_index") || identical(idx_node$kind, "missing_index") ||
             !is.null(idx_node$pos_vec_selection) || !is.null(idx_node$neg_vec_exclusion))) {
            has_vector_selector <- TRUE
            break
          }
          idx_type <- tryCatch(.mojor_ir_infer_type(idx_node, type_env), error = function(e) "unknown")
          if (!is.null(idx_type) && .mojor_type_is_array(idx_type)) {
            has_vector_selector <- TRUE
            break
          }
        }
      }
      if (isTRUE(has_vector_selector)) {
        return(vector_type_from_elem(elem_type))
      }
      if (elem_type == "lgl") elem_type <- "bool"
      return(elem_type)
    }

 # If base is not an array, it's unknown
    return("unknown")
  }

  if (node$kind == "c") {
    if (is.null(node$parts) || !is.list(node$parts) || length(node$parts) == 0) {
      return("unknown")
    }
    part_types <- vapply(node$parts, function(p) .mojor_ir_infer_type(p, type_env), character(1))
    if (all(part_types == "chr")) {
      return("chr[]")
    }
    elem_type <- promote_many(part_types)
    return(vector_type_from_elem(elem_type))
  }

  if (node$kind == "rep") {
    x_type <- .mojor_ir_infer_type(node$x, type_env)
    if (.mojor_type_is_array(x_type)) {
      return(vector_type_from_elem(.mojor_type_elem(x_type)))
    }
    return(vector_type_from_elem(x_type))
  }

  if (node$kind == "rep_len") {
    x_type <- .mojor_ir_infer_type(node$x, type_env)
    if (.mojor_type_is_array(x_type)) {
      return(vector_type_from_elem(.mojor_type_elem(x_type)))
    }
    return(vector_type_from_elem(x_type))
  }

  if (node$kind == "seq") {
    from_type <- .mojor_ir_infer_type(node$from, type_env)
    to_type <- if (!is.null(node$to)) .mojor_ir_infer_type(node$to, type_env) else from_type
    if (from_type %in% c("i32", "lgl", "bool") && to_type %in% c("i32", "lgl", "bool")) {
      return("i32[]")
    }
    if (from_type %in% c("unknown", "chr") || to_type %in% c("unknown", "chr")) {
      return("unknown")
    }
    return("f64[]")
  }

  if (node$kind == "call") {
 # Special case: length()/nrow()/ncol() return i32
    if (node$fn %in% c("len", "length", "nrow", "ncol")) {
      return("i32")
    }
    if (node$fn %in% c("is.na", "is.nan", "is.finite", "is.infinite", "as.logical")) {
      return("bool")
    }
 # Type query calls return string vectors in compiled lane.
    if (node$fn %in% c("typeof", "mode", "class")) {
      return("chr[]")
    }

 # Math functions - most return f64
    if (node$fn %in% c(
      "sin", "cos", "tan", "asin", "acos", "atan",
      "sinh", "cosh", "tanh", "asinh", "acosh", "atanh",
      "log", "log10", "log1p", "log2", "exp", "expm1",
      "sqrt", "floor", "ceil", "trunc", "round", "abs",
      "sign", "cbrt", "lgamma", "erf", "gamma", "atan2", "hypot", "pow"
    )) {
      return("f64")
    }
    if (node$fn == "abs2") {
      return("f64")
    }
    if (node$fn %in% c("min", "max", "pmin", "pmax") && length(node$args) == 2) {
      lhs_type <- .mojor_ir_infer_type(node$args[[1]], type_env)
      rhs_type <- .mojor_ir_infer_type(node$args[[2]], type_env)
      return(.mojor_type_promote(lhs_type, rhs_type))
    }

 # Other calls: unknown (could add function signature table)
    return("unknown")
  }

 # Set/Match Primitives
  if (node$kind == "unique") {
 # unique(x) returns same type as input
    return(.mojor_ir_infer_type(node$x, type_env))
  }
  if (node$kind == "duplicated") {
    return("lgl[]")
  }
  if (node$kind == "any_duplicated") {
    return("i32")
  }
  if (node$kind == "match") {
    return("i32[]")
  }
  if (node$kind == "in") {
    return("lgl[]")
  }

 # Quantiles & Robust Stats
  if (node$kind %in% c("median", "iqr", "mad")) {
    return("f64")
  }
  if (node$kind == "quantile") {
    return("f64[]")
  }

 # Step 6.3: Type inference for ifelse
  if (node$kind == "ifelse") {
    yes_type <- .mojor_ir_infer_type(node$yes, type_env)
    no_type <- .mojor_ir_infer_type(node$no, type_env)
    if (identical(yes_type, "bool") && identical(no_type, "bool")) {
      return("bool")
    }
 # Type is the promoted type of yes/no branches
    return(.mojor_type_promote(yes_type, no_type))
  }

 # Type metadata expressions (before folding)
  if (node$kind == "type_predicate") {
    return("bool")
  }
  if (node$kind == "type_query") {
    return("chr[]")
  }
  if (node$kind == "dim") {
    x_type <- .mojor_ir_infer_type(node$x, type_env)
    if (is.null(x_type) || identical(x_type, "unknown") || !.mojor_type_is_array(x_type)) {
      return("unknown")
    }
    if (.mojor_type_ndim(x_type) < 2L) {
      return("unknown")
    }
    return("i32[]")
  }
  if (node$kind == "vexpr") {
    len_type <- .mojor_ir_infer_type(node$len, type_env)
    if (!(len_type %in% c("i32", "lgl", "bool"))) {
      return("unknown")
    }
    body_type <- .mojor_ir_infer_type(node$body, type_env)
    if (is.null(body_type) || identical(body_type, "unknown") || .mojor_type_is_array(body_type)) {
      return("unknown")
    }
    if (identical(body_type, "f64")) {
      return("f64[]")
    }
    if (identical(body_type, "f32")) {
      return("f32[]")
    }
    if (identical(body_type, "i32")) {
      return("i32[]")
    }
    if (identical(body_type, "bool")) {
      return("bool[]")
    }
    if (identical(body_type, "lgl")) {
      return("lgl[]")
    }
    return("unknown")
  }

  if (node$kind == "raw") {
 # Raw nodes have unknown type unless they are constructor expressions
    if (!is.null(node$expr) && .mojor_ir_expr_has_constructor(node$expr)) {
      kind <- .mojor_expr_kind(node$expr, type_env)
      if (kind == "float") {
        return("f64")
      }
      if (kind == "int") {
        return("i32")
      }
      if (kind == "bool") {
        return("bool")
      }
    }
    return("unknown")
  }

 # Higher-Order Functions
  if (node$kind == "vapply") {
    if (is.character(node$fun_value_type) && length(node$fun_value_type) == 1) {
      if (node$fun_value_type %in% c("f64", "numeric", "double")) {
        return("f64[]")
      }
      if (node$fun_value_type %in% c("i32", "integer")) {
        return("i32[]")
      }
      if (node$fun_value_type %in% c("lgl", "bool", "logical")) {
        return("lgl[]")
      }
    }
    return("unknown")
  }
  if (node$kind %in% c("sapply", "lapply")) {
    x_type <- .mojor_ir_infer_type(node$x, type_env)
    if (x_type %in% c("f64[]", "i32[]", "lgl[]", "bool[]")) {
      return(x_type)
    }
    return("unknown")
  }
  if (node$kind == "mapply") {
    if (is.list(node$args) && length(node$args) >= 1) {
      first_t <- .mojor_ir_infer_type(node$args[[1]], type_env)
      if (first_t %in% c("f64[]", "i32[]", "lgl[]", "bool[]")) {
        return(first_t)
      }
    }
    return("unknown")
  }

 # Sampling & Permutation
  if (node$kind == "sample_int") {
    n_type <- .mojor_ir_infer_type(node$n, type_env)
    size_type <- .mojor_ir_infer_type(node$size, type_env)
    if (!is_i32_or_integral_literal(node$n, n_type) ||
      !is_i32_or_integral_literal(node$size, size_type)) {
      return("unknown")
    }
    return("i32[]")
  }
  if (node$kind == "sample") {
    x_type <- .mojor_ir_infer_type(node$x, type_env)
    size_type <- .mojor_ir_infer_type(node$size, type_env)
    if (!is_i32_or_integral_literal(node$size, size_type)) {
      return("unknown")
    }
    if (x_type %in% c("f64[]", "i32[]", "lgl[]", "bool[]")) {
      return(x_type)
    }
    return("unknown")
  }

 # String Basics
  if (node$kind == "nchar") {
 # nchar() contract requires chr[] input in strict paths.
    x_type <- .mojor_ir_infer_type(node$x, type_env)
    if (!identical(x_type, "chr[]")) {
      return("unknown")
    }
    return("i32[]")
  }
  if (node$kind == "nzchar") {
 # nzchar() contract requires chr[] input in strict paths.
    x_type <- .mojor_ir_infer_type(node$x, type_env)
    if (!identical(x_type, "chr[]")) {
      return("unknown")
    }
    return("lgl[]")
  }
  if (node$kind == "substr") {
 # substr() contract requires chr[] input and returns character vectors.
    x_type <- .mojor_ir_infer_type(node$x, type_env)
    if (!identical(x_type, "chr[]")) {
      return("unknown")
    }
    return("chr[]")
  }
  if (node$kind == "paste") {
 # paste()/paste0() contract requires chr[] input args and returns character vectors.
    arg_types <- vapply(node$args, function(a) .mojor_ir_infer_type(a, type_env), character(1))
    if (length(arg_types) == 0 || any(arg_types != "chr[]")) {
      return("unknown")
    }
    return("chr[]")
  }

 # Data structures, regex, and table utilities
  if (node$kind == "df_col_exists_guard") {
    return("bool")
  }
  if (node$kind == "df_col_read") {
    if (is.character(node$col_type) && length(node$col_type) == 1 && nzchar(node$col_type)) {
      return(node$col_type)
    }
    return("unknown")
  }
  if (node$kind == "df_make") {
    return("df")
  }
  if (node$kind == "list_make") {
    return("list")
  }
  if (node$kind == "regex_grepl") {
    return("lgl[]")
  }
  if (node$kind == "regex_grep") {
    value_true <- isTRUE(node$value)
    if (is.list(node$value) && identical(node$value$kind, "const")) {
      raw_val <- node$value$value
      if (is.logical(raw_val) && length(raw_val) == 1 && !is.na(raw_val)) {
        value_true <- isTRUE(raw_val)
      } else if (is.character(raw_val) && length(raw_val) == 1) {
        value_true <- identical(toupper(raw_val), "TRUE")
      } else if (is.numeric(raw_val) && length(raw_val) == 1 && !is.na(raw_val) && raw_val %in% c(0, 1)) {
        value_true <- isTRUE(as.logical(raw_val))
      }
    }
    if (isTRUE(value_true)) {
      return("chr[]")
    }
    return("i32[]")
  }
  if (node$kind == "regex_sub") {
    return("chr[]")
  }
  if (node$kind %in% c("row_matrix", "col_matrix")) {
    return("i32[2d]")
  }
  if (node$kind == "expand_grid") {
    return("df")
  }
  if (node$kind == "matmul") {
    lhs_type <- .mojor_ir_infer_type(node$lhs, type_env)
    rhs_type <- .mojor_ir_infer_type(node$rhs, type_env)
    return(matrix_type_from_operands(lhs_type, rhs_type))
  }
  if (node$kind == "crossprod") {
    lhs_type <- .mojor_ir_infer_type(node$lhs, type_env)
    rhs_type <- if (!is.null(node$rhs)) .mojor_ir_infer_type(node$rhs, type_env) else lhs_type
    return(matrix_type_from_operands(lhs_type, rhs_type))
  }
  if (node$kind == "tcrossprod") {
    lhs_type <- .mojor_ir_infer_type(node$lhs, type_env)
    rhs_type <- if (!is.null(node$rhs)) .mojor_ir_infer_type(node$rhs, type_env) else lhs_type
    return(matrix_type_from_operands(lhs_type, rhs_type))
  }
  if (node$kind == "cov") {
    x_type <- .mojor_ir_infer_type(node$x, type_env)
    y_type <- if (!is.null(node$y)) .mojor_ir_infer_type(node$y, type_env) else x_type
    return(cov_cor_result_type_from_operands(x_type, y_type))
  }
  if (node$kind == "cor") {
    x_type <- .mojor_ir_infer_type(node$x, type_env)
    y_type <- if (!is.null(node$y)) .mojor_ir_infer_type(node$y, type_env) else x_type
    return(cov_cor_result_type_from_operands(x_type, y_type))
  }

  return("unknown")
}

.mojor_ir_insert_casts <- function(node, type_env = list()) {
 # Recursively walk IR and insert casts where needed
 # This handles type mismatches in arithmetic, comparisons, and logical ops

  if (is.null(node) || is.null(node$kind)) {
    return(node)
  }

 # Handle binary operators
  if (node$kind == "binop") {
 # Recursively process children first
    lhs <- .mojor_ir_insert_casts(node$lhs, type_env)
    rhs <- .mojor_ir_insert_casts(node$rhs, type_env)

 # Infer operand types
    lhs_type <- .mojor_ir_infer_type(lhs, type_env)
    rhs_type <- .mojor_ir_infer_type(rhs, type_env)

    cast_to_target <- function(expr_node, expr_type, target_type) {
      if (is.null(expr_type) || is.null(target_type) ||
          identical(expr_type, "unknown") || identical(target_type, "unknown") ||
          identical(expr_type, target_type)) {
        return(expr_node)
      }
      if (.mojor_type_is_array(target_type) && !.mojor_type_is_array(expr_type)) {
        target_elem <- .mojor_type_elem(target_type)
        if (identical(expr_type, target_elem)) {
          return(expr_node)
        }
        return(.mojor_ir_cast(target_elem, expr_node))
      }
      .mojor_ir_cast(target_type, expr_node)
    }
    prefer_f32_literal_promotion <- function(lhs_node, rhs_node, lhs_type, rhs_type, target_type) {
      if (!identical(target_type, "f64")) {
        return(target_type)
      }
      lhs_const_f64 <- identical(lhs_type, "f64") && !is.null(lhs_node$kind) && identical(lhs_node$kind, "const")
      rhs_const_f64 <- identical(rhs_type, "f64") && !is.null(rhs_node$kind) && identical(rhs_node$kind, "const")
      if ((identical(lhs_type, "f32") && rhs_const_f64) ||
          (identical(rhs_type, "f32") && lhs_const_f64)) {
        return("f32")
      }
      target_type
    }

 # For arithmetic ops, promote operands to common type
    if (node$op %in% c("+", "-", "*", "/", "%%", "%/%")) {
      target_type <- .mojor_type_promote(lhs_type, rhs_type)
      target_type <- prefer_f32_literal_promotion(lhs, rhs, lhs_type, rhs_type, target_type)
      lhs <- cast_to_target(lhs, lhs_type, target_type)
      rhs <- cast_to_target(rhs, rhs_type, target_type)
    }

 # For comparison ops, ensure operands have same type
    if (node$op %in% c(">", "<", ">=", "<=", "==", "!=")) {
      if (lhs_type != rhs_type && lhs_type != "unknown" && rhs_type != "unknown") {
        target_type <- .mojor_type_promote(lhs_type, rhs_type)
        lhs <- cast_to_target(lhs, lhs_type, target_type)
        rhs <- cast_to_target(rhs, rhs_type, target_type)
      }
    }

 # For logical ops, ensure operands are bool
    if (node$op %in% c("&&", "||")) {
      if (lhs_type != "bool" && lhs_type != "unknown") {
        lhs <- .mojor_ir_bool_coerce(lhs, lhs_type)
      }
      if (rhs_type != "bool" && rhs_type != "unknown") {
        rhs <- .mojor_ir_bool_coerce(rhs, rhs_type)
      }
    }

    node$lhs <- lhs
    node$rhs <- rhs
    return(node)
  }

 # Handle unary operators
  if (node$kind == "unop") {
    node$expr <- .mojor_ir_insert_casts(node$expr, type_env)

 # ! operator expects bool
    if (node$op == "!") {
      expr_type <- .mojor_ir_infer_type(node$expr, type_env)
      if (expr_type != "bool" && expr_type != "unknown") {
        node$expr <- .mojor_ir_bool_coerce(node$expr, expr_type)
      }
    }

    return(node)
  }

 # Handle index expressions
  if (node$kind == "index") {
    node$base <- .mojor_ir_insert_casts(node$base, type_env)
    for (i in seq_along(node$indices)) {
      node$indices[[i]] <- .mojor_ir_insert_casts(node$indices[[i]], type_env)
    }
    return(node)
  }

 # Handle call expressions
  if (node$kind == "call") {
    for (i in seq_along(node$args)) {
      node$args[[i]] <- .mojor_ir_insert_casts(node$args[[i]], type_env)
    }
    return(node)
  }

 # Handle cast nodes (recursively process child)
  if (node$kind == "cast") {
    node$expr <- .mojor_ir_insert_casts(node$expr, type_env)
    return(node)
  }

 # Step 6.3: Handle ifelse expressions
  if (node$kind == "ifelse") {
 # Recursively process all three branches
    node$cond <- .mojor_ir_insert_casts(node$cond, type_env)
    node$yes <- .mojor_ir_insert_casts(node$yes, type_env)
    node$no <- .mojor_ir_insert_casts(node$no, type_env)

 # Ensure condition is bool
    cond_type <- .mojor_ir_infer_type(node$cond, type_env)
    if (cond_type != "bool" && cond_type != "unknown") {
      node$cond <- .mojor_ir_bool_coerce(node$cond, cond_type)
    }

 # Promote yes/no branches to common type
    yes_type <- .mojor_ir_infer_type(node$yes, type_env)
    no_type <- .mojor_ir_infer_type(node$no, type_env)

    if (yes_type != no_type && yes_type != "unknown" && no_type != "unknown") {
      target_type <- .mojor_type_promote(yes_type, no_type)
      if (yes_type != target_type) {
        node$yes <- .mojor_ir_cast(target_type, node$yes)
      }
      if (no_type != target_type) {
        node$no <- .mojor_ir_cast(target_type, node$no)
      }
    }

    return(node)
  }

 # Other node types (const, var, raw) need no transformation
  return(node)
}

# =============================================================================
# Step 3: Typed IR - Statement Type Checking
# =============================================================================

.mojor_ir_type_check_stmt <- function(stmt, type_env = list()) {
 # Type check and transform a statement
 # Inserts casts for assignments and validates types

  if (is.null(stmt) || is.null(stmt$kind)) {
    return(stmt)
  }

  strict_ir <- isTRUE(tryCatch(.mojor_state$options$ir_only, error = function(e) FALSE))
  .tier8_kinds <- c(
    "vapply", "sapply", "lapply", "mapply",
    "nchar", "nzchar", "substr", "paste",
    "sample_int", "sample"
  )
  .tier9_kinds <- c(
    "df_col_read", "df_col_exists_guard", "df_make", "list_make",
    "regex_grepl", "regex_grep", "regex_sub",
    "row_matrix", "col_matrix", "expand_grid"
  )
  .tier_node_label <- function(kind) {
    if (kind %in% .tier8_kinds) {
      return("compiled subset")
    }
    if (kind %in% .tier9_kinds) {
      return("compiled subset")
    }
    NULL
  }
  .is_tier89_typed_node <- function(node) {
    if (is.null(node) || is.null(node$kind)) {
      return(FALSE)
    }
    !is.null(.tier_node_label(node$kind))
  }
  .is_ir_node <- function(node) {
    is.list(node) &&
      !is.null(node$kind) &&
      is.character(node$kind) &&
      length(node$kind) == 1 &&
      nzchar(node$kind)
  }
  .for_each_ir_child <- function(node, fn) {
    if (!is.list(node)) {
      return(invisible(NULL))
    }
    fields <- names(node)
    if (is.null(fields) || length(fields) == 0) {
      return(invisible(NULL))
    }
    for (field in fields) {
      child <- node[[field]]
      if (.is_ir_node(child)) {
        fn(child)
        next
      }
      if (is.list(child)) {
        for (item in child) {
          if (.is_ir_node(item)) fn(item)
        }
      }
    }
    invisible(NULL)
  }
  .require_known_tier89_type <- function(node, inferred, where_label) {
    if (!isTRUE(strict_ir) || !isTRUE(.is_tier89_typed_node(node))) {
      return(invisible(NULL))
    }
    if (is.null(inferred) || !is.character(inferred) || length(inferred) != 1 || !nzchar(inferred)) {
      inferred <- "unknown"
    }
    if (identical(inferred, "unknown")) {
      tier_label <- .tier_node_label(node$kind)
      stop(
        "mojor_transpile: strict IR type check failed: ",
        tier_label, " node '", node$kind, "' has unknown result type in ", where_label,
        call. = FALSE
      )
    }
    invisible(NULL)
  }
  .validate_tier89_expr_contracts <- function(node, where_label) {
    if (!isTRUE(strict_ir) || !.is_ir_node(node)) {
      return(invisible(NULL))
    }
    .walk <- function(cur) {
      if (!.is_ir_node(cur)) {
        return(invisible(NULL))
      }
      if (.is_tier89_typed_node(cur)) {
        tier_label <- .tier_node_label(cur$kind)
        verify_res <- tryCatch(
          .mojor_ir_verify(cur, ctx = list(ir_only = TRUE, type_env = type_env)),
          error = function(e) e
        )
        if (inherits(verify_res, "error")) {
          stop(
            "mojor_transpile: strict IR type check failed: ",
            tier_label, " node '", cur$kind, "' type contract failed in ",
            where_label, ": ", conditionMessage(verify_res),
            call. = FALSE
          )
        }
        inferred <- .mojor_ir_infer_type(cur, type_env)
        .require_known_tier89_type(cur, inferred, where_label)
      }
      .for_each_ir_child(cur, .walk)
      invisible(NULL)
    }
    .walk(node)
    invisible(NULL)
  }

  if (stmt$kind == "assign") {
 # Process RHS with cast insertion
    rhs <- .mojor_ir_insert_casts(stmt$rhs, type_env)
    .validate_tier89_expr_contracts(rhs, "assignment RHS")
    rhs_type <- .mojor_ir_infer_type(rhs, type_env)
    .require_known_tier89_type(rhs, rhs_type, "assignment RHS")

 # Get LHS type and insert cast if needed
    if (stmt$lhs$kind == "var") {
 # Simple variable assignment
      lhs_type <- type_env[[stmt$lhs$name]]
      if (!is.null(lhs_type) && lhs_type != rhs_type &&
        rhs_type != "unknown" && lhs_type != "unknown") {
 # Insert cast to match LHS type
        rhs <- .mojor_ir_cast(lhs_type, rhs)
      }
    } else if (stmt$lhs$kind == "index") {
 # Array element assignment
      base_name <- if (stmt$lhs$base$kind == "var") stmt$lhs$base$name else NULL
      if (!is.null(base_name)) {
        base_type <- type_env[[base_name]]
        if (.mojor_type_is_array(base_type)) {
 # Extract element type from array type
          elem_type <- .mojor_type_elem(base_type)
          if (elem_type != rhs_type && rhs_type != "unknown" && elem_type != "unknown") {
 # Step 8.1: Validate that explicit casts are used for non-trivial conversions
 # Don't auto-insert casts for call expressions (require explicit cast)
            if (rhs$kind == "call" && elem_type == "i32" && rhs_type == "f64") {
              stop("mojor_transpile: integer output requires explicit cast from ", rhs_type, " (use as.integer())")
            }
 # Insert cast to match array element type
            rhs <- .mojor_ir_cast(elem_type, rhs)
          }
        }
      }
 # Process LHS indices (but don't insert casts on them - keep as-is)
      stmt$lhs <- .mojor_ir_insert_casts(stmt$lhs, type_env)
      .validate_tier89_expr_contracts(stmt$lhs, "assignment LHS")
    } else if (stmt$lhs$kind == "subscript") {
 # Slice assignment (mat[i, ] <- c(...))
 # Reject non-scalar/vectorized indices for now.
 # `rows[, cols] <- m` and similar forms need explicit legality checks.
      .is_supported_i32_index_expr <- function(expr_node) {
        if (is.null(expr_node) || is.null(expr_node$kind)) {
          return(FALSE)
        }
        if (identical(expr_node$kind, "var")) {
          expr_type <- .mojor_ir_infer_type(expr_node, type_env)
          return(identical(expr_type, "i32[]"))
        }
        if (expr_node$kind %in% c("c", "rep", "rep_len", "seq")) {
          return(TRUE)
        }
        if (identical(expr_node$kind, "cast") && !is.null(expr_node$expr)) {
          return(.is_supported_i32_index_expr(expr_node$expr))
        }
        if (identical(expr_node$kind, "unop") && !is.null(expr_node$expr)) {
          return(.is_supported_i32_index_expr(expr_node$expr))
        }
        if (identical(expr_node$kind, "binop") && !is.null(expr_node$lhs) && !is.null(expr_node$rhs)) {
          if (!(expr_node$op %in% c("+", "-", "*", "%/%", "%%"))) {
            return(FALSE)
          }
          lhs_vec <- .is_supported_i32_index_expr(expr_node$lhs)
          rhs_vec <- .is_supported_i32_index_expr(expr_node$rhs)
          lhs_type <- .mojor_ir_infer_type(expr_node$lhs, type_env)
          rhs_type <- .mojor_ir_infer_type(expr_node$rhs, type_env)
          lhs_scalar <- lhs_type %in% c("i32", "lgl", "bool")
          rhs_scalar <- rhs_type %in% c("i32", "lgl", "bool")
          return((lhs_vec && rhs_vec) || (lhs_vec && rhs_scalar) || (rhs_vec && lhs_scalar))
        }
        if (identical(expr_node$kind, "ifelse") &&
          !is.null(expr_node$cond) && !is.null(expr_node$yes) && !is.null(expr_node$no)) {
          yes_vec <- .is_supported_i32_index_expr(expr_node$yes)
          no_vec <- .is_supported_i32_index_expr(expr_node$no)
          yes_type <- .mojor_ir_infer_type(expr_node$yes, type_env)
          no_type <- .mojor_ir_infer_type(expr_node$no, type_env)
          yes_scalar <- yes_type %in% c("i32", "lgl", "bool")
          no_scalar <- no_type %in% c("i32", "lgl", "bool")
          cond_type <- .mojor_ir_infer_type(expr_node$cond, type_env)
          cond_ok <- cond_type %in% c("lgl", "bool", "lgl[]", "bool[]")
          return(cond_ok && ((yes_vec && no_vec) || (yes_vec && no_scalar) || (no_vec && yes_scalar)))
        }
        if (identical(expr_node$kind, "call") &&
          !is.null(expr_node$fn) &&
          expr_node$fn %in% c("min", "max") &&
          is.list(expr_node$args) &&
          length(expr_node$args) == 2) {
          lhs_node <- expr_node$args[[1]]
          rhs_node <- expr_node$args[[2]]
          lhs_vec <- .is_supported_i32_index_expr(lhs_node)
          rhs_vec <- .is_supported_i32_index_expr(rhs_node)
          lhs_type <- .mojor_ir_infer_type(lhs_node, type_env)
          rhs_type <- .mojor_ir_infer_type(rhs_node, type_env)
          lhs_scalar <- lhs_type %in% c("i32", "lgl", "bool")
          rhs_scalar <- rhs_type %in% c("i32", "lgl", "bool")
          return((lhs_vec && rhs_vec) || (lhs_vec && rhs_scalar) || (rhs_vec && lhs_scalar))
        }
        FALSE
      }
      for (idx in stmt$lhs$indices) {
        if (is.null(idx$kind)) next
        idx_node <- idx
        if (identical(idx$kind, "scalar_index") && !is.null(idx$expr)) {
          idx_node <- idx$expr
        }
        if (idx_node$kind %in% c("missing_index", "slice_index")) next
        idx_type <- .mojor_ir_infer_type(idx_node, type_env)
        if (!is.null(idx_type) && .mojor_type_is_array(idx_type)) {
          if (identical(idx_node$kind, "var") &&
              (idx_type %in% c("i32[]", "chr[]") || .mojor_is_logical_mask_type(idx_type))) {
            next
          }
          if (idx_type %in% c("i32[]", "f64[]") && .is_supported_i32_index_expr(idx_node)) {
            next
          }
          if (idx_type %in% c("i32[]", "f64[]", "chr[]") &&
            idx_node$kind %in% c("c", "rep", "rep_len", "seq")) {
            next
          }
          stop(
            "mojor_transpile: unsupported subscript assignment index type; expected scalar index or supported vector index in assignment",
            call. = FALSE
          )
        }
      }

 # Slice assignment (mat[i, ] <- c(...))
 # subscript has var:string instead of base:node
      base_name <- stmt$lhs$var
      if (!is.null(base_name)) {
        base_type <- type_env[[base_name]]
        if (.mojor_type_is_array(base_type)) {
 # For slice assignments with c() constructor RHS, type checking is handled
 # during emission when the constructor is expanded into the loop
 # Just process the RHS to insert any needed casts
          stmt$rhs <- rhs
          return(stmt)
        }
      }
    }

    stmt$rhs <- rhs
    return(stmt)
  }

  if (stmt$kind == "if") {
 # Process condition with cast insertion
    cond <- .mojor_ir_insert_casts(stmt$cond, type_env)
    .validate_tier89_expr_contracts(cond, "if condition")
    cond_type <- .mojor_ir_infer_type(cond, type_env)

 # If condition is not bool, coerce it
    if (cond_type != "bool" && cond_type != "unknown") {
      cond <- .mojor_ir_bool_coerce(cond, cond_type)
    }

    stmt$cond <- cond

 # Process then and else blocks
    stmt$then <- .mojor_ir_type_check_block(stmt$then, type_env)
    if (!is.null(stmt$else_block)) {
      stmt$else_block <- .mojor_ir_type_check_block(stmt$else_block, type_env)
    }

    return(stmt)
  }

  if (stmt$kind == "loop") {
    if (!is.null(stmt$range)) {
      .validate_tier89_expr_contracts(stmt$range, "loop range")
    }
 # Loop variable is always i32
    new_env <- type_env
    new_env[[stmt$var]] <- "i32"

 # Process loop body with extended environment
    stmt$body <- .mojor_ir_type_check_block(stmt$body, new_env)

 # Reduction detection: skip if already annotated by builder
    if (is.null(stmt$reduce)) {
      pattern <- .mojor_ir_detect_reduction_pattern(stmt)
      if (!is.null(pattern)) stmt$reduce <- pattern
    }

    return(stmt)
  }

 # Step 8.2: While loop type checking
  if (stmt$kind == "while") {
 # Process condition with cast insertion
    cond <- .mojor_ir_insert_casts(stmt$cond, type_env)
    .validate_tier89_expr_contracts(cond, "while condition")
    cond_type <- .mojor_ir_infer_type(cond, type_env)

 # If condition is not bool, coerce it
    if (cond_type != "bool" && cond_type != "unknown") {
      cond <- .mojor_ir_bool_coerce(cond, cond_type)
    }

    stmt$cond <- cond

 # Process loop body (no loop variable to add to environment)
    stmt$body <- .mojor_ir_type_check_block(stmt$body, type_env)

    return(stmt)
  }

 # Step 8.7: Repeat loop type checking
  if (stmt$kind == "repeat") {
 # Process loop body (no condition or loop variable)
    stmt$body <- .mojor_ir_type_check_block(stmt$body, type_env)
    return(stmt)
  }

  if (stmt$kind == "block") {
 # Process all statements in block
    return(.mojor_ir_type_check_block(stmt, type_env))
  }

 # Step 8.2: Break and next need no type checking, pass through
  if (stmt$kind %in% c("break", "next")) {
    return(stmt)
  }

 # Step 8.8: Return statement type checking
  if (stmt$kind == "return") {
    if (!is.null(stmt$value)) {
      stmt$value <- .mojor_ir_insert_casts(stmt$value, type_env)
      .validate_tier89_expr_contracts(stmt$value, "return")
      ret_type <- .mojor_ir_infer_type(stmt$value, type_env)
      .require_known_tier89_type(stmt$value, ret_type, "return")
    }
    return(stmt)
  }

 # Step 19: Scalar reduction - no sub-expressions to check
  if (stmt$kind == "scalar_reduce") {
    return(stmt)
  }
  if (stmt$kind == "scheduled_reduce") {
    return(stmt)
  }

 # Other statement types (raw, etc.) - return as-is
  return(stmt)
}
