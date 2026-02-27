# =============================================================================
# Expression-Only Transpiler (Step 1a MVP)
# =============================================================================
# Handles standalone reduction compilation: mojor_fn(function(x) sum(x))

# Shared expression operation allowlists/imports are defined in
# transpile/expression_registry.R and consumed from this file.

.mojor_expr_only_single_expr <- function(blocks) {
  if (length(blocks) != 1) {
    return(NULL)
  }
  block <- blocks[[1]]
  if (is.call(block) && as.character(block[[1]]) == "return" && length(block) >= 2) {
    block <- block[[2]]
  }
  block
}

.mojor_expr_only_single_call <- function(blocks) {
  block <- .mojor_expr_only_single_expr(blocks)
  if (!is.call(block)) {
    return(NULL)
  }
  block
}

.mojor_expr_only_call_name <- function(call_expr) {
  if (!is.call(call_expr) || length(call_expr) < 1) {
    return(NULL)
  }
  op <- as.character(call_expr[[1]])
  if (!is.character(op) || length(op) != 1) {
    return(NULL)
  }
  op
}

.mojor_expr_only_single_supported_call <- function(blocks, supported_ops) {
  block <- .mojor_expr_only_single_call(blocks)
  if (is.null(block)) {
    return(NULL)
  }
  op_name <- .mojor_expr_only_call_name(block)
  if (is.null(op_name) || !(op_name %in% supported_ops)) {
    return(NULL)
  }
  list(block = block, op_name = op_name)
}

.mojor_expr_only_call_parts <- function(call_expr) {
  parts <- as.list(call_expr)[-1]
  nms <- names(parts)
  if (is.null(nms)) nms <- rep("", length(parts))
  list(parts = parts, nms = nms)
}

.mojor_expr_only_named_args <- function(nms) {
  if (length(nms) == 0L) {
    return(character(0))
  }
  nms[nzchar(nms)]
}

.mojor_expr_only_bad_named_args <- function(nms, allowed) {
  setdiff(.mojor_expr_only_named_args(nms), allowed)
}

.mojor_expr_only_get_arg <- function(parts, nms, name, pos, aliases = character(0)) {
  all_names <- c(name, aliases)
  for (nm in all_names) {
    if (nm %in% nms) {
      return(parts[[which(nms == nm)[1]]])
    }
  }
  if (length(parts) >= pos && !nzchar(nms[[pos]])) {
    return(parts[[pos]])
  }
  NULL
}

.mojor_expr_only_parse_bool_literal <- function(expr, error_message, default = FALSE) {
  if (is.null(expr)) {
    return(default)
  }
  if (is.logical(expr) && length(expr) == 1 && !is.na(expr)) {
    return(isTRUE(expr))
  }
  if (is.numeric(expr) && length(expr) == 1 && !is.na(expr) && expr %in% c(0, 1)) {
    return(as.logical(expr))
  }
  if (is.name(expr) && identical(as.character(expr), "TRUE")) {
    return(TRUE)
  }
  if (is.name(expr) && identical(as.character(expr), "FALSE")) {
    return(FALSE)
  }
  stop(error_message)
}

.mojor_expr_only_is_null_expr <- function(expr) {
  is.null(expr) || (is.name(expr) && identical(as.character(expr), "NULL"))
}

.mojor_expr_only_require_direct_name <- function(expr, op_name, arg_name, mode_label) {
  if (!is.name(expr)) {
    stop(
      "mojor_transpile: ", op_name, "() requires ", arg_name,
      " to be a direct ", mode_label, " argument in expression-only mode"
    )
  }
  invisible(NULL)
}

.mojor_expr_only_parse_regex_flags <- function(parts, nms, op_name, fixed_pos, perl_pos) {
  fixed_expr <- .mojor_expr_only_get_arg(parts, nms, "fixed", fixed_pos)
  perl_expr <- .mojor_expr_only_get_arg(parts, nms, "perl", perl_pos)
  literal_bool_or_na <- function(expr, default) {
    if (is.null(expr)) {
      return(default)
    }
    if (is.logical(expr) && length(expr) == 1 && !is.na(expr)) {
      return(isTRUE(expr))
    }
    if (is.numeric(expr) && length(expr) == 1 && !is.na(expr) && expr %in% c(0, 1)) {
      return(isTRUE(as.logical(expr)))
    }
    if (is.name(expr) && identical(as.character(expr), "TRUE")) {
      return(TRUE)
    }
    if (is.name(expr) && identical(as.character(expr), "FALSE")) {
      return(FALSE)
    }
    NA
  }
  fixed_lit <- literal_bool_or_na(fixed_expr, FALSE)
  perl_lit <- literal_bool_or_na(perl_expr, TRUE)
  if (!is.na(fixed_lit) && !is.na(perl_lit) && isTRUE(fixed_lit) && isTRUE(perl_lit)) {
    stop("mojor_transpile: ", op_name, "() does not support fixed=TRUE with perl=TRUE")
  }
  list(fixed_expr = fixed_expr, perl_expr = perl_expr)
}

.mojor_expr_only_false_result <- function(flag_name) {
  out <- list(FALSE)
  names(out) <- flag_name
  out
}

.mojor_expr_only_supported_call_or_false <- function(blocks, supported_ops, flag_name) {
  call_info <- .mojor_expr_only_single_supported_call(blocks, supported_ops)
  if (is.null(call_info)) {
    return(.mojor_expr_only_false_result(flag_name))
  }
  call_info
}

.mojor_expr_only_array_ptr_info <- function(array_type, error_message = NULL) {
  key <- if (identical(array_type, "lgl[]")) "i32[]" else array_type
  info <- switch(key,
    "f64[]" = c("ImmutFloat64Ptr", "MutFloat64Ptr", "Float64"),
    "f32[]" = c("ImmutFloat32Ptr", "MutFloat32Ptr", "Float32"),
    "i32[]" = c("ImmutInt32Ptr", "MutInt32Ptr", "Int32"),
    NULL
  )
  if (!is.null(info)) {
    return(list(immut_ptr = info[[1]], mut_ptr = info[[2]], cast = info[[3]]))
  }
  if (!is.null(error_message)) stop(error_message)
  NULL
}

.mojor_expr_only_numeric_array_type_info <- function(array_type, error_message) {
  suffix <- switch(array_type,
    "i32[]" = "i32",
    "f32[]" = "f32",
    "f64[]" = "f64",
    NULL
  )
  if (is.null(suffix)) stop(error_message)
  ptr <- .mojor_expr_only_array_ptr_info(array_type)
  list(suffix = suffix, immut_ptr = ptr$immut_ptr, mut_ptr = ptr$mut_ptr, cast = ptr$cast)
}

.mojor_expr_only_pointer_preamble <- function(header,
                                              imports = character(0),
                                              include_mut_opaque = FALSE,
                                              include_immut_types = c("f64", "i32"),
                                              include_mut_types = c("f64", "i32"),
                                              include_mut_u64 = FALSE) {
  base_name <- c(f64 = "Float64", f32 = "Float32", i32 = "Int32", u64 = "UInt64")
  ptr_decl <- function(type_id, mut = FALSE) {
    base <- base_name[[type_id]]
    if (is.null(base) || (identical(type_id, "u64") && !isTRUE(mut))) {
      stop("mojor_transpile: unsupported pointer preamble type id: ", type_id)
    }
    qual <- if (isTRUE(mut)) "Mut" else "Immut"
    suffix <- if (identical(type_id, "u64")) "U64" else base
    origin <- if (isTRUE(mut)) "MutAnyOrigin" else "ImmutAnyOrigin"
    mut_val <- if (isTRUE(mut)) "True" else "False"
    paste0(
      "comptime ", qual, suffix, "Ptr = UnsafePointer[mut=", mut_val,
      ", type=", base, ", origin=", origin, "]"
    )
  }

  preamble <- c(
    header,
    "from memory import OpaquePointer, UnsafePointer",
    imports,
    "comptime ImmutOpaqueAny = OpaquePointer[mut=False, origin=ImmutAnyOrigin]"
  )
  if (isTRUE(include_mut_opaque)) {
    preamble <- c(preamble, "comptime MutOpaqueAny = OpaquePointer[mut=True, origin=MutAnyOrigin]")
  }
  if (length(include_immut_types) > 0) {
    preamble <- c(preamble, vapply(include_immut_types, ptr_decl, character(1), mut = FALSE))
  }
  if (length(include_mut_types) > 0) {
    preamble <- c(preamble, vapply(include_mut_types, ptr_decl, character(1), mut = TRUE))
  }
  if (isTRUE(include_mut_u64)) {
    preamble <- c(preamble, ptr_decl("u64", mut = TRUE))
  }
  c(preamble, "")
}

.mojor_expr_only_bind_array_ptr_len <- function(name, ptr_type, cast_type, len_var = NULL, len_param = NULL) {
  if (is.null(len_var)) len_var <- paste0("n_", name)
  if (is.null(len_param)) len_param <- paste0("__mojor_len_", name)
  c(
    paste0("    var ", name, ": ", ptr_type, " = ", name, "_ptr.bitcast[", cast_type, "]()"),
    paste0("    var ", len_var, " = Int(", len_param, ")")
  )
}

# Helper: Detect standalone reduction in expression-only mode
.mojor_detect_standalone_reduction <- function(blocks) {
  supported_reductions <- .mojor_expr_registry_standalone_reductions()
  call_info <- .mojor_expr_only_supported_call_or_false(blocks, supported_reductions, "is_reduction")
  if (is.null(call_info$block)) {
    return(call_info)
  }
  block <- call_info$block
  fn_name <- call_info$op_name

 # Extract the argument
  if (length(block) < 2) {
    stop("mojor_transpile: ", fn_name, "() requires an argument")
  }

  arg_expr <- block[[2]]

  list(
    is_reduction = TRUE,
    fn_name = fn_name,
    arg = arg_expr
  )
}

# Helper: Detect matrix multiplication operations
.mojor_detect_matrix_operation <- function(blocks) {
  call_info <- .mojor_expr_only_supported_call_or_false(
    blocks,
    c("%*%", "crossprod", "tcrossprod"),
    "is_matmul"
  )
  if (is.null(call_info$block)) {
    return(call_info)
  }
  block <- call_info$block
  op_name <- call_info$op_name

 # Check for %*% operator
  if (op_name == "%*%") {
    if (length(block) != 3) {
      stop("mojor_transpile: %*% requires exactly two arguments")
    }
    return(list(
      is_matmul = TRUE,
      operation = "matmul",
      lhs = block[[2]],
      rhs = block[[3]]
    ))
  }

  if (length(block) < 2) {
    stop("mojor_transpile: ", op_name, "() requires at least one argument")
  }
  if (length(block) > 3) {
    stop("mojor_transpile: ", op_name, "() accepts at most two arguments")
  }
  list(
    is_matmul = TRUE,
    operation = op_name,
    lhs = block[[2]],
    rhs = if (length(block) == 3) block[[3]] else NULL
  )
}

# Helper: Detect covariance/correlation operations (PR-B7.2)
.mojor_detect_cov_cor_operation <- function(blocks) {
  call_info <- .mojor_expr_only_supported_call_or_false(blocks, c("cov", "cor"), "is_cov_cor")
  if (is.null(call_info$block)) {
    return(call_info)
  }
  block <- call_info$block
  op_name <- call_info$op_name

  if (length(block) < 2) {
    stop("mojor_transpile: ", op_name, "() requires at least one argument")
  }
  if (length(block) > 3) {
    stop("mojor_transpile: ", op_name, "() accepts at most two arguments")
  }
  list(
    is_cov_cor = TRUE,
    operation = op_name,
    x = block[[2]],
    y = if (length(block) == 3) block[[3]] else NULL
  )
}

# Helper: Detect set/match operations ()
.mojor_detect_set_match_operation <- function(blocks) {
  call_info <- .mojor_expr_only_supported_call_or_false(
    blocks,
    c("unique", "duplicated", "anyDuplicated", "match", "%in%"),
    "is_set_match"
  )
  if (is.null(call_info$block)) {
    return(call_info)
  }
  block <- call_info$block
  op_name <- call_info$op_name

  if (op_name %in% c("unique", "duplicated", "anyDuplicated")) {
    if (length(block) != 2) {
      stop("mojor_transpile: ", op_name, "() requires exactly one argument")
    }
    return(list(
      is_set_match = TRUE,
      operation = op_name,
      x = block[[2]],
      table = NULL
    ))
  }

  if (op_name %in% c("match", "%in%")) {
    if (length(block) != 3) {
      stop("mojor_transpile: ", op_name, "() requires exactly two arguments")
    }
    return(list(
      is_set_match = TRUE,
      operation = op_name,
      x = block[[2]],
      table = block[[3]]
    ))
  }
}

# Helper: Detect quantile/robust-stat operations ()
.mojor_detect_quantile_operation <- function(blocks) {
  call_info <- .mojor_expr_only_supported_call_or_false(
    blocks,
    c("median", "quantile", "IQR", "mad"),
    "is_quantile"
  )
  if (is.null(call_info$block)) {
    return(call_info)
  }
  block <- call_info$block
  op_name <- call_info$op_name

  if (op_name %in% c("median", "IQR", "mad")) {
    if (length(block) != 2) {
      stop("mojor_transpile: ", op_name, "() requires exactly one argument")
    }
    return(list(
      is_quantile = TRUE,
      operation = op_name,
      x = block[[2]],
      probs = NULL,
      na_rm = FALSE,
      type = 7L
    ))
  }

  call_parts <- .mojor_expr_only_call_parts(block)
  parts <- call_parts$parts
  nms <- call_parts$nms

  x_expr <- .mojor_expr_only_get_arg(parts, nms, "x", 1)
  probs_expr <- .mojor_expr_only_get_arg(parts, nms, "probs", 2)
  if (is.null(x_expr) || is.null(probs_expr)) {
    stop("mojor_transpile: quantile() requires x and probs arguments")
  }

  na_rm <- FALSE
  na_rm_expr <- .mojor_expr_only_get_arg(parts, nms, "na.rm", 3)
  if (!is.null(na_rm_expr)) {
    if (!is.logical(na_rm_expr) || length(na_rm_expr) != 1 || is.na(na_rm_expr)) {
      stop("mojor_transpile: quantile() na.rm must be TRUE or FALSE in expression-only mode")
    }
    na_rm <- isTRUE(na_rm_expr)
  }

  type <- 7L
  type_expr <- .mojor_expr_only_get_arg(parts, nms, "type", 4)
  if (!is.null(type_expr)) {
    if (!(is.numeric(type_expr) || is.integer(type_expr)) || length(type_expr) != 1 || is.na(type_expr)) {
      stop("mojor_transpile: quantile() type must be a scalar integer in [1, 8] in expression-only mode")
    }
    type <- as.integer(type_expr)
    if (type < 1L || type > 8L) {
      stop("mojor_transpile: quantile() type must be in [1, 8]")
    }
  }

  list(
    is_quantile = TRUE,
    operation = op_name,
    x = x_expr,
    probs = probs_expr,
    na_rm = na_rm,
    type = type
  )
}

# Helper: Detect string operations (strict native subset: nchar/nzchar/substr/substring/paste/paste0)
.mojor_detect_string_operation <- function(blocks) {
  call_info <- .mojor_expr_only_supported_call_or_false(
    blocks,
    c("nchar", "nzchar", "substr", "substring", "paste", "paste0"),
    "is_string"
  )
  if (is.null(call_info$block)) {
    return(call_info)
  }
  block <- call_info$block
  op_name <- call_info$op_name

  call_parts <- .mojor_expr_only_call_parts(block)
  parts <- call_parts$parts
  nms <- call_parts$nms
  if (op_name %in% c("nchar", "nzchar")) {
    bad_named <- .mojor_expr_only_bad_named_args(nms, "x")
    if (length(bad_named) > 0 || length(parts) != 1) {
      stop("mojor_transpile: ", op_name, "() currently supports only ", op_name, "(x) in expression-only mode")
    }
    x_expr <- .mojor_expr_only_get_arg(parts, nms, "x", 1)
    if (is.null(x_expr)) {
      stop("mojor_transpile: ", op_name, "() requires x in expression-only mode")
    }
    .mojor_expr_only_require_direct_name(x_expr, op_name, "x", "character vector")
    return(list(
      is_string = TRUE,
      operation = op_name,
      x_expr = x_expr
    ))
  }

  if (op_name %in% c("substr", "substring")) {
    op_label <- if (identical(op_name, "substring")) "substring" else "substr"
    allowed_named <- if (identical(op_name, "substring")) c("text", "first", "last") else c("x", "start", "stop")
    bad_named <- .mojor_expr_only_bad_named_args(nms, allowed_named)
    if (length(bad_named) > 0 || length(parts) > 3) {
      stop(
        "mojor_transpile: ", op_label,
        "() currently supports only ", op_label,
        if (identical(op_name, "substring")) "(text, first, last)" else "(x, start, stop)",
        " in expression-only mode"
      )
    }
    x_expr <- if (identical(op_name, "substring")) {
      .mojor_expr_only_get_arg(parts, nms, "text", 1, aliases = "x")
    } else {
      .mojor_expr_only_get_arg(parts, nms, "x", 1)
    }
    start_expr <- if (identical(op_name, "substring")) {
      .mojor_expr_only_get_arg(parts, nms, "first", 2, aliases = "start")
    } else {
      .mojor_expr_only_get_arg(parts, nms, "start", 2)
    }
    stop_expr <- if (identical(op_name, "substring")) {
      .mojor_expr_only_get_arg(parts, nms, "last", 3, aliases = "stop")
    } else {
      .mojor_expr_only_get_arg(parts, nms, "stop", 3)
    }
    if (is.null(x_expr) || is.null(start_expr) || is.null(stop_expr)) {
      stop(
        "mojor_transpile: ", op_label,
        "() requires ",
        if (identical(op_name, "substring")) "text, first, and last" else "x, start, and stop",
        " in expression-only mode"
      )
    }
    .mojor_expr_only_require_direct_name(
      x_expr,
      op_label,
      if (identical(op_name, "substring")) "text" else "x",
      "character vector"
    )
    return(list(
      is_string = TRUE,
      operation = "substr",
      x_expr = x_expr,
      start_expr = start_expr,
      stop_expr = stop_expr
    ))
  }

  if (op_name %in% c("paste", "paste0")) {
    op_label <- op_name
    allowed_named <- if (identical(op_name, "paste")) c("x", "y", "sep", "collapse") else c("x", "y", "collapse")
    bad_named <- .mojor_expr_only_bad_named_args(nms, allowed_named)
    if (length(bad_named) > 0 || length(parts) < 2 || length(parts) > if (identical(op_name, "paste")) 4 else 3) {
      stop(
        "mojor_transpile: ", op_label, "() currently supports only ",
        if (identical(op_name, "paste")) "paste(x, y, sep = \"...\", collapse = \"...\"|NULL)" else "paste0(x, y, collapse = \"...\"|NULL)",
        " in expression-only mode"
      )
    }

    x_expr <- .mojor_expr_only_get_arg(parts, nms, "x", 1)
    y_expr <- .mojor_expr_only_get_arg(parts, nms, "y", 2)
    if (is.null(x_expr) || is.null(y_expr)) {
      stop("mojor_transpile: ", op_label, "() requires x and y in expression-only mode")
    }
    .mojor_expr_only_require_direct_name(x_expr, op_label, "x", "character vector")
    .mojor_expr_only_require_direct_name(y_expr, op_label, "y", "character vector")

    sep_value <- if (identical(op_name, "paste")) " " else ""
    if (identical(op_name, "paste")) {
      sep_expr <- .mojor_expr_only_get_arg(parts, nms, "sep", 3)
      if (!is.null(sep_expr)) {
        if (!(is.character(sep_expr) && length(sep_expr) == 1 && !is.na(sep_expr))) {
          stop("mojor_transpile: paste() sep must be a scalar character literal in expression-only mode")
        }
        sep_value <- as.character(sep_expr[[1]])
      }
    }
    collapse_value <- NULL
    collapse_expr <- .mojor_expr_only_get_arg(
      parts,
      nms,
      "collapse",
      if (identical(op_name, "paste")) 4 else 3
    )
    if (!is.null(collapse_expr)) {
      if (is.name(collapse_expr) && identical(as.character(collapse_expr), "NULL")) {
        collapse_value <- NULL
      } else if (is.character(collapse_expr) && length(collapse_expr) == 1 && !is.na(collapse_expr)) {
        collapse_value <- as.character(collapse_expr[[1]])
      } else {
        stop("mojor_transpile: ", op_label, "() collapse must be a scalar character literal or NULL in expression-only mode")
      }
    }

    return(list(
      is_string = TRUE,
      operation = op_name,
      x_expr = x_expr,
      y_expr = y_expr,
      sep = sep_value,
      collapse = collapse_value
    ))
  }

  stop(
    "mojor_transpile: compiled subset strict expression-only currently supports only ",
    "nchar(x), nzchar(x), substr()/substring(), paste()/paste0()"
  )
}

.mojor_dim_index_expr_to_c <- function(idx_expr, args, types) {
  deps <- character(0)

  arg_type <- function(nm) {
    idx <- which(args == nm)
    if (length(idx) != 1L) {
      return(NULL)
    }
    types[[idx]]
  }

  supported_scalar_spec <- function(spec) {
    identical(spec, "i32") || spec %in% c("f64", "f32", "lgl", "bool")
  }

  emit <- function(node) {
    if ((is.integer(node) || is.numeric(node)) && length(node) == 1 && !is.na(node) && is.finite(node)) {
      val <- as.numeric(node)
      if (!isTRUE(all.equal(val, as.integer(val), tolerance = 0))) {
        stop("mojor_transpile: dim() index expression must be integer-valued in expression-only mode")
      }
      ival <- as.integer(val)
      if (is.na(ival)) {
        stop("mojor_transpile: dim() index expression literal is outside i32 range in expression-only mode")
      }
      return(as.character(ival))
    }

    if (is.name(node)) {
      nm <- as.character(node)
      if (!(nm %in% args)) {
        stop("mojor_transpile: dim() index expression argument '", nm, "' is not a function parameter")
      }
      spec <- arg_type(nm)
      if (!supported_scalar_spec(spec)) {
        stop("mojor_transpile: dim() index expression argument '", nm, "' must be scalar i32/f64/f32/lgl/bool in expression-only mode")
      }
      deps <<- unique(c(deps, nm))
      return(paste0("__mojor_dim_idx_", nm))
    }

    if (!is.call(node)) {
      stop("mojor_transpile: dim() index in expression-only mode must be a positive integer literal, scalar argument, or supported scalar expression")
    }

    op <- as.character(node[[1]])
    if (op == "(" && length(node) == 2) {
      return(emit(node[[2]]))
    }
    if (op == "+" && length(node) == 2) {
      return(paste0("(+(", emit(node[[2]]), "))"))
    }
    if (op == "-" && length(node) == 2) {
      return(paste0("(-(", emit(node[[2]]), "))"))
    }
    if (op == "as.integer" && length(node) == 2) {
      return(paste0("(int)(", emit(node[[2]]), ")"))
    }
    if (op == "abs" && length(node) == 2) {
      inner <- emit(node[[2]])
      return(paste0("(((", inner, ") < 0) ? (-(", inner, ")) : (", inner, "))"))
    }
    if (op %in% c("+", "-", "*", "%/%", "%%") && length(node) == 3) {
      lhs <- emit(node[[2]])
      rhs <- emit(node[[3]])
      op_c <- if (identical(op, "%/%")) "/" else op
      return(paste0("((", lhs, ") ", op_c, " (", rhs, "))"))
    }
    if (op %in% c("min", "max") && length(node) >= 3) {
      vals <- lapply(as.list(node)[-1], emit)
      acc <- vals[[1]]
      if (length(vals) >= 2) {
        for (i in 2:length(vals)) {
          rhs <- vals[[i]]
          if (identical(op, "min")) {
            acc <- paste0("(((", acc, ") < (", rhs, ")) ? (", acc, ") : (", rhs, "))")
          } else {
            acc <- paste0("(((", acc, ") > (", rhs, ")) ? (", acc, ") : (", rhs, "))")
          }
        }
      }
      return(acc)
    }

    stop(
      "mojor_transpile: dim() index expression in expression-only mode supports only integer-valued scalar literals/args with +, -, *, %/%, %%, unary +/-, abs(), as.integer(), min(), max()"
    )
  }

  list(c_expr = emit(idx_expr), arg_names = deps)
}

# Helper: Detect dim() metadata query operation
.mojor_detect_dim_operation <- function(blocks, args, types) {
  expr <- .mojor_expr_only_single_expr(blocks)
  if (is.null(expr)) {
    # Support no-loop let-bindings: d <- dim(x); d
    expr <- .mojor_inline_let_bindings(blocks)
  }
  if (is.call(expr) && as.character(expr[[1]]) == "return" && length(expr) >= 2) {
    expr <- expr[[2]]
  }
  if (is.null(expr) || !is.call(expr)) {
    return(list(is_dim = FALSE))
  }
  parse_dim_x <- function(dim_call) {
    if (!(is.call(dim_call) && identical(.mojor_expr_only_call_name(dim_call), "dim"))) {
      return(NULL)
    }
    call_parts <- .mojor_expr_only_call_parts(dim_call)
    parts <- call_parts$parts
    nms <- call_parts$nms
    bad_named <- .mojor_expr_only_bad_named_args(nms, "x")
    if (length(bad_named) > 0 || length(parts) != 1) {
      stop("mojor_transpile: dim() currently supports only dim(x), dim(x)[k], or length(dim(x)) in expression-only mode")
    }
    x_expr <- .mojor_expr_only_get_arg(parts, nms, "x", 1)
    if (is.null(x_expr)) {
      stop("mojor_transpile: dim() requires x in expression-only mode")
    }
    .mojor_expr_only_require_direct_name(x_expr, "dim", "x", "array")
    x_expr
  }
  parse_dim_index <- function(idx_expr) {
    supported_scalar_spec <- function(spec) {
      identical(spec, "i32") || spec %in% c("f64", "f32", "lgl", "bool")
    }
    if ((is.integer(idx_expr) || is.numeric(idx_expr)) && length(idx_expr) == 1) {
      idx_val <- as.integer(idx_expr)
      if (!is.na(idx_val) && abs(idx_expr - idx_val) < 1e-12 && idx_val >= 1L) {
        return(list(kind = "const", value = as.integer(idx_val)))
      }
    }
    if (is.name(idx_expr)) {
      idx_name <- as.character(idx_expr)
      if (!(idx_name %in% args)) {
        stop("mojor_transpile: dim() index argument '", idx_name, "' is not a function parameter")
      }
      idx_type <- types[[which(args == idx_name)]]
      if (!supported_scalar_spec(idx_type)) {
        stop("mojor_transpile: dim() index argument '", idx_name, "' must be scalar i32/f64/f32/lgl/bool in expression-only mode")
      }
      return(list(kind = "arg", name = idx_name))
    }
    idx_c <- .mojor_dim_index_expr_to_c(idx_expr, args, types)
    return(list(kind = "expr", c_expr = idx_c$c_expr, arg_names = idx_c$arg_names))
  }

  op_name <- .mojor_expr_only_call_name(expr)
  if (identical(op_name, "dim")) {
    x_expr <- parse_dim_x(expr)
    if (is.null(x_expr)) {
      return(list(is_dim = FALSE))
    }
    return(list(
      is_dim = TRUE,
      operation = "dim_vector",
      x_expr = x_expr
    ))
  }
  if (op_name %in% c("[", "[[") && length(expr) == 3) {
    base <- expr[[2]]
    idx_expr <- expr[[3]]
    x_expr <- parse_dim_x(base)
    if (is.null(x_expr)) {
      return(list(is_dim = FALSE))
    }
    return(list(
      is_dim = TRUE,
      operation = "dim_index",
      x_expr = x_expr,
      idx_info = parse_dim_index(idx_expr)
    ))
  }
  if (identical(op_name, "length") && length(expr) == 2) {
    x_expr <- parse_dim_x(expr[[2]])
    if (is.null(x_expr)) {
      return(list(is_dim = FALSE))
    }
    return(list(
      is_dim = TRUE,
      operation = "dim_ndim",
      x_expr = x_expr
    ))
  }
  list(is_dim = FALSE)
}

# Helper: Detect /9.3 regex & table utility operations (strict expression-only subset)
.mojor_detect_tier9_regex_table_operation <- function(blocks) {
  call_info <- .mojor_expr_only_supported_call_or_false(
    blocks,
    c("grepl", "grep", "sub", "gsub", "row", "col", "expand.grid"),
    "is_tier9_regex_table"
  )
  if (is.null(call_info$block)) {
    return(call_info)
  }
  block <- call_info$block
  op_name <- call_info$op_name

  call_parts <- .mojor_expr_only_call_parts(block)
  parts <- call_parts$parts
  nms <- call_parts$nms

  if (op_name %in% c("grepl", "grep")) {
    allowed_named <- c("pattern", "x", "X", "fixed", "perl")
    bad_named <- .mojor_expr_only_bad_named_args(nms, allowed_named)
    if (length(bad_named) > 0 || length(parts) > 4) {
      stop(
        "mojor_transpile: ", op_name,
        "() currently supports only ", op_name,
        "(pattern, x, fixed = <scalar bool>, perl = <scalar bool>) in expression-only mode"
      )
    }
    pattern_expr <- .mojor_expr_only_get_arg(parts, nms, "pattern", 1)
    x_expr <- .mojor_expr_only_get_arg(parts, nms, "x", 2, aliases = "X")
    if (is.null(pattern_expr) || is.null(x_expr)) {
      stop("mojor_transpile: ", op_name, "() requires pattern and x in expression-only mode")
    }
    .mojor_expr_only_require_direct_name(x_expr, op_name, "x", "character vector")
    flags <- .mojor_expr_only_parse_regex_flags(parts, nms, op_name, fixed_pos = 3, perl_pos = 4)
    return(list(
      is_tier9_regex_table = TRUE,
      operation = op_name,
      pattern_expr = pattern_expr,
      replacement_expr = NULL,
      x_expr = x_expr,
      fixed_expr = flags$fixed_expr,
      perl_expr = flags$perl_expr
    ))
  }

  if (op_name %in% c("sub", "gsub")) {
    allowed_named <- c("pattern", "replacement", "x", "fixed", "perl")
    bad_named <- .mojor_expr_only_bad_named_args(nms, allowed_named)
    if (length(bad_named) > 0 || length(parts) > 5) {
      stop(
        "mojor_transpile: ", op_name, "() currently supports only ", op_name,
        "(pattern, replacement, x, fixed = <scalar bool>, perl = <scalar bool>) in expression-only mode"
      )
    }
    pattern_expr <- .mojor_expr_only_get_arg(parts, nms, "pattern", 1)
    replacement_expr <- .mojor_expr_only_get_arg(parts, nms, "replacement", 2)
    x_expr <- .mojor_expr_only_get_arg(parts, nms, "x", 3, aliases = "X")
    if (is.null(pattern_expr) || is.null(replacement_expr) || is.null(x_expr)) {
      stop("mojor_transpile: ", op_name, "() requires pattern, replacement, and x in expression-only mode")
    }
    .mojor_expr_only_require_direct_name(x_expr, op_name, "x", "character vector")
    flags <- .mojor_expr_only_parse_regex_flags(parts, nms, op_name, fixed_pos = 4, perl_pos = 5)
    return(list(
      is_tier9_regex_table = TRUE,
      operation = op_name,
      pattern_expr = pattern_expr,
      replacement_expr = replacement_expr,
      x_expr = x_expr,
      fixed_expr = flags$fixed_expr,
      perl_expr = flags$perl_expr
    ))
  }

  if (op_name %in% c("row", "col")) {
    bad_named <- .mojor_expr_only_bad_named_args(nms, "x")
    if (length(bad_named) > 0 || length(parts) != 1) {
      stop("mojor_transpile: ", op_name, "() currently supports only ", op_name, "(x) in expression-only mode")
    }
    x_expr <- .mojor_expr_only_get_arg(parts, nms, "x", 1, aliases = "X")
    if (is.null(x_expr)) {
      stop("mojor_transpile: ", op_name, "() requires x to be a direct matrix argument in expression-only mode")
    }
    .mojor_expr_only_require_direct_name(x_expr, op_name, "x", "matrix")
    return(list(
      is_tier9_regex_table = TRUE,
      operation = op_name,
      x_expr = x_expr
    ))
  }

  if (op_name == "expand.grid") {
    if (length(parts) < 1) {
      stop("mojor_transpile: expand.grid() requires at least one vector argument in expression-only mode")
    }
    arg_labels <- rep("", length(parts))
    if (length(nms) == length(parts)) {
      has_nm <- nzchar(nms)
      arg_labels[has_nm] <- nms[has_nm]
    }
    return(list(
      is_tier9_regex_table = TRUE,
      operation = op_name,
      arg_exprs = parts,
      arg_labels = arg_labels
    ))
  }

  list(is_tier9_regex_table = FALSE)
}

# Helper: Extract inlineable FUN for expression-only path
.mojor_extract_hof_inline_fun <- function(fun_expr, op_name, expected_arity, known_scalar_captures = NULL) {
  allowed_hof_named_unary_funs <- .mojor_expr_registry_vectorized_math_unary()
  allowed_hof_named_multiary_funs <- .mojor_expr_registry_vectorized_multiary_calls()

  if (is.name(fun_expr)) {
    fun_name <- as.character(fun_expr)
    if (expected_arity == 1L) {
      if (!(fun_name %in% allowed_hof_named_unary_funs)) {
        stop(
          "mojor_transpile: ", op_name,
          "() named FUN '", fun_name, "' is unsupported in expression-only mode; use an anonymous function or one of: ",
          paste(allowed_hof_named_unary_funs, collapse = ", ")
        )
      }
    } else if (expected_arity >= 2L) {
      if (!(fun_name %in% allowed_hof_named_multiary_funs)) {
        stop(
          "mojor_transpile: ", op_name,
          "() named FUN '", fun_name, "' is unsupported for arity ", expected_arity,
          " in expression-only mode; use an anonymous function or one of: ",
          paste(allowed_hof_named_multiary_funs, collapse = ", ")
        )
      }
    } else {
      stop("mojor_transpile: ", op_name, "() named FUN is unsupported for arity ", expected_arity, " in expression-only mode")
    }
    param_names <- paste0("__mojor_hof_arg", seq_len(expected_arity))
    body_ir <- .mojor_ir_call(fun_name, lapply(param_names, function(nm) .mojor_ir_var(nm)))
    return(list(param_names = param_names, body_ir = body_ir, return_parts = NULL, return_arity = 1L))
  }

  if (!(is.call(fun_expr) && identical(as.character(fun_expr[[1]]), "function"))) {
    stop("mojor_transpile: ", op_name, "() requires FUN to be an anonymous function or supported named function in expression-only mode")
  }

  fmls <- fun_expr[[2]]
  arg_names <- names(fmls)
  if (is.null(arg_names) || length(arg_names) != expected_arity || any(!nzchar(arg_names))) {
    stop("mojor_transpile: ", op_name, "() FUN arity must match provided vector arguments in expression-only mode")
  }

  blocks <- .mojor_extract_block(fun_expr[[3]])
  body_expr <- .mojor_expr_only_single_expr(blocks)
  if (is.null(body_expr)) {
    body_expr <- .mojor_inline_let_bindings(blocks)
  }
  if (is.null(body_expr)) {
    stop("mojor_transpile: ", op_name, "() FUN must be a single-expression or let-binding body in expression-only mode")
  }

  captures <- setdiff(all.vars(body_expr, functions = FALSE, unique = TRUE), arg_names)
  if (!is.null(known_scalar_captures) && length(known_scalar_captures) > 0L) {
    captures <- setdiff(captures, known_scalar_captures)
  }
  if (length(captures) > 0) {
    stop(
      "mojor_transpile: ", op_name,
      "() does not support closures/non-inlineable captures in expression-only mode: ",
      paste(captures, collapse = ", ")
    )
  }

  body_ir <- .mojor_ir_expr_build(body_expr)
  if (is.null(body_ir)) {
    stop("mojor_transpile: ", op_name, "() FUN body is not inlineable in expression-only mode")
  }
  if (identical(body_ir$kind, "list")) {
    stop("mojor_transpile: ", op_name, "() FUN list(...) return values are unsupported in expression-only mode")
  }
  if (body_ir$kind %in% c("rep", "rep_len", "vapply", "sapply", "lapply", "mapply")) {
    stop("mojor_transpile: ", op_name, "() FUN return value is unsupported in expression-only mode")
  }
  if (identical(body_ir$kind, "c")) {
    if (is.null(body_ir$parts) || !is.list(body_ir$parts) || length(body_ir$parts) < 1L) {
      stop("mojor_transpile: ", op_name, "() FUN c(...) return value must contain at least one scalar expression")
    }
    bad_part <- vapply(body_ir$parts, function(part) {
      if (is.null(part) || is.null(part$kind)) {
        return(TRUE)
      }
      part$kind %in% c("c", "list", "rep", "rep_len", "vapply", "sapply", "lapply", "mapply")
    }, logical(1))
    if (any(bad_part)) {
      stop("mojor_transpile: ", op_name, "() FUN c(...) return values must contain only scalar expressions")
    }
    return(list(
      param_names = arg_names,
      body_ir = body_ir,
      return_parts = body_ir$parts,
      return_arity = as.integer(length(body_ir$parts))
    ))
  }

  list(param_names = arg_names, body_ir = body_ir, return_parts = NULL, return_arity = 1L)
}

# Helper: Detect higher-order function operations
.mojor_detect_hof_operation <- function(blocks) {
  call_info <- .mojor_expr_only_supported_call_or_false(
    blocks,
    c("vapply", "sapply", "lapply", "mapply"),
    "is_hof"
  )
  if (is.null(call_info$block)) {
    return(call_info)
  }
  block <- call_info$block
  op_name <- call_info$op_name

  call_parts <- .mojor_expr_only_call_parts(block)
  parts <- call_parts$parts
  nms <- call_parts$nms

  if (op_name %in% c("sapply", "lapply", "vapply")) {
    x_expr <- .mojor_expr_only_get_arg(parts, nms, "X", 1, aliases = "x")
    fun_expr <- .mojor_expr_only_get_arg(parts, nms, "FUN", 2, aliases = "fun")
    if (is.null(x_expr) || is.null(fun_expr)) {
      stop("mojor_transpile: ", op_name, "() requires X and FUN arguments")
    }
    if (!is.name(x_expr)) {
      stop("mojor_transpile: ", op_name, "() requires direct vector arguments in expression-only mode")
    }

    out_type <- NULL
    if (op_name == "vapply") {
      fun_value <- .mojor_expr_only_get_arg(parts, nms, "FUN.VALUE", 3, aliases = "FUN_VALUE")
      if (is.null(fun_value)) {
        stop("mojor_transpile: vapply() requires FUN.VALUE in expression-only mode")
      }
      if (!(is.call(fun_value) && length(fun_value) == 2)) {
        stop("mojor_transpile: vapply() FUN.VALUE must be numeric(1), integer(1), or logical(1)")
      }
      ctor <- as.character(fun_value[[1]])
      n_arg <- suppressWarnings(as.integer(fun_value[[2]]))
      if (is.na(n_arg) || n_arg != 1L) {
        stop("mojor_transpile: vapply() FUN.VALUE must be numeric(1), integer(1), or logical(1)")
      }
      out_type <- if (ctor %in% c("numeric", "double")) {
        "f64[]"
      } else if (ctor %in% c("integer")) {
        "i32[]"
      } else if (ctor %in% c("logical")) {
        "lgl[]"
      } else {
        NULL
      }
      if (is.null(out_type)) {
        stop("mojor_transpile: vapply() FUN.VALUE must be numeric(1), integer(1), or logical(1)")
      }
    }

    fun_info <- .mojor_extract_hof_inline_fun(fun_expr, op_name, expected_arity = 1L)
    return(list(
      is_hof = TRUE,
      operation = op_name,
      arg_exprs = list(x_expr),
      fun_expr = fun_expr,
      fun_info = fun_info,
      out_type_hint = out_type
    ))
  }

  if (op_name == "mapply") {
    if (length(parts) < 3) {
      stop("mojor_transpile: mapply() requires FUN and at least two vector arguments")
    }
    fun_named_idx <- which(nms == "FUN")
    if (length(fun_named_idx) > 1L) {
      ref_fun <- parts[[fun_named_idx[[1L]]]]
      all_same <- all(vapply(fun_named_idx[-1L], function(idx) identical(parts[[idx]], ref_fun), logical(1)))
      if (!isTRUE(all_same)) {
        stop("mojor_transpile: mapply() duplicate FUN arguments must be identical in expression-only mode")
      }
    }
    has_fun_name <- length(fun_named_idx) > 0L
    fun_idx <- if (has_fun_name) fun_named_idx[[1L]] else 1L
    fun_expr <- parts[[fun_idx]]
    drop_idx <- if (has_fun_name) fun_named_idx else fun_idx
    arg_exprs <- parts[-drop_idx]
    if (length(arg_exprs) < 2) {
      stop("mojor_transpile: mapply() requires at least two direct vector arguments in expression-only mode")
    }
    if (!all(vapply(arg_exprs, is.name, logical(1)))) {
      stop("mojor_transpile: mapply() requires direct vector arguments in expression-only mode")
    }

    fun_info <- .mojor_extract_hof_inline_fun(fun_expr, "mapply", expected_arity = length(arg_exprs))
    return(list(
      is_hof = TRUE,
      operation = op_name,
      arg_exprs = arg_exprs,
      fun_expr = fun_expr,
      fun_info = fun_info,
      out_type_hint = NULL
    ))
  }

  list(is_hof = FALSE)
}

# Transpile higher-order function operations
.mojor_transpile_hof_operation <- function(hof_info, args, types, name = "mojor_kernel") {
  operation <- hof_info$operation
  arg_exprs <- hof_info$arg_exprs
  arg_names <- vapply(arg_exprs, as.character, character(1))

  if (length(arg_names) == 0) {
    stop("mojor_transpile: ", operation, "() requires at least one vector argument")
  }
  if (!all(arg_names %in% args)) {
    missing <- arg_names[!arg_names %in% args]
    stop(
      "mojor_transpile: ", operation, "() argument(s) not found in function signature: ",
      paste(unique(missing), collapse = ", ")
    )
  }

  arg_types <- vapply(arg_names, function(nm) types[[which(args == nm)]], character(1))
  if (!all(arg_types %in% c("f64[]", "i32[]", "lgl[]"))) {
    bad_idx <- which(!(arg_types %in% c("f64[]", "i32[]", "lgl[]")))[1]
    stop(
      "mojor_transpile: ", operation,
      "() currently supports only numeric/integer/logical vector arguments; got: ",
      arg_types[[bad_idx]]
    )
  }

  vector_elem_type <- function(vec_type) {
    if (identical(vec_type, "f64[]")) return("f64")
    if (identical(vec_type, "i32[]")) return("i32")
    if (vec_type %in% c("lgl[]", "bool[]")) return("lgl")
    NULL
  }
  elem_vector_type <- function(elem_type) {
    if (elem_type %in% c("f64", "f32")) return("f64[]")
    if (elem_type %in% c("i32", "i64")) return("i32[]")
    if (elem_type %in% c("lgl", "bool")) return("lgl[]")
    NULL
  }
  promote_many <- function(type_values) {
    if (length(type_values) == 0L) {
      return("unknown")
    }
    out <- type_values[[1]]
    if (length(type_values) > 1L) {
      for (type_value in type_values[-1L]) {
        out <- .mojor_type_promote(out, type_value)
      }
    }
    out
  }
  infer_hof_out_type <- function() {
    fun_info <- hof_info$fun_info
    if (is.null(fun_info) || is.null(fun_info$param_names) || is.null(fun_info$body_ir)) {
      return(NULL)
    }
    param_scalar_types <- vapply(arg_types, vector_elem_type, character(1))
    if (length(param_scalar_types) != length(fun_info$param_names) || any(!nzchar(param_scalar_types))) {
      return(NULL)
    }
    param_env <- as.list(param_scalar_types)
    names(param_env) <- fun_info$param_names
    if (!is.null(fun_info$return_parts) && is.list(fun_info$return_parts) && length(fun_info$return_parts) > 0L) {
      part_types <- vapply(fun_info$return_parts, function(part) .mojor_ir_infer_type(part, param_env), character(1))
      if (any(.mojor_type_is_array(part_types))) {
        return(NULL)
      }
      vec_type <- elem_vector_type(promote_many(part_types))
      if (!is.null(vec_type)) {
        return(vec_type)
      }
      return(NULL)
    }
    inferred <- .mojor_ir_infer_type(fun_info$body_ir, param_env)
    vec_type <- elem_vector_type(inferred)
    if (!is.null(vec_type)) {
      return(vec_type)
    }
    if (any(arg_types == "f64[]")) return("f64[]")
    if (any(arg_types == "i32[]")) return("i32[]")
    if (any(arg_types %in% c("lgl[]", "bool[]"))) return("lgl[]")
    NULL
  }

  out_type <- hof_info$out_type_hint
  if (is.null(out_type)) {
    out_type <- infer_hof_out_type()
    if (is.null(out_type)) {
      out_type <- arg_types[[1]]
    }
  }
  fun_return_arity <- 1L
  if (!is.null(hof_info$fun_info$return_arity)) {
    fun_return_arity <- suppressWarnings(as.integer(hof_info$fun_info$return_arity))
    if (is.na(fun_return_arity) || fun_return_arity < 1L) {
      stop("mojor_transpile: ", operation, "() FUN return arity must be a positive integer")
    }
  }
  if (!(out_type %in% c("f64[]", "i32[]", "lgl[]"))) {
    stop("mojor_transpile: ", operation, "() output type is unsupported in expression-only mode: ", out_type)
  }

  arg_ptr_info <- lapply(arg_types, .mojor_expr_only_array_ptr_info)
  out_ptr_info <- .mojor_expr_only_array_ptr_info(out_type)
  out_name_collision <- "out" %in% arg_names
  out_slot <- if (isTRUE(out_name_collision)) "__mojor_out" else "out"
  out_ptr_name <- if (isTRUE(out_name_collision)) "__mojor_out_ptr" else "out_ptr"

  scalar_arg_info <- if (!is.null(hof_info$scalar_args) && length(hof_info$scalar_args) > 0L) {
    lapply(hof_info$scalar_args, function(nm) {
      spec <- types[[which(args == nm)]]
      mojo_type <- switch(spec,
        "f64" = "Float64", "f32" = "Float32",
        "i32" = "Int32", "lgl" = "Int32", "bool" = "Int32",
        stop("mojor_transpile: unsupported scalar type in vectorized expression: ", spec))
      list(name = nm, spec = spec, mojo_type = mojo_type)
    })
  } else {
    NULL
  }

  params <- character(0)
  for (nm in arg_names) {
    params <- c(
      params,
      paste0("    ", nm, "_ptr: ImmutOpaqueAny,"),
      paste0("    __mojor_len_", nm, ": Int32,")
    )
  }
  if (!is.null(scalar_arg_info)) {
    for (si in scalar_arg_info) {
      params <- c(params, paste0("    ", si$name, "_val: ", si$mojo_type, ","))
    }
  }
  params <- c(params, paste0("    ", out_ptr_name, ": MutOpaqueAny"))

  signature <- c(
    paste0('@export("', name, '", ABI="C")'),
    paste0("fn ", name, "("),
    params,
    ") -> Int32:"
  )

  body <- character(0)
  for (i in seq_along(arg_names)) {
    nm <- arg_names[[i]]
    ptr <- arg_ptr_info[[i]]
    body <- c(
      body,
      .mojor_expr_only_bind_array_ptr_len(nm, ptr$immut_ptr, ptr$cast)
    )
  }
  body <- c(body, paste0("    var ", out_slot, ": ", out_ptr_info$mut_ptr, " = ", out_ptr_name, ".bitcast[", out_ptr_info$cast, "]()"))
  if (!is.null(scalar_arg_info)) {
    for (si in scalar_arg_info) {
      body <- c(body, paste0("    var ", si$name, " = ", si$mojo_type, "(", si$name, "_val)"))
    }
  }
  for (nm in arg_names[-1]) {
    body <- c(
      body,
      paste0("    if n_", nm, " != n_", arg_names[[1]], ":"),
      "        return Int32(-1)"
    )
  }

  type_env <- stats::setNames(as.list(arg_types), arg_names)
  if (!is.null(scalar_arg_info)) {
    for (si in scalar_arg_info) {
      type_env[[si$name]] <- si$spec
    }
  }
  fun_expr <- hof_info$fun_expr
  hof_node <- switch(operation,
    vapply = .mojor_ir_vapply(
      x = .mojor_ir_var(arg_names[[1]]),
      fun = fun_expr,
      fun_value_type = if (out_type == "f64[]") "f64" else if (out_type == "i32[]") "i32" else "lgl"
    ),
    sapply = .mojor_ir_sapply(
      x = .mojor_ir_var(arg_names[[1]]),
      fun = fun_expr
    ),
    lapply = .mojor_ir_lapply(
      x = .mojor_ir_var(arg_names[[1]]),
      fun = fun_expr
    ),
    mapply = .mojor_ir_mapply(
      fun = fun_expr,
      args = lapply(arg_names, .mojor_ir_var)
    ),
    stop("mojor_transpile: unsupported compiled subset higher-order operation: ", operation)
  )

  old_len_var_map <- .mojor_state$current_len_var_map
  on.exit(
    {
      .mojor_state$current_len_var_map <- old_len_var_map
    },
    add = TRUE
  )
  len_var_map <- old_len_var_map
  if (is.null(len_var_map)) len_var_map <- list()
  for (nm in arg_names) {
    len_var_map[[nm]] <- paste0("n_", nm)
  }
  .mojor_state$current_len_var_map <- len_var_map

  tier8_lines <- .mojor_ir_tier8_hof_assign_emit(
    node = hof_node,
    out_name = out_slot,
    type_env = type_env,
    indent = "    ",
    zero_based_vars = NULL,
    allow_constructor_returns = !is.null(hof_info$fun_info$return_parts),
    known_scalar_captures = hof_info$scalar_args
  )
  if (is.null(tier8_lines) || length(tier8_lines) == 0) {
    stop("mojor_transpile: ", operation, "() could not be emitted in expression-only mode")
  }

  return_len_expr <- if (fun_return_arity > 1L) {
    paste0("Int(n_", arg_names[[1]], ") * Int(", fun_return_arity, ")")
  } else {
    paste0("n_", arg_names[[1]])
  }
  body <- c(body, tier8_lines, paste0("    return Int32(", return_len_expr, ")"))

  mojo_math_fns <- .mojor_expr_registry_mojo_math_imports()
  all_lines <- c(signature, body)
  used_math <- mojo_math_fns[vapply(mojo_math_fns, function(fn) {
    any(grepl(paste0("\\b", fn, "\\s*\\("), all_lines, perl = TRUE))
  }, logical(1))]
  math_imports <- if (length(used_math) > 0L) {
    paste0("from math import ", paste(used_math, collapse = ", "))
  } else {
    character(0)
  }

  # Detect if generated code needs inf/nan constants or NA helpers
  all_code <- paste(all_lines, collapse = "\n")
  needs_inf <- grepl("_MOJOR_INF|_MOJOR_NINF", all_code, perl = TRUE)
  needs_nan <- grepl("_MOJOR_NAN", all_code, perl = TRUE)
  needs_na_helpers <- grepl("_mojor_is_nan_f(64|32)", all_code, perl = TRUE)
  inf_imports <- character(0)
  if (needs_inf || needs_nan) {
    inf_imports <- c(inf_imports, "from math import inf, nan")
  }
  if (needs_inf) {
    inf_imports <- c(inf_imports,
      "comptime _MOJOR_INF = inf[DType.float64]()",
      "comptime _MOJOR_NINF = -inf[DType.float64]()",
      "comptime _MOJOR_INF_F32 = inf[DType.float32]()",
      "comptime _MOJOR_NINF_F32 = -inf[DType.float32]()"
    )
  }
  if (needs_nan) {
    inf_imports <- c(inf_imports,
      "comptime _MOJOR_NAN = nan[DType.float64]()",
      "comptime _MOJOR_NAN_F32 = nan[DType.float32]()"
    )
  }
  if (needs_na_helpers) {
    inf_imports <- c(inf_imports, "from na_helpers import _mojor_is_nan_f64, _mojor_is_nan_f32")
  }

  preamble <- .mojor_expr_only_pointer_preamble(
    "# Generated by mojor_transpile (expression-only mode - compiled subset higher-order functions)",
    imports = c(math_imports, inf_imports),
    include_mut_opaque = TRUE,
    include_immut_types = c("f64", "i32"),
    include_mut_types = c("f64", "i32")
  )

  mojo_code <- paste(c(preamble, signature, body), collapse = "\n")
  list(
    mojo = mojo_code,
    is_expression_kernel = TRUE,
    is_vector_output = TRUE,
    out_type = out_type,
    return_type = "Int32",
    vector_len_arg = arg_names[[1]],
    vector_len_scale = as.integer(fun_return_arity),
    args = args,
    types = types
  )
}

# Transpile string operations via strict native nchar/nzchar/substr/paste implementation
.mojor_transpile_string_operation <- function(string_info, args, types, name = "mojor_kernel") {
  require_chr_arg <- function(expr, label, op_label) {
    if (!is.name(expr)) {
      stop("mojor_transpile: ", op_label, "() ", label, " must be a direct character vector argument in expression-only mode")
    }
    nm <- as.character(expr)
    if (!(nm %in% args)) {
      stop("mojor_transpile: ", op_label, "() argument '", nm, "' is not a function parameter")
    }
    spec <- types[[which(args == nm)]]
    if (!(identical(spec, "chr[]") || identical(spec, "chr"))) {
      stop("mojor_transpile: ", op_label, "() currently supports only chr[]/chr input; got: ", spec)
    }
    list(name = nm, spec = spec, is_vector = identical(spec, "chr[]"))
  }
  parse_i32_scalar <- function(expr, arg_label, op_label) {
    if (is.name(expr)) {
      nm <- as.character(expr)
      if (!(nm %in% args)) {
        stop("mojor_transpile: ", op_label, "() ", arg_label, " argument '", nm, "' is not a function parameter")
      }
      spec <- types[[which(args == nm)]]
      if (!identical(spec, "i32")) {
        stop("mojor_transpile: ", op_label, "() ", arg_label, " must be an i32 scalar argument or integer literal in expression-only mode")
      }
      return(list(kind = "arg", name = nm))
    }
    if ((is.integer(expr) || is.numeric(expr)) && length(expr) == 1 && !is.na(expr)) {
      v <- as.integer(expr)
      return(list(kind = "const", value = v))
    }
    stop("mojor_transpile: ", op_label, "() ", arg_label, " must be an i32 scalar argument or integer literal in expression-only mode")
  }

  op <- string_info$operation
  dummy_mojo <- paste(
    c(
      "# Generated by mojor_transpile (expression-only mode - compiled subset native nchar/nzchar/substr/paste path)",
      paste0('@export("', name, '", ABI="C")'),
      paste0("fn ", name, "() -> Int32:"),
      "    return Int32(0)"
    ),
    collapse = "\n"
  )

  if (op %in% c("nchar", "nzchar")) {
    x_info <- require_chr_arg(string_info$x_expr, "x", op)
    x_name <- x_info$name
    is_vector_output <- isTRUE(x_info$is_vector)
    return(list(
      mojo = dummy_mojo,
      is_expression_kernel = TRUE,
      is_vector_output = is_vector_output,
      out_type = if (identical(op, "nchar")) {
        if (is_vector_output) "i32[]" else "i32"
      } else {
        if (is_vector_output) "lgl[]" else "lgl"
      },
      return_type = "Int32",
      vector_len_arg = if (is_vector_output) x_name else NULL,
      kernel_args = character(0),
      string_builtin = TRUE,
      string_op = op,
      string_x_name = x_name,
      args = args,
      types = types
    ))
  }

  if (identical(op, "substr")) {
    x_info <- require_chr_arg(string_info$x_expr, "x", "substr")
    x_name <- x_info$name
    is_vector_output <- isTRUE(x_info$is_vector)
    start_info <- parse_i32_scalar(string_info$start_expr, "start", "substr")
    stop_info <- parse_i32_scalar(string_info$stop_expr, "stop", "substr")
    kernel_args <- character(0)
    if (identical(start_info$kind, "arg")) kernel_args <- c(kernel_args, start_info$name)
    if (identical(stop_info$kind, "arg")) kernel_args <- c(kernel_args, stop_info$name)
    return(list(
      mojo = dummy_mojo,
      is_expression_kernel = TRUE,
      is_vector_output = is_vector_output,
      out_type = if (is_vector_output) "chr[]" else "chr",
      return_type = "Int32",
      vector_len_arg = if (is_vector_output) x_name else NULL,
      kernel_args = kernel_args,
      string_builtin = TRUE,
      string_op = "substr",
      string_x_name = x_name,
      string_start = start_info,
      string_stop = stop_info,
      args = args,
      types = types
    ))
  }

  if (op %in% c("paste", "paste0")) {
    x_info <- require_chr_arg(string_info$x_expr, "x", op)
    y_info <- require_chr_arg(string_info$y_expr, "y", op)
    x_name <- x_info$name
    y_name <- y_info$name
    sep_value <- if (identical(op, "paste")) string_info$sep else ""
    collapse_value <- string_info$collapse
    if (!(is.character(sep_value) && length(sep_value) == 1 && !is.na(sep_value))) {
      stop("mojor_transpile: ", op, "() internal error: sep metadata must be scalar character")
    }
    if (!(is.null(collapse_value) || (is.character(collapse_value) && length(collapse_value) == 1 && !is.na(collapse_value)))) {
      stop("mojor_transpile: ", op, "() internal error: collapse metadata must be scalar character or NULL")
    }
    out_is_vector <- is.null(collapse_value) && (isTRUE(x_info$is_vector) || isTRUE(y_info$is_vector))
    out_len_arg <- NULL
    if (out_is_vector) {
      out_len_arg <- if (isTRUE(x_info$is_vector)) x_name else y_name
    }
    return(list(
      mojo = dummy_mojo,
      is_expression_kernel = TRUE,
      is_vector_output = out_is_vector,
      out_type = if (out_is_vector) "chr[]" else "chr",
      return_type = "Int32",
      vector_len_arg = out_len_arg,
      kernel_args = character(0),
      string_builtin = TRUE,
      string_op = op,
      string_x_name = x_name,
      string_y_name = y_name,
      string_sep = sep_value,
      string_collapse = collapse_value,
      string_arg_names = c(x_name, y_name),
      args = args,
      types = types
    ))
  }

  stop(
    "mojor_transpile: compiled subset strict expression-only currently supports only ",
    "nchar(x), nzchar(x), substr()/substring(), paste()/paste0()"
  )
}

# Transpile dim() metadata query in expression-only mode
.mojor_transpile_dim_operation <- function(dim_info, args, types, name = "mojor_kernel") {
  x_expr <- dim_info$x_expr
  if (!is.name(x_expr)) {
    stop("mojor_transpile: dim() requires x to be a direct array argument in expression-only mode")
  }
  x_name <- as.character(x_expr)
  if (!(x_name %in% args)) {
    stop("mojor_transpile: dim() argument '", x_name, "' is not a function parameter")
  }
  x_type <- types[[which(args == x_name)]]
  if (!.mojor_is_array(x_type)) {
    stop("mojor_transpile: dim() currently supports only matrix/ND array inputs in expression-only mode; got: ", x_type)
  }
  expected_ndim <- if (.mojor_is_matrix(x_type)) 2L else .mojor_type_ndim(x_type)
  if (is.null(expected_ndim) || !is.finite(expected_ndim) || expected_ndim < 2L) {
    stop("mojor_transpile: dim() currently supports only matrix/ND array inputs in expression-only mode; got: ", x_type)
  }

  dummy_mojo <- paste(
    c(
      "# Generated by mojor_transpile (expression-only mode - compiled subset dim metadata path)",
      paste0('@export("', name, '", ABI="C")'),
      paste0("fn ", name, "() -> Int32:"),
      "    return Int32(0)"
    ),
    collapse = "\n"
  )
  op <- .mojor_state$`%||%`(dim_info$operation, "dim_vector")
  if (identical(op, "dim_vector")) {
    return(list(
      mojo = dummy_mojo,
      is_expression_kernel = TRUE,
      is_vector_output = TRUE,
      out_type = "i32[]",
      return_type = "Int32",
      vector_len_const = as.integer(expected_ndim),
      kernel_args = character(0),
      dim_builtin = TRUE,
      dim_builtin_mode = "vector",
      dim_x_name = x_name,
      dim_expected_ndim = as.integer(expected_ndim),
      args = args,
      types = types
    ))
  }
  if (identical(op, "dim_index")) {
    idx_info <- dim_info$idx_info
    if (is.null(idx_info) || is.null(idx_info$kind)) {
      stop("mojor_transpile: dim() index metadata missing in expression-only mode")
    }
    kernel_args <- character(0)
    out <- list(
      mojo = dummy_mojo,
      is_expression_kernel = TRUE,
      out_type = "i32",
      return_type = "Int32",
      kernel_args = character(0),
      dim_builtin = TRUE,
      dim_builtin_mode = "index",
      dim_x_name = x_name,
      dim_expected_ndim = as.integer(expected_ndim),
      dim_index_kind = idx_info$kind,
      args = args,
      types = types
    )
    if (identical(idx_info$kind, "const")) {
      out$dim_index_value <- as.integer(idx_info$value)
    } else if (identical(idx_info$kind, "arg")) {
      out$dim_index_name <- idx_info$name
      kernel_args <- idx_info$name
    } else if (identical(idx_info$kind, "expr")) {
      out$dim_index_c_expr <- idx_info$c_expr
      out$dim_index_arg_names <- idx_info$arg_names
      kernel_args <- idx_info$arg_names
    } else {
      stop("mojor_transpile: dim() index metadata has unsupported kind: ", idx_info$kind)
    }
    out$kernel_args <- kernel_args
    return(out)
  }
  if (identical(op, "dim_ndim")) {
    return(list(
      mojo = dummy_mojo,
      is_expression_kernel = TRUE,
      out_type = "i32",
      return_type = "Int32",
      kernel_args = character(0),
      dim_builtin = TRUE,
      dim_builtin_mode = "ndim",
      dim_x_name = x_name,
      dim_expected_ndim = as.integer(expected_ndim),
      args = args,
      types = types
    ))
  }
  stop("mojor_transpile: unsupported dim() expression-only operation: ", op)
}

# Transpile /9.3 operations.
# 9.2 regex + 9.3 expand.grid use strict shared runtime bridge plans.
# 9.3 row/col use an IR-native matrix kernel path.
.mojor_transpile_tier9_regex_table_operation <- function(tier9_info, args, types, name = "mojor_kernel") {
  parse_chr_vector <- function(expr, op_label, arg_label) {
    if (!is.name(expr)) {
      stop("mojor_transpile: ", op_label, "() ", arg_label, " must be a direct character vector argument in expression-only mode")
    }
    nm <- as.character(expr)
    if (!(nm %in% args)) {
      stop("mojor_transpile: ", op_label, "() argument '", nm, "' is not a function parameter")
    }
    spec <- types[[which(args == nm)]]
    if (!identical(spec, "chr[]")) {
      stop("mojor_transpile: ", op_label, "() ", arg_label, " must have type chr[] in expression-only mode; got: ", spec)
    }
    nm
  }
  parse_chr_scalar_or_literal <- function(expr, op_label, arg_label) {
    if (is.name(expr)) {
      nm <- as.character(expr)
      if (!(nm %in% args)) {
        stop("mojor_transpile: ", op_label, "() argument '", nm, "' is not a function parameter")
      }
      spec <- types[[which(args == nm)]]
      if (!identical(spec, "chr")) {
        stop("mojor_transpile: ", op_label, "() ", arg_label, " must be a chr scalar argument or scalar character literal in expression-only mode")
      }
      return(list(kind = "arg", name = nm))
    }
    if (is.character(expr) && length(expr) == 1 && !is.na(expr)) {
      return(list(kind = "const", value = as.character(expr[[1]])))
    }
    stop("mojor_transpile: ", op_label, "() ", arg_label, " must be a chr scalar argument or scalar character literal in expression-only mode")
  }
  parse_bool_scalar_or_literal <- function(expr, op_label, arg_label, default) {
    if (is.null(expr)) {
      return(list(kind = "const", value = isTRUE(default)))
    }
    if (is.logical(expr) && length(expr) == 1 && !is.na(expr)) {
      return(list(kind = "const", value = isTRUE(expr)))
    }
    if (is.numeric(expr) && length(expr) == 1 && !is.na(expr) && expr %in% c(0, 1)) {
      return(list(kind = "const", value = isTRUE(as.logical(expr))))
    }
    if (is.name(expr)) {
      nm <- as.character(expr)
      if (identical(nm, "TRUE")) {
        return(list(kind = "const", value = TRUE))
      }
      if (identical(nm, "FALSE")) {
        return(list(kind = "const", value = FALSE))
      }
      if (!(nm %in% args)) {
        stop("mojor_transpile: ", op_label, "() argument '", nm, "' is not a function parameter")
      }
      spec <- types[[which(args == nm)]]
      if (!(spec %in% c("lgl", "bool", "i32"))) {
        stop(
          "mojor_transpile: ", op_label, "() ", arg_label,
          " must be a scalar logical argument/literal (lgl/bool/i32) in expression-only mode"
        )
      }
      return(list(kind = "arg", name = nm))
    }
    stop(
      "mojor_transpile: ", op_label, "() ", arg_label,
      " must be a scalar logical argument/literal in expression-only mode"
    )
  }
  parse_matrix_arg <- function(expr, op_label, arg_label) {
    if (!is.name(expr)) {
      stop("mojor_transpile: ", op_label, "() ", arg_label, " must be a direct matrix argument in expression-only mode")
    }
    nm <- as.character(expr)
    if (!(nm %in% args)) {
      stop("mojor_transpile: ", op_label, "() argument '", nm, "' is not a function parameter")
    }
    spec <- types[[which(args == nm)]]
    if (!(spec %in% c("f64[,]", "f32[,]", "i32[,]", "lgl[,]", "bool[,]"))) {
      stop("mojor_transpile: ", op_label, "() ", arg_label, " must be a numeric/integer/logical matrix argument; got: ", spec)
    }
    nm
  }
  parse_vector_arg <- function(expr, op_label) {
    resolve_arg_spec <- function(nm) {
      if (!(nm %in% args)) {
        stop("mojor_transpile: ", op_label, "() argument '", nm, "' is not a function parameter")
      }
      types[[which(args == nm)]]
    }
    ensure_expand_grid_ref_type <- function(nm, expr_for_err) {
      spec <- resolve_arg_spec(nm)
      if (!(spec %in% c("f64[]", "i32[]", "lgl[]", "f64", "i32", "lgl", "bool"))) {
        .mojor_err(
          paste0(
            op_label,
            "() argument '",
            nm,
            "' must be typed as f64[]/i32[]/lgl[] or scalar f64/i32/lgl/bool in expression-only mode"
          ),
          expr_for_err,
          "Use direct vector arguments or strict arithmetic/constructor expressions over supported typed arguments"
        )
      }
      spec
    }
    collect_arg_refs <- function(e) {
      if (is.name(e)) {
        nm <- as.character(e)
        if (nm %in% c("TRUE", "FALSE", "T", "F", "NA", "NULL")) {
          return(character(0))
        }
        return(if (nm %in% args) nm else character(0))
      }
      if (is.call(e)) {
        parts <- as.list(e)[-1]
        refs <- lapply(parts, collect_arg_refs)
        return(unique(unlist(refs, use.names = FALSE)))
      }
      character(0)
    }
    validate_expand_grid_expr <- function(e) {
      allowed_call_ops <- c(
        "(", "+", "-", "*", "/", "%%", "%/%", "^", ":",
        "c", "seq", "seq.int", "seq_len", "rep", "rep_len",
        "as.integer", "as.double", "as.numeric", "as.logical",
        "abs", "floor", "ceiling", "trunc", "round", "min", "max", "ifelse",
        "log", "log10", "log2", "log1p", "exp", "expm1", "sqrt",
        "sin", "cos", "tan", "asin", "acos", "atan", "atan2",
        "sinh", "cosh", "tanh", "pmin", "pmax", "sign"
      )
      if (is.null(e)) {
        .mojor_err(
          paste0(op_label, "() argument expression is unsupported in expression-only mode"),
          expr,
          "Use direct vector arguments or strict arithmetic/constructor expressions"
        )
      }
      if (is.numeric(e) || is.integer(e) || is.logical(e)) {
        return(invisible(NULL))
      }
      if (is.name(e)) {
        nm <- as.character(e)
        if (nm %in% c("TRUE", "FALSE", "T", "F", "NA", "NULL")) {
          return(invisible(NULL))
        }
        ensure_expand_grid_ref_type(nm, expr)
        return(invisible(NULL))
      }
      if (!is.call(e)) {
        .mojor_err(
          paste0(op_label, "() argument expression is unsupported in expression-only mode"),
          expr,
          "Use direct vector arguments or strict arithmetic/constructor expressions"
        )
      }
      op <- as.character(e[[1]])
      if (!(op %in% allowed_call_ops)) {
        .mojor_err(
          paste0(op_label, "() argument expression uses unsupported operator/function '", op, "'"),
          expr,
          "Use arithmetic operators (+,-,*,/,%%,%/%,^,:), constructors (c/seq/rep), math helpers (log/exp/sqrt/trig), ifelse(), and basic casts"
        )
      }
      parts <- as.list(e)[-1]
      for (part in parts) {
        validate_expand_grid_expr(part)
      }
      invisible(NULL)
    }

    if (is.name(expr)) {
      nm <- as.character(expr)
      spec <- ensure_expand_grid_ref_type(nm, expr)
      if (!(spec %in% c("f64[]", "i32[]", "lgl[]"))) {
        stop("mojor_transpile: ", op_label, "() direct argument '", nm, "' must be typed f64[]/i32[]/lgl[]; got: ", spec)
      }
      return(list(kind = "arg", name = nm, spec = spec))
    }

    validate_expand_grid_expr(expr)
    refs <- collect_arg_refs(expr)
    list(kind = "expr", expr = expr, arg_refs = refs)
  }
  has_backref <- function(txt) {
    is.character(txt) && length(txt) == 1 && !is.na(txt) && grepl("\\\\[1-9]", txt, perl = TRUE)
  }
  build_row_col_kernel <- function(op_label, kernel_name, x_name, x_type) {
    assign_expr <- if (identical(op_label, "row")) "Int32(i)" else "Int32(j)"

    preamble <- .mojor_expr_only_pointer_preamble(
      paste0("# Generated by mojor_transpile (expression-only mode - compiled subset ", op_label, " IR-native matrix path)"),
      include_mut_opaque = TRUE,
      include_immut_types = c("f64", "f32", "i32"),
      include_mut_types = c("i32")
    )
    signature <- c(
      paste0('@export("', kernel_name, '", ABI="C")'),
      paste0("fn ", kernel_name, "("),
      paste0("    ", x_name, "_ptr: ImmutOpaqueAny,"),
      paste0("    __mojor_len_", x_name, ": Int32,"),
      paste0("    __mojor_dim_", x_name, ": ImmutOpaqueAny,"),
      "    out_ptr: MutOpaqueAny,",
      "    out_nrow: Int32,",
      "    out_ncol: Int32",
      ") -> None:"
    )
    body <- c(
      paste0("    var ", x_name, "_dim: ImmutInt32Ptr = __mojor_dim_", x_name, ".bitcast[Int32]()"),
      paste0("    var nrow_", x_name, " = Int(", x_name, "_dim[0])"),
      paste0("    var ncol_", x_name, " = Int(", x_name, "_dim[1])"),
      paste0("    if Int(out_nrow) != nrow_", x_name, " or Int(out_ncol) != ncol_", x_name, ":"),
      "        return",
      "    var out: MutInt32Ptr = out_ptr.bitcast[Int32]()",
      paste0("    for j in range(1, ncol_", x_name, " + 1):"),
      paste0("        for i in range(1, nrow_", x_name, " + 1):"),
      paste0("            var idx = Int((i - 1) + (j - 1) * nrow_", x_name, ")"),
      paste0("            out[idx] = ", assign_expr)
    )
    paste(c(preamble, signature, body), collapse = "\n")
  }

  op <- tier9_info$operation
  regex_dummy_mojo <- paste(
    c(
      "# Generated by mojor_transpile (expression-only mode - compiled subset regex runtime bridge path)",
      paste0('@export("', name, '", ABI="C")'),
      paste0("fn ", name, "() -> Int32:"),
      "    return Int32(0)"
    ),
    collapse = "\n"
  )
  expand_dummy_mojo <- paste(
    c(
      "# Generated by mojor_transpile (expression-only mode - compiled subset expand.grid runtime bridge path)",
      paste0('@export("', name, '", ABI="C")'),
      paste0("fn ", name, "() -> Int32:"),
      "    return Int32(0)"
    ),
    collapse = "\n"
  )

  if (op %in% c("grepl", "grep", "sub", "gsub")) {
    x_name <- parse_chr_vector(tier9_info$x_expr, op, "x")
    pattern_info <- parse_chr_scalar_or_literal(tier9_info$pattern_expr, op, "pattern")
    replacement_info <- NULL
    fixed_info <- parse_bool_scalar_or_literal(tier9_info$fixed_expr, op, "fixed", default = FALSE)
    perl_info <- parse_bool_scalar_or_literal(tier9_info$perl_expr, op, "perl", default = TRUE)
    if (op %in% c("sub", "gsub")) {
      replacement_info <- parse_chr_scalar_or_literal(tier9_info$replacement_expr, op, "replacement")
    }

    if (identical(fixed_info$kind, "const") &&
        identical(perl_info$kind, "const") &&
        isTRUE(fixed_info$value) &&
        isTRUE(perl_info$value)) {
      stop("mojor_transpile: ", op, "() does not support fixed=TRUE with perl=TRUE")
    }

    if (identical(pattern_info$kind, "const") && has_backref(pattern_info$value)) {
      stop("mojor_transpile: ", op, "() backreferences in pattern are not supported in the compiled subset")
    }
    if ((op %in% c("sub", "gsub")) && identical(replacement_info$kind, "const") && has_backref(replacement_info$value)) {
      stop("mojor_transpile: ", op, "() backreferences in replacement are not supported in the compiled subset")
    }

    kernel_args <- x_name
    if (identical(pattern_info$kind, "arg")) kernel_args <- c(kernel_args, pattern_info$name)
    if (!is.null(replacement_info) && identical(replacement_info$kind, "arg")) kernel_args <- c(kernel_args, replacement_info$name)
    if (identical(fixed_info$kind, "arg")) kernel_args <- c(kernel_args, fixed_info$name)
    if (identical(perl_info$kind, "arg")) kernel_args <- c(kernel_args, perl_info$name)
    kernel_args <- unique(kernel_args)

    return(list(
      mojo = regex_dummy_mojo,
      is_expression_kernel = TRUE,
      is_vector_output = TRUE,
      out_type = if (identical(op, "grepl")) "lgl[]" else if (identical(op, "grep")) "i32[]" else "chr[]",
      return_type = "Int32",
      vector_len_arg = x_name,
      kernel_args = kernel_args,
      tier9_runtime_plan = list(
        kind = "regex_native",
        op = op,
        x_name = x_name,
        pattern = pattern_info,
        replacement = replacement_info,
        fixed = fixed_info,
        perl = perl_info
      ),
      args = args,
      types = types
    ))
  }

  if (op %in% c("row", "col")) {
    x_name <- parse_matrix_arg(tier9_info$x_expr, op, "x")
    x_type <- types[[which(args == x_name)]]
    operation_tag <- if (identical(op, "row")) "row_matrix" else "col_matrix"
    mojo_code <- build_row_col_kernel(op, name, x_name, x_type)
    return(list(
      mojo = mojo_code,
      is_expression_kernel = TRUE,
      is_matrix_output = TRUE,
      out_type = "i32[,]",
      return_type = "Matrix",
      operation = operation_tag,
      kernel_args = x_name,
      args = args,
      types = types
    ))
  }

  if (identical(op, "expand.grid")) {
    arg_desc <- lapply(tier9_info$arg_exprs, function(x) parse_vector_arg(x, "expand.grid"))
    arg_names <- vapply(
      arg_desc,
      function(d) if (identical(d$kind, "arg")) d$name else "",
      character(1)
    )
    arg_types <- vapply(
      arg_desc,
      function(d) if (!is.null(d$spec)) d$spec else "",
      character(1)
    )
    arg_entries <- lapply(arg_desc, function(d) {
      if (identical(d$kind, "arg")) {
        return(list(kind = "arg", name = d$name, spec = d$spec))
      }
      list(kind = "expr", expr = d$expr, arg_refs = d$arg_refs)
    })
    arg_labels <- tier9_info$arg_labels
    if (is.null(arg_labels)) {
      arg_labels <- rep("", length(arg_entries))
    }
    if (!is.character(arg_labels) || length(arg_labels) != length(arg_entries)) {
      stop("mojor_transpile: expand.grid() internal metadata mismatch for argument labels")
    }
    return(list(
      mojo = expand_dummy_mojo,
      is_expression_kernel = TRUE,
      return_type = "SEXP",
      tier9_runtime_plan = list(
        kind = "expand_grid_native",
        op = "expand.grid",
        arg_entries = arg_entries,
        arg_names = arg_names,
        arg_types = arg_types,
        arg_labels = arg_labels
      ),
      args = args,
      types = types
    ))
  }

  stop("mojor_transpile: unsupported compiled subset regex/table operation in expression-only mode: ", op)
}

# Transpile set/match operations ()
.mojor_transpile_set_match_operation <- function(set_match_info, args, types, name = "mojor_kernel") {
  operation <- set_match_info$operation
  x_expr <- set_match_info$x
  table_expr <- set_match_info$table

  if (!is.name(x_expr)) {
    stop("mojor_transpile: ", operation, " argument must be a simple variable name")
  }
  x_name <- as.character(x_expr)
  if (!(x_name %in% args)) {
    stop("mojor_transpile: ", operation, " argument '", x_name, "' is not a function parameter")
  }

  x_type <- types[[which(args == x_name)]]
  if (!.mojor_is_array(x_type)) {
    stop("mojor_transpile: ", operation, " requires array input, got: ", x_type)
  }
  x_type_info <- .mojor_expr_only_numeric_array_type_info(
    x_type,
    paste0("mojor_transpile: ", operation, " currently supports only f64[], f32[], i32[]")
  )
  type_suffix <- x_type_info$suffix

  table_name <- NULL
  if (operation %in% c("match", "%in%")) {
    if (!is.name(table_expr)) {
      stop("mojor_transpile: ", operation, " table argument must be a simple variable name")
    }
    table_name <- as.character(table_expr)
    if (!(table_name %in% args)) {
      stop("mojor_transpile: ", operation, " table argument '", table_name, "' is not a function parameter")
    }

    table_type <- types[[which(args == table_name)]]
    if (!.mojor_is_array(table_type)) {
      stop("mojor_transpile: ", operation, " table argument must be an array, got: ", table_type)
    }
    table_suffix <- .mojor_expr_only_numeric_array_type_info(
      table_type,
      paste0("mojor_transpile: ", operation, " table argument currently supports only f64[], f32[], i32[]")
    )$suffix

    if (!identical(type_suffix, table_suffix)) {
      stop("mojor_transpile: ", operation, " requires x and table to have matching element types")
    }
  }

  helper_import <- switch(operation,
    unique = paste0("from set_match_helpers import mojor_unique_", type_suffix),
    duplicated = paste0("from set_match_helpers import mojor_duplicated_", type_suffix),
    anyDuplicated = paste0("from set_match_helpers import mojor_any_duplicated_", type_suffix),
    match = paste0("from set_match_helpers import mojor_match_", type_suffix),
    `%in%` = paste0("from set_match_helpers import mojor_in_", type_suffix)
  )

  preamble <- .mojor_expr_only_pointer_preamble(
    "# Generated by mojor_transpile (expression-only mode - compiled subset set/match)",
    imports = helper_import,
    include_mut_opaque = TRUE,
    include_immut_types = c("f64", "f32", "i32"),
    include_mut_types = c("f64", "f32", "i32")
  )

  if (operation == "anyDuplicated") {
    signature <- c(
      paste0('@export("', name, '", ABI="C")'),
      paste0("fn ", name, "("),
      paste0("    ", x_name, "_ptr: ImmutOpaqueAny,"),
      paste0("    __mojor_len_", x_name, ": Int32"),
      ") -> Int32:"
    )

    body <- c(
      .mojor_expr_only_bind_array_ptr_len(x_name, x_type_info$immut_ptr, x_type_info$cast),
      paste0("    return mojor_any_duplicated_", type_suffix, "(", x_name, ", n_", x_name, ")")
    )

    mojo_code <- paste(c(preamble, signature, body), collapse = "\n")
    return(list(
      mojo = mojo_code,
      is_expression_kernel = TRUE,
      return_type = "Int32",
      na_skip_args = x_name,
      set_match_needed = TRUE,
      set_match_op = operation,
      set_match_x_name = x_name,
      args = args,
      types = types
    ))
  }

  out_type <- if (operation == "unique") {
    x_type
  } else if (operation == "match") {
    "i32[]"
  } else {
    "lgl[]"
  }
  out_ptr_info <- .mojor_expr_only_array_ptr_info(out_type)

  params <- c(
    paste0("    ", x_name, "_ptr: ImmutOpaqueAny,"),
    paste0("    __mojor_len_", x_name, ": Int32,")
  )
  if (operation %in% c("match", "%in%")) {
    params <- c(
      params,
      paste0("    ", table_name, "_ptr: ImmutOpaqueAny,"),
      paste0("    __mojor_len_", table_name, ": Int32,")
    )
  }
  params <- c(params, "    out_ptr: MutOpaqueAny")

  signature <- c(
    paste0('@export("', name, '", ABI="C")'),
    paste0("fn ", name, "("),
    params,
    ") -> Int32:"
  )

  body <- c(
    .mojor_expr_only_bind_array_ptr_len(x_name, x_type_info$immut_ptr, x_type_info$cast),
    paste0("    var out: ", out_ptr_info$mut_ptr, " = out_ptr.bitcast[", out_ptr_info$cast, "]()")
  )

  if (operation %in% c("match", "%in%")) {
    body <- c(
      body,
      .mojor_expr_only_bind_array_ptr_len(table_name, x_type_info$immut_ptr, x_type_info$cast)
    )
  }

  if (operation == "unique") {
    body <- c(
      body,
      paste0("    return mojor_unique_", type_suffix, "(", x_name, ", n_", x_name, ", out)")
    )
  } else if (operation == "duplicated") {
    body <- c(
      body,
      paste0("    _ = mojor_duplicated_", type_suffix, "(", x_name, ", n_", x_name, ", out)"),
      paste0("    return Int32(n_", x_name, ")")
    )
  } else if (operation == "match") {
    body <- c(
      body,
      paste0("    _ = mojor_match_", type_suffix, "(", x_name, ", n_", x_name, ", ", table_name, ", n_", table_name, ", out)"),
      paste0("    return Int32(n_", x_name, ")")
    )
  } else if (operation == "%in%") {
    body <- c(
      body,
      paste0("    _ = mojor_in_", type_suffix, "(", x_name, ", n_", x_name, ", ", table_name, ", n_", table_name, ", out)"),
      paste0("    return Int32(n_", x_name, ")")
    )
  }

  mojo_code <- paste(c(preamble, signature, body), collapse = "\n")
  list(
    mojo = mojo_code,
    is_expression_kernel = TRUE,
    is_vector_output = TRUE,
    out_type = out_type,
    return_type = "Int32",
    na_skip_args = unique(c(x_name, if (!is.null(table_name)) table_name else character(0))),
    set_match_needed = TRUE,
    set_match_op = operation,
    set_match_x_name = x_name,
    args = args,
    types = types
  )
}

# Transpile quantile/robust-stat operations ()
.mojor_transpile_quantile_operation <- function(quantile_info, args, types, name = "mojor_kernel") {
  operation <- quantile_info$operation
  x_expr <- quantile_info$x

  if (!is.name(x_expr)) {
    stop("mojor_transpile: ", operation, " argument must be a simple variable name")
  }
  x_name <- as.character(x_expr)
  if (!(x_name %in% args)) {
    stop("mojor_transpile: ", operation, " argument '", x_name, "' is not a function parameter")
  }

  x_type <- types[[which(args == x_name)]]
  if (!.mojor_is_array(x_type)) {
    stop("mojor_transpile: ", operation, " requires array input, got: ", x_type)
  }
  x_type_info <- .mojor_expr_only_numeric_array_type_info(
    x_type,
    paste0("mojor_transpile: ", operation, " currently supports only f64[], f32[], i32[]")
  )
  type_suffix <- x_type_info$suffix

  na_rm_str <- if (isTRUE(quantile_info$na_rm)) "True" else "False"
  type_str <- as.integer(quantile_info$type)

  if (operation == "quantile") {
    probs_expr <- quantile_info$probs
    probs_name <- NULL
    vector_len_arg <- NULL
    vector_len_const <- NULL
    kernel_args <- args
    literal_array_args <- NULL

    if (is.name(probs_expr)) {
      probs_name <- as.character(probs_expr)
      if (!(probs_name %in% args)) {
        stop("mojor_transpile: quantile() probs argument '", probs_name, "' is not a function parameter")
      }
      probs_type <- types[[which(args == probs_name)]]
      if (!identical(probs_type, "f64[]")) {
        stop("mojor_transpile: quantile() probs currently supports only f64[] in expression-only mode")
      }
      vector_len_arg <- probs_name
    } else {
      parse_prob_scalar <- function(p) {
        if ((is.numeric(p) || is.integer(p)) && length(p) == 1 && !is.na(p)) {
          val <- as.numeric(p)
          if (is.finite(val)) {
            return(val)
          }
          return(NA_real_)
        }
        if (is.name(p)) {
          nm <- as.character(p)
          if (identical(nm, "pi")) {
            return(pi)
          }
          return(NA_real_)
        }
        if (!is.call(p)) {
          return(NA_real_)
        }
        op <- as.character(p[[1]])
        if (identical(op, "(") && length(p) == 2) {
          return(parse_prob_scalar(p[[2]]))
        }
        if (op %in% c("+", "-") && length(p) == 2) {
          inner <- parse_prob_scalar(p[[2]])
          if (is.na(inner)) return(NA_real_)
          return(if (identical(op, "+")) inner else -inner)
        }
        if (op %in% c("+", "-", "*", "/", "%%", "%/%", "^") && length(p) == 3) {
          lhs <- parse_prob_scalar(p[[2]])
          rhs <- parse_prob_scalar(p[[3]])
          if (is.na(lhs) || is.na(rhs)) {
            return(NA_real_)
          }
          out <- tryCatch(
            switch(op,
              "+" = lhs + rhs,
              "-" = lhs - rhs,
              "*" = lhs * rhs,
              "/" = lhs / rhs,
              "%%" = lhs %% rhs,
              "%/%" = lhs %/% rhs,
              "^" = lhs^rhs,
              NA_real_
            ),
            error = function(e) NA_real_
          )
          if (is.finite(out)) return(as.numeric(out))
          return(NA_real_)
        }
        if (op %in% c("min", "max") && length(p) >= 3) {
          vals <- vapply(as.list(p)[-1], parse_prob_scalar, numeric(1))
          if (any(is.na(vals))) {
            return(NA_real_)
          }
          out <- tryCatch(
            if (identical(op, "min")) min(vals) else max(vals),
            error = function(e) NA_real_
          )
          if (is.finite(out)) return(as.numeric(out))
          return(NA_real_)
        }
        if (op %in% c("as.numeric", "as.double", "as.integer") && length(p) == 2) {
          inner <- parse_prob_scalar(p[[2]])
          if (is.na(inner)) {
            return(NA_real_)
          }
          out <- tryCatch(
            if (identical(op, "as.integer")) as.integer(inner) else as.numeric(inner),
            error = function(e) NA_real_
          )
          if (length(out) == 1 && !is.na(out) && is.finite(as.numeric(out))) {
            return(as.numeric(out))
          }
          return(NA_real_)
        }
        NA_real_
      }
      parse_prob_vector <- function(expr) {
        if ((is.numeric(expr) || is.integer(expr)) && length(expr) > 0) {
          vals <- as.numeric(expr)
          if (all(!is.na(vals)) && all(is.finite(vals))) {
            return(vals)
          }
          return(NULL)
        }
        if (!is.call(expr)) {
          return(NULL)
        }
        op <- as.character(expr[[1]])
        if (identical(op, "(") && length(expr) == 2) {
          return(parse_prob_vector(expr[[2]]))
        }
        if (identical(op, "c") && length(expr) >= 2) {
          parts <- as.list(expr)[-1]
          vals <- vapply(parts, parse_prob_scalar, numeric(1))
          if (length(vals) > 0 && all(!is.na(vals)) && all(is.finite(vals))) {
            return(as.numeric(vals))
          }
          return(NULL)
        }
        if (identical(op, ":") && length(expr) == 3) {
          from <- parse_prob_scalar(expr[[2]])
          to <- parse_prob_scalar(expr[[3]])
          if (is.na(from) || is.na(to)) {
            return(NULL)
          }
          vals <- seq(from, to)
          if (length(vals) > 0 && all(!is.na(vals)) && all(is.finite(vals))) {
            return(as.numeric(vals))
          }
          return(NULL)
        }
        if (op %in% c("seq", "seq.int")) {
          parts <- as.list(expr)[-1]
          nms <- names(parts)
          nms <- if (is.null(nms)) rep("", length(parts)) else ifelse(is.na(nms), "", nms)
          pick_arg <- function(name, pos) {
            idx_named <- which(nms == name)
            if (length(idx_named) > 0) {
              return(parts[[idx_named[[1]]]])
            }
            if (pos <= length(parts) && !nzchar(nms[[pos]])) {
              return(parts[[pos]])
            }
            NULL
          }
          from_expr <- pick_arg("from", 1)
          to_expr <- pick_arg("to", 2)
          by_expr <- pick_arg("by", 3)
          length_out_expr <- pick_arg("length.out", 3)

          if (is.null(to_expr) && !is.null(from_expr) && is.null(by_expr) && is.null(length_out_expr)) {
            to_expr <- from_expr
            from_expr <- 1
          }
          from_val <- if (is.null(from_expr)) 1 else parse_prob_scalar(from_expr)
          to_val <- if (is.null(to_expr)) NA_real_ else parse_prob_scalar(to_expr)
          by_val <- if (is.null(by_expr)) NA_real_ else parse_prob_scalar(by_expr)
          length_out_val <- if (is.null(length_out_expr)) NA_real_ else parse_prob_scalar(length_out_expr)
          if (is.na(from_val) || is.na(to_val)) {
            return(NULL)
          }
          vals <- tryCatch(
            if (!is.na(length_out_val)) {
              seq(from = from_val, to = to_val, length.out = as.integer(length_out_val))
            } else if (!is.na(by_val)) {
              seq(from = from_val, to = to_val, by = by_val)
            } else {
              seq(from = from_val, to = to_val)
            },
            error = function(e) NULL
          )
          if (!is.null(vals) && length(vals) > 0 && all(!is.na(vals)) && all(is.finite(vals))) {
            return(as.numeric(vals))
          }
          return(NULL)
        }
        if (op %in% c("as.numeric", "as.double") && length(expr) == 2) {
          return(parse_prob_vector(expr[[2]]))
        }
        NULL
      }

      literal_probs <- parse_prob_vector(probs_expr)

      if (is.null(literal_probs) || length(literal_probs) == 0) {
        stop("mojor_transpile: quantile() probs must be a function parameter or supported compile-time numeric vector expression in expression-only mode")
      }

      probs_name <- "__mojor_norm_probs"
      if (probs_name %in% args) {
        stop("mojor_transpile: quantile() internal literal probs name collides with function parameter")
      }
      kernel_args <- c(args, probs_name)
      vector_len_const <- length(literal_probs)
      literal_array_args <- list()
      literal_array_args[[probs_name]] <- list(
        type = "f64[]",
        values = as.numeric(literal_probs)
      )
    }

    preamble <- .mojor_expr_only_pointer_preamble(
      "# Generated by mojor_transpile (expression-only mode - compiled subset quantile)",
      imports = paste0("from quantile_helpers import mojor_quantile_", type_suffix),
      include_mut_opaque = TRUE,
      include_immut_types = c("f64", "f32", "i32"),
      include_mut_types = c("f64")
    )

    signature <- c(
      paste0('@export("', name, '", ABI="C")'),
      paste0("fn ", name, "("),
      paste0("    ", x_name, "_ptr: ImmutOpaqueAny,"),
      paste0("    __mojor_len_", x_name, ": Int32,"),
      paste0("    ", probs_name, "_ptr: ImmutOpaqueAny,"),
      paste0("    __mojor_len_", probs_name, ": Int32,"),
      "    out_ptr: MutOpaqueAny",
      ") -> Int32:"
    )

    body <- c(
      .mojor_expr_only_bind_array_ptr_len(x_name, x_type_info$immut_ptr, x_type_info$cast),
      paste0("    var ", probs_name, ": ImmutFloat64Ptr = ", probs_name, "_ptr.bitcast[Float64]()"),
      paste0("    var n_", probs_name, " = Int(__mojor_len_", probs_name, ")"),
      "    var out: MutFloat64Ptr = out_ptr.bitcast[Float64]()",
      paste0("    _ = mojor_quantile_", type_suffix, "(", x_name, ", n_", x_name, ", ", probs_name, ", n_", probs_name, ", ", type_str, ", ", na_rm_str, ", out)"),
      paste0("    return Int32(n_", probs_name, ")")
    )

    mojo_code <- paste(c(preamble, signature, body), collapse = "\n")
    return(list(
      mojo = mojo_code,
      is_expression_kernel = TRUE,
      is_vector_output = TRUE,
      out_type = "f64[]",
      return_type = "Int32",
      vector_len_arg = vector_len_arg,
      vector_len_const = vector_len_const,
      kernel_args = kernel_args,
      literal_array_args = literal_array_args,
      quantile_needed = TRUE,
      quantile_op = operation,
      args = args,
      types = types
    ))
  }

  helper_imports <- switch(operation,
    median = c(
      paste0("from quantile_helpers import mojor_median_", type_suffix)
    ),
    IQR = c(
      paste0("from quantile_helpers import mojor_iqr_", type_suffix)
    ),
    mad = c(
      paste0("from quantile_helpers import mojor_median_", type_suffix),
      paste0("from quantile_helpers import mojor_mad_", type_suffix)
    )
  )

  preamble <- .mojor_expr_only_pointer_preamble(
    "# Generated by mojor_transpile (expression-only mode - compiled subset quantile/robust)",
    imports = helper_imports,
    include_mut_opaque = FALSE,
    include_immut_types = c("f64", "f32", "i32"),
    include_mut_types = character(0)
  )

  signature <- c(
    paste0('@export("', name, '", ABI="C")'),
    paste0("fn ", name, "("),
    paste0("    ", x_name, "_ptr: ImmutOpaqueAny,"),
    paste0("    __mojor_len_", x_name, ": Int32"),
    ") -> Float64:"
  )

  body <- c(
    .mojor_expr_only_bind_array_ptr_len(x_name, x_type_info$immut_ptr, x_type_info$cast)
  )
  if (operation == "median") {
    body <- c(
      body,
      paste0("    return mojor_median_", type_suffix, "(", x_name, ", n_", x_name, ", ", na_rm_str, ")")
    )
  } else if (operation == "IQR") {
    body <- c(
      body,
      paste0("    return mojor_iqr_", type_suffix, "(", x_name, ", n_", x_name, ", ", na_rm_str, ")")
    )
  } else if (operation == "mad") {
    body <- c(
      body,
      paste0("    var center = mojor_median_", type_suffix, "(", x_name, ", n_", x_name, ", ", na_rm_str, ")"),
      paste0("    return mojor_mad_", type_suffix, "(", x_name, ", n_", x_name, ", center, ", na_rm_str, ", 1.4826, ", type_str, ")")
    )
  }

  mojo_code <- paste(c(preamble, signature, body), collapse = "\n")
  list(
    mojo = mojo_code,
    is_expression_kernel = TRUE,
    return_type = "Float64",
    quantile_needed = TRUE,
    quantile_op = operation,
    args = args,
    types = types
  )
}

# Helper: Detect sampling operations in expression-only mode
.mojor_detect_sampling_operation <- function(blocks) {
  call_info <- .mojor_expr_only_supported_call_or_false(
    blocks,
    c("sample.int", "sample"),
    "is_sampling"
  )
  if (is.null(call_info$block)) {
    return(call_info)
  }
  block <- call_info$block
  op_name <- call_info$op_name

  call_parts <- .mojor_expr_only_call_parts(block)
  parts <- call_parts$parts
  nms <- call_parts$nms
  validate_sampling_signature <- function(op_label, allowed_named, max_positional = 4L) {
    named_args <- nms[nzchar(nms)]
    if (length(named_args) > 0) {
      bad_named <- setdiff(unique(named_args), allowed_named)
      if (length(bad_named) > 0) {
        stop(
          "mojor_transpile: ", op_label, "() supports only ",
          paste(allowed_named, collapse = ", "),
          " arguments in expression-only mode"
        )
      }
      dup_named <- names(which(table(named_args) > 1L))
      if (length(dup_named) > 0) {
        stop("mojor_transpile: ", op_label, "() duplicate named arguments are not supported in expression-only mode")
      }
    }
    positional_count <- sum(!nzchar(nms))
    if (positional_count > max_positional) {
      stop("mojor_transpile: ", op_label, "() supports at most ", max_positional, " positional arguments in expression-only mode")
    }
  }
  if (op_name == "sample.int") {
    validate_sampling_signature("sample.int", c("n", "size", "replace", "prob"), max_positional = 4L)
    n_expr <- .mojor_expr_only_get_arg(parts, nms, "n", 1)
    size_expr <- .mojor_expr_only_get_arg(parts, nms, "size", 2)
    replace_expr <- .mojor_expr_only_get_arg(parts, nms, "replace", 3)
    prob_expr <- .mojor_expr_only_get_arg(parts, nms, "prob", 4)

    if (is.null(n_expr)) {
      stop("mojor_transpile: sample.int() requires n in expression-only mode")
    }
    if (is.null(size_expr)) {
      size_expr <- n_expr
    }

    return(list(
      is_sampling = TRUE,
      operation = "sample_int",
      call_expr = block,
      n_expr = n_expr,
      size_expr = size_expr,
      replace = replace_expr
    ))
  }

  validate_sampling_signature("sample", c("x", "X", "size", "replace", "prob"), max_positional = 4L)
  if (sum(nms %in% c("x", "X")) > 1L) {
    stop("mojor_transpile: sample() duplicate x/X arguments are not supported in expression-only mode")
  }
  x_expr <- .mojor_expr_only_get_arg(parts, nms, "x", 1, aliases = "X")
  size_expr <- .mojor_expr_only_get_arg(parts, nms, "size", 2)
  replace_expr <- .mojor_expr_only_get_arg(parts, nms, "replace", 3)
  prob_expr <- .mojor_expr_only_get_arg(parts, nms, "prob", 4)

  if (is.null(x_expr)) {
    stop("mojor_transpile: sample() requires x in expression-only mode")
  }
  if (!is.name(x_expr)) {
    stop("mojor_transpile: sample() requires x to be a direct vector argument in expression-only mode")
  }

  list(
    is_sampling = TRUE,
    operation = "sample",
    call_expr = block,
    x_expr = x_expr,
    size_expr = size_expr,
    replace = replace_expr
  )
}

# Transpile sampling operations (sample.int/sample) in expression-only mode
.mojor_transpile_sampling_operation <- function(sampling_info, args, types, name = "mojor_kernel") {
  operation <- sampling_info$operation

  parse_i32_scalar_ir <- function(node_expr, arg_label, op_label, allow_len_x = NULL) {
    if (!is.list(node_expr) || is.null(node_expr$kind)) {
      stop("mojor_transpile: ", op_label, "() ", arg_label, " must be a direct i32 argument or integer literal in expression-only mode")
    }
    if (identical(node_expr$kind, "var")) {
      nm <- node_expr$name
      if (!(nm %in% args)) {
        stop("mojor_transpile: ", op_label, "() argument '", nm, "' is not a function parameter")
      }
      spec <- types[[which(args == nm)]]
      if (!identical(spec, "i32")) {
        stop("mojor_transpile: ", op_label, "() argument '", nm, "' must be typed as i32 in expression-only mode")
      }
      return(list(kind = "arg", name = nm, mojo = paste0("Int(", nm, "_val)")))
    }
    if (identical(node_expr$kind, "const")) {
      v <- suppressWarnings(as.numeric(node_expr$value))
      if (length(v) != 1 || is.na(v) || !is.finite(v)) {
        stop("mojor_transpile: ", op_label, "() ", arg_label, " must be a direct i32 argument or integer literal in expression-only mode")
      }
      if (!isTRUE(all.equal(v, as.integer(v), tolerance = 0))) {
        stop("mojor_transpile: ", op_label, "() ", arg_label, " must be integer-valued in expression-only mode")
      }
      return(list(kind = "const", value = as.integer(v), mojo = as.character(as.integer(v))))
    }
    if (!is.null(allow_len_x) &&
      identical(node_expr$kind, "call") &&
      identical(node_expr$fn, "length") &&
      length(node_expr$args) == 1 &&
      !is.null(node_expr$args[[1]]) &&
      identical(node_expr$args[[1]]$kind, "var") &&
      identical(node_expr$args[[1]]$name, allow_len_x)) {
      return(list(kind = "len_x", name = allow_len_x, mojo = paste0("n_", allow_len_x)))
    }
    stop("mojor_transpile: ", op_label, "() ", arg_label, " must be a direct i32 argument or integer literal in expression-only mode")
  }

  parse_replace_mode_ir <- function(replace_node, op_label) {
    if (is.null(replace_node)) {
      return(list(mode = "const", value = FALSE))
    }
    if (!is.list(replace_node) || is.null(replace_node$kind)) {
      stop("mojor_transpile: ", op_label, "() requires replace to be a scalar lgl/bool/i32 argument or literal TRUE/FALSE in expression-only mode")
    }
    if (identical(replace_node$kind, "const")) {
      rv <- replace_node$value
      if (is.logical(rv) && length(rv) == 1 && !is.na(rv)) {
        return(list(mode = "const", value = isTRUE(rv)))
      }
      if (is.numeric(rv) && length(rv) == 1 && !is.na(rv) && rv %in% c(0, 1)) {
        return(list(mode = "const", value = as.logical(rv)))
      }
      if (is.character(rv) && length(rv) == 1) {
        if (identical(toupper(rv), "TRUE")) {
          return(list(mode = "const", value = TRUE))
        }
        if (identical(toupper(rv), "FALSE")) {
          return(list(mode = "const", value = FALSE))
        }
      }
      stop("mojor_transpile: ", op_label, "() requires replace to be a scalar lgl/bool/i32 argument or literal TRUE/FALSE in expression-only mode")
    }
    if (!identical(replace_node$kind, "var")) {
      stop("mojor_transpile: ", op_label, "() requires replace to be a scalar lgl/bool/i32 argument or literal TRUE/FALSE in expression-only mode")
    }
    nm <- replace_node$name
    if (!(nm %in% args)) {
      stop("mojor_transpile: ", op_label, "() argument '", nm, "' is not a function parameter")
    }
    spec <- types[[which(args == nm)]]
    if (!(spec %in% c("lgl", "bool", "i32"))) {
      stop("mojor_transpile: ", op_label, "() replace argument '", nm, "' must be typed as lgl, bool, or i32 in expression-only mode")
    }
    list(mode = "expr", name = nm, mojo = paste0("(", nm, "_val != 0)"))
  }
  parse_prob_mode_ir <- function(prob_node, op_label) {
    is_null_prob <- function(node_prob) {
      if (is.null(node_prob)) {
        return(TRUE)
      }
      if (!is.list(node_prob) || is.null(node_prob$kind)) {
        return(FALSE)
      }
      if (identical(node_prob$kind, "var") && identical(node_prob$name, "NULL")) {
        return(TRUE)
      }
      if (identical(node_prob$kind, "const")) {
        val <- node_prob$value
        if (is.character(val) && length(val) == 1 && identical(toupper(val), "NULL")) {
          return(TRUE)
        }
      }
      FALSE
    }
    if (is_null_prob(prob_node)) {
      return(list(mode = "null"))
    }
    if (!(is.list(prob_node) && identical(prob_node$kind, "var"))) {
      stop("mojor_transpile: ", op_label, "() weighted sampling currently requires prob to be a direct f64[] argument in expression-only mode")
    }
    nm <- prob_node$name
    if (!(nm %in% args)) {
      stop("mojor_transpile: ", op_label, "() prob argument '", nm, "' is not a function parameter")
    }
    spec <- types[[which(args == nm)]]
    if (!identical(spec, "f64[]")) {
      stop("mojor_transpile: ", op_label, "() weighted sampling requires prob argument '", nm, "' to be typed f64[] in expression-only mode")
    }
    list(mode = "arg", name = nm)
  }

  sample_node <- NULL
  if (!is.null(sampling_info$call_expr)) {
    sample_node <- .mojor_ir_expr_build(sampling_info$call_expr)
  }
  if (is.null(sample_node) || is.null(sample_node$kind)) {
    stop("mojor_transpile: failed to lower sampling call into IR node in expression-only mode")
  }
  if (operation == "sample_int" && !identical(sample_node$kind, "sample_int")) {
    stop("mojor_transpile: internal sampling lowering mismatch for sample.int()")
  }
  if (operation == "sample" && !identical(sample_node$kind, "sample")) {
    stop("mojor_transpile: internal sampling lowering mismatch for sample()")
  }

  kernel_args <- character(0)
  out_type <- "i32[]"
  body <- character(0)
  vector_len_arg <- NULL
  vector_len_const <- NULL

  if (operation == "sample_int") {
    prob_mode <- parse_prob_mode_ir(sample_node$prob, "sample.int")
    replace_mode <- parse_replace_mode_ir(sample_node$replace, "sample.int")
    n_info <- parse_i32_scalar_ir(sample_node$n, "n", "sample.int")
    size_info <- parse_i32_scalar_ir(sample_node$size, "size", "sample.int")
    if (identical(n_info$kind, "arg")) kernel_args <- c(kernel_args, n_info$name)
    if (identical(size_info$kind, "arg")) kernel_args <- c(kernel_args, size_info$name)
    if (identical(prob_mode$mode, "arg")) kernel_args <- c(kernel_args, prob_mode$name)
    if (identical(replace_mode$mode, "expr")) kernel_args <- c(kernel_args, replace_mode$name)
    kernel_args <- unique(kernel_args)

    body <- c(
      body,
      paste0("    var n_total = ", n_info$mojo),
      paste0("    var out_n = Int(", size_info$mojo, ")"),
      "    if n_total <= 0:",
      "        return Int32(-1)",
      "    if out_n < 0:",
      "        return Int32(-1)"
    )
    if (identical(prob_mode$mode, "arg")) {
      body <- c(
        body,
        .mojor_expr_only_bind_array_ptr_len(prob_mode$name, "ImmutFloat64Ptr", "Float64")
      )
      if (identical(replace_mode$mode, "expr")) {
        body <- c(
          body,
          paste0("    var replace_enabled = ", replace_mode$mojo),
          "    if not replace_enabled and out_n > n_total:",
          "        return Int32(-1)"
        )
      } else if (!isTRUE(replace_mode$value)) {
        body <- c(
          body,
          "    if out_n > n_total:",
          "        return Int32(-1)"
        )
      }
    } else if (identical(replace_mode$mode, "expr")) {
      body <- c(
        body,
        paste0("    var replace_enabled = ", replace_mode$mojo),
        "    if not replace_enabled and out_n > n_total:",
        "        return Int32(-1)"
      )
    } else if (!isTRUE(replace_mode$value)) {
      body <- c(
        body,
        "    if out_n > n_total:",
        "        return Int32(-1)"
      )
    }

    body <- c(
      body,
      "    var out: MutInt32Ptr = out_ptr.bitcast[Int32]()"
    )
    if (identical(prob_mode$mode, "arg")) {
      if (identical(replace_mode$mode, "expr")) {
        body <- c(
          body,
          "    if replace_enabled:",
          "        for i in range(1, out_n + 1):",
          "            var idx: Int = 1",
          paste0("            if n_", prob_mode$name, " == n_total:"),
          "                var prob_total: Float64 = 0.0",
          paste0("                for prob_j in range(n_", prob_mode$name, "):"),
          paste0("                    var prob_w = ", prob_mode$name, "[Int(prob_j)]"),
          "                    if prob_w > 0.0:",
          "                        prob_total += prob_w",
          "                if prob_total > 0.0:",
          "                    var prob_target = _rng_next_f64(__mojor_rng_state) * prob_total",
          "                    var prob_acc: Float64 = 0.0",
          "                    idx = n_total",
          paste0("                    for prob_j in range(n_", prob_mode$name, "):"),
          paste0("                        var prob_w = ", prob_mode$name, "[Int(prob_j)]"),
          "                        if prob_w > 0.0:",
          "                            prob_acc += prob_w",
          "                            if prob_target <= prob_acc:",
          "                                idx = Int(prob_j + 1)",
          "                                break",
          "                else:",
          "                    idx = Int((_rng_next_f64(__mojor_rng_state) * Float64(n_total)) + 1.0)",
          "                    if idx > n_total:",
          "                        idx = n_total",
          "            else:",
          "                idx = Int((_rng_next_f64(__mojor_rng_state) * Float64(n_total)) + 1.0)",
          "                if idx > n_total:",
          "                    idx = n_total",
          "            out[Int(i - 1)] = Int32(idx)",
          "    else:",
          "        var used = Dict[Int, Bool]()",
          "        for i in range(1, out_n + 1):",
          "            while True:",
          "                var idx: Int = 1",
          paste0("                if n_", prob_mode$name, " == n_total:"),
          "                    var prob_total: Float64 = 0.0",
          paste0("                    for prob_j in range(n_", prob_mode$name, "):"),
          paste0("                        var prob_w = ", prob_mode$name, "[Int(prob_j)]"),
          "                        if prob_w > 0.0:",
          "                            prob_total += prob_w",
          "                    if prob_total > 0.0:",
          "                        var prob_target = _rng_next_f64(__mojor_rng_state) * prob_total",
          "                        var prob_acc: Float64 = 0.0",
          "                        idx = n_total",
          paste0("                        for prob_j in range(n_", prob_mode$name, "):"),
          paste0("                            var prob_w = ", prob_mode$name, "[Int(prob_j)]"),
          "                            if prob_w > 0.0:",
          "                                prob_acc += prob_w",
          "                                if prob_target <= prob_acc:",
          "                                    idx = Int(prob_j + 1)",
          "                                    break",
          "                    else:",
          "                        idx = Int((_rng_next_f64(__mojor_rng_state) * Float64(n_total)) + 1.0)",
          "                        if idx > n_total:",
          "                            idx = n_total",
          "                else:",
          "                    idx = Int((_rng_next_f64(__mojor_rng_state) * Float64(n_total)) + 1.0)",
          "                    if idx > n_total:",
          "                        idx = n_total",
          "                if idx not in used:",
          "                    used[idx] = True",
          "                    out[Int(i - 1)] = Int32(idx)",
          "                    break"
        )
      } else if (isTRUE(replace_mode$value)) {
        body <- c(
          body,
          "    for i in range(1, out_n + 1):",
          "        var idx: Int = 1",
          paste0("        if n_", prob_mode$name, " == n_total:"),
          "            var prob_total: Float64 = 0.0",
          paste0("            for prob_j in range(n_", prob_mode$name, "):"),
          paste0("                var prob_w = ", prob_mode$name, "[Int(prob_j)]"),
          "                if prob_w > 0.0:",
          "                    prob_total += prob_w",
          "            if prob_total > 0.0:",
          "                var prob_target = _rng_next_f64(__mojor_rng_state) * prob_total",
          "                var prob_acc: Float64 = 0.0",
          "                idx = n_total",
          paste0("                for prob_j in range(n_", prob_mode$name, "):"),
          paste0("                    var prob_w = ", prob_mode$name, "[Int(prob_j)]"),
          "                    if prob_w > 0.0:",
          "                        prob_acc += prob_w",
          "                        if prob_target <= prob_acc:",
          "                            idx = Int(prob_j + 1)",
          "                            break",
          "            else:",
          "                idx = Int((_rng_next_f64(__mojor_rng_state) * Float64(n_total)) + 1.0)",
          "                if idx > n_total:",
          "                    idx = n_total",
          "        else:",
          "            idx = Int((_rng_next_f64(__mojor_rng_state) * Float64(n_total)) + 1.0)",
          "            if idx > n_total:",
          "                idx = n_total",
          "        out[Int(i - 1)] = Int32(idx)"
        )
      } else {
        body <- c(
          body,
          "    var used = Dict[Int, Bool]()",
          "    for i in range(1, out_n + 1):",
          "        while True:",
          "            var idx: Int = 1",
          paste0("            if n_", prob_mode$name, " == n_total:"),
          "                var prob_total: Float64 = 0.0",
          paste0("                for prob_j in range(n_", prob_mode$name, "):"),
          paste0("                    var prob_w = ", prob_mode$name, "[Int(prob_j)]"),
          "                    if prob_w > 0.0:",
          "                        prob_total += prob_w",
          "                if prob_total > 0.0:",
          "                    var prob_target = _rng_next_f64(__mojor_rng_state) * prob_total",
          "                    var prob_acc: Float64 = 0.0",
          "                    idx = n_total",
          paste0("                    for prob_j in range(n_", prob_mode$name, "):"),
          paste0("                        var prob_w = ", prob_mode$name, "[Int(prob_j)]"),
          "                        if prob_w > 0.0:",
          "                            prob_acc += prob_w",
          "                            if prob_target <= prob_acc:",
          "                                idx = Int(prob_j + 1)",
          "                                break",
          "                else:",
          "                    idx = Int((_rng_next_f64(__mojor_rng_state) * Float64(n_total)) + 1.0)",
          "                    if idx > n_total:",
          "                        idx = n_total",
          "            else:",
          "                idx = Int((_rng_next_f64(__mojor_rng_state) * Float64(n_total)) + 1.0)",
          "                if idx > n_total:",
          "                    idx = n_total",
          "            if idx not in used:",
          "                used[idx] = True",
          "                out[Int(i - 1)] = Int32(idx)",
          "                break"
        )
      }
    } else if (identical(replace_mode$mode, "expr")) {
      body <- c(
        body,
        "    if replace_enabled:",
        "        for i in range(1, out_n + 1):",
        "            var idx = Int((_rng_next_f64(__mojor_rng_state) * Float64(n_total)) + 1.0)",
        "            if idx > n_total:",
        "                idx = n_total",
        "            out[Int(i - 1)] = Int32(idx)",
        "    else:",
        "        var used = Dict[Int, Bool]()",
        "        for i in range(1, out_n + 1):",
        "            while True:",
        "                var idx = Int((_rng_next_f64(__mojor_rng_state) * Float64(n_total)) + 1.0)",
        "                if idx > n_total:",
        "                    idx = n_total",
        "                if idx not in used:",
        "                    used[idx] = True",
        "                    out[Int(i - 1)] = Int32(idx)",
        "                    break"
      )
    } else if (isTRUE(replace_mode$value)) {
      body <- c(
        body,
        "    for i in range(1, out_n + 1):",
        "        var idx = Int((_rng_next_f64(__mojor_rng_state) * Float64(n_total)) + 1.0)",
        "        if idx > n_total:",
        "            idx = n_total",
        "        out[Int(i - 1)] = Int32(idx)"
      )
    } else {
      body <- c(
        body,
        "    var used = Dict[Int, Bool]()",
        "    for i in range(1, out_n + 1):",
        "        while True:",
        "            var idx = Int((_rng_next_f64(__mojor_rng_state) * Float64(n_total)) + 1.0)",
        "            if idx > n_total:",
        "                idx = n_total",
        "            if idx not in used:",
        "                used[idx] = True",
        "                out[Int(i - 1)] = Int32(idx)",
        "                break"
      )
    }
    if (identical(size_info$kind, "arg")) vector_len_arg <- size_info$name
    if (identical(size_info$kind, "const")) vector_len_const <- as.integer(size_info$value)
  } else {
    prob_mode <- parse_prob_mode_ir(sample_node$prob, "sample")
    replace_mode <- parse_replace_mode_ir(sample_node$replace, "sample")
    if (is.null(sample_node$x) || !identical(sample_node$x$kind, "var")) {
      stop("mojor_transpile: sample() requires x to be a direct vector argument in expression-only mode")
    }
    x_name <- sample_node$x$name
    if (!(x_name %in% args)) {
      stop("mojor_transpile: sample() argument '", x_name, "' is not a function parameter")
    }
    x_type <- types[[which(args == x_name)]]
    if (!(x_type %in% c("f64[]", "i32[]", "lgl[]"))) {
      stop("mojor_transpile: sample() currently supports only f64[], i32[], lgl[] in expression-only mode")
    }
    kernel_args <- c(kernel_args, x_name)

    size_info <- parse_i32_scalar_ir(sample_node$size, "size", "sample", allow_len_x = x_name)
    if (identical(prob_mode$mode, "arg")) kernel_args <- c(kernel_args, prob_mode$name)
    if (identical(size_info$kind, "arg")) kernel_args <- c(kernel_args, size_info$name)
    if (identical(replace_mode$mode, "expr")) kernel_args <- c(kernel_args, replace_mode$name)
    kernel_args <- unique(kernel_args)

    x_ptr_info <- .mojor_expr_only_array_ptr_info(x_type)
    out_type <- if (identical(x_type, "f64[]")) "f64[]" else if (identical(x_type, "i32[]")) "i32[]" else "lgl[]"
    out_ptr_info <- .mojor_expr_only_array_ptr_info(out_type)

    body <- c(
      body,
      .mojor_expr_only_bind_array_ptr_len(x_name, x_ptr_info$immut_ptr, x_ptr_info$cast),
      paste0("    var out_n = Int(", size_info$mojo, ")"),
      paste0("    if n_", x_name, " <= 0:"),
      "        return Int32(-1)",
      "    if out_n < 0:",
      "        return Int32(-1)"
    )
    if (identical(prob_mode$mode, "arg")) {
      body <- c(
        body,
        .mojor_expr_only_bind_array_ptr_len(prob_mode$name, "ImmutFloat64Ptr", "Float64")
      )
      if (identical(replace_mode$mode, "expr")) {
        body <- c(
          body,
          paste0("    var replace_enabled = ", replace_mode$mojo),
          paste0("    if not replace_enabled and out_n > n_", x_name, ":"),
          "        return Int32(-1)"
        )
      } else if (!isTRUE(replace_mode$value)) {
        body <- c(
          body,
          paste0("    if out_n > n_", x_name, ":"),
          "        return Int32(-1)"
        )
      }
    } else if (identical(replace_mode$mode, "expr")) {
      body <- c(
        body,
        paste0("    var replace_enabled = ", replace_mode$mojo),
        paste0("    if not replace_enabled and out_n > n_", x_name, ":"),
        "        return Int32(-1)"
      )
    } else if (!isTRUE(replace_mode$value)) {
      body <- c(
        body,
        paste0("    if out_n > n_", x_name, ":"),
        "        return Int32(-1)"
      )
    }

    body <- c(
      body,
      paste0("    var out: ", out_ptr_info$mut_ptr, " = out_ptr.bitcast[", out_ptr_info$cast, "]()")
    )
    if (identical(prob_mode$mode, "arg")) {
      if (identical(replace_mode$mode, "expr")) {
        body <- c(
          body,
          "    if replace_enabled:",
          "        for i in range(1, out_n + 1):",
          "            var idx: Int = 1",
          paste0("            if n_", prob_mode$name, " == n_", x_name, ":"),
          "                var prob_total: Float64 = 0.0",
          paste0("                for prob_j in range(n_", x_name, "):"),
          paste0("                    var prob_w = ", prob_mode$name, "[Int(prob_j)]"),
          "                    if prob_w > 0.0:",
          "                        prob_total += prob_w",
          "                if prob_total > 0.0:",
          "                    var prob_target = _rng_next_f64(__mojor_rng_state) * prob_total",
          "                    var prob_acc: Float64 = 0.0",
          paste0("                    idx = n_", x_name),
          paste0("                    for prob_j in range(n_", x_name, "):"),
          paste0("                        var prob_w = ", prob_mode$name, "[Int(prob_j)]"),
          "                        if prob_w > 0.0:",
          "                            prob_acc += prob_w",
          "                            if prob_target <= prob_acc:",
          "                                idx = Int(prob_j + 1)",
          "                                break",
          "                else:",
          paste0("                    idx = Int((_rng_next_f64(__mojor_rng_state) * Float64(n_", x_name, ")) + 1.0)"),
          paste0("                    if idx > n_", x_name, ":"),
          paste0("                        idx = n_", x_name),
          "            else:",
          paste0("                idx = Int((_rng_next_f64(__mojor_rng_state) * Float64(n_", x_name, ")) + 1.0)"),
          paste0("                if idx > n_", x_name, ":"),
          paste0("                    idx = n_", x_name),
          paste0("            out[Int(i - 1)] = ", out_ptr_info$cast, "(", x_name, "[Int(idx - 1)])"),
          "    else:",
          "        var used = Dict[Int, Bool]()",
          "        for i in range(1, out_n + 1):",
          "            while True:",
          "                var idx: Int = 1",
          paste0("                if n_", prob_mode$name, " == n_", x_name, ":"),
          "                    var prob_total: Float64 = 0.0",
          paste0("                    for prob_j in range(n_", x_name, "):"),
          paste0("                        var prob_w = ", prob_mode$name, "[Int(prob_j)]"),
          "                        if prob_w > 0.0:",
          "                            prob_total += prob_w",
          "                    if prob_total > 0.0:",
          "                        var prob_target = _rng_next_f64(__mojor_rng_state) * prob_total",
          "                        var prob_acc: Float64 = 0.0",
          paste0("                        idx = n_", x_name),
          paste0("                        for prob_j in range(n_", x_name, "):"),
          paste0("                            var prob_w = ", prob_mode$name, "[Int(prob_j)]"),
          "                            if prob_w > 0.0:",
          "                                prob_acc += prob_w",
          "                                if prob_target <= prob_acc:",
          "                                    idx = Int(prob_j + 1)",
          "                                    break",
          "                    else:",
          paste0("                        idx = Int((_rng_next_f64(__mojor_rng_state) * Float64(n_", x_name, ")) + 1.0)"),
          paste0("                        if idx > n_", x_name, ":"),
          paste0("                            idx = n_", x_name),
          "                else:",
          paste0("                    idx = Int((_rng_next_f64(__mojor_rng_state) * Float64(n_", x_name, ")) + 1.0)"),
          paste0("                    if idx > n_", x_name, ":"),
          paste0("                        idx = n_", x_name),
          "                if idx not in used:",
          "                    used[idx] = True",
          paste0("                    out[Int(i - 1)] = ", out_ptr_info$cast, "(", x_name, "[Int(idx - 1)])"),
          "                    break"
        )
      } else if (isTRUE(replace_mode$value)) {
        body <- c(
          body,
          "    for i in range(1, out_n + 1):",
          "        var idx: Int = 1",
          paste0("        if n_", prob_mode$name, " == n_", x_name, ":"),
          "            var prob_total: Float64 = 0.0",
          paste0("            for prob_j in range(n_", x_name, "):"),
          paste0("                var prob_w = ", prob_mode$name, "[Int(prob_j)]"),
          "                if prob_w > 0.0:",
          "                    prob_total += prob_w",
          "            if prob_total > 0.0:",
          "                var prob_target = _rng_next_f64(__mojor_rng_state) * prob_total",
          "                var prob_acc: Float64 = 0.0",
          paste0("                idx = n_", x_name),
          paste0("                for prob_j in range(n_", x_name, "):"),
          paste0("                    var prob_w = ", prob_mode$name, "[Int(prob_j)]"),
          "                    if prob_w > 0.0:",
          "                        prob_acc += prob_w",
          "                        if prob_target <= prob_acc:",
          "                            idx = Int(prob_j + 1)",
          "                            break",
          "            else:",
          paste0("                idx = Int((_rng_next_f64(__mojor_rng_state) * Float64(n_", x_name, ")) + 1.0)"),
          paste0("                if idx > n_", x_name, ":"),
          paste0("                    idx = n_", x_name),
          "        else:",
          paste0("            idx = Int((_rng_next_f64(__mojor_rng_state) * Float64(n_", x_name, ")) + 1.0)"),
          paste0("            if idx > n_", x_name, ":"),
          paste0("                idx = n_", x_name),
          paste0("        out[Int(i - 1)] = ", out_ptr_info$cast, "(", x_name, "[Int(idx - 1)])")
        )
      } else {
        body <- c(
          body,
          "    var used = Dict[Int, Bool]()",
          "    for i in range(1, out_n + 1):",
          "        while True:",
          "            var idx: Int = 1",
          paste0("            if n_", prob_mode$name, " == n_", x_name, ":"),
          "                var prob_total: Float64 = 0.0",
          paste0("                for prob_j in range(n_", x_name, "):"),
          paste0("                    var prob_w = ", prob_mode$name, "[Int(prob_j)]"),
          "                    if prob_w > 0.0:",
          "                        prob_total += prob_w",
          "                if prob_total > 0.0:",
          "                    var prob_target = _rng_next_f64(__mojor_rng_state) * prob_total",
          "                    var prob_acc: Float64 = 0.0",
          paste0("                    idx = n_", x_name),
          paste0("                    for prob_j in range(n_", x_name, "):"),
          paste0("                        var prob_w = ", prob_mode$name, "[Int(prob_j)]"),
          "                        if prob_w > 0.0:",
          "                            prob_acc += prob_w",
          "                            if prob_target <= prob_acc:",
          "                                idx = Int(prob_j + 1)",
          "                                break",
          "                else:",
          paste0("                    idx = Int((_rng_next_f64(__mojor_rng_state) * Float64(n_", x_name, ")) + 1.0)"),
          paste0("                    if idx > n_", x_name, ":"),
          paste0("                        idx = n_", x_name),
          "            else:",
          paste0("                idx = Int((_rng_next_f64(__mojor_rng_state) * Float64(n_", x_name, ")) + 1.0)"),
          paste0("                if idx > n_", x_name, ":"),
          paste0("                    idx = n_", x_name),
          "            if idx not in used:",
          "                used[idx] = True",
          paste0("                out[Int(i - 1)] = ", out_ptr_info$cast, "(", x_name, "[Int(idx - 1)])"),
          "                break"
        )
      }
    } else if (identical(replace_mode$mode, "expr")) {
      body <- c(
        body,
        "    if replace_enabled:",
        "        for i in range(1, out_n + 1):",
        paste0("            var idx = Int((_rng_next_f64(__mojor_rng_state) * Float64(n_", x_name, ")) + 1.0)"),
        paste0("            if idx > n_", x_name, ":"),
        paste0("                idx = n_", x_name),
        paste0("            out[Int(i - 1)] = ", out_ptr_info$cast, "(", x_name, "[Int(idx - 1)])"),
        "    else:",
        "        var used = Dict[Int, Bool]()",
        "        for i in range(1, out_n + 1):",
        "            while True:",
        paste0("                var idx = Int((_rng_next_f64(__mojor_rng_state) * Float64(n_", x_name, ")) + 1.0)"),
        paste0("                if idx > n_", x_name, ":"),
        paste0("                    idx = n_", x_name),
        "                if idx not in used:",
        "                    used[idx] = True",
        paste0("                    out[Int(i - 1)] = ", out_ptr_info$cast, "(", x_name, "[Int(idx - 1)])"),
        "                    break"
      )
    } else if (isTRUE(replace_mode$value)) {
      body <- c(
        body,
        "    for i in range(1, out_n + 1):",
        paste0("        var idx = Int((_rng_next_f64(__mojor_rng_state) * Float64(n_", x_name, ")) + 1.0)"),
        paste0("        if idx > n_", x_name, ":"),
        paste0("            idx = n_", x_name),
        paste0("        out[Int(i - 1)] = ", out_ptr_info$cast, "(", x_name, "[Int(idx - 1)])")
      )
    } else {
      body <- c(
        body,
        "    var used = Dict[Int, Bool]()",
        "    for i in range(1, out_n + 1):",
        "        while True:",
        paste0("            var idx = Int((_rng_next_f64(__mojor_rng_state) * Float64(n_", x_name, ")) + 1.0)"),
        paste0("            if idx > n_", x_name, ":"),
        paste0("                idx = n_", x_name),
        "            if idx not in used:",
        "                used[idx] = True",
        paste0("                out[Int(i - 1)] = ", out_ptr_info$cast, "(", x_name, "[Int(idx - 1)])"),
        "                break"
      )
    }

    if (identical(size_info$kind, "arg")) vector_len_arg <- size_info$name
    if (identical(size_info$kind, "const")) vector_len_const <- as.integer(size_info$value)
    if (identical(size_info$kind, "len_x")) vector_len_arg <- x_name
  }

  params <- character(0)
  for (nm in kernel_args) {
    spec <- types[[which(args == nm)]]
    if (.mojor_is_array(spec)) {
      params <- c(
        params,
        paste0("    ", nm, "_ptr: ImmutOpaqueAny,"),
        paste0("    __mojor_len_", nm, ": Int32,")
      )
    } else {
      params <- c(params, paste0("    ", nm, "_val: Int32,"))
    }
  }
  params <- c(params, "    __mojor_rng_state_ptr: MutOpaqueAny,", "    out_ptr: MutOpaqueAny")

  preamble <- .mojor_expr_only_pointer_preamble(
    "# Generated by mojor_transpile (expression-only mode - compiled subset sampling, IR-native node emit)",
    imports = "from rng_helpers import _rng_next_f64",
    include_mut_opaque = TRUE,
    include_immut_types = c("f64", "i32"),
    include_mut_types = c("f64", "i32"),
    include_mut_u64 = TRUE
  )
  signature <- c(
    paste0('@export("', name, '", ABI="C")'),
    paste0("fn ", name, "("),
    params,
    ") -> Int32:"
  )
  body <- c(
    "    var __mojor_rng_state: MutU64Ptr = __mojor_rng_state_ptr.bitcast[UInt64]()",
    body,
    "    return Int32(out_n)"
  )

  list(
    mojo = paste(c(preamble, signature, body), collapse = "\n"),
    is_expression_kernel = TRUE,
    is_vector_output = TRUE,
    out_type = out_type,
    return_type = "Int32",
    vector_len_arg = vector_len_arg,
    vector_len_const = vector_len_const,
    kernel_args = kernel_args,
    rng_needed = TRUE,
    sampling_op = operation,
    args = args,
    types = types
  )
}

# Helper: Map reduction names to backend functions
.mojor_reduction_backend_map <- function() {
  list(
    sum = list(fn = "mojor_sum_f64", return_type = "Float64"),
    mean = list(fn = "mojor_mean_f64", return_type = "Float64"),
    min = list(fn = "mojor_min_f64", return_type = "Float64"),
    max = list(fn = "mojor_max_f64", return_type = "Float64"),
    prod = list(fn = "mojor_prod_f64", return_type = "Float64"),
    sd = list(fn = "mojor_sd_f64_nomiss", return_type = "Float64"),
    var = list(fn = "mojor_var_f64_nomiss", return_type = "Float64")
  )
}

# Helper: get argument type by name in expression-only paths.
.mojor_expr_only_arg_type <- function(arg_name, args, types) {
  by_name <- types[[arg_name]]
  if (!is.null(by_name)) {
    return(by_name)
  }
  idx <- match(arg_name, args, nomatch = 0L)
  if (idx > 0L) {
    return(types[[idx]])
  }
  NULL
}

# Recursively substitute variable names using a substitution map.
.mojor_substitute_vars <- function(expr, env) {
  if (is.name(expr)) {
    nm <- as.character(expr)
    sub <- env[[nm]]
    if (!is.null(sub)) return(sub)
    return(expr)
  }
  if (is.call(expr)) {
    for (i in seq_along(expr)[-1]) {
      expr[[i]] <- .mojor_substitute_vars(expr[[i]], env)
    }
    return(expr)
  }
  expr
}

# Inline let-bindings in a multi-statement block into a single expression.
# Returns the inlined expression, or NULL if the block cannot be inlined.
.mojor_inline_let_bindings <- function(blocks) {
  if (length(blocks) < 2L) return(NULL)

  env <- list()

  for (i in seq_len(length(blocks) - 1L)) {
    stmt <- blocks[[i]]
    if (!is.call(stmt) || !identical(as.character(stmt[[1]]), "<-")) return(NULL)
    if (!is.name(stmt[[2]])) return(NULL)

    nm <- as.character(stmt[[2]])
    rhs <- .mojor_substitute_vars(stmt[[3]], env)
    env[[nm]] <- rhs
  }

  final <- blocks[[length(blocks)]]
  if (is.call(final) && identical(as.character(final[[1]]), "return") && length(final) >= 2L) {
    final <- final[[2]]
  }

  .mojor_substitute_vars(final, env)
}

# Detect simple top-level implicit vector expressions (for example x + y * 2).
.mojor_detect_vectorized_expression_operation <- function(blocks, args, types) {
  if (length(blocks) == 1L) {
    expr <- blocks[[1]]
  } else {
    expr <- .mojor_inline_let_bindings(blocks)
    if (is.null(expr)) return(list(is_vectorized_expression = FALSE))
  }
  if (is.call(expr) && as.character(expr[[1]]) == "return" && length(expr) >= 2L) {
    expr <- expr[[2]]
  }
  if (is.null(expr)) {
    return(list(is_vectorized_expression = FALSE))
  }

  vector_args <- character(0)
  scalar_args <- character(0)
  allowed_unary <- c("+", "-", "!")
  allowed_binary <- c("+", "-", "*", "/", "^", "%%", "%/%",
                      "==", "!=", "<", ">", "<=", ">=", "&", "|")
  allowed_unary_calls <- c(
    .mojor_expr_registry_vectorized_math_unary(),
    .mojor_expr_registry_vectorized_type_checks(),
    .mojor_expr_registry_vectorized_type_casts()
  )
  allowed_binary_calls <- .mojor_expr_registry_vectorized_binary_calls()

  walk <- function(node) {
    if (is.numeric(node) || is.integer(node) || is.logical(node)) {
      return(TRUE)
    }
    if (is.name(node)) {
      nm <- as.character(node)
      if (!(nm %in% args)) {
        return(FALSE)
      }
      spec <- .mojor_expr_only_arg_type(nm, args, types)
      if (is.null(spec)) {
        return(FALSE)
      }
      if (spec %in% c("f64[]", "i32[]", "lgl[]")) {
        vector_args <<- unique(c(vector_args, nm))
        return(TRUE)
      }
      if (spec %in% c("f64", "f32", "i32", "lgl", "bool")) {
        scalar_args <<- unique(c(scalar_args, nm))
        return(TRUE)
      }
      return(FALSE)
    }
    if (!is.call(node)) {
      return(FALSE)
    }

    op <- as.character(node[[1]])
    if (op == "(" && length(node) == 2L) {
      return(walk(node[[2]]))
    }
    if (op %in% allowed_unary && length(node) == 2L) {
      return(walk(node[[2]]))
    }
    if (op %in% allowed_binary && length(node) == 3L) {
      return(walk(node[[2]]) && walk(node[[3]]))
    }
    if (op %in% c("min", "max", "pmin", "pmax") && length(node) >= 3L) {
      vals <- as.list(node)[-1]
      return(all(vapply(vals, walk, logical(1))))
    }
    if (op %in% c("ifelse", "if") && length(node) == 4L) {
      return(walk(node[[2]]) && walk(node[[3]]) && walk(node[[4]]))
    }
    if (op %in% allowed_unary_calls && length(node) == 2L) {
      return(walk(node[[2]]))
    }
    if (op %in% allowed_binary_calls && length(node) == 3L) {
      return(walk(node[[2]]) && walk(node[[3]]))
    }
    FALSE
  }

  if (!isTRUE(walk(expr))) {
    return(list(is_vectorized_expression = FALSE))
  }
  if (length(vector_args) < 1L) {
    return(list(is_vectorized_expression = FALSE))
  }
  vector_types <- vapply(
    vector_args,
    function(nm) .mojor_expr_only_arg_type(nm, args, types),
    character(1)
  )
  if (!all(vector_types %in% c("f64[]", "i32[]", "lgl[]"))) {
    return(list(is_vectorized_expression = FALSE))
  }
  list(
    is_vectorized_expression = TRUE,
    expr = expr,
    arg_names = vector_args,
    scalar_args = scalar_args,
    out_type = NULL
  )
}

# Lower implicit vectorized top-level expressions via HOF expression-only path.
.mojor_transpile_vectorized_expression_operation <- function(info, args, types, name = "mojor_kernel") {
  op_name <- if (length(info$arg_names) == 1L) "sapply" else "mapply"
  formals <- as.pairlist(stats::setNames(vector("list", length(info$arg_names)), info$arg_names))
  fun_expr <- as.call(list(as.name("function"), formals, info$expr))
  fun_info <- .mojor_extract_hof_inline_fun(
    fun_expr,
    op_name,
    expected_arity = length(info$arg_names),
    known_scalar_captures = info$scalar_args
  )
  hof_info <- list(
    is_hof = TRUE,
    operation = op_name,
    arg_exprs = lapply(info$arg_names, as.name),
    fun_expr = fun_expr,
    fun_info = fun_info,
    out_type_hint = info$out_type,
    scalar_args = info$scalar_args
  )
  .mojor_transpile_hof_operation(hof_info, args, types, name)
}

# Main expression-only transpiler
.mojor_transpile_expression_only <- function(blocks, args, types, name = "mojor_kernel") {
 # 1. Detect expression type
  reduction_info <- .mojor_detect_standalone_reduction(blocks)

  if (reduction_info$is_reduction) {
 # Standalone reduction path (Step 1a)
    return(.mojor_transpile_standalone_reduction(reduction_info, args, types, name))
  }

 # 2. Check for matrix multiplication operations (PR-B6)
  matmul_info <- .mojor_detect_matrix_operation(blocks)

  if (matmul_info$is_matmul) {
 # Matrix multiplication path (PR-B6)
    return(.mojor_transpile_matrix_operation(matmul_info, args, types, name))
  }

 # 3. Check for covariance/correlation operations (PR-B7.2)
  cov_cor_info <- .mojor_detect_cov_cor_operation(blocks)

  if (cov_cor_info$is_cov_cor) {
 # Covariance/correlation path (PR-B7.2)
    return(.mojor_transpile_cov_cor_operation(cov_cor_info, args, types, name))
  }

 # 4. Check for set/match operations ()
  set_match_info <- .mojor_detect_set_match_operation(blocks)

  if (set_match_info$is_set_match) {
    return(.mojor_transpile_set_match_operation(set_match_info, args, types, name))
  }

 # 5. Check for quantile/robust-stat operations ()
  quantile_info <- .mojor_detect_quantile_operation(blocks)

  if (quantile_info$is_quantile) {
    return(.mojor_transpile_quantile_operation(quantile_info, args, types, name))
  }

 # 6. Check for higher-order functions ()
  hof_info <- .mojor_detect_hof_operation(blocks)

  if (hof_info$is_hof) {
    return(.mojor_transpile_hof_operation(hof_info, args, types, name))
  }

 # 7. Check for sampling operations ()
  sampling_info <- .mojor_detect_sampling_operation(blocks)

  if (sampling_info$is_sampling) {
    return(.mojor_transpile_sampling_operation(sampling_info, args, types, name))
  }

 # 8. Check for string operations ()
  string_info <- .mojor_detect_string_operation(blocks)

  if (string_info$is_string) {
    return(.mojor_transpile_string_operation(string_info, args, types, name))
  }

 # 9. Check for dim() metadata query operation
  dim_info <- .mojor_detect_dim_operation(blocks, args, types)

  if (dim_info$is_dim) {
    return(.mojor_transpile_dim_operation(dim_info, args, types, name))
  }

 # 10. Check for regex/table wrappers (9.2/9.3)
  tier9_regex_table_info <- .mojor_detect_tier9_regex_table_operation(blocks)

  if (tier9_regex_table_info$is_tier9_regex_table) {
    return(.mojor_transpile_tier9_regex_table_operation(tier9_regex_table_info, args, types, name))
  }

 # 11. Try implicit vectorized expression path (subset)
  vexpr_info <- .mojor_detect_vectorized_expression_operation(blocks, args, types)
  if (isTRUE(vexpr_info$is_vectorized_expression)) {
    return(.mojor_transpile_vectorized_expression_operation(vexpr_info, args, types, name))
  }

 # 12. Try scalar expression path (Step 1b)
  if (length(blocks) == 1) {
    expr <- blocks[[1]]

 # Handle return(expr)
    if (is.call(expr) && as.character(expr[[1]]) == "return" && length(expr) >= 2) {
      expr <- expr[[2]]
    }

 # Check if it's a scalar expression (binary op, etc.)
    if (.mojor_is_scalar_expression(expr, args, types)) {
      return(.mojor_transpile_scalar_expression(expr, args, types, name))
    }
  }

 # 13. Not supported yet
  stop(
    "mojor_transpile: strict no-loop path currently only supports standalone reductions (",
    .mojor_expr_registry_standalone_reductions_label(),
    "), matrix operations (%*%, crossprod, tcrossprod), covariance/correlation (cov, cor), set/match primitives (unique, duplicated, anyDuplicated, match, %in%), quantile/robust stats (median, quantile, IQR, mad), higher-order functions (vapply, sapply, lapply, mapply) under strict inline subset, implicit top-level vectorized arithmetic/comparison/logical expressions over direct f64[]/i32[]/lgl[] args (including mixed-type and multi-arg forms via mapply lowering), sampling (sample.int, sample) under strict scalar-arg subset, native string subset (nchar, nzchar, substr, substring, paste, paste0) under strict chr[] direct-arg constraints, dim() metadata queries for direct matrix/ND array args (dim(x), dim(x)[k], length(dim(x))), regex-table subset (grepl, grep, sub, gsub, row, col, expand.grid) under strict argument constraints, and scalar expressions."
  )
}

# Helper: Check if expression is a composable expression (scalar or reduction)
.mojor_is_scalar_expression <- function(expr, args, types) {
 # Composable expression: can be scalar ops, reductions, or combinations
  if (is.name(expr)) {
 # Simple variable reference - allow both scalar and array (for reductions)
    arg_name <- as.character(expr)
    return(arg_name %in% args)
  }

  if (is.numeric(expr) || is.logical(expr)) {
 # Literal constant
    return(TRUE)
  }

  if (is.call(expr)) {
    op <- as.character(expr[[1]])

 # Parentheses - unwrap and check inner expression
    if (op == "(") {
      return(.mojor_is_scalar_expression(expr[[2]], args, types))
    }

 # Reduction functions (Step 1b Step 2)
    if (op %in% c("sum", "mean", "min", "max", "prod", "sd", "var")) {
      return(TRUE)
    }

 # Binary operators
    if (op %in% c("+", "-", "*", "/", "^", "%%", "%/%")) {
 # Check if all operands are composable
      for (i in 2:length(expr)) {
        if (!.mojor_is_scalar_expression(expr[[i]], args, types)) {
          return(FALSE)
        }
      }
      return(TRUE)
    }
  }

  FALSE
}

# Transpile standalone reduction (Step 1a - extracted from original function)
.mojor_transpile_standalone_reduction <- function(reduction_info, args, types, name) {
 # 2. Validate argument
  arg_expr <- reduction_info$arg
  if (!is.name(arg_expr)) {
    stop("mojor_transpile: reduction argument must be a simple variable name, got: ", deparse(arg_expr))
  }

  arg_name <- as.character(arg_expr)
  if (!(arg_name %in% args)) {
    stop("mojor_transpile: argument '", arg_name, "' not found in function arguments")
  }

 # 3. Get type info
  arg_type <- types[[arg_name]]
  if (is.null(arg_type)) {
    stop("mojor_transpile: no type annotation for argument '", arg_name, "'")
  }

 # 4. Validate type (must be array)
  if (!.mojor_is_array(arg_type)) {
    stop("mojor_transpile: reduction argument '", arg_name, "' must be an array type, got: ", arg_type)
  }

 # 5. Get backend function mapping
  backend_map <- .mojor_reduction_backend_map()
  backend_info <- backend_map[[reduction_info$fn_name]]

  if (is.null(backend_info)) {
    stop("mojor_transpile: unsupported reduction: ", reduction_info$fn_name)
  }

 # 6. Generate Mojo code
  mojo_code <- .mojor_generate_expression_kernel(
    name = name,
    fn_name = reduction_info$fn_name,
    backend_fn = backend_info$fn,
    return_type = backend_info$return_type,
    arg_name = arg_name,
    arg_type = arg_type
  )

 # 7. Return result (similar structure to loop transpiler)
  list(
    mojo = mojo_code,
    is_expression_kernel = TRUE,
    return_type = backend_info$return_type,
    args = args,
    types = types
  )
}

# Helper: Generate Mojo code for expression kernel
.mojor_generate_expression_kernel <- function(name, fn_name, backend_fn, return_type, arg_name, arg_type) {
 # Preamble
  preamble <- .mojor_expr_only_pointer_preamble(
    "# Generated by mojor_transpile (expression-only mode - reduction path)",
    include_immut_types = c("f64"),
    include_mut_types = character(0)
  )

 # Function signature
  len_param <- paste0("__mojor_len_", arg_name)
  signature <- paste0(
    "@export(\"", name, "\", ABI=\"C\")\n",
    "fn ", name, "(\n",
    "    ", arg_name, "_ptr: ImmutOpaqueAny,\n",
    "    ", len_param, ": Int32\n",
    ") -> ", return_type, ":"
  )

  body <- .mojor_expr_only_bind_array_ptr_len(
    arg_name,
    "ImmutFloat64Ptr",
    "Float64",
    len_var = "n",
    len_param = len_param
  )

  if (fn_name == "sum") {
    body <- c(
      body,
      "    var acc: Float64 = 0.0",
      "    for i in range(n):",
      paste0("        acc += ", arg_name, "[i]"),
      "    return acc"
    )
  } else if (fn_name == "mean") {
    body <- c(
      body,
      "    var acc: Float64 = 0.0",
      "    for i in range(n):",
      paste0("        acc += ", arg_name, "[i]"),
      "    return acc / Float64(n)"
    )
  } else if (fn_name == "min") {
    body <- c(
      body,
      paste0("    var acc: Float64 = ", arg_name, "[0]"),
      "    for i in range(1, n):",
      paste0("        if ", arg_name, "[i] < acc:"),
      paste0("            acc = ", arg_name, "[i]"),
      "    return acc"
    )
  } else if (fn_name == "max") {
    body <- c(
      body,
      paste0("    var acc: Float64 = ", arg_name, "[0]"),
      "    for i in range(1, n):",
      paste0("        if ", arg_name, "[i] > acc:"),
      paste0("            acc = ", arg_name, "[i]"),
      "    return acc"
    )
  } else if (fn_name == "prod") {
    body <- c(
      body,
      "    var acc: Float64 = 1.0",
      "    for i in range(n):",
      paste0("        acc *= ", arg_name, "[i]"),
      "    return acc"
    )
  } else if (fn_name == "sd" || fn_name == "var") {
    body <- c(
      body,
      "    # First pass: compute mean",
      "    var mean: Float64 = 0.0",
      "    for i in range(n):",
      paste0("        mean += ", arg_name, "[i]"),
      "    mean = mean / Float64(n)",
      "    # Second pass: compute variance",
      "    var var_acc: Float64 = 0.0",
      "    for i in range(n):",
      paste0("        var diff = ", arg_name, "[i] - mean"),
      "        var_acc += diff * diff",
      "    var variance = var_acc / Float64(n - 1)"
    )
    if (fn_name == "sd") {
      body <- c(body, "    return sqrt(variance)")
    } else {
      body <- c(body, "    return variance")
    }
  }

  paste(c(preamble, signature, body), collapse = "\n")
}

# =============================================================================
# PR-B6: Matrix Multiplication Operations
# =============================================================================

# Transpile matrix multiplication operations
.mojor_transpile_matrix_operation <- function(matmul_info, args, types, name = "mojor_kernel") {
 # 1. Extract operation details
  operation <- matmul_info$operation
  lhs_expr <- matmul_info$lhs
  rhs_expr <- matmul_info$rhs

 # 2. Validate LHS argument
  if (!is.name(lhs_expr)) {
    stop("mojor_transpile: ", operation, " left-hand argument must be a simple variable name, got: ", deparse(lhs_expr))
  }
  lhs_name <- as.character(lhs_expr)

  if (!(lhs_name %in% args)) {
    stop("mojor_transpile: ", operation, " left-hand argument '", lhs_name, "' is not a function parameter")
  }

  lhs_type <- types[[which(args == lhs_name)]]
  if (!.mojor_is_matrix(lhs_type)) {
    stop("mojor_transpile: ", operation, " left-hand argument '", lhs_name, "' must be a matrix type (f64[,]), got: ", lhs_type)
  }

 # 3. Validate RHS argument (if provided)
  if (!is.null(rhs_expr)) {
    if (!is.name(rhs_expr)) {
      stop("mojor_transpile: ", operation, " right-hand argument must be a simple variable name, got: ", deparse(rhs_expr))
    }
    rhs_name <- as.character(rhs_expr)

    if (!(rhs_name %in% args)) {
      stop("mojor_transpile: ", operation, " right-hand argument '", rhs_name, "' is not a function parameter")
    }

    rhs_type <- types[[which(args == rhs_name)]]
    if (!.mojor_is_matrix(rhs_type)) {
      stop("mojor_transpile: ", operation, " right-hand argument '", rhs_name, "' must be a matrix type (f64[,]), got: ", rhs_type)
    }
  } else {
    rhs_name <- NULL
  }

 # 4. Generate Mojo code using inline implementation
  mojo_code <- .mojor_generate_matrix_kernel(
    name = name,
    operation = operation,
    lhs_name = lhs_name,
    rhs_name = rhs_name,
    args = args,
    types = types
  )

 # 5. Return result
  list(
    mojo = mojo_code,
    is_expression_kernel = TRUE,
    is_matrix_output = TRUE,
    return_type = "Matrix",
    operation = operation,
    args = args,
    types = types
  )
}

# Generate Mojo code for matrix operation kernel
.mojor_generate_matrix_kernel <- function(name, operation, lhs_name, rhs_name, args, types) {
  preamble <- .mojor_expr_only_pointer_preamble(
    "# Generated by mojor_transpile (matrix operation - PR-B6)",
    include_mut_opaque = TRUE,
    include_immut_types = c("f64", "i32"),
    include_mut_types = c("f64")
  )

  params <- character(0)
  body_setup <- character(0)

  for (i in seq_along(args)) {
    arg_name <- args[i]
    arg_type <- types[i]

    if (.mojor_is_matrix(arg_type)) {
      params <- c(
        params,
        paste0("    ", arg_name, "_ptr: ImmutOpaqueAny,"),
        paste0("    __mojor_len_", arg_name, ": Int32,"),
        paste0("    __mojor_dim_", arg_name, ": ImmutOpaqueAny,")
      )
      body_setup <- c(
        body_setup,
        paste0("    var ", arg_name, ": ImmutFloat64Ptr = ", arg_name, "_ptr.bitcast[Float64]()"),
        paste0("    var ", arg_name, "_dim: ImmutInt32Ptr = __mojor_dim_", arg_name, ".bitcast[Int32]()"),
        paste0("    var ", arg_name, "_nrow = Int(", arg_name, "_dim[0])"),
        paste0("    var ", arg_name, "_ncol = Int(", arg_name, "_dim[1])")
      )
    }
  }

  params <- c(
    params,
    "    out_ptr: MutOpaqueAny,",
    "    out_nrow: Int32,",
    "    out_ncol: Int32"
  )

  params[length(params)] <- sub(",$", "", params[length(params)])

  signature <- c(
    paste0("@export(\"", name, "\", ABI=\"C\")"),
    paste0("fn ", name, "("),
    params,
    ") -> None:"
  )

  body_setup <- c(
    body_setup,
    "    var out: MutFloat64Ptr = out_ptr.bitcast[Float64]()",
    "    var m = Int(out_nrow)",
    "    var p = Int(out_ncol)"
  )

  if (operation == "matmul") {
    body_compute <- c(
      paste0("    var n = ", lhs_name, "_ncol"),
      paste0("    var m_dim = ", lhs_name, "_nrow"),
      paste0("    var n_dim = ", rhs_name, "_nrow"),
      "    for i in range(m):",
      "        for j in range(p):",
      "            var sum: Float64 = 0.0",
      "            for k in range(n):",
      paste0("                var a_val = ", lhs_name, "[i + k * m_dim]  # A[i,k]"),
      paste0("                var b_val = ", rhs_name, "[k + j * n_dim]  # B[k,j]"),
      "                sum += a_val * b_val",
      "            out[i + j * m] = sum  # C[i,j]"
    )
  } else if (operation == "crossprod") {
    if (is.null(rhs_name)) {
      body_compute <- c(
        paste0("    var m_dim = ", lhs_name, "_nrow"),
        "    for i in range(p):", # p = lhs_ncol
        "        for j in range(p):",
        "            var sum: Float64 = 0.0",
        "            for k in range(m_dim):",
        paste0("                var a_i = ", lhs_name, "[k + i * m_dim]"),
        paste0("                var a_j = ", lhs_name, "[k + j * m_dim]"),
        "                sum += a_i * a_j",
        "            out[i + j * p] = sum  # C[i,j]"
      )
    } else {
      body_compute <- c(
        paste0("    var m_dim = ", lhs_name, "_nrow"),
        paste0("    var n_dim = ", lhs_name, "_ncol"), # output rows
        paste0("    var q_dim = ", rhs_name, "_ncol"), # output cols
        "    for i in range(m):  # i in [0, n)",
        "        for j in range(p):  # j in [0, q)",
        "            var sum: Float64 = 0.0",
        "            for k in range(m_dim):  # k in [0, m)",
        paste0("                var a_val = ", lhs_name, "[k + i * m_dim]"),
        paste0("                var b_val = ", rhs_name, "[k + j * m_dim]"),
        "                sum += a_val * b_val",
        "            out[i + j * m] = sum  # C[i,j]"
      )
    }
  } else if (operation == "tcrossprod") {
    if (is.null(rhs_name)) {
      body_compute <- c(
        paste0("    var n = ", lhs_name, "_ncol"),
        paste0("    var m_dim = ", lhs_name, "_nrow"),
        "    for i in range(m):",
        "        for j in range(m):",
        "            var sum: Float64 = 0.0",
        "            for k in range(n):",
        paste0("                var a_i = ", lhs_name, "[i + k * m_dim]"),
        paste0("                var a_j = ", lhs_name, "[j + k * m_dim]"),
        "                sum += a_i * a_j",
        "            out[i + j * m] = sum  # C[i,j]"
      )
    } else {
      body_compute <- c(
        paste0("    var n = ", lhs_name, "_ncol"),
        paste0("    var m_dim = ", lhs_name, "_nrow"),
        paste0("    var p_dim = ", rhs_name, "_nrow"),
        "    for i in range(m):",
        "        for j in range(p):",
        "            var sum: Float64 = 0.0",
        "            for k in range(n):",
        paste0("                var a_val = ", lhs_name, "[i + k * m_dim]"),
        paste0("                var b_val = ", rhs_name, "[j + k * p_dim]"),
        "                sum += a_val * b_val",
        "            out[i + j * m] = sum  # C[i,j]"
      )
    }
  } else {
    stop("Unsupported matrix operation: ", operation)
  }

  paste(c(preamble, signature, body_setup, body_compute), collapse = "\n")
}

# Transpile covariance/correlation operations (PR-B7.2)
.mojor_transpile_cov_cor_operation <- function(cov_cor_info, args, types, name = "mojor_kernel") {
 # 1. Extract operation details
  operation <- cov_cor_info$operation
  x_expr <- cov_cor_info$x
  y_expr <- cov_cor_info$y

 # 2. Validate x argument
  if (!is.name(x_expr)) {
    stop("mojor_transpile: ", operation, "() first argument must be a simple variable name, got: ", deparse(x_expr))
  }
  x_name <- as.character(x_expr)

  if (!(x_name %in% args)) {
    stop("mojor_transpile: ", operation, "() first argument '", x_name, "' is not a function parameter")
  }

  x_type <- types[[which(args == x_name)]]

 # 3. Validate y argument (when present)
  y_name <- NULL
  y_type <- NULL
  if (!is.null(y_expr)) {
    if (!is.name(y_expr)) {
      stop("mojor_transpile: ", operation, "() second argument must be a simple variable name, got: ", deparse(y_expr))
    }
    y_name <- as.character(y_expr)

    if (!(y_name %in% args)) {
      stop("mojor_transpile: ", operation, "() second argument '", y_name, "' is not a function parameter")
    }

    y_type <- types[[which(args == y_name)]]
  }

 # 4. Determine input/output mode
  x_is_matrix <- .mojor_is_matrix(x_type)
  y_is_matrix <- if (is.null(y_type)) FALSE else .mojor_is_matrix(y_type)
  matrix_mode <- FALSE

  if (is.null(y_expr)) {
 # Single argument: vectors are not accepted in this lane.
    if (!x_is_matrix) {
      stop("mojor_transpile: ", operation, "() with single argument requires a matrix, got vector. Use var(x) for variance or provide two vector arguments.")
    }
    matrix_mode <- TRUE
  } else if (x_is_matrix || y_is_matrix) {
    if (!(x_is_matrix && y_is_matrix)) {
      stop("mojor_transpile: ", operation, "() arguments must have matching rank (both vectors or both matrices)")
    }
    matrix_mode <- TRUE
  }

 # 5. Generate Mojo code
  mojo_code <- .mojor_generate_cov_cor_kernel(
    name = name,
    operation = operation,
    x_name = x_name,
    y_name = y_name,
    matrix_mode = matrix_mode
  )

  kernel_args <- if (is.null(y_name)) x_name else c(x_name, y_name)

 # 6. Return result
  if (isTRUE(matrix_mode)) {
    return(list(
      mojo = mojo_code,
      is_expression_kernel = TRUE,
      is_matrix_output = TRUE,
      out_type = "f64[,]",
      return_type = "Matrix",
      operation = operation,
      kernel_args = kernel_args,
      args = args,
      types = types
    ))
  }

  list(
    mojo = mojo_code,
    is_expression_kernel = TRUE,
    is_matrix_output = FALSE, # Scalar output for vectors
    return_type = "Float64",
    operation = operation,
    kernel_args = kernel_args,
    args = args,
    types = types
  )
}

# Generate Mojo code for cov/cor kernels:
# - vector inputs -> scalar output
# - matrix inputs -> matrix output
.mojor_generate_cov_cor_kernel <- function(name, operation, x_name, y_name = NULL, matrix_mode = FALSE) {
  if (!isTRUE(matrix_mode)) {
    preamble <- .mojor_expr_only_pointer_preamble(
      "# Generated by mojor_transpile (covariance/correlation - PR-B7.2)",
      imports = "from math import sqrt",
      include_immut_types = c("f64"),
      include_mut_types = character(0)
    )

    params <- c(
      paste0("    ", x_name, "_ptr: ImmutOpaqueAny,"),
      paste0("    __mojor_len_", x_name, ": Int32,"),
      paste0("    ", y_name, "_ptr: ImmutOpaqueAny,"),
      paste0("    __mojor_len_", y_name, ": Int32")
    )
    signature <- c(
      paste0("@export(\"", name, "\", ABI=\"C\")"),
      paste0("fn ", name, "("),
      params,
      ") -> Float64:"
    )
    body_setup <- c(
      .mojor_expr_only_bind_array_ptr_len(
        x_name,
        "ImmutFloat64Ptr",
        "Float64",
        len_var = paste0("n_", x_name)
      ),
      .mojor_expr_only_bind_array_ptr_len(
        y_name,
        "ImmutFloat64Ptr",
        "Float64",
        len_var = paste0("n_", y_name)
      )
    )

    if (operation == "cov") {
      body_compute <- c(
        paste0("    var n = n_", x_name),
        "    # Compute means",
        "    var sum_x: Float64 = 0.0",
        "    var sum_y: Float64 = 0.0",
        "    for i in range(n):",
        paste0("        sum_x += ", x_name, "[i]"),
        paste0("        sum_y += ", y_name, "[i]"),
        "    var mean_x = sum_x / Float64(n)",
        "    var mean_y = sum_y / Float64(n)",
        "    # Compute covariance",
        "    var sum_prod: Float64 = 0.0",
        "    for i in range(n):",
        paste0("        var dev_x = ", x_name, "[i] - mean_x"),
        paste0("        var dev_y = ", y_name, "[i] - mean_y"),
        "        sum_prod += dev_x * dev_y",
        "    return sum_prod / Float64(n - 1)"
      )
    } else if (operation == "cor") {
      body_compute <- c(
        paste0("    var n = n_", x_name),
        "    # Compute means",
        "    var sum_x: Float64 = 0.0",
        "    var sum_y: Float64 = 0.0",
        "    for i in range(n):",
        paste0("        sum_x += ", x_name, "[i]"),
        paste0("        sum_y += ", y_name, "[i]"),
        "    var mean_x = sum_x / Float64(n)",
        "    var mean_y = sum_y / Float64(n)",
        "    # Compute covariance and variances",
        "    var sum_prod: Float64 = 0.0",
        "    var sum_sq_x: Float64 = 0.0",
        "    var sum_sq_y: Float64 = 0.0",
        "    for i in range(n):",
        paste0("        var dev_x = ", x_name, "[i] - mean_x"),
        paste0("        var dev_y = ", y_name, "[i] - mean_y"),
        "        sum_prod += dev_x * dev_y",
        "        sum_sq_x += dev_x * dev_x",
        "        sum_sq_y += dev_y * dev_y",
        "    var cov_xy = sum_prod / Float64(n - 1)",
        "    var var_x = sum_sq_x / Float64(n - 1)",
        "    var var_y = sum_sq_y / Float64(n - 1)",
        "    var sd_x = sqrt(var_x)",
        "    var sd_y = sqrt(var_y)",
        "    return cov_xy / (sd_x * sd_y)"
      )
    } else {
      stop("Unsupported cov/cor operation: ", operation)
    }

    return(paste(c(preamble, signature, body_setup, body_compute), collapse = "\n"))
  }

 # Matrix cov/cor path.
  preamble <- .mojor_expr_only_pointer_preamble(
    "# Generated by mojor_transpile (covariance/correlation matrix lane - PR-B7.2)",
    imports = "from math import sqrt",
    include_mut_opaque = TRUE,
    include_immut_types = c("f64", "i32"),
    include_mut_types = c("f64")
  )

  params <- c(
    paste0("    ", x_name, "_ptr: ImmutOpaqueAny,"),
    paste0("    __mojor_len_", x_name, ": Int32,"),
    paste0("    __mojor_dim_", x_name, ": ImmutOpaqueAny,")
  )
  if (!is.null(y_name)) {
    params <- c(
      params,
      paste0("    ", y_name, "_ptr: ImmutOpaqueAny,"),
      paste0("    __mojor_len_", y_name, ": Int32,"),
      paste0("    __mojor_dim_", y_name, ": ImmutOpaqueAny,")
    )
  }
  params <- c(
    params,
    "    out_ptr: MutOpaqueAny,",
    "    out_nrow: Int32,",
    "    out_ncol: Int32"
  )

  signature <- c(
    paste0("@export(\"", name, "\", ABI=\"C\")"),
    paste0("fn ", name, "("),
    params,
    ") -> None:"
  )

  y_row_read <- if (is.null(y_name)) {
    paste0(x_name, "[k + j * m]")
  } else {
    paste0(y_name, "[k + j * m]")
  }
  y_dev_read <- if (is.null(y_name)) {
    paste0(x_name, "[k + j * m] - mean_y")
  } else {
    paste0(y_name, "[k + j * m] - mean_y")
  }

  body_setup <- c(
    paste0("    var ", x_name, ": ImmutFloat64Ptr = ", x_name, "_ptr.bitcast[Float64]()"),
    paste0("    var ", x_name, "_dim: ImmutInt32Ptr = __mojor_dim_", x_name, ".bitcast[Int32]()"),
    paste0("    var m = Int(", x_name, "_dim[0])"),
    paste0("    var p = Int(", x_name, "_dim[1])"),
    "    var q = p"
  )
  if (!is.null(y_name)) {
    body_setup <- c(
      body_setup,
      paste0("    var ", y_name, ": ImmutFloat64Ptr = ", y_name, "_ptr.bitcast[Float64]()"),
      paste0("    var ", y_name, "_dim: ImmutInt32Ptr = __mojor_dim_", y_name, ".bitcast[Int32]()"),
      paste0("    if Int(", y_name, "_dim[0]) != m:"),
      "        return",
      paste0("    q = Int(", y_name, "_dim[1])")
    )
  }
  body_setup <- c(
    body_setup,
    "    if Int(out_nrow) != p or Int(out_ncol) != q:",
    "        return",
    "    var out: MutFloat64Ptr = out_ptr.bitcast[Float64]()"
  )

  body_compute <- c(
    "    for i in range(p):",
    "        for j in range(q):",
    "            var sum_x: Float64 = 0.0",
    "            var sum_y: Float64 = 0.0",
    "            for k in range(m):",
    paste0("                sum_x += ", x_name, "[k + i * m]"),
    paste0("                sum_y += ", y_row_read),
    "            var mean_x = sum_x / Float64(m)",
    "            var mean_y = sum_y / Float64(m)",
    "            var sum_prod: Float64 = 0.0"
  )

  if (operation == "cor") {
    body_compute <- c(
      body_compute,
      "            var sum_sq_x: Float64 = 0.0",
      "            var sum_sq_y: Float64 = 0.0"
    )
  }

  body_compute <- c(
    body_compute,
    "            for k in range(m):",
    paste0("                var dev_x = ", x_name, "[k + i * m] - mean_x"),
    paste0("                var dev_y = ", y_dev_read),
    "                sum_prod += dev_x * dev_y"
  )

  if (operation == "cor") {
    body_compute <- c(
      body_compute,
      "                sum_sq_x += dev_x * dev_x",
      "                sum_sq_y += dev_y * dev_y",
      "            var var_x = sum_sq_x / Float64(m - 1)",
      "            var var_y = sum_sq_y / Float64(m - 1)",
      "            var denom = sqrt(var_x) * sqrt(var_y)",
      "            out[i + j * p] = (sum_prod / Float64(m - 1)) / denom"
    )
  } else if (operation == "cov") {
    body_compute <- c(
      body_compute,
      "            out[i + j * p] = sum_prod / Float64(m - 1)"
    )
  } else {
    stop("Unsupported cov/cor operation: ", operation)
  }

  paste(c(preamble, signature, body_setup, body_compute), collapse = "\n")
}

# Helper: Check if type is a matrix
.mojor_is_matrix <- function(type) {
  grepl("\\[,\\]$", type) || grepl("\\[2d\\]$", type) || grepl("^mat\\[", type)
}

cat("Expression-only transpiler functions loaded.\n")
