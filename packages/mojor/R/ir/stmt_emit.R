.mojor_ir_sample_is_prob_null <- function(prob_node) {
  if (is.null(prob_node)) {
    return(TRUE)
  }
  if (is.list(prob_node) && identical(prob_node$kind, "var") && identical(prob_node$name, "NULL")) {
    return(TRUE)
  }
  if (is.list(prob_node) && identical(prob_node$kind, "const")) {
    val <- prob_node$value
    if (is.character(val) && length(val) == 1 && identical(toupper(val), "NULL")) {
      return(TRUE)
    }
  }
  FALSE
}

.mojor_ir_sample_parse_prob_var <- function(prob_node, op_label, src, type_env = NULL) {
  if (is.null(prob_node)) {
    return(NULL)
  }
  if (!(is.list(prob_node) && identical(prob_node$kind, "var") && nzchar(prob_node$name))) {
    .mojor_err(
      paste0(op_label, "() weighted sampling currently requires prob to be a direct vector variable in this release"),
      src,
      "Use a direct f64[] variable for prob or set prob = NULL"
    )
  }
  prob_name <- prob_node$name
  if (!is.null(type_env)) {
    prob_type <- type_env[[prob_name]]
    if (!is.null(prob_type) && !(prob_type %in% c("f64[]", "f64", "unknown"))) {
      .mojor_err(
        paste0(op_label, "() weighted sampling requires prob to have f64[] type in this release"),
        src,
        paste0("Got prob type: ", prob_type)
      )
    }
  }
  len_var_map <- .mojor_state$current_len_var_map
  const_array_vars <- .mojor_state$current_const_array_vars
  prob_len <- .mojor_ir_emit_len_lookup(
    prob_name,
    len_var_map = len_var_map,
    const_array_vars = const_array_vars,
    default = paste0("len_", prob_name)
  )
  list(name = prob_name, len = prob_len)
}

.mojor_ir_sample_parse_replace_mode <- function(
  replace_node,
  op_label,
  src,
  zero_based_vars = NULL,
  type_env = NULL,
  loop_var = NULL
) {
  parse_bool_literal <- function(v) {
    if (is.logical(v) && length(v) == 1 && !is.na(v)) {
      return(isTRUE(v))
    }
    if (is.character(v) && length(v) == 1) {
      up <- toupper(v)
      if (up %in% c("TRUE", "FALSE")) {
        return(identical(up, "TRUE"))
      }
    }
    if (is.numeric(v) && length(v) == 1 && !is.na(v) && v %in% c(0, 1)) {
      return(isTRUE(as.logical(v)))
    }
    NULL
  }
  fail <- function() {
    .mojor_err(
      paste0(op_label, "() replace must be a scalar boolean/int flag (literal or typed scalar expression) in this release"),
      src,
      "Use replace = TRUE/FALSE or a scalar lgl/bool/i32 variable/expression"
    )
  }
  if (is.null(replace_node)) {
    return(list(is_const = TRUE, value = FALSE, expr = NULL))
  }
  if (is.list(replace_node) && !is.null(replace_node$kind)) {
    if (identical(replace_node$kind, "const")) {
      lit <- parse_bool_literal(replace_node$value)
      if (is.null(lit)) fail()
      return(list(is_const = TRUE, value = lit, expr = NULL))
    }
    replace_expr <- .mojor_ir_expr_emit(
      replace_node,
      zero_based_vars,
      type_env,
      loop_var,
      index_context = TRUE
    )
    if (is.null(replace_expr) || !nzchar(replace_expr)) {
      fail()
    }
    inferred <- tryCatch(
      if (!is.null(type_env)) .mojor_ir_infer_type(replace_node, type_env) else "unknown",
      error = function(e) "unknown"
    )
    if (is.character(inferred) && length(inferred) == 1 &&
      inferred %in% c("i32", "f64", "f32", "Int", "Int32", "Int64", "Float64", "Float32")) {
      replace_expr <- paste0("((", replace_expr, ") != 0)")
    }
    return(list(is_const = FALSE, value = NA, expr = replace_expr))
  }
  lit <- parse_bool_literal(replace_node)
  if (is.null(lit)) fail()
  list(is_const = TRUE, value = lit, expr = NULL)
}

.mojor_ir_sample_emit_uniform_pick_unused <- function(base_indent, limit_expr, used_var) {
  c(
    paste0(base_indent, "while True:"),
    paste0(base_indent, "  idx = Int((_rng_next_f64(__mojor_rng_state) * ", limit_expr, ") + 1)"),
    paste0(base_indent, "  if idx > Int(", limit_expr, "): idx = Int(", limit_expr, ")"),
    paste0(base_indent, "  if not ", used_var, "[Int(idx - 1)]:"),
    paste0(base_indent, "    break")
  )
}

.mojor_ir_sample_emit_weighted_replace_true <- function(
  base_indent,
  size_str,
  limit_expr,
  prob_info,
  prob_total,
  prob_target,
  prob_acc,
  prob_j,
  prob_w,
  out_line
) {
  c(
    paste0(base_indent, "for i in range(1, ", size_str, " + 1):"),
    paste0(base_indent, "  var idx: Int = 1"),
    paste0(base_indent, "  if Int(", prob_info$len, ") == Int(", limit_expr, "):"),
    paste0(base_indent, "    var ", prob_total, ": Float64 = 0.0"),
    paste0(base_indent, "    for ", prob_j, " in range(Int(", limit_expr, ")):"),
    paste0(base_indent, "      var ", prob_w, " = _mojor_read_f64(", prob_info$name, ", ", prob_j, ", Int(", prob_info$len, "))"),
    paste0(base_indent, "      if ", prob_w, " > 0.0:"),
    paste0(base_indent, "        ", prob_total, " += ", prob_w),
    paste0(base_indent, "    if ", prob_total, " > 0.0:"),
    paste0(base_indent, "      var ", prob_target, " = _rng_next_f64(__mojor_rng_state) * ", prob_total),
    paste0(base_indent, "      var ", prob_acc, ": Float64 = 0.0"),
    paste0(base_indent, "      idx = Int(", limit_expr, ")"),
    paste0(base_indent, "      for ", prob_j, " in range(Int(", limit_expr, ")):"),
    paste0(base_indent, "        var ", prob_w, " = _mojor_read_f64(", prob_info$name, ", ", prob_j, ", Int(", prob_info$len, "))"),
    paste0(base_indent, "        if ", prob_w, " > 0.0:"),
    paste0(base_indent, "          ", prob_acc, " += ", prob_w),
    paste0(base_indent, "          if ", prob_target, " <= ", prob_acc, ":"),
    paste0(base_indent, "            idx = Int(", prob_j, " + 1)"),
    paste0(base_indent, "            break"),
    paste0(base_indent, "    else:"),
    paste0(base_indent, "      idx = Int((_rng_next_f64(__mojor_rng_state) * ", limit_expr, ") + 1)"),
    paste0(base_indent, "      if idx > Int(", limit_expr, "): idx = Int(", limit_expr, ")"),
    paste0(base_indent, "  else:"),
    paste0(base_indent, "    idx = Int((_rng_next_f64(__mojor_rng_state) * ", limit_expr, ") + 1)"),
    paste0(base_indent, "    if idx > Int(", limit_expr, "): idx = Int(", limit_expr, ")"),
    paste0(base_indent, "  ", out_line)
  )
}

.mojor_ir_sample_emit_weighted_replace_false <- function(
  base_indent,
  size_str,
  limit_expr,
  prob_info,
  prob_total,
  prob_target,
  prob_acc,
  prob_j,
  prob_w,
  used_var,
  out_line
) {
  c(
    paste0(base_indent, "var ", used_var, " = alloc_bool(", limit_expr, ")"),
    paste0(base_indent, "for i in range(1, ", size_str, " + 1):"),
    paste0(base_indent, "  var idx: Int = 1"),
    paste0(base_indent, "  if Int(", prob_info$len, ") == Int(", limit_expr, "):"),
    paste0(base_indent, "    var ", prob_total, ": Float64 = 0.0"),
    paste0(base_indent, "    for ", prob_j, " in range(Int(", limit_expr, ")):"),
    paste0(base_indent, "      if ", used_var, "[Int(", prob_j, ")]:"),
    paste0(base_indent, "        continue"),
    paste0(base_indent, "      var ", prob_w, " = _mojor_read_f64(", prob_info$name, ", ", prob_j, ", Int(", prob_info$len, "))"),
    paste0(base_indent, "      if ", prob_w, " > 0.0:"),
    paste0(base_indent, "        ", prob_total, " += ", prob_w),
    paste0(base_indent, "    if ", prob_total, " > 0.0:"),
    paste0(base_indent, "      var ", prob_target, " = _rng_next_f64(__mojor_rng_state) * ", prob_total),
    paste0(base_indent, "      var ", prob_acc, ": Float64 = 0.0"),
    paste0(base_indent, "      idx = Int(", limit_expr, ")"),
    paste0(base_indent, "      for ", prob_j, " in range(Int(", limit_expr, ")):"),
    paste0(base_indent, "        if ", used_var, "[Int(", prob_j, ")]:"),
    paste0(base_indent, "          continue"),
    paste0(base_indent, "        var ", prob_w, " = _mojor_read_f64(", prob_info$name, ", ", prob_j, ", Int(", prob_info$len, "))"),
    paste0(base_indent, "        if ", prob_w, " > 0.0:"),
    paste0(base_indent, "          ", prob_acc, " += ", prob_w),
    paste0(base_indent, "          if ", prob_target, " <= ", prob_acc, ":"),
    paste0(base_indent, "            idx = Int(", prob_j, " + 1)"),
    paste0(base_indent, "            break"),
    paste0(base_indent, "    else:"),
    .mojor_ir_sample_emit_uniform_pick_unused(paste0(base_indent, "      "), limit_expr, used_var),
    paste0(base_indent, "  else:"),
    .mojor_ir_sample_emit_uniform_pick_unused(paste0(base_indent, "    "), limit_expr, used_var),
    paste0(base_indent, "  ", used_var, "[Int(idx - 1)] = True"),
    paste0(base_indent, "  ", out_line)
  )
}

.mojor_ir_sample_emit_replace_true <- function(base_indent, size_str, limit_expr, out_line) {
  c(
    paste0(base_indent, "for i in range(1, ", size_str, " + 1):"),
    paste0(base_indent, "  idx = Int((_rng_next_f64(__mojor_rng_state) * ", limit_expr, ") + 1)"),
    paste0(base_indent, "  if idx > ", limit_expr, ": idx = ", limit_expr),
    paste0(base_indent, "  ", out_line)
  )
}

.mojor_ir_sample_emit_replace_false <- function(base_indent, size_str, limit_expr, used_var, out_line) {
  c(
    paste0(base_indent, used_var, " = alloc_bool(", limit_expr, ")"),
    paste0(base_indent, "for i in range(1, ", size_str, " + 1):"),
    paste0(base_indent, "  while True:"),
    paste0(base_indent, "    idx = Int((_rng_next_f64(__mojor_rng_state) * ", limit_expr, ") + 1)"),
    paste0(base_indent, "    if idx > ", limit_expr, ": idx = ", limit_expr),
    paste0(base_indent, "    if not ", used_var, "[Int(idx - 1)]:"),
    paste0(base_indent, "      ", used_var, "[Int(idx - 1)] = True"),
    paste0(base_indent, "      ", out_line),
    paste0(base_indent, "      break")
  )
}

.mojor_ir_stmt_emit <- function(node, indent = "    ", zero_based_vars = NULL, out_name = NULL, na_guard = "forbid", bounds_check = FALSE, loop_var = NULL, scalar_name = NULL, type_env = NULL, unroll = NULL, schedule = NULL, bounds_guard_cache = NULL, na_guard_cache = NULL) {
 # Step 4.4: Added loop_var parameter for guard optimization
 # Step 5.1: Added type_env parameter for logical array support
 # Step 8.12: Added unroll parameter for loop unrolling optimization
  if (is.null(node) || is.null(node$kind)) {
    return(NULL)
  }
  if (node$kind == "raw") {
    return(NULL)
  }
  if (node$kind == "assign") {
    nd_special_rhs_scalar <- TRUE
    if (!is.null(type_env) && !is.null(node$rhs)) {
      rhs_ty <- tryCatch(.mojor_ir_infer_type(node$rhs, type_env), error = function(e) NULL)
      if (!is.null(rhs_ty) && .mojor_type_is_array(rhs_ty)) {
        nd_special_rhs_scalar <- FALSE
      }
    }
    is_nd_negative_exclusion <- function(idx) {
      !is.null(idx$neg_exclusion) || !is.null(idx$neg_vec_exclusion)
    }
    has_negative_write_selector <- function(lhs_node) {
      if (is.null(lhs_node) || is.null(lhs_node$kind) ||
          !(lhs_node$kind %in% c("index", "subscript")) ||
          is.null(lhs_node$indices) || length(lhs_node$indices) == 0L) {
        return(FALSE)
      }
      any(vapply(lhs_node$indices, is_nd_negative_exclusion, logical(1)))
    }
    lhs_base_name <- function(lhs_node) {
      if (is.null(lhs_node) || is.null(lhs_node$kind)) {
        return(NULL)
      }
      if (identical(lhs_node$kind, "subscript")) {
        return(lhs_node$var)
      }
      if (identical(lhs_node$kind, "index") &&
          !is.null(lhs_node$base) &&
          !is.null(lhs_node$base$kind) &&
          identical(lhs_node$base$kind, "var")) {
        return(lhs_node$base$name)
      }
      NULL
    }
    if (has_negative_write_selector(node$lhs)) {
      neg_base <- lhs_base_name(node$lhs)
      if (is.null(out_name) || !nzchar(out_name) ||
          is.null(neg_base) || !identical(neg_base, out_name)) {
        .mojor_err(
          .mojor_diag_index_negative_write_non_output(),
          node$src,
          "rewrite to assign through the output variable or use explicit loop updates"
        )
      }
    }
 # Step 8.5: Check for subscript LHS with slices
    if (!is.null(node$lhs$kind) && node$lhs$kind == "subscript") {
 # Step 8.5a: Check for N-d negative exclusion write patterns before slice assign.
      has_nd_special_write <- length(node$lhs$indices) >= 2 &&
        any(vapply(
          node$lhs$indices,
          is_nd_negative_exclusion,
          logical(1)
        ))
      if (has_nd_special_write && isTRUE(nd_special_rhs_scalar)) {
        nd_excl_write_lines <- .mojor_ir_nd_exclusion_write_emit(
          node, indent, zero_based_vars, type_env, loop_var,
          bounds_check = bounds_check
        )
        if (!is.null(nd_excl_write_lines)) {
          return(nd_excl_write_lines)
        }
      }
 # Check if any indices are slices
      has_slice <- FALSE
      for (idx in node$lhs$indices) {
        if (idx$kind %in% c("slice_index", "missing_index")) {
          has_slice <- TRUE
          break
        }
      }
      if (has_slice) {
        slice_lines <- .mojor_ir_slice_assign_emit(node, indent, zero_based_vars, out_name, type_env, bounds_check = bounds_check)
        if (is.null(slice_lines)) {
          .mojor_err(
            .mojor_diag_index_slice_assign_lowering_failed(),
            node$src,
            "Use canonical slice/index expressions or simplify RHS indexing"
          )
        }
        return(slice_lines)
      }
 # Step 8.11: Handle subscript without slices (regular 2D matrix indexing)
 # Convert subscript to index node and continue processing
 # Convert subscript node to index node (subscript has var:string, index has base:node)
      index_node <- list(
        kind = "index",
        base = list(kind = "var", name = node$lhs$var),
        indices = node$lhs$indices,
        index_base = "one_based"
      )
      node$lhs <- index_node
    }
    if (!is.null(node$lhs$kind) &&
        node$lhs$kind == "index" &&
        length(node$lhs$indices) >= 2 &&
        any(vapply(node$lhs$indices, is_nd_negative_exclusion, logical(1))) &&
        isTRUE(nd_special_rhs_scalar)) {
      nd_excl_write_lines <- .mojor_ir_nd_exclusion_write_emit(
        node, indent, zero_based_vars, type_env, loop_var,
        bounds_check = bounds_check
      )
      if (!is.null(nd_excl_write_lines)) {
        return(nd_excl_write_lines)
      }
    }

 # Step 8.10: Check for mask assignment (logical array index)
    if (!is.null(node$lhs$kind) && node$lhs$kind == "index" && !is.null(type_env)) {
 # Check if any index is a logical array
      for (idx in node$lhs$indices) {
        if (idx$kind == "var") {
          idx_type <- type_env[[idx$name]]
          if (!is.null(idx_type) && .mojor_is_logical_mask_type(idx_type)) {
 # This is a mask assignment: out[mask] <- rhs
            mask_lines <- .mojor_ir_mask_assign_emit(node, indent, zero_based_vars, type_env, loop_var)
            if (!is.null(mask_lines)) {
              return(mask_lines)
            }
          }
        }
      }
    }
 # Step 8.12: Check for mask extraction assignment: out <- x[mask]
    if (!is.null(node$lhs$kind) && node$lhs$kind == "var" && !is.null(type_env)) {
      mask_extract_lines <- .mojor_ir_mask_extract_assign_emit(node, indent, zero_based_vars, type_env, loop_var)
      if (!is.null(mask_extract_lines)) {
        return(mask_extract_lines)
      }
    }
 # Matrix line extraction: lhs <- mat[row, ] or lhs <- mat[, col]
    if (!is.null(node$lhs$kind) && node$lhs$kind == "var") {
      nd_missing_matrix_lines <- .mojor_ir_nd_missing_matrix_subset_emit(
        node,
        indent,
        zero_based_vars,
        type_env,
        loop_var,
        out_name = out_name
      )
      if (!is.null(nd_missing_matrix_lines)) {
        return(nd_missing_matrix_lines)
      }
      matrix_missing_lines <- .mojor_ir_matrix_missing_subset_emit(
        node,
        indent,
        zero_based_vars,
        type_env,
        loop_var,
        out_name = out_name
      )
      if (!is.null(matrix_missing_lines)) {
        return(matrix_missing_lines)
      }
    }
 # Positive gather subset: out <- x[idx]
    if (!is.null(node$lhs$kind) && node$lhs$kind == "var") {
      pos_subset_lines <- .mojor_ir_positive_subset_emit(
        node,
        indent,
        zero_based_vars,
        type_env,
        loop_var,
        out_name = out_name
      )
      if (!is.null(pos_subset_lines)) {
        return(pos_subset_lines)
      }
    }
 # Step 8.13: Check for exclusion subset assignment: out <- x[-k] or out <- x[-c(...)]
    if (!is.null(node$lhs$kind) && node$lhs$kind == "var") {
      excl_lines <- .mojor_ir_exclusion_subset_emit(
        node,
        indent,
        zero_based_vars,
        type_env,
        loop_var,
        out_name = out_name
      )
      if (!is.null(excl_lines)) {
        return(excl_lines)
      }
      vec_excl_lines <- .mojor_ir_vec_exclusion_subset_emit(
        node,
        indent,
        zero_based_vars,
        type_env,
        loop_var,
        out_name = out_name
      )
      if (!is.null(vec_excl_lines)) {
        return(vec_excl_lines)
      }
      nd_excl_lines <- .mojor_ir_nd_exclusion_subset_emit(
        node, indent, zero_based_vars, type_env, loop_var,
        bounds_check = bounds_check
      )
      if (!is.null(nd_excl_lines)) {
        return(nd_excl_lines)
      }
    }
 # Step 8.14: Check for exclusion write: out[-k] <- val or out[-c(...)] <- val
    if (!is.null(node$lhs$kind) && node$lhs$kind == "index" &&
        length(node$lhs$indices) == 1 &&
        !is.null(node$lhs$indices[[1]]$neg_exclusion)) {
      excl_write_lines <- .mojor_ir_exclusion_write_emit(node, indent, zero_based_vars, type_env, loop_var)
      if (!is.null(excl_write_lines)) {
        return(excl_write_lines)
      }
    }
    if (!is.null(node$lhs$kind) && node$lhs$kind == "index" &&
        length(node$lhs$indices) == 1 &&
        !is.null(node$lhs$indices[[1]]$neg_vec_exclusion)) {
      vec_excl_write_lines <- .mojor_ir_vec_exclusion_write_emit(node, indent, zero_based_vars, type_env, loop_var)
      if (!is.null(vec_excl_write_lines)) {
        return(vec_excl_write_lines)
      }
    }
 # Step 5.1: Check if LHS is logical array
    is_logical_output <- FALSE
    if (!is.null(type_env)) {
      if (node$lhs$kind == "var") {
        lhs_type <- type_env[[node$lhs$name]]
        if (!is.null(lhs_type) && lhs_type %in% c("lgl", "bool")) {
          is_logical_output <- TRUE
        }
      } else if (node$lhs$kind == "index" && node$lhs$base$kind == "var") {
        lhs_type <- type_env[[node$lhs$base$name]]
        if (!is.null(lhs_type) && lhs_type %in% c("lgl[]", "bool[]")) {
          is_logical_output <- TRUE
        }
      }
    }

    lhs_str <- NULL
    if (node$lhs$kind == "var") {
      lhs_str <- node$lhs$name
    } else if (node$lhs$kind == "index") {
 # For LHS, don't use _mojor_read_lgl (we're writing, not reading)
 # Step 8.11: Pass out_name for matrix output detection
      lhs_str <- .mojor_ir_index_emit(node$lhs, zero_based_vars, type_env, loop_var, out_name, write_context = TRUE)
    }
    if (is.null(lhs_str)) {
      return(NULL)
    }

    if (node$lhs$kind == "var" && is.null(loop_var) &&
      !is.null(node$rhs$kind) && node$rhs$kind == "c" &&
      !is.null(node$rhs$parts) && length(node$rhs$parts) > 0) {
      parts <- node$rhs$parts
      lhs_name <- node$lhs$name
      to_num_const <- function(part) {
        if (is.null(part) || !is.list(part) || is.null(part$kind) || part$kind != "const") {
          return(NULL)
        }
        val <- part$value
        if ((is.numeric(val) || is.integer(val)) && length(val) == 1 && !is.na(val) && is.finite(as.numeric(val))) {
          return(format(as.numeric(val), scientific = FALSE, trim = TRUE))
        }
        if (is.character(val) && length(val) == 1 && nzchar(val)) {
          num <- suppressWarnings(as.numeric(val))
          if (!is.na(num) && is.finite(num)) {
            return(format(num, scientific = FALSE, trim = TRUE))
          }
        }
        NULL
      }
      to_chr_const <- function(part) {
        if (is.null(part) || !is.list(part) || is.null(part$kind) || part$kind != "const") {
          return(NULL)
        }
        val <- part$value
        if (is.character(val) && length(val) == 1 &&
          (grepl("^\".*\"$", val) || grepl("^'.*'$", val))) {
          return(val)
        }
        NULL
      }
      num_values <- vapply(parts, function(part) {
        out <- to_num_const(part)
        if (is.null(out)) "" else out
      }, character(1))
      chr_values <- vapply(parts, function(part) {
        out <- to_chr_const(part)
        if (is.null(out)) "" else out
      }, character(1))
      if (all(nzchar(num_values)) || all(nzchar(chr_values))) {
        if (is.null(.mojor_state$current_const_array_vars)) {
          .mojor_state$current_const_array_vars <- list()
        }
        .mojor_state$current_const_array_vars[[lhs_name]] <- TRUE
        if (is.null(.mojor_state$current_len_var_map)) {
          .mojor_state$current_len_var_map <- list()
        }
        values_len <- if (all(nzchar(num_values))) length(num_values) else length(chr_values)
        len_var_name <- paste0("n_", lhs_name, "_i")
        len_expr <- paste0("Int(", values_len, ")")
        .mojor_state$current_len_var_map[[lhs_name]] <- len_var_name
        if (all(nzchar(num_values))) {
          is_int_const <- all(grepl("^-?[0-9]+$", num_values))
          alloc_type <- if (is_int_const) "Int32" else "Float64"
          out_lines <- c(
            paste0(indent, "var ", len_var_name, " = ", len_expr),
            paste0(indent, "var ", lhs_name, " = alloc[", alloc_type, "](", length(num_values), ")")
          )
          for (i in seq_along(num_values)) {
            value_expr <- if (is_int_const) paste0("Int32(", num_values[[i]], ")") else num_values[[i]]
            out_lines <- c(out_lines, paste0(indent, lhs_name, "[", i - 1L, "] = ", value_expr))
          }
          return(out_lines)
        }
        # Local chr vars used for element-wise loop indexing are emitted
        # in the prelude (transpile.R). Skip re-emission here.
        local_chr_info <- .mojor_state$current_chr_index_local_vars
        if (!is.null(local_chr_info) && lhs_name %in% names(local_chr_info$vars)) {
          return(paste0(indent, "# ", lhs_name, " emitted in prelude"))
        }
        out_lines <- c(
          paste0(indent, "var ", len_var_name, " = ", len_expr),
          paste0(indent, "var ", lhs_name, " = alloc[String](", length(chr_values), ")")
        )
        for (i in seq_along(chr_values)) {
          out_lines <- c(out_lines, paste0(indent, lhs_name, "[", i - 1L, "] = ", chr_values[[i]]))
        }
        return(out_lines)
      }
    }

 # Whole-array set/match and quantile ops emit as calls that fill LHS.
    if (node$lhs$kind == "var" && is.null(loop_var) &&
      !is.null(node$rhs$kind) &&
      node$rhs$kind %in% c("unique", "duplicated", "match", "in", "quantile")) {
      tier7_lines <- .mojor_ir_tier7_vector_call_emit(node$rhs, node$lhs$name, type_env, indent)
      if (!is.null(tier7_lines)) {
        return(tier7_lines)
      }
    }
 # Whole-array HOF ops emit as explicit loops filling LHS.
    if (node$lhs$kind == "var" && is.null(loop_var) &&
      !is.null(node$rhs$kind) &&
      node$rhs$kind %in% c("vapply", "sapply", "lapply", "mapply")) {
      tier8_lines <- .mojor_ir_tier8_hof_assign_emit(
        node$rhs,
        out_name = node$lhs$name,
        type_env = type_env,
        indent = indent,
        zero_based_vars = zero_based_vars
      )
      if (!is.null(tier8_lines)) {
        return(tier8_lines)
      }
    }
 # Whole-array native string ops (IR-native strict subset).
    if (node$lhs$kind == "var" && is.null(loop_var) &&
      !is.null(node$rhs$kind) &&
      node$rhs$kind %in% c("nchar", "nzchar", "substr", "paste")) {
      tier82_lines <- .mojor_ir_tier8_string_assign_emit(
        node$rhs,
        out_name = node$lhs$name,
        type_env = type_env,
        indent = indent,
        zero_based_vars = zero_based_vars
      )
      if (!is.null(tier82_lines)) {
        return(tier82_lines)
      }
    }
    if (node$lhs$kind == "var" && is.null(loop_var) &&
      !is.null(node$rhs$kind) && node$rhs$kind == "apply") {
      apply_lines <- .mojor_ir_apply_assign_emit(
        node$rhs,
        node$lhs$name,
        kernel_out_name = out_name,
        type_env = type_env,
        indent = indent,
        zero_based_vars = zero_based_vars,
        loop_var = loop_var
      )
      if (!is.null(apply_lines)) {
        return(apply_lines)
      }
    }
 # Loop-body constructor re-initialization (hoisted allocation)
 # When temp <- numeric(K) appears inside a loop body, the allocation was
 # hoisted to pre-loop scope by analysis. Emit a zero-fill loop here.
    if (node$lhs$kind == "var" && !is.null(loop_var) &&
      !is.null(node$rhs$kind)) {
      rhs_reinit <- NULL
      if (node$rhs$kind == "raw" && is.call(node$rhs$expr)) {
        rhs_reinit <- node$rhs$expr
      } else if (node$rhs$kind == "call" && is.call(node$rhs$src)) {
        rhs_reinit <- node$rhs$src
      }
      if (!is.null(rhs_reinit)) {
        reinit_op <- as.character(rhs_reinit[[1]])
        if (length(reinit_op) == 1 && reinit_op %in% c("numeric", "integer", "logical")) {
          lhs_name <- node$lhs$name
          local_info <- NULL
          if (!is.null(.mojor_state$current_local_vector_lengths)) {
            local_info <- .mojor_state$current_local_vector_lengths[[lhs_name]]
          }
          if (!is.null(local_info)) {
            len_ref <- .mojor_state$current_len_var_map[[lhs_name]]
            if (is.null(len_ref)) {
              len_ref <- if (is.numeric(local_info$length)) {
                paste0("Int(", as.integer(local_info$length), ")")
              } else {
                paste0("Int(", deparse(local_info$length), ")")
              }
            }
            alloc_type <- switch(reinit_op,
              integer = "Int32",
              logical = "Int32",
              "Float64"
            )
            fill_val <- if (alloc_type == "Int32") "Int32(0)" else "0.0"
            reinit_var <- .mojor_unique_loop_var("__mojor_reinit_i",
              unique(c(lhs_name, names(type_env))))
            return(c(
              paste0(indent, "for ", reinit_var, " in range(0, ", len_ref, "):"),
              paste0(indent, "  ", lhs_name, "[", reinit_var, "] = ", fill_val)
            ))
          }
        }
      }
    }
    if (node$lhs$kind == "var" && is.null(loop_var) &&
      !is.null(node$rhs$kind)) {
      rhs_expr <- NULL
      if (node$rhs$kind == "raw" && is.call(node$rhs$expr)) {
        rhs_expr <- node$rhs$expr
      } else if (node$rhs$kind == "call" && is.call(node$rhs$src)) {
        rhs_expr <- node$rhs$src
      }
      if (!is.null(rhs_expr)) {
        rhs_op <- as.character(rhs_expr[[1]])
        if (length(rhs_op) == 1 && rhs_op %in% c("numeric", "integer", "logical", "matrix", "array")) {
          rhs_parts <- as.list(rhs_expr)[-1]
          rhs_part_names <- names(rhs_parts)
          lhs_name <- node$lhs$name
          if (is.null(type_env)) type_env <- list()

          get_ctor_arg <- function(nm, pos = 1) {
            if (!is.null(rhs_part_names) && nm %in% rhs_part_names) {
              return(rhs_parts[[which(rhs_part_names == nm)[1]]])
            }
            if (length(rhs_parts) >= pos &&
              (is.null(rhs_part_names) || is.null(rhs_part_names[[pos]]) || rhs_part_names[[pos]] == "")) {
              return(rhs_parts[[pos]])
            }
            NULL
          }
          emit_scalar <- function(expr, loop_vars = character(0), idx_override = NULL, idx_zero_based = FALSE) {
            .mojor_expr_to_mojo(
              expr,
              loop_vars = loop_vars,
              types = type_env,
              in_cond = FALSE,
              zero_based_vars = zero_based_vars,
              idx_override = idx_override,
              idx_zero_based = idx_zero_based,
              suppress_len_checks = TRUE
            )
          }
          alloc_type_for <- function(lhs_spec, ctor_op) {
            if (!is.null(lhs_spec)) {
              lhs_base <- .mojor_type_base(lhs_spec)
              if (lhs_base %in% c("i32", "lgl", "bool")) {
                return("Int32")
              }
              if (identical(lhs_base, "f32")) {
                return("Float32")
              }
            }
            if (ctor_op %in% c("integer", "logical")) {
              return("Int32")
            }
            "Float64"
          }
          default_init_for <- function(alloc_type) {
            if (identical(alloc_type, "Float32")) {
              return("Float32(0.0)")
            }
            if (identical(alloc_type, "Int32")) {
              return("Int32(0)")
            }
            "0.0"
          }
          parse_ctor_bool <- function(value_expr, arg_name, default = FALSE) {
            parse_const_bool <- function(v) {
              if (is.logical(v) && length(v) == 1 && !is.na(v)) {
                return(isTRUE(v))
              }
              if (is.numeric(v) && length(v) == 1 && !is.na(v) && v %in% c(0, 1)) {
                return(isTRUE(as.logical(v)))
              }
              if (is.character(v) && length(v) == 1) {
                up <- toupper(v)
                if (up %in% c("TRUE", "FALSE")) {
                  return(identical(up, "TRUE"))
                }
              }
              NULL
            }
            parse_bool_type <- function(expr_node) {
              if (is.null(expr_node)) {
                return(NULL)
              }
              ir_node <- tryCatch(.mojor_ir_expr_build(expr_node), error = function(e) NULL)
              if (is.null(ir_node) || !is.list(ir_node) || is.null(ir_node$kind)) {
                return(NULL)
              }
              tryCatch(
                if (!is.null(type_env)) .mojor_ir_infer_type(ir_node, type_env) else "unknown",
                error = function(e) "unknown"
              )
            }
            emit_bool_expr <- function(expr_node) {
              prev_ctor_mode <- .mojor_state$current_constructor_mode
              on.exit({
                .mojor_state$current_constructor_mode <- prev_ctor_mode
              }, add = TRUE)
              .mojor_state$current_constructor_mode <- TRUE
              emit_scalar(expr_node)
            }
            if (is.null(value_expr)) {
              return(list(is_const = TRUE, value = isTRUE(default), cond_expr = if (isTRUE(default)) "True" else "False"))
            }
            if (is.name(value_expr)) {
              nm <- as.character(value_expr)
              if (nm %in% c("TRUE", "FALSE")) {
                val <- identical(nm, "TRUE")
                return(list(is_const = TRUE, value = val, cond_expr = if (isTRUE(val)) "True" else "False"))
              }
              spec <- if (!is.null(type_env)) type_env[[nm]] else NULL
              if (is.null(spec) || !(spec %in% c("lgl", "bool", "i32"))) {
                stop(
                  "mojor_transpile: matrix(", arg_name,
                  "=...) non-literal flags must be lgl/bool/i32 scalar variables"
                )
              }
              cond_expr <- if (spec %in% c("i32")) paste0("(Int(", nm, ") != 0)") else nm
              return(list(is_const = FALSE, value = NA, cond_expr = cond_expr))
            }
            const_node <- tryCatch(.mojor_ir_expr_build(value_expr), error = function(e) NULL)
            if (!is.null(const_node) && is.list(const_node) && !is.null(const_node$kind)) {
              const_val <- tryCatch(.mojor_ir_eval_const(const_node), error = function(e) NULL)
              lit_const <- parse_const_bool(const_val)
              if (!is.null(lit_const)) {
                return(list(is_const = TRUE, value = lit_const, cond_expr = if (isTRUE(lit_const)) "True" else "False"))
              }
            }
            lit <- parse_const_bool(value_expr)
            if (!is.null(lit)) {
              return(list(is_const = TRUE, value = lit, cond_expr = if (isTRUE(lit)) "True" else "False"))
            }
            inferred <- parse_bool_type(value_expr)
            if (is.null(inferred) || !(inferred %in% c("lgl", "bool", "i32", "f64", "f32", "Int", "Int32", "Int64", "Float64", "Float32"))) {
              stop(
                "mojor_transpile: matrix(", arg_name,
                "=...) non-literal flags must be typed scalar lgl/bool/i32 expressions"
              )
            }
            cond_expr <- emit_bool_expr(value_expr)
            if (is.null(cond_expr) || !nzchar(cond_expr)) {
              stop(
                "mojor_transpile: matrix(", arg_name,
                "=...) non-literal flags must be typed scalar lgl/bool/i32 expressions"
              )
            }
            if (inferred %in% c("i32", "f64", "f32", "Int", "Int32", "Int64", "Float64", "Float32")) {
              cond_expr <- paste0("((", cond_expr, ") != 0)")
            }
            return(list(is_const = FALSE, value = NA, cond_expr = cond_expr))
            stop(
              "mojor_transpile: matrix(", arg_name,
              "=...) must be a scalar logical/int flag (literal or typed scalar variable)"
            )
          }
          cast_value_for <- function(value_expr, alloc_type, data_expr = NULL) {
            if (is.null(value_expr) || !nzchar(value_expr)) {
              return(NULL)
            }
            if (identical(alloc_type, "Int32")) {
              if (is.logical(data_expr) && length(data_expr) == 1) {
                if (is.na(data_expr)) {
                  return("Int32(-2147483648)")
                }
                return(if (isTRUE(data_expr)) "Int32(1)" else "Int32(0)")
              }
              return(paste0("Int32(", value_expr, ")"))
            }
            if (identical(alloc_type, "Float32")) {
              return(paste0("Float32(", value_expr, ")"))
            }
            paste0("Float64(", value_expr, ")")
          }
          read_helper_for_spec <- function(spec) {
            if (is.null(spec)) {
              return("_mojor_read_f64")
            }
            spec_base <- .mojor_type_base(spec)
            if (spec_base %in% c("i32", "lgl", "bool")) {
              return("_mojor_read_i32")
            }
            if (identical(spec_base, "f32")) {
              return("_mojor_read_f32")
            }
            "_mojor_read_f64"
          }
          emit_fill_value <- function(data_expr, idx_expr, idx_var, alloc_type) {
            if (is.null(data_expr)) {
              return(NULL)
            }
            if (is.name(data_expr)) {
              src_name <- as.character(data_expr)
              src_spec <- type_env[[src_name]]
              if (!is.null(src_spec) && .mojor_type_is_array(src_spec)) {
                len_var_map <- .mojor_state$current_len_var_map
                src_len <- NULL
                if (!is.null(len_var_map) && !is.null(len_var_map[[src_name]])) {
                  src_len <- len_var_map[[src_name]]
                } else {
                  n_source_name <- .mojor_state$current_n_source_name
                  if (!is.null(n_source_name) && identical(src_name, n_source_name)) src_len <- "n_i"
                }
                if (is.null(src_len)) {
                  return(NULL)
                }
                src_idx <- paste0("Int((", idx_expr, ") % Int(", src_len, "))")
                read_expr <- .mojor_ir_read_call(
                  read_helper_for_spec(src_spec),
                  src_name,
                  src_idx,
                  paste0("Int(", src_len, ")")
                )
                return(cast_value_for(read_expr, alloc_type, data_expr))
              }
            }
            prev_ctor_mode <- .mojor_state$current_constructor_mode
            value_expr <- tryCatch(
              {
                .mojor_state$current_constructor_mode <- TRUE
                emit_scalar(data_expr, loop_vars = idx_var, idx_override = idx_expr, idx_zero_based = TRUE)
              },
              finally = {
                .mojor_state$current_constructor_mode <- prev_ctor_mode
              }
            )
            cast_value_for(value_expr, alloc_type, data_expr)
          }
          emit_alloc_init <- function(total_len_expr, alloc_type, data_expr = NULL, data_idx_fn = NULL) {
            if (is.null(total_len_expr) || !nzchar(total_len_expr)) {
              return(NULL)
            }
            if (is.null(.mojor_state$current_len_var_map)) {
              .mojor_state$current_len_var_map <- list()
            }
            len_expr <- total_len_expr
            .mojor_state$current_len_var_map[[lhs_name]] <- len_expr
            reuse_existing_out <- !is.null(out_name) && identical(lhs_name, out_name)

            init_i <- .mojor_unique_loop_var("__mojor_alloc_i", unique(c(lhs_name, names(type_env))))
            idx_expr <- if (!is.null(data_idx_fn)) data_idx_fn(init_i) else init_i
            fill_expr <- emit_fill_value(data_expr, idx_expr, init_i, alloc_type)
            if (is.null(fill_expr)) fill_expr <- default_init_for(alloc_type)
            alloc_line <- if (isTRUE(reuse_existing_out)) {
              character(0)
            } else {
              paste0(indent, "var ", lhs_name, " = alloc[", alloc_type, "](", len_expr, ")")
            }
            c(
              alloc_line,
              paste0(indent, "for ", init_i, " in range(0, ", len_expr, "):"),
              paste0(indent, "  ", lhs_name, "[", init_i, "] = ", fill_expr)
            )
          }
          emit_alloc_init_dynamic_byrow <- function(total_len_expr, alloc_type, data_expr, byrow_cond_expr, nrow_ref, ncol_ref) {
            if (is.null(total_len_expr) || !nzchar(total_len_expr)) {
              return(NULL)
            }
            if (is.null(.mojor_state$current_len_var_map)) {
              .mojor_state$current_len_var_map <- list()
            }
            len_expr <- total_len_expr
            .mojor_state$current_len_var_map[[lhs_name]] <- len_expr
            reuse_existing_out <- !is.null(out_name) && identical(lhs_name, out_name)
            init_i <- .mojor_unique_loop_var("__mojor_alloc_i", unique(c(lhs_name, names(type_env))))
            idx_row_expr <- paste0(
              "(((", init_i, ") % Int(", nrow_ref, ")) * Int(", ncol_ref,
              ") + (", init_i, " // Int(", nrow_ref, ")))"
            )
            fill_col_expr <- emit_fill_value(data_expr, init_i, init_i, alloc_type)
            if (is.null(fill_col_expr)) fill_col_expr <- default_init_for(alloc_type)
            fill_row_expr <- emit_fill_value(data_expr, idx_row_expr, init_i, alloc_type)
            if (is.null(fill_row_expr)) fill_row_expr <- default_init_for(alloc_type)
            alloc_line <- if (isTRUE(reuse_existing_out)) {
              character(0)
            } else {
              paste0(indent, "var ", lhs_name, " = alloc[", alloc_type, "](", len_expr, ")")
            }
            c(
              alloc_line,
              paste0(indent, "for ", init_i, " in range(0, ", len_expr, "):"),
              paste0(indent, "  if ", byrow_cond_expr, ":"),
              paste0(indent, "    ", lhs_name, "[", init_i, "] = ", fill_row_expr),
              paste0(indent, "  else:"),
              paste0(indent, "    ", lhs_name, "[", init_i, "] = ", fill_col_expr)
            )
          }

          lhs_spec <- type_env[[lhs_name]]
          alloc_type <- alloc_type_for(lhs_spec, rhs_op)

          if (rhs_op %in% c("numeric", "integer", "logical")) {
            len_expr <- get_ctor_arg("length", 1)
            if (is.null(len_expr)) len_expr <- get_ctor_arg("length.out", 1)
            if (is.null(len_expr)) len_expr <- 0L
            len_expr_mojo <- emit_scalar(len_expr)
            if (!is.null(len_expr_mojo) && nzchar(len_expr_mojo)) {
              emitted <- emit_alloc_init(paste0("Int(", len_expr_mojo, ")"), alloc_type, data_expr = NULL)
              if (!is.null(emitted)) {
                return(emitted)
              }
            }
          }

          if (rhs_op == "matrix") {
            nrow_var_map <- .mojor_state$current_nrow_var_map
            ncol_var_map <- .mojor_state$current_ncol_var_map
            nrow_ref <- if (!is.null(nrow_var_map) && !is.null(nrow_var_map[[lhs_name]])) {
              nrow_var_map[[lhs_name]]
            } else {
              nrow_expr <- get_ctor_arg("nrow", 2)
              emit_scalar(nrow_expr)
            }
            ncol_ref <- if (!is.null(ncol_var_map) && !is.null(ncol_var_map[[lhs_name]])) {
              ncol_var_map[[lhs_name]]
            } else {
              ncol_expr <- get_ctor_arg("ncol", 3)
              emit_scalar(ncol_expr)
            }
            if (!is.null(nrow_ref) && !is.null(ncol_ref) && nzchar(nrow_ref) && nzchar(ncol_ref)) {
              data_expr <- get_ctor_arg("data", 1)
              if (is.null(data_expr)) data_expr <- 0
              total_len_expr <- paste0("Int(", nrow_ref, " * ", ncol_ref, ")")
              byrow_expr <- get_ctor_arg("byrow", 4)
              byrow_info <- parse_ctor_bool(byrow_expr, "byrow", default = FALSE)
              emitted <- NULL
              if (isTRUE(byrow_info$is_const)) {
                data_idx_fn <- NULL
                if (isTRUE(byrow_info$value)) {
                  data_idx_fn <- function(idx_var) {
                    paste0(
                      "(((", idx_var, ") % Int(", nrow_ref, ")) * Int(", ncol_ref,
                      ") + (", idx_var, " // Int(", nrow_ref, ")))"
                    )
                  }
                }
                emitted <- emit_alloc_init(
                  total_len_expr, alloc_type,
                  data_expr = data_expr,
                  data_idx_fn = data_idx_fn
                )
              } else {
                emitted <- emit_alloc_init_dynamic_byrow(
                  total_len_expr = total_len_expr,
                  alloc_type = alloc_type,
                  data_expr = data_expr,
                  byrow_cond_expr = byrow_info$cond_expr,
                  nrow_ref = nrow_ref,
                  ncol_ref = ncol_ref
                )
              }
              if (!is.null(emitted)) {
                return(emitted)
              }
            }
          }

          if (rhs_op == "array") {
            total_len_expr <- NULL
            dim_var_map <- .mojor_state$current_dim_var_map
            local_array_dims <- .mojor_state$current_local_array_dims
            dim_var <- if (!is.null(dim_var_map) && !is.null(dim_var_map[[lhs_name]])) dim_var_map[[lhs_name]] else NULL
            arr_dims <- if (!is.null(local_array_dims) && !is.null(local_array_dims[[lhs_name]])) local_array_dims[[lhs_name]]$dim else NULL
            if (!is.null(dim_var) && !is.null(arr_dims) && length(arr_dims) > 0) {
              factors <- vapply(seq_along(arr_dims), function(i) {
                paste0("Int(", dim_var, "[", i - 1L, "])")
              }, character(1))
              total_len_expr <- paste0("Int(", paste(factors, collapse = " * "), ")")
            } else {
              dim_expr <- get_ctor_arg("dim", 2)
              dim_parts <- NULL
              if (is.call(dim_expr) && as.character(dim_expr[[1]]) == "c") {
                dim_parts <- as.list(dim_expr)[-1]
              } else if (!is.null(dim_expr)) {
                dim_parts <- list(dim_expr)
              }
              if (!is.null(dim_parts) && length(dim_parts) > 0) {
                dim_terms <- vapply(dim_parts, function(de) {
                  de_mojo <- emit_scalar(de)
                  if (is.null(de_mojo) || !nzchar(de_mojo)) {
                    return("")
                  }
                  paste0("Int(", de_mojo, ")")
                }, character(1))
                if (all(nzchar(dim_terms))) {
                  total_len_expr <- paste0("Int(", paste(dim_terms, collapse = " * "), ")")
                }
              }
            }
            if (!is.null(total_len_expr) && nzchar(total_len_expr)) {
              data_expr <- get_ctor_arg("data", 1)
              if (is.null(data_expr)) data_expr <- 0
              emitted <- emit_alloc_init(total_len_expr, alloc_type, data_expr = data_expr)
              if (!is.null(emitted)) {
                return(emitted)
              }
            }
          }
        }
      }
    }

    rhs_str <- NULL
    if (!is.null(node$rhs$kind) && node$rhs$kind %in% c("sample_int", "sample")) {
      if (node$lhs$kind == "var") {
        if (node$rhs$kind == "sample_int") {
          return(.mojor_ir_sample_int_emit(node$rhs, indent, zero_based_vars, node$lhs$name, type_env, loop_var))
        }
        return(.mojor_ir_sample_emit(node$rhs, indent, zero_based_vars, node$lhs$name, type_env, loop_var))
      }
    }
    slice_reduce_lines <- .mojor_ir_matrix_slice_reduce_assign_emit(
      node, lhs_str, indent, zero_based_vars, loop_var, type_env
    )
    if (!is.null(slice_reduce_lines)) {
      return(slice_reduce_lines)
    }
    sample_index_lines <- .mojor_ir_sample_index_assign_emit(
      node = node,
      lhs_str = lhs_str,
      indent = indent,
      zero_based_vars = zero_based_vars,
      type_env = type_env,
      loop_var = loop_var,
      na_guard = na_guard,
      bounds_check = bounds_check,
      bounds_guard_cache = bounds_guard_cache,
      na_guard_cache = na_guard_cache
    )
    if (!is.null(sample_index_lines)) {
      return(sample_index_lines)
    }

    if (node$lhs$kind == "index" && !is.null(loop_var)) {
      register_ctor_len_check <- function(expr) {
        if (!is.call(expr)) {
          return(invisible(NULL))
        }
        op <- as.character(expr[[1]])
        if (length(op) != 1) op <- op[[1]]
        add_len_check <- function(chk) {
          if (is.null(.mojor_state$current_len_checks_c)) {
            .mojor_state$current_len_checks_c <<- list()
          }
          .mojor_state$current_len_checks_c <<- unique(c(.mojor_state$current_len_checks_c, list(chk)))
        }
        if (identical(op, "c")) {
          parts <- as.list(expr)[-1]
          if (length(parts) == 0) {
            return(invisible(NULL))
          }
          check_parts <- vector("list", length(parts))
          for (i in seq_along(parts)) {
            p <- parts[[i]]
            if (is.name(p)) {
              nm <- as.character(p)
              spec <- if (!is.null(type_env)) type_env[[nm]] else NULL
              if (!is.null(spec) && .mojor_type_is_array(spec)) {
                check_parts[[i]] <- list(kind = "array", name = nm)
              } else {
                check_parts[[i]] <- list(kind = "expr", expr_c = "1")
              }
            } else if ((is.numeric(p) || is.integer(p) || is.logical(p)) && length(p) == 1) {
              check_parts[[i]] <- list(kind = "expr", expr_c = "1")
            } else {
              return(invisible(NULL))
            }
          }
          if (!all(vapply(check_parts, function(p) identical(p$kind, "array"), logical(1)))) {
            return(invisible(NULL))
          }
          add_len_check(list(
            kind = "sum_len",
            parts = check_parts,
            message = "c(): output length must match sum of parts"
          ))
          return(invisible(NULL))
        }
        if (!op %in% c("rep_len", "rep", "rep.int")) {
          return(invisible(NULL))
        }
        parts <- as.list(expr)[-1]
        if (length(parts) < 2) {
          return(invisible(NULL))
        }
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
        len_arg <- NULL
        if (identical(op, "rep_len")) {
          len_arg <- get_arg("length.out", 2)
        } else if (!is.null(nms) && "length.out" %in% nms) {
          len_arg <- parts[[which(nms == "length.out")[1]]]
        }
        if (is.null(len_arg)) {
          return(invisible(NULL))
        }
        if ((is.numeric(len_arg) || is.integer(len_arg)) && length(len_arg) == 1 && !is.na(len_arg)) {
          add_len_check(list(kind = "rep_len", len_value = as.integer(len_arg)))
        } else if (is.name(len_arg)) {
          add_len_check(list(kind = "rep_len", len_name = as.character(len_arg)))
        }
        invisible(NULL)
      }
      if (node$rhs$kind %in% c("raw", "cast")) {
        raw_expr <- NULL
        ctor_cast <- NULL
        if (node$rhs$kind == "raw") {
          raw_expr <- node$rhs$expr
        } else if (!is.null(node$rhs$expr) && node$rhs$expr$kind == "raw") {
          raw_expr <- node$rhs$expr$expr
          ctor_cast <- node$rhs$to
        }
        if (!is.null(raw_expr) && is.call(raw_expr) &&
          as.character(raw_expr[[1]]) == "[" && length(raw_expr) >= 3) {
          base <- raw_expr[[2]]
          idx <- raw_expr[[3]]
          if (is.call(base) && .mojor_ir_expr_has_constructor(base) &&
            is.name(idx) && as.character(idx) %in% loop_var) {
            register_ctor_len_check(base)
            base_ir <- .mojor_ir_expr_build(base)
            if (!is.null(base_ir)) {
              rhs_str <- .mojor_ir_expr_emit(base_ir, zero_based_vars, type_env, loop_vars = loop_var)
              if (!is.null(rhs_str) && !is.null(ctor_cast)) {
                rhs_str <- paste0(.mojor_r_to_mojo_type(ctor_cast), "(", rhs_str, ")")
              }
            }
          }
        }
      }
      if (is.null(rhs_str)) {
        ctor_expr <- NULL
        ctor_cast <- NULL
        ctor_expr_node <- NULL
        if (node$rhs$kind %in% c("rep", "rep_len", "c", "seq", "transpose", "cbind", "rbind", "diag") && !is.null(node$rhs$src)) {
          ctor_expr_node <- node$rhs
        } else if (node$rhs$kind == "raw" && .mojor_ir_expr_has_constructor(node$rhs$expr)) {
          ctor_expr_node <- node$rhs$expr
        } else if (node$rhs$kind == "cast" && !is.null(node$rhs$expr)) {
          if (node$rhs$expr$kind %in% c("rep", "rep_len", "c", "seq", "transpose", "cbind", "rbind", "diag") && !is.null(node$rhs$expr$src)) {
            ctor_expr_node <- node$rhs$expr
            ctor_cast <- node$rhs$to
          } else if (node$rhs$expr$kind == "raw" && .mojor_ir_expr_has_constructor(node$rhs$expr$expr)) {
            ctor_expr_node <- node$rhs$expr
            ctor_cast <- node$rhs$to
          }
        }
        if (!is.null(ctor_expr_node)) {
          ctor_expr <- if (is.list(ctor_expr_node) && !is.null(ctor_expr_node$kind)) ctor_expr_node else ctor_expr_node$src
          loop_var_name <- if (length(loop_var) > 0) loop_var[[length(loop_var)]] else NULL
          if (is.null(loop_var_name)) {
            return(NULL)
          }
 # For scalar LHS assignment from a direct constructor (e.g. out[i] <- c(1, 2)),
 # R takes only the first RHS element each iteration (with warning).
          ctor_direct <- FALSE
          if (is.list(ctor_expr_node) && !is.null(ctor_expr_node$kind)) {
            ctor_direct <- ctor_expr_node$kind %in% c("rep", "rep_len", "c")
          } else if (is.call(ctor_expr)) {
            ctor_op <- as.character(ctor_expr[[1]])
            if (length(ctor_op) != 1) ctor_op <- ctor_op[[1]]
            ctor_direct <- ctor_op %in% c("c", "rep", "rep_len", "rep.int")
          }
          idx_override <- if (ctor_direct && !isTRUE(.mojor_state$current_constructor_mode)) {
            "0"
          } else if (!is.null(zero_based_vars) && loop_var_name %in% zero_based_vars) {
            loop_var_name
          } else {
            paste0("(", loop_var_name, " - 1)")
          }
          rhs_str <- .mojor_expr_to_mojo(
            ctor_expr,
            loop_vars = loop_var,
            types = type_env,
            in_cond = FALSE,
            zero_based_vars = zero_based_vars,
            idx_override = idx_override,
            idx_zero_based = TRUE,
            suppress_len_checks = TRUE
          )
          if (!is.null(rhs_str) && !is.null(ctor_cast)) {
            rhs_str <- paste0(.mojor_r_to_mojo_type(ctor_cast), "(", rhs_str, ")")
          }
        }
      }
    }
 # PR-B5 Step 2: Cumulative operations (cumsum, cumprod)
 # PR-B5 Step 3: Extended cumulative operations (cummax, cummin)
 # These require loop-carried dependency tracking with accumulator pattern
    if (is.null(rhs_str) && node$rhs$kind %in% c("cumsum", "cumprod", "cummax", "cummin")) {
      cumul_kind <- node$rhs$kind
      x_node <- node$rhs$x

 # Get or create accumulator variable
      acc_counter <- .mojor_state$current_cumul_acc_counter
      if (is.null(acc_counter)) acc_counter <- 0
      acc_counter <- acc_counter + 1
      .mojor_state$current_cumul_acc_counter <- acc_counter
      acc_var <- paste0("__mojor_cumul_acc_", acc_counter)

 # Store accumulator info for pre-loop initialization
      acc_init <- switch(cumul_kind,
        cumsum = "0.0",
        cumprod = "1.0",
        cummax = "-inf[DType.float64]()", # Start with negative infinity
        cummin = "inf[DType.float64]()" # Start with positive infinity
      )
      if (is.null(.mojor_state$current_cumul_accs)) {
        .mojor_state$current_cumul_accs <- list()
      }
      .mojor_state$current_cumul_accs[[length(.mojor_state$current_cumul_accs) + 1]] <- list(
        var = acc_var,
        init = acc_init,
        kind = cumul_kind
      )

 # Emit x[i] expression
      x_str <- .mojor_ir_expr_emit(x_node, zero_based_vars, type_env, loop_vars = loop_var)
      if (is.null(x_str)) {
        return(NULL)
      }

 # Emit assignment and accumulator update based on operation type
      assign_line <- paste0(indent, lhs_str, " = ", acc_var)

      if (cumul_kind == "cumsum") {
        update_line <- paste0(indent, acc_var, " = (", acc_var, " + ", x_str, ")")
      } else if (cumul_kind == "cumprod") {
        update_line <- paste0(indent, acc_var, " = (", acc_var, " * ", x_str, ")")
      } else if (cumul_kind == "cummax") {
        update_line <- paste0(indent, acc_var, " = max(", acc_var, ", ", x_str, ")")
      } else if (cumul_kind == "cummin") {
        update_line <- paste0(indent, acc_var, " = min(", acc_var, ", ", x_str, ")")
      }

 # Return lines with any needed guards
      bounds_guard_result <- .mojor_ir_emit_bounds_guards(node$rhs, indent, zero_based_vars, bounds_check, loop_var, guard_cache = bounds_guard_cache)
      na_guard_result <- .mojor_ir_emit_na_guard(node$rhs, indent, na_guard, zero_based_vars, type_env, guard_cache = na_guard_cache)
      assign_lines <- assign_line
      if (isTRUE(bounds_check) &&
        !is.null(node$lhs) &&
        identical(node$lhs$kind, "index")) {
        lhs_guard_result <- .mojor_ir_emit_bounds_guards(
          node$lhs,
          indent,
          zero_based_vars,
          TRUE,
          loop_var,
          guard_cache = NULL
        )
        extract_if_cond <- function(line) {
          hit <- regexec("^\\s*if\\s+(.*):\\s*$", line, perl = TRUE)
          reg <- regmatches(line, hit)[[1]]
          if (length(reg) < 2) {
            return(character(0))
          }
          cond <- trimws(reg[[2]])
          if (!nzchar(cond)) {
            return(character(0))
          }
          cond
        }
        lhs_cond_parts <- unique(unlist(lapply(lhs_guard_result$lines, extract_if_cond), use.names = FALSE))
        if (length(lhs_cond_parts) > 0) {
          lhs_oob_cond <- paste(lhs_cond_parts, collapse = " or ")
          assign_body <- assign_line
          if (startsWith(assign_line, indent)) {
            assign_body <- substring(assign_line, nchar(indent) + 1L)
          } else {
            assign_body <- trimws(assign_line)
          }
          oob_line <- if (isTRUE(.mojor_state$options$index_bounds)) {
            paste0(indent, "    __mojor_na_flag[0] = Int32(2)")
          } else {
            paste0(indent, "    _mojor_oob()")
          }
          assign_lines <- c(
            paste0(indent, "if ", lhs_oob_cond, ":"),
            oob_line,
            paste0(indent, "else:"),
            paste0(indent, "    ", assign_body)
          )
        }
      }

      return(c(
        bounds_guard_result$lines,
        na_guard_result$lines,
        update_line, # Update accumulator first
        assign_lines # Then assign to output
      ))
    }

    if (is.null(rhs_str)) {
      rhs_str <- .mojor_ir_expr_emit(node$rhs, zero_based_vars, type_env, loop_vars = loop_var)
    }
    if (is.null(rhs_str)) {
      return(NULL)
    }

 # Preserve i32 scalar intent for first-assignment locals (e.g., acc <- 0L in loops).
    if (node$lhs$kind == "var") {
      lhs_type <- if (!is.null(type_env)) type_env[[node$lhs$name]] else NULL
      rhs_type <- if (!is.null(node$rhs$type)) node$rhs$type else .mojor_ir_infer_type(node$rhs, type_env)
      if (is.null(lhs_type) && identical(rhs_type, "i32") && !grepl("^Int32\\(", rhs_str)) {
        rhs_str <- paste0("Int32(", rhs_str, ")")
      }
    }

 # Step 4.3: Bounds guards (emitted before NA guards)
 # Step 4.4: Pass loop_var for optimization
 # Step 16: Guard CSE - cache prevents redundant checks
    bounds_guard_result <- .mojor_ir_emit_bounds_guards(node$rhs, indent, zero_based_vars, bounds_check, loop_var, guard_cache = bounds_guard_cache)
    bounds_guard_lines <- bounds_guard_result$lines

 # Step 5.1: Logical output conversion
    if (is_logical_output) {
 # Check if RHS is a boolean expression
      rhs_type <- if (!is.null(node$rhs$type)) node$rhs$type else .mojor_ir_infer_type(node$rhs, type_env)
      na_checks <- character(0)
      if (identical(na_guard, "propagate")) {
        na_sources <- unique(.mojor_ir_collect_na_sources(node$rhs, zero_based_vars, type_env))
        na_checks <- .mojor_ir_na_checks(na_sources, type_env)
      }
      if (rhs_type == "bool") {
 # Convert boolean to Int32: Int32(1 if <cond> else 0)
        rhs_str <- paste0("Int32(1 if ", rhs_str, " else 0)")
      } else if (!rhs_type %in% c("i32", "lgl", "unknown")) {
 # Not a compatible integer/logical type - fallback
        return(NULL)
      }
      if (length(na_checks) > 0) {
        rhs_str <- paste0(
          "Int32(-2147483648 if (", paste(na_checks, collapse = " or "),
          ") else (", rhs_str, "))"
        )
      }
 # else: rhs_str is already an integer expression (e.g. Int32(1)), use as-is
    }

    lhs_assign_type <- NULL
    if (!is.null(type_env)) {
      if (node$lhs$kind == "var") {
        lhs_assign_type <- type_env[[node$lhs$name]]
      } else if (node$lhs$kind == "index" && !is.null(node$lhs$base) &&
        node$lhs$base$kind == "var") {
        lhs_assign_type <- type_env[[node$lhs$base$name]]
      }
    }
    rhs_assign_type <- if (!is.null(node$rhs$type)) node$rhs$type else .mojor_ir_infer_type(node$rhs, type_env)
    lhs_assign_base <- if (!is.null(lhs_assign_type)) .mojor_type_base(lhs_assign_type) else NULL
    needs_i32_assign_cast <- !isTRUE(is_logical_output) &&
      identical(lhs_assign_base, "i32") &&
      (
        identical(rhs_assign_type, "f64") ||
        identical(rhs_assign_type, "f32") ||
        grepl("_mojor_read_f64\\(", rhs_str, fixed = FALSE) ||
        grepl("_mojor_read_f32\\(", rhs_str, fixed = FALSE) ||
        grepl("^Float64\\(", trimws(rhs_str)) ||
        grepl("^Float32\\(", trimws(rhs_str))
      )
    needs_f32_assign_cast <- !isTRUE(is_logical_output) &&
      identical(lhs_assign_base, "f32") &&
      (identical(rhs_assign_type, "f64") || grepl("Float64\\(", rhs_str, fixed = TRUE))
    needs_f64_assign_cast <- !isTRUE(is_logical_output) &&
      identical(lhs_assign_base, "f64") &&
      (
        identical(rhs_assign_type, "f32") ||
        identical(rhs_assign_type, "i32") ||
        identical(rhs_assign_type, "lgl") ||
        identical(rhs_assign_type, "bool") ||
        grepl("_mojor_read_f32\\(", rhs_str, fixed = FALSE) ||
        grepl("_mojor_read_i32\\(", rhs_str, fixed = FALSE) ||
        grepl("^Int32\\(", trimws(rhs_str))
      )
    cast_rhs_for_lhs <- function(expr_str) {
      if (isTRUE(needs_i32_assign_cast)) {
        trimmed <- trimws(expr_str)
        if (grepl("^Int32\\(", trimmed)) {
          return(expr_str)
        }
        return(paste0("Int32(", expr_str, ")"))
      }
      if (!isTRUE(needs_f32_assign_cast)) {
        if (!isTRUE(needs_f64_assign_cast)) {
          return(expr_str)
        }
        trimmed <- trimws(expr_str)
        if (grepl("^Float64\\(", trimmed)) {
          return(expr_str)
        }
        return(paste0("Float64(", expr_str, ")"))
      }
      trimmed <- trimws(expr_str)
      if (grepl("^Float32\\(", trimmed)) {
        return(expr_str)
      }
      paste0("Float32(", expr_str, ")")
    }
    lhs_write_oob_cond <- NULL
    if (isTRUE(bounds_check) &&
      !is.null(node$lhs) &&
      identical(node$lhs$kind, "index")) {
      lhs_guard_result <- .mojor_ir_emit_bounds_guards(
        node$lhs,
        indent,
        zero_based_vars,
        TRUE,
        loop_var,
        guard_cache = NULL
      )
      extract_if_cond <- function(line) {
        hit <- regexec("^\\s*if\\s+(.*):\\s*$", line, perl = TRUE)
        reg <- regmatches(line, hit)[[1]]
        if (length(reg) < 2) {
          return(character(0))
        }
        cond <- trimws(reg[[2]])
        if (!nzchar(cond)) {
          return(character(0))
        }
        cond
      }
      lhs_cond_parts <- unique(unlist(lapply(lhs_guard_result$lines, extract_if_cond), use.names = FALSE))
      if (length(lhs_cond_parts) > 0) {
        lhs_write_oob_cond <- paste(lhs_cond_parts, collapse = " or ")
      }
    }
    wrap_assign_line <- function(assign_line) {
      if (is.null(lhs_write_oob_cond) || !nzchar(lhs_write_oob_cond)) {
        return(assign_line)
      }
      assign_body <- assign_line
      if (startsWith(assign_line, indent)) {
        assign_body <- substring(assign_line, nchar(indent) + 1L)
      } else {
        assign_body <- trimws(assign_line)
      }
      oob_line <- if (isTRUE(.mojor_state$options$index_bounds)) {
        paste0(indent, "    __mojor_na_flag[0] = Int32(2)")
      } else {
        paste0(indent, "    _mojor_oob()")
      }
      c(
        paste0(indent, "if ", lhs_write_oob_cond, ":"),
        oob_line,
        paste0(indent, "else:"),
        paste0(indent, "    ", assign_body)
      )
    }

 # Step 4: NA guard handling
 # PR-B5 Step 4: Also collect any pending cumulative updates
    cumul_update_lines <- character(0)
    pending_updates <- .mojor_state$current_cumul_updates
    if (!is.null(pending_updates) && length(pending_updates) > 0) {
      for (upd in pending_updates) {
        cumul_update_lines <- c(
          cumul_update_lines,
          paste0(indent, upd$var, " = ", upd$update)
        )
      }
 # Clear pending updates after collecting them
      .mojor_state$current_cumul_updates <- list()
    }

    if (na_guard == "assign" && .mojor_ir_needs_na_guard(node$rhs, "forbid")) {
 # Assign mode: wrap RHS in ternary for NA propagation
      sources <- .mojor_ir_collect_na_sources(node$rhs, zero_based_vars, type_env)
      if (length(sources) > 0) {
        checks <- .mojor_ir_na_checks(sources, type_env)
        if (length(checks) > 0) {
          rhs_str <- paste0("(nan[DType.float64]() if (", paste(checks, collapse = " or "), ") else (", rhs_str, "))")
        }
      }
      rhs_str <- cast_rhs_for_lhs(rhs_str)
      assign_line <- paste0(indent, lhs_str, " = ", rhs_str)
      assign_lines <- wrap_assign_line(assign_line)
      return(c(bounds_guard_lines, cumul_update_lines, assign_lines))
    } else {
 # Forbid mode: emit guard checks before assignment
 # Step 16: Guard CSE - pass cache to avoid redundant checks
      na_guard_result <- .mojor_ir_emit_na_guard(node$rhs, indent, na_guard, zero_based_vars, type_env, guard_cache = na_guard_cache)
      na_guard_lines <- na_guard_result$lines
      rhs_str <- cast_rhs_for_lhs(rhs_str)
      assign_line <- paste0(indent, lhs_str, " = ", rhs_str)
      assign_lines <- wrap_assign_line(assign_line)
      return(c(bounds_guard_lines, na_guard_lines, cumul_update_lines, assign_lines))
    }
  }
  if (node$kind == "loop") {
    return(.mojor_ir_loop_emit(node, indent, zero_based_vars, out_name, na_guard, bounds_check, loop_var, scalar_name, type_env, unroll, schedule))
  }
 # Step 8.2: While loop dispatcher
  if (node$kind == "while") {
    return(.mojor_ir_while_emit(node, indent, zero_based_vars, out_name, na_guard, bounds_check, loop_var, scalar_name, type_env, schedule))
  }
 # Step 8.2: Break statement dispatcher
  if (node$kind == "break") {
    return(.mojor_ir_break_emit(node, indent))
  }
 # Step 8.2: Next statement dispatcher
  if (node$kind == "next") {
    return(.mojor_ir_next_emit(node, indent))
  }
 # Step 8.8: Return statement dispatcher
  if (node$kind == "return") {
    return(.mojor_ir_return_emit(
      node, indent, out_name, scalar_name, zero_based_vars, type_env
    ))
  }
 # Step 19: Scalar reduction dispatcher
  if (node$kind == "scalar_reduce") {
    return(.mojor_ir_scalar_reduce_emit(node, indent, scalar_name, type_env))
  }
  if (node$kind == "scheduled_reduce") {
    return(.mojor_ir_scheduled_reduce_emit(node, indent, scalar_name, type_env))
  }
 # Step 8.7: Repeat loop dispatcher
  if (node$kind == "repeat") {
    return(.mojor_ir_repeat_emit(node, indent, zero_based_vars, out_name, na_guard, bounds_check, loop_var, scalar_name, type_env, schedule))
  }
  if (node$kind == "if") {
    return(.mojor_ir_if_emit(node, indent, zero_based_vars, out_name, na_guard, bounds_check, loop_var, scalar_name, type_env, schedule))
  }
  if (node$kind == "block") {
    return(.mojor_ir_block_emit(node, indent, zero_based_vars, out_name, na_guard, bounds_check, loop_var, scalar_name, type_env, schedule))
  }
 # Vectorized RNG - emit fill loop for rng_vec
  if (node$kind == "rng_vec") {
    return(.mojor_ir_rng_vec_emit(node, indent, zero_based_vars, out_name, type_env, loop_var))
  }
 # apply() - emit as loops + reductions
  if (node$kind == "apply") {
    return(.mojor_ir_apply_emit(node, indent, zero_based_vars, out_name, type_env, loop_var))
  }
 # sample_int() - emit sampling loop
  if (node$kind == "sample_int") {
    return(.mojor_ir_sample_int_emit(node, indent, zero_based_vars, out_name, type_env, loop_var))
  }
 # sample() - emit sampling from vector
  if (node$kind == "sample") {
    return(.mojor_ir_sample_emit(node, indent, zero_based_vars, out_name, type_env, loop_var))
  }
}

# Emit fill loop for rng_vec node
.mojor_ir_rng_vec_emit <- function(node, indent = "    ", zero_based_vars = NULL, out_name = NULL, type_env = NULL, loop_var = NULL) {
 # rng_vec(dist, n, params) - emit fill loop
  dist <- node$dist
  n_expr <- node$n
  params <- node$params

 # Use out_name parameter directly
  if (is.null(out_name)) {
 # No output variable - cannot emit
    return(NULL)
  }

 # Get the loop variable
  if (is.null(loop_var) || length(loop_var) == 0) {
    return(NULL)
  }
  loop_var_name <- loop_var[[length(loop_var)]]

 # Build 0-based position
  if (!is.null(zero_based_vars) && loop_var_name %in% zero_based_vars) {
    pos0 <- loop_var_name
  } else {
    pos0 <- paste0("(", loop_var_name, " - 1)")
  }

 # Get n (number of samples)
  n_str <- .mojor_ir_expr_emit(n_expr, zero_based_vars, type_env, loop_var)
  if (is.null(n_str)) {
    return(NULL)
  }

 # Build parameter values
  param_strs <- character(length(params))
  for (i in seq_along(params)) {
    param_strs[i] <- .mojor_ir_expr_emit(params[[i]], zero_based_vars, type_env, loop_var)
    if (is.null(param_strs[i])) {
      return(NULL)
    }
  }

 # Emit the fill loop
  .mojor_state$needs_mojo_random <- TRUE

  rng_expr <- .mojor_ir_scalar_rng_emit(dist, param_strs)
  if (is.null(rng_expr)) {
    return(NULL)
  }
  loop_body <- paste0(indent, "  ", out_name, "[", pos0, "] = ", rng_expr)

 # Return the loop structure as lines
  return(c(
    paste0(indent, "for ", loop_var_name, " in range(1, ", n_str, " + 1):"),
    loop_body
  ))
}



.mojor_ir_matrix_slice_reduce_assign_emit <- function(node, lhs_str, indent, zero_based_vars, loop_var, type_env) {
  if (is.null(node) || is.null(node$rhs) || is.null(node$rhs$kind)) {
    return(NULL)
  }
  if (is.null(loop_var) || length(loop_var) == 0) {
    return(NULL)
  }

  rhs <- node$rhs
  fn <- NULL
  x_node <- NULL
  na_rm <- FALSE
  if (rhs$kind == "mean") {
    fn <- "mean"
    x_node <- rhs$x
  } else if (rhs$kind == "call" && rhs$fn %in% c("sum", "min", "max") && length(rhs$args) == 1) {
    fn <- rhs$fn
    x_node <- rhs$args[[1]]
    if (rhs$fn == "sum") na_rm <- isTRUE(rhs$na_rm)
  } else {
    return(NULL)
  }

  if (is.null(x_node) || x_node$kind != "index") {
    return(NULL)
  }
  if (is.null(x_node$base) || x_node$base$kind != "var") {
    return(NULL)
  }
  if (is.null(x_node$indices) || length(x_node$indices) != 2) {
    return(NULL)
  }

  unwrap_idx <- function(idx) {
    cur <- idx
    repeat {
      if (is.null(cur) || !is.list(cur) || is.null(cur$kind)) break
      if (cur$kind == "scalar_index" && !is.null(cur$expr)) {
        cur <- cur$expr
        next
      }
      if (cur$kind == "cast" && !is.null(cur$expr)) {
        cur <- cur$expr
        next
      }
      break
    }
    cur
  }
  is_missing_idx <- function(idx) {
    idx <- unwrap_idx(idx)
    is.list(idx) && identical(idx$kind, "missing_index")
  }

  idx1 <- x_node$indices[[1]]
  idx2 <- x_node$indices[[2]]
  margin <- NULL
  fixed_idx <- NULL
  if (!is_missing_idx(idx1) && is_missing_idx(idx2)) {
    margin <- 1L
    fixed_idx <- unwrap_idx(idx1)
  } else if (is_missing_idx(idx1) && !is_missing_idx(idx2)) {
    margin <- 2L
    fixed_idx <- unwrap_idx(idx2)
  } else {
    return(NULL)
  }
  fixed_str <- .mojor_ir_expr_emit(fixed_idx, zero_based_vars, type_env, loop_var, index_context = TRUE)
  if (is.null(fixed_str) || fixed_str == "") {
    return(NULL)
  }

  x_name <- x_node$base$name
  local_matrix_dims <- .mojor_state$current_local_matrix_dims
  local_matrix <- NULL
  if (!is.null(local_matrix_dims) && !is.null(local_matrix_dims[[x_name]])) {
    local_matrix <- local_matrix_dims[[x_name]]
  }
  x_read_name <- x_name
  if (!is.null(local_matrix) && !is.null(local_matrix$data) && nzchar(local_matrix$data)) {
    x_read_name <- local_matrix$data
  }
  nrow_map <- .mojor_state$current_nrow_var_map
  ncol_map <- .mojor_state$current_ncol_var_map
  len_map <- .mojor_state$current_len_var_map
  if (!is.null(local_matrix)) {
    nrow_var <- paste0("nrow_", x_name)
    ncol_var <- paste0("ncol_", x_name)
  } else {
    nrow_var <- if (!is.null(nrow_map) && !is.null(nrow_map[[x_name]])) nrow_map[[x_name]] else paste0("nrow_", x_name, "_i")
    ncol_var <- if (!is.null(ncol_map) && !is.null(ncol_map[[x_name]])) ncol_map[[x_name]] else paste0("ncol_", x_name, "_i")
  }
  len_var <- if (!is.null(len_map) && !is.null(len_map[[x_name]])) len_map[[x_name]] else paste0("(", nrow_var, " * ", ncol_var, ")")
  iter_extent <- if (margin == 1L) ncol_var else nrow_var

  x_type <- if (!is.null(type_env)) type_env[[x_name]] else NULL
  read_helper <- if (!is.null(x_type) && x_type %in% c("i32[]", "i32")) {
    "_mojor_read_i32"
  } else if (!is.null(x_type) && x_type %in% c("f32[]", "f32")) {
    "_mojor_read_f32"
  } else {
    "_mojor_read_f64"
  }

  used_names <- unique(c(loop_var, names(type_env), x_name))
  inner_var <- .mojor_unique_loop_var("__mojor_j", used_names)
  used_names <- c(used_names, inner_var)
  acc_var <- .mojor_unique_loop_var("__mojor_acc", used_names)
  used_names <- c(used_names, acc_var)
  val_var <- .mojor_unique_loop_var("__mojor_val", used_names)
  used_names <- c(used_names, val_var)
  idx_var <- .mojor_unique_loop_var("__mojor_idx", used_names)

  row_expr <- if (margin == 1L) fixed_str else inner_var
  col_expr <- if (margin == 1L) inner_var else fixed_str
  linear_expr <- paste0("Int(((", row_expr, " - 1) + (", col_expr, " - 1) * ", nrow_var, "))")

  init_expr <- if (fn == "min") "_MOJOR_INF" else if (fn == "max") "_MOJOR_NINF" else "0.0"
  lines <- c(
    paste0(indent, "var ", acc_var, " = ", init_expr),
    paste0(indent, "for ", inner_var, " in range(1, ", iter_extent, " + 1):"),
    paste0(indent, "  var ", idx_var, " = ", linear_expr),
    paste0(
      indent, "  var ", val_var, " = ",
      .mojor_ir_read_call(read_helper, x_read_name, idx_var, paste0("Int(", len_var, ")"))
    )
  )

  if (fn == "sum") {
    if (isTRUE(na_rm)) {
      lines <- c(lines, paste0(indent, "  if ", val_var, " == ", val_var, ":"))
      lines <- c(lines, paste0(indent, "    ", acc_var, " = ", acc_var, " + ", val_var))
    } else {
      lines <- c(lines, paste0(indent, "  ", acc_var, " = ", acc_var, " + ", val_var))
    }
  } else if (fn == "mean") {
    lines <- c(lines, paste0(indent, "  ", acc_var, " = ", acc_var, " + ", val_var))
  } else if (fn == "min") {
    lines <- c(lines, paste0(indent, "  if ", val_var, " != ", val_var, ":"))
    lines <- c(lines, paste0(indent, "    ", acc_var, " = _MOJOR_NAN"))
    lines <- c(lines, paste0(indent, "  elif ", val_var, " < ", acc_var, ":"))
    lines <- c(lines, paste0(indent, "    ", acc_var, " = ", val_var))
  } else if (fn == "max") {
    lines <- c(lines, paste0(indent, "  if ", val_var, " != ", val_var, ":"))
    lines <- c(lines, paste0(indent, "    ", acc_var, " = _MOJOR_NAN"))
    lines <- c(lines, paste0(indent, "  elif ", val_var, " > ", acc_var, ":"))
    lines <- c(lines, paste0(indent, "    ", acc_var, " = ", val_var))
  } else {
    return(NULL)
  }

  result_expr <- if (fn == "mean") paste0(acc_var, " / Float64(", iter_extent, ")") else acc_var
  c(lines, paste0(indent, lhs_str, " = ", result_expr))
}

# Resolve matrix metadata for apply() emission.
.mojor_ir_apply_matrix_meta <- function(x_name, type_env = NULL) {
  if (is.null(x_name) || !nzchar(x_name)) {
    return(NULL)
  }

  local_matrix_dims <- .mojor_state$current_local_matrix_dims
  local_matrix <- NULL
  if (!is.null(local_matrix_dims) && !is.null(local_matrix_dims[[x_name]])) {
    local_matrix <- local_matrix_dims[[x_name]]
  }

  x_read_name <- x_name
  if (!is.null(local_matrix) && !is.null(local_matrix$data) && nzchar(local_matrix$data)) {
    x_read_name <- local_matrix$data
  }

  nrow_map <- .mojor_state$current_nrow_var_map
  ncol_map <- .mojor_state$current_ncol_var_map
  len_map <- .mojor_state$current_len_var_map
  dim_map <- .mojor_state$current_dim_var_map

  has_nrow <- FALSE
  has_ncol <- FALSE
  if (!is.null(local_matrix)) {
    nrow_var <- paste0("nrow_", x_name)
    ncol_var <- paste0("ncol_", x_name)
    has_nrow <- TRUE
    has_ncol <- TRUE
  } else {
    if (!is.null(nrow_map) && !is.null(nrow_map[[x_name]])) {
      nrow_var <- nrow_map[[x_name]]
      has_nrow <- TRUE
    }
    if (!is.null(ncol_map) && !is.null(ncol_map[[x_name]])) {
      ncol_var <- ncol_map[[x_name]]
      has_ncol <- TRUE
    }
    if ((!has_nrow || !has_ncol) && !is.null(dim_map) && !is.null(dim_map[[x_name]])) {
      dim_var <- dim_map[[x_name]]
      if (!has_nrow) {
        nrow_var <- paste0("Int(", dim_var, "[0])")
        has_nrow <- TRUE
      }
      if (!has_ncol) {
        ncol_var <- paste0("Int(", dim_var, "[1])")
        has_ncol <- TRUE
      }
    }
    if (!has_nrow) {
      nrow_var <- paste0("nrow_", x_name, "_i")
    }
    if (!has_ncol) {
      ncol_var <- paste0("ncol_", x_name, "_i")
    }
  }
  len_var <- if (!is.null(len_map) && !is.null(len_map[[x_name]])) len_map[[x_name]] else paste0("(", nrow_var, " * ", ncol_var, ")")

  x_type <- if (!is.null(type_env)) type_env[[x_name]] else NULL
  is_i32 <- !is.null(x_type) && x_type %in% c("i32", "i32[]", "i32[,]")
  is_f32 <- !is.null(x_type) && x_type %in% c("f32", "f32[]", "f32[,]")

  read_helper <- if (is_i32) {
    "_mojor_read_i32"
  } else if (is_f32) {
    "_mojor_read_f32"
  } else {
    "_mojor_read_f64"
  }

  list(
    x_read_name = x_read_name,
    nrow_var = nrow_var,
    ncol_var = ncol_var,
    len_var = len_var,
    read_helper = read_helper,
    x_type = x_type,
    is_i32 = is_i32,
    is_f32 = is_f32,
    nan_const = if (is_f32) "_MOJOR_NAN_F32" else "_MOJOR_NAN",
    inf_const = if (is_f32) "_MOJOR_INF_F32" else "_MOJOR_INF",
    ninf_const = if (is_f32) "_MOJOR_NINF_F32" else "_MOJOR_NINF"
  )
}

# Emit lhs <- apply(...) by allocating a local vector and filling it.
.mojor_ir_apply_assign_emit <- function(node, out_name, kernel_out_name = NULL, type_env = NULL, indent = "    ", zero_based_vars = NULL, loop_var = NULL) {
  if (is.null(node) || is.null(node$kind) || node$kind != "apply") {
    return(NULL)
  }
  if (is.null(out_name) || !nzchar(out_name)) {
    return(NULL)
  }
  if (is.null(node$x) || node$x$kind != "var") {
    return(NULL)
  }

  if (!is.null(kernel_out_name) && identical(out_name, kernel_out_name)) {
    return(.mojor_ir_apply_emit(node, indent, zero_based_vars, out_name, type_env, loop_var))
  }

  x_name <- node$x$name
  meta <- .mojor_ir_apply_matrix_meta(x_name, type_env)
  if (is.null(meta)) {
    return(NULL)
  }

  margin <- as.integer(node$margin)
  if (!(margin %in% c(1L, 2L))) {
    return(NULL)
  }
  extent <- if (margin == 1L) meta$nrow_var else meta$ncol_var

  lhs_type <- if (!is.null(type_env)) type_env[[out_name]] else NULL
  fun <- as.character(node$fun)
  out_mojo_type <- if (identical(fun, "mean")) {
    "Float64"
  } else if (!is.null(lhs_type) && lhs_type %in% c("i32[]", "i32")) {
    "Int32"
  } else if (!is.null(lhs_type) && lhs_type %in% c("f32[]", "f32")) {
    "Float32"
  } else if (isTRUE(meta$is_i32)) {
    "Int32"
  } else if (isTRUE(meta$is_f32)) {
    "Float32"
  } else {
    "Float64"
  }

  if (is.null(.mojor_state$current_len_var_map)) {
    .mojor_state$current_len_var_map <- list()
  }
  .mojor_state$current_len_var_map[[out_name]] <- paste0("Int(len(", out_name, "))")

  body_lines <- .mojor_ir_apply_emit(node, indent, zero_based_vars, out_name, type_env, loop_var)
  if (is.null(body_lines)) {
    return(NULL)
  }

  c(
    paste0(indent, "var ", out_name, " = alloc[", out_mojo_type, "](Int(", extent, "))"),
    body_lines
  )
}

# Emit apply() as loops + reductions.
.mojor_ir_apply_emit <- function(node, indent = "    ", zero_based_vars = NULL, out_name = NULL, type_env = NULL, loop_var = NULL) {
  if (is.null(node) || is.null(node$kind) || node$kind != "apply") {
    return(NULL)
  }
  if (is.null(out_name) || !nzchar(out_name)) {
    return(NULL)
  }
  if (is.null(node$x) || node$x$kind != "var") {
    return(NULL)
  }

  margin <- as.integer(node$margin)
  fun <- as.character(node$fun)
  scalar_bool_from_value <- function(v) {
    if (is.logical(v) && length(v) == 1 && !is.na(v)) return(isTRUE(v))
    if (is.numeric(v) && length(v) == 1 && !is.na(v) && v %in% c(0, 1)) return(isTRUE(as.logical(v)))
    if (is.character(v) && length(v) == 1) {
      if (identical(toupper(v), "TRUE")) return(TRUE)
      if (identical(toupper(v), "FALSE")) return(FALSE)
    }
    NULL
  }
  na_rm_mode <- "const"
  na_rm <- FALSE
  na_rm_expr <- NULL
  if (is.list(node$na_rm) && !is.null(node$na_rm$kind)) {
    if (identical(node$na_rm$kind, "const")) {
      v <- scalar_bool_from_value(node$na_rm$value)
      if (is.null(v)) {
        return(NULL)
      }
      na_rm <- isTRUE(v)
    } else {
      na_rm_mode <- "expr"
      na_rm_expr <- .mojor_ir_expr_emit(node$na_rm, zero_based_vars, type_env, loop_var)
      if (is.null(na_rm_expr) || !nzchar(na_rm_expr)) {
        return(NULL)
      }
      na_rm_type <- tryCatch(.mojor_ir_infer_type(node$na_rm, type_env), error = function(e) "unknown")
      if (is.character(na_rm_type) && length(na_rm_type) == 1 && na_rm_type %in% c("i32", "Int", "Int32", "Int64")) {
        na_rm_expr <- paste0("(", na_rm_expr, " != 0)")
      }
    }
  } else if (!is.null(node$na_rm)) {
    v <- scalar_bool_from_value(node$na_rm)
    if (is.null(v)) {
      return(NULL)
    }
    na_rm <- isTRUE(v)
  }
  if (!(margin %in% c(1L, 2L))) {
    return(NULL)
  }
  if (!(fun %in% c("sum", "mean", "min", "max"))) {
    return(NULL)
  }

  x_name <- node$x$name
  meta <- .mojor_ir_apply_matrix_meta(x_name, type_env)
  if (is.null(meta)) {
    return(NULL)
  }

  outer_extent <- if (margin == 1L) meta$nrow_var else meta$ncol_var
  inner_extent <- if (margin == 1L) meta$ncol_var else meta$nrow_var

  used_names <- unique(c(loop_var, names(type_env), x_name, out_name))
  outer_var <- .mojor_unique_loop_var("__mojor_apply_i", used_names)
  used_names <- c(used_names, outer_var)
  inner_var <- .mojor_unique_loop_var("__mojor_apply_j", used_names)
  used_names <- c(used_names, inner_var)
  acc_var <- .mojor_unique_loop_var("__mojor_apply_acc", used_names)
  used_names <- c(used_names, acc_var)
  val_var <- .mojor_unique_loop_var("__mojor_apply_val", used_names)
  used_names <- c(used_names, val_var)
  idx_var <- .mojor_unique_loop_var("__mojor_apply_idx", used_names)
  used_names <- c(used_names, idx_var)
  count_var <- .mojor_unique_loop_var("__mojor_apply_count", used_names)

  row_expr <- if (margin == 1L) outer_var else inner_var
  col_expr <- if (margin == 1L) inner_var else outer_var
  linear_expr <- paste0("Int(((", row_expr, " - 1) + (", col_expr, " - 1) * ", meta$nrow_var, "))")

  acc_init <- if (fun == "min") {
    if (meta$is_i32) "Int32(2147483647)" else meta$inf_const
  } else if (fun == "max") {
    if (meta$is_i32) "Int32(-2147483648)" else meta$ninf_const
  } else if (fun == "sum" && meta$is_i32) {
    "Int32(0)"
  } else {
    "0.0"
  }

  valid_pred <- if (meta$is_i32) paste0(val_var, " != Int32(-2147483648)") else paste0(val_var, " == ", val_var)
  val_for_mean <- if (meta$is_i32 || meta$is_f32) paste0("Float64(", val_var, ")") else val_var

  lines <- c(
    paste0(indent, "for ", outer_var, " in range(1, ", outer_extent, " + 1):"),
    paste0(indent, "  var ", acc_var, " = ", acc_init)
  )
  if (fun == "mean" && (na_rm || identical(na_rm_mode, "expr"))) {
    lines <- c(lines, paste0(indent, "  var ", count_var, " = 0"))
  }
  lines <- c(
    lines,
    paste0(indent, "  for ", inner_var, " in range(1, ", inner_extent, " + 1):"),
    paste0(indent, "    var ", idx_var, " = ", linear_expr),
    paste0(
      indent, "    var ", val_var, " = ",
      .mojor_ir_read_call(meta$read_helper, meta$x_read_name, idx_var, paste0("Int(", meta$len_var, ")"))
    )
  )

  if (fun == "sum") {
    if (identical(na_rm_mode, "expr")) {
      lines <- c(lines, paste0(indent, "    if ", na_rm_expr, ":"))
      lines <- c(lines, paste0(indent, "      if ", valid_pred, ":"))
      lines <- c(lines, paste0(indent, "        ", acc_var, " = ", acc_var, " + ", val_var))
      lines <- c(lines, paste0(indent, "    else:"))
      lines <- c(lines, paste0(indent, "      ", acc_var, " = ", acc_var, " + ", val_var))
    } else if (na_rm) {
      lines <- c(lines, paste0(indent, "    if ", valid_pred, ":"))
      lines <- c(lines, paste0(indent, "      ", acc_var, " = ", acc_var, " + ", val_var))
    } else {
      lines <- c(lines, paste0(indent, "    ", acc_var, " = ", acc_var, " + ", val_var))
    }
  } else if (fun == "mean") {
    if (identical(na_rm_mode, "expr")) {
      lines <- c(lines, paste0(indent, "    if ", na_rm_expr, ":"))
      lines <- c(lines, paste0(indent, "      if ", valid_pred, ":"))
      lines <- c(lines, paste0(indent, "        ", acc_var, " = ", acc_var, " + ", val_for_mean))
      lines <- c(lines, paste0(indent, "        ", count_var, " = ", count_var, " + 1"))
      lines <- c(lines, paste0(indent, "    else:"))
      lines <- c(lines, paste0(indent, "      ", acc_var, " = ", acc_var, " + ", val_for_mean))
      lines <- c(lines, paste0(indent, "      ", count_var, " = ", count_var, " + 1"))
    } else if (na_rm) {
      lines <- c(lines, paste0(indent, "    if ", valid_pred, ":"))
      lines <- c(lines, paste0(indent, "      ", acc_var, " = ", acc_var, " + ", val_for_mean))
      lines <- c(lines, paste0(indent, "      ", count_var, " = ", count_var, " + 1"))
    } else {
      lines <- c(lines, paste0(indent, "    ", acc_var, " = ", acc_var, " + ", val_for_mean))
    }
  } else if (fun == "min") {
    if (identical(na_rm_mode, "expr")) {
      lines <- c(lines, paste0(indent, "    if ", na_rm_expr, ":"))
      lines <- c(lines, paste0(indent, "      if ", valid_pred, ":"))
      lines <- c(lines, paste0(indent, "        if ", val_var, " < ", acc_var, ":"))
      lines <- c(lines, paste0(indent, "          ", acc_var, " = ", val_var))
      lines <- c(lines, paste0(indent, "    else:"))
      if (meta$is_i32) {
        lines <- c(lines, paste0(indent, "      if ", val_var, " < ", acc_var, ":"))
        lines <- c(lines, paste0(indent, "        ", acc_var, " = ", val_var))
      } else {
        lines <- c(lines, paste0(indent, "      if ", val_var, " != ", val_var, ":"))
        lines <- c(lines, paste0(indent, "        ", acc_var, " = ", meta$nan_const))
        lines <- c(lines, paste0(indent, "      elif ", val_var, " < ", acc_var, ":"))
        lines <- c(lines, paste0(indent, "        ", acc_var, " = ", val_var))
      }
    } else if (na_rm) {
      lines <- c(lines, paste0(indent, "    if ", valid_pred, ":"))
      lines <- c(lines, paste0(indent, "      if ", val_var, " < ", acc_var, ":"))
      lines <- c(lines, paste0(indent, "        ", acc_var, " = ", val_var))
    } else if (meta$is_i32) {
      lines <- c(lines, paste0(indent, "    if ", val_var, " < ", acc_var, ":"))
      lines <- c(lines, paste0(indent, "      ", acc_var, " = ", val_var))
    } else {
      lines <- c(lines, paste0(indent, "    if ", val_var, " != ", val_var, ":"))
      lines <- c(lines, paste0(indent, "      ", acc_var, " = ", meta$nan_const))
      lines <- c(lines, paste0(indent, "    elif ", val_var, " < ", acc_var, ":"))
      lines <- c(lines, paste0(indent, "      ", acc_var, " = ", val_var))
    }
  } else if (fun == "max") {
    if (identical(na_rm_mode, "expr")) {
      lines <- c(lines, paste0(indent, "    if ", na_rm_expr, ":"))
      lines <- c(lines, paste0(indent, "      if ", valid_pred, ":"))
      lines <- c(lines, paste0(indent, "        if ", val_var, " > ", acc_var, ":"))
      lines <- c(lines, paste0(indent, "          ", acc_var, " = ", val_var))
      lines <- c(lines, paste0(indent, "    else:"))
      if (meta$is_i32) {
        lines <- c(lines, paste0(indent, "      if ", val_var, " > ", acc_var, ":"))
        lines <- c(lines, paste0(indent, "        ", acc_var, " = ", val_var))
      } else {
        lines <- c(lines, paste0(indent, "      if ", val_var, " != ", val_var, ":"))
        lines <- c(lines, paste0(indent, "        ", acc_var, " = ", meta$nan_const))
        lines <- c(lines, paste0(indent, "      elif ", val_var, " > ", acc_var, ":"))
        lines <- c(lines, paste0(indent, "        ", acc_var, " = ", val_var))
      }
    } else if (na_rm) {
      lines <- c(lines, paste0(indent, "    if ", valid_pred, ":"))
      lines <- c(lines, paste0(indent, "      if ", val_var, " > ", acc_var, ":"))
      lines <- c(lines, paste0(indent, "        ", acc_var, " = ", val_var))
    } else if (meta$is_i32) {
      lines <- c(lines, paste0(indent, "    if ", val_var, " > ", acc_var, ":"))
      lines <- c(lines, paste0(indent, "      ", acc_var, " = ", val_var))
    } else {
      lines <- c(lines, paste0(indent, "    if ", val_var, " != ", val_var, ":"))
      lines <- c(lines, paste0(indent, "      ", acc_var, " = ", meta$nan_const))
      lines <- c(lines, paste0(indent, "    elif ", val_var, " > ", acc_var, ":"))
      lines <- c(lines, paste0(indent, "      ", acc_var, " = ", val_var))
    }
  }

  result_expr <- if (fun == "mean") {
    if (identical(na_rm_mode, "expr")) {
      paste0(
        "((",
        meta$nan_const,
        " if ",
        count_var,
        " == 0 else (",
        acc_var,
        " / Float64(",
        count_var,
        "))) if ",
        na_rm_expr,
        " else (",
        acc_var,
        " / Float64(",
        inner_extent,
        ")))"
      )
    } else if (na_rm) {
      paste0("(", meta$nan_const, " if ", count_var, " == 0 else (", acc_var, " / Float64(", count_var, ")))")
    } else {
      paste0(acc_var, " / Float64(", inner_extent, ")")
    }
  } else {
    acc_var
  }

  c(lines, paste0(indent, "  ", out_name, "[Int(", outer_var, " - 1)] = ", result_expr))
}

# Emit indexed sampling assignment for dynamic index in loop context.
# Handles forms like out[i] <- sample.int(n, size = k, ...)[i] without relying
# on scalar-expression emit path. Constant-index forms keep existing expr emit path.
.mojor_ir_sample_index_assign_emit <- function(node, lhs_str, indent = "    ",
                                               zero_based_vars = NULL, type_env = NULL,
                                               loop_var = NULL, na_guard = "forbid",
                                               bounds_check = FALSE, bounds_guard_cache = NULL,
                                               na_guard_cache = NULL) {
  if (is.null(node) || is.null(node$kind) || node$kind != "assign") {
    return(NULL)
  }
  if (is.null(node$lhs) || is.null(node$rhs)) {
    return(NULL)
  }
  if (node$lhs$kind != "index" || is.null(loop_var)) {
    return(NULL)
  }
  if (is.null(node$rhs$kind) || node$rhs$kind != "index") {
    return(NULL)
  }
  rhs_index <- node$rhs
  if (is.null(rhs_index$base) || is.null(rhs_index$base$kind) ||
    !(rhs_index$base$kind %in% c("sample_int", "sample"))) {
    return(NULL)
  }
  if (is.null(rhs_index$indices) || length(rhs_index$indices) != 1) {
    return(NULL)
  }

  idx_const <- suppressWarnings(as.numeric(.mojor_ir_eval_const(rhs_index$indices[[1]])))
  if (length(idx_const) == 1 && is.finite(idx_const) && idx_const == floor(idx_const)) {
 # Keep existing constant-index scalar path in expression emitter.
    return(NULL)
  }

  sample_node <- rhs_index$base
  op_label <- if (identical(sample_node$kind, "sample_int")) "sample.int" else "sample"

  replace_mode <- .mojor_ir_sample_parse_replace_mode(
    sample_node$replace,
    op_label,
    sample_node$src,
    zero_based_vars = zero_based_vars,
    type_env = type_env,
    loop_var = loop_var
  )
  replace <- if (isTRUE(replace_mode$is_const)) isTRUE(replace_mode$value) else FALSE
  prob_info <- if (!.mojor_ir_sample_is_prob_null(sample_node$prob)) {
    .mojor_ir_sample_parse_prob_var(
      sample_node$prob,
      op_label = op_label,
      src = sample_node$src,
      type_env = type_env
    )
  } else {
    NULL
  }
  size_str <- .mojor_ir_expr_emit(sample_node$size, zero_based_vars, type_env, loop_var, index_context = TRUE)
  if (is.null(size_str) || !nzchar(size_str)) {
    return(NULL)
  }

  idx_one_based_str <- .mojor_ir_expr_emit(rhs_index$indices[[1]], zero_based_vars, type_env, loop_var, index_context = TRUE)
  if (is.null(idx_one_based_str) || !nzchar(idx_one_based_str)) {
    return(NULL)
  }
  if (identical(rhs_index$index_base, "zero_based")) {
    idx_one_based_str <- paste0("(", idx_one_based_str, " + 1)")
  }

  lhs_base_type <- NULL
  if (!is.null(type_env) && !is.null(node$lhs$base) && identical(node$lhs$base$kind, "var")) {
    lhs_base_type <- type_env[[node$lhs$base$name]]
  }
  lhs_na <- if (!is.null(lhs_base_type) && lhs_base_type %in% c("i32[]", "i32", "lgl[]", "lgl", "bool[]", "bool")) {
    "Int32(-2147483648)"
  } else if (!is.null(lhs_base_type) && lhs_base_type %in% c("f32[]", "f32")) {
    "nan[DType.float32]()"
  } else {
    "nan[DType.float64]()"
  }

  sample_tmp_suffix <- paste0(lhs_str, "_", idx_one_based_str)
  sample_tmp_suffix <- gsub("[^A-Za-z0-9_]", "_", sample_tmp_suffix)
  sample_tmp_suffix <- gsub("_+", "_", sample_tmp_suffix)
  if (!nzchar(sample_tmp_suffix)) sample_tmp_suffix <- "u"
  if (nchar(sample_tmp_suffix) > 32) {
    sample_tmp_suffix <- substr(sample_tmp_suffix, nchar(sample_tmp_suffix) - 31, nchar(sample_tmp_suffix))
  }
  pick_pos_base <- paste0("__mojor_sample_pick_pos_", sample_tmp_suffix)
  pick_done_base <- paste0("__mojor_sample_pick_done_", sample_tmp_suffix)
  draw_i_base <- paste0("__mojor_sample_draw_i_", sample_tmp_suffix)
  idx_base <- paste0("__mojor_sample_idx_", sample_tmp_suffix)
  used_mask_base <- paste0("__mojor_sample_used_", sample_tmp_suffix)

  used_names <- unique(c(
    if (!is.null(names(type_env))) names(type_env) else character(0),
    if (is.null(loop_var)) character(0) else loop_var,
    if (is.null(zero_based_vars)) character(0) else zero_based_vars
  ))
  pick_pos_var <- .mojor_unique_loop_var(pick_pos_base, used_names)
  used_names <- unique(c(used_names, pick_pos_var))
  pick_done_var <- .mojor_unique_loop_var(pick_done_base, used_names)
  used_names <- unique(c(used_names, pick_done_var))
  draw_i_var <- .mojor_unique_loop_var(draw_i_base, used_names)
  used_names <- unique(c(used_names, draw_i_var))
  idx_var <- .mojor_unique_loop_var(idx_base, used_names)
  used_names <- unique(c(used_names, idx_var))
  used_mask <- .mojor_unique_loop_var(used_mask_base, used_names)
  prob_total <- prob_target <- prob_acc <- prob_j <- prob_w <- NULL
  if (!is.null(prob_info) && isTRUE(replace)) {
    used_names <- unique(c(used_names, used_mask))
    prob_total <- .mojor_unique_loop_var("__mojor_prob_total", used_names)
    used_names <- unique(c(used_names, prob_total))
    prob_target <- .mojor_unique_loop_var("__mojor_prob_target", used_names)
    used_names <- unique(c(used_names, prob_target))
    prob_acc <- .mojor_unique_loop_var("__mojor_prob_acc", used_names)
    used_names <- unique(c(used_names, prob_acc))
    prob_j <- .mojor_unique_loop_var("__mojor_prob_j", used_names)
    used_names <- unique(c(used_names, prob_j))
    prob_w <- .mojor_unique_loop_var("__mojor_prob_w", used_names)
  }

  .mojor_state$needs_mojo_random <- TRUE

  lines <- character(0)
  bounds_guard_result <- .mojor_ir_emit_bounds_guards(node$rhs, indent, zero_based_vars, bounds_check, loop_var, guard_cache = bounds_guard_cache)
  na_guard_result <- .mojor_ir_emit_na_guard(node$rhs, indent, na_guard, zero_based_vars, type_env, guard_cache = na_guard_cache)
  lines <- c(lines, bounds_guard_result$lines, na_guard_result$lines)

  lines <- c(lines, paste0(indent, lhs_str, " = ", lhs_na))
  lines <- c(lines, paste0(indent, "var ", pick_pos_var, ": Int = Int(", idx_one_based_str, ")"))
  lines <- c(lines, paste0(indent, "var ", pick_done_var, ": Bool = False"))
  lines <- c(lines, paste0(indent, "if ", pick_pos_var, " >= 1 and ", pick_pos_var, " <= Int(", size_str, "):"))
  if (!is.null(prob_info) && !isTRUE(replace_mode$is_const)) {
    .mojor_err(
      paste0(op_label, "() weighted sampling currently requires replace to be statically TRUE in this release"),
      sample_node$src,
      "Use replace = TRUE when prob is provided"
    )
  }
  if (!is.null(prob_info) && isTRUE(replace_mode$is_const) && !isTRUE(replace_mode$value)) {
    .mojor_err(
      paste0(op_label, "() weighted sampling currently supports only replace = TRUE in this release"),
      sample_node$src,
      "Use replace = TRUE for prob-based sampling or set prob = NULL"
    )
  }

  if (identical(sample_node$kind, "sample_int")) {
    n_str <- .mojor_ir_expr_emit(sample_node$n, zero_based_vars, type_env, loop_var, index_context = TRUE)
    if (is.null(n_str) || !nzchar(n_str)) {
      return(NULL)
    }

    if (is.null(prob_info) && !isTRUE(replace_mode$is_const)) {
      lines <- c(lines, paste0(indent, "  var ", idx_var, " = _mojor_sample_pick_index(__mojor_rng_state, Int(", n_str, "), Int(", size_str, "), ", pick_pos_var, ", ", replace_mode$expr, ")"))
      lines <- c(lines, paste0(indent, "  ", lhs_str, " = ", idx_var))
      lines <- c(lines, paste0(indent, "  ", pick_done_var, " = True"))
    } else if (!is.null(prob_info) && isTRUE(replace)) {
      lines <- c(lines, paste0(indent, "  for ", draw_i_var, " in range(1, Int(", size_str, ") + 1):"))
      lines <- c(lines, paste0(indent, "    var ", idx_var, ": Int = 1"))
      lines <- c(lines, paste0(indent, "    if Int(", prob_info$len, ") == Int(", n_str, "):"))
      lines <- c(lines, paste0(indent, "      var ", prob_total, ": Float64 = 0.0"))
      lines <- c(lines, paste0(indent, "      for ", prob_j, " in range(Int(", n_str, ")):"))
      lines <- c(lines, paste0(indent, "        var ", prob_w, " = _mojor_read_f64(", prob_info$name, ", ", prob_j, ", Int(", prob_info$len, "))"))
      lines <- c(lines, paste0(indent, "        if ", prob_w, " > 0.0:"))
      lines <- c(lines, paste0(indent, "          ", prob_total, " += ", prob_w))
      lines <- c(lines, paste0(indent, "      if ", prob_total, " > 0.0:"))
      lines <- c(lines, paste0(indent, "        var ", prob_target, " = _rng_next_f64(__mojor_rng_state) * ", prob_total))
      lines <- c(lines, paste0(indent, "        var ", prob_acc, ": Float64 = 0.0"))
      lines <- c(lines, paste0(indent, "        ", idx_var, " = Int(", n_str, ")"))
      lines <- c(lines, paste0(indent, "        for ", prob_j, " in range(Int(", n_str, ")):"))
      lines <- c(lines, paste0(indent, "          var ", prob_w, " = _mojor_read_f64(", prob_info$name, ", ", prob_j, ", Int(", prob_info$len, "))"))
      lines <- c(lines, paste0(indent, "          if ", prob_w, " > 0.0:"))
      lines <- c(lines, paste0(indent, "            ", prob_acc, " += ", prob_w))
      lines <- c(lines, paste0(indent, "            if ", prob_target, " <= ", prob_acc, ":"))
      lines <- c(lines, paste0(indent, "              ", idx_var, " = Int(", prob_j, " + 1)"))
      lines <- c(lines, paste0(indent, "              break"))
      lines <- c(lines, paste0(indent, "      else:"))
      lines <- c(lines, paste0(indent, "        ", idx_var, " = Int((_rng_next_f64(__mojor_rng_state) * Float64(Int(", n_str, "))) + 1.0)"))
      lines <- c(lines, paste0(indent, "        if ", idx_var, " > Int(", n_str, "): ", idx_var, " = Int(", n_str, ")"))
      lines <- c(lines, paste0(indent, "    else:"))
      lines <- c(lines, paste0(indent, "      ", idx_var, " = Int((_rng_next_f64(__mojor_rng_state) * Float64(Int(", n_str, "))) + 1.0)"))
      lines <- c(lines, paste0(indent, "      if ", idx_var, " > Int(", n_str, "): ", idx_var, " = Int(", n_str, ")"))
      lines <- c(lines, paste0(indent, "    if Int(", draw_i_var, ") == ", pick_pos_var, ":"))
      lines <- c(lines, paste0(indent, "      ", lhs_str, " = ", idx_var))
      lines <- c(lines, paste0(indent, "      ", pick_done_var, " = True"))
    } else if (isTRUE(replace)) {
      lines <- c(lines, paste0(indent, "  for ", draw_i_var, " in range(1, Int(", size_str, ") + 1):"))
      lines <- c(lines, paste0(indent, "    var ", idx_var, " = Int((_rng_next_f64(__mojor_rng_state) * Float64(Int(", n_str, "))) + 1.0)"))
      lines <- c(lines, paste0(indent, "    if ", idx_var, " > Int(", n_str, "): ", idx_var, " = Int(", n_str, ")"))
      lines <- c(lines, paste0(indent, "    if Int(", draw_i_var, ") == ", pick_pos_var, ":"))
      lines <- c(lines, paste0(indent, "      ", lhs_str, " = ", idx_var))
      lines <- c(lines, paste0(indent, "      ", pick_done_var, " = True"))
    } else {
      lines <- c(lines, paste0(indent, "  var ", used_mask, " = alloc_bool(Int(", n_str, "))"))
      lines <- c(lines, paste0(indent, "  for ", draw_i_var, " in range(1, Int(", size_str, ") + 1):"))
      lines <- c(lines, paste0(indent, "    while True:"))
      lines <- c(lines, paste0(indent, "      var ", idx_var, " = Int((_rng_next_f64(__mojor_rng_state) * Float64(Int(", n_str, "))) + 1.0)"))
      lines <- c(lines, paste0(indent, "      if ", idx_var, " > Int(", n_str, "): ", idx_var, " = Int(", n_str, ")"))
      lines <- c(lines, paste0(indent, "      if not ", used_mask, "[Int(", idx_var, " - 1)]:"))
      lines <- c(lines, paste0(indent, "        ", used_mask, "[Int(", idx_var, " - 1)] = True"))
      lines <- c(lines, paste0(indent, "        if Int(", draw_i_var, ") == ", pick_pos_var, ":"))
      lines <- c(lines, paste0(indent, "          ", lhs_str, " = ", idx_var))
      lines <- c(lines, paste0(indent, "          ", pick_done_var, " = True"))
      lines <- c(lines, paste0(indent, "        break"))
    }
  } else {
    if (is.null(sample_node$x) || sample_node$x$kind != "var") {
      .mojor_err(
        "sample() indexed dynamic emission requires x to be a direct vector variable in this release",
        sample_node$src,
        "Use sample(x, ...)[i] with direct vector argument x"
      )
    }

    x_name <- sample_node$x$name
    len_var_map <- .mojor_state$current_len_var_map
    x_len <- if (!is.null(len_var_map) && !is.null(len_var_map[[x_name]])) len_var_map[[x_name]] else "n_i"
    x_type <- if (!is.null(type_env)) type_env[[x_name]] else NULL
    sample_read <- if (!is.null(x_type) && x_type %in% c("i32[]", "i32", "lgl[]", "lgl", "bool[]", "bool")) {
      "_mojor_read_i32"
    } else if (!is.null(x_type) && x_type %in% c("f32[]", "f32")) {
      "_mojor_read_f32"
    } else {
      "_mojor_read_f64"
    }

    if (is.null(prob_info) && !isTRUE(replace_mode$is_const)) {
      lines <- c(lines, paste0(indent, "  var ", idx_var, " = _mojor_sample_pick_index(__mojor_rng_state, Int(", x_len, "), Int(", size_str, "), ", pick_pos_var, ", ", replace_mode$expr, ")"))
      lines <- c(lines, paste0(indent, "  ", lhs_str, " = ", sample_read, "(", x_name, ", Int(", idx_var, " - 1), Int(", x_len, "))"))
      lines <- c(lines, paste0(indent, "  ", pick_done_var, " = True"))
    } else if (!is.null(prob_info) && isTRUE(replace)) {
      lines <- c(lines, paste0(indent, "  for ", draw_i_var, " in range(1, Int(", size_str, ") + 1):"))
      lines <- c(lines, paste0(indent, "    var ", idx_var, ": Int = 1"))
      lines <- c(lines, paste0(indent, "    if Int(", prob_info$len, ") == Int(", x_len, "):"))
      lines <- c(lines, paste0(indent, "      var ", prob_total, ": Float64 = 0.0"))
      lines <- c(lines, paste0(indent, "      for ", prob_j, " in range(Int(", x_len, ")):"))
      lines <- c(lines, paste0(indent, "        var ", prob_w, " = _mojor_read_f64(", prob_info$name, ", ", prob_j, ", Int(", prob_info$len, "))"))
      lines <- c(lines, paste0(indent, "        if ", prob_w, " > 0.0:"))
      lines <- c(lines, paste0(indent, "          ", prob_total, " += ", prob_w))
      lines <- c(lines, paste0(indent, "      if ", prob_total, " > 0.0:"))
      lines <- c(lines, paste0(indent, "        var ", prob_target, " = _rng_next_f64(__mojor_rng_state) * ", prob_total))
      lines <- c(lines, paste0(indent, "        var ", prob_acc, ": Float64 = 0.0"))
      lines <- c(lines, paste0(indent, "        ", idx_var, " = Int(", x_len, ")"))
      lines <- c(lines, paste0(indent, "        for ", prob_j, " in range(Int(", x_len, ")):"))
      lines <- c(lines, paste0(indent, "          var ", prob_w, " = _mojor_read_f64(", prob_info$name, ", ", prob_j, ", Int(", prob_info$len, "))"))
      lines <- c(lines, paste0(indent, "          if ", prob_w, " > 0.0:"))
      lines <- c(lines, paste0(indent, "            ", prob_acc, " += ", prob_w))
      lines <- c(lines, paste0(indent, "            if ", prob_target, " <= ", prob_acc, ":"))
      lines <- c(lines, paste0(indent, "              ", idx_var, " = Int(", prob_j, " + 1)"))
      lines <- c(lines, paste0(indent, "              break"))
      lines <- c(lines, paste0(indent, "      else:"))
      lines <- c(lines, paste0(indent, "        ", idx_var, " = Int((_rng_next_f64(__mojor_rng_state) * Float64(Int(", x_len, "))) + 1.0)"))
      lines <- c(lines, paste0(indent, "        if ", idx_var, " > Int(", x_len, "): ", idx_var, " = Int(", x_len, ")"))
      lines <- c(lines, paste0(indent, "    else:"))
      lines <- c(lines, paste0(indent, "      ", idx_var, " = Int((_rng_next_f64(__mojor_rng_state) * Float64(Int(", x_len, "))) + 1.0)"))
      lines <- c(lines, paste0(indent, "      if ", idx_var, " > Int(", x_len, "): ", idx_var, " = Int(", x_len, ")"))
      lines <- c(lines, paste0(indent, "    if Int(", draw_i_var, ") == ", pick_pos_var, ":"))
      lines <- c(lines, paste0(indent, "      ", lhs_str, " = ", sample_read, "(", x_name, ", Int(", idx_var, " - 1), Int(", x_len, "))"))
      lines <- c(lines, paste0(indent, "      ", pick_done_var, " = True"))
    } else if (isTRUE(replace)) {
      lines <- c(lines, paste0(indent, "  for ", draw_i_var, " in range(1, Int(", size_str, ") + 1):"))
      lines <- c(lines, paste0(indent, "    var ", idx_var, " = Int((_rng_next_f64(__mojor_rng_state) * Float64(Int(", x_len, "))) + 1.0)"))
      lines <- c(lines, paste0(indent, "    if ", idx_var, " > Int(", x_len, "): ", idx_var, " = Int(", x_len, ")"))
      lines <- c(lines, paste0(indent, "    if Int(", draw_i_var, ") == ", pick_pos_var, ":"))
      lines <- c(lines, paste0(indent, "      ", lhs_str, " = ", sample_read, "(", x_name, ", Int(", idx_var, " - 1), Int(", x_len, "))"))
      lines <- c(lines, paste0(indent, "      ", pick_done_var, " = True"))
    } else {
      lines <- c(lines, paste0(indent, "  var ", used_mask, " = alloc_bool(Int(", x_len, "))"))
      lines <- c(lines, paste0(indent, "  for ", draw_i_var, " in range(1, Int(", size_str, ") + 1):"))
      lines <- c(lines, paste0(indent, "    while True:"))
      lines <- c(lines, paste0(indent, "      var ", idx_var, " = Int((_rng_next_f64(__mojor_rng_state) * Float64(Int(", x_len, "))) + 1.0)"))
      lines <- c(lines, paste0(indent, "      if ", idx_var, " > Int(", x_len, "): ", idx_var, " = Int(", x_len, ")"))
      lines <- c(lines, paste0(indent, "      if not ", used_mask, "[Int(", idx_var, " - 1)]:"))
      lines <- c(lines, paste0(indent, "        ", used_mask, "[Int(", idx_var, " - 1)] = True"))
      lines <- c(lines, paste0(indent, "        if Int(", draw_i_var, ") == ", pick_pos_var, ":"))
      lines <- c(lines, paste0(indent, "          ", lhs_str, " = ", sample_read, "(", x_name, ", Int(", idx_var, " - 1), Int(", x_len, "))"))
      lines <- c(lines, paste0(indent, "          ", pick_done_var, " = True"))
      lines <- c(lines, paste0(indent, "        break"))
    }
  }

  lines <- c(lines, paste0(indent, "  if not ", pick_done_var, ":"))
  lines <- c(lines, paste0(indent, "    ", lhs_str, " = ", lhs_na))
  lines
}

# Emit sample_int() as sampling loop
.mojor_ir_sample_int_emit <- function(node, indent = "    ", zero_based_vars = NULL, out_name = NULL, type_env = NULL, loop_var = NULL) {
 # sample_int(n, size, replace) - emit sampling from 1:n

  n <- node$n
  size <- node$size
  replace <- node$replace
  prob_info <- if (!.mojor_ir_sample_is_prob_null(node$prob)) {
    .mojor_ir_sample_parse_prob_var(
      node$prob,
      op_label = "sample.int",
      src = node$src,
      type_env = type_env
    )
  } else {
    NULL
  }
  out_name <- if (!is.null(out_name) && nzchar(out_name)) out_name else "out"

  replace_mode <- .mojor_ir_sample_parse_replace_mode(
    replace,
    op_label = "sample.int",
    src = node$src,
    zero_based_vars = zero_based_vars,
    type_env = type_env,
    loop_var = loop_var
  )

  .mojor_state$needs_mojo_random <- TRUE

  if (is.null(out_name)) {
    return(NULL)
  }

  n_str <- .mojor_ir_expr_emit(n, zero_based_vars, type_env, loop_var)
  size_str <- .mojor_ir_expr_emit(size, zero_based_vars, type_env, loop_var)

  if (is.null(n_str) || is.null(size_str)) {
    return(NULL)
  }

  if (!is.null(prob_info)) {
    used_names <- unique(c(
      if (!is.null(names(type_env))) names(type_env) else character(0),
      if (is.null(loop_var)) character(0) else loop_var,
      if (is.null(zero_based_vars)) character(0) else zero_based_vars,
      out_name
    ))
    prob_total <- .mojor_unique_loop_var("__mojor_prob_total", used_names)
    used_names <- unique(c(used_names, prob_total))
    prob_target <- .mojor_unique_loop_var("__mojor_prob_target", used_names)
    used_names <- unique(c(used_names, prob_target))
    prob_acc <- .mojor_unique_loop_var("__mojor_prob_acc", used_names)
    used_names <- unique(c(used_names, prob_acc))
    prob_j <- .mojor_unique_loop_var("__mojor_prob_j", used_names)
    used_names <- unique(c(used_names, prob_j))
    prob_w <- .mojor_unique_loop_var("__mojor_prob_w", used_names)
    used <- .mojor_unique_loop_var("__mojor_prob_used", used_names)

    weighted_out_line <- paste0(out_name, "[Int(i - 1)] = idx")
    if (isTRUE(replace_mode$is_const)) {
      return(
        if (isTRUE(replace_mode$value)) {
          .mojor_ir_sample_emit_weighted_replace_true(
            indent,
            size_str,
            n_str,
            prob_info,
            prob_total,
            prob_target,
            prob_acc,
            prob_j,
            prob_w,
            weighted_out_line
          )
        } else {
          .mojor_ir_sample_emit_weighted_replace_false(
            indent,
            size_str,
            n_str,
            prob_info,
            prob_total,
            prob_target,
            prob_acc,
            prob_j,
            prob_w,
            used,
            weighted_out_line
          )
        }
      )
    }
    return(c(
      paste0(indent, "if ", replace_mode$expr, ":"),
      .mojor_ir_sample_emit_weighted_replace_true(
        paste0(indent, "  "),
        size_str,
        n_str,
        prob_info,
        prob_total,
        prob_target,
        prob_acc,
        prob_j,
        prob_w,
        weighted_out_line
      ),
      paste0(indent, "else:"),
      .mojor_ir_sample_emit_weighted_replace_false(
        paste0(indent, "  "),
        size_str,
        n_str,
        prob_info,
        prob_total,
        prob_target,
        prob_acc,
        prob_j,
        prob_w,
        used,
        weighted_out_line
      )
    ))
  }

  out_line <- paste0(out_name, "[Int(i - 1)] = idx")
  lines <- character(0)
  if (isTRUE(replace_mode$is_const)) {
    lines <- c(
      lines,
      if (isTRUE(replace_mode$value)) {
        .mojor_ir_sample_emit_replace_true(indent, size_str, n_str, out_line)
      } else {
        .mojor_ir_sample_emit_replace_false(indent, size_str, n_str, "used", out_line)
      }
    )
  } else {
    lines <- c(
      lines,
      paste0(indent, "if ", replace_mode$expr, ":"),
      .mojor_ir_sample_emit_replace_true(paste0(indent, "  "), size_str, n_str, out_line),
      paste0(indent, "else:"),
      .mojor_ir_sample_emit_replace_false(paste0(indent, "  "), size_str, n_str, "used", out_line)
    )
  }

  return(lines)
}

# Emit sample() as sampling from vector
.mojor_ir_sample_emit <- function(node, indent = "    ", zero_based_vars = NULL, out_name = NULL, type_env = NULL, loop_var = NULL) {
 # sample(x, size, replace) - emit sampling from vector x

  x <- node$x
  size <- node$size
  replace <- node$replace
  prob_info <- if (!.mojor_ir_sample_is_prob_null(node$prob)) {
    .mojor_ir_sample_parse_prob_var(
      node$prob,
      op_label = "sample",
      src = node$src,
      type_env = type_env
    )
  } else {
    NULL
  }
  out_name <- if (!is.null(out_name) && nzchar(out_name)) out_name else "out"

  replace_mode <- .mojor_ir_sample_parse_replace_mode(
    replace,
    op_label = "sample",
    src = node$src,
    zero_based_vars = zero_based_vars,
    type_env = type_env,
    loop_var = loop_var
  )

 # For now, only support direct variable references
  if (x$kind != "var") {
    return(NULL)
  }
  x_name <- x$name

 # Get size
  size_str <- .mojor_ir_expr_emit(size, zero_based_vars, type_env, loop_var)
  if (is.null(size_str)) {
    return(NULL)
  }

 # Get length of x from type_env or metadata
  len_var_map <- .mojor_state$current_len_var_map
  x_len <- if (!is.null(len_var_map) && !is.null(len_var_map[[x_name]])) len_var_map[[x_name]] else "len_x"
  x_type <- if (!is.null(type_env)) type_env[[x_name]] else NULL
  sample_read <- if (!is.null(x_type) && x_type %in% c("i32[]", "i32")) {
    "_mojor_read_i32"
  } else if (!is.null(x_type) && x_type %in% c("f32[]", "f32")) {
    "_mojor_read_f32"
  } else {
    "_mojor_read_f64"
  }

  .mojor_state$needs_mojo_random <- TRUE

  if (!is.null(prob_info)) {
    used_names <- unique(c(
      if (!is.null(names(type_env))) names(type_env) else character(0),
      if (is.null(loop_var)) character(0) else loop_var,
      if (is.null(zero_based_vars)) character(0) else zero_based_vars,
      out_name,
      x_name
    ))
    prob_total <- .mojor_unique_loop_var("__mojor_prob_total", used_names)
    used_names <- unique(c(used_names, prob_total))
    prob_target <- .mojor_unique_loop_var("__mojor_prob_target", used_names)
    used_names <- unique(c(used_names, prob_target))
    prob_acc <- .mojor_unique_loop_var("__mojor_prob_acc", used_names)
    used_names <- unique(c(used_names, prob_acc))
    prob_j <- .mojor_unique_loop_var("__mojor_prob_j", used_names)
    used_names <- unique(c(used_names, prob_j))
    prob_w <- .mojor_unique_loop_var("__mojor_prob_w", used_names)
    used <- .mojor_unique_loop_var("__mojor_prob_used", used_names)

    weighted_out_line <- paste0(out_name, "[Int(i - 1)] = ", sample_read, "(", x_name, ", Int(idx - 1), Int(", x_len, "))")
    if (isTRUE(replace_mode$is_const)) {
      return(
        if (isTRUE(replace_mode$value)) {
          .mojor_ir_sample_emit_weighted_replace_true(
            indent,
            size_str,
            x_len,
            prob_info,
            prob_total,
            prob_target,
            prob_acc,
            prob_j,
            prob_w,
            weighted_out_line
          )
        } else {
          .mojor_ir_sample_emit_weighted_replace_false(
            indent,
            size_str,
            x_len,
            prob_info,
            prob_total,
            prob_target,
            prob_acc,
            prob_j,
            prob_w,
            used,
            weighted_out_line
          )
        }
      )
    }
    return(c(
      paste0(indent, "if ", replace_mode$expr, ":"),
      .mojor_ir_sample_emit_weighted_replace_true(
        paste0(indent, "  "),
        size_str,
        x_len,
        prob_info,
        prob_total,
        prob_target,
        prob_acc,
        prob_j,
        prob_w,
        weighted_out_line
      ),
      paste0(indent, "else:"),
      .mojor_ir_sample_emit_weighted_replace_false(
        paste0(indent, "  "),
        size_str,
        x_len,
        prob_info,
        prob_total,
        prob_target,
        prob_acc,
        prob_j,
        prob_w,
        used,
        weighted_out_line
      )
    ))
  }

 # Emit similar to sample_int but with array indexing
  out_line <- paste0(out_name, "[Int(i - 1)] = ", sample_read, "(", x_name, ", Int(idx - 1), ", x_len, ")")
  lines <- character(0)
  if (isTRUE(replace_mode$is_const)) {
    lines <- c(
      lines,
      if (isTRUE(replace_mode$value)) {
        .mojor_ir_sample_emit_replace_true(indent, size_str, x_len, out_line)
      } else {
        .mojor_ir_sample_emit_replace_false(indent, size_str, x_len, "used", out_line)
      }
    )
  } else {
    lines <- c(
      lines,
      paste0(indent, "if ", replace_mode$expr, ":"),
      .mojor_ir_sample_emit_replace_true(paste0(indent, "  "), size_str, x_len, out_line),
      paste0(indent, "else:"),
      .mojor_ir_sample_emit_replace_false(paste0(indent, "  "), size_str, x_len, "used", out_line)
    )
  }

  return(lines)
}

# Emit whole-array higher-order function calls as explicit loops.
.mojor_ir_tier8_hof_assign_emit <- function(node, out_name, type_env = NULL, indent = "    ", zero_based_vars = NULL, allow_constructor_returns = FALSE, known_scalar_captures = NULL) {
  if (is.null(node) || is.null(node$kind)) {
    return(NULL)
  }
  if (!(node$kind %in% c("vapply", "sapply", "lapply", "mapply"))) {
    return(NULL)
  }
  if (is.null(out_name) || !nzchar(out_name)) {
    return(NULL)
  }
  if (is.null(type_env)) {
    return(NULL)
  }

  ensure_vec_type <- function(var_name, op_name) {
    if (is.null(var_name) || !nzchar(var_name)) {
      stop("IR emit [", op_name, "]: argument must be a variable")
    }
    t <- type_env[[var_name]]
    if (is.null(t) || !t %in% c("f64[]", "i32[]", "lgl[]")) {
      stop(
        "IR emit [", op_name, "]: argument '", var_name,
        "' must be numeric/integer/logical vector in the compiled subset"
      )
    }
    t
  }
  allowed_hof_named_unary_funs <- c(
    "sin", "cos", "tan", "asin", "acos", "atan",
    "sinh", "cosh", "tanh", "asinh", "acosh", "atanh",
    "log", "log10", "log1p", "log2", "exp", "expm1",
    "sqrt", "abs", "floor", "ceiling", "trunc", "round",
    "sign", "cbrt", "lgamma", "erf", "gamma"
  )
  allowed_hof_named_multiary_funs <- c("pmin", "pmax", "min", "max")

  parse_fun <- function(fun_expr, op_name, expected_arity) {
    if (is.name(fun_expr)) {
      fun_name <- as.character(fun_expr)
      if (expected_arity == 1L) {
        if (!(fun_name %in% allowed_hof_named_unary_funs)) {
          stop(
            "IR emit [", op_name, "]: FUN named function '", fun_name,
            "' is unsupported; use an anonymous function or one of: ",
            paste(allowed_hof_named_unary_funs, collapse = ", ")
          )
        }
      } else if (expected_arity %in% c(2L, 3L)) {
        if (!(fun_name %in% allowed_hof_named_multiary_funs)) {
          stop(
            "IR emit [", op_name, "]: FUN named function '", fun_name,
            "' is unsupported for arity ", expected_arity,
            "; use an anonymous function or one of: ",
            paste(allowed_hof_named_multiary_funs, collapse = ", ")
          )
        }
      } else {
        stop("IR emit [", op_name, "]: named FUN is unsupported for arity ", expected_arity)
      }
      param_names <- paste0("__mojor_hof_arg", seq_len(expected_arity))
      body_args <- lapply(param_names, function(nm) .mojor_ir_var(nm))
      return(list(
        param_names = param_names,
        body_ir = .mojor_ir_call(fun_name, body_args),
        return_parts_ir = NULL,
        return_arity = 1L
      ))
    }

    if (!(is.call(fun_expr) && identical(as.character(fun_expr[[1]]), "function"))) {
      stop("IR emit [", op_name, "]: FUN must be an anonymous function or supported unary function name")
    }
    fmls <- fun_expr[[2]]
    param_names <- names(fmls)
    if (is.null(param_names) || length(param_names) != expected_arity || any(!nzchar(param_names))) {
      stop("IR emit [", op_name, "]: FUN arity must match vector arguments")
    }

    blocks <- .mojor_extract_block(fun_expr[[3]])
    if (length(blocks) == 1) {
      body_expr <- blocks[[1]]
    } else {
      body_expr <- .mojor_inline_let_bindings(blocks)
      if (is.null(body_expr)) {
        stop("IR emit [", op_name, "]: FUN must have a single-expression or let-binding body")
      }
    }
    if (is.call(body_expr) && identical(as.character(body_expr[[1]]), "return") && length(body_expr) >= 2) {
      body_expr <- body_expr[[2]]
    }
    captures <- setdiff(all.vars(body_expr, functions = FALSE, unique = TRUE), param_names)
    if (!is.null(known_scalar_captures) && length(known_scalar_captures) > 0L) {
      captures <- setdiff(captures, known_scalar_captures)
    }
    if (length(captures) > 0) {
      stop(
        "IR emit [", op_name, "]: FUN closures/non-inlineable captures are unsupported: ",
        paste(captures, collapse = ", ")
      )
    }

    body_ir <- .mojor_ir_expr_build(body_expr)
    if (is.null(body_ir)) {
      stop("IR emit [", op_name, "]: FUN body is not inlineable")
    }
    if (identical(body_ir$kind, "list")) {
      stop("IR emit [", op_name, "]: FUN list(...) returns are unsupported in the compiled subset")
    }
    if (body_ir$kind %in% c("rep", "rep_len", "vapply", "sapply", "lapply", "mapply")) {
      stop("IR emit [", op_name, "]: FUN must return a scalar value")
    }
    if (identical(body_ir$kind, "c")) {
      if (!isTRUE(allow_constructor_returns)) {
        stop("IR emit [", op_name, "]: FUN must return a scalar value")
      }
      if (is.null(body_ir$parts) || !is.list(body_ir$parts) || length(body_ir$parts) < 1L) {
        stop("IR emit [", op_name, "]: FUN c(...) return must contain at least one scalar expression")
      }
      bad_part <- vapply(body_ir$parts, function(part) {
        if (is.null(part) || is.null(part$kind)) {
          return(TRUE)
        }
        part$kind %in% c("c", "list", "rep", "rep_len", "vapply", "sapply", "lapply", "mapply")
      }, logical(1))
      if (any(bad_part)) {
        stop("IR emit [", op_name, "]: FUN c(...) return values must contain only scalar expressions")
      }
      return(list(
        param_names = param_names,
        body_ir = body_ir,
        return_parts_ir = body_ir$parts,
        return_arity = as.integer(length(body_ir$parts))
      ))
    }

    list(param_names = param_names, body_ir = body_ir, return_parts_ir = NULL, return_arity = 1L)
  }

  len_var_map <- .mojor_state$current_len_var_map
  const_array_vars <- .mojor_state$current_const_array_vars
  len_of <- function(name) .mojor_ir_emit_len_lookup(name, len_var_map, const_array_vars, default = "n_i")

  arg_names <- character(0)
  arg_types <- character(0)
  fun_expr <- NULL
  fun_info <- NULL

  if (node$kind %in% c("vapply", "sapply", "lapply")) {
    if (is.null(node$x) || node$x$kind != "var") {
      stop("IR emit [", node$kind, "]: argument must be a direct vector variable")
    }
    arg_names <- c(node$x$name)
    arg_types <- c(ensure_vec_type(node$x$name, node$kind))
    fun_expr <- node$fun
    fun_info <- parse_fun(fun_expr, node$kind, expected_arity = 1L)
    if (node$kind == "vapply") {
      valid_fun_value <- !is.null(node$fun_value_type) &&
        node$fun_value_type %in% c("f64", "i32", "lgl", "bool", "numeric", "double", "integer", "logical")
      if (!valid_fun_value) {
        stop("IR emit [vapply]: FUN.VALUE type must be numeric(1), integer(1), or logical(1)")
      }
    }
  } else {
    if (is.null(node$args) || !is.list(node$args)) {
      stop("IR emit [mapply]: args must be a list of vector variables")
    }
    if (length(node$args) < 2) {
      stop("IR emit [mapply]: requires at least two vector arguments")
    }
    if (!all(vapply(node$args, function(a) !is.null(a$kind) && a$kind == "var", logical(1)))) {
      stop("IR emit [mapply]: arguments must be direct vector variables")
    }
    arg_names <- vapply(node$args, function(a) a$name, character(1))
    arg_types <- vapply(arg_names, ensure_vec_type, character(1), op_name = "mapply")
    fun_expr <- node$fun
    fun_info <- parse_fun(fun_expr, "mapply", expected_arity = length(arg_names))
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
  param_scalar_types <- vapply(arg_types, vector_elem_type, character(1))
  if (length(param_scalar_types) != length(fun_info$param_names) || any(!nzchar(param_scalar_types))) {
    stop("IR emit [", node$kind, "]: unsupported vector argument type")
  }
  param_env <- as.list(param_scalar_types)
  names(param_env) <- fun_info$param_names

  out_type <- if (node$kind == "vapply") {
    if (node$fun_value_type %in% c("f64", "numeric", "double")) {
      "f64[]"
    } else if (node$fun_value_type %in% c("i32", "integer")) {
      "i32[]"
    } else if (node$fun_value_type %in% c("lgl", "bool", "logical")) {
      "lgl[]"
    } else {
      stop("IR emit [vapply]: unsupported FUN.VALUE type in the compiled subset")
    }
  } else {
    inferred_vec <- NULL
    if (!is.null(fun_info$return_parts_ir) && is.list(fun_info$return_parts_ir) && length(fun_info$return_parts_ir) > 0L) {
      part_types <- vapply(fun_info$return_parts_ir, function(part) .mojor_ir_infer_type(part, param_env), character(1))
      if (!any(.mojor_type_is_array(part_types))) {
        inferred_vec <- elem_vector_type(promote_many(part_types))
      }
    } else {
      inferred <- .mojor_ir_infer_type(fun_info$body_ir, param_env)
      inferred_vec <- elem_vector_type(inferred)
    }
    if (!is.null(inferred_vec)) {
      inferred_vec
    } else if (any(arg_types == "f64[]")) {
      "f64[]"
    } else if (any(arg_types == "i32[]")) {
      "i32[]"
    } else if (any(arg_types %in% c("lgl[]", "bool[]"))) {
      "lgl[]"
    } else {
      arg_types[[1]]
    }
  }
  out_cast <- if (out_type == "f64[]") "Float64" else "Int32"

  param_emit_types <- param_scalar_types
  if (node$kind == "mapply" && any(arg_types == "f64[]") && any(arg_types != "f64[]")) {
    for (i in seq_along(arg_types)) {
      if (!identical(arg_types[[i]], "f64[]")) {
        param_emit_types[[i]] <- "f64"
      }
    }
  }
  param_emit_env <- as.list(param_emit_types)
  names(param_emit_env) <- fun_info$param_names

  cast_param_value <- function(expr, scalar_type) {
    if (identical(scalar_type, "f64")) {
      return(paste0("Float64(", expr, ")"))
    }
    if (identical(scalar_type, "f32")) {
      return(paste0("Float32(", expr, ")"))
    }
    if (scalar_type %in% c("i32", "lgl", "bool")) {
      return(paste0("Int32(", expr, ")"))
    }
    expr
  }

  loop_var <- .mojor_unique_loop_var("__mojor_hof_i", c(out_name, arg_names, names(type_env), if (is.null(zero_based_vars)) character(0) else zero_based_vars))

  expr_env <- type_env
  for (nm in names(param_emit_env)) expr_env[[nm]] <- param_emit_env[[nm]]
  zvars <- unique(c(if (is.null(zero_based_vars)) character(0) else zero_based_vars, loop_var))

  return_arity <- if (!is.null(fun_info$return_arity)) suppressWarnings(as.integer(fun_info$return_arity)) else 1L
  if (is.na(return_arity) || return_arity < 1L) {
    stop("IR emit [", node$kind, "]: FUN return arity must be a positive integer")
  }
  return_parts_ir <- if (!is.null(fun_info$return_parts_ir) && is.list(fun_info$return_parts_ir) && length(fun_info$return_parts_ir) > 0L) {
    fun_info$return_parts_ir
  } else {
    NULL
  }

  base_len <- len_of(arg_names[[1]])
  lines <- c(paste0(indent, "for ", loop_var, " in range(Int(", base_len, ")):"))
  for (i in seq_along(fun_info$param_names)) {
    rhs_val <- cast_param_value(
      paste0(arg_names[[i]], "[", loop_var, "]"),
      param_emit_types[[i]]
    )
    lines <- c(lines, paste0(indent, "    var ", fun_info$param_names[[i]], " = ", rhs_val))
  }
  if (is.null(return_parts_ir)) {
    rhs_expr <- .mojor_ir_expr_emit(fun_info$body_ir, zero_based_vars = zvars, type_env = expr_env, loop_vars = loop_var)
    if (is.null(rhs_expr) || !nzchar(rhs_expr)) {
      stop("IR emit [", node$kind, "]: FUN body could not be emitted")
    }
    lines <- c(lines, paste0(indent, "    ", out_name, "[", loop_var, "] = ", out_cast, "(", rhs_expr, ")"))
  } else {
    for (part_i in seq_along(return_parts_ir)) {
      rhs_part <- .mojor_ir_expr_emit(return_parts_ir[[part_i]], zero_based_vars = zvars, type_env = expr_env, loop_vars = loop_var)
      if (is.null(rhs_part) || !nzchar(rhs_part)) {
        stop("IR emit [", node$kind, "]: FUN return part ", part_i, " could not be emitted")
      }
      out_idx <- paste0("(", loop_var, " * ", return_arity, " + ", as.integer(part_i - 1L), ")")
      lines <- c(lines, paste0(indent, "    ", out_name, "[", out_idx, "] = ", out_cast, "(", rhs_part, ")"))
    }
  }
  lines
}

# IR-native whole-vector string assign emit for nchar/nzchar/substr/paste/paste0.
.mojor_ir_tier8_string_assign_emit <- function(node, out_name, type_env = NULL, indent = "    ", zero_based_vars = NULL) {
  if (is.null(node) || is.null(node$kind) || !(node$kind %in% c("nchar", "nzchar", "substr", "paste"))) {
    return(NULL)
  }
  if (is.null(out_name) || !nzchar(out_name)) {
    return(NULL)
  }
  len_var_map <- .mojor_state$current_len_var_map
  len_of <- function(name) {
    if (!is.null(len_var_map) && !is.null(len_var_map[[name]])) {
      return(len_var_map[[name]])
    }
    paste0("Int(len(", name, "))")
  }

  type_names <- if (is.null(type_env)) character(0) else names(type_env)

  if (identical(node$kind, "paste")) {
    if (is.null(node$args) || !is.list(node$args) || length(node$args) < 1L) {
      return(NULL)
    }

    has_collapse <- !is.null(node$collapse) &&
      !(is.character(node$collapse) && length(node$collapse) == 1 && identical(node$collapse, "NULL"))
    if (isTRUE(has_collapse) &&
      !(is.character(node$collapse) && length(node$collapse) == 1 && !is.na(node$collapse))) {
      return(NULL)
    }

    sep <- node$sep
    if (!is.character(sep) || length(sep) != 1) {
      return(NULL)
    }

    arg_names <- vapply(node$args, function(arg) {
      if (is.null(arg) || !is.list(arg) || !identical(arg$kind, "var") || is.null(arg$name) || !nzchar(arg$name)) {
        return(NA_character_)
      }
      arg$name
    }, character(1))
    if (anyNA(arg_names)) {
      return(NULL)
    }

    if (!is.null(type_env)) {
      arg_types <- vapply(arg_names, function(nm) {
        val <- type_env[[nm]]
        if (is.null(val)) "unknown" else val
      }, character(1))
      if (any(!(arg_types %in% c("chr[]", "unknown")))) {
        return(NULL)
      }
    }

    arg_lens <- vapply(arg_names, len_of, character(1))
    max_len <- arg_lens[[1]]
    if (length(arg_lens) > 1L) {
      for (i in 2:length(arg_lens)) {
        max_len <- paste0("max(Int(", max_len, "), Int(", arg_lens[[i]], "))")
      }
    }
    zero_len_pred <- paste(
      vapply(arg_lens, function(le) paste0("Int(", le, ") == 0"), character(1)),
      collapse = " or "
    )
    derived_out_len <- paste0("(0 if ", zero_len_pred, " else Int(", max_len, "))")

    out_type <- if (!is.null(type_env)) type_env[[out_name]] else NULL
    allowed_out_types <- if (isTRUE(has_collapse)) c("chr", "chr[]", "unknown") else c("chr[]", "unknown")
    if (!is.null(out_type) && !out_type %in% allowed_out_types) {
      return(NULL)
    }

    out_len <- if (isTRUE(has_collapse)) {
      derived_out_len
    } else if (!is.null(len_var_map) && !is.null(len_var_map[[out_name]])) {
      len_var_map[[out_name]]
    } else {
      derived_out_len
    }

    loop_var <- .mojor_unique_loop_var(
      "__mojor_str_i",
      c(out_name, arg_names, type_names, if (is.null(zero_based_vars)) character(0) else zero_based_vars)
    )
    arg_exprs <- vapply(seq_along(arg_names), function(i) {
      nm <- arg_names[[i]]
      le <- arg_lens[[i]]
      paste0(nm, "[Int(", loop_var, " % Int(", le, "))]")
    }, character(1))

    rhs_expr <- arg_exprs[[1]]
    if (length(arg_exprs) > 1) {
      for (i in 2:length(arg_exprs)) {
        rhs_expr <- paste0("(", rhs_expr, " + ", sep, " + ", arg_exprs[[i]], ")")
      }
    }

    if (isTRUE(has_collapse)) {
      collapse <- node$collapse[[1]]
      assign_target <- if (!is.null(out_type) && identical(out_type, "chr[]")) paste0(out_name, "[Int(0)]") else out_name
      acc_var <- .mojor_unique_loop_var(
        "__mojor_str_acc",
        c(out_name, arg_names, type_names, if (is.null(zero_based_vars)) character(0) else zero_based_vars)
      )
      return(c(
        paste0(indent, "var ", acc_var, " = \"\""),
        paste0(indent, "for ", loop_var, " in range(Int(", out_len, ")):"),
        paste0(indent, "    if ", loop_var, " > 0:"),
        paste0(indent, "      ", acc_var, " = ", acc_var, " + ", collapse),
        paste0(indent, "    ", acc_var, " = ", acc_var, " + ", rhs_expr),
        paste0(indent, assign_target, " = ", acc_var)
      ))
    }

    return(c(
      paste0(indent, "for ", loop_var, " in range(Int(", out_len, ")):"),
      paste0(indent, "    ", out_name, "[", loop_var, "] = ", rhs_expr)
    ))
  }

  if (is.null(node$x) || node$x$kind != "var") {
    return(NULL)
  }
  x_name <- node$x$name
  x_type <- if (!is.null(type_env)) type_env[[x_name]] else NULL
  if (!is.null(x_type) && !identical(x_type, "chr[]")) {
    return(NULL)
  }

  out_type <- if (!is.null(type_env)) type_env[[out_name]] else NULL
  allowed_out <- if (identical(node$kind, "substr")) c("chr[]", "unknown") else c("i32[]", "lgl[]", "bool[]", "unknown")
  if (!is.null(out_type) && !out_type %in% allowed_out) {
    return(NULL)
  }

  x_len <- len_of(x_name)
  loop_var <- .mojor_unique_loop_var(
    "__mojor_str_i",
    c(out_name, x_name, type_names, if (is.null(zero_based_vars)) character(0) else zero_based_vars)
  )
  elem_expr <- paste0(x_name, "[", loop_var, "]")
  rhs_expr <- if (identical(node$kind, "nchar")) {
    paste0("Int32(len(", elem_expr, "))")
  } else if (identical(node$kind, "nzchar")) {
    paste0("Int32(1 if len(", elem_expr, ") > 0 else 0)")
  } else {
    start_str <- .mojor_ir_expr_emit(node$start, zero_based_vars = zero_based_vars, type_env = type_env, loop_vars = NULL, index_context = TRUE)
    stop_str <- .mojor_ir_expr_emit(node$stop, zero_based_vars = zero_based_vars, type_env = type_env, loop_vars = NULL, index_context = TRUE)
    if (is.null(start_str) || !nzchar(start_str) || is.null(stop_str) || !nzchar(stop_str)) {
      return(NULL)
    }
    paste0(elem_expr, "[", start_str, ":", stop_str, "]")
  }

  c(
    paste0(indent, "for ", loop_var, " in range(Int(", x_len, ")):"),
    paste0(indent, "    ", out_name, "[", loop_var, "] = ", rhs_expr)
  )
}

.mojor_ir_tier7_type_suffix <- function(x_type, strict = FALSE) {
  if (!is.null(x_type) && x_type %in% c("i32[]", "i32")) {
    return("i32")
  }
  if (!is.null(x_type) && x_type %in% c("f32[]", "f32")) {
    return("f32")
  }
  if (isTRUE(strict) && !is.null(x_type) && !x_type %in% c("f64[]", "f64", "unknown")) {
    return(NULL)
  }
  "f64"
}

.mojor_ir_emit_len_lookup <- function(name, len_var_map = NULL, const_array_vars = NULL, default = "n_i") {
  if (!is.null(len_var_map) && !is.null(len_var_map[[name]])) {
    return(len_var_map[[name]])
  }
  if (!is.null(const_array_vars) && isTRUE(const_array_vars[[name]])) {
    return(paste0("Int(len(", name, "))"))
  }
  default
}

.mojor_ir_emit_ptr_lookup <- function(name, const_array_vars = NULL) {
  name
}

.mojor_ir_tier7_set_match_call <- function(kind, type_suffix, x_ptr, x_len, out_name = NULL, table_ptr = NULL, table_len = NULL, indent = "") {
  if (kind %in% c("unique", "duplicated")) {
    if (is.null(out_name) || !nzchar(out_name)) {
      return(NULL)
    }
    return(paste0(indent, "mojor_", kind, "_", type_suffix, "(", x_ptr, ", ", x_len, ", ", out_name, ")"))
  }
  if (kind == "any_duplicated") {
    return(paste0(indent, "mojor_any_duplicated_", type_suffix, "(", x_ptr, ", ", x_len, ")"))
  }
  if (!(kind %in% c("match", "in")) || is.null(out_name) || !nzchar(out_name) || is.null(table_ptr) || is.null(table_len)) {
    return(NULL)
  }
  paste0(indent, "mojor_", kind, "_", type_suffix, "(", x_ptr, ", ", x_len, ", ", table_ptr, ", ", table_len, ", ", out_name, ")")
}

# Emit whole-array set/match + quantile calls that fill output buffers.
.mojor_ir_tier7_vector_call_emit <- function(node, out_name, type_env = NULL, indent = "    ") {
  if (is.null(node) || is.null(node$kind)) {
    return(NULL)
  }
  if (is.null(out_name) || !nzchar(out_name)) {
    return(NULL)
  }

  len_var_map <- .mojor_state$current_len_var_map
  const_array_vars <- .mojor_state$current_const_array_vars
  len_of <- function(name) .mojor_ir_emit_len_lookup(name, len_var_map, const_array_vars, default = "n_i")
  ptr_of <- function(name) .mojor_ir_emit_ptr_lookup(name, const_array_vars)

  x_node <- node$x
  if (is.null(x_node) || x_node$kind != "var") {
    return(NULL)
  }
  x_name <- x_node$name
  x_ptr <- ptr_of(x_name)
  x_len <- len_of(x_name)
  x_type <- if (!is.null(type_env)) type_env[[x_name]] else NULL
  type_suffix <- .mojor_ir_tier7_type_suffix(x_type, strict = TRUE)
  if (is.null(type_suffix)) {
    return(NULL)
  }

  if (node$kind %in% c("unique", "duplicated", "match", "in")) {
    .mojor_state$needs_set_match <- TRUE
  }
  if (node$kind == "quantile") {
    .mojor_state$needs_quantile <- TRUE
  }

  if (node$kind %in% c("unique", "duplicated")) {
    return(.mojor_ir_tier7_set_match_call(node$kind, type_suffix, x_ptr, x_len, out_name = out_name, indent = indent))
  }
  if (node$kind %in% c("match", "in")) {
    table_node <- node$table
    if (is.null(table_node) || table_node$kind != "var") {
      return(NULL)
    }
    table_name <- table_node$name
    table_ptr <- ptr_of(table_name)
    table_len <- len_of(table_name)
    if (!is.null(type_env)) {
      table_type <- type_env[[table_name]]
      if (!is.null(x_type) && !is.null(table_type) &&
        sub("\\[.*\\]$", "", as.character(x_type[[1]])) != sub("\\[.*\\]$", "", as.character(table_type[[1]]))) {
        return(NULL)
      }
    }
    return(.mojor_ir_tier7_set_match_call(node$kind, type_suffix, x_ptr, x_len,
      out_name = out_name, table_ptr = table_ptr, table_len = table_len, indent = indent
    ))
  }
  if (node$kind == "quantile") {
    probs_node <- node$probs
    if (is.null(probs_node) || probs_node$kind != "var") {
      return(NULL)
    }
    probs_name <- probs_node$name
    probs_ptr <- ptr_of(probs_name)
    probs_len <- len_of(probs_name)
    if (!is.null(type_env)) {
      probs_type <- type_env[[probs_name]]
      if (!is.null(probs_type) && !probs_type %in% c("f64[]", "f64", "unknown")) {
        return(NULL)
      }
    }
    type_str <- if (!is.null(node$type)) .mojor_ir_expr_emit(node$type, NULL, type_env, loop_vars = NULL) else "7"
    na_rm_str <- if (!is.null(node$na_rm)) .mojor_ir_expr_emit(node$na_rm, NULL, type_env, loop_vars = NULL) else "False"
    if (type_str == "" || is.null(type_str) || na_rm_str == "" || is.null(na_rm_str)) {
      return(NULL)
    }
    return(paste0(indent, "mojor_quantile_", type_suffix, "(", x_ptr, ", ", x_len, ", ", probs_ptr, ", ", probs_len, ", ", type_str, ", ", na_rm_str, ", ", out_name, ")"))
  }

  NULL
}
