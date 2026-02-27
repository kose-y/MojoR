.mojor_ir_expr_emit <- function(node, zero_based_vars = NULL, type_env = NULL, loop_vars = NULL, index_context = FALSE) {
 # Step 5.1: Added type_env parameter for logical array support
  if (is.null(node) || is.null(node$kind)) {
    return(NULL)
  }
  .mojor_ir_sample_parse_replace_literal <- function(replace_node, op_label, src) {
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
      return(list(is_const = TRUE, value = FALSE, expr = "False"))
    }
    if (is.list(replace_node) && !is.null(replace_node$kind)) {
      if (identical(replace_node$kind, "const")) {
        lit <- parse_bool_literal(replace_node$value)
        if (is.null(lit)) fail()
        return(list(is_const = TRUE, value = lit, expr = if (isTRUE(lit)) "True" else "False"))
      }
      replace_expr <- .mojor_ir_expr_emit(replace_node, zero_based_vars, type_env, loop_vars, index_context = TRUE)
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
    list(is_const = TRUE, value = lit, expr = if (isTRUE(lit)) "True" else "False")
  }
  .mojor_ir_sample_scalar_emit <- function(sample_node, indexed_context = FALSE, requested_index = NULL) {
    if (is.null(sample_node) || !(sample_node$kind %in% c("sample_int", "sample"))) {
      return(NULL)
    }
    op_label <- if (identical(sample_node$kind, "sample_int")) "sample.int" else "sample"

    required_index <- 1L
    if (!is.null(requested_index)) {
      requested_num <- suppressWarnings(as.numeric(requested_index))
      if (length(requested_num) != 1 || !is.finite(requested_num) || requested_num != floor(requested_num) || requested_num < 1) {
        .mojor_err(
          paste0(op_label, "() indexed scalar emission requires a positive integer index in this release"),
          sample_node$src,
          paste0("Use ", op_label, "(..., size = k)[j] with integer j >= 1")
        )
      }
      required_index <- as.integer(requested_num)
    }

    size_str <- .mojor_ir_expr_emit(sample_node$size, zero_based_vars, type_env, loop_vars, index_context = TRUE)
    if (is.null(size_str) || !nzchar(size_str)) {
      return(NULL)
    }
    size_val <- suppressWarnings(as.numeric(.mojor_ir_eval_const(sample_node$size)))
    if (length(size_val) == 1 && is.finite(size_val) && size_val == floor(size_val)) {
      if (size_val < 1) {
        .mojor_err(
          paste0(op_label, "() scalar emission requires size >= 1 in this release"),
          sample_node$src,
          paste0("Use ", op_label, "(..., size = k) with integer k >= 1")
        )
      }
      if (size_val < required_index) {
        .mojor_err(
          paste0(op_label, "() indexed scalar emission requires size >= requested index in this release"),
          sample_node$src,
          paste0("Use ", op_label, "(..., size = k)[j] where k >= j")
        )
      }
    }
    if (!is.null(sample_node$prob)) {
      .mojor_err(
        paste0(op_label, "() weighted sampling via prob is not supported in this release"),
        sample_node$src,
        "Use uniform sampling with prob = NULL"
      )
    }

    .mojor_ir_sample_parse_replace_literal(sample_node$replace, op_label, sample_node$src)
    .mojor_state$needs_mojo_random <- TRUE

    if (identical(sample_node$kind, "sample_int")) {
      n_str <- .mojor_ir_expr_emit(sample_node$n, zero_based_vars, type_env, loop_vars, index_context = TRUE)
      if (is.null(n_str) || !nzchar(n_str)) {
        return(NULL)
      }
      draw_expr <- paste0("Int((_rng_next_f64(__mojor_rng_state) * Float64(Int(", n_str, "))) + 1.0)")
      return(paste0("min(", draw_expr, ", Int(", n_str, "))"))
    }

    if (is.null(sample_node$x) || sample_node$x$kind != "var") {
      .mojor_err(
        if (isTRUE(indexed_context)) {
          "sample() indexed scalar emission requires x to be a direct vector variable in this release"
        } else {
          "sample() scalar emission requires x to be a direct vector variable in this release"
        },
        sample_node$src,
        if (isTRUE(indexed_context)) {
          "Use sample(x, size = 1)[1] with direct vector argument x"
        } else {
          "Use sample(x, size = 1) with direct vector argument x"
        }
      )
    }

    x_name <- sample_node$x$name
    x_len <- .mojor_ir_resolve_var_len(x_name)
    x_type <- if (!is.null(type_env)) type_env[[x_name]] else NULL
    sample_read <- if (!is.null(x_type) && x_type %in% c("i32[]", "i32", "lgl[]", "lgl", "bool[]", "bool")) {
      "_mojor_read_i32"
    } else if (!is.null(x_type) && x_type %in% c("f32[]", "f32")) {
      "_mojor_read_f32"
    } else {
      "_mojor_read_f64"
    }

    draw_expr <- paste0("min(Int((_rng_next_f64(__mojor_rng_state) * ", x_len, ") + 1), Int(", x_len, "))")
    .mojor_ir_read_call(sample_read, x_name, paste0("Int(", draw_expr, " - 1)"), paste0("Int(", x_len, ")"))
  }
  .mojor_ir_sample_index_dynamic_emit <- function(index_node, idx_one_based_expr) {
    sample_node <- index_node$base
    op_label <- if (identical(sample_node$kind, "sample_int")) "sample.int" else "sample"

    if (!is.null(sample_node$prob)) {
      .mojor_err(
        paste0(op_label, "() weighted sampling via prob is not supported in this release"),
        sample_node$src,
        "Use uniform sampling with prob = NULL"
      )
    }
    replace_mode <- .mojor_ir_sample_parse_replace_literal(sample_node$replace, op_label, sample_node$src)
    size_str <- .mojor_ir_expr_emit(sample_node$size, zero_based_vars, type_env, loop_vars, index_context = TRUE)
    if (is.null(size_str) || !nzchar(size_str)) {
      return(NULL)
    }

    .mojor_state$needs_mojo_random <- TRUE
    replace_str <- replace_mode$expr
    idx_guard <- paste0("(Int(", idx_one_based_expr, ") >= 1 and Int(", idx_one_based_expr, ") <= Int(", size_str, "))")

    if (identical(sample_node$kind, "sample_int")) {
      n_str <- .mojor_ir_expr_emit(sample_node$n, zero_based_vars, type_env, loop_vars, index_context = TRUE)
      if (is.null(n_str) || !nzchar(n_str)) {
        return(NULL)
      }
      helper_call <- paste0(
        "_mojor_sample_pick_index(__mojor_rng_state, Int(", n_str, "), Int(", size_str, "), Int(", idx_one_based_expr, "), ", replace_str, ")"
      )
      return(paste0("(Int(-2147483648) if not ", idx_guard, " else Int(", helper_call, "))"))
    }

    if (is.null(sample_node$x) || sample_node$x$kind != "var") {
      .mojor_err(
        "sample() indexed dynamic emission requires x to be a direct vector variable in this release",
        sample_node$src,
        "Use sample(x, ...)[i] with direct vector argument x"
      )
    }

    x_name <- sample_node$x$name
    x_len <- .mojor_ir_resolve_var_len(x_name)
    x_type <- if (!is.null(type_env)) type_env[[x_name]] else NULL
    sample_read <- if (!is.null(x_type) && x_type %in% c("i32[]", "i32", "lgl[]", "lgl", "bool[]", "bool")) {
      "_mojor_read_i32"
    } else if (!is.null(x_type) && x_type %in% c("f32[]", "f32")) {
      "_mojor_read_f32"
    } else {
      "_mojor_read_f64"
    }
    na_value <- if (!is.null(x_type) && x_type %in% c("i32[]", "i32", "lgl[]", "lgl", "bool[]", "bool")) {
      "Int32(-2147483648)"
    } else if (!is.null(x_type) && x_type %in% c("f32[]", "f32")) {
      "nan[DType.float32]()"
    } else {
      "nan[DType.float64]()"
    }

    helper_call <- paste0(
      "_mojor_sample_pick_index(__mojor_rng_state, Int(", x_len, "), Int(", size_str, "), Int(", idx_one_based_expr, "), ", replace_str, ")"
    )
    read_expr <- .mojor_ir_read_call(
      sample_read,
      x_name,
      paste0("Int(", helper_call, " - 1)"),
      paste0("Int(", x_len, ")")
    )
    paste0("(", na_value, " if not ", idx_guard, " else ", read_expr, ")")
  }
  .mojor_ir_sample_index_scalar_emit <- function(index_node) {
    if (is.null(index_node) || index_node$kind != "index") {
      return(NULL)
    }
    if (is.null(index_node$base) || !(index_node$base$kind %in% c("sample_int", "sample"))) {
      return(NULL)
    }
    op_label <- if (identical(index_node$base$kind, "sample_int")) "sample.int" else "sample"
    if (is.null(index_node$indices) || length(index_node$indices) != 1) {
      .mojor_err(
        paste0(op_label, "() indexed scalar emission requires exactly one index in this release"),
        index_node$src,
        paste0("Use ", op_label, "(..., size = k)[j] with scalar integer j")
      )
    }

    idx_str <- .mojor_ir_expr_emit(index_node$indices[[1]], zero_based_vars, type_env, loop_vars, index_context = TRUE)
    if (is.null(idx_str) || !nzchar(idx_str)) {
      return(NULL)
    }

    idx_val <- suppressWarnings(as.numeric(.mojor_ir_eval_const(index_node$indices[[1]])))
    if (length(idx_val) == 1 && is.finite(idx_val) && idx_val == floor(idx_val)) {
      one_based <- !identical(index_node$index_base, "zero_based")
      idx_one_based <- if (one_based) as.integer(idx_val) else as.integer(idx_val + 1)
      if (idx_one_based < 1L) {
        .mojor_err(
          paste0(op_label, "() indexed scalar emission requires index >= 1 in this release"),
          index_node$src,
          paste0("Use ", op_label, "(..., size = k)[j] with j >= 1")
        )
      }
      size_val <- suppressWarnings(as.numeric(.mojor_ir_eval_const(index_node$base$size)))
      if (length(size_val) == 1 && is.finite(size_val) && size_val == floor(size_val)) {
        return(.mojor_ir_sample_scalar_emit(index_node$base, indexed_context = TRUE, requested_index = idx_one_based))
      }
      return(.mojor_ir_sample_index_dynamic_emit(index_node, as.character(idx_one_based)))
    }

    idx_one_based_str <- if (identical(index_node$index_base, "zero_based")) {
      paste0("(", idx_str, " + 1)")
    } else {
      idx_str
    }
    .mojor_ir_sample_index_dynamic_emit(index_node, idx_one_based_str)
  }
  .mojor_ir_resolve_var_len <- function(var_name) {
    len_var_map <- .mojor_state$current_len_var_map
    n_source_name <- .mojor_state$current_n_source_name
    const_array_vars <- .mojor_state$current_const_array_vars
    nrow_var_map <- .mojor_state$current_nrow_var_map
    ncol_var_map <- .mojor_state$current_ncol_var_map
    dim_var_map <- .mojor_state$current_dim_var_map
    local_matrix_dims <- .mojor_state$current_local_matrix_dims
    local_array_dims <- .mojor_state$current_local_array_dims
    ndim_var_map <- .mojor_state$current_ndim_var_map
    out_name <- .mojor_state$current_out_name
    out_dim_var <- .mojor_state$current_out_dim_var

    dim_product_expr <- function(dim_terms) {
      if (length(dim_terms) == 0) {
        return(NULL)
      }
      expr <- dim_terms[[1]]
      if (length(dim_terms) > 1) {
        for (term in dim_terms[-1]) {
          expr <- paste0("(", expr, " * ", term, ")")
        }
      }
      expr
    }

    if (!is.null(len_var_map) && !is.null(len_var_map[[var_name]])) {
      return(len_var_map[[var_name]])
    }
    if (!is.null(const_array_vars) && isTRUE(const_array_vars[[var_name]])) {
      return(paste0("Int(len(", var_name, "))"))
    }
    if (!is.null(n_source_name) && identical(var_name, n_source_name)) {
      return("n_i")
    }

    if (!is.null(local_matrix_dims) && !is.null(local_matrix_dims[[var_name]])) {
      return(paste0("(nrow_", var_name, " * ncol_", var_name, ")"))
    }

    if (!is.null(nrow_var_map) && !is.null(ncol_var_map) &&
        !is.null(nrow_var_map[[var_name]]) && !is.null(ncol_var_map[[var_name]])) {
      return(paste0("(", nrow_var_map[[var_name]], " * ", ncol_var_map[[var_name]], ")"))
    }

    if (!is.null(local_array_dims) && !is.null(local_array_dims[[var_name]]) &&
        !is.null(local_array_dims[[var_name]]$dim)) {
      local_dims <- as.character(local_array_dims[[var_name]]$dim)
      local_dims <- local_dims[nzchar(local_dims)]
      local_len <- dim_product_expr(local_dims)
      if (!is.null(local_len)) {
        return(local_len)
      }
    }

    var_spec <- if (!is.null(type_env)) type_env[[var_name]] else NULL
    var_rank <- if (!is.null(var_spec) && .mojor_type_is_array(var_spec)) {
      .mojor_type_ndim(var_spec)
    } else {
      NA_integer_
    }

    if (!is.null(dim_var_map) && !is.null(dim_var_map[[var_name]])) {
      dim_var <- dim_var_map[[var_name]]
      if (is.finite(var_rank) && var_rank >= 1L) {
        dim_terms <- vapply(
          seq_len(as.integer(var_rank)),
          function(i) paste0("Int(", dim_var, "[", i - 1L, "])"),
          character(1)
        )
        dim_len <- dim_product_expr(dim_terms)
        if (!is.null(dim_len)) {
          return(dim_len)
        }
      }
      if (!is.null(ndim_var_map) && !is.null(ndim_var_map[[var_name]])) {
        ndim_var <- ndim_var_map[[var_name]]
        if (is.character(ndim_var) && grepl("^[0-9]+$", ndim_var)) {
          ndim_fixed <- suppressWarnings(as.integer(ndim_var))
          if (is.finite(ndim_fixed) && ndim_fixed >= 1L) {
            dim_terms <- vapply(
              seq_len(ndim_fixed),
              function(i) paste0("Int(", dim_var, "[", i - 1L, "])"),
              character(1)
            )
            dim_len <- dim_product_expr(dim_terms)
            if (!is.null(dim_len)) {
              return(dim_len)
            }
          }
        }
      }
    }

    if (!is.null(out_name) && identical(var_name, out_name) &&
        !is.null(out_dim_var) && is.finite(var_rank) && var_rank >= 1L) {
      out_terms <- vapply(
        seq_len(as.integer(var_rank)),
        function(i) paste0("Int(", out_dim_var, "[", i - 1L, "])"),
        character(1)
      )
      out_len <- dim_product_expr(out_terms)
      if (!is.null(out_len)) {
        return(out_len)
      }
    }

    # Check for input array length parameter (n_<var>_i)
    len_param <- paste0("n_", var_name, "_i")
    # Verify this parameter exists in the current scope by checking if it would be valid
    # The kernel signature includes n_<var> parameters for array inputs
    if (!is.null(var_spec) && .mojor_type_is_array(var_spec)) {
      return(len_param)
    }

    "n_i"
  }
  .mojor_ir_loop_pos0 <- function(loop_var) {
    if (!is.null(zero_based_vars) && loop_var %in% zero_based_vars) {
      return(loop_var)
    }
    paste0("(", loop_var, " - 1)")
  }
  .mojor_ir_emit_indexed_literal_ctor <- function(src_expr, idx_expr, debug_literal = FALSE) {
    if (isTRUE(debug_literal) && isTRUE(getOption("mojor.debug.constructor"))) {
      cat("DEBUG: Checking node$x$src for literal c():", deparse(src_expr), "\n")
    }
    if (!(is.call(src_expr) && as.character(src_expr[[1]]) == "c")) {
      return(NULL)
    }

    parts <- as.list(src_expr)[-1]
    nms <- names(parts)
    if (!is.null(nms)) {
      named_idx <- which(nms != "")
      if (length(named_idx) > 0) parts <- parts[-named_idx]
    }
    if (length(parts) == 0) {
      return(NULL)
    }

    all_literals <- all(vapply(parts, function(p) {
      is.numeric(p) || is.integer(p) || is.logical(p)
    }, logical(1)))
    if (!(all_literals && length(parts) <= 8)) {
      return(NULL)
    }

    values <- vapply(parts, function(p) {
      if (is.integer(p)) {
        as.character(p)
      } else if (is.numeric(p)) {
        val <- as.character(p)
        if (!grepl("[eE]", val) && !grepl("\\.", val) && (p %% 1) == 0) {
          paste0(val, ".0")
        } else {
          val
        }
      } else if (is.logical(p)) {
        if (isTRUE(p)) "1" else "0"
      } else {
        as.character(p)
      }
    }, character(1))

    if (length(values) == 1) {
      return(values[1])
    }

    expr_parts <- character(length(values))
    for (i in seq_along(values)) {
      idx_val <- i - 1L
      expr_parts[i] <- paste0(values[i], " if ", idx_expr, " == ", idx_val, " else ")
    }
    paste0("(", paste(expr_parts, collapse = ""), values[length(values)], ")")
  }
  .mojor_ir_var_array_len <- function(var_node) {
    if (is.null(var_node) || var_node$kind != "var" || is.null(type_env)) {
      return(NULL)
    }
    var_name <- var_node$name
    var_spec <- type_env[[var_name]]
    if (!.mojor_type_is_array(var_spec)) {
      return(NULL)
    }
    .mojor_ir_resolve_var_len(var_name)
  }
  .mojor_ir_resolve_matrix_dim <- function(var_name, dim_kind = c("nrow", "ncol")) {
    dim_kind <- match.arg(dim_kind)
    out_name <- .mojor_state$current_out_name
    if (!is.null(out_name) && identical(var_name, out_name)) {
      if (identical(dim_kind, "nrow")) {
        out_var <- .mojor_state$current_out_nrow_var
      } else {
        out_var <- .mojor_state$current_out_ncol_var
      }
      if (!is.null(out_var)) {
        return(out_var)
      }
    }
    if (identical(dim_kind, "nrow")) {
      dim_map <- .mojor_state$current_nrow_var_map
      fallback <- "nrow_out_i"
    } else {
      dim_map <- .mojor_state$current_ncol_var_map
      fallback <- "ncol_out_i"
    }
    if (!is.null(dim_map) && !is.null(dim_map[[var_name]])) {
      return(dim_map[[var_name]])
    }
    fallback
  }
  .mojor_ir_attach_index_loop_meta <- function(child_node, parent_node) {
    if (is.null(child_node) || !is.list(child_node) || is.null(parent_node)) {
      return(child_node)
    }
    override_loop_var <- parent_node[["__mojor_index_loop_var"]]
    if (!is.null(override_loop_var) && is.character(override_loop_var) && length(override_loop_var) == 1) {
      child_node[["__mojor_index_loop_var"]] <- override_loop_var
    }
    if (!is.null(parent_node[["__mojor_slice_loop_vars"]]) && is.null(child_node[["__mojor_slice_loop_vars"]])) {
      child_node[["__mojor_slice_loop_vars"]] <- parent_node[["__mojor_slice_loop_vars"]]
    }
    child_node
  }

  if (node$kind == "const") {
    val <- node$value
    if (is.logical(val) && length(val) == 1 && !is.na(val)) {
      return(if (isTRUE(val)) "True" else "False")
    }
    if (is.character(val) && length(val) == 1) {
      if (identical(val, "TRUE")) {
        return("True")
      }
      if (identical(val, "FALSE")) {
        return("False")
      }
      if (identical(val, "true")) {
        return("True")
      }
      if (identical(val, "false")) {
        return("False")
      }
    }
    if (isTRUE(index_context) && is.character(val) && grepl("^-?[0-9]+\\.0+$", val)) {
      val <- sub("\\.0+$", "", val)
    }
    if (identical(val, "Inf")) {
      return("_MOJOR_INF")
    }
    if (identical(val, "-Inf")) {
      return("_MOJOR_NINF")
    }
    if (identical(val, "NaN")) {
      return("_MOJOR_NAN")
    }
    return(val)
  }
  if (node$kind == "var") {
    iter_map <- .mojor_state$current_iter_map
    if (!is.null(iter_map) && !is.null(iter_map[[node$name]])) {
      iter_entry <- iter_map[[node$name]]
      if (!is.null(iter_entry$expr)) {
        return(iter_entry$expr)
      }
      if (!is.null(iter_entry$source) && !is.null(iter_entry$index)) {
        idx_name <- iter_entry$index
        if (!is.null(zero_based_vars) && idx_name %in% zero_based_vars) {
          return(paste0(iter_entry$source, "[", idx_name, "]"))
        }
        return(paste0(iter_entry$source, "[(", idx_name, " - 1)]"))
      }
    }
    override_loop_var <- node[["__mojor_index_loop_var"]]
    if (!is.null(override_loop_var) && is.character(override_loop_var) && length(override_loop_var) == 1 && !is.null(type_env)) {
      var_type <- type_env[[node$name]]
      if (!is.null(var_type) && .mojor_type_is_array(var_type)) {
        elem_type <- .mojor_type_elem(var_type)
        read_helper <- switch(elem_type,
          "i32" = "_mojor_read_i32",
          "lgl" = "_mojor_read_i32",
          "bool" = "_mojor_read_i32",
          "f64" = "_mojor_read_f64",
          "f32" = "_mojor_read_f32",
          NULL
        )
        if (!is.null(read_helper)) {
          len_expr <- .mojor_ir_resolve_var_len(node$name)
          len_arg <- if (grepl("^Int\\(", len_expr)) len_expr else paste0("Int(", len_expr, ")")
          return(.mojor_ir_read_call(read_helper, node$name, override_loop_var, len_arg))
        }
      }
    }
    return(node$name)
  }
  if (node$kind == "unop") {
    expr_node <- .mojor_ir_attach_index_loop_meta(node$expr, node)
    inner <- .mojor_ir_expr_emit(expr_node, zero_based_vars, type_env, loop_vars, index_context = index_context)
    if (is.null(inner)) {
      return(NULL)
    }
    if (node$op == "!") {
      return(paste0("(not ", inner, ")"))
    }
    return(paste0("(-", inner, ")"))
  }
  if (node$kind == "binop") {
 # Emit-time fallback fold for all-const subtrees to avoid backend overflow
 # behavior differences (e.g. 1e308 * 2 becoming NaN at runtime).
    if (.mojor_ir_all_const(node)) {
      folded_val <- .mojor_ir_eval_const(node)
      if (!is.null(folded_val)) {
        folded_node <- .mojor_ir_value_to_const(folded_val)
        return(.mojor_ir_expr_emit(folded_node, zero_based_vars, type_env, loop_vars, index_context = index_context))
      }
    }
    lhs_node <- .mojor_ir_attach_index_loop_meta(node$lhs, node)
    rhs_node <- .mojor_ir_attach_index_loop_meta(node$rhs, node)
    lhs <- .mojor_ir_expr_emit(lhs_node, zero_based_vars, type_env, loop_vars, index_context = index_context)
    rhs <- .mojor_ir_expr_emit(rhs_node, zero_based_vars, type_env, loop_vars, index_context = index_context)
    if (is.null(lhs) || is.null(rhs)) {
      return(NULL)
    }

 # Step 7.1: NA propagation for arithmetic/comparison operations
 # When na_mode="propagate", wrap operations in NA-aware helpers
    na_mode <- if (!is.null(.mojor_state$current_na_mode)) .mojor_state$current_na_mode else "forbid"
    if (na_mode == "propagate" && node$op %in% c("+", "-", "*", "/", ">", "<", ">=", "<=", "==", "!=")) {
 # Get types for NA detection
      lhs_type <- .mojor_ir_infer_type(node$lhs, type_env)
      rhs_type <- .mojor_ir_infer_type(node$rhs, type_env)
      is_float_type <- function(t) !is.null(t) && t %in% c("f64", "f32", "unknown")

      if (is_float_type(lhs_type) || is_float_type(rhs_type)) {
 # Determine if f32 or f64
        is_f32 <- (!is.null(lhs_type) && lhs_type == "f32") || (!is.null(rhs_type) && rhs_type == "f32")
        type_suffix <- if (is_f32) "f32" else "f64"

        if (node$op %in% c("+", "-", "*", "/")) {
 # Arithmetic: use _mojor_{op}_{type}_na(a, b, __mojor_na_flag)
          op_name <- switch(node$op,
            "+" = "add",
            "-" = "sub",
            "*" = "mul",
            "/" = "div"
          )
          .mojor_state$needs_na_helpers <- TRUE
          return(paste0("_mojor_", op_name, "_", type_suffix, "_na(", lhs, ", ", rhs, ", __mojor_na_flag)"))
        } else {
 # Comparisons: use _mojor_{op}_{type}_na(a, b, __mojor_na_flag)
          op_name <- switch(node$op,
            ">" = "gt",
            "<" = "lt",
            ">=" = "ge",
            "<=" = "le",
            "==" = "eq",
            "!=" = "ne"
          )
          .mojor_state$needs_na_helpers <- TRUE
          return(paste0("_mojor_", op_name, "_", type_suffix, "_na(", lhs, ", ", rhs, ", __mojor_na_flag)"))
        }
      }
    }

    if (isTRUE(index_context) && node$op %in% c("+", "-")) {
      is_zero_const <- function(expr_node) {
        val <- .mojor_ir_eval_const(expr_node)
        if (is.null(val) || length(val) != 1L || is.na(val)) {
          return(FALSE)
        }
        num <- suppressWarnings(as.numeric(val))
        isTRUE(is.finite(num) && num == 0)
      }
      if (node$op == "+" && is_zero_const(node$rhs)) {
        return(lhs)
      }
      if (node$op == "+" && is_zero_const(node$lhs)) {
        return(rhs)
      }
      if (node$op == "-" && is_zero_const(node$rhs)) {
        return(lhs)
      }
    }
    if (node$op %in% c(">", "<", ">=", "<=", "==", "!=") && !is.null(type_env)) {
      wrap_int <- function(x) {
        if (grepl("^Int\\(", x)) {
          return(x)
        }
        paste0("Int(", x, ")")
      }
      if (!is.null(loop_vars) && length(loop_vars) > 0) {
        if (node$lhs$kind == "var" && node$lhs$name %in% loop_vars) {
          rhs_type <- .mojor_ir_infer_type(node$rhs, type_env)
          if (!is.null(rhs_type) && rhs_type == "i32") rhs <- wrap_int(rhs)
        }
        if (node$rhs$kind == "var" && node$rhs$name %in% loop_vars) {
          lhs_type <- .mojor_ir_infer_type(node$lhs, type_env)
          if (!is.null(lhs_type) && lhs_type == "i32") lhs <- wrap_int(lhs)
        }
      }
      lhs_type <- .mojor_ir_infer_type(node$lhs, type_env)
      rhs_type <- .mojor_ir_infer_type(node$rhs, type_env)
      is_float <- function(t) !is.null(t) && t %in% c("f64", "f32")
      if (!is_float(lhs_type) && !is_float(rhs_type)) {
        if (!is.null(lhs_type) && lhs_type == "i32") lhs <- wrap_int(lhs)
        if (!is.null(rhs_type) && rhs_type == "i32") rhs <- wrap_int(rhs)
      }
    }
    mop <- if (node$op == "%/%") "//" else if (node$op == "%%") "%" else node$op
    if (node$op == "&&") mop <- "and"
    if (node$op == "||") mop <- "or"
    return(paste0("(", lhs, " ", mop, " ", rhs, ")"))
  }
  if (node$kind == "cast") {
    expr_node <- .mojor_ir_attach_index_loop_meta(node$expr, node)
    inner <- .mojor_ir_expr_emit(expr_node, zero_based_vars, type_env, loop_vars, index_context = index_context)
    if (is.null(inner)) {
      return(NULL)
    }
    mojo_type <- .mojor_r_to_mojo_type(node$to)
    na_mode <- if (!is.null(.mojor_state$current_na_mode)) .mojor_state$current_na_mode else "forbid"
    to_f64 <- node$to %in% c("f64", "Float64")
    to_f32 <- node$to %in% c("f32", "Float32")
    if (identical(na_mode, "propagate") && (to_f64 || to_f32)) {
      from_type <- .mojor_ir_infer_type(node$expr, type_env)
      if (!is.null(from_type) && from_type %in% c("i32", "lgl", "bool")) {
        na_expr <- if (to_f32) "_mojor_r_na_f32()" else "_mojor_r_na_f64()"
        cast_expr <- paste0(mojo_type, "(", inner, ")")
        return(paste0("(", na_expr, " if (Int32(", inner, ") == Int32(-2147483648)) else ", cast_expr, ")"))
      }
    }
    return(paste0(mojo_type, "(", inner, ")"))
  }
  if (node$kind == "rng_vec") {
    return(.mojor_ir_rng_vec_scalar_expr_emit(
      node,
      zero_based_vars = zero_based_vars,
      type_env = type_env,
      loop_vars = loop_vars,
      index_context = index_context
    ))
  }
  if (node$kind == "call") {
    args <- character(length(node$args))
    for (i in seq_along(node$args)) {
      arg_node <- .mojor_ir_attach_index_loop_meta(node$args[[i]], node)
      arg_i <- .mojor_ir_expr_emit(arg_node, zero_based_vars, type_env, loop_vars, index_context = index_context)
      if (is.null(arg_i) || length(arg_i) == 0 || arg_i == "") {
        return(NULL)
      }
      args[i] <- arg_i
    }
    if (node$fn == "length" && length(args) == 1) {
      arg_node <- node$args[[1]]
      if (!is.null(arg_node) && arg_node$kind == "var") {
        return(.mojor_ir_resolve_var_len(arg_node$name))
      }
      # For non-variable args, we can't determine length at compile time
      return(NULL)
    }
 # PR-B3: Shape helpers - nrow, ncol, dim
    if (node$fn == "nrow" && length(args) == 1) {
      arg_node <- node$args[[1]]
      if (!is.null(arg_node) && arg_node$kind == "var") {
        return(.mojor_ir_resolve_matrix_dim(arg_node$name, "nrow"))
      }
      return("nrow_out_i") # Fallback
    }
    if (node$fn == "ncol" && length(args) == 1) {
      arg_node <- node$args[[1]]
      if (!is.null(arg_node) && arg_node$kind == "var") {
        return(.mojor_ir_resolve_matrix_dim(arg_node$name, "ncol"))
      }
      return("ncol_out_i") # Fallback
    }
    if (node$fn %in% c("min", "max", "pmin", "pmax") && length(node$args) >= 1) {
      emit_dim_reduction_terms <- function(arg_node) {
        if (is.null(arg_node) || arg_node$kind != "dim" || is.null(arg_node$x) ||
            arg_node$x$kind != "var") {
          return(NULL)
        }
        var_name <- arg_node$x$name
        var_spec <- if (!is.null(type_env)) type_env[[var_name]] else NULL
        var_rank <- if (!is.null(var_spec) && .mojor_type_is_array(var_spec)) {
          .mojor_type_ndim(var_spec)
        } else {
          NA_integer_
        }

        out_name <- .mojor_state$current_out_name
        if (!is.null(out_name) && identical(var_name, out_name)) {
          out_nrow_var <- .mojor_state$current_out_nrow_var
          out_ncol_var <- .mojor_state$current_out_ncol_var
          out_dim_var <- .mojor_state$current_out_dim_var
          if (!is.null(out_nrow_var) && !is.null(out_ncol_var) &&
              (!is.finite(var_rank) || identical(var_rank, 2L))) {
            return(c(out_nrow_var, out_ncol_var))
          }
          if (!is.null(out_dim_var) && is.finite(var_rank) && var_rank >= 1L) {
            return(vapply(
              seq_len(as.integer(var_rank)),
              function(k) paste0("Int(", out_dim_var, "[", k - 1L, "])"),
              character(1)
            ))
          }
        }

        nrow_map <- .mojor_state$current_nrow_var_map
        ncol_map <- .mojor_state$current_ncol_var_map
        if (!is.null(nrow_map) && !is.null(ncol_map) &&
            !is.null(nrow_map[[var_name]]) && !is.null(ncol_map[[var_name]]) &&
            (!is.finite(var_rank) || identical(var_rank, 2L))) {
          return(c(nrow_map[[var_name]], ncol_map[[var_name]]))
        }

        dim_map <- .mojor_state$current_dim_var_map
        if (!is.null(dim_map) && !is.null(dim_map[[var_name]]) &&
            is.finite(var_rank) && var_rank >= 1L) {
          dim_var <- dim_map[[var_name]]
          return(vapply(
            seq_len(as.integer(var_rank)),
            function(k) paste0("Int(", dim_var, "[", k - 1L, "])"),
            character(1)
          ))
        }
        NULL
      }

      minmax_args <- character(0)
      for (i in seq_along(node$args)) {
        dim_terms <- emit_dim_reduction_terms(node$args[[i]])
        if (!is.null(dim_terms)) {
          minmax_args <- c(minmax_args, dim_terms)
        } else {
          minmax_args <- c(minmax_args, args[[i]])
        }
      }
      if (length(minmax_args) == 0) {
        return(NULL)
      }
      fn_name <- if (identical(node$fn, "pmin")) "min" else if (identical(node$fn, "pmax")) "max" else node$fn
      if (length(minmax_args) == 1) {
        return(minmax_args[[1]])
      }
      expr <- minmax_args[[1]]
      for (term in minmax_args[-1]) {
        expr <- paste0(fn_name, "(", expr, ", ", term, ")")
      }
      return(expr)
    }
    if (node$fn == "as.logical" && length(args) == 1) {
      arg_type <- .mojor_ir_infer_type(node$args[[1]], type_env)
      if (!is.null(arg_type) && arg_type %in% c("bool", "lgl")) {
        return(args[[1]])
      }
      return(paste0("(", args[[1]], " != 0)"))
    }
    if (node$fn %in% c("is.na", "is.nan", "is.finite", "is.infinite") && length(args) == 1) {
      arg_type <- .mojor_ir_infer_type(node$args[[1]], type_env)
      is_float <- !is.null(arg_type) && arg_type %in% c("f64", "f32", "unknown")
      is_f32 <- !is.null(arg_type) && identical(arg_type, "f32")
      inf_pos <- if (is_f32) "_MOJOR_INF_F32" else "_MOJOR_INF"
      inf_neg <- if (is_f32) "_MOJOR_NINF_F32" else "_MOJOR_NINF"
      if (node$fn == "is.nan") {
        if (!is_float) {
          return("False")
        }
        .mojor_state$needs_na_helpers <- TRUE
        helper <- if (is_f32) "_mojor_is_nan_f32" else "_mojor_is_nan_f64"
        return(paste0(helper, "(", args[[1]], ")"))
      }
      if (node$fn == "is.infinite") {
        return(if (is_float) paste0("((", args[[1]], " == ", inf_pos, ") or (", args[[1]], " == ", inf_neg, "))") else "False")
      }
      if (node$fn == "is.finite") {
        return(if (is_float) paste0("((", args[[1]], " != ", inf_pos, ") and (", args[[1]], " != ", inf_neg, ") and (", args[[1]], " == ", args[[1]], "))") else "True")
      }
 # is.na
      if (is_float) {
        return(paste0("(", args[[1]], " != ", args[[1]], ")"))
      }
      if (!is.null(arg_type) && arg_type %in% c("i32", "lgl", "bool")) {
        return(paste0("(", args[[1]], " == -2147483648)"))
      }
      return("False")
    }
 # Step RNG-IR: Handle scalar RNG function calls (n must be 1).
    scalar_rng_fns <- .mojor_ir_rng_call_fns()
    if (node$fn %in% scalar_rng_fns && length(node$args) >= 1) {
      n_val <- suppressWarnings(as.numeric(.mojor_ir_eval_const(node$args[[1]])))
      n_is_one <- isTRUE(length(n_val) == 1 && is.finite(n_val) && all.equal(n_val, 1))
      if (!n_is_one) {
        return(NULL)
      }
      params <- if (length(args) >= 2) args[2:length(args)] else character(0)
      return(.mojor_ir_scalar_rng_emit(node$fn, params))
    }
 # Step 7: Map R math function names to Mojo names
    if (node$fn == "abs2" && length(args) == 1) {
      return(paste0("(", args[[1]], " * ", args[[1]], ")"))
    }
    if (node$fn == "sign" && length(args) == 1) {
      a <- args[[1]]
      return(paste0("(Float64(-1.0) if ", a, " < 0.0 else (Float64(1.0) if ", a, " > 0.0 else Float64(0.0)))"))
    }
    mojo_fn <- node$fn
    if (node$fn == "gamma") mojo_fn <- "tgamma"
    if (node$fn == "ceiling") mojo_fn <- "ceil"
    if (node$fn == "pmin") mojo_fn <- "min"
    if (node$fn == "pmax") mojo_fn <- "max"
    if (node$fn == "log10") mojo_fn <- "log10"
    if (node$fn == "log2") mojo_fn <- "log2"
    return(paste0(mojo_fn, "(", paste(args, collapse = ", "), ")"))
  }
 # dim() - Return integer vector of array dimensions
  if (node$kind == "dim") {
 # dim(x) - emit as call to Mojo runtime helper
 # Returns an integer vector of dimensions
    arg <- node$x
    if (!is.null(arg) && arg$kind == "var") {
      arg_name <- arg$name
 # Check if this is the output matrix
      out_name <- .mojor_state$current_out_name
      if (!is.null(out_name) && identical(arg_name, out_name)) {
        out_dim_var <- .mojor_state$current_out_dim_var
        out_ndim_var <- .mojor_state$current_out_ndim_var
        if (is.null(out_dim_var) && exists(".mojor_out_dim_var_name", mode = "function")) {
          out_dim_var <- .mojor_out_dim_var_name()
        }
        if (is.null(out_ndim_var) && exists(".mojor_out_ndim_var_name", mode = "function")) {
          out_ndim_var <- .mojor_out_ndim_var_name()
        }
        if (!is.null(out_dim_var) && !is.null(out_ndim_var)) {
 # Emit direct dimension pointer expression; index lowering handles dim(x)[k].
          return(out_dim_var)
        }
      }
 # Check input arrays
      dim_map <- .mojor_state$current_dim_var_map
      if (!is.null(dim_map) && !is.null(dim_map[[arg_name]])) {
        dim_var <- dim_map[[arg_name]]
        ndim_var <- .mojor_state$current_ndim_var_map[[arg_name]]
        if (!is.null(dim_var) && !is.null(ndim_var)) {
          return(dim_var)
        }
      }
      arg_dim_guess <- if (exists(".mojor_dim_var_name", mode = "function")) {
        .mojor_dim_var_name(arg_name)
      } else {
        paste0("dim_", arg_name, "_ptr")
      }
      arg_ndim_guess <- if (exists(".mojor_ndim_var_name", mode = "function")) {
        .mojor_ndim_var_name(arg_name)
      } else {
        paste0("ndim_", arg_name, "_i")
      }
      if (is.character(arg_dim_guess) && nzchar(arg_dim_guess) &&
          is.character(arg_ndim_guess) && nzchar(arg_ndim_guess)) {
        return(arg_dim_guess)
      }
 # Fallback: try to get dimensions from type info
      arg_type <- .mojor_ir_infer_type(arg, type_env)
      if (!is.null(arg_type)) {
        ndim <- .mojor_type_ndim(arg_type)
        if (!is.null(ndim) && ndim > 0) {
          stop("IR emit [dim]: unresolved runtime dim pointer for '", arg_name, "' (ndim=", ndim, ")")
        }
      }
    }
 # Generic dim(x) emission is not supported without dim metadata.
    stop("IR emit [dim]: unsupported dim() source; use direct array variables with available dim metadata")
  }
 # FFI - c_call emission via Mojo external_call
  if (node$kind == "c_call") {
    decl <- .mojor_state$declared_c_functions[[node$name]]
    if (is.null(decl)) {
      stop(".mojor_ir_expr_emit: c_call function '", node$name, "' not declared")
    }
    if (!is.character(node$returns) || length(node$returns) != 1 || !nzchar(node$returns)) {
      stop(".mojor_ir_expr_emit: c_call returns must be a non-empty character string")
    }
    if (!identical(node$returns, decl$returns)) {
      stop(
        ".mojor_ir_expr_emit: c_call return type mismatch for '", node$name,
        "': expected ", decl$returns, ", got ", node$returns
      )
    }
    expected_arity <- length(decl$args)
    actual_arity <- length(node$args)
    if (!identical(actual_arity, expected_arity)) {
      stop(
        ".mojor_ir_expr_emit: c_call argument count mismatch for '", node$name,
        "': expected ", expected_arity, ", got ", actual_arity
      )
    }
    node_arg_types <- NULL
    if (!is.null(node$arg_types)) {
      node_arg_types <- as.character(unname(unlist(node$arg_types, use.names = FALSE)))
    }
    node_arg_names <- NULL
    if (!is.null(node$arg_names)) {
      node_arg_names <- as.character(unname(unlist(node$arg_names, use.names = FALSE)))
    }
    if (!is.null(node$arg_types)) {
      if (!identical(length(node_arg_types), expected_arity)) {
        stop(
          ".mojor_ir_expr_emit: c_call arg_types length mismatch for '", node$name,
          "': expected ", expected_arity, ", got ", length(node_arg_types)
        )
      }
      declared_types_cmp <- as.character(unname(unlist(decl$args, use.names = FALSE)))
      node_types_cmp <- node_arg_types
      if (!identical(node_types_cmp, declared_types_cmp)) {
        stop(
          ".mojor_ir_expr_emit: c_call arg_types metadata mismatch for '", node$name,
          "' against declaration"
        )
      }
    }
    if (!is.null(node$arg_names)) {
      if (!identical(length(node_arg_names), expected_arity)) {
        stop(
          ".mojor_ir_expr_emit: c_call arg_names length mismatch for '", node$name,
          "': expected ", expected_arity, ", got ", length(node_arg_names)
        )
      }
      declared_names_cmp <- as.character(names(decl$args))
      node_names_cmp <- node_arg_names
      if (!identical(node_names_cmp, declared_names_cmp)) {
        stop(
          ".mojor_ir_expr_emit: c_call arg_names metadata mismatch for '", node$name,
          "' against declaration"
        )
      }
    }
    declared_types <- as.character(unname(unlist(decl$args, use.names = FALSE)))
    declared_names <- names(decl$args)
    for (i in seq_along(node$args)) {
      if (length(declared_types) < i) break
      expected_type <- declared_types[[i]]
      inferred_type <- tryCatch(
        .mojor_ir_infer_type(node$args[[i]], type_env),
        error = function(e) "unknown"
      )
      if (is.null(inferred_type) || !is.character(inferred_type) || length(inferred_type) != 1 || !nzchar(inferred_type)) {
        inferred_type <- "unknown"
      }
      if (!identical(inferred_type, "unknown") && !.mojor_ffi_ir_type_compatible(expected_type, inferred_type)) {
        arg_label <- if (!is.null(declared_names) && length(declared_names) >= i && nzchar(declared_names[[i]])) {
          declared_names[[i]]
        } else {
          paste0("#", i)
        }
        stop(
          ".mojor_ir_expr_emit: c_call argument type mismatch for '", node$name,
          "' argument '", arg_label, "': expected ", expected_type, ", got ", inferred_type
        )
      }
    }

    emitted_args <- character(length(node$args))
    for (i in seq_along(node$args)) {
      arg_i <- .mojor_ir_expr_emit(
        node$args[[i]], zero_based_vars, type_env, loop_vars, index_context
      )
      if (is.null(arg_i) || length(arg_i) == 0 || arg_i == "") {
        return(NULL)
      }
      emitted_args[i] <- arg_i
    }

    mojo_ret <- .mojor_ffi_to_mojo_type(decl$returns)
    .mojor_state$needs_ffi <- TRUE

    return(paste0(
      'external_call["', node$name, '", ', mojo_ret, "](",
      paste(emitted_args, collapse = ", "), ")"
    ))
  }
 # Step 24: GPU Array - Reduction and Matmul
  if (node$kind == "gpu_reduce") {
 # Keep parity with lowering canonical call:
 # _mojor_gpu_reduce(arg, "op", dims_or_empty, keepdims, dims_default, length(arg))
    op <- node$op
    if (is.null(op)) {
      return(NULL)
    }
    dims_node <- if (!is.null(node$dims)) node$dims else .mojor_ir_const("Int(0)")
    keepdims_node <- .mojor_ir_const(if (isTRUE(node$keepdims)) "True" else "False")
    dims_default_node <- .mojor_ir_const(if (is.null(node$dims)) "True" else "False")
    n_node <- .mojor_ir_call("length", list(node$arg))
    call_node <- .mojor_ir_call(
      "_mojor_gpu_reduce",
      list(
        node$arg,
        .mojor_ir_const(paste0("\"", op, "\"")),
        dims_node,
        keepdims_node,
        dims_default_node,
        n_node
      )
    )
    return(.mojor_ir_expr_emit(call_node, zero_based_vars, type_env, loop_vars, index_context))
  }
  if (node$kind == "gpu_matmul") {
 # Keep parity with lowering canonical call:
 # _mojor_gpu_matmul(a, b, transpose_a, transpose_b)
    call_node <- .mojor_ir_call(
      "_mojor_gpu_matmul",
      list(
        node$a,
        node$b,
        .mojor_ir_const(if (isTRUE(node$transpose_a)) "True" else "False"),
        .mojor_ir_const(if (isTRUE(node$transpose_b)) "True" else "False")
      )
    )
    return(.mojor_ir_expr_emit(call_node, zero_based_vars, type_env, loop_vars, index_context))
  }
  if (node$kind == "index") {
    sample_scalar <- .mojor_ir_sample_index_scalar_emit(node)
    if (!is.null(sample_scalar)) {
      return(sample_scalar)
    }
 # Note: out_name not available in expr context, only in stmt context
    return(.mojor_ir_index_emit(node, zero_based_vars, type_env, loop_vars, NULL))
  }
  # Step 6.4: Emit ifelse as Mojo ternary expression
  if (node$kind == "ifelse") {
    node_for_attach <- node
    override_loop_var <- node_for_attach[["__mojor_index_loop_var"]]
    if ((is.null(override_loop_var) || !is.character(override_loop_var) || length(override_loop_var) != 1) &&
      isTRUE(index_context) && !is.null(loop_vars) && length(loop_vars) > 0 && !is.null(type_env)) {
      child_types <- c(
        .mojor_ir_infer_type(node$cond, type_env),
        .mojor_ir_infer_type(node$yes, type_env),
        .mojor_ir_infer_type(node$no, type_env)
      )
      if (any(vapply(child_types, .mojor_type_is_array, logical(1)))) {
        node_for_attach[["__mojor_index_loop_var"]] <- loop_vars[[length(loop_vars)]]
      }
    }
    cond_node <- .mojor_ir_attach_index_loop_meta(node$cond, node_for_attach)
    yes_node <- .mojor_ir_attach_index_loop_meta(node$yes, node_for_attach)
    no_node <- .mojor_ir_attach_index_loop_meta(node$no, node_for_attach)
    cond_str <- .mojor_ir_expr_emit(cond_node, zero_based_vars, type_env, loop_vars, index_context = index_context)
    yes_str <- .mojor_ir_expr_emit(yes_node, zero_based_vars, type_env, loop_vars, index_context = index_context)
    no_str <- .mojor_ir_expr_emit(no_node, zero_based_vars, type_env, loop_vars, index_context = index_context)
    if (is.null(cond_str) || is.null(yes_str) || is.null(no_str)) {
      return(NULL)
    }
    base_expr <- paste0("(", yes_str, " if ", cond_str, " else ", no_str, ")")

 # deterministic NA propagation for ifelse condition in propagate mode.
    na_mode <- if (!is.null(.mojor_state$current_na_mode)) .mojor_state$current_na_mode else "forbid"
    if (identical(na_mode, "propagate")) {
      cond_sources <- unique(.mojor_ir_collect_na_sources(node$cond, zero_based_vars, type_env))
      cond_checks <- .mojor_ir_na_checks(cond_sources, type_env)
      if (length(cond_checks) > 0) {
        out_type <- .mojor_ir_infer_type(node, type_env)
        na_expr <- NULL
        if (out_type %in% c("f64", "unknown")) {
          na_expr <- "_mojor_r_na_f64()"
        } else if (identical(out_type, "f32")) {
          na_expr <- "_mojor_r_na_f32()"
        } else if (out_type %in% c("i32", "lgl")) {
          na_expr <- "Int32(-2147483648)"
        } else if (identical(out_type, "bool")) {
          na_expr <- "False"
        }
        if (!is.null(na_expr)) {
          return(paste0("(", na_expr, " if (", paste(cond_checks, collapse = " or "), ") else ", base_expr, ")"))
        }
      }
    }
    return(base_expr)
  }

 # Step 8.12: Emit constructor expressions
  if (node$kind == "rep") {
 # rep(x, times=, each=, length.out=) - emit recycling with modulo
 # Requires loop_vars to know which index to use
    override_loop_var <- node[["__mojor_index_loop_var"]]
    if (!is.null(override_loop_var) && is.character(override_loop_var) && length(override_loop_var) == 1) {
      loop_var <- override_loop_var
    } else {
      if (is.null(loop_vars) || length(loop_vars) == 0) {
        return(NULL)
      }
      loop_var <- loop_vars[[length(loop_vars)]]
    }

 # Build 0-based position respecting zero_based_vars.
 # If loop_var is already 0-based (e.g., from range(0, n) or explicit 0-start),
 # do NOT subtract 1 <U+2014> that would produce -1 at the first element.
    pos0 <- .mojor_ir_loop_pos0(loop_var)

    each_str <- NULL
    if (!is.null(node$each)) {
      each_str <- .mojor_ir_expr_emit(node$each, zero_based_vars, type_env, loop_vars, index_context)
      if (is.null(each_str)) {
        return(NULL)
      }
    }

 # Special case: constructor x (c/rep/rep_len) <U+2014> emit element via idx_override
    if (node$x$kind %in% c("c", "rep", "rep_len") && !is.null(node$x$src)) {
      len_var_map <- .mojor_state$current_len_var_map
      n_source_name <- .mojor_state$current_n_source_name
      x_len <- .mojor_ir_ctor_len_expr(node$x$src, type_env, len_var_map, n_source_name)
      if (!is.null(x_len)) {
        idx_expr <- if (!is.null(each_str)) {
          paste0("(", pos0, " // ", each_str, ") % ", x_len)
        } else {
          paste0(pos0, " % ", x_len)
        }

        lit_ctor <- .mojor_ir_emit_indexed_literal_ctor(node$x$src, idx_expr, debug_literal = TRUE)
        if (!is.null(lit_ctor)) {
          return(lit_ctor)
        }

        ctor_expr <- .mojor_expr_to_mojo(
          node$x$src,
          loop_vars = loop_var,
          types = type_env,
          in_cond = FALSE,
          zero_based_vars = zero_based_vars,
          idx_override = idx_expr,
          idx_zero_based = TRUE,
          suppress_len_checks = TRUE
        )
        if (!is.null(ctor_expr)) {
          return(ctor_expr)
        }
      }
    }

    x_str <- .mojor_ir_expr_emit(node$x, zero_based_vars, type_env, loop_vars, index_context)
    if (is.null(x_str)) {
      return(NULL)
    }

 # Get length of x
    x_len <- .mojor_ir_var_array_len(node$x)
    if (!is.null(x_len)) {
      if (!is.null(each_str)) {
        idx_expr <- paste0("(", pos0, " // ", each_str, ") % ", x_len)
      } else {
        idx_expr <- paste0(pos0, " % ", x_len)
      }
      return(paste0(x_str, "[", idx_expr, "]"))
    }

 # x is a scalar - just return it
    return(x_str)
  }

  if (node$kind == "rep_len") {
 # rep_len(x, length.out) - same as rep() for emission
    override_loop_var <- node[["__mojor_index_loop_var"]]
    if (!is.null(override_loop_var) && is.character(override_loop_var) && length(override_loop_var) == 1) {
      loop_var <- override_loop_var
    } else {
      if (is.null(loop_vars) || length(loop_vars) == 0) {
        return(NULL)
      }
      loop_var <- loop_vars[[length(loop_vars)]]
    }

 # Build 0-based position respecting zero_based_vars (see rep emission above)
    pos0 <- .mojor_ir_loop_pos0(loop_var)

 # Special case: constructor x (c/rep/rep_len) <U+2014> emit element via idx_override
    if (node$x$kind %in% c("c", "rep", "rep_len") && !is.null(node$x$src)) {
      len_var_map <- .mojor_state$current_len_var_map
      n_source_name <- .mojor_state$current_n_source_name
      x_len <- .mojor_ir_ctor_len_expr(node$x$src, type_env, len_var_map, n_source_name)
      if (!is.null(x_len)) {
        idx_expr <- paste0("(", pos0, " % ", x_len, ")")

        lit_ctor <- .mojor_ir_emit_indexed_literal_ctor(node$x$src, idx_expr)
        if (!is.null(lit_ctor)) {
          return(lit_ctor)
        }

        ctor_expr <- .mojor_expr_to_mojo(
          node$x$src,
          loop_vars = loop_var,
          types = type_env,
          in_cond = FALSE,
          zero_based_vars = zero_based_vars,
          idx_override = idx_expr,
          idx_zero_based = TRUE,
          suppress_len_checks = TRUE
        )
        if (!is.null(ctor_expr)) {
          return(ctor_expr)
        }
      }
    }

    x_str <- .mojor_ir_expr_emit(node$x, zero_based_vars, type_env, loop_vars, index_context)
    if (is.null(x_str)) {
      return(NULL)
    }

 # Get length of x
    x_len <- .mojor_ir_var_array_len(node$x)
    if (!is.null(x_len)) {
      idx_expr <- paste0("(", pos0, " % ", x_len, ")")
      return(paste0(x_str, "[", idx_expr, "]"))
    }

    return(x_str)
  }

  if (node$kind == "seq") {
 # seq(from, to, length.out) - emit linear interpolation
 # Formula: from + i * (to - from) / (length.out - 1)
 # Edge case: when length.out = 1, return just 'from' (no interpolation)
    override_loop_var <- node[["__mojor_index_loop_var"]]
    if (!is.null(override_loop_var) && is.character(override_loop_var) && length(override_loop_var) == 1) {
      loop_var <- override_loop_var
    } else {
      if (is.null(loop_vars) || length(loop_vars) == 0) {
        return(NULL)
      }
      loop_var <- loop_vars[[length(loop_vars)]]
    }

 # Get 0-based position
    pos0 <- .mojor_ir_loop_pos0(loop_var)

    from_str <- .mojor_ir_expr_emit(node$from, zero_based_vars, type_env, loop_vars, index_context)
    if (is.null(from_str)) {
      return(NULL)
    }

    length_out_str <- .mojor_ir_expr_emit(node$length_out, zero_based_vars, type_env, loop_vars, index_context)
    if (is.null(length_out_str)) {
      return(NULL)
    }

 # If to is provided, use linear interpolation: from + pos0 * (to - from) / (length.out - 1)
 # If to is NULL, we need a default - use from + pos0 (identity sequence)
    if (!is.null(node$to)) {
      to_str <- .mojor_ir_expr_emit(node$to, zero_based_vars, type_env, loop_vars, index_context)
      if (is.null(to_str)) {
        return(NULL)
      }
 # Linear interpolation - cast denominator to Float64 to avoid Int32 division issues
 # Handle edge case: when length.out == 1, just return 'from' (avoids division by zero)
      return(paste0("(", from_str, " if (", length_out_str, " == 1) else (", from_str, " + ", pos0, " * (", to_str, " - ", from_str, ") / Float64((", length_out_str, " - 1))))"))
    } else {
 # Default: identity sequence starting from 'from'
      return(paste0("(", from_str, " + ", pos0, ")"))
    }
  }

 # PR-B3 Step 3: Array utility function emission
  if (node$kind == "transpose") {
 # t(x) - matrix transpose
 # For 1D linearized indexing: t(x)[k] where k is the linearized position
 # In column-major order, t(x)[k] = x[transpose_index(k)]
 # For a matrix with dims (nrow, ncol), t(x) has dims (ncol, nrow)
 # Position k in t(x) maps to: row = k % ncol, col = k / ncol
 # In original x: x[col, row] = linearized position col * nrow + row
 #
 # For now, emit the underlying expression. The index transformation for 2D
 # indexing (t(x)[i,j] = x[j,i]) must be handled by the index emitter.
    x_str <- .mojor_ir_expr_emit(node$x, zero_based_vars, type_env, loop_vars, index_context)
    return(x_str)
  }

  if (node$kind == "cbind") {
 # cbind(...) - column binding
 # Creates a matrix by binding arguments as columns
 # For 1D linearized indexing: cbind(a, b, c)[k]
 # In column-major order with n rows and m columns:
 # Position k = col * n + row where col = k / n, row = k % n
 # Value = arg[col][row]
 #
 # For equal-length args, we cycle through args and divide index by num_args:
 # arg_index = pos / num_args, arg_select = pos % num_args
    if (length(node$args) == 0) {
      return(NULL)
    }
    if (is.null(loop_vars) || length(loop_vars) == 0) {
      return(NULL)
    }
    loop_var <- loop_vars[[length(loop_vars)]]

 # Get 0-based position
    if (!is.null(zero_based_vars) && loop_var %in% zero_based_vars) {
      pos0 <- loop_var
    } else {
      pos0 <- paste0("(", loop_var, " - 1)")
    }

    n_args <- length(node$args)

 # Emit each argument with adjusted index: pos / n_args
 # The index for each argument is pos0 / n_args (integer division)
    idx_expr <- paste0("Int((", pos0, " / ", n_args, "))")

 # Create indexed version of each argument
    args_str <- character(n_args)
    for (i in seq_along(node$args)) {
      arg_base <- .mojor_ir_expr_emit(node$args[[i]], zero_based_vars, type_env, loop_vars, index_context = TRUE)
      if (is.null(arg_base)) {
        return(NULL)
      }
      args_str[i] <- paste0(arg_base, "[", idx_expr, "]")
    }

    if (n_args == 1) {
      return(args_str[1])
    }

 # Build chained conditional based on position modulo n_args
 # pos % n_args selects which argument, pos / n_args selects the index within that argument
    result <- args_str[n_args]
    for (i in (n_args - 1):1) {
      result <- paste0("(", args_str[i], " if (", pos0, " % ", n_args, " == ", i - 1, ") else ", result, ")")
    }
    return(result)
  }

  if (node$kind == "rbind") {
 # rbind(...) - row binding
 # Creates a matrix by binding arguments as rows
 # For 1D linearized indexing: rbind(a, b, c)[k]
 # Arguments are concatenated: a[1], a[2], ..., b[1], b[2], ...
 # Position k maps to: arg = k / len, idx = k % len (assuming equal lengths)
 #
 # For equal-length args, we use same cycling as cbind
    if (length(node$args) == 0) {
      return(NULL)
    }
    if (is.null(loop_vars) || length(loop_vars) == 0) {
      return(NULL)
    }
    loop_var <- loop_vars[[length(loop_vars)]]

 # Get 0-based position
    if (!is.null(zero_based_vars) && loop_var %in% zero_based_vars) {
      pos0 <- loop_var
    } else {
      pos0 <- paste0("(", loop_var, " - 1)")
    }

    n_args <- length(node$args)

 # Emit each argument with adjusted index: pos / n_args
    idx_expr <- paste0("Int((", pos0, " / ", n_args, "))")

    args_str <- character(n_args)
    for (i in seq_along(node$args)) {
      arg_base <- .mojor_ir_expr_emit(node$args[[i]], zero_based_vars, type_env, loop_vars, index_context = TRUE)
      if (is.null(arg_base)) {
        return(NULL)
      }
      args_str[i] <- paste0(arg_base, "[", idx_expr, "]")
    }

    if (n_args == 1) {
      return(args_str[1])
    }

 # Build chained conditional
    result <- args_str[n_args]
    for (i in (n_args - 1):1) {
      result <- paste0("(", args_str[i], " if (", pos0, " % ", n_args, " == ", i - 1, ") else ", result, ")")
    }
    return(result)
  }

 # PR-B4: diag() - matrix diagonal operations
  if (node$kind == "diag") {
 # Three modes:
 # 1. Extraction: diag(matrix)[i] -> matrix[i, i] (diagonal elements)
 # In column-major order: diagonal index = (i-1) * (nrow+1)
 # 2. Creation: diag(vector)[i,j] in 2D matrix context -> if (i==j) x[i] else 0
 # 3. Identity: diag(n)[i,j] in 2D matrix context -> if (i==j) 1 else 0
 #
 # For 1D indexing in constructor context:
 # - If x is a matrix: extract diagonal using (i-1)*(nrow+1) calculation
 # - If x is a vector in 1D context: passthrough
 # - If x is a vector in 2D context: emit conditional (i==j) pattern
 # - If n is provided: return 1 (identity diagonal)

    if (!is.null(node$x)) {
 # Check if we're in 2D context (have 2 loop vars)
      is_2d_context <- !is.null(loop_vars) && length(loop_vars) >= 2

      if (is_2d_context) {
 # 2D context: emit conditional pattern
 # diag(x)[i,j] = (x[i] if (i == j) else 0.0)
        i_var <- loop_vars[[length(loop_vars) - 1]] # Row index
        j_var <- loop_vars[[length(loop_vars)]] # Column index

 # Build 0-based indices
        if (!is.null(zero_based_vars) && i_var %in% zero_based_vars) {
          i_idx <- i_var
        } else {
          i_idx <- paste0("(", i_var, " - 1)")
        }

        if (node$x$kind == "var") {
 # Vector case: emit (x[i] if (i == j) else 0.0)
          x_name <- node$x$name
          x_elem <- paste0(x_name, "[Int(", i_idx, ")]_")
          return(paste0("(", x_elem, " if (", i_var, " == ", j_var, ") else 0.0)"))
        } else {
 # Expression case: emit (x_expr if (i == j) else 0.0)
          x_str <- .mojor_ir_expr_emit(node$x, zero_based_vars, type_env, loop_vars, index_context)
          if (is.null(x_str)) {
            return(NULL)
          }
          return(paste0("(", x_str, " if (", i_var, " == ", j_var, ") else 0.0)"))
        }
      }

 # 1D context: check for matrix extraction or vector passthrough
      if (is.null(loop_vars) || length(loop_vars) == 0) {
        return(NULL)
      }
      loop_var <- loop_vars[[length(loop_vars)]]

 # Build 0-based position
      if (!is.null(zero_based_vars) && loop_var %in% zero_based_vars) {
        pos0 <- loop_var
      } else {
        pos0 <- paste0("(", loop_var, " - 1)")
      }

 # Check if x is a matrix (has nrow) or a vector
      x_is_matrix <- FALSE
      nrow_expr <- NULL

      if (node$x$kind == "var") {
        x_name <- node$x$name
        nrow_map <- .mojor_state$current_nrow_var_map
        if (!is.null(nrow_map) && !is.null(nrow_map[[x_name]])) {
          x_is_matrix <- TRUE
          nrow_expr <- nrow_map[[x_name]]
        }
      }

      if (x_is_matrix && !is.null(nrow_expr)) {
 # Matrix extraction mode: diag(matrix)[i] = matrix[(i-1)*(nrow+1)]
 # In column-major order, diagonal element i is at (i-1)*(nrow+1)
        diag_idx <- paste0("(", pos0, " * (", nrow_expr, " + 1))")
        base_str <- .mojor_ir_expr_emit(node$x, zero_based_vars, type_env, loop_vars, index_context)
        if (is.null(base_str)) {
          return(NULL)
        }
        return(paste0(base_str, "[Int(", diag_idx, ")]_"))
      } else {
 # Vector passthrough mode: diag(vector) in 1D context
        x_str <- .mojor_ir_expr_emit(node$x, zero_based_vars, type_env, loop_vars, index_context)
        if (is.null(x_str)) {
          return(NULL)
        }
        return(x_str)
      }
    } else if (!is.null(node$n)) {
 # Identity mode: check for 2D context
      is_2d_context <- !is.null(loop_vars) && length(loop_vars) >= 2

      if (is_2d_context) {
 # diag(n) in 2D context: (1.0 if (i == j) else 0.0)
        i_var <- loop_vars[[length(loop_vars) - 1]]
        j_var <- loop_vars[[length(loop_vars)]]
        return(paste0("(1.0 if (", i_var, " == ", j_var, ") else 0.0)"))
      } else {
 # 1D context: return 1.0
        return("1.0")
      }
    }
    return(NULL)
  }

 # PR-B5: Cumulative operations
 # These have loop-carried dependencies and are NOT parallelizable
 # They use the running accumulator pattern
 #
 # PR-B5 Step 4 Extension: Handle cumulative ops in general expressions
 # Register accumulator and return accumulator variable reference
 # The accumulator update will be added after the statement completes
  if (node$kind %in% c("cumsum", "cumprod", "cummax", "cummin")) {
    cumul_kind <- node$kind
    x_node <- node$x

 # Check if this cumulative node already has an accumulator assigned
 # Use the node's src as a unique identifier
    node_key <- NULL
    if (!is.null(node$src)) {
      node_key <- paste0(cumul_kind, "_", deparse(node$src))
    }
    if (!is.null(node_key) && !is.null(.mojor_state$current_cumul_node_map) &&
      !is.null(.mojor_state$current_cumul_node_map[[node_key]])) {
 # Return existing accumulator for this node
      return(.mojor_state$current_cumul_node_map[[node_key]])
    }

 # Get or create accumulator variable
    acc_counter <- .mojor_state$current_cumul_acc_counter
    if (is.null(acc_counter)) acc_counter <- 0
    acc_counter <- acc_counter + 1
    .mojor_state$current_cumul_acc_counter <- acc_counter
    acc_var <- paste0("__mojor_cumul_acc_", acc_counter)

 # Store node-to-accumulator mapping
    if (!is.null(node_key)) {
      if (is.null(.mojor_state$current_cumul_node_map)) {
        .mojor_state$current_cumul_node_map <- list()
      }
      .mojor_state$current_cumul_node_map[[node_key]] <- acc_var
    }

 # Store accumulator info for pre-loop initialization
    acc_init <- switch(cumul_kind,
      cumsum = "0.0",
      cumprod = "1.0",
      cummax = "-inf[DType.float64]()",
      cummin = "inf[DType.float64]()"
    )
    if (is.null(.mojor_state$current_cumul_accs)) {
      .mojor_state$current_cumul_accs <- list()
    }
    .mojor_state$current_cumul_accs[[length(.mojor_state$current_cumul_accs) + 1]] <- list(
      var = acc_var,
      init = acc_init,
      kind = cumul_kind
    )

 # Emit x[i] expression for the update that will happen after this statement
 # PR-B5 Step 4: Need to index the array with the loop variable
 # Build an index node for x[i] where i is the current loop variable
    if (!is.null(loop_vars) && length(loop_vars) > 0) {
 # Get the innermost loop variable
      loop_var <- loop_vars[[length(loop_vars)]]
 # Emit x with indexing: x[i] or x[(i-1)] depending on zero_based_vars
      if (!is.null(zero_based_vars) && loop_var %in% zero_based_vars) {
        x_str <- paste0(x_node$name, "[", loop_var, "]")
      } else {
        x_str <- paste0(x_node$name, "[Int((", loop_var, " - 1))]")
      }
    } else {
 # No loop context - just emit x (this shouldn't happen in valid code)
      x_str <- .mojor_ir_expr_emit(x_node, zero_based_vars, type_env, loop_vars, index_context)
      if (is.null(x_str)) {
        return(NULL)
      }
    }

 # Store pending accumulator update
    if (cumul_kind == "cumsum") {
      update_expr <- paste0("(", acc_var, " + ", x_str, ")")
    } else if (cumul_kind == "cumprod") {
      update_expr <- paste0("(", acc_var, " * ", x_str, ")")
    } else if (cumul_kind == "cummax") {
      update_expr <- paste0("max(", acc_var, ", ", x_str, ")")
    } else if (cumul_kind == "cummin") {
      update_expr <- paste0("min(", acc_var, ", ", x_str, ")")
    }

 # Store for later emission after the statement
    if (is.null(.mojor_state$current_cumul_updates)) {
      .mojor_state$current_cumul_updates <- list()
    }
    .mojor_state$current_cumul_updates[[length(.mojor_state$current_cumul_updates) + 1]] <- list(
      var = acc_var,
      update = update_expr
    )

 # Return accumulator variable reference (the current value before update)
    return(acc_var)
  }

 # PR-B5: Statistical functions
  if (node$kind == "mean") {
 # mean(x) - arithmetic mean (scalar reduction)
 # Emits as: sum(x) / length(x)
    x_str <- .mojor_ir_expr_emit(node$x, zero_based_vars, type_env, loop_vars, index_context)
    if (is.null(x_str)) {
      return(NULL)
    }

 # Use the reduction sum pattern then divide by length
 # For now, emit placeholder - needs reduction infrastructure
    return(paste0("(__mojor_sum_", x_str, " / n_i)"))
  }

 # PR-B5 Step 3: Variance and standard deviation
  if (node$kind == "var_stat") {
 # var(x) - sample variance (scalar reduction)
 # Formula: sum((x - mean(x))^2) / (n - 1)
    x_str <- .mojor_ir_expr_emit(node$x, zero_based_vars, type_env, loop_vars, index_context)
    if (is.null(x_str)) {
      return(NULL)
    }

 # Placeholder - requires two-pass or online algorithm
    return(paste0("__mojor_var_", x_str))
  }

  if (node$kind == "sd") {
 # sd(x) - standard deviation (scalar reduction)
 # Formula: sqrt(var(x))
    x_str <- .mojor_ir_expr_emit(node$x, zero_based_vars, type_env, loop_vars, index_context)
    if (is.null(x_str)) {
      return(NULL)
    }

 # Placeholder - depends on variance calculation
    return(paste0("sqrt(__mojor_var_", x_str, ")"))
  }

  if (node$kind == "c") {
 # c(...) - emit concatenation with offset calculation.
 # Uses 0-based position `pos0` for half-open interval conditions,
 # so part k is selected when: offset0 <= pos0 < offset0 + part_len.
 # This avoids the off-by-one that arises when comparing 1-based loop_var directly.
 #
 # ND Fix: For multi-dimensional slices (e.g., arr[, , k] <- c(...)),
 # we need to linearize the position across all inner loop dimensions.
 # Position = i1 + i2*dim1 + i3*dim1*dim2 + ... (column-major order)
    if (!is.null(node[["__mojor_slice_loop_vars"]]) && length(node[["__mojor_slice_loop_vars"]]) > 0) {
      loop_vars <- node[["__mojor_slice_loop_vars"]]
    }
    if (is.null(loop_vars) || length(loop_vars) == 0) {
      return(NULL)
    }

 # Helper to get 0-based loop variable expression
    get_loop_var_0based <- function(lv) {
      if (!is.null(zero_based_vars) && lv %in% zero_based_vars) {
        lv
      } else {
        paste0("(", lv, " - 1)")
      }
    }

 # Prefer slice-assignment loops (e.g., __mojor_i1, __mojor_i2) when present.
 # This keeps constructor indexing aligned with missing-index expansion and avoids using
 # unrelated outer loops that should not affect c() element selection.
    candidate_loop_vars <- unique(loop_vars)
    slice_loop_vars <- candidate_loop_vars[grepl("^__mojor_i[0-9]+$", candidate_loop_vars)]
    if (length(slice_loop_vars) > 0) {
      real_loop_vars <- slice_loop_vars
    } else {
 # Fallback for non-slice uses: filter only synthetic idx_override vars and keep all loops.
 # These are added for no-loop constructor synthesis and shouldn't affect dimension calculation.
      real_loop_vars <- candidate_loop_vars[!grepl("^__mojor_idx_override", candidate_loop_vars)]
      if (length(real_loop_vars) == 0 && length(candidate_loop_vars) > 0) {
 # Constructor data fill uses idx_override-only context (no outer loop vars).
 # Keep the placeholder loop var so c() element selection can emit and later
 # substitute idx_override into the emitted expression.
        real_loop_vars <- candidate_loop_vars
      }
    }

    slice_dim_map <- NULL
    state_slice_map <- .mojor_state$current_slice_loop_extent_map
    if (!is.null(state_slice_map)) {
      if (!is.list(state_slice_map)) {
        state_slice_map <- as.list(state_slice_map)
      }
      if (!is.null(names(state_slice_map)) && length(state_slice_map) > 0) {
        state_slice_map <- vapply(state_slice_map, as.character, character(1))
        slice_dim_map <- state_slice_map
      }
    }
    if (!is.null(node[["__mojor_slice_loop_vars"]]) &&
        !is.null(node[["__mojor_slice_dim_exprs"]])) {
      meta_vars <- as.character(node[["__mojor_slice_loop_vars"]])
      meta_dims <- as.character(node[["__mojor_slice_dim_exprs"]])
      if (length(meta_vars) == length(meta_dims) && length(meta_vars) > 0) {
        slice_dim_map <- stats::setNames(meta_dims, meta_vars)
      }
    }

 # Build linearized 0-based position expression across all loop dimensions
 # For 1D: pos0 = i1
 # For 2D: pos0 = i1 + i2 * dim1
 # For 3D: pos0 = i1 + i2 * dim1 + i3 * dim1 * dim2
    if (length(real_loop_vars) == 0) {
 # No loop vars after filtering (shouldn't happen in valid code)
      return(NULL)
    } else if (length(real_loop_vars) == 1) {
 # Single loop var - original behavior
      pos0_expr <- get_loop_var_0based(real_loop_vars[[1]])
    } else {
 # Multiple loop vars - linearize position
 # Try to get dimension info from state for computing strides
      dim_vars <- character(0)
      if (!is.null(slice_dim_map)) {
        mapped_dims <- unname(slice_dim_map[real_loop_vars])
        if (length(mapped_dims) == length(real_loop_vars) &&
            all(!is.na(mapped_dims)) &&
            all(nzchar(mapped_dims))) {
          dim_vars <- as.character(mapped_dims)
        }
      }
      if (length(dim_vars) == 0) {
        out_dim_var <- .mojor_state$current_out_dim_var
        if (!is.null(out_dim_var)) {
 # For output array slice, map loop vars to their source axes when possible.
          axis_idx <- suppressWarnings(as.integer(sub("^__mojor_i([0-9]+)$", "\\1", real_loop_vars)))
          if (length(axis_idx) == length(real_loop_vars) &&
              all(is.finite(axis_idx)) &&
              all(axis_idx >= 1L)) {
            dim_vars <- vapply(
              axis_idx,
              function(ax) paste0(out_dim_var, "[", ax - 1L, "]"),
              character(1)
            )
          } else {
            for (j in 1:(length(real_loop_vars) - 1)) {
              dim_vars <- c(dim_vars, paste0(out_dim_var, "[", j - 1L, "]"))
            }
          }
        } else {
 # Matrix slices still have static row/col shape metadata.
          out_nrow_var <- .mojor_state$current_out_nrow_var
          out_ncol_var <- .mojor_state$current_out_ncol_var
          if (!is.null(out_nrow_var)) {
            dim_vars <- c(dim_vars, out_nrow_var)
            if (!is.null(out_ncol_var) && length(real_loop_vars) > 2) {
              dim_vars <- c(dim_vars, out_ncol_var)
            }
          }
        }
      }

 # Build linearized position: i1 + i2*dim1 + i3*dim1*dim2 + ...
      terms <- list(get_loop_var_0based(real_loop_vars[[1]]))
      stride_expr <- NULL

      for (j in 2:length(real_loop_vars)) {
 # Get dimension for this axis if available
        if (j - 1 <= length(dim_vars)) {
          dim_expr <- dim_vars[j - 1]
        } else {
 # Fallback: try to infer from context or use generic
          dim_expr <- "n_i" # generic fallback
        }

 # Build stride: dim1 * dim2 * ... * dim_{j-1}
        if (is.null(stride_expr)) {
          stride_expr <- dim_expr
        } else {
          stride_expr <- paste0("(", stride_expr, " * ", dim_expr, ")")
        }

 # Add term: loop_var_j * stride
        lv_0based <- get_loop_var_0based(real_loop_vars[[j]])
        terms[[length(terms) + 1]] <- paste0("(", lv_0based, " * ", stride_expr, ")")
      }

      pos0_expr <- paste(unlist(terms), collapse = " + ")
    }

    offset0 <- 0 # cumulative 0-based offset (integer or string expression)
    result_parts <- list()

 # Calculate total length for recycling
 # If c() is used in RHS and loops beyond its length, we need modulo
    all_const_parts <- TRUE
    total_len <- 0L
    total_len_expr <- NULL
    for (part in node$parts) {
      if (part$kind == "const") {
        total_len <- total_len + 1L
      } else {
        all_const_parts <- FALSE
        if (part$kind %in% c("c", "rep", "rep_len") && !is.null(part$src)) {
 # Nested constructor lengths are expression-valued; avoid fixed modulo recycling.
          total_len_expr <- "dynamic"
          break
        }
        part_type <- if (!is.null(type_env)) .mojor_ir_infer_type(part, type_env) else NULL
        if (!is.null(part_type) && .mojor_type_is_array(part_type)) {
 # Arrays and unknown-length expressions make length dynamic.
          total_len_expr <- "dynamic"
          break
        }
        total_len <- total_len + 1L
      }
    }

 # PR-B7: Scalar assignment from literal constructors takes first element only
 # When out[i] <- c(1, 2) appears in loop code (not constructor initialization),
 # R takes only the first element (with warning), not recycling.
 # Check if we're NOT in constructor mode and all parts are constants.
    has_slice_loop_context <- !is.null(node[["__mojor_slice_loop_vars"]]) &&
      length(node[["__mojor_slice_loop_vars"]]) > 0
    in_scalar_assign_context <- !isTRUE(.mojor_state$current_constructor_mode) &&
      !isTRUE(has_slice_loop_context) &&
      isTRUE(all_const_parts) &&
      is.null(total_len_expr) && total_len > 0

    if (in_scalar_assign_context) {
 # Emit only the first element for scalar assignment from literal constructor
      first_part <- node$parts[[1]]
      first_str <- .mojor_ir_expr_emit(first_part, zero_based_vars, type_env, loop_vars, index_context)
      if (!is.null(first_str)) {
        return(first_str)
      }
    }

 # Apply modulo for recycling if all parts are scalar constants
    pos0_recycled <- pos0_expr
    if (!is.null(total_len_expr) && total_len_expr != "dynamic" || (is.null(total_len_expr) && total_len > 0)) {
 # All parts are constants, apply modulo
      if (total_len > 0) {
        pos0_recycled <- paste0("((", pos0_expr, ") % ", total_len, ")")
      }
    }

    for (i in seq_along(node$parts)) {
      part <- node$parts[[i]]
      part_str <- .mojor_ir_expr_emit(part, zero_based_vars, type_env, loop_vars, index_context)
      if (is.null(part_str)) {
        return(NULL)
      }

 # Determine if part is scalar or array
      is_array <- FALSE
      part_len <- 1L # default: scalar contributes 1 element
      part_expr <- part_str

      if (part$kind == "var" && !is.null(type_env)) {
        part_name <- part$name
        part_spec <- type_env[[part_name]]
        if (.mojor_type_is_array(part_spec)) {
          is_array <- TRUE
          len_var_map <- .mojor_state$current_len_var_map
          part_len <- .mojor_ir_resolve_var_len(part_name)
        }
      } else if (part$kind %in% c("c", "rep", "rep_len") && !is.null(part$src)) {
        len_var_map <- .mojor_state$current_len_var_map
        n_source_name <- .mojor_state$current_n_source_name
        ctor_len <- .mojor_ir_ctor_len_expr(part$src, type_env, len_var_map, n_source_name)
        if (!is.null(ctor_len)) {
          is_array <- TRUE
          part_len <- ctor_len
          idx_expr <- if (identical(offset0, 0L) || identical(offset0, 0)) {
            pos0_expr
          } else {
            paste0(pos0_expr, " - (", offset0, ")")
          }
          ctor_expr <- .mojor_expr_to_mojo(
            part$src,
            loop_vars = real_loop_vars,
            types = type_env,
            in_cond = FALSE,
            zero_based_vars = zero_based_vars,
            idx_override = idx_expr,
            idx_zero_based = TRUE,
            suppress_len_checks = TRUE
          )
          if (!is.null(ctor_expr)) {
            part_expr <- ctor_expr
          }
        }
      }

      if (is_array && part$kind == "var") {
 # Array part: index within part = pos0 - offset0
        idx_expr <- if (identical(offset0, 0L) || identical(offset0, 0)) {
          pos0_expr
        } else {
          paste0(pos0_expr, " - (", offset0, ")")
        }
        part_expr <- paste0(part_str, "[", idx_expr, "]")
      }

 # Condition: pos0 < offset0 + part_len (half-open: [offset0, offset0 + part_len))
 # Use pos0_recycled which includes modulo if applicable
      end_offset <- if (identical(offset0, 0L) || identical(offset0, 0)) {
        as.character(part_len)
      } else {
        paste0("(", offset0, ") + ", part_len)
      }
      cond <- paste0(pos0_recycled, " < ", end_offset)

      result_parts[[i]] <- list(cond = cond, expr = part_expr)

 # Advance cumulative offset
      offset0 <- if (identical(offset0, 0L) || identical(offset0, 0)) {
        part_len
      } else {
        paste0("(", offset0, ") + ", part_len)
      }
    }

 # Build nested ternary from right to left
    result <- result_parts[[length(result_parts)]]$expr
    if (length(result_parts) > 1) {
      for (i in (length(result_parts) - 1):1) {
        result <- paste0("(", result_parts[[i]]$expr, " if ", result_parts[[i]]$cond, " else ", result, ")")
      }
    }

    return(result)
  }

 # Higher-Order Functions
  if (node$kind == "vapply") {
    .mojor_err(
      "vapply() is only supported as a whole-vector assignment in the compiled subset",
      node$src,
      "Use `out <- vapply(...)` with anonymous single-expression FUN and scalar return"
    )
  }

  if (node$kind == "sapply") {
    .mojor_err(
      "sapply() is only supported as a whole-vector assignment in the compiled subset",
      node$src,
      "Use `out <- sapply(...)` with anonymous single-expression FUN and scalar return"
    )
  }

  if (node$kind == "lapply") {
    .mojor_err(
      "lapply() is only supported as a whole-vector assignment in the compiled subset",
      node$src,
      "In v1, lapply compiles only when scalar FUN output can be represented as a vector"
    )
  }

  if (node$kind == "mapply") {
    .mojor_err(
      "mapply() is only supported as a whole-vector assignment in the compiled subset",
      node$src,
      "Use `out <- mapply(function(a,b,...) expr, x, y, ...)` with at least two direct vectors"
    )
  }

 # Sampling & Permutation
  if (node$kind == "sample_int") {
    sample_scalar <- .mojor_ir_sample_scalar_emit(node, indexed_context = FALSE)
    if (!is.null(sample_scalar)) {
      return(sample_scalar)
    }
    .mojor_err(
      "sample.int() scalar expression emission is unsupported for this form in this release",
      node$src,
      "Use sample.int(...)[j] with scalar index j, or use a vector assignment path"
    )
  }

 # Set/Match Primitives
 # These are whole-array operations emitting calls to set_match_helpers.mojo
  if (node$kind %in% c("unique", "duplicated", "any_duplicated", "match", "in")) {
    .mojor_state$needs_set_match <- TRUE
    x_node <- node$x
    if (is.null(x_node) || x_node$kind != "var") {
      return(NULL)
    }
    x_name <- x_node$name

    x_type <- if (!is.null(type_env)) type_env[[x_name]] else NULL
    type_suffix <- .mojor_ir_tier7_type_suffix(x_type, strict = FALSE)

 # Length variable for x
    x_len <- .mojor_ir_resolve_var_len(x_name)

    if (node$kind %in% c("unique", "duplicated", "any_duplicated")) {
      return(.mojor_ir_tier7_set_match_call(
        kind = node$kind,
        type_suffix = type_suffix,
        x_ptr = x_name,
        x_len = x_len,
        out_name = "out"
      ))
    }
 # match and %in% need a second argument (table)
    if (node$kind %in% c("match", "in")) {
      table_node <- node$table
      if (is.null(table_node) || table_node$kind != "var") {
        return(NULL)
      }
      table_name <- table_node$name
      table_len <- .mojor_ir_resolve_var_len(table_name)

      return(.mojor_ir_tier7_set_match_call(
        kind = node$kind,
        type_suffix = type_suffix,
        x_ptr = x_name,
        x_len = x_len,
        out_name = "out",
        table_ptr = table_name,
        table_len = table_len
      ))
    }
  }

 # Quantiles & Robust Stats
  if (node$kind %in% c("median", "quantile", "iqr", "mad")) {
    .mojor_state$needs_quantile <- TRUE
    x_node <- node$x
    if (is.null(x_node) || x_node$kind != "var") {
      return(NULL)
    }
    x_name <- x_node$name

    x_type <- if (!is.null(type_env)) type_env[[x_name]] else NULL
    type_suffix <- .mojor_ir_tier7_type_suffix(x_type, strict = FALSE)

    x_len <- .mojor_ir_resolve_var_len(x_name)

    if (node$kind == "median") {
      return(paste0("mojor_median_", type_suffix, "(", x_name, ", ", x_len, ", False)"))
    }
    if (node$kind == "iqr") {
      return(paste0("mojor_iqr_", type_suffix, "(", x_name, ", ", x_len, ", False)"))
    }
    if (node$kind == "mad") {
      center_str <- paste0("mojor_median_", type_suffix, "(", x_name, ", ", x_len, ", False)")
      return(paste0("mojor_mad_", type_suffix, "(", x_name, ", ", x_len, ", ", center_str, ", False, 1.4826, 7)"))
    }
    if (node$kind == "quantile") {
      probs_node <- node$probs
      if (is.null(probs_node) || probs_node$kind != "var") {
        return(NULL)
      }
      probs_name <- probs_node$name
      probs_len <- .mojor_ir_resolve_var_len(probs_name)
      type_str <- if (!is.null(node$type)) .mojor_ir_expr_emit(node$type, zero_based_vars, type_env, loop_vars) else "7"
      na_rm_str <- if (!is.null(node$na_rm)) .mojor_ir_expr_emit(node$na_rm, zero_based_vars, type_env, loop_vars) else "False"
      if (type_str == "" || is.null(type_str) || na_rm_str == "" || is.null(na_rm_str)) {
        return(NULL)
      }
      return(paste0("mojor_quantile_", type_suffix, "(", x_name, ", ", x_len, ", ", probs_name, ", ", probs_len, ", ", type_str, ", ", na_rm_str, ", out)"))
    }
  }

  if (node$kind == "sample") {
    sample_scalar <- .mojor_ir_sample_scalar_emit(node, indexed_context = FALSE)
    if (!is.null(sample_scalar)) {
      return(sample_scalar)
    }
    .mojor_err(
      "sample() scalar expression emission is unsupported for this form in this release",
      node$src,
      "Use sample(x, ...)[j] with scalar index j, or use a vector assignment path"
    )
  }

 # String Basics
  if (node$kind == "nchar") {
 # nchar(x) scalar element emission
    if (is.null(node$x) || node$x$kind != "var") {
      return(NULL)
    }
    if (is.null(loop_vars) || length(loop_vars) == 0) {
      .mojor_err(
        "nchar() scalar expression emission is unsupported outside loop/index context in this release",
        node$src,
        "Use whole-vector assignment `out <- nchar(x)` or strict expression-only wrapper mode"
      )
    }
    lv <- loop_vars[[length(loop_vars)]]
    idx0 <- if (!is.null(zero_based_vars) && lv %in% zero_based_vars) {
      lv
    } else {
      paste0("Int(", lv, " - 1)")
    }
    return(paste0("Int32(len(", node$x$name, "[", idx0, "]))"))
  }

  if (node$kind == "nzchar") {
 # nzchar(x) scalar element emission
    if (is.null(node$x) || node$x$kind != "var") {
      return(NULL)
    }
    if (is.null(loop_vars) || length(loop_vars) == 0) {
      .mojor_err(
        "nzchar() scalar expression emission is unsupported outside loop/index context in this release",
        node$src,
        "Use whole-vector assignment `out <- nzchar(x)` or strict expression-only wrapper mode"
      )
    }
    lv <- loop_vars[[length(loop_vars)]]
    idx0 <- if (!is.null(zero_based_vars) && lv %in% zero_based_vars) {
      lv
    } else {
      paste0("Int(", lv, " - 1)")
    }
    return(paste0("(len(", node$x$name, "[", idx0, "]) > 0)"))
  }

  if (node$kind == "substr") {
 # substr(x, start, stop) scalar element emission
    if (is.null(node$x) || node$x$kind != "var") {
      return(NULL)
    }
    if (is.null(node$start) || is.null(node$stop)) {
      return(NULL)
    }

    if (is.null(loop_vars) || length(loop_vars) == 0) {
      .mojor_err(
        "substr() scalar expression emission is unsupported outside loop/index context in this release",
        node$src,
        "Use whole-vector assignment `out <- substr(x, start, stop)` or strict expression-only wrapper mode"
      )
    }

    lv <- loop_vars[[length(loop_vars)]]
    idx0 <- if (!is.null(zero_based_vars) && lv %in% zero_based_vars) {
      lv
    } else {
      paste0("Int(", lv, " - 1)")
    }

    start_str <- .mojor_ir_expr_emit(node$start, zero_based_vars, type_env, loop_vars, index_context = TRUE)
    stop_str <- .mojor_ir_expr_emit(node$stop, zero_based_vars, type_env, loop_vars, index_context = TRUE)
    if (is.null(start_str) || !nzchar(start_str) || is.null(stop_str) || !nzchar(stop_str)) {
      return(NULL)
    }

    x_elem <- paste0(node$x$name, "[", idx0, "]")
    return(paste0(x_elem, "[", start_str, ":", stop_str, "]"))
  }

  if (node$kind == "paste") {
 # paste()/paste0() scalar element emission in loop/index contexts.
    if (is.null(node$args) || !is.list(node$args) || length(node$args) == 0) {
      return(NULL)
    }

    collapse_is_null <- is.null(node$collapse) ||
      (is.character(node$collapse) && length(node$collapse) == 1 && identical(node$collapse, "NULL"))
    if (!collapse_is_null) {
      .mojor_err(
        "paste()/paste0() scalar expression emission with collapse is unsupported in this release",
        node$src,
        "Use whole-vector assignment `out <- paste(..., collapse = NULL)` or strict expression-only wrapper mode"
      )
    }

    if (is.null(loop_vars) || length(loop_vars) == 0) {
      .mojor_err(
        "paste()/paste0() scalar expression emission is unsupported outside loop/index context in this release",
        node$src,
        "Use whole-vector assignment `out <- paste(x, y)` or strict expression-only wrapper mode"
      )
    }

    sep <- node$sep
    if (!is.character(sep) || length(sep) != 1) {
      return(NULL)
    }

    lv <- loop_vars[[length(loop_vars)]]
    idx0 <- if (!is.null(zero_based_vars) && lv %in% zero_based_vars) {
      lv
    } else {
      paste0("Int(", lv, " - 1)")
    }

    args_str <- character(length(node$args))
    for (i in seq_along(node$args)) {
      arg <- node$args[[i]]
      if (!is.null(arg$kind) && identical(arg$kind, "var")) {
        args_str[[i]] <- paste0(arg$name, "[", idx0, "]")
      } else {
        arg_str <- .mojor_ir_expr_emit(arg, zero_based_vars, type_env, loop_vars, index_context)
        if (is.null(arg_str)) {
          return(NULL)
        }
        args_str[[i]] <- arg_str
      }
    }

    if (length(args_str) == 1) {
      return(args_str[[1]])
    }

    result <- args_str[[1]]
    for (i in 2:length(args_str)) {
      result <- paste0("(", result, " + ", sep, " + ", args_str[[i]], ")")
    }
    return(result)
  }

  if (node$kind == "raw") {
    if (!is.null(node$expr) && .mojor_ir_expr_has_constructor(node$expr)) {
      if (is.null(loop_vars) || length(loop_vars) == 0) {
        return(NULL)
      }
      if (is.null(type_env)) type_env <- list()
      ctor_loop_var <- loop_vars[[length(loop_vars)]]
      idx_zero_based <- !is.null(zero_based_vars) && ctor_loop_var %in% zero_based_vars
      expr_out <- tryCatch(
        .mojor_expr_to_mojo(
          node$expr,
          loop_vars = ctor_loop_var,
          types = type_env,
          in_cond = FALSE,
          zero_based_vars = zero_based_vars,
          idx_override = ctor_loop_var,
          idx_zero_based = idx_zero_based
        ),
        error = function(e) NULL
      )
      return(expr_out)
    }
    return(NULL)
  }
  NULL
}

.mojor_ir_rng_vec_scalar_expr_emit <- function(node, zero_based_vars = NULL, type_env = NULL, loop_vars = NULL, index_context = FALSE) {
 # rng_vec is expression-lowerable only for scalar draws (n == 1).
  n_node <- node$n
  if (is.null(n_node) || is.null(n_node$kind) || n_node$kind != "const") {
    return(NULL)
  }
  n_val <- suppressWarnings(as.numeric(n_node$value))
  if (!is.finite(n_val) || !isTRUE(all.equal(n_val, 1))) {
    return(NULL)
  }

  params <- node$params
  if (is.null(params)) params <- list()
  param_strs <- character(length(params))
  for (i in seq_along(params)) {
    param_strs[[i]] <- .mojor_ir_expr_emit(
      params[[i]],
      zero_based_vars = zero_based_vars,
      type_env = type_env,
      loop_vars = loop_vars,
      index_context = index_context
    )
    if (is.null(param_strs[[i]])) {
      return(NULL)
    }
  }

  dist <- as.character(node$dist)
  .mojor_ir_scalar_rng_emit(dist, param_strs)
}
