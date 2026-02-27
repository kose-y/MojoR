# Transpile helpers: diagnostics, typing, expression lowering, and early utility helpers.

# ----------------------------------------------------------------------------
# Step 1: Minimal R -> Mojo transpiler (subset)
# ----------------------------------------------------------------------------

.mojor_mojo_type <- function(spec) {
  base <- .mojor_type_spec_base(spec)
  if (is.null(base)) {
    return(NULL)
  }
  switch(base,
    "f64" = "Float64",
    "f32" = "Float32",
    "i32" = "Int32",
    "lgl" = "Int32",
    "bool" = "Int32",
    NULL
  )
}

.mojor_mojo_dtype <- function(spec) {
  base <- .mojor_type_spec_base(spec)
  if (is.null(base)) {
    return(NULL)
  }
  switch(base,
    "f64" = "DType.float64",
    "f32" = "DType.float32",
    "i32" = "DType.int32",
    "lgl" = "DType.int32",
    "bool" = "DType.int32",
    NULL
  )
}

.mojor_type_spec_base <- function(spec) {
  if (is.null(spec)) {
    return(NULL)
  }
  if (!is.character(spec) || length(spec) != 1 || is.na(spec)) {
    return(NULL)
  }
  spec_norm <- gsub("\\s+", "", trimws(spec), perl = TRUE)
  base <- sub("\\[.*\\]$", "", spec_norm)
  if (identical(base, "logical")) base <- "lgl"
  base
}

.mojor_is_array <- function(spec) {
  is.character(spec) && length(spec) == 1 && !is.na(spec) && grepl("\\[", spec)
}

.mojor_resolve_array_source_name <- function(name, args, arg_specs, alias_map = NULL) {
  if (!is.character(name) || length(name) != 1 || is.na(name) || !nzchar(name)) {
    return(NULL)
  }
  if (is.null(alias_map)) {
    alias_map <- tryCatch(.mojor_state$current_array_aliases, error = function(e) NULL)
  }

  seen <- character(0)
  current <- name
  repeat {
    if (current %in% seen) {
      return(NULL)
    }
    seen <- c(seen, current)

    if (current %in% args) {
      spec <- arg_specs[[current]]
      if (!is.null(spec) && .mojor_is_array(spec)) {
        return(current)
      }
      return(NULL)
    }

    if (is.null(alias_map) || is.null(alias_map[[current]])) {
      return(NULL)
    }

    next_name <- alias_map[[current]]
    if (!is.character(next_name) || length(next_name) != 1 || is.na(next_name) || !nzchar(next_name)) {
      return(NULL)
    }
    current <- next_name
  }
}

.mojor_normalize_type_spec <- function(spec) {
  normalize_array_shape_spec <- function(base, value) {
    if (is.matrix(value)) {
      return(paste0(base, "[,]"))
    }
    if (is.array(value)) {
      dims <- dim(value)
      nd <- if (is.null(dims)) 1L else as.integer(length(dims))
      if (nd <= 1L) {
        return(paste0(base, "[]"))
      }
      if (nd == 2L) {
        return(paste0(base, "[,]"))
      }
      return(paste0(base, "[", nd, "d]"))
    }
    if (length(value) == 1) {
      return(base)
    }
    paste0(base, "[]")
  }

  normalize_char_spec <- function(x) {
    x <- trimws(x)
    if (!nzchar(x)) {
      return(NULL)
    }
    x <- gsub("\\s+", "", x, perl = TRUE)
    # Normalise [1d] alias to canonical [] form
    x <- sub("\\[1d\\]$", "[]", x)
    if (identical(x, "logical")) {
      return("lgl")
    }
    if (identical(x, "logical[]")) {
      return("lgl[]")
    }
    if (identical(x, "logical[,]")) {
      return("lgl[,]")
    }
    if (identical(x, "character")) {
      return("chr")
    }
    if (identical(x, "character[]")) {
      return("chr[]")
    }
    if (identical(x, "character[,]")) {
      return("chr[,]")
    }
    x
  }

  if (is.character(spec) && length(spec) == 1 && !is.na(spec)) {
    return(normalize_char_spec(spec))
  }

  if (methods::is(spec, "float32")) {
    return(normalize_array_shape_spec("f32", spec))
  }

  if (is.logical(spec)) {
    return(normalize_array_shape_spec("lgl", spec))
  }

  if (is.integer(spec)) {
    return(normalize_array_shape_spec("i32", spec))
  }

  if (is.double(spec)) {
    return(normalize_array_shape_spec("f64", spec))
  }

  if (is.character(spec)) {
    return(normalize_array_shape_spec("chr", spec))
  }

  if (is.data.frame(spec)) {
    return("df")
  }

  NULL
}

.mojor_normalize_arg_specs <- function(arg_names, types) {
  out <- types
  for (arg in arg_names) {
    raw <- out[[arg]]
    if (is.null(raw)) {
      stop("mojor_transpile: missing type annotation/value for ", arg)
    }
    spec <- .mojor_normalize_type_spec(raw)
    if (is.null(spec)) {
      stop("mojor_transpile: unsupported type spec for ", arg)
    }
    out[[arg]] <- spec
  }
  out
}

.mojor_type_is_tier9 <- function(spec) {
  is.character(spec) && length(spec) == 1 && !is.na(spec) &&
    spec %in% c("df", "chr[,]")
}

.mojor_validate_df_schema <- function(arg_names, arg_specs, df_schema = NULL) {
  if (is.null(df_schema)) {
    return(NULL)
  }
  if (!is.list(df_schema) || is.null(names(df_schema))) {
    stop("mojor_transpile: df_schema must be a named list keyed by data.frame argument name")
  }

  df_args <- arg_names[vapply(arg_names, function(a) identical(arg_specs[[a]], "df"), logical(1))]
  if (length(df_args) == 0) {
    stop("mojor_transpile: df_schema provided but no arguments are annotated as 'df'")
  }

  missing_schema <- setdiff(df_args, names(df_schema))
  if (length(missing_schema) > 0) {
    stop("mojor_transpile: df_schema missing entries for: ", paste(missing_schema, collapse = ", "))
  }

  allowed_col_specs <- c("f64[]", "f32[]", "i32[]", "lgl[]", "chr[]")
  out <- list()
  for (df_arg in df_args) {
    cols <- df_schema[[df_arg]]
    if (!(is.list(cols) || is.atomic(cols)) || is.null(names(cols)) || any(!nzchar(names(cols)))) {
      stop("mojor_transpile: df_schema[['", df_arg, "']] must be a named list/vector of column specs")
    }
    col_specs <- as.list(cols)
    for (nm in names(col_specs)) {
      col_spec <- .mojor_normalize_type_spec(col_specs[[nm]])
      if (is.null(col_spec) || !(col_spec %in% allowed_col_specs)) {
        stop(
          "mojor_transpile: df_schema[['", df_arg, "']][['", nm,
          "']] must be one of: ", paste(allowed_col_specs, collapse = ", ")
        )
      }
      col_specs[[nm]] <- col_spec
    }
    out[[df_arg]] <- col_specs
  }
  out
}

.mojor_df_hidden_arg_name <- function(df_arg, col_name) {
  sanitize <- function(x) {
    x <- gsub("[^A-Za-z0-9_]", "_", x)
    if (!nzchar(x)) x <- "x"
    if (grepl("^[0-9]", x)) x <- paste0("m_", x)
    x
  }
  paste0("__mojor_df__", sanitize(df_arg), "__", sanitize(col_name))
}

.mojor_df_dynamic_arg_name <- function(df_arg, selector_name) {
  sanitize <- function(x) {
    x <- gsub("[^A-Za-z0-9_]", "_", x)
    if (!nzchar(x)) x <- "x"
    if (grepl("^[0-9]", x)) x <- paste0("m_", x)
    x
  }
  paste0("__mojor_dfsel__", sanitize(df_arg), "__", sanitize(selector_name))
}

.mojor_df_rewrite_expr <- function(expr, df_schema, df_args, hidden_map, arg_specs = NULL, dynamic_selectors = NULL, context = "mojor_transpile") {
  if (is.null(expr)) {
    return(NULL)
  }
  if (!is.call(expr)) {
    return(expr)
  }

  op <- as.character(expr[[1]])
  if (length(op) == 1 && op == "$" && length(expr) == 3 && is.name(expr[[2]])) {
    df_arg <- as.character(expr[[2]])
    if (df_arg %in% df_args) {
      col_expr <- expr[[3]]
      if (!(is.name(col_expr) && nzchar(as.character(col_expr)))) {
        stop(context, ": data.frame $ access requires a literal column name symbol")
      }
      col_name <- as.character(col_expr)
      if (!(col_name %in% names(df_schema[[df_arg]]))) {
        stop(context, ": data.frame column '", col_name, "' not declared in df_schema[['", df_arg, "']]")
      }
      return(as.name(hidden_map[[df_arg]][[col_name]]))
    }
  }

  if (length(op) == 1 && op == "[[" && length(expr) >= 3 && is.name(expr[[2]])) {
    df_arg <- as.character(expr[[2]])
    if (df_arg %in% df_args) {
      if (length(expr) != 3) {
        stop(context, ": data.frame [[ ]] access supports only df[[\"col\"]] in the compiled subset")
      }
      col_expr <- expr[[3]]
      if (!(is.character(col_expr) && length(col_expr) == 1 && !is.na(col_expr) && nzchar(col_expr))) {
        stop(context, ": data.frame [[ ]] access requires a literal string column name")
      }
      col_name <- col_expr[[1]]
      if (!(col_name %in% names(df_schema[[df_arg]]))) {
        stop(context, ": data.frame column '", col_name, "' not declared in df_schema[['", df_arg, "']]")
      }
      return(as.name(hidden_map[[df_arg]][[col_name]]))
    }
  }

  if (length(op) == 1 && op == "[" && length(expr) >= 3 && is.name(expr[[2]])) {
    df_arg <- as.character(expr[[2]])
    if (df_arg %in% df_args) {
      is_missing_arg <- function(x) {
        identical(x, quote(expr =))
      }
      resolve_col_selector <- function(col_expr) {
        col_names <- names(df_schema[[df_arg]])
        if (is.character(col_expr) && length(col_expr) == 1 && !is.na(col_expr) && nzchar(col_expr)) {
          return(list(mode = "literal", col_name = col_expr[[1]]))
        }
        if ((is.integer(col_expr) || is.numeric(col_expr)) && length(col_expr) == 1) {
          idx <- suppressWarnings(as.integer(col_expr))
          if (is.na(idx) || idx < 1L || idx > length(col_names)) {
            stop(
              context, ": data.frame [ ] column index ", deparse(col_expr),
              " is out of bounds for df_schema[['", df_arg, "']] (1..", length(col_names), ")"
            )
          }
          return(list(mode = "literal", col_name = col_names[[idx]]))
        }
        if (is.name(col_expr) && nzchar(as.character(col_expr))) {
          selector_name <- as.character(col_expr)
          selector_spec <- if (!is.null(arg_specs) && !is.null(arg_specs[[selector_name]])) {
            .mojor_normalize_type_spec(arg_specs[[selector_name]])
          } else {
            NULL
          }
          if (!identical(selector_spec, "i32")) {
            stop(
              context,
              ": data.frame [ ] dynamic column selector '", selector_name,
              "' must be a scalar i32 argument in the compiled subset"
            )
          }
          col_specs <- unname(df_schema[[df_arg]])
          known_specs <- unique(col_specs[nzchar(col_specs)])
          if (length(known_specs) != 1L) {
            stop(
              context,
              ": data.frame [ ] dynamic column selectors require homogeneous df_schema[['", df_arg, "']] column specs"
            )
          }
          dynamic_name <- .mojor_df_dynamic_arg_name(df_arg, selector_name)
          if (!is.null(dynamic_selectors)) {
            selector_key <- paste0(df_arg, "::", selector_name)
            if (is.null(dynamic_selectors[[selector_key]])) {
              dynamic_selectors[[selector_key]] <- list(
                df_arg = df_arg,
                selector_name = selector_name,
                selector_spec = "i32",
                arg_name = dynamic_name,
                col_spec = known_specs[[1]],
                col_names = col_names
              )
            }
          }
          return(list(mode = "dynamic", arg_name = dynamic_name))
        }
        stop(
          context,
          ": data.frame [ ] access supports literal string/integer column selectors or scalar i32 argument selectors in the compiled subset"
        )
      }
      if (length(expr) < 4 || length(expr) > 5) {
        stop(
          context,
          ": data.frame [ ] access supports only df[i, j] / df[i, \"col\"] (optional drop=...) in the compiled subset"
        )
      }
      row_expr <- expr[[3]]
      col_expr <- expr[[4]]
      if (is_missing_arg(row_expr) || is_missing_arg(col_expr)) {
        stop(
          context,
          ": data.frame [ ] access requires explicit row and column selectors in the compiled subset"
        )
      }
      if (length(expr) == 5) {
        drop_expr <- expr[[5]]
        if (!is_missing_arg(drop_expr)) {
          if (!(is.logical(drop_expr) && length(drop_expr) == 1 && !is.na(drop_expr))) {
            stop(context, ": data.frame [ ] drop argument must be a scalar logical literal when provided")
          }
        }
      }
      col_selector <- resolve_col_selector(col_expr)
      if (identical(col_selector$mode, "literal")) {
        col_name <- col_selector$col_name
        if (!(col_name %in% names(df_schema[[df_arg]]))) {
          stop(context, ": data.frame column '", col_name, "' not declared in df_schema[['", df_arg, "']]")
        }
        hidden_col <- as.name(hidden_map[[df_arg]][[col_name]])
      } else if (identical(col_selector$mode, "dynamic")) {
        hidden_col <- as.name(col_selector$arg_name)
      } else {
        stop(context, ": internal data.frame [ ] selector rewrite failure")
      }
      row_rewritten <- .mojor_df_rewrite_expr(
        row_expr, df_schema, df_args, hidden_map,
        arg_specs = arg_specs, dynamic_selectors = dynamic_selectors, context = context
      )
      return(as.call(list(as.name("["), hidden_col, row_rewritten)))
    }
  }

  parts <- as.list(expr)
  for (i in seq_along(parts)) {
    parts[[i]] <- .mojor_df_rewrite_expr(
      parts[[i]], df_schema, df_args, hidden_map,
      arg_specs = arg_specs, dynamic_selectors = dynamic_selectors, context = context
    )
  }
  as.call(parts)
}

.mojor_df_rewrite_callable <- function(fn, arg_specs, df_schema, context = "mojor_transpile") {
  if (!is.function(fn)) stop(context, ": fn must be a function")
  arg_names <- names(formals(fn))
  if (is.null(arg_names)) arg_names <- character(0)
  if (length(arg_names) == 0) {
    return(list(fn = fn, arg_specs = arg_specs, hidden_map = list(), df_args = character(0)))
  }

  df_args <- arg_names[vapply(arg_names, function(a) identical(arg_specs[[a]], "df"), logical(1))]
  if (length(df_args) == 0) {
    return(list(fn = fn, arg_specs = arg_specs, hidden_map = list(), df_args = character(0)))
  }

 # A supports primitive homogeneous columns only on the compiled lane.
  for (df_arg in df_args) {
    col_specs <- df_schema[[df_arg]]
    for (col_name in names(col_specs)) {
      col_spec <- col_specs[[col_name]]
      if (!(col_spec %in% c("f64[]", "f32[]", "i32[]", "lgl[]", "chr[]"))) {
        stop(
          context,
          ": data.frame column '", df_arg, "$", col_name,
          "' with spec '", col_spec,
          "' is not supported on the compiled lane"
        )
      }
    }
  }

  hidden_map <- list()
  for (df_arg in df_args) {
    cols <- names(df_schema[[df_arg]])
    hidden_cols <- setNames(vapply(cols, function(col_name) {
      .mojor_df_hidden_arg_name(df_arg, col_name)
    }, character(1)), cols)
    hidden_map[[df_arg]] <- hidden_cols
  }

  dynamic_selectors_env <- new.env(parent = emptyenv())
  new_body <- .mojor_df_rewrite_expr(
    body(fn), df_schema, df_args, hidden_map,
    arg_specs = arg_specs, dynamic_selectors = dynamic_selectors_env, context = context
  )
  selector_keys <- ls(dynamic_selectors_env, all.names = TRUE)
  dynamic_selectors <- if (length(selector_keys) == 0) {
    list()
  } else {
    setNames(lapply(selector_keys, function(k) dynamic_selectors_env[[k]]), selector_keys)
  }

  old_formals <- formals(fn)
  new_formals_list <- list()
  new_arg_specs <- list()
  for (arg in arg_names) {
    spec <- arg_specs[[arg]]
    if (identical(spec, "df")) {
      cols <- names(df_schema[[arg]])
      for (col_name in cols) {
        hidden_name <- hidden_map[[arg]][[col_name]]
        new_formals_list[[hidden_name]] <- quote(expr = )
        new_arg_specs[[hidden_name]] <- df_schema[[arg]][[col_name]]
      }
      if (length(dynamic_selectors) > 0) {
        for (selector in dynamic_selectors) {
          if (!identical(selector$df_arg, arg)) {
            next
          }
          if (is.null(new_arg_specs[[selector$arg_name]])) {
            new_formals_list[[selector$arg_name]] <- quote(expr = )
            new_arg_specs[[selector$arg_name]] <- selector$col_spec
          }
        }
      }
    } else {
      new_formals_list[[arg]] <- old_formals[[arg]]
      new_arg_specs[[arg]] <- spec
    }
  }
  new_formals <- as.pairlist(new_formals_list)

  new_fn <- fn
  formals(new_fn) <- new_formals
  body(new_fn) <- new_body
  environment(new_fn) <- environment(fn)

  list(
    fn = new_fn,
    arg_specs = new_arg_specs,
    hidden_map = hidden_map,
    df_args = df_args,
    dynamic_selectors = dynamic_selectors
  )
}

.mojor_df_prepare_compiled_args <- function(resolved_args, arg_specs, df_schema, hidden_map, dynamic_selectors = NULL, compiled_arg_order = NULL, context = "mojor_build") {
  out <- list()
  arg_names <- names(arg_specs)
  if (is.null(arg_names)) {
    return(out)
  }

  for (arg in arg_names) {
    spec <- arg_specs[[arg]]
    if (!identical(spec, "df")) {
      out[[arg]] <- resolved_args[[arg]]
      next
    }

    df_val <- resolved_args[[arg]]
    if (!is.data.frame(df_val)) {
      stop(context, ": argument '", arg, "' must be a data.frame")
    }

    cols <- names(df_schema[[arg]])
    ref_len <- NULL
    for (col_name in cols) {
      if (!(col_name %in% names(df_val))) {
        stop(context, ": data.frame argument '", arg, "' is missing required column '", col_name, "'")
      }
      col_spec <- df_schema[[arg]][[col_name]]
      col_val <- df_val[[col_name]]
      if (is.null(col_val) || !is.atomic(col_val) || !is.null(dim(col_val))) {
        stop(context, ": data.frame column '", arg, "$", col_name, "' must be an atomic vector")
      }

      if (identical(col_spec, "f64[]")) {
        if (!is.numeric(col_val)) {
          stop(context, ": data.frame column '", arg, "$", col_name, "' must be numeric for spec f64[]")
        }
        col_out <- as.double(col_val)
      } else if (identical(col_spec, "f32[]")) {
        if (!is.numeric(col_val)) {
          stop(context, ": data.frame column '", arg, "$", col_name, "' must be numeric for spec f32[]")
        }
        col_out <- as.double(col_val)
      } else if (identical(col_spec, "i32[]")) {
        if (!is.integer(col_val)) {
          stop(context, ": data.frame column '", arg, "$", col_name, "' must be integer for spec i32[]")
        }
        col_out <- col_val
      } else if (identical(col_spec, "lgl[]")) {
        if (!is.logical(col_val)) {
          stop(context, ": data.frame column '", arg, "$", col_name, "' must be logical for spec lgl[]")
        }
        col_out <- col_val
      } else if (identical(col_spec, "chr[]")) {
        if (!is.character(col_val)) {
          stop(context, ": data.frame column '", arg, "$", col_name, "' must be character for spec chr[]")
        }
        col_out <- col_val
      } else {
        stop(
          context,
          ": data.frame column '", arg, "$", col_name,
          "' with spec '", col_spec,
          "' is not supported on the compiled lane"
        )
      }

      cur_len <- length(col_out)
      if (is.null(ref_len)) {
        ref_len <- cur_len
      } else if (!identical(cur_len, ref_len)) {
        stop(context, ": data.frame argument '", arg, "' has non-uniform column lengths")
      }
      out[[hidden_map[[arg]][[col_name]]]] <- col_out
    }
  }
  if (!is.null(dynamic_selectors) && length(dynamic_selectors) > 0) {
    for (selector in dynamic_selectors) {
      df_arg <- selector$df_arg
      selector_name <- selector$selector_name
      selector_value <- resolved_args[[selector_name]]
      if (!(is.integer(selector_value) && length(selector_value) == 1 && !is.na(selector_value))) {
        stop(
          context,
          ": data.frame [ ] dynamic selector argument '", selector_name,
          "' must be an i32 scalar"
        )
      }
      col_names <- selector$col_names
      idx <- as.integer(selector_value[[1]])
      if (is.na(idx) || idx < 1L || idx > length(col_names)) {
        stop(
          context,
          ": data.frame [ ] dynamic selector argument '", selector_name,
          "' is out of bounds for df_schema[['", df_arg, "']] (1..", length(col_names), ")"
        )
      }
      col_name <- col_names[[idx]]
      hidden_col <- hidden_map[[df_arg]][[col_name]]
      if (is.null(hidden_col) || is.null(out[[hidden_col]])) {
        stop(context, ": internal data.frame [ ] dynamic selector resolution failure")
      }
      out[[selector$arg_name]] <- out[[hidden_col]]
    }
  }
  if (!is.null(compiled_arg_order)) {
    missing_args <- setdiff(compiled_arg_order, names(out))
    if (length(missing_args) > 0) {
      stop(
        context,
        ": internal compiled argument assembly is missing rewritten args: ",
        paste(missing_args, collapse = ", ")
      )
    }
    out <- out[compiled_arg_order]
  }
  out
}

.mojor_tier9_terminal_expr <- function(fn) {
  if (!is.function(fn)) {
    return(NULL)
  }
  blocks <- .mojor_extract_block(body(fn))
  if (length(blocks) == 0) {
    return(NULL)
  }
  last_stmt <- blocks[[length(blocks)]]
  if (is.call(last_stmt) && identical(as.character(last_stmt[[1]]), "return")) {
    if (length(last_stmt) < 2) {
      return(NULL)
    }
    return(last_stmt[[2]])
  }
  last_stmt
}

.mojor_tier9_return_ctor_kind <- function(fn) {
  expr <- .mojor_tier9_terminal_expr(fn)
  if (!is.call(expr) || length(expr) < 1) {
    return(NULL)
  }
  op <- as.character(expr[[1]])
  if (!is.character(op) || length(op) != 1) {
    return(NULL)
  }
  if (op %in% c("data.frame", "list")) {
    return(op)
  }
  NULL
}

.mojor_tier9_parse_return_ctor <- function(fn, context = "mojor_transpile") {
  expr <- .mojor_tier9_terminal_expr(fn)
  if (!is.call(expr) || length(expr) < 1) {
    return(NULL)
  }
  op <- as.character(expr[[1]])
  if (!is.character(op) || length(op) != 1 || !(op %in% c("data.frame", "list"))) {
    return(NULL)
  }

  args <- as.list(expr)[-1]
  arg_names <- names(args)
  if (length(args) == 0) {
    stop(context, ": ", op, "(...) requires at least one argument in the compiled subset")
  }

  parse_entry <- function(x, idx, nm, ctor_kind) {
    if (missing(x) || is.null(x)) {
      stop(context, ": ", ctor_kind, "(...) does not support missing/NULL entries in the compiled subset")
    }
    if (!is.name(x) || !nzchar(as.character(x))) {
      stop(
        context, ": ", ctor_kind,
        "(...) supports only precomputed vector variables at the return boundary in the compiled subset"
      )
    }
    list(name = nm, expr = x, index = idx)
  }

  if (identical(op, "data.frame")) {
    if (is.null(arg_names) || any(!nzchar(arg_names))) {
      stop(context, ": data.frame(...) requires explicitly named columns in the compiled subset")
    }
    reserved <- c("row.names", "check.rows", "check.names", "fix.empty.names", "stringsAsFactors")
    bad_reserved <- intersect(arg_names, reserved)
    if (length(bad_reserved) > 0) {
      stop(
        context, ": data.frame(...) does not support argument(s) in the compiled subset: ",
        paste(bad_reserved, collapse = ", ")
      )
    }
    if (any(duplicated(arg_names))) {
      stop(context, ": data.frame(...) requires unique column names in the compiled subset")
    }
    entries <- lapply(seq_along(args), function(i) parse_entry(args[[i]], i, arg_names[[i]], "data.frame"))
    return(list(kind = "data.frame", names = arg_names, entries = entries, expr = expr))
  }

 # list(...)
  if (is.null(arg_names)) arg_names <- rep.int("", length(args))
  entries <- lapply(seq_along(args), function(i) parse_entry(args[[i]], i, arg_names[[i]], "list"))
  list(kind = "list", names = arg_names, entries = entries, expr = expr)
}

.mojor_tier9_validate_ctor_arg_specs <- function(ctor, arg_specs, context = "mojor_transpile") {
  if (is.null(ctor) || is.null(ctor$entries) || !is.list(ctor$entries)) {
    return(invisible(TRUE))
  }
  if (is.null(arg_specs) || !is.list(arg_specs) || length(arg_specs) == 0) {
    return(invisible(TRUE))
  }

  allowed_specs <- c("f64[]", "f32[]", "i32[]", "lgl[]", "bool[]")
  if (identical(ctor$kind, "list") || identical(ctor$kind, "data.frame")) {
    allowed_specs <- c(allowed_specs, "chr[]")
  }
  known_base_types <- character(0)

  for (entry in ctor$entries) {
    if (is.null(entry$expr) || !is.name(entry$expr)) next
    var_name <- as.character(entry$expr)
    if (!nzchar(var_name) || !(var_name %in% names(arg_specs))) next

    spec <- .mojor_normalize_type_spec(arg_specs[[var_name]])
    if (is.null(spec)) {
      stop(context, ": compiled subset constructor entry '", var_name, "' has unknown argument type")
    }
    if (identical(spec, "df")) {
      stop(context, ": compiled subset constructor entry '", var_name, "' cannot be a data.frame argument directly")
    }
    if (!(spec %in% allowed_specs)) {
      stop(
        context,
        ": compiled subset constructor entry '", var_name,
        "' must have a vector type (",
        paste(sprintf("'%s'", allowed_specs), collapse = ", "),
        ") in the compiled subset strict mode (got '",
        spec,
        "')"
      )
    }

    known_base_types <- c(known_base_types, sub("\\[.*\\]$", "", spec))
  }

  if (identical(ctor$kind, "list")) {
    known_base_types <- unique(known_base_types[nzchar(known_base_types)])
    if (length(known_base_types) > 1) {
      stop(context, ": list(...) requires homogeneous known argument types in the compiled subset strict mode")
    }
  }

  invisible(TRUE)
}

.mojor_tier9_replace_terminal_expr <- function(fn, new_expr, context = "mojor_build") {
  if (!is.function(fn)) stop(context, ": fn must be a function")
  blocks <- .mojor_extract_block(body(fn))
  if (length(blocks) == 0) stop(context, ": function body is empty")

  idx <- length(blocks)
  last_stmt <- blocks[[idx]]
  if (is.call(last_stmt) && identical(as.character(last_stmt[[1]]), "return")) {
    blocks[[idx]] <- as.call(list(as.name("return"), new_expr))
  } else {
    blocks[[idx]] <- new_expr
  }

  out <- fn
  body(out) <- .mojor_wrap_block_expr(blocks)
  environment(out) <- environment(fn)
  out
}

.mojor_tier9_validate_atomic_vector <- function(value, label, context, allow_chr = FALSE) {
  if (is.factor(value)) {
    stop(context, ": ", label, " must not be a factor in the compiled subset")
  }
  if (!is.atomic(value) || !is.null(dim(value))) {
    stop(context, ": ", label, " must be an atomic vector")
  }
  spec <- .mojor_normalize_type_spec(value)
  allowed <- c("f64", "f64[]", "i32", "i32[]", "lgl", "lgl[]")
  if (isTRUE(allow_chr)) {
    allowed <- c(allowed, "chr", "chr[]")
  }
  if (is.null(spec) || !(spec %in% allowed)) {
    stop(
      context, ": ", label, " has unsupported vector type for the compiled subset: ",
      if (is.null(spec)) "<unknown>" else spec
    )
  }
  list(
    value = value,
    len = length(value),
    base = sub("\\[.*$", "", spec)
  )
}

.mojor_tier9_runtime_make_data_frame <- function(values, col_names, context = "mojor_build") {
  if (length(values) != length(col_names)) {
    stop(context, ": internal compiled subset data.frame assembly mismatch")
  }
  cols <- vector("list", length(values))
  names(cols) <- col_names
  ref_len <- NULL
  for (i in seq_along(values)) {
    checked <- .mojor_tier9_validate_atomic_vector(
      value = values[[i]],
      label = paste0("data.frame column '", col_names[[i]], "'"),
      context = context,
      allow_chr = TRUE
    )
    if (is.null(ref_len)) {
      ref_len <- checked$len
    } else if (!identical(ref_len, checked$len)) {
      stop(context, ": data.frame(...) column lengths must match in the compiled subset")
    }
    cols[[i]] <- checked$value
  }
  as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
}

.mojor_tier9_runtime_make_list <- function(values, item_names = NULL, context = "mojor_build") {
  if (length(values) == 0) {
    stop(context, ": list(...) requires at least one entry in the compiled subset")
  }
  checked <- vector("list", length(values))
  base_types <- character(length(values))
  for (i in seq_along(values)) {
    nm <- ""
    if (!is.null(item_names) && length(item_names) >= i && nzchar(item_names[[i]])) {
      nm <- item_names[[i]]
    }
    label <- if (nzchar(nm)) paste0("list item '", nm, "'") else paste0("list item #", i)
    checked[[i]] <- .mojor_tier9_validate_atomic_vector(
      value = values[[i]],
      label = label,
      context = context,
      allow_chr = TRUE
    )
    base_types[[i]] <- checked[[i]]$base
  }
  if (length(unique(base_types)) != 1) {
    stop(context, ": list(...) requires homogeneous atomic vector element types in the compiled subset")
  }
  out <- lapply(checked, function(x) x$value)
  if (!is.null(item_names) && length(item_names) == length(out)) {
    names(out) <- item_names
  }
  out
}

.mojor_tier9_runtime_regex <- function(op, pattern, x, replacement = NULL, fixed = FALSE, perl = TRUE, context = "mojor_build") {
  if (!is.character(op) || length(op) != 1 || !(op %in% c("grepl", "grep", "sub", "gsub"))) {
    stop(context, ": compiled subset regex runtime received unsupported op")
  }
  if (!(is.character(pattern) && length(pattern) == 1 && !is.na(pattern))) {
    stop(context, ": compiled subset regex runtime pattern must be a scalar character value")
  }
  if (!is.character(x)) {
    stop(context, ": compiled subset regex runtime x must be a character vector")
  }
  if (op %in% c("sub", "gsub")) {
    if (!(is.character(replacement) && length(replacement) == 1 && !is.na(replacement))) {
      stop(context, ": compiled subset regex runtime replacement must be a scalar character value")
    }
  } else {
    replacement <- NULL
  }
  if (!is.logical(fixed) || length(fixed) != 1 || is.na(fixed)) {
    stop(context, ": compiled subset regex runtime fixed must be TRUE or FALSE")
  }
  if (!is.logical(perl) || length(perl) != 1 || is.na(perl)) {
    stop(context, ": compiled subset regex runtime perl must be TRUE or FALSE")
  }
  if (isTRUE(fixed) && isTRUE(perl)) {
    stop(context, ": compiled subset regex runtime does not support fixed=TRUE with perl=TRUE")
  }
  .mojor_call_bridge(
    "mojor_tier9_regex_runtime",
    op,
    pattern,
    if (is.null(replacement)) NULL else replacement,
    x,
    as.logical(fixed),
    as.logical(perl)
  )
}

.mojor_tier9_runtime_expand_grid <- function(values, arg_labels = NULL, context = "mojor_build") {
  if (!is.list(values) || length(values) < 1) {
    stop(context, ": compiled subset expand.grid runtime requires a non-empty list of vectors")
  }
  for (i in seq_along(values)) {
    v <- values[[i]]
    if (!is.atomic(v) || !is.null(dim(v))) {
      stop(context, ": compiled subset expand.grid runtime argument #", i, " must be an atomic vector")
    }
    spec <- .mojor_normalize_type_spec(v)
    if (is.null(spec) || !(spec %in% c("f64[]", "i32[]", "lgl[]"))) {
      stop(context, ": compiled subset expand.grid runtime argument #", i, " must be f64[]/i32[]/lgl[]")
    }
  }
  if (is.null(arg_labels)) {
    arg_labels <- rep("", length(values))
  }
  if (!is.character(arg_labels) || length(arg_labels) != length(values)) {
    stop(context, ": compiled subset expand.grid runtime argument labels are invalid")
  }
  if (anyNA(arg_labels)) {
    stop(context, ": compiled subset expand.grid runtime argument labels must not be NA")
  }
  has_named <- nzchar(arg_labels)
  if (any(has_named)) {
    names(values)[has_named] <- arg_labels[has_named]
  }

  # Fast bridge path is fixed-arity and label-agnostic; use R fallback for
  # generalized arity/label cases.
  if (length(values) < 2 || length(values) > 4 || any(has_named)) {
    return(do.call(expand.grid, values))
  }

  a1 <- values[[1]]
  a2 <- values[[2]]
  a3 <- if (length(values) >= 3) values[[3]] else NULL
  a4 <- if (length(values) >= 4) values[[4]] else NULL
  .mojor_call_bridge(
    "mojor_tier9_expand_grid_runtime",
    a1, a2, a3, a4, as.integer(length(values))
  )
}

.mojor_tier9_runtime_execute_plan <- function(plan, args_in, arg_specs = NULL, context = "mojor_build") {
  if (is.null(plan) || !is.list(plan) || is.null(plan$kind)) {
    stop(context, ": missing compiled subset runtime plan metadata")
  }
  if (is.null(args_in)) args_in <- list()

  resolve_chr_scalar <- function(spec, label) {
    if (is.null(spec) || is.null(spec$kind)) {
      stop(context, ": compiled subset runtime ", label, " metadata is invalid")
    }
    if (identical(spec$kind, "const")) {
      val <- spec$value
      if (!(is.character(val) && length(val) == 1 && !is.na(val))) {
        stop(context, ": compiled subset runtime ", label, " const must be scalar character")
      }
      return(val)
    }
    if (identical(spec$kind, "arg")) {
      nm <- spec$name
      if (!is.character(nm) || length(nm) != 1 || !nzchar(nm) || is.null(args_in[[nm]])) {
        stop(context, ": compiled subset runtime missing argument for ", label, ": ", nm)
      }
      val <- args_in[[nm]]
      if (!(is.character(val) && length(val) == 1 && !is.na(val))) {
        stop(context, ": compiled subset runtime argument ", nm, " for ", label, " must be scalar character")
      }
      return(val)
    }
    stop(context, ": compiled subset runtime ", label, " metadata kind is unsupported: ", spec$kind)
  }
  bool_scalar <- function(val, label) {
    if (is.logical(val) && length(val) == 1 && !is.na(val)) {
      return(isTRUE(val))
    }
    if (is.numeric(val) && length(val) == 1 && !is.na(val) && val %in% c(0, 1)) {
      return(isTRUE(as.logical(val)))
    }
    if (is.character(val) && length(val) == 1 && toupper(val) %in% c("TRUE", "FALSE")) {
      return(identical(toupper(val), "TRUE"))
    }
    stop(context, ": compiled subset runtime ", label, " must be a scalar logical value")
  }
  resolve_bool_scalar <- function(spec, label) {
    if (is.null(spec)) {
      stop(context, ": compiled subset runtime ", label, " metadata is invalid")
    }
    if (!is.list(spec) || is.null(spec$kind)) {
      return(bool_scalar(spec, label))
    }
    if (identical(spec$kind, "const")) {
      return(bool_scalar(spec$value, label))
    }
    if (identical(spec$kind, "arg")) {
      nm <- spec$name
      if (!is.character(nm) || length(nm) != 1 || !nzchar(nm) || is.null(args_in[[nm]])) {
        stop(context, ": compiled subset runtime missing argument for ", label, ": ", nm)
      }
      return(bool_scalar(args_in[[nm]], label))
    }
    stop(context, ": compiled subset runtime ", label, " metadata kind is unsupported: ", spec$kind)
  }

  if (identical(plan$kind, "regex_native")) {
    op <- plan$op
    x_name <- plan$x_name
    if (!is.character(x_name) || length(x_name) != 1 || !nzchar(x_name) || is.null(args_in[[x_name]])) {
      stop(context, ": compiled subset regex runtime missing x argument metadata")
    }
    x <- args_in[[x_name]]
    pattern <- resolve_chr_scalar(plan$pattern, "pattern")
    replacement <- NULL
    if (op %in% c("sub", "gsub")) {
      replacement <- resolve_chr_scalar(plan$replacement, "replacement")
    }
    fixed <- resolve_bool_scalar(plan$fixed, "fixed")
    perl <- resolve_bool_scalar(plan$perl, "perl")
    return(.mojor_tier9_runtime_regex(
      op = op,
      pattern = pattern,
      x = x,
      replacement = replacement,
      fixed = fixed,
      perl = perl,
      context = context
    ))
  }

  if (identical(plan$kind, "expand_grid_native")) {
    arg_entries <- plan$arg_entries
    if (is.null(arg_entries)) {
      arg_names <- plan$arg_names
      if (!is.character(arg_names) || length(arg_names) < 1) {
        stop(context, ": compiled subset expand.grid runtime plan requires at least one argument entry")
      }
      arg_entries <- lapply(arg_names, function(nm) list(kind = "arg", name = nm))
    }
    if (!is.list(arg_entries) || length(arg_entries) < 1) {
      stop(context, ": compiled subset expand.grid runtime plan requires at least one argument entry")
    }
    values <- lapply(arg_entries, function(entry) {
      if (!is.list(entry) || !is.character(entry$kind) || length(entry$kind) != 1) {
        stop(context, ": compiled subset expand.grid runtime encountered invalid argument entry metadata")
      }
      if (identical(entry$kind, "arg")) {
        nm <- entry$name
        if (!is.character(nm) || length(nm) != 1 || !nzchar(nm)) {
          stop(context, ": compiled subset expand.grid runtime argument entry is missing a valid argument name")
        }
        if (is.null(args_in[[nm]])) {
          stop(context, ": compiled subset expand.grid runtime missing argument: ", nm)
        }
        return(args_in[[nm]])
      }
      if (identical(entry$kind, "expr")) {
        expr <- entry$expr
        if (is.character(expr) && length(expr) == 1 && nzchar(expr)) {
          expr <- tryCatch(str2lang(expr), error = function(e) NULL)
        }
        if (is.null(expr)) {
          stop(context, ": compiled subset expand.grid runtime expression entry is invalid")
        }
        eval_env <- list2env(args_in, parent = baseenv())
        value <- tryCatch(
          eval(expr, envir = eval_env),
          error = function(e) {
            stop(context, ": compiled subset expand.grid runtime expression evaluation failed: ", conditionMessage(e))
          }
        )
        return(value)
      }
      stop(context, ": compiled subset expand.grid runtime argument entry kind is unsupported: ", entry$kind)
    })
    arg_count <- length(arg_entries)
    arg_labels <- plan$arg_labels
    if (is.null(arg_labels)) {
      arg_labels <- rep("", arg_count)
    }
    if (!is.character(arg_labels) || length(arg_labels) != arg_count) {
      stop(context, ": compiled subset expand.grid runtime argument labels must align with argument entries")
    }
    return(.mojor_tier9_runtime_expand_grid(values, arg_labels = arg_labels, context = context))
  }

  stop(context, ": unsupported compiled subset runtime plan kind: ", plan$kind)
}

.mojor_expr_srcref <- function(expr) {
  if (is.null(expr)) {
    return(NULL)
  }
  sr <- attr(expr, "srcref")
  if (is.null(sr) && is.call(expr)) {
 # Try to recover from nested call elements.
    for (i in seq_along(expr)) {
      sr <- attr(expr[[i]], "srcref")
      if (!is.null(sr)) break
    }
  }
  sr
}

.mojor_srcref <- function(expr, srcfile = NULL) {
  if (is.null(expr)) {
    return(NULL)
  }
  sr <- .mojor_expr_srcref(expr)
  if (is.null(sr) && !is.null(.mojor_state$current_srcref)) {
    sr <- .mojor_state$current_srcref
  }
  if (is.null(sr)) {
    return(NULL)
  }
  if (!inherits(sr, "srcref")) {
    return(sr)
  }
  if (!is.null(srcfile)) attr(sr, "srcfile") <- srcfile
  sr
}

.mojor_format_srcref <- function(sr) {
  if (is.null(sr) || length(sr) < 4) {
    return(NULL)
  }
  start_line <- sr[[1]]
  start_col <- sr[[2]]
  end_line <- sr[[3]]
  end_col <- sr[[4]]
  if (is.null(start_line) || is.null(start_col)) {
    return(NULL)
  }
  if (!is.null(end_line) && !is.null(end_col) &&
    (end_line != start_line || end_col != start_col)) {
    return(paste0("line ", start_line, ":", start_col, "-", end_line, ":", end_col))
  }
  paste0("line ", start_line, ":", start_col)
}

.mojor_srcref_file <- function(sr) {
  if (is.null(sr)) {
    return(NULL)
  }
  srcfile <- attr(sr, "srcfile")
  if (is.null(srcfile)) {
    return(NULL)
  }
  if (is.list(srcfile) && !is.null(srcfile$filename)) {
    return(srcfile$filename)
  }
  if (is.character(srcfile)) {
    return(srcfile)
  }
  NULL
}

.mojor_signal_error <- function(message, file = NULL, line = NULL, col = NULL, end_line = NULL, end_col = NULL, expr = NULL, severity = "error") {
  cond <- structure(
    list(
      message = message,
      file = file,
      line = line,
      col = col,
      end_line = end_line,
      end_col = end_col,
      severity = severity,
      expr = expr
    ),
    class = c("mojor_error", "error", "condition")
  )
  stop(cond)
}

.mojor_add_diag <- function(severity, message, file = NULL, line = NULL, col = NULL, end_line = NULL, end_col = NULL, expr = NULL) {
  entry <- list(
    severity = severity,
    message = message,
    file = file,
    line = line,
    col = col,
    end_line = end_line,
    end_col = end_col,
    expr = expr
  )
  .mojor_state$diagnostics <- c(.mojor_state$diagnostics, list(entry))
  if (length(.mojor_state$diagnostics_sink) > 0) {
    for (sink in .mojor_state$diagnostics_sink) {
      try(sink(entry), silent = TRUE)
    }
  }
  invisible(entry)
}

.mojor_validate_severity <- function(severity, label) {
  severity <- tolower(as.character(severity))
  if (!severity %in% c("info", "warn", "error")) {
    stop(label, ": severity must be one of info, warn, error")
  }
  severity
}

.mojor_diag_context <- function(msg, expr = NULL, hint = NULL, ensure_prefix = NULL) {
  if (!is.null(ensure_prefix) && !startsWith(msg, ensure_prefix)) {
    msg <- paste0(ensure_prefix, msg)
  }
  if (!is.null(expr)) {
    msg <- paste0(msg, ": ", paste(deparse(expr), collapse = " "))
  }
  sr <- .mojor_expr_srcref(expr)
  if (is.null(sr)) {
    sr <- .mojor_state$current_srcref
  }
  file <- .mojor_srcref_file(sr)
  sr_loc <- .mojor_format_srcref(sr)
  if (!is.null(sr_loc)) {
    if (!is.null(file)) {
      msg <- paste0(msg, " (", file, ", ", sr_loc, ")")
    } else {
      msg <- paste0(msg, " (", sr_loc, ")")
    }
  }
  if (!is.null(hint)) {
    msg <- paste0(msg, " (hint: ", hint, ")")
  }
  line <- NULL
  col <- NULL
  end_line <- NULL
  end_col <- NULL
  if (inherits(sr, "srcref") && length(sr) >= 2) {
    line <- sr[[1]]
    col <- sr[[2]]
    if (length(sr) >= 4) {
      end_line <- sr[[3]]
      end_col <- sr[[4]]
    }
  }
  list(
    message = msg,
    file = file,
    line = line,
    col = col,
    end_line = end_line,
    end_col = end_col
  )
}

.mojor_info <- function(msg, expr = NULL, hint = NULL) {
  ctx <- .mojor_diag_context(msg = msg, expr = expr, hint = hint)
  .mojor_add_diag(
    "info",
    ctx$message,
    file = ctx$file,
    line = ctx$line,
    col = ctx$col,
    end_line = ctx$end_line,
    end_col = ctx$end_col,
    expr = expr
  )
}

# Public wrapper for info diagnostics.
mojor_info <- function(message, expr = NULL, hint = NULL) {
  .mojor_info(message, expr = expr, hint = hint)
}

.mojor_warn <- function(msg, expr = NULL, hint = NULL) {
  ctx <- .mojor_diag_context(msg = msg, expr = expr, hint = hint, ensure_prefix = "mojor: ")
  .mojor_add_diag(
    "warn",
    ctx$message,
    file = ctx$file,
    line = ctx$line,
    col = ctx$col,
    end_line = ctx$end_line,
    end_col = ctx$end_col,
    expr = expr
  )
  mojor_warning(
    ctx$message,
    file = ctx$file,
    line = ctx$line,
    col = ctx$col,
    end_line = ctx$end_line,
    end_col = ctx$end_col,
    expr = expr
  )
}
.mojor_err <- function(msg, expr = NULL, hint = NULL) {
  ctx <- .mojor_diag_context(msg = msg, expr = expr, hint = hint)
  full <- paste0("mojor_transpile: ", ctx$message, " | see SUBSET_V1.md")
  .mojor_signal_error(
    full,
    file = ctx$file,
    line = ctx$line,
    col = ctx$col,
    end_line = ctx$end_line,
    end_col = ctx$end_col,
    expr = expr,
    severity = "error"
  )
}

.mojor_return_ctor_for_mode <- function(return_ctor_kind, strict_ir_only = FALSE, context = "mojor_transpile") {
  return_ctor_kind
}

# Public helper: return srcref metadata for an expression.
mojor_srcref <- function(expr) {
  .mojor_srcref(expr)
}

mojor_error_info <- function(err) {
  if (!inherits(err, "mojor_error")) {
    stop("mojor_error_info: err must be a mojor_error")
  }
  loc <- NULL
  if (!is.null(err$file) && nzchar(err$file)) {
    loc <- err$file
  }
  if (!is.null(err$line) && !is.null(err$col)) {
    span <- paste0("line ", err$line, ":", err$col)
    if (!is.null(err$end_line) && !is.null(err$end_col) &&
      (err$end_line != err$line || err$end_col != err$col)) {
      span <- paste0(span, "-", err$end_line, ":", err$end_col)
    }
    loc <- if (is.null(loc)) span else paste0(loc, ", ", span)
  }
  if (is.null(loc)) {
    return(err$message)
  }
  paste0(err$message, " [", loc, "]")
}

format.mojor_error <- function(x, ...) {
  mojor_error_info(x)
}

mojor_error_as_list <- function(err) {
  if (!inherits(err, "mojor_error")) {
    stop("mojor_error_as_list: err must be a mojor_error")
  }
  list(
    message = err$message,
    file = err$file,
    line = err$line,
    col = err$col,
    end_line = err$end_line,
    end_col = err$end_col,
    severity = err$severity,
    expr = err$expr
  )
}

mojor_error_trace <- function(err, n = 5L) {
  if (!inherits(err, "mojor_error")) {
    stop("mojor_error_trace: err must be a mojor_error")
  }
  n <- as.integer(n)
  if (is.na(n) || n <= 0) {
    stop("mojor_error_trace: n must be a positive integer")
  }
  call <- sys.call(-1)
  trace <- utils::capture.output(base::traceback(n))
  list(
    message = err$message,
    call = call,
    trace = trace
  )
}

mojor_error_print <- function(err, file = stderr()) {
  if (!inherits(err, "mojor_error")) {
    stop("mojor_error_print: err must be a mojor_error")
  }
  msg <- mojor_error_info(err)
  cat(msg, "\n", file = file)
  invisible(err)
}

mojor_error_json <- function(err) {
  if (!inherits(err, "mojor_error")) {
    stop("mojor_error_json: err must be a mojor_error")
  }
  payload <- mojor_error_as_list(err)
  payload$message <- mojor_error_info(err)
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("mojor_error_json: jsonlite not available")
  }
  jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
}

mojor_error_class <- function(err) {
  if (!inherits(err, "mojor_error")) {
    stop("mojor_error_class: err must be a mojor_error")
  }
  class(err)
}

mojor_error_report <- function(err, n = 5L) {
  if (!inherits(err, "mojor_error")) {
    stop("mojor_error_report: err must be a mojor_error")
  }
  info <- mojor_error_info(err)
  trace <- mojor_error_trace(err, n = n)
  list(
    info = info,
    list = mojor_error_as_list(err),
    trace = trace
  )
}

mojor_diagnostics <- function() {
  .mojor_state$diagnostics
}

mojor_diagnostics_filter <- function(severity = NULL) {
  diags <- .mojor_state$diagnostics
  if (is.null(severity)) {
    return(diags)
  }
  keep <- vapply(diags, function(d) identical(d$severity, severity), logical(1))
  diags[keep]
}

mojor_diagnostics_clear <- function() {
  .mojor_state$diagnostics <- list()
  invisible(TRUE)
}

mojor_diagnostics_report <- function(severity = NULL) {
  diags <- mojor_diagnostics_filter(severity)
  if (length(diags) == 0) {
    return(data.frame(
      severity = character(0),
      message = character(0),
      file = character(0),
      line = integer(0),
      col = integer(0),
      end_line = integer(0),
      end_col = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    severity = vapply(diags, function(d) d$severity, character(1)),
    message = vapply(diags, function(d) d$message, character(1)),
    file = vapply(diags, function(d) if (is.null(d$file)) NA_character_ else d$file, character(1)),
    line = vapply(diags, function(d) if (is.null(d$line)) NA_integer_ else as.integer(d$line), integer(1)),
    col = vapply(diags, function(d) if (is.null(d$col)) NA_integer_ else as.integer(d$col), integer(1)),
    end_line = vapply(diags, function(d) if (is.null(d$end_line)) NA_integer_ else as.integer(d$end_line), integer(1)),
    end_col = vapply(diags, function(d) if (is.null(d$end_col)) NA_integer_ else as.integer(d$end_col), integer(1)),
    stringsAsFactors = FALSE
  )
}

mojor_diagnostics_set <- function(diags) {
  if (!is.list(diags)) {
    stop("mojor_diagnostics_set: diags must be a list")
  }
  .mojor_state$diagnostics <- diags
  invisible(TRUE)
}

mojor_diagnostics_add <- function(severity, message, file = NULL, line = NULL, col = NULL, end_line = NULL, end_col = NULL, expr = NULL) {
  severity <- .mojor_validate_severity(severity, "mojor_diagnostics_add")
  .mojor_add_diag(severity, message, file = file, line = line, col = col, end_line = end_line, end_col = end_col, expr = expr)
}

mojor_diagnostics_sink <- function(fn = NULL, severity = NULL) {
  if (is.null(fn)) {
    return(.mojor_state$diagnostics_sink)
  }
  if (!is.function(fn)) {
    stop("mojor_diagnostics_sink: fn must be a function or NULL")
  }
  if (!is.null(severity)) {
    severity <- .mojor_validate_severity(severity, "mojor_diagnostics_sink")
    wrapper <- function(entry) {
      if (identical(entry$severity, severity)) {
        fn(entry)
      }
    }
    .mojor_state$diagnostics_sink <- c(.mojor_state$diagnostics_sink, list(wrapper))
  } else {
    .mojor_state$diagnostics_sink <- c(.mojor_state$diagnostics_sink, list(fn))
  }
  invisible(TRUE)
}

mojor_diagnostics_clear_sinks <- function() {
  .mojor_state$diagnostics_sink <- list()
  invisible(TRUE)
}

mojor_diagnostics_snapshot <- function(severity = NULL) {
  diags <- mojor_diagnostics_filter(severity)
  mojor_diagnostics_clear()
  diags
}

mojor_diagnostics_export <- function(path, format = c("json", "csv"), severity = NULL) {
  format <- match.arg(format)
  df <- mojor_diagnostics_report(severity)
  if (format == "csv") {
    utils::write.csv(df, path, row.names = FALSE)
    return(invisible(path))
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("mojor_diagnostics_export: jsonlite not available")
  }
  jsonlite::write_json(df, path, pretty = TRUE, auto_unbox = TRUE, na = "null")
  invisible(path)
}

mojor_warning <- function(message, file = NULL, line = NULL, col = NULL, end_line = NULL, end_col = NULL, expr = NULL) {
  cond <- structure(
    list(
      message = message,
      file = file,
      line = line,
      col = col,
      end_line = end_line,
      end_col = end_col,
      severity = "warn",
      expr = expr
    ),
    class = c("mojor_warning", "warning", "condition")
  )
  warning(cond)
  invisible(cond)
}

mojor_error_is_fatal <- function(err) {
  if (!inherits(err, "mojor_error")) {
    stop("mojor_error_is_fatal: err must be a mojor_error")
  }
  err$severity %in% c("error", "fatal")
}

as.data.frame.mojor_error <- function(x, ...) {
  data.frame(
    message = x$message,
    file = x$file,
    line = x$line,
    col = x$col,
    end_line = x$end_line,
    end_col = x$end_col,
    severity = x$severity,
    stringsAsFactors = FALSE
  )
}

print.mojor_error <- function(x, ...) {
  mojor_error_print(x, file = stderr())
}

.mojor_is_logical <- function(spec) {
  spec %in% c("lgl", "lgl[]", "bool", "bool[]")
}

.mojor_is_logical_mask_type <- function(spec) {
  if (!is.character(spec) || length(spec) != 1 || is.na(spec)) return(FALSE)
  # 1D: lgl[], bool[]  Matrix: lgl[,], bool[,]  ND: lgl[2d], bool[3d], ...
  grepl("^(lgl|bool)\\[", spec)
}

.mojor_expr_kind <- function(expr, types, loop_vars = NULL) {
  if (is.null(loop_vars)) loop_vars <- character(0)
  loop_vars <- as.character(loop_vars)
  if (is.logical(expr)) {
    return("bool")
  }
  if (is.integer(expr)) {
    return("int")
  }
  if (is.numeric(expr)) {
    return("float")
  }
  if (is.name(expr)) {
    name <- as.character(expr)
    if (name %in% c("TRUE", "FALSE")) {
      return("bool")
    }
    if (!is.null(.mojor_state$current_iter_map) && !is.null(.mojor_state$current_iter_map[[name]])) {
      iter_entry <- .mojor_state$current_iter_map[[name]]
      if (!is.null(iter_entry$spec)) {
        spec <- iter_entry$spec
        if (.mojor_is_logical(spec)) {
          return("bool")
        }
        if (spec %in% c("i32")) {
          return("int")
        }
        if (spec %in% c("f64", "f32")) {
          return("float")
        }
      }
      src <- iter_entry$source
      if (!is.null(types[[src]]) && .mojor_is_logical(types[[src]])) {
        return("bool")
      }
      if (!is.null(types[[src]]) && types[[src]] %in% c("i32[]")) {
        return("int")
      }
      if (!is.null(types[[src]]) && types[[src]] %in% c("f64[]", "f32[]")) {
        return("float")
      }
    }
    local_spec <- if (!is.null(.mojor_state$current_local_types)) .mojor_state$current_local_types[[name]] else NULL
    spec <- if (!is.null(types[[name]])) types[[name]] else local_spec
    if (!is.null(spec) && .mojor_is_logical(spec)) {
      return("bool")
    }
    if (!is.null(spec) && spec %in% c("i32")) {
      return("int")
    }
    if (!is.null(spec) && spec %in% c("f64", "f32")) {
      return("float")
    }
    return("unknown")
  }
  if (is.call(expr)) {
    op <- as.character(expr[[1]])
    if (op == "(" && length(expr) == 2) {
      return(.mojor_expr_kind(expr[[2]], types))
    }
    if (op == "as.integer") {
      return("int")
    }
    if (op == "as.double") {
      return("float")
    }
    if (op == "as.single") {
      return("float")
    }
    if (op == "as.logical") {
      return("bool")
    }
    if (op == "xor" && length(expr) == 3) {
      return("bool")
    }
    if (op == "ifelse") {
      if (length(expr) < 4) {
        return("unknown")
      }
      k_yes <- .mojor_expr_kind(expr[[3]], types)
      k_no <- .mojor_expr_kind(expr[[4]], types)
      if (k_yes == "float" || k_no == "float") {
        return("float")
      }
      if (k_yes == "int" || k_no == "int") {
        return("int")
      }
      if (k_yes == "bool" || k_no == "bool") {
        return("bool")
      }
      return("unknown")
    }
    if (op == "abs") {
      k <- .mojor_expr_kind(expr[[2]], types)
      if (k == "unknown") {
        return("unknown")
      }
      if (k == "int") {
        return("int")
      }
      if (k == "bool") {
        return("int")
      }
      if (k == "float") {
        return("float")
      }
      return("unknown")
    }
    if (op %in% c("floor", "ceiling", "trunc", "round", "sign", "cbrt", "lgamma", "erf", "gamma")) {
      k <- .mojor_expr_kind(expr[[2]], types)
      if (k == "unknown") {
        return("unknown")
      }
      return("float")
    }
    if (op %in% c("!", "&&", "||", "&", "|")) {
      return("bool")
    }
    if (op %in% c(
      "sqrt", "exp", "log", "sin", "cos", "tan", "asin", "acos", "atan",
      "sinh", "cosh", "tanh", "log1p", "expm1"
    )) {
      k <- .mojor_expr_kind(expr[[2]], types)
      if (k == "unknown") {
        return("unknown")
      }
      return("float")
    }
    if (op == "hypot") {
      k1 <- .mojor_expr_kind(expr[[2]], types)
      k2 <- .mojor_expr_kind(expr[[3]], types)
      if (k1 == "unknown" || k2 == "unknown") {
        return("unknown")
      }
      return("float")
    }
    if (op %in% c("min", "max", "pmin", "pmax")) {
      if (length(expr) < 3) {
        return("unknown")
      }
      kinds <- vapply(as.list(expr)[-1], function(e) .mojor_expr_kind(e, types), character(1))
      if (any(kinds == "unknown")) {
        return("unknown")
      }
      if (any(kinds == "float")) {
        return("float")
      }
      if (any(kinds == "int")) {
        return("int")
      }
      if (any(kinds == "bool")) {
        return("int")
      }
      return("unknown")
    }
    if (op %in% c("rep", "rep_len", "rep.int")) {
      if (length(expr) < 2) {
        return("unknown")
      }
      parts <- as.list(expr)[-1]
      nms <- names(parts)
      get_ctor_arg <- function(nm, pos) {
        if (!is.null(nms) && nm %in% nms) {
          return(parts[[which(nms == nm)[1]]])
        }
        if (length(parts) >= pos && (is.null(nms) || is.null(nms[[pos]]) || nms[[pos]] == "")) {
          return(parts[[pos]])
        }
        NULL
      }
      val <- get_ctor_arg("x", 1)
      if (is.null(val)) {
        return("unknown")
      }
      k <- .mojor_expr_kind(val, types)
      if (k == "unknown" && is.name(val)) {
        name <- as.character(val)
        spec <- types[[name]]
        if (!is.null(spec) && .mojor_is_array(spec)) {
          if (spec %in% c("f64[]", "f32[]")) {
            return("float")
          }
          if (spec %in% c("i32[]")) {
            return("int")
          }
          if (spec %in% c("lgl[]", "bool[]")) {
            return("bool")
          }
        }
      }
      return(k)
    }
    if (op == "c") {
      if (length(expr) < 2) {
        return("unknown")
      }
      kinds <- vapply(as.list(expr)[-1], function(e) {
        k <- .mojor_expr_kind(e, types)
        if (k == "unknown" && is.name(e)) {
          name <- as.character(e)
          spec <- types[[name]]
          if (!is.null(spec) && .mojor_is_array(spec)) {
            if (spec %in% c("f64[]", "f32[]")) {
              return("float")
            }
            if (spec %in% c("i32[]")) {
              return("int")
            }
            if (spec %in% c("lgl[]", "bool[]")) {
              return("bool")
            }
          }
        }
        k
      }, character(1))
      if (any(kinds == "unknown")) {
        return("unknown")
      }
      if (any(kinds == "float")) {
        return("float")
      }
      if (any(kinds == "int")) {
        return("int")
      }
      if (any(kinds == "bool")) {
        return("int")
      }
      return("unknown")
    }
    if (op == "!" && length(expr) == 2) {
      return("bool")
    }
    if (op %in% c("&&", "||", "&", "|")) {
      return("bool")
    }
    if (op %in% c("is.na", "is.nan", "is.finite", "is.infinite")) {
      return("bool")
    }
    if (op %in% c(">", "<", ">=", "<=", "==", "!=")) {
      return("bool")
    }
    if (op %in% c("[", "[[")) {
      if (is.call(expr[[2]])) {
        inner_op <- as.character(expr[[2]][[1]])
        if (inner_op %in% c("rep", "rep_len", "rep.int", "c")) {
          if (length(loop_vars) == 0) {
            return(.mojor_expr_kind(expr[[2]], types, loop_vars = loop_vars))
          }
          idx_expr <- expr[[3]]
          idx_info <- .mojor_index_expr_info(idx_expr, loop_vars)
          if (is.null(idx_info)) {
            stop("Indexing must use a loop variable (optionally +/- integer literal, scalar variable, or multiplied/divided by integer)")
          }
          if (inner_op == "c") {
            parts <- as.list(expr[[2]])[-1]
            out <- concat_expr(parts, idx_info$expr)
            if (is.null(out)) .mojor_err("c() supports scalar literals and array names only", expr[[2]])
            return(out)
          }
          .mojor_warn(paste0(inner_op, "() lowered to recycling"), expr[[2]], "length.out/times ignored; uses output length")
          inner_ctor <- expr[[2]]
          inner_parts <- as.list(inner_ctor)[-1]
          inner_nms <- names(inner_parts)
          get_inner_ctor_arg <- function(nm, pos) {
            if (!is.null(inner_nms) && nm %in% inner_nms) {
              return(inner_parts[[which(inner_nms == nm)[1]]])
            }
            if (length(inner_parts) >= pos && (is.null(inner_nms) || is.null(inner_nms[[pos]]) || inner_nms[[pos]] == "")) {
              return(inner_parts[[pos]])
            }
            NULL
          }
          if (inner_op == "rep_len") {
            len_arg <- get_inner_ctor_arg("length.out", 2)
            if (is.null(len_arg)) .mojor_err("rep_len() requires length.out", expr[[2]])
            len_expr <- len_arg_expr(len_arg)
            if (is.null(len_expr)) .mojor_err("rep_len() length.out must be an integer literal or scalar arg", expr[[2]])
            if (is.numeric(len_arg) && length(len_arg) == 1) {
              len_spec <- list(kind = "rep_len", len_value = as.integer(len_arg))
            } else {
              len_spec <- list(kind = "rep_len", len_name = len_expr)
            }
            if (!isTRUE(.mojor_state$current_suppress_len_checks)) {
              .mojor_state$current_len_checks_c <- unique(c(.mojor_state$current_len_checks_c, list(len_spec)))
            }
          }
          val_expr <- get_inner_ctor_arg("x", 1)
          if (is.null(val_expr)) .mojor_err(paste0(inner_op, "() requires a value"), expr[[2]])
          out <- rep_elem_expr(val_expr, idx_info$expr)
          if (is.null(out)) .mojor_err(paste0(inner_op, "() only supports scalar or array arguments"), expr[[2]])
          return(out)
        }
      }
      if (!is.name(expr[[2]])) {
        return("unknown")
      }
      var <- as.character(expr[[2]])
      idxs <- as.list(expr)[3:length(expr)]
      if (length(idxs) > 0 && isTRUE(.mojor_state$options$index_bounds)) {
        for (ix in idxs) {
          if (!is.name(ix)) next
          nm <- as.character(ix)
          if (!is.null(.mojor_state$current_len_var_map) &&
            !is.null(names(.mojor_state$current_len_var_map)) &&
            nm %in% names(.mojor_state$current_len_var_map)) {
            .mojor_state$current_len_hint[[var]] <- .mojor_state$current_len_var_map[[nm]]
          }
        }
      }
      if (!is.null(types[[var]]) && .mojor_is_logical(types[[var]])) {
        return("bool")
      }
      if (!is.null(types[[var]]) && types[[var]] %in% c("i32[]")) {
        return("int")
      }
      if (!is.null(types[[var]]) && types[[var]] %in% c("f64[]", "f32[]")) {
        return("float")
      }
      return("unknown")
    }
    if (op %in% c("+", "-", "*", "^")) {
      if (length(expr) < 3) {
        return("unknown")
      }
      k1 <- .mojor_expr_kind(expr[[2]], types)
      k2 <- .mojor_expr_kind(expr[[3]], types)
      if (k1 == "unknown" || k2 == "unknown") {
        return("unknown")
      }
      if (k1 == "bool") k1 <- "int"
      if (k2 == "bool") k2 <- "int"
      if (op == "^") {
        return("float")
      }
      if (k1 == "float" || k2 == "float") {
        return("float")
      }
      if (k1 == "int" || k2 == "int") {
        return("int")
      }
      if (k1 == "bool" || k2 == "bool") {
        return("int")
      }
      return("unknown")
    }
    if (op %in% c("%/%", "%%")) {
      if (length(expr) < 3) {
        return("unknown")
      }
      k1 <- .mojor_expr_kind(expr[[2]], types)
      k2 <- .mojor_expr_kind(expr[[3]], types)
      if (k1 == "unknown" || k2 == "unknown") {
        return("unknown")
      }
      if (k1 == "bool") k1 <- "int"
      if (k2 == "bool") k2 <- "int"
      if (k1 == "float" || k2 == "float") {
        return("float")
      }
      if (k1 == "int" || k2 == "int") {
        return("int")
      }
      if (k1 == "bool" || k2 == "bool") {
        return("int")
      }
      return("unknown")
    }
    if (op == "/") {
      return("float")
    }
  }
  "unknown"
}

.mojor_call_op_name <- function(expr) {
  if (!is.call(expr) || length(expr) < 1) {
    return(NULL)
  }
  op <- as.character(expr[[1]])
  if (!is.character(op) || length(op) == 0) {
    return(NULL)
  }
  op[[1]]
}

.mojor_call_args <- function(expr) {
  if (!is.call(expr) || length(expr) < 2) {
    return(list())
  }
  as.list(expr)[-1]
}

.mojor_expr_contains_ifelse <- function(expr) {
  op <- .mojor_call_op_name(expr)
  if (!is.null(op)) {
    if (identical(op, "ifelse")) {
      return(TRUE)
    }
    for (p in .mojor_call_args(expr)) {
      if (.mojor_expr_contains_ifelse(p)) {
        return(TRUE)
      }
    }
  }
  FALSE
}


.mojor_expr_has_math_fn <- function(expr, fns) {
  op <- .mojor_call_op_name(expr)
  if (!is.null(op)) {
    if (op %in% fns || identical(op, "^")) {
      return(TRUE)
    }
    for (p in .mojor_call_args(expr)) {
      if (.mojor_expr_has_math_fn(p, fns)) {
        return(TRUE)
      }
    }
  }
  FALSE
}

.mojor_find_used_math_fns <- function(expr, fns) {
  found <- character(0)
  op <- .mojor_call_op_name(expr)
  if (!is.null(op)) {
    if (op %in% fns) found <- c(found, op)
    parts <- .mojor_call_args(expr)
    for (p in parts) {
 # Skip empty names (from trailing commas like x[i, ])
      is_empty <- tryCatch(
        {
          is.name(p) && as.character(p) == ""
        },
        error = function(e) TRUE
      )
      if (is_empty) {
        next
      }
      found <- c(found, .mojor_find_used_math_fns(p, fns))
    }
  }
  unique(found)
}

.mojor_out_ref_indices <- function(expr, out_name) {
  idxs <- character(0)
  if (is.call(expr)) {
    op <- as.character(expr[[1]])
    if (op %in% c("[", "[[") && length(expr) >= 3) {
      var <- expr[[2]]
      if (is.name(var) && as.character(var) == out_name) {
        for (k in 3:length(expr)) {
          idx <- expr[[k]]
          if (is.name(idx)) {
            idxs <- c(idxs, as.character(idx))
          } else {
            idxs <- c(idxs, "<nonloop>")
          }
        }
      }
    }
 # Iterate over expression elements directly to avoid issues with empty names
 # from trailing commas like x[i, ] which create empty name objects
    for (i in 2:length(expr)) {
      p <- expr[[i]]
 # Skip empty names (from trailing commas like x[i, ])
 # Use tryCatch to handle the case where is.name() fails on empty names
      is_empty <- tryCatch(
        {
          is.name(p) && as.character(p) == ""
        },
        error = function(e) TRUE
      )
      if (is_empty) {
        next
      }
      if (!is.null(p)) {
        idxs <- c(idxs, .mojor_out_ref_indices(p, out_name))
      }
    }
  }
  unique(idxs)
}

.mojor_out_ref_ok <- function(expr, out_name, loop_var) {
  refs <- .mojor_out_ref_indices(expr, out_name)
  if (length(refs) == 0) {
    return(TRUE)
  }
  all(refs == loop_var)
}

.mojor_gpu_elementwise_array_spec_ok <- function(spec, dtype = NULL) {
  if (is.null(spec) || !.mojor_is_array(spec)) {
    return(FALSE)
  }
  rank <- .mojor_type_ndim(spec)
  is_rank2 <- .mojor_is_matrix(spec) || identical(rank, 2L)
  is_rank1 <- !is_rank2 && identical(rank, 1L)
  if (!(is_rank1 || is_rank2)) {
    return(FALSE)
  }
  base <- .mojor_type_base(spec)
  if (is.null(dtype)) {
    return(base %in% c("f32", "f64"))
  }
  dtype <- match.arg(dtype, c("f32", "f64"))
  if (identical(dtype, "f64")) {
    return(identical(base, "f64"))
  }
  identical(base, "f32")
}

.mojor_gpu_elementwise_out_dtype <- function(out_type) {
  if (is.null(out_type) || !.mojor_is_array(out_type)) {
    return(NULL)
  }
  rank <- .mojor_type_ndim(out_type)
  is_rank2 <- .mojor_is_matrix(out_type) || identical(rank, 2L)
  is_rank1 <- !is_rank2 && identical(rank, 1L)
  if (!(is_rank1 || is_rank2)) {
    return(NULL)
  }
  base <- .mojor_type_base(out_type)
  if (identical(base, "f64")) {
    return("f64")
  }
  if (identical(base, "f32")) {
    return("f32")
  }
  NULL
}

.mojor_gpu_matrix_array_spec_ok <- function(spec, dtype = NULL) {
  if (is.null(spec) || !.mojor_is_array(spec) ||
      !(.mojor_is_matrix(spec) || identical(.mojor_type_ndim(spec), 2L))) {
    return(FALSE)
  }
  base <- .mojor_type_base(spec)
  if (is.null(dtype)) {
    return(base %in% c("f32", "f64"))
  }
  dtype <- match.arg(dtype, c("f32", "f64"))
  if (identical(dtype, "f64")) {
    return(identical(base, "f64"))
  }
  identical(base, "f32")
}

.mojor_gpu_matrix2d_unwrap_index_expr <- function(expr, depth = 0L) {
  if (is.null(expr) || depth > 24L) {
    return(expr)
  }
  cur <- expr
  repeat {
    if (is.call(cur) && length(cur) == 2L && identical(as.character(cur[[1L]]), "(")) {
      cur <- cur[[2L]]
      next
    }
    if (is.call(cur) && length(cur) == 2L && identical(as.character(cur[[1L]]), "as.integer")) {
      cur <- cur[[2L]]
      next
    }
    break
  }
  cur
}

.mojor_gpu_matrix2d_parse_dim_source <- function(dim_expr, arg_specs, axis = c("nrow", "ncol")) {
  axis <- match.arg(axis)
  axis_idx <- if (identical(axis, "nrow")) 1L else 2L
  expr <- .mojor_gpu_matrix2d_unwrap_index_expr(dim_expr)

  parse_name_ref <- function(ref_node) {
    if (!is.name(ref_node)) {
      return(NULL)
    }
    name <- as.character(ref_node)
    spec <- arg_specs[[name]]
    if (is.null(spec) || !.mojor_is_array(spec)) {
      return(NULL)
    }
    list(name = name, axis = axis)
  }

  if (is.call(expr) && identical(as.character(expr[[1L]]), axis) && length(expr) == 2L) {
    return(parse_name_ref(expr[[2L]]))
  }

  if (is.call(expr) && as.character(expr[[1L]]) %in% c("[", "[[")) {
    if (length(expr) < 3L) {
      return(NULL)
    }
    base <- .mojor_gpu_matrix2d_unwrap_index_expr(expr[[2L]])
    if (!(is.call(base) && identical(as.character(base[[1L]]), "dim") && length(base) == 2L)) {
      return(NULL)
    }
    hit <- parse_name_ref(base[[2L]])
    if (is.null(hit)) {
      return(NULL)
    }
    idx_expr <- .mojor_gpu_matrix2d_unwrap_index_expr(expr[[3L]])
    idx_val <- .mojor_gpu_matrix2d_int_literal(idx_expr)
    if (is.null(idx_val) || idx_val != axis_idx) {
      return(NULL)
    }
    return(hit)
  }

  NULL
}

.mojor_gpu_seq_len_dim_source <- function(seq_expr, arg_specs, axis = c("nrow", "ncol")) {
  axis <- match.arg(axis)
  seq_expr <- .mojor_gpu_matrix2d_unwrap_index_expr(seq_expr)
  if (!is.call(seq_expr) || !identical(as.character(seq_expr[[1]]), "seq_len") || length(seq_expr) != 2) {
    return(NULL)
  }
  .mojor_gpu_matrix2d_parse_dim_source(seq_expr[[2L]], arg_specs = arg_specs, axis = axis)
}

.mojor_gpu_matrix2d_int_literal <- function(node) {
  if (!(is.numeric(node) || is.integer(node)) || length(node) != 1L || is.na(node) || !is.finite(node)) {
    return(NULL)
  }
  v <- as.numeric(node)
  if (v != floor(v)) {
    return(NULL)
  }
  as.integer(v)
}

.mojor_gpu_matrix2d_int_constant <- function(node, const_int_env = NULL, depth = 0L) {
  if (is.null(node) || depth > 24L) {
    return(NULL)
  }
  lit <- .mojor_gpu_matrix2d_int_literal(node)
  if (!is.null(lit)) {
    return(lit)
  }
  if (is.name(node)) {
    nm <- as.character(node)
    if (!is.null(const_int_env[[nm]])) {
      return(as.integer(const_int_env[[nm]]))
    }
    return(NULL)
  }
  if (!is.call(node)) {
    return(NULL)
  }
  op <- as.character(node[[1]])
  if (op == "(" && length(node) == 2L) {
    return(.mojor_gpu_matrix2d_int_constant(node[[2]], const_int_env = const_int_env, depth = depth + 1L))
  }
  if (op == "as.integer" && length(node) == 2L) {
    return(.mojor_gpu_matrix2d_int_constant(node[[2]], const_int_env = const_int_env, depth = depth + 1L))
  }
  if (op %in% c("+", "-") && length(node) == 2L) {
    inner <- .mojor_gpu_matrix2d_int_constant(node[[2]], const_int_env = const_int_env, depth = depth + 1L)
    if (is.null(inner)) {
      return(NULL)
    }
    if (op == "+") {
      return(as.integer(inner))
    }
    return(as.integer(-inner))
  }
  if (!(op %in% c("+", "-", "*", "%/%")) || length(node) != 3L) {
    return(NULL)
  }
  lhs <- .mojor_gpu_matrix2d_int_constant(node[[2]], const_int_env = const_int_env, depth = depth + 1L)
  rhs <- .mojor_gpu_matrix2d_int_constant(node[[3]], const_int_env = const_int_env, depth = depth + 1L)
  if (is.null(lhs) || is.null(rhs)) {
    return(NULL)
  }
  if (op == "+") {
    return(as.integer(lhs + rhs))
  }
  if (op == "-") {
    return(as.integer(lhs - rhs))
  }
  if (op == "*") {
    return(as.integer(lhs * rhs))
  }
  if (rhs == 0L) {
    return(NULL)
  }
  as.integer(lhs %/% rhs)
}

.mojor_gpu_matrix2d_const_int_env <- function() {
  local_scalars <- tryCatch(.mojor_state$current_scalar_inits, error = function(e) NULL)
  if (is.null(local_scalars) || !is.list(local_scalars) || length(local_scalars) == 0L) {
    return(list())
  }
  int_env <- list()
  parse_int32_scalar <- function(val) {
    if ((is.numeric(val) || is.integer(val)) && length(val) == 1L && !is.na(val) && is.finite(val)) {
      if (as.numeric(val) == floor(as.numeric(val))) {
        return(as.integer(val))
      }
      return(NULL)
    }
    if (is.character(val) && length(val) == 1L) {
      m <- regmatches(val, regexec("^Int(?:32)?\\((-?[0-9]+)\\)$", val, perl = TRUE))[[1L]]
      if (length(m) == 2L) {
        return(as.integer(m[[2L]]))
      }
      return(NULL)
    }
    NULL
  }
  pending <- names(local_scalars)
  if (is.null(pending)) {
    return(list())
  }
  pending <- pending[vapply(pending, function(nm) {
    entry <- local_scalars[[nm]]
    is.list(entry) && identical(entry$type, "i32")
  }, logical(1))]
  if (length(pending) == 0L) {
    return(list())
  }
  repeat {
    changed <- FALSE
    for (nm in pending) {
      if (!is.null(int_env[[nm]])) {
        next
      }
      entry <- local_scalars[[nm]]
      if (!is.list(entry) || !identical(entry$type, "i32")) {
        next
      }
      val <- parse_int32_scalar(entry$value)
      if (is.null(val) && is.list(entry$value) && identical(entry$value$kind, "expr") &&
          !is.null(entry$value$expr)) {
        val <- .mojor_gpu_matrix2d_int_constant(entry$value$expr, const_int_env = int_env)
      }
      if (!is.null(val)) {
        int_env[[nm]] <- as.integer(val)
        changed <- TRUE
      }
    }
    if (!changed) {
      break
    }
  }
  int_env
}

.mojor_gpu_matrix2d_parse_offset <- function(idx_expr, loop_var, const_int_env = NULL) {
  normalize_term <- function(node, depth = 0L) {
    node <- .mojor_gpu_matrix2d_unwrap_index_expr(node)
    if (is.null(node) || depth > 24L) {
      return(NULL)
    }
    if (is.numeric(node) || is.integer(node)) {
      lit <- .mojor_gpu_matrix2d_int_literal(node)
      if (!is.null(lit)) {
        return(as.integer(lit))
      }
      return(NULL)
    }
    if (is.name(node)) {
      nm <- as.character(node)
      if (identical(nm, loop_var)) {
        return(NULL)
      }
      if (!is.null(const_int_env[[nm]])) {
        return(as.integer(const_int_env[[nm]]))
      }
      return(as.name(nm))
    }
    if (!is.call(node)) {
      return(NULL)
    }
    op <- as.character(node[[1L]])
    if (op %in% c("(", "as.integer") && length(node) == 2L) {
      return(normalize_term(node[[2L]], depth = depth + 1L))
    }
    if (op %in% c("+", "-") && length(node) == 2L) {
      inner <- normalize_term(node[[2L]], depth = depth + 1L)
      if (is.null(inner)) {
        return(NULL)
      }
      if (identical(op, "+")) {
        return(inner)
      }
      return(call("-", inner))
    }
    if (!(op %in% c("+", "-", "*", "%/%", "%%")) || length(node) != 3L) {
      return(NULL)
    }
    lhs <- normalize_term(node[[2L]], depth = depth + 1L)
    rhs <- normalize_term(node[[3L]], depth = depth + 1L)
    if (is.null(lhs) || is.null(rhs)) {
      return(NULL)
    }
    as.call(list(as.name(op), lhs, rhs))
  }

  make_literal <- function(offset_i) {
    offset_i <- as.integer(offset_i)
    expr <- if (offset_i == 0L) {
      as.name(loop_var)
    } else if (offset_i > 0L) {
      call("+", as.name(loop_var), as.integer(offset_i))
    } else {
      call("-", as.name(loop_var), as.integer(abs(offset_i)))
    }
    list(ok = TRUE, offset = offset_i, kind = "literal", expr = expr)
  }

  idx_expr <- .mojor_gpu_matrix2d_unwrap_index_expr(idx_expr)
  if (is.name(idx_expr)) {
    if (identical(as.character(idx_expr), loop_var)) {
      return(make_literal(0L))
    }
    return(list(ok = FALSE, kind = "irregular"))
  }
  if (!is.call(idx_expr)) {
    return(list(ok = FALSE, kind = "irregular"))
  }
  op <- as.character(idx_expr[[1L]])
  if (!(op %in% c("+", "-")) || length(idx_expr) != 3L) {
    return(list(ok = FALSE, kind = "irregular"))
  }
  lhs <- .mojor_gpu_matrix2d_unwrap_index_expr(idx_expr[[2L]])
  rhs <- .mojor_gpu_matrix2d_unwrap_index_expr(idx_expr[[3L]])
  lhs_is_var <- is.name(lhs) && identical(as.character(lhs), loop_var)
  rhs_is_var <- is.name(rhs) && identical(as.character(rhs), loop_var)
  lhs_int <- .mojor_gpu_matrix2d_int_constant(lhs, const_int_env = const_int_env)
  rhs_int <- .mojor_gpu_matrix2d_int_constant(rhs, const_int_env = const_int_env)

  if (identical(op, "+")) {
    if (lhs_is_var && !is.null(rhs_int)) {
      return(make_literal(rhs_int))
    }
    if (rhs_is_var && !is.null(lhs_int)) {
      return(make_literal(lhs_int))
    }
  } else if (lhs_is_var && !is.null(rhs_int)) {
    return(make_literal(as.integer(-rhs_int)))
  }

  if (identical(op, "+") && lhs_is_var) {
    rhs_term <- normalize_term(rhs)
    if (!is.null(rhs_term)) {
      rhs_term_int <- .mojor_gpu_matrix2d_int_constant(rhs_term, const_int_env = const_int_env)
      if (!is.null(rhs_term_int)) {
        return(make_literal(rhs_term_int))
      }
      return(list(
        ok = TRUE,
        offset = NULL,
        kind = "dynamic",
        expr = call("+", as.name(loop_var), rhs_term)
      ))
    }
  }
  if (identical(op, "+") && rhs_is_var) {
    lhs_term <- normalize_term(lhs)
    if (!is.null(lhs_term)) {
      lhs_term_int <- .mojor_gpu_matrix2d_int_constant(lhs_term, const_int_env = const_int_env)
      if (!is.null(lhs_term_int)) {
        return(make_literal(lhs_term_int))
      }
      return(list(
        ok = TRUE,
        offset = NULL,
        kind = "dynamic",
        expr = call("+", as.name(loop_var), lhs_term)
      ))
    }
  }
  if (identical(op, "-") && lhs_is_var) {
    rhs_term <- normalize_term(rhs)
    if (!is.null(rhs_term)) {
      rhs_term_int <- .mojor_gpu_matrix2d_int_constant(rhs_term, const_int_env = const_int_env)
      if (!is.null(rhs_term_int)) {
        return(make_literal(as.integer(-rhs_term_int)))
      }
      return(list(
        ok = TRUE,
        offset = NULL,
        kind = "dynamic",
        expr = call("-", as.name(loop_var), rhs_term)
      ))
    }
  }

  if (lhs_is_var || rhs_is_var) {
    return(list(ok = FALSE, kind = "nonliteral"))
  }
  list(ok = FALSE, kind = "irregular")
}

.mojor_gpu_matrix2d_guard_negate_atom <- function(node) {
  atom <- .mojor_gpu_matrix2d_unwrap_index_expr(node)
  if (!is.call(atom) || length(atom) != 3L) {
    return(NULL)
  }
  op <- as.character(atom[[1L]])
  inv <- switch(
    op,
    ">=" = "<",
    ">" = "<=",
    "<=" = ">",
    "<" = ">=",
    NULL
  )
  if (is.null(inv)) {
    return(NULL)
  }
  as.call(list(as.name(inv), atom[[2L]], atom[[3L]]))
}

.mojor_gpu_matrix2d_guard_normalize <- function(node, negated = FALSE, depth = 0L) {
  if (is.null(node) || depth > 32L) {
    return(NULL)
  }
  node <- .mojor_gpu_matrix2d_unwrap_index_expr(node)
  if (!is.call(node)) {
    return(NULL)
  }
  op <- as.character(node[[1L]])
  if (op == "!" && length(node) == 2L) {
    return(.mojor_gpu_matrix2d_guard_normalize(
      node[[2L]],
      negated = !isTRUE(negated),
      depth = depth + 1L
    ))
  }
  if (op %in% c("(", "as.integer") && length(node) == 2L) {
    return(.mojor_gpu_matrix2d_guard_normalize(
      node[[2L]],
      negated = isTRUE(negated),
      depth = depth + 1L
    ))
  }
  if (op %in% c(">=", ">", "<=", "<") && length(node) == 3L) {
    if (!isTRUE(negated)) {
      return(node)
    }
    return(.mojor_gpu_matrix2d_guard_negate_atom(node))
  }
  if (!(op %in% c("&&", "&", "||", "|")) || length(node) != 3L) {
    return(NULL)
  }
  lhs <- .mojor_gpu_matrix2d_guard_normalize(
    node[[2L]],
    negated = isTRUE(negated),
    depth = depth + 1L
  )
  rhs <- .mojor_gpu_matrix2d_guard_normalize(
    node[[3L]],
    negated = isTRUE(negated),
    depth = depth + 1L
  )
  if (is.null(lhs) || is.null(rhs)) {
    return(NULL)
  }
  if (!isTRUE(negated)) {
    op_out <- if (op %in% c("&&", "&")) "&&" else "||"
    return(as.call(list(as.name(op_out), lhs, rhs)))
  }
  op_out <- if (op %in% c("&&", "&")) "||" else "&&"
  as.call(list(as.name(op_out), lhs, rhs))
}

.mojor_gpu_matrix2d_guard_has_or <- function(node) {
  node <- .mojor_gpu_matrix2d_unwrap_index_expr(node)
  if (!is.call(node)) {
    return(FALSE)
  }
  op <- as.character(node[[1L]])
  if (op %in% c("||", "|")) {
    return(TRUE)
  }
  if (length(node) <= 1L) {
    return(FALSE)
  }
  any(vapply(as.list(node)[-1L], .mojor_gpu_matrix2d_guard_has_or, logical(1)))
}

.mojor_gpu_matrix2d_parse_dim_bound <- function(expr, axis = c("nrow", "ncol"), matrix_source,
                                                const_int_env = NULL) {
  axis <- match.arg(axis)
  if (is.call(expr) && identical(as.character(expr[[1]]), "(") && length(expr) == 2L) {
    return(.mojor_gpu_matrix2d_parse_dim_bound(
      expr[[2]],
      axis = axis,
      matrix_source = matrix_source,
      const_int_env = const_int_env
    ))
  }
  is_dim_call <- function(node) {
    is.call(node) &&
      identical(as.character(node[[1]]), axis) &&
      length(node) == 2 &&
      is.name(node[[2]]) &&
      identical(as.character(node[[2]]), matrix_source)
  }
  if (is_dim_call(expr)) {
    return(list(kind = "dim", offset = 0L))
  }
  if (!is.call(expr) || length(expr) != 3 || !(as.character(expr[[1]]) %in% c("+", "-"))) {
    return(NULL)
  }
  op <- as.character(expr[[1]])
  lhs <- expr[[2]]
  rhs <- expr[[3]]
  lhs_dim <- is_dim_call(lhs)
  rhs_dim <- is_dim_call(rhs)
  lhs_int <- .mojor_gpu_matrix2d_int_constant(lhs, const_int_env = const_int_env)
  rhs_int <- .mojor_gpu_matrix2d_int_constant(rhs, const_int_env = const_int_env)
  if (op == "+") {
    if (lhs_dim && !is.null(rhs_int)) {
      return(list(kind = "dim", offset = rhs_int))
    }
    if (rhs_dim && !is.null(lhs_int)) {
      return(list(kind = "dim", offset = lhs_int))
    }
  } else {
    if (lhs_dim && !is.null(rhs_int)) {
      return(list(kind = "dim", offset = as.integer(-rhs_int)))
    }
  }
  NULL
}

.mojor_gpu_matrix2d_parse_var_term <- function(expr, outer_var, inner_var, const_int_env = NULL) {
  parse_one <- function(node, depth = 0L) {
    if (depth > 24L) {
      return(NULL)
    }
    if (is.name(node)) {
      nm <- as.character(node)
      if (identical(nm, outer_var)) {
        return(list(var = "i", offset = 0L))
      }
      if (identical(nm, inner_var)) {
        return(list(var = "j", offset = 0L))
      }
      return(NULL)
    }
    if (!is.call(node)) {
      return(NULL)
    }
    op <- as.character(node[[1]])
    if (op == "(" && length(node) == 2L) {
      return(parse_one(node[[2]], depth = depth + 1L))
    }
    if (!(op %in% c("+", "-")) || length(node) != 3L) {
      return(NULL)
    }
    lhs <- node[[2]]
    rhs <- node[[3]]
    lhs_term <- parse_one(lhs, depth = depth + 1L)
    rhs_term <- parse_one(rhs, depth = depth + 1L)
    lhs_int <- .mojor_gpu_matrix2d_int_constant(lhs, const_int_env = const_int_env, depth = depth + 1L)
    rhs_int <- .mojor_gpu_matrix2d_int_constant(rhs, const_int_env = const_int_env, depth = depth + 1L)
    if (op == "+") {
      if (!is.null(lhs_term) && is.null(rhs_term) && !is.null(rhs_int)) {
        return(list(var = lhs_term$var, offset = as.integer(lhs_term$offset + rhs_int)))
      }
      if (!is.null(rhs_term) && is.null(lhs_term) && !is.null(lhs_int)) {
        return(list(var = rhs_term$var, offset = as.integer(rhs_term$offset + lhs_int)))
      }
      return(NULL)
    }
    if (!is.null(lhs_term) && is.null(rhs_term) && !is.null(rhs_int)) {
      return(list(var = lhs_term$var, offset = as.integer(lhs_term$offset - rhs_int)))
    }
    NULL
  }
  parse_one(expr, depth = 0L)
}

.mojor_gpu_matrix2d_guard_bound_empty <- function() {
  list(
    i = list(lower_const = NULL, upper_dim = NULL, upper_const = NULL),
    j = list(lower_const = NULL, upper_dim = NULL, upper_const = NULL)
  )
}

.mojor_gpu_matrix2d_guard_apply <- function(bounds, var_name, field, value) {
  slot <- bounds[[var_name]][[field]]
  if (is.null(slot)) {
    bounds[[var_name]][[field]] <- value
  } else if (field == "lower_const") {
    bounds[[var_name]][[field]] <- max(slot, value)
  } else {
    bounds[[var_name]][[field]] <- min(slot, value)
  }
  bounds
}

.mojor_gpu_matrix2d_parse_guard_atom <- function(atom, outer_var, inner_var, matrix_source,
                                                 const_int_env = NULL) {
  if (!is.call(atom) || length(atom) != 3) {
    return(list(ok = FALSE, reason = "gpu elementwise matrix2d guard must be canonical conjunction of bounds checks"))
  }
  op <- as.character(atom[[1]])
  if (!(op %in% c(">=", ">", "<=", "<"))) {
    return(list(ok = FALSE, reason = "gpu elementwise matrix2d guard must be canonical conjunction of bounds checks"))
  }
  lhs <- atom[[2]]
  rhs <- atom[[3]]
  lhs_term <- .mojor_gpu_matrix2d_parse_var_term(
    lhs,
    outer_var = outer_var,
    inner_var = inner_var,
    const_int_env = const_int_env
  )
  rhs_term <- .mojor_gpu_matrix2d_parse_var_term(
    rhs,
    outer_var = outer_var,
    inner_var = inner_var,
    const_int_env = const_int_env
  )

  normalize <- function(var_term, side_expr, cmp_op, reversed = FALSE) {
    if (is.null(var_term) || is.null(var_term$var)) {
      return(NULL)
    }
    var_tag <- var_term$var
    var_offset <- as.integer(var_term$offset)
    axis <- if (identical(var_tag, "i")) "nrow" else "ncol"
    term_const <- .mojor_gpu_matrix2d_int_constant(side_expr, const_int_env = const_int_env)
    term_dim <- .mojor_gpu_matrix2d_parse_dim_bound(
      side_expr,
      axis = axis,
      matrix_source = matrix_source,
      const_int_env = const_int_env
    )
    op_eff <- cmp_op
    if (reversed) {
      op_eff <- switch(cmp_op, ">=" = "<=", ">" = "<", "<=" = ">=", "<" = ">", cmp_op)
    }
    if (!is.null(term_const)) {
      if (op_eff == ">=") {
        return(list(ok = TRUE, var = var_tag, field = "lower_const", value = as.integer(term_const - var_offset)))
      }
      if (op_eff == ">") {
        return(list(ok = TRUE, var = var_tag, field = "lower_const", value = as.integer(term_const + 1L - var_offset)))
      }
      if (op_eff == "<=") {
        return(list(ok = TRUE, var = var_tag, field = "upper_const", value = as.integer(term_const - var_offset)))
      }
      if (op_eff == "<") {
        return(list(ok = TRUE, var = var_tag, field = "upper_const", value = as.integer(term_const - 1L - var_offset)))
      }
    }
    if (!is.null(term_dim)) {
      if (op_eff == "<=") {
        return(list(ok = TRUE, var = var_tag, field = "upper_dim", value = as.integer(term_dim$offset - var_offset)))
      }
      if (op_eff == "<") {
        return(list(ok = TRUE, var = var_tag, field = "upper_dim", value = as.integer(term_dim$offset - 1L - var_offset)))
      }
      return(list(ok = FALSE, reason = "gpu elementwise matrix2d guard must be canonical conjunction of bounds checks"))
    }
    list(ok = FALSE, reason = "gpu elementwise matrix2d guard must be canonical conjunction of bounds checks")
  }

  parsed <- NULL
  if (!is.null(lhs_term) && is.null(rhs_term)) {
    parsed <- normalize(lhs_term, rhs, op, reversed = FALSE)
  } else if (!is.null(rhs_term) && is.null(lhs_term)) {
    parsed <- normalize(rhs_term, lhs, op, reversed = TRUE)
  } else {
    parsed <- list(ok = FALSE, reason = "gpu elementwise matrix2d guard must be canonical conjunction of bounds checks")
  }
  parsed
}

.mojor_gpu_matrix2d_parse_guard <- function(cond, outer_var, inner_var, matrix_source,
                                            const_int_env = NULL) {
  normalized <- .mojor_gpu_matrix2d_guard_normalize(cond, negated = FALSE)
  if (is.null(normalized)) {
    return(list(
      ok = FALSE,
      reason = "gpu elementwise matrix2d does not support complex boolean guard operators"
    ))
  }
  if (isTRUE(.mojor_gpu_matrix2d_guard_has_or(normalized))) {
    return(list(
      ok = FALSE,
      reason = "gpu elementwise matrix2d does not support complex boolean guard operators"
    ))
  }

  walk <- function(node) {
    if (!is.call(node)) {
      return(list(ok = FALSE, reason = "gpu elementwise matrix2d guard must be canonical conjunction of bounds checks"))
    }
    node <- .mojor_gpu_matrix2d_unwrap_index_expr(node)
    op <- as.character(node[[1]])
    if (op == "(" && length(node) == 2) {
      return(walk(node[[2]]))
    }
    if (op %in% c("&&", "&")) {
      if (length(node) != 3) {
        return(list(ok = FALSE, reason = "gpu elementwise matrix2d guard must be canonical conjunction of bounds checks"))
      }
      left <- walk(node[[2]])
      if (!isTRUE(left$ok)) {
        return(left)
      }
      right <- walk(node[[3]])
      if (!isTRUE(right$ok)) {
        return(right)
      }
      bounds <- left$bounds
      for (v in c("i", "j")) {
        for (f in c("lower_const", "upper_dim", "upper_const")) {
          val <- right$bounds[[v]][[f]]
          if (!is.null(val)) {
            bounds <- .mojor_gpu_matrix2d_guard_apply(bounds, v, f, val)
          }
        }
      }
      return(list(ok = TRUE, bounds = bounds))
    }
    atom <- .mojor_gpu_matrix2d_parse_guard_atom(
      node,
      outer_var = outer_var,
      inner_var = inner_var,
      matrix_source = matrix_source,
      const_int_env = const_int_env
    )
    if (!isTRUE(atom$ok)) {
      return(atom)
    }
    bounds <- .mojor_gpu_matrix2d_guard_bound_empty()
    bounds <- .mojor_gpu_matrix2d_guard_apply(bounds, atom$var, atom$field, atom$value)
    list(ok = TRUE, bounds = bounds)
  }
  out <- walk(normalized)
  if (isTRUE(out$ok)) {
    out$normalized <- normalized
  }
  out
}

.mojor_gpu_matrix2d_guard_covers <- function(guard_bounds, di, dj) {
  if (is.null(guard_bounds)) {
    return(FALSE)
  }
  row_ok <- TRUE
  col_ok <- TRUE
  if (di < 0L) {
    need_lower <- as.integer(1L - di)
    got_lower <- guard_bounds$i$lower_const
    row_ok <- !is.null(got_lower) && (got_lower >= need_lower)
  } else if (di > 0L) {
    need_upper <- as.integer(-di)
    got_upper <- guard_bounds$i$upper_dim
    row_ok <- !is.null(got_upper) && (got_upper <= need_upper)
  }
  if (dj < 0L) {
    need_lower <- as.integer(1L - dj)
    got_lower <- guard_bounds$j$lower_const
    col_ok <- !is.null(got_lower) && (got_lower >= need_lower)
  } else if (dj > 0L) {
    need_upper <- as.integer(-dj)
    got_upper <- guard_bounds$j$upper_dim
    col_ok <- !is.null(got_upper) && (got_upper <= need_upper)
  }
  row_ok && col_ok
}

.mojor_gpu_matrix2d_expr_identical <- function(lhs, rhs) {
  identical(
    .mojor_gpu_matrix2d_unwrap_index_expr(lhs),
    .mojor_gpu_matrix2d_unwrap_index_expr(rhs)
  )
}

.mojor_gpu_matrix2d_guard_atoms <- function(node) {
  node <- .mojor_gpu_matrix2d_unwrap_index_expr(node)
  if (!is.call(node)) {
    return(list())
  }
  op <- as.character(node[[1L]])
  if (op %in% c("&&", "&") && length(node) == 3L) {
    return(c(
      .mojor_gpu_matrix2d_guard_atoms(node[[2L]]),
      .mojor_gpu_matrix2d_guard_atoms(node[[3L]])
    ))
  }
  list(node)
}

.mojor_gpu_matrix2d_guard_covers_index_expr <- function(guard_cond, idx_expr, axis = c("nrow", "ncol"),
                                                        matrix_source, const_int_env = NULL) {
  axis <- match.arg(axis)
  idx_expr <- .mojor_gpu_matrix2d_unwrap_index_expr(idx_expr)
  if (is.null(guard_cond)) {
    return(FALSE)
  }
  atoms <- .mojor_gpu_matrix2d_guard_atoms(guard_cond)
  if (length(atoms) == 0L) {
    return(FALSE)
  }
  lower_ok <- FALSE
  upper_ok <- FALSE

  check_side <- function(term_expr, bound_expr, cmp_op, reversed = FALSE) {
    if (!.mojor_gpu_matrix2d_expr_identical(term_expr, idx_expr)) {
      return(list(lower = FALSE, upper = FALSE))
    }
    op_eff <- cmp_op
    if (isTRUE(reversed)) {
      op_eff <- switch(cmp_op, ">=" = "<=", ">" = "<", "<=" = ">=", "<" = ">", cmp_op)
    }
    term_const <- .mojor_gpu_matrix2d_int_constant(bound_expr, const_int_env = const_int_env)
    term_dim <- .mojor_gpu_matrix2d_parse_dim_bound(
      bound_expr,
      axis = axis,
      matrix_source = matrix_source,
      const_int_env = const_int_env
    )
    lower_hit <- FALSE
    upper_hit <- FALSE
    if (!is.null(term_const)) {
      if (op_eff == ">=" && term_const >= 1L) {
        lower_hit <- TRUE
      } else if (op_eff == ">" && term_const >= 0L) {
        lower_hit <- TRUE
      }
    }
    if (!is.null(term_dim)) {
      if (op_eff == "<=" && term_dim$offset <= 0L) {
        upper_hit <- TRUE
      } else if (op_eff == "<" && term_dim$offset <= 1L) {
        upper_hit <- TRUE
      }
    }
    list(lower = lower_hit, upper = upper_hit)
  }

  for (atom in atoms) {
    if (!is.call(atom) || length(atom) != 3L) {
      next
    }
    op <- as.character(atom[[1L]])
    if (!(op %in% c(">=", ">", "<=", "<"))) {
      next
    }
    lhs <- atom[[2L]]
    rhs <- atom[[3L]]
    lhs_hit <- check_side(lhs, rhs, op, reversed = FALSE)
    rhs_hit <- check_side(rhs, lhs, op, reversed = TRUE)
    lower_ok <- lower_ok || isTRUE(lhs_hit$lower) || isTRUE(rhs_hit$lower)
    upper_ok <- upper_ok || isTRUE(lhs_hit$upper) || isTRUE(rhs_hit$upper)
  }

  lower_ok && upper_ok
}

.mojor_gpu_matrix2d_replace_loop_vars <- function(expr, outer_var, inner_var,
                                                  row_var = "__mojor_m2_i",
                                                  col_var = "__mojor_m2_j") {
  if (is.name(expr)) {
    nm <- as.character(expr)
    if (identical(nm, outer_var)) {
      return(as.name(row_var))
    }
    if (identical(nm, inner_var)) {
      return(as.name(col_var))
    }
    return(expr)
  }
  if (!is.call(expr)) {
    return(expr)
  }
  parts <- as.list(expr)
  if (length(parts) > 1L) {
    for (k in 2:length(parts)) {
      parts[[k]] <- .mojor_gpu_matrix2d_replace_loop_vars(
        parts[[k]],
        outer_var = outer_var,
        inner_var = inner_var,
        row_var = row_var,
        col_var = col_var
      )
    }
  }
  as.call(parts)
}

.mojor_gpu_matrix2d_linear_index_one_based_expr <- function(row_expr, col_expr, nrow_var = "__mojor_m2_nrow_i") {
  call(
    "+",
    row_expr,
    call("*", call("-", col_expr, 1L), as.name(nrow_var))
  )
}

.mojor_gpu_matrix2d_linear_index_one_based <- function(row_var, col_var, di = 0L, dj = 0L, nrow_var = "__mojor_m2_nrow_i") {
  row_expr <- as.name(row_var)
  if (di != 0L) {
    row_expr <- if (di > 0L) {
      call("+", row_expr, as.integer(di))
    } else {
      call("-", row_expr, as.integer(abs(di)))
    }
  }
  col_expr <- as.name(col_var)
  if (dj != 0L) {
    col_expr <- if (dj > 0L) {
      call("+", col_expr, as.integer(dj))
    } else {
      call("-", col_expr, as.integer(abs(dj)))
    }
  }
  .mojor_gpu_matrix2d_linear_index_one_based_expr(
    row_expr = row_expr,
    col_expr = col_expr,
    nrow_var = nrow_var
  )
}

.mojor_gpu_matrix2d_intify_guard_expr <- function(expr) {
  if (is.numeric(expr) && length(expr) == 1L && !is.na(expr) && is.finite(expr)) {
    rounded <- round(expr)
    if (abs(expr - rounded) < .Machine$double.eps^0.5) {
      return(as.integer(rounded))
    }
  }
  if (!is.call(expr)) {
    return(expr)
  }
  parts <- as.list(expr)
  if (length(parts) > 1L) {
    for (i in 2:length(parts)) {
      parts[[i]] <- .mojor_gpu_matrix2d_intify_guard_expr(parts[[i]])
    }
  }
  as.call(parts)
}

.mojor_gpu_matrix2d_rewrite_expr <- function(expr, outer_var, inner_var, linear_var, type_env, matrix_source,
                                             allow_loop_vars = FALSE, guard_info = NULL,
                                             const_int_env = NULL) {
  reduce_fns <- c("sum", "mean", "prod", "min", "max", "which.min", "which.max", "gpu_reduce", "gpu_sum", "gpu_mean", "gpu_argmin", "gpu_argmax")
  row_var <- "__mojor_m2_i"
  col_var <- "__mojor_m2_j"
  nrow_var <- "__mojor_m2_nrow_i"
  ncol_var <- "__mojor_m2_ncol_i"
  rewrite <- function(node) {
    if (is.name(node)) {
      nm <- as.character(node)
      if (nm %in% c(outer_var, inner_var)) {
        if (!isTRUE(allow_loop_vars)) {
          return(list(ok = FALSE, reason = "gpu elementwise matrix2d does not support direct loop-var rhs"))
        }
        mapped <- if (identical(nm, outer_var)) row_var else col_var
        return(list(ok = TRUE, expr = as.name(mapped), uses_neighbors = FALSE))
      }
      if (!is.null(const_int_env[[nm]])) {
        return(list(ok = TRUE, expr = as.integer(const_int_env[[nm]]), uses_neighbors = FALSE))
      }
      return(list(ok = TRUE, expr = node, uses_neighbors = FALSE))
    }
    if (!is.call(node)) {
      return(list(ok = TRUE, expr = node, uses_neighbors = FALSE))
    }
    op <- as.character(node[[1]])
    if (op %in% c("for", "while", "repeat", "if")) {
      return(list(ok = FALSE, reason = "gpu elementwise matrix2d does not support control flow in inner body"))
    }
    if (op %in% reduce_fns) {
      return(list(ok = FALSE, reason = "gpu elementwise matrix2d does not support reductions in kernel body"))
    }
    if ((op %in% c("nrow", "ncol")) && length(node) == 2 && is.name(node[[2]]) &&
        identical(as.character(node[[2]]), matrix_source)) {
      return(list(
        ok = TRUE,
        expr = as.name(if (op == "nrow") nrow_var else ncol_var),
        uses_neighbors = FALSE
      ))
    }
    if (op %in% c("[", "[[")) {
      if (!is.name(node[[2]])) {
        return(list(ok = FALSE, reason = "gpu elementwise matrix2d does not support irregular indexing"))
      }
      var <- as.character(node[[2]])
      spec <- type_env[[var]]
      if (is.null(spec) || !.mojor_gpu_matrix_array_spec_ok(spec)) {
        return(list(ok = FALSE, reason = "gpu elementwise matrix2d requires matrix array indexing"))
      }
      if (length(node) != 4) {
        return(list(ok = FALSE, reason = "gpu elementwise matrix2d does not support irregular indexing"))
      }
      off_i <- .mojor_gpu_matrix2d_parse_offset(node[[3]], outer_var, const_int_env = const_int_env)
      off_j <- .mojor_gpu_matrix2d_parse_offset(node[[4]], inner_var, const_int_env = const_int_env)
      if (!isTRUE(off_i$ok) || !isTRUE(off_j$ok)) {
        if ((!isTRUE(off_i$ok) && identical(off_i$kind, "nonliteral")) ||
            (!isTRUE(off_j$ok) && identical(off_j$kind, "nonliteral"))) {
          return(list(ok = FALSE, reason = "gpu elementwise matrix2d does not support non-literal neighbor offsets"))
        }
        return(list(ok = FALSE, reason = "gpu elementwise matrix2d does not support irregular indexing"))
      }
      row_idx_src <- if (!is.null(off_i$expr)) off_i$expr else as.name(outer_var)
      col_idx_src <- if (!is.null(off_j$expr)) off_j$expr else as.name(inner_var)
      off_i_literal <- !is.null(off_i$offset)
      off_j_literal <- !is.null(off_j$offset)
      di <- if (off_i_literal) as.integer(off_i$offset) else 0L
      dj <- if (off_j_literal) as.integer(off_j$offset) else 0L
      uses_neighbors <- !(.mojor_gpu_matrix2d_expr_identical(row_idx_src, as.name(outer_var)) &&
        .mojor_gpu_matrix2d_expr_identical(col_idx_src, as.name(inner_var)))
      if (uses_neighbors) {
        if (is.null(guard_info)) {
          return(list(ok = FALSE, reason = "gpu elementwise matrix2d neighbor indexing requires explicit in-bounds guard"))
        }
        if (!isTRUE(guard_info$ok)) {
          return(list(ok = FALSE, reason = guard_info$reason))
        }
        if (isTRUE(off_i_literal) && isTRUE(off_j_literal)) {
          if (!.mojor_gpu_matrix2d_guard_covers(guard_info$bounds, di, dj)) {
            return(list(ok = FALSE, reason = "gpu elementwise matrix2d neighbor indexing requires explicit in-bounds guard"))
          }
        } else {
          row_guard_ok <- .mojor_gpu_matrix2d_expr_identical(row_idx_src, as.name(outer_var)) ||
            .mojor_gpu_matrix2d_guard_covers_index_expr(
              guard_info$normalized,
              idx_expr = row_idx_src,
              axis = "nrow",
              matrix_source = matrix_source,
              const_int_env = const_int_env
            )
          col_guard_ok <- .mojor_gpu_matrix2d_expr_identical(col_idx_src, as.name(inner_var)) ||
            .mojor_gpu_matrix2d_guard_covers_index_expr(
              guard_info$normalized,
              idx_expr = col_idx_src,
              axis = "ncol",
              matrix_source = matrix_source,
              const_int_env = const_int_env
            )
          if (!(isTRUE(row_guard_ok) && isTRUE(col_guard_ok))) {
            return(list(ok = FALSE, reason = "gpu elementwise matrix2d neighbor indexing requires explicit in-bounds guard"))
          }
        }
      }
      if (!uses_neighbors) {
        return(list(ok = TRUE, expr = call("[", as.name(var), as.name(linear_var)), uses_neighbors = FALSE))
      }
      row_idx_codegen <- .mojor_gpu_matrix2d_replace_loop_vars(
        row_idx_src,
        outer_var = outer_var,
        inner_var = inner_var,
        row_var = row_var,
        col_var = col_var
      )
      col_idx_codegen <- .mojor_gpu_matrix2d_replace_loop_vars(
        col_idx_src,
        outer_var = outer_var,
        inner_var = inner_var,
        row_var = row_var,
        col_var = col_var
      )
      lin_one_based <- .mojor_gpu_matrix2d_linear_index_one_based_expr(
        row_expr = row_idx_codegen,
        col_expr = col_idx_codegen,
        nrow_var = nrow_var
      )
      return(list(ok = TRUE, expr = call("[", as.name(var), lin_one_based), uses_neighbors = TRUE))
    }
    parts <- as.list(node)
    out <- vector("list", length(parts))
    out[[1]] <- parts[[1]]
    used_neighbors <- FALSE
    if (length(parts) > 1) {
      for (i in 2:length(parts)) {
        rw <- rewrite(parts[[i]])
        if (!isTRUE(rw$ok)) {
          return(rw)
        }
        out[[i]] <- rw$expr
        used_neighbors <- used_neighbors || isTRUE(rw$uses_neighbors)
      }
    }
    list(ok = TRUE, expr = as.call(out), uses_neighbors = used_neighbors)
  }
  rewrite(expr)
}

.mojor_gpu_cast_f32_literals <- function(expr) {
  if (!is.character(expr) || length(expr) != 1L || !nzchar(expr)) {
    return(expr)
  }
  pat <- "(?<![A-Za-z0-9_\\.])(-?(?:[0-9]+\\.[0-9]*|[0-9]*\\.[0-9]+|[0-9]+[eE][+-]?[0-9]+))(?![A-Za-z0-9_])"
  loc <- gregexpr(pat, expr, perl = TRUE)[[1]]
  if (length(loc) == 1L && loc[[1]] == -1L) {
    return(expr)
  }
  len <- attr(loc, "match.length")
  for (k in rev(seq_along(loc))) {
    start <- loc[[k]]
    end <- start + len[[k]] - 1L
    if (start <= 0L || end < start) {
      next
    }
    prefix <- substr(expr, max(1L, start - 14L), start - 1L)
    if (grepl("Float32\\($|Float64\\($|Scalar\\[dtype\\]\\($", prefix)) {
      next
    }
    lit <- substr(expr, start, end)
    expr <- paste0(
      substr(expr, 1L, start - 1L),
      "Scalar[dtype](", lit, ")",
      substr(expr, end + 1L, nchar(expr))
    )
  }
  expr
}

.mojor_gpu_matrix2d_plan <- function(li, out_name, out_type, out_matrix = FALSE, args, arg_specs, local_types) {
  out <- list(
    ok = FALSE, reason = "gpu elementwise matrix2d requires canonical nested loops",
    assigns = list(), inplace = FALSE, matrix_source = NULL, matrix_dim_arrays = character(0),
    uses_control_flow = FALSE, uses_neighbors = FALSE
  )
  out_matrix_spec <- NULL
  if (!is.null(local_types) && !is.null(out_name)) {
    out_matrix_spec <- local_types[[out_name]]
  }
  if (is.null(out_matrix_spec) && isTRUE(out_matrix) && !is.null(out_type)) {
    out_matrix_spec <- .mojor_type_tag_ndim(out_type, 2L)
  }
  out_rank <- if (is.null(out_matrix_spec)) NULL else .mojor_type_ndim(out_matrix_spec)
  out_is_matrix <- !is.null(out_matrix_spec) &&
    (.mojor_is_matrix(out_matrix_spec) || identical(out_rank, 2L))
  if (!isTRUE(out_matrix) || is.null(out_matrix_spec) || !isTRUE(out_is_matrix) ||
      !(.mojor_type_base(out_matrix_spec) %in% c("f32", "f64"))) {
    out$reason <- "gpu elementwise matrix2d requires f32[,] or f64[,] output"
    return(out)
  }
  if (length(li$blocks) != 1 || !(is.call(li$blocks[[1]]) && identical(as.character(li$blocks[[1]][[1]]), "for"))) {
    out$reason <- "gpu elementwise matrix2d requires a single nested for loop"
    return(out)
  }
  outer_src <- .mojor_gpu_seq_len_dim_source(li$seq, arg_specs, axis = "nrow")
  if (is.null(outer_src)) {
    out$reason <- "gpu elementwise matrix2d requires outer loop seq_len(nrow(X))"
    return(out)
  }
  inner <- li$blocks[[1]]
  inner_var <- as.character(inner[[2]])
  if (!nzchar(inner_var)) {
    out$reason <- "gpu elementwise matrix2d requires a valid inner loop index"
    return(out)
  }
  inner_src <- .mojor_gpu_seq_len_dim_source(inner[[3]], arg_specs, axis = "ncol")
  if (is.null(inner_src) || !identical(inner_src$name, outer_src$name)) {
    out$reason <- "gpu elementwise matrix2d requires inner loop seq_len(ncol(X))"
    return(out)
  }
  inner_blocks <- .mojor_extract_block(inner[[4]])
  if (length(inner_blocks) == 0) {
    out$reason <- "gpu elementwise matrix2d inner loop contains no statements"
    return(out)
  }
  type_env <- if (is.null(local_types)) list() else local_types
  type_env[[out_name]] <- out_matrix_spec
  const_int_env <- .mojor_gpu_matrix2d_const_int_env()
  parse_assign_rhs <- function(stmt) {
    if (!(is.call(stmt) && as.character(stmt[[1]]) %in% c("<-", "="))) {
      if (is.call(stmt) && as.character(stmt[[1]]) == "if") {
        return(list(ok = FALSE, reason = "gpu elementwise matrix2d does not support control flow in inner body"))
      }
      return(list(ok = FALSE, reason = "gpu elementwise matrix2d requires assignment-only inner loop body"))
    }
    lhs <- stmt[[2]]
    rhs <- stmt[[3]]
    if (!(is.call(lhs) && identical(as.character(lhs[[1]]), "[") &&
      is.name(lhs[[2]]) && identical(as.character(lhs[[2]]), out_name) &&
      length(lhs) == 4)) {
      return(list(ok = FALSE, reason = "gpu elementwise matrix2d requires out[i, j] assignment"))
    }
    lhs_i <- .mojor_gpu_matrix2d_parse_offset(lhs[[3L]], li$var, const_int_env = const_int_env)
    lhs_j <- .mojor_gpu_matrix2d_parse_offset(lhs[[4L]], inner_var, const_int_env = const_int_env)
    if (!isTRUE(lhs_i$ok) || !isTRUE(lhs_j$ok) ||
      lhs_i$offset != 0L || lhs_j$offset != 0L) {
      return(list(ok = FALSE, reason = "gpu elementwise matrix2d requires out[i, j] assignment"))
    }
    list(ok = TRUE, rhs = rhs)
  }

  current_rhs <- NULL
  for (s in inner_blocks) {
    if (is.call(s) && as.character(s[[1]]) == "if") {
      if (length(s) < 3 || length(s) > 4) {
        out$reason <- "gpu elementwise matrix2d does not support control flow in inner body"
        return(out)
      }
      cond <- s[[2]]
      then_blocks <- .mojor_extract_block(s[[3]])
      else_blocks <- if (length(s) == 4) .mojor_extract_block(s[[4]]) else list()
      if (length(then_blocks) != 1L || (length(else_blocks) > 0L && length(else_blocks) != 1L)) {
        out$reason <- "gpu elementwise matrix2d does not support control flow in inner body"
        return(out)
      }
      then_assign <- parse_assign_rhs(then_blocks[[1]])
      if (!isTRUE(then_assign$ok)) {
        out$reason <- then_assign$reason
        return(out)
      }
      else_assign <- NULL
      if (length(else_blocks) == 1L) {
        else_assign <- parse_assign_rhs(else_blocks[[1]])
        if (!isTRUE(else_assign$ok)) {
          out$reason <- else_assign$reason
          return(out)
        }
      }
      cond_rewrite <- .mojor_gpu_matrix2d_rewrite_expr(
        cond,
        outer_var = li$var,
        inner_var = inner_var,
        linear_var = li$var,
        type_env = type_env,
        matrix_source = outer_src$name,
        allow_loop_vars = TRUE,
        guard_info = NULL,
        const_int_env = const_int_env
      )
      if (!isTRUE(cond_rewrite$ok)) {
        out$reason <- cond_rewrite$reason
        return(out)
      }
      guard_info <- .mojor_gpu_matrix2d_parse_guard(
        cond,
        outer_var = li$var,
        inner_var = inner_var,
        matrix_source = outer_src$name,
        const_int_env = const_int_env
      )
      if (isTRUE(guard_info$ok)) {
        cond_rewrite$expr <- .mojor_gpu_matrix2d_intify_guard_expr(cond_rewrite$expr)
      }
      then_rewrite <- .mojor_gpu_matrix2d_rewrite_expr(
        then_assign$rhs,
        outer_var = li$var,
        inner_var = inner_var,
        linear_var = li$var,
        type_env = type_env,
        matrix_source = outer_src$name,
        allow_loop_vars = FALSE,
        guard_info = guard_info,
        const_int_env = const_int_env
      )
      if (!isTRUE(then_rewrite$ok)) {
        out$reason <- then_rewrite$reason
        return(out)
      }
      else_expr <- NULL
      else_uses_neighbors <- FALSE
      if (!is.null(else_assign)) {
        else_rewrite <- .mojor_gpu_matrix2d_rewrite_expr(
          else_assign$rhs,
          outer_var = li$var,
          inner_var = inner_var,
          linear_var = li$var,
          type_env = type_env,
          matrix_source = outer_src$name,
          allow_loop_vars = FALSE,
          guard_info = NULL,
          const_int_env = const_int_env
        )
        if (!isTRUE(else_rewrite$ok)) {
          out$reason <- else_rewrite$reason
          return(out)
        }
        else_expr <- else_rewrite$expr
        else_uses_neighbors <- isTRUE(else_rewrite$uses_neighbors)
      } else {
        if (is.null(current_rhs)) {
          out$reason <- "gpu elementwise matrix2d requires out[i, j] assignment"
          return(out)
        }
        else_expr <- current_rhs
      }
      rewritten_expr <- call("ifelse", cond_rewrite$expr, then_rewrite$expr, else_expr)
      current_rhs <- rewritten_expr
      out$assigns[[length(out$assigns) + 1L]] <- list(rhs = rewritten_expr)
      out$uses_control_flow <- TRUE
      out$uses_neighbors <- out$uses_neighbors || isTRUE(then_rewrite$uses_neighbors) || else_uses_neighbors
      refs <- .mojor_out_ref_indices(rewritten_expr, out_name)
      if (length(refs) > 0) {
        out$inplace <- TRUE
      }
      if (!.mojor_out_ref_ok(rewritten_expr, out_name, li$var)) {
        out$reason <- "rhs references output array with non-loop index"
        return(out)
      }
      next
    }

    assign <- parse_assign_rhs(s)
    if (!isTRUE(assign$ok)) {
      out$reason <- assign$reason
      return(out)
    }
    rewritten <- .mojor_gpu_matrix2d_rewrite_expr(
      assign$rhs,
      outer_var = li$var,
      inner_var = inner_var,
      linear_var = li$var,
      type_env = type_env,
      matrix_source = outer_src$name,
      allow_loop_vars = FALSE,
      guard_info = NULL,
      const_int_env = const_int_env
    )
    if (!isTRUE(rewritten$ok)) {
      out$reason <- rewritten$reason
      return(out)
    }
    refs <- .mojor_out_ref_indices(rewritten$expr, out_name)
    if (length(refs) > 0) {
      out$inplace <- TRUE
    }
    if (!.mojor_out_ref_ok(rewritten$expr, out_name, li$var)) {
      out$reason <- "rhs references output array with non-loop index"
      return(out)
    }
    current_rhs <- rewritten$expr
    out$assigns[[length(out$assigns) + 1L]] <- list(rhs = rewritten$expr)
    out$uses_neighbors <- out$uses_neighbors || isTRUE(rewritten$uses_neighbors)
  }
  matrix_dim_arrays <- args[vapply(args, function(a) .mojor_gpu_matrix_array_spec_ok(arg_specs[[a]]), logical(1))]
  if (length(matrix_dim_arrays) == 0) {
    out$reason <- "gpu matrix2d buffers require matrix array inputs"
    return(out)
  }
  if (!(outer_src$name %in% matrix_dim_arrays)) {
    out$reason <- "gpu elementwise matrix2d requires loop source X to be a matrix input"
    return(out)
  }
  out$ok <- TRUE
  out$matrix_source <- outer_src$name
  out$matrix_dim_arrays <- unique(matrix_dim_arrays)
  out
}

.mojor_elementwise_plan <- function(li, out_name, out_kind, out_type, elementwise_target, elementwise_cpu, non_unit_range,
                                    args = NULL, arg_specs = NULL, elementwise_size = NULL, seq_info = NULL,
                                    local_types = NULL, name = NULL, broadcast_nd = FALSE, out_matrix = FALSE,
                                    allow_dynamic_gpu_size = FALSE, elementwise_gpu_layouttensor = NULL) {
  plan <- list(
    ok = FALSE, reason = NULL, inplace = FALSE, gpu_buf_info = NULL, gpu_buf_reason = NULL,
    broadcast_nd = isTRUE(broadcast_nd), index_mode = "linear1d"
  )
  find_blocked_gpu_math <- function(expr, blocked = c("tan", "expm1")) {
    if (!is.call(expr)) {
      return(character(0))
    }
    op <- as.character(expr[[1]])
    hits <- if (op %in% blocked) op else character(0)
    if (length(expr) > 1) {
      for (i in 2:length(expr)) {
        hits <- c(hits, find_blocked_gpu_math(expr[[i]], blocked = blocked))
      }
    }
    unique(hits)
  }
  if (is.null(li) || !identical(li$kind, "for")) {
    plan$reason <- "loop is not a for loop"
    return(plan)
  }
  if (!(out_kind %in% c("vector", "matrix"))) {
    plan$reason <- "elementwise requires vector or matrix output"
    return(plan)
  }
  if (isTRUE(non_unit_range)) {
    plan$reason <- "loop range is not unit stride"
    return(plan)
  }
  effective_out_type <- out_type
  if (identical(elementwise_target, "gpu") && isTRUE(out_matrix) &&
      identical(effective_out_type, "f64[]") && !is.null(args) && !is.null(arg_specs)) {
    has_f64_input <- any(vapply(args, function(a) {
      spec <- arg_specs[[a]]
      !is.null(spec) && spec %in% c("f64", "f64[]", "f64[,]")
    }, logical(1)))
    has_f32_matrix <- any(vapply(args, function(a) {
      spec <- arg_specs[[a]]
      !is.null(spec) && identical(spec, "f32[,]")
    }, logical(1)))
    if (isTRUE(has_f32_matrix) && !isTRUE(has_f64_input)) {
      effective_out_type <- "f32[]"
    }
  }
  out_dtype <- .mojor_gpu_elementwise_out_dtype(effective_out_type)
  if (is.null(out_dtype)) {
    plan$reason <- "elementwise requires f32[]/f64[] or f32[,]/f64[,] output"
    return(plan)
  }
  if (!identical(elementwise_target, "cpu") && !identical(elementwise_target, "gpu")) {
    plan$reason <- "elementwise target not supported"
    return(plan)
  }
  if (identical(elementwise_target, "cpu") && !isTRUE(elementwise_cpu)) {
    plan$reason <- "cpu elementwise disabled"
    return(plan)
  }
  if (length(li$blocks) == 0) {
    plan$reason <- "loop contains no statements"
    return(plan)
  }
  has_nested_for <- any(vapply(li$blocks, function(s) is.call(s) && identical(as.character(s[[1]]), "for"), logical(1)))
  index_mode <- "linear1d"
  ew_assigns <- li$blocks
  loop_var <- li$var
  matrix_dim_source <- NULL
  matrix_dim_arrays <- character(0)
  matrix2d_uses_control_flow <- FALSE
  matrix2d_uses_neighbors <- FALSE
  if (isTRUE(has_nested_for) && identical(elementwise_target, "gpu")) {
    matrix2d <- .mojor_gpu_matrix2d_plan(
      li = li, out_name = out_name, out_type = effective_out_type, out_matrix = out_matrix,
      args = args, arg_specs = arg_specs, local_types = local_types
    )
    if (!isTRUE(matrix2d$ok)) {
      plan$reason <- matrix2d$reason
      return(plan)
    }
    index_mode <- "matrix2d"
    ew_assigns <- lapply(matrix2d$assigns, function(s) call("<-", as.name(out_name), s$rhs))
    plan$inplace <- isTRUE(matrix2d$inplace)
    matrix_dim_source <- matrix2d$matrix_source
    matrix_dim_arrays <- matrix2d$matrix_dim_arrays
    matrix2d_uses_control_flow <- isTRUE(matrix2d$uses_control_flow)
    matrix2d_uses_neighbors <- isTRUE(matrix2d$uses_neighbors)
  } else {
    for (s in li$blocks) {
      if (!(is.call(s) && as.character(s[[1]]) %in% c("<-", "="))) {
        plan$reason <- "loop contains non-assignment statements"
        return(plan)
      }
      lhs <- s[[2]]
      rhs <- s[[3]]
      if (!(is.call(lhs) && as.character(lhs[[1]]) == "[" &&
        is.name(lhs[[2]]) && as.character(lhs[[2]]) == out_name)) {
        plan$reason <- "loop assigns to non-output target"
        return(plan)
      }
      if (length(lhs) != 3) {
        plan$reason <- "loop assigns to non-vector target"
        return(plan)
      }
      if (!is.name(lhs[[3]]) || as.character(lhs[[3]]) != li$var) {
        plan$reason <- "loop index not used directly"
        return(plan)
      }
      refs <- .mojor_out_ref_indices(rhs, out_name)
      if (length(refs) > 0) {
        plan$inplace <- TRUE
      }
      if (!.mojor_out_ref_ok(rhs, out_name, li$var)) {
        plan$reason <- "rhs references output array with non-loop index"
        return(plan)
      }
    }
  }
  plan$index_mode <- index_mode
  if (identical(elementwise_target, "gpu")) {
    if (is.null(elementwise_size) && !isTRUE(allow_dynamic_gpu_size)) {
      plan$reason <- "gpu elementwise requires elementwise_size"
      return(plan)
    }
    if (is.null(args) || is.null(arg_specs) || is.null(seq_info) || is.null(local_types)) {
      plan$reason <- "gpu elementwise missing context"
      return(plan)
    }
    blocked_gpu_math <- character(0)
    for (s in ew_assigns) {
      blocked_gpu_math <- unique(c(blocked_gpu_math, find_blocked_gpu_math(s[[3]])))
    }
    if (length(blocked_gpu_math) > 0) {
      plan$reason <- paste0(
        "gpu elementwise does not support ",
        paste(paste0(blocked_gpu_math, "()"), collapse = ", "),
        " offload"
      )
      return(plan)
    }
    for (a in args) {
      spec <- arg_specs[[a]]
      if (!is.null(spec) && .mojor_is_array(spec)) {
        if (identical(index_mode, "matrix2d")) {
          if (!.mojor_gpu_matrix_array_spec_ok(spec)) {
            plan$reason <- "gpu elementwise matrix2d requires matrix array inputs"
            return(plan)
          }
        } else if (!.mojor_gpu_elementwise_array_spec_ok(spec)) {
          plan$reason <- "gpu elementwise requires f32[] or f64[] inputs"
          return(plan)
        }
      }
    }
    if (identical(out_dtype, "f64")) {
      for (a in args) {
        spec <- arg_specs[[a]]
        if (!is.null(spec) && .mojor_is_array(spec) &&
          !(spec %in% c("f64[]", "f32[]", "f64[,]", "f32[,]"))) {
          plan$reason <- "gpu elementwise f64 requires f64[] or f32[] inputs"
          return(plan)
        }
      }
      for (a in args) {
        spec <- arg_specs[[a]]
        if (!is.null(spec) && !.mojor_is_array(spec) && spec == "f32") {
          plan$reason <- "gpu elementwise f64 requires f64 scalars"
          return(plan)
        }
      }
    }

    ew_specs <- list()
    ew_scalar_specs <- list()
    ew_arrays <- character(0)
    ew_dtype_tag <- if (identical(out_dtype, "f64")) "float64" else "float32"
    arg_specs_by_name <- arg_specs[args]
    arg_is_array <- vapply(arg_specs_by_name, .mojor_is_array, logical(1))
    scalar_f64 <- args[!arg_is_array &
      vapply(arg_specs_by_name, function(s) !is.null(s) && s == "f64", logical(1))]
    scalar_f32 <- args[!arg_is_array &
      vapply(arg_specs_by_name, function(s) !is.null(s) && s == "f32", logical(1))]
    use_vec_chunks <- !identical(elementwise_gpu_layouttensor, FALSE) &&
      !isTRUE(broadcast_nd) && !identical(index_mode, "matrix2d")
    for (s in ew_assigns) {
      rhs <- s[[3]]
      if (use_vec_chunks) {
        vec_chunks <- .mojor_expr_to_mojo_vec_chunks(rhs, loop_var, local_types)
        if (is.null(vec_chunks)) {
          plan$reason <- "gpu elementwise requires vectorizable rhs"
          return(plan)
        }
        ew_arrays <- unique(c(ew_arrays, vec_chunks$arrays))
        expr <- vec_chunks$expr
        if (length(scalar_f64) > 0 && ew_dtype_tag == "float32") {
          for (sn in scalar_f64) {
            expr <- gsub(paste0("\\b", sn, "\\b"), paste0(sn, "_f32"), expr, perl = TRUE)
          }
        }
        ew_specs[[length(ew_specs) + 1L]] <- expr
      }
 # GPU kernels use 0-based thread indices; keep loop var zero-based in RHS indexing.
      expr_scalar <- .mojor_expr_to_mojo(
        rhs,
        c(loop_var),
        local_types,
        zero_based_vars = c(loop_var),
        suppress_read_helpers = TRUE
      )
      if (length(scalar_f64) > 0 && ew_dtype_tag == "float32") {
        for (sn in scalar_f64) {
          expr_scalar <- gsub(paste0("\\b", sn, "\\b"), paste0(sn, "_f32"), expr_scalar, perl = TRUE)
        }
      }
      if (ew_dtype_tag == "float32" &&
          !(identical(index_mode, "matrix2d") && grepl("__mojor_m2_", expr_scalar, fixed = TRUE))) {
        expr_scalar <- .mojor_gpu_cast_f32_literals(expr_scalar)
      }
      if (identical(index_mode, "matrix2d")) {
        expr_scalar <- paste0("Scalar[dtype](", expr_scalar, ")")
      }
      ew_scalar_specs[[length(ew_scalar_specs) + 1L]] <- expr_scalar
    }
    out_in_rhs <- if (use_vec_chunks) out_name %in% ew_arrays else isTRUE(plan$inplace)
    gpu_buf_ok <- TRUE
    gpu_buf_reason <- NULL
    gpu_buf_dtype <- out_dtype
    gpu_buf_array_args <- args[arg_is_array]
    if (length(gpu_buf_array_args) == 0) {
      gpu_buf_ok <- FALSE
      gpu_buf_reason <- "gpu buffers require array inputs"
    }
    if (gpu_buf_ok && is.null(out_dtype)) {
      gpu_buf_ok <- FALSE
      gpu_buf_reason <- "gpu buffers require f32[]/f64[] output"
    }
    if (gpu_buf_ok && out_in_rhs) {
      gpu_buf_ok <- FALSE
      gpu_buf_reason <- "gpu buffers do not support output in rhs"
    }
    if (gpu_buf_ok) {
      loop_var_pat <- paste0("\\b", loop_var, "\\b")
      uses_loop_var_direct <- any(vapply(ew_specs, function(es) {
        is.character(es) && length(es) == 1 && nzchar(es) &&
          grepl(loop_var_pat, es, perl = TRUE)
      }, logical(1)))
      if (uses_loop_var_direct) {
        gpu_buf_ok <- FALSE
        gpu_buf_reason <- "gpu buffers do not support direct loop-var rhs"
      }
    }
    if (gpu_buf_ok) {
      for (a in gpu_buf_array_args) {
        spec <- arg_specs[[a]]
        if (identical(index_mode, "matrix2d")) {
          if (is.null(spec) || !.mojor_gpu_matrix_array_spec_ok(spec)) {
            gpu_buf_ok <- FALSE
            gpu_buf_reason <- "gpu matrix2d buffers require f32[,] or f64[,] inputs"
            break
          }
        } else if (is.null(spec) ||
          !.mojor_gpu_elementwise_array_spec_ok(spec, dtype = gpu_buf_dtype)) {
          gpu_buf_ok <- FALSE
          gpu_buf_reason <- if (gpu_buf_dtype == "f64") "gpu buffers require f64[] inputs" else "gpu buffers require f32[] inputs"
          break
        }
      }
    }
    if (gpu_buf_ok) {
      for (a in args[!arg_is_array]) {
        spec <- arg_specs[[a]]
        if (!is.null(spec) && !(spec %in% c("f32", "f64"))) {
          gpu_buf_ok <- FALSE
          gpu_buf_reason <- "gpu buffers require f32/f64 scalars"
          break
        }
        if (!is.null(spec) && gpu_buf_dtype == "f64" && spec == "f32") {
          gpu_buf_ok <- FALSE
          gpu_buf_reason <- "gpu buffers f64 require f64 scalars"
          break
        }
      }
    }
    if (gpu_buf_ok && identical(index_mode, "matrix2d") && length(matrix_dim_arrays) == 0) {
      gpu_buf_ok <- FALSE
      gpu_buf_reason <- "gpu matrix2d buffers require matrix array inputs"
    }
    if (gpu_buf_ok && !identical(index_mode, "matrix2d") && !seq_info$kind %in% c("array", "iter")) {
      gpu_buf_ok <- FALSE
      gpu_buf_reason <- "gpu buffers require array length source"
    }
    if (gpu_buf_ok) {
      plan$gpu_buf_info <- list(
        name = paste0(name, "_gpu_buf_", gpu_buf_dtype),
        buf_dtype = gpu_buf_dtype,
        bytes_per = if (gpu_buf_dtype == "f64") 8L else 4L,
        args = args,
        array_args = gpu_buf_array_args,
        scalar_f64 = scalar_f64,
        scalar_f32 = scalar_f32,
        ew_specs = ew_specs,
        ew_scalar = ew_scalar_specs,
        out_name = out_name,
        elementwise_size = elementwise_size,
        loop_var = loop_var,
        linear_loop_var = loop_var,
        index_mode = index_mode,
        matrix_dim_source = matrix_dim_source,
        matrix_dim_arrays = if (identical(index_mode, "matrix2d")) matrix_dim_arrays else character(0),
        matrix2d_uses_control_flow = isTRUE(matrix2d_uses_control_flow),
        matrix2d_uses_neighbors = isTRUE(matrix2d_uses_neighbors),
        broadcast_nd = isTRUE(broadcast_nd),
        dim_arrays = if (isTRUE(broadcast_nd)) gpu_buf_array_args else character(0)
      )
    } else {
      plan$gpu_buf_reason <- gpu_buf_reason
      plan$reason <- if (!is.null(gpu_buf_reason)) gpu_buf_reason else "gpu buffer emission unavailable"
      return(plan)
    }
  }
  plan$out_type <- effective_out_type
  plan$ok <- TRUE
  plan
}

.mojor_loop_parallel_safe <- function(blocks, loop_var, out_name) {
  if (is.null(out_name)) {
    return(FALSE)
  }
  for (s in blocks) {
    if (is.call(s) && as.character(s[[1]]) == "for") {
      return(FALSE)
    }
    if (is.call(s) && as.character(s[[1]]) %in% c("<-", "=")) {
      rhs <- s[[3]]
      refs <- .mojor_out_ref_indices(rhs, out_name)
      if (length(refs) > 0 && any(refs != loop_var)) {
        return(FALSE)
      }
    }
    if (is.call(s) && as.character(s[[1]]) == "if") {
      then_blocks <- .mojor_extract_block(s[[3]])
      if (length(then_blocks) == 1 && !.mojor_loop_parallel_safe(then_blocks, loop_var, out_name)) {
        return(FALSE)
      }
      if (length(s) >= 4) {
        else_blocks <- .mojor_extract_block(s[[4]])
        if (length(else_blocks) == 1 && !.mojor_loop_parallel_safe(else_blocks, loop_var, out_name)) {
          return(FALSE)
        }
      }
    }
  }
  TRUE
}

.mojor_expr_to_mojo <- function(expr, loop_vars, types, in_cond = FALSE, zero_based_vars = NULL, idx_override = NULL, idx_zero_based = FALSE, suppress_len_checks = FALSE, suppress_read_helpers = FALSE, index_context = FALSE) {
  ir <- expr
  if (!is.list(ir) || is.null(ir$kind)) {
    ir <- .mojor_ir_expr_build(expr)
  }
  if (is.null(ir)) .mojor_err("unsupported expression", expr, "simplify the expression")
  if (is.null(loop_vars)) loop_vars <- character(0)
  loop_vars <- as.character(loop_vars)
  if (is.null(zero_based_vars)) zero_based_vars <- character(0)
  placeholder <- NULL
  prev_suppress <- .mojor_state$current_suppress_read_helpers
  if (isTRUE(suppress_read_helpers)) {
    .mojor_state$current_suppress_read_helpers <- TRUE
  }
  on.exit(
    {
      .mojor_state$current_suppress_read_helpers <- prev_suppress
    },
    add = TRUE
  )
  if (!is.null(idx_override) && length(loop_vars) > 0) {
    placeholder <- "__mojor_idx_override"
    loop_vars[length(loop_vars)] <- placeholder
    if (isTRUE(idx_zero_based)) {
      zero_based_vars <- unique(c(zero_based_vars, placeholder))
    }
    if (is.list(ir) && !is.null(ir$kind)) {
      ir[["__mojor_index_loop_var"]] <- placeholder
    }
  }
  if (isTRUE(suppress_len_checks)) {
    old_suppress <- .mojor_state$current_suppress_len_checks
    .mojor_state$current_suppress_len_checks <- TRUE
    on.exit(.mojor_state$current_suppress_len_checks <- old_suppress, add = TRUE)
  }
  expr_str <- .mojor_ir_expr_emit(
    ir,
    zero_based_vars = zero_based_vars,
    type_env = types,
    loop_vars = loop_vars,
    index_context = isTRUE(index_context)
  )
  if (is.null(expr_str)) .mojor_err("unsupported expression", expr, "simplify the expression")
  if (!is.null(placeholder)) {
    expr_str <- gsub(paste0("\\b", placeholder, "\\b"), paste0("(", as.character(idx_override), ")"), expr_str)
  }
  if (isTRUE(in_cond)) {
    expr_type <- .mojor_ir_infer_type(ir, types)
    if (!is.null(expr_type) && expr_type != "bool" && expr_type != "unknown") {
      expr_str <- paste0("(", expr_str, " != 0)")
    }
  }
  expr_str
}

.mojor_mojo_numeric_literal <- function(val) {
  if (!is.numeric(val) || length(val) != 1 || is.na(val)) {
    return(as.character(val))
  }
  out <- as.character(val)
  if (!grepl("[eE]|\\.", out) && is.finite(val) && val == floor(val)) {
    out <- paste0(out, ".0")
  }
  out
}

.mojor_expr_to_mojo_vec <- function(expr, loop_var, types) {
  if (is.numeric(expr)) {
    return(.mojor_mojo_numeric_literal(expr))
  }
  if (is.name(expr)) {
    name <- as.character(expr)
    if (!is.null(types[[name]]) && types[[name]] %in% c("f64")) {
      return(name)
    }
    return(name)
  }
  if (is.call(expr)) {
    op <- as.character(expr[[1]])
    if (op %in% c("[", "[[") && length(expr) >= 3) {
      if (length(expr) >= 4) {
        return(NULL)
      }
      if (!is.name(expr[[2]])) {
        return(NULL)
      }
      var <- as.character(expr[[2]])
      idx <- expr[[3]]
      if (is.name(idx) && as.character(idx) == loop_var) {
        if (!is.null(types[[var]]) &&
          types[[var]] %in% c("f64[]", "f64[,]")) {
          return(paste0(var, ".load[width=_MOJOR_ALIGN_F64, alignment=_MOJOR_ALIGN](", loop_var, ")"))
        }
        if (!is.null(types[[var]]) &&
          types[[var]] %in% c("f32[]", "f32[,]")) {
          return(paste0(var, ".load[width=_MOJOR_ALIGN_F32, alignment=_MOJOR_ALIGN](", loop_var, ")"))
        }
      }
      return(NULL)
    }
    if (op %in% c("+", "-", "*", "/", "^")) {
      a <- .mojor_expr_to_mojo_vec(expr[[2]], loop_var, types)
      b <- .mojor_expr_to_mojo_vec(expr[[3]], loop_var, types)
      if (is.null(a) || is.null(b)) {
        return(NULL)
      }
      if (op == "^") {
        return(paste0("pow(", a, ", ", b, ")"))
      }
      return(paste0("(", a, " ", op, " ", b, ")"))
    }
    if (op %in% c(
      "sin", "cos", "tan", "asin", "acos", "atan",
      "sinh", "cosh", "tanh", "log1p", "expm1",
      "floor", "ceiling", "trunc", "round", "sign", "abs", "abs2",
      "cbrt", "lgamma", "erf", "gamma"
    )) {
      a <- .mojor_expr_to_mojo_vec(expr[[2]], loop_var, types)
      if (is.null(a)) {
        return(NULL)
      }
      if (op == "abs2") {
        return(paste0("(", a, " * ", a, ")"))
      }
      if (op == "sign") {
        return(paste0("(Float64(-1.0) if ", a, " < 0.0 else (Float64(1.0) if ", a, " > 0.0 else Float64(0.0)))"))
      }
      mop <- if (op == "ceiling") "ceil" else if (op == "gamma") "tgamma" else op
      return(paste0(mop, "(", a, ")"))
    }
    if (op %in% c("min", "max", "pmin", "pmax", "hypot", "atan2")) {
      a <- .mojor_expr_to_mojo_vec(expr[[2]], loop_var, types)
      b <- .mojor_expr_to_mojo_vec(expr[[3]], loop_var, types)
      if (is.null(a) || is.null(b)) {
        return(NULL)
      }
      mop <- if (op == "pmin") "min" else if (op == "pmax") "max" else op
      return(paste0(mop, "(", a, ", ", b, ")"))
    }
  }
  NULL
}

.mojor_expr_to_mojo_vec_chunks <- function(expr, loop_var, types) {
  arrays <- character(0)
  build <- function(e) {
    if (is.numeric(e)) {
      return(.mojor_mojo_numeric_literal(e))
    }
    if (is.name(e)) {
      name <- as.character(e)
      return(name)
    }
    if (is.call(e)) {
      op <- as.character(e[[1]])
      if (op %in% c("[", "[[") && length(e) >= 3) {
        if (length(e) >= 4) {
          return(NULL)
        }
        if (!is.name(e[[2]])) {
          return(NULL)
        }
        var <- as.character(e[[2]])
        idx <- e[[3]]
        if (is.name(idx) && as.character(idx) == loop_var &&
          !is.null(types[[var]]) &&
          types[[var]] %in% c("f64[]", "f32[]", "f64[,]", "f32[,]")) {
          arrays <<- unique(c(arrays, var))
          return(paste0(var, "_chunk"))
        }
        return(NULL)
      }
      if (op %in% c(
        "+", "-", "*", "/", "min", "max", "abs", "pmin", "pmax", "^",
        "sin", "cos", "tan", "asin", "acos", "atan", "atan2",
        "sinh", "cosh", "tanh", "log1p", "expm1",
        "floor", "ceiling", "trunc", "round", "sign", "abs2",
        "hypot", "cbrt", "lgamma", "erf", "gamma"
      )) {
        if (op %in% c(
          "sin", "cos", "tan", "asin", "acos", "atan", "sinh", "cosh", "tanh", "log1p", "expm1",
          "floor", "ceiling", "trunc", "round", "sign", "abs2", "cbrt", "lgamma", "erf", "gamma"
        )) {
          a <- build(e[[2]])
          if (is.null(a)) {
            return(NULL)
          }
          if (op == "abs2") {
            return(paste0("(", a, " * ", a, ")"))
          }
          if (op == "sign") {
            return(paste0("(Float64(-1.0) if ", a, " < 0.0 else (Float64(1.0) if ", a, " > 0.0 else Float64(0.0)))"))
          }
          mop <- if (op == "ceiling") "ceil" else if (op == "gamma") "tgamma" else op
          return(paste0(mop, "(", a, ")"))
        }
        if (op == "hypot") {
          a <- build(e[[2]])
          b <- build(e[[3]])
          if (is.null(a) || is.null(b)) {
            return(NULL)
          }
          return(paste0("hypot(", a, ", ", b, ")"))
        }
        if (op == "atan2") {
          a <- build(e[[2]])
          b <- build(e[[3]])
          if (is.null(a) || is.null(b)) {
            return(NULL)
          }
          return(paste0("atan2(", a, ", ", b, ")"))
        }
        if (op == "abs") {
          a <- build(e[[2]])
          if (is.null(a)) {
            return(NULL)
          }
          return(paste0("abs(", a, ")"))
        }
        if (op %in% c("min", "max", "pmin", "pmax")) {
          mop <- if (op == "pmin") "min" else if (op == "pmax") "max" else op
          is_simd_expr <- function(txt) {
            is.character(txt) && length(txt) == 1L &&
              grepl("_chunk\\b", txt, perl = TRUE)
          }
          promote_to_simd <- function(txt) {
            paste0("SIMD[dtype, simd_width](", txt, ")")
          }
          args <- as.list(e)[-1]
          if (length(args) < 2) {
            return(NULL)
          }
          acc <- build(args[[1]])
          if (is.null(acc)) {
            return(NULL)
          }
          for (k in 2:length(args)) {
            nxt <- build(args[[k]])
            if (is.null(nxt)) {
              return(NULL)
            }
            lhs <- acc
            rhs <- nxt
            lhs_simd <- is_simd_expr(lhs)
            rhs_simd <- is_simd_expr(rhs)
            if (lhs_simd && !rhs_simd) {
              rhs <- promote_to_simd(rhs)
            } else if (!lhs_simd && rhs_simd) {
              lhs <- promote_to_simd(lhs)
            }
            acc <- paste0(mop, "(", lhs, ", ", rhs, ")")
          }
          return(acc)
        }
        if (op == "^") {
          a <- build(e[[2]])
          b <- build(e[[3]])
          if (is.null(a) || is.null(b)) {
            return(NULL)
          }
          return(paste0("pow(", a, ", ", b, ")"))
        }
        a <- build(e[[2]])
        b <- build(e[[3]])
        if (is.null(a) || is.null(b)) {
          return(NULL)
        }
        return(paste0("(", a, " ", op, " ", b, ")"))
      }
    }
    NULL
  }
  expr_str <- build(expr)
  if (is.null(expr_str)) {
    return(NULL)
  }
  list(expr = expr_str, arrays = arrays)
}

.mojor_rename_expr <- function(expr, from, to) {
  if (is.name(expr)) {
    if (as.character(expr) == from) {
      return(as.name(to))
    }
    return(expr)
  }
  if (is.call(expr)) {
    op <- expr[[1]]
    args <- as.list(expr)[-1]
    new_args <- lapply(args, function(e) .mojor_rename_expr(e, from, to))
    return(as.call(c(op, new_args)))
  }
  expr
}

.mojor_rename_block <- function(blocks, from, to) {
  lapply(blocks, function(b) .mojor_rename_expr(b, from, to))
}

.mojor_assignment_target <- function(stmt, loop_vars, out_name, scalar_name) {
  if (!is.call(stmt) || !(as.character(stmt[[1]]) %in% c("<-", "="))) {
    return(NULL)
  }
  lhs <- stmt[[2]]
  if (is.call(lhs) && as.character(lhs[[1]]) == "[") {
    lhs_var <- as.character(lhs[[2]])
    if (!is.null(out_name) && lhs_var == out_name) {
      tensor_map <- .mojor_state$current_tensor_map
      tensor_ranks <- .mojor_state$current_tensor_ranks
      if (length(lhs) == 3) {
        idx <- lhs[[3]]
        if (!is.name(idx) || !(as.character(idx) %in% loop_vars)) {
          return(NULL)
        }
        return(list(kind = "vector", var = lhs_var, idx = as.character(idx)))
      }
      if (length(lhs) == 4) {
        idx1 <- lhs[[3]]
        idx2 <- lhs[[4]]
        if (!is.name(idx1) || !is.name(idx2)) {
          return(NULL)
        }
        if (!(as.character(idx1) %in% loop_vars) || !(as.character(idx2) %in% loop_vars)) {
          return(NULL)
        }
        tensor_name <- .mojor_tensor_name(tensor_map, lhs_var)
        if (!is.null(tensor_name)) {
          idx_list <- .mojor_indexlist_expr(c(as.character(idx1), as.character(idx2)), 2, loop_vars = loop_vars)
          if (is.null(idx_list)) {
            return(NULL)
          }
          return(list(kind = "vector", var = lhs_var, idx = idx_list, target_expr = paste0(tensor_name, "[", idx_list, "]")))
        }
        out_nrow_var <- .mojor_state$current_out_nrow_var
        if (is.null(out_nrow_var)) {
          return(NULL)
        }
        lin <- paste0("(", as.character(idx1), " - 1 + (", as.character(idx2), " - 1) * ", out_nrow_var, ")")
        return(list(kind = "vector", var = lhs_var, idx = lin))
      }
      if (length(lhs) > 4) {
        out_dim_var <- .mojor_state$current_out_dim_var
        if (is.null(out_dim_var)) {
          return(NULL)
        }
        idxs <- as.list(lhs)[3:length(lhs)]
        if (!all(vapply(idxs, function(ix) is.name(ix) && (as.character(ix) %in% loop_vars), logical(1)))) {
          return(NULL)
        }
        idx_names <- vapply(idxs, function(ix) as.character(ix), character(1))
        tensor_name <- .mojor_tensor_name(tensor_map, lhs_var)
        if (!is.null(tensor_name)) {
          idx_list <- .mojor_indexlist_expr(idx_names, length(idx_names), loop_vars = loop_vars)
          if (is.null(idx_list)) {
            return(NULL)
          }
          return(list(kind = "vector", var = lhs_var, idx = idx_list, target_expr = paste0(tensor_name, "[", idx_list, "]")))
        }
        terms <- c(paste0("(", idx_names[[1]], " - 1)"))
        if (length(idx_names) >= 2) {
          prod <- paste0("Int(", out_dim_var, "[0])")
          for (k in 2:length(idx_names)) {
            terms <- c(terms, paste0("(", idx_names[[k]], " - 1) * ", prod))
            if (k < length(idx_names)) {
              prod <- paste0(prod, " * Int(", out_dim_var, "[", k - 1, "])")
            }
          }
        }
        lin <- paste0("(", paste(terms, collapse = " + "), ")")
        return(list(kind = "vector", var = lhs_var, idx = lin))
      }
    }
    return(NULL)
  }
  if (is.name(lhs)) {
    if (!is.null(scalar_name) && as.character(lhs) == scalar_name) {
      return(list(kind = "scalar", var = as.character(lhs)))
    }
  }
  NULL
}

.mojor_na_assign_lines <- function(target, types, indent) character(0)

.mojor_assign_literal <- function(stmt, scalar_name) {
  if (!is.call(stmt) || !(as.character(stmt[[1]]) %in% c("<-", "="))) {
    return(NULL)
  }
  lhs <- stmt[[2]]
  rhs <- stmt[[3]]
  if (!is.name(lhs) || as.character(lhs) != scalar_name) {
    return(NULL)
  }
  if (is.name(rhs) && as.character(rhs) == scalar_name) {
    return("self")
  }
  if (is.name(rhs) && as.character(rhs) %in% c("TRUE", "FALSE")) {
    return(if (as.character(rhs) == "TRUE") "true" else "false")
  }
  if (is.logical(rhs)) {
    return(if (isTRUE(rhs)) "true" else "false")
  }
  NULL
}

.mojor_scalar_sum_reduction <- function(blocks, loop_var, scalar_name, types) {
  if (length(blocks) != 1) {
    return(NULL)
  }
  stmt <- blocks[[1]]
  if (!is.call(stmt) || !(as.character(stmt[[1]]) %in% c("<-", "="))) {
    return(NULL)
  }
  lhs <- stmt[[2]]
  rhs <- stmt[[3]]
  if (!is.name(lhs) || as.character(lhs) != scalar_name) {
    return(NULL)
  }
  if (!is.call(rhs)) {
    return(NULL)
  }
  op <- as.character(rhs[[1]])
  if (op != "+") {
    return(NULL)
  }
  a <- rhs[[2]]
  b <- rhs[[3]]
  is_self <- function(x) is.name(x) && as.character(x) == scalar_name
  if (is_self(a) && !is_self(b)) {
    expr <- b
  } else if (is_self(b) && !is_self(a)) {
    expr <- a
  } else {
    return(NULL)
  }
  vec_chunks <- .mojor_expr_to_mojo_vec_chunks(expr, loop_var, types)
  if (is.null(vec_chunks)) {
    return(NULL)
  }
  list(expr = vec_chunks$expr, arrays = vec_chunks$arrays)
}

.mojor_scalar_reduction <- function(blocks, loop_var, scalar_name, types) {
  if (length(blocks) != 1) {
    return(NULL)
  }
  stmt <- blocks[[1]]
  if (!is.call(stmt) || !(as.character(stmt[[1]]) %in% c("<-", "="))) {
    return(NULL)
  }
  lhs <- stmt[[2]]
  rhs <- stmt[[3]]
  if (!is.name(lhs) || as.character(lhs) != scalar_name) {
    return(NULL)
  }
  if (!is.call(rhs)) {
    return(NULL)
  }
  op <- as.character(rhs[[1]])
  if (!op %in% c("+", "min", "max")) {
    return(NULL)
  }
  if (op == "+") {
    return(.mojor_scalar_sum_reduction(blocks, loop_var, scalar_name, types))
  }
  a <- rhs[[2]]
  b <- rhs[[3]]
  is_self <- function(x) is.name(x) && as.character(x) == scalar_name
  if (is_self(a) && !is_self(b)) {
    expr <- b
  } else if (is_self(b) && !is_self(a)) {
    expr <- a
  } else {
    return(NULL)
  }
  vec_chunks <- .mojor_expr_to_mojo_vec_chunks(expr, loop_var, types)
  if (is.null(vec_chunks)) {
    return(NULL)
  }
  list(expr = vec_chunks$expr, arrays = vec_chunks$arrays, op = op)
}

.mojor_detect_short_circuit <- function(stmt, scalar_name, init_value) {
  if (!is.call(stmt)) {
    return(NULL)
  }
  op <- as.character(stmt[[1]])
  if (op == "if") {
    cond <- stmt[[2]]
    then_blocks <- .mojor_extract_block(stmt[[3]])
    if (length(then_blocks) != 1) {
      return(NULL)
    }
    then_val <- .mojor_assign_literal(then_blocks[[1]], scalar_name)
    if (is.null(then_val)) {
      return(NULL)
    }
    if (!is.null(init_value) && init_value == 0L && then_val == "true") {
      return(list(mode = "any", cond = cond, set = 1L))
    }
    if (!is.null(init_value) && init_value == 1L && then_val == "false") {
      return(list(mode = "all", cond = cond, set = 0L))
    }
  }
  if (op %in% c("<-", "=")) {
    lhs <- stmt[[2]]
    rhs <- stmt[[3]]
    if (is.name(lhs) && as.character(lhs) == scalar_name && is.call(rhs)) {
      rop <- as.character(rhs[[1]])
      if (rop %in% c("||", "&&")) {
        a <- rhs[[2]]
        b <- rhs[[3]]
        a_is_self <- is.name(a) && as.character(a) == scalar_name
        b_is_self <- is.name(b) && as.character(b) == scalar_name
        other <- if (a_is_self && !b_is_self) b else if (b_is_self && !a_is_self) a else NULL
        if (!is.null(other)) {
          if (rop == "||" && !is.null(init_value) && init_value == 0L) {
            return(list(mode = "any", cond = other, set = 1L))
          }
          if (rop == "&&" && !is.null(init_value) && init_value == 1L) {
            return(list(mode = "all", cond = call("!", other), set = 0L))
          }
        }
      }
    }
  }
  NULL
}

.mojor_collect_array_args <- function(expr, arg_specs, args) {
  out <- character(0)
  .safe_call_arg <- function(node, i) {
    is_missing_arg <- tryCatch(
      identical(node[[i]], quote(expr = )),
      error = function(e) {
        grepl("argument.*missing|subscript out of bounds", conditionMessage(e), ignore.case = TRUE)
      }
    )
    if (isTRUE(is_missing_arg)) {
      return(list(missing = TRUE, value = NULL))
    }
    val <- tryCatch(
      node[[i]],
      error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("argument.*missing|subscript out of bounds", msg, ignore.case = TRUE)) {
          return(NULL)
        }
        stop(e)
      }
    )
    if (is.null(val)) {
      return(list(missing = TRUE, value = NULL))
    }
    list(missing = FALSE, value = val)
  }
  if (is.name(expr)) {
    name <- as.character(expr)
    if (name %in% args && .mojor_is_array(arg_specs[[name]])) {
      out <- c(out, name)
    }
    return(out)
  }
  if (is.call(expr)) {
    op <- as.character(expr[[1]])
    if (op %in% c("[", "[[")) {
 # For indexed expressions, prefer index-source arrays first (e.g., x[idx])
 # so synthesized any/all loops can iterate over selector arrays.
      idx_pos <- if (length(expr) >= 3L) seq.int(3L, length(expr)) else integer(0)
      expr_names <- names(expr)
      control_names <- if (op == "[[") c("drop", "exact") else "drop"
      saw_index_array <- FALSE
      for (k in idx_pos) {
        arg_name <- if (!is.null(expr_names) && length(expr_names) >= k) expr_names[[k]] else ""
        if (!is.null(arg_name) && is.character(arg_name) && nzchar(arg_name) &&
            (tolower(arg_name) %in% control_names)) {
          next
        }
        ix_info <- .safe_call_arg(expr, k)
        if (isTRUE(ix_info$missing)) {
          next
        }
        ix_out <- .mojor_collect_array_args(ix_info$value, arg_specs, args)
        if (length(ix_out) > 0) saw_index_array <- TRUE
        out <- c(out, ix_out)
      }
 # If the index expression already depends on array selector(s), do not
 # include the base array in loop-domain equality checks (e.g., x[idx]).
      if (!saw_index_array) {
        base <- expr[[2]]
        if (is.name(base)) {
          name <- as.character(base)
          if (name %in% args && .mojor_is_array(arg_specs[[name]])) {
            out <- c(out, name)
          }
        }
      }
      return(out)
    }
    if (length(expr) >= 2L) {
      for (k in seq.int(2L, length(expr))) {
        part_info <- .safe_call_arg(expr, k)
        if (isTRUE(part_info$missing)) {
          next
        }
        out <- c(out, .mojor_collect_array_args(part_info$value, arg_specs, args))
      }
    }
    return(out)
  }
  out
}

.mojor_expr_has_isna <- function(expr) FALSE

.mojor_collect_value_terms <- function(expr, loop_vars, types) {
  terms <- list()
  if (is.name(expr)) {
    name <- as.character(expr)
    if (!is.null(types[[name]]) && !.mojor_is_array(types[[name]])) {
      terms <- c(terms, list(list(expr = name, spec = types[[name]])))
    }
    return(terms)
  }
  op <- .mojor_call_op_name(expr)
  if (!is.null(op)) {
    if (op %in% c("[", "[[")) {
      var <- expr[[2]]
      idx <- expr[[3]]
      if (is.name(var) && is.name(idx) && as.character(idx) %in% loop_vars) {
        name <- as.character(var)
        spec <- types[[name]]
        if (!is.null(spec)) {
          terms <- c(terms, list(list(expr = paste0(name, "[", as.character(idx), "]"), spec = spec)))
        }
      }
      return(terms)
    }
    for (p in .mojor_call_args(expr)) {
      terms <- c(terms, .mojor_collect_value_terms(p, loop_vars, types))
    }
  }
  terms
}

.mojor_na_checks <- function(expr, loop_vars, types, include_inf = FALSE) character(0)

.mojor_ifelse_parts <- function(expr) {
  if (!identical(.mojor_call_op_name(expr), "ifelse")) {
    return(NULL)
  }
  if (length(expr) != 4) stop("ifelse() must take three arguments")
  list(cond = expr[[2]], yes = expr[[3]], no = expr[[4]])
}

.mojor_ifelse_assign_vector <- function(lhs_var, idx, cond, yes, no, loop_vars, types, target_spec, indent, target_expr = NULL) {
  cond_mojo <- .mojor_expr_to_mojo(cond, loop_vars, types, in_cond = TRUE)
  if (.mojor_expr_kind(cond, types) != "bool") {
    cond_mojo <- paste0("(", cond_mojo, " != 0)")
  }
  include_inf <- .mojor_is_logical(target_spec)
  cond_checks <- .mojor_na_checks(cond, loop_vars, types, include_inf = FALSE)
  yes_checks <- .mojor_na_checks(yes, loop_vars, types, include_inf = include_inf)
  no_checks <- .mojor_na_checks(no, loop_vars, types, include_inf = include_inf)

  if (.mojor_is_logical(target_spec)) {
    if (.mojor_expr_kind(yes, types) != "bool" || .mojor_expr_kind(no, types) != "bool") {
      stop("Logical outputs must be assigned from boolean expressions")
    }
    yes_cond <- .mojor_expr_to_mojo(yes, loop_vars, types, in_cond = TRUE)
    no_cond <- .mojor_expr_to_mojo(no, loop_vars, types, in_cond = TRUE)
    yes_mojo <- paste0("Int32(1 if ", yes_cond, " else 0)")
    no_mojo <- paste0("Int32(1 if ", no_cond, " else 0)")
  } else {
    if (!is.null(target_spec) && target_spec %in% c("i32[]")) {
      if (.mojor_expr_kind(yes, types) == "float" || .mojor_expr_kind(no, types) == "float") {
        stop("Integer outputs require explicit cast (use as.integer())")
      }
    }
    yes_mojo <- .mojor_expr_to_mojo(yes, loop_vars, types)
    no_mojo <- .mojor_expr_to_mojo(no, loop_vars, types)
  }

  if (!is.null(target_spec) && target_spec %in% c("f64[]", "f32[]")) {
    nan_val <- if (target_spec == "f32[]") "Float32(0.0/0.0)" else "_MOJOR_NAN"
  } else {
    nan_val <- "-2147483648"
  }

  if (is.null(target_expr)) {
    assign_yes <- paste0(lhs_var, "[", idx, "] = ", yes_mojo)
    assign_no <- paste0(lhs_var, "[", idx, "] = ", no_mojo)
    assign_na <- paste0(lhs_var, "[", idx, "] = ", nan_val)
  } else {
    assign_yes <- paste0(target_expr, " = ", yes_mojo)
    assign_no <- paste0(target_expr, " = ", no_mojo)
    assign_na <- paste0(target_expr, " = ", nan_val)
  }

  lines <- character(0)
  base_indent <- indent
  if (length(cond_checks) > 0) {
    cond_na <- paste(cond_checks, collapse = " or ")
    lines <- c(
      lines,
      paste0(base_indent, "if ", cond_na, ":"),
      paste0(base_indent, "    ", assign_na),
      paste0(base_indent, "else:")
    )
    base_indent <- paste0(base_indent, "    ")
  }

  lines <- c(lines, paste0(base_indent, "if ", cond_mojo, ":"))
  if (length(yes_checks) > 0) {
    yes_na <- paste(yes_checks, collapse = " or ")
    lines <- c(
      lines,
      paste0(base_indent, "    if ", yes_na, ":"),
      paste0(base_indent, "        ", assign_na),
      paste0(base_indent, "    else:"),
      paste0(base_indent, "        ", assign_yes)
    )
  } else {
    lines <- c(lines, paste0(base_indent, "    ", assign_yes))
  }
  lines <- c(lines, paste0(base_indent, "else:"))
  if (length(no_checks) > 0) {
    no_na <- paste(no_checks, collapse = " or ")
    lines <- c(
      lines,
      paste0(base_indent, "    if ", no_na, ":"),
      paste0(base_indent, "        ", assign_na),
      paste0(base_indent, "    else:"),
      paste0(base_indent, "        ", assign_no)
    )
  } else {
    lines <- c(lines, paste0(base_indent, "    ", assign_no))
  }
  lines
}

.mojor_ifelse_assign_scalar <- function(lhs_var, cond, yes, no, loop_vars, types, target_spec, indent, break_on_na = TRUE) {
  cond_mojo <- .mojor_expr_to_mojo(cond, loop_vars, types, in_cond = TRUE)
  if (.mojor_expr_kind(cond, types) != "bool") {
    cond_mojo <- paste0("(", cond_mojo, " != 0)")
  }
  include_inf <- .mojor_is_logical(target_spec)
  cond_checks <- .mojor_na_checks(cond, loop_vars, types, include_inf = FALSE)
  yes_checks <- .mojor_na_checks(yes, loop_vars, types, include_inf = include_inf)
  no_checks <- .mojor_na_checks(no, loop_vars, types, include_inf = include_inf)

  if (.mojor_is_logical(target_spec)) {
    if (.mojor_expr_kind(yes, types) != "bool" || .mojor_expr_kind(no, types) != "bool") {
      stop("Logical outputs must be assigned from boolean expressions")
    }
    yes_cond <- .mojor_expr_to_mojo(yes, loop_vars, types, in_cond = TRUE)
    no_cond <- .mojor_expr_to_mojo(no, loop_vars, types, in_cond = TRUE)
    yes_mojo <- paste0("Int32(1 if ", yes_cond, " else 0)")
    no_mojo <- paste0("Int32(1 if ", no_cond, " else 0)")
  } else {
    if (!is.null(target_spec) && target_spec %in% c("i32")) {
      if (.mojor_expr_kind(yes, types) == "float" || .mojor_expr_kind(no, types) == "float") {
        stop("Integer outputs require explicit cast (use as.integer())")
      }
    }
    yes_mojo <- .mojor_expr_to_mojo(yes, loop_vars, types)
    no_mojo <- .mojor_expr_to_mojo(no, loop_vars, types)
  }

  if (!is.null(target_spec) && target_spec %in% c("f64", "f32")) {
    nan_val <- if (target_spec == "f32") "Float32(0.0/0.0)" else "_MOJOR_NAN"
  } else {
    nan_val <- "-2147483648"
  }

  assign_yes <- paste0(lhs_var, " = ", yes_mojo)
  assign_no <- paste0(lhs_var, " = ", no_mojo)
  assign_na <- paste0(lhs_var, " = ", nan_val)

  lines <- character(0)
  base_indent <- indent
  if (length(cond_checks) > 0) {
    cond_na <- paste(cond_checks, collapse = " or ")
    lines <- c(
      lines,
      paste0(base_indent, "if ", cond_na, ":"),
      paste0(base_indent, "    ", assign_na),
      if (break_on_na) paste0(base_indent, "    break") else NULL,
      paste0(base_indent, "else:")
    )
    base_indent <- paste0(base_indent, "    ")
  }

  lines <- c(lines, paste0(base_indent, "if ", cond_mojo, ":"))
  if (length(yes_checks) > 0) {
    yes_na <- paste(yes_checks, collapse = " or ")
    lines <- c(
      lines,
      paste0(base_indent, "    if ", yes_na, ":"),
      paste0(base_indent, "        ", assign_na),
      if (break_on_na) paste0(base_indent, "        break") else NULL,
      paste0(base_indent, "    else:"),
      paste0(base_indent, "        ", assign_yes)
    )
  } else {
    lines <- c(lines, paste0(base_indent, "    ", assign_yes))
  }
  lines <- c(lines, paste0(base_indent, "else:"))
  if (length(no_checks) > 0) {
    no_na <- paste(no_checks, collapse = " or ")
    lines <- c(
      lines,
      paste0(base_indent, "    if ", no_na, ":"),
      paste0(base_indent, "        ", assign_na),
      if (break_on_na) paste0(base_indent, "        break") else NULL,
      paste0(base_indent, "    else:"),
      paste0(base_indent, "        ", assign_no)
    )
  } else {
    lines <- c(lines, paste0(base_indent, "    ", assign_no))
  }
  lines
}

.mojor_lower_ifelse_expr <- function(expr, loop_vars, types, target_spec, indent) {
  if (.mojor_expr_contains_ifelse(expr) && isTRUE(.mojor_state$options$warn_ifelse)) {
    .mojor_warn("ifelse() expression lowered to a temporary", expr, "prefer explicit loop assignment for best performance")
  }
  state <- new.env(parent = emptyenv())
  state$idx <- 1L
  state$lines <- character(0)
  state$temp_types <- list()
  base_spec <- sub("\\[\\]$", "", target_spec)
  base_ty <- .mojor_mojo_type(base_spec)

  walk <- function(e) {
    if (is.call(e) && as.character(e[[1]]) == "ifelse") {
      parts <- .mojor_ifelse_parts(e)
      cond_expr <- walk(parts$cond)
      yes_expr <- walk(parts$yes)
      no_expr <- walk(parts$no)
      tmp_name <- paste0("_mojor_ifelse_", state$idx)
      state$idx <- state$idx + 1L
      state$temp_types[[tmp_name]] <- base_spec
      if (length(state$lines) > 0) {
        state$lines <- c(state$lines, "")
      }
      state$lines <- c(state$lines, paste0(indent, "var ", tmp_name, ": ", base_ty))
      state$lines <- c(
        state$lines,
        .mojor_ifelse_assign_scalar(
          tmp_name,
          cond_expr,
          yes_expr,
          no_expr,
          loop_vars,
          c(types, state$temp_types),
          base_spec,
          indent,
          break_on_na = FALSE
        )
      )
      return(as.name(tmp_name))
    }
    if (is.call(e)) {
      op <- as.character(e[[1]])
      new_args <- lapply(as.list(e)[-1], walk)
      return(as.call(c(as.name(op), new_args)))
    }
    e
  }

  lowered <- walk(expr)
  list(lines = state$lines, expr = lowered, temp_types = state$temp_types)
}

.mojor_vector_expr_to_element <- function(expr, loop_var, arg_specs, args) {
  if (is.numeric(expr) || is.logical(expr)) {
    return(expr)
  }
  if (is.name(expr)) {
    name <- as.character(expr)
    if (name %in% args && .mojor_is_array(arg_specs[[name]])) {
      return(call("[", as.name(name), as.name(loop_var)))
    }
    return(expr)
  }
  if (is.call(expr)) {
    op <- as.character(expr[[1]])
    if (op == "&" || op == "|") {
      op <- if (op == "&") "&&" else "||"
      new_args <- lapply(as.list(expr)[-1], function(e) .mojor_vector_expr_to_element(e, loop_var, arg_specs, args))
      return(as.call(c(as.name(op), new_args)))
    }
    if (op == "xor") {
      if (length(expr) != 3) stop("xor() must take two arguments")
      lhs <- .mojor_vector_expr_to_element(expr[[2]], loop_var, arg_specs, args)
      rhs <- .mojor_vector_expr_to_element(expr[[3]], loop_var, arg_specs, args)
      return(call("!=", call("as.logical", lhs), call("as.logical", rhs)))
    }
    if (op %in% c("is.na", "is.nan", "is.finite", "is.infinite")) {
      if (length(expr) < 2) stop("is.na() must take one argument")
      inner <- .mojor_vector_expr_to_element(expr[[2]], loop_var, arg_specs, args)
      return(call(op, inner))
    }
    if (op == "as.logical") {
      if (length(expr) < 2) stop("as.logical() must take one argument")
      inner <- .mojor_vector_expr_to_element(expr[[2]], loop_var, arg_specs, args)
      return(call("as.logical", inner))
    }
    if (op == "ifelse") {
      if (length(expr) != 4) stop("ifelse() must take three arguments")
      cond <- .mojor_vector_expr_to_element(expr[[2]], loop_var, arg_specs, args)
      yes <- .mojor_vector_expr_to_element(expr[[3]], loop_var, arg_specs, args)
      no <- .mojor_vector_expr_to_element(expr[[4]], loop_var, arg_specs, args)
      return(call("ifelse", cond, yes, no))
    }
    if (op == "abs2") {
      if (length(expr) != 2) stop("abs2() must take one argument")
      inner <- .mojor_vector_expr_to_element(expr[[2]], loop_var, arg_specs, args)
      return(call("*", inner, inner))
    }
    if (op == "sign") {
      if (length(expr) != 2) stop("sign() must take one argument")
      inner <- .mojor_vector_expr_to_element(expr[[2]], loop_var, arg_specs, args)
      return(
        call(
          "ifelse",
          call("<", inner, 0.0),
          -1.0,
          call("ifelse", call(">", inner, 0.0), 1.0, 0.0)
        )
      )
    }
    if (op == "atan2") {
      if (length(expr) != 3) stop("atan2() must take two arguments")
      y_val <- .mojor_vector_expr_to_element(expr[[2]], loop_var, arg_specs, args)
      x_val <- .mojor_vector_expr_to_element(expr[[3]], loop_var, arg_specs, args)
      angle <- call("atan", call("/", y_val, x_val))
      return(
        call(
          "ifelse",
          call(">", x_val, 0.0),
          angle,
          call(
            "ifelse",
            call("<", x_val, 0.0),
            call(
              "ifelse",
              call(">=", y_val, 0.0),
              call("+", angle, 3.141592653589793),
              call("-", angle, 3.141592653589793)
            ),
            call(
              "ifelse",
              call(">", y_val, 0.0),
              1.5707963267948966,
              call("ifelse", call("<", y_val, 0.0), -1.5707963267948966, 0.0)
            )
          )
        )
      )
    }
    if (op %in% c("as.integer", "as.double", "as.single")) {
      if (length(expr) < 2) stop(paste0(op, "() must take one argument"))
      inner <- .mojor_vector_expr_to_element(expr[[2]], loop_var, arg_specs, args)
      return(call(op, inner))
    }
    if (op %in% c("[", "[[")) {
      index_args <- as.list(expr)[-c(1, 2)]
      index_names <- names(index_args)
      selector_mask <- rep(TRUE, length(index_args))
      if (!is.null(index_names)) {
        control_names <- if (op == "[[") c("drop", "exact") else "drop"
        selector_mask <- vapply(index_names, function(nm) {
          if (is.null(nm) || is.na(nm) || !nzchar(nm)) {
            return(TRUE)
          }
          !(tolower(nm) %in% control_names)
        }, logical(1))
      }
      selector_args <- index_args[selector_mask]
      is_array_arg <- function(nm) {
        nm %in% args && .mojor_is_array(arg_specs[[nm]])
      }
      if (length(selector_args) == 0) {
        stop("Indexing in any()/all() expressions must include at least one positional selector")
      }
      vectorize_idx <- function(ix) {
        if (is.name(ix)) {
          nm <- as.character(ix)
          if (nm == loop_var) {
            return(ix)
          }
          if (is_array_arg(nm)) {
            return(call("[", as.name(nm), as.name(loop_var)))
          }
          return(ix)
        }
        if (is.call(ix)) {
          ix_op <- as.character(ix[[1]])
          if (ix_op == "length") {
            return(ix)
          }
          parts <- as.list(ix)[-1]
          new_parts <- lapply(parts, vectorize_idx)
          return(as.call(c(as.name(ix_op), new_parts)))
        }
        ix
      }
      new_index_args <- index_args
      if (length(selector_args) > 0) {
        new_index_args[selector_mask] <- lapply(selector_args, vectorize_idx)
      }
      return(as.call(c(list(as.name(op), expr[[2]]), new_index_args)))
    }
    new_args <- lapply(as.list(expr)[-1], function(e) .mojor_vector_expr_to_element(e, loop_var, arg_specs, args))
    return(as.call(c(as.name(op), new_args)))
  }
  .mojor_err("unsupported any()/all() expression", expr, "use array expressions with &, |, comparisons, or is.na/is.nan/is.finite/is.infinite")
}

.mojor_c_type <- function(spec) {
  if (is.null(spec) || !is.character(spec) || length(spec) != 1 || is.na(spec)) {
    return(NULL)
  }

  direct <- switch(spec,
    "f64" = "double",
    "f64[]" = "double*",
    "f64[,]" = "double*", # Matrix
    "i32" = "int",
    "i32[]" = "int*",
    "i32[,]" = "int*", # Matrix
    "f32" = "float",
    "f32[]" = "float*",
    "f32[,]" = "float*", # Matrix
    "lgl" = "int",
    "lgl[]" = "int*",
    "lgl[,]" = "int*", # Matrix
    "bool" = "int",
    "bool[]" = "int*",
    "bool[,]" = "int*", # Matrix
    NULL
  )
  if (!is.null(direct)) {
    return(direct)
  }

  # Generic ND arrays (e.g., f64[3d], i32[4d]) use pointer C ABI.
  if (grepl("\\[", spec, fixed = FALSE)) {
    base <- sub("\\[.*$", "", spec)
    return(switch(base,
      "f64" = "double*",
      "i32" = "int*",
      "f32" = "float*",
      "lgl" = "int*",
      "bool" = "int*",
      "chr" = "int*",
      NULL
    ))
  }

  NULL
}

.mojor_c_ptr_type <- function(spec) {
  base <- sub("\\[.*\\]$", "", spec)
  switch(base,
    "f64" = "double*",
    "i32" = "int*",
    "f32" = "float*",
    "lgl" = "int*",
    "bool" = "int*",
    NULL
  )
}

.mojor_emit_gpu_buf_wrapper <- function(dtype, gpu_buf_name, gpu_call_name, args, arg_specs, array_args, gpu_n_name,
                                        broadcast_nd = FALSE, dim_arrays = NULL,
                                        index_mode = "linear1d", matrix_dim_source = NULL,
                                        matrix_dim_arrays = NULL) {
  tag <- paste0("mojor_gpu_buf_", dtype)
  struct_name <- paste0(tag, "_t")
  alloc_fn <- paste0(tag, "_alloc")
  free_fn <- paste0(tag, "_free")
  ptr_fn <- paste0(tag, "_ptr")
  index_mode <- suppressWarnings(as.character(index_mode[[1]]))
  if (length(index_mode) != 1 || is.na(index_mode) || !nzchar(index_mode)) {
    index_mode <- "linear1d"
  }
  if (!index_mode %in% c("linear1d", "matrix2d")) {
    index_mode <- "linear1d"
  }
  broadcast_nd <- isTRUE(broadcast_nd)
  if (!broadcast_nd) {
    dim_arrays <- character(0)
  } else if (is.null(dim_arrays)) {
    dim_arrays <- array_args
  }
  dim_arrays <- unique(dim_arrays)
  if (!identical(index_mode, "matrix2d")) {
    matrix_dim_source <- NULL
    matrix_dim_arrays <- character(0)
  } else {
    if (is.null(matrix_dim_arrays)) {
      matrix_dim_arrays <- array_args[vapply(array_args, function(a) .mojor_gpu_matrix_array_spec_ok(arg_specs[[a]]), logical(1))]
    }
    matrix_dim_arrays <- unique(intersect(matrix_dim_arrays, array_args))
    if (length(matrix_dim_arrays) == 0) {
      index_mode <- "linear1d"
      matrix_dim_source <- NULL
    } else if (is.null(matrix_dim_source) || !nzchar(matrix_dim_source) || !(matrix_dim_source %in% matrix_dim_arrays)) {
      matrix_dim_source <- matrix_dim_arrays[[1]]
    }
  }
  matrix2d_active <- identical(index_mode, "matrix2d") && length(matrix_dim_arrays) > 0 && !isTRUE(broadcast_nd)
  lines <- c(
    "",
    "typedef struct {",
    "    void* ctx;",
    "    void* handle;",
    "    int n;",
    paste0("} ", struct_name, ";"),
    "",
    "static void* mojor_gpu_ctx_get(SEXP ext) {",
    "    if (ext == R_NilValue) return NULL;",
    "    if (TYPEOF(ext) != EXTPTRSXP) error(\"mojor_gpu_ctx: invalid context\");",
    "    return R_ExternalPtrAddr(ext);",
    "}",
    "",
    "static SEXP mojor_gpu_buf_extract_handle(SEXP ext) {",
    "    if (TYPEOF(ext) != VECSXP) return ext;",
    "    SEXP names = getAttrib(ext, R_NamesSymbol);",
    "    if (names == R_NilValue) return ext;",
    "    for (R_xlen_t i = 0; i < XLENGTH(names); i++) {",
    "        if (strcmp(CHAR(STRING_ELT(names, i)), \"handle\") == 0) return VECTOR_ELT(ext, i);",
    "    }",
    "    return ext;",
    "}",
    "",
    "static SEXP mojor_gpu_buf_extract_dim(SEXP ext) {",
    "    if (TYPEOF(ext) != VECSXP) {",
    "        SEXP mojor_dim = getAttrib(ext, install(\"mojor_dim\"));",
    "        if (mojor_dim != R_NilValue) return mojor_dim;",
    "        return getAttrib(ext, R_DimSymbol);",
    "    }",
    "    SEXP names = getAttrib(ext, R_NamesSymbol);",
    "    if (names != R_NilValue) {",
    "        for (R_xlen_t i = 0; i < XLENGTH(names); i++) {",
    "            if (strcmp(CHAR(STRING_ELT(names, i)), \"dim\") == 0) return VECTOR_ELT(ext, i);",
    "        }",
    "    }",
    "    return getAttrib(ext, R_DimSymbol);",
    "}",
    "",
    paste0("static ", struct_name, "* ", tag, "_get(SEXP ext) {"),
    "    ext = mojor_gpu_buf_extract_handle(ext);",
    "    if (TYPEOF(ext) != EXTPTRSXP) return NULL;",
    paste0("    return (", struct_name, "*) R_ExternalPtrAddr(ext);"),
    "}",
    "",
    paste0("extern void* ", alloc_fn, "(void* ctx, int n);"),
    paste0("extern int ", free_fn, "(void* ctx, void* handle);"),
    paste0("extern void* ", ptr_fn, "(void* handle);")
  )
  gpu_sig_parts <- c("void* ctx", "void* out_bufp", "int n")
  for (a in args) {
    spec <- arg_specs[[a]]
    if (.mojor_is_array(spec)) {
      gpu_sig_parts <- c(gpu_sig_parts, paste0("void* ", a, "_bufp"))
    } else {
      gpu_sig_parts <- c(gpu_sig_parts, paste0(.mojor_c_type(spec), " ", a))
    }
  }
  if (isTRUE(matrix2d_active)) {
    gpu_sig_parts <- c(gpu_sig_parts, "int __mojor_m2_nrow", "int __mojor_m2_ncol")
  }
  if (broadcast_nd && length(dim_arrays) > 0) {
    gpu_sig_parts <- c(
      gpu_sig_parts,
      paste0("int* ", .mojor_out_dim_param_name(), "_ptr"),
      paste0("int ", .mojor_out_ndim_param_name())
    )
    for (a in dim_arrays) {
      gpu_sig_parts <- c(
        gpu_sig_parts,
        paste0("int* ", .mojor_dim_param_name(a), "_ptr"),
        paste0("int ", .mojor_ndim_param_name(a))
      )
    }
  }
  gpu_sig_parts <- c(gpu_sig_parts, "int* status")
  lines <- c(lines, paste0("extern int ", gpu_buf_name, "(", paste(gpu_sig_parts, collapse = ", "), ");"))
  lines <- c(
    lines,
    "",
    paste0("static void ", tag, "_finalizer(SEXP ext) {"),
    paste0("    ", struct_name, "* buf = ", tag, "_get(ext);"),
    "    if (!buf) return;",
    "    if (buf->handle) {",
    paste0("        ", free_fn, "(buf->ctx, buf->handle);"),
    "        buf->handle = NULL;",
    "    }",
    "    buf->ctx = NULL;",
    "    R_Free(buf);",
    "    R_ClearExternalPtr(ext);",
    "}",
    "",
    paste0("SEXP ", gpu_call_name, "(SEXP r_ctx, SEXP ", paste(args, collapse = ", SEXP "), ") {"),
    "    void* ctx = mojor_gpu_ctx_get(r_ctx);",
    paste0("    if (!ctx) error(\"", tag, ": GPU context unavailable\");")
  )
  for (a in array_args) {
    lines <- c(
      lines,
      sprintf("    %s* %s_buf = %s_get(%s);", struct_name, a, tag, a),
      sprintf("    if (!%s_buf || !%s_buf->handle) error(\"%s: invalid gpu buffer\");", a, a, a)
    )
  }
  lines <- c(lines, "    int n = 0;")
  if (broadcast_nd && length(dim_arrays) > 0) {
    for (a in dim_arrays) {
      lines <- c(
        lines,
        sprintf("    SEXP dim_%s = mojor_gpu_buf_extract_dim(%s);", a, a),
        sprintf("    int* %s_ptr = NULL;", .mojor_dim_param_name(a)),
        sprintf("    int %s = 1;", .mojor_ndim_param_name(a)),
        sprintf("    if (dim_%s == R_NilValue) {", a),
        sprintf("      %s_ptr = (int*) R_alloc(1, sizeof(int));", .mojor_dim_param_name(a)),
        sprintf("      %s_ptr[0] = %s_buf->n;", .mojor_dim_param_name(a), a),
        "    } else {",
        sprintf("      %s_ptr = INTEGER(dim_%s);", .mojor_dim_param_name(a), a),
        sprintf("      %s = LENGTH(dim_%s);", .mojor_ndim_param_name(a), a),
        "    }"
      )
    }
    lines <- c(lines, "    int __mojor_bc_ndim = 1;")
    for (a in dim_arrays) {
      lines <- c(lines, sprintf("    if (%s > __mojor_bc_ndim) __mojor_bc_ndim = %s;", .mojor_ndim_param_name(a), .mojor_ndim_param_name(a)))
    }
    lines <- c(
      lines,
      "    int* __mojor_bc_dim = (int*) R_alloc((size_t) __mojor_bc_ndim, sizeof(int));",
      "    for (int i = 0; i < __mojor_bc_ndim; i++) __mojor_bc_dim[i] = 1;"
    )
    for (a in dim_arrays) {
      lines <- c(
        lines,
        sprintf("    for (int di = 0; di < %s; di++) {", .mojor_ndim_param_name(a)),
        sprintf("      int axis = __mojor_bc_ndim - %s + di;", .mojor_ndim_param_name(a)),
        sprintf("      int d = %s_ptr[di];", .mojor_dim_param_name(a)),
        "      if (d < 0) error(\"broadcast: dim must be non-negative\");",
        "      int cur = __mojor_bc_dim[axis];",
        "      if (cur == 1) __mojor_bc_dim[axis] = d;",
        "      else if (d != 1 && d != cur) error(\"broadcast: incompatible dims\");",
        "    }"
      )
    }
    lines <- c(
      lines,
      "    n = 1;",
      "    for (int i = 0; i < __mojor_bc_ndim; i++) n *= __mojor_bc_dim[i];",
      sprintf("    int %s = __mojor_bc_ndim;", .mojor_out_ndim_param_name()),
      sprintf("    int* %s_ptr = __mojor_bc_dim;", .mojor_out_dim_param_name())
    )
  } else {
    if (isTRUE(matrix2d_active)) {
      for (a in matrix_dim_arrays) {
        lines <- c(
          lines,
          sprintf("    SEXP dim_%s = mojor_gpu_buf_extract_dim(%s);", a, a),
          sprintf("    if (dim_%s == R_NilValue || LENGTH(dim_%s) != 2) error(\"%s: matrix dim metadata required\");", a, a, a)
        )
      }
      lines <- c(
        lines,
        sprintf("    int __mojor_m2_nrow = INTEGER(dim_%s)[0];", matrix_dim_source),
        sprintf("    int __mojor_m2_ncol = INTEGER(dim_%s)[1];", matrix_dim_source),
        "    if (__mojor_m2_nrow <= 0 || __mojor_m2_ncol <= 0) error(\"matrix2d: dims must be positive\");",
        "    n = __mojor_m2_nrow * __mojor_m2_ncol;"
      )
      for (a in matrix_dim_arrays) {
        lines <- c(
          lines,
          sprintf("    if (INTEGER(dim_%s)[0] != __mojor_m2_nrow || INTEGER(dim_%s)[1] != __mojor_m2_ncol) error(\"matrix2d: incompatible dims\");", a, a),
          sprintf("    if (%s_buf->n != n) error(\"%s: length mismatch\");", a, a)
        )
      }
      for (a in setdiff(array_args, matrix_dim_arrays)) {
        lines <- c(lines, sprintf("    if (%s_buf->n != n) error(\"%s: length mismatch\");", a, a))
      }
    } else {
      lines <- c(lines, sprintf("    n = %s_buf->n;", gpu_n_name))
      for (a in array_args) {
        lines <- c(lines, sprintf("    if (%s_buf->n != n) error(\"%s: length mismatch\");", a, a))
      }
    }
  }
  for (a in args) {
    spec <- arg_specs[[a]]
    if (.mojor_is_array(spec)) next
    if (spec %in% c("f64", "f32")) {
      c_type <- .mojor_c_type(spec)
      lines <- c(
        lines,
        sprintf("    if ((TYPEOF(%s) != REALSXP && TYPEOF(%s) != INTSXP) || LENGTH(%s) != 1) error(\"%s: expected numeric scalar\");", a, a, a, a),
        sprintf("    %s %s_val = (TYPEOF(%s) == REALSXP) ? (%s) REAL(%s)[0] : (%s) INTEGER(%s)[0];", c_type, a, a, c_type, a, c_type, a)
      )
    }
  }
  lines <- c(
    lines,
    "    int status = 0;",
    paste0("    void* out_handle = ", alloc_fn, "(ctx, n);"),
    paste0("    if (!out_handle) error(\"", tag, ": allocation failed\");"),
    paste0("    void* out_bufp = ", ptr_fn, "(out_handle);"),
    paste0("    if (!out_bufp) error(\"", tag, ": output buffer unavailable\");")
  )
  gpu_call_args <- c("ctx", "out_bufp", "n")
  for (a in args) {
    spec <- arg_specs[[a]]
    if (.mojor_is_array(spec)) {
      lines <- c(lines, sprintf("    void* %s_bufp = %s(%s_buf->handle);", a, ptr_fn, a))
      lines <- c(lines, sprintf("    if (!%s_bufp) error(\"%s: buffer unavailable\");", a, a))
      gpu_call_args <- c(gpu_call_args, paste0(a, "_bufp"))
    } else if (spec %in% c("f64", "f32")) {
      gpu_call_args <- c(gpu_call_args, paste0(a, "_val"))
    }
  }
  if (isTRUE(matrix2d_active)) {
    gpu_call_args <- c(gpu_call_args, "__mojor_m2_nrow", "__mojor_m2_ncol")
  }
  if (broadcast_nd && length(dim_arrays) > 0) {
    gpu_call_args <- c(
      gpu_call_args,
      paste0(.mojor_out_dim_param_name(), "_ptr"),
      .mojor_out_ndim_param_name()
    )
    for (a in dim_arrays) {
      gpu_call_args <- c(
        gpu_call_args,
        paste0(.mojor_dim_param_name(a), "_ptr"),
        .mojor_ndim_param_name(a)
      )
    }
  }
  gpu_call_args <- c(gpu_call_args, "&status")
  lines <- c(
    lines,
    sprintf("    int ok = %s(%s);", gpu_buf_name, paste(gpu_call_args, collapse = ", ")),
    paste0("    if (!ok) error(\"", tag, ": kernel failed (status=%d)\", status);"),
    paste0("    ", struct_name, "* out = (", struct_name, "*) R_Calloc(1, ", struct_name, ");"),
    "    out->ctx = ctx;",
    "    out->handle = out_handle;",
    "    out->n = n;",
    paste0("    SEXP ext = PROTECT(R_MakeExternalPtr(out, install(\"", tag, "\"), R_NilValue));"),
    paste0("    R_RegisterCFinalizerEx(ext, ", tag, "_finalizer, TRUE);"),
    "    setAttrib(ext, install(\"n\"), ScalarInteger(n));",
    "    setAttrib(ext, install(\"gpu_status\"), ScalarInteger(status));",
    "    UNPROTECT(1);",
    "    return ext;",
    "}"
  )
  lines
}

.mojor_len_expr_c <- function(arg, spec) {
  if (identical(spec, "f32[]")) {
    return(paste0("__mojor_len_", arg))
  }
  paste0("LENGTH(", arg, ")")
}

# Compute C-level output length expression for N-d subscript (e.g., mat[-1, ], mat[c(1,3), ])
# Product of all dims with: excluded dim reduced, scalar dim omitted, vec_sel uses count
.mojor_nd_exclusion_len_expr_c <- function(out_len_source, arg_specs) {
  nd_base <- out_len_source$base
  nd_n_dims <- out_len_source$n_dims
  nd_excl_dims <- if (!is.null(out_len_source$excl_dims)) out_len_source$excl_dims else {
    if (!is.null(out_len_source$excl_dim)) out_len_source$excl_dim else integer(0)
  }
  nd_scalar_dims <- if (!is.null(out_len_source$scalar_dims)) out_len_source$scalar_dims else integer(0)
  nd_vec_sel_dims <- if (!is.null(out_len_source$vec_sel_dims)) out_len_source$vec_sel_dims else integer(0)
  nd_vec_sel_infos <- if (!is.null(out_len_source$vec_sel_infos)) out_len_source$vec_sel_infos else list()
  nd_neg_vec_excl_dims <- if (!is.null(out_len_source$neg_vec_excl_dims)) out_len_source$neg_vec_excl_dims else integer(0)
  nd_neg_vec_excl_infos <- if (!is.null(out_len_source$neg_vec_excl_infos)) out_len_source$neg_vec_excl_infos else list()
  nd_excl_count <- if (!is.null(out_len_source$excl_count)) out_len_source$excl_count else "1"
  dim_parts <- character(0)
  for (d in seq_len(nd_n_dims)) {
    if (d %in% nd_scalar_dims) {
      # Scalar dim selects one slice — contributes 1 (omit from product)
      next
    }
    dim_expr_d <- if (d == 1L) .mojor_nrow_param_name(nd_base)
                  else if (d == 2L) .mojor_ncol_param_name(nd_base)
                  else paste0("__mojor_dim", d, "_", nd_base)
    if (d %in% nd_excl_dims) {
      dim_parts <- c(dim_parts, paste0("(", dim_expr_d, " - ", nd_excl_count, ")"))
    } else if (d %in% nd_vec_sel_dims) {
      # Vector selection: contributes the count of selected elements
      sel_idx <- match(d, nd_vec_sel_dims)
      sel_info <- nd_vec_sel_infos[[sel_idx]]
      if (!is.null(sel_info$count) && is.numeric(sel_info$count)) {
        dim_parts <- c(dim_parts, as.character(as.integer(sel_info$count)))
      } else if (sel_info$type == "range" && !is.null(sel_info$start_r) && !is.null(sel_info$end_r)) {
        a <- sel_info$start_r; b <- sel_info$end_r
        if (is.numeric(a) && is.numeric(b)) {
          dim_parts <- c(dim_parts, as.character(as.integer(b) - as.integer(a) + 1L))
        } else {
          # Dynamic range — use expression
          a_c <- if (is.name(a)) paste0("asInteger(", as.character(a), ")") else as.character(as.integer(a))
          b_c <- if (is.name(b)) paste0("asInteger(", as.character(b), ")") else as.character(as.integer(b))
          dim_parts <- c(dim_parts, paste0("(", b_c, " - ", a_c, " + 1)"))
        }
      } else {
        dim_parts <- c(dim_parts, as.character(as.integer(sel_info$count)))
      }
    } else if (d %in% nd_neg_vec_excl_dims) {
      # Negative vector exclusion: dim_size - count_of_excluded
      nve_idx <- match(d, nd_neg_vec_excl_dims)
      nve_info <- nd_neg_vec_excl_infos[[nve_idx]]
      # nve_info$value is the call (c(...) or a:b), count elements
      val <- nve_info$value
      if (is.call(val)) {
        val_op <- as.character(val[[1]])
        if (val_op == "c") {
          cnt <- length(as.list(val)) - 1L
        } else if (val_op == ":") {
          a <- val[[2]]; b <- val[[3]]
          if (is.numeric(a) && is.numeric(b)) {
            cnt <- as.integer(b) - as.integer(a) + 1L
          } else { cnt <- NULL }
        } else { cnt <- NULL }
      } else { cnt <- NULL }
      if (!is.null(cnt)) {
        dim_parts <- c(dim_parts, paste0("(", dim_expr_d, " - ", cnt, ")"))
      } else {
        dim_parts <- c(dim_parts, dim_expr_d)
      }
    } else {
      dim_parts <- c(dim_parts, dim_expr_d)
    }
  }
  if (length(dim_parts) == 0L) return("1")
  paste0("(", paste(dim_parts, collapse = " * "), ")")
}

.mojor_sexp_type <- function(spec) {
  if (is.null(spec) || !is.character(spec) || length(spec) != 1 || is.na(spec)) {
    return(NULL)
  }

  direct <- switch(spec,
    "f64" = "REALSXP",
    "f64[]" = "REALSXP",
    "f64[,]" = "REALSXP", # Matrix
    "i32" = "INTSXP",
    "i32[]" = "INTSXP",
    "i32[,]" = "INTSXP", # Matrix
    "f32" = "REALSXP",
    "f32[]" = "REALSXP",
    "f32[,]" = "REALSXP", # Matrix
    "lgl" = "LGLSXP",
    "lgl[]" = "LGLSXP",
    "lgl[,]" = "LGLSXP", # Matrix
    "bool" = "LGLSXP",
    "bool[]" = "LGLSXP",
    "bool[,]" = "LGLSXP", # Matrix
    "chr" = "STRSXP",
    "chr[]" = "STRSXP",
    "chr[,]" = "STRSXP",
    NULL
  )
  if (!is.null(direct)) {
    return(direct)
  }

  # Generic ND arrays (e.g., f64[3d], i32[4d], bool[5d]) map by base dtype.
  if (grepl("^[A-Za-z0-9_]+\\[[0-9]+d\\]$", spec)) {
    base <- sub("\\[.*$", "", spec)
    return(switch(base,
      "f64" = "REALSXP",
      "f32" = "REALSXP",
      "i32" = "INTSXP",
      "lgl" = "LGLSXP",
      "bool" = "LGLSXP",
      "chr" = "STRSXP",
      NULL
    ))
  }

  NULL
}
