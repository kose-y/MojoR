.mojor_ir_verify <- function(node, ctx = list(
                               loop_vars = character(),
                               defined_vars = NULL,
                               check_scope = FALSE,
                               ir_only = FALSE,
                               in_loop = FALSE,
                               type_env = list(),
                               has_returned = FALSE,
                               in_assign_rhs = FALSE
                             )) {
  if (is.null(node)) stop("IR verify: NULL node")
  if (!is.list(node)) stop("IR verify: node must be a list")
  k_raw <- node$kind
  k <- if (exists(".mojor_ir_base_kind", mode = "function")) .mojor_ir_base_kind(k_raw) else k_raw
  if (is.null(k) || !is.character(k) || length(k) != 1) {
    stop("IR verify: node missing valid 'kind' field")
  }

  .req <- function(...) {
    for (f in c(...)) {
      if (is.null(node[[f]])) {
        stop(sprintf("IR verify [%s]: required field '%s' missing", k, f))
      }
    }
  }
  .req_node <- function(f) {
    .req(f)
    .mojor_ir_verify(node[[f]], ctx)
  }
  .req_nodes <- function(f) {
    .req(f)
    if (!is.list(node[[f]])) {
      stop(sprintf("IR verify [%s]: field '%s' must be a list", k, f))
    }
    for (child in node[[f]]) .mojor_ir_verify(child, ctx)
  }

  allowed_binops <- c(
    "+", "-", "*", "/", "%%", "%/%",
    "<", ">", "<=", ">=", "==", "!=",
    "&&", "||", "&", "|", "^"
  )
  allowed_types <- c(
    "i32", "f64", "f32", "bool", "lgl",
    "Int", "Int32", "Int64", "Float64", "Float32",
    "i32[]", "f64[]", "f32[]", "bool[]", "lgl[]"
  )
  allowed_hof_vector_types <- c("f64[]", "i32[]", "lgl[]", "bool[]")
  allowed_hof_named_unary_funs <- c(
    "sin", "cos", "tan", "asin", "acos", "atan",
    "sinh", "cosh", "tanh", "asinh", "acosh", "atanh",
    "log", "log10", "log1p", "log2", "exp", "expm1",
    "sqrt", "abs", "floor", "ceiling", "trunc", "round",
    "sign", "cbrt", "lgamma", "erf", "gamma"
  )
  allowed_hof_named_multiary_funs <- c("pmin", "pmax", "min", "max")
  allowed_tier9_vector_types <- c("f64[]", "f32[]", "i32[]", "lgl[]", "bool[]", "chr[]")
  allowed_tier9_expand_grid_vector_types <- c("f64[]", "i32[]", "lgl[]")
  allowed_tier9_matrix_types <- c(
    "f64[2d]", "f32[2d]", "i32[2d]", "lgl[2d]", "bool[2d]",
    "f64[,]", "f32[,]", "i32[,]", "lgl[,]", "bool[,]"
  )
  allowed_gpu_matmul_matrix_types <- c(
    "f64[2d]", "f32[2d]", "i32[2d]",
    "f64[,]", "f32[,]", "i32[,]"
  )
  allowed_type_predicates <- c(
    "is.vector", "is.matrix", "is.array",
    "is.numeric", "is.integer", "is.double", "is.logical"
  )
  allowed_type_queries <- c("typeof", "mode", "class")
  allowed_vexpr_body_types <- c("f64", "f32", "i32", "bool", "lgl")
  allowed_vexpr_result_types <- c("f64[]", "f32[]", "i32[]", "bool[]", "lgl[]")
  allowed_cov_cor_types <- c(
    "f64[]", "f32[]", "i32[]",
    "f64[2d]", "f32[2d]", "i32[2d]",
    "f64[,]", "f32[,]", "i32[,]"
  )
  .strict_type_mode <- function() {
    isTRUE(ctx$ir_only)
  }
  .selector_marker_names <- c("__pos_vec_sel__", "__pos_vec_selection__")
  .selector_has_unresolved_marker <- function(x, depth = 0L) {
    if (is.null(x) || depth > 64L) {
      return(FALSE)
    }
    if (is.list(x)) {
      if (!is.null(x$kind) && identical(x$kind, "var") &&
          !is.null(x$name) && is.character(x$name) &&
          length(x$name) == 1L && x$name %in% .selector_marker_names) {
        return(TRUE)
      }
      if (!is.null(x$pos_vec_selection)) {
        return(TRUE)
      }
      for (v in x) {
        if (.selector_has_unresolved_marker(v, depth + 1L)) {
          return(TRUE)
        }
      }
      return(FALSE)
    }
    if (is.call(x) || is.pairlist(x)) {
      vals <- as.list(x)
      if (length(vals) > 0L) {
        for (v in vals) {
          if (.selector_has_unresolved_marker(v, depth + 1L)) {
            return(TRUE)
          }
        }
      }
    }
    FALSE
  }
  .verify_selector_canonical_node <- function(idx_node, owner_kind) {
    if (is.null(idx_node) || !is.list(idx_node) || is.null(idx_node$kind)) {
      return(invisible(NULL))
    }
    if (identical(idx_node$kind, "raw")) {
      stop(sprintf(
        "IR verify [%s]: selector raw fallback node is not allowed after canonicalization",
        owner_kind
      ))
    }
    if (identical(idx_node$kind, "scalar_index") &&
        is.list(idx_node$expr) &&
        identical(idx_node$expr$kind, "raw")) {
      stop(sprintf(
        "IR verify [%s]: selector raw fallback node is not allowed after canonicalization",
        owner_kind
      ))
    }
    if (.selector_has_unresolved_marker(idx_node)) {
      stop(sprintf(
        "IR verify [%s]: unresolved selector marker leaked after canonicalization",
        owner_kind
      ))
    }
    invisible(NULL)
  }
  .is_logical_scalar_value <- function(v) {
    if (is.logical(v) && length(v) == 1 && !is.na(v)) {
      return(TRUE)
    }
    if (is.character(v) && length(v) == 1) {
      return(toupper(v) %in% c("TRUE", "FALSE"))
    }
    if (is.numeric(v) && length(v) == 1 && !is.na(v)) {
      return(v %in% c(0, 1))
    }
    FALSE
  }
  .logical_scalar_to_bool <- function(v, field_label, op_label) {
    if (!.is_logical_scalar_value(v)) {
      stop(sprintf(
        "IR verify [%s]: %s must be a single logical value",
        op_label, field_label
      ))
    }
    if (is.logical(v)) {
      return(isTRUE(v))
    }
    if (is.character(v)) {
      return(identical(toupper(v), "TRUE"))
    }
    isTRUE(as.logical(v))
  }
  .verify_replace_arg <- function(replace_node, op_label, strict_type_mode = .strict_type_mode()) {
    if (is.null(replace_node)) {
      return(invisible(NULL))
    }
    if (is.list(replace_node)) {
      .mojor_ir_verify(replace_node, ctx)
      if (replace_node$kind == "const" && !.is_logical_scalar_value(replace_node$value)) {
        stop(sprintf(
          "IR verify [%s]: replace must be a single logical value",
          op_label
        ))
      }
      if (strict_type_mode && !identical(replace_node$kind, "const")) {
        if (is.list(replace_node) && identical(replace_node$kind, "var")) {
          inferred <- tryCatch(
            .mojor_ir_infer_type(replace_node, ctx$type_env),
            error = function(e) "unknown"
          )
          if (!(inferred %in% c("bool", "lgl", "i32"))) {
            replace_name <- as.character(replace_node$name)
            if (!is.character(replace_name) || length(replace_name) != 1L || !nzchar(replace_name)) {
              replace_name <- "replace"
            }
            stop(sprintf(
              "IR verify [%s]: replace argument '%s' must be typed as lgl, bool, or i32 in strict mode",
              op_label,
              replace_name
            ))
          }
        } else {
          .require_type_strict(
            replace_node,
            "replace",
            op_label,
            c("bool", "lgl", "i32")
          )
        }
      }
      return(invisible(NULL))
    }
    if (!.is_logical_scalar_value(replace_node)) {
      stop(sprintf(
        "IR verify [%s]: replace must be a single logical value",
        op_label
      ))
    }
    invisible(NULL)
  }
  .verify_bool_flag_arg <- function(flag_node, field_label, op_label, strict_type_mode = .strict_type_mode()) {
    if (is.null(flag_node)) {
      return(invisible(NULL))
    }
    if (is.list(flag_node)) {
      .mojor_ir_verify(flag_node, ctx)
      if (flag_node$kind == "const" && !.is_logical_scalar_value(flag_node$value)) {
        stop(sprintf(
          "IR verify [%s]: %s must be a single logical value",
          op_label, field_label
        ))
      }
      if (strict_type_mode && !identical(flag_node$kind, "const")) {
        inferred <- tryCatch(
          .mojor_ir_infer_type(flag_node, ctx$type_env),
          error = function(e) "unknown"
        )
        if (!(inferred %in% c("bool", "lgl", "i32"))) {
          stop(sprintf(
            "IR verify [%s]: %s must be a scalar logical argument/literal in strict mode (typed as bool/lgl/i32)",
            op_label,
            field_label
          ))
        }
      }
      return(invisible(NULL))
    }
    if (!.is_logical_scalar_value(flag_node)) {
      stop(sprintf(
        "IR verify [%s]: %s must be a single logical value",
        op_label, field_label
      ))
    }
    invisible(NULL)
  }
  .verify_prob_arg <- function(prob_node, op_label, strict_type_mode = .strict_type_mode()) {
    .is_null_prob <- function(node_prob) {
      if (is.null(node_prob)) {
        return(TRUE)
      }
      if (is.list(node_prob) && identical(node_prob$kind, "var") && identical(node_prob$name, "NULL")) {
        return(TRUE)
      }
      if (is.list(node_prob) && identical(node_prob$kind, "const")) {
        val <- node_prob$value
        if (is.character(val) && length(val) == 1 && identical(toupper(val), "NULL")) {
          return(TRUE)
        }
      }
      FALSE
    }
    if (.is_null_prob(prob_node)) {
      return(invisible(NULL))
    }
    .mojor_ir_verify(prob_node, ctx)
    if (strict_type_mode) {
      if (!(is.list(prob_node) && identical(prob_node$kind, "var"))) {
        stop(sprintf(
          "IR verify [%s]: prob must be a direct f64[] argument in strict mode",
          op_label
        ))
      }
      .require_type_strict(prob_node, "prob", op_label, c("f64[]"))
    }
    invisible(NULL)
  }
  .verify_weighted_replace_strict <- function(prob_node, replace_node, op_label, strict_type_mode = .strict_type_mode()) {
    invisible(NULL)
  }
  .verify_hof_fun <- function(fun_node, op_label, expected_arity = 1L, allow_named = FALSE) {
    is_anon <- is.call(fun_node) && identical(as.character(fun_node[[1]]), "function")
    if (is_anon) {
      return(invisible(NULL))
    }
    if (isTRUE(allow_named) && is.name(fun_node)) {
      fun_name <- as.character(fun_node)
      if (expected_arity == 1L) {
        if (fun_name %in% allowed_hof_named_unary_funs) {
          return(invisible(NULL))
        }
        stop(
          sprintf(
            "IR verify [%s]: FUN named function '%s' is unsupported; use an anonymous function or one of: %s",
            op_label,
            fun_name,
            paste(allowed_hof_named_unary_funs, collapse = ", ")
          )
        )
      }
      if (expected_arity %in% c(2L, 3L)) {
        if (fun_name %in% allowed_hof_named_multiary_funs) {
          return(invisible(NULL))
        }
        stop(
          sprintf(
            "IR verify [%s]: FUN named function '%s' is unsupported for arity %d; use an anonymous function or one of: %s",
            op_label,
            fun_name,
            expected_arity,
            paste(allowed_hof_named_multiary_funs, collapse = ", ")
          )
        )
      }
    }
    if (isTRUE(allow_named)) {
      stop(sprintf(
        "IR verify [%s]: FUN must be an anonymous function or a supported named function in the compiled subset",
        op_label
      ))
    }
    stop(sprintf("IR verify [%s]: FUN must be an anonymous function in the compiled subset", op_label))
  }
  .verify_var_field <- function(field_name, op_label) {
    .req_node(field_name)
    if (node[[field_name]]$kind != "var") {
      stop(sprintf("IR verify [%s]: %s must be a variable", op_label, field_name))
    }
    invisible(NULL)
  }
  .require_chr_array_strict <- function(expr_node, field_label, op_label) {
    strict_type_mode <- .strict_type_mode()
    if (!strict_type_mode) {
      return(invisible(NULL))
    }
    inferred <- tryCatch(
      .mojor_ir_infer_type(expr_node, ctx$type_env),
      error = function(e) "unknown"
    )
    if (!identical(inferred, "chr[]")) {
      if (is.null(inferred) || !is.character(inferred) || length(inferred) != 1 || !nzchar(inferred)) {
        inferred <- "unknown"
      }
      stop(sprintf(
        "IR verify [%s]: %s must have type 'chr[]' in strict mode (got '%s')",
        op_label, field_label, inferred
      ))
    }
    invisible(NULL)
  }
  .is_chr_scalar_literal <- function(expr_node) {
    if (!(is.list(expr_node) && identical(expr_node$kind, "const"))) {
      return(FALSE)
    }
    val <- tryCatch(.mojor_ir_eval_const(expr_node), error = function(e) NULL)
    is.character(val) && length(val) == 1 && !is.na(val)
  }
  .require_chr_scalar_or_literal_strict <- function(expr_node, field_label, op_label) {
    strict_type_mode <- .strict_type_mode()
    if (!strict_type_mode) {
      return(invisible(NULL))
    }
    if (.is_chr_scalar_literal(expr_node)) {
      return(invisible(NULL))
    }
    if (is.list(expr_node) && identical(expr_node$kind, "var")) {
      inferred <- tryCatch(
        .mojor_ir_infer_type(expr_node, ctx$type_env),
        error = function(e) "unknown"
      )
      if (identical(inferred, "chr")) {
        return(invisible(NULL))
      }
      stop(sprintf(
        "IR verify [%s]: %s must have type 'chr' in strict mode",
        op_label,
        field_label
      ))
    }
    stop(sprintf(
      "IR verify [%s]: %s must have type 'chr' in strict mode",
      op_label, field_label
    ))
  }
  .require_direct_var_strict <- function(expr_node, field_label, op_label) {
    strict_type_mode <- .strict_type_mode()
    if (!strict_type_mode) {
      return(invisible(NULL))
    }
    if (is.list(expr_node) && identical(expr_node$kind, "var")) {
      return(invisible(NULL))
    }
    stop(sprintf(
      "IR verify [%s]: %s must be a direct variable in strict mode",
      op_label, field_label
    ))
  }
  .require_type_strict <- function(expr_node, field_label, op_label, allowed_types) {
    strict_type_mode <- .strict_type_mode()
    if (!strict_type_mode) {
      return(invisible(NULL))
    }
    inferred <- tryCatch(
      .mojor_ir_infer_type(expr_node, ctx$type_env),
      error = function(e) "unknown"
    )
    if (!(inferred %in% allowed_types)) {
      if (is.null(inferred) || !is.character(inferred) || length(inferred) != 1 || !nzchar(inferred)) {
        inferred <- "unknown"
      }
      stop(sprintf(
        "IR verify [%s]: %s must have type %s in strict mode (got '%s')",
        op_label,
        field_label,
        paste(sprintf("'%s'", allowed_types), collapse = " or "),
        inferred
      ))
    }
    invisible(NULL)
  }
  .require_nd_array_strict <- function(expr_node, field_label, op_label, min_rank = 2L) {
    strict_type_mode <- .strict_type_mode()
    if (!strict_type_mode) {
      return(invisible(NULL))
    }
    inferred <- tryCatch(
      .mojor_ir_infer_type(expr_node, ctx$type_env),
      error = function(e) "unknown"
    )
    if (is.null(inferred) || !is.character(inferred) || length(inferred) != 1 || !nzchar(inferred)) {
      inferred <- "unknown"
    }
    if (identical(inferred, "unknown") || !.mojor_type_is_array(inferred)) {
      stop(sprintf(
        "IR verify [%s]: %s must have a statically known array type in strict mode (got '%s')",
        op_label, field_label, inferred
      ))
    }
    inferred_rank <- .mojor_type_ndim(inferred)
    if (is.null(inferred_rank) || !is.finite(inferred_rank) || inferred_rank < min_rank) {
      stop(sprintf(
        "IR verify [%s]: %s must have array rank >= %d in strict mode (got '%s')",
        op_label, field_label, as.integer(min_rank), inferred
      ))
    }
    invisible(NULL)
  }
  .is_i32_integral_literal <- function(expr_node) {
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
  .require_i32_or_integral_literal_strict <- function(expr_node, field_label, op_label) {
    strict_type_mode <- .strict_type_mode()
    if (!strict_type_mode) {
      return(invisible(NULL))
    }
    if (.is_i32_integral_literal(expr_node)) {
      return(invisible(NULL))
    }
    .require_type_strict(expr_node, field_label, op_label, "i32")
  }
  .require_result_type_strict <- function(expr_node, op_label, allowed_types = NULL) {
    strict_type_mode <- .strict_type_mode()
    if (!strict_type_mode) {
      return(invisible(NULL))
    }
    inferred <- tryCatch(
      .mojor_ir_infer_type(expr_node, ctx$type_env),
      error = function(e) "unknown"
    )
    if (is.null(inferred) || !is.character(inferred) || length(inferred) != 1 || !nzchar(inferred)) {
      inferred <- "unknown"
    }
    if (is.null(allowed_types)) {
      if (identical(inferred, "unknown")) {
        stop(sprintf(
          "IR verify [%s]: result type must be statically known in strict mode (got '%s')",
          op_label, inferred
        ))
      }
      return(invisible(NULL))
    }
    if (!(inferred %in% allowed_types)) {
      stop(sprintf(
        "IR verify [%s]: result type must be %s in strict mode (got '%s')",
        op_label,
        paste(sprintf("'%s'", allowed_types), collapse = " or "),
        inferred
      ))
    }
    invisible(NULL)
  }
  .require_result_type_strict_if_known <- function(expr_node, op_label, allowed_types = NULL) {
    strict_type_mode <- .strict_type_mode()
    if (!strict_type_mode) {
      return(invisible(NULL))
    }
    inferred <- tryCatch(
      .mojor_ir_infer_type(expr_node, ctx$type_env),
      error = function(e) "unknown"
    )
    if (is.null(inferred) || !is.character(inferred) || length(inferred) != 1 || !nzchar(inferred)) {
      inferred <- "unknown"
    }
    if (identical(inferred, "unknown")) {
      return(invisible(NULL))
    }
    .require_result_type_strict(expr_node, op_label, allowed_types)
  }

  switch(k,
 # --- Expression nodes ---
    const = {
      .req("value")
    },
    var = {
      .req("name")
      if (!is.character(node$name) || length(node$name) != 1) {
        stop("IR verify [var]: name must be a single character string")
      }
      if (isTRUE(ctx$check_scope) && !is.null(ctx$defined_vars) &&
        !node$name %in% ctx$defined_vars) {
        stop(sprintf(
          "IR verify [var]: '%s' not in scope (defined: %s)",
          node$name,
          paste(ctx$defined_vars, collapse = ", ")
        ))
      }
    },
    unop = {
      .req("op")
      if (!node$op %in% c("-", "!")) {
        stop(sprintf("IR verify [unop]: unknown op '%s'", node$op))
      }
      .req_node("expr")
    },
    binop = {
      .req("op")
      if (!node$op %in% allowed_binops) {
        stop(sprintf("IR verify [binop]: unknown op '%s'", node$op))
      }
      .req_node("lhs")
      .req_node("rhs")
    },
    cast = {
      .req("to", "expr")
      if (!node$to %in% allowed_types) {
        stop(sprintf("IR verify [cast]: unknown type '%s'", node$to))
      }
      .req_node("expr")
 # Type consistency: cast should change type (src <U+2260> dst)
      if (!is.null(ctx$type_env) && length(ctx$type_env) > 0) {
        src_type <- tryCatch(
          .mojor_ir_infer_type(node$expr, ctx$type_env),
          error = function(e) NULL
        )
        if (!is.null(src_type) && !is.na(src_type) && src_type != "unknown") {
 # Strip array suffix for comparison
          src_base <- sub("\\[.*\\]$", "", src_type)
          dst_base <- sub("\\[.*\\]$", "", node$to)
          if (src_base == dst_base) {
            warning(sprintf(
              "IR verify [cast]: redundant cast from '%s' to '%s' (same base type)",
              src_type, node$to
            ))
          }
        }
      }
    },
    call = {
      .req("fn", "args")
      if (!is.character(node$fn) || length(node$fn) != 1) {
        stop("IR verify [call]: fn must be a single character string")
      }
      if (!is.list(node$args)) {
        stop("IR verify [call]: args must be a list")
      }
      for (a in node$args) .mojor_ir_verify(a, ctx)
      if (.mojor_ir_is_rng_call_fn(node$fn)) {
        info <- .mojor_ir_rng_metadata()[[node$fn]]
        min_params <- as.integer(info$min_params[[1]])
        max_params <- as.integer(info$max_params[[1]])
        if (max_params < min_params) {
          stop(sprintf("IR verify [call]: invalid RNG param bounds metadata for '%s'", node$fn))
        }
 # Scalar RNG call form keeps `n` as first argument.
        min_arity <- min_params + 1L
        max_arity <- max_params + 1L
        if (length(node$args) < min_arity || length(node$args) > max_arity) {
          stop(sprintf(
            "IR verify [call]: RNG '%s' expects %d..%d arguments (including n), got %d",
            node$fn, min_arity, max_arity, length(node$args)
          ))
        }
      }
 # Known function arity validation
 # Note: RNG functions (rnorm, runif, rgamma) removed - they take multiple args
      known_unary <- c(
        "sin", "cos", "tan", "asin", "acos", "atan",
        "sinh", "cosh", "tanh", "asinh", "acosh", "atanh",
        "log", "log10", "log1p", "log2", "exp", "expm1",
        "sqrt", "abs", "abs2", "floor", "ceiling", "trunc", "round",
        "sign", "cbrt", "lgamma", "erf", "gamma",
        "as.logical",
        "length", "is.na", "is.nan", "is.finite", "is.infinite"
      )
      known_binary <- c("atan2", "hypot", "pow")
      if (node$fn %in% known_unary && length(node$args) != 1) {
        stop(sprintf(
          "IR verify [call]: '%s' expects 1 argument, got %d",
          node$fn, length(node$args)
        ))
      }
      if (node$fn %in% c("min", "max", "pmin", "pmax") && !(length(node$args) %in% c(1, 2))) {
        stop(sprintf(
          "IR verify [call]: '%s' expects 1 or 2 arguments, got %d",
          node$fn, length(node$args)
        ))
      }
      if (node$fn %in% known_binary && length(node$args) != 2) {
        stop(sprintf(
          "IR verify [call]: '%s' expects 2 arguments, got %d",
          node$fn, length(node$args)
        ))
      }
    },
    index = {
      .req("base", "indices")
 # base is a var node (wrapped variable reference) or constructor (c, rep, rep_len, seq), not a plain string
 # PR-B5 Step 4: Added cumulative operations as valid index bases
      valid_base_kinds <- c(
        "var", "index", "c", "rep", "rep_len", "seq", "transpose", "cbind", "rbind", "diag",
        "cumsum", "cumprod", "cummax", "cummin", "sample_int", "sample", "dim"
      )
      if (!is.list(node$base) || !(node$base$kind %in% valid_base_kinds)) {
        stop("IR verify [index]: base must be a var/index or constructor node")
      }
      .mojor_ir_verify(node$base, ctx)
      if (!is.null(node$index_base) &&
        !node$index_base %in% c("one_based", "zero_based")) {
        stop(sprintf(
          "IR verify [index]: index_base must be one_based/zero_based, got '%s'",
          node$index_base
        ))
      }
      if (!is.list(node$indices)) {
        stop("IR verify [index]: indices must be a list")
      }
      if (length(node$indices) == 0) {
        stop("IR verify [index]: indices list cannot be empty")
      }
      is_selector_ctor <- function(idx_node) {
        if (is.null(idx_node) || !is.list(idx_node) || is.null(idx_node$kind)) {
          return(FALSE)
        }
        if (idx_node$kind %in% c("c", "seq", "rep", "rep_len", "slice_index", "vec_index")) {
          return(TRUE)
        }
        FALSE
      }
      for (idx in node$indices) {
        .verify_selector_canonical_node(idx, "index")
        .mojor_ir_verify(idx, ctx)
 # Index expressions should be scalar (not array-valued)
 # Check if index is a known array type in type_env
        if (!is.null(ctx$type_env) && length(ctx$type_env) > 0) {
          idx_type <- tryCatch(
            .mojor_ir_infer_type(idx, ctx$type_env),
            error = function(e) NULL
          )
          if (!is.null(idx_type) && grepl("\\[\\]$", idx_type)) {
 # Vector selectors are valid index expressions for array indexing.
            if (!idx_type %in% c("i32[]", "lgl[]", "bool[]", "chr[]")) {
              if (isTRUE(is_selector_ctor(idx))) {
                next
              }
              stop(sprintf(
                "IR verify [index]: index expression has array type '%s' (expected scalar)",
                idx_type
              ))
            }
          }
        }
      }
    },
    ifelse = {
      .req_node("cond")
      .req_node("yes")
      .req_node("no")
    },
    vexpr = {
      .req("len", "body")
      .mojor_ir_verify(node$len, ctx)
      .mojor_ir_verify(node$body, ctx)
      if (.strict_type_mode()) {
        .require_i32_or_integral_literal_strict(node$len, "len", "vexpr")
        .require_type_strict(node$body, "body", "vexpr", allowed_vexpr_body_types)
        .require_result_type_strict_if_known(node, "vexpr", allowed_vexpr_result_types)
      }
    },
 # --- Indexing helper nodes ---
    scalar_index = {
      .req_node("expr")
      .verify_selector_canonical_node(node, "scalar_index")
    },
    vec_index = {
      if (is.null(node$expr)) {
        stop("IR verify [vec_index]: expected expr")
      }
      .verify_selector_canonical_node(node, "vec_index")
      .mojor_ir_verify(node$expr, ctx)
    },
    slice_index = {
      .req_node("start")
      .req_node("end")
    },
    missing_index = {
 # No required fields beyond kind
    },
    subscript = {
      .req("var", "indices")
      if (!is.character(node$var) || length(node$var) != 1) {
        stop("IR verify [subscript]: var must be a single character string")
      }
      if (!is.list(node$indices) || length(node$indices) == 0) {
        stop("IR verify [subscript]: indices must be a non-empty list")
      }
      for (idx in node$indices) {
        .verify_selector_canonical_node(idx, "subscript")
        .mojor_ir_verify(idx, ctx)
      }
    },
 # --- Constructor nodes ---
    rep = {
      .req_node("x")
      if (is.null(node$times) && is.null(node$each) && is.null(node$length_out)) {
        stop("IR verify [rep]: at least one of times/each/length_out required")
      }
      if (!is.null(node$times)) .mojor_ir_verify(node$times, ctx)
      if (!is.null(node$each)) .mojor_ir_verify(node$each, ctx)
      if (!is.null(node$length_out)) .mojor_ir_verify(node$length_out, ctx)
    },
    rep_len = {
      .req_node("x")
      .req_node("length_out")
    },
    c = {
      .req("parts")
      if (!is.list(node$parts) || length(node$parts) == 0) {
        stop("IR verify [c]: parts must be a non-empty list")
      }
      for (p in node$parts) .mojor_ir_verify(p, ctx)
    },
    seq = {
      .req("from", "length_out")
      .mojor_ir_verify(node$from, ctx)
      .mojor_ir_verify(node$length_out, ctx)
      if (!is.null(node$to)) .mojor_ir_verify(node$to, ctx)
    },
    transpose = {
      .req("x")
      .mojor_ir_verify(node$x, ctx)
    },
    cbind = {
      .req("args")
      if (!is.list(node$args) || length(node$args) == 0) {
        stop("IR verify [cbind]: args must be a non-empty list")
      }
      for (a in node$args) .mojor_ir_verify(a, ctx)
    },
    rbind = {
      .req("args")
      if (!is.list(node$args) || length(node$args) == 0) {
        stop("IR verify [rbind]: args must be a non-empty list")
      }
      for (a in node$args) .mojor_ir_verify(a, ctx)
    },
    diag = {
 # diag has either x (vector/matrix) or n (scalar for identity)
      if (is.null(node$x) && is.null(node$n)) {
        stop("IR verify [diag]: must have either x or n")
      }
      if (!is.null(node$x)) .mojor_ir_verify(node$x, ctx)
      if (!is.null(node$n)) .mojor_ir_verify(node$n, ctx)
    },
 # PR-B5 Step 4: Cumulative operation nodes
    cumsum = {
      .req_node("x")
    },
    cumprod = {
      .req_node("x")
    },
    cummax = {
      .req_node("x")
    },
    cummin = {
      .req_node("x")
    },
 # PR-B5 Step 3: Statistical function nodes
    mean = {
      .req_node("x")
      if (!is.null(node$na_rm)) {
        if (!is.logical(node$na_rm) || length(node$na_rm) != 1 || is.na(node$na_rm)) {
          stop("IR verify [mean]: na_rm must be a logical value")
        }
      }
    },
    var_stat = {
      .req_node("x")
      if (!is.null(node$na_rm)) {
        if (!is.logical(node$na_rm) || length(node$na_rm) != 1 || is.na(node$na_rm)) {
          stop("IR verify [var_stat]: na_rm must be a logical value")
        }
      }
    },
    sd = {
      .req_node("x")
      if (!is.null(node$na_rm)) {
        if (!is.logical(node$na_rm) || length(node$na_rm) != 1 || is.na(node$na_rm)) {
          stop("IR verify [sd]: na_rm must be a logical value")
        }
      }
    },
    cov = {
      .req("x")
      .mojor_ir_verify(node$x, ctx)
      if (!is.null(node$y)) {
        .mojor_ir_verify(node$y, ctx)
      }
      if (.strict_type_mode()) {
        .require_direct_var_strict(node$x, "x", "cov")
        .require_type_strict(node$x, "x", "cov", allowed_cov_cor_types)
        x_type <- tryCatch(.mojor_ir_infer_type(node$x, ctx$type_env), error = function(e) "unknown")
        y_type <- x_type
        if (!is.null(node$y)) {
          .require_direct_var_strict(node$y, "y", "cov")
          .require_type_strict(node$y, "y", "cov", allowed_cov_cor_types)
          y_type <- tryCatch(.mojor_ir_infer_type(node$y, ctx$type_env), error = function(e) "unknown")
        }
        x_label <- if (is.character(x_type) && length(x_type) == 1 && nzchar(x_type)) x_type else "unknown"
        y_label <- if (is.character(y_type) && length(y_type) == 1 && nzchar(y_type)) y_type else "unknown"
        x_ndim <- if (!identical(x_label, "unknown")) .mojor_type_ndim(x_label) else NA_integer_
        y_ndim <- if (!identical(y_label, "unknown")) .mojor_type_ndim(y_label) else NA_integer_
        if (!is.na(x_ndim) && !is.na(y_ndim) && !identical(x_ndim, y_ndim)) {
          stop(
            "IR verify [cov]: x and y must have matching rank in strict mode (got '",
            x_label, "' vs '", y_label, "')"
          )
        }
        x_elem <- sub("\\[.*$", "", x_label)
        y_elem <- sub("\\[.*$", "", y_label)
        if (!identical(x_elem, y_elem)) {
          stop(
            "IR verify [cov]: x and y must have matching element type in strict mode (got '",
            x_label, "' vs '", y_label, "')"
          )
        }
        .require_result_type_strict_if_known(node, "cov", c("f64", "f64[2d]", "f64[,]"))
      }
    },
    cor = {
      .req("x")
      .mojor_ir_verify(node$x, ctx)
      if (!is.null(node$y)) {
        .mojor_ir_verify(node$y, ctx)
      }
      if (.strict_type_mode()) {
        .require_direct_var_strict(node$x, "x", "cor")
        .require_type_strict(node$x, "x", "cor", allowed_cov_cor_types)
        x_type <- tryCatch(.mojor_ir_infer_type(node$x, ctx$type_env), error = function(e) "unknown")
        y_type <- x_type
        if (!is.null(node$y)) {
          .require_direct_var_strict(node$y, "y", "cor")
          .require_type_strict(node$y, "y", "cor", allowed_cov_cor_types)
          y_type <- tryCatch(.mojor_ir_infer_type(node$y, ctx$type_env), error = function(e) "unknown")
        }
        x_label <- if (is.character(x_type) && length(x_type) == 1 && nzchar(x_type)) x_type else "unknown"
        y_label <- if (is.character(y_type) && length(y_type) == 1 && nzchar(y_type)) y_type else "unknown"
        x_ndim <- if (!identical(x_label, "unknown")) .mojor_type_ndim(x_label) else NA_integer_
        y_ndim <- if (!identical(y_label, "unknown")) .mojor_type_ndim(y_label) else NA_integer_
        if (!is.na(x_ndim) && !is.na(y_ndim) && !identical(x_ndim, y_ndim)) {
          stop(
            "IR verify [cor]: x and y must have matching rank in strict mode (got '",
            x_label, "' vs '", y_label, "')"
          )
        }
        x_elem <- sub("\\[.*$", "", x_label)
        y_elem <- sub("\\[.*$", "", y_label)
        if (!identical(x_elem, y_elem)) {
          stop(
            "IR verify [cor]: x and y must have matching element type in strict mode (got '",
            x_label, "' vs '", y_label, "')"
          )
        }
        .require_result_type_strict_if_known(node, "cor", c("f64", "f64[2d]", "f64[,]"))
      }
    },
    dim = {
      .req("x")
      .mojor_ir_verify(node$x, ctx)
      if (.strict_type_mode()) {
        .require_direct_var_strict(node$x, "x", "dim")
        .require_nd_array_strict(node$x, "x", "dim", min_rank = 2L)
        .require_result_type_strict_if_known(node, "dim", "i32[]")
      }
    },
    type_predicate = {
      .req("predicate", "x")
      if (!is.character(node$predicate) || length(node$predicate) != 1 || !nzchar(node$predicate)) {
        stop("IR verify [type_predicate]: predicate must be a non-empty character string")
      }
      if (!(node$predicate %in% allowed_type_predicates)) {
        stop(sprintf(
          "IR verify [type_predicate]: unsupported predicate '%s'. Supported: %s",
          node$predicate, paste(allowed_type_predicates, collapse = ", ")
        ))
      }
      .mojor_ir_verify(node$x, ctx)
      if (.strict_type_mode()) {
        .require_direct_var_strict(node$x, "x", "type_predicate")
        .require_result_type_strict_if_known(node, "type_predicate", c("bool", "lgl"))
      }
    },
    type_query = {
      .req("query", "x")
      if (!is.character(node$query) || length(node$query) != 1 || !nzchar(node$query)) {
        stop("IR verify [type_query]: query must be a non-empty character string")
      }
      if (!(node$query %in% allowed_type_queries)) {
        stop(sprintf(
          "IR verify [type_query]: unsupported query '%s'. Supported: %s",
          node$query, paste(allowed_type_queries, collapse = ", ")
        ))
      }
      .mojor_ir_verify(node$x, ctx)
      if (.strict_type_mode()) {
        .require_direct_var_strict(node$x, "x", "type_query")
        .require_result_type_strict_if_known(node, "type_query", "chr[]")
      }
    },
 # --- Statement nodes ---
    block = {
      .req("stmts")
      if (!is.list(node$stmts)) {
        stop("IR verify [block]: stmts must be a list")
      }
      if (isTRUE(ctx$check_scope)) {
 # Sequential: each assign defines its LHS var for later stmts
        cur_ctx <- ctx
        for (i in seq_along(node$stmts)) {
          s <- node$stmts[[i]]
 # Check for unreachable code after return
          if (isTRUE(cur_ctx$has_returned)) {
            stop(sprintf(
              "IR verify [block]: unreachable statement at position %d after return",
              i
            ))
          }
          .mojor_ir_verify(s, cur_ctx)
 # Track variable definitions
          if (is.list(s) && identical(s$kind, "assign") &&
            is.list(s$lhs) && identical(s$lhs$kind, "var")) {
            cur_ctx$defined_vars <- c(cur_ctx$defined_vars, s$lhs$name)
          }
 # Track return statements
          if (is.list(s) && identical(s$kind, "return")) {
            cur_ctx$has_returned <- TRUE
          }
        }
      } else {
        for (s in node$stmts) .mojor_ir_verify(s, ctx)
      }
    },
    assign = {
      .req("lhs", "rhs")
      if (!is.list(node$lhs) ||
        !node$lhs$kind %in% c("var", "index", "subscript")) {
        stop(sprintf(
          "IR verify [assign]: lhs must be var/index/subscript, got '%s'",
          if (is.list(node$lhs)) node$lhs$kind else "non-node"
        ))
      }
 # Plain var LHS is being defined <U+2014> skip scope check on it.
 # index/subscript LHS reads the base array, so keep scope check.
      lhs_ctx <- if (isTRUE(ctx$check_scope) &&
        identical(node$lhs$kind, "var")) {
        modifyList(ctx, list(check_scope = FALSE))
      } else {
        ctx
      }
      .mojor_ir_verify(node$lhs, lhs_ctx)
      .req("rhs")
      rhs_ctx <- modifyList(ctx, list(in_assign_rhs = TRUE))
      .mojor_ir_verify(node$rhs, rhs_ctx)
    },
    `if` = {
      .req_node("cond")
      .req("then")
      .mojor_ir_verify(node$then, ctx)
      if (!is.null(node$else_block)) .mojor_ir_verify(node$else_block, ctx)
    },
    loop = {
      .req("var", "range", "body")
      if (!is.character(node$var) || length(node$var) != 1) {
        stop("IR verify [loop]: var must be a single character string")
      }
      if (node$var %in% ctx$loop_vars) {
        stop(sprintf("IR verify [loop]: var '%s' shadows outer loop variable", node$var))
      }
      .mojor_ir_verify(node$range, ctx)
      inner_ctx <- modifyList(ctx, list(
        loop_vars = c(ctx$loop_vars, node$var),
        in_loop = TRUE, # Enable break/next validation
        defined_vars = if (!is.null(ctx$defined_vars)) {
          c(ctx$defined_vars, node$var)
        } else {
          NULL
        },
        has_returned = FALSE # Reset for loop body
      ))
      .mojor_ir_verify(node$body, inner_ctx)
 # Validate optional reduce annotation
      if (!is.null(node$reduce)) {
        red <- node$reduce
        allowed_reduce_ops <- c("sum", "product", "min", "max")
        if (is.null(red$kind) || !red$kind %in% allowed_reduce_ops) {
          stop(sprintf(
            "IR verify [loop$reduce]: kind must be one of %s",
            paste(allowed_reduce_ops, collapse = "/")
          ))
        }
        if (is.null(red$acc) || !is.character(red$acc)) {
          stop("IR verify [loop$reduce]: acc must be a character variable name")
        }
        if (!is.null(red$rhs)) .mojor_ir_verify(red$rhs, inner_ctx)
      }
    },
    `while` = {
      .req_node("cond")
      .req("body")
 # While body allows break/next
      inner_ctx <- modifyList(ctx, list(
        in_loop = TRUE,
        has_returned = FALSE
      ))
      .mojor_ir_verify(node$body, inner_ctx)
    },
    `repeat` = {
      .req("body")
 # Repeat body allows break/next
      inner_ctx <- modifyList(ctx, list(
        in_loop = TRUE,
        has_returned = FALSE
      ))
      .mojor_ir_verify(node$body, inner_ctx)
    },
    `break` = {
 # No required fields
 # Control flow: break only valid inside loops
      if (!isTRUE(ctx$in_loop)) {
        stop("IR verify [break]: break statement outside loop")
      }
    },
    `next` = {
 # No required fields
 # Control flow: next only valid inside loops
      if (!isTRUE(ctx$in_loop)) {
        stop("IR verify [next]: next statement outside loop")
      }
    },
    `return` = {
      if (!is.null(node$value)) .mojor_ir_verify(node$value, ctx)
    },
    scalar_reduce = {
      allowed_reduce_ops <- c("sum", "product", "min", "max", "which.min", "which.max")
      if (is.null(node$op) || !node$op %in% allowed_reduce_ops) {
        stop(sprintf(
          "IR verify [scalar_reduce]: op must be one of %s",
          paste(allowed_reduce_ops, collapse = "/")
        ))
      }
      if (is.null(node$acc) || !is.character(node$acc) || length(node$acc) != 1) {
        stop("IR verify [scalar_reduce]: acc must be a single character string")
      }
      if (is.null(node$arg) || !is.character(node$arg) || length(node$arg) != 1) {
        stop("IR verify [scalar_reduce]: arg must be a single character string")
      }
 # Step 23: Verify first-class semantics fields
      if (!is.null(node$init)) {
 # init must be an IR expression node if present
        .mojor_ir_verify(node$init, ctx)
      }
      if (!is.null(node$axis)) {
        if (!is.numeric(node$axis) || length(node$axis) != 1) {
          stop("IR verify [scalar_reduce]: axis must be a single integer")
        }
      }
      if (!is.null(node$associative)) {
        if (!is.logical(node$associative) || length(node$associative) != 1) {
          stop("IR verify [scalar_reduce]: associative must be a single logical value")
        }
      }
      if (!is.null(node$commutative)) {
        if (!is.logical(node$commutative) || length(node$commutative) != 1) {
          stop("IR verify [scalar_reduce]: commutative must be a single logical value")
        }
      }
    },
    scheduled_reduce = {
      if (is.null(node$mode) || !node$mode %in% c("tree", "simd")) {
        stop("IR verify [scheduled_reduce]: mode must be one of tree/simd")
      }
      if (is.null(node$op) || !node$op %in% c("sum", "product", "min", "max", "which.min", "which.max")) {
        stop("IR verify [scheduled_reduce]: op must be one of sum/product/min/max/which.min/which.max")
      }
      if (is.null(node$acc) || !is.character(node$acc) || length(node$acc) != 1) {
        stop("IR verify [scheduled_reduce]: acc must be a single character string")
      }
      if (is.null(node$arg) || !is.character(node$arg) || length(node$arg) != 1) {
        stop("IR verify [scheduled_reduce]: arg must be a single character string")
      }
      if (!is.null(node$n_var) && (!is.character(node$n_var) || length(node$n_var) != 1)) {
        stop("IR verify [scheduled_reduce]: n_var must be a single character string")
      }
      if (!is.null(node$empty_val) && (!is.character(node$empty_val) || length(node$empty_val) != 1)) {
        stop("IR verify [scheduled_reduce]: empty_val must be a single character string")
      }
      if (!is.null(node$value_cast) && (!is.character(node$value_cast) || length(node$value_cast) != 1)) {
        stop("IR verify [scheduled_reduce]: value_cast must be a single character string")
      }
      if (!is.null(node$dtype)) {
        ok_dtype <- if (node$mode == "tree") {
          node$dtype %in% c("Float64", "Float32", "Int32")
        } else {
          node$dtype %in% c("DType.float64", "DType.float32", "DType.int32")
        }
        if (!ok_dtype) {
          stop("IR verify [scheduled_reduce]: dtype is invalid for mode")
        }
      }
    },
 # Vectorized RNG node
    rng_vec = {
      .req("dist", "n")
      if (!is.character(node$dist) || length(node$dist) != 1) {
        stop("IR verify [rng_vec]: dist must be a single character string")
      }
 # Whitelist supported distributions
      meta <- .mojor_ir_rng_metadata()
      allowed_dists <- names(meta)
      if (!node$dist %in% allowed_dists) {
        stop(sprintf(
          "IR verify [rng_vec]: unsupported distribution '%s'. Supported: %s",
          node$dist, paste(allowed_dists, collapse = ", ")
        ))
      }
 # n must be a scalar expression
      .mojor_ir_verify(node$n, ctx)
 # params is optional
      n_params <- 0L
      if (!is.null(node$params)) {
        if (!is.list(node$params)) {
          stop("IR verify [rng_vec]: params must be a list")
        }
        n_params <- length(node$params)
        for (p in node$params) .mojor_ir_verify(p, ctx)
      }
      info <- meta[[node$dist]]
      min_params <- as.integer(info$min_params[[1]])
      max_params <- as.integer(info$max_params[[1]])
      if (max_params < min_params) {
        stop(sprintf("IR verify [rng_vec]: invalid param bounds metadata for '%s'", node$dist))
      }
      if (n_params < min_params || n_params > max_params) {
        stop(sprintf(
          "IR verify [rng_vec]: distribution '%s' expects %d..%d params, got %d",
          node$dist, min_params, max_params, n_params
        ))
      }
    },
 # Allocation node
    alloc = {
      .req("len", "dtype")
 # len must be a scalar expression
      .mojor_ir_verify(node$len, ctx)
 # dtype must be a valid type string
      if (!is.character(node$dtype) || length(node$dtype) != 1) {
        stop("IR verify [alloc]: dtype must be a single character string")
      }
      allowed_dtypes <- c("f64", "f32", "i32", "bool")
      if (!node$dtype %in% allowed_dtypes) {
        stop(sprintf(
          "IR verify [alloc]: unsupported dtype '%s'. Supported: %s",
          node$dtype, paste(allowed_dtypes, collapse = ", ")
        ))
      }
    },
 # apply() - matrix apply with row/column margins
    apply = {
      .req("x", "margin", "fun")
      .mojor_ir_verify(node$x, ctx)
      if (!is.numeric(node$margin) || length(node$margin) != 1) {
        stop("IR verify [apply]: margin must be a single numeric value")
      }
      if (node$margin != 1 && node$margin != 2) {
        stop(sprintf("IR verify [apply]: margin must be 1 or 2, got %s", node$margin))
      }
      if (!is.character(node$fun) || length(node$fun) != 1) {
        stop("IR verify [apply]: fun must be a single character string")
      }
 # MVP: only support sum/mean/min/max
      allowed_funs <- c("sum", "mean", "min", "max")
      if (!node$fun %in% allowed_funs) {
        stop(sprintf(
          "IR verify [apply]: unsupported fun '%s'. Supported: %s",
          node$fun, paste(allowed_funs, collapse = ", ")
        ))
      }
      .verify_bool_flag_arg(node$na_rm, "na_rm", "apply")
    },
 # / sample_int() - sampling from integers
    sample_int = {
      .req("n", "size")
      .mojor_ir_verify(node$n, ctx)
      .mojor_ir_verify(node$size, ctx)
      .require_i32_or_integral_literal_strict(node$n, "n", "sample_int")
      .require_i32_or_integral_literal_strict(node$size, "size", "sample_int")
      strict_type_mode <- .strict_type_mode()
      .verify_replace_arg(node$replace, "sample_int", strict_type_mode)
      .verify_prob_arg(node$prob, "sample_int", strict_type_mode)
      .verify_weighted_replace_strict(node$prob, node$replace, "sample_int", strict_type_mode)
      .require_result_type_strict_if_known(node, "sample_int", "i32[]")
    },
 # / sample() - sampling from vector
    sample = {
      .req("x", "size")
      .mojor_ir_verify(node$x, ctx)
      .mojor_ir_verify(node$size, ctx)
      strict_type_mode <- .strict_type_mode()
      if (strict_type_mode && node$x$kind != "var") {
        stop("IR verify [sample]: x must be a direct vector variable/argument in strict mode")
      }
      .require_type_strict(node$x, "x", "sample", c("f64[]", "i32[]", "lgl[]", "bool[]"))
      .require_i32_or_integral_literal_strict(node$size, "size", "sample")
      .verify_replace_arg(node$replace, "sample", strict_type_mode)
      .verify_prob_arg(node$prob, "sample", strict_type_mode)
      .verify_weighted_replace_strict(node$prob, node$replace, "sample", strict_type_mode)
      .require_result_type_strict_if_known(node, "sample", c("f64[]", "i32[]", "lgl[]", "bool[]"))
    },
 # Higher-Order Functions
    vapply = {
      .req("x", "fun", "fun_value_type")
      .mojor_ir_verify(node$x, ctx)
      if (node$x$kind != "var") {
        stop("IR verify [vapply]: x must be a direct vector variable in the compiled subset")
      }
      .require_type_strict(node$x, "x", "vapply", allowed_hof_vector_types)
      .verify_hof_fun(node$fun, "vapply", expected_arity = 1L, allow_named = TRUE)
 # fun is an R function (not an IR node) - just verify it's a function name
      if (!is.character(node$fun_value_type) || length(node$fun_value_type) != 1) {
        stop("IR verify [vapply]: fun_value_type must be a single character string")
      }
      if (!(node$fun_value_type %in% c("f64", "i32", "lgl", "bool", "numeric", "double", "integer", "logical"))) {
        stop("IR verify [vapply]: fun_value_type must be numeric(1), integer(1), or logical(1)")
      }
      .require_result_type_strict(node, "vapply", c("f64[]", "i32[]", "lgl[]", "bool[]"))
    },
    sapply = {
      .req("x", "fun")
      .mojor_ir_verify(node$x, ctx)
      if (node$x$kind != "var") {
        stop("IR verify [sapply]: x must be a direct vector variable in the compiled subset")
      }
      .require_type_strict(node$x, "x", "sapply", allowed_hof_vector_types)
      .verify_hof_fun(node$fun, "sapply", expected_arity = 1L, allow_named = TRUE)
      .require_result_type_strict(node, "sapply", c("f64[]", "i32[]", "lgl[]", "bool[]"))
    },
    lapply = {
      .req("x", "fun")
      .mojor_ir_verify(node$x, ctx)
      if (node$x$kind != "var") {
        stop("IR verify [lapply]: x must be a direct vector variable in the compiled subset")
      }
      .require_type_strict(node$x, "x", "lapply", allowed_hof_vector_types)
      .verify_hof_fun(node$fun, "lapply", expected_arity = 1L, allow_named = TRUE)
      .require_result_type_strict(node, "lapply", c("f64[]", "i32[]", "lgl[]", "bool[]"))
    },
    mapply = {
      .req("fun", "args")
      if (!is.list(node$args)) {
        stop("IR verify [mapply]: args must be a list")
      }
      if (length(node$args) < 2) {
        stop("IR verify [mapply]: requires at least two vector arguments in the compiled subset")
      }
      .verify_hof_fun(node$fun, "mapply", expected_arity = length(node$args), allow_named = TRUE)
      for (a in node$args) .mojor_ir_verify(a, ctx)
      if (!all(vapply(node$args, function(a) identical(a$kind, "var"), logical(1)))) {
        stop("IR verify [mapply]: arguments must be direct vector variables in the compiled subset")
      }
      for (i in seq_along(node$args)) {
        .require_type_strict(node$args[[i]], paste0("args[[", i, "]]"), "mapply", allowed_hof_vector_types)
      }
      .require_result_type_strict(node, "mapply", c("f64[]", "i32[]", "lgl[]", "bool[]"))
    },
 # Set/Match Primitives
    unique = {
      .verify_var_field("x", "unique")
    },
    duplicated = {
      .verify_var_field("x", "duplicated")
    },
    any_duplicated = {
      .verify_var_field("x", "any_duplicated")
    },
    match = {
      .verify_var_field("x", "match")
      .verify_var_field("table", "match")
    },
    `in` = {
      .verify_var_field("x", "in")
      .verify_var_field("table", "in")
    },
 # Quantiles & Robust Stats
    median = {
      .verify_var_field("x", "median")
    },
    quantile = {
      .verify_var_field("x", "quantile")
      .verify_var_field("probs", "quantile")
      if (!is.null(node$na_rm)) {
        .mojor_ir_verify(node$na_rm, ctx)
        if (node$na_rm$kind == "const") {
          value <- node$na_rm$value
          if (!.is_logical_scalar_value(value)) {
            stop("IR verify [quantile]: na_rm must be a logical value")
          }
        }
      }
      if (!is.null(node$type)) {
        .mojor_ir_verify(node$type, ctx)
        if (node$type$kind == "const") {
          value <- suppressWarnings(as.numeric(node$type$value))
          if (is.na(value) || value != floor(value) || value < 1 || value > 8) {
            stop("IR verify [quantile]: type must be an integer in [1, 8]")
          }
        }
      }
    },
    iqr = {
      .verify_var_field("x", "iqr")
    },
    mad = {
      .verify_var_field("x", "mad")
    },
 # String Basics
    nchar = {
      .req("x")
      .mojor_ir_verify(node$x, ctx)
      .require_chr_array_strict(node$x, "x", "nchar")
      .require_result_type_strict(node, "nchar", "i32[]")
    },
    nzchar = {
      .req("x")
      .mojor_ir_verify(node$x, ctx)
      .require_chr_array_strict(node$x, "x", "nzchar")
      .require_result_type_strict(node, "nzchar", c("lgl[]", "bool[]"))
    },
    substr = {
      .req("x", "start", "stop")
      .mojor_ir_verify(node$x, ctx)
      .mojor_ir_verify(node$start, ctx)
      .mojor_ir_verify(node$stop, ctx)
      .require_chr_array_strict(node$x, "x", "substr")
      .require_type_strict(node$start, "start", "substr", "i32")
      .require_type_strict(node$stop, "stop", "substr", "i32")
      .require_result_type_strict(node, "substr", "chr[]")
    },
    paste = {
      .req("args")
      if (!is.list(node$args)) {
        stop("IR verify [paste]: args must be a list")
      }
      for (i in seq_along(node$args)) {
        .mojor_ir_verify(node$args[[i]], ctx)
        .require_chr_array_strict(node$args[[i]], paste0("args[[", i, "]]"), "paste")
      }
      if (!is.character(node$sep) || length(node$sep) != 1) {
        stop("IR verify [paste]: sep must be a single character string")
      }
      .require_result_type_strict(node, "paste", "chr[]")
    },
 # Data structures
    df_col_read = {
      .req("df", "col")
      .mojor_ir_verify(node$df, ctx)
      .mojor_ir_verify(node$col, ctx)
      .require_type_strict(node$df, "df", "df_col_read", "df")
      .require_type_strict(node$col, "col", "df_col_read", "chr[]")
      .require_result_type_strict(node, "df_col_read", allowed_tier9_vector_types)
    },
    df_col_exists_guard = {
      .req("df", "col")
      .mojor_ir_verify(node$df, ctx)
      .mojor_ir_verify(node$col, ctx)
      .require_type_strict(node$df, "df", "df_col_exists_guard", "df")
      .require_type_strict(node$col, "col", "df_col_exists_guard", "chr[]")
      .require_result_type_strict(node, "df_col_exists_guard", c("bool", "lgl"))
    },
    df_make = {
      .req("cols")
      if (!is.list(node$cols)) {
        stop("IR verify [df_make]: cols must be a list")
      }
      if (length(node$cols) == 0) {
        stop("IR verify [df_make]: cols must contain at least one column")
      }
      for (i in seq_along(node$cols)) {
        .mojor_ir_verify(node$cols[[i]], ctx)
        .require_type_strict(node$cols[[i]], paste0("cols[[", i, "]]"), "df_make", allowed_tier9_vector_types)
      }
      .require_result_type_strict(node, "df_make", "df")
    },
    list_make = {
      .req("items")
      if (!is.list(node$items)) {
        stop("IR verify [list_make]: items must be a list")
      }
      if (length(node$items) == 0) {
        stop("IR verify [list_make]: items must contain at least one entry")
      }
      for (i in seq_along(node$items)) {
        .mojor_ir_verify(node$items[[i]], ctx)
        .require_type_strict(node$items[[i]], paste0("items[[", i, "]]"), "list_make", allowed_tier9_vector_types)
      }
      if (!is.null(node$names)) {
        if (!is.character(node$names) || length(node$names) != length(node$items) || any(!nzchar(node$names))) {
          stop("IR verify [list_make]: names must be a character vector with one non-empty name per item")
        }
      }
      .require_result_type_strict(node, "list_make", "list")
    },
 # Regex
    regex_grepl = {
      .req("pattern", "x")
      .mojor_ir_verify(node$pattern, ctx)
      .mojor_ir_verify(node$x, ctx)
      .require_chr_scalar_or_literal_strict(node$pattern, "pattern", "regex_grepl")
      if (.strict_type_mode() && !(is.list(node$x) && identical(node$x$kind, "var"))) {
        stop("IR verify [regex_grepl]: x must be a direct character vector argument in strict mode")
      }
      .require_chr_array_strict(node$x, "x", "regex_grepl")
      .verify_bool_flag_arg(node$ignore_case, "ignore_case", "regex_grepl")
      .verify_bool_flag_arg(node$perl, "perl", "regex_grepl")
      .verify_bool_flag_arg(node$fixed, "fixed", "regex_grepl")
      .require_result_type_strict(node, "regex_grepl", c("lgl[]", "bool[]"))
    },
    regex_grep = {
      .req("pattern", "x", "value")
      .mojor_ir_verify(node$pattern, ctx)
      .mojor_ir_verify(node$x, ctx)
      .require_chr_scalar_or_literal_strict(node$pattern, "pattern", "regex_grep")
      if (.strict_type_mode() && !(is.list(node$x) && identical(node$x$kind, "var"))) {
        stop("IR verify [regex_grep]: x must be a direct character vector argument in strict mode")
      }
      .require_chr_array_strict(node$x, "x", "regex_grep")
      value_bool <- FALSE
      if (is.list(node$value) && identical(node$value$kind, "const")) {
        value_bool <- .logical_scalar_to_bool(node$value$value, "value", "regex_grep")
      } else {
        value_bool <- .logical_scalar_to_bool(node$value, "value", "regex_grep")
      }
      .verify_bool_flag_arg(node$ignore_case, "ignore_case", "regex_grep")
      .verify_bool_flag_arg(node$perl, "perl", "regex_grep")
      .verify_bool_flag_arg(node$fixed, "fixed", "regex_grep")
      expected_type <- if (isTRUE(value_bool)) "chr[]" else "i32[]"
      .require_result_type_strict(node, "regex_grep", expected_type)
    },
    regex_sub = {
      .req("pattern", "replacement", "x")
      .mojor_ir_verify(node$pattern, ctx)
      .mojor_ir_verify(node$replacement, ctx)
      .mojor_ir_verify(node$x, ctx)
      .require_chr_scalar_or_literal_strict(node$pattern, "pattern", "regex_sub")
      .require_chr_scalar_or_literal_strict(node$replacement, "replacement", "regex_sub")
      if (.strict_type_mode() && !(is.list(node$x) && identical(node$x$kind, "var"))) {
        stop("IR verify [regex_sub]: x must be a direct character vector argument in strict mode")
      }
      .require_chr_array_strict(node$x, "x", "regex_sub")
      .verify_bool_flag_arg(node$ignore_case, "ignore_case", "regex_sub")
      .verify_bool_flag_arg(node$perl, "perl", "regex_sub")
      .verify_bool_flag_arg(node$fixed, "fixed", "regex_sub")
      .verify_bool_flag_arg(node$global, "global", "regex_sub")
      .require_result_type_strict(node, "regex_sub", "chr[]")
    },
 # Table utilities
    row_matrix = {
      .req("x")
      .mojor_ir_verify(node$x, ctx)
      if (.strict_type_mode() && !(is.list(node$x) && identical(node$x$kind, "var"))) {
        stop("IR verify [row_matrix]: x must be a direct matrix argument in strict mode")
      }
      .require_type_strict(node$x, "x", "row_matrix", allowed_tier9_matrix_types)
      .require_result_type_strict(node, "row_matrix", c("i32[2d]", "i32[,]"))
    },
    col_matrix = {
      .req("x")
      .mojor_ir_verify(node$x, ctx)
      if (.strict_type_mode() && !(is.list(node$x) && identical(node$x$kind, "var"))) {
        stop("IR verify [col_matrix]: x must be a direct matrix argument in strict mode")
      }
      .require_type_strict(node$x, "x", "col_matrix", allowed_tier9_matrix_types)
      .require_result_type_strict(node, "col_matrix", c("i32[2d]", "i32[,]"))
    },
    expand_grid = {
      .req("args")
      if (!is.list(node$args)) {
        stop("IR verify [expand_grid]: args must be a list")
      }
      if (length(node$args) < 2 || length(node$args) > 4) {
        stop("IR verify [expand_grid]: supports only 2 to 4 arguments in this release")
      }
      for (i in seq_along(node$args)) {
        .mojor_ir_verify(node$args[[i]], ctx)
        .require_direct_var_strict(node$args[[i]], paste0("args[[", i, "]]"), "expand_grid")
        .require_type_strict(
          node$args[[i]],
          paste0("args[[", i, "]]"),
          "expand_grid",
          allowed_tier9_expand_grid_vector_types
        )
      }
      .require_result_type_strict(node, "expand_grid", "df")
    },
    gpu_reduce = {
      .req("op", "arg")
      if (!is.character(node$op) || length(node$op) != 1) {
        stop("IR verify [gpu_reduce]: op must be a single character string")
      }
      allowed_ops <- c("sum", "mean", "prod", "min", "max", "argmin", "argmax")
      if (!node$op %in% allowed_ops) {
        stop(sprintf(
          "IR verify [gpu_reduce]: unsupported op '%s'. Supported: %s",
          node$op, paste(allowed_ops, collapse = ", ")
        ))
      }
      .mojor_ir_verify(node$arg, ctx)
      if (!is.null(node$dims)) .mojor_ir_verify(node$dims, ctx)
      if (!is.logical(node$keepdims) || length(node$keepdims) != 1 || is.na(node$keepdims)) {
        stop("IR verify [gpu_reduce]: keepdims must be a single logical value")
      }
    },
    gpu_matmul = {
      .req("a", "b", "transpose_a", "transpose_b")
      .mojor_ir_verify(node$a, ctx)
      .mojor_ir_verify(node$b, ctx)
      if (!is.logical(node$transpose_a) || length(node$transpose_a) != 1 || is.na(node$transpose_a)) {
        stop("IR verify [gpu_matmul]: transpose_a must be a single logical value")
      }
      if (!is.logical(node$transpose_b) || length(node$transpose_b) != 1 || is.na(node$transpose_b)) {
        stop("IR verify [gpu_matmul]: transpose_b must be a single logical value")
      }
      if (.strict_type_mode()) {
        .require_type_strict(node$a, "a", "gpu_matmul", allowed_gpu_matmul_matrix_types)
        .require_type_strict(node$b, "b", "gpu_matmul", allowed_gpu_matmul_matrix_types)
        a_type <- tryCatch(.mojor_ir_infer_type(node$a, ctx$type_env), error = function(e) "unknown")
        b_type <- tryCatch(.mojor_ir_infer_type(node$b, ctx$type_env), error = function(e) "unknown")
        a_label <- if (is.character(a_type) && length(a_type) == 1 && nzchar(a_type)) a_type else "unknown"
        b_label <- if (is.character(b_type) && length(b_type) == 1 && nzchar(b_type)) b_type else "unknown"
        a_elem <- sub("\\[.*$", "", a_label)
        b_elem <- sub("\\[.*$", "", b_label)
        if (!identical(a_elem, b_elem)) {
          stop(
            "IR verify [gpu_matmul]: a and b must have matching element type in strict mode (got '",
            a_label, "' vs '", b_label, "')"
          )
        }
      }
      unified_mode <- identical(ctx$gpu_jit_mode, "unified_preview") ||
        identical(ctx$gpu_jit_mode, "auto")
      if (!isTRUE(ctx$in_assign_rhs) && !unified_mode) {
        stop("IR verify [gpu_matmul]: expression form requires gpu_jit_mode='auto' or 'unified_preview'; otherwise assign the result to a variable")
      }
    },
    matmul = {
      .req("lhs", "rhs")
      .mojor_ir_verify(node$lhs, ctx)
      .mojor_ir_verify(node$rhs, ctx)
      if (.strict_type_mode()) {
        .require_direct_var_strict(node$lhs, "lhs", "matmul")
        .require_direct_var_strict(node$rhs, "rhs", "matmul")
        .require_type_strict(node$lhs, "lhs", "matmul", allowed_gpu_matmul_matrix_types)
        .require_type_strict(node$rhs, "rhs", "matmul", allowed_gpu_matmul_matrix_types)
        lhs_type <- tryCatch(.mojor_ir_infer_type(node$lhs, ctx$type_env), error = function(e) "unknown")
        rhs_type <- tryCatch(.mojor_ir_infer_type(node$rhs, ctx$type_env), error = function(e) "unknown")
        lhs_label <- if (is.character(lhs_type) && length(lhs_type) == 1 && nzchar(lhs_type)) lhs_type else "unknown"
        rhs_label <- if (is.character(rhs_type) && length(rhs_type) == 1 && nzchar(rhs_type)) rhs_type else "unknown"
        lhs_elem <- sub("\\[.*$", "", lhs_label)
        rhs_elem <- sub("\\[.*$", "", rhs_label)
        if (!identical(lhs_elem, rhs_elem)) {
          stop(
            "IR verify [matmul]: lhs and rhs must have matching element type in strict mode (got '",
            lhs_label, "' vs '", rhs_label, "')"
          )
        }
        .require_result_type_strict_if_known(node, "matmul", allowed_gpu_matmul_matrix_types)
      }
    },
    crossprod = {
      .req("lhs")
      .mojor_ir_verify(node$lhs, ctx)
      if (!is.null(node$rhs)) {
        .mojor_ir_verify(node$rhs, ctx)
      }
      if (.strict_type_mode()) {
        .require_direct_var_strict(node$lhs, "lhs", "crossprod")
        .require_type_strict(node$lhs, "lhs", "crossprod", allowed_gpu_matmul_matrix_types)
        lhs_type <- tryCatch(.mojor_ir_infer_type(node$lhs, ctx$type_env), error = function(e) "unknown")
        rhs_type <- lhs_type
        if (!is.null(node$rhs)) {
          .require_direct_var_strict(node$rhs, "rhs", "crossprod")
          .require_type_strict(node$rhs, "rhs", "crossprod", allowed_gpu_matmul_matrix_types)
          rhs_type <- tryCatch(.mojor_ir_infer_type(node$rhs, ctx$type_env), error = function(e) "unknown")
        }
        lhs_label <- if (is.character(lhs_type) && length(lhs_type) == 1 && nzchar(lhs_type)) lhs_type else "unknown"
        rhs_label <- if (is.character(rhs_type) && length(rhs_type) == 1 && nzchar(rhs_type)) rhs_type else "unknown"
        lhs_elem <- sub("\\[.*$", "", lhs_label)
        rhs_elem <- sub("\\[.*$", "", rhs_label)
        if (!identical(lhs_elem, rhs_elem)) {
          stop(
            "IR verify [crossprod]: lhs and rhs must have matching element type in strict mode (got '",
            lhs_label, "' vs '", rhs_label, "')"
          )
        }
        .require_result_type_strict_if_known(node, "crossprod", allowed_gpu_matmul_matrix_types)
      }
    },
    tcrossprod = {
      .req("lhs")
      .mojor_ir_verify(node$lhs, ctx)
      if (!is.null(node$rhs)) {
        .mojor_ir_verify(node$rhs, ctx)
      }
      if (.strict_type_mode()) {
        .require_direct_var_strict(node$lhs, "lhs", "tcrossprod")
        .require_type_strict(node$lhs, "lhs", "tcrossprod", allowed_gpu_matmul_matrix_types)
        lhs_type <- tryCatch(.mojor_ir_infer_type(node$lhs, ctx$type_env), error = function(e) "unknown")
        rhs_type <- lhs_type
        if (!is.null(node$rhs)) {
          .require_direct_var_strict(node$rhs, "rhs", "tcrossprod")
          .require_type_strict(node$rhs, "rhs", "tcrossprod", allowed_gpu_matmul_matrix_types)
          rhs_type <- tryCatch(.mojor_ir_infer_type(node$rhs, ctx$type_env), error = function(e) "unknown")
        }
        lhs_label <- if (is.character(lhs_type) && length(lhs_type) == 1 && nzchar(lhs_type)) lhs_type else "unknown"
        rhs_label <- if (is.character(rhs_type) && length(rhs_type) == 1 && nzchar(rhs_type)) rhs_type else "unknown"
        lhs_elem <- sub("\\[.*$", "", lhs_label)
        rhs_elem <- sub("\\[.*$", "", rhs_label)
        if (!identical(lhs_elem, rhs_elem)) {
          stop(
            "IR verify [tcrossprod]: lhs and rhs must have matching element type in strict mode (got '",
            lhs_label, "' vs '", rhs_label, "')"
          )
        }
        .require_result_type_strict_if_known(node, "tcrossprod", allowed_gpu_matmul_matrix_types)
      }
    },
 # --- Range / wrapper nodes ---
    range = {
      .req_node("start")
      .req_node("end")
      if (!is.null(node$step)) {
        .mojor_ir_verify(node$step, ctx)
 # Range well-formedness: step should not be zero (if constant)
        if (node$step$kind == "const") {
          step_val <- suppressWarnings(as.numeric(node$step$value))
          if (!is.na(step_val) && step_val == 0) {
            stop("IR verify [range]: step cannot be zero")
          }
        }
      }
 # Check if start/end are const integers and validate order
      if (node$start$kind == "const" && node$end$kind == "const") {
        start_val <- suppressWarnings(as.numeric(node$start$value))
        end_val <- suppressWarnings(as.numeric(node$end$value))
        if (!is.na(start_val) && !is.na(end_val)) {
          step_val <- 1
          if (!is.null(node$step)) {
            if (node$step$kind == "const") {
              step_val <- suppressWarnings(as.numeric(node$step$value))
            } else if (node$step$kind == "unop" && !is.null(node$step$op) && node$step$op == "-") {
              # Handle unary minus (negative step)
              step_val <- -1
            }
          }
          if (!is.na(step_val) && step_val != 0) {
 # For positive step, start should be <= end; for negative, start >= end
            if (step_val > 0 && start_val > end_val) {
              warning(sprintf(
                "IR verify [range]: empty range (start=%s > end=%s with positive step)",
                start_val, end_val
              ))
            } else if (step_val < 0 && start_val < end_val) {
              warning(sprintf(
                "IR verify [range]: empty range (start=%s < end=%s with negative step)",
                start_val, end_val
              ))
            }
          }
        }
      }
    },
    range_expr = {
      .req("expr")
 # expr is an R language object, not an IR node
    },
    c_call = {
      .req("name")
      .req("args")
      .req("returns")
      if (!is.character(node$name) || length(node$name) != 1 || !nzchar(node$name)) {
        stop("IR verify [c_call]: name must be a non-empty character string")
      }
      if (!is.character(node$returns) || length(node$returns) != 1 || !nzchar(node$returns)) {
        stop("IR verify [c_call]: returns must be a non-empty character string")
      }
      actual_arity <- length(node$args)
      node_arg_types <- NULL
      if (!is.null(node$arg_types)) {
        node_arg_types <- as.character(unname(unlist(node$arg_types, use.names = FALSE)))
      }
      node_arg_names <- NULL
      if (!is.null(node$arg_names)) {
        node_arg_names <- as.character(unname(unlist(node$arg_names, use.names = FALSE)))
      }
      if (!is.null(node$arg_types)) {
        if (!identical(length(node_arg_types), actual_arity)) {
          stop(
            "IR verify [c_call]: arg_types length mismatch for '", node$name,
            "': expected ", actual_arity, ", got ", length(node_arg_types)
          )
        }
      }
      if (!is.null(node$arg_names)) {
        if (!identical(length(node_arg_names), actual_arity)) {
          stop(
            "IR verify [c_call]: arg_names length mismatch for '", node$name,
            "': expected ", actual_arity, ", got ", length(node_arg_names)
          )
        }
      }
      if (!is.null(node$expected_arity)) {
        if (!is.numeric(node$expected_arity) || length(node$expected_arity) != 1 || is.na(node$expected_arity) || node$expected_arity < 0) {
          stop("IR verify [c_call]: expected_arity must be a non-negative scalar")
        }
        expected_arity <- as.integer(node$expected_arity)
        if (!identical(actual_arity, expected_arity)) {
          stop(
            "IR verify [c_call]: argument count mismatch for '", node$name,
            "': expected ", expected_arity, ", got ", actual_arity
          )
        }
      }
      decl <- .mojor_state$declared_c_functions[[node$name]]
      if (!is.null(decl)) {
        declared_arity <- length(decl$args)
        if (!identical(actual_arity, declared_arity)) {
          stop(
            "IR verify [c_call]: argument count mismatch for '", node$name,
            "': expected ", declared_arity, ", got ", actual_arity
          )
        }
        if (!identical(node$returns, decl$returns)) {
          stop(
            "IR verify [c_call]: return type mismatch for '", node$name,
            "': expected ", decl$returns, ", got ", node$returns
          )
        }
        if (!is.null(node$arg_types)) {
          declared_types_cmp <- as.character(unname(unlist(decl$args, use.names = FALSE)))
          node_types_cmp <- node_arg_types
          if (!identical(node_types_cmp, declared_types_cmp)) {
            stop(
              "IR verify [c_call]: arg_types metadata mismatch for '", node$name,
              "' against declaration"
            )
          }
        }
        if (!is.null(node$arg_names)) {
          declared_names_cmp <- as.character(names(decl$args))
          node_names_cmp <- node_arg_names
          if (!identical(node_names_cmp, declared_names_cmp)) {
            stop(
              "IR verify [c_call]: arg_names metadata mismatch for '", node$name,
              "' against declaration"
            )
          }
        }
      }
      declared_types <- NULL
      declared_names <- NULL
      if (!is.null(decl) && is.list(decl$args)) {
        declared_types <- as.character(unname(unlist(decl$args, use.names = FALSE)))
        declared_names <- names(decl$args)
      } else if (!is.null(node_arg_types)) {
        declared_types <- node_arg_types
      }
      if (is.null(declared_names) && !is.null(node_arg_names)) {
        declared_names <- node_arg_names
      }
      strict_type_mode <- .strict_type_mode()
      for (i in seq_along(node$args)) {
        a <- node$args[[i]]
        .mojor_ir_verify(a, ctx)
        if (is.null(declared_types) || length(declared_types) < i) next
        expected_type <- declared_types[[i]]
        inferred_type <- tryCatch(
          .mojor_ir_infer_type(a, ctx$type_env),
          error = function(e) "unknown"
        )
        if (is.null(inferred_type) || !is.character(inferred_type) || length(inferred_type) != 1 || !nzchar(inferred_type)) {
          inferred_type <- "unknown"
        }
        arg_label <- if (!is.null(declared_names) && length(declared_names) >= i && nzchar(declared_names[[i]])) {
          declared_names[[i]]
        } else {
          paste0("#", i)
        }
        if (strict_type_mode && identical(inferred_type, "unknown")) {
          stop(
            "IR verify [c_call]: argument type for '", node$name, "' argument '", arg_label,
            "' must be statically known in strict mode (expected ", expected_type, ")"
          )
        }
        if (!identical(inferred_type, "unknown") && !.mojor_ffi_ir_type_compatible(expected_type, inferred_type)) {
          stop(
            "IR verify [c_call]: argument type mismatch for '", node$name, "' argument '", arg_label,
            "': expected ", expected_type, ", got ", inferred_type
          )
        }
      }
    },
    raw = {
      .req("expr")
      if (isTRUE(ctx$ir_only)) {
        stop("IR verify [raw]: raw fallback node not allowed in ir_only mode")
      }
 # expr is an R language object, not an IR node
    },
    {
      stop(sprintf("IR verify: unknown node kind '%s'", k))
    }
  )
  invisible(TRUE)
}

# =============================================================================
# Step 20: HIR <U+2192> LIR Lowering Pass
# =============================================================================
#
# Explicit lowering pass that transforms HIR nodes into LIR (Core IR).
# HIR nodes (high-level, semantic) are expanded into loops/conditionals/assigns.
# LIR nodes (low-level) are directly emitted by the code generator.
#
# HIR nodes: scalar_reduce, subscript+slices, c/rep (future)
# LIR nodes: loop, if, assign (with index), while, repeat, break, next, return
#
# This pass runs after normalize and verify, before emit.

# Step 22: Subscript slice lowering (COMPLETE)
