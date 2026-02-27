# Error Message Diagnostics
# Enhanced error messages with context and actionable guidance

#' Diagnose why a top-level loop failed to transpile
#'
#' @param loop_ast AST of the failed loop
#' @param ir The IR that was built (may be NULL)
#' @param ir_typed The typed IR (may be NULL)
#' @return A list with `reason` (string) and `hint` (string) describing the failure
.mojor_diagnose_loop_failure <- function(loop_ast, ir = NULL, ir_typed = NULL) {
 # Default generic message
  reason <- "unsupported loop pattern"
  hint <- "see TROUBLESHOOTING.md for common patterns"
  doc_link <- "https://github.com/kose-y/MojoR/blob/main/docs/TROUBLESHOOTING.md"

 # Check for unsupported seq() calls first (before IR check)
  if (is.null(ir) && .mojor_has_unsupported_seq(loop_ast)) {
    seq_info <- .mojor_analyze_seq_pattern(loop_ast)
    if (!is.null(seq_info)) {
      return(list(
        reason = seq_info$reason,
        hint = seq_info$hint,
        doc_link = seq_info$doc_link,
        pattern = seq_info$pattern
      ))
    }
  }
  loop_shape_info <- .mojor_diagnose_loop_shape(loop_ast)
  if (!is.null(loop_shape_info)) {
    return(loop_shape_info)
  }

 # If IR was NULL, the IR builder failed
  if (is.null(ir)) {
 # Try to diagnose from AST
    body <- loop_ast[[3]] # for loop body is 3rd element

 # Check for unsupported seq() calls
    if (.mojor_has_unsupported_seq(loop_ast)) {
      reason <- "unsupported seq() call (use seq_len() or seq_along())"
      hint <- "replace seq(to=n) with seq_len(n)"
      doc_link <- "SUBSET_V1.md"
      return(list(
        reason = reason,
        hint = hint,
        doc_link = doc_link,
        pattern = "unsupported_seq"
      ))
    }

 # Check for type mismatch in assignment
    if (.mojor_has_type_mismatch_assignment(body)) {
      reason <- "type mismatch in assignment (e.g., logical output with numeric RHS)"
      hint <- "ensure output type matches expression type"
      doc_link <- "SUBSET_V1.md"
      return(list(
        reason = reason,
        hint = hint,
        doc_link = doc_link,
        pattern = "type_mismatch"
      ))
    }
  }

 # If IR was built but typed IR was NULL, the type checker failed
  if (!is.null(ir) && is.null(ir_typed)) {
    reason <- "type checking failed for loop body"
    hint <- "check for unsupported operations or type mismatches"
    doc_link <- "TROUBLESHOOTING.md#type-errors"
    return(list(
      reason = reason,
      hint = hint,
      doc_link = doc_link,
      pattern = "type_check_failed"
    ))
  }

 # If both IR and typed IR exist but emission failed, it's an emission issue
  if (!is.null(ir) && !is.null(ir_typed)) {
    reason <- "code generation failed for loop"
    hint <- "check for complex expressions that need simplification"
    doc_link <- "TROUBLESHOOTING.md"
    return(list(
      reason = reason,
      hint = hint,
      doc_link = doc_link,
      pattern = "codegen_failed"
    ))
  }

 # Generic fallback
  return(list(
    reason = reason,
    hint = hint,
    doc_link = doc_link,
    pattern = "unknown"
  ))
}

#' Diagnose common unsupported loop-shape patterns.
#'
#' @param loop_ast Loop AST node.
#' @return A diagnostic list or NULL when no specific shape pattern matches.
.mojor_diagnose_loop_shape <- function(loop_ast) {
  if (!is.call(loop_ast) || length(loop_ast) < 1) {
    return(NULL)
  }
  loop_kind <- as.character(loop_ast[[1]])
  if (length(loop_kind) != 1) {
    loop_kind <- loop_kind[[1]]
  }
  if (identical(loop_kind, "for") && length(loop_ast) >= 3) {
    iter_expr <- loop_ast[[3]]
    if (is.call(iter_expr)) {
      iter_fn <- as.character(iter_expr[[1]])
      if (length(iter_fn) != 1) {
        iter_fn <- iter_fn[[1]]
      }
      if (iter_fn %in% c("sample", "sample.int")) {
        return(list(
          reason = "code generation failed for loop (non-deterministic sampled iterator)",
          hint = "rewrite `for (i in sample(...))` as a deterministic range loop and sample inside the loop body",
          doc_link = "docs/TROUBLESHOOTING.md",
          pattern = "for_sample_iterator"
        ))
      }
      supported_iter_fns <- c(":", "seq", "seq.int", "seq_len", "seq_along")
      if (!(iter_fn %in% supported_iter_fns)) {
        return(list(
          reason = paste0("code generation failed for loop (unsupported iterator function `", iter_fn, "()`)"),
          hint = "use `:`, `seq`, `seq.int`, `seq_len`, or `seq_along` for loop iteration",
          doc_link = "docs/TROUBLESHOOTING.md",
          pattern = "for_unsupported_iterator_fn"
        ))
      }
    }
    return(NULL)
  }
  if (identical(loop_kind, "repeat") && length(loop_ast) >= 2) {
    if (!.mojor_has_break_call(loop_ast[[2]])) {
      return(list(
        reason = "code generation failed for loop (repeat loop without explicit break)",
        hint = "add an explicit `break` guard in `repeat` loops, or rewrite using `while`/`for`",
        doc_link = "docs/TROUBLESHOOTING.md",
        pattern = "repeat_without_break"
      ))
    }
  }
  NULL
}

#' Check whether an expression subtree includes an explicit `break`.
#'
#' @param expr AST node to inspect recursively.
#' @return TRUE when at least one `break` call exists.
.mojor_has_break_call <- function(expr) {
  if (is.null(expr) || !is.call(expr) || length(expr) == 0) {
    return(FALSE)
  }
  op <- as.character(expr[[1]])
  if (length(op) != 1) {
    op <- op[[1]]
  }
  if (identical(op, "break")) {
    return(TRUE)
  }
  if (identical(op, "{")) {
    for (i in 2:length(expr)) {
      if (.mojor_has_break_call(expr[[i]])) {
        return(TRUE)
      }
    }
    return(FALSE)
  }
  if (identical(op, "if")) {
    if (length(expr) >= 3 && .mojor_has_break_call(expr[[3]])) {
      return(TRUE)
    }
    if (length(expr) >= 4 && .mojor_has_break_call(expr[[4]])) {
      return(TRUE)
    }
    return(FALSE)
  }
  for (i in 2:length(expr)) {
    if (.mojor_has_break_call(expr[[i]])) {
      return(TRUE)
    }
  }
  FALSE
}

#' Check if body contains slice on RHS (`x[1:n]`)
#'
#' @param body Loop/body AST node to inspect recursively.
.mojor_has_slice_on_rhs <- function(body) {
  if (!is.call(body)) {
    return(FALSE)
  }

  if (as.character(body[[1]]) == "{") {
 # Block: check all statements
    for (i in 2:length(body)) {
      if (.mojor_has_slice_on_rhs(body[[i]])) {
        return(TRUE)
      }
    }
    return(FALSE)
  }

  if (as.character(body[[1]]) == "<-") {
    rhs <- body[[3]]
    return(.mojor_expr_has_slice(rhs))
  }

  FALSE
}

#' Check if expression contains a slice (`start:end`)
#'
#' @param expr Expression AST node to inspect recursively.
.mojor_expr_has_slice <- function(expr) {
  if (!is.call(expr)) {
    return(FALSE)
  }

  op <- as.character(expr[[1]])

  if (op == "[" && length(expr) >= 3) {
 # Check if index is a range expression
    idx <- expr[[3]]
    if (is.call(idx) && as.character(idx[[1]]) == ":") {
      return(TRUE)
    }
  }

 # Recursively check sub-expressions
  for (i in 2:length(expr)) {
    if (.mojor_expr_has_slice(expr[[i]])) {
      return(TRUE)
    }
  }

  FALSE
}

#' Check if loop uses unsupported seq() call
#'
#' @param loop_ast Loop AST node to inspect.
.mojor_has_unsupported_seq <- function(loop_ast) {
  if (!is.call(loop_ast) || as.character(loop_ast[[1]]) != "for") {
    return(FALSE)
  }

  iter_expr <- loop_ast[[3]] # for (i in ITER_EXPR)

  if (is.call(iter_expr)) {
    fn_name <- as.character(iter_expr[[1]])

 # Check for seq(to = n) without from
    if (fn_name == "seq") {
 # Check if it has named 'to' argument without 'from'
      arg_names <- names(iter_expr)
      if (!is.null(arg_names) && "to" %in% arg_names) {
        if (!"from" %in% arg_names && !"along.with" %in% arg_names) {
          return(TRUE)
        }
      }
    }
  }

  FALSE
}

#' Analyze unsupported seq() pattern and return diagnostic info
#'
#' @param loop_ast Loop AST node to inspect.
.mojor_analyze_seq_pattern <- function(loop_ast) {
  if (!is.call(loop_ast) || as.character(loop_ast[[1]]) != "for") {
    return(NULL)
  }

  iter_expr <- loop_ast[[3]] # for (i in ITER_EXPR)

  if (!is.call(iter_expr)) {
    return(NULL)
  }

  fn_name <- as.character(iter_expr[[1]])

 # seq() without from or along.with - use seq_len() instead
  if (fn_name == "seq") {
    arg_names <- names(iter_expr)
    if (!is.null(arg_names) && "to" %in% arg_names) {
      if (!"from" %in% arg_names && !"along.with" %in% arg_names) {
        return(list(
          reason = "unsupported seq() call (use seq_len() or seq_along())",
          hint = "replace seq(to=n) with seq_len(n) or use seq_along(array)",
          doc_link = "SUBSET_V1.md#loop-ranges",
          pattern = "unsupported_seq_to_only"
        ))
      }
    }
 # seq() with from/to but no length.out - use seq.int() instead
    if (!is.null(arg_names) && "from" %in% arg_names && "to" %in% arg_names) {
      if (!"length.out" %in% arg_names && !"by" %in% arg_names) {
        return(list(
          reason = "seq() with from/to requires length.out or by argument",
          hint = "add length.out=n or by=step to seq(from,to)",
          doc_link = "SUBSET_V1.md#loop-ranges",
          pattern = "unsupported_seq_from_to"
        ))
      }
    }
  }

 # seq.int() with float by for indexing
  if (fn_name == "seq.int") {
    arg_names <- names(iter_expr)
    if (!is.null(arg_names) && "by" %in% arg_names) {
      return(list(
        reason = "seq.int() float by not supported for indexing",
        hint = "use integer by or use seq() for float steps",
        doc_link = "SUBSET_V1.md#loop-ranges",
        pattern = "unsupported_seqint_float_by"
      ))
    }
  }

 # seq_along() or seq_len() with unsupported argument
  if (fn_name %in% c("seq_along", "seq_len")) {
    if (length(iter_expr) >= 2) {
      arg <- iter_expr[[2]]
      if (is.call(arg) && as.character(arg[[1]]) == "length") {
 # seq_len(length(x)) is fine, but seq_len(length(x) + y) might not be
        return(list(
          reason = "unsupported seq_len() or seq_along() argument",
          hint = "use seq_len(length(array)) or seq_along(array) with a simple name",
          doc_link = "SUBSET_V1.md#loop-ranges",
          pattern = "unsupported_seq_len_arg"
        ))
      }
    }
  }

  NULL
}

#' Check if body has type mismatch in assignment
#'
#' @param body Loop/body AST node to inspect recursively.
.mojor_has_type_mismatch_assignment <- function(body) {
  if (!is.call(body)) {
    return(FALSE)
  }

  if (as.character(body[[1]]) == "{") {
 # Block: check all statements
    for (i in 2:length(body)) {
      if (.mojor_has_type_mismatch_assignment(body[[i]])) {
        return(TRUE)
      }
    }
    return(FALSE)
  }

  if (as.character(body[[1]]) == "<-") {
    lhs <- body[[2]]
    rhs <- body[[3]]

 # Check if assigning to logical() array with non-logical RHS
 # This is a heuristic based on common patterns
    if (is.call(lhs) && as.character(lhs[[1]]) == "[") {
      base_var <- lhs[[2]]
 # We'd need type info to be certain, but can check for obvious cases
 # like numeric operations assigned to logical array
      if (is.call(rhs)) {
        rhs_op <- as.character(rhs[[1]])
 # Arithmetic operators suggest numeric result
        if (rhs_op %in% c("+", "-", "*", "/", "^", "%%", "%/%")) {
 # This might be a type mismatch (but we need context to be sure)
 # For now, we'll be conservative and not flag this
          return(FALSE)
        }
      }
    }
  }

  FALSE
}
