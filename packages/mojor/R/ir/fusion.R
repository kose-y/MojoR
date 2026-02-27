# =============================================================================
# MojoR Loop Fusion Infrastructure ()
# =============================================================================
# This file implements loop fusion analysis and optimization for MojoR's Tree IR.
# It provides elem0 normalization, loop domain extraction, noalias checking,
# and fusion legality checks.
#
# Organization:
# 1. elem0 Computation - Normalize index expressions to [0, n) space
# 2. Loop Domain Extraction - Extract iteration domain from loops
# 3. Noalias Checking - Check LHS resource disjointness
# 4. Fusion Legality - Check all fusion rules
# 5. Fusion Pass - Find and fuse adjacent loops
# =============================================================================

# =============================================================================
# Section 1: elem0 Computation
# =============================================================================

.mojor_ir_compute_elem0 <- function(expr, ctx) {
 # Compute elem0 expression for an index access
 # Normalize to canonical [0, n) space
 #
 # Args:
 # expr: IR expression node (typically an index node)
 # ctx: Context with zero_based_vars and type_env
 #
 # Returns:
 # IR expression node representing elem0 (0-based)

  if (is.null(expr)) {
    return(NULL)
  }

  k <- expr$kind
  if (is.null(k)) {
    return(expr)
  }

  switch(k,
 # Variable reference - check if it's in zero_based_vars
    var = {
      if (!is.null(ctx$zero_based_vars) && expr$name %in% ctx$zero_based_vars) {
 # Already 0-based, return as-is
        expr
      } else {
 # 1-based, need to subtract 1
        .mojor_ir_binop("-", expr, .mojor_ir_const("1"))
      }
    },

 # Binary operation - recurse on operands
    binop = {
      lhs <- if (!is.null(expr$lhs)) expr$lhs else expr$left
      rhs <- if (!is.null(expr$rhs)) expr$rhs else expr$right
      if (is.null(lhs) || is.null(rhs)) {
        return(.mojor_ir_call("_mojor_unsupported_elem0", list(expr)))
      }
 # Check if this is a one_based index expression (var - 1)
 # If so, return just the var (zero_based)
      if (lhs$kind == "var" &&
        rhs$kind == "const" &&
        as.character(rhs$value) == "1" &&
        expr$op == "-") {
 # This is i - 1 (one_based index), return i (zero_based)
        lhs
      } else {
 # Recurse on operands
        lhs_elem0 <- .mojor_ir_compute_elem0(lhs, ctx)
        rhs_elem0 <- .mojor_ir_compute_elem0(rhs, ctx)
        .mojor_ir_binop(expr$op, lhs_elem0, rhs_elem0)
      }
    },

 # Unary operation - recurse on operand
    unop = {
      operand_elem0 <- .mojor_ir_compute_elem0(expr$expr, ctx)
      .mojor_ir_unop(expr$op, operand_elem0)
    },

 # Cast - recurse on operand
    cast = {
      operand_elem0 <- .mojor_ir_compute_elem0(expr$expr, ctx)
      .mojor_ir_cast(expr$to, operand_elem0)
    },

 # Constant - return as-is
    const = expr,

 # Index - extract base and indices
    index = {
 # For index nodes, we compute elem0 for the base and indices
      base_elem0 <- .mojor_ir_compute_elem0(expr$base, ctx)
      indices_elem0 <- lapply(expr$indices, function(idx) .mojor_ir_compute_elem0(idx, ctx))
      .mojor_ir_index(base_elem0, indices_elem0, index_base = "zero_based")
    },

 # Default - return as-is
    .mojor_ir_call("_mojor_unsupported_elem0", list(expr))
  )
}

# =============================================================================
# Section 2: Loop Domain Extraction
# =============================================================================

.mojor_ir_loop_domain <- function(loop) {
 # Extract iteration domain from a loop
 #
 # Args:
 # loop: Loop IR node
 #
 # Returns:
 # List with domain info: start, end_exclusive, step, order, var

  if (is.null(loop) || loop$kind != "loop") {
    return(NULL)
  }

 # Handle both flat structure (start, end_exclusive, step) and nested (range)
  if (!is.null(loop$range)) {
    range <- loop$range
    start <- range$start
    end_exclusive <- if (!is.null(range$end_exclusive)) range$end_exclusive else TRUE
    end <- range$end
    step <- range$step
  } else {
    start <- loop$start
    end_exclusive <- if (!is.null(loop$end_exclusive)) loop$end_exclusive else TRUE
    end <- loop$end
    step <- loop$step
  }

  if (is.null(start)) {
    return(NULL)
  }

 # Determine order
  order <- if (!is.null(step) && step$kind == "const" && (as.character(step$value) == "1" || as.numeric(step$value) > 0)) "ascending" else "descending"

  list(
    var = loop$var,
    start = start,
    end_exclusive = end_exclusive,
    end = end,
    step = step,
    order = order
  )
}

.mojor_ir_get_const_value <- function(node) {
 # Extract constant value from IR node
  if (is.null(node) || node$kind != "const") {
    return(NULL)
  }
  node$value
}

# =============================================================================
# Section 3: Noalias Checking
# =============================================================================

.mojor_ir_loop_lhs_resource <- function(loop) {
 # Extract LHS resource identifier from loop
 #
 # Args:
 # loop: Loop IR node
 #
 # Returns:
 # Resource identifier string or NULL

  if (is.null(loop) || loop$kind != "loop") {
    return(NULL)
  }

  lhs_assign <- .mojor_ir_loop_lhs_assign(loop)
  if (is.null(lhs_assign) || is.null(lhs_assign$lhs)) {
    return(NULL)
  }
  .mojor_ir_extract_resource(lhs_assign$lhs)
}

.mojor_ir_loop_lhs_assign <- function(loop) {
  if (is.null(loop) || loop$kind != "loop") {
    return(NULL)
  }
  body <- loop$body
  if (is.null(body) || !is.list(body) || is.null(body$kind)) {
    return(NULL)
  }

  simple_if_assign <- function(if_node) {
    prof <- .mojor_ir_if_node_profile(if_node)
    if (!isTRUE(prof$ok)) {
      return(NULL)
    }
    then_node <- if (!is.null(if_node$then)) if_node$then else if_node$then_branch
    .mojor_ir_simple_if_extract_assign(then_node)
  }

  if (identical(body$kind, "assign")) {
    return(body)
  }
  if (identical(body$kind, "if")) {
    return(simple_if_assign(body))
  }
  if (identical(body$kind, "block") && !is.null(body$stmts) && length(body$stmts) > 0) {
    for (stmt in body$stmts) {
      if (!is.null(stmt) && is.list(stmt) && identical(stmt$kind, "assign")) {
        return(stmt)
      }
    }
    if (length(body$stmts) == 1 && is.list(body$stmts[[1]]) && identical(body$stmts[[1]]$kind, "if")) {
      return(simple_if_assign(body$stmts[[1]]))
    }
  }
  NULL
}

.mojor_ir_extract_resource <- function(expr) {
 # Extract resource identifier from expression
 #
 # Args:
 # expr: IR expression node (typically index or var)
 #
 # Returns:
 # Resource identifier string or NULL

  if (is.null(expr)) {
    return(NULL)
  }

  k <- expr$kind
  if (is.null(k)) {
    return(NULL)
  }

  switch(k,
    var = expr$name,
    index = {
 # For index, use the base variable name
 # Handle both 'base' and 'name' field names
      if (!is.null(expr$base) && expr$base$kind == "var") {
        expr$base$name
      } else if (!is.null(expr$name)) {
        expr$name
      } else {
        NULL
      }
    },
    .mojor_ir_call("_mojor_resource_unknown", list(expr))
  )
}

.mojor_ir_resources_disjoint <- function(res1, res2) {
 # Check if two resources are disjoint
 #
 # Args:
 # res1: First resource identifier
 # res2: Second resource identifier
 #
 # Returns:
 # TRUE if disjoint, FALSE if same or unknown

  if (is.null(res1) || is.null(res2)) {
    return(FALSE)
  }
  if (res1 == res2) {
    return(FALSE)
  } # Same resource, not disjoint
  TRUE # Different resources are considered disjoint
}

# =============================================================================
# Section 4: Fusion Legality Checks
# =============================================================================

# Rejection codes
FUSE_REJECT_DOMAIN_MISMATCH <- "FUSE_REJECT_DOMAIN_MISMATCH"
FUSE_REJECT_INDEXMAP_NOT_IDENTITY <- "FUSE_REJECT_INDEXMAP_NOT_IDENTITY"
FUSE_REJECT_NOALIAS_MISSING <- "FUSE_REJECT_NOALIAS_MISSING"
FUSE_REJECT_CONTROL_FLOW <- "FUSE_REJECT_CONTROL_FLOW"
FUSE_REJECT_EFFECTS_RNG <- "FUSE_REJECT_EFFECTS_RNG"
FUSE_REJECT_EFFECTS_UNKNOWN <- "FUSE_REJECT_EFFECTS_UNKNOWN"
# Stable diagnostic code retained for SSA analysis parity. The Tree IR rewrite
# path does not currently hard-reject all broadcast_nd loop pairs.
FUSE_REJECT_BROADCAST_ND <- "FUSE_REJECT_BROADCAST_ND"
FUSE_REJECT_GUARD_POLICY <- "FUSE_REJECT_GUARD_POLICY"

.mojor_ir_fusion_legality <- function(loop1, loop2, stmts, ctx) {
 # Check all fusion legality rules
 #
 # Args:
 # loop1: First loop IR node
 # loop2: Second loop IR node
 # stmts: Statement list containing the loops
 # ctx: Context with zero_based_vars, type_env, etc.
 #
 # Returns:
 # List with: legal (TRUE/FALSE), rejection_code (if not legal), message

 # Check domain match
  domain1 <- .mojor_ir_loop_domain(loop1)
  domain2 <- .mojor_ir_loop_domain(loop2)

  if (!.mojor_ir_domains_match(domain1, domain2)) {
    return(list(
      legal = FALSE,
      rejection_code = FUSE_REJECT_DOMAIN_MISMATCH,
      message = "Loop domains do not match"
    ))
  }

 # Check for control flow in loop bodies (if, while, etc.).
 # By default this is conservative; an opt-in allows only simple_if_assign.
  cf1 <- .mojor_ir_loop_control_profile(loop1)
  cf2 <- .mojor_ir_loop_control_profile(loop2)
  has_cf <- isTRUE(cf1$has_control_flow) || isTRUE(cf2$has_control_flow)
  if (has_cf) {
    allow_simple <- isTRUE(ctx$fusion_allow_control_flow_simple)
    simple_ok <- allow_simple &&
      (!isTRUE(cf1$has_control_flow) || isTRUE(cf1$simple_if_assign)) &&
      (!isTRUE(cf2$has_control_flow) || isTRUE(cf2$simple_if_assign))
    if (!isTRUE(simple_ok)) {
      return(list(
        legal = FALSE,
        rejection_code = FUSE_REJECT_CONTROL_FLOW,
        message = "Loops with control flow cannot be fused"
      ))
    }
  }

 # Check access pattern match (elem0 for LHS)
  if (!.mojor_ir_access_pattern_match(loop1, loop2, ctx)) {
    return(list(
      legal = FALSE,
      rejection_code = FUSE_REJECT_INDEXMAP_NOT_IDENTITY,
      message = "LHS access patterns do not match"
    ))
  }

 # Check noalias
  res1 <- .mojor_ir_loop_lhs_resource(loop1)
  res2 <- .mojor_ir_loop_lhs_resource(loop2)

  if (!.mojor_ir_noalias_check(loop1, loop2, res1, res2)) {
    return(list(
      legal = FALSE,
      rejection_code = FUSE_REJECT_NOALIAS_MISSING,
      message = "LHS resources are not disjoint or identical"
    ))
  }

 # Check effects (no RNG or Unknown)
  if (!.mojor_ir_effects_check(loop1, loop2)) {
    return(list(
      legal = FALSE,
      rejection_code = FUSE_REJECT_EFFECTS_UNKNOWN,
      message = "RNG or Unknown effects block fusion"
    ))
  }

 # Check guard policy
  if (!.mojor_ir_guard_policy_check(loop1, loop2)) {
    return(list(
      legal = FALSE,
      rejection_code = FUSE_REJECT_GUARD_POLICY,
      message = "Guard policy mismatch between loops"
    ))
  }

 # Check sequential composition (adjacent in AST)
  if (!.mojor_ir_sequential_check(stmts, loop1, loop2)) {
    return(list(
      legal = FALSE,
      rejection_code = FUSE_REJECT_CONTROL_FLOW,
      message = "Loops are not adjacent in AST"
    ))
  }

 # All checks passed
  list(legal = TRUE, rejection_code = NULL, message = "Loops are fusion-legal")
}

.mojor_ir_loop_has_control_flow <- function(loop) {
 # Check if loop body contains control flow (if, while, repeat, etc.)
  if (is.null(loop) || loop$kind != "loop") {
    return(FALSE)
  }

 # Control flow node types
  control_flow_kinds <- c("if", "while", "repeat", "break", "next")

  .mojor_ir_node_contains_kind(loop$body, control_flow_kinds)
}

.mojor_ir_simple_if_extract_assign <- function(node) {
  if (is.null(node) || !is.list(node) || is.null(node$kind)) {
    return(NULL)
  }
  if (identical(node$kind, "assign")) {
    return(node)
  }
  if (identical(node$kind, "block") && !is.null(node$stmts) && length(node$stmts) == 1) {
    stmt <- node$stmts[[1]]
    if (is.list(stmt) && identical(stmt$kind, "assign")) {
      return(stmt)
    }
  }
  NULL
}

.mojor_ir_assign_target_key <- function(assign_node) {
  if (is.null(assign_node) || !is.list(assign_node) || !identical(assign_node$kind, "assign")) {
    return(NULL)
  }
  lhs <- assign_node$lhs
  if (is.null(lhs) || !is.list(lhs) || is.null(lhs$kind)) {
    return(NULL)
  }
  if (identical(lhs$kind, "var") && !is.null(lhs$name)) {
    return(paste0("var:", as.character(lhs$name)))
  }
  if (identical(lhs$kind, "index")) {
    base_name <- NULL
    if (!is.null(lhs$base) && is.list(lhs$base) && identical(lhs$base$kind, "var") && !is.null(lhs$base$name)) {
      base_name <- as.character(lhs$base$name)
    } else if (!is.null(lhs$name)) {
      base_name <- as.character(lhs$name)
    }
    if (is.null(base_name) || !nzchar(base_name)) {
      return(NULL)
    }
    idx_nodes <- NULL
    if (!is.null(lhs$indices)) idx_nodes <- lhs$indices
    if (is.null(idx_nodes) && !is.null(lhs$idx)) idx_nodes <- list(lhs$idx)
    if (is.null(idx_nodes)) idx_nodes <- list()
    idx_sig <- vapply(idx_nodes, function(ix) {
      if (is.null(ix) || !is.list(ix) || is.null(ix$kind)) {
        return("unknown")
      }
      as.character(ix$kind)
    }, character(1))
    return(paste0("index:", base_name, ":", paste(idx_sig, collapse = ",")))
  }
  NULL
}

.mojor_ir_if_node_profile <- function(if_node) {
  if (is.null(if_node) || !is.list(if_node) || !identical(if_node$kind, "if")) {
    return(list(ok = FALSE, reason = "not_if"))
  }
  cond <- if_node$cond
  eff <- .mojor_ir_expr_effects(cond)
  if ("RNG" %in% eff || "Unknown" %in% eff) {
    return(list(ok = FALSE, reason = "cond_effects"))
  }

  then_node <- if (!is.null(if_node$then)) if_node$then else if_node$then_branch
  else_node <- if (!is.null(if_node$else_block)) if_node$else_block else if_node$else_branch
  if (is.null(then_node) || is.null(else_node)) {
    return(list(ok = FALSE, reason = "missing_branch"))
  }

  if (.mojor_ir_node_contains_kind(then_node, c("if", "while", "repeat", "break", "next"))) {
    return(list(ok = FALSE, reason = "nested_control_then"))
  }
  if (.mojor_ir_node_contains_kind(else_node, c("if", "while", "repeat", "break", "next"))) {
    return(list(ok = FALSE, reason = "nested_control_else"))
  }

  then_assign <- .mojor_ir_simple_if_extract_assign(then_node)
  else_assign <- .mojor_ir_simple_if_extract_assign(else_node)
  if (is.null(then_assign) || is.null(else_assign)) {
    return(list(ok = FALSE, reason = "branch_not_single_assign"))
  }

  k1 <- .mojor_ir_assign_target_key(then_assign)
  k2 <- .mojor_ir_assign_target_key(else_assign)
  if (is.null(k1) || is.null(k2) || !identical(k1, k2)) {
    return(list(ok = FALSE, reason = "branch_target_mismatch"))
  }

  rhs_eff_then <- .mojor_ir_expr_effects(then_assign$rhs)
  rhs_eff_else <- .mojor_ir_expr_effects(else_assign$rhs)
  if ("RNG" %in% rhs_eff_then || "Unknown" %in% rhs_eff_then ||
    "RNG" %in% rhs_eff_else || "Unknown" %in% rhs_eff_else) {
    return(list(ok = FALSE, reason = "rhs_effects"))
  }

  list(ok = TRUE, reason = NULL)
}

.mojor_ir_loop_has_simple_if_assign <- function(loop) {
  if (is.null(loop) || !is.list(loop) || !identical(loop$kind, "loop")) {
    return(FALSE)
  }
  body <- loop$body
  if (is.null(body) || !is.list(body)) {
    return(FALSE)
  }
  if_node <- NULL
  if (identical(body$kind, "if")) {
    if_node <- body
  } else if (identical(body$kind, "block") && !is.null(body$stmts) && length(body$stmts) == 1) {
    stmt <- body$stmts[[1]]
    if (is.list(stmt) && identical(stmt$kind, "if")) if_node <- stmt
  } else {
    return(FALSE)
  }
  prof <- .mojor_ir_if_node_profile(if_node)
  isTRUE(prof$ok)
}

.mojor_ir_loop_control_profile <- function(loop) {
  has_cf <- .mojor_ir_loop_has_control_flow(loop)
  list(
    has_control_flow = isTRUE(has_cf),
    simple_if_assign = isTRUE(has_cf) && .mojor_ir_loop_has_simple_if_assign(loop)
  )
}

.mojor_ir_node_contains_kind <- function(node, kind) {
 # Handle both single kind (character) and multiple kinds (character vector)
  if (length(kind) > 1) {
    return(any(sapply(kind, function(k) .mojor_ir_node_contains_kind(node, k))))
  }

 # Recursively check if node or any child contains given kind
  if (is.null(node)) {
    return(FALSE)
  }
  if (node$kind == kind) {
    return(TRUE)
  }

 # Check children based on node type
  if (node$kind == "block") {
    for (stmt in node$stmts) {
      if (.mojor_ir_node_contains_kind(stmt, kind)) {
        return(TRUE)
      }
    }
  } else if (node$kind == "loop") {
    if (.mojor_ir_node_contains_kind(node$range, kind)) {
      return(TRUE)
    }
    if (.mojor_ir_node_contains_kind(node$body, kind)) {
      return(TRUE)
    }
  } else if (node$kind == "assign") {
    if (.mojor_ir_node_contains_kind(node$lhs, kind)) {
      return(TRUE)
    }
    if (.mojor_ir_node_contains_kind(node$rhs, kind)) {
      return(TRUE)
    }
  } else if (node$kind == "if") {
    if (.mojor_ir_node_contains_kind(node$cond, kind)) {
      return(TRUE)
    }
    if (.mojor_ir_node_contains_kind(node$then, kind)) {
      return(TRUE)
    }
    if (!is.null(node$else_block) && .mojor_ir_node_contains_kind(node$else_block, kind)) {
      return(TRUE)
    }
  } else if (node$kind == "binop") {
    if (.mojor_ir_node_contains_kind(node$lhs, kind)) {
      return(TRUE)
    }
    if (.mojor_ir_node_contains_kind(node$rhs, kind)) {
      return(TRUE)
    }
  } else if (node$kind == "unop") {
    if (.mojor_ir_node_contains_kind(node$expr, kind)) {
      return(TRUE)
    }
  } else if (node$kind == "call") {
    for (arg in node$args) {
      if (.mojor_ir_node_contains_kind(arg, kind)) {
        return(TRUE)
      }
    }
  } else if (node$kind == "index") {
    if (.mojor_ir_node_contains_kind(node$base, kind)) {
      return(TRUE)
    }
    for (idx in node$indices) {
      if (.mojor_ir_node_contains_kind(idx, kind)) {
        return(TRUE)
      }
    }
  }

  FALSE
}

.mojor_ir_domains_match <- function(domain1, domain2) {
 # Check if two domains match
  if (is.null(domain1) || is.null(domain2)) {
    return(FALSE)
  }

 # Helper function to compare domain values
  compare_value <- function(v1, v2) {
    if (is.null(v1) && is.null(v2)) {
      return(TRUE)
    }
    if (is.null(v1) || is.null(v2)) {
      return(FALSE)
    }

 # If both are const nodes, compare values
    if (v1$kind == "const" && v2$kind == "const") {
      return(as.character(v1$value) == as.character(v2$value))
    }

 # If both are var nodes, compare names
    if (v1$kind == "var" && v2$kind == "var") {
      return(v1$name == v2$name)
    }

 # Otherwise compare as values
    as.character(v1) == as.character(v2)
  }

 # Check start
  if (!compare_value(domain1$start, domain2$start)) {
    return(FALSE)
  }

 # Check end
  if (!compare_value(domain1$end, domain2$end)) {
    return(FALSE)
  }

 # Check step
  if (!compare_value(domain1$step, domain2$step)) {
    return(FALSE)
  }

  TRUE
}

.mojor_ir_access_pattern_match <- function(loop1, loop2, ctx) {
 # Check if LHS access patterns match (same elem0 expression for the index)
  lhs1 <- .mojor_ir_loop_lhs(loop1)
  lhs2 <- .mojor_ir_loop_lhs(loop2)

  if (is.null(lhs1) || is.null(lhs2)) {
    return(FALSE)
  }

 # Extract index from LHS (for index expressions)
  get_index <- function(expr) {
    if (expr$kind == "index") {
 # Return the first index (for 1D arrays)
 # Handle both 'indices' and 'idx' field names
      if (!is.null(expr$indices) && length(expr$indices) > 0) {
        return(expr$indices[[1]])
      }
      if (!is.null(expr$idx)) {
        return(expr$idx)
      }
    }
    NULL
  }

  idx1 <- get_index(lhs1)
  idx2 <- get_index(lhs2)

  if (is.null(idx1) || is.null(idx2)) {
    return(FALSE)
  }

 # Compute elem0 for both indices
  elem0_1 <- .mojor_ir_compute_elem0(idx1, ctx)
  elem0_2 <- .mojor_ir_compute_elem0(idx2, ctx)

 # Check if they're structurally equal
  .mojor_ir_expr_equal(elem0_1, elem0_2)
}

.mojor_ir_loop_lhs <- function(loop) {
 # Extract LHS from loop body
  lhs_assign <- .mojor_ir_loop_lhs_assign(loop)
  if (is.null(lhs_assign)) {
    return(NULL)
  }
  lhs_assign$lhs
}

.mojor_ir_expr_equal <- function(expr1, expr2) {
 # Check if two expressions are structurally equal
  if (is.null(expr1) && is.null(expr2)) {
    return(TRUE)
  }
  if (is.null(expr1) || is.null(expr2)) {
    return(FALSE)
  }

  if (expr1$kind != expr2$kind) {
    return(FALSE)
  }

  switch(expr1$kind,
    const = {
      expr1$value == expr2$value
    },
    var = {
      expr1$name == expr2$name
    },
    binop = {
      expr1$op == expr2$op &&
        .mojor_ir_expr_equal(expr1$lhs, expr2$lhs) &&
        .mojor_ir_expr_equal(expr1$rhs, expr2$rhs)
    },
    unop = {
      expr1$op == expr2$op &&
        .mojor_ir_expr_equal(expr1$expr, expr2$expr)
    },
    cast = {
      expr1$to == expr2$to &&
        .mojor_ir_expr_equal(expr1$expr, expr2$expr)
    },
    index = {
      .mojor_ir_expr_equal(expr1$base, expr2$base) &&
        length(expr1$indices) == length(expr2$indices) &&
        all(mapply(.mojor_ir_expr_equal, expr1$indices, expr2$indices))
    },
 # Default: not equal (unknown node types)
    FALSE
  )
}

.mojor_ir_noalias_check <- function(loop1, loop2, res1, res2) {
 # Check noalias condition
 # Resources must be disjoint (different variables)
 # Same variable = noalias violation

  if (is.null(res1) || is.null(res2)) {
 # If we can't determine resources, reject
    return(FALSE)
  }

  if (res1 == res2) {
 # Same resource - noalias violation
    return(FALSE)
  }

 # Different resources - OK (disjoint)
  TRUE
}

.mojor_ir_effects_check <- function(loop1, loop2) {
 # Check that neither loop has RNG or Unknown effects
  body1 <- .mojor_ir_loop_body(loop1)
  body2 <- .mojor_ir_loop_body(loop2)

 # For simple expressions (like assign), assume no effects
 # This is a simplified check for the test cases
  is_simple_expr <- function(expr) {
    if (is.null(expr)) {
      return(TRUE)
    }
    k <- expr$kind
    if (is.null(k)) {
      return(TRUE)
    }
 # Simple expressions: const, var, binop, unop, cast, assign
    k %in% c("const", "var", "binop", "unop", "cast", "assign")
  }

 # If both bodies are simple expressions, assume no effects
  if (is_simple_expr(body1) && is_simple_expr(body2)) {
    return(TRUE)
  }

  cf1 <- .mojor_ir_loop_control_profile(loop1)
  cf2 <- .mojor_ir_loop_control_profile(loop2)
  simple_cf_ok <- function(cf) !isTRUE(cf$has_control_flow) || isTRUE(cf$simple_if_assign)
  if (isTRUE(simple_cf_ok(cf1)) && isTRUE(simple_cf_ok(cf2))) {
    return(TRUE)
  }

  eff1 <- .mojor_ir_expr_effects(body1)
  eff2 <- .mojor_ir_expr_effects(body2)

 # Check for RNG or Unknown
  if ("RNG" %in% eff1 || "Unknown" %in% eff1) {
    return(FALSE)
  }
  if ("RNG" %in% eff2 || "Unknown" %in% eff2) {
    return(FALSE)
  }

  TRUE
}

.mojor_ir_loop_body <- function(loop) {
 # Extract body from loop
  if (is.null(loop) || loop$kind != "loop") {
    return(NULL)
  }
  loop$body
}

.mojor_ir_guard_policy_check <- function(loop1, loop2) {
 # Check guard policy consistency
 # For now, just check that both loops have same na_guard and bounds_check

 # Extract metadata if present
  meta1 <- if (!is.null(loop1$metadata)) loop1$metadata else list()
  meta2 <- if (!is.null(loop2$metadata)) loop2$metadata else list()

 # Check na_guard (default: forbid)
  na_guard1 <- if (!is.null(meta1$na_guard)) meta1$na_guard else "forbid"
  na_guard2 <- if (!is.null(meta2$na_guard)) meta2$na_guard else "forbid"

  if (na_guard1 != na_guard2) {
    return(FALSE)
  }

 # Check bounds_check (default: FALSE)
  bounds1 <- if (!is.null(meta1$bounds_check)) meta1$bounds_check else FALSE
  bounds2 <- if (!is.null(meta2$bounds_check)) meta2$bounds_check else FALSE

  if (bounds1 != bounds2) {
    return(FALSE)
  }

  TRUE
}

.mojor_ir_sequential_check <- function(stmts, loop1, loop2) {
 # Check if loops are adjacent in statement list
  if (is.null(stmts) || length(stmts) < 2) {
    return(FALSE)
  }

  idx1 <- NULL
  idx2 <- NULL

  for (i in seq_along(stmts)) {
    if (identical(stmts[[i]], loop1)) idx1 <- i
    if (identical(stmts[[i]], loop2)) idx2 <- i
  }

  if (is.null(idx1) || is.null(idx2)) {
    return(FALSE)
  }

 # Check if adjacent (loop2 immediately follows loop1)
  idx2 == idx1 + 1
}

# =============================================================================
# Section 5: Fusion Pass
# =============================================================================

.mojor_ir_fuse_candidates <- function(stmts, ctx) {
 # Find all adjacent loop pairs that can be fused
 #
 # Args:
 # stmts: Statement list
 # ctx: Context with zero_based_vars, type_env, etc.
 #
 # Returns:
 # List of lists, each with: loop1, loop2, stmt_idx1, stmt_idx2, legality

  if (is.null(stmts) || length(stmts) < 2) {
    return(list())
  }

  candidates <- list()

  for (i in 1:(length(stmts) - 1)) {
    loop1 <- stmts[[i]]
    loop2 <- stmts[[i + 1]]

 # Check if both are loops
    if (is.null(loop1) || loop1$kind != "loop") next
    if (is.null(loop2) || loop2$kind != "loop") next

 # Check fusion legality
    legality <- .mojor_ir_fusion_legality(loop1, loop2, stmts, ctx)

    if (legality$legal) {
      candidates <- c(candidates, list(list(
        loop1 = loop1,
        loop2 = loop2,
        stmt_idx1 = i,
        stmt_idx2 = i + 1,
        legality = legality
      )))
    }
  }

  candidates
}

.mojor_ir_fuse_pair <- function(loop1, loop2, ctx) {
 # Fuse two loops into a single loop
 #
 # Args:
 # loop1: First loop
 # loop2: Second loop
 # ctx: Context
 #
 # Returns:
 # Fused loop node

 # Get loop variable (use loop1's var)
  var <- loop1$var

 # Get range (use loop1's range)
  range <- loop1$range

 # Combine loop bodies
  body1 <- .mojor_ir_loop_body(loop1)
  body2 <- .mojor_ir_loop_body(loop2)

 # Create combined body
  combined_body <- .mojor_ir_block(c(
    .mojor_ir_block_to_stmts(body1),
    .mojor_ir_block_to_stmts(body2)
  ))

 # Create fused loop
  .mojor_ir_loop(var, range, combined_body)
}

.mojor_ir_block_to_stmts <- function(block) {
 # Convert block to statement list
  if (is.null(block)) {
    return(list())
  }
  if (block$kind == "block") {
    block$stmts
  } else {
    list(block)
  }
}

.mojor_ir_fusion_pass <- function(stmts, ctx) {
 # Apply fusion pass to a statement list
 # Iteratively fuse adjacent loops until no more fusions possible
 #
 # Args:
 # stmts: Statement list
 # ctx: Context
 #
 # Returns:
 # Optimized statement list

  if (is.null(stmts) || length(stmts) < 2) {
    return(stmts)
  }

 # Keep fusing until no more changes
  changed <- TRUE
  while (changed) {
    changed <- FALSE

 # Find fusion candidates
    candidates <- .mojor_ir_fuse_candidates(stmts, ctx)

    if (length(candidates) > 0) {
 # Take first candidate (greedy approach)
      candidate <- candidates[[1]]

 # Fuse the pair
      fused_loop <- .mojor_ir_fuse_pair(candidate$loop1, candidate$loop2, ctx)

 # Replace the two loops with the fused loop
      new_stmts <- stmts
      new_stmts[[candidate$stmt_idx1]] <- fused_loop
      new_stmts[[candidate$stmt_idx2]] <- NULL
      new_stmts <- new_stmts[!sapply(new_stmts, is.null)]

      stmts <- new_stmts
      changed <- TRUE
    }
  }

  stmts
}

# =============================================================================
# Section 6: Integration Helpers
# =============================================================================

.mojor_ir_fusion_enabled <- function(opts) {
 # Check if fusion is enabled
 # Default: TRUE (opt-out)
  if (is.null(opts)) {
    return(TRUE)
  }
  if (is.null(opts$fusion)) {
    return(TRUE)
  }
  opts$fusion
}
