# =============================================================================
# Stage 5a: Structured IR Node Helpers
# =============================================================================

.mojor_ir_base_kind <- function(kind) {
  if (is.null(kind) || !is.character(kind) || length(kind) != 1) {
    return(kind)
  }
  switch(as.character(kind),
    s_if = "if",
    s_for = "loop",
    s_while = "while",
    s_repeat = "repeat",
    as.character(kind)
  )
}

.mojor_ir_is_structured_kind <- function(kind) {
  if (is.null(kind) || !is.character(kind) || length(kind) != 1) {
    return(FALSE)
  }
  as.character(kind) %in% c("s_if", "s_for", "s_while", "s_repeat")
}

.mojor_ir_s_if <- function(cond, then, else_block = NULL, attrs = NULL, src = NULL) {
  node <- list(kind = "s_if", cond = cond, then = then, else_block = else_block, src = src)
  if (!is.null(attrs)) node$attrs <- attrs
  node
}

.mojor_ir_s_for <- function(var, range, body, loop_id = NULL, attrs = NULL, src = NULL) {
  node <- list(
    kind = "s_for",
    var = var,
    range = range,
    body = body,
    metadata = NULL,
    src = src
  )
  if (!is.null(loop_id)) node$loop_id <- as.character(loop_id)
  if (!is.null(attrs)) node$attrs <- attrs
  node
}

.mojor_ir_s_while <- function(cond, body, loop_id = NULL, attrs = NULL, src = NULL) {
  node <- list(kind = "s_while", cond = cond, body = body, src = src)
  if (!is.null(loop_id)) node$loop_id <- as.character(loop_id)
  if (!is.null(attrs)) node$attrs <- attrs
  node
}

.mojor_ir_s_repeat <- function(body, loop_id = NULL, attrs = NULL, src = NULL) {
  node <- list(kind = "s_repeat", body = body, src = src)
  if (!is.null(loop_id)) node$loop_id <- as.character(loop_id)
  if (!is.null(attrs)) node$attrs <- attrs
  node
}

.mojor_ir_structured_promote <- function(node, assign_loop_ids = TRUE) {
  st <- new.env(parent = emptyenv())
  st$for_n <- 0L
  st$while_n <- 0L
  st$repeat_n <- 0L

  next_loop_id <- function(prefix) {
    if (identical(prefix, "for")) {
      st$for_n <- st$for_n + 1L
      return(paste0("for_", st$for_n))
    }
    if (identical(prefix, "while")) {
      st$while_n <- st$while_n + 1L
      return(paste0("while_", st$while_n))
    }
    st$repeat_n <- st$repeat_n + 1L
    paste0("repeat_", st$repeat_n)
  }

  rec <- function(n) {
    if (is.null(n) || !is.list(n) || is.null(n$kind)) {
      return(n)
    }
    raw_kind <- as.character(n$kind)
    kind <- .mojor_ir_base_kind(raw_kind)

    if (identical(kind, "block") && !is.null(n$stmts) && is.list(n$stmts)) {
      n$stmts <- lapply(n$stmts, rec)
      return(n)
    }

    if (identical(kind, "if")) {
      n$kind <- "s_if"
      n$cond <- rec(n$cond)
      n$then <- rec(n$then)
      if (!is.null(n$else_block)) n$else_block <- rec(n$else_block)
      return(n)
    }

    if (identical(kind, "loop")) {
      n$kind <- "s_for"
      n$range <- rec(n$range)
      n$body <- rec(n$body)
      if (isTRUE(assign_loop_ids)) {
        if (is.null(n$loop_id) || !nzchar(as.character(n$loop_id))) {
          n$loop_id <- next_loop_id("for")
        } else {
          n$loop_id <- as.character(n$loop_id)
        }
      }
      return(n)
    }

    if (identical(kind, "while")) {
      n$kind <- "s_while"
      n$cond <- rec(n$cond)
      n$body <- rec(n$body)
      if (isTRUE(assign_loop_ids)) {
        if (is.null(n$loop_id) || !nzchar(as.character(n$loop_id))) {
          n$loop_id <- next_loop_id("while")
        } else {
          n$loop_id <- as.character(n$loop_id)
        }
      }
      return(n)
    }

    if (identical(kind, "repeat")) {
      n$kind <- "s_repeat"
      n$body <- rec(n$body)
      if (isTRUE(assign_loop_ids)) {
        if (is.null(n$loop_id) || !nzchar(as.character(n$loop_id))) {
          n$loop_id <- next_loop_id("repeat")
        } else {
          n$loop_id <- as.character(n$loop_id)
        }
      }
      return(n)
    }

 # Generic recursion for expression-like nodes that carry nested IR children.
    child_fields <- c("lhs", "rhs", "expr", "base", "start", "end", "step", "cond", "yes", "no", "value")
    for (f in child_fields) {
      if (!is.null(n[[f]]) && is.list(n[[f]]) && !is.null(n[[f]]$kind)) {
        n[[f]] <- rec(n[[f]])
      }
    }
    list_fields <- c("args", "indices", "parts", "stmts")
    for (f in list_fields) {
      if (!is.null(n[[f]]) && is.list(n[[f]]) && length(n[[f]]) > 0) {
        n[[f]] <- lapply(n[[f]], rec)
      }
    }

    n
  }

  rec(node)
}
