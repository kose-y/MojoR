# =============================================================================
# SSA Parse/Semantic Helpers
# =============================================================================

.mojor_ssa_parse_error <- function(line_no, message, line_text = NULL) {
    line_desc <- if (!is.null(line_no) &&
        !is.na(line_no))
        paste0(
            " line ", as.integer(line_no),
            ": "
        ) else ": "
    detail <- if (!is.null(line_text))
        paste0("\n  ", line_text) else ""
    stop(paste0("SSA parse error at", line_desc, message, detail))
}

.mojor_ssa_split_csv <- function(text) {
    if (is.null(text)) {
        return(character(0))
    }
    text <- trimws(as.character(text))
    if (!nzchar(text)) {
        return(character(0))
    }
    parts <- strsplit(text, ",", fixed = TRUE)[[1]]
    out <- trimws(parts)
    out[nzchar(out)]
}

.mojor_ssa_parse_params <- function(param_text, line_no) {
    parts <- .mojor_ssa_split_csv(param_text)
    if (length(parts) ==
        0) {
        return(list())
    }
    params <- vector("list", length(parts))
    for (i in seq_along(parts)) {
        part <- parts[[i]]
        m <- regexec(
            "^(%[A-Za-z][A-Za-z0-9_]*)(?:/\\*\\s*([^*]*?)\\s*\\*/)?$",
            part, perl = TRUE
        )
        g <- regmatches(part, m)[[1]]
        if (length(g) ==
            0) {
            .mojor_ssa_parse_error(line_no, paste0("invalid block parameter '", part, "'"))
        }
        var_name <- NULL
        if (length(g) >=
            3 && !is.na(g[[3]]) &&
            nzchar(g[[3]])) {
            var_name <- g[[3]]
        }
        params[[i]] <- list(id = g[[2]], var = var_name)
    }
    params
}

.mojor_ssa_parse_stmt_line <- function(line, line_no) {
    stmt <- trimws(line)
    m_id <- regexec(
        "^(%[A-Za-z][A-Za-z0-9_]*)\\s*=\\s*([^\\s(]+)\\((.*)\\)\\s*$",
        stmt, perl = TRUE
    )
    g_id <- regmatches(stmt, m_id)[[1]]
    if (length(g_id) >
        0) {
        return(
            list(
                kind = "ssa_stmt", id = g_id[[2]], op = g_id[[3]], args = .mojor_ssa_split_csv(g_id[[4]])
            )
        )
    }

    m_eff <- regexec("^([^\\s(]+)\\((.*)\\)\\s*$", stmt, perl = TRUE)
    g_eff <- regmatches(stmt, m_eff)[[1]]
    if (length(g_eff) >
        0) {
        return(
            list(
                kind = "ssa_effect", op = g_eff[[2]], args = .mojor_ssa_split_csv(g_eff[[3]])
            )
        )
    }

    .mojor_ssa_parse_error(line_no, "invalid statement syntax", line)
}

.mojor_ssa_parse_term_line <- function(line, line_no) {
    term <- trimws(line)

    m_cond <- regexec(
        "^condbr\\s+(%[A-Za-z][A-Za-z0-9_]*)\\s*\\?\\s*([A-Za-z_][A-Za-z0-9_\\.]*)\\((.*)\\)\\s*:\\s*([A-Za-z_][A-Za-z0-9_\\.]*)\\((.*)\\)\\s*$",
        term, perl = TRUE
    )
    g_cond <- regmatches(term, m_cond)[[1]]
    if (length(g_cond) >
        0) {
        return(
            list(
                kind = "condbr", cond = g_cond[[2]], then = g_cond[[3]],
                then_args = .mojor_ssa_split_csv(g_cond[[4]]),
                `else` = g_cond[[5]], else_args = .mojor_ssa_split_csv(g_cond[[6]])
            )
        )
    }

    m_br <- regexec(
        "^br\\s+([A-Za-z_][A-Za-z0-9_\\.]*)\\((.*)\\)\\s*$", term, perl = TRUE
    )
    g_br <- regmatches(term, m_br)[[1]]
    if (length(g_br) >
        0) {
        return(
            list(
                kind = "br", target = g_br[[2]], args = .mojor_ssa_split_csv(g_br[[3]])
            )
        )
    }

    m_ret <- regexec("^ret(?:\\s+(%[A-Za-z][A-Za-z0-9_]*))?\\s*$", term, perl = TRUE)
    g_ret <- regmatches(term, m_ret)[[1]]
    if (length(g_ret) >
        0) {
        value <- NULL
        if (length(g_ret) >=
            2 && !is.na(g_ret[[2]]) &&
            nzchar(g_ret[[2]])) {
            value <- g_ret[[2]]
        }
        return(list(kind = "ret", value = value))
    }

    NULL
}

.mojor_ssa_parse_lossless <- function(text) {
    if (!is.character(text) ||
        length(text) !=
            1) {
        stop("SSA parse: text must be a single character string")
    }

    text <- gsub("\r\n", "\n", text, fixed = TRUE)
    raw_lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
    if (length(raw_lines) >
        0 && !nzchar(raw_lines[[length(raw_lines)]])) {
        lines <- raw_lines[-length(raw_lines)]
    } else {
        lines <- raw_lines
    }
    if (length(lines) ==
        0) {
        stop("SSA parse: empty input")
    }

    first <- trimws(lines[[1]])
    m_fn <- regexec(
        "^ssa_fn\\s+entry=([A-Za-z_][A-Za-z0-9_\\.]*)\\s*$", first, perl = TRUE
    )
    g_fn <- regmatches(first, m_fn)[[1]]
    if (length(g_fn) ==
        0) {
        .mojor_ssa_parse_error(1L, "expected 'ssa_fn entry=<block>' header", lines[[1]])
    }
    entry <- g_fn[[2]]

    blocks <- list()
    block_order <- character(0)
    i <- 2L
    n <- length(lines)

    while (i <= n) {
        cur <- trimws(lines[[i]])
        if (!nzchar(cur)) {
            i <- i + 1L
            next
        }

        m_block <- regexec("^([A-Za-z_][A-Za-z0-9_\\.]*)\\((.*)\\):\\s*$", cur, perl = TRUE)
        g_block <- regmatches(cur, m_block)[[1]]
        if (length(g_block) ==
            0) {
            .mojor_ssa_parse_error(i, "expected block header", lines[[i]])
        }

        block_name <- g_block[[2]]
        params <- .mojor_ssa_parse_params(g_block[[3]], i)
        i <- i + 1L

        stmts <- list()
        term <- NULL
        block_end <- i - 1L
        while (i <= n) {
            line_trim <- trimws(lines[[i]])
            if (!nzchar(line_trim)) {
                i <- i + 1L
                next
            }

            m_next_block <- regexec(
                "^([A-Za-z_][A-Za-z0-9_\\.]*)\\((.*)\\):\\s*$", line_trim,
                perl = TRUE
            )
            g_next_block <- regmatches(line_trim, m_next_block)[[1]]
            if (length(g_next_block) >
                0) {
                break
            }

            parsed_term <- .mojor_ssa_parse_term_line(lines[[i]], i)
            if (!is.null(parsed_term)) {
                term <- parsed_term
                block_end <- i
                i <- i + 1L
                while (i <= n) {
                  trail <- trimws(lines[[i]])
                  if (!nzchar(trail)) {
                    i <- i + 1L
                    next
                  }
                  m_after <- regexec(
                    "^([A-Za-z_][A-Za-z0-9_\\.]*)\\((.*)\\):\\s*$", trail,
                    perl = TRUE
                )
                  g_after <- regmatches(trail, m_after)[[1]]
                  if (length(g_after) >
                    0)
                    break
                  .mojor_ssa_parse_error(i, "statement found after terminator", lines[[i]])
                }
                break
            }

            stmts[[length(stmts) +
                1L]] <- .mojor_ssa_parse_stmt_line(lines[[i]], i)
            block_end <- i
            i <- i + 1L
        }

        if (is.null(term)) {
            .mojor_ssa_parse_error(block_end, paste0("block '", block_name, "' missing terminator"))
        }
        if (block_name %in% block_order) {
            .mojor_ssa_parse_error(block_end, paste0("duplicate block name '", block_name, "'"))
        }

        blocks[[block_name]] <- list(
            kind = "raw_ssa_block", name = block_name, params = params,
            stmts = stmts, term = term, trivia = list()
        )
        block_order <- c(block_order, block_name)
    }

    if (!entry %in% block_order) {
        stop(sprintf("SSA parse: entry block '%s' not found", entry))
    }

    list(
        kind = "raw_ssa_fn", entry = entry, blocks = blocks, block_order = block_order,
        src_text = text, parse_metadata = list(normalized = FALSE)
    )
}

.mojor_ssa_print_lossless <- function(tree) {
    if (is.null(tree) ||
        !is.list(tree) ||
        is.null(tree$kind) ||
        !identical(tree$kind, "raw_ssa_fn")) {
        stop(
            "SSA print_lossless: input must be RawSyntaxTree (kind='raw_ssa_fn')"
        )
    }
    if (is.null(tree$src_text) ||
        !is.character(tree$src_text) ||
        length(tree$src_text) !=
            1) {
        stop("SSA print_lossless: RawSyntaxTree missing src_text")
    }
    tree$src_text
}

.mojor_ssa_lower_to_semantic <- function(tree) {
    if (is.null(tree) ||
        !is.list(tree) ||
        is.null(tree$kind)) {
        stop("SSA lower: input must be RawSyntaxTree")
    }
    if (identical(tree$kind, "ssa_fn")) {
        return(tree)
    }
    if (!identical(tree$kind, "raw_ssa_fn")) {
        stop("SSA lower: expected RawSyntaxTree (kind='raw_ssa_fn')")
    }

    blocks <- tree$blocks
    if (is.null(blocks) ||
        !is.list(blocks) ||
        length(blocks) ==
            0) {
        stop("SSA lower: RawSyntaxTree has no blocks")
    }

    sem_blocks <- list()
    block_order <- if (!is.null(tree$block_order) &&
        length(tree$block_order) >
            0)
        tree$block_order else names(blocks)
    for (bn in block_order) {
        blk <- blocks[[bn]]
        if (is.null(blk) ||
            !is.list(blk)) {
            stop(sprintf("SSA lower: missing block '%s'", bn))
        }
        sem_blocks[[bn]] <- list(
            kind = "ssa_block", name = as.character(blk$name),
            params = if (is.null(blk$params)) {
                list()
            } else {
                lapply(
                  blk$params, function(p) {
                    list(
                      id = as.character(p$id),
                      var = if (is.null(p$var)) NULL else as.character(p$var)
                  )
                  }
              )
            }, stmts = if (is.null(blk$stmts)) {
                list()
            } else {
                lapply(
                  blk$stmts, function(st) {
                    out <- list(
                      kind = as.character(st$kind),
                      op = as.character(st$op),
                      args = if (is.null(st$args)) character(0) else as.character(st$args)
                  )
                    if (!is.null(st$id)) out$id <- as.character(st$id)
                    out
                  }
              )
            }, term = blk$term
        )
    }

    sem <- list(
        kind = "ssa_fn", entry = as.character(tree$entry),
        blocks = sem_blocks
    )
    .mojor_ir_verify_ssa(sem)
    sem
}

.mojor_ssa_remap_refs <- function(values, value_map) {
    if (is.null(values) ||
        length(values) ==
            0) {
        return(values)
    }
    out <- as.character(values)
    for (i in seq_along(out)) {
        mapped <- value_map[[out[[i]]]]
        if (!is.null(mapped))
            out[[i]] <- mapped
    }
    out
}

.mojor_ssa_canonical_renumber <- function(ssa) {
    if (is.null(ssa$blocks) ||
        !is.list(ssa$blocks) ||
        length(ssa$blocks) ==
            0) {
        return(ssa)
    }

    value_map <- list()
    arg_idx <- 0L
    val_idx <- 0L
    block_names <- names(ssa$blocks)

    for (bn in block_names) {
        blk <- ssa$blocks[[bn]]
        if (!is.null(blk$params) &&
            length(blk$params) >
                0) {
            for (p in blk$params) {
                pid <- if (is.null(p$id))
                  NULL else as.character(p$id)
                if (!is.null(pid) &&
                  .mojor_ssa_is_value_ref(pid)) {
                  arg_idx <- arg_idx + 1L
                  value_map[[pid]] <- sprintf("%%arg%d", arg_idx)
                }
            }
        }
        if (!is.null(blk$stmts) &&
            length(blk$stmts) >
                0) {
            for (st in blk$stmts) {
                sid <- if (is.null(st$id))
                  NULL else as.character(st$id)
                if (!is.null(sid) &&
                  .mojor_ssa_is_value_ref(sid)) {
                  val_idx <- val_idx + 1L
                  value_map[[sid]] <- sprintf("%%v%d", val_idx)
                }
            }
        }
    }

    out <- ssa
    for (bn in block_names) {
        blk <- out$blocks[[bn]]
        if (!is.null(blk$params) &&
            length(blk$params) >
                0) {
            for (i in seq_along(blk$params)) {
                pid <- if (is.null(blk$params[[i]]$id))
                  NULL else as.character(blk$params[[i]]$id)
                if (!is.null(pid)) {
                  mapped <- value_map[[pid]]
                  if (!is.null(mapped))
                    blk$params[[i]]$id <- mapped
                }
            }
        }

        if (!is.null(blk$stmts) &&
            length(blk$stmts) >
                0) {
            for (i in seq_along(blk$stmts)) {
                st <- blk$stmts[[i]]
                if (!is.null(st$id)) {
                  mapped_id <- value_map[[as.character(st$id)]]
                  if (!is.null(mapped_id))
                    st$id <- mapped_id
                }
                if (!is.null(st$args)) {
                  st$args <- .mojor_ssa_remap_refs(st$args, value_map)
                }
                blk$stmts[[i]] <- st
            }
        }

        term <- blk$term
        if (!is.null(term)) {
            if (identical(term$kind, "ret") &&
                !is.null(term$value)) {
                mapped <- value_map[[as.character(term$value)]]
                if (!is.null(mapped))
                  term$value <- mapped
            } else if (identical(term$kind, "br")) {
                term$args <- .mojor_ssa_remap_refs(term$args, value_map)
            } else if (identical(term$kind, "condbr")) {
                mapped_cond <- value_map[[as.character(term$cond)]]
                if (!is.null(mapped_cond))
                  term$cond <- mapped_cond
                term$then_args <- .mojor_ssa_remap_refs(term$then_args, value_map)
                term$else_args <- .mojor_ssa_remap_refs(term$else_args, value_map)
            }
            blk$term <- term
        }

        out$blocks[[bn]] <- blk
    }

    out
}

.mojor_ssa_canonicalize <- function(ir, strict_schema = NULL) {
    if (is.null(ir) ||
        !is.list(ir) ||
        is.null(ir$kind)) {
        stop("SSA canonicalize: input must be SemanticIR (ssa_fn)")
    }
    if (!identical(ir$kind, "ssa_fn")) {
        stop(
            "SSA canonicalize: input must be SemanticIR (ssa_fn), not RawSyntaxTree"
        )
    }

    strict_enabled <- .mojor_ssa_schema_strict_enabled(strict_schema)
    .mojor_ir_verify_ssa(ir, strict_schema = strict_schema)
    out <- .mojor_ir_ssa_prune_unreachable(ir)
    if (isTRUE(strict_enabled)) {
        out <- .mojor_ir_ssa_canonicalize_ops(out)
    }
    out <- .mojor_ssa_canonical_renumber(out)
    .mojor_ir_verify_ssa(out, strict_schema = strict_schema)
    out
}

.mojor_ssa_memory_canonicalize_stmt <- function(st) {
    if (is.null(st) ||
        !is.list(st) ||
        is.null(st$op)) {
        return(st)
    }
    op <- as.character(st$op)
    op_can <- .mojor_ssa_op_canonical(op)

    if (identical(op, "index") ||
        identical(op_can, "mojor.mem.index")) {
        st$op <- "mojor.mem.load"
        return(st)
    }

    if (!is.null(st$kind) &&
        identical(st$kind, "ssa_effect")) {
        if (identical(op, "store_index") ||
            identical(op_can, "mojor.mem.store_index")) {
            st$op <- "mojor.mem.store"
            st$legacy_memory_form <- "store_index"
            return(st)
        }

        if (identical(op, "store_subscript") ||
            identical(op_can, "mojor.mem.store_subscript")) {
            st$op <- "mojor.mem.store"
            st$legacy_memory_form <- "store_subscript"
            return(st)
        }

        if (startsWith(op, "store:") ||
            (startsWith(op_can, "mojor.mem.store.") &&
                !identical(op_can, "mojor.mem.store"))) {
            suffix <- if (startsWith(op, "store:")) {
                sub("^store:", "", op)
            } else {
                sub("^mojor\\.mem\\.store\\.", "", op_can)
            }
            st$op <- "mojor.mem.store"
            st$legacy_memory_form <- "store_surface"
            st$legacy_store_suffix <- as.character(suffix)
            return(st)
        }
    }

    st
}

.mojor_ssa_memory_canonicalize <- function(ir, verify = TRUE, strict_schema = NULL) {
    ir <- .mojor_ssa_require_semantic_ir(ir, where = "SSA memory_canonicalize")
    if (isTRUE(verify))
        .mojor_ir_verify_ssa(ir, strict_schema = strict_schema)

    out <- ir
    for (bn in names(out$blocks)) {
        blk <- out$blocks[[bn]]
        if (is.null(blk$stmts) ||
            length(blk$stmts) ==
                0)
            next
        for (i in seq_along(blk$stmts)) {
            blk$stmts[[i]] <- .mojor_ssa_memory_canonicalize_stmt(blk$stmts[[i]])
        }
        out$blocks[[bn]] <- blk
    }

    if (isTRUE(verify))
        .mojor_ir_verify_ssa(out, strict_schema = strict_schema)
    out
}

.mojor_ssa_print_semantic <- function(ir, mode = c("pretty", "compat")) {
    mode <- match.arg(mode)
    if (is.null(ir) ||
        !is.list(ir) ||
        is.null(ir$kind) ||
        !identical(ir$kind, "ssa_fn")) {
        stop("SSA print_semantic: input must be SemanticIR (ssa_fn)")
    }
    if (identical(mode, "compat")) {
        return(.mojor_ir_ssa_format(ir))
    }
    can <- .mojor_ssa_canonicalize(ir)
    can_mem <- .mojor_ssa_memory_canonicalize(can, verify = FALSE)
    .mojor_ir_ssa_format(can_mem)
}

.mojor_ssa_parse_norm <- function(text) {
    raw <- .mojor_ssa_parse_lossless(text)
    sem <- .mojor_ssa_lower_to_semantic(raw)
    pretty <- .mojor_ssa_print_semantic(sem, mode = "pretty")
    norm <- .mojor_ssa_parse_lossless(pretty)
    norm$parse_metadata$normalized <- TRUE
    norm
}

.mojor_ssa_semantic_eq <- function(ir1, ir2) {
    can1 <- tryCatch(
        .mojor_ssa_memory_canonicalize(
            .mojor_ssa_canonicalize(ir1),
            verify = FALSE
        ),
        error = function(e) NULL
    )
    can2 <- tryCatch(
        .mojor_ssa_memory_canonicalize(
            .mojor_ssa_canonicalize(ir2),
            verify = FALSE
        ),
        error = function(e) NULL
    )
    if (is.null(can1) ||
        is.null(can2)) {
        return(FALSE)
    }
    txt1 <- .mojor_ssa_print_semantic(can1, mode = "pretty")
    txt2 <- .mojor_ssa_print_semantic(can2, mode = "pretty")
    identical(txt1, txt2)
}

.mojor_ir_ssa_format <- function(ssa, indent = "", as_lines = FALSE) {
    if (is.null(ssa) ||
        !is.list(ssa) ||
        is.null(ssa$kind) ||
        ssa$kind != "ssa_fn") {
        line <- paste0(indent, "<invalid ssa>")
        if (isTRUE(as_lines)) {
            return(line)
        }
        return(paste(line, collapse = "\n"))
    }
    lines <- c(paste0(indent, "ssa_fn entry=", ssa$entry))
    for (bn in names(ssa$blocks)) {
        blk <- ssa$blocks[[bn]]
        params <- character(0)
        if (!is.null(blk$params) &&
            length(blk$params) >
                0) {
            params <- vapply(
                blk$params, function(p) {
                  if (!is.null(p$var) &&
                    nzchar(p$var)) {
                    paste0(p$id, "/*", p$var, "*/")
                  } else {
                    p$id
                  }
                }, character(1)
            )
        }
        lines <- c(
            lines, paste0(
                indent, bn, "(", paste(params, collapse = ", "),
                "):"
            )
        )

        if (!is.null(blk$stmts) &&
            length(blk$stmts) >
                0) {
            for (st in blk$stmts) {
                if (is.null(st$id)) {
                  args <- if (is.null(st$args) ||
                    length(st$args) ==
                      0)
                    "" else paste(st$args, collapse = ", ")
                  lines <- c(lines, paste0(indent, "  ", st$op, "(", args, ")"))
                } else {
                  args <- if (is.null(st$args) ||
                    length(st$args) ==
                      0)
                    "" else paste(st$args, collapse = ", ")
                  lines <- c(lines, paste0(indent, "  ", st$id, " = ", st$op, "(", args, ")"))
                }
            }
        }

        term <- blk$term
        if (!is.null(term)) {
            term_line <- switch(
                term$kind, br = {
                  args <- if (is.null(term$args) ||
                    length(term$args) ==
                      0) "" else paste(term$args, collapse = ", ")
                  paste0("br ", term$target, "(", args, ")")
                }, condbr = {
                  ta <- if (is.null(term$then_args) ||
                    length(term$then_args) ==
                      0) "" else paste(term$then_args, collapse = ", ")
                  ea <- if (is.null(term$else_args) ||
                    length(term$else_args) ==
                      0) "" else paste(term$else_args, collapse = ", ")
                  paste0(
                    "condbr ", term$cond, " ? ", term$then, "(", ta, ") : ",
                    term[["else"]], "(", ea, ")"
                )
                }, ret = {
                  if (is.null(term$value)) "ret" else paste0("ret ", term$value)
                }, paste0("<term ", term$kind, ">")
            )
            lines <- c(lines, paste0(indent, "  ", term_line))
        }
    }

    if (isTRUE(as_lines)) {
        return(lines)
    }
    paste(lines, collapse = "\n")
}

.mojor_ir_ssa_dump <- function(expr_or_fn) {
    ir <- .mojor_ir_dump(expr_or_fn)
    ir <- .mojor_ir_normalize(ir)
    if (exists(".mojor_ir_structured_lower_to_ssa", mode = "function")) {
        ssa <- .mojor_ir_structured_lower_to_ssa(ir, verify_ir = FALSE, attach_loop_metadata = TRUE)
    } else {
        ssa <- .mojor_ir_to_ssa(ir)
    }
    list(ir = ir, ssa = ssa, formatted = .mojor_ir_ssa_format(ssa))
}
