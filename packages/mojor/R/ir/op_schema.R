# =============================================================================
# SSA Op Canonical Namespaces / Schema / Semantic Hash (Stage 1 seed)
# =============================================================================

.mojor_ir_op_compat_map <- function() {
    c(
        const = "mojor.arith.const", `const:null` = "mojor.misc.const_null",
        select = "mojor.arith.select", load = "mojor.mem.load", index_cast = "mojor.arith.index_cast",
        index = "mojor.mem.index", idx_missing = "mojor.misc.idx_missing",
        idx_slice = "mojor.misc.idx_slice", loop_cond = "mojor.cf.loop_cond",
        loop_next = "mojor.cf.loop_next", store_index = "mojor.mem.store_index",
        store_subscript = "mojor.mem.store_subscript", scheduled_reduce = "mojor.misc.stmt.scheduled_reduce",
        scalar_reduce = "mojor.misc.stmt.scalar_reduce", s_if = "mojor.misc.stmt.if",
        s_for = "mojor.misc.stmt.for", s_while = "mojor.misc.stmt.while",
        s_repeat = "mojor.misc.stmt.repeat"
    )
}

.mojor_ir_op_canonicalize <- function(op) {
    op <- as.character(op)
    compat <- .mojor_ir_op_compat_map()
    mapped <- unname(compat[op])
    if (length(mapped) ==
        1 && !is.na(mapped) &&
        nzchar(mapped)) {
        return(mapped)
    }

    if (startsWith(op, "binop:")) {
        suffix <- substring(
            op, nchar("binop:") +
                1L
        )
        bin_map <- c(
            `+` = "add", `-` = "sub", `*` = "mul", `/` = "div", `%%` = "mod",
            `%/%` = "idiv", `<` = "cmp_lt", `>` = "cmp_gt", `<=` = "cmp_le",
            `>=` = "cmp_ge", `==` = "cmp_eq", `!=` = "cmp_ne", `&&` = "and",
            `||` = "or", `&` = "bit_and", `|` = "bit_or", `^` = "xor"
        )
        mapped_bin <- bin_map[[suffix]]
        if (!is.null(mapped_bin)) {
            return(paste0("mojor.arith.", mapped_bin))
        }
        return(paste0("mojor.arith.binop.", gsub("[^A-Za-z0-9_]+", "_", suffix)))
    }

    if (startsWith(op, "unop:")) {
        suffix <- substring(
            op, nchar("unop:") +
                1L
        )
        un_map <- c(`-` = "neg", `+` = "pos", `!` = "not")
        mapped_un <- un_map[[suffix]]
        if (!is.null(mapped_un)) {
            return(paste0("mojor.arith.", mapped_un))
        }
        return(paste0("mojor.arith.unop.", gsub("[^A-Za-z0-9_]+", "_", suffix)))
    }

    if (identical(op, "cmpi")) {
        return("mojor.arith.cmpi")
    }
    if (identical(op, "cmpf")) {
        return("mojor.arith.cmpf")
    }
    if (startsWith(op, "cmpi:")) {
        suffix <- substring(
            op, nchar("cmpi:") +
                1L
        )
        return(paste0("mojor.arith.cmpi.", gsub("[^A-Za-z0-9_]+", "_", suffix)))
    }
    if (startsWith(op, "cmpf:")) {
        suffix <- substring(
            op, nchar("cmpf:") +
                1L
        )
        return(paste0("mojor.arith.cmpf.", gsub("[^A-Za-z0-9_]+", "_", suffix)))
    }

    if (identical(op, "index_cast")) {
        return("mojor.arith.index_cast")
    }
    if (startsWith(op, "index_cast:")) {
        suffix <- substring(
            op, nchar("index_cast:") +
                1L
        )
        if (!nzchar(suffix)) {
            return("mojor.arith.index_cast")
        }
        return(
            paste0("mojor.arith.index_cast.", gsub("[^A-Za-z0-9_]+", "_", suffix))
        )
    }

    if (startsWith(op, "cast:")) {
        suffix <- substring(
            op, nchar("cast:") +
                1L
        )
        return(paste0("mojor.arith.cast.", gsub("[^A-Za-z0-9_]+", "_", suffix)))
    }

    if (startsWith(op, "call:")) {
        suffix <- substring(
            op, nchar("call:") +
                1L
        )
        return(paste0("mojor.misc.call.", gsub("[^A-Za-z0-9_]+", "_", suffix)))
    }

    if (startsWith(op, "store:")) {
        suffix <- substring(
            op, nchar("store:") +
                1L
        )
        return(paste0("mojor.mem.store.", gsub("[^A-Za-z0-9_]+", "_", suffix)))
    }

    if (startsWith(op, "load:")) {
        suffix <- substring(
            op, nchar("load:") +
                1L
        )
        return(paste0("mojor.mem.load.", gsub("[^A-Za-z0-9_]+", "_", suffix)))
    }

    if (startsWith(op, "stmt:")) {
        suffix <- substring(
            op, nchar("stmt:") +
                1L
        )
        return(paste0("mojor.misc.stmt.", gsub("[^A-Za-z0-9_]+", "_", suffix)))
    }

    op
}

.mojor_ir_op_schema_table <- function() {
    list(
        mojor.arith.const = list(
            namespace = "mojor.arith", interfaces = c("Pure"),
            min_args = 0L, max_args = 0L
        ),
        mojor.misc.const_null = list(
            namespace = "mojor.misc", interfaces = c("Pure"),
            min_args = 0L, max_args = 0L
        ),
        mojor.arith.select = list(
            namespace = "mojor.arith", interfaces = c("Pure"),
            min_args = 3L, max_args = 3L
        ),
        mojor.mem.load = list(
            namespace = "mojor.mem", interfaces = c("ReadsMem"),
            min_args = 2L, max_args = Inf
        ),
        mojor.mem.store = list(
            namespace = "mojor.mem", interfaces = c("WritesMem"),
            min_args = 3L, max_args = Inf
        ),
        mojor.mem.index = list(
            namespace = "mojor.mem", interfaces = c("ReadsMem"),
            min_args = 2L, max_args = Inf
        ),
        mojor.misc.idx_missing = list(
            namespace = "mojor.misc", interfaces = c("Pure"),
            min_args = 0L, max_args = 0L
        ),
        mojor.misc.idx_slice = list(
            namespace = "mojor.misc", interfaces = c("Pure"),
            min_args = 2L, max_args = 2L
        ),
        mojor.cf.loop_cond = list(
            namespace = "mojor.cf", interfaces = c("Pure"),
            min_args = 2L, max_args = 2L
        ),
        mojor.cf.loop_next = list(
            namespace = "mojor.cf", interfaces = c("Pure"),
            min_args = 2L, max_args = 2L
        ),
        mojor.mem.store_index = list(
            namespace = "mojor.mem", interfaces = c("WritesMem"),
            min_args = 3L, max_args = Inf
        ),
        mojor.mem.store_subscript = list(
            namespace = "mojor.mem", interfaces = c("WritesMem"),
            min_args = 3L, max_args = Inf
        ),
        mojor.arith.index_cast = list(
            namespace = "mojor.arith", interfaces = c("Pure"),
            min_args = 1L, max_args = 1L
        ),
        mojor.arith.cmpi = list(
            namespace = "mojor.arith", interfaces = c("Pure"),
            min_args = 2L, max_args = 2L
        ),
        mojor.arith.cmpf = list(
            namespace = "mojor.arith", interfaces = c("Pure"),
            min_args = 2L, max_args = 2L
        ),
        mojor.arith.add = list(
            namespace = "mojor.arith", interfaces = c("Pure"),
            min_args = 2L, max_args = 2L
        ),
        mojor.arith.sub = list(
            namespace = "mojor.arith", interfaces = c("Pure"),
            min_args = 2L, max_args = 2L
        ),
        mojor.arith.mul = list(
            namespace = "mojor.arith", interfaces = c("Pure"),
            min_args = 2L, max_args = 2L
        ),
        mojor.arith.div = list(
            namespace = "mojor.arith", interfaces = c("Pure"),
            min_args = 2L, max_args = 2L
        ),
        mojor.arith.mod = list(
            namespace = "mojor.arith", interfaces = c("Pure"),
            min_args = 2L, max_args = 2L
        ),
        mojor.arith.idiv = list(
            namespace = "mojor.arith", interfaces = c("Pure"),
            min_args = 2L, max_args = 2L
        ),
        mojor.arith.cmp_lt = list(
            namespace = "mojor.arith", interfaces = c("Pure"),
            min_args = 2L, max_args = 2L
        ),
        mojor.arith.cmp_gt = list(
            namespace = "mojor.arith", interfaces = c("Pure"),
            min_args = 2L, max_args = 2L
        ),
        mojor.arith.cmp_le = list(
            namespace = "mojor.arith", interfaces = c("Pure"),
            min_args = 2L, max_args = 2L
        ),
        mojor.arith.cmp_ge = list(
            namespace = "mojor.arith", interfaces = c("Pure"),
            min_args = 2L, max_args = 2L
        ),
        mojor.arith.cmp_eq = list(
            namespace = "mojor.arith", interfaces = c("Pure"),
            min_args = 2L, max_args = 2L
        ),
        mojor.arith.cmp_ne = list(
            namespace = "mojor.arith", interfaces = c("Pure"),
            min_args = 2L, max_args = 2L
        ),
        mojor.arith.and = list(
            namespace = "mojor.arith", interfaces = c("Pure"),
            min_args = 2L, max_args = 2L
        ),
        mojor.arith.or = list(
            namespace = "mojor.arith", interfaces = c("Pure"),
            min_args = 2L, max_args = 2L
        ),
        mojor.arith.bit_and = list(
            namespace = "mojor.arith", interfaces = c("Pure"),
            min_args = 2L, max_args = 2L
        ),
        mojor.arith.bit_or = list(
            namespace = "mojor.arith", interfaces = c("Pure"),
            min_args = 2L, max_args = 2L
        ),
        mojor.arith.xor = list(
            namespace = "mojor.arith", interfaces = c("Pure"),
            min_args = 2L, max_args = 2L
        ),
        mojor.arith.neg = list(
            namespace = "mojor.arith", interfaces = c("Pure"),
            min_args = 1L, max_args = 1L
        ),
        mojor.arith.pos = list(
            namespace = "mojor.arith", interfaces = c("Pure"),
            min_args = 1L, max_args = 1L
        ),
        mojor.arith.not = list(
            namespace = "mojor.arith", interfaces = c("Pure"),
            min_args = 1L, max_args = 1L
        ),
        mojor.misc.stmt.break = list(
            namespace = "mojor.misc", interfaces = c("Effect"),
            min_args = 0L, max_args = 0L
        ),
        mojor.misc.stmt.next = list(
            namespace = "mojor.misc", interfaces = c("Effect"),
            min_args = 0L, max_args = 0L
        ),
        mojor.misc.stmt.raw = list(
            namespace = "mojor.misc", interfaces = c("Effect"),
            min_args = 0L, max_args = Inf
        ),
        mojor.misc.stmt.if = list(
            namespace = "mojor.misc", interfaces = c("Effect"),
            min_args = 0L, max_args = 0L
        ),
        mojor.misc.stmt.for = list(
            namespace = "mojor.misc", interfaces = c("Effect"),
            min_args = 0L, max_args = 0L
        ),
        mojor.misc.stmt.while = list(
            namespace = "mojor.misc", interfaces = c("Effect"),
            min_args = 0L, max_args = 0L
        ),
        mojor.misc.stmt.repeat = list(
            namespace = "mojor.misc", interfaces = c("Effect"),
            min_args = 0L, max_args = 0L
        )
    )
}

.mojor_ir_op_schema_patterns <- function() {
    list(
        list(
            pattern = "^mojor\\.arith\\.index_cast\\.", namespace = "mojor.arith",
            interfaces = c("Pure"),
            min_args = 1L, max_args = 1L
        ),
        list(
            pattern = "^mojor\\.arith\\.cmpi\\.", namespace = "mojor.arith",
            interfaces = c("Pure"),
            min_args = 2L, max_args = 2L
        ),
        list(
            pattern = "^mojor\\.arith\\.cmpf\\.", namespace = "mojor.arith",
            interfaces = c("Pure"),
            min_args = 2L, max_args = 2L
        ),
        list(
            pattern = "^mojor\\.arith\\.cast\\.", namespace = "mojor.arith",
            interfaces = c("Pure"),
            min_args = 1L, max_args = 1L
        ),
        list(
            pattern = "^mojor\\.misc\\.call\\.", namespace = "mojor.misc",
            interfaces = c("Call"),
            min_args = 0L, max_args = Inf
        ),
        list(
            pattern = "^mojor\\.mem\\.load\\.", namespace = "mojor.mem",
            interfaces = c("ReadsMem"),
            min_args = 2L, max_args = Inf
        ),
        list(
            pattern = "^mojor\\.mem\\.store\\.", namespace = "mojor.mem",
            interfaces = c("WritesMem"),
            min_args = 3L, max_args = Inf
        ),
        list(
            pattern = "^mojor\\.misc\\.stmt\\.", namespace = "mojor.misc",
            interfaces = c("Effect"),
            min_args = 0L, max_args = Inf
        ),
        list(
            pattern = "^mojor\\.arith\\.binop\\.", namespace = "mojor.arith",
            interfaces = c("Pure"),
            min_args = 2L, max_args = 2L
        ),
        list(
            pattern = "^mojor\\.arith\\.unop\\.", namespace = "mojor.arith",
            interfaces = c("Pure"),
            min_args = 1L, max_args = 1L
        )
    )
}

.mojor_ir_op_schema_get <- function(op, canonicalize = TRUE) {
    op <- as.character(op)
    can <- if (isTRUE(canonicalize))
        .mojor_ir_op_canonicalize(op) else op
    table <- .mojor_ir_op_schema_table()
    exact <- table[[can]]
    if (!is.null(exact)) {
        exact$op <- can
        return(exact)
    }
    pats <- .mojor_ir_op_schema_patterns()
    for (p in pats) {
        if (grepl(p$pattern, can, perl = TRUE)) {
            out <- p
            out$op <- can
            return(out)
        }
    }
    NULL
}

.mojor_ir_ssa_canonicalize_ops <- function(ssa) {
    ssa <- .mojor_ssa_require_semantic_ir(ssa, where = "SSA canonicalize_ops")
    out <- ssa
    for (bn in names(out$blocks)) {
        blk <- out$blocks[[bn]]
        if (!is.null(blk$stmts) &&
            length(blk$stmts) >
                0) {
            for (i in seq_along(blk$stmts)) {
                st <- blk$stmts[[i]]
                if (!is.null(st$op)) {
                  st$op <- .mojor_ir_op_canonicalize(st$op)
                }
                blk$stmts[[i]] <- st
            }
            out$blocks[[bn]] <- blk
        }
    }
    out
}

.mojor_ir_validate_op_schema <- function(ssa, strict = FALSE, canonicalize = TRUE) {
    ssa <- .mojor_ssa_require_semantic_ir(ssa, where = "SSA validate_op_schema")
    diagnostics <- list()
    .has_iface <- function(ifaces, target) {
        any(
            as.character(ifaces) ==
                target
        )
    }
    .starts_with_namespace <- function(op, namespace) {
        op <- as.character(op)
        namespace <- as.character(namespace)
        startsWith(op, paste0(namespace, "."))
    }

    for (bn in names(ssa$blocks)) {
        blk <- ssa$blocks[[bn]]
        if (is.null(blk$stmts) ||
            length(blk$stmts) ==
                0)
            next
        for (idx in seq_along(blk$stmts)) {
            st <- blk$stmts[[idx]]
            if (is.null(st$op))
                next
            op <- as.character(st$op)
            stmt_kind <- if (is.null(st$kind))
                "" else as.character(st$kind)
            can <- if (isTRUE(canonicalize))
                .mojor_ir_op_canonicalize(op) else op
            schema <- .mojor_ir_op_schema_get(can, canonicalize = FALSE)
            argn <- if (is.null(st$args))
                0L else length(st$args)
            if (is.null(schema)) {
                diagnostics[[length(diagnostics) +
                  1L]] <- list(
                  code = "OP_SCHEMA_UNKNOWN", severity = "error", block = bn,
                  stmt_index = as.integer(idx),
                  op = op, canonical_op = can, message = sprintf(
                    "Unknown op schema for '%s' (canonical '%s')", op,
                    can
                )
              )
                next
            }
            if (!is.null(schema$namespace) &&
                !.starts_with_namespace(can, schema$namespace)) {
                diagnostics[[length(diagnostics) +
                  1L]] <- list(
                  code = "OP_SCHEMA_NAMESPACE", severity = "error", block = bn,
                  stmt_index = as.integer(idx),
                  op = op, canonical_op = can, message = sprintf(
                    "Op '%s' does not match schema namespace '%s'", can,
                    as.character(schema$namespace)
                )
              )
            }
            min_args <- if (is.null(schema$min_args))
                0L else as.integer(schema$min_args)
            max_args <- if (is.null(schema$max_args))
                Inf else schema$max_args
            if (argn < min_args || (!is.infinite(max_args) &&
                argn > as.integer(max_args))) {
                diagnostics[[length(diagnostics) +
                  1L]] <- list(
                  code = "OP_SCHEMA_ARITY", severity = "error", block = bn,
                  stmt_index = as.integer(idx),
                  op = op, canonical_op = can, message = sprintf(
                    "Op '%s' has arity %d (expected %s..%s)", can, argn,
                    min_args, if (is.infinite(max_args)) "Inf" else as.character(max_args)
                )
              )
            }

            if (identical(stmt_kind, "ssa_stmt")) {
                if (is.null(st$id) ||
                  !is.character(st$id) ||
                  length(st$id) !=
                    1L || !nzchar(st$id)) {
                  diagnostics[[length(diagnostics) +
                    1L]] <- list(
                    code = "OP_SCHEMA_ID_REQUIRED", severity = "error",
                    block = bn, stmt_index = as.integer(idx),
                    op = op, canonical_op = can, message = sprintf("SSA stmt for op '%s' requires a non-empty id", can)
                )
                }
                if (.has_iface(schema$interfaces, "WritesMem") ||
                  .has_iface(schema$interfaces, "Effect")) {
                  diagnostics[[length(diagnostics) +
                    1L]] <- list(
                    code = "OP_SCHEMA_KIND_MISMATCH", severity = "error",
                    block = bn, stmt_index = as.integer(idx),
                    op = op, canonical_op = can, message = sprintf(
                      "Op '%s' has side-effect interfaces and must be emitted as ssa_effect",
                      can
                  )
                )
                }
            }

            if (identical(stmt_kind, "ssa_effect")) {
                if (.has_iface(schema$interfaces, "Pure")) {
                  diagnostics[[length(diagnostics) +
                    1L]] <- list(
                    code = "OP_SCHEMA_KIND_MISMATCH", severity = "error",
                    block = bn, stmt_index = as.integer(idx),
                    op = op, canonical_op = can, message = sprintf(
                      "Op '%s' has Pure interface and must be emitted as ssa_stmt",
                      can
                  )
                )
                }
                if (.has_iface(schema$interfaces, "Call")) {
                  diagnostics[[length(diagnostics) +
                    1L]] <- list(
                    code = "OP_SCHEMA_KIND_MISMATCH", severity = "error",
                    block = bn, stmt_index = as.integer(idx),
                    op = op, canonical_op = can, message = sprintf(
                      "Op '%s' has Call interface and must be emitted as ssa_stmt",
                      can
                  )
                )
                }
            }

            if (!identical(stmt_kind, "ssa_stmt") &&
                !identical(stmt_kind, "ssa_effect")) {
                diagnostics[[length(diagnostics) +
                  1L]] <- list(
                  code = "OP_SCHEMA_STMT_KIND_UNKNOWN", severity = "error",
                  block = bn, stmt_index = as.integer(idx),
                  op = op, canonical_op = can, message = sprintf(
                    "Op '%s' emitted with unsupported stmt kind '%s'",
                    can, stmt_kind
                )
              )
            }
        }
    }

    ok <- length(diagnostics) ==
        0
    if (!ok && isTRUE(strict)) {
        first <- diagnostics[[1]]
        stop(
            sprintf("%s: %s (block '%s')", first$code, first$message, first$block)
        )
    }
    list(ok = ok, diagnostics = diagnostics)
}

.mojor_hash_text_md5 <- function(text) {
    text <- as.character(text)
    tmp <- tempfile(fileext = ".txt")
    on.exit(
        unlink(tmp),
        add = TRUE
    )
    writeChar(text, tmp, eos = NULL, useBytes = TRUE)
    as.character(tools::md5sum(tmp))
}

.mojor_ssa_semantic_hash_fn <- function(ssa) {
    ssa <- .mojor_ssa_require_semantic_ir(ssa, where = "SSA semantic_hash_fn")
    can_ops <- .mojor_ir_ssa_canonicalize_ops(ssa)
    txt <- .mojor_ir_ssa_format(can_ops)
    .mojor_hash_text_md5(txt)
}

.mojor_ssa_semantic_hash_alpha_fn <- function(ssa) {
    ssa <- .mojor_ssa_require_semantic_ir(ssa, where = "SSA semantic_hash_alpha_fn")
    can <- .mojor_ssa_canonicalize(ssa)
    can_ops <- .mojor_ir_ssa_canonicalize_ops(can)
    txt <- .mojor_ir_ssa_format(can_ops)
    .mojor_hash_text_md5(txt)
}

.mojor_ssa_loop_destination_metadata <- function(ssa) {
    ssa <- .mojor_ssa_require_semantic_ir(ssa, where = "SSA loop_destination_metadata")
    block_names <- names(ssa$blocks)
    if (is.null(block_names) ||
        length(block_names) ==
            0) {
        return(list())
    }

    meta <- list()
    has_meta <- function(loop_id) !is.null(meta[[loop_id]])

    merge_meta_entry <- function(cur, incoming) {
        out <- if (is.null(cur) ||
            !is.list(cur))
            list() else cur
        for (nm in names(incoming)) {
            val <- incoming[[nm]]
            if (is.list(val) &&
                !is.null(out[[nm]]) &&
                is.list(out[[nm]])) {
                for (sn in names(val)) {
                  sval <- val[[sn]]
                  if (is.null(out[[nm]][[sn]]) ||
                    !nzchar(as.character(out[[nm]][[sn]]))) {
                    out[[nm]][[sn]] <- sval
                  }
                }
            } else if (is.null(out[[nm]]) ||
                (is.character(out[[nm]]) &&
                  !nzchar(as.character(out[[nm]])))) {
                out[[nm]] <- val
            }
        }
        out
    }

    for (bn in block_names) {
        blk <- ssa$blocks[[bn]]
        attrs <- blk$attrs
        if (is.null(attrs) ||
            !is.list(attrs) ||
            is.null(attrs$loop_id))
            next
        loop_id <- as.character(attrs$loop_id)
        if (!nzchar(loop_id))
            next

        kind <- if (!is.null(attrs$loop_kind)) {
            as.character(attrs$loop_kind)
        } else {
            if (grepl("^while_", loop_id))
                "while" else if (grepl("^repeat_", loop_id))
                "repeat" else "for"
        }
        role <- if (!is.null(attrs$loop_role))
            as.character(attrs$loop_role) else ""
        continue_block <- if (!is.null(attrs$loop_continue_dest))
            as.character(attrs$loop_continue_dest) else ""
        break_block <- if (!is.null(attrs$loop_break_dest))
            as.character(attrs$loop_break_dest) else ""
        if (!nzchar(continue_block) &&
            identical(role, "header"))
            continue_block <- bn
        if (!nzchar(break_block) &&
            identical(role, "header")) {
            exit_block <- NULL
            for (cand in block_names) {
                cattrs <- ssa$blocks[[cand]]$attrs
                if (is.null(cattrs) ||
                  !is.list(cattrs))
                  next
                cid <- if (is.null(cattrs$loop_id))
                  "" else as.character(cattrs$loop_id)
                crole <- if (is.null(cattrs$loop_role))
                  "" else as.character(cattrs$loop_role)
                if (identical(cid, loop_id) &&
                  identical(crole, "exit")) {
                  exit_block <- cand
                  break
                }
            }
            if (!is.null(exit_block))
                break_block <- exit_block
        }

        incoming <- list(
            loop_id = loop_id, kind = kind, continue_dest = list(
                loop_id = loop_id, role = "Continue", block = if (nzchar(continue_block)) continue_block else NULL
            ),
            break_dest = list(
                loop_id = loop_id, role = "Break", block = if (nzchar(break_block)) break_block else NULL
            )
        )
        if (identical(role, "header"))
            incoming$anchor_block <- bn

        if (!has_meta(loop_id)) {
            meta[[loop_id]] <- incoming
        } else {
            meta[[loop_id]] <- merge_meta_entry(meta[[loop_id]], incoming)
        }
    }

    patterns <- list(
        list(kind = "for", hdr = "^loop_header([0-9]+)$", ex = "loop_exit%s"),
        list(kind = "while", hdr = "^while_header([0-9]+)$", ex = "while_exit%s"),
        list(
            kind = "repeat", hdr = "^repeat_header([0-9]+)$", ex = "repeat_exit%s"
        )
    )

    for (bn in block_names) {
        blk <- ssa$blocks[[bn]]
        attrs <- blk$attrs
        if (!is.null(attrs) &&
            is.list(attrs) &&
            !is.null(attrs$loop_id) &&
            nzchar(as.character(attrs$loop_id))) {
            next
        }
        for (p in patterns) {
            m <- regexec(p$hdr, bn, perl = TRUE)
            g <- regmatches(bn, m)[[1]]
            if (length(g) ==
                0)
                next
            idx <- g[[2]]
            loop_id <- paste0(p$kind, "_", idx)
            exit_name <- sprintf(p$ex, idx)
            incoming <- list(
                loop_id = loop_id, kind = p$kind, anchor_block = bn, continue_dest = list(loop_id = loop_id, role = "Continue", block = bn),
                break_dest = list(
                  loop_id = loop_id, role = "Break", block = if (exit_name %in%
                    block_names) exit_name else NULL
              )
            )
            if (!has_meta(loop_id)) {
                meta[[loop_id]] <- incoming
            } else {
                meta[[loop_id]] <- merge_meta_entry(meta[[loop_id]], incoming)
            }
            break
        }
    }
    meta
}

.mojor_ssa_attach_loop_destination_metadata <- function(ssa) {
    ssa <- .mojor_ssa_require_semantic_ir(ssa, where = "SSA attach_loop_destination_metadata")
    out <- ssa
    out$loop_metadata <- .mojor_ssa_loop_destination_metadata(ssa)
    out
}
