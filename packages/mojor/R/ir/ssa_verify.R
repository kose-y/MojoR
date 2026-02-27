# =============================================================================
# SSA Verify Helpers
# =============================================================================

.mojor_ssa_memory_legacy_allowlist_default <- function() {
    character(0)
}

.mojor_ssa_is_legacy_memory_op <- function(op, allowlist = NULL) {
    if (is.null(op) ||
        !is.character(op) ||
        length(op) !=
            1 || !nzchar(op)) {
        return(FALSE)
    }
    allowlist <- if (is.null(allowlist))
        character(0) else as.character(allowlist)
    op_can <- .mojor_ssa_op_canonical(op)

    if (op %in% allowlist || op_can %in% allowlist) {
        return(FALSE)
    }

    legacy_exact <- c(
        "index", "store_index", "store_subscript", "mojor.mem.index", "mojor.mem.store_index",
        "mojor.mem.store_subscript"
    )
    if (op %in% legacy_exact || op_can %in% legacy_exact) {
        return(TRUE)
    }

    if (startsWith(op, "store:")) {
        return(TRUE)
    }
    if (startsWith(op_can, "mojor.mem.store.") &&
        !identical(op_can, "mojor.mem.store")) {
        return(TRUE)
    }

    FALSE
}

.mojor_ssa_collect_memory_legacy_diagnostics <- function(ssa, allowlist = NULL) {
    ssa <- .mojor_ssa_require_semantic_ir(ssa, where = "SSA memory legacy diagnostics")
    diagnostics <- list()
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
            if (!.mojor_ssa_is_legacy_memory_op(op, allowlist = allowlist))
                next
            diagnostics[[length(diagnostics) +
                1L]] <- list(
                code = "LEGACY_MEMORY_SPELLING", severity = "error", block = bn,
                stmt_index = as.integer(idx),
                message = sprintf(
                  "Legacy memory op spelling '%s' is not allowed at this boundary",
                  op
              ),
                hint = "Run .mojor_ssa_memory_canonicalize() before boundary verification"
            )
        }
    }
    diagnostics
}

.mojor_ssa_structured_op_spelling <- function(op) {
    if (is.null(op) ||
        !is.character(op) ||
        length(op) !=
            1 || !nzchar(op)) {
        return(FALSE)
    }
    op_raw <- as.character(op)
    op_can <- .mojor_ssa_op_canonical(op_raw)
    if (op_raw %in% c("s_if", "s_for", "s_while", "s_repeat")) {
        return(TRUE)
    }
    if (op_can %in% c("s_if", "s_for", "s_while", "s_repeat")) {
        return(TRUE)
    }
    if (grepl("(^|[.:])s_(if|for|while|repeat)$", op_raw, perl = TRUE)) {
        return(TRUE)
    }
    if (grepl("(^|[.:])s_(if|for|while|repeat)$", op_can, perl = TRUE)) {
        return(TRUE)
    }
    FALSE
}

.mojor_ssa_loop_skeleton_present <- function(ssa) {
    block_names <- names(ssa$blocks)
    if (is.null(block_names) ||
        length(block_names) ==
            0) {
        return(FALSE)
    }

    for (bn in block_names) {
        blk <- ssa$blocks[[bn]]
        attrs <- blk$attrs
        if (!is.null(attrs) &&
            is.list(attrs)) {
            role <- if (is.null(attrs$loop_role))
                "" else as.character(attrs$loop_role)
            loop_id <- if (is.null(attrs$loop_id))
                "" else as.character(attrs$loop_id)
            if (nzchar(role) &&
                nzchar(loop_id)) {
                return(TRUE)
            }
        }
        if (grepl("^(loop|while|repeat)_header[0-9]+$", bn, perl = TRUE)) {
            return(TRUE)
        }
    }

    FALSE
}

.mojor_ssa_collect_structured_lowering_diagnostics <- function(ssa) {
    ssa <- .mojor_ssa_require_semantic_ir(ssa, where = "SSA structured-lowering diagnostics")
    diagnostics <- list()

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
            if (!.mojor_ssa_structured_op_spelling(op))
                next
            diagnostics[[length(diagnostics) +
                1L]] <- list(
                code = "STRUCTURED_OP_REMAINING", severity = "error", block = bn,
                stmt_index = as.integer(idx),
                message = sprintf(
                  "Structured op '%s' must be lowered before CFG boundary verification",
                  op
              ),
                hint = "Run .mojor_ir_structured_lower_to_ssa() before post_structured_lowering checks"
            )
        }
    }

    loops_present <- .mojor_ssa_loop_skeleton_present(ssa)
    loop_meta <- if (!is.null(ssa$loop_metadata) &&
        is.list(ssa$loop_metadata)) {
        ssa$loop_metadata
    } else if (exists(".mojor_ssa_loop_destination_metadata", mode = "function")) {
        .mojor_ssa_loop_destination_metadata(ssa)
    } else {
        list()
    }

    if (isTRUE(loops_present) &&
        length(loop_meta) ==
            0) {
        diagnostics[[length(diagnostics) +
            1L]] <- list(
            code = "LOOP_METADATA_MISSING", severity = "error", block = "",
            message = "Lowered loop skeleton requires logical loop destination metadata",
            hint = "Attach loop metadata with .mojor_ssa_attach_loop_destination_metadata()"
        )
        return(diagnostics)
    }

    if (length(loop_meta) >
        0) {
        for (loop_id in names(loop_meta)) {
            meta <- loop_meta[[loop_id]]
            cont_block <- if (!is.null(meta$continue_dest$block))
                as.character(meta$continue_dest$block) else ""
            break_block <- if (!is.null(meta$break_dest$block))
                as.character(meta$break_dest$block) else ""
            if (is.null(meta$loop_id) ||
                !nzchar(as.character(meta$loop_id)) ||
                !nzchar(cont_block) ||
                !nzchar(break_block) ||
                !cont_block %in% names(ssa$blocks) ||
                !break_block %in% names(ssa$blocks)) {
                diagnostics[[length(diagnostics) +
                  1L]] <- list(
                  code = "LOOP_METADATA_INCOMPLETE", severity = "error",
                  block = if (nzchar(cont_block)) cont_block else "",
                  message = sprintf(
                    "Loop metadata for '%s' is incomplete (loop_id/continue/break destinations required)",
                    loop_id
                ),
                  hint = "Ensure structured lowering annotates loop_id plus valid continue/break destination blocks"
              )
            }
        }
    }

    diagnostics
}

.mojor_verify_semantic_ir <- function(
    ir, mode = c("core", "boundary"),
    boundary = c(
        "none", "post_canonicalize", "post_memory_canonicalize", "post_structured_lowering",
        "post_recanonicalize", "post_typing"
    ),
    opts = NULL, strict_schema = NULL
) {
    ir <- .mojor_ssa_require_semantic_ir(ir, where = "verify_semantic_ir")
    mode <- match.arg(mode)
    boundary <- match.arg(boundary)
    opts <- if (is.null(opts))
        list() else opts

    if (identical(mode, "core") &&
        !identical(boundary, "none")) {
        return(
            list(
                ok = FALSE, diagnostics = list(
                  list(
                    code = "INVALID_VERIFIER_BOUNDARY", severity = "error",
                    block = "", message = "Core verifier mode requires boundary='none'",
                    hint = "Use mode='boundary' to run boundary invariants"
                )
              )
            )
        )
    }
    if (identical(mode, "boundary") &&
        identical(boundary, "none")) {
        return(
            list(
                ok = FALSE, diagnostics = list(
                  list(
                    code = "INVALID_VERIFIER_BOUNDARY", severity = "error",
                    block = "", message = "Boundary verifier mode requires a concrete boundary",
                    hint = "Set boundary to a post-* boundary name"
                )
              )
            )
        )
    }

    diagnostics <- list()
    core_err <- tryCatch(
        {
            .mojor_ir_verify_ssa(ir, strict_schema = strict_schema)
            NULL
        }, error = function(e) e
    )
    if (!is.null(core_err)) {
        diagnostics[[1L]] <- list(
            code = "CORE_VERIFY_FAILED", severity = "error", block = "",
            message = conditionMessage(core_err),
            hint = "Fix core SSA verifier violations before running boundary checks"
        )
        return(list(ok = FALSE, diagnostics = diagnostics))
    }

    enforce_legacy_memory <- isTRUE(opts$forbid_legacy_memory_spellings) ||
        (identical(mode, "boundary") &&
            identical(boundary, "post_memory_canonicalize"))
    if (isTRUE(enforce_legacy_memory)) {
        allowlist <- opts$legacy_memory_allowlist
        if (is.null(allowlist))
            allowlist <- .mojor_ssa_memory_legacy_allowlist_default()
        diagnostics <- c(
            diagnostics, .mojor_ssa_collect_memory_legacy_diagnostics(ir, allowlist = allowlist)
        )
    }

    enforce_typing <- isTRUE(opts$require_typing) ||
        (identical(mode, "boundary") &&
            identical(boundary, "post_typing"))
    if (isTRUE(enforce_typing)) {
        diagnostics <- c(diagnostics, .mojor_ssa_collect_typing_diagnostics(ir, opts = opts))
    }

    enforce_structured_lowering <- identical(mode, "boundary") &&
        identical(boundary, "post_structured_lowering")
    if (isTRUE(enforce_structured_lowering)) {
        diagnostics <- c(diagnostics, .mojor_ssa_collect_structured_lowering_diagnostics(ir))
    }

    enforce_recanonicalize <- identical(mode, "boundary") &&
        identical(boundary, "post_recanonicalize")
    if (isTRUE(enforce_recanonicalize)) {
        recovery <- opts$loop_recovery
        if (!is.null(recovery) &&
            exists(".mojor_loop_recovery_boundary_enabled", mode = "function") &&
            !isTRUE(.mojor_loop_recovery_boundary_enabled(recovery))) {
            diagnostics <- c(
                diagnostics, list(
                  list(
                    code = "RECANONICALIZE_BOUNDARY_DISABLED_RECOVERY_UNKNOWN",
                    severity = "warning", block = "", message = "post_recanonicalize boundary checks disabled because loop recovery status is Unknown",
                    hint = "Run recanonicalization only when loop_recovery_analysis reports status='Recovered'"
                )
              )
            )
            return(list(ok = TRUE, diagnostics = diagnostics))
        }
        if (exists(".mojor_ssa_collect_recanonicalize_diagnostics", mode = "function")) {
            diagnostics <- c(diagnostics, .mojor_ssa_collect_recanonicalize_diagnostics(ir))
        }
    }

    list(
        ok = length(diagnostics) ==
            0L, diagnostics = diagnostics
    )
}

.mojor_ssa_diag_is_error <- function(diag) {
    if (is.null(diag) ||
        !is.list(diag)) {
        return(FALSE)
    }
    if (is.null(diag$severity)) {
        return(FALSE)
    }
    identical(
        tolower(as.character(diag$severity)),
        "error"
    )
}

.mojor_ssa_verify_has_errors <- function(result) {
    if (is.null(result) ||
        !is.list(result)) {
        return(TRUE)
    }
    if (!is.null(result$ok) &&
        identical(result$ok, FALSE)) {
        return(TRUE)
    }
    diags <- result$diagnostics
    if (is.null(diags) ||
        length(diags) ==
            0) {
        return(FALSE)
    }
    any(vapply(diags, .mojor_ssa_diag_is_error, logical(1)))
}

.mojor_ssa_verify_or_stop <- function(
    ssa, mode = c("core", "boundary"),
    boundary = c(
        "none", "post_canonicalize", "post_memory_canonicalize", "post_structured_lowering",
        "post_recanonicalize", "post_typing"
    ),
    opts = NULL, strict_schema = NULL, where = "SSA verifier"
) {
    mode <- match.arg(mode)
    boundary <- match.arg(boundary)
    res <- .mojor_verify_semantic_ir(
        ssa, mode = mode, boundary = boundary, opts = opts, strict_schema = strict_schema
    )
    if (!.mojor_ssa_verify_has_errors(res)) {
        return(res)
    }

    diags <- if (is.null(res$diagnostics) ||
        length(res$diagnostics) ==
            0) {
        list(
            list(
                code = "VERIFIER_FAILED", message = "verifier failed with no diagnostics"
            )
        )
    } else {
        res$diagnostics
    }
    first <- diags[[1L]]
    code <- if (!is.null(first$code))
        as.character(first$code) else "VERIFIER_FAILED"
    msg <- if (!is.null(first$message))
        as.character(first$message) else "verifier failed"
    block <- if (!is.null(first$block) &&
        nzchar(as.character(first$block))) {
        paste0(
            " [block=", as.character(first$block),
            "]"
        )
    } else {
        ""
    }
    stop(sprintf("%s: %s: %s%s", where, code, msg, block))
}

.mojor_ssa_successors <- function(term) {
    if (is.null(term) ||
        is.null(term$kind)) {
        return(character(0))
    }
    if (identical(term$kind, "br") &&
        !is.null(term$target)) {
        return(term$target)
    }
    if (identical(term$kind, "condbr")) {
        out <- character(0)
        if (!is.null(term$then))
            out <- c(out, term$then)
        if (!is.null(term[["else"]]))
            out <- c(out, term[["else"]])
        return(unique(out))
    }
    character(0)
}

.mojor_ssa_require_semantic_ir <- function(ir, where = "SSA pass") {
    if (is.null(ir) ||
        !is.list(ir) ||
        is.null(ir$kind)) {
        stop(sprintf("%s: expected SemanticIR (ssa_fn)", where))
    }
    if (identical(ir$kind, "raw_ssa_fn")) {
        stop(
            sprintf(
                "%s: got RawSyntaxTree (raw_ssa_fn); call .mojor_ssa_lower_to_semantic() first",
                where
            )
        )
    }
    if (!identical(ir$kind, "ssa_fn")) {
        stop(
            sprintf(
                "%s: expected SemanticIR (ssa_fn), got '%s'", where, as.character(ir$kind)
            )
        )
    }
    ir
}

.mojor_ssa_schema_strict_enabled <- function(strict_schema = NULL) {
    if (!is.null(strict_schema)) {
        return(isTRUE(strict_schema))
    }
    isTRUE(getOption("mojor.ssa_schema_strict", FALSE))
}

.mojor_ssa_schema_validate_if_strict <- function(ssa, strict_schema = NULL, where = "SSA verify") {
    if (!.mojor_ssa_schema_strict_enabled(strict_schema)) {
        return(invisible(TRUE))
    }
    if (!exists(".mojor_ir_validate_op_schema", mode = "function")) {
        stop(
            sprintf(
                "%s: strict schema requested but schema validator is unavailable",
                where
            )
        )
    }
    res <- .mojor_ir_validate_op_schema(ssa, strict = FALSE, canonicalize = TRUE)
    if (isTRUE(res$ok)) {
        return(invisible(TRUE))
    }
    first <- res$diagnostics[[1]]
    stop(
        sprintf(
            "%s: %s: %s (block '%s')", where, first$code, first$message,
            first$block
        )
    )
}
