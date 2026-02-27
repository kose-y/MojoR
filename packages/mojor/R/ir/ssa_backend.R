# =============================================================================
# SSA Backend Pipeline
# =============================================================================

.mojor_ir_ssa_backend_cfg <- function(ssa, verify = TRUE, strict_schema = NULL) {
    if (is.null(ssa) ||
        !is.list(ssa) ||
        is.null(ssa$kind) ||
        ssa$kind != "ssa_fn") {
        stop("SSA backend cfg: input must be an ssa_fn")
    }
    if (isTRUE(verify))
        .mojor_ir_verify_ssa(ssa, strict_schema = strict_schema)

    block_names <- names(ssa$blocks)
    blocks <- setNames(
        vector("list", length(block_names)),
        block_names
    )
    for (bn in block_names) {
        blk <- ssa$blocks[[bn]]
        params <- character(0)
        param_vars <- character(0)
        if (!is.null(blk$params) &&
            length(blk$params) >
                0) {
            params <- vapply(
                blk$params, function(p) as.character(p$id),
                character(1)
            )
            param_vars <- vapply(
                blk$params, function(p) {
                  if (is.null(p$var))
                    "" else as.character(p$var)
                }, character(1)
            )
        }

        stmts <- list()
        if (!is.null(blk$stmts) &&
            length(blk$stmts) >
                0) {
            stmts <- lapply(
                blk$stmts, function(st) {
                  out <- list(
                    kind = if (is.null(st$kind)) "" else as.character(st$kind),
                    op = if (is.null(st$op)) "" else as.character(st$op),
                    args = if (is.null(st$args)) character(0) else as.character(st$args)
                )
                  if (!is.null(st$id))
                    out$id <- as.character(st$id)
                  if (!is.null(st$value))
                    out$value <- as.character(st$value)
                  if (!is.null(st$target))
                    out$target <- as.character(st$target)
                  if (!is.null(st$index_kinds))
                    out$index_kinds <- as.character(st$index_kinds)
                  if (!is.null(st$reduce_mode))
                    out$reduce_mode <- as.character(st$reduce_mode)
                  if (!is.null(st$reduce_op))
                    out$reduce_op <- as.character(st$reduce_op)
                  if (!is.null(st$reduce_acc)) {
                    out$reduce_acc <- as.character(st$reduce_acc)
                  } else if (!is.null(st[["acc"]]))
                    out$reduce_acc <- as.character(st[["acc"]])
                  if (!is.null(st$reduce_arg)) {
                    out$reduce_arg <- as.character(st$reduce_arg)
                  } else if (!is.null(st[["arg"]]))
                    out$reduce_arg <- as.character(st[["arg"]])
                  if (!is.null(st$n_var))
                    out$n_var <- as.character(st$n_var)
                  if (!is.null(st$dtype))
                    out$dtype <- as.character(st$dtype)
                  if (!is.null(st$init_val))
                    out$init_val <- as.character(st$init_val)
                  if (!is.null(st$empty_val))
                    out$empty_val <- as.character(st$empty_val)
                  if (!is.null(st$nan_val))
                    out$nan_val <- as.character(st$nan_val)
                  if (!is.null(st$value_cast))
                    out$value_cast <- as.character(st$value_cast)
                  if (!is.null(st$axis))
                    out$axis <- as.character(st$axis)
                  if (!is.null(st$associative))
                    out$associative <- as.character(st$associative)
                  if (!is.null(st$commutative))
                    out$commutative <- as.character(st$commutative)
                  out
                }
            )
        }

        term <- blk$term
        term_out <- list(kind = as.character(term$kind))
        if (identical(term$kind, "ret")) {
            term_out$value <- if (is.null(term$value))
                NULL else as.character(term$value)
        } else if (identical(term$kind, "br")) {
            term_out$target <- as.character(term$target)
            term_out$args <- if (is.null(term$args))
                character(0) else as.character(term$args)
        } else if (identical(term$kind, "condbr")) {
            term_out$cond <- as.character(term$cond)
            term_out$then <- as.character(term$then)
            term_out[["else"]] <- as.character(term[["else"]])
            term_out$then_args <- if (is.null(term$then_args))
                character(0) else as.character(term$then_args)
            term_out$else_args <- if (is.null(term$else_args))
                character(0) else as.character(term$else_args)
        }

        blocks[[bn]] <- list(
            name = bn, params = params, param_vars = param_vars, stmts = stmts,
            term = term_out, successors = .mojor_ssa_successors(blk$term)
        )
    }

    list(
        kind = "ssa_backend_cfg", entry = ssa$entry, block_order = block_names,
        blocks = blocks
    )
}

.mojor_ir_check_ssa_backend_invariants <- function(prep) {
    if (is.null(prep) ||
        !is.list(prep)) {
        stop("SSA backend invariant: backend prep result must be a list")
    }

    pipeline <- prep$pipeline
    if (!is.character(pipeline) ||
        length(pipeline) ==
            0) {
        stop(
            "SSA backend invariant: pipeline must be a non-empty character vector"
        )
    }

    required <- c(
        "schedule_ir", "to_ssa", "memory_canonicalize", "annotate_effects_resources",
        "typing", "backend_cfg"
    )
    missing <- setdiff(required, pipeline)
    if (length(missing) >
        0) {
        stop(
            paste0(
                "SSA backend invariant: pipeline missing required stage(s): ",
                paste(missing, collapse = ", ")
            )
        )
    }

    pos_schedule <- match("schedule_ir", pipeline)
    pos_to_ssa <- match("to_ssa", pipeline)
    pos_mem <- match("memory_canonicalize", pipeline)
    pos_effect <- match("annotate_effects_resources", pipeline)
    pos_typing <- match("typing", pipeline)
    pos_cfg <- match("backend_cfg", pipeline)
    if (!(pos_schedule < pos_to_ssa && pos_to_ssa < pos_cfg)) {
        stop(
            "SSA backend invariant: expected stage order schedule_ir -> to_ssa -> backend_cfg"
        )
    }
    if (!(pos_to_ssa < pos_mem && pos_mem < pos_effect && pos_effect <
        pos_typing && pos_typing < pos_cfg)) {
        stop(
            "SSA backend invariant: expected stage order to_ssa -> memory_canonicalize -> annotate_effects_resources -> typing -> backend_cfg"
        )
    }
    if ("loop_recovery" %in% pipeline && !(match("annotate_effects_resources", pipeline) <
        match("loop_recovery", pipeline))) {
        stop(
            "SSA backend invariant: loop_recovery must run after annotate_effects_resources"
        )
    }
    if ("recanonicalize_loop_cfg" %in% pipeline) {
        if (!(match("annotate_effects_resources", pipeline) <
            match("recanonicalize_loop_cfg", pipeline))) {
            stop(
                "SSA backend invariant: recanonicalize_loop_cfg must run after annotate_effects_resources"
            )
        }
        if (!(match("recanonicalize_loop_cfg", pipeline) <
            match("typing", pipeline))) {
            stop(
                "SSA backend invariant: recanonicalize_loop_cfg must run before typing"
            )
        }
    }
    if ("fusion_candidate_analysis" %in% pipeline) {
        if (!(match("annotate_effects_resources", pipeline) <
            match("fusion_candidate_analysis", pipeline))) {
            stop(
                "SSA backend invariant: fusion_candidate_analysis must run after annotate_effects_resources"
            )
        }
        if (!(match("fusion_candidate_analysis", pipeline) <
            match("typing", pipeline))) {
            stop(
                "SSA backend invariant: fusion_candidate_analysis must run before typing"
            )
        }
    }
    if ("fusion_rewrite" %in% pipeline) {
        if (!("fusion_candidate_analysis" %in% pipeline)) {
            stop(
                "SSA backend invariant: fusion_rewrite requires fusion_candidate_analysis in pipeline"
            )
        }
        if (!(match("fusion_candidate_analysis", pipeline) <
            match("fusion_rewrite", pipeline))) {
            stop(
                "SSA backend invariant: fusion_rewrite must run after fusion_candidate_analysis"
            )
        }
        if (!(match("fusion_rewrite", pipeline) <
            match("typing", pipeline))) {
            stop("SSA backend invariant: fusion_rewrite must run before typing")
        }
    }

    ir_sched <- prep$ir$scheduled
    if (is.null(ir_sched) ||
        !is.list(ir_sched) ||
        is.null(ir_sched$kind)) {
        stop(
            "SSA backend invariant: schedule_ir must produce an IR node with kind"
        )
    }

    ssa_raw <- prep$ssa$raw
    if (is.null(ssa_raw) ||
        !is.list(ssa_raw) ||
        !identical(ssa_raw$kind, "ssa_fn")) {
        stop("SSA backend invariant: to_ssa must produce an ssa_fn")
    }
    ssa_mem <- prep$ssa$memory_canonical
    if (is.null(ssa_mem) ||
        !is.list(ssa_mem) ||
        !identical(ssa_mem$kind, "ssa_fn")) {
        stop("SSA backend invariant: memory_canonicalize must produce an ssa_fn")
    }
    ssa_eff <- prep$ssa$effect_annotated
    if (is.null(ssa_eff) ||
        !is.list(ssa_eff) ||
        !identical(ssa_eff$kind, "ssa_fn")) {
        stop(
            "SSA backend invariant: annotate_effects_resources must produce an ssa_fn"
        )
    }
    ssa_typed <- prep$ssa$typed
    if (is.null(ssa_typed) ||
        !is.list(ssa_typed) ||
        !identical(ssa_typed$kind, "ssa_fn")) {
        stop("SSA backend invariant: typing must produce an ssa_fn")
    }
    if ("fusion_candidate_analysis" %in% pipeline) {
        fa <- prep$ssa$fusion_analysis
        if (is.null(fa) ||
            !is.list(fa)) {
            stop(
                "SSA backend invariant: fusion_candidate_analysis stage must produce analysis metadata"
            )
        }
        if (is.null(fa$analysis_only) ||
            !isTRUE(fa$analysis_only)) {
            stop(
                "SSA backend invariant: fusion_candidate_analysis must remain analysis-only in P3"
            )
        }
    }
    if ("fusion_rewrite" %in% pipeline) {
        fr <- prep$ssa$fusion_rewrite
        if (is.null(fr) ||
            !is.list(fr) ||
            !identical(fr$kind, "fusion_rewrite")) {
            stop(
                "SSA backend invariant: fusion_rewrite stage must produce rewrite metadata"
            )
        }
        if (is.null(fr$ssa) ||
            !is.list(fr$ssa) ||
            !identical(fr$ssa$kind, "ssa_fn")) {
            stop(
                "SSA backend invariant: fusion_rewrite stage must produce rewritten ssa_fn"
            )
        }
    }

    cfg <- prep$backend_cfg
    if (is.null(cfg) ||
        !is.list(cfg) ||
        !identical(cfg$kind, "ssa_backend_cfg")) {
        stop("SSA backend invariant: backend_cfg must produce an ssa_backend_cfg")
    }
    if (is.null(cfg$entry) ||
        !is.character(cfg$entry) ||
        length(cfg$entry) !=
            1 || !nzchar(cfg$entry)) {
        stop(
            "SSA backend invariant: backend_cfg entry must be a non-empty string"
        )
    }
    if (is.null(cfg$blocks) ||
        !is.list(cfg$blocks) ||
        length(cfg$blocks) ==
            0) {
        stop("SSA backend invariant: backend_cfg blocks must be a non-empty list")
    }
    if (!(cfg$entry %in% names(cfg$blocks))) {
        stop(
            "SSA backend invariant: backend_cfg entry must reference an existing block"
        )
    }
    if (!is.null(cfg$block_order) &&
        length(cfg$block_order) >
            0 && !all(cfg$block_order %in% names(cfg$blocks))) {
        stop(
            "SSA backend invariant: backend_cfg block_order must reference existing blocks"
        )
    }

    pass_trace <- prep$ssa$pass_trace
    if (!is.null(pass_trace)) {
        if (!is.list(pass_trace)) {
            stop("SSA backend invariant: ssa pass_trace must be a list")
        }
        for (step in pass_trace) {
            if (is.null(step) ||
                !is.list(step) ||
                is.null(step$pass) ||
                !is.character(step$pass) ||
                length(step$pass) !=
                  1 || !nzchar(step$pass)) {
                stop(
                  "SSA backend invariant: each ssa pass_trace entry must include a non-empty pass name"
              )
            }
        }
    }

    if ("lower_backend" %in% pipeline) {
        lowered <- prep$backend_lowered
        if (is.null(lowered) ||
            !is.list(lowered) ||
            !identical(lowered$kind, "ssa_backend_lowered")) {
            stop(
                "SSA backend invariant: lower_backend stage must produce an ssa_backend_lowered artifact"
            )
        }
        if (is.null(lowered$entry) ||
            !is.character(lowered$entry) ||
            length(lowered$entry) !=
                1 || !nzchar(lowered$entry)) {
            stop(
                "SSA backend invariant: backend_lowered entry must be a non-empty string"
            )
        }
        if (is.null(lowered$blocks) ||
            !is.list(lowered$blocks) ||
            length(lowered$blocks) ==
                0) {
            stop(
                "SSA backend invariant: backend_lowered blocks must be a non-empty list"
            )
        }
        if (!(lowered$entry %in% names(lowered$blocks))) {
            stop(
                "SSA backend invariant: backend_lowered entry must reference an existing block"
            )
        }
    }

    if ("select_backend" %in% pipeline) {
        selected <- prep$backend_selected
        if (is.null(selected) ||
            !is.list(selected) ||
            !identical(selected$kind, "ssa_backend_selected")) {
            stop(
                "SSA backend invariant: select_backend stage must produce an ssa_backend_selected artifact"
            )
        }
        if (is.null(selected$entry) ||
            !is.character(selected$entry) ||
            length(selected$entry) !=
                1 || !nzchar(selected$entry)) {
            stop(
                "SSA backend invariant: backend_selected entry must be a non-empty string"
            )
        }
        if (is.null(selected$blocks) ||
            !is.list(selected$blocks) ||
            length(selected$blocks) ==
                0) {
            stop(
                "SSA backend invariant: backend_selected blocks must be a non-empty list"
            )
        }
        if (!(selected$entry %in% names(selected$blocks))) {
            stop(
                "SSA backend invariant: backend_selected entry must reference an existing block"
            )
        }
    }

    if ("codegen_backend" %in% pipeline) {
        if (!("select_backend" %in% pipeline)) {
            stop(
                "SSA backend invariant: codegen_backend requires select_backend in pipeline"
            )
        }
        if (!(match("select_backend", pipeline) <
            match("codegen_backend", pipeline))) {
            stop(
                "SSA backend invariant: expected stage order select_backend -> codegen_backend"
            )
        }
        codegen <- prep$backend_codegen
        if (is.null(codegen) ||
            !is.list(codegen) ||
            !identical(codegen$kind, "ssa_backend_codegen")) {
            stop(
                "SSA backend invariant: codegen_backend stage must produce an ssa_backend_codegen artifact"
            )
        }
        if (is.null(codegen$target) ||
            !is.character(codegen$target) ||
            length(codegen$target) !=
                1 || !nzchar(codegen$target)) {
            stop(
                "SSA backend invariant: backend_codegen target must be a non-empty string"
            )
        }
        if (is.null(codegen$entry) ||
            !is.character(codegen$entry) ||
            length(codegen$entry) !=
                1 || !nzchar(codegen$entry)) {
            stop(
                "SSA backend invariant: backend_codegen entry must be a non-empty string"
            )
        }
        if (is.null(codegen$blocks) ||
            !is.list(codegen$blocks) ||
            length(codegen$blocks) ==
                0) {
            stop(
                "SSA backend invariant: backend_codegen blocks must be a non-empty list"
            )
        }
        if (!(codegen$entry %in% names(codegen$blocks))) {
            stop(
                "SSA backend invariant: backend_codegen entry must reference an existing block"
            )
        }
    }

    invisible(TRUE)
}

.mojor_ir_ssa_backend_lower <- function(prep_or_cfg, verify = TRUE, strict_schema = NULL) {
    cfg <- prep_or_cfg
    if (is.list(prep_or_cfg) &&
        is.null(prep_or_cfg$kind) &&
        !is.null(prep_or_cfg$backend_cfg)) {
        cfg <- prep_or_cfg$backend_cfg
    }
    if (is.null(cfg) ||
        !is.list(cfg) ||
        !identical(cfg$kind, "ssa_backend_cfg")) {
        stop(
            "SSA backend lower: input must be an ssa_backend_cfg or backend prep result"
        )
    }

    if (isTRUE(verify)) {
        if (is.null(cfg$entry) ||
            !is.character(cfg$entry) ||
            length(cfg$entry) !=
                1 || !nzchar(cfg$entry)) {
            stop("SSA backend lower: cfg entry must be a non-empty string")
        }
        if (is.null(cfg$blocks) ||
            !is.list(cfg$blocks) ||
            length(cfg$blocks) ==
                0) {
            stop("SSA backend lower: cfg blocks must be a non-empty list")
        }
        if (!(cfg$entry %in% names(cfg$blocks))) {
            stop("SSA backend lower: cfg entry must reference an existing block")
        }
    }

    block_order <- cfg$block_order
    if (is.null(block_order) ||
        length(block_order) ==
            0) {
        block_order <- names(cfg$blocks)
    }
    block_order <- as.character(block_order)
    block_order <- unique(block_order[block_order %in% names(cfg$blocks)])
    remaining <- setdiff(
        names(cfg$blocks),
        block_order
    )
    if (length(remaining) >
        0)
        block_order <- c(block_order, remaining)

    fmt_args <- function(args) {
        if (is.null(args) ||
            length(args) ==
                0) {
            return("")
        }
        paste(
            as.character(args),
            collapse = ", "
        )
    }

    blocks <- setNames(
        vector("list", length(block_order)),
        block_order
    )
    instructions <- character(0)

    for (bn in block_order) {
        blk <- cfg$blocks[[bn]]
        params <- if (is.null(blk$params))
            character(0) else as.character(blk$params)
        param_vars <- if (is.null(blk$param_vars))
            character(0) else as.character(blk$param_vars)
        if (length(param_vars) <
            length(params)) {
            param_vars <- c(
                param_vars, rep(
                  "", length(params) -
                    length(param_vars)
              )
            )
        }

        param_repr <- character(0)
        if (length(params) >
            0) {
            for (i in seq_along(params)) {
                p <- params[[i]]
                pv <- param_vars[[i]]
                if (!is.null(pv) &&
                  nzchar(pv)) {
                  param_repr <- c(param_repr, paste0(p, "/*", pv, "*/"))
                } else {
                  param_repr <- c(param_repr, p)
                }
            }
        }

        stmt_lines <- character(0)
        if (!is.null(blk$stmts) &&
            length(blk$stmts) >
                0) {
            for (st in blk$stmts) {
                if (is.null(st) ||
                  !is.list(st))
                  next
                op <- if (is.null(st$op) ||
                  !nzchar(as.character(st$op)))
                  "<op>" else as.character(st$op)
                rhs <- paste0(
                  op, "(", fmt_args(st$args),
                  ")"
              )
                if (!is.null(st$value) &&
                  length(st$value) ==
                    1 && nzchar(as.character(st$value))) {
                  rhs <- paste0(rhs, " value=", as.character(st$value))
                }
                if (!is.null(st$target) &&
                  length(st$target) ==
                    1 && nzchar(as.character(st$target))) {
                  rhs <- paste0(rhs, " target=", as.character(st$target))
                }
                if (!is.null(st$index_kinds) &&
                  length(st$index_kinds) >
                    0) {
                  rhs <- paste0(
                    rhs, " index_kinds=[", paste(
                      as.character(st$index_kinds),
                      collapse = ","
                  ),
                    "]"
                )
                }
                if (!is.null(st$reduce_mode))
                  rhs <- paste0(rhs, " reduce_mode=", as.character(st$reduce_mode))
                if (!is.null(st$reduce_op))
                  rhs <- paste0(rhs, " reduce_op=", as.character(st$reduce_op))
                if (!is.null(st$reduce_acc)) {
                  rhs <- paste0(rhs, " acc=", as.character(st$reduce_acc))
                } else if (!is.null(st[["acc"]]))
                  rhs <- paste0(rhs, " acc=", as.character(st[["acc"]]))
                if (!is.null(st$reduce_arg)) {
                  rhs <- paste0(rhs, " arg=", as.character(st$reduce_arg))
                } else if (!is.null(st[["arg"]]))
                  rhs <- paste0(rhs, " arg=", as.character(st[["arg"]]))
                if (!is.null(st$n_var))
                  rhs <- paste0(rhs, " n_var=", as.character(st$n_var))
                if (!is.null(st$dtype))
                  rhs <- paste0(rhs, " dtype=", as.character(st$dtype))
                if (!is.null(st$init_val))
                  rhs <- paste0(rhs, " init_val=", as.character(st$init_val))
                if (!is.null(st$empty_val))
                  rhs <- paste0(rhs, " empty_val=", as.character(st$empty_val))
                if (!is.null(st$nan_val))
                  rhs <- paste0(rhs, " nan_val=", as.character(st$nan_val))
                if (!is.null(st$value_cast))
                  rhs <- paste0(rhs, " value_cast=", as.character(st$value_cast))
                if (!is.null(st$id) &&
                  length(st$id) ==
                    1 && nzchar(as.character(st$id))) {
                  stmt_lines <- c(
                    stmt_lines, paste0(
                      as.character(st$id),
                      " = ", rhs
                  )
                )
                } else {
                  stmt_lines <- c(stmt_lines, rhs)
                }
            }
        }

        term <- blk$term
        term_line <- "<term>"
        if (!is.null(term) &&
            is.list(term) &&
            !is.null(term$kind)) {
            if (identical(term$kind, "ret")) {
                if (is.null(term$value) ||
                  !nzchar(as.character(term$value))) {
                  term_line <- "ret"
                } else {
                  term_line <- paste0("ret ", as.character(term$value))
                }
            } else if (identical(term$kind, "br")) {
                term_line <- paste0(
                  "br ", as.character(term$target),
                  "(", fmt_args(term$args),
                  ")"
              )
            } else if (identical(term$kind, "condbr")) {
                term_line <- paste0(
                  "condbr ", as.character(term$cond),
                  " ? ", as.character(term$then),
                  "(", fmt_args(term$then_args),
                  ")", " : ", as.character(term[["else"]]),
                  "(", fmt_args(term$else_args),
                  ")"
              )
            } else {
                term_line <- paste0(
                  "<term ", as.character(term$kind),
                  ">"
              )
            }
        }

        successors <- if (is.null(blk$successors))
            character(0) else as.character(blk$successors)
        succ_line <- if (length(successors) ==
            0)
            "succ: <none>" else paste0("succ: ", paste(successors, collapse = ", "))

        blocks[[bn]] <- list(
            name = bn, params = params, param_vars = param_vars, param_repr = param_repr,
            stmts = stmt_lines, term = term_line, successors = successors,
            ssa_stmts = blk$stmts, ssa_term = blk$term
        )

        instructions <- c(
            instructions, paste0(
                "label ", bn, "(", paste(param_repr, collapse = ", "),
                ")"
            ),
            paste0("  ", stmt_lines),
            paste0("  ", term_line),
            paste0("  ", succ_line)
        )
    }

    list(
        kind = "ssa_backend_lowered", entry = cfg$entry, block_order = block_order,
        blocks = blocks, instructions = instructions
    )
}

.mojor_ir_backend_sanitize_opcode <- function(op) {
    out <- gsub("[^A-Za-z0-9]+", "_", as.character(op))
    out <- gsub("^_+|_+$", "", out)
    if (!nzchar(out))
        out <- "op"
    tolower(out)
}

.mojor_ir_backend_normalize_type <- function(type_name) {
    t <- tolower(as.character(type_name))
    if (t %in% c("int", "int32", "i32")) {
        return("i32")
    }
    if (t %in% c("int64", "i64")) {
        return("i64")
    }
    if (t %in% c("double", "float64", "f64")) {
        return("f64")
    }
    if (t %in% c("float32", "single", "f32")) {
        return("f32")
    }
    if (t %in% c("bool", "logical", "lgl")) {
        return("bool")
    }
    .mojor_ir_backend_sanitize_opcode(t)
}

.mojor_ir_backend_normalize_store_kind <- function(index_kinds, default = "scalar") {
    if (is.null(index_kinds) ||
        length(index_kinds) ==
            0) {
        return(default)
    }
    kinds <- unique(as.character(index_kinds))
    if ("slice" %in% kinds) {
        return("slice")
    }
    if ("missing" %in% kinds) {
        return("missing")
    }
    default
}

.mojor_ir_backend_selected_opcode_normalize <- function(inst) {
    if (is.null(inst) ||
        !is.list(inst) ||
        is.null(inst$opcode)) {
        return(inst)
    }
    op <- as.character(inst$opcode)
    if (!startsWith(op, "mojor.")) {
        return(inst)
    }

    op_can <- .mojor_ssa_op_canonical(op)
    args <- if (is.null(inst$args))
        character(0) else as.character(inst$args)

    arith_bin_map <- c(
        mojor.arith.add = "add", mojor.arith.sub = "sub", mojor.arith.mul = "mul",
        mojor.arith.div = "div", mojor.arith.mod = "mod", mojor.arith.idiv = "floordiv",
        mojor.arith.cmp_gt = "cmp_gt", mojor.arith.cmp_lt = "cmp_lt", mojor.arith.cmp_ge = "cmp_ge",
        mojor.arith.cmp_le = "cmp_le", mojor.arith.cmp_eq = "cmp_eq", mojor.arith.cmp_ne = "cmp_ne",
        mojor.arith.and = "and", mojor.arith.or = "or"
    )
    mapped_bin <- unname(arith_bin_map[op_can])
    if (length(mapped_bin) ==
        1 && !is.na(mapped_bin) &&
        nzchar(mapped_bin)) {
        inst$opcode <- mapped_bin
        return(inst)
    }

    if (identical(op_can, "mojor.arith.const") ||
        identical(op_can, "mojor.misc.const_null")) {
        inst$opcode <- "mov_const"
        return(inst)
    }
    if (identical(op_can, "mojor.arith.pos")) {
        inst$opcode <- "mov"
        return(inst)
    }
    if (identical(op_can, "mojor.arith.neg")) {
        inst$opcode <- "neg"
        return(inst)
    }
    if (identical(op_can, "mojor.arith.not")) {
        inst$opcode <- "not"
        return(inst)
    }
    if (startsWith(op_can, "mojor.arith.cast.")) {
        cast_to <- .mojor_ir_backend_normalize_type(sub("^mojor\\.arith\\.cast\\.", "", op_can))
        inst$opcode <- paste0("cast_", cast_to)
        if (is.null(inst$cast_to) ||
            !nzchar(as.character(inst$cast_to)))
            inst$cast_to <- cast_to
        return(inst)
    }
    if (startsWith(op_can, "mojor.misc.call.")) {
        callee <- sub("^mojor\\.misc\\.call\\.", "", op_can)
        inst$opcode <- "call"
        if (is.null(inst$callee) ||
            !nzchar(as.character(inst$callee)))
            inst$callee <- callee
        return(inst)
    }
    if (identical(op_can, "mojor.mem.index") ||
        identical(op_can, "mojor.mem.load")) {
        inst$opcode <- "load_index"
        return(inst)
    }
    if (identical(op_can, "mojor.arith.select")) {
        inst$opcode <- "select"
        return(inst)
    }
    if (identical(op_can, "mojor.misc.idx_missing")) {
        inst$opcode <- "idx_missing"
        return(inst)
    }
    if (identical(op_can, "mojor.misc.idx_slice")) {
        inst$opcode <- "idx_slice"
        return(inst)
    }
    if (identical(op_can, "mojor.cf.loop_cond")) {
        inst$opcode <- "loop_cond"
        return(inst)
    }
    if (identical(op_can, "mojor.cf.loop_next")) {
        inst$opcode <- "loop_next"
        return(inst)
    }
    if (identical(op_can, "mojor.mem.store_index")) {
        store_kind <- .mojor_ir_backend_normalize_store_kind(inst$index_kinds, default = "scalar")
        inst$opcode <- paste0("mem_store_index_", store_kind)
        if (is.null(inst$base) &&
            length(args) >=
                1)
            inst$base <- args[[1]]
        if (is.null(inst$value) &&
            length(args) >=
                2)
            inst$value <- args[[length(args)]]
        if (is.null(inst$index_args) &&
            length(args) >
                2)
            inst$index_args <- args[2:(length(args) -
                1)]
        return(inst)
    }
    if (identical(op_can, "mojor.mem.store_subscript")) {
        store_kind <- .mojor_ir_backend_normalize_store_kind(inst$index_kinds, default = "scalar")
        inst$opcode <- paste0("mem_store_subscript_", store_kind)
        if (is.null(inst$target) &&
            !is.null(inst$target_name))
            inst$target <- as.character(inst$target_name)
        return(inst)
    }
    if (identical(op_can, "mojor.mem.store")) {
        store_kind <- .mojor_ir_backend_normalize_store_kind(inst$index_kinds, default = "scalar")
        legacy_form <- if (!is.null(inst$legacy_memory_form))
            as.character(inst$legacy_memory_form[[1]]) else ""
        is_subscript <- identical(legacy_form, "store_subscript")
        if (!is_subscript && !is.null(inst$target) &&
            nzchar(as.character(inst$target))) {
            is_subscript <- TRUE
        }
        if (isTRUE(is_subscript)) {
            inst$opcode <- paste0("mem_store_subscript_", store_kind)
            if (is.null(inst$target) &&
                !is.null(inst$target_name))
                inst$target <- as.character(inst$target_name)
            return(inst)
        }
        inst$opcode <- paste0("mem_store_index_", store_kind)
        if (is.null(inst$base) &&
            length(args) >=
                1)
            inst$base <- args[[1]]
        if (is.null(inst$value) &&
            length(args) >=
                2)
            inst$value <- args[[length(args)]]
        if (is.null(inst$index_args) &&
            length(args) >
                2)
            inst$index_args <- args[2:(length(args) -
                1)]
        return(inst)
    }
    if (startsWith(op_can, "mojor.mem.store.")) {
        lhs <- .mojor_ir_backend_sanitize_opcode(sub("^mojor\\.mem\\.store\\.", "", op_can))
        inst$opcode <- paste0("mem_store_", lhs)
        return(inst)
    }
    if (startsWith(op_can, "mojor.misc.stmt.")) {
        sk <- .mojor_ir_backend_sanitize_opcode(sub("^mojor\\.misc\\.stmt\\.", "", op_can))
        inst$opcode <- paste0("effect_stmt_", sk)
        return(inst)
    }

    inst
}

.mojor_ir_backend_select_stmt <- function(st) {
    if (is.null(st) ||
        !is.list(st)) {
        return(NULL)
    }
    op <- if (is.null(st$op))
        "" else as.character(st$op)
    op_can <- .mojor_ssa_op_canonical(op)
    st_kind <- if (is.null(st$kind))
        "" else as.character(st$kind)
    dst <- if (is.null(st$id))
        NULL else as.character(st$id)
    args <- if (is.null(st$args))
        character(0) else as.character(st$args)

    mk <- function(opcode, extra = list()) {
        out <- c(
            list(kind = "inst", opcode = opcode, args = args),
            extra
        )
        if (!is.null(dst) &&
            length(dst) ==
                1 && nzchar(dst))
            out$dst <- dst
        out
    }

    if (identical(op, "const") ||
        identical(op_can, "mojor.arith.const") ||
        identical(op_can, "mojor.misc.const_null")) {
        return(
            mk(
                "mov_const", list(value = if (is.null(st$value)) "" else as.character(st$value))
            )
        )
    }

    can_suffix <- sub("^mojor\\.arith\\.", "", op_can)
    can_binop_map <- c(
        add = "+", sub = "-", mul = "*", div = "/", mod = "%%", idiv = "%/%",
        cmp_gt = ">", cmp_lt = "<", cmp_ge = ">=", cmp_le = "<=", cmp_eq = "==",
        cmp_ne = "!=", and = "&&", or = "||"
    )
    is_can_binop <- (!is.na(can_suffix) &&
        can_suffix %in% names(can_binop_map)) ||
        startsWith(op_can, "mojor.arith.binop.")
    if (startsWith(op, "binop:") ||
        is_can_binop) {
        bop <- sub("^binop:", "", op)
        if (is_can_binop) {
            mapped_legacy <- unname(can_binop_map[can_suffix])
            if (length(mapped_legacy) ==
                1 && !is.na(mapped_legacy) &&
                nzchar(mapped_legacy)) {
                bop <- mapped_legacy
            } else if (!startsWith(op, "binop:")) {
                bop <- can_suffix
            }
        }
        binop_map <- c(
            `+` = "add", `-` = "sub", `*` = "mul", `/` = "div", `%%` = "mod",
            `%/%` = "floordiv", `>` = "cmp_gt", `<` = "cmp_lt", `>=` = "cmp_ge",
            `<=` = "cmp_le", `==` = "cmp_eq", `!=` = "cmp_ne", `&&` = "and",
            `||` = "or"
        )
        mapped <- unname(binop_map[bop])
        opcode <- if (length(mapped) ==
            1 && !is.na(mapped) &&
            nzchar(mapped)) {
            as.character(mapped)
        } else {
            paste0("binop_", .mojor_ir_backend_sanitize_opcode(bop))
        }
        return(mk(opcode))
    }

    if (startsWith(op, "unop:") ||
        op_can %in% c("mojor.arith.pos", "mojor.arith.neg", "mojor.arith.not")) {
        uop <- sub("^unop:", "", op)
        if (!startsWith(op, "unop:")) {
            uop <- switch(
                op_can, mojor.arith.pos = "+", mojor.arith.neg = "-", mojor.arith.not = "!",
                ""
            )
        }
        unop_map <- c(`+` = "mov", `-` = "neg", `!` = "not")
        mapped <- unname(unop_map[uop])
        opcode <- if (length(mapped) ==
            1 && !is.na(mapped) &&
            nzchar(mapped)) {
            as.character(mapped)
        } else {
            paste0("unop_", .mojor_ir_backend_sanitize_opcode(uop))
        }
        return(mk(opcode))
    }

    if (startsWith(op, "cast:") ||
        startsWith(op_can, "mojor.arith.cast.")) {
        cast_raw <- if (startsWith(op, "cast:"))
            sub("^cast:", "", op) else sub("^mojor\\.arith\\.cast\\.", "", op_can)
        cast_to <- .mojor_ir_backend_normalize_type(cast_raw)
        return(
            mk(
                paste0("cast_", cast_to),
                list(cast_to = cast_to)
            )
        )
    }

    if (startsWith(op, "call:") ||
        startsWith(op_can, "mojor.misc.call.")) {
        callee <- if (startsWith(op, "call:"))
            sub("^call:", "", op) else sub("^mojor\\.misc\\.call\\.", "", op_can)
        return(mk("call", list(callee = as.character(callee))))
    }

    if (identical(op, "index") ||
        identical(op_can, "mojor.mem.index") ||
        identical(op_can, "mojor.mem.load")) {
        return(mk("load_index"))
    }

    if (identical(op, "select") ||
        identical(op_can, "mojor.arith.select")) {
        return(mk("select"))
    }

    if (identical(op, "idx_missing") ||
        identical(op_can, "mojor.misc.idx_missing")) {
        out <- mk("idx_missing")
        if (!is.null(st$index_kinds))
            out$index_kinds <- as.character(st$index_kinds)
        return(out)
    }

    if (identical(op, "idx_slice") ||
        identical(op_can, "mojor.misc.idx_slice")) {
        out <- mk("idx_slice")
        if (!is.null(st$index_kinds))
            out$index_kinds <- as.character(st$index_kinds)
        return(out)
    }

    if (identical(op, "loop_cond") ||
        identical(op_can, "mojor.cf.loop_cond")) {
        return(mk("loop_cond"))
    }

    if (identical(op, "loop_next") ||
        identical(op_can, "mojor.cf.loop_next")) {
        return(mk("loop_next"))
    }

    if (identical(st_kind, "ssa_effect")) {
        if (identical(op, "scheduled_reduce") ||
            identical(op, "scalar_reduce") ||
            identical(op_can, "mojor.misc.stmt.scheduled_reduce") ||
            identical(op_can, "mojor.misc.stmt.scalar_reduce")) {
            is_sched_reduce <- identical(op, "scheduled_reduce") ||
                identical(op_can, "mojor.misc.stmt.scheduled_reduce")
            mode <- if (!is.null(st$reduce_mode) &&
                nzchar(as.character(st$reduce_mode)))
                as.character(st$reduce_mode) else if (is_sched_reduce)
                "tree" else "linear"
            redop <- if (!is.null(st$reduce_op) &&
                nzchar(as.character(st$reduce_op)))
                as.character(st$reduce_op) else "sum"
            acc <- if (!is.null(st$reduce_acc) &&
                nzchar(as.character(st$reduce_acc)))
                as.character(st$reduce_acc) else if (!is.null(st[["acc"]]) &&
                nzchar(as.character(st[["acc"]])))
                as.character(st[["acc"]]) else if (length(args) >=
                1)
                args[[1]] else ""
            arg_name <- if (!is.null(st$reduce_arg) &&
                nzchar(as.character(st$reduce_arg)))
                as.character(st$reduce_arg) else if (!is.null(st[["arg"]]) &&
                nzchar(as.character(st[["arg"]])))
                as.character(st[["arg"]]) else if (length(args) >=
                2)
                args[[2]] else ""
            n_var <- if (!is.null(st$n_var) &&
                nzchar(as.character(st$n_var)))
                as.character(st$n_var) else if (length(args) >=
                3)
                args[[3]] else "n_i"
            is_arg_reduce <- redop %in% c("which.min", "which.max")
            merge_kind <- if (identical(mode, "simd"))
                "simd_lane" else if (identical(mode, "tree"))
                "tree_pairwise" else "linear_fold"

            shared_meta <- list(
                reduce_mode = mode, reduce_op = redop, reduce_acc = acc,
                reduce_arg = arg_name, n_var = n_var
            )
            if (!is.null(st$dtype))
                shared_meta$dtype <- as.character(st$dtype)
            if (!is.null(st$init_val))
                shared_meta$init_val <- as.character(st$init_val)
            if (!is.null(st$empty_val))
                shared_meta$empty_val <- as.character(st$empty_val)
            if (!is.null(st$nan_val))
                shared_meta$nan_val <- as.character(st$nan_val)
            if (!is.null(st$value_cast))
                shared_meta$value_cast <- as.character(st$value_cast)
            if (is_arg_reduce)
                shared_meta$tie_break <- "first"

            mk_reduce <- function(opcode, extra = list()) {
                c(
                  list(kind = "inst", opcode = opcode, args = c(acc, arg_name, n_var)),
                  shared_meta, extra
              )
            }

            bundle <- list(
                mk_reduce("reduce_init"),
                mk_reduce("reduce_step"),
                mk_reduce("reduce_merge", list(merge_kind = merge_kind)),
                mk_reduce("reduce_finalize")
            )
            return(list(kind = "inst_bundle", instructions = bundle))
        }

        if (identical(op_can, "mojor.mem.store")) {
            store_kind <- if (!is.null(st$index_kinds) &&
                length(st$index_kinds) >
                  0) {
                kinds <- unique(as.character(st$index_kinds))
                if ("slice" %in% kinds) {
                  "slice"
                } else if ("missing" %in% kinds) {
                  "missing"
                } else {
                  "scalar"
                }
            } else {
                "scalar"
            }
            legacy_form <- if (!is.null(st$legacy_memory_form))
                as.character(st$legacy_memory_form[[1]]) else ""
            is_subscript <- identical(legacy_form, "store_subscript")
            if (!is_subscript && !is.null(st$target) &&
                nzchar(as.character(st$target))) {
                is_subscript <- TRUE
            }
            if (isTRUE(is_subscript)) {
                return(
                  list(
                    kind = "inst", opcode = paste0("mem_store_subscript_", store_kind),
                    target = if (is.null(st$target)) "" else as.character(st$target),
                    args = args, index_kinds = if (is.null(st$index_kinds)) character(0) else as.character(st$index_kinds),
                    legacy_memory_form = legacy_form
                )
              )
            }
            base <- if (length(args) >=
                1)
                args[[1]] else ""
            value <- if (length(args) >=
                2)
                args[[length(args)]] else ""
            idx_args <- if (length(args) >
                2)
                args[2:(length(args) -
                  1)] else character(0)
            return(
                list(
                  kind = "inst", opcode = paste0("mem_store_index_", store_kind),
                  base = base, index_args = idx_args, value = value, args = args,
                  index_kinds = if (is.null(st$index_kinds)) character(0) else as.character(st$index_kinds),
                  legacy_memory_form = legacy_form
              )
            )
        }

        if (identical(op, "store_index") ||
            identical(op_can, "mojor.mem.store_index")) {
            base <- if (length(args) >=
                1)
                args[[1]] else ""
            value <- if (length(args) >=
                2)
                args[[length(args)]] else ""
            idx_args <- if (length(args) >
                2)
                args[2:(length(args) -
                  1)] else character(0)
            store_kind <- if (!is.null(st$index_kinds) &&
                length(st$index_kinds) >
                  0) {
                kinds <- unique(as.character(st$index_kinds))
                if ("slice" %in% kinds) {
                  "slice"
                } else if ("missing" %in% kinds) {
                  "missing"
                } else {
                  "scalar"
                }
            } else {
                "scalar"
            }
            return(
                list(
                  kind = "inst", opcode = paste0("mem_store_index_", store_kind),
                  base = base, index_args = idx_args, value = value, args = args,
                  index_kinds = if (is.null(st$index_kinds)) character(0) else as.character(st$index_kinds)
              )
            )
        }

        if (identical(op, "store_subscript") ||
            identical(op_can, "mojor.mem.store_subscript")) {
            store_kind <- if (!is.null(st$index_kinds) &&
                length(st$index_kinds) >
                  0) {
                kinds <- unique(as.character(st$index_kinds))
                if ("slice" %in% kinds) {
                  "slice"
                } else if ("missing" %in% kinds) {
                  "missing"
                } else {
                  "scalar"
                }
            } else {
                "scalar"
            }
            return(
                list(
                  kind = "inst", opcode = paste0("mem_store_subscript_", store_kind),
                  target = if (is.null(st$target)) "" else as.character(st$target),
                  args = args, index_kinds = if (is.null(st$index_kinds)) character(0) else as.character(st$index_kinds)
              )
            )
        }

        if (startsWith(op, "store:") ||
            startsWith(op_can, "mojor.mem.store.")) {
            lhs_kind <- if (startsWith(op, "store:")) {
                .mojor_ir_backend_sanitize_opcode(sub("^store:", "", op))
            } else {
                .mojor_ir_backend_sanitize_opcode(sub("^mojor\\.mem\\.store\\.", "", op_can))
            }
            return(mk(paste0("mem_store_", lhs_kind)))
        }

        if (startsWith(op, "stmt:") ||
            startsWith(op_can, "mojor.misc.stmt.")) {
            stmt_kind <- if (startsWith(op, "stmt:")) {
                .mojor_ir_backend_sanitize_opcode(sub("^stmt:", "", op))
            } else {
                .mojor_ir_backend_sanitize_opcode(sub("^mojor\\.misc\\.stmt\\.", "", op_can))
            }
            return(mk(paste0("effect_stmt_", stmt_kind)))
        }
    }

    if (startsWith(op, "idx_")) {
        out <- mk(.mojor_ir_backend_sanitize_opcode(op))
        if (!is.null(st$index_kinds))
            out$index_kinds <- as.character(st$index_kinds)
        return(out)
    }

    out <- mk(.mojor_ir_backend_sanitize_opcode(op))
    if (!is.null(st$value))
        out$value <- as.character(st$value)
    if (!is.null(st$target))
        out$target <- as.character(st$target)
    if (!is.null(st$index_kinds))
        out$index_kinds <- as.character(st$index_kinds)
    out
}

.mojor_ir_backend_select_term <- function(term) {
    if (is.null(term) ||
        !is.list(term) ||
        is.null(term$kind)) {
        return(list(kind = "term", opcode = "term_invalid"))
    }

    if (identical(term$kind, "ret")) {
        out <- list(kind = "term", opcode = "ret")
        if (!is.null(term$value))
            out$value <- as.character(term$value)
        return(out)
    }
    if (identical(term$kind, "br")) {
        return(
            list(
                kind = "term", opcode = "br", target = as.character(term$target),
                args = if (is.null(term$args)) character(0) else as.character(term$args)
            )
        )
    }
    if (identical(term$kind, "condbr")) {
        return(
            list(
                kind = "term", opcode = "condbr", cond = as.character(term$cond),
                then = as.character(term$then),
                term_else = as.character(term[["else"]]),
                then_args = if (is.null(term$then_args)) character(0) else as.character(term$then_args),
                else_args = if (is.null(term$else_args)) character(0) else as.character(term$else_args)
            )
        )
    }

    list(
        kind = "term", opcode = paste0("term_", .mojor_ir_backend_sanitize_opcode(term$kind))
    )
}

.mojor_ir_ssa_backend_select <- function(prep_or_lowered_or_cfg, verify = TRUE, strict_schema = NULL) {
    lowered <- prep_or_lowered_or_cfg
    if (is.list(prep_or_lowered_or_cfg) &&
        is.null(prep_or_lowered_or_cfg$kind) &&
        !is.null(prep_or_lowered_or_cfg$backend_lowered)) {
        lowered <- prep_or_lowered_or_cfg$backend_lowered
    }
    if (is.null(lowered) ||
        !is.list(lowered) ||
        !identical(lowered$kind, "ssa_backend_lowered")) {
        lowered <- .mojor_ir_ssa_backend_lower(
            prep_or_lowered_or_cfg, verify = verify, strict_schema = strict_schema
        )
    }

    if (isTRUE(verify)) {
        if (is.null(lowered$entry) ||
            !is.character(lowered$entry) ||
            length(lowered$entry) !=
                1 || !nzchar(lowered$entry)) {
            stop("SSA backend select: lowered entry must be a non-empty string")
        }
        if (is.null(lowered$blocks) ||
            !is.list(lowered$blocks) ||
            length(lowered$blocks) ==
                0) {
            stop("SSA backend select: lowered blocks must be a non-empty list")
        }
        if (!(lowered$entry %in% names(lowered$blocks))) {
            stop("SSA backend select: lowered entry must reference an existing block")
        }
    }

    block_order <- lowered$block_order
    if (is.null(block_order) ||
        length(block_order) ==
            0) {
        block_order <- names(lowered$blocks)
    }
    block_order <- as.character(block_order)

    blocks <- setNames(
        vector("list", length(block_order)),
        block_order
    )
    instructions <- character(0)

    for (bn in block_order) {
        blk <- lowered$blocks[[bn]]
        params <- if (is.null(blk$params))
            character(0) else as.character(blk$params)
        param_repr <- if (is.null(blk$param_repr))
            params else as.character(blk$param_repr)
        successors <- if (is.null(blk$successors))
            character(0) else as.character(blk$successors)
        raw_stmts <- if (is.null(blk$ssa_stmts))
            list() else blk$ssa_stmts
        raw_term <- blk$ssa_term

        insts <- list()
        if (length(raw_stmts) >
            0) {
            for (st in raw_stmts) {
                inst <- .mojor_ir_backend_select_stmt(st)
                if (is.null(inst))
                  next
                if (is.list(inst) &&
                  !is.null(inst$kind) &&
                  identical(inst$kind, "inst_bundle") &&
                  !is.null(inst$instructions) &&
                  length(inst$instructions) >
                    0) {
                  insts <- c(insts, inst$instructions)
                } else {
                  insts <- c(insts, list(inst))
                }
            }
        }
        term_inst <- .mojor_ir_backend_select_term(raw_term)

        blocks[[bn]] <- list(
            name = bn, params = params, param_repr = param_repr, instructions = insts,
            terminator = term_inst, successors = successors
        )

        instructions <- c(
            instructions, paste0(
                "label ", bn, "(", paste(param_repr, collapse = ", "),
                ")"
            )
        )
        if (length(insts) >
            0) {
            for (inst in insts) {
                op <- as.character(inst$opcode)
                args <- if (is.null(inst$args) ||
                  length(inst$args) ==
                    0)
                  "" else paste(
                  as.character(inst$args),
                  collapse = ", "
              )
                lhs <- if (!is.null(inst$dst) &&
                  nzchar(as.character(inst$dst)))
                  paste0(
                    as.character(inst$dst),
                    " = "
                ) else ""
                tail <- character(0)
                if (!is.null(inst$value))
                  tail <- c(tail, paste0("value=", as.character(inst$value)))
                if (!is.null(inst$type))
                  tail <- c(tail, paste0("type=", as.character(inst$type)))
                if (!is.null(inst$cast_to))
                  tail <- c(tail, paste0("cast_to=", as.character(inst$cast_to)))
                if (!is.null(inst$callee))
                  tail <- c(tail, paste0("callee=", as.character(inst$callee)))
                if (!is.null(inst$base))
                  tail <- c(tail, paste0("base=", as.character(inst$base)))
                if (!is.null(inst$index_args) &&
                  length(inst$index_args) >
                    0) {
                  tail <- c(
                    tail, paste0(
                      "index_args=[", paste(
                        as.character(inst$index_args),
                        collapse = ","
                    ),
                      "]"
                  )
                )
                }
                if (!is.null(inst$reduce_mode))
                  tail <- c(tail, paste0("reduce_mode=", as.character(inst$reduce_mode)))
                if (!is.null(inst$reduce_op))
                  tail <- c(tail, paste0("reduce_op=", as.character(inst$reduce_op)))
                if (!is.null(inst$reduce_acc)) {
                  tail <- c(tail, paste0("acc=", as.character(inst$reduce_acc)))
                } else if (!is.null(inst[["acc"]]))
                  tail <- c(tail, paste0("acc=", as.character(inst[["acc"]])))
                if (!is.null(inst$reduce_arg)) {
                  tail <- c(tail, paste0("arg=", as.character(inst$reduce_arg)))
                } else if (!is.null(inst[["arg"]]))
                  tail <- c(tail, paste0("arg=", as.character(inst[["arg"]])))
                if (!is.null(inst$n_var))
                  tail <- c(tail, paste0("n_var=", as.character(inst$n_var)))
                if (!is.null(inst$dtype))
                  tail <- c(tail, paste0("dtype=", as.character(inst$dtype)))
                if (!is.null(inst$init_val))
                  tail <- c(tail, paste0("init_val=", as.character(inst$init_val)))
                if (!is.null(inst$empty_val))
                  tail <- c(tail, paste0("empty_val=", as.character(inst$empty_val)))
                if (!is.null(inst$nan_val))
                  tail <- c(tail, paste0("nan_val=", as.character(inst$nan_val)))
                if (!is.null(inst$value_cast))
                  tail <- c(tail, paste0("value_cast=", as.character(inst$value_cast)))
                if (!is.null(inst$merge_kind))
                  tail <- c(tail, paste0("merge_kind=", as.character(inst$merge_kind)))
                if (!is.null(inst$tie_break))
                  tail <- c(tail, paste0("tie_break=", as.character(inst$tie_break)))
                if (!is.null(inst$target))
                  tail <- c(tail, paste0("target=", as.character(inst$target)))
                if (!is.null(inst$index_kinds))
                  tail <- c(
                    tail, paste0(
                      "index_kinds=[", paste(
                        as.character(inst$index_kinds),
                        collapse = ","
                    ),
                      "]"
                  )
                )
                trailer <- if (length(tail) ==
                  0)
                  "" else paste0(" ", paste(tail, collapse = " "))
                instructions <- c(instructions, paste0("  ", lhs, op, "(", args, ")", trailer))
            }
        }
        term <- term_inst
        term_line <- if (identical(term$opcode, "ret")) {
            if (is.null(term$value) ||
                !nzchar(as.character(term$value)))
                "ret" else paste0("ret ", as.character(term$value))
        } else if (identical(term$opcode, "br")) {
            targs <- if (is.null(term$args) ||
                length(term$args) ==
                  0)
                "" else paste(
                as.character(term$args),
                collapse = ", "
            )
            paste0(
                "br ", as.character(term$target),
                "(", targs, ")"
            )
        } else if (identical(term$opcode, "condbr")) {
            ta <- if (is.null(term$then_args) ||
                length(term$then_args) ==
                  0)
                "" else paste(
                as.character(term$then_args),
                collapse = ", "
            )
            ea <- if (is.null(term$else_args) ||
                length(term$else_args) ==
                  0)
                "" else paste(
                as.character(term$else_args),
                collapse = ", "
            )
            paste0(
                "condbr ", as.character(term$cond),
                " ? ", as.character(term$then),
                "(", ta, ") : ", as.character(term$term_else),
                "(", ea, ")"
            )
        } else {
            as.character(term$opcode)
        }
        instructions <- c(instructions, paste0("  ", term_line))
        succ_line <- if (length(successors) ==
            0)
            "  succ: <none>" else paste0("  succ: ", paste(successors, collapse = ", "))
        instructions <- c(instructions, succ_line)
    }

    list(
        kind = "ssa_backend_selected", entry = lowered$entry, block_order = block_order,
        blocks = blocks, instructions = instructions
    )
}

.mojor_ir_backend_codegen_inst <- function(inst, target = "mojo_cpu") {
    if (is.null(inst) ||
        !is.list(inst) ||
        is.null(inst$opcode)) {
        return(NULL)
    }
    if (!identical(target, "mojo_cpu")) {
        stop(
            sprintf("SSA backend codegen: unsupported target '%s'", as.character(target))
        )
    }

    inst <- .mojor_ir_backend_selected_opcode_normalize(inst)

    src_opcode <- as.character(inst$opcode)
    args <- if (is.null(inst$args))
        character(0) else as.character(inst$args)
    out <- list(
        kind = "target_inst", opcode = paste0("cpu_", .mojor_ir_backend_sanitize_opcode(src_opcode)),
        args = args, source_opcode = src_opcode
    )

 # Reduction bundles carry scheduling metadata and lower to
 # mode/op-specific target ops.
    if (src_opcode %in% c("reduce_init", "reduce_step", "reduce_merge", "reduce_finalize")) {
        mode <- if (!is.null(inst$reduce_mode))
            as.character(inst$reduce_mode) else "linear"
        redop <- if (!is.null(inst$reduce_op))
            as.character(inst$reduce_op) else "sum"
        mode_tag <- .mojor_ir_backend_sanitize_opcode(mode)
        op_tag <- .mojor_ir_backend_sanitize_opcode(redop)
        out$opcode <- paste0(
            "cpu_", .mojor_ir_backend_sanitize_opcode(src_opcode),
            "_", mode_tag, "_", op_tag
        )
        if (identical(src_opcode, "reduce_merge")) {
            merge_tag <- if (!is.null(inst$merge_kind))
                .mojor_ir_backend_sanitize_opcode(inst$merge_kind) else "linear_fold"
            out$opcode <- paste0(out$opcode, "_", merge_tag)
            out$merge_kind <- merge_tag
        }
        if (!is.null(inst$reduce_mode))
            out$reduce_mode <- as.character(inst$reduce_mode)
        if (!is.null(inst$reduce_op))
            out$reduce_op <- as.character(inst$reduce_op)
        if (!is.null(inst$reduce_acc))
            out$reduce_acc <- as.character(inst$reduce_acc)
        if (!is.null(inst$reduce_arg))
            out$reduce_arg <- as.character(inst$reduce_arg)
        if (!is.null(inst$n_var))
            out$n_var <- as.character(inst$n_var)
        if (!is.null(inst$dtype))
            out$dtype <- as.character(inst$dtype)
        if (!is.null(inst$init_val))
            out$init_val <- as.character(inst$init_val)
        if (!is.null(inst$empty_val))
            out$empty_val <- as.character(inst$empty_val)
        if (!is.null(inst$nan_val))
            out$nan_val <- as.character(inst$nan_val)
        if (!is.null(inst$value_cast))
            out$value_cast <- as.character(inst$value_cast)
        if (!is.null(inst$tie_break))
            out$tie_break <- as.character(inst$tie_break)
    } else {
        if (!is.null(inst$value))
            out$value <- as.character(inst$value)
        if (!is.null(inst$type))
            out$type <- as.character(inst$type)
        if (!is.null(inst$cast_to))
            out$cast_to <- as.character(inst$cast_to)
        if (!is.null(inst$callee))
            out$callee <- as.character(inst$callee)
        if (!is.null(inst$base))
            out$base <- as.character(inst$base)
        if (!is.null(inst$index_args))
            out$index_args <- as.character(inst$index_args)
        if (!is.null(inst$target))
            out$target <- as.character(inst$target)
        if (!is.null(inst$index_kinds))
            out$index_kinds <- as.character(inst$index_kinds)
    }

    if (!is.null(inst$dst) &&
        nzchar(as.character(inst$dst)))
        out$dst <- as.character(inst$dst)
    out
}

.mojor_ir_backend_codegen_term <- function(term, target = "mojo_cpu") {
    if (is.null(term) ||
        !is.list(term) ||
        is.null(term$opcode)) {
        return(list(kind = "target_term", opcode = "cpu_term_invalid"))
    }
    if (!identical(target, "mojo_cpu")) {
        stop(
            sprintf("SSA backend codegen: unsupported target '%s'", as.character(target))
        )
    }

    opcode <- as.character(term$opcode)
    out <- list(
        kind = "target_term", opcode = paste0("cpu_", .mojor_ir_backend_sanitize_opcode(opcode))
    )

    if (identical(opcode, "ret")) {
        if (!is.null(term$value))
            out$value <- as.character(term$value)
    } else if (identical(opcode, "br")) {
        out$target <- as.character(term$target)
        out$args <- if (is.null(term$args))
            character(0) else as.character(term$args)
    } else if (identical(opcode, "condbr")) {
        out$cond <- as.character(term$cond)
        out$then <- as.character(term$then)
        out$term_else <- as.character(term$term_else)
        out$then_args <- if (is.null(term$then_args))
            character(0) else as.character(term$then_args)
        out$else_args <- if (is.null(term$else_args))
            character(0) else as.character(term$else_args)
    }

    out
}

.mojor_ir_ssa_backend_codegen <- function(
    prep_or_selected_or_lowered_or_cfg, target = "mojo_cpu", verify = TRUE,
    strict_schema = NULL
) {
    selected <- prep_or_selected_or_lowered_or_cfg
    if (is.list(prep_or_selected_or_lowered_or_cfg) &&
        is.null(prep_or_selected_or_lowered_or_cfg$kind) &&
        !is.null(prep_or_selected_or_lowered_or_cfg$backend_selected)) {
        selected <- prep_or_selected_or_lowered_or_cfg$backend_selected
    }
    if (is.null(selected) ||
        !is.list(selected) ||
        !identical(selected$kind, "ssa_backend_selected")) {
        selected <- .mojor_ir_ssa_backend_select(
            prep_or_selected_or_lowered_or_cfg, verify = verify, strict_schema = strict_schema
        )
    }

    if (isTRUE(verify)) {
        if (is.null(selected$entry) ||
            !is.character(selected$entry) ||
            length(selected$entry) !=
                1 || !nzchar(selected$entry)) {
            stop("SSA backend codegen: selected entry must be a non-empty string")
        }
        if (is.null(selected$blocks) ||
            !is.list(selected$blocks) ||
            length(selected$blocks) ==
                0) {
            stop("SSA backend codegen: selected blocks must be a non-empty list")
        }
        if (!(selected$entry %in% names(selected$blocks))) {
            stop(
                "SSA backend codegen: selected entry must reference an existing block"
            )
        }
    }

    block_order <- selected$block_order
    if (is.null(block_order) ||
        length(block_order) ==
            0) {
        block_order <- names(selected$blocks)
    }
    block_order <- as.character(block_order)

    blocks <- setNames(
        vector("list", length(block_order)),
        block_order
    )
    instructions <- character(0)

    for (bn in block_order) {
        blk <- selected$blocks[[bn]]
        params <- if (is.null(blk$params))
            character(0) else as.character(blk$params)
        param_repr <- if (is.null(blk$param_repr))
            params else as.character(blk$param_repr)
        successors <- if (is.null(blk$successors))
            character(0) else as.character(blk$successors)

        target_insts <- list()
        if (!is.null(blk$instructions) &&
            length(blk$instructions) >
                0) {
            for (inst in blk$instructions) {
                lowered_inst <- .mojor_ir_backend_codegen_inst(inst, target = target)
                if (is.null(lowered_inst))
                  next
                target_insts <- c(target_insts, list(lowered_inst))
            }
        }
        target_term <- .mojor_ir_backend_codegen_term(blk$terminator, target = target)

        blocks[[bn]] <- list(
            name = bn, params = params, param_repr = param_repr, instructions = target_insts,
            terminator = target_term, successors = successors
        )

        instructions <- c(
            instructions, paste0(
                "label ", bn, "(", paste(param_repr, collapse = ", "),
                ")"
            )
        )
        if (length(target_insts) >
            0) {
            for (inst in target_insts) {
                op <- as.character(inst$opcode)
                args <- if (is.null(inst$args) ||
                  length(inst$args) ==
                    0)
                  "" else paste(
                  as.character(inst$args),
                  collapse = ", "
              )
                lhs <- if (!is.null(inst$dst) &&
                  nzchar(as.character(inst$dst)))
                  paste0(
                    as.character(inst$dst),
                    " = "
                ) else ""
                tail <- character(0)
                if (!is.null(inst$source_opcode))
                  tail <- c(tail, paste0("source=", as.character(inst$source_opcode)))
                if (!is.null(inst$reduce_mode))
                  tail <- c(tail, paste0("reduce_mode=", as.character(inst$reduce_mode)))
                if (!is.null(inst$reduce_op))
                  tail <- c(tail, paste0("reduce_op=", as.character(inst$reduce_op)))
                if (!is.null(inst$reduce_acc))
                  tail <- c(tail, paste0("acc=", as.character(inst$reduce_acc)))
                if (!is.null(inst$reduce_arg))
                  tail <- c(tail, paste0("arg=", as.character(inst$reduce_arg)))
                if (!is.null(inst$n_var))
                  tail <- c(tail, paste0("n_var=", as.character(inst$n_var)))
                if (!is.null(inst$merge_kind))
                  tail <- c(tail, paste0("merge_kind=", as.character(inst$merge_kind)))
                if (!is.null(inst$tie_break))
                  tail <- c(tail, paste0("tie_break=", as.character(inst$tie_break)))
                trailer <- if (length(tail) ==
                  0)
                  "" else paste0(" ", paste(tail, collapse = " "))
                instructions <- c(instructions, paste0("  ", lhs, op, "(", args, ")", trailer))
            }
        }

        term <- target_term
        term_line <- if (identical(term$opcode, "cpu_ret")) {
            if (is.null(term$value) ||
                !nzchar(as.character(term$value)))
                "cpu_ret" else paste0("cpu_ret ", as.character(term$value))
        } else if (identical(term$opcode, "cpu_br")) {
            targs <- if (is.null(term$args) ||
                length(term$args) ==
                  0)
                "" else paste(
                as.character(term$args),
                collapse = ", "
            )
            paste0(
                "cpu_br ", as.character(term$target),
                "(", targs, ")"
            )
        } else if (identical(term$opcode, "cpu_condbr")) {
            ta <- if (is.null(term$then_args) ||
                length(term$then_args) ==
                  0)
                "" else paste(
                as.character(term$then_args),
                collapse = ", "
            )
            ea <- if (is.null(term$else_args) ||
                length(term$else_args) ==
                  0)
                "" else paste(
                as.character(term$else_args),
                collapse = ", "
            )
            paste0(
                "cpu_condbr ", as.character(term$cond),
                " ? ", as.character(term$then),
                "(", ta, ") : ", as.character(term$term_else),
                "(", ea, ")"
            )
        } else {
            as.character(term$opcode)
        }
        instructions <- c(instructions, paste0("  ", term_line))
        succ_line <- if (length(successors) ==
            0)
            "  succ: <none>" else paste0("  succ: ", paste(successors, collapse = ", "))
        instructions <- c(instructions, succ_line)
    }

    list(
        kind = "ssa_backend_codegen", target = as.character(target),
        entry = selected$entry, block_order = block_order, blocks = blocks,
        instructions = instructions
    )
}

.mojor_ir_prepare_ssa_backend <- function(
    ir_or_expr, layout_ctx = list(n_var = "n_i", type_env = NULL),
    schedule = NULL, opt_level = 2, ssa_passes = c("prune_unreachable", "copy_propagate", "prune_dead_stmts"),
    instrument_passes = TRUE, recanonicalize_loops = TRUE, fusion_analysis = TRUE,
    fusion_rewrite = FALSE, fusion_rewrite_mode = c("skeleton", "constrained"),
    enforce_typing_boundary = FALSE, backend_lower = FALSE, backend_select = FALSE,
    backend_codegen = FALSE, backend_target = "mojo_cpu", verify = TRUE,
    strict_schema = NULL
) {
    fusion_rewrite_mode <- match.arg(fusion_rewrite_mode)
    mark <- character(0)
    add_mark <- function(step) {
        mark <<- c(mark, step)
        invisible(NULL)
    }

    ir_in <- ir_or_expr
    if (!(is.list(ir_or_expr) &&
        !is.null(ir_or_expr$kind))) {
        add_mark("dump")
        ir_in <- .mojor_ir_dump(ir_or_expr)
    }
    if (is.null(ir_in) ||
        !is.list(ir_in) ||
        is.null(ir_in$kind)) {
        stop("SSA backend prep: input must be IR node or expression/function")
    }

    effective_layout_ctx <- layout_ctx
    if (!is.null(schedule) &&
        is.list(schedule)) {
        if (is.null(effective_layout_ctx$reduction_mode) &&
            !is.null(schedule$reduction) &&
            schedule$reduction %in% c("tree", "simd")) {
            effective_layout_ctx$reduction_mode <- schedule$reduction
        }
        if (is.null(effective_layout_ctx$n_var) &&
            !is.null(schedule$n_var)) {
            effective_layout_ctx$n_var <- schedule$n_var
        }
        if (is.null(effective_layout_ctx$type_env) &&
            !is.null(schedule$type_env)) {
            effective_layout_ctx$type_env <- schedule$type_env
        }
    }

    add_mark("normalize")
    ir_norm <- .mojor_ir_normalize(ir_in)
    if (isTRUE(verify)) {
        add_mark("verify_ir")
        .mojor_ir_verify(ir_norm)
    }

    add_mark("optimize_ir")
    ir_opt <- .mojor_ir_optimize(ir_norm, opt_level = opt_level)
    if (isTRUE(verify)) {
        add_mark("verify_ir")
        .mojor_ir_verify(ir_opt)
    }

    add_mark("lower_ir")
    ir_low <- .mojor_ir_lower(ir_opt, ctx = effective_layout_ctx)
    if (isTRUE(verify)) {
        add_mark("verify_ir")
        .mojor_ir_verify(ir_low)
    }

    add_mark("schedule_ir")
    ir_sched <- .mojor_ir_schedule(ir_low, schedule = schedule)
    if (isTRUE(verify)) {
        add_mark("verify_ir")
        .mojor_ir_verify(ir_sched)
    }

    add_mark("to_ssa")
    if (exists(".mojor_ir_structured_lower_to_ssa", mode = "function")) {
        ssa_raw <- .mojor_ir_structured_lower_to_ssa(
            ir_sched, verify_ir = FALSE, attach_loop_metadata = TRUE, strict_schema = strict_schema
        )
    } else {
        ssa_raw <- .mojor_ir_to_ssa(ir_sched)
    }
    verifier_reports <- list()
    loop_recovery <- NULL
    recanonicalize <- NULL
    fusion <- NULL
    fusion_rewrite_result <- NULL

    if (isTRUE(verify)) {
        add_mark("verify_ssa")
        .mojor_ir_verify_ssa(ssa_raw, strict_schema = strict_schema)
        add_mark("verify_boundary_post_structured_lowering")
        verifier_reports$post_structured_lowering <- .mojor_ssa_verify_or_stop(
            ssa_raw, mode = "boundary", boundary = "post_structured_lowering",
            strict_schema = strict_schema, where = "SSA boundary post_structured_lowering"
        )
    }

    add_mark("memory_canonicalize")
    ssa_mem <- .mojor_ssa_memory_canonicalize(ssa_raw, verify = verify, strict_schema = strict_schema)
    if (isTRUE(verify)) {
        add_mark("verify_boundary_post_memory_canonicalize")
        verifier_reports$post_memory_canonicalize <- .mojor_ssa_verify_or_stop(
            ssa_mem, mode = "boundary", boundary = "post_memory_canonicalize",
            opts = list(forbid_legacy_memory_spellings = TRUE),
            strict_schema = strict_schema, where = "SSA boundary post_memory_canonicalize"
        )
    }

    add_mark("annotate_effects_resources")
    ssa_effect <- .mojor_ssa_annotate_effects_resources(ssa_mem)
    if (isTRUE(verify)) {
        add_mark("verify_ssa")
        .mojor_ir_verify_ssa(ssa_effect, strict_schema = strict_schema)
    }

    add_mark("loop_recovery")
    if (exists(".mojor_loop_recovery_analysis", mode = "function")) {
        loop_recovery <- .mojor_loop_recovery_analysis(ssa_effect)
    } else {
        loop_recovery <- list(
            status = "Unavailable", boundary_verifier_enabled = FALSE,
            diagnostics = list()
        )
    }

    ssa_recanonical <- ssa_effect
    if (isTRUE(recanonicalize_loops) &&
        exists(".mojor_recanonicalize_loop_cfg", mode = "function")) {
        add_mark("recanonicalize_loop_cfg")
        recanonicalize <- .mojor_recanonicalize_loop_cfg(
            ssa_effect, recovery = loop_recovery, verify = verify, strict_schema = strict_schema
        )
        if (!is.null(recanonicalize$ssa))
            ssa_recanonical <- recanonicalize$ssa
        if (!is.null(recanonicalize$recovery))
            loop_recovery <- recanonicalize$recovery
        if (isTRUE(verify)) {
            add_mark("verify_boundary_post_recanonicalize")
            verifier_reports$post_recanonicalize <- .mojor_ssa_verify_or_stop(
                ssa_recanonical, mode = "boundary", boundary = "post_recanonicalize",
                opts = list(loop_recovery = loop_recovery),
                strict_schema = strict_schema, where = "SSA boundary post_recanonicalize"
            )
        }
    }

    ssa_for_typing <- ssa_recanonical
    if (isTRUE(fusion_analysis)) {
        add_mark("fusion_candidate_analysis")
        fusion_policy <- list(
            fusion_allow_control_flow_simple = isTRUE(effective_layout_ctx$fusion_allow_control_flow_simple),
            fusion_allow_broadcast_nd_identity = isTRUE(effective_layout_ctx$fusion_allow_broadcast_nd_identity)
        )
        fusion <- .mojor_ssa_fusion_candidate_analysis(ssa_recanonical, recovery = loop_recovery, policy = fusion_policy)
        ssa_for_typing <- .mojor_ssa_attach_fusion_candidate_analysis(ssa_recanonical, analysis = fusion, policy = fusion_policy)
        if (isTRUE(fusion_rewrite)) {
            add_mark("fusion_rewrite")
            fusion_rewrite_result <- if (identical(fusion_rewrite_mode, "constrained")) {
                .mojor_ssa_fusion_rewrite_constrained(
                  ssa_for_typing, analysis = fusion, recovery = loop_recovery,
                  policy = fusion_policy
              )
            } else {
                .mojor_ssa_fusion_rewrite_skeleton(
                  ssa_for_typing, analysis = fusion, recovery = loop_recovery,
                  policy = fusion_policy
              )
            }
            if (!is.null(fusion_rewrite_result$ssa) &&
                is.list(fusion_rewrite_result$ssa) &&
                identical(fusion_rewrite_result$ssa$kind, "ssa_fn")) {
                ssa_for_typing <- .mojor_ssa_attach_fusion_candidate_analysis(fusion_rewrite_result$ssa, analysis = fusion, policy = fusion_policy)
            }
        }
    }

    add_mark("typing")
    ssa_typed <- .mojor_ssa_annotate_types(ssa_for_typing)
    if (isTRUE(verify) &&
        isTRUE(enforce_typing_boundary)) {
        add_mark("verify_boundary_post_typing")
        v_opts <- list(require_typing = TRUE)
        if (!is.null(loop_recovery))
            v_opts$loop_recovery <- loop_recovery
        verifier_reports$post_typing <- .mojor_ssa_verify_or_stop(
            ssa_typed, mode = "boundary", boundary = "post_typing", opts = v_opts,
            strict_schema = strict_schema, where = "SSA boundary post_typing"
        )
    } else if (isTRUE(verify)) {
        add_mark("verify_ssa")
        .mojor_ir_verify_ssa(ssa_typed, strict_schema = strict_schema)
    }

    add_mark("optimize_ssa")
    ssa_opt_result <- .mojor_ir_ssa_optimize(
        ssa_typed, passes = ssa_passes, verify = verify, trace = isTRUE(instrument_passes),
        strict_schema = strict_schema
    )
    ssa_opt <- ssa_opt_result
    pass_trace <- NULL
    if (isTRUE(instrument_passes) &&
        is.list(ssa_opt_result) &&
        !is.null(ssa_opt_result$ssa)) {
        ssa_opt <- ssa_opt_result$ssa
        pass_trace <- ssa_opt_result$pass_trace
    }
    if (isTRUE(verify)) {
        add_mark("verify_ssa")
        .mojor_ir_verify_ssa(ssa_opt, strict_schema = strict_schema)
    }

    add_mark("backend_cfg")
    cfg <- .mojor_ir_ssa_backend_cfg(ssa_opt, verify = verify, strict_schema = strict_schema)
    lowered <- NULL
    selected <- NULL
    codegen <- NULL
    need_select <- isTRUE(backend_select) ||
        isTRUE(backend_codegen)
    need_lower <- isTRUE(backend_lower) ||
        need_select
    if (need_lower) {
        add_mark("lower_backend")
        lowered <- .mojor_ir_ssa_backend_lower(cfg, verify = verify, strict_schema = strict_schema)
    }
    if (need_select) {
        add_mark("select_backend")
        selected <- .mojor_ir_ssa_backend_select(lowered, verify = verify, strict_schema = strict_schema)
    }
    if (isTRUE(backend_codegen)) {
        add_mark("codegen_backend")
        codegen <- .mojor_ir_ssa_backend_codegen(
            selected, target = backend_target, verify = verify, strict_schema = strict_schema
        )
    }

    out <- list(
        pipeline = mark, ir = list(
            input = ir_in, normalized = ir_norm, optimized = ir_opt, lowered = ir_low,
            scheduled = ir_sched
        ),
        ssa = list(
            raw = ssa_raw, memory_canonical = ssa_mem, effect_annotated = ssa_effect,
            loop_recovery = loop_recovery, recanonicalized = ssa_recanonical,
            typed = ssa_typed, fusion_analysis = fusion, fusion_rewrite = fusion_rewrite_result,
            optimized = ssa_opt, pass_trace = pass_trace
        ),
        verifier = list(boundary = verifier_reports),
        recanonicalize = recanonicalize, backend_cfg = cfg, backend_lowered = lowered,
        backend_selected = selected, backend_codegen = codegen
    )

    .mojor_ir_check_ssa_backend_invariants(out)
    out
}

.mojor_ir_ssa_backend_format <- function(
    prep_or_cfg_or_lowered, verify = TRUE, as_lines = FALSE, strict_schema = NULL
) {
    lowered <- prep_or_cfg_or_lowered
    if (is.null(lowered) ||
        !is.list(lowered) ||
        !identical(lowered$kind, "ssa_backend_lowered")) {
        lowered <- .mojor_ir_ssa_backend_lower(
            prep_or_cfg_or_lowered, verify = verify, strict_schema = strict_schema
        )
    }

    lines <- c(paste0("ssa_backend entry=", lowered$entry))
    for (bn in lowered$block_order) {
        blk <- lowered$blocks[[bn]]
        params <- if (is.null(blk$param_repr))
            character(0) else as.character(blk$param_repr)
        lines <- c(
            lines, paste0(
                bn, "(", paste(params, collapse = ", "),
                "):"
            )
        )
        if (!is.null(blk$stmts) &&
            length(blk$stmts) >
                0) {
            lines <- c(lines, paste0("  ", as.character(blk$stmts)))
        }
        lines <- c(lines, paste0("  ", as.character(blk$term)))
        succ <- if (is.null(blk$successors))
            character(0) else as.character(blk$successors)
        succ_line <- if (length(succ) ==
            0)
            "  succ: <none>" else paste0("  succ: ", paste(succ, collapse = ", "))
        lines <- c(lines, succ_line)
    }

    if (isTRUE(as_lines)) {
        return(lines)
    }
    paste(lines, collapse = "\n")
}

.mojor_ir_ssa_backend_selected_format <- function(
    prep_or_lowered_or_selected, verify = TRUE, as_lines = FALSE, strict_schema = NULL
) {
    selected <- prep_or_lowered_or_selected
    if (is.null(selected) ||
        !is.list(selected) ||
        !identical(selected$kind, "ssa_backend_selected")) {
        selected <- .mojor_ir_ssa_backend_select(
            prep_or_lowered_or_selected, verify = verify, strict_schema = strict_schema
        )
    }

    lines <- c(paste0("ssa_backend_selected entry=", selected$entry))
    for (bn in selected$block_order) {
        blk <- selected$blocks[[bn]]
        params <- if (is.null(blk$param_repr))
            character(0) else as.character(blk$param_repr)
        lines <- c(
            lines, paste0(
                bn, "(", paste(params, collapse = ", "),
                "):"
            )
        )

        if (!is.null(blk$instructions) &&
            length(blk$instructions) >
                0) {
            for (inst in blk$instructions) {
                op <- as.character(inst$opcode)
                args <- if (is.null(inst$args) ||
                  length(inst$args) ==
                    0)
                  "" else paste(
                  as.character(inst$args),
                  collapse = ", "
              )
                lhs <- if (!is.null(inst$dst) &&
                  nzchar(as.character(inst$dst)))
                  paste0(
                    as.character(inst$dst),
                    " = "
                ) else ""
                tail <- character(0)
                if (!is.null(inst$value))
                  tail <- c(tail, paste0("value=", as.character(inst$value)))
                if (!is.null(inst$type))
                  tail <- c(tail, paste0("type=", as.character(inst$type)))
                if (!is.null(inst$cast_to))
                  tail <- c(tail, paste0("cast_to=", as.character(inst$cast_to)))
                if (!is.null(inst$callee))
                  tail <- c(tail, paste0("callee=", as.character(inst$callee)))
                if (!is.null(inst$base))
                  tail <- c(tail, paste0("base=", as.character(inst$base)))
                if (!is.null(inst$index_args) &&
                  length(inst$index_args) >
                    0) {
                  tail <- c(
                    tail, paste0(
                      "index_args=[", paste(
                        as.character(inst$index_args),
                        collapse = ","
                    ),
                      "]"
                  )
                )
                }
                if (!is.null(inst$reduce_mode))
                  tail <- c(tail, paste0("reduce_mode=", as.character(inst$reduce_mode)))
                if (!is.null(inst$reduce_op))
                  tail <- c(tail, paste0("reduce_op=", as.character(inst$reduce_op)))
                if (!is.null(inst$reduce_acc)) {
                  tail <- c(tail, paste0("acc=", as.character(inst$reduce_acc)))
                } else if (!is.null(inst[["acc"]]))
                  tail <- c(tail, paste0("acc=", as.character(inst[["acc"]])))
                if (!is.null(inst$reduce_arg)) {
                  tail <- c(tail, paste0("arg=", as.character(inst$reduce_arg)))
                } else if (!is.null(inst[["arg"]]))
                  tail <- c(tail, paste0("arg=", as.character(inst[["arg"]])))
                if (!is.null(inst$n_var))
                  tail <- c(tail, paste0("n_var=", as.character(inst$n_var)))
                if (!is.null(inst$dtype))
                  tail <- c(tail, paste0("dtype=", as.character(inst$dtype)))
                if (!is.null(inst$init_val))
                  tail <- c(tail, paste0("init_val=", as.character(inst$init_val)))
                if (!is.null(inst$empty_val))
                  tail <- c(tail, paste0("empty_val=", as.character(inst$empty_val)))
                if (!is.null(inst$nan_val))
                  tail <- c(tail, paste0("nan_val=", as.character(inst$nan_val)))
                if (!is.null(inst$value_cast))
                  tail <- c(tail, paste0("value_cast=", as.character(inst$value_cast)))
                if (!is.null(inst$merge_kind))
                  tail <- c(tail, paste0("merge_kind=", as.character(inst$merge_kind)))
                if (!is.null(inst$tie_break))
                  tail <- c(tail, paste0("tie_break=", as.character(inst$tie_break)))
                if (!is.null(inst$target))
                  tail <- c(tail, paste0("target=", as.character(inst$target)))
                if (!is.null(inst$index_kinds))
                  tail <- c(
                    tail, paste0(
                      "index_kinds=[", paste(
                        as.character(inst$index_kinds),
                        collapse = ","
                    ),
                      "]"
                  )
                )
                trailer <- if (length(tail) ==
                  0)
                  "" else paste0(" ", paste(tail, collapse = " "))
                lines <- c(lines, paste0("  ", lhs, op, "(", args, ")", trailer))
            }
        }

        term <- blk$terminator
        term_line <- if (identical(term$opcode, "ret")) {
            if (is.null(term$value) ||
                !nzchar(as.character(term$value)))
                "ret" else paste0("ret ", as.character(term$value))
        } else if (identical(term$opcode, "br")) {
            targs <- if (is.null(term$args) ||
                length(term$args) ==
                  0)
                "" else paste(
                as.character(term$args),
                collapse = ", "
            )
            paste0(
                "br ", as.character(term$target),
                "(", targs, ")"
            )
        } else if (identical(term$opcode, "condbr")) {
            ta <- if (is.null(term$then_args) ||
                length(term$then_args) ==
                  0)
                "" else paste(
                as.character(term$then_args),
                collapse = ", "
            )
            ea <- if (is.null(term$else_args) ||
                length(term$else_args) ==
                  0)
                "" else paste(
                as.character(term$else_args),
                collapse = ", "
            )
            paste0(
                "condbr ", as.character(term$cond),
                " ? ", as.character(term$then),
                "(", ta, ") : ", as.character(term$term_else),
                "(", ea, ")"
            )
        } else {
            as.character(term$opcode)
        }
        lines <- c(lines, paste0("  ", term_line))
        succ <- if (is.null(blk$successors))
            character(0) else as.character(blk$successors)
        succ_line <- if (length(succ) ==
            0)
            "  succ: <none>" else paste0("  succ: ", paste(succ, collapse = ", "))
        lines <- c(lines, succ_line)
    }

    if (isTRUE(as_lines)) {
        return(lines)
    }
    paste(lines, collapse = "\n")
}

.mojor_ir_ssa_backend_codegen_format <- function(
    prep_or_selected_or_codegen, target = "mojo_cpu", verify = TRUE, as_lines = FALSE,
    strict_schema = NULL
) {
    codegen <- prep_or_selected_or_codegen
    if (is.list(prep_or_selected_or_codegen) &&
        is.null(prep_or_selected_or_codegen$kind) &&
        !is.null(prep_or_selected_or_codegen$backend_codegen)) {
        codegen <- prep_or_selected_or_codegen$backend_codegen
    }
    if (is.null(codegen) ||
        !is.list(codegen) ||
        !identical(codegen$kind, "ssa_backend_codegen")) {
        codegen <- .mojor_ir_ssa_backend_codegen(
            prep_or_selected_or_codegen, target = target, verify = verify,
            strict_schema = strict_schema
        )
    }

    lines <- c(
        paste0(
            "ssa_backend_codegen target=", as.character(codegen$target),
            " entry=", codegen$entry
        )
    )
    for (bn in codegen$block_order) {
        blk <- codegen$blocks[[bn]]
        params <- if (is.null(blk$param_repr))
            character(0) else as.character(blk$param_repr)
        lines <- c(
            lines, paste0(
                bn, "(", paste(params, collapse = ", "),
                "):"
            )
        )

        if (!is.null(blk$instructions) &&
            length(blk$instructions) >
                0) {
            for (inst in blk$instructions) {
                op <- as.character(inst$opcode)
                args <- if (is.null(inst$args) ||
                  length(inst$args) ==
                    0)
                  "" else paste(
                  as.character(inst$args),
                  collapse = ", "
              )
                lhs <- if (!is.null(inst$dst) &&
                  nzchar(as.character(inst$dst)))
                  paste0(
                    as.character(inst$dst),
                    " = "
                ) else ""
                tail <- character(0)
                if (!is.null(inst$source_opcode))
                  tail <- c(tail, paste0("source=", as.character(inst$source_opcode)))
                if (!is.null(inst$reduce_mode))
                  tail <- c(tail, paste0("reduce_mode=", as.character(inst$reduce_mode)))
                if (!is.null(inst$reduce_op))
                  tail <- c(tail, paste0("reduce_op=", as.character(inst$reduce_op)))
                if (!is.null(inst$reduce_acc))
                  tail <- c(tail, paste0("acc=", as.character(inst$reduce_acc)))
                if (!is.null(inst$reduce_arg))
                  tail <- c(tail, paste0("arg=", as.character(inst$reduce_arg)))
                if (!is.null(inst$n_var))
                  tail <- c(tail, paste0("n_var=", as.character(inst$n_var)))
                if (!is.null(inst$merge_kind))
                  tail <- c(tail, paste0("merge_kind=", as.character(inst$merge_kind)))
                if (!is.null(inst$tie_break))
                  tail <- c(tail, paste0("tie_break=", as.character(inst$tie_break)))
                trailer <- if (length(tail) ==
                  0)
                  "" else paste0(" ", paste(tail, collapse = " "))
                lines <- c(lines, paste0("  ", lhs, op, "(", args, ")", trailer))
            }
        }

        term <- blk$terminator
        term_line <- if (identical(term$opcode, "cpu_ret")) {
            if (is.null(term$value) ||
                !nzchar(as.character(term$value)))
                "cpu_ret" else paste0("cpu_ret ", as.character(term$value))
        } else if (identical(term$opcode, "cpu_br")) {
            targs <- if (is.null(term$args) ||
                length(term$args) ==
                  0)
                "" else paste(
                as.character(term$args),
                collapse = ", "
            )
            paste0(
                "cpu_br ", as.character(term$target),
                "(", targs, ")"
            )
        } else if (identical(term$opcode, "cpu_condbr")) {
            ta <- if (is.null(term$then_args) ||
                length(term$then_args) ==
                  0)
                "" else paste(
                as.character(term$then_args),
                collapse = ", "
            )
            ea <- if (is.null(term$else_args) ||
                length(term$else_args) ==
                  0)
                "" else paste(
                as.character(term$else_args),
                collapse = ", "
            )
            paste0(
                "cpu_condbr ", as.character(term$cond),
                " ? ", as.character(term$then),
                "(", ta, ") : ", as.character(term$term_else),
                "(", ea, ")"
            )
        } else {
            as.character(term$opcode)
        }
        lines <- c(lines, paste0("  ", term_line))
        succ <- if (is.null(blk$successors))
            character(0) else as.character(blk$successors)
        succ_line <- if (length(succ) ==
            0)
            "  succ: <none>" else paste0("  succ: ", paste(succ, collapse = ", "))
        lines <- c(lines, succ_line)
    }

    if (isTRUE(as_lines)) {
        return(lines)
    }
    paste(lines, collapse = "\n")
}

.mojor_ir_ssa_backend_dump <- function(
    expr_or_fn, layout_ctx = list(n_var = "n_i", type_env = NULL),
    schedule = NULL, opt_level = 2, ssa_passes = c("prune_unreachable", "copy_propagate", "prune_dead_stmts"),
    instrument_passes = TRUE, backend_select = FALSE, backend_codegen = FALSE,
    backend_target = "mojo_cpu", verify = TRUE, strict_schema = NULL
) {
    prep <- .mojor_ir_prepare_ssa_backend(
        expr_or_fn, layout_ctx = layout_ctx, schedule = schedule, opt_level = opt_level,
        ssa_passes = ssa_passes, instrument_passes = instrument_passes,
        backend_lower = TRUE, backend_select = backend_select || backend_codegen,
        backend_codegen = backend_codegen, backend_target = backend_target,
        verify = verify, strict_schema = strict_schema
    )
    lowered <- prep$backend_lowered
    if (is.null(lowered))
        lowered <- .mojor_ir_ssa_backend_lower(prep, verify = verify, strict_schema = strict_schema)
    selected <- prep$backend_selected
    if (isTRUE(backend_select) &&
        is.null(selected)) {
        selected <- .mojor_ir_ssa_backend_select(lowered, verify = verify, strict_schema = strict_schema)
    }
    codegen <- prep$backend_codegen
    if (isTRUE(backend_codegen) &&
        is.null(codegen)) {
        codegen <- .mojor_ir_ssa_backend_codegen(
            selected, target = backend_target, verify = verify, strict_schema = strict_schema
        )
    }
    list(
        prep = prep, lowered = lowered, selected = selected, codegen = codegen,
        formatted = .mojor_ir_ssa_backend_format(lowered, verify = FALSE),
        formatted_selected = if (!is.null(selected)) .mojor_ir_ssa_backend_selected_format(selected, verify = FALSE) else NULL,
        formatted_codegen = if (!is.null(codegen)) .mojor_ir_ssa_backend_codegen_format(codegen, target = backend_target, verify = FALSE) else NULL
    )
}
