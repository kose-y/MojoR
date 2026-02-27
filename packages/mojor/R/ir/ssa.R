# =============================================================================
# Step 25: SSA Skeleton (Structured IR -> Basic Blocks + Block
# Params)
# =============================================================================

.mojor_ssa_block <- function(name, params = list(), stmts = list(), term = NULL) {
    list(
        kind = "ssa_block", name = name, params = params, stmts = stmts,
        term = term
    )
}

.mojor_ssa_br <- function(target, args = character(0)) {
    list(kind = "br", target = target, args = as.character(args))
}

.mojor_ssa_condbr <- function(
    cond, then, else_target, then_args = character(0),
    else_args = character(0)
) {
    list(
        kind = "condbr", cond = cond, then = then, `else` = else_target,
        then_args = as.character(then_args),
        else_args = as.character(else_args)
    )
}

.mojor_ssa_ret <- function(value = NULL) {
    list(kind = "ret", value = value)
}

.mojor_ssa_is_value_ref <- function(x) {
    is.character(x) &&
        length(x) ==
            1 && grepl("^%(v|arg)[0-9]+$", x)
}

.mojor_ir_collect_assigned_vars <- function(node) {
    if (is.null(node) ||
        !is.list(node) ||
        is.null(node$kind)) {
        return(character(0))
    }
    kind <- if (exists(".mojor_ir_base_kind", mode = "function"))
        .mojor_ir_base_kind(node$kind) else node$kind
    if (kind == "assign") {
        lhs <- node$lhs
        if (!is.null(lhs) &&
            !is.null(lhs$kind) &&
            lhs$kind == "var") {
            return(lhs$name)
        }
        return(character(0))
    }
    if (kind == "block") {
        out <- character(0)
        for (s in node$stmts) out <- c(out, .mojor_ir_collect_assigned_vars(s))
        return(unique(out))
    }
    if (kind == "if") {
        out <- c(
            .mojor_ir_collect_assigned_vars(node$then),
            .mojor_ir_collect_assigned_vars(node$else_block)
        )
        return(unique(out))
    }
    if (kind %in% c("loop", "while", "repeat")) {
        return(.mojor_ir_collect_assigned_vars(node$body))
    }
    character(0)
}

.mojor_ir_loop_noalias_write_bases <- function(loop_node) {
    out <- list(provable = FALSE, bases = character(0))
    if (is.null(loop_node) ||
        !is.list(loop_node) ||
        is.null(loop_node$kind)) {
        return(out)
    }
    kind <- if (exists(".mojor_ir_base_kind", mode = "function"))
        .mojor_ir_base_kind(loop_node$kind) else loop_node$kind
    if (!identical(kind, "loop")) {
        return(out)
    }
    body <- loop_node$body
    if (is.null(body) ||
        !is.list(body) ||
        is.null(body$kind)) {
        return(out)
    }
    body_kind <- if (exists(".mojor_ir_base_kind", mode = "function"))
        .mojor_ir_base_kind(body$kind) else body$kind
    if (!identical(body_kind, "block") ||
        is.null(body$stmts)) {
        return(out)
    }

    bases <- character(0)
    for (st in body$stmts) {
        if (is.null(st) ||
            !is.list(st) ||
            is.null(st$kind))
            next
        st_kind <- if (exists(".mojor_ir_base_kind", mode = "function"))
            .mojor_ir_base_kind(st$kind) else st$kind
        if (!identical(st_kind, "assign"))
            next
        lhs <- st$lhs
        if (is.null(lhs) ||
            !is.list(lhs) ||
            is.null(lhs$kind))
            next
        lhs_kind <- if (exists(".mojor_ir_base_kind", mode = "function"))
            .mojor_ir_base_kind(lhs$kind) else lhs$kind
        if (!identical(lhs_kind, "index"))
            next
        base <- lhs$base
        if (is.null(base) ||
            !is.list(base) ||
            is.null(base$kind)) {
            return(out)
        }
        base_kind <- if (exists(".mojor_ir_base_kind", mode = "function"))
            .mojor_ir_base_kind(base$kind) else base$kind
        if (!identical(base_kind, "var") ||
            is.null(base$name) ||
            !nzchar(as.character(base$name))) {
            return(out)
        }
        bases <- c(bases, as.character(base$name))
    }

    bases <- unique(as.character(bases))
    if (length(bases) ==
        0L) {
        return(out)
    }
    list(provable = TRUE, bases = bases)
}

.mojor_ir_mark_loop_noalias_proven <- function(loop_node) {
    if (is.null(loop_node) ||
        !is.list(loop_node)) {
        return(loop_node)
    }
    out <- loop_node

    ia <- if (!is.null(out$intent_attrs) &&
        is.list(out$intent_attrs))
        out$intent_attrs else list()
    if (!identical(ia$fusion_noalias_proven, FALSE))
        ia$fusion_noalias_proven <- TRUE
    out$intent_attrs <- ia

    md <- if (!is.null(out$metadata) &&
        is.list(out$metadata))
        out$metadata else list()
    if (!identical(md$fusion_noalias_proven, FALSE))
        md$fusion_noalias_proven <- TRUE
    out$metadata <- md
    out
}

.mojor_ir_annotate_fusion_noalias_intent <- function(node) {
    if (is.null(node) ||
        !is.list(node) ||
        is.null(node$kind)) {
        return(node)
    }
    kind <- if (exists(".mojor_ir_base_kind", mode = "function"))
        .mojor_ir_base_kind(node$kind) else node$kind

    if (identical(kind, "if")) {
        out_if <- node
        out_if$then <- .mojor_ir_annotate_fusion_noalias_intent(out_if$then)
        if (!is.null(out_if$else_block))
            out_if$else_block <- .mojor_ir_annotate_fusion_noalias_intent(out_if$else_block)
        return(out_if)
    }
    if (kind %in% c("loop", "while", "repeat")) {
        out_loop <- node
        if (!is.null(out_loop$body))
            out_loop$body <- .mojor_ir_annotate_fusion_noalias_intent(out_loop$body)
        return(out_loop)
    }
    if (!identical(kind, "block")) {
        return(node)
    }

    out <- node
    if (is.null(out$stmts) ||
        length(out$stmts) ==
            0L) {
        return(out)
    }
    stmts <- lapply(out$stmts, .mojor_ir_annotate_fusion_noalias_intent)

    is_loop_stmt <- function(st) {
        if (is.null(st) ||
            !is.list(st) ||
            is.null(st$kind)) {
            return(FALSE)
        }
        st_kind <- if (exists(".mojor_ir_base_kind", mode = "function"))
            .mojor_ir_base_kind(st$kind) else st$kind
        identical(st_kind, "loop")
    }

    if (length(stmts) >=
        2L) {
        for (i in seq_len(
            length(stmts) -
                1L
        )) {
            left <- stmts[[i]]
            right <- stmts[[i + 1L]]
            if (!isTRUE(is_loop_stmt(left)) ||
                !isTRUE(is_loop_stmt(right)))
                next
            left_w <- .mojor_ir_loop_noalias_write_bases(left)
            right_w <- .mojor_ir_loop_noalias_write_bases(right)
            if (!isTRUE(left_w$provable) ||
                !isTRUE(right_w$provable))
                next
            if (length(intersect(left_w$bases, right_w$bases)) !=
                0L)
                next
            stmts[[i]] <- .mojor_ir_mark_loop_noalias_proven(left)
            stmts[[i + 1L]] <- .mojor_ir_mark_loop_noalias_proven(right)
        }
    }

    out$stmts <- stmts
    out
}

.mojor_ir_to_ssa <- function(node) {
    if (is.null(node) ||
        !is.list(node) ||
        is.null(node$kind)) {
        return(NULL)
    }

    if (node$kind != "block") {
        node <- .mojor_ir_block(list(node))
    }
    node <- .mojor_ir_annotate_fusion_noalias_intent(node)

    st <- new.env(parent = emptyenv())
    st$next_value <- 0L
    st$next_block <- 0L
    st$blocks <- list()
    st$current <- NULL
    st$entry <- NULL
    st$entry_params <- list()
    st$entry_param_map <- list()
    st$next_loop_tag <- 0L

    new_value <- function() {
        st$next_value <- st$next_value + 1L
        paste0("%v", st$next_value)
    }

    new_block <- function(prefix = "bb") {
        st$next_block <- st$next_block + 1L
        paste0(prefix, st$next_block)
    }

    ensure_block <- function(name) {
        if (is.null(st$blocks[[name]])) {
            st$blocks[[name]] <- .mojor_ssa_block(name = name)
        }
    }

    set_current <- function(name) {
        ensure_block(name)
        st$current <- name
    }

    append_stmt <- function(stmt) {
        if (is.null(st$current))
            stop("SSA lower: no current block")
        blk <- st$blocks[[st$current]]
        blk$stmts <- c(blk$stmts, list(stmt))
        st$blocks[[st$current]] <- blk
    }

    has_term <- function(name = st$current) {
        if (is.null(name)) {
            return(FALSE)
        }
        blk <- st$blocks[[name]]
        !is.null(blk$term) &&
            is.list(blk$term) &&
            !is.null(blk$term$kind)
    }

    set_term <- function(term) {
        if (is.null(st$current))
            stop("SSA lower: no current block")
        blk <- st$blocks[[st$current]]
        if (!is.null(blk$term)) {
            return(invisible(NULL))
        }
        blk$term <- term
        st$blocks[[st$current]] <- blk
        invisible(NULL)
    }

    add_entry_param <- function(var_name, env_map) {
        existing <- st$entry_param_map[[var_name]]
        if (!is.null(existing)) {
            env_map[[var_name]] <- existing
            return(env_map)
        }
        id <- new_value()
        st$entry_params <- c(st$entry_params, list(list(id = id, var = var_name)))
        st$entry_param_map[[var_name]] <- id
        env_map[[var_name]] <- id
        env_map
    }

    base_kind <- function(kind) {
        if (exists(".mojor_ir_base_kind", mode = "function")) {
            return(.mojor_ir_base_kind(kind))
        }
        kind
    }

    new_loop_id <- function(prefix = "for") {
        st$next_loop_tag <- st$next_loop_tag + 1L
        paste0(
            as.character(prefix),
            "_", st$next_loop_tag
        )
    }

    merge_attrs <- function(existing, extra) {
        out <- if (is.null(existing) ||
            !is.list(existing))
            list() else existing
        if (is.null(extra) ||
            !is.list(extra) ||
            length(extra) ==
                0) {
            return(out)
        }
        out[names(extra)] <- extra
        out
    }

    structured_intent_attrs <- function(stmt) {
        attrs <- if (!is.null(stmt$attrs) &&
            is.list(stmt$attrs))
            stmt$attrs else list()
        if (!is.null(stmt$intent_attrs) &&
            is.list(stmt$intent_attrs)) {
            attrs <- merge_attrs(attrs, stmt$intent_attrs)
        }
        attrs
    }

    apply_loop_role_attrs <- function(
        block_name, loop_id, loop_kind, role, intent_attrs = NULL, extra = NULL
    ) {
        ensure_block(block_name)
        blk <- st$blocks[[block_name]]
        merged <- merge_attrs(blk$attrs, intent_attrs)
        role_attrs <- list(
            loop_id = as.character(loop_id),
            loop_kind = as.character(loop_kind),
            loop_role = as.character(role)
        )
        if (!is.null(extra) &&
            is.list(extra) &&
            length(extra) >
                0) {
            role_attrs <- c(role_attrs, extra)
        }
        merged <- merge_attrs(merged, role_attrs)
        blk$attrs <- merged
        st$blocks[[block_name]] <- blk
        invisible(NULL)
    }

    lower_index_component <- function(idx_node, env_map) {
        if (is.null(idx_node) ||
            !is.list(idx_node) ||
            is.null(idx_node$kind)) {
            part <- lower_expr(idx_node, env_map)
            return(list(value = part$value, env = part$env, idx_kind = "scalar"))
        }

        if (idx_node$kind == "scalar_index") {
            part <- lower_expr(idx_node$expr, env_map)
            return(list(value = part$value, env = part$env, idx_kind = "scalar"))
        }

        if (idx_node$kind == "missing_index") {
            id <- new_value()
            append_stmt(
                list(kind = "ssa_stmt", id = id, op = "idx_missing", args = character(0))
            )
            return(list(value = id, env = env_map, idx_kind = "missing"))
        }

        if (idx_node$kind == "slice_index") {
            cur_env <- env_map
            parts <- character(0)
            if (!is.null(idx_node$start)) {
                lo <- lower_expr(idx_node$start, cur_env)
                cur_env <- lo$env
                parts <- c(parts, lo$value)
            }
            if (!is.null(idx_node$end)) {
                hi <- lower_expr(idx_node$end, cur_env)
                cur_env <- hi$env
                parts <- c(parts, hi$value)
            }
            id <- new_value()
            append_stmt(list(kind = "ssa_stmt", id = id, op = "idx_slice", args = parts))
            return(list(value = id, env = cur_env, idx_kind = "slice"))
        }

        part <- lower_expr(idx_node, env_map)
        list(value = part$value, env = part$env, idx_kind = "scalar")
    }

    lower_expr <- function(expr, env_map) {
        if (is.null(expr) ||
            !is.list(expr) ||
            is.null(expr$kind)) {
            id <- new_value()
            append_stmt(
                list(kind = "ssa_stmt", id = id, op = "const:null", args = character(0))
            )
            return(list(value = id, env = env_map))
        }
        kind <- expr$kind

        if (kind == "const") {
            id <- new_value()
            append_stmt(
                list(
                  kind = "ssa_stmt", id = id, op = "const", value = expr$value,
                  args = character(0)
              )
            )
            return(list(value = id, env = env_map))
        }

        if (kind == "var") {
            nm <- expr$name
            if (is.null(env_map[[nm]])) {
                env_map <- add_entry_param(nm, env_map)
            }
            return(list(value = env_map[[nm]], env = env_map))
        }

        if (kind == "scalar_index" || kind == "slice_index" || kind ==
            "missing_index") {
            idx <- lower_index_component(expr, env_map)
            return(list(value = idx$value, env = idx$env))
        }

        if (kind == "unop") {
            inner <- lower_expr(expr$expr, env_map)
            env_map <- inner$env
            id <- new_value()
            append_stmt(
                list(
                  kind = "ssa_stmt", id = id, op = paste0("unop:", expr$op),
                  args = c(inner$value)
              )
            )
            return(list(value = id, env = env_map))
        }

        if (kind == "binop") {
            lhs <- lower_expr(expr$lhs, env_map)
            rhs <- lower_expr(expr$rhs, lhs$env)
            env_map <- rhs$env
            id <- new_value()
            append_stmt(
                list(
                  kind = "ssa_stmt", id = id, op = paste0("binop:", expr$op),
                  args = c(lhs$value, rhs$value)
              )
            )
            return(list(value = id, env = env_map))
        }

        if (kind == "cast") {
            inner <- lower_expr(expr$expr, env_map)
            env_map <- inner$env
            id <- new_value()
            append_stmt(
                list(
                  kind = "ssa_stmt", id = id, op = paste0("cast:", expr$to),
                  args = c(inner$value)
              )
            )
            return(list(value = id, env = env_map))
        }

        if (kind == "call") {
            args <- character(0)
            cur_env <- env_map
            if (!is.null(expr$args)) {
                for (a in expr$args) {
                  part <- lower_expr(a, cur_env)
                  cur_env <- part$env
                  args <- c(args, part$value)
                }
            }
            id <- new_value()
            append_stmt(
                list(
                  kind = "ssa_stmt", id = id, op = paste0("call:", expr$fn),
                  args = args
              )
            )
            return(list(value = id, env = cur_env))
        }

        if (kind == "ifelse") {
            cnd <- lower_expr(expr$cond, env_map)
            ys <- lower_expr(expr$yes, cnd$env)
            ns <- lower_expr(expr$no, ys$env)
            env_map <- ns$env
            id <- new_value()
            append_stmt(
                list(
                  kind = "ssa_stmt", id = id, op = "select", args = c(cnd$value, ys$value, ns$value)
              )
            )
            return(list(value = id, env = env_map))
        }

        if (kind == "index") {
            base <- lower_expr(expr$base, env_map)
            cur_env <- base$env
            args <- c(base$value)
            if (!is.null(expr$indices)) {
                for (idx in expr$indices) {
                  part <- lower_expr(idx, cur_env)
                  cur_env <- part$env
                  args <- c(args, part$value)
                }
            }
            id <- new_value()
            append_stmt(list(kind = "ssa_stmt", id = id, op = "index", args = args))
            return(list(value = id, env = cur_env))
        }

        id <- new_value()
        append_stmt(
            list(
                kind = "ssa_stmt", id = id, op = paste0("expr:", kind),
                args = character(0)
            )
        )
        list(value = id, env = env_map)
    }

    lower_store_effect <- function(lhs, rhs_value, env_map) {
        cur_env <- env_map
        store_op <- "store"
        store_args <- c(rhs_value)
        store_target <- NULL
        idx_kinds <- character(0)

        if (!is.null(lhs) &&
            !is.null(lhs$kind) &&
            lhs$kind == "index") {
            base <- lower_expr(lhs$base, cur_env)
            cur_env <- base$env
            store_op <- "store_index"
            idx_vals <- character(0)
            if (!is.null(lhs$indices)) {
                for (idx in lhs$indices) {
                  part <- lower_index_component(idx, cur_env)
                  cur_env <- part$env
                  idx_vals <- c(idx_vals, part$value)
                  idx_kinds <- c(idx_kinds, part$idx_kind)
                }
            }
            store_args <- c(base$value, idx_vals, rhs_value)
        } else if (!is.null(lhs) &&
            !is.null(lhs$kind) &&
            lhs$kind == "subscript") {
            cur_env <- add_entry_param(lhs$var, cur_env)
            store_op <- "store_subscript"
            store_target <- lhs$var
            idx_vals <- character(0)
            if (!is.null(lhs$indices)) {
                for (idx in lhs$indices) {
                  part <- lower_index_component(idx, cur_env)
                  cur_env <- part$env
                  idx_vals <- c(idx_vals, part$value)
                  idx_kinds <- c(idx_kinds, part$idx_kind)
                }
            }
            store_args <- c(cur_env[[lhs$var]], idx_vals, rhs_value)
        } else if (!is.null(lhs)) {
            store_op <- paste0("store:", lhs$kind)
        }

        effect <- list(kind = "ssa_effect", op = store_op, args = store_args)
        if (!is.null(store_target))
            effect$target <- store_target
        if (length(idx_kinds) >
            0)
            effect$index_kinds <- idx_kinds
        append_stmt(effect)
        cur_env
    }

    lower_stmt <- function(stmt, env_map, loop_ctx = NULL) {
        if (is.null(stmt) ||
            !is.list(stmt) ||
            is.null(stmt$kind)) {
            return(env_map)
        }
        kind_raw <- as.character(stmt$kind)
        kind <- base_kind(kind_raw)

        if (kind == "block") {
            cur_env <- env_map
            if (!is.null(stmt$stmts)) {
                for (s in stmt$stmts) {
                  cur_env <- lower_stmt(s, cur_env, loop_ctx = loop_ctx)
                  if (has_term())
                    break
                }
            }
            return(cur_env)
        }

        if (kind == "assign") {
            rhs <- lower_expr(stmt$rhs, env_map)
            cur_env <- rhs$env
            lhs <- stmt$lhs
            if (!is.null(lhs) &&
                !is.null(lhs$kind) &&
                lhs$kind == "var") {
                cur_env[[lhs$name]] <- rhs$value
                return(cur_env)
            }
            cur_env <- lower_store_effect(lhs, rhs$value, cur_env)
            return(cur_env)
        }

        if (kind == "return") {
            if (is.null(stmt$value)) {
                set_term(.mojor_ssa_ret())
                return(env_map)
            }
            val <- lower_expr(stmt$value, env_map)
            set_term(.mojor_ssa_ret(val$value))
            return(val$env)
        }

        if (kind == "if") {
            cond <- lower_expr(stmt$cond, env_map)
            env_before <- cond$env
            then_name <- new_block("then")
            else_name <- new_block("else")
            merge_name <- new_block("merge")

            set_term(
                .mojor_ssa_condbr(cond$value, then = then_name, else_target = else_name)
            )

            set_current(then_name)
            env_then <- lower_stmt(stmt$then, env_before, loop_ctx = loop_ctx)
            term_then <- st$blocks[[then_name]]$term

            set_current(else_name)
            if (!is.null(stmt$else_block)) {
                env_else <- lower_stmt(stmt$else_block, env_before, loop_ctx = loop_ctx)
            } else {
                env_else <- env_before
            }
            term_else <- st$blocks[[else_name]]$term

 # If both arms already terminate, there is no merge
 # continuation path.
            if (!is.null(term_then) &&
                !is.null(term_else)) {
                set_current(else_name)
                return(env_before)
            }

            all_vars <- unique(
                c(
                  names(env_before),
                  names(env_then),
                  names(env_else)
              )
            )
            merge_vars <- character(0)
            for (nm in all_vars) {
                v_before <- env_before[[nm]]
                v_then <- env_then[[nm]]
                v_else <- env_else[[nm]]
                if (!identical(v_then, v_else) ||
                  !identical(v_then, v_before) ||
                  !identical(v_else, v_before)) {
                  merge_vars <- c(merge_vars, nm)
                }
            }
            merge_vars <- unique(merge_vars)

            merge_params <- list()
            then_args <- character(0)
            else_args <- character(0)
            env_after <- env_before
            for (nm in merge_vars) {
                tval <- env_then[[nm]]
                eval <- env_else[[nm]]
                bval <- env_before[[nm]]
                if (is.null(tval))
                  tval <- bval
                if (is.null(eval))
                  eval <- bval
                if (is.null(tval) ||
                  is.null(eval))
                  next
                pid <- new_value()
                merge_params <- c(merge_params, list(list(id = pid, var = nm)))
                then_args <- c(then_args, tval)
                else_args <- c(else_args, eval)
                env_after[[nm]] <- pid
            }

            if (is.null(term_then)) {
                set_current(then_name)
                set_term(.mojor_ssa_br(merge_name, args = then_args))
            }
            if (is.null(term_else)) {
                set_current(else_name)
                set_term(.mojor_ssa_br(merge_name, args = else_args))
            }

            ensure_block(merge_name)
            merge_blk <- st$blocks[[merge_name]]
            merge_blk$params <- merge_params
            st$blocks[[merge_name]] <- merge_blk
            set_current(merge_name)
            return(env_after)
        }

        if (kind == "loop") {
            range <- stmt$range
            start_expr <- if (!is.null(range) &&
                !is.null(range$kind) &&
                range$kind == "range")
                range$start else .mojor_ir_const("1")
            end_expr <- if (!is.null(range) &&
                !is.null(range$kind) &&
                range$kind == "range")
                range$end else .mojor_ir_var("n")
            step_expr <- if (!is.null(range) &&
                !is.null(range$kind) &&
                range$kind == "range" && !is.null(range$step))
                range$step else .mojor_ir_const("1")
            end_excl <- isTRUE(
                !is.null(range) &&
                  !is.null(range$end_exclusive) &&
                  range$end_exclusive
            )
            loop_kind <- "for"
            loop_id <- if (!is.null(stmt$loop_id) &&
                nzchar(as.character(stmt$loop_id)))
                as.character(stmt$loop_id) else new_loop_id(loop_kind)
            intent_attrs <- structured_intent_attrs(stmt)

            start_val <- lower_expr(start_expr, env_map)
            end_val <- lower_expr(end_expr, start_val$env)
            step_val <- lower_expr(step_expr, end_val$env)
            pre_env <- step_val$env

            header_name <- new_block("loop_header")
            body_name <- new_block("loop_body")
            exit_name <- new_block("loop_exit")
            apply_loop_role_attrs(
                header_name, loop_id = loop_id, loop_kind = loop_kind,
                role = "header", intent_attrs = intent_attrs, extra = list(
                  loop_continue_dest = header_name, loop_break_dest = exit_name,
                  loop_anchor = TRUE
              )
            )
            apply_loop_role_attrs(exit_name, loop_id = loop_id, loop_kind = loop_kind, role = "exit")
            apply_loop_role_attrs(body_name, loop_id = loop_id, loop_kind = loop_kind, role = "body")

            carried <- setdiff(
                .mojor_ir_collect_assigned_vars(stmt$body),
                stmt$var
            )
            carried <- unique(carried)

            for (nm in carried) {
                if (is.null(pre_env[[nm]]))
                  pre_env <- add_entry_param(nm, pre_env)
            }

            init_args <- c(start_val$value)
            if (length(carried) >
                0) {
                init_args <- c(init_args, unlist(pre_env[carried], use.names = FALSE))
            }
            set_term(.mojor_ssa_br(header_name, args = init_args))

            header_params <- list()
            iter_param <- new_value()
            header_params <- c(header_params, list(list(id = iter_param, var = stmt$var)))
            header_carry_ids <- character(0)
            if (length(carried) >
                0) {
                for (nm in carried) {
                  pid <- new_value()
                  header_params <- c(header_params, list(list(id = pid, var = nm)))
                  header_carry_ids <- c(header_carry_ids, pid)
                }
            }
            ensure_block(header_name)
            hb <- st$blocks[[header_name]]
            hb$params <- header_params
            st$blocks[[header_name]] <- hb

            set_current(header_name)
            env_header <- pre_env
            env_header[[stmt$var]] <- iter_param
            if (length(carried) >
                0) {
                for (k in seq_along(carried)) env_header[[carried[[k]]]] <- header_carry_ids[[k]]
            }

            cond_id <- new_value()
            append_stmt(
                list(
                  kind = "ssa_stmt", id = cond_id, op = "loop_cond", args = c(iter_param, end_val$value, step_val$value),
                  end_exclusive = end_excl
              )
            )

            exit_params <- list()
            exit_param_ids <- character(0)
            if (length(carried) >
                0) {
                for (nm in carried) {
                  pid <- new_value()
                  exit_params <- c(exit_params, list(list(id = pid, var = nm)))
                  exit_param_ids <- c(exit_param_ids, pid)
                }
            }
            ensure_block(exit_name)
            xb <- st$blocks[[exit_name]]
            xb$params <- exit_params
            st$blocks[[exit_name]] <- xb

            exit_args <- if (length(carried) >
                0)
                unlist(env_header[carried], use.names = FALSE) else character(0)
            set_term(
                .mojor_ssa_condbr(
                  cond_id, then = body_name, else_target = exit_name, else_args = exit_args
              )
            )

            set_current(body_name)
            env_body <- env_header
            loop_ctx_body <- list(
                kind = "loop", header_name = header_name, exit_name = exit_name,
                iter_var = stmt$var, step_value = step_val$value, carried = carried,
                header_env = env_header
            )
            env_body <- lower_stmt(stmt$body, env_body, loop_ctx = loop_ctx_body)

            if (!has_term()) {
                next_iter <- new_value()
                append_stmt(
                  list(
                    kind = "ssa_stmt", id = next_iter, op = "loop_next",
                    args = c(env_body[[stmt$var]], step_val$value)
                )
              )
                back_args <- c(next_iter)
                if (length(carried) >
                  0) {
                  carry_out <- character(0)
                  for (nm in carried) {
                    v <- env_body[[nm]]
                    if (is.null(v))
                      v <- env_header[[nm]]
                    carry_out <- c(carry_out, v)
                  }
                  back_args <- c(back_args, carry_out)
                }
                set_term(.mojor_ssa_br(header_name, args = back_args))
            }

            set_current(exit_name)
            env_after <- pre_env
            if (length(carried) >
                0) {
                for (k in seq_along(carried)) env_after[[carried[[k]]]] <- exit_param_ids[[k]]
            }
            return(env_after)
        }

        if (kind %in% c("while", "repeat")) {
            body_node <- stmt$body
            cond_node <- if (kind == "while")
                stmt$cond else .mojor_ir_const("True")
            loop_kind <- if (kind == "while")
                "while" else "repeat"
            loop_id <- if (!is.null(stmt$loop_id) &&
                nzchar(as.character(stmt$loop_id)))
                as.character(stmt$loop_id) else new_loop_id(loop_kind)
            intent_attrs <- structured_intent_attrs(stmt)

            pre_env <- env_map
            header_name <- new_block(paste0(kind, "_header"))
            body_name <- new_block(paste0(kind, "_body"))
            exit_name <- new_block(paste0(kind, "_exit"))
            apply_loop_role_attrs(
                header_name, loop_id = loop_id, loop_kind = loop_kind,
                role = "header", intent_attrs = intent_attrs, extra = list(
                  loop_continue_dest = header_name, loop_break_dest = exit_name,
                  loop_anchor = TRUE
              )
            )
            apply_loop_role_attrs(exit_name, loop_id = loop_id, loop_kind = loop_kind, role = "exit")
            apply_loop_role_attrs(body_name, loop_id = loop_id, loop_kind = loop_kind, role = "body")

            carried <- unique(.mojor_ir_collect_assigned_vars(body_node))
            for (nm in carried) {
                if (is.null(pre_env[[nm]]))
                  pre_env <- add_entry_param(nm, pre_env)
            }

            init_args <- if (length(carried) >
                0)
                unlist(pre_env[carried], use.names = FALSE) else character(0)
            set_term(.mojor_ssa_br(header_name, args = init_args))

            header_params <- list()
            header_carry_ids <- character(0)
            if (length(carried) >
                0) {
                for (nm in carried) {
                  pid <- new_value()
                  header_params <- c(header_params, list(list(id = pid, var = nm)))
                  header_carry_ids <- c(header_carry_ids, pid)
                }
            }
            ensure_block(header_name)
            hb <- st$blocks[[header_name]]
            hb$params <- header_params
            st$blocks[[header_name]] <- hb

            set_current(header_name)
            env_header <- pre_env
            if (length(carried) >
                0) {
                for (k in seq_along(carried)) env_header[[carried[[k]]]] <- header_carry_ids[[k]]
            }

            cond_val <- lower_expr(cond_node, env_header)
            env_header <- cond_val$env

            exit_params <- list()
            exit_param_ids <- character(0)
            if (length(carried) >
                0) {
                for (nm in carried) {
                  pid <- new_value()
                  exit_params <- c(exit_params, list(list(id = pid, var = nm)))
                  exit_param_ids <- c(exit_param_ids, pid)
                }
            }
            ensure_block(exit_name)
            xb <- st$blocks[[exit_name]]
            xb$params <- exit_params
            st$blocks[[exit_name]] <- xb

            exit_args <- if (length(carried) >
                0)
                unlist(env_header[carried], use.names = FALSE) else character(0)
            set_term(
                .mojor_ssa_condbr(
                  cond_val$value, then = body_name, else_target = exit_name,
                  else_args = exit_args
              )
            )

            set_current(body_name)
            env_body <- env_header
            loop_ctx_body <- list(
                kind = kind, header_name = header_name, exit_name = exit_name,
                carried = carried, header_env = env_header
            )
            env_body <- lower_stmt(body_node, env_body, loop_ctx = loop_ctx_body)

            if (!has_term()) {
                back_args <- character(0)
                if (length(carried) >
                  0) {
                  carry_out <- character(0)
                  for (nm in carried) {
                    v <- env_body[[nm]]
                    if (is.null(v))
                      v <- env_header[[nm]]
                    carry_out <- c(carry_out, v)
                  }
                  back_args <- carry_out
                }
                set_term(.mojor_ssa_br(header_name, args = back_args))
            }

            set_current(exit_name)
            env_after <- pre_env
            if (length(carried) >
                0) {
                for (k in seq_along(carried)) env_after[[carried[[k]]]] <- exit_param_ids[[k]]
            }
            return(env_after)
        }

        if (kind == "break") {
            if (is.null(loop_ctx) ||
                is.null(loop_ctx$exit_name)) {
                append_stmt(list(kind = "ssa_effect", op = "stmt:break", args = character(0)))
                return(env_map)
            }
            carried <- if (is.null(loop_ctx$carried))
                character(0) else loop_ctx$carried
            exit_args <- character(0)
            if (length(carried) >
                0) {
                for (nm in carried) {
                  v <- env_map[[nm]]
                  if (is.null(v) &&
                    !is.null(loop_ctx$header_env))
                    v <- loop_ctx$header_env[[nm]]
                  if (!is.null(v))
                    exit_args <- c(exit_args, v)
                }
            }
            set_term(.mojor_ssa_br(loop_ctx$exit_name, args = exit_args))
            return(env_map)
        }

        if (kind == "next") {
            if (is.null(loop_ctx) ||
                is.null(loop_ctx$header_name)) {
                append_stmt(list(kind = "ssa_effect", op = "stmt:next", args = character(0)))
                return(env_map)
            }
            carried <- if (is.null(loop_ctx$carried))
                character(0) else loop_ctx$carried
            back_args <- character(0)

            if (!is.null(loop_ctx$iter_var) &&
                !is.null(loop_ctx$step_value)) {
                next_iter <- new_value()
                append_stmt(
                  list(
                    kind = "ssa_stmt", id = next_iter, op = "loop_next",
                    args = c(env_map[[loop_ctx$iter_var]], loop_ctx$step_value)
                )
              )
                back_args <- c(back_args, next_iter)
            }

            if (length(carried) >
                0) {
                for (nm in carried) {
                  v <- env_map[[nm]]
                  if (is.null(v) &&
                    !is.null(loop_ctx$header_env))
                    v <- loop_ctx$header_env[[nm]]
                  if (!is.null(v))
                    back_args <- c(back_args, v)
                }
            }

            set_term(.mojor_ssa_br(loop_ctx$header_name, args = back_args))
            return(env_map)
        }

        if (kind == "scheduled_reduce") {
            effect <- list(
                kind = "ssa_effect", op = "scheduled_reduce", args = c(
                  as.character(stmt$acc),
                  as.character(stmt$arg),
                  as.character(stmt$n_var)
              ),
                reduce_mode = as.character(stmt$mode),
                reduce_op = as.character(stmt$op),
                reduce_acc = as.character(stmt$acc),
                reduce_arg = as.character(stmt$arg),
                n_var = as.character(stmt$n_var)
            )
            if (!is.null(stmt$dtype))
                effect$dtype <- as.character(stmt$dtype)
            if (!is.null(stmt$init_val))
                effect$init_val <- as.character(stmt$init_val)
            if (!is.null(stmt$empty_val))
                effect$empty_val <- as.character(stmt$empty_val)
            if (!is.null(stmt$nan_val))
                effect$nan_val <- as.character(stmt$nan_val)
            if (!is.null(stmt$value_cast))
                effect$value_cast <- as.character(stmt$value_cast)
            append_stmt(effect)
            return(env_map)
        }

        if (kind == "scalar_reduce") {
            effect <- list(
                kind = "ssa_effect", op = "scalar_reduce", args = c(
                  as.character(stmt$acc),
                  as.character(stmt$arg)
              ),
                reduce_mode = "linear", reduce_op = as.character(stmt$op),
                reduce_acc = as.character(stmt$acc),
                reduce_arg = as.character(stmt$arg),
                n_var = if (!is.null(stmt$n_var)) as.character(stmt$n_var) else "n_i"
            )
            if (!is.null(stmt$axis))
                effect$axis <- as.character(stmt$axis)
            if (!is.null(stmt$associative))
                effect$associative <- as.character(stmt$associative)
            if (!is.null(stmt$commutative))
                effect$commutative <- as.character(stmt$commutative)
            append_stmt(effect)
            return(env_map)
        }

        append_stmt(
            list(
                kind = "ssa_effect", op = paste0("stmt:", kind),
                args = character(0)
            )
        )
        env_map
    }

    st$entry <- new_block("entry")
    set_current(st$entry)
    env_end <- lower_stmt(node, list(), loop_ctx = NULL)

    if (!has_term(st$current)) {
        if (!is.null(env_end[["return"]])) {
            set_term(.mojor_ssa_ret(env_end[["return"]]))
        } else {
            set_term(.mojor_ssa_ret())
        }
    }

    entry_blk <- st$blocks[[st$entry]]
    entry_blk$params <- st$entry_params
    st$blocks[[st$entry]] <- entry_blk

    out <- list(kind = "ssa_fn", entry = st$entry, blocks = st$blocks)
    if (exists(".mojor_ssa_attach_loop_destination_metadata", mode = "function")) {
        out <- .mojor_ssa_attach_loop_destination_metadata(out)
    }
    out
}

.mojor_ir_ssa_dom_cache <- new.env(parent = emptyenv())
.mojor_ir_ssa_dom_cache_state <- new.env(parent = emptyenv())
.mojor_ir_ssa_dom_cache_state$hits <- 0L
.mojor_ir_ssa_dom_cache_state$misses <- 0L

.mojor_ssa_dom_cache_reset <- function() {
    keys <- ls(envir = .mojor_ir_ssa_dom_cache, all.names = TRUE)
    if (length(keys) >
        0)
        rm(list = keys, envir = .mojor_ir_ssa_dom_cache)
    .mojor_ir_ssa_dom_cache_state$hits <- 0L
    .mojor_ir_ssa_dom_cache_state$misses <- 0L
    invisible(TRUE)
}

.mojor_ssa_dom_cache_stats <- function() {
    list(
        hits = as.integer(.mojor_ir_ssa_dom_cache_state$hits),
        misses = as.integer(.mojor_ir_ssa_dom_cache_state$misses),
        entries = as.integer(length(ls(envir = .mojor_ir_ssa_dom_cache, all.names = TRUE)))
    )
}

.mojor_ssa_cfg_signature <- function(ssa) {
    if (is.null(ssa) ||
        !is.list(ssa) ||
        is.null(ssa$kind) ||
        !identical(ssa$kind, "ssa_fn")) {
        stop("SSA cfg signature: expected SemanticIR (ssa_fn)")
    }
    if (is.null(ssa$entry) ||
        !is.character(ssa$entry) ||
        length(ssa$entry) !=
            1) {
        stop("SSA cfg signature: missing entry block name")
    }
    if (is.null(ssa$blocks) ||
        !is.list(ssa$blocks) ||
        length(ssa$blocks) ==
            0 || is.null(names(ssa$blocks))) {
        stop("SSA cfg signature: blocks must be a named non-empty list")
    }

    parts <- c(paste0("entry=", ssa$entry))
    block_names <- names(ssa$blocks)
    parts <- c(parts, paste0("blocks=", paste(block_names, collapse = ",")))
    for (bn in block_names) {
        blk <- ssa$blocks[[bn]]
        term <- blk$term
        kind <- if (is.null(term) ||
            is.null(term$kind))
            "" else as.character(term$kind)
        succ <- .mojor_ssa_successors(term)
        succ <- sort(unique(as.character(succ)))
        parts <- c(
            parts, paste0("b:", bn, ";k=", kind, ";succ=", paste(succ, collapse = ","))
        )
    }
    paste(parts, collapse = "|")
}

.mojor_ssa_cfg_cache_key <- function(ssa) {
    sig <- .mojor_ssa_cfg_signature(ssa)
    cfg_version <- ssa$cfg_version
    if (!is.null(cfg_version) &&
        length(cfg_version) ==
            1) {
        return(paste0(sig, "|cfg_version=", as.character(cfg_version)))
    }
    sig
}

.mojor_ssa_build_preds_dom <- function(ssa, successors_of) {
    block_names <- names(ssa$blocks)
    preds <- setNames(
        vector("list", length(block_names)),
        block_names
    )
    for (bn in block_names) {
        succ <- successors_of(ssa$blocks[[bn]]$term)
        if (length(succ) ==
            0)
            next
        for (sn in succ) {
            if (!sn %in% block_names)
                next
            preds[[sn]] <- unique(c(preds[[sn]], bn))
        }
    }

    dom <- setNames(
        vector("list", length(block_names)),
        block_names
    )
    for (bn in block_names) {
        if (identical(bn, ssa$entry)) {
            dom[[bn]] <- bn
        } else {
            dom[[bn]] <- block_names
        }
    }
    changed <- TRUE
    while (isTRUE(changed)) {
        changed <- FALSE
        for (bn in block_names) {
            if (identical(bn, ssa$entry))
                next
            pb <- preds[[bn]]
            if (is.null(pb) ||
                length(pb) ==
                  0) {
                new_dom <- bn
            } else {
                inter <- dom[[pb[[1]]]]
                if (length(pb) >
                  1) {
                  for (p in pb[-1]) inter <- intersect(inter, dom[[p]])
                }
                new_dom <- unique(c(bn, inter))
            }
            if (!setequal(dom[[bn]], new_dom)) {
                dom[[bn]] <- new_dom
                changed <- TRUE
            }
        }
    }

    list(preds = preds, dom = dom)
}

.mojor_ir_verify_ssa <- function(ssa, strict_schema = NULL) {
    if (is.null(ssa) ||
        !is.list(ssa) ||
        is.null(ssa$kind) ||
        ssa$kind != "ssa_fn") {
        stop("SSA verify: root must be an ssa_fn")
    }
    if (is.null(ssa$entry) ||
        !is.character(ssa$entry) ||
        length(ssa$entry) !=
            1) {
        stop("SSA verify: missing entry block name")
    }
    if (is.null(ssa$blocks) ||
        !is.list(ssa$blocks) ||
        length(ssa$blocks) ==
            0) {
        stop("SSA verify: blocks must be a non-empty list")
    }
    if (is.null(names(ssa$blocks)) ||
        !ssa$entry %in% names(ssa$blocks)) {
        stop("SSA verify: entry block not found")
    }

    all_defs <- character(0)
    value_def_block <- list()
    for (bn in names(ssa$blocks)) {
        blk <- ssa$blocks[[bn]]
        if (is.null(blk) ||
            !is.list(blk) ||
            is.null(blk$kind) ||
            blk$kind != "ssa_block") {
            stop(sprintf("SSA verify: block '%s' is not an ssa_block", bn))
        }
        if (is.null(blk$term) ||
            is.null(blk$term$kind)) {
            stop(sprintf("SSA verify: block '%s' missing terminator", bn))
        }
        if (!blk$term$kind %in% c("br", "condbr", "ret")) {
            stop(
                sprintf(
                  "SSA verify: block '%s' has invalid terminator '%s'",
                  bn, blk$term$kind
              )
            )
        }
        if (!is.null(blk$params) &&
            length(blk$params) >
                0) {
            for (p in blk$params) {
                if (is.null(p$id) ||
                  !.mojor_ssa_is_value_ref(p$id)) {
                  stop(sprintf("SSA verify: block '%s' has invalid param id", bn))
                }
                if (p$id %in% all_defs) {
                  stop(sprintf("SSA verify: duplicate SSA value '%s'", p$id))
                }
                all_defs <- c(all_defs, p$id)
                value_def_block[[p$id]] <- bn
            }
        }
        if (!is.null(blk$stmts) &&
            length(blk$stmts) >
                0) {
            for (st in blk$stmts) {
                if (!is.null(st$kind) &&
                  identical(st$kind, "ssa_effect") &&
                  !is.null(st$op)) {
                  op_raw <- as.character(st$op)
                  op_can <- .mojor_ssa_op_canonical(op_raw)
                  if (op_raw %in% c("stmt:break", "stmt:next") ||
                    op_can %in% c("mojor.misc.stmt.break", "mojor.misc.stmt.next")) {
                    stop(
                      sprintf(
                        "SSA verify: CFG IR forbids '%s'; break/next must be lowered to edges",
                        op_raw
                    )
                  )
                  }
                }
                if (is.null(st$id) ||
                  !.mojor_ssa_is_value_ref(st$id))
                  next
                if (st$id %in% all_defs) {
                  stop(sprintf("SSA verify: duplicate SSA value '%s'", st$id))
                }
                all_defs <- c(all_defs, st$id)
                value_def_block[[st$id]] <- bn
            }
        }
    }

    .mojor_ssa_schema_validate_if_strict(ssa, strict_schema = strict_schema, where = "SSA verify")

    block_param_count <- function(name) {
        blk <- ssa$blocks[[name]]
        if (is.null(blk$params)) {
            return(0L)
        }
        length(blk$params)
    }

    successors_of <- function(term) {
        if (is.null(term) ||
            is.null(term$kind)) {
            return(character(0))
        }
        if (term$kind == "br" && !is.null(term$target)) {
            return(term$target)
        }
        if (term$kind == "condbr") {
            out <- character(0)
            if (!is.null(term$then))
                out <- c(out, term$then)
            if (!is.null(term[["else"]]))
                out <- c(out, term[["else"]])
            return(unique(out))
        }
        character(0)
    }

    block_names <- names(ssa$blocks)
    cache_key <- .mojor_ssa_cfg_cache_key(ssa)
    cache_entry <- .mojor_ir_ssa_dom_cache[[cache_key]]
    if (is.list(cache_entry) &&
        !is.null(cache_entry$preds) &&
        !is.null(cache_entry$dom)) {
        preds <- cache_entry$preds
        dom <- cache_entry$dom
        .mojor_ir_ssa_dom_cache_state$hits <- .mojor_ir_ssa_dom_cache_state$hits +
            1L
    } else {
        built <- .mojor_ssa_build_preds_dom(ssa, successors_of)
        preds <- built$preds
        dom <- built$dom
        .mojor_ir_ssa_dom_cache[[cache_key]] <- built
        .mojor_ir_ssa_dom_cache_state$misses <- .mojor_ir_ssa_dom_cache_state$misses +
            1L
    }

    value_dominates_block <- function(val, use_block) {
        def_block <- value_def_block[[val]]
        if (is.null(def_block) ||
            is.null(dom[[use_block]])) {
            return(FALSE)
        }
        def_block %in% dom[[use_block]]
    }

    check_ref_visible <- function(ref, bn, visible_defs, context) {
        if (!.mojor_ssa_is_value_ref(ref)) {
            return(invisible(NULL))
        }
        if (ref %in% visible_defs) {
            return(invisible(NULL))
        }
        if (!ref %in% all_defs) {
            stop(
                sprintf(
                  "SSA verify: use-before-def '%s' in block '%s'", ref,
                  bn
              )
            )
        }
        if (!value_dominates_block(ref, bn)) {
            stop(
                sprintf(
                  "SSA verify: value '%s' does not dominate %s in block '%s'",
                  ref, context, bn
              )
            )
        }
        invisible(NULL)
    }

    for (bn in names(ssa$blocks)) {
        blk <- ssa$blocks[[bn]]
        visible_defs <- character(0)
        if (!is.null(blk$params) &&
            length(blk$params) >
                0) {
            visible_defs <- c(
                visible_defs, vapply(
                  blk$params, function(p) p$id,
                  character(1)
              )
            )
        }
        if (!is.null(blk$stmts) &&
            length(blk$stmts) >
                0) {
            stmt_defs <- vapply(
                blk$stmts, function(st) {
                  if (!is.null(st$id) &&
                    .mojor_ssa_is_value_ref(st$id))
                    st$id else NA_character_
                }, character(1)
            )
            for (idx in seq_along(blk$stmts)) {
                st <- blk$stmts[[idx]]
                args <- st$args
                if (idx < length(stmt_defs)) {
                  future_defs <- stmt_defs[seq.int(
                    idx + 1L, length(stmt_defs),
                    by = 1L
                )]
                  future_defs <- future_defs[!is.na(future_defs)]
                } else {
                  future_defs <- character(0)
                }
                if (!is.null(args) &&
                  length(args) >
                    0) {
                  for (a in args) {
                    if (!.mojor_ssa_is_value_ref(a))
                      next
                    if (a %in% visible_defs)
                      next
                    if (a %in% future_defs) {
                      stop(
                        sprintf(
                          "SSA verify: use-before-def '%s' in block '%s'",
                          a, bn
                      )
                    )
                    }
                    check_ref_visible(a, bn, visible_defs, "statement in")
                  }
                }
                if (!is.null(st$id) &&
                  .mojor_ssa_is_value_ref(st$id)) {
                  visible_defs <- c(visible_defs, st$id)
                }
            }
        }

        term <- blk$term
        if (term$kind == "br") {
            if (is.null(term$target) ||
                !term$target %in% names(ssa$blocks)) {
                stop(sprintf("SSA verify: br from '%s' targets unknown block", bn))
            }
            nargs <- if (is.null(term$args))
                0L else length(term$args)
            expected <- block_param_count(term$target)
            if (nargs != expected) {
                stop(
                  sprintf(
                    "SSA verify: br arg arity mismatch from '%s' to '%s' (%d != %d)",
                    bn, term$target, nargs, expected
                )
              )
            }
            if (nargs > 0) {
                for (a in term$args) {
                  check_ref_visible(a, bn, visible_defs, "branch arg in")
                }
            }
        }

        if (term$kind == "condbr") {
            if (is.null(term$cond) ||
                !.mojor_ssa_is_value_ref(term$cond)) {
                stop(
                  sprintf(
                    "SSA verify: condbr in '%s' has invalid condition",
                    bn
                )
              )
            }
            check_ref_visible(term$cond, bn, visible_defs, "condbr condition in")
            if (is.null(term$then) ||
                !term$then %in% names(ssa$blocks) ||
                is.null(term[["else"]]) ||
                !term[["else"]] %in% names(ssa$blocks)) {
                stop(
                  sprintf(
                    "SSA verify: condbr in '%s' targets unknown block",
                    bn
                )
              )
            }
            then_args <- if (is.null(term$then_args))
                character(0) else term$then_args
            else_args <- if (is.null(term$else_args))
                character(0) else term$else_args
            if (length(then_args) !=
                block_param_count(term$then)) {
                stop(
                  sprintf(
                    "SSA verify: condbr then arg arity mismatch from '%s' to '%s'",
                    bn, term$then
                )
              )
            }
            if (length(else_args) !=
                block_param_count(term[["else"]])) {
                stop(
                  sprintf(
                    "SSA verify: condbr else arg arity mismatch from '%s' to '%s'",
                    bn, term[["else"]]
                )
              )
            }
            for (a in c(then_args, else_args)) {
                check_ref_visible(a, bn, visible_defs, "condbr arg in")
            }
        }

        if (term$kind == "ret" && !is.null(term$value)) {
            if (!.mojor_ssa_is_value_ref(term$value)) {
                stop(sprintf("SSA verify: ret in '%s' uses unknown value", bn))
            }
            check_ref_visible(term$value, bn, visible_defs, "ret in")
        }
    }

    TRUE
}

.mojor_ir_ssa_prune_unreachable <- function(ssa) {
    ssa <- .mojor_ssa_require_semantic_ir(ssa, where = "SSA prune_unreachable")
    if (is.null(ssa$entry) ||
        is.null(ssa$blocks) ||
        !is.list(ssa$blocks) ||
        length(ssa$blocks) ==
            0) {
        return(ssa)
    }
    block_names <- names(ssa$blocks)
    if (is.null(block_names) ||
        !ssa$entry %in% block_names) {
        return(ssa)
    }

    reachable <- character(0)
    work <- ssa$entry
    while (length(work) >
        0) {
        bn <- work[[1]]
        work <- work[-1]
        if (bn %in% reachable)
            next
        if (!bn %in% block_names)
            next
        reachable <- c(reachable, bn)
        succ <- .mojor_ssa_successors(ssa$blocks[[bn]]$term)
        if (length(succ) >
            0) {
            work <- c(work, succ[!succ %in% reachable])
        }
    }

    if (length(reachable) ==
        length(block_names)) {
        return(ssa)
    }
    ssa$blocks <- ssa$blocks[reachable]
    ssa
}

.mojor_ssa_resource_from_operand <- function(operand, access = "Unknown") {
    if (is.null(operand)) {
        return(
            list(
                resource_kind = "Unknown", resource_id = "unknown:<null>",
                access = as.character(access)
            )
        )
    }
    opnd <- as.character(operand)
    if (length(opnd) !=
        1 || is.na(opnd) ||
        !nzchar(opnd)) {
        return(
            list(
                resource_kind = "Unknown", resource_id = "unknown:<empty>",
                access = as.character(access)
            )
        )
    }
    if (grepl("^%arg[0-9]+$", opnd)) {
        return(
            list(
                resource_kind = "Ref", resource_id = paste0("arg_slot:", sub("^%arg", "", opnd)),
                access = as.character(access)
            )
        )
    }
    if (grepl("^%v[0-9]+$", opnd)) {
        return(
            list(
                resource_kind = "Local", resource_id = paste0("ssa_value:", sub("^%v", "", opnd)),
                access = as.character(access)
            )
        )
    }
    if (grepl("^[A-Za-z_][A-Za-z0-9_\\.]*$", opnd)) {
        return(
            list(
                resource_kind = "Global", resource_id = paste0("symbol:", opnd),
                access = as.character(access)
            )
        )
    }
    list(
        resource_kind = "Unknown", resource_id = paste0("unknown:", opnd),
        access = as.character(access)
    )
}

.mojor_ssa_rng_call_fns <- function() {
    .mojor_ir_rng_call_fns()
}

.mojor_ssa_is_rng_call_fn <- function(fn) {
    fn <- as.character(fn)
    length(fn) ==
        1 && !is.na(fn) &&
        fn %in% .mojor_ssa_rng_call_fns()
}

.mojor_ssa_rng_resource <- function() {
    list(resource_kind = "Ref", resource_id = "rng:state", access = "RNG")
}

.mojor_ssa_stmt_effect_resource_summary <- function(stmt) {
    if (is.null(stmt) ||
        !is.list(stmt) ||
        is.null(stmt$kind) ||
        is.null(stmt$op)) {
        return(list(effect_class = "Unknown", resources = list()))
    }

    op <- as.character(stmt$op)
    op_can <- .mojor_ssa_op_canonical(op)
    interfaces <- .mojor_ssa_op_schema_interfaces(op)

    effect_class <- "Unknown"
    if ("WritesMem" %in% interfaces) {
        effect_class <- "Write"
    } else if ("ReadsMem" %in% interfaces) {
        effect_class <- "Read"
    } else if ("RNG" %in% interfaces) {
        effect_class <- "RNG"
    } else if (any(interfaces %in% c("Effect", "Call", "Unknown"))) {
        if (startsWith(op_can, "mojor.misc.call.")) {
            fn <- sub("^mojor\\.misc\\.call\\.", "", op_can)
            if (.mojor_ssa_is_rng_call_fn(fn)) {
                effect_class <- "RNG"
            } else {
                effect_class <- "Unknown"
            }
        } else {
            effect_class <- "Unknown"
        }
    } else if (length(interfaces) >
        0) {
        effect_class <- "None"
    } else {
        if (startsWith(op, "call:")) {
            fn <- sub("^call:", "", op)
            if (.mojor_ssa_is_rng_call_fn(fn)) {
                effect_class <- "RNG"
            } else {
                effect_class <- "Unknown"
            }
        } else if (startsWith(op, "expr:")) {
            effect_class <- "Unknown"
        } else if (startsWith(op, "unop:") ||
            startsWith(op, "binop:") ||
            startsWith(op, "cast:") ||
            op %in% c(
                "const", "const:null", "select", "idx_missing", "idx_slice",
                "loop_cond", "loop_next"
            )) {
            effect_class <- "None"
        } else if (op %in% c("index", "mojor.mem.index", "mojor.mem.load")) {
            effect_class <- "Read"
        } else if (op %in% c(
            "store_index", "store_subscript", "mojor.mem.store_index",
            "mojor.mem.store_subscript", "mojor.mem.store"
        )) {
            effect_class <- "Write"
        } else {
            effect_class <- "Unknown"
        }
    }

    resources <- list()
    if (effect_class %in% c("Read", "Write")) {
        args <- if (is.null(stmt$args))
            character(0) else as.character(stmt$args)
        base_operand <- if (length(args) >=
            1)
            args[[1]] else if (!is.null(stmt$target))
            as.character(stmt$target) else NULL
        resources[[1L]] <- .mojor_ssa_resource_from_operand(
            base_operand, access = if (identical(effect_class, "Read"))
                "Read" else "Write"
        )
    } else if (identical(effect_class, "RNG")) {
        resources[[1L]] <- .mojor_ssa_rng_resource()
    }

    list(effect_class = effect_class, resources = resources)
}

.mojor_ssa_annotate_effects_resources <- function(ssa) {
    ssa <- .mojor_ssa_require_semantic_ir(ssa, where = "SSA annotate_effects_resources")
    out <- ssa
    for (bn in names(out$blocks)) {
        blk <- out$blocks[[bn]]
        if (is.null(blk$stmts) ||
            length(blk$stmts) ==
                0)
            next
        for (i in seq_along(blk$stmts)) {
            st <- blk$stmts[[i]]
            if (is.null(st) ||
                !is.list(st) ||
                is.null(st$op))
                next
            info <- .mojor_ssa_stmt_effect_resource_summary(st)
            st$effect_class <- info$effect_class
            st$resources <- info$resources
            if (length(info$resources) >
                0 && !is.null(info$resources[[1]]$resource_id)) {
                st$resource_id <- as.character(info$resources[[1]]$resource_id)
            }
            blk$stmts[[i]] <- st
        }
        out$blocks[[bn]] <- blk
    }
    out
}

.mojor_ssa_loop_metadata_resolve <- function(ssa) {
    if (!is.null(ssa$loop_metadata) &&
        is.list(ssa$loop_metadata)) {
        return(ssa$loop_metadata)
    }
    if (exists(".mojor_ssa_loop_destination_metadata", mode = "function")) {
        meta <- tryCatch(
            .mojor_ssa_loop_destination_metadata(ssa),
            error = function(e) list()
        )
        if (!is.null(meta) &&
            is.list(meta)) {
            return(meta)
        }
    }
    list()
}

.mojor_ssa_fusion_loop_records <- function(ssa, recovery = NULL) {
    ssa <- .mojor_ssa_require_semantic_ir(ssa, where = "SSA fusion loop records")
    block_names <- names(ssa$blocks)
    if (is.null(block_names) ||
        length(block_names) ==
            0) {
        return(list())
    }
    block_pos <- stats::setNames(
        seq_along(block_names),
        block_names
    )

    meta <- .mojor_ssa_loop_metadata_resolve(ssa)
    rec_loops <- if (!is.null(recovery) &&
        is.list(recovery) &&
        !is.null(recovery$loops) &&
        is.list(recovery$loops)) {
        recovery$loops
    } else {
        list()
    }
    loop_ids <- unique(
        c(
            names(meta),
            names(rec_loops)
        )
    )
    if (length(loop_ids) ==
        0) {
        return(list())
    }

    records <- list()
    for (loop_id in loop_ids) {
        m <- meta[[loop_id]]
        rl <- rec_loops[[loop_id]]

        header <- ""
        if (!is.null(rl$header) &&
            nzchar(as.character(rl$header))) {
            header <- as.character(rl$header)
        } else if (!is.null(m$anchor_block) &&
            nzchar(as.character(m$anchor_block))) {
            header <- as.character(m$anchor_block)
        } else if (!is.null(m$continue_dest$block) &&
            nzchar(as.character(m$continue_dest$block))) {
            header <- as.character(m$continue_dest$block)
        }
        if (!header %in% block_names)
            header <- ""

        kind <- ""
        if (!is.null(rl$kind) &&
            nzchar(as.character(rl$kind))) {
            kind <- as.character(rl$kind)
        } else if (!is.null(m$kind) &&
            nzchar(as.character(m$kind))) {
            kind <- as.character(m$kind)
        }

        nodes <- character(0)
        if (!is.null(rl$nodes) &&
            length(rl$nodes) >
                0) {
            nodes <- unique(as.character(rl$nodes))
        } else {
            nodes <- unique(
                c(
                  header, if (!is.null(m$continue_dest$block)) as.character(m$continue_dest$block) else character(0),
                  if (!is.null(m$break_dest$block)) as.character(m$break_dest$block) else character(0)
              )
            )
        }
        nodes <- nodes[nodes %in% block_names]
        if (length(nodes) ==
            0 && nzchar(header))
            nodes <- header

        header_attrs <- if (nzchar(header))
            ssa$blocks[[header]]$attrs else NULL
        rec <- list(
            loop_id = as.character(loop_id),
            kind = if (nzchar(kind)) kind else "unknown",
            header = header, nodes = nodes, continue_block = if (!is.null(m$continue_dest$block)) as.character(m$continue_dest$block) else "",
            break_block = if (!is.null(m$break_dest$block)) as.character(m$break_dest$block) else "",
            attrs = if (is.null(header_attrs) ||
                !is.list(header_attrs)) list() else header_attrs
        )
        records[[length(records) +
            1L]] <- rec
    }

    if (length(records) <=
        1L) {
        return(records)
    }
    ord <- order(
        vapply(
            records, function(x) {
                hdr <- if (is.null(x$header))
                  "" else as.character(x$header)
                if (nzchar(hdr) &&
                  hdr %in% names(block_pos))
                  block_pos[[hdr]] else Inf
            }, numeric(1)
        ),
        vapply(
            records, function(x) as.character(x$loop_id),
            character(1)
        )
    )
    records[ord]
}

.mojor_ssa_fusion_block_has_broadcast <- function(blk) {
    if (is.null(blk) ||
        !is.list(blk)) {
        return(FALSE)
    }
    attrs <- blk$attrs
    if (!is.null(attrs) &&
        is.list(attrs)) {
        if (isTRUE(attrs$broadcast_nd) ||
            isTRUE(attrs$has_broadcast_nd)) {
            return(TRUE)
        }
    }
    if (is.null(blk$stmts) ||
        length(blk$stmts) ==
            0) {
        return(FALSE)
    }
    for (st in blk$stmts) {
        if (is.null(st) ||
            !is.list(st) ||
            is.null(st$op))
            next
        op <- as.character(st$op)
        op_can <- .mojor_ssa_op_canonical(op)
        if (grepl("broadcast_nd", op, fixed = TRUE) ||
            grepl("broadcast_nd", op_can, fixed = TRUE)) {
            return(TRUE)
        }
        args <- if (is.null(st$args))
            character(0) else as.character(st$args)
        if (any(grepl("_mojor_bcast_index", args, fixed = TRUE))) {
            return(TRUE)
        }
    }
    FALSE
}

.mojor_ssa_fusion_loop_has_forbidden_control <- function(ssa, loop_nodes) {
    if (length(loop_nodes) ==
        0) {
        return(TRUE)
    }
    for (bn in loop_nodes) {
        if (!bn %in% names(ssa$blocks))
            next
        blk <- ssa$blocks[[bn]]
        term <- blk$term
        if (!is.null(term) &&
            is.list(term) &&
            !is.null(term$kind) &&
            identical(
                as.character(term$kind),
                "ret"
            )) {
            return(TRUE)
        }
        if (is.null(blk$stmts) ||
            length(blk$stmts) ==
                0)
            next
        for (st in blk$stmts) {
            if (is.null(st) ||
                !is.list(st) ||
                is.null(st$op))
                next
            op <- as.character(st$op)
            op_can <- .mojor_ssa_op_canonical(op)
            if (op %in% c("stmt:break", "stmt:next") ||
                op_can %in% c("mojor.misc.stmt.break", "mojor.misc.stmt.next")) {
                return(TRUE)
            }
            if (startsWith(op, "expr:") ||
                startsWith(op_can, "mojor.misc.stmt.raw")) {
                return(TRUE)
            }
        }
    }
    FALSE
}

.mojor_ssa_fusion_policy <- function(policy = NULL) {
    default <- list(
        fusion_allow_control_flow_simple = FALSE, fusion_allow_broadcast_nd_identity = FALSE
    )
    if (exists(".mojor_state", inherits = TRUE)) {
        opts <- tryCatch(
            get(".mojor_state", inherits = TRUE)$options,
            error = function(e) NULL
        )
        if (is.list(opts)) {
            default$fusion_allow_control_flow_simple <- isTRUE(opts$fusion_allow_control_flow_simple)
            default$fusion_allow_broadcast_nd_identity <- isTRUE(opts$fusion_allow_broadcast_nd_identity)
        }
    }
    if (is.null(policy) ||
        !is.list(policy)) {
        return(default)
    }
    default$fusion_allow_control_flow_simple <- isTRUE(policy$fusion_allow_control_flow_simple)
    default$fusion_allow_broadcast_nd_identity <- isTRUE(policy$fusion_allow_broadcast_nd_identity)
    default
}

.mojor_ssa_fusion_loop_alias_map <- function(ssa, loop_nodes) {
    alias_map <- list()
    if (length(loop_nodes) ==
        0) {
        return(alias_map)
    }
    for (bn in loop_nodes) {
        if (!bn %in% names(ssa$blocks))
            next
        blk <- ssa$blocks[[bn]]
        if (is.null(blk$stmts) ||
            length(blk$stmts) ==
                0)
            next
        for (st in blk$stmts) {
            if (is.null(st) ||
                !is.list(st) ||
                is.null(st$op))
                next
            if (.mojor_ssa_is_index_cast_op(st$op) &&
                !is.null(st$id) &&
                .mojor_ssa_is_value_ref(st$id) &&
                !is.null(st$args) &&
                length(st$args) ==
                  1 && .mojor_ssa_is_value_ref(st$args[[1]])) {
                alias_map[[as.character(st$id)]] <- as.character(st$args[[1]])
            }
        }
    }
    alias_map
}

.mojor_ssa_fusion_resolve_ref <- function(ref, alias_map) {
    if (!.mojor_ssa_is_value_ref(ref)) {
        return(ref)
    }
    if (exists(".mojor_ssa_resolve_alias", mode = "function")) {
        return(.mojor_ssa_resolve_alias(ref, alias_map))
    }
    ref
}

.mojor_ssa_fusion_loop_induction_ref <- function(ssa, loop_rec) {
    header <- if (is.null(loop_rec$header))
        "" else as.character(loop_rec$header)
    if (!nzchar(header) ||
        !header %in% names(ssa$blocks)) {
        return("")
    }
    hblk <- ssa$blocks[[header]]
    if (is.null(hblk$params) ||
        length(hblk$params) ==
            0) {
        return("")
    }
    if (!is.null(hblk$params[[1]]$id) &&
        .mojor_ssa_is_value_ref(hblk$params[[1]]$id)) {
        return(as.character(hblk$params[[1]]$id))
    }
    ""
}

.mojor_ssa_fusion_store_signature <- function(stmt, resolve_ref) {
    if (is.null(stmt) ||
        !is.list(stmt) ||
        is.null(stmt$op)) {
        return(NULL)
    }
    op_can <- .mojor_ssa_op_canonical(stmt$op)
    if (!op_can %in% c(
        "mojor.mem.store", "mojor.mem.store_index", "mojor.mem.store_subscript"
    )) {
        return(NULL)
    }
    args <- if (is.null(stmt$args))
        character(0) else as.character(stmt$args)
    if (length(args) <
        3) {
        return(NULL)
    }
    base <- args[[1]]
    idx <- args[2:(length(args) -
        1)]
    idx_norm <- vapply(
        idx, function(arg) {
            if (.mojor_ssa_is_value_ref(arg)) {
                return(resolve_ref(arg))
            }
            as.character(arg)
        }, character(1)
    )
    paste0(base, "::", paste(idx_norm, collapse = ","))
}

.mojor_ssa_fusion_loop_identity_writes <- function(ssa, loop_rec) {
    iv <- .mojor_ssa_fusion_loop_induction_ref(ssa, loop_rec)
    if (!nzchar(iv)) {
        return(FALSE)
    }
    loop_nodes <- if (is.null(loop_rec$nodes))
        character(0) else as.character(loop_rec$nodes)
    alias_map <- .mojor_ssa_fusion_loop_alias_map(ssa, loop_nodes)
    resolve_ref <- function(ref) .mojor_ssa_fusion_resolve_ref(ref, alias_map)

    saw_store <- FALSE
    for (bn in loop_nodes) {
        if (!bn %in% names(ssa$blocks))
            next
        blk <- ssa$blocks[[bn]]
        if (is.null(blk$stmts) ||
            length(blk$stmts) ==
                0)
            next
        for (st in blk$stmts) {
            if (is.null(st) ||
                !is.list(st) ||
                is.null(st$op))
                next
            op_can <- .mojor_ssa_op_canonical(st$op)
            if (!op_can %in% c(
                "mojor.mem.store", "mojor.mem.store_index", "mojor.mem.store_subscript"
            ))
                next
            idx_ops <- .mojor_ssa_memory_index_operands(st)
            if (length(idx_ops) ==
                0) {
                return(FALSE)
            }
            idx_refs <- idx_ops[vapply(idx_ops, .mojor_ssa_is_value_ref, logical(1))]
            if (length(idx_refs) ==
                0) {
                return(FALSE)
            }
            idx_refs <- vapply(idx_refs, resolve_ref, character(1))
            if (!all(idx_refs == iv)) {
                return(FALSE)
            }
            saw_store <- TRUE
        }
    }
    isTRUE(saw_store)
}

.mojor_ssa_fusion_loop_broadcast_signatures <- function(ssa, loop_rec) {
    loop_nodes <- if (is.null(loop_rec$nodes))
        character(0) else as.character(loop_rec$nodes)
    sig <- character(0)
    if (length(loop_nodes) ==
        0) {
        return(sig)
    }
    for (bn in loop_nodes) {
        if (!bn %in% names(ssa$blocks))
            next
        blk <- ssa$blocks[[bn]]
        if (is.null(blk$stmts) ||
            length(blk$stmts) ==
                0)
            next
        for (st in blk$stmts) {
            args <- if (is.null(st$args))
                character(0) else as.character(st$args)
            if (length(args) ==
                0)
                next
            hit <- grepl("_mojor_bcast_index", args, fixed = TRUE)
            if (!any(hit))
                next
            raw <- args[hit]
            norm <- gsub("%[A-Za-z0-9_\\.]+", "%v", raw)
            norm <- gsub("\\s+", "", norm)
            sig <- c(sig, norm)
        }
    }
    unique(sig)
}

.mojor_ssa_fusion_identity_safe_broadcast_pair <- function(ssa, left, right) {
    if (!isTRUE(.mojor_ssa_fusion_loop_identity_writes(ssa, left)) ||
        !isTRUE(.mojor_ssa_fusion_loop_identity_writes(ssa, right))) {
        return(FALSE)
    }
    left_sig <- .mojor_ssa_fusion_loop_broadcast_signatures(ssa, left)
    right_sig <- .mojor_ssa_fusion_loop_broadcast_signatures(ssa, right)
    if (length(left_sig) ==
        0 || length(right_sig) ==
        0) {
        return(FALSE)
    }
    identical(
        sort(unique(left_sig)),
        sort(unique(right_sig))
    )
}

.mojor_ssa_fusion_loop_control_profile <- function(ssa, loop_rec) {
    loop_nodes <- if (is.null(loop_rec$nodes))
        character(0) else as.character(loop_rec$nodes)
    header <- if (is.null(loop_rec$header))
        "" else as.character(loop_rec$header)
    forbidden <- .mojor_ssa_fusion_loop_has_forbidden_control(ssa, loop_nodes)
    if (length(loop_nodes) ==
        0) {
        return(
            list(has_control_flow = TRUE, simple_if_assign = FALSE, forbidden = TRUE)
        )
    }

    cond_blocks <- character(0)
    for (bn in loop_nodes) {
        if (!bn %in% names(ssa$blocks))
            next
        blk <- ssa$blocks[[bn]]
        term <- blk$term
        if (is.null(term) ||
            !is.list(term) ||
            is.null(term$kind))
            next
        if (identical(
            as.character(term$kind),
            "condbr"
        ) &&
            !identical(bn, header)) {
            cond_blocks <- c(cond_blocks, bn)
        }
    }
    cond_blocks <- unique(cond_blocks)
    has_cf <- isTRUE(forbidden) ||
        length(cond_blocks) >
            0L
    if (!isTRUE(has_cf)) {
        return(
            list(
                has_control_flow = FALSE, simple_if_assign = FALSE, forbidden = isTRUE(forbidden)
            )
        )
    }
    if (isTRUE(forbidden) ||
        length(cond_blocks) !=
            1L) {
        return(
            list(
                has_control_flow = TRUE, simple_if_assign = FALSE, forbidden = isTRUE(forbidden)
            )
        )
    }

    cond_blk <- ssa$blocks[[cond_blocks[[1]]]]
    term <- cond_blk$term
    then_bn <- if (!is.null(term$then))
        as.character(term$then) else ""
    else_bn <- if (!is.null(term[["else"]]))
        as.character(term[["else"]]) else if (!is.null(term$term_else))
        as.character(term$term_else) else ""
    if (!nzchar(then_bn) ||
        !nzchar(else_bn) ||
        !then_bn %in% loop_nodes || !else_bn %in% loop_nodes) {
        return(
            list(
                has_control_flow = TRUE, simple_if_assign = FALSE, forbidden = isTRUE(forbidden)
            )
        )
    }
    if (!then_bn %in% names(ssa$blocks) ||
        !else_bn %in% names(ssa$blocks)) {
        return(
            list(
                has_control_flow = TRUE, simple_if_assign = FALSE, forbidden = isTRUE(forbidden)
            )
        )
    }

    then_blk <- ssa$blocks[[then_bn]]
    else_blk <- ssa$blocks[[else_bn]]
    then_term <- then_blk$term
    else_term <- else_blk$term
    if (is.null(then_term) ||
        is.null(else_term) ||
        !identical(
            as.character(then_term$kind),
            "br"
        ) ||
        !identical(
            as.character(else_term$kind),
            "br"
        )) {
        return(
            list(
                has_control_flow = TRUE, simple_if_assign = FALSE, forbidden = isTRUE(forbidden)
            )
        )
    }
    merge_bn <- if (!is.null(then_term$target))
        as.character(then_term$target) else ""
    merge_else <- if (!is.null(else_term$target))
        as.character(else_term$target) else ""
    if (!nzchar(merge_bn) ||
        !identical(merge_bn, merge_else) ||
        !merge_bn %in% loop_nodes || !merge_bn %in% names(ssa$blocks)) {
        return(
            list(
                has_control_flow = TRUE, simple_if_assign = FALSE, forbidden = isTRUE(forbidden)
            )
        )
    }
    merge_term <- ssa$blocks[[merge_bn]]$term
    if (is.null(merge_term) ||
        !is.list(merge_term) ||
        !identical(
            as.character(merge_term$kind),
            "br"
        )) {
        return(
            list(
                has_control_flow = TRUE, simple_if_assign = FALSE, forbidden = isTRUE(forbidden)
            )
        )
    }
    merge_target <- if (!is.null(merge_term$target))
        as.character(merge_term$target) else ""
    if (!nzchar(merge_target) ||
        !identical(merge_target, header)) {
        return(
            list(
                has_control_flow = TRUE, simple_if_assign = FALSE, forbidden = isTRUE(forbidden)
            )
        )
    }

    loop_nodes_other <- setdiff(cond_blocks, cond_blocks[[1]])
    if (length(loop_nodes_other) >
        0) {
        return(
            list(
                has_control_flow = TRUE, simple_if_assign = FALSE, forbidden = isTRUE(forbidden)
            )
        )
    }

    alias_map <- .mojor_ssa_fusion_loop_alias_map(ssa, loop_nodes)
    resolve_ref <- function(ref) .mojor_ssa_fusion_resolve_ref(ref, alias_map)
    then_sigs <- unique(
        Filter(
            Negate(is.null),
            lapply(
                then_blk$stmts, .mojor_ssa_fusion_store_signature, resolve_ref = resolve_ref
            )
        )
    )
    else_sigs <- unique(
        Filter(
            Negate(is.null),
            lapply(
                else_blk$stmts, .mojor_ssa_fusion_store_signature, resolve_ref = resolve_ref
            )
        )
    )
    if (length(then_sigs) !=
        1L || length(else_sigs) !=
        1L || !identical(then_sigs[[1]], else_sigs[[1]])) {
        return(
            list(
                has_control_flow = TRUE, simple_if_assign = FALSE, forbidden = isTRUE(forbidden)
            )
        )
    }

    list(
        has_control_flow = TRUE, simple_if_assign = TRUE, forbidden = isTRUE(forbidden)
    )
}

.mojor_ssa_fusion_loop_effect_flags <- function(ssa, loop_nodes) {
    has_rng <- FALSE
    has_unknown <- FALSE
    if (length(loop_nodes) ==
        0) {
        return(list(has_rng = TRUE, has_unknown = TRUE))
    }
    for (bn in loop_nodes) {
        if (!bn %in% names(ssa$blocks))
            next
        blk <- ssa$blocks[[bn]]
        if (is.null(blk$stmts) ||
            length(blk$stmts) ==
                0)
            next
        for (st in blk$stmts) {
            if (is.null(st) ||
                !is.list(st) ||
                is.null(st$op))
                next
            eff <- if (!is.null(st$effect_class)) {
                as.character(st$effect_class)
            } else {
                .mojor_ssa_stmt_effect_resource_summary(st)$effect_class
            }
            if (identical(eff, "RNG"))
                has_rng <- TRUE
            if (identical(eff, "Unknown"))
                has_unknown <- TRUE
            if (isTRUE(has_rng) &&
                isTRUE(has_unknown)) {
                return(list(has_rng = TRUE, has_unknown = TRUE))
            }
        }
    }
    list(has_rng = has_rng, has_unknown = has_unknown)
}

.mojor_ssa_fusion_identity_index_map <- function(ssa, loop_rec) {
    header <- if (is.null(loop_rec$header))
        "" else as.character(loop_rec$header)
    if (!nzchar(header) ||
        !header %in% names(ssa$blocks)) {
        return(FALSE)
    }
    hblk <- ssa$blocks[[header]]
    if (is.null(hblk$params) ||
        length(hblk$params) ==
            0) {
        return(FALSE)
    }

    iv <- NULL
    if (!is.null(hblk$params[[1]]$id) &&
        .mojor_ssa_is_value_ref(hblk$params[[1]]$id)) {
        iv <- as.character(hblk$params[[1]]$id)
    }
    if (is.null(iv) ||
        !nzchar(iv)) {
        return(FALSE)
    }

    alias_map <- list()
    saw_index_access <- FALSE
    loop_nodes <- if (is.null(loop_rec$nodes))
        character(0) else as.character(loop_rec$nodes)
    for (bn in loop_nodes) {
        if (!bn %in% names(ssa$blocks))
            next
        blk <- ssa$blocks[[bn]]
        if (is.null(blk$stmts) ||
            length(blk$stmts) ==
                0)
            next
        for (st in blk$stmts) {
            if (is.null(st) ||
                !is.list(st) ||
                is.null(st$op))
                next
            if (.mojor_ssa_is_index_cast_op(st$op) &&
                !is.null(st$id) &&
                .mojor_ssa_is_value_ref(st$id) &&
                !is.null(st$args) &&
                length(st$args) ==
                  1 && .mojor_ssa_is_value_ref(st$args[[1]])) {
                alias_map[[as.character(st$id)]] <- as.character(st$args[[1]])
            }
        }
    }

    resolve_ref <- function(ref) {
        if (!.mojor_ssa_is_value_ref(ref)) {
            return(ref)
        }
        if (exists(".mojor_ssa_resolve_alias", mode = "function")) {
            return(.mojor_ssa_resolve_alias(ref, alias_map))
        }
        ref
    }

    for (bn in loop_nodes) {
        if (!bn %in% names(ssa$blocks))
            next
        blk <- ssa$blocks[[bn]]
        if (is.null(blk$stmts) ||
            length(blk$stmts) ==
                0)
            next
        for (st in blk$stmts) {
            if (is.null(st) ||
                !is.list(st) ||
                is.null(st$op))
                next
            op_can <- .mojor_ssa_op_canonical(st$op)
            if (!op_can %in% c(
                "mojor.mem.load", "mojor.mem.index", "mojor.mem.store",
                "mojor.mem.store_index", "mojor.mem.store_subscript"
            )) {
                next
            }
            idx_ops <- .mojor_ssa_memory_index_operands(st)
            if (length(idx_ops) ==
                0)
                next
            saw_index_access <- TRUE
            idx_refs <- idx_ops[vapply(idx_ops, .mojor_ssa_is_value_ref, logical(1))]
            if (length(idx_refs) ==
                0) {
                return(FALSE)
            }
            idx_refs <- vapply(idx_refs, resolve_ref, character(1))
            if (!all(idx_refs == iv)) {
                return(FALSE)
            }
        }
    }

    isTRUE(saw_index_access)
}

.mojor_ssa_fusion_guard_policy_mismatch <- function(left_attrs, right_attrs) {
    if (is.null(left_attrs) ||
        !is.list(left_attrs) ||
        is.null(right_attrs) ||
        !is.list(right_attrs)) {
        return(FALSE)
    }
    keys <- c("na_guard", "bounds_check", "bounds_policy")
    for (k in keys) {
        lv <- left_attrs[[k]]
        rv <- right_attrs[[k]]
        if (is.null(lv) ||
            is.null(rv))
            next
        if (!identical(lv, rv)) {
            return(TRUE)
        }
    }
    FALSE
}

.mojor_ssa_fusion_candidate_analysis <- function(ssa, recovery = NULL, policy = NULL) {
    ssa <- .mojor_ssa_require_semantic_ir(ssa, where = "SSA fusion candidate analysis")
    fusion_policy <- .mojor_ssa_fusion_policy(policy)
    rec <- recovery
    if (is.null(rec) &&
        exists(".mojor_loop_recovery_analysis", mode = "function")) {
        rec <- tryCatch(
            .mojor_loop_recovery_analysis(ssa, require_reducible = FALSE, require_single_exit = FALSE),
            error = function(e) NULL
        )
    }

    loops <- .mojor_ssa_fusion_loop_records(ssa, recovery = rec)
    if (length(loops) <
        2L) {
        return(
            list(
                kind = "fusion_candidate_analysis", status = "NoCandidates",
                analysis_only = TRUE, recovery_status = if (!is.null(rec) &&
                  !is.null(rec$status)) as.character(rec$status) else "Unavailable",
                candidates = list(), diagnostics = list()
            )
        )
    }

    reason_order <- c(
        "FUSE_REJECT_DOMAIN_MISMATCH", "FUSE_REJECT_INDEXMAP_NOT_IDENTITY",
        "FUSE_REJECT_NOALIAS_MISSING", "FUSE_REJECT_CONTROL_FLOW", "FUSE_REJECT_EFFECTS_RNG",
        "FUSE_REJECT_EFFECTS_UNKNOWN", "FUSE_REJECT_BROADCAST_ND", "FUSE_REJECT_GUARD_POLICY"
    )
    candidates <- list()
    diagnostics <- list()

    for (i in seq_len(
        length(loops) -
            1L
    )) {
        left <- loops[[i]]
        right <- loops[[i + 1L]]
        reject <- setNames(
            as.list(rep(FALSE, length(reason_order))),
            reason_order
        )
        same_kind <- !is.null(left$kind) &&
            !is.null(right$kind) &&
            identical(
                as.character(left$kind),
                as.character(right$kind)
            )

        if (is.null(left$loop_id) ||
            is.null(right$loop_id) ||
            !nzchar(as.character(left$loop_id)) ||
            !nzchar(as.character(right$loop_id)) ||
            !isTRUE(same_kind)) {
            reject[["FUSE_REJECT_DOMAIN_MISMATCH"]] <- TRUE
        }
        if (!.mojor_ssa_fusion_identity_index_map(ssa, left) ||
            !.mojor_ssa_fusion_identity_index_map(ssa, right)) {
            reject[["FUSE_REJECT_INDEXMAP_NOT_IDENTITY"]] <- TRUE
        }
        if (!isTRUE(left$attrs$fusion_noalias_proven) ||
            !isTRUE(right$attrs$fusion_noalias_proven)) {
            reject[["FUSE_REJECT_NOALIAS_MISSING"]] <- TRUE
        }
        control_left <- .mojor_ssa_fusion_loop_control_profile(ssa, left)
        control_right <- .mojor_ssa_fusion_loop_control_profile(ssa, right)
        has_control <- isTRUE(control_left$has_control_flow) ||
            isTRUE(control_right$has_control_flow)
        if (has_control) {
            allow_simple <- isTRUE(fusion_policy$fusion_allow_control_flow_simple)
            simple_ok <- allow_simple && (!isTRUE(control_left$has_control_flow) ||
                isTRUE(control_left$simple_if_assign)) &&
                (!isTRUE(control_right$has_control_flow) ||
                  isTRUE(control_right$simple_if_assign))
            if (!isTRUE(simple_ok)) {
                reject[["FUSE_REJECT_CONTROL_FLOW"]] <- TRUE
            }
        }
        domain <- if (isTRUE(same_kind) &&
            identical(
                as.character(left$kind),
                "for"
            ) &&
            !isTRUE(has_control)) {
            "single_basic_loop"
        } else {
            "structured_or_unknown"
        }

        eff_l <- .mojor_ssa_fusion_loop_effect_flags(ssa, left$nodes)
        eff_r <- .mojor_ssa_fusion_loop_effect_flags(ssa, right$nodes)
        if (isTRUE(eff_l$has_rng) ||
            isTRUE(eff_r$has_rng)) {
            reject[["FUSE_REJECT_EFFECTS_RNG"]] <- TRUE
        }
        if (isTRUE(eff_l$has_unknown) ||
            isTRUE(eff_r$has_unknown)) {
            reject[["FUSE_REJECT_EFFECTS_UNKNOWN"]] <- TRUE
        }

        has_broadcast <- FALSE
        for (bn in unique(c(left$nodes, right$nodes))) {
            if (!bn %in% names(ssa$blocks))
                next
            if (.mojor_ssa_fusion_block_has_broadcast(ssa$blocks[[bn]])) {
                has_broadcast <- TRUE
                break
            }
        }
        if (isTRUE(has_broadcast)) {
            allow_bcast <- isTRUE(fusion_policy$fusion_allow_broadcast_nd_identity) &&
                .mojor_ssa_fusion_identity_safe_broadcast_pair(ssa, left, right)
            if (!isTRUE(allow_bcast))
                reject[["FUSE_REJECT_BROADCAST_ND"]] <- TRUE
        }

        if (.mojor_ssa_fusion_guard_policy_mismatch(left$attrs, right$attrs)) {
            reject[["FUSE_REJECT_GUARD_POLICY"]] <- TRUE
        }

        reasons <- reason_order[as.logical(unlist(reject, use.names = FALSE))]
        accepted <- length(reasons) ==
            0L
        cand <- list(
            producer = as.character(left$loop_id),
            consumer = as.character(right$loop_id),
            domain = domain, accepted = accepted, reasons = reasons
        )
        candidates[[length(candidates) +
            1L]] <- cand

        if (!accepted) {
            for (code in reasons) {
                diagnostics[[length(diagnostics) +
                  1L]] <- list(
                  code = as.character(code),
                  severity = "warning", block = if (!is.null(left$header)) as.character(left$header) else "",
                  message = sprintf(
                    "Fusion candidate '%s' -> '%s' rejected by %s", as.character(left$loop_id),
                    as.character(right$loop_id),
                    as.character(code)
                ),
                  hint = "P3 allows analysis-only fusion diagnostics; rewrite transforms are deferred post-P3"
              )
            }
        }
    }

    list(
        kind = "fusion_candidate_analysis", status = "Completed", analysis_only = TRUE,
        recovery_status = if (!is.null(rec) &&
            !is.null(rec$status)) as.character(rec$status) else "Unavailable",
        candidates = candidates, diagnostics = diagnostics
    )
}

.mojor_ssa_attach_fusion_candidate_analysis <- function(ssa, analysis = NULL, policy = NULL) {
    ssa <- .mojor_ssa_require_semantic_ir(ssa, where = "SSA attach fusion candidate analysis")
    out <- ssa
    out$fusion_analysis <- if (!is.null(analysis)) {
        analysis
    } else {
        .mojor_ssa_fusion_candidate_analysis(ssa, policy = policy)
    }
    out
}

.mojor_ssa_fusion_stmt_is_loop_next <- function(stmt) {
    if (is.null(stmt) ||
        !is.list(stmt) ||
        is.null(stmt$op)) {
        return(FALSE)
    }
    op_can <- .mojor_ssa_op_canonical(stmt$op)
    identical(op_can, "mojor.cf.loop_next") ||
        identical(
            as.character(stmt$op),
            "loop_next"
        )
}

.mojor_ssa_fusion_replace_refs <- function(node, replace_map) {
    if (is.null(node) ||
        length(replace_map) ==
            0) {
        return(node)
    }
    if (is.character(node)) {
        out <- as.character(node)
        for (i in seq_along(out)) {
            ref <- out[[i]]
            if (!.mojor_ssa_is_value_ref(ref))
                next
            if (!ref %in% names(replace_map))
                next
            out[[i]] <- as.character(replace_map[[ref]])
        }
        return(out)
    }
    if (is.list(node)) {
        out <- node
        for (i in seq_along(out)) {
            out[[i]] <- .mojor_ssa_fusion_replace_refs(out[[i]], replace_map)
        }
        return(out)
    }
    node
}

.mojor_ssa_fusion_loop_shape <- function(ssa, loop_rec) {
    nodes <- if (is.null(loop_rec$nodes))
        character(0) else as.character(loop_rec$nodes)
    header <- if (is.null(loop_rec$header))
        "" else as.character(loop_rec$header)
    if (!nzchar(header) ||
        !header %in% names(ssa$blocks)) {
        return(list(ok = FALSE, reason = "FUSE_REWRITE_SKIP_LEFT_SHAPE"))
    }
    if (length(nodes) ==
        0) {
        return(list(ok = FALSE, reason = "FUSE_REWRITE_SKIP_LEFT_SHAPE"))
    }
    hblk <- ssa$blocks[[header]]
    if (is.null(hblk$term) ||
        !is.list(hblk$term) ||
        !identical(
            as.character(hblk$term$kind),
            "condbr"
        )) {
        return(list(ok = FALSE, reason = "FUSE_REWRITE_SKIP_LEFT_SHAPE"))
    }
    body <- if (!is.null(hblk$term$then))
        as.character(hblk$term$then) else ""
    exit <- if (!is.null(hblk$term[["else"]]))
        as.character(hblk$term[["else"]]) else if (!is.null(hblk$term$term_else))
        as.character(hblk$term$term_else) else ""
    if (!nzchar(body) ||
        !nzchar(exit) ||
        !body %in% names(ssa$blocks) ||
        !exit %in% names(ssa$blocks)) {
        return(list(ok = FALSE, reason = "FUSE_REWRITE_SKIP_LEFT_SHAPE"))
    }
    exit_in_nodes <- exit %in% nodes
    exit_blk <- ssa$blocks[[exit]]
    exit_attrs <- if (!is.null(exit_blk$attrs) &&
        is.list(exit_blk$attrs))
        exit_blk$attrs else list()
    loop_id <- if (is.null(loop_rec$loop_id))
        "" else as.character(loop_rec$loop_id)
    exit_role_ok <- nzchar(loop_id) &&
        identical(
            as.character(exit_attrs$loop_id),
            loop_id
        ) &&
        identical(
            as.character(exit_attrs$loop_role),
            "exit"
        )
    break_block <- if (is.null(loop_rec$break_block))
        "" else as.character(loop_rec$break_block)
    break_attr <- if (is.null(loop_rec$attrs$loop_break_dest))
        "" else as.character(loop_rec$attrs$loop_break_dest)
    exit_break_ok <- (nzchar(break_block) &&
        identical(exit, break_block)) ||
        (nzchar(break_attr) &&
            identical(exit, break_attr))
    if (!body %in% nodes || (!isTRUE(exit_in_nodes) &&
        !isTRUE(exit_role_ok) &&
        !isTRUE(exit_break_ok))) {
        return(list(ok = FALSE, reason = "FUSE_REWRITE_SKIP_LEFT_SHAPE"))
    }
    bblk <- ssa$blocks[[body]]
    if (is.null(bblk$term) ||
        !is.list(bblk$term) ||
        !identical(
            as.character(bblk$term$kind),
            "br"
        )) {
        return(list(ok = FALSE, reason = "FUSE_REWRITE_SKIP_LEFT_SHAPE"))
    }
    if (is.null(bblk$term$target) ||
        !identical(
            as.character(bblk$term$target),
            header
        )) {
        return(list(ok = FALSE, reason = "FUSE_REWRITE_SKIP_LEFT_SHAPE"))
    }
    if (is.null(hblk$params) ||
        length(hblk$params) ==
            0 || is.null(hblk$params[[1]]$id) ||
        !.mojor_ssa_is_value_ref(hblk$params[[1]]$id)) {
        return(list(ok = FALSE, reason = "FUSE_REWRITE_SKIP_LEFT_SHAPE"))
    }
    iv <- as.character(hblk$params[[1]]$id)
    stmts <- if (is.null(bblk$stmts))
        list() else bblk$stmts
    if (length(stmts) ==
        0) {
        return(list(ok = FALSE, reason = "FUSE_REWRITE_SKIP_LEFT_SHAPE"))
    }
    next_idx <- which(vapply(stmts, .mojor_ssa_fusion_stmt_is_loop_next, logical(1)))
    if (length(next_idx) !=
        1) {
        return(list(ok = FALSE, reason = "FUSE_REWRITE_SKIP_LEFT_SHAPE"))
    }
    list(
        ok = TRUE, header = header, body = body, exit = exit, iv = iv,
        loop_next_idx = as.integer(next_idx[[1]])
    )
}

.mojor_ssa_fusion_loop_effects_pure <- function(ssa, loop_nodes) {
    eff <- .mojor_ssa_fusion_loop_effect_flags(ssa, loop_nodes)
    !isTRUE(eff$has_rng) &&
        !isTRUE(eff$has_unknown)
}

.mojor_ssa_fusion_pair_safe_shape <- function(ssa, left, right, candidate = NULL) {
    reasons <- character(0)

    cand_domain <- if (!is.null(candidate$domain))
        as.character(candidate$domain) else ""
    if (nzchar(cand_domain) &&
        !identical(cand_domain, "single_basic_loop")) {
        reasons <- c(reasons, "FUSE_REWRITE_SKIP_UNSAFE_DOMAIN")
    }

    left_kind <- if (is.null(left$kind))
        "" else as.character(left$kind)
    right_kind <- if (is.null(right$kind))
        "" else as.character(right$kind)
    if (!identical(left_kind, "for") ||
        !identical(right_kind, "for")) {
        reasons <- c(reasons, "FUSE_REWRITE_SKIP_UNSAFE_DOMAIN")
    }

    control_left <- .mojor_ssa_fusion_loop_control_profile(ssa, left)
    control_right <- .mojor_ssa_fusion_loop_control_profile(ssa, right)
    if (isTRUE(control_left$has_control_flow) ||
        isTRUE(control_right$has_control_flow)) {
        reasons <- c(reasons, "FUSE_REWRITE_SKIP_UNSAFE_DOMAIN")
    }

    if (!.mojor_ssa_fusion_identity_index_map(ssa, left) ||
        !.mojor_ssa_fusion_identity_index_map(ssa, right)) {
        reasons <- c(reasons, "FUSE_REWRITE_SKIP_INDEXMAP_NOT_IDENTITY")
    }

    if (!isTRUE(left$attrs$fusion_noalias_proven) ||
        !isTRUE(right$attrs$fusion_noalias_proven)) {
        reasons <- c(reasons, "FUSE_REWRITE_SKIP_NOALIAS_MISSING")
    }

    if (!isTRUE(.mojor_ssa_fusion_loop_effects_pure(ssa, left$nodes)) ||
        !isTRUE(.mojor_ssa_fusion_loop_effects_pure(ssa, right$nodes))) {
        reasons <- c(reasons, "FUSE_REWRITE_SKIP_EFFECTS_NOT_PURE")
    }

    reasons <- unique(as.character(reasons))
    list(
        ok = length(reasons) ==
            0L, reasons = reasons
    )
}

.mojor_ssa_fusion_try_rewrite_pair <- function(ssa, left, right) {
    left_shape <- .mojor_ssa_fusion_loop_shape(ssa, left)
    if (!isTRUE(left_shape$ok)) {
        return(list(applied = FALSE, reason = left_shape$reason))
    }
    right_shape <- .mojor_ssa_fusion_loop_shape(ssa, right)
    if (!isTRUE(right_shape$ok)) {
        reason <- sub(
            "^FUSE_REWRITE_SKIP_LEFT_SHAPE$", "FUSE_REWRITE_SKIP_RIGHT_SHAPE",
            as.character(right_shape$reason)
        )
        return(list(applied = FALSE, reason = reason))
    }

    left_exit_blk <- ssa$blocks[[left_shape$exit]]
    if (is.null(left_exit_blk$term) ||
        !is.list(left_exit_blk$term) ||
        !identical(
            as.character(left_exit_blk$term$kind),
            "br"
        ) ||
        is.null(left_exit_blk$term$target) ||
        !identical(
            as.character(left_exit_blk$term$target),
            right_shape$header
        )) {
        return(list(applied = FALSE, reason = "FUSE_REWRITE_SKIP_NON_ADJACENT"))
    }

    right_body <- ssa$blocks[[right_shape$body]]
    right_stmts <- if (is.null(right_body$stmts))
        list() else right_body$stmts
    if (length(right_stmts) ==
        0) {
        return(list(applied = FALSE, reason = "FUSE_REWRITE_SKIP_RIGHT_SHAPE"))
    }
    right_payload <- right_stmts[!vapply(right_stmts, .mojor_ssa_fusion_stmt_is_loop_next, logical(1))]
    collect_value_ids <- function(ir) {
        ids <- character(0)
        if (is.null(ir$blocks) ||
            !is.list(ir$blocks)) {
            return(ids)
        }
        for (blk in ir$blocks) {
            if (!is.null(blk$params) &&
                length(blk$params) >
                  0) {
                for (p in blk$params) {
                  if (!is.null(p$id) &&
                    .mojor_ssa_is_value_ref(p$id)) {
                    ids <- c(ids, as.character(p$id))
                  }
                }
            }
            if (is.null(blk$stmts) ||
                length(blk$stmts) ==
                  0)
                next
            for (st in blk$stmts) {
                if (!is.null(st$id) &&
                  .mojor_ssa_is_value_ref(st$id)) {
                  ids <- c(ids, as.character(st$id))
                }
            }
        }
        unique(ids)
    }
    next_fusion_id <- 1L
    used_ids <- collect_value_ids(ssa)
    rename_map <- list()
    make_fresh_id <- function() {
        repeat {
            candidate <- sprintf("%%fuse_%d", as.integer(next_fusion_id))
            next_fusion_id <<- next_fusion_id + 1L
            if (!(candidate %in% used_ids)) {
                return(candidate)
            }
        }
    }
    for (st in right_payload) {
        if (is.null(st) ||
            !is.list(st) ||
            is.null(st$id) ||
            !.mojor_ssa_is_value_ref(st$id))
            next
        sid <- as.character(st$id)
        if (sid %in% used_ids || sid %in% names(rename_map)) {
            fresh <- make_fresh_id()
            rename_map[[sid]] <- fresh
            used_ids <- c(used_ids, fresh)
        } else {
            used_ids <- c(used_ids, sid)
        }
    }

    replace_map <- c(
        stats::setNames(
            list(left_shape$iv),
            right_shape$iv
        ),
        rename_map
    )
    right_payload <- lapply(
        right_payload, .mojor_ssa_fusion_replace_refs, replace_map = replace_map
    )

    out <- ssa
    left_body <- out$blocks[[left_shape$body]]
    left_stmts <- if (is.null(left_body$stmts))
        list() else left_body$stmts
    idx <- left_shape$loop_next_idx
    left_body$stmts <- c(
        if (idx > 1) left_stmts[seq_len(idx - 1)] else list(),
        right_payload, left_stmts[seq.int(idx, length(left_stmts))]
    )
    out$blocks[[left_shape$body]] <- left_body

    left_header <- out$blocks[[left_shape$header]]
    term <- left_header$term
    term[["else"]] <- right_shape$exit
    if (!is.null(term$term_else))
        term$term_else <- right_shape$exit
    term$else_args <- character(0)
    left_header$term <- term
    attrs <- if (is.null(left_header$attrs) ||
        !is.list(left_header$attrs))
        list() else left_header$attrs
    attrs$loop_break_dest <- right_shape$exit
    left_header$attrs <- attrs
    out$blocks[[left_shape$header]] <- left_header

    if (!is.null(out$loop_metadata) &&
        is.list(out$loop_metadata)) {
        left_id <- if (is.null(left$loop_id))
            "" else as.character(left$loop_id)
        right_id <- if (is.null(right$loop_id))
            "" else as.character(right$loop_id)
        if (nzchar(left_id) &&
            left_id %in% names(out$loop_metadata) &&
            !is.null(out$loop_metadata[[left_id]]$break_dest) &&
            is.list(out$loop_metadata[[left_id]]$break_dest)) {
            out$loop_metadata[[left_id]]$break_dest$block <- right_shape$exit
        }
        if (nzchar(right_id) &&
            right_id %in% names(out$loop_metadata)) {
            out$loop_metadata[[right_id]] <- NULL
        }
    }

    list(
        applied = TRUE, ssa = out, producer = as.character(left$loop_id),
        consumer = as.character(right$loop_id),
        header = left_shape$header, body = left_shape$body, exit = right_shape$exit
    )
}

.mojor_ssa_fusion_rewrite_constrained <- function(ssa, analysis = NULL, recovery = NULL, policy = NULL) {
    ssa <- .mojor_ssa_require_semantic_ir(ssa, where = "SSA fusion constrained rewrite")
    fa_initial <- if (!is.null(analysis)) {
        analysis
    } else {
        .mojor_ssa_fusion_candidate_analysis(ssa, recovery = recovery, policy = policy)
    }
    if (is.null(fa_initial$candidates) ||
        length(fa_initial$candidates) ==
            0) {
        return(
            list(
                kind = "fusion_rewrite", status = "NoCandidates", mode = "constrained",
                applied_count = 0L, applied_pairs = list(), eligible_pairs = list(),
                diagnostics = list(), ssa = ssa
            )
        )
    }

    accepted_initial <- Filter(
        function(ca) isTRUE(ca$accepted),
        fa_initial$candidates
    )
    if (length(accepted_initial) ==
        0) {
        return(
            list(
                kind = "fusion_rewrite", status = "NoAcceptedCandidates",
                mode = "constrained", applied_count = 0L, applied_pairs = list(),
                eligible_pairs = list(), diagnostics = list(), ssa = ssa
            )
        )
    }

    out <- ssa
    applied_pairs <- list()
    diagnostics <- list()
    round_fa <- fa_initial
    round_recovery <- recovery
    max_rounds <- 64L
    round <- 0L
    repeat {
        round <- round + 1L
        if (round > max_rounds)
            break

        accepted <- if (!is.null(round_fa$candidates) &&
            is.list(round_fa$candidates)) {
            Filter(
                function(ca) isTRUE(ca$accepted),
                round_fa$candidates
            )
        } else {
            list()
        }
        if (length(accepted) ==
            0L)
            break

        applied_this_round <- FALSE
        for (ca in accepted) {
            loops <- .mojor_ssa_fusion_loop_records(out, recovery = round_recovery)
            left <- NULL
            right <- NULL
            for (lr in loops) {
                if (is.null(left) &&
                  identical(
                    as.character(lr$loop_id),
                    as.character(ca$producer)
                ))
                  left <- lr
                if (is.null(right) &&
                  identical(
                    as.character(lr$loop_id),
                    as.character(ca$consumer)
                ))
                  right <- lr
            }
            if (is.null(left) ||
                is.null(right)) {
                diagnostics[[length(diagnostics) +
                  1L]] <- list(
                  code = "FUSE_REWRITE_SKIP_MISSING_LOOP", severity = "info",
                  message = sprintf(
                    "Fusion rewrite skipped for '%s' -> '%s': loop metadata not found in current SSA",
                    as.character(ca$producer),
                    as.character(ca$consumer)
                )
              )
                next
            }
            safe_gate <- .mojor_ssa_fusion_pair_safe_shape(out, left, right, candidate = ca)
            if (!isTRUE(safe_gate$ok)) {
                for (code in safe_gate$reasons) {
                  diagnostics[[length(diagnostics) +
                    1L]] <- list(
                    code = as.character(code),
                    severity = "info", message = sprintf(
                      "Fusion rewrite skipped for '%s' -> '%s': %s", as.character(ca$producer),
                      as.character(ca$consumer),
                      as.character(code)
                  )
                )
                }
                next
            }
            attempt <- .mojor_ssa_fusion_try_rewrite_pair(out, left, right)
            if (!isTRUE(attempt$applied)) {
                diagnostics[[length(diagnostics) +
                  1L]] <- list(
                  code = as.character(attempt$reason),
                  severity = "info", message = sprintf(
                    "Fusion rewrite skipped for '%s' -> '%s': %s", as.character(ca$producer),
                    as.character(ca$consumer),
                    as.character(attempt$reason)
                )
              )
                next
            }
            out <- attempt$ssa
            out <- .mojor_ir_ssa_prune_unreachable(out)
            applied_pairs[[length(applied_pairs) +
                1L]] <- list(
                producer = as.character(attempt$producer),
                consumer = as.character(attempt$consumer),
                header = as.character(attempt$header),
                body = as.character(attempt$body),
                exit = as.character(attempt$exit)
            )
            applied_this_round <- TRUE
            break
        }

        if (!isTRUE(applied_this_round))
            break

        if (exists(".mojor_loop_recovery_analysis", mode = "function")) {
            round_recovery <- tryCatch(
                .mojor_loop_recovery_analysis(out, require_reducible = FALSE, require_single_exit = FALSE),
                error = function(e) round_recovery
            )
        }
        round_fa <- .mojor_ssa_fusion_candidate_analysis(out, recovery = round_recovery, policy = policy)
    }

    list(
        kind = "fusion_rewrite", status = if (length(applied_pairs) >
            0) "Applied" else "NoChange", mode = "constrained", applied_count = as.integer(length(applied_pairs)),
        applied_pairs = applied_pairs, eligible_pairs = applied_pairs,
        diagnostics = diagnostics, ssa = out
    )
}

.mojor_ssa_fusion_rewrite_skeleton <- function(ssa, analysis = NULL, recovery = NULL, policy = NULL) {
    ssa <- .mojor_ssa_require_semantic_ir(ssa, where = "SSA fusion rewrite skeleton")
    fa <- if (!is.null(analysis)) {
        analysis
    } else {
        .mojor_ssa_fusion_candidate_analysis(ssa, recovery = recovery, policy = policy)
    }

    candidates <- if (!is.null(fa$candidates) &&
        is.list(fa$candidates))
        fa$candidates else list()
    accepted <- Filter(
        function(ca) isTRUE(ca$accepted),
        candidates
    )
    eligible_pairs <- lapply(
        accepted, function(ca) {
            list(
                producer = if (is.null(ca$producer)) "" else as.character(ca$producer),
                consumer = if (is.null(ca$consumer)) "" else as.character(ca$consumer),
                domain = if (is.null(ca$domain)) "" else as.character(ca$domain),
                reasons = if (is.null(ca$reasons)) character(0) else as.character(ca$reasons)
            )
        }
    )

    diagnostics <- list()
    status <- "NoAcceptedCandidates"
    if (length(eligible_pairs) >
        0L) {
        status <- "SkeletonNoOp"
        diagnostics[[1L]] <- list(
            code = "FUSE_REWRITE_SKELETON_NOOP", severity = "info", message = sprintf(
                "Fusion rewrite skeleton identified %d legality-accepted pair(s); rewrite is intentionally disabled",
                as.integer(length(eligible_pairs))
            )
        )
    }

    list(
        kind = "fusion_rewrite", status = status, mode = "skeleton", applied_count = 0L,
        applied_pairs = list(), eligible_pairs = eligible_pairs, diagnostics = diagnostics,
        ssa = ssa
    )
}

.mojor_ssa_stmt_is_pure <- function(stmt) {
    if (is.null(stmt) ||
        !is.list(stmt) ||
        is.null(stmt$kind) ||
        stmt$kind != "ssa_stmt") {
        return(FALSE)
    }
    info <- .mojor_ssa_stmt_effect_resource_summary(stmt)
    identical(info$effect_class, "None")
}

.mojor_ssa_op_schema_interfaces <- function(op) {
    if (is.null(op) ||
        !is.character(op) ||
        length(op) !=
            1 || !nzchar(op)) {
        return(character(0))
    }
    if (!exists(".mojor_ir_op_schema_get", mode = "function")) {
        return(character(0))
    }
    schema <- tryCatch(
        .mojor_ir_op_schema_get(op, canonicalize = TRUE),
        error = function(e) NULL
    )
    if (is.null(schema) ||
        is.null(schema$interfaces) ||
        length(schema$interfaces) ==
            0) {
        return(character(0))
    }
    as.character(schema$interfaces)
}

.mojor_ssa_op_canonical <- function(op) {
    if (is.null(op) ||
        !is.character(op) ||
        length(op) !=
            1 || !nzchar(op)) {
        return("")
    }
    if (!exists(".mojor_ir_op_canonicalize", mode = "function")) {
        return(op)
    }
    out <- tryCatch(
        .mojor_ir_op_canonicalize(op),
        error = function(e) op
    )
    if (is.null(out) ||
        !is.character(out) ||
        length(out) !=
            1) {
        return(op)
    }
    out
}

.mojor_ssa_type_normalize <- function(type_name) {
    if (is.null(type_name) ||
        length(type_name) ==
            0) {
        return("unknown")
    }
    raw <- trimws(as.character(type_name[[1]]))
    if (!nzchar(raw)) {
        return("unknown")
    }
    low <- tolower(raw)

    scalar_map <- c(
        i1 = "i1", bool = "i1", logical = "i1", lgl = "i1", i32 = "i32",
        int32 = "i32", int = "i32", i64 = "i64", int64 = "i64", f32 = "f32",
        float32 = "f32", single = "f32", f64 = "f64", float64 = "f64",
        double = "f64", index = "index", index_vector = "index_vector",
        unknown = "unknown"
    )
    mapped <- unname(scalar_map[low])
    if (length(mapped) ==
        1 && !is.na(mapped) &&
        nzchar(mapped)) {
        return(mapped)
    }

    arr_m <- regexec("^([A-Za-z][A-Za-z0-9_]*)\\[(?:([0-9]+)d)?\\]$", raw, perl = TRUE)
    arr_g <- regmatches(raw, arr_m)[[1]]
    if (length(arr_g) >
        0) {
        base <- .mojor_ssa_type_normalize(arr_g[[2]])
        if (identical(base, "unknown")) {
            return("unknown")
        }
        rank <- if (length(arr_g) >=
            3 && nzchar(arr_g[[3]]))
            as.character(as.integer(arr_g[[3]])) else "1"
        return(paste0("ref<", base, ",", rank, ">"))
    }

    ref_m <- regexec("^ref\\s*<\\s*([^,>]+)\\s*(?:,\\s*([^>]+)\\s*)?>$", raw, perl = TRUE)
    ref_g <- regmatches(raw, ref_m)[[1]]
    if (length(ref_g) >
        0) {
        elem <- .mojor_ssa_type_normalize(ref_g[[2]])
        elem_cls <- .mojor_ssa_type_class(elem)
        if (elem_cls %in% c("unknown", "ref", "index_vector")) {
            return("unknown")
        }

        rank_raw <- if (length(ref_g) >=
            3)
            trimws(ref_g[[3]]) else ""
        rank <- "?"
        if (nzchar(rank_raw) &&
            grepl("^[0-9]+$", rank_raw)) {
            rank <- as.character(as.integer(rank_raw))
        } else if (nzchar(rank_raw) &&
            !(rank_raw %in% c("?", "dynamic", "dyn", "*"))) {
            rank <- "?"
        }
        return(paste0("ref<", elem, ",", rank, ">"))
    }

    "unknown"
}

.mojor_ssa_type_class <- function(type_name) {
    t <- .mojor_ssa_type_normalize(type_name)
    if (startsWith(t, "ref<")) {
        return("ref")
    }
    if (identical(t, "index_vector")) {
        return("index_vector")
    }
    if (identical(t, "index")) {
        return("index")
    }
    if (identical(t, "i1")) {
        return("bool")
    }
    if (t %in% c("i32", "i64")) {
        return("int")
    }
    if (t %in% c("f32", "f64")) {
        return("float")
    }
    "unknown"
}

.mojor_ssa_type_is_known <- function(type_name) {
    !identical(
        .mojor_ssa_type_normalize(type_name),
        "unknown"
    )
}

.mojor_ssa_type_is_bool <- function(type_name) {
    identical(
        .mojor_ssa_type_class(type_name),
        "bool"
    )
}

.mojor_ssa_type_is_intlike <- function(type_name) {
    cls <- .mojor_ssa_type_class(type_name)
    cls %in% c("int", "index")
}

.mojor_ssa_type_is_floatlike <- function(type_name) {
    identical(
        .mojor_ssa_type_class(type_name),
        "float"
    )
}

.mojor_ssa_type_is_index_operand <- function(type_name) {
    cls <- .mojor_ssa_type_class(type_name)
    cls %in% c("index", "index_vector")
}

.mojor_ssa_ref_type_parts <- function(type_name) {
    t <- .mojor_ssa_type_normalize(type_name)
    m <- regexec("^ref<([^,>]+),([^>]+)>$", t, perl = TRUE)
    g <- regmatches(t, m)[[1]]
    if (length(g) ==
        0) {
        return(NULL)
    }
    list(
        elem = trimws(g[[2]]),
        rank = trimws(g[[3]])
    )
}

.mojor_ssa_type_compatible <- function(actual, expected) {
    act <- .mojor_ssa_type_normalize(actual)
    exp <- .mojor_ssa_type_normalize(expected)
    if (identical(act, "unknown") ||
        identical(exp, "unknown")) {
        return(TRUE)
    }
    if (identical(act, exp)) {
        return(TRUE)
    }

    act_ref <- .mojor_ssa_ref_type_parts(act)
    exp_ref <- .mojor_ssa_ref_type_parts(exp)
    if (!is.null(act_ref) &&
        !is.null(exp_ref)) {
        if (!identical(act_ref$elem, exp_ref$elem)) {
            return(FALSE)
        }
        if (identical(act_ref$rank, exp_ref$rank) ||
            identical(act_ref$rank, "?") ||
            identical(exp_ref$rank, "?")) {
            return(TRUE)
        }
        return(FALSE)
    }

    FALSE
}

.mojor_ssa_type_promote_numeric <- function(lhs_type, rhs_type) {
    l <- .mojor_ssa_type_normalize(lhs_type)
    r <- .mojor_ssa_type_normalize(rhs_type)
    if (identical(l, "unknown") ||
        identical(r, "unknown")) {
        return("unknown")
    }

    l_cls <- .mojor_ssa_type_class(l)
    r_cls <- .mojor_ssa_type_class(r)

    if (l_cls == "float" || r_cls == "float") {
        if (l %in% c("f64") ||
            r %in% c("f64")) {
            return("f64")
        }
        return("f32")
    }

    if (l_cls %in% c("int", "index") &&
        r_cls %in% c("int", "index")) {
        if (identical(l, "index") &&
            identical(r, "index")) {
            return("index")
        }
        if (identical(l, "index") ||
            identical(r, "index")) {
            return("unknown")
        }
        if (l %in% c("i64") ||
            r %in% c("i64")) {
            return("i64")
        }
        return("i32")
    }

    if (l_cls == "bool" && r_cls == "bool") {
        return("i1")
    }
    if (l_cls == "bool" && r_cls %in% c("int", "index")) {
        return(r)
    }
    if (r_cls == "bool" && l_cls %in% c("int", "index")) {
        return(l)
    }
    if (l_cls == "bool" && r_cls == "float") {
        return(r)
    }
    if (r_cls == "bool" && l_cls == "float") {
        return(l)
    }

    "unknown"
}

.mojor_ssa_operand_type <- function(operand, value_types = list(), symbol_types = list()) {
    if (is.null(operand) ||
        length(operand) ==
            0) {
        return("unknown")
    }
    opnd <- as.character(operand[[1]])
    if (!nzchar(opnd)) {
        return("unknown")
    }

    if (.mojor_ssa_is_value_ref(opnd)) {
        vt <- value_types[[opnd]]
        if (is.null(vt)) {
            return("unknown")
        }
        return(.mojor_ssa_type_normalize(vt))
    }

    sym_t <- symbol_types[[opnd]]
    if (!is.null(sym_t)) {
        return(.mojor_ssa_type_normalize(sym_t))
    }

    if (opnd %in% c("TRUE", "FALSE", "True", "False")) {
        return("i1")
    }
    if (grepl("^-?[0-9]+$", opnd)) {
        return("i32")
    }
    if (tolower(opnd) %in%
        c("inf", "-inf", "nan")) {
        return("f64")
    }
    if (grepl("^-?[0-9]*\\.[0-9]+([eE][+-]?[0-9]+)?$", opnd) ||
        grepl("^-?[0-9]+\\.[0-9]*([eE][+-]?[0-9]+)?$", opnd) ||
        grepl("^[+-]?[0-9]+[eE][+-]?[0-9]+$", opnd)) {
        return("f64")
    }

    "unknown"
}

.mojor_ssa_stmt_declared_type <- function(st) {
    if (is.null(st) ||
        !is.list(st)) {
        return("unknown")
    }
    if (!is.null(st$type)) {
        return(.mojor_ssa_type_normalize(st$type))
    }
    if (!is.null(st$result_type)) {
        return(.mojor_ssa_type_normalize(st$result_type))
    }
    if (!is.null(st$value_type)) {
        return(.mojor_ssa_type_normalize(st$value_type))
    }
    "unknown"
}

.mojor_ssa_const_type_from_value <- function(value) {
    if (is.null(value)) {
        return("unknown")
    }
    v <- as.character(value[[1]])
    if (!nzchar(v)) {
        return("unknown")
    }
    if (v %in% c("TRUE", "FALSE", "True", "False")) {
        return("i1")
    }
    if (tolower(v) %in%
        c("inf", "-inf", "nan")) {
        return("f64")
    }
    if (grepl("^-?[0-9]+$", v)) {
        return("i32")
    }
    if (grepl("^-?[0-9]*\\.[0-9]+([eE][+-]?[0-9]+)?$", v) ||
        grepl("^-?[0-9]+\\.[0-9]*([eE][+-]?[0-9]+)?$", v) ||
        grepl("^[+-]?[0-9]+[eE][+-]?[0-9]+$", v)) {
        return("f64")
    }
    "unknown"
}

.mojor_ssa_is_index_cast_op <- function(op) {
    op_can <- .mojor_ssa_op_canonical(op)
    op_raw <- as.character(op)
    identical(op_raw, "index_cast") ||
        startsWith(op_raw, "index_cast:") ||
        identical(op_can, "mojor.arith.index_cast") ||
        startsWith(op_can, "mojor.arith.index_cast.") ||
        identical(op_can, "mojor.arith.cast.index")
}

.mojor_ssa_is_compare_op <- function(op) {
    op_can <- .mojor_ssa_op_canonical(op)
    op_raw <- as.character(op)
    op_can %in% c(
        "mojor.arith.cmp_gt", "mojor.arith.cmp_lt", "mojor.arith.cmp_ge",
        "mojor.arith.cmp_le", "mojor.arith.cmp_eq", "mojor.arith.cmp_ne"
    ) ||
        startsWith(op_can, "mojor.arith.cmpi") ||
        startsWith(op_can, "mojor.arith.cmpf") ||
        op_raw %in% c("binop:>", "binop:<", "binop:>=", "binop:<=", "binop:==", "binop:!=")
}

.mojor_ssa_compare_kind <- function(op, lhs_type, rhs_type) {
    op_can <- .mojor_ssa_op_canonical(op)
    op_raw <- as.character(op)
    if (startsWith(op_can, "mojor.arith.cmpi")) {
        return("cmpi")
    }
    if (startsWith(op_can, "mojor.arith.cmpf")) {
        return("cmpf")
    }

    lhs_cls <- .mojor_ssa_type_class(lhs_type)
    rhs_cls <- .mojor_ssa_type_class(rhs_type)
    if (lhs_cls == "unknown" || rhs_cls == "unknown") {
        return("unknown")
    }

    is_eq_ne <- op_can %in% c("mojor.arith.cmp_eq", "mojor.arith.cmp_ne") ||
        op_raw %in% c("binop:==", "binop:!=")
    if (is_eq_ne && lhs_cls == "bool" && rhs_cls == "bool") {
        return("cmpi")
    }

    if (lhs_cls %in% c("int", "index") &&
        rhs_cls %in% c("int", "index")) {
        return("cmpi")
    }
    if (lhs_cls == "float" && rhs_cls == "float") {
        return("cmpf")
    }
    "mismatch"
}

.mojor_ssa_memory_index_operands <- function(stmt) {
    if (is.null(stmt) ||
        !is.list(stmt) ||
        is.null(stmt$op)) {
        return(character(0))
    }
    op_can <- .mojor_ssa_op_canonical(stmt$op)
    args <- if (is.null(stmt$args))
        character(0) else as.character(stmt$args)
    if (length(args) ==
        0) {
        return(character(0))
    }

    if (op_can %in% c("mojor.mem.load", "mojor.mem.index")) {
        if (length(args) <=
            1) {
            return(character(0))
        }
        return(args[2:length(args)])
    }
    if (op_can %in% c(
        "mojor.mem.store", "mojor.mem.store_index", "mojor.mem.store_subscript"
    )) {
        if (length(args) <=
            2) {
            return(character(0))
        }
        return(
            args[2:(length(args) -
                1)]
        )
    }
    if (op_can %in% c("mojor.cf.loop_cond", "mojor.cf.loop_next")) {
        return(args)
    }
    character(0)
}

.mojor_ssa_stmt_infer_type <- function(st, value_types = list(), symbol_types = list()) {
    if (is.null(st) ||
        !is.list(st) ||
        is.null(st$kind) ||
        !identical(st$kind, "ssa_stmt")) {
        return("unknown")
    }

    declared <- .mojor_ssa_stmt_declared_type(st)
    if (!identical(declared, "unknown")) {
        return(declared)
    }

    op <- as.character(st$op)
    op_can <- .mojor_ssa_op_canonical(op)
    args <- if (is.null(st$args))
        character(0) else as.character(st$args)
    arg_types <- if (length(args) ==
        0) {
        character(0)
    } else {
        vapply(
            args, .mojor_ssa_operand_type, character(1),
            value_types = value_types, symbol_types = symbol_types
        )
    }

    if (op_can %in% c("mojor.arith.const", "mojor.misc.const_null") ||
        identical(op, "const")) {
        return(.mojor_ssa_const_type_from_value(st$value))
    }

    if (.mojor_ssa_is_index_cast_op(op)) {
        return("index")
    }

    if (startsWith(op_can, "mojor.arith.cast.")) {
        cast_to <- sub("^mojor\\.arith\\.cast\\.", "", op_can)
        return(.mojor_ssa_type_normalize(cast_to))
    }
    if (startsWith(op, "cast:")) {
        cast_to <- sub("^cast:", "", op)
        return(.mojor_ssa_type_normalize(cast_to))
    }

    if (op_can %in% c("mojor.mem.load", "mojor.mem.index") ||
        identical(op, "index")) {
        if (length(arg_types) >=
            1) {
            base_ref <- .mojor_ssa_ref_type_parts(arg_types[[1]])
            if (!is.null(base_ref)) {
                return(base_ref$elem)
            }
        }
        return("unknown")
    }

    if (.mojor_ssa_is_compare_op(op)) {
        return("i1")
    }

    if (op_can %in% c("mojor.arith.and", "mojor.arith.or", "mojor.arith.not") ||
        op %in% c("binop:&&", "binop:||", "unop:!")) {
        return("i1")
    }

    if (op_can %in% c(
        "mojor.arith.add", "mojor.arith.sub", "mojor.arith.mul", "mojor.arith.div",
        "mojor.arith.mod", "mojor.arith.idiv"
    ) ||
        op %in% c("binop:+", "binop:-", "binop:*", "binop:/", "binop:%%", "binop:%/%")) {
        if (length(arg_types) <
            2) {
            return("unknown")
        }
        return(.mojor_ssa_type_promote_numeric(arg_types[[1]], arg_types[[2]]))
    }

    if (op_can %in% c("mojor.arith.select") ||
        identical(op, "select")) {
        if (length(arg_types) <
            3) {
            return("unknown")
        }
        return(.mojor_ssa_type_promote_numeric(arg_types[[2]], arg_types[[3]]))
    }

    if (op_can %in% c("mojor.cf.loop_cond")) {
        return("i1")
    }
    if (op_can %in% c("mojor.cf.loop_next")) {
        return("index")
    }

    "unknown"
}

.mojor_ssa_edge_args_to <- function(term, target) {
    if (is.null(term) ||
        !is.list(term) ||
        is.null(term$kind) ||
        is.null(target)) {
        return(character(0))
    }
    if (identical(term$kind, "br") &&
        !is.null(term$target) &&
        identical(
            as.character(term$target),
            as.character(target)
        )) {
        if (is.null(term$args)) {
            return(character(0))
        }
        return(as.character(term$args))
    }
    if (identical(term$kind, "condbr")) {
        if (!is.null(term$then) &&
            identical(
                as.character(term$then),
                as.character(target)
            )) {
            if (is.null(term$then_args)) {
                return(character(0))
            }
            return(as.character(term$then_args))
        }
        if (!is.null(term[["else"]]) &&
            identical(
                as.character(term[["else"]]),
                as.character(target)
            )) {
            if (is.null(term$else_args)) {
                return(character(0))
            }
            return(as.character(term$else_args))
        }
    }
    character(0)
}

.mojor_ssa_collect_declared_symbol_types <- function(ssa) {
    out <- list()
    if (!is.null(ssa$type_env) &&
        is.list(ssa$type_env) &&
        length(ssa$type_env) >
            0) {
        for (nm in names(ssa$type_env)) {
            t <- .mojor_ssa_type_normalize(ssa$type_env[[nm]])
            if (!identical(t, "unknown"))
                out[[nm]] <- t
        }
    }
    if (!is.null(ssa$blocks) &&
        is.list(ssa$blocks) &&
        length(ssa$blocks) >
            0) {
        for (bn in names(ssa$blocks)) {
            blk <- ssa$blocks[[bn]]
            if (is.null(blk$params) ||
                length(blk$params) ==
                  0)
                next
            for (p in blk$params) {
                if (is.null(p$var) ||
                  is.null(p$type))
                  next
                t <- .mojor_ssa_type_normalize(p$type)
                if (identical(t, "unknown"))
                  next
                out[[as.character(p$var)]] <- t
            }
        }
    }
    out
}

.mojor_ssa_infer_value_types <- function(ssa, max_iter = 12L) {
    ssa <- .mojor_ssa_require_semantic_ir(ssa, where = "SSA infer_value_types")
    value_types <- list()
    symbol_types <- .mojor_ssa_collect_declared_symbol_types(ssa)

    for (bn in names(ssa$blocks)) {
        blk <- ssa$blocks[[bn]]
        if (!is.null(blk$params) &&
            length(blk$params) >
                0) {
            for (p in blk$params) {
                if (!is.null(p$id) &&
                  .mojor_ssa_is_value_ref(p$id) &&
                  !is.null(p$type)) {
                  t <- .mojor_ssa_type_normalize(p$type)
                  if (!identical(t, "unknown"))
                    value_types[[as.character(p$id)]] <- t
                }
                if (!is.null(p$var) &&
                  !is.null(p$type)) {
                  t <- .mojor_ssa_type_normalize(p$type)
                  if (!identical(t, "unknown"))
                    symbol_types[[as.character(p$var)]] <- t
                }
            }
        }
        if (is.null(blk$stmts) ||
            length(blk$stmts) ==
                0)
            next
        for (st in blk$stmts) {
            if (!is.null(st$id) &&
                .mojor_ssa_is_value_ref(st$id)) {
                t <- .mojor_ssa_stmt_declared_type(st)
                if (!identical(t, "unknown"))
                  value_types[[as.character(st$id)]] <- t
            }
        }
    }

    built <- .mojor_ssa_build_preds_dom(ssa, .mojor_ssa_successors)
    preds <- built$preds

    for (iter in seq_len(max_iter)) {
        changed <- FALSE

        for (bn in names(ssa$blocks)) {
            blk <- ssa$blocks[[bn]]
            if (is.null(blk$stmts) ||
                length(blk$stmts) ==
                  0)
                next
            for (st in blk$stmts) {
                if (is.null(st$id) ||
                  !.mojor_ssa_is_value_ref(st$id))
                  next
                id <- as.character(st$id)
                cur <- value_types[[id]]
                cur_norm <- if (is.null(cur))
                  "unknown" else .mojor_ssa_type_normalize(cur)
                inf <- .mojor_ssa_stmt_infer_type(st, value_types = value_types, symbol_types = symbol_types)
                if (!identical(inf, "unknown") &&
                  identical(cur_norm, "unknown")) {
                  value_types[[id]] <- inf
                  changed <- TRUE
                }
            }
        }

        for (bn in names(ssa$blocks)) {
            blk <- ssa$blocks[[bn]]
            if (is.null(blk$params) ||
                length(blk$params) ==
                  0)
                next
            pb <- preds[[bn]]
            if (is.null(pb) ||
                length(pb) ==
                  0)
                next
            for (i in seq_along(blk$params)) {
                p <- blk$params[[i]]
                if (is.null(p$id) ||
                  !.mojor_ssa_is_value_ref(p$id))
                  next
                pid <- as.character(p$id)
                cur <- value_types[[pid]]
                cur_norm <- if (is.null(cur))
                  "unknown" else .mojor_ssa_type_normalize(cur)
                if (!identical(cur_norm, "unknown"))
                  next

                cand <- character(0)
                for (pred_bn in pb) {
                  pred_blk <- ssa$blocks[[pred_bn]]
                  edge_args <- .mojor_ssa_edge_args_to(pred_blk$term, bn)
                  if (length(edge_args) <
                    i)
                    next
                  arg_ty <- .mojor_ssa_operand_type(
                    edge_args[[i]], value_types = value_types, symbol_types = symbol_types
                )
                  if (.mojor_ssa_type_is_known(arg_ty))
                    cand <- c(cand, arg_ty)
                }
                cand <- unique(cand)
                if (length(cand) ==
                  0)
                  next

                if (all(vapply(cand, .mojor_ssa_type_is_intlike, logical(1)))) {
                  value_types[[pid]] <- "index"
                  changed <- TRUE
                } else if (length(cand) ==
                  1L) {
                  value_types[[pid]] <- cand[[1]]
                  changed <- TRUE
                }
            }
        }

        for (bn in names(ssa$blocks)) {
            blk <- ssa$blocks[[bn]]
            if (is.null(blk$stmts) ||
                length(blk$stmts) ==
                  0)
                next
            for (st in blk$stmts) {
                idx_ops <- .mojor_ssa_memory_index_operands(st)
                if (length(idx_ops) ==
                  0)
                  next
                for (opnd in idx_ops) {
                  if (!.mojor_ssa_is_value_ref(opnd))
                    next
                  cur <- value_types[[opnd]]
                  cur_norm <- if (is.null(cur))
                    "unknown" else .mojor_ssa_type_normalize(cur)
                  cur_cls <- .mojor_ssa_type_class(cur_norm)
                  if (cur_cls %in% c("unknown", "int", "index")) {
                    if (!identical(cur_norm, "index")) {
                      value_types[[opnd]] <- "index"
                      changed <- TRUE
                    }
                  }
                }
            }
        }

        for (bn in names(ssa$blocks)) {
            blk <- ssa$blocks[[bn]]
            term <- blk$term
            if (is.null(term) ||
                !is.list(term) ||
                is.null(term$kind))
                next

            if (identical(term$kind, "br") &&
                !is.null(term$target) &&
                !is.null(term$args) &&
                length(term$args) >
                  0) {
                tgt <- as.character(term$target)
                tgt_blk <- ssa$blocks[[tgt]]
                if (!is.null(tgt_blk$params) &&
                  length(tgt_blk$params) >
                    0) {
                  for (i in seq_along(term$args)) {
                    if (length(tgt_blk$params) <
                      i)
                      next
                    param <- tgt_blk$params[[i]]
                    if (is.null(param$id) ||
                      !.mojor_ssa_is_value_ref(param$id))
                      next
                    pty <- value_types[[as.character(param$id)]]
                    if (!identical(
                      .mojor_ssa_type_normalize(pty),
                      "index"
                  ))
                      next
                    arg <- as.character(term$args[[i]])
                    if (!.mojor_ssa_is_value_ref(arg))
                      next
                    aty <- .mojor_ssa_type_normalize(value_types[[arg]])
                    if (.mojor_ssa_type_class(aty) %in%
                      c("unknown", "int", "index") &&
                      !identical(aty, "index")) {
                      value_types[[arg]] <- "index"
                      changed <- TRUE
                    }
                  }
                }
            }

            if (identical(term$kind, "condbr")) {
                edges <- list(
                  list(
                    target = term$then, args = if (is.null(term$then_args)) character(0) else as.character(term$then_args)
                ),
                  list(
                    target = term[["else"]], args = if (is.null(term$else_args)) character(0) else as.character(term$else_args)
                )
              )
                for (edge in edges) {
                  tgt <- as.character(edge$target)
                  tgt_blk <- ssa$blocks[[tgt]]
                  if (is.null(tgt_blk$params) ||
                    length(tgt_blk$params) ==
                      0 || length(edge$args) ==
                    0)
                    next
                  for (i in seq_along(edge$args)) {
                    if (length(tgt_blk$params) <
                      i)
                      next
                    param <- tgt_blk$params[[i]]
                    if (is.null(param$id) ||
                      !.mojor_ssa_is_value_ref(param$id))
                      next
                    pty <- value_types[[as.character(param$id)]]
                    if (!identical(
                      .mojor_ssa_type_normalize(pty),
                      "index"
                  ))
                      next
                    arg <- as.character(edge$args[[i]])
                    if (!.mojor_ssa_is_value_ref(arg))
                      next
                    aty <- .mojor_ssa_type_normalize(value_types[[arg]])
                    if (.mojor_ssa_type_class(aty) %in%
                      c("unknown", "int", "index") &&
                      !identical(aty, "index")) {
                      value_types[[arg]] <- "index"
                      changed <- TRUE
                    }
                  }
                }
            }
        }

        if (!isTRUE(changed))
            break
    }

    list(
        value_types = value_types, symbol_types = symbol_types, preds = preds
    )
}

.mojor_ssa_collect_typing_diagnostics <- function(ssa, opts = NULL) {
    ssa <- .mojor_ssa_require_semantic_ir(ssa, where = "SSA typing diagnostics")
    inferred <- .mojor_ssa_infer_value_types(ssa)
    value_types <- inferred$value_types
    symbol_types <- inferred$symbol_types
    diagnostics <- list()

    add_diag <- function(code, block, message, hint = NULL, stmt_index = NULL) {
        d <- list(
            code = as.character(code),
            severity = "error", block = as.character(block),
            message = as.character(message)
        )
        if (!is.null(hint) &&
            nzchar(as.character(hint)))
            d$hint <- as.character(hint)
        if (!is.null(stmt_index))
            d$stmt_index <- as.integer(stmt_index)
        diagnostics[[length(diagnostics) +
            1L]] <<- d
    }

    for (bn in names(ssa$blocks)) {
        blk <- ssa$blocks[[bn]]
        term <- blk$term
        if (!is.null(term) &&
            is.list(term) &&
            !is.null(term$kind)) {
            if (identical(term$kind, "condbr")) {
                cond_ty <- .mojor_ssa_operand_type(term$cond, value_types = value_types, symbol_types = symbol_types)
                if (.mojor_ssa_type_is_known(cond_ty) &&
                  !.mojor_ssa_type_is_bool(cond_ty)) {
                  add_diag(
                    "TYPE_CONDBR_COND_TYPE", bn, sprintf(
                      "condbr condition has type '%s' (expected i1/bool)",
                      cond_ty
                  ),
                    "Ensure condition-producing compare/logical ops yield i1"
                )
                }
            }

            if (identical(term$kind, "ret") &&
                !is.null(term$value) &&
                !is.null(ssa$return_type)) {
                ret_ty <- .mojor_ssa_type_normalize(ssa$return_type)
                val_ty <- .mojor_ssa_operand_type(term$value, value_types = value_types, symbol_types = symbol_types)
                if (.mojor_ssa_type_is_known(ret_ty) &&
                  .mojor_ssa_type_is_known(val_ty) &&
                  !.mojor_ssa_type_compatible(val_ty, ret_ty)) {
                  add_diag(
                    "TYPE_RET_MISMATCH", bn, sprintf(
                      "ret value type '%s' is incompatible with function return type '%s'",
                      val_ty, ret_ty
                  ),
                    "Insert explicit casts or align return type annotations"
                )
                }
            }

            edges <- list()
            if (identical(term$kind, "br")) {
                edges[[1L]] <- list(
                  target = term$target, args = if (is.null(term$args)) character(0) else as.character(term$args)
              )
            } else if (identical(term$kind, "condbr")) {
                edges[[1L]] <- list(
                  target = term$then, args = if (is.null(term$then_args)) character(0) else as.character(term$then_args)
              )
                edges[[2L]] <- list(
                  target = term[["else"]], args = if (is.null(term$else_args)) character(0) else as.character(term$else_args)
              )
            }

            if (length(edges) >
                0) {
                for (edge in edges) {
                  tgt <- as.character(edge$target)
                  tgt_blk <- ssa$blocks[[tgt]]
                  if (is.null(tgt_blk) ||
                    is.null(tgt_blk$params) ||
                    length(tgt_blk$params) ==
                      0)
                    next
                  if (length(edge$args) ==
                    0)
                    next
                  for (i in seq_along(edge$args)) {
                    if (length(tgt_blk$params) <
                      i)
                      next
                    param <- tgt_blk$params[[i]]
                    if (is.null(param$type))
                      next
                    param_ty <- .mojor_ssa_type_normalize(param$type)
                    if (!.mojor_ssa_type_is_known(param_ty))
                      next
                    arg_ty <- .mojor_ssa_operand_type(
                      edge$args[[i]], value_types = value_types, symbol_types = symbol_types
                  )
                    if (!.mojor_ssa_type_is_known(arg_ty))
                      next
                    if (!.mojor_ssa_type_compatible(arg_ty, param_ty)) {
                      add_diag(
                        "TYPE_BRANCH_ARG_MISMATCH", bn, sprintf(
                          "branch arg %d to '%s' has type '%s' (target param expects '%s')",
                          i, tgt, arg_ty, param_ty
                      ),
                        "Add explicit casts before branch or align block parameter typing"
                    )
                    }
                  }
                }
            }
        }

        if (is.null(blk$stmts) ||
            length(blk$stmts) ==
                0)
            next
        for (idx in seq_along(blk$stmts)) {
            st <- blk$stmts[[idx]]
            if (is.null(st) ||
                !is.list(st) ||
                is.null(st$op))
                next

            if (.mojor_ssa_is_index_cast_op(st$op)) {
                args <- if (is.null(st$args))
                  character(0) else as.character(st$args)
                if (length(args) !=
                  1L) {
                  add_diag(
                    "TYPE_INDEX_CAST_ARITY", bn, sprintf("index_cast expects 1 operand, got %d", length(args)),
                    "Use exactly one integer/index operand for index_cast",
                    stmt_index = idx
                )
                } else {
                  src_ty <- .mojor_ssa_operand_type(args[[1]], value_types = value_types, symbol_types = symbol_types)
                  if (.mojor_ssa_type_is_known(src_ty) &&
                    !.mojor_ssa_type_is_intlike(src_ty)) {
                    add_diag(
                      "TYPE_INDEX_CAST_SOURCE_TYPE", bn, sprintf(
                        "index_cast source has type '%s' (expected integer/index)",
                        src_ty
                    ),
                      "Cast float/bool values to integer before index_cast",
                      stmt_index = idx
                  )
                  }
                }

                if (!is.null(st$id) &&
                  .mojor_ssa_is_value_ref(st$id)) {
                  dst_ty <- .mojor_ssa_type_normalize(value_types[[as.character(st$id)]])
                  if (.mojor_ssa_type_is_known(dst_ty) &&
                    !.mojor_ssa_type_is_intlike(dst_ty)) {
                    add_diag(
                      "TYPE_INDEX_CAST_RESULT_TYPE", bn, sprintf(
                        "index_cast result has non-index/integer type '%s'",
                        dst_ty
                    ),
                      "index_cast results must be index/integer typed",
                      stmt_index = idx
                  )
                  }
                }
            }

            if (.mojor_ssa_is_compare_op(st$op)) {
                args <- if (is.null(st$args))
                  character(0) else as.character(st$args)
                if (length(args) >=
                  2) {
                  lhs_ty <- .mojor_ssa_operand_type(args[[1]], value_types = value_types, symbol_types = symbol_types)
                  rhs_ty <- .mojor_ssa_operand_type(args[[2]], value_types = value_types, symbol_types = symbol_types)
                  cmp_kind <- .mojor_ssa_compare_kind(st$op, lhs_ty, rhs_ty)

                  if (identical(cmp_kind, "mismatch")) {
                    add_diag(
                      "TYPE_COMPARE_CLASS_MISMATCH", bn, sprintf(
                        "compare operands have incompatible types '%s' and '%s'",
                        lhs_ty, rhs_ty
                    ),
                      "Use explicit casts and typed compare conventions (cmpi/cmpf)",
                      stmt_index = idx
                  )
                  }

                  op_can <- .mojor_ssa_op_canonical(st$op)
                  if (startsWith(op_can, "mojor.arith.cmpi") &&
                    .mojor_ssa_type_is_known(lhs_ty) &&
                    .mojor_ssa_type_is_known(rhs_ty) &&
                    !(.mojor_ssa_type_is_intlike(lhs_ty) &&
                      .mojor_ssa_type_is_intlike(rhs_ty))) {
                    add_diag(
                      "TYPE_CMPI_OPERAND_TYPE", bn, sprintf(
                        "cmpi expects integer/index operands, got '%s' and '%s'",
                        lhs_ty, rhs_ty
                    ),
                      "Use cmpf for float operands or cast to integer/index first",
                      stmt_index = idx
                  )
                  }
                  if (startsWith(op_can, "mojor.arith.cmpf") &&
                    .mojor_ssa_type_is_known(lhs_ty) &&
                    .mojor_ssa_type_is_known(rhs_ty) &&
                    !(.mojor_ssa_type_is_floatlike(lhs_ty) &&
                      .mojor_ssa_type_is_floatlike(rhs_ty))) {
                    add_diag(
                      "TYPE_CMPF_OPERAND_TYPE", bn, sprintf(
                        "cmpf expects float operands, got '%s' and '%s'",
                        lhs_ty, rhs_ty
                    ),
                      "Use cmpi for integer/index operands or cast to float first",
                      stmt_index = idx
                  )
                  }
                }
            }

            idx_ops <- .mojor_ssa_memory_index_operands(st)
            if (length(idx_ops) >
                0) {
                for (opnd in idx_ops) {
                  idx_ty <- .mojor_ssa_operand_type(opnd, value_types = value_types, symbol_types = symbol_types)
                  if (.mojor_ssa_type_is_known(idx_ty) &&
                    !.mojor_ssa_type_is_index_operand(idx_ty)) {
                    add_diag(
                      "TYPE_INDEX_OPERAND_NOT_INDEX", bn, sprintf(
                        "index operand '%s' has type '%s' (expected index/index_vector)",
                        opnd, idx_ty
                    ),
                      "Insert index_cast and keep memory/IV operands in index type",
                      stmt_index = idx
                  )
                  }
                }
            }
        }
    }

    diagnostics
}

.mojor_ssa_annotate_types <- function(ssa) {
    ssa <- .mojor_ssa_require_semantic_ir(ssa, where = "SSA annotate_types")
    inferred <- .mojor_ssa_infer_value_types(ssa)
    value_types <- inferred$value_types
    out <- ssa

    for (bn in names(out$blocks)) {
        blk <- out$blocks[[bn]]
        if (!is.null(blk$params) &&
            length(blk$params) >
                0) {
            for (i in seq_along(blk$params)) {
                p <- blk$params[[i]]
                if (!is.null(p$id) &&
                  .mojor_ssa_is_value_ref(p$id)) {
                  t <- value_types[[as.character(p$id)]]
                  if (!is.null(t) &&
                    !identical(
                      .mojor_ssa_type_normalize(t),
                      "unknown"
                  )) {
                    blk$params[[i]]$type <- .mojor_ssa_type_normalize(t)
                  }
                }
            }
        }
        if (!is.null(blk$stmts) &&
            length(blk$stmts) >
                0) {
            for (i in seq_along(blk$stmts)) {
                st <- blk$stmts[[i]]
                if (!is.null(st$id) &&
                  .mojor_ssa_is_value_ref(st$id)) {
                  t <- value_types[[as.character(st$id)]]
                  if (!is.null(t) &&
                    !identical(
                      .mojor_ssa_type_normalize(t),
                      "unknown"
                  )) {
                    blk$stmts[[i]]$type <- .mojor_ssa_type_normalize(t)
                  }
                }
            }
        }
        out$blocks[[bn]] <- blk
    }

    out$type_summary <- list(value_types = value_types, symbol_types = inferred$symbol_types)
    out
}

.mojor_ssa_collect_value_refs <- function(values) {
    if (is.null(values) ||
        length(values) ==
            0) {
        return(character(0))
    }
    vals <- as.character(values)
    vals[vapply(vals, .mojor_ssa_is_value_ref, logical(1))]
}

.mojor_ssa_term_value_refs <- function(term) {
    if (is.null(term) ||
        !is.list(term) ||
        is.null(term$kind)) {
        return(character(0))
    }
    if (identical(term$kind, "ret")) {
        return(.mojor_ssa_collect_value_refs(term$value))
    }
    if (identical(term$kind, "br")) {
        return(.mojor_ssa_collect_value_refs(term$args))
    }
    if (identical(term$kind, "condbr")) {
        return(
            .mojor_ssa_collect_value_refs(c(term$cond, term$then_args, term$else_args))
        )
    }
    character(0)
}

.mojor_ssa_resolve_alias <- function(ref, alias_map) {
    if (!.mojor_ssa_is_value_ref(ref)) {
        return(ref)
    }
    cur <- ref
    seen <- character(0)
    while (!is.null(alias_map[[cur]]) &&
        .mojor_ssa_is_value_ref(alias_map[[cur]]) &&
        !identical(alias_map[[cur]], cur) &&
        !cur %in% seen) {
        seen <- c(seen, cur)
        cur <- alias_map[[cur]]
    }
    cur
}

.mojor_ssa_replace_aliases <- function(values, alias_map) {
    if (is.null(values) ||
        length(values) ==
            0) {
        return(values)
    }
    out <- values
    for (i in seq_along(out)) {
        if (.mojor_ssa_is_value_ref(out[[i]])) {
            out[[i]] <- .mojor_ssa_resolve_alias(out[[i]], alias_map)
        }
    }
    out
}

.mojor_ir_ssa_copy_propagate <- function(ssa) {
    ssa <- .mojor_ssa_require_semantic_ir(ssa, where = "SSA copy_propagate")
    if (is.null(ssa$blocks) ||
        !is.list(ssa$blocks) ||
        length(ssa$blocks) ==
            0) {
        return(ssa)
    }

    out <- ssa
    use_blocks <- list()
    add_use <- function(vals, bn) {
        refs <- .mojor_ssa_collect_value_refs(vals)
        if (length(refs) ==
            0) {
            return(invisible(NULL))
        }
        for (r in refs) {
            cur <- use_blocks[[r]]
            if (is.null(cur)) {
                use_blocks[[r]] <<- bn
            } else if (!bn %in% cur) {
                use_blocks[[r]] <<- c(cur, bn)
            }
        }
        invisible(NULL)
    }
    for (bn in names(out$blocks)) {
        blk <- out$blocks[[bn]]
        if (is.null(blk) ||
            !is.list(blk))
            next
        add_use(
            .mojor_ssa_term_value_refs(blk$term),
            bn
        )
        if (!is.null(blk$stmts) &&
            length(blk$stmts) >
                0) {
            for (st in blk$stmts) {
                if (is.null(st) ||
                  !is.list(st))
                  next
                add_use(st$args, bn)
            }
        }
    }

    for (bn in names(out$blocks)) {
        blk <- out$blocks[[bn]]
        if (is.null(blk) ||
            !is.list(blk))
            next

        alias_map <- list()
        def_stmt <- list()
        new_stmts <- list()
        stmts <- blk$stmts

        if (!is.null(stmts) &&
            length(stmts) >
                0) {
            for (st in stmts) {
                if (is.null(st) ||
                  !is.list(st) ||
                  is.null(st$kind))
                  next
                st2 <- st
                if (!is.null(st2$args) &&
                  length(st2$args) >
                    0) {
                  st2$args <- .mojor_ssa_replace_aliases(st2$args, alias_map)
                }

                if (!identical(st2$kind, "ssa_stmt") ||
                  is.null(st2$id) ||
                  !.mojor_ssa_is_value_ref(st2$id)) {
                  new_stmts <- c(new_stmts, list(st2))
                  next
                }

                aliased_to <- NULL
                op_can <- .mojor_ssa_op_canonical(st2$op)
                if ((identical(st2$op, "unop:+") ||
                  identical(op_can, "mojor.arith.pos")) &&
                  length(st2$args) ==
                    1 && .mojor_ssa_is_value_ref(st2$args[[1]])) {
                  aliased_to <- st2$args[[1]]
                } else if ((identical(st2$op, "select") ||
                  identical(op_can, "mojor.arith.select")) &&
                  length(st2$args) ==
                    3 && .mojor_ssa_is_value_ref(st2$args[[2]]) &&
                  identical(st2$args[[2]], st2$args[[3]])) {
                  aliased_to <- st2$args[[2]]
                } else if ((startsWith(st2$op, "cast:") ||
                  startsWith(op_can, "mojor.arith.cast.")) &&
                  length(st2$args) ==
                    1 && .mojor_ssa_is_value_ref(st2$args[[1]])) {
                  src <- st2$args[[1]]
                  src_def <- def_stmt[[src]]
                  src_def_can <- if (!is.null(src_def) &&
                    !is.null(src_def$op))
                    .mojor_ssa_op_canonical(src_def$op) else ""
                  if (!is.null(src_def) &&
                    is.list(src_def) &&
                    identical(src_def$kind, "ssa_stmt") &&
                    (identical(src_def$op, st2$op) ||
                      identical(src_def_can, op_can)) &&
                    length(src_def$args) ==
                      1) {
                    aliased_to <- src
                  }
                }

                use_blks <- use_blocks[[st2$id]]
                alias_is_local <- is.null(use_blks) ||
                  all(use_blks == bn)
                if (!is.null(aliased_to) &&
                  isTRUE(alias_is_local)) {
                  alias_map[[st2$id]] <- .mojor_ssa_resolve_alias(aliased_to, alias_map)
                  next
                }

                new_stmts <- c(new_stmts, list(st2))
                def_stmt[[st2$id]] <- st2
            }
        }

        blk$stmts <- new_stmts
        term <- blk$term
        if (!is.null(term) &&
            is.list(term) &&
            !is.null(term$kind)) {
            if (identical(term$kind, "ret") &&
                !is.null(term$value)) {
                if (.mojor_ssa_is_value_ref(term$value)) {
                  term$value <- .mojor_ssa_resolve_alias(term$value, alias_map)
                }
            } else if (identical(term$kind, "br")) {
                if (!is.null(term$args) &&
                  length(term$args) >
                    0) {
                  term$args <- .mojor_ssa_replace_aliases(term$args, alias_map)
                }
            } else if (identical(term$kind, "condbr")) {
                if (!is.null(term$cond) &&
                  .mojor_ssa_is_value_ref(term$cond)) {
                  term$cond <- .mojor_ssa_resolve_alias(term$cond, alias_map)
                }
                if (!is.null(term$then_args) &&
                  length(term$then_args) >
                    0) {
                  term$then_args <- .mojor_ssa_replace_aliases(term$then_args, alias_map)
                }
                if (!is.null(term$else_args) &&
                  length(term$else_args) >
                    0) {
                  term$else_args <- .mojor_ssa_replace_aliases(term$else_args, alias_map)
                }
            }
            blk$term <- term
        }

        out$blocks[[bn]] <- blk
    }
    out
}

.mojor_ir_ssa_prune_dead_stmts <- function(ssa) {
    ssa <- .mojor_ssa_require_semantic_ir(ssa, where = "SSA prune_dead_stmts")
    if (is.null(ssa$blocks) ||
        !is.list(ssa$blocks) ||
        length(ssa$blocks) ==
            0) {
        return(ssa)
    }

    defs <- list()
    live <- character(0)
    work <- character(0)

    enqueue <- function(vals) {
        vals <- .mojor_ssa_collect_value_refs(vals)
        if (length(vals) ==
            0) {
            return(invisible(NULL))
        }
        new_vals <- vals[!vals %in% live]
        if (length(new_vals) ==
            0) {
            return(invisible(NULL))
        }
        live <<- c(live, new_vals)
        work <<- c(work, new_vals)
        invisible(NULL)
    }

    for (bn in names(ssa$blocks)) {
        blk <- ssa$blocks[[bn]]
        if (is.null(blk) ||
            !is.list(blk))
            next
        enqueue(.mojor_ssa_term_value_refs(blk$term))
        stmts <- blk$stmts
        if (is.null(stmts) ||
            length(stmts) ==
                0)
            next
        for (st in stmts) {
            if (is.null(st) ||
                !is.list(st) ||
                is.null(st$kind))
                next
            if (identical(st$kind, "ssa_effect")) {
                enqueue(st$args)
                next
            }
            if (!identical(st$kind, "ssa_stmt"))
                next
            if (is.null(st$id) ||
                !.mojor_ssa_is_value_ref(st$id))
                next
            defs[[st$id]] <- st
            if (!.mojor_ssa_stmt_is_pure(st)) {
                enqueue(c(st$id, st$args))
            }
        }
    }

    while (length(work) >
        0) {
        id <- work[[length(work)]]
        work <- work[-length(work)]
        st <- defs[[id]]
        if (is.null(st) ||
            !is.list(st))
            next
        enqueue(st$args)
    }

    out <- ssa
    for (bn in names(out$blocks)) {
        blk <- out$blocks[[bn]]
        stmts <- blk$stmts
        if (is.null(stmts) ||
            length(stmts) ==
                0)
            next
        kept <- list()
        for (st in stmts) {
            keep <- TRUE
            if (!is.null(st) &&
                is.list(st) &&
                identical(st$kind, "ssa_stmt") &&
                !is.null(st$id) &&
                .mojor_ssa_is_value_ref(st$id) &&
                .mojor_ssa_stmt_is_pure(st)) {
                keep <- st$id %in% live
            }
            if (isTRUE(keep))
                kept <- c(kept, list(st))
        }
        blk$stmts <- kept
        out$blocks[[bn]] <- blk
    }
    out
}

.mojor_ir_ssa_metrics <- function(ssa) {
    if (is.null(ssa) ||
        !is.list(ssa) ||
        is.null(ssa$kind) ||
        ssa$kind != "ssa_fn" || is.null(ssa$blocks) ||
        !is.list(ssa$blocks)) {
        return(
            list(
                block_count = 0L, stmt_count = 0L, value_stmt_count = 0L,
                effect_stmt_count = 0L, term_counts = character(0)
            )
        )
    }

    block_count <- length(ssa$blocks)
    stmt_count <- 0L
    value_stmt_count <- 0L
    effect_stmt_count <- 0L
    term_kinds <- character(0)

    for (bn in names(ssa$blocks)) {
        blk <- ssa$blocks[[bn]]
        if (is.null(blk) ||
            !is.list(blk))
            next
        if (!is.null(blk$stmts) &&
            length(blk$stmts) >
                0) {
            stmt_count <- stmt_count + as.integer(length(blk$stmts))
            for (st in blk$stmts) {
                if (is.null(st) ||
                  !is.list(st) ||
                  is.null(st$kind))
                  next
                if (identical(st$kind, "ssa_stmt"))
                  value_stmt_count <- value_stmt_count + 1L
                if (identical(st$kind, "ssa_effect"))
                  effect_stmt_count <- effect_stmt_count + 1L
            }
        }
        if (!is.null(blk$term) &&
            is.list(blk$term) &&
            !is.null(blk$term$kind)) {
            term_kinds <- c(term_kinds, as.character(blk$term$kind))
        }
    }

    term_counts <- character(0)
    if (length(term_kinds) >
        0) {
        tab <- sort(
            table(term_kinds),
            decreasing = FALSE
        )
        term_counts <- paste0(
            names(tab),
            "=", as.integer(tab)
        )
    }

    list(
        block_count = as.integer(block_count),
        stmt_count = as.integer(stmt_count),
        value_stmt_count = as.integer(value_stmt_count),
        effect_stmt_count = as.integer(effect_stmt_count),
        term_counts = term_counts
    )
}

.mojor_ir_ssa_optimize <- function(
    ssa, passes = c("prune_unreachable", "copy_propagate", "prune_dead_stmts"),
    verify = TRUE, trace = FALSE, strict_schema = NULL
) {
    ssa <- .mojor_ssa_require_semantic_ir(ssa, where = "SSA optimize")
    ssa <- .mojor_ssa_annotate_effects_resources(ssa)
    if (is.null(passes) ||
        length(passes) ==
            0) {
        if (isTRUE(verify))
            .mojor_ir_verify_ssa(ssa, strict_schema = strict_schema)
        if (isTRUE(trace)) {
            return(list(ssa = ssa, pass_trace = list()))
        }
        return(ssa)
    }

    out <- ssa
    pass_trace <- list()
    for (pass in passes) {
        before <- NULL
        prev <- NULL
        if (isTRUE(trace)) {
            before <- .mojor_ir_ssa_metrics(out)
            prev <- out
        }
        if (identical(pass, "prune_unreachable")) {
            out <- .mojor_ir_ssa_prune_unreachable(out)
        } else if (identical(pass, "copy_propagate")) {
            out <- .mojor_ir_ssa_copy_propagate(out)
        } else if (identical(pass, "prune_dead_stmts")) {
            out <- .mojor_ir_ssa_prune_dead_stmts(out)
        } else {
            stop(sprintf("SSA optimize: unknown pass '%s'", pass))
        }
        if (isTRUE(trace)) {
            pass_trace[[length(pass_trace) +
                1L]] <- list(
                pass = as.character(pass),
                before = before, after = .mojor_ir_ssa_metrics(out),
                changed = !identical(prev, out)
            )
        }
    }
    out <- .mojor_ssa_annotate_effects_resources(out)
    if (isTRUE(verify))
        .mojor_ir_verify_ssa(out, strict_schema = strict_schema)
    if (isTRUE(trace)) {
        return(list(ssa = out, pass_trace = pass_trace))
    }
    out
}
