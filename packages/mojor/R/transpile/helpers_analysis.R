# Transpile helpers: block extraction, index/loop analysis, and shape
# inference helpers.

.mojor_extract_block <- function(fn_body) {
    if (is.call(fn_body) &&
        identical(fn_body[[1]], as.name("{"))) {
        items <- as.list(fn_body)[-1]
        sr <- attr(fn_body, "srcref")
        if (!is.null(sr)) {
            if (inherits(sr, "srcref")) {
                for (i in seq_along(items)) {
                  if (!is.name(items[[i]]) &&
                    is.null(attr(items[[i]], "srcref"))) {
                    attr(items[[i]], "srcref") <- sr
                  }
                }
            } else if (is.list(sr) &&
                length(sr) >=
                  length(items)) {
                for (i in seq_along(items)) {
                  if (!is.name(items[[i]]) &&
                    is.null(attr(items[[i]], "srcref")) &&
                    inherits(sr[[i]], "srcref")) {
                    attr(items[[i]], "srcref") <- sr[[i]]
                  }
                }
            }
        }
        items
    } else {
        list(fn_body)
    }
}

.mojor_wrap_block_expr <- function(stmts) {
    if (length(stmts) ==
        1) {
        return(stmts[[1]])
    }
    as.call(
        c(
            as.name("{"),
            stmts
        )
    )
}

.mojor_is_subscript_call <- function(node) {
    if (!is.call(node) || length(node) < 2L) {
        return(FALSE)
    }
    op <- as.character(node[[1]])
    if (length(op) != 1L) {
        op <- op[[1L]]
    }
    op %in% c("[", "[[")
}

.mojor_is_missing_call_arg <- function(call_node, pos) {
    tryCatch(
        {
            val <- call_node[[pos]]
            is.symbol(val) && identical(as.character(val), "")
        },
        error = function(e) {
            msg <- conditionMessage(e)
            grepl("argument.*missing|subscript out of bounds", msg, ignore.case = TRUE)
        }
    )
}

.mojor_rewrite_nested_subscript_expr <- function(expr, fresh_tmp) {
    if (!is.call(expr)) {
        return(list(pre = list(), expr = expr))
    }

    out_expr <- expr
    pre <- list()

    op <- as.character(out_expr[[1]])
    if (length(op) != 1L) {
        op <- op[[1L]]
    }

    if (.mojor_is_subscript_call(out_expr)) {
        base_res <- .mojor_rewrite_nested_subscript_expr(out_expr[[2]], fresh_tmp)
        pre <- c(pre, base_res$pre)
        base_expr <- base_res$expr

        if (.mojor_is_subscript_call(base_expr)) {
            tmp_name <- fresh_tmp()
            pre <- c(pre, list(as.call(list(as.name("<-"), as.name(tmp_name), base_expr))))
            base_expr <- as.name(tmp_name)
        }
        out_expr[[2]] <- base_expr

        if (length(out_expr) >= 3L) {
            for (k in 3:length(out_expr)) {
                if (isTRUE(.mojor_is_missing_call_arg(out_expr, k))) {
                    next
                }
                arg_res <- .mojor_rewrite_nested_subscript_expr(out_expr[[k]], fresh_tmp)
                pre <- c(pre, arg_res$pre)
                out_expr[[k]] <- arg_res$expr
            }
        }
        return(list(pre = pre, expr = out_expr))
    }

    if (length(out_expr) >= 2L) {
        for (k in 2:length(out_expr)) {
            if (isTRUE(.mojor_is_missing_call_arg(out_expr, k))) {
                next
            }
            arg_res <- .mojor_rewrite_nested_subscript_expr(out_expr[[k]], fresh_tmp)
            pre <- c(pre, arg_res$pre)
            out_expr[[k]] <- arg_res$expr
        }
    }

    list(pre = pre, expr = out_expr)
}

.mojor_rewrite_nested_subscript_stmt <- function(stmt, fresh_tmp) {
    if (!is.call(stmt)) {
        return(list(stmt))
    }
    op <- as.character(stmt[[1]])
    if (length(op) != 1L) {
        op <- op[[1L]]
    }

    if (op %in% c("<-", "=") && length(stmt) >= 3L) {
        rhs <- stmt[[3]]
        nested_has_missing_selector <- function(node) {
            if (!.mojor_is_subscript_call(node) || length(node) < 3L) {
                return(FALSE)
            }
            for (k in 3:length(node)) {
                if (isTRUE(.mojor_is_missing_call_arg(node, k))) {
                    return(TRUE)
                }
            }
            FALSE
        }
        if (.mojor_is_subscript_call(rhs) &&
            length(rhs) >= 3L &&
            .mojor_is_subscript_call(rhs[[2]]) &&
            (nested_has_missing_selector(rhs) || nested_has_missing_selector(rhs[[2]]))) {
            stop(.mojor_diag_index_nested_missing_selector())
        }
        rhs_res <- .mojor_rewrite_nested_subscript_expr(rhs, fresh_tmp)
        rhs_expr <- rhs_res$expr
        if (length(rhs_res$pre) > 0L) {
            new_assign <- stmt
            new_assign[[3]] <- rhs_expr
            return(c(rhs_res$pre, list(new_assign)))
        }
        if (!identical(rhs_expr, rhs)) {
            new_assign <- stmt
            new_assign[[3]] <- rhs_expr
            return(list(new_assign))
        }
    }

    list(stmt)
}

.mojor_expand_chained_assignment_stmt <- function(stmt, fresh_tmp) {
    if (!is.call(stmt)) {
        return(list(stmt))
    }
    op <- as.character(stmt[[1]])
    if (length(op) !=
        1)
        op <- op[[1]]
    if (op %in% c("<-", "=") &&
        length(stmt) >=
            3) {
        rhs <- stmt[[3]]
        if (is.call(rhs)) {
            rhs_op <- as.character(rhs[[1]])
            if (length(rhs_op) !=
                1)
                rhs_op <- rhs_op[[1]]
            if (rhs_op %in% c("<-", "=") &&
                length(rhs) >=
                  3 && is.name(rhs[[2]])) {
                inner <- .mojor_expand_chained_assignment_stmt(rhs, fresh_tmp)
                rhs_name <- as.character(rhs[[2]])
                outer <- as.call(
                  list(
                    as.name(op),
                    stmt[[2]], as.name(rhs_name)
                )
              )
                return(c(inner, .mojor_rewrite_nested_subscript_stmt(outer, fresh_tmp)))
            }
        }
        return(.mojor_rewrite_nested_subscript_stmt(stmt, fresh_tmp))
    }
    if (op == "if") {
        then_stmts <- .mojor_expand_chained_assignments_blocks(.mojor_extract_block(stmt[[3]]))
        then_expr <- .mojor_wrap_block_expr(then_stmts)
        if (length(stmt) >=
            4) {
            else_stmts <- .mojor_expand_chained_assignments_blocks(.mojor_extract_block(stmt[[4]]))
            else_expr <- .mojor_wrap_block_expr(else_stmts)
            return(
                list(
                  as.call(
                    list(
                      as.name("if"),
                      stmt[[2]], then_expr, else_expr
                  )
                )
              )
            )
        }
        return(
            list(
                as.call(
                  list(
                    as.name("if"),
                    stmt[[2]], then_expr
                )
              )
            )
        )
    }
    if (op == "for" && length(stmt) >=
        4) {
        body_stmts <- .mojor_expand_chained_assignments_blocks(.mojor_extract_block(stmt[[4]]))
        body_expr <- .mojor_wrap_block_expr(body_stmts)
        return(
            list(
                as.call(
                  list(
                    as.name("for"),
                    stmt[[2]], stmt[[3]], body_expr
                )
              )
            )
        )
    }
    if (op == "while" && length(stmt) >=
        3) {
        body_stmts <- .mojor_expand_chained_assignments_blocks(.mojor_extract_block(stmt[[3]]))
        body_expr <- .mojor_wrap_block_expr(body_stmts)
        return(
            list(
                as.call(
                  list(
                    as.name("while"),
                    stmt[[2]], body_expr
                )
              )
            )
        )
    }
    if (op == "repeat" && length(stmt) >=
        2) {
        body_stmts <- .mojor_expand_chained_assignments_blocks(.mojor_extract_block(stmt[[2]]))
        body_expr <- .mojor_wrap_block_expr(body_stmts)
        return(
            list(
                as.call(
                  list(
                    as.name("repeat"),
                    body_expr
                )
              )
            )
        )
    }
    .mojor_rewrite_nested_subscript_stmt(stmt, fresh_tmp)
}

.mojor_expand_chained_assignments_blocks <- function(blocks) {
    tmp_counter <- 0L
    fresh_tmp <- function() {
        tmp_counter <<- tmp_counter + 1L
        paste0("__mojor_chain_tmp_", tmp_counter)
    }
    out <- list()
    for (b in blocks) {
        out <- c(out, .mojor_expand_chained_assignment_stmt(b, fresh_tmp))
    }
    out
}

.mojor_collect_index_vars <- function(expr) {
    if (missing(expr) ||
        is.null(expr)) {
        return(character(0))
    }
    vars <- character(0)
    if (is.call(expr)) {
        op <- as.character(expr[[1]])
        if (op %in% c("[", "[[")) {
            if (length(expr) >=
                3) {
                for (k in 3:length(expr)) {
                  idx <- expr[[k]]
                  if (is.name(idx))
                    vars <- c(vars, as.character(idx))
                }
            }
        }
        for (p in as.list(expr)[-1]) {
            vars <- c(vars, .mojor_collect_index_vars(p))
        }
    }
    vars
}

.mojor_index_expr_loop_var <- function(idx_expr, loop_vars) {
    if (is.name(idx_expr)) {
        name <- as.character(idx_expr)
        if (name %in% loop_vars) {
            return(name)
        }
        return(NULL)
    }
    if (is.call(idx_expr) &&
        length(idx_expr) ==
            3) {
        op <- as.character(idx_expr[[1]])
        if (!op %in% c("+", "-")) {
            return(NULL)
        }
        a <- idx_expr[[2]]
        b <- idx_expr[[3]]
        parse_part <- function(x) {
            if (is.name(x)) {
                nm <- as.character(x)
                if (nm %in% loop_vars) {
                  return(list(kind = "var", name = nm))
                }
            }
            if (is.numeric(x) &&
                length(x) ==
                  1) {
                val <- as.integer(x)
                if (!is.na(val)) {
                  return(list(kind = "num", value = val))
                }
            }
            NULL
        }
        pa <- parse_part(a)
        pb <- parse_part(b)
        if (is.null(pa) ||
            is.null(pb)) {
            return(NULL)
        }
        if (pa$kind == "var" && pb$kind == "num") {
            return(pa$name)
        }
        if (pa$kind == "num" && pb$kind == "var" && op == "+") {
            return(pb$name)
        }
    }
    NULL
}

.mojor_index_expr_loop_vars <- function(idx_expr, loop_vars) {
    vars <- character(0)
    if (is.name(idx_expr)) {
        name <- as.character(idx_expr)
        if (name %in% loop_vars)
            vars <- c(vars, name)
        return(vars)
    }
    if (is.call(idx_expr)) {
        parts <- as.list(idx_expr)[-1]
        for (p in parts) {
            vars <- c(vars, .mojor_index_expr_loop_vars(p, loop_vars))
        }
    }
    unique(vars)
}

.mojor_int_literal_value <- function(expr) {
    if (!(is.numeric(expr) &&
        length(expr) ==
            1)) {
        return(NULL)
    }
    if (is.na(expr) ||
        !is.finite(expr)) {
        return(NULL)
    }
    if (abs(expr - round(expr)) >
        0) {
        return(NULL)
    }
    as.integer(round(expr))
}

.mojor_add_const_to_base <- function(base_expr, const_val) {
    if (is.null(const_val) ||
        const_val == 0L) {
        return(base_expr)
    }
    if (const_val > 0L) {
        return(call("+", base_expr, const_val))
    }
    call("-", base_expr, abs(const_val))
}

.mojor_simplify_index_add_sub <- function(idx_expr) {
    if (!is.call(idx_expr)) {
        return(idx_expr)
    }
    op <- as.character(idx_expr[[1]])
    if (length(op) !=
        1)
        op <- op[[1]]
    if (op == "(" && length(idx_expr) ==
        2) {
        return(.mojor_simplify_index_add_sub(idx_expr[[2]]))
    }
    if (op == "+" && length(idx_expr) ==
        2) {
        return(.mojor_simplify_index_add_sub(idx_expr[[2]]))
    }
    if (op == "-" && length(idx_expr) ==
        2) {
        inner <- .mojor_simplify_index_add_sub(idx_expr[[2]])
        inner_num <- .mojor_int_literal_value(inner)
        if (!is.null(inner_num)) {
            return(-inner_num)
        }
        return(call("-", inner))
    }
    if (!op %in% c("+", "-") ||
        length(idx_expr) !=
            3) {
        parts <- as.list(idx_expr)
        if (length(parts) >
            1) {
            for (k in 2:length(parts)) {
                parts[[k]] <- .mojor_simplify_index_add_sub(parts[[k]])
            }
        }
        return(as.call(parts))
    }

    lhs <- .mojor_simplify_index_add_sub(idx_expr[[2]])
    rhs <- .mojor_simplify_index_add_sub(idx_expr[[3]])
    lhs_num <- .mojor_int_literal_value(lhs)
    rhs_num <- .mojor_int_literal_value(rhs)

    if (!is.null(lhs_num) &&
        !is.null(rhs_num)) {
        return(if (op == "+") lhs_num + rhs_num else lhs_num - rhs_num)
    }
    if (op == "+" && !is.null(rhs_num) &&
        rhs_num == 0L) {
        return(lhs)
    }
    if (op == "+" && !is.null(lhs_num) &&
        lhs_num == 0L) {
        return(rhs)
    }
    if (op == "-" && !is.null(rhs_num) &&
        rhs_num == 0L) {
        return(lhs)
    }

    if (is.call(lhs) &&
        length(lhs) ==
            3 && !is.null(rhs_num)) {
        lhs_op <- as.character(lhs[[1]])
        if (length(lhs_op) !=
            1)
            lhs_op <- lhs_op[[1]]
        lhs_rhs_num <- .mojor_int_literal_value(lhs[[3]])
        if (lhs_op %in% c("+", "-") &&
            !is.null(lhs_rhs_num)) {
            lhs_const <- if (lhs_op == "+")
                lhs_rhs_num else -lhs_rhs_num
            net_const <- if (op == "+")
                lhs_const + rhs_num else lhs_const - rhs_num
            return(.mojor_add_const_to_base(lhs[[2]], net_const))
        }
    }

    call(op, lhs, rhs)
}

.mojor_simplify_selector_index_expr <- function(expr) {
    if (!is.call(expr)) {
        return(expr)
    }
    op <- as.character(expr[[1]])
    if (length(op) !=
        1)
        op <- op[[1]]
    parts <- as.list(expr)
    if (length(parts) >
        1) {
        for (k in 2:length(parts)) {
            parts[[k]] <- .mojor_simplify_selector_index_expr(parts[[k]])
        }
    }
    if (op %in% c("[", "[[") &&
        length(parts) >=
            3) {
        for (k in 3:length(parts)) {
            idx <- parts[[k]]
            if (identical(idx, quote(expr =)))
                next
            parts[[k]] <- .mojor_simplify_index_add_sub(idx)
        }
    }
    as.call(parts)
}

.mojor_collect_array_index_map <- function(blocks, array_args) {
    map <- list()
    add_info <- function(arr, vars, simple_var, complex) {
        if (is.null(map[[arr]])) {
            map[[arr]] <<- list(
                vars = character(0),
                simple_vars = character(0),
                complex = FALSE, unknown = FALSE
            )
        }
        if (length(vars) ==
            0) {
            map[[arr]]$unknown <<- TRUE
            return()
        }
        map[[arr]]$vars <<- unique(c(map[[arr]]$vars, vars))
        if (!is.null(simple_var)) {
            map[[arr]]$simple_vars <<- unique(c(map[[arr]]$simple_vars, simple_var))
        }
        if (isTRUE(complex))
            map[[arr]]$complex <<- TRUE
    }
    walk <- function(expr, loop_vars) {
        if (missing(expr) ||
            is.null(expr)) {
            return(invisible(NULL))
        }
        if (is.call(expr)) {
            op <- as.character(expr[[1]])
            if (length(op) !=
                1)
                op <- op[[1]]
            if (op == "for" && length(expr) >=
                4) {
                loop_var <- as.character(expr[[2]])
                walk(expr[[3]], loop_vars)
                walk(expr[[4]], unique(c(loop_vars, loop_var)))
                return()
            }
            if (op == "while" && length(expr) >=
                3) {
                walk(expr[[2]], loop_vars)
                walk(expr[[3]], loop_vars)
                return()
            }
            if (op == "repeat" && length(expr) >=
                2) {
                walk(expr[[2]], loop_vars)
                return()
            }
            if (op %in% c("[", "[[") &&
                length(expr) >=
                  3) {
                arr_expr <- expr[[2]]
                if (is.name(arr_expr)) {
                  arr <- as.character(arr_expr)
                  if (arr %in% array_args) {
                    if (length(expr) ==
                      3) {
                      idx_vars <- .mojor_index_expr_loop_vars(expr[[3]], loop_vars)
                      simple_var <- .mojor_index_expr_loop_var(expr[[3]], loop_vars)
                      complex <- length(idx_vars) >
                        0 && is.null(simple_var)
                      add_info(arr, idx_vars, simple_var, complex)
                    } else {
                      for (k in 3:length(expr)) {
                        idx_vars <- .mojor_index_expr_loop_vars(expr[[k]], loop_vars)
                        simple_var <- .mojor_index_expr_loop_var(expr[[k]], loop_vars)
                        complex <- length(idx_vars) >
                          0 && is.null(simple_var)
                        add_info(arr, idx_vars, simple_var, complex)
                      }
                    }
                  }
                }
            }
            for (p in as.list(expr)[-1]) {
                walk(p, loop_vars)
            }
        }
    }
    for (b in blocks) walk(b, character(0))
    map
}

.mojor_collect_length_arrays <- function(expr) {
    if (missing(expr) ||
        is.null(expr)) {
        return(character(0))
    }
    vars <- character(0)
    if (is.call(expr)) {
        op <- as.character(expr[[1]])
        if (identical(op, "length") &&
            length(expr) >=
                2) {
            arg <- expr[[2]]
            if (is.name(arg))
                vars <- c(vars, as.character(arg))
        }
        if (length(op) ==
            1 && op %in% c("rep", "rep_len", "rep.int") &&
            length(expr) >=
                2) {
            parts <- as.list(expr)[-1]
            nms <- names(parts)
            get_arg <- function(nm, pos) {
                if (!is.null(nms) &&
                  nm %in% nms) {
                  return(parts[[which(nms == nm)[1]]])
                }
                if (length(parts) >=
                  pos && (is.null(nms) ||
                  is.null(nms[[pos]]) ||
                  nms[[pos]] == "")) {
                  return(parts[[pos]])
                }
                NULL
            }
            arg <- get_arg("x", 1)
            if (is.name(arg))
                vars <- c(vars, as.character(arg))
        }
        if (identical(op, "c") &&
            length(expr) >=
                2) {
            for (p in as.list(expr)[-1]) {
                if (is.name(p))
                  vars <- c(vars, as.character(p))
            }
        }
        for (p in as.list(expr)[-1]) {
            vars <- c(vars, .mojor_collect_length_arrays(p))
        }
    }
    vars
}

.mojor_collect_unique_from_blocks <- function(blocks, collector, ...) {
    if (length(blocks) ==
        0) {
        return(character(0))
    }
    unique(
        unlist(
            lapply(blocks, function(b) collector(b, ...)),
            use.names = FALSE
        )
    )
}

.mojor_collect_length_arrays_blocks <- function(blocks) {
    .mojor_collect_unique_from_blocks(blocks, .mojor_collect_length_arrays)
}

.mojor_collect_recycle_arrays <- function(expr, types) {
    arrays <- character(0)
    walk <- function(e) {
        if (is.call(e)) {
            op <- as.character(e[[1]])
            if (length(op) ==
                1 && op %in% c("rep", "rep_len", "rep.int") &&
                length(e) >=
                  2) {
                parts <- as.list(e)[-1]
                nms <- names(parts)
                get_arg <- function(nm, pos) {
                  if (!is.null(nms) &&
                    nm %in% nms) {
                    return(parts[[which(nms == nm)[1]]])
                  }
                  if (length(parts) >=
                    pos && (is.null(nms) ||
                    is.null(nms[[pos]]) ||
                    nms[[pos]] == "")) {
                    return(parts[[pos]])
                  }
                  NULL
                }
                arg <- get_arg("x", 1)
                if (is.name(arg)) {
                  nm <- as.character(arg)
                  if (!is.null(types[[nm]]) &&
                    .mojor_is_array(types[[nm]])) {
                    arrays <<- unique(c(arrays, nm))
                  }
                }
            }
            if (identical(op, "c")) {
                if (length(e) <
                  2) {
                  return()
                }
                for (k in 2:length(e)) {
                  p <- e[[k]]
                  if (is.name(p)) {
                    nm <- as.character(p)
                    if (!is.null(types[[nm]]) &&
                      .mojor_is_array(types[[nm]])) {
                      arrays <<- unique(c(arrays, nm))
                    }
                  }
                  walk(p)
                }
                return()
            }
            if (length(e) <
                2) {
                return()
            }
            for (k in 2:length(e)) walk(e[[k]])
        }
    }
    walk(expr)
    arrays
}

.mojor_collect_recycle_arrays_blocks <- function(blocks, types) {
    .mojor_collect_unique_from_blocks(blocks, .mojor_collect_recycle_arrays, types = types)
}

.mojor_collect_scalar_inits <- function(blocks) {
    inits <- list()
    queue <- list(blocks)
    while (length(queue) >
        0) {
        blk <- queue[[1]]
        queue <- queue[-1]
        for (b in blk) {
            if (is.call(b) &&
                as.character(b[[1]]) %in%
                  c("<-", "=")) {
                lhs <- b[[2]]
                rhs <- b[[3]]
                if (is.name(lhs)) {
                  name <- as.character(lhs)
                  if (is.integer(rhs) &&
                    length(rhs) ==
                      1) {
                    if (is.null(inits[[name]]))
                      inits[[name]] <- list(type = "i32", value = rhs)
                  } else if (is.logical(rhs) &&
                    length(rhs) ==
                      1) {
                    if (is.null(inits[[name]]))
                      inits[[name]] <- list(type = "lgl", value = if (isTRUE(rhs)) 1L else 0L)
                  } else if (is.numeric(rhs) &&
                    length(rhs) ==
                      1) {
                    if (is.null(inits[[name]]))
                      inits[[name]] <- list(type = "f64", value = rhs)
                  } else if (is.name(rhs)) {
                    rhs_name <- as.character(rhs)
                    if (!is.null(inits[[rhs_name]]) &&
                      is.null(inits[[name]])) {
                      inits[[name]] <- inits[[rhs_name]]
                    }
                  }
                }
            }
            if (is.call(b) &&
                as.character(b[[1]]) ==
                  "if") {
                then_blocks <- .mojor_extract_block(b[[3]])
                if (length(then_blocks) >
                  0)
                  queue <- c(queue, list(then_blocks))
                if (length(b) >=
                  4) {
                  else_blocks <- .mojor_extract_block(b[[4]])
                  if (length(else_blocks) >
                    0)
                    queue <- c(queue, list(else_blocks))
                }
            }
            if (is.call(b) &&
                as.character(b[[1]]) %in%
                  c("for", "while", "repeat")) {
                body <- if (as.character(b[[1]]) ==
                  "for")
                  b[[4]] else b[[3]]
                body_blocks <- .mojor_extract_block(body)
                if (length(body_blocks) >
                  0)
                  queue <- c(queue, list(body_blocks))
            }
        }
    }
    inits
}

.mojor_apply_out_type_from_spec <- function(fun_name, x_spec) {
    if (identical(fun_name, "mean")) {
        return("f64[]")
    }
    if (!is.null(x_spec) &&
        x_spec %in% c("i32[]", "i32[,]", "i32")) {
        return("i32[]")
    }
    if (!is.null(x_spec) &&
        x_spec %in% c("f32[]", "f32[,]", "f32")) {
        return("f32[]")
    }
    "f64[]"
}

.mojor_detect_apply_assignment <- function(lhs, rhs, args, types) {
    if (is.null(rhs) ||
        !is.call(rhs) ||
        as.character(rhs[[1]]) !=
            "apply") {
        return(NULL)
    }
    apply_node <- .mojor_ir_expr_build(rhs)
    if (is.null(apply_node) ||
        is.null(apply_node$kind) ||
        apply_node$kind != "apply") {
        return(NULL)
    }
    if (is.null(apply_node$x) ||
        is.null(apply_node$x$kind) ||
        apply_node$x$kind != "var") {
        return(NULL)
    }
    x_name <- apply_node$x$name
    if (!(x_name %in% args)) {
        return(NULL)
    }
    margin <- as.integer(apply_node$margin)
    if (!(margin %in% c(1L, 2L))) {
        return(NULL)
    }
    out_len_expr <- if (margin == 1L)
        call("nrow", as.name(x_name)) else call("ncol", as.name(x_name))
    x_spec <- types[[x_name]]
    fun_name <- as.character(apply_node$fun)
    list(
        out_name = lhs, out_type = .mojor_apply_out_type_from_spec(fun_name, x_spec),
        out_len_source = list(kind = "expr", expr = out_len_expr)
    )
}

.mojor_canonicalize_no_loop_apply_blocks <- function(blocks) {
    if (length(blocks) !=
        1) {
        return(blocks)
    }
    stmt <- blocks[[1]]
    apply_expr <- NULL
    if (is.call(stmt) &&
        as.character(stmt[[1]]) ==
            "apply") {
        apply_expr <- stmt
    } else if (is.call(stmt) &&
        as.character(stmt[[1]]) ==
            "return" && length(stmt) >=
        2 && is.call(stmt[[2]]) &&
        as.character(stmt[[2]][[1]]) ==
            "apply") {
        apply_expr <- stmt[[2]]
    }
    if (is.null(apply_expr)) {
        return(blocks)
    }
    apply_node <- .mojor_ir_expr_build(apply_expr)
    if (is.null(apply_node) ||
        is.null(apply_node$kind) ||
        apply_node$kind != "apply") {
        return(blocks)
    }
    used_names <- unique(
        all.vars(
            as.call(
                c(
                  as.name("{"),
                  blocks
              )
            ),
            functions = FALSE
        )
    )
    out_name <- "__mojor_apply_out"
    while (out_name %in% used_names) out_name <- paste0(out_name, "_")
    list(
        call(
            "<-", as.name(out_name),
            apply_expr
        ),
        call("return", as.name(out_name))
    )
}

.mojor_parse_bool_literal <- function(x) {
    if (is.logical(x) &&
        length(x) ==
            1 && !is.na(x)) {
        return(isTRUE(x))
    }
    if (is.name(x)) {
        nm <- as.character(x)
        if (identical(nm, "TRUE")) {
            return(TRUE)
        }
        if (identical(nm, "FALSE")) {
            return(FALSE)
        }
    }
    NULL
}

.mojor_parse_gpu_matmul_dims <- function(expr) {
    if (!is.call(expr) ||
        !identical(
            as.character(expr[[1]]),
            "gpu_matmul"
        )) {
        return(NULL)
    }
    if (length(expr) <
        3) {
        return(NULL)
    }

    ta <- FALSE
    tb <- FALSE
    arg_names <- names(expr)
    if (is.null(arg_names))
        arg_names <- rep("", length(expr))

    if (length(expr) >=
        4 && (!nzchar(arg_names[[4]]) ||
        is.na(arg_names[[4]]))) {
        ta_val <- .mojor_parse_bool_literal(expr[[4]])
        if (!is.null(ta_val))
            ta <- ta_val
    }
    if (length(expr) >=
        5 && (!nzchar(arg_names[[5]]) ||
        is.na(arg_names[[5]]))) {
        tb_val <- .mojor_parse_bool_literal(expr[[5]])
        if (!is.null(tb_val))
            tb <- tb_val
    }

    for (i in seq_along(arg_names)) {
        nm <- arg_names[[i]]
        if (identical(nm, "transpose_a")) {
            ta_val <- .mojor_parse_bool_literal(expr[[i]])
            if (!is.null(ta_val))
                ta <- ta_val
        } else if (identical(nm, "transpose_b")) {
            tb_val <- .mojor_parse_bool_literal(expr[[i]])
            if (!is.null(tb_val))
                tb <- tb_val
        }
    }

    a_expr <- expr[[2]]
    b_expr <- expr[[3]]
    out_nrow_expr <- if (isTRUE(ta))
        call("ncol", a_expr) else call("nrow", a_expr)
    out_ncol_expr <- if (isTRUE(tb))
        call("nrow", b_expr) else call("ncol", b_expr)

    list(out_nrow_expr = out_nrow_expr, out_ncol_expr = out_ncol_expr)
}

.mojor_scan_blocks_assignments_and_returns <- function(blocks, args, types) {
    out_name <- NULL
    out_type <- NULL
    out_matrix <- FALSE
    out_nrow_expr <- NULL
    out_ncol_expr <- NULL
    out_array <- FALSE
    out_dim_exprs <- NULL
    out_dim_name <- NULL
    out_len_source <- NULL
    loops <- list()
    scalar_inits <- list()
    array_aliases <- list()
    pre_loop_out_assigns <- list()
    scalar_init <- NULL
    scalar_type <- NULL
    scalar_init_value <- NULL
    scalar_reduction_op <- NULL
    scalar_reduction_arg <- NULL
    scalar_reduce_ir_node <- NULL
    return_name <- NULL
    return_expr <- NULL
    return_div <- NULL
    mean_ignore_pending <- FALSE
    mean_ignore_expr <- NULL
    mean_ignore_loop_var <- NULL
    any_all_call <- NULL
    any_all_arg <- NULL
    any_all_mode <- NULL
    seq_extra <- NULL
    constructor_len_arrays <- character(0)
    local_array_specs <- list()
    local_array_len_exprs <- list()
    name_assign_rhs <- list()
    block_vars <- if (length(blocks) >
        0) {
        unique(
            all.vars(
                as.call(
                  c(
                    as.name("{"),
                    blocks
                )
              ),
                functions = FALSE
            )
        )
    } else {
        character(0)
    }
    .fresh_temp_out_name <- function(prefix = "__mojor_gpu_matmul_out") {
        used_names <- unique(
            c(
                args, names(types),
                block_vars
            )
        )
        candidate <- prefix
        while (candidate %in% used_names) {
            candidate <- paste0(candidate, "_")
        }
        candidate
    }
    .resolve_array_alias_target <- function(name) {
        .mojor_resolve_array_source_name(name, args, types, alias_map = array_aliases)
    }
    .vector_out_spec <- function(spec) {
        if (is.null(spec) || !is.character(spec) || length(spec) != 1L) {
            return(NULL)
        }
        if (grepl("\\[\\]$", spec)) {
            return(spec)
        }
        if (grepl("\\[,\\]$", spec) || grepl("\\[[0-9]+d\\]$", spec)) {
            elem <- sub("\\[.*$", "", spec)
            return(paste0(elem, "[]"))
        }
        NULL
    }
    .extract_selector_arg <- function(expr, pos = 3L) {
        if (length(expr) < pos) {
            return(list(missing = TRUE, value = NULL))
        }
        is_missing_sel <- tryCatch(
            identical(expr[[pos]], quote(expr = )) ||
                (is.symbol(expr[[pos]]) && identical(as.character(expr[[pos]]), "")),
            error = function(e) {
                msg <- conditionMessage(e)
                if (grepl("argument.*missing|subscript out of bounds", msg, ignore.case = TRUE)) {
                    return(TRUE)
                }
                stop(e)
            }
        )
        if (isTRUE(is_missing_sel)) {
            return(list(missing = TRUE, value = NULL))
        }
        val <- tryCatch(
            expr[[pos]],
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
    .base_len_expr_for <- function(name) {
        if (!is.null(local_array_len_exprs[[name]])) {
            return(local_array_len_exprs[[name]])
        }
        alias <- .resolve_array_alias_target(name)
        if (!is.null(alias)) {
            alias_spec <- types[[alias]]
            if (!is.null(alias_spec) && .mojor_is_array(alias_spec)) {
                return(call("length", as.name(alias)))
            }
        }
        NULL
    }
    .subset_len_expr <- function(base_name, idx_expr) {
        base_len <- .base_len_expr_for(base_name)
        if (is.null(base_len)) {
            return(NULL)
        }
        if (missing(idx_expr) ||
            is.null(idx_expr) ||
            (is.symbol(idx_expr) && identical(as.character(idx_expr), ""))) {
            return(base_len)
        }
        if (is.name(idx_expr)) {
            idx_name <- as.character(idx_expr)
            if (!is.null(local_array_len_exprs[[idx_name]])) {
                return(local_array_len_exprs[[idx_name]])
            }
            idx_spec <- types[[idx_name]]
            if (is.null(idx_spec) && !is.null(local_array_specs[[idx_name]])) {
                idx_spec <- local_array_specs[[idx_name]]
            }
            if (!is.null(idx_spec) && .mojor_is_logical_mask_type(idx_spec)) {
                return(call("sum", as.name(idx_name)))
            }
            if (!is.null(idx_spec) && .mojor_is_array(idx_spec)) {
                return(call("length", as.name(idx_name)))
            }
            return(1L)
        }
        if ((is.numeric(idx_expr) || is.integer(idx_expr)) &&
            length(idx_expr) == 1 &&
            !is.na(idx_expr)) {
            return(1L)
        }
        neg_info <- .mojor_ir_detect_negative_index(idx_expr)
        if (isTRUE(neg_info$is_negative)) {
            if (!isTRUE(neg_info$is_vector)) {
                return(call("-", base_len, 1L))
            }
            vec_excl <- .mojor_ir_build_vector_exclusion_index(neg_info)
            if (!is.null(vec_excl)) {
                count_expr <- NULL
                count_raw <- vec_excl$neg_vec_exclusion$count
                if (!is.null(count_raw)) {
                    if (is.numeric(count_raw) || is.integer(count_raw)) {
                        count_expr <- as.integer(count_raw)
                    } else if (is.character(count_raw) && length(count_raw) == 1L && nzchar(count_raw)) {
                        parsed_num <- suppressWarnings(as.integer(count_raw))
                        if (!is.na(parsed_num)) {
                            count_expr <- parsed_num
                        } else {
                            count_expr <- parse(text = count_raw)[[1]]
                        }
                    }
                }
                if (is.null(count_expr) &&
                    identical(vec_excl$neg_vec_exclusion$type, "range")) {
                    start_ast <- vec_excl$neg_vec_exclusion$start_expr_ast
                    end_ast <- vec_excl$neg_vec_exclusion$end_expr_ast
                    if (!is.null(start_ast) && !is.null(end_ast)) {
                        start_is_one <- FALSE
                        if ((is.numeric(start_ast) || is.integer(start_ast)) &&
                            length(start_ast) == 1L &&
                            !is.na(start_ast)) {
                            start_is_one <- as.integer(start_ast) == 1L
                        } else if (is.character(start_ast) && length(start_ast) == 1L) {
                            parsed_start <- suppressWarnings(as.integer(start_ast))
                            start_is_one <- !is.na(parsed_start) && parsed_start == 1L
                        }
                        if (isTRUE(start_is_one) && identical(end_ast, base_len)) {
                            return(0L)
                        }
                        count_expr <- call("+", call("-", end_ast, start_ast), 1L)
                    }
                }
                if (!is.null(count_expr)) {
                    return(call("-", base_len, count_expr))
                }
            }
        }
        if (is.call(idx_expr)) {
            op <- as.character(idx_expr[[1]])
            if (identical(op, "c")) {
                elems <- as.list(idx_expr)[-1]
                count <- 0L
                for (elem in elems) {
                    if ((is.numeric(elem) || is.integer(elem)) &&
                        length(elem) == 1 &&
                        !is.na(elem) &&
                        as.integer(elem) == 0L) {
                        next
                    }
                    count <- count + 1L
                }
                return(as.integer(count))
            }
            if (identical(op, ":") && length(idx_expr) == 3L) {
                return(call("+", call("-", idx_expr[[3]], idx_expr[[2]]), 1L))
            }
            if (op %in% c("integer", "numeric", "logical") && length(idx_expr) == 2L) {
                arg <- idx_expr[[2]]
                if ((is.numeric(arg) || is.integer(arg)) &&
                    length(arg) == 1 &&
                    !is.na(arg) &&
                    as.integer(arg) == 0L) {
                    return(0L)
                }
            }
        }
        NULL
    }
    # Track active loop variables for i32 scalar expression checking
    active_loop_vars <- character(0)
    .is_dim_vector_expr <- function(node) {
        if (!is.call(node) ||
            as.character(node[[1]]) != "dim" ||
            length(node) != 2 ||
            !is.name(node[[2]])) {
            return(FALSE)
        }
        arr_name <- .resolve_array_alias_target(as.character(node[[2]]))
        if (is.null(arr_name)) {
            return(FALSE)
        }
        spec <- types[[arr_name]]
        if (is.null(spec) || !.mojor_is_array(spec)) {
            return(FALSE)
        }
        if (.mojor_is_matrix(spec)) {
            return(TRUE)
        }
        rank <- .mojor_type_ndim(spec)
        !is.na(rank) && rank >= 2L
    }
    .is_i32_scalar_expr <- function(node) {
        if (is.integer(node) &&
            length(node) == 1 &&
            !is.na(node)) {
            return(TRUE)
        }
        if (is.numeric(node) &&
            length(node) == 1) {
            val <- as.integer(node)
            return(!is.na(val) && abs(node - val) < 1e-12)
        }
        if (is.name(node)) {
            nm <- as.character(node)
            if (nm %in% args) {
                spec <- types[[nm]]
                return(!is.null(spec) && !.mojor_is_array(spec) && spec == "i32")
            }
            if (!is.null(scalar_inits[[nm]]) &&
                !is.null(scalar_inits[[nm]]$type)) {
                return(identical(scalar_inits[[nm]]$type, "i32"))
            }
            # Check if it's an active loop variable (loop vars are always i32)
            if (nm %in% active_loop_vars) {
                return(TRUE)
            }
            return(FALSE)
        }
        if (is.call(node)) {
            op <- as.character(node[[1]])
            if (op == "(" && length(node) == 2) {
                return(.is_i32_scalar_expr(node[[2]]))
            }
            if (op == "as.integer" && length(node) == 2) {
                return(.is_i32_scalar_expr(node[[2]]))
            }
            if (op == "-" && length(node) == 2) {
                return(.is_i32_scalar_expr(node[[2]]))
            }
            if (op %in% c("+", "-", "*", "%/%", "%%") &&
                length(node) == 3) {
                return(.is_i32_scalar_expr(node[[2]]) && .is_i32_scalar_expr(node[[3]]))
            }
            if (op %in% c("min", "max", "pmin", "pmax") && length(node) >= 2) {
                vals <- as.list(node)[-1]
                return(length(vals) > 0 && all(vapply(
                    vals,
                    function(v) .is_i32_scalar_expr(v) || .is_dim_vector_expr(v),
                    logical(1)
                )))
            }
            if (op == "length" && length(node) == 2) {
                arg <- node[[2]]
                if (!is.name(arg)) {
                    return(FALSE)
                }
                arr_name <- .resolve_array_alias_target(as.character(arg))
                if (is.null(arr_name)) {
                    return(FALSE)
                }
                spec <- types[[arr_name]]
                return(!is.null(spec) && .mojor_is_array(spec))
            }
            if (op %in% c("nrow", "ncol") && length(node) == 2) {
                arg <- node[[2]]
                if (!is.name(arg)) {
                    return(FALSE)
                }
                arr_name <- .resolve_array_alias_target(as.character(arg))
                if (is.null(arr_name)) {
                    return(FALSE)
                }
                spec <- types[[arr_name]]
                return(!is.null(spec) && .mojor_is_array(spec))
            }
            if (op %in% c("[", "[[") && length(node) == 3) {
                base <- node[[2]]
                idx_info <- .extract_selector_arg(node, 3L)
                if (isTRUE(idx_info$missing)) {
                    return(FALSE)
                }
                idx <- idx_info$value
                if (!is.call(base) ||
                    as.character(base[[1]]) != "dim" ||
                    length(base) != 2 ||
                    !is.name(base[[2]])) {
                    return(FALSE)
                }
                if (!.is_i32_scalar_expr(idx)) {
                    return(FALSE)
                }
                arr_name <- .resolve_array_alias_target(as.character(base[[2]]))
                if (is.null(arr_name)) {
                    return(FALSE)
                }
                spec <- types[[arr_name]]
                if (is.null(spec) || !.mojor_is_array(spec)) {
                    return(FALSE)
                }
                if (.mojor_is_matrix(spec)) {
                    return(TRUE)
                }
                rank <- .mojor_type_ndim(spec)
                return(!is.na(rank) && rank >= 2L)
            }
        }
        FALSE
    }
    .mojor_vector_ir_kinds_supported <- c(
        "unique", "duplicated", "match", "in", "quantile",
        "vapply", "sapply", "lapply", "mapply",
        "nchar", "nzchar", "substr", "paste",
        "sample", "sample_int",
        "regex_grepl", "regex_grep", "regex_sub",
        "row_matrix", "col_matrix", "expand_grid"
    )
    .mojor_scalar_ir_kinds_supported <- c("any_duplicated", "median", "iqr", "mad")
    .mojor_ir_kind_name <- function(node) {
        if (!is.list(node) || is.null(node$kind)) {
            return(NULL)
        }
        kind <- as.character(node$kind)
        if (!is.character(kind) || length(kind) != 1L || !nzchar(kind)) {
            return(NULL)
        }
        kind
    }
    .mojor_ir_var_name <- function(node) {
        if (!is.list(node) || !identical(node$kind, "var") || is.null(node$name)) {
            return(NULL)
        }
        nm <- as.character(node$name)
        if (!is.character(nm) || length(nm) != 1L || !nzchar(nm)) {
            return(NULL)
        }
        nm
    }
    .mojor_normalize_inferred_type <- function(spec) {
        if (!is.character(spec) || length(spec) != 1L || !nzchar(spec)) {
            return(NULL)
        }
        if (spec %in% c("bool[]", "Bool[]")) {
            return("lgl[]")
        }
        if (spec %in% c("bool", "Bool")) {
            return("lgl")
        }
        if (spec %in% c("Int", "Int32")) {
            return("i32")
        }
        if (spec %in% c("Float64")) {
            return("f64")
        }
        if (spec %in% c("Float32")) {
            return("f32")
        }
        spec
    }
    .mojor_rhs_type_env <- function() {
        env <- types
        if (length(scalar_inits) > 0L) {
            for (nm in names(scalar_inits)) {
                ty <- scalar_inits[[nm]]$type
                if (!is.null(ty) && is.character(ty) && length(ty) == 1L && nzchar(ty)) {
                    env[[nm]] <- ty
                }
            }
        }
        if (length(active_loop_vars) > 0L) {
            for (nm in active_loop_vars) {
                env[[nm]] <- "i32"
            }
        }
        env
    }
    .mojor_infer_rhs_ir_assignment <- function(rhs_expr) {
        rhs_ir <- tryCatch(.mojor_ir_expr_build(rhs_expr), error = function(e) NULL)
        if (is.null(rhs_ir)) {
            return(NULL)
        }
        rhs_kind <- .mojor_ir_kind_name(rhs_ir)
        if (is.null(rhs_kind)) {
            return(NULL)
        }
        rhs_type <- tryCatch(
            .mojor_ir_infer_type(rhs_ir, .mojor_rhs_type_env()),
            error = function(e) NULL
        )
        rhs_type <- .mojor_normalize_inferred_type(rhs_type)
        if (is.null(rhs_type)) {
            return(NULL)
        }
        list(kind = rhs_kind, type = rhs_type, node = rhs_ir)
    }
    .mojor_ir_vector_assignment_supported <- function(rhs_ir_info) {
        if (!is.list(rhs_ir_info) || is.null(rhs_ir_info$node) || is.null(rhs_ir_info$kind)) {
            return(FALSE)
        }
        node <- rhs_ir_info$node
        kind <- rhs_ir_info$kind
        x_name <- .mojor_ir_var_name(node$x)
        if (kind %in% c("unique", "duplicated", "vapply", "sapply", "lapply", "nchar", "nzchar", "substr")) {
            return(!is.null(x_name))
        }
        if (kind == "paste") {
            if (is.null(node$args) || length(node$args) == 0L) {
                return(FALSE)
            }
            return(TRUE)
        }
        if (kind %in% c("match", "in")) {
            return(!is.null(x_name) && !is.null(.mojor_ir_var_name(node$table)))
        }
        if (kind == "quantile") {
            return(!is.null(x_name) && !is.null(.mojor_ir_var_name(node$probs)))
        }
        if (kind == "sample_int") {
            return(!is.null(node$n) && !is.null(node$size))
        }
        if (kind == "sample") {
            return(!is.null(node$x) && !is.null(node$size))
        }
        if (kind == "mapply") {
            if (is.null(node$args) || length(node$args) < 2L) {
                return(FALSE)
            }
            return(TRUE)
        }
        if (kind %in% c("regex_grepl", "regex_grep", "regex_sub")) {
            return(!is.null(node$pattern) && !is.null(node$x))
        }
        if (kind %in% c("row_matrix", "col_matrix")) {
            return(!is.null(node$x))
        }
        if (kind == "expand_grid") {
            if (is.null(node$args) || length(node$args) == 0L) {
                return(FALSE)
            }
            return(TRUE)
        }
        FALSE
    }
    .mojor_rhs_out_len_source_from_ir <- function(rhs_ir_info) {
        if (!is.list(rhs_ir_info) || is.null(rhs_ir_info$node) || is.null(rhs_ir_info$kind)) {
            return(NULL)
        }
        node <- rhs_ir_info$node
        kind <- rhs_ir_info$kind
        x_name <- .mojor_ir_var_name(node$x)
        if (kind %in% c("unique", "duplicated", "match", "in", "vapply", "sapply", "lapply", "nchar", "nzchar", "substr")) {
            if (!is.null(x_name)) {
                return(list(kind = "array", name = x_name))
            }
            return(NULL)
        }
        if (kind == "paste") {
            if (is.null(node$args) || length(node$args) == 0L) {
                return(NULL)
            }
            first_name <- .mojor_ir_var_name(node$args[[1L]])
            if (!is.null(first_name)) {
                return(list(kind = "array", name = first_name))
            }
            return(NULL)
        }
        if (kind == "quantile") {
            probs_name <- .mojor_ir_var_name(node$probs)
            if (!is.null(probs_name)) {
                if (probs_name %in% args) {
                    return(list(kind = "array", name = probs_name))
                }
                local_vecs <- tryCatch(.mojor_state$current_local_vector_lengths, error = function(e) NULL)
                if (is.list(local_vecs) && is.list(local_vecs[[probs_name]]) &&
                    !is.null(local_vecs[[probs_name]]$length)) {
                    len_val <- local_vecs[[probs_name]]$length
                    if (is.numeric(len_val) && length(len_val) == 1L && is.finite(len_val)) {
                        return(list(kind = "expr", expr = as.integer(len_val)))
                    }
                }
            }
            return(NULL)
        }
        if (kind == "mapply") {
            if (is.null(node$args) || length(node$args) == 0L) {
                return(NULL)
            }
            src <- .mojor_ir_var_name(node$args[[1L]])
            if (!is.null(src)) {
                return(list(kind = "array", name = src))
            }
        }
        if (kind == "sample_int") {
            size_node <- node$size
            if (is.list(size_node) && identical(size_node$kind, "var")) {
                nm <- as.character(size_node$name)
                if (is.character(nm) && length(nm) == 1L && nzchar(nm)) {
                    if (nm %in% args && !isTRUE(.mojor_is_array(types[[nm]]))) {
                        return(list(kind = "scalar", name = nm))
                    }
                    if (!is.null(scalar_inits[[nm]]) &&
                        is.list(scalar_inits[[nm]]) &&
                        !is.null(scalar_inits[[nm]]$type) &&
                        scalar_inits[[nm]]$type %in% c("i32", "f64", "f32", "lgl", "bool")) {
                        return(list(kind = "scalar", name = nm))
                    }
                }
            }
            if (is.list(size_node) && identical(size_node$kind, "const")) {
                v <- suppressWarnings(as.integer(size_node$value))
                if (is.finite(v)) {
                    return(list(kind = "expr", expr = as.integer(v)))
                }
            }
            return(NULL)
        }
        if (kind == "sample") {
            size_node <- node$size
            if (is.list(size_node) && identical(size_node$kind, "var")) {
                nm <- as.character(size_node$name)
                if (is.character(nm) && length(nm) == 1L && nzchar(nm) &&
                    nm %in% args && !isTRUE(.mojor_is_array(types[[nm]]))) {
                    return(list(kind = "scalar", name = nm))
                }
            }
            if (is.list(size_node) && identical(size_node$kind, "const")) {
                v <- suppressWarnings(as.integer(size_node$value))
                if (is.finite(v)) {
                    return(list(kind = "expr", expr = as.integer(v)))
                }
            }
            if (is.list(size_node) &&
                identical(size_node$kind, "call") &&
                identical(size_node$fn, "length") &&
                is.list(size_node$args) &&
                length(size_node$args) == 1L &&
                is.list(size_node$args[[1L]]) &&
                identical(size_node$args[[1L]]$kind, "var")) {
                src <- .mojor_ir_var_name(size_node$args[[1L]])
                if (!is.null(src)) {
                    return(list(kind = "array", name = src))
                }
            }
            if (!is.null(x_name)) {
                return(list(kind = "array", name = x_name))
            }
            return(NULL)
        }
        if (kind %in% c("regex_grepl", "regex_grep", "regex_sub", "row_matrix", "col_matrix")) {
            if (!is.null(x_name)) {
                return(list(kind = "array", name = x_name))
            }
            return(NULL)
        }
        if (kind == "expand_grid") {
            if (is.null(node$args) || length(node$args) == 0L) {
                return(NULL)
            }
            src <- .mojor_ir_var_name(node$args[[1L]])
            if (!is.null(src)) {
                return(list(kind = "array", name = src))
            }
            return(NULL)
        }
        NULL
    }
    .collect_modified_args <- function(stmts) {
        modified <- character(0)
        alias_map <- list()

        is_array_arg_name <- function(name) {
            if (!(name %in% args)) {
                return(FALSE)
            }
            spec <- types[[name]]
            !is.null(spec) && .mojor_is_array(spec)
        }

        normalize_alias_targets <- function(targets) {
            if (is.null(targets) || length(targets) == 0L) {
                return(character(0))
            }
            targets <- as.character(targets)
            targets <- targets[nzchar(targets)]
            targets <- targets[targets %in% args]
            targets <- targets[vapply(targets, is_array_arg_name, logical(1))]
            unique(targets)
        }

        clone_alias_map <- function(map) {
            out <- list()
            if (length(map) == 0L) {
                return(out)
            }
            for (nm in names(map)) {
                out[[nm]] <- normalize_alias_targets(map[[nm]])
            }
            out
        }

        merge_alias_maps <- function(left, right) {
            out <- list()
            keys <- unique(c(names(left), names(right)))
            if (length(keys) == 0L) {
                return(out)
            }
            for (nm in keys) {
                out[[nm]] <- normalize_alias_targets(c(left[[nm]], right[[nm]]))
            }
            out
        }

        resolve_alias_targets <- function(name, map = alias_map, seen = character(0)) {
            if (!is.character(name) ||
                length(name) != 1L ||
                !nzchar(name)) {
                return(character(0))
            }
            if (is_array_arg_name(name)) {
                return(name)
            }
            if (name %in% seen) {
                return(character(0))
            }
            alias_targets <- map[[name]]
            if (is.null(alias_targets) ||
                length(alias_targets) == 0L) {
                return(character(0))
            }
            resolved <- character(0)
            for (target in alias_targets) {
                if (!is.character(target) ||
                    length(target) != 1L ||
                    !nzchar(target)) {
                    next
                }
                if (is_array_arg_name(target)) {
                    resolved <- c(resolved, target)
                    next
                }
                resolved <- c(
                    resolved,
                    resolve_alias_targets(target, map = map, seen = c(seen, name))
                )
            }
            normalize_alias_targets(resolved)
        }

        collect_rhs_alias_targets <- function(rhs_expr, map = alias_map) {
            if (is.name(rhs_expr)) {
                return(resolve_alias_targets(as.character(rhs_expr), map = map))
            }
            if (!is.call(rhs_expr) || length(rhs_expr) == 0L) {
                return(character(0))
            }
            op <- as.character(rhs_expr[[1]])
            if (length(op) != 1L) {
                op <- op[[1]]
            }
            if (op == "(" && length(rhs_expr) == 2L) {
                return(collect_rhs_alias_targets(rhs_expr[[2]], map = map))
            }
            if (op == "{" && length(rhs_expr) >= 2L) {
                return(collect_rhs_alias_targets(rhs_expr[[length(rhs_expr)]], map = map))
            }
            if (op == "if" && length(rhs_expr) >= 3L) {
                then_targets <- collect_rhs_alias_targets(rhs_expr[[3]], map = map)
                else_targets <- character(0)
                if (length(rhs_expr) >= 4L) {
                    else_targets <- collect_rhs_alias_targets(rhs_expr[[4]], map = map)
                }
                return(normalize_alias_targets(c(then_targets, else_targets)))
            }
            character(0)
        }

        extract_base_name <- function(expr) {
            cur <- expr
            repeat {
                if (is.call(cur) &&
                    as.character(cur[[1]]) == "(" &&
                    length(cur) == 2L) {
                    cur <- cur[[2]]
                    next
                }
                break
            }
            expr <- cur
            if (is.name(expr)) {
                return(as.character(expr))
            }
            NULL
        }

        mark_modified_base <- function(base_name, map = alias_map) {
            resolved <- resolve_alias_targets(base_name, map = map)
            if (length(resolved) == 0L &&
                is_array_arg_name(base_name)) {
                resolved <- base_name
            }
            if (length(resolved) > 0L) {
                modified <<- unique(c(modified, resolved))
            }
            invisible(NULL)
        }

        walk_blocks <- NULL
        walk_stmt <- function(stmt, current_alias_map) {
            if (!is.call(stmt) ||
                length(stmt) == 0L) {
                return(current_alias_map)
            }
            op <- as.character(stmt[[1]])
            if (length(op) != 1L) {
                op <- op[[1]]
            }
            next_alias_map <- current_alias_map

            if (op %in% c("<-", "=", "<<-") &&
                length(stmt) >= 3L) {
                lhs_expr <- stmt[[2]]
                rhs_expr <- stmt[[3]]
                if (is.name(lhs_expr)) {
                    lhs_name <- as.character(lhs_expr)
                    if (!(lhs_name %in% args)) {
                        rhs_targets <- collect_rhs_alias_targets(rhs_expr, map = current_alias_map)
                        next_alias_map[[lhs_name]] <- normalize_alias_targets(rhs_targets)
                    }
                } else if (is.call(lhs_expr) &&
                    length(lhs_expr) >= 2L) {
                    lhs_op <- as.character(lhs_expr[[1]])
                    if (length(lhs_op) != 1L) {
                        lhs_op <- lhs_op[[1]]
                    }
                    if (lhs_op %in% c("[", "[[")) {
                        base_name <- extract_base_name(lhs_expr[[2]])
                        if (!is.null(base_name)) {
                            mark_modified_base(base_name, map = current_alias_map)
                        }
                    }
                }
            }

            if (op == "if" &&
                length(stmt) >= 3L) {
                then_alias_map <- walk_blocks(
                    .mojor_extract_block(stmt[[3]]),
                    clone_alias_map(current_alias_map)
                )
                else_alias_map <- NULL
                if (length(stmt) >= 4L) {
                    else_alias_map <- walk_blocks(
                        .mojor_extract_block(stmt[[4]]),
                        clone_alias_map(current_alias_map)
                    )
                }
                if (is.null(else_alias_map)) {
                    next_alias_map <- merge_alias_maps(current_alias_map, then_alias_map)
                } else {
                    next_alias_map <- merge_alias_maps(then_alias_map, else_alias_map)
                }
            } else if (op == "for" &&
                length(stmt) >= 4L) {
                loop_alias_map <- walk_blocks(
                    .mojor_extract_block(stmt[[4]]),
                    clone_alias_map(current_alias_map)
                )
                next_alias_map <- merge_alias_maps(current_alias_map, loop_alias_map)
            } else if (op == "while" &&
                length(stmt) >= 3L) {
                loop_alias_map <- walk_blocks(
                    .mojor_extract_block(stmt[[3]]),
                    clone_alias_map(current_alias_map)
                )
                next_alias_map <- merge_alias_maps(current_alias_map, loop_alias_map)
            } else if (op == "repeat" &&
                length(stmt) >= 2L) {
                loop_alias_map <- walk_blocks(
                    .mojor_extract_block(stmt[[2]]),
                    clone_alias_map(current_alias_map)
                )
                next_alias_map <- merge_alias_maps(current_alias_map, loop_alias_map)
            }
            next_alias_map
        }

        walk_blocks <- function(block_items, current_alias_map = list()) {
            if (length(block_items) == 0L) {
                return(current_alias_map)
            }
            alias_state <- current_alias_map
            for (stmt in block_items) {
                alias_state <- walk_stmt(stmt, alias_state)
            }
            alias_state
        }

        alias_map <- walk_blocks(stmts, alias_map)
        unique(modified)
    }

    for (b in blocks) {
        if (is.call(b) &&
            as.character(b[[1]]) %in%
                c("<-", "=")) {
            lhs_expr <- b[[2]]
 # Handle both simple names and indexed assignment (e.g.,
 # out['a', 'c'])
            if (is.call(lhs_expr) &&
                as.character(lhs_expr[[1]]) ==
                  "[") {
                lhs <- as.character(lhs_expr[[2]])
            } else if (is.name(lhs_expr)) {
                lhs <- as.character(lhs_expr)
            } else {
                next  # Skip unsupported LHS forms
            }
            if (!is.null(out_name) &&
                is.call(lhs_expr) &&
                as.character(lhs_expr[[1]]) ==
                  "[" && lhs == out_name) {
                next
            }
            rhs <- b[[3]]
            name_assign_rhs[[lhs]] <- rhs
            if (is.name(lhs_expr) && !(lhs %in% args)) {
                alias_target <- NULL
                if (is.name(rhs)) {
                    alias_target <- .resolve_array_alias_target(as.character(rhs))
                }
                if (!is.null(alias_target) && !identical(lhs, alias_target)) {
                    array_aliases[[lhs]] <- alias_target
                } else {
                    array_aliases[[lhs]] <- NULL
                }
            }
            if (!is.name(lhs_expr)) {
                # Indexed writes (e.g., out[i] <- v, out[i, j] <- v) should not
                # be interpreted as scalar/output-initialization assignments.
                next
            }
            if (is.call(rhs) &&
                length(rhs) >=
                  1 && is.character(as.character(rhs[[1]])[1]) &&
                as.character(rhs[[1]])[1] %in%
                  c("numeric", "integer", "logical")) {
                out_name <- lhs
                if (as.character(rhs[[1]]) ==
                  "integer") {
                  out_type <- "i32[]"
                } else if (as.character(rhs[[1]]) ==
                  "logical") {
                  out_type <- "lgl[]"
                } else {
                  out_type <- "f64[]"
                }
                out_matrix <- FALSE
                out_array <- FALSE
                if (!(lhs %in% args)) {
                  local_array_specs[[lhs]] <- out_type
                }
 # Capture output length source for histogram/count
 # patterns e.g., integer(length(bins)) ->
 # out_len_source = list(kind='array', name='bins')
 # e.g., integer(nbins) -> out_len_source =
 # list(kind='scalar', name='nbins')
                if (length(rhs) ==
                  2) {
                  len_expr <- rhs[[2]]
                  resolve_local_len_refs <- function(expr, seen = character(0)) {
                    if (is.null(expr)) {
                      return(expr)
                    }
                    if (is.name(expr)) {
                      nm <- as.character(expr)
                      if (!(nm %in% seen) &&
                        !is.null(local_array_len_exprs[[nm]])) {
                        return(resolve_local_len_refs(local_array_len_exprs[[nm]], c(seen, nm)))
                      }
                      if (!(nm %in% seen) &&
                        !is.null(scalar_inits[[nm]]) &&
                        is.list(scalar_inits[[nm]]$value)) {
                        sval <- scalar_inits[[nm]]$value
                        if (identical(sval$kind, "len") && !is.null(sval$name)) {
                          base_expr <- call("length", as.name(sval$name))
                          if (!is.null(sval$delta) && as.integer(sval$delta) != 0L) {
                            delta <- as.integer(sval$delta)
                            base_expr <- if (delta > 0L) {
                              call("+", base_expr, delta)
                            } else {
                              call("-", base_expr, abs(delta))
                            }
                          }
                          return(resolve_local_len_refs(base_expr, c(seen, nm)))
                        }
                        if (identical(sval$kind, "expr") && !is.null(sval$expr)) {
                          return(resolve_local_len_refs(sval$expr, c(seen, nm)))
                        }
                      }
                      return(expr)
                    }
                    if (!is.call(expr)) {
                      return(expr)
                    }
                    op <- as.character(expr[[1]])
                    if (op == "length" && length(expr) == 2 && is.name(expr[[2]])) {
                      nm <- as.character(expr[[2]])
                      if (!(nm %in% seen) &&
                        !is.null(local_array_len_exprs[[nm]])) {
                        return(resolve_local_len_refs(local_array_len_exprs[[nm]], c(seen, nm)))
                      }
                      return(expr)
                    }
                    parts <- as.list(expr)
                    if (length(parts) >= 2) {
                      for (k in 2:length(parts)) {
                        parts[[k]] <- resolve_local_len_refs(parts[[k]], seen)
                      }
                    }
                    as.call(parts)
                  }
                  if (is.call(len_expr) &&
                    as.character(len_expr[[1]]) ==
                      "length" && length(len_expr) ==
                    2) {
                    len_arg <- len_expr[[2]]
                    if (is.name(len_arg)) {
                      len_arg_sym <- as.character(len_arg)
                      len_arg_name <- .resolve_array_alias_target(len_arg_sym)
                      if (!is.null(len_arg_name)) {
                        out_len_source <- list(
                          kind = "array",
                          name = len_arg_name,
                          preserve_dim = TRUE
                        )
                      } else if (!is.null(local_array_len_exprs[[len_arg_sym]])) {
                        out_len_source <- list(kind = "expr", expr = local_array_len_exprs[[len_arg_sym]])
                      }
                    }
                  } else if (is.call(len_expr) &&
                    as.character(len_expr[[1]]) == "sum" &&
                    length(len_expr) >= 2) {
                    # Handle mask-sized outputs: out <- numeric(sum(mask))
                    # by routing output length through mask_true metadata.
                    sum_arg <- len_expr[[2]]
                    if (is.name(sum_arg)) {
                      sum_name <- as.character(sum_arg)
                      sum_type <- types[[sum_name]]
                      if (!is.null(sum_type) &&
                        .mojor_is_logical_mask_type(sum_type)) {
                        out_len_source <- list(kind = "mask_true", name = sum_name)
                      }
                    }
                  } else if (is.name(len_expr)) {
                    len_arg_name <- as.character(len_expr)
                    if (len_arg_name %in% args && !.mojor_is_array(types[[len_arg_name]])) {
                      out_len_source <- list(kind = "scalar", name = len_arg_name)
                    } else if (!is.null(scalar_inits[[len_arg_name]]) &&
                      is.list(scalar_inits[[len_arg_name]]$value) &&
                      identical(scalar_inits[[len_arg_name]]$value$kind, "len")) {
                      len_info <- scalar_inits[[len_arg_name]]$value
                      len_expr_ast <- call("length", as.name(len_info$name))
                      if (!is.null(len_info$delta) &&
                        len_info$delta != 0L) {
                        delta <- as.integer(len_info$delta)
                        if (!is.na(delta) &&
                          delta != 0L) {
                          len_expr_ast <- if (delta > 0L) {
                            call("+", len_expr_ast, delta)
                          } else {
                            call("-", len_expr_ast, abs(delta))
                          }
                        }
                      }
                      out_len_source <- list(kind = "expr", expr = len_expr_ast)
                    }
                  } else if (is.numeric(len_expr) &&
                    length(len_expr) ==
                      1 && !is.na(len_expr)) {
                    out_len_source <- list(kind = "expr", expr = as.integer(len_expr))
                  }
                  if (is.null(out_len_source) && !is.null(len_expr)) {
                    resolved_len_expr <- resolve_local_len_refs(len_expr)
                    len_expr_ok <- tryCatch({
                      .mojor_len_expr_to_c(
                        resolved_len_expr,
                        args = args,
                        arg_specs = types,
                        allow_scalar_cast = TRUE
                      )
                      TRUE
                    }, error = function(e) FALSE)
                    if (isTRUE(len_expr_ok)) {
                      out_len_source <- list(kind = "expr", expr = resolved_len_expr)
                    }
                  }
 # Step 5.1 Stage B: Track local vector allocations
 # (not function parameters)
                  if (!out_name %in% args && !is.null(len_expr)) {
                    if (is.null(.mojor_state$current_local_vector_lengths)) {
                      .mojor_state$current_local_vector_lengths <- list()
                    }
 # Use resolved expression from scalar_inits when
 # available (e.g., n <- length(x); out <-
 # numeric(n) <U+2192> store length(x) instead of
 # n)
                    resolved_len <- resolve_local_len_refs(len_expr)
                    local_array_len_exprs[[out_name]] <- resolved_len
                    .mojor_state$current_local_vector_lengths[[out_name]] <- list(
                      length = resolved_len,
                      length_alloc = resolved_len,
                      length_runtime = resolved_len,
                      type = out_type,
                      origin = "ctor"
                    )
                  }
                }
            } else if (is.call(rhs) &&
                length(rhs) >= 2 &&
                identical(as.character(rhs[[1]]), "c") &&
                !lhs %in% args) {
                ctor_parts <- as.list(rhs)[-1]
                if (length(ctor_parts) > 0 &&
                  all(vapply(ctor_parts, function(part) {
                    (is.numeric(part) || is.integer(part)) &&
                      length(part) == 1L &&
                      !is.na(part)
                  }, logical(1)))) {
                    all_int <- all(vapply(ctor_parts, is.integer, logical(1)))
                    if (is.null(.mojor_state$current_local_vector_lengths)) {
                      .mojor_state$current_local_vector_lengths <- list()
                    }
                    .mojor_state$current_local_vector_lengths[[lhs]] <- list(
                      length = as.integer(length(ctor_parts)),
                      type = if (isTRUE(all_int)) "i32[]" else "f64[]"
                    )
                }
            } else if (!is.null(out_name) &&
                lhs == out_name && is.call(rhs) &&
                as.character(rhs[[1]]) ==
                  "[" && length(rhs) ==
                3) {
                base <- rhs[[2]]
                idx_info <- .extract_selector_arg(rhs, 3L)
                if (is.name(base) && !isTRUE(idx_info$missing)) {
                  idx <- idx_info$value
                  if (is.name(idx)) {
                    idx_name <- as.character(idx)
                    if (!is.null(types[[idx_name]]) &&
                      .mojor_is_logical_mask_type(types[[idx_name]])) {
                      out_len_source <- list(kind = "mask_true", name = idx_name)
                    }
                  }
                }
                # Detect R-style negative-index exclusion: out <- x[-k] or out <- x[-c(...)]
                if (is.name(base) && is.null(out_len_source) && !isTRUE(idx_info$missing)) {
                  idx <- idx_info$value
                  neg_info <- .mojor_ir_detect_negative_index(idx)
                  if (neg_info$is_negative && !isTRUE(neg_info$is_vector)) {
                    base_name <- as.character(base)
                    out_len_source <- list(kind = "exclusion", base = base_name,
                                           neg_value = neg_info$value,
                                           is_dynamic = isTRUE(neg_info$is_dynamic))
                    out_name <- lhs
                    if (!is.null(types[[base_name]])) {
                      out_type <- types[[base_name]]
                    }
                  } else if (neg_info$is_negative && isTRUE(neg_info$is_vector)) {
                    vec_excl <- .mojor_ir_build_vector_exclusion_index(neg_info)
                    if (!is.null(vec_excl)) {
                      base_name <- as.character(base)
                      excl_count <- vec_excl$neg_vec_exclusion$count
                      out_len_source <- list(kind = "exclusion", base = base_name,
                                             neg_value = neg_info$value,
                                             is_dynamic = FALSE,
                                             excl_count = excl_count)
                      out_name <- lhs
                      if (!is.null(types[[base_name]])) {
                        out_type <- types[[base_name]]
                      }
                    }
                  }
                }
            } else if (is.call(rhs) &&
                length(rhs) ==
                  2 && is.call(rhs[[1]]) &&
                as.character(rhs[[1]][[1]]) ==
                  "::" && as.character(rhs[[1]][[2]]) ==
                "float" && as.character(rhs[[1]][[3]]) ==
                "float32") {
                out_name <- lhs
                out_type <- "f32[]"
                out_matrix <- FALSE
                out_array <- FALSE
            } else if (is.call(rhs) &&
                as.character(rhs[[1]]) ==
                  "apply") {
                apply_out <- .mojor_detect_apply_assignment(lhs, rhs, args, types)
                if (!is.null(apply_out)) {
                  out_name <- apply_out$out_name
                  out_type <- apply_out$out_type
                  out_matrix <- FALSE
                  out_array <- FALSE
                  out_len_source <- apply_out$out_len_source
                }
            } else if (is.integer(rhs) &&
                length(rhs) ==
                  1) {
                if (is.null(scalar_inits[[lhs]]))
                  scalar_inits[[lhs]] <- list(type = "i32", value = rhs)
            } else if (is.logical(rhs) &&
                length(rhs) ==
                  1) {
                if (is.null(scalar_inits[[lhs]]))
                  scalar_inits[[lhs]] <- list(type = "lgl", value = if (isTRUE(rhs)) 1L else 0L)
            } else if (is.numeric(rhs) &&
                length(rhs) ==
                  1) {
                if (is.nan(rhs)) {
                  if (is.null(scalar_inits[[lhs]]))
                    scalar_inits[[lhs]] <- list(type = "f64", value = "_MOJOR_NAN")
                } else if (is.infinite(rhs)) {
                  val <- if (rhs > 0)
                    "_MOJOR_INF" else "_MOJOR_NINF"
                  if (is.null(scalar_inits[[lhs]]))
                    scalar_inits[[lhs]] <- list(type = "f64", value = val)
                } else {
                  if (is.null(scalar_inits[[lhs]]))
                    scalar_inits[[lhs]] <- list(type = "f64", value = rhs)
                }
            } else if (is.name(rhs) &&
                as.character(rhs) %in%
                  c("Inf", "NaN")) {
                val <- if (as.character(rhs) ==
                  "Inf")
                  "_MOJOR_INF" else "_MOJOR_NAN"
                if (is.null(scalar_inits[[lhs]]))
                  scalar_inits[[lhs]] <- list(type = "f64", value = val)
            } else if (is.call(rhs) &&
                as.character(rhs[[1]]) ==
                  "-" && length(rhs) ==
                2) {
                inner <- rhs[[2]]
                if (is.name(inner) &&
                  as.character(inner) %in%
                    c("Inf", "NaN")) {
                  val <- if (as.character(inner) ==
                    "Inf")
                    "_MOJOR_NINF" else "_MOJOR_NAN"
                  if (is.null(scalar_inits[[lhs]]))
                    scalar_inits[[lhs]] <- list(type = "f64", value = val)
                } else if (is.numeric(inner) &&
                  length(inner) ==
                    1) {
                  if (is.nan(inner)) {
                    if (is.null(scalar_inits[[lhs]]))
                      scalar_inits[[lhs]] <- list(type = "f64", value = "_MOJOR_NAN")
                  } else if (is.infinite(inner)) {
                    val <- if (inner > 0)
                      "_MOJOR_NINF" else "_MOJOR_INF"
                    if (is.null(scalar_inits[[lhs]]))
                      scalar_inits[[lhs]] <- list(type = "f64", value = val)
                  } else {
                    if (is.null(scalar_inits[[lhs]]))
                      scalar_inits[[lhs]] <- list(type = "f64", value = -inner)
                  }
                } else if (is.integer(inner) &&
                  length(inner) ==
                    1) {
                  if (is.null(scalar_inits[[lhs]]))
                    scalar_inits[[lhs]] <- list(type = "i32", value = as.integer(-inner))
                }
            } else if (is.call(rhs) &&
                length(rhs) ==
                  2 && as.character(rhs[[1]]) %in%
                c("as.single", "as.double", "as.integer")) {
                inner <- rhs[[2]]
                if ((is.numeric(inner) ||
                  is.integer(inner) ||
                  is.logical(inner)) &&
                  length(inner) ==
                    1) {
                  inner_val <- if (is.logical(inner))
                    if (isTRUE(inner))
                      1L else 0L else inner
                  op_rhs <- as.character(rhs[[1]])
                  if (op_rhs == "as.single") {
                    if (is.null(scalar_inits[[lhs]]))
                      scalar_inits[[lhs]] <- list(
                        type = "f32", value = paste0(
                          "Float32(", as.numeric(inner_val),
                          ")"
                      )
                    )
                  } else if (op_rhs == "as.double") {
                    if (is.null(scalar_inits[[lhs]]))
                      scalar_inits[[lhs]] <- list(type = "f64", value = as.numeric(inner_val))
                  } else if (op_rhs == "as.integer") {
                    if (is.null(scalar_inits[[lhs]]))
                      scalar_inits[[lhs]] <- list(
                        type = "i32", value = paste0(
                          "Int32(", as.integer(inner_val),
                          ")"
                      )
                    )
                  }
                } else if (as.character(rhs[[1]]) ==
                  "as.integer") {
                  len_call <- NULL
                  delta <- 0L
                  if (is.call(inner) &&
                    as.character(inner[[1]]) ==
                      "length" && length(inner) ==
                    2) {
                    len_call <- inner
                  } else if (is.call(inner) &&
                    as.character(inner[[1]]) %in%
                      c("+", "-") &&
                    length(inner) ==
                      3) {
                    lhs_inner <- inner[[2]]
                    rhs_inner <- inner[[3]]
                    if (is.call(lhs_inner) &&
                      as.character(lhs_inner[[1]]) ==
                        "length" && length(lhs_inner) ==
                      2 && (is.numeric(rhs_inner) ||
                      is.integer(rhs_inner)) &&
                      length(rhs_inner) ==
                        1) {
                      len_call <- lhs_inner
                      delta <- if (as.character(inner[[1]]) ==
                        "+")
                        as.integer(rhs_inner) else -as.integer(rhs_inner)
                    } else if (as.character(inner[[1]]) ==
                      "+" && is.call(rhs_inner) &&
                      as.character(rhs_inner[[1]]) ==
                        "length" && length(rhs_inner) ==
                      2 && (is.numeric(lhs_inner) ||
                      is.integer(lhs_inner)) &&
                      length(lhs_inner) ==
                        1) {
                      len_call <- rhs_inner
                      delta <- as.integer(lhs_inner)
                    }
                  }
                  if (!is.null(len_call)) {
                    arg <- len_call[[2]]
                    if (is.name(arg)) {
                      len_arg_name <- .resolve_array_alias_target(as.character(arg))
                      spec <- if (!is.null(len_arg_name))
                        types[[len_arg_name]] else NULL
                      if (!is.null(spec) &&
                        .mojor_is_array(spec)) {
                        scalar_inits[[lhs]] <- list(
                          type = "i32", value = list(kind = "len", name = len_arg_name, delta = delta)
                      )
                      }
                    }
                  }
                  if (is.null(scalar_inits[[lhs]]) &&
                      .is_i32_scalar_expr(rhs)) {
                      scalar_inits[[lhs]] <- list(
                        type = "i32",
                        value = list(kind = "expr", expr = rhs)
                      )
                  }
                }
            } else if (.is_i32_scalar_expr(rhs)) {
                if (is.null(scalar_inits[[lhs]])) {
                    if (is.call(rhs) &&
                        as.character(rhs[[1]]) == "length" &&
                        length(rhs) == 2 &&
                        is.name(rhs[[2]])) {
                        len_arg_name <- .resolve_array_alias_target(as.character(rhs[[2]]))
                        len_spec <- if (!is.null(len_arg_name)) types[[len_arg_name]] else NULL
                        if (!is.null(len_spec) && .mojor_is_array(len_spec)) {
                            scalar_inits[[lhs]] <- list(
                                type = "i32",
                                value = list(kind = "len", name = len_arg_name, delta = 0L)
                            )
                        } else {
                            scalar_inits[[lhs]] <- list(
                                type = "i32",
                                value = list(kind = "expr", expr = rhs)
                            )
                        }
                    } else {
                        scalar_inits[[lhs]] <- list(
                            type = "i32",
                            value = list(kind = "expr", expr = rhs)
                        )
                    }
                }
            } else if (is.name(rhs)) {
                rhs_name <- as.character(rhs)
                rhs_alias <- .resolve_array_alias_target(rhs_name)
                rhs_alias_spec <- if (!is.null(rhs_alias)) types[[rhs_alias]] else NULL
                if (!is.null(rhs_alias_spec) && .mojor_is_array(rhs_alias_spec)) {
                    local_array_specs[[lhs]] <- rhs_alias_spec
                    rhs_len_expr <- .base_len_expr_for(rhs_name)
                    if (!is.null(rhs_len_expr)) {
                        local_array_len_exprs[[lhs]] <- rhs_len_expr
                    }
                    out_name <- lhs
                    out_type <- rhs_alias_spec
                    rank <- .mojor_type_ndim(rhs_alias_spec)
                    if (!is.na(rank) && rank == 2L) {
                        out_matrix <- TRUE
                        out_array <- FALSE
                        out_nrow_expr <- call("nrow", as.name(rhs_alias))
                        out_ncol_expr <- call("ncol", as.name(rhs_alias))
                        out_dim_exprs <- NULL
                        out_dim_name <- NULL
                    } else if (!is.na(rank) && rank >= 3L) {
                        out_matrix <- FALSE
                        out_array <- TRUE
                        out_nrow_expr <- NULL
                        out_ncol_expr <- NULL
                        out_dim_name <- NULL
                        out_dim_exprs <- lapply(seq_len(rank), function(k) {
                            call("[", call("dim", as.name(rhs_alias)), as.integer(k))
                        })
                    } else {
                        out_matrix <- FALSE
                        out_array <- FALSE
                        out_nrow_expr <- NULL
                        out_ncol_expr <- NULL
                        out_dim_exprs <- NULL
                        out_dim_name <- NULL
                    }
                }
                if (!is.null(scalar_inits[[rhs_name]]) &&
                  is.null(scalar_inits[[lhs]])) {
                  scalar_inits[[lhs]] <- scalar_inits[[rhs_name]]
                }
                if (is.null(scalar_inits[[lhs]]) &&
                  rhs_name %in% args) {
                  rhs_spec <- types[[rhs_name]]
                  if (!is.null(rhs_spec) &&
                    !.mojor_is_array(rhs_spec)) {
                    scalar_inits[[lhs]] <- list(type = rhs_spec, value = list(kind = "arg", name = rhs_name))
                  }
                }
            } else if (is.call(rhs) &&
                as.character(rhs[[1]]) ==
                  "[" && length(rhs) ==
                3 && is.name(rhs[[2]])) {
                arr_name <- as.character(rhs[[2]])
                idx_info <- .extract_selector_arg(rhs, 3L)
                idx <- idx_info$value
                arr_arg_name <- .resolve_array_alias_target(arr_name)
                spec <- if (!is.null(arr_arg_name)) types[[arr_arg_name]] else local_array_specs[[arr_name]]
                vec_spec <- .vector_out_spec(spec)
                idx_is_scalar <- FALSE
                if (isTRUE(idx_info$missing)) {
                    idx_is_scalar <- FALSE
                } else if ((is.integer(idx) || is.numeric(idx)) &&
                    length(idx) == 1 &&
                    !is.na(idx) &&
                    as.integer(idx) > 0L) {
                    idx_is_scalar <- TRUE
                } else if (is.name(idx)) {
                    idx_name <- as.character(idx)
                    idx_type <- types[[idx_name]]
                    if (is.null(idx_type) && !is.null(scalar_inits[[idx_name]]) &&
                        !is.null(scalar_inits[[idx_name]]$type)) {
                        idx_type <- scalar_inits[[idx_name]]$type
                    }
                    if (!is.null(idx_type) && !.mojor_is_array(idx_type)) {
                        idx_is_scalar <- TRUE
                    }
                    if (idx_name %in% active_loop_vars) {
                        idx_is_scalar <- TRUE
                    }
                }
                if (!is.null(vec_spec) && !isTRUE(idx_is_scalar)) {
                    local_array_specs[[lhs]] <- vec_spec
                    lhs_len_expr <- .subset_len_expr(arr_name, idx)
                    if (!is.null(lhs_len_expr)) {
                        local_array_len_exprs[[lhs]] <- lhs_len_expr
                    }
                    if (is.null(.mojor_state$current_local_vector_lengths)) {
                        .mojor_state$current_local_vector_lengths <- list()
                    }
                    subset_len_expr <- if (!is.null(lhs_len_expr)) lhs_len_expr else call("length", as.name(arr_name))
                    prior_vec_info <- .mojor_state$current_local_vector_lengths[[lhs]]
                    alloc_len_expr <- subset_len_expr
                    if (!is.null(prior_vec_info) && identical(prior_vec_info$origin, "subset")) {
                        prev_alloc_len <- prior_vec_info$length_alloc
                        if (is.null(prev_alloc_len)) {
                            prev_alloc_len <- prior_vec_info$length
                        }
                        if (!is.null(prev_alloc_len)) {
                            alloc_len_expr <- call("max", prev_alloc_len, subset_len_expr)
                        }
                    }
                    .mojor_state$current_local_vector_lengths[[lhs]] <- list(
                        length = alloc_len_expr,
                        length_alloc = alloc_len_expr,
                        length_runtime = subset_len_expr,
                        type = vec_spec,
                        origin = "subset"
                    )
                }
                if (!isTRUE(idx_info$missing) &&
                  (is.integer(idx) ||
                  is.numeric(idx)) &&
                  length(idx) ==
                    1 && as.integer(idx) ==
                  1) {
                  if (!is.null(spec) &&
                    .mojor_is_array(spec)) {
                    scalar_type <- if (spec %in% c("i32[]"))
                      "i32" else if (spec %in% c("lgl[]", "bool[]"))
                      "lgl" else "f64"
                    if (is.null(scalar_inits[[lhs]])) {
                      scalar_inits[[lhs]] <- list(type = scalar_type, value = paste0(arr_name, "[0]"))
                    }
                  }
                }
            } else if (is.call(rhs) &&
                as.character(rhs[[1]]) %in%
                  c("nrow", "ncol") &&
                length(rhs) ==
                  2 && is.name(rhs[[2]])) {
 # PR-B3: Shape helpers - nrow/ncol
                arr_name <- .resolve_array_alias_target(as.character(rhs[[2]]))
                spec <- if (!is.null(arr_name))
                    types[[arr_name]] else NULL
                if (!is.null(spec) &&
                  .mojor_is_array(spec)) {
                  is_nrow <- as.character(rhs[[1]]) ==
                    "nrow"
                  scalar_inits[[lhs]] <- list(
                    type = "i32", value = list(kind = "dim", name = arr_name, dim = if (is_nrow) 1L else 2L)
                )
                }
            } else if (is.call(rhs) &&
                as.character(rhs[[1]]) ==
                  "dim" && length(rhs) ==
                2 && is.name(rhs[[2]])) {
 # dim(x)[k] pattern handled separately below
            } else if (is.call(rhs) &&
                as.character(rhs[[1]]) ==
                  "[" && length(rhs) >=
                3) {
 # Check for dim(x)[k] pattern
                base <- rhs[[2]]
                if (is.call(base) &&
                  as.character(base[[1]]) ==
                    "dim" && length(base) ==
                  2 && is.name(base[[2]])) {
                  arr_name <- .resolve_array_alias_target(as.character(base[[2]]))
                  spec <- if (!is.null(arr_name))
                    types[[arr_name]] else NULL
                  idx_info <- .extract_selector_arg(rhs, 3L)
                  if (isTRUE(idx_info$missing)) {
                    idx <- NULL
                  } else {
                    idx <- idx_info$value
                  }
                  if (!is.null(spec) &&
                    .mojor_is_array(spec) &&
                    !is.null(idx)) {
                    if ((is.numeric(idx) || is.integer(idx)) && length(idx) == 1) {
                      dim_idx <- as.integer(idx)
                      if (dim_idx >= 1 && dim_idx <= 9) {
                        scalar_inits[[lhs]] <- list(
                          type = "i32", value = list(kind = "dim", name = arr_name, dim = dim_idx)
                        )
                      }
                    } else if (.is_i32_scalar_expr(idx)) {
                      scalar_inits[[lhs]] <- list(
                        type = "i32", value = list(kind = "expr", expr = rhs)
                      )
                    }
                  }
                }
            } else if (is.call(rhs) &&
                as.character(rhs[[1]]) ==
                  "matrix") {
                parsed <- .mojor_parse_matrix_alloc(rhs, args, types)
                out_name <- lhs
                out_type <- parsed$type
                out_matrix <- TRUE
                out_array <- FALSE
                out_nrow_expr <- parsed$nrow
                out_ncol_expr <- parsed$ncol
 # Step 5.1: Track local matrix allocations (not
 # function parameters)
                if (!out_name %in% args) {
                  if (is.null(.mojor_state$current_local_matrix_dims)) {
                    .mojor_state$current_local_matrix_dims <- list()
                  }
                  .mojor_state$current_local_matrix_dims[[out_name]] <- list(
                    nrow = parsed$nrow, ncol = parsed$ncol, type = out_type,
                    data = parsed$data, byrow = isTRUE(parsed$byrow)
                )
                }
                if (!is.null(parsed$dimnames)) {
                  if (is.null(.mojor_state$matrix_dimnames)) {
                    .mojor_state$matrix_dimnames <- list()
                  }
                  .mojor_state$matrix_dimnames[[out_name]] <- parsed$dimnames
                }
            } else if (is.call(rhs) &&
                as.character(rhs[[1]]) ==
                  "array") {
                parsed <- .mojor_parse_array_alloc(rhs, args, types)
                out_name <- lhs
                out_type <- parsed$type
                out_matrix <- FALSE
                out_array <- TRUE
                dim_info <- .mojor_dim_list_to_c(parsed$dim, args, types)
                out_dim_exprs <- dim_info$exprs
                out_dim_name <- dim_info$dim_name
 # Step 5.1 Stage C: Track local array allocations
 # (not function parameters)
                if (!out_name %in% args && !is.null(parsed$dim)) {
                  if (is.null(.mojor_state$current_local_array_dims)) {
                    .mojor_state$current_local_array_dims <- list()
                  }
                  .mojor_state$current_local_array_dims[[out_name]] <- list(dim = out_dim_exprs, type = out_type)
                }
                if (!is.null(parsed$dimnames)) {
                  if (is.null(.mojor_state$matrix_dimnames)) {
                    .mojor_state$matrix_dimnames <- list()
                  }
                  .mojor_state$matrix_dimnames[[out_name]] <- parsed$dimnames
                }
            } else if (is.call(rhs) &&
                as.character(rhs[[1]]) ==
                  "gpu_matmul") {
                matmul_dims <- .mojor_parse_gpu_matmul_dims(rhs)
                if (!is.null(matmul_dims)) {
                  if (is.null(out_name))
                    out_name <- lhs
                  if (!is.null(out_name) &&
                    identical(lhs, out_name)) {
                    out_matrix <- TRUE
                    out_array <- FALSE
                    out_nrow_expr <- matmul_dims$out_nrow_expr
                    out_ncol_expr <- matmul_dims$out_ncol_expr
                    if (is.null(out_type) &&
                      is.name(rhs[[2]])) {
                      a_name <- as.character(rhs[[2]])
                      a_spec <- types[[a_name]]
                      if (!is.null(a_spec) &&
                        grepl("\\[,\\]$", a_spec)) {
                        out_type <- a_spec
                      }
                    }
                  }
                }
            } else if (is.call(rhs)) {
                rhs_ir_info <- .mojor_infer_rhs_ir_assignment(rhs)
                rhs_handled <- FALSE
                if (!is.null(rhs_ir_info) &&
                    rhs_ir_info$kind %in% .mojor_vector_ir_kinds_supported &&
                    rhs_ir_info$type %in% c(
                        "f64[]", "f32[]", "i32[]", "lgl[]", "chr[]", "df",
                        "f64[,]", "f32[,]", "i32[,]", "lgl[,]", "i32[2d]"
                    ) &&
                    isTRUE(.mojor_ir_vector_assignment_supported(rhs_ir_info))) {
                    out_name <- lhs
                    out_type <- rhs_ir_info$type
                    out_matrix <- isTRUE(.mojor_is_matrix(out_type))
                    out_array <- isTRUE(!isTRUE(out_matrix) && !is.na(.mojor_type_ndim(out_type)) && .mojor_type_ndim(out_type) >= 3L)
                    rhs_out_len <- .mojor_rhs_out_len_source_from_ir(rhs_ir_info)
                    if (is.list(rhs_out_len) && !is.null(rhs_out_len$kind)) {
                        out_len_source <- rhs_out_len
                    }
                    rhs_handled <- TRUE
                } else if (!is.null(rhs_ir_info) &&
                    rhs_ir_info$kind %in% .mojor_scalar_ir_kinds_supported &&
                    rhs_ir_info$type %in% c("f64", "f32", "i32", "lgl")) {
                    if (is.null(scalar_inits[[lhs]])) {
                        scalar_inits[[lhs]] <- list(
                            type = rhs_ir_info$type,
                            value = list(kind = "expr", expr = rhs)
                        )
                    }
                    rhs_handled <- TRUE
                }
                if (!isTRUE(rhs_handled)) {
                    len_call <- NULL
                    delta <- 0L
                    op_rhs <- as.character(rhs[[1]])
                    if (op_rhs == "length" && length(rhs) ==
                      2) {
                      len_call <- rhs
                    } else if (op_rhs == "as.integer" && length(rhs) ==
                      2) {
                      inner <- rhs[[2]]
                      if (is.call(inner) &&
                        as.character(inner[[1]]) ==
                          "length" && length(inner) ==
                        2) {
                        len_call <- inner
                      } else if (is.call(inner) &&
                        as.character(inner[[1]]) %in%
                          c("+", "-") &&
                        length(inner) ==
                          3) {
                        lhs_inner <- inner[[2]]
                        rhs_inner <- inner[[3]]
                        if (is.call(lhs_inner) &&
                          as.character(lhs_inner[[1]]) ==
                            "length" && length(lhs_inner) ==
                          2 && (is.numeric(rhs_inner) ||
                          is.integer(rhs_inner)) &&
                          length(rhs_inner) ==
                            1) {
                          len_call <- lhs_inner
                          delta <- if (as.character(inner[[1]]) ==
                            "+")
                            as.integer(rhs_inner) else -as.integer(rhs_inner)
                        } else if (as.character(inner[[1]]) ==
                          "+" && is.call(rhs_inner) &&
                          as.character(rhs_inner[[1]]) ==
                            "length" && length(rhs_inner) ==
                          2 && (is.numeric(lhs_inner) ||
                          is.integer(lhs_inner)) &&
                          length(lhs_inner) ==
                            1) {
                          len_call <- rhs_inner
                          delta <- as.integer(lhs_inner)
                        }
                      }
                    } else if (op_rhs == "-" && length(rhs) ==
                      3) {
                      lhs_rhs <- rhs[[2]]
                      rhs_rhs <- rhs[[3]]
                      if (is.numeric(rhs_rhs) &&
                        length(rhs_rhs) ==
                          1 && as.integer(rhs_rhs) ==
                        1L) {
                        if (is.call(lhs_rhs) &&
                          as.character(lhs_rhs[[1]]) ==
                            "length" && length(lhs_rhs) ==
                          2) {
                          len_call <- lhs_rhs
                          delta <- -1L
                        } else if (is.call(lhs_rhs) &&
                          as.character(lhs_rhs[[1]]) ==
                            "as.integer" && length(lhs_rhs) ==
                          2) {
                          inner <- lhs_rhs[[2]]
                          if (is.call(inner) &&
                            as.character(inner[[1]]) ==
                              "length" && length(inner) ==
                            2) {
                            len_call <- inner
                            delta <- -1L
                          }
                        }
                      }
                    } else if (op_rhs %in% c("+", "-") &&
                      length(rhs) ==
                        3) {
                      lhs_rhs <- rhs[[2]]
                      rhs_rhs <- rhs[[3]]
                      if (is.call(lhs_rhs) &&
                        as.character(lhs_rhs[[1]]) ==
                          "length" && length(lhs_rhs) ==
                        2 && (is.numeric(rhs_rhs) ||
                        is.integer(rhs_rhs)) &&
                        length(rhs_rhs) ==
                          1) {
                        len_call <- lhs_rhs
                        delta <- if (op_rhs == "+")
                          as.integer(rhs_rhs) else -as.integer(rhs_rhs)
                      } else if (op_rhs == "+" && is.call(rhs_rhs) &&
                        as.character(rhs_rhs[[1]]) ==
                          "length" && length(rhs_rhs) ==
                        2 && (is.numeric(lhs_rhs) ||
                        is.integer(lhs_rhs)) &&
                        length(lhs_rhs) ==
                          1) {
                        len_call <- rhs_rhs
                        delta <- as.integer(lhs_rhs)
                      }
                    }
                    if (!is.null(len_call)) {
                      arg <- len_call[[2]]
                      if (is.name(arg)) {
                        len_arg_name <- .resolve_array_alias_target(as.character(arg))
                        spec <- if (!is.null(len_arg_name))
                          types[[len_arg_name]] else NULL
                        if (!is.null(spec) &&
                          .mojor_is_array(spec)) {
                          scalar_inits[[lhs]] <- list(
                            type = "i32", value = list(kind = "len", name = len_arg_name, delta = delta)
                        )
                        }
                      }
                    }
                    if (is.null(scalar_inits[[lhs]]) &&
                        .is_i32_scalar_expr(rhs)) {
                      scalar_inits[[lhs]] <- list(
                        type = "i32",
                        value = list(kind = "expr", expr = rhs)
                      )
                    }
     # Track scalar assignments from function arguments
     # (e.g., N <- n where n is i32 arg)
                    if (is.name(rhs)) {
                      rhs_name <- as.character(rhs)
                      if (rhs_name %in% args) {
                        rhs_spec <- types[[rhs_name]]
                        if (!is.null(rhs_spec) &&
                          !.mojor_is_array(rhs_spec)) {
                          scalar_inits[[lhs]] <- list(type = rhs_spec, value = list(kind = "arg", name = rhs_name))
                        }
                      }
                    }
                }
            }
        }
        if (is.call(b) &&
            as.character(b[[1]]) ==
                "for") {
            loops[[length(loops) +
                1]] <- b
        }
        if (is.call(b) &&
            as.character(b[[1]]) ==
                "while") {
            loops[[length(loops) +
                1]] <- b
        }
        if (is.call(b) &&
            as.character(b[[1]]) ==
                "repeat") {
            loops[[length(loops) +
                1]] <- b
        }
 # Scan loop bodies for local array allocations (hoist to pre-loop scope)
        if (is.call(b) && as.character(b[[1]]) %in% c("for", "while", "repeat")) {
            loop_body_stmts <- NULL
            if (as.character(b[[1]]) == "for" && length(b) >= 4) {
                loop_body_stmts <- .mojor_extract_block(b[[4]])
            } else if (as.character(b[[1]]) == "while" && length(b) >= 3) {
                loop_body_stmts <- .mojor_extract_block(b[[3]])
            } else if (as.character(b[[1]]) == "repeat" && length(b) >= 2) {
                loop_body_stmts <- .mojor_extract_block(b[[2]])
            }
            if (!is.null(loop_body_stmts)) {
                .mojor_scan_loop_body_allocs(loop_body_stmts, args, types, scalar_inits)
            }
        }
        if (is.call(b) &&
            as.character(b[[1]]) ==
                "return") {
            if (length(b) <
                2)
                stop("mojor_transpile: return() must include a value")
            if (is.name(b[[2]])) {
                return_name <- as.character(b[[2]])
            } else if (is.call(b[[2]]) &&
                as.character(b[[2]][[1]]) ==
                  "/") {
                return_expr <- b[[2]]
            } else if (is.call(b[[2]]) &&
                as.character(b[[2]][[1]]) ==
                  "gpu_matmul") {
                if (is.null(out_name) ||
                  !nzchar(out_name)) {
                  out_name <- .fresh_temp_out_name()
                }
                return_name <- out_name
                matmul_dims <- .mojor_parse_gpu_matmul_dims(b[[2]])
                if (!is.null(matmul_dims)) {
                  out_matrix <- TRUE
                  out_array <- FALSE
                  out_nrow_expr <- matmul_dims$out_nrow_expr
                  out_ncol_expr <- matmul_dims$out_ncol_expr
                  if (is.null(out_type) &&
                    is.name(b[[2]][[2]])) {
                    a_name <- as.character(b[[2]][[2]])
                    a_spec <- types[[a_name]]
                    if (!is.null(a_spec) &&
                      grepl("\\[,\\]$", a_spec)) {
                      out_type <- a_spec
                    }
                  }
                }
            } else if (is.call(b[[2]]) &&
                as.character(b[[2]][[1]]) %in%
                  c("any", "all")) {
                any_all_call <- b[[2]]
            } else if (is.call(b[[2]]) &&
                as.character(b[[2]][[1]]) ==
                  "ifelse") {
 # Handled by expression-only vectorized path
            } else if (is.call(b[[2]]) &&
                as.character(b[[2]][[1]]) %in%
                  c("sum", "prod", "min", "max", "which.min", "which.max")) {
 # Handled below as scalar_reduction_op
            } else if (!is.null(.mojor_inline_let_bindings(blocks))) {
# Handled by expression-only vectorized path via let-binding inlining
            } else {
                stop("mojor_transpile: return() must return a name or acc / <i32-expr>")
            }
        }
    }
    if (is.null(return_name) &&
        length(blocks) > 0L) {
        last_block <- blocks[[length(blocks)]]
        if (is.name(last_block)) {
            return_name <- as.character(last_block)
        }
    }
    if (!is.null(return_name) &&
        return_name %in% names(local_array_specs) &&
        (is.null(out_name) || !identical(out_name, return_name))) {
        out_name <- return_name
        out_type <- local_array_specs[[return_name]]
        out_matrix <- FALSE
        out_array <- FALSE
        out_nrow_expr <- NULL
        out_ncol_expr <- NULL
        out_dim_exprs <- NULL
        out_dim_name <- NULL
        ret_len_expr <- local_array_len_exprs[[return_name]]
        if (!is.null(ret_len_expr)) {
            out_len_source <- list(kind = "expr", expr = ret_len_expr)
        }
        ret_rhs <- name_assign_rhs[[return_name]]
        if (is.call(ret_rhs) &&
            as.character(ret_rhs[[1]]) == "[" &&
            length(ret_rhs) == 3 &&
            is.name(ret_rhs[[2]])) {
            base_name <- as.character(ret_rhs[[2]])
            idx_info <- .extract_selector_arg(ret_rhs, 3L)
            if (!isTRUE(idx_info$missing)) {
                idx <- idx_info$value
                if (is.name(idx)) {
                    idx_name <- as.character(idx)
                    idx_spec <- types[[idx_name]]
                    if (!is.null(idx_spec) && .mojor_is_logical_mask_type(idx_spec)) {
                        out_len_source <- list(kind = "mask_true", name = idx_name)
                    }
                }
            }
            if (is.null(out_len_source) && !isTRUE(idx_info$missing)) {
                idx <- idx_info$value
                neg_info <- .mojor_ir_detect_negative_index(idx)
                if (isTRUE(neg_info$is_negative) && !isTRUE(neg_info$is_vector)) {
                    out_len_source <- list(
                        kind = "exclusion",
                        base = base_name,
                        neg_value = neg_info$value,
                        is_dynamic = isTRUE(neg_info$is_dynamic)
                    )
                } else if (isTRUE(neg_info$is_negative) && isTRUE(neg_info$is_vector)) {
                    vec_excl <- .mojor_ir_build_vector_exclusion_index(neg_info)
                    if (!is.null(vec_excl)) {
                        out_len_source <- list(
                            kind = "exclusion",
                            base = base_name,
                            neg_value = neg_info$value,
                            is_dynamic = FALSE,
                            excl_count = vec_excl$neg_vec_exclusion$count
                        )
                    }
                }
            }
        }
    }
    modified_args <- .collect_modified_args(blocks)
    if ((is.null(out_name) || !nzchar(out_name)) &&
        !is.null(return_name) &&
        return_name %in% modified_args) {
        ret_spec <- types[[return_name]]
        if (!is.null(ret_spec) &&
            .mojor_is_array(ret_spec) &&
            !.mojor_is_matrix(ret_spec) &&
            .mojor_type_ndim(ret_spec) <= 1L) {
            out_name <- return_name
            out_type <- ret_spec
        }
    }
    .mojor_validate_negative_write_targets(
        blocks,
        out_name = out_name,
        return_name = return_name,
        modified_args = modified_args
    )

    list(
        out_name = out_name, out_type = out_type, out_matrix = out_matrix,
        out_nrow_expr = out_nrow_expr, out_ncol_expr = out_ncol_expr, out_array = out_array,
        out_dim_exprs = out_dim_exprs, out_dim_name = out_dim_name, out_len_source = out_len_source,
        loops = loops, scalar_inits = scalar_inits, array_aliases = array_aliases,
        pre_loop_out_assigns = pre_loop_out_assigns,
        scalar_init = scalar_init, scalar_type = scalar_type, scalar_init_value = scalar_init_value,
        scalar_reduction_op = scalar_reduction_op, scalar_reduction_arg = scalar_reduction_arg,
        scalar_reduce_ir_node = scalar_reduce_ir_node, return_name = return_name,
        return_expr = return_expr, return_div = return_div, mean_ignore_pending = mean_ignore_pending,
        mean_ignore_expr = mean_ignore_expr, mean_ignore_loop_var = mean_ignore_loop_var,
        any_all_call = any_all_call, any_all_arg = any_all_arg, any_all_mode = any_all_mode,
        seq_extra = seq_extra, constructor_len_arrays = constructor_len_arrays,
        modified_args = modified_args
    )
}

.mojor_collect_pre_loop_out_assignments <- function(
    blocks, out_name, scalar_inits, args = character(0), types = list(),
    array_aliases = list()
) {
    pre_loop_out_assigns <- list()
    resolve_array_alias_target <- function(name) {
        .mojor_resolve_array_source_name(name, args, types, alias_map = array_aliases)
    }

    is_loop_stmt <- function(stmt) {
        is.call(stmt) &&
            as.character(stmt[[1]]) %in%
                c("for", "while", "repeat")
    }
    has_loops <- any(vapply(blocks, is_loop_stmt, logical(1)))
    selector_has_negative <- function(idx_expr) {
        if (is.null(idx_expr)) return(FALSE)
        if (is.symbol(idx_expr)) {
            return(FALSE)
        }
        if (is.numeric(idx_expr) || is.integer(idx_expr)) {
            return(length(idx_expr) >= 1L && any(!is.na(idx_expr) & idx_expr < 0))
        }
        if (!is.call(idx_expr)) return(FALSE)
        op <- as.character(idx_expr[[1]])[1L]
        if (identical(op, "(") || identical(op, "+")) {
            if (length(idx_expr) >= 2L) return(selector_has_negative(idx_expr[[2L]]))
            return(FALSE)
        }
        if (identical(op, "-")) {
            return(TRUE)
        }
        if (identical(op, "c")) {
            parts <- as.list(idx_expr)[-1L]
            if (length(parts) == 0L) return(FALSE)
            return(any(vapply(parts, selector_has_negative, logical(1))))
        }
        if (identical(op, ":")) {
            parts <- as.list(idx_expr)[-1L]
            if (length(parts) == 0L) return(FALSE)
            return(any(vapply(parts, selector_has_negative, logical(1))))
        }
        FALSE
    }
    indexed_lhs_is_supported <- function(lhs_expr) {
        if (!(is.call(lhs_expr) &&
            as.character(lhs_expr[[1]]) == "[" &&
            length(lhs_expr) >= 3L &&
            is.name(lhs_expr[[2L]]))) {
            return(FALSE)
        }
        base_name <- as.character(lhs_expr[[2L]])
        if (!is.null(out_name) && nzchar(out_name) && identical(base_name, out_name)) {
            return(TRUE)
        }
        if (length(lhs_expr) <= 2L) return(TRUE)
        idx_args <- as.list(lhs_expr)[-c(1L, 2L)]
        if (length(idx_args) == 0L) return(TRUE)
        !any(vapply(idx_args, selector_has_negative, logical(1)))
    }
    is_preloop_assign <- function(stmt) {
        if (!is.call(stmt) ||
            !(as.character(stmt[[1]]) %in%
                c("<-", "="))) {
            return(FALSE)
        }
        lhs_expr <- stmt[[2]]
        if (is.name(lhs_expr)) {
            return(TRUE)
        }
        if (indexed_lhs_is_supported(lhs_expr)) {
            return(TRUE)
        }
        FALSE
    }
    strict_ir <- isTRUE(tryCatch(.mojor_state$options$ir_only, error = function(e) FALSE))
    is_ignorable_stmt <- function(stmt) {
        if (!is.call(stmt) ||
            !(as.character(stmt[[1]]) %in%
                c("<-", "="))) {
            return(FALSE)
        }
        lhs_expr <- stmt[[2]]
        rhs <- stmt[[3]]
        if (is.name(lhs_expr)) {
            lhs <- as.character(lhs_expr)
            # Check if this is a seq alias assignment (v <- seq_len(n))
            if (is.call(rhs)) {
                rhs_op <- as.character(rhs[[1]])[1L]
                if (rhs_op %in% c("seq_len", "seq_along", "seq", "seq.int")) {
                    return(TRUE)
                }
            }
            if (identical(lhs, out_name)) {
                if (is.name(rhs)) {
                    rhs_name <- as.character(rhs)
                    rhs_alias <- resolve_array_alias_target(rhs_name)
                    rhs_spec <- if (!is.null(rhs_alias)) types[[rhs_alias]] else NULL
                    if (!is.null(rhs_spec) && .mojor_is_array(rhs_spec)) {
                        # out <- mat alias init is handled via prelude copy loops.
                        # Emitting this as a top-level IR assign can lower to an invalid
                        # scalar-style cast for matrix/ND aliases (e.g. f64[2d](mat)).
                        return(TRUE)
                    }
                }
                if (is.call(rhs) &&
                  length(rhs) >=
                    1) {
                  op_rhs <- as.character(rhs[[1]])
                  if (length(op_rhs) ==
                    1 && op_rhs %in% c("numeric", "integer", "logical", "matrix", "array")) {
                    return(isTRUE(has_loops))
                  }
                }
                if (is.call(rhs) &&
                  length(rhs) ==
                    2 && is.call(rhs[[1]]) &&
                  as.character(rhs[[1]][[1]]) ==
                    "::" && as.character(rhs[[1]][[2]]) ==
                  "float" && as.character(rhs[[1]][[3]]) ==
                  "float32") {
                  return(isTRUE(has_loops))
                }
            }
            if (!is.null(scalar_inits[[lhs]])) {
                init_info <- scalar_inits[[lhs]]
                if (!is.call(rhs)) {
                    return(TRUE)
                }
                if (is.list(init_info$value) &&
                    identical(init_info$value$kind, "expr") &&
                    !is.null(init_info$value$expr) &&
                    identical(init_info$value$expr, rhs)) {
                    return(TRUE)
                }
                if (is.list(init_info$value) &&
                    identical(init_info$value$kind, "dim") &&
                    identical(rhs, call(
                        if (isTRUE(init_info$value$dim == 1L)) "nrow" else "ncol",
                        as.name(init_info$value$name)
                    ))) {
                    return(TRUE)
                }
                return(FALSE)
            }
        }
        FALSE
    }
    is_return_gpu_matmul_stmt <- function(stmt) {
        if (is.null(out_name) ||
            !nzchar(out_name)) {
            return(FALSE)
        }
        if (!is.call(stmt) ||
            !identical(
                as.character(stmt[[1]]),
                "return"
            ) ||
            length(stmt) <
                2) {
            return(FALSE)
        }
        ret_expr <- stmt[[2]]
        is.call(ret_expr) &&
            identical(
                as.character(ret_expr[[1]]),
                "gpu_matmul"
            )
    }
    is_preloop_stmt <- function(stmt) {
        if (is_preloop_assign(stmt)) {
            if (is_ignorable_stmt(stmt)) {
                return(FALSE)
            }
            return(TRUE)
        }
        if (is.call(stmt) &&
            as.character(stmt[[1]]) ==
                "if") {
            then_blocks <- .mojor_extract_block(stmt[[3]])
            if (length(then_blocks) ==
                0) {
                return(FALSE)
            }
            else_blocks <- if (length(stmt) >=
                4)
                .mojor_extract_block(stmt[[4]]) else NULL
            if (!is.null(else_blocks) &&
                length(else_blocks) ==
                  0) {
                return(FALSE)
            }
            if (any(vapply(then_blocks, is_loop_stmt, logical(1)))) {
                return(FALSE)
            }
            if (!is.null(else_blocks) &&
                any(vapply(else_blocks, is_loop_stmt, logical(1)))) {
                return(FALSE)
            }
            ok_then <- all(vapply(then_blocks, is_preloop_stmt, logical(1)))
            ok_else <- is.null(else_blocks) ||
                all(vapply(else_blocks, is_preloop_stmt, logical(1)))
            return(ok_then && ok_else)
        }
        FALSE
    }

    return_gpu_matmul_expr <- function(stmt) {
        if (!is.call(stmt) ||
            !identical(
                as.character(stmt[[1]]),
                "return"
            ) ||
            length(stmt) <
                2) {
            return(NULL)
        }
        expr <- stmt[[2]]
        if (!is.call(expr) ||
            !identical(
                as.character(expr[[1]]),
                "gpu_matmul"
            )) {
            return(NULL)
        }
        expr
    }

    for (b in blocks) {
        if (is_loop_stmt(b))
            break
        if (is_return_gpu_matmul_stmt(b)) {
            pre_loop_out_assigns[[length(pre_loop_out_assigns) +
                1L]] <- call(
                "<-", as.name(out_name),
                b[[2]]
            )
            break
        }
        if (is_preloop_stmt(b)) {
            pre_loop_out_assigns[[length(pre_loop_out_assigns) +
                1L]] <- b
            next
        }
        if (is_ignorable_stmt(b)) {
            next
        }
        ret_mm <- return_gpu_matmul_expr(b)
        if (!is.null(ret_mm) &&
            !is.null(out_name) &&
            nzchar(out_name)) {
            pre_loop_out_assigns[[length(pre_loop_out_assigns) +
                1L]] <- call(
                "<-", as.name(out_name),
                ret_mm
            )
        }
        break
    }

    pre_loop_out_assigns
}

.mojor_collect_post_loop_out_assignments <- function(
    blocks, out_name, scalar_inits, args = character(0), types = list(),
    array_aliases = list()
) {
    post_loop_out_assigns <- list()
    resolve_array_alias_target <- function(name) {
        .mojor_resolve_array_source_name(name, args, types, alias_map = array_aliases)
    }

    is_loop_stmt <- function(stmt) {
        is.call(stmt) &&
            as.character(stmt[[1]]) %in%
                c("for", "while", "repeat")
    }
    selector_has_negative <- function(idx_expr) {
        if (is.null(idx_expr)) return(FALSE)
        if (is.symbol(idx_expr)) {
            return(FALSE)
        }
        if (is.numeric(idx_expr) || is.integer(idx_expr)) {
            return(length(idx_expr) >= 1L && any(!is.na(idx_expr) & idx_expr < 0))
        }
        if (!is.call(idx_expr)) return(FALSE)
        op <- as.character(idx_expr[[1]])[1L]
        if (identical(op, "(") || identical(op, "+")) {
            if (length(idx_expr) >= 2L) return(selector_has_negative(idx_expr[[2L]]))
            return(FALSE)
        }
        if (identical(op, "-")) {
            return(TRUE)
        }
        if (identical(op, "c")) {
            parts <- as.list(idx_expr)[-1L]
            if (length(parts) == 0L) return(FALSE)
            return(any(vapply(parts, selector_has_negative, logical(1))))
        }
        if (identical(op, ":")) {
            parts <- as.list(idx_expr)[-1L]
            if (length(parts) == 0L) return(FALSE)
            return(any(vapply(parts, selector_has_negative, logical(1))))
        }
        FALSE
    }
    indexed_lhs_is_supported <- function(lhs_expr) {
        if (!(is.call(lhs_expr) &&
            as.character(lhs_expr[[1]]) == "[" &&
            length(lhs_expr) >= 3L &&
            is.name(lhs_expr[[2L]]))) {
            return(FALSE)
        }
        base_name <- as.character(lhs_expr[[2L]])
        if (!is.null(out_name) && nzchar(out_name) && identical(base_name, out_name)) {
            return(TRUE)
        }
        if (length(lhs_expr) <= 2L) return(TRUE)
        idx_args <- as.list(lhs_expr)[-c(1L, 2L)]
        if (length(idx_args) == 0L) return(TRUE)
        !any(vapply(idx_args, selector_has_negative, logical(1)))
    }
    is_postloop_assign <- function(stmt) {
        if (!is.call(stmt) ||
            !(as.character(stmt[[1]]) %in%
                c("<-", "="))) {
            return(FALSE)
        }
        lhs_expr <- stmt[[2]]
        if (is.name(lhs_expr)) {
            return(TRUE)
        }
        if (indexed_lhs_is_supported(lhs_expr)) {
            return(TRUE)
        }
        FALSE
    }
    is_ignorable_stmt <- function(stmt) {
        if (!is.call(stmt) ||
            !(as.character(stmt[[1]]) %in%
                c("<-", "="))) {
            return(FALSE)
        }
        lhs_expr <- stmt[[2]]
        rhs <- stmt[[3]]
        if (!is.name(lhs_expr)) {
            return(FALSE)
        }
        lhs <- as.character(lhs_expr)
        if (!identical(lhs, out_name)) {
            return(FALSE)
        }
        if (is.name(rhs)) {
            rhs_name <- as.character(rhs)
            rhs_alias <- resolve_array_alias_target(rhs_name)
            rhs_spec <- if (!is.null(rhs_alias)) types[[rhs_alias]] else NULL
            if (!is.null(rhs_spec) && .mojor_is_array(rhs_spec)) {
                return(TRUE)
            }
        }
        if (is.call(rhs) &&
            length(rhs) >=
                1) {
            op_rhs <- as.character(rhs[[1]])
            if (length(op_rhs) ==
                1 && op_rhs %in% c("numeric", "integer", "logical", "matrix", "array")) {
                return(TRUE)
            }
        }
        if (is.call(rhs) &&
            length(rhs) ==
                2 && is.call(rhs[[1]]) &&
            as.character(rhs[[1]][[1]]) ==
            "::" && as.character(rhs[[1]][[2]]) ==
            "float" && as.character(rhs[[1]][[3]]) ==
            "float32") {
            return(TRUE)
        }
        FALSE
    }
    is_postloop_stmt <- function(stmt) {
        if (is_postloop_assign(stmt)) {
            if (is_ignorable_stmt(stmt)) {
                return(FALSE)
            }
            return(TRUE)
        }
        if (is.call(stmt) &&
            as.character(stmt[[1]]) ==
                "if") {
            then_blocks <- .mojor_extract_block(stmt[[3]])
            if (length(then_blocks) ==
                0) {
                return(FALSE)
            }
            else_blocks <- if (length(stmt) >=
                4)
                .mojor_extract_block(stmt[[4]]) else NULL
            if (!is.null(else_blocks) &&
                length(else_blocks) ==
                  0) {
                return(FALSE)
            }
            if (any(vapply(then_blocks, is_loop_stmt, logical(1)))) {
                return(FALSE)
            }
            if (!is.null(else_blocks) &&
                any(vapply(else_blocks, is_loop_stmt, logical(1)))) {
                return(FALSE)
            }
            ok_then <- all(vapply(then_blocks, is_postloop_stmt, logical(1)))
            ok_else <- is.null(else_blocks) ||
                all(vapply(else_blocks, is_postloop_stmt, logical(1)))
            return(ok_then && ok_else)
        }
        FALSE
    }

    loop_idxs <- which(vapply(blocks, is_loop_stmt, logical(1)))
    if (length(loop_idxs) ==
        0) {
        return(post_loop_out_assigns)
    }
    start_idx <- max(loop_idxs) + 1L
    if (start_idx >
        length(blocks)) {
        return(post_loop_out_assigns)
    }

    for (idx in seq.int(start_idx, length(blocks))) {
        b <- blocks[[idx]]
        if (is_postloop_stmt(b)) {
            post_loop_out_assigns[[length(post_loop_out_assigns) +
                1L]] <- b
            next
        }
        if (is_ignorable_stmt(b)) {
            next
        }
        break
    }

    post_loop_out_assigns
}

.mojor_resolve_no_loop_synthesis <- function(
    blocks, loops, args, types, any_all_call, any_all_arg, any_all_mode,
    seq_extra, scalar_init, scalar_type, scalar_init_value, scalar_inits,
    return_name, scalar_reduction_op, scalar_reduction_arg, scalar_reduce_ir_node,
    out_name, out_type, out_matrix, out_array, out_dim_exprs, out_dim_name,
    constructor_len_arrays, na_rm = FALSE
) {
    no_loop_scalar_any_all <- FALSE
    any_all_na_rm <- FALSE

    if (length(loops) ==
        0 && length(blocks) ==
        1) {
        b <- blocks[[1]]
        if (is.call(b) &&
            as.character(b[[1]]) %in%
                c("any", "all")) {
            any_all_call <- b
        } else if (is.call(b) &&
            as.character(b[[1]]) ==
                "return" && length(b) >=
            2 && is.call(b[[2]]) &&
            as.character(b[[2]][[1]]) %in%
                c("any", "all")) {
            any_all_call <- b[[2]]
        }
        if (!is.null(any_all_call)) {
            any_all_mode <- as.character(any_all_call[[1]])
            any_all_args <- as.list(any_all_call)[-1]
            arg_names <- names(any_all_args)
            if (is.null(arg_names))
                arg_names <- rep("", length(any_all_args))
            positional <- list()
            for (k in seq_along(any_all_args)) {
                nm <- arg_names[[k]]
                val <- any_all_args[[k]]
                has_name <- is.character(nm) &&
                  length(nm) ==
                    1 && nzchar(nm)
                if (has_name && identical(nm, "na.rm")) {
                  if (!(is.logical(val) &&
                    length(val) ==
                      1 && !is.na(val))) {
                    .mojor_err(
                      "any()/all() na.rm must be TRUE or FALSE", any_all_call,
                      "use a scalar logical value"
                  )
                  }
                  any_all_na_rm <- isTRUE(val)
                  next
                }
                positional[[length(positional) +
                  1]] <- val
            }
            if (length(positional) ==
                0) {
                any_all_arg <- if (identical(any_all_mode, "all"))
                  TRUE else FALSE
            } else if (length(positional) ==
                1) {
                any_all_arg <- positional[[1]]
            } else {
                comb_op <- if (identical(any_all_mode, "all"))
                  "&" else "|"
                combined <- positional[[1]]
                for (k in 2:length(positional)) {
                  combined <- call(comb_op, combined, positional[[k]])
                }
                any_all_arg <- combined
            }
        }
    }

    if (length(loops) ==
        0 && !is.null(any_all_call)) {
        loop_var <- "i"
        array_refs <- unique(.mojor_collect_array_args(any_all_arg, types, args))
        scalar_only_any_all <- length(array_refs) ==
            0
        if (scalar_only_any_all) {
            elem_expr <- any_all_arg
        } else {
            any_all_name <- array_refs[[1]]
            if (length(array_refs) >
                1)
                seq_extra <- array_refs[-1]
            elem_expr <- .mojor_vector_expr_to_element(any_all_arg, loop_var, types, args)
            elem_expr <- .mojor_simplify_selector_index_expr(elem_expr)
        }
        elem_expr_raw <- elem_expr
        if (is.call(elem_expr) &&
            as.character(elem_expr[[1]]) ==
                "ifelse") {
            parts <- .mojor_ifelse_parts(elem_expr)
            yes_kind <- .mojor_expr_kind(parts$yes, types)
            no_kind <- .mojor_expr_kind(parts$no, types)
            if (yes_kind == "bool" && no_kind == "bool") {
                elem_expr <- call(
                  "||", call("&&", parts$cond, parts$yes),
                  call(
                    "&&", call("!", parts$cond),
                    parts$no
                )
              )
            } else {
                yes_expr <- if (yes_kind == "bool")
                  parts$yes else call("!=", parts$yes, 0)
                no_expr <- if (no_kind == "bool")
                  parts$no else call("!=", parts$no, 0)
                elem_expr <- call("ifelse", parts$cond, yes_expr, no_expr)
            }
        }
        if (.mojor_expr_kind(elem_expr, types) !=
            "bool") {
            elem_expr <- call("!=", elem_expr, 0)
        }
        na_guard_expr <- NULL
        if (isTRUE(any_all_na_rm)) {
            collect_na_terms <- function(expr) {
                if (is.null(expr) ||
                  !is.call(expr)) {
                  return(list())
                }
                op <- as.character(expr[[1]])
                if (length(op) !=
                  1)
                  op <- op[[1]]
                if (identical(op, "[") &&
                  length(expr) ==
                    3) {
                  return(list(call("is.na", expr)))
                }
                parts <- as.list(expr)[-1]
                out <- list()
                for (p in parts) {
                  out <- c(out, collect_na_terms(p))
                }
                out
            }
            na_terms <- collect_na_terms(elem_expr_raw)
            if (length(na_terms) >
                0) {
                na_guard_expr <- na_terms[[1]]
                if (length(na_terms) >
                  1) {
                  for (k in 2:length(na_terms)) {
                    na_guard_expr <- call("|", na_guard_expr, na_terms[[k]])
                  }
                }
            } else {
 # Scalar-only fallback: preserve na.rm identity semantics.
                identity_val <- if (any_all_mode == "all")
                  TRUE else FALSE
                if ((is.logical(elem_expr) || is.numeric(elem_expr) || is.integer(elem_expr)) &&
                    length(elem_expr) == 1 && !is.na(elem_expr)) {
                    elem_expr <- identity_val && isTRUE(as.logical(elem_expr))
                } else {
                    elem_expr <- call(
                      "ifelse", call("is.na", elem_expr),
                      identity_val, elem_expr
                  )
                }
            }
        }
        folded_scalar_value <- NULL
        if (scalar_only_any_all) {
            if (is.logical(elem_expr) &&
                length(elem_expr) ==
                  1 && !is.na(elem_expr)) {
                folded_scalar_value <- isTRUE(elem_expr)
            } else if ((is.numeric(elem_expr) ||
                is.integer(elem_expr)) &&
                length(elem_expr) ==
                  1 && !is.na(elem_expr)) {
                folded_scalar_value <- as.double(elem_expr) !=
                  0
            }
        }
        if (any_all_mode == "all") {
            cond_base <- call("!", elem_expr)
            scalar_init_value <- 1L
        } else {
            cond_base <- elem_expr
            scalar_init_value <- 0L
        }
        cond <- cond_base
        if (isTRUE(any_all_na_rm) &&
            !is.null(na_guard_expr)) {
            cond <- call(
                "&&", call("!", na_guard_expr),
                cond_base
            )
        }
        scalar_init <- "acc"
        scalar_type <- "lgl"
        scalar_inits[[scalar_init]] <- list(type = scalar_type, value = scalar_init_value)
        return_name <- scalar_init
        if (scalar_only_any_all && !is.null(folded_scalar_value)) {
 # Constant scalar-only any/all can be emitted without
 # loop scaffolding.
            scalar_init_value <- if (isTRUE(folded_scalar_value))
                1L else 0L
            scalar_inits[[scalar_init]] <- list(type = scalar_type, value = scalar_init_value)
            blocks <- list()
            loops <- list()
            no_loop_scalar_any_all <- TRUE
        } else if (scalar_only_any_all) {
 # Non-constant scalar-only any/all can still emit without
 # loop scaffolding: scalar result is the boolean scalar
 # expression itself.
            scalar_init_value <- list(kind = "expr", expr = elem_expr)
            scalar_inits[[scalar_init]] <- list(type = scalar_type, value = scalar_init_value)
            blocks <- list()
            loops <- list()
            no_loop_scalar_any_all <- TRUE
        } else {
            loop_body <- call(
                "if", cond, call(
                  "<-", as.name(scalar_init),
                  if (any_all_mode == "all") FALSE else TRUE
              )
            )
            loops[[1]] <- call(
                "for", as.name(loop_var),
                call("seq_along", as.name(any_all_name)),
                loop_body
            )
 # Keep downstream length/index analysis in loop context.
            blocks <- list(loops[[1]])
        }
    }

    if (length(loops) ==
        0 && length(blocks) ==
        1) {
        b <- blocks[[1]]
        red_call <- NULL
        na_rm <- FALSE
        combine_reduction_args <- function(op, values) {
            if (length(values) == 0) {
                return(NULL)
            }
            out <- values[[1]]
            if (length(values) > 1) {
                for (val in values[-1]) {
                    if (op == "sum") {
                        out <- call("+", out, val)
                    } else if (op == "product") {
                        out <- call("*", out, val)
                    } else if (op == "min") {
                        out <- call("min", out, val)
                    } else if (op == "max") {
                        out <- call("max", out, val)
                    } else {
                        out <- call(op, out, val)
                    }
                }
            }
            out
        }
        if (is.call(b) &&
            as.character(b[[1]]) %in%
                c(
                  "sum", "prod", "mean", "var", "sd", "min", "max", "which.min",
                  "which.max"
              )) {
            red_call <- b
        } else if (is.call(b) &&
            as.character(b[[1]]) ==
                "return" && length(b) >=
            2 && is.call(b[[2]]) &&
            as.character(b[[2]][[1]]) %in%
                c(
                  "sum", "prod", "mean", "var", "sd", "min", "max", "which.min",
                  "which.max"
              )) {
            red_call <- b[[2]]
        }
        if (!is.null(red_call)) {
            scalar_reduction_fn <- as.character(red_call[[1]])
            scalar_reduction_op <- if (identical(scalar_reduction_fn, "prod"))
                "product" else scalar_reduction_fn
            red_args <- as.list(red_call)[-1]
            red_arg_names <- names(red_args)
            if (is.null(red_arg_names))
                red_arg_names <- rep("", length(red_args))
            arg <- NULL
            arg_expr_ast <- NULL
            if (scalar_reduction_fn %in% c("sum", "prod", "mean", "var", "sd", "min", "max")) {
                data_args <- list()
                for (k in seq_along(red_args)) {
                  nm <- red_arg_names[[k]]
                  val <- red_args[[k]]
                  if (is.character(nm) &&
                    length(nm) ==
                      1 && identical(nm, "na.rm")) {
                    if (!(is.logical(val) &&
                      length(val) ==
                        1 && !is.na(val))) {
                      .mojor_err("na.rm must be TRUE or FALSE (or omitted)", red_call)
                    }
                    na_rm <- isTRUE(val)
                    next
                  }
                  data_args[[length(data_args) +
                    1L]] <- val
                }
                if (length(data_args) ==
                  2 && is.logical(data_args[[2]]) &&
                  length(data_args[[2]]) ==
                    1 && !is.na(data_args[[2]])) {
                  na_rm <- isTRUE(data_args[[2]])
                  data_args <- data_args[1]
                }
                if (scalar_reduction_op %in% c("sum", "product", "min", "max")) {
                  if (length(data_args) < 1) {
                    .mojor_err(
                      "sum()/prod()/min()/max() reductions require at least one argument",
                      red_call
                    )
                  }
                  arg <- combine_reduction_args(scalar_reduction_op, data_args)
                } else if (length(data_args) !=
                  1) {
                  .mojor_err(
                    "mean()/var()/sd() reductions only support a single array argument",
                    red_call
                  )
                } else {
                  arg <- data_args[[1]]
                }
            } else {
                if (length(red_args) !=
                  1) {
                  .mojor_err(
                    "which.min()/which.max() reductions only support a single array argument",
                    red_call
                )
                }
                arg <- red_args[[1]]
            }
            arg_name <- NULL
            arg_seq_name <- NULL
            arg_elem_expr <- NULL
            arg_expr_mode <- FALSE
            spec <- NULL
            value_name <- NULL
            if (is.name(arg)) {
                arg_name <- as.character(arg)
                scalar_reduction_arg <- arg_name
                if (!(arg_name %in% args && .mojor_is_array(types[[arg_name]]))) {
                  .mojor_err(
                    "sum()/prod()/mean()/var()/sd()/min()/max()/which.min()/which.max() reductions require a typed array argument",
                    red_call
                )
                }
                spec <- types[[arg_name]]
            } else if (scalar_reduction_op %in% c("which.min", "which.max")) {
                arg_refs <- unique(.mojor_collect_array_args(arg, types, args))
                if (length(arg_refs) ==
                  0) {
                  .mojor_err(
                    "which.min()/which.max() reductions over expressions require array-backed selector arguments",
                    red_call
                )
                }
                arg_seq_name <- arg_refs[[1]]
                if (!(arg_seq_name %in% args && .mojor_is_array(types[[arg_seq_name]]))) {
                  .mojor_err(
                    "which.min()/which.max() reductions require a typed array argument",
                    red_call
                )
                }
                selector_expr <- is.call(arg) &&
                    as.character(arg[[1]]) == "[" &&
                    length(arg) >= 2 &&
                    is.name(arg[[2]])
                if (isTRUE(selector_expr)) {
                    value_name <- as.character(arg[[2]])
                    if (!(value_name %in% args && .mojor_is_array(types[[value_name]]))) {
                      .mojor_err(
                        "which.min()/which.max() reductions require selector base x to be a typed array argument",
                        red_call
                    )
                    }
                    spec <- types[[value_name]]
                } else {
                    ref_specs <- vapply(arg_refs, function(nm) as.character(types[[nm]]), character(1))
                    ref_bases <- unique(vapply(ref_specs, .mojor_type_spec_base, character(1)))
                    if (length(ref_bases) > 1) {
                      .mojor_err(
                        "which.min()/which.max() reductions over expressions require homogeneous array dtypes",
                        red_call
                    )
                    }
                    spec <- types[[arg_seq_name]]
                    value_name <- NULL
                }
                arg_expr_mode <- TRUE
            } else {
                arg_refs <- unique(.mojor_collect_array_args(arg, types, args))
                if (length(arg_refs) == 0) {
                  .mojor_err(
                    "sum()/prod()/mean()/var()/sd()/min()/max() reductions over expressions require array-backed arguments",
                    red_call
                  )
                }
                arg_seq_name <- arg_refs[[1]]
                if (!(arg_seq_name %in% args && .mojor_is_array(types[[arg_seq_name]]))) {
                  .mojor_err(
                    "sum()/prod()/mean()/var()/sd()/min()/max() reductions require typed array arguments",
                    red_call
                  )
                }
                ref_specs <- vapply(arg_refs, function(nm) as.character(types[[nm]]), character(1))
                ref_bases <- unique(vapply(ref_specs, .mojor_type_spec_base, character(1)))
                if (length(ref_bases) > 1) {
                  .mojor_err(
                    "sum()/prod()/mean()/var()/sd()/min()/max() reductions over expressions require homogeneous array dtypes",
                    red_call
                  )
                }
                arg_name <- arg_seq_name
                scalar_reduction_arg <- arg_name
                spec <- types[[arg_name]]
                arg_expr_ast <- arg
                arg_expr_mode <- TRUE
            }
            if (scalar_reduction_op %in% c("which.min", "which.max")) {
                if (!spec %in% c("f64[]", "f32[]", "i32[]", "lgl[]", "bool[]")) {
                  .mojor_err(
                    "which.min()/which.max() reductions require f64[]/f32[]/i32[]/lgl[] input",
                    red_call
                )
                }
                scalar_init <- if (isTRUE(arg_expr_mode))
                  "__mojor_idx" else "idx"
                scalar_type <- "i32"
                scalar_init_value <- 0L
                scalar_inits[[scalar_init]] <- list(type = scalar_type, value = scalar_init_value)
                return_name <- scalar_init
            } else {
                scalar_init <- "acc"
                if (scalar_reduction_op %in% c("mean", "var", "sd")) {
                  if (!spec %in% c("f64[]", "f32[]", "i32[]", "lgl[]", "bool[]")) {
                    .mojor_err(
                      "mean()/var()/sd() reductions require f64[]/f32[]/i32[]/lgl[] input",
                      red_call
                  )
                  }
                  scalar_type <- "f64"
                  scalar_init_value <- "0.0"
                } else if (spec %in% c("f64[]")) {
                  scalar_type <- "f64"
                  scalar_init_value <- if (scalar_reduction_op == "sum")
                    "0.0" else if (scalar_reduction_op == "product")
                    "1.0" else if (scalar_reduction_op == "min")
                    "_MOJOR_INF" else "_MOJOR_NINF"
                } else if (spec %in% c("f32[]")) {
                  scalar_type <- "f32"
                  scalar_init_value <- if (scalar_reduction_op == "sum")
                    "Float32(0.0)" else if (scalar_reduction_op == "product")
                    "Float32(1.0)" else if (scalar_reduction_op == "min")
                    "Float32(1.0/0.0)" else "Float32(-1.0/0.0)"
                } else if (spec %in% c("i32[]", "lgl[]", "bool[]")) {
                  is_logical_input <- spec %in% c("lgl[]", "bool[]")
                  if (is_logical_input && scalar_reduction_op %in% c("sum", "product")) {
                    scalar_type <- "i32"
                  } else if (is_logical_input) {
                    scalar_type <- "lgl"
                  } else {
                    scalar_type <- "i32"
                  }
                  scalar_init_value <- if (scalar_reduction_op == "sum")
                    0L else if (scalar_reduction_op == "product")
                    1L else if (scalar_reduction_op == "min")
                    .Machine$integer.max else .Machine$integer.min
                } else {
                  .mojor_err(
                    "sum()/prod()/min()/max() reductions require f64[]/f32[]/i32[]/lgl[] input",
                    red_call
                )
                }
                scalar_inits[[scalar_init]] <- list(type = scalar_type, value = scalar_init_value)
                return_name <- scalar_init
            }
            loop_var <- "i"
            if (isTRUE(arg_expr_mode) &&
                !is.null(arg_expr_ast) &&
                !(scalar_reduction_op %in% c("which.min", "which.max"))) {
                arg_elem_expr <- .mojor_vector_expr_to_element(arg_expr_ast, loop_var, types, args)
                arg_elem_expr <- .mojor_simplify_selector_index_expr(arg_elem_expr)
            }
            read_expr <- NULL
            if (!(scalar_reduction_op %in% c("which.min", "which.max"))) {
                read_expr <- if (!is.null(arg_elem_expr))
                    arg_elem_expr else call(
                    "[", as.name(arg_name),
                    as.name(loop_var)
                )
            }
            if (arg_expr_mode && scalar_reduction_op %in% c("which.min", "which.max")) {
                arg_elem_expr <- .mojor_vector_expr_to_element(arg, loop_var, types, args)
                if (is.null(arg_elem_expr)) {
                  .mojor_err(
                    "which.min()/which.max() expression reductions could not be lowered to element-wise selector access",
                    red_call
                )
                }
                arg_elem_expr <- .mojor_simplify_selector_index_expr(arg_elem_expr)
                selector_idx_expr <- NULL
                if (is.call(arg_elem_expr) &&
                  as.character(arg_elem_expr[[1]]) ==
                    "[" && length(arg_elem_expr) >=
                  3) {
                  selector_idx_expr <- arg_elem_expr[[3]]
                }
                best_name <- "__mojor_best"
                has_value_name <- "__mojor_has_value"
                if (spec %in% c("f64[]")) {
                  scalar_inits[[best_name]] <- list(type = "f64", value = "0.0")
                } else if (spec %in% c("f32[]")) {
                  scalar_inits[[best_name]] <- list(type = "f32", value = "Float32(0.0)")
                } else if (spec %in% c("i32[]")) {
                  scalar_inits[[best_name]] <- list(type = "i32", value = 0L)
                } else {
                  scalar_inits[[best_name]] <- list(type = "lgl", value = 0L)
                }
                scalar_inits[[has_value_name]] <- list(type = "lgl", value = 0L)
                cmp_call <- call(
                  if (scalar_reduction_op == "which.min") "<" else ">",
                  arg_elem_expr, as.name(best_name)
              )
                first_block <- as.call(
                  c(
                    as.name("{"),
                    list(
                      call(
                        "<-", as.name(best_name),
                        arg_elem_expr
                    ),
                      call(
                        "<-", as.name(scalar_init),
                        call("as.integer", as.name(loop_var))
                    ),
                      call(
                        "<-", as.name(has_value_name),
                        TRUE
                    )
                  )
                )
              )
                update_block <- as.call(
                  c(
                    as.name("{"),
                    list(
                      call(
                        "<-", as.name(best_name),
                        arg_elem_expr
                    ),
                      call(
                        "<-", as.name(scalar_init),
                        call("as.integer", as.name(loop_var))
                    )
                  )
                )
              )
                loop_body <- call(
                  "if", call("!", as.name(has_value_name)),
                  first_block, call("if", cmp_call, update_block)
              )
                if (!is.null(selector_idx_expr) &&
                    !is.null(value_name) &&
                    nzchar(value_name)) {
                    valid_selector_cond <- call(
                      "&&", call(">=", selector_idx_expr, 1L),
                      call("<=", selector_idx_expr, call("length", as.name(value_name)))
                  )
                    loop_body <- call("if", valid_selector_cond, loop_body)
                }
                loops[[1]] <- call(
                  "for", as.name(loop_var),
                  call("seq_along", as.name(arg_seq_name)),
                  loop_body
              )
                blocks <- list(loops[[1]])
                scalar_reduction_arg <- NULL
                scalar_reduce_ir_node <- NULL
            } else if (scalar_reduction_op %in% c("which.min", "which.max")) {
                best_name <- "__mojor_best"
                has_value_name <- "__mojor_has_value"
                if (spec %in% c("f64[]")) {
                  scalar_inits[[best_name]] <- list(type = "f64", value = "0.0")
                } else if (spec %in% c("f32[]")) {
                  scalar_inits[[best_name]] <- list(type = "f32", value = "Float32(0.0)")
                } else if (spec %in% c("i32[]")) {
                  scalar_inits[[best_name]] <- list(type = "i32", value = 0L)
                } else {
                  scalar_inits[[best_name]] <- list(type = "lgl", value = 0L)
                }
                scalar_inits[[has_value_name]] <- list(type = "lgl", value = 0L)
                arg_elem_expr <- call("[", as.name(arg_name), as.name(loop_var))
                cmp_call <- call(
                  if (scalar_reduction_op == "which.min") "<" else ">",
                  arg_elem_expr, as.name(best_name)
              )
                first_block <- as.call(
                  c(
                    as.name("{"),
                    list(
                      call(
                        "<-", as.name(best_name),
                        arg_elem_expr
                    ),
                      call(
                        "<-", as.name(scalar_init),
                        call("as.integer", as.name(loop_var))
                    ),
                      call(
                        "<-", as.name(has_value_name),
                        TRUE
                    )
                  )
                )
              )
                update_block <- as.call(
                  c(
                    as.name("{"),
                    list(
                      call(
                        "<-", as.name(best_name),
                        arg_elem_expr
                    ),
                      call(
                        "<-", as.name(scalar_init),
                        call("as.integer", as.name(loop_var))
                    )
                  )
                )
              )
                loop_body <- call(
                  "if", call("!", as.name(has_value_name)),
                  first_block, call("if", cmp_call, update_block)
              )
            } else if (scalar_reduction_op == "sum") {
                loop_body <- call(
                  "<-", as.name(scalar_init),
                  call(
                    "+", as.name(scalar_init),
                    read_expr
                )
              )
            } else if (scalar_reduction_op == "product") {
                loop_body <- call(
                  "<-", as.name(scalar_init),
                  call(
                    "*", as.name(scalar_init),
                    read_expr
                )
              )
            } else if (scalar_reduction_op == "mean") {
                count_name <- paste0(scalar_init, "_count")
                scalar_inits[[count_name]] <- list(type = "i32", value = 0L)
                update_acc <- call(
                  "<-", as.name(scalar_init),
                  call(
                    "+", as.name(scalar_init),
                    read_expr
                )
              )
                update_count <- call(
                  "<-", as.name(count_name),
                  call(
                    "+", as.name(count_name),
                    1L
                )
              )
                if (isTRUE(na_rm)) {
                  loop_body <- call(
                    "if", call("!", call("is.na", read_expr)),
                    as.call(
                      c(
                        as.name("{"),
                        list(update_acc, update_count)
                    )
                  )
                )
                } else {
                  loop_body <- as.call(
                    c(
                      as.name("{"),
                      list(update_acc, update_count)
                  )
                )
                }
            } else if (scalar_reduction_op %in% c("var", "sd")) {
                count_name <- paste0(scalar_init, "_count")
                mean_name <- paste0(scalar_init, "_mean")
                delta_name <- paste0(scalar_init, "_delta")
                delta2_name <- paste0(scalar_init, "_delta2")
                next_count_name <- paste0(scalar_init, "_count_next")
                scalar_inits[[count_name]] <- list(type = "i32", value = 0L)
                scalar_inits[[mean_name]] <- list(type = "f64", value = "0.0")
                scalar_inits[[delta_name]] <- list(type = "f64", value = "0.0")
                scalar_inits[[delta2_name]] <- list(type = "f64", value = "0.0")
                scalar_inits[[next_count_name]] <- list(type = "i32", value = 0L)
                welford_updates <- list(
                  call(
                    "<-", as.name(next_count_name),
                    call(
                      "+", as.name(count_name),
                      1L
                  )
                ),
                  call(
                    "<-", as.name(delta_name),
                    call("-", read_expr, as.name(mean_name))
                ),
                  call(
                    "<-", as.name(mean_name),
                    call(
                      "+", as.name(mean_name),
                      call(
                        "/", as.name(delta_name),
                        call("as.double", as.name(next_count_name))
                    )
                  )
                ),
                  call(
                    "<-", as.name(delta2_name),
                    call("-", read_expr, as.name(mean_name))
                ),
                  call(
                    "<-", as.name(scalar_init),
                    call(
                      "+", as.name(scalar_init),
                      call(
                        "*", as.name(delta_name),
                        as.name(delta2_name)
                    )
                  )
                ),
                  call(
                    "<-", as.name(count_name),
                    as.name(next_count_name)
                )
              )
                if (isTRUE(na_rm)) {
                  loop_body <- call(
                    "if", call("!", call("is.na", read_expr)),
                    as.call(
                      c(
                        as.name("{"),
                        welford_updates
                    )
                  )
                )
                } else {
                  loop_body <- as.call(
                    c(
                      as.name("{"),
                      welford_updates
                  )
                )
                }
            } else {
                loop_body <- call(
                  "<-", as.name(scalar_init),
                  call(
                    scalar_reduction_fn, as.name(scalar_init),
                    read_expr
                )
              )
            }
            if (!arg_expr_mode || (!is.null(scalar_reduction_arg) &&
                !is.null(arg_name))) {
                loops[[1]] <- call(
                  "for", as.name(loop_var),
                  call("seq_along", as.name(arg_name)),
                  loop_body
              )
                is_which <- scalar_reduction_op %in% c("which.min", "which.max")
                if (!isTRUE(arg_expr_mode) &&
                    scalar_reduction_op %in% c("sum", "product", "min", "max", "which.min", "which.max")) {
                  scalar_reduce_ir_node <- .mojor_ir_scalar_reduce(
                    op = scalar_reduction_op, acc = scalar_init, arg = arg_name,
                    init = NULL, axis = 0L, associative = !is_which, commutative = !is_which,
                    na_rm = na_rm
                  )
                } else {
                  scalar_reduce_ir_node <- NULL
                }
            }
        }
    }

    if (length(loops) ==
        0) {
        ctor <- .mojor_constructor_info(blocks, args, types)
        if (!is.null(ctor)) {
            has_array_arg <- any(vapply(
                args,
                function(a) !is.null(types[[a]]) && .mojor_is_array(types[[a]]),
                logical(1)
            ))
            if (!isTRUE(has_array_arg)) {
                ctor_len_ok <- tryCatch({
                    .mojor_len_expr_to_c(
                        ctor$len_expr,
                        args = args,
                        arg_specs = types,
                        allow_scalar_cast = TRUE
                    )
                    TRUE
                }, error = function(e) FALSE)
                if (!isTRUE(ctor_len_ok)) {
                    stop("mojor_transpile: constructor length must be inferable from scalar args or literals when no array arguments are present")
                }
            }
            .mojor_state$current_constructor_mode <- TRUE
            out_name <- if (!is.null(ctor$out_name))
                ctor$out_name else "__mojor_out"
            out_type <- ctor$out_type
            out_matrix <- FALSE
            out_array <- FALSE
            out_dim_exprs <- NULL
            out_dim_name <- NULL
            return_name <- out_name
            constructor_len_arrays <- ctor$len_arrays
            loop_var <- "i"
            loop_body <- call(
                "<-", call(
                  "[", as.name(out_name),
                  as.name(loop_var)
              ),
                ctor$expr
            )
            seq_expr <- call("mojor_seq_len", ctor$len_expr)
            loop_call <- call(
                "for", as.name(loop_var),
                seq_expr, loop_body
            )
            loops <- list(loop_call)
            blocks <- list(loop_call)
        }
    }

    list(
        blocks = blocks, loops = loops, any_all_call = any_all_call, any_all_arg = any_all_arg,
        any_all_mode = any_all_mode, seq_extra = seq_extra, scalar_init = scalar_init,
        scalar_type = scalar_type, scalar_init_value = scalar_init_value,
        scalar_inits = scalar_inits, return_name = return_name, scalar_reduction_op = scalar_reduction_op,
        scalar_reduction_arg = scalar_reduction_arg, scalar_reduce_ir_node = scalar_reduce_ir_node,
        out_name = out_name, out_type = out_type, out_matrix = out_matrix,
        out_array = out_array, out_dim_exprs = out_dim_exprs, out_dim_name = out_dim_name,
        constructor_len_arrays = constructor_len_arrays, no_loop_scalar_any_all = no_loop_scalar_any_all,
        na_rm = na_rm
    )
}

.mojor_collect_loop_infos <- function(loops) {
    loop_infos <- list()
    for (lp in loops) {
        if (is.call(lp) &&
            as.character(lp[[1]]) ==
                "for") {
            loop_var <- as.character(lp[[2]])
            loop_seq <- lp[[3]]
            loop_body <- lp[[4]]
            loop_blocks <- .mojor_extract_block(loop_body)
            if (length(loop_blocks) ==
                0) {
                .mojor_err("loop body must contain at least one statement", loop_body)
            }

 # Step 8.1: Transform iterator loops (for v in x) to
 # indexed loops.
            if (is.name(loop_seq)) {
                array_name <- as.character(loop_seq)
                iter_var <- "__mojor_iter_i"
                # Check if this is a seq alias (v <- seq_len(n); for (i in v))
                # by looking up in the state
                seq_alias_expr <- tryCatch({
                    alias_map <- .mojor_state$current_seq_aliases
                    if (!is.null(alias_map) && !is.null(alias_map[[array_name]])) {
                        alias_map[[array_name]]
                    } else {
                        NULL
                    }
                }, error = function(e) NULL)
                if (!is.null(seq_alias_expr) && is.call(seq_alias_expr)) {
                    # Use the original seq expression instead of seq_along
                    loop_seq <- seq_alias_expr
                } else {
                    loop_seq <- call("seq_along", as.name(array_name))
                    value_extract <- call(
                      "<-", as.name(loop_var),
                      call(
                        "[", as.name(array_name),
                        as.name(iter_var)
                    )
                  )
                    loop_blocks <- c(
                      list(value_extract),
                      loop_blocks
                  )
                    loop_var <- iter_var
                }
            }

            loop_infos[[length(loop_infos) +
                1]] <- list(kind = "for", var = loop_var, seq = loop_seq, blocks = loop_blocks)
        } else if (is.call(lp) &&
            as.character(lp[[1]]) ==
                "while") {
            loop_body <- lp[[3]]
            loop_blocks <- .mojor_extract_block(loop_body)
            if (length(loop_blocks) ==
                0) {
                .mojor_err("while body must contain at least one statement", loop_body)
            }
            loop_vars <- unique(
                c(
                  .mojor_loop_vars_from_blocks(loop_blocks),
                  .mojor_collect_index_vars(lp[[2]])
              )
            )
            loop_infos[[length(loop_infos) +
                1]] <- list(
                kind = "while", cond = lp[[2]], blocks = loop_blocks, loop_vars = loop_vars
            )
        } else if (is.call(lp) &&
            as.character(lp[[1]]) ==
                "repeat") {
            loop_body <- lp[[2]]
            loop_blocks <- .mojor_extract_block(loop_body)
            if (length(loop_blocks) ==
                0) {
                .mojor_err("repeat body must contain at least one statement", loop_body)
            }
            loop_vars <- unique(.mojor_loop_vars_from_blocks(loop_blocks))
            loop_infos[[length(loop_infos) +
                1]] <- list(kind = "repeat", blocks = loop_blocks, loop_vars = loop_vars)
        }
    }
    loop_infos
}

.mojor_loop_vars_from_blocks <- function(blocks) {
    vars <- character(0)
    for (b in blocks) {
        vars <- c(vars, .mojor_collect_index_vars(b))
    }
    unique(vars)
}

.mojor_blocks_assign_scalar <- function(blocks, scalar_names) {
    if (length(scalar_names) ==
        0) {
        return(FALSE)
    }
    for (b in blocks) {
        if (is.call(b) &&
            as.character(b[[1]]) %in%
                c("<-", "=")) {
            lhs <- b[[2]]
            if (is.name(lhs) &&
                as.character(lhs) %in%
                  scalar_names) {
                return(TRUE)
            }
        }
    }
    FALSE
}

.mojor_default_n_source <- function(
    arg_specs,
    args,
    allow_scalar_i32 = FALSE,
    ref_exprs = NULL,
    context_kind = "while",
    scalar_inits = NULL
) {
    scalar_n_source_spec <- function(spec) {
        !is.null(spec) &&
            identical(length(spec), 1L) &&
            spec %in% c("i32", "f64", "f32", "lgl", "bool")
    }
    array_args <- args[vapply(arg_specs[args], .mojor_is_array, logical(1))]
    if (length(array_args) ==
        0) {
        if (isTRUE(allow_scalar_i32)) {
            scalar_args <- args[vapply(
                args,
                function(nm) {
                    spec <- arg_specs[[nm]]
                    scalar_n_source_spec(spec)
                },
                logical(1)
            )]
            scalar_locals <- character(0)
            if (!is.null(scalar_inits) &&
                length(scalar_inits) > 0) {
                local_names <- names(scalar_inits)
                if (!is.null(local_names) &&
                    length(local_names) > 0) {
                    scalar_locals <- local_names[vapply(
                        local_names,
                        function(nm) {
                            init <- scalar_inits[[nm]]
                            is.list(init) &&
                                !is.null(init$type) &&
                                scalar_n_source_spec(init$type)
                        },
                        logical(1)
                    )]
                }
            }
            scalar_candidates <- unique(c(scalar_args, scalar_locals))
            if (length(scalar_candidates) > 0) {
                chosen <- if (length(scalar_args) > 0) scalar_args[[1]] else scalar_locals[[1]]
                selected_from_ref <- FALSE
                if ("n" %in% scalar_candidates) {
                    chosen <- "n"
                }
                if (!is.null(ref_exprs)) {
                    if (!is.list(ref_exprs)) {
                        ref_exprs <- list(ref_exprs)
                    }
                    ref_names <- unique(unlist(lapply(
                        ref_exprs,
                        function(expr) {
                            if (is.null(expr)) {
                                return(character(0))
                            }
                            all.names(expr, functions = FALSE, unique = TRUE)
                        }
                    )))
                    scalar_refs_args <- intersect(scalar_args, ref_names)
                    if (length(scalar_refs_args) > 0) {
                        chosen <- scalar_refs_args[[1]]
                        selected_from_ref <- TRUE
                    } else {
                        scalar_refs_locals <- intersect(ref_names, scalar_locals)
                        if ("n" %in% scalar_candidates) {
                            chosen <- "n"
                        } else if (length(scalar_refs_locals) > 0) {
                            chosen <- scalar_refs_locals[[1]]
                            selected_from_ref <- TRUE
                        }
                    }
                }
                if ("n" %in% scalar_candidates && !isTRUE(selected_from_ref)) {
                    chosen <- "n"
                }
                return(list(
                    kind = "scalar", name = chosen, extra = NULL, end_delta = 0L
                ))
            }
            if (context_kind %in% c("while", "repeat", "for")) {
                return(list(kind = "none", name = NULL, extra = NULL, end_delta = 0L))
            }
            stop(
                sprintf(
                    "mojor_transpile: %s loops require a typed array argument, supported scalar argument, or local supported scalar variable to determine n",
                    context_kind
                )
            )
        }
        stop(
            sprintf(
                "mojor_transpile: %s loops require at least one array argument to determine n",
                context_kind
            )
        )
    }
    list(
        kind = "array", name = array_args[[1]], extra = if (length(array_args) >
            1) array_args[-1] else NULL, end_delta = 0L
    )
}

.mojor_len_param_name <- function(name) {
    paste0("__mojor_len_", name)
}

.mojor_len_var_name <- function(name) {
    paste0("n_", name, "_i")
}

.mojor_nrow_param_name <- function(name) {
    paste0("__mojor_nrow_", name)
}

.mojor_ncol_param_name <- function(name) {
    paste0("__mojor_ncol_", name)
}

.mojor_nrow_var_name <- function(name) {
    paste0("nrow_", name, "_i")
}

.mojor_ncol_var_name <- function(name) {
    paste0("ncol_", name, "_i")
}

.mojor_dim_param_name <- function(name) {
    paste0("__mojor_dim_", name)
}

.mojor_ndim_param_name <- function(name) {
    paste0("__mojor_ndim_", name)
}

.mojor_dim_var_name <- function(name) {
    paste0("dim_", name, "_ptr")
}

.mojor_ndim_var_name <- function(name) {
    paste0("ndim_", name, "_i")
}

.mojor_out_nrow_param_name <- function() {
    "__mojor_out_nrow"
}

.mojor_out_nrow_var_name <- function() {
    "nrow_out_i"
}

.mojor_out_ncol_param_name <- function() {
    "__mojor_out_ncol"
}

.mojor_out_ncol_var_name <- function() {
    "ncol_out_i"
}

.mojor_out_dim_param_name <- function() {
    "__mojor_out_dim"
}

.mojor_out_ndim_param_name <- function() {
    "__mojor_out_ndim"
}

.mojor_out_dim_var_name <- function() {
    "dim_out_ptr"
}

.mojor_out_ndim_var_name <- function() {
    "ndim_out_i"
}

.mojor_len_var_for <- function(seq_info, n_source_name, len_var_map) {
    if (is.null(seq_info)) {
        return(NULL)
    }
    if (identical(seq_info$kind, "expr")) {
        return("n_i")
    }
    if (seq_info$kind %in% c("array", "iter")) {
        if (!is.null(n_source_name) &&
            identical(seq_info$name, n_source_name)) {
            return("n_i")
        }
        if (!is.null(len_var_map) &&
            !is.null(len_var_map[[seq_info$name]])) {
            return(len_var_map[[seq_info$name]])
        }
        return(NULL)
    }
    if (identical(seq_info$kind, "scalar")) {
        return(paste0("Int(", seq_info$name, ")"))
    }
    NULL
}

.mojor_collect_matrix_arrays <- function(expr) {
    if (missing(expr) ||
        is.null(expr)) {
        return(character(0))
    }
    vars <- character(0)
    if (is.call(expr)) {
        op <- as.character(expr[[1]])
        if (identical(op, "[") &&
            length(expr) ==
                4) {
            var <- expr[[2]]
            if (is.name(var))
                vars <- c(vars, as.character(var))
        } else if (identical(op, "apply") &&
            length(expr) >=
                2) {
            x_arg <- expr[[2]]
            if (is.name(x_arg))
                vars <- c(vars, as.character(x_arg))
        }
        for (p in as.list(expr)[-1]) {
            vars <- c(vars, .mojor_collect_matrix_arrays(p))
        }
    }
    vars
}

.mojor_collect_nd_arrays <- function(expr) {
    if (missing(expr) ||
        is.null(expr)) {
        return(character(0))
    }
    vars <- character(0)
    if (is.call(expr)) {
        op <- as.character(expr[[1]])
        if (identical(op, "[") &&
            length(expr) >
                4) {
            var <- expr[[2]]
            if (is.name(var))
                vars <- c(vars, as.character(var))
        }
        for (p in as.list(expr)[-1]) {
            vars <- c(vars, .mojor_collect_nd_arrays(p))
        }
    }
    vars
}

.mojor_collect_tensor_ranks <- function(expr) {
    if (missing(expr) ||
        is.null(expr)) {
        return(list())
    }
    ranks <- list()
    walk <- function(e) {
        if (missing(e) ||
            is.null(e)) {
            return(invisible(NULL))
        }
        if (is.call(e)) {
            op <- as.character(e[[1]])
            if (identical(op, "apply") &&
                length(e) >=
                  2) {
                x_arg <- e[[2]]
                if (is.name(x_arg)) {
                  nm <- as.character(x_arg)
                  rank <- 2L
                  if (!is.null(ranks[[nm]]) &&
                    ranks[[nm]] != rank) {
                    .mojor_err(
                      "mixed ranks for array indexing", e, "use a single rank per array in a function"
                  )
                  }
                  ranks[[nm]] <<- rank
                }
            }
            if (identical(op, "[") &&
                length(e) >=
                  4) {
                var <- e[[2]]
                if (is.name(var)) {
 # Exclude named parameters (like drop=FALSE) from
 # rank calculation
                  all_args <- as.list(e)[3:length(e)]
                  arg_names <- names(all_args)
                  idxs <- all_args[!nzchar(if (is.null(arg_names)) "" else arg_names)]
                  rank <- length(idxs)
                  if (rank >= 2) {
                    nm <- as.character(var)
                    if (!is.null(ranks[[nm]]) &&
                      ranks[[nm]] != rank) {
                      .mojor_err(
                        "mixed ranks for array indexing", e, "use a single rank per array in a function"
                    )
                    }
                    ranks[[nm]] <<- rank
                  }
                }
            }
            for (p in as.list(e)[-1]) walk(p)
        }
    }
    walk(expr)
    ranks
}

.mojor_collect_tensor_ranks_blocks <- function(blocks) {
    if (length(blocks) ==
        0) {
        return(list())
    }
    ranks <- list()
    merge_ranks <- function(dst, src) {
        for (nm in names(src)) {
            if (!is.null(dst[[nm]]) &&
                dst[[nm]] != src[[nm]]) {
                .mojor_err(
                  "mixed ranks for array indexing", NULL, "use a single rank per array in a function"
              )
            }
            dst[[nm]] <- src[[nm]]
        }
        dst
    }
    for (rank_map in lapply(blocks, .mojor_collect_tensor_ranks)) {
        ranks <- merge_ranks(ranks, rank_map)
    }
    ranks
}

.mojor_collect_matrix_arrays_blocks <- function(blocks) {
    .mojor_collect_unique_from_blocks(blocks, .mojor_collect_matrix_arrays)
}

.mojor_collect_nd_arrays_blocks <- function(blocks) {
    .mojor_collect_unique_from_blocks(blocks, .mojor_collect_nd_arrays)
}

.mojor_collect_slice_rhs_arrays <- function(expr, arg_specs, args) {
    if (!is.call(expr)) {
        return(character(0))
    }
    is_missing_index <- function(ix) {
        is.symbol(ix) && identical(as.character(ix), "")
    }
    is_scalar_index_expr <- function(ix) {
        if (is_missing_index(ix)) {
            return(FALSE)
        }
        if (is.name(ix)) {
            nm <- as.character(ix)
            if (!is.null(arg_specs[[nm]]) && .mojor_is_array(arg_specs[[nm]])) {
                return(FALSE)
            }
            return(TRUE)
        }
        if (is.atomic(ix)) {
            return(length(ix) == 1L && !anyNA(ix))
        }
        if (!is.call(ix)) {
            return(FALSE)
        }
        op_ix <- as.character(ix[[1]])
        if (op_ix %in% c(":", "seq", "seq.int", "seq_len", "seq_along", "c", "rep", "rep.int", "rep_len")) {
            return(FALSE)
        }
        parts_ix <- as.list(ix)
        if (length(parts_ix) < 2) {
            return(FALSE)
        }
        all(vapply(parts_ix[-1], is_scalar_index_expr, logical(1)))
    }
    is_slice_index <- function(ix) {
        if (is_missing_index(ix)) {
            return(TRUE)
        }
        !is_scalar_index_expr(ix)
    }
    collect_rhs_slice_arrays <- function(node) {
        if (!is.call(node)) {
            return(character(0))
        }
        hits <- character(0)
        op_node <- as.character(node[[1]])
        if (identical(op_node, "[") && length(node) >= 3 && is.name(node[[2]])) {
            base_nm <- as.character(node[[2]])
            if (base_nm %in% args &&
                !is.null(arg_specs[[base_nm]]) &&
                .mojor_is_array(arg_specs[[base_nm]])) {
                idx_parts <- as.list(node)[-c(1, 2)]
                if (length(idx_parts) > 0 && any(vapply(idx_parts, is_slice_index, logical(1)))) {
                    hits <- c(hits, base_nm)
                }
            }
        }
        parts <- as.list(node)
        if (length(parts) >= 2) {
            for (i in 2:length(parts)) {
                hits <- c(hits, collect_rhs_slice_arrays(parts[[i]]))
            }
        }
        unique(hits)
    }
    op <- as.character(expr[[1]])
    if (length(op) ==
        1 && op %in% c("<-", "=") &&
        length(expr) >=
            3) {
        lhs <- expr[[2]]
        rhs <- expr[[3]]
        if (is.call(lhs) &&
            as.character(lhs[[1]]) ==
                "[" && length(lhs) >=
            3) {
            lhs_idx_parts <- as.list(lhs)[-c(1, 2)]
            lhs_is_slice <- length(lhs_idx_parts) > 0 &&
                any(vapply(lhs_idx_parts, is_slice_index, logical(1)))
            if (isTRUE(lhs_is_slice)) {
                rhs_arrays <- unique(.mojor_collect_array_args(rhs, arg_specs, args))
                if (length(rhs_arrays) > 0) {
                    return(rhs_arrays)
                }
            }
            rhs_arrays <- collect_rhs_slice_arrays(rhs)
            if (length(rhs_arrays) > 0) {
                return(rhs_arrays)
            }
        }
    }
    vars <- character(0)
    parts <- as.list(expr)
    if (length(parts) >=
        2) {
        for (i in 2:length(parts)) {
            vars <- c(vars, .mojor_collect_slice_rhs_arrays(parts[[i]], arg_specs, args))
        }
    }
    vars
}

.mojor_collect_slice_rhs_arrays_blocks <- function(blocks, arg_specs, args) {
    .mojor_collect_unique_from_blocks(
        blocks, .mojor_collect_slice_rhs_arrays, arg_specs = arg_specs,
        args = args
    )
}

.mojor_slice_bounds <- function(idx_expr, args, arg_specs, n_source_name = NULL, len_var_map = NULL) {
    slice_end <- function(end_expr) {
        list(
            end = .mojor_dim_expr_to_mojo(
                end_expr, args, arg_specs, len_var_map = len_var_map, n_source_name = n_source_name
            )$mojo_expr,
            end_c = .mojor_dim_expr_to_c(
                end_expr, args, arg_specs, len_var_map = len_var_map, n_source_name = n_source_name
            )$c_expr
        )
    }
    if (!is.call(idx_expr)) {
        return(NULL)
    }
    op <- as.character(idx_expr[[1]])
    if (op == ":" && length(idx_expr) ==
        3) {
        start <- idx_expr[[2]]
        end <- idx_expr[[3]]
        if (!is.numeric(start)) {
            return(NULL)
        }
        start_val <- as.integer(start)
        if (is.na(start_val) ||
            start_val < 1L) {
            return(NULL)
        }
        end_info <- slice_end(end)
        return(list(start = start_val, end = end_info$end, end_c = end_info$end_c))
    }
    if (op == "seq_len" && length(idx_expr) ==
        2) {
        end <- idx_expr[[2]]
        end_info <- slice_end(end)
        return(list(start = 1L, end = end_info$end, end_c = end_info$end_c))
    }
    if (op == "seq.int") {
        if (length(idx_expr) ==
            2) {
            end <- idx_expr[[2]]
            end_info <- slice_end(end)
            return(list(start = 1L, end = end_info$end, end_c = end_info$end_c))
        }
        if (length(idx_expr) >=
            3) {
            start <- idx_expr[[2]]
            end <- idx_expr[[3]]
            if (!is.numeric(start)) {
                return(NULL)
            }
            start_val <- as.integer(start)
            if (is.na(start_val) ||
                start_val < 1L) {
                return(NULL)
            }
            end_info <- slice_end(end)
            return(list(start = start_val, end = end_info$end, end_c = end_info$end_c))
        }
    }
    if (op == "seq_along" && length(idx_expr) ==
        2) {
        end <- idx_expr[[2]]
        end_info <- slice_end(call("length", end))
        return(list(start = 1L, end = end_info$end, end_c = end_info$end_c))
    }
    NULL
}

.mojor_index_expr_info <- function(idx_expr, loop_vars, zero_based_vars = NULL, scalar_vars = NULL) {
    if (is.null(zero_based_vars)) {
        zero_based_vars <- .mojor_state$current_zero_based_vars
        if (is.null(zero_based_vars))
            zero_based_vars <- character(0)
    }
    if (is.null(scalar_vars)) {
        scalar_vars <- .mojor_state$current_scalar_vars
        if (is.null(scalar_vars))
            scalar_vars <- character(0)
    }
    as_int <- function(expr) {
        if (grepl("^Int\\(", expr)) {
            return(expr)
        }
        paste0("Int(", expr, ")")
    }

 # Helper to build index expression with zero-based adjustment
    build_expr <- function(base_expr, var_name) {
        if (is.null(zero_based_vars) ||
            !(var_name %in% zero_based_vars)) {
            paste0("(", base_expr, " - 1)")
        } else {
            base_expr
        }
    }

 # Direct loop variable: i
    if (is.name(idx_expr)) {
        name <- as.character(idx_expr)
        if (name %in% loop_vars) {
            expr <- build_expr(name, name)
            return(
                list(
                  expr = as_int(expr),
                  offset = 0L, scale = 1L
              )
            )
        }
        return(NULL)
    }

    if (is.call(idx_expr) &&
        length(idx_expr) ==
            3) {
        op <- as.character(idx_expr[[1]])
        a <- idx_expr[[2]]
        b <- idx_expr[[3]]

 # Parse a part: can be loop var, scalar var, or numeric
        parse_part <- function(x) {
            if (is.name(x)) {
                nm <- as.character(x)
                if (nm %in% loop_vars) {
                  return(list(kind = "var", name = nm))
                }
                if (nm %in% scalar_vars) {
                  return(list(kind = "scalar", name = nm))
                }
            }
            if (is.numeric(x) &&
                length(x) ==
                  1) {
                val <- as.integer(x)
                if (!is.na(val)) {
                  return(list(kind = "num", value = val))
                }
            }
            NULL
        }

 # Multiplication/division: i * k, k * i, i / k
        if (op %in% c("*", "/")) {
            pa <- parse_part(a)
            pb <- parse_part(b)
            if (is.null(pa) ||
                is.null(pb)) {
                return(NULL)
            }

 # var * num or num * var
            var_part <- NULL
            num_part <- NULL
            if (pa$kind == "var" && pb$kind == "num") {
                var_part <- pa
                num_part <- pb$value
            } else if (pa$kind == "num" && pb$kind == "var" && op == "*") {
                var_part <- pb
                num_part <- pa$value
            }

            if (!is.null(var_part) &&
                !is.null(num_part)) {
                if (op == "/" && num_part == 0)
                  {
                    return(NULL)
                  }  # Division by zero

 # Build expression: (var OP num - 1) for 1-based
                if (op == "*") {
                  base_expr <- paste0("(", var_part$name, " * ", num_part, ")")
                } else {
                  base_expr <- paste0("(", var_part$name, " / ", num_part, ")")
                }
                expr <- build_expr(base_expr, var_part$name)
                return(
                  list(
                    expr = as_int(expr),
                    offset = 0L, scale = num_part
                )
              )
            }
            return(NULL)
        }

 # Addition/subtraction
        if (!op %in% c("+", "-")) {
            return(NULL)
        }

        pa <- parse_part(a)
        pb <- parse_part(b)
        if (is.null(pa) ||
            is.null(pb)) {
            return(NULL)
        }

 # var +/- num (existing)
        if (pa$kind == "var" && pb$kind == "num") {
            offset <- if (op == "+")
                pb$value else -pb$value
            if (offset == 0L) {
                base_expr <- pa$name
            } else if (offset < 0L) {
                base_expr <- paste0(
                  "(", pa$name, " - ", abs(offset),
                  ")"
              )
            } else {
                base_expr <- paste0("(", pa$name, " + ", offset, ")")
            }
            expr <- build_expr(base_expr, pa$name)
            return(
                list(
                  expr = as_int(expr),
                  offset = offset, scale = 1L
              )
            )
        }

 # num + var (existing)
        if (pa$kind == "num" && pb$kind == "var" && op == "+") {
            offset <- pa$value
            if (offset == 0L) {
                base_expr <- pb$name
            } else if (offset < 0L) {
                base_expr <- paste0(
                  "(", pb$name, " - ", abs(offset),
                  ")"
              )
            } else {
                base_expr <- paste0("(", pb$name, " + ", offset, ")")
            }
            expr <- build_expr(base_expr, pb$name)
            return(
                list(
                  expr = as_int(expr),
                  offset = offset, scale = 1L
              )
            )
        }

 # NEW: var +/- scalar_var (e.g., i + offset, i - stride)
        if (pa$kind == "var" && pb$kind == "scalar") {
 # i + offset or i - offset
            if (op == "+") {
                base_expr <- paste0("(", pa$name, " + ", pb$name, ")")
            } else {
                base_expr <- paste0("(", pa$name, " - ", pb$name, ")")
            }
            expr <- build_expr(base_expr, pa$name)
            return(
                list(
                  expr = as_int(expr),
                  offset = NULL, scale = 1L, offset_var = pb$name
              )
            )
        }

 # NEW: scalar_var + var (e.g., offset + i)
        if (pa$kind == "scalar" && pb$kind == "var" && op == "+") {
            base_expr <- paste0("(", pa$name, " + ", pb$name, ")")
            expr <- build_expr(base_expr, pb$name)
            return(
                list(
                  expr = as_int(expr),
                  offset = NULL, scale = 1L, offset_var = pa$name
              )
            )
        }
    }
    NULL
}

.mojor_index_constant_scalar_value <- function(idx_expr) {
    if (is.numeric(idx_expr) &&
        length(idx_expr) ==
            1 && !is.na(idx_expr) &&
        is.finite(idx_expr)) {
        return(as.numeric(idx_expr))
    }
    if (is.integer(idx_expr) &&
        length(idx_expr) ==
            1 && !is.na(idx_expr)) {
        return(as.numeric(idx_expr))
    }
    if (!is.call(idx_expr)) {
        return(NULL)
    }
    op <- as.character(idx_expr[[1]])
    if (length(op) !=
        1)
        op <- op[[1]]
    if (op == "(" && length(idx_expr) ==
        2) {
        return(.mojor_index_constant_scalar_value(idx_expr[[2]]))
    }
    if (op == "+" && length(idx_expr) ==
        2) {
        return(.mojor_index_constant_scalar_value(idx_expr[[2]]))
    }
    if (op == "-" && length(idx_expr) ==
        2) {
        rhs <- .mojor_index_constant_scalar_value(idx_expr[[2]])
        if (is.null(rhs)) {
            return(NULL)
        }
        return(-rhs)
    }
    if (op %in% c("+", "-", "*", "/", "%/%", "%%") &&
        length(idx_expr) ==
            3) {
        lhs <- .mojor_index_constant_scalar_value(idx_expr[[2]])
        rhs <- .mojor_index_constant_scalar_value(idx_expr[[3]])
        if (is.null(lhs) || is.null(rhs)) {
            return(NULL)
        }
        if (op %in% c("/", "%/%", "%%") && rhs == 0) {
            return(NULL)
        }
        if (op == "+")
            return(lhs + rhs)
        if (op == "-")
            return(lhs - rhs)
        if (op == "*")
            return(lhs * rhs)
        if (op == "/")
            return(lhs / rhs)
        if (op == "%/%")
            return(lhs %/% rhs)
        return(lhs %% rhs)
    }
    if (op == "abs" && length(idx_expr) ==
        2) {
        rhs <- .mojor_index_constant_scalar_value(idx_expr[[2]])
        if (is.null(rhs)) {
            return(NULL)
        }
        return(abs(rhs))
    }
    if (op == "as.integer" && length(idx_expr) ==
        2) {
        rhs <- .mojor_index_constant_scalar_value(idx_expr[[2]])
        if (is.null(rhs) || is.na(rhs) || !is.finite(rhs)) {
            return(NULL)
        }
        return(as.integer(rhs))
    }
    if (op %in% c("min", "max", "pmin", "pmax") &&
        length(idx_expr) >=
            3) {
        vals <- as.list(idx_expr)[-1]
        parsed <- vapply(
            vals,
            function(v) {
                vv <- .mojor_index_constant_scalar_value(v)
                if (is.null(vv) || is.na(vv) || !is.finite(vv)) {
                  return(NA_real_)
                }
                as.numeric(vv)
            },
            numeric(1)
        )
        if (anyNA(parsed)) {
            return(NULL)
        }
        if (op %in% c("min", "pmin")) {
            return(min(parsed))
        }
        return(max(parsed))
    }
    NULL
}

.mojor_index_has_negative_literal <- function(idx_expr) {
    if (identical(idx_expr, quote(expr =))) {
        return(FALSE)
    }
    const_val <- .mojor_index_constant_scalar_value(idx_expr)
    if (!is.null(const_val)) {
        return(is.finite(const_val) && const_val < 0)
    }
    if (!(is.call(idx_expr) &&
        length(idx_expr) >
            1)) {
        return(FALSE)
    }
    op <- as.character(idx_expr[[1]])
    if (length(op) !=
        1)
        op <- op[[1]]
    if (op == "-" &&
        length(idx_expr) ==
            2) {
        inner_val <- .mojor_index_constant_scalar_value(idx_expr[[2]])
        if (!is.null(inner_val)) {
            return(is.finite(inner_val) && inner_val > 0)
        }
        return(TRUE)
    }
    scalar_affine_ops <- c(
        "(", "+", "-", "*", "/", "%/%", "%%",
        "as.integer", "abs", "min", "max", "pmin", "pmax"
    )
    if (op %in% scalar_affine_ops) {
        return(FALSE)
    }
    parts <- as.list(idx_expr)[-1]
    for (k in seq_along(parts)) {
        if (.mojor_index_has_negative_literal(parts[[k]])) {
            return(TRUE)
        }
    }
    FALSE
}

.mojor_validate_negative_indices <- function(blocks) {
    # R-style negative indexing (exclusion) is supported for scalar indices:
    #   - Whole-vector assignment: out <- x[-k] (copy-skip loop)
    #   - Loop body read: out[i] <- x[-k] (first element of exclusion)
    #   - Write-side: out[-k] <- val (assign to all except k-th)
    # Vector exclusion forms (x[-c(1,2)], x[-(1:n)]) are rejected.

    is_missing_call_arg <- function(call_node, pos) {
        tryCatch(
            {
                val <- call_node[[pos]]
                is.symbol(val) && identical(as.character(val), "")
            },
            error = function(e) {
                grepl("argument.*missing|subscript out of bounds", conditionMessage(e), ignore.case = TRUE)
            }
        )
    }

    walk <- function(node) {
        if (identical(node, quote(expr =))) return(invisible(NULL))
        if (!is.call(node)) return(invisible(NULL))
        op <- as.character(node[[1]])
        if (length(op) != 1) op <- op[[1]]

        # Check for vector negative indices in [ and [[ calls
        # Supported forms: -c(lit,...) and -(a:b); unsupported forms still error
        if (op %in% c("[", "[[") && length(node) >= 3) {
            for (k in 3:length(node)) {
                if (isTRUE(is_missing_call_arg(node, k))) next
                idx_expr <- node[[k]]
                if (identical(idx_expr, quote(expr =))) next
                neg_info <- .mojor_ir_detect_negative_index(idx_expr)
                if (neg_info$is_negative && isTRUE(neg_info$is_vector)) {
                    vec_excl <- .mojor_ir_build_vector_exclusion_index(neg_info)
                    if (is.null(vec_excl)) {
                        .mojor_err(
                            "unsupported vector negative indexing form; supported: x[-c(lit,...)], x[-(a:b)]",
                            node
                        )
                    }
                }
            }
        }

        parts <- as.list(node)[-1]
        for (k in seq_along(parts)) {
            if (isTRUE(is_missing_call_arg(node, k + 1L))) next
            walk(parts[[k]])
        }
        invisible(NULL)
    }

    for (b in blocks) walk(b)
    invisible(NULL)
}

.mojor_validate_negative_write_targets <- function(
    blocks,
    out_name = NULL,
    return_name = NULL,
    modified_args = character(0)
) {
    allowed_targets <- character(0)
    if (!is.null(out_name) && nzchar(out_name)) {
        allowed_targets <- c(allowed_targets, out_name)
    }
    if (!is.null(return_name) && nzchar(return_name) &&
        return_name %in% modified_args) {
        allowed_targets <- c(allowed_targets, return_name)
    }
    allowed_targets <- unique(allowed_targets)

    is_missing_call_arg <- function(call_node, pos) {
        tryCatch(
            {
                val <- call_node[[pos]]
                is.symbol(val) && identical(as.character(val), "")
            },
            error = function(e) {
                grepl("argument.*missing|subscript out of bounds", conditionMessage(e), ignore.case = TRUE)
            }
        )
    }

    walk <- function(node) {
        if (identical(node, quote(expr =))) return(invisible(NULL))
        if (!is.call(node)) return(invisible(NULL))

        op <- as.character(node[[1]])
        if (length(op) != 1L) op <- op[[1L]]
        if (op %in% c("<-", "=") && length(node) >= 3L) {
            lhs <- node[[2L]]
            if (is.call(lhs) &&
                identical(as.character(lhs[[1L]])[1L], "[") &&
                length(lhs) >= 3L &&
                is.name(lhs[[2L]])) {
                has_negative_selector <- FALSE
                if (length(lhs) >= 3L) {
                    for (k in 3:length(lhs)) {
                        if (isTRUE(is_missing_call_arg(lhs, k))) next
                        idx_expr <- lhs[[k]]
                        if (identical(idx_expr, quote(expr =))) next
                        neg_info <- .mojor_ir_detect_negative_index(idx_expr)
                        if (isTRUE(neg_info$is_negative)) {
                            has_negative_selector <- TRUE
                            break
                        }
                    }
                }
                if (isTRUE(has_negative_selector)) {
                    base_name <- as.character(lhs[[2L]])
                    if (!(base_name %in% allowed_targets)) {
                        .mojor_err(
                            "negative indexed writes are supported only on the output base variable",
                            node,
                            "return the modified target (or assign through an explicit output variable) when using negative write selectors"
                        )
                    }
                }
            }
        }

        parts <- as.list(node)[-1]
        for (k in seq_along(parts)) {
            if (isTRUE(is_missing_call_arg(node, k + 1L))) next
            walk(parts[[k]])
        }
        invisible(NULL)
    }

    for (b in blocks) walk(b)
    invisible(NULL)
}

.mojor_unique_loop_var <- function(base, used) {
    var <- base
    i <- 1L
    while (var %in% used) {
        var <- paste0(base, i)
        i <- i + 1L
    }
    var
}

.mojor_tensor_name <- function(tensor_map, name) {
    if (is.null(tensor_map)) {
        return(NULL)
    }
    if (!is.character(name) ||
        length(name) !=
            1) {
        return(NULL)
    }
    if (!length(tensor_map)) {
        return(NULL)
    }
    nms <- names(tensor_map)
    if (is.null(nms) ||
        !(name %in% nms)) {
        return(NULL)
    }
    tensor_map[[name]]
}

.mojor_slice_index_expr <- function(var_name, loop_var, offset) {
    if (offset == 0L) {
        return(
            call(
                "[", as.name(var_name),
                as.name(loop_var)
            )
        )
    }
    if (offset < 0L) {
        return(
            call(
                "[", as.name(var_name),
                call(
                  "-", as.name(loop_var),
                  abs(offset)
              )
            )
        )
    }
    call(
        "[", as.name(var_name),
        call(
            "+", as.name(loop_var),
            offset
        )
    )
}

.mojor_linear_index_expr_out <- function(
    idx_exprs, out_nrow_var = NULL, out_dim_var = NULL, loop_vars = NULL,
    zero_based_vars = NULL
) {
    if (is.null(zero_based_vars)) {
        zero_based_vars <- .mojor_state$current_zero_based_vars
        if (is.null(zero_based_vars))
            zero_based_vars <- loop_vars
    }
    zero_expr <- function(x) {
        if (!is.null(zero_based_vars) &&
            x %in% zero_based_vars) {
            return(x)
        }
        paste0("(", x, " - 1)")
    }
    if (!is.null(out_nrow_var)) {
        if (length(idx_exprs) !=
            2) {
            return(NULL)
        }
        return(
            paste0(
                "(", zero_expr(idx_exprs[[1]]),
                " + (", zero_expr(idx_exprs[[2]]),
                ") * ", out_nrow_var, ")"
            )
        )
    }
    if (!is.null(out_dim_var)) {
        if (length(idx_exprs) <
            2) {
            return(NULL)
        }
        terms <- c(
            paste0(
                "(", zero_expr(idx_exprs[[1]]),
                ")"
            )
        )
        if (length(idx_exprs) >=
            2) {
            prod <- paste0("Int(", out_dim_var, "[0])")
            for (k in 2:length(idx_exprs)) {
                terms <- c(
                  terms, paste0(
                    "(", zero_expr(idx_exprs[[k]]),
                    ") * ", prod
                )
              )
                if (k < length(idx_exprs)) {
                  prod <- paste0(prod, " * Int(", out_dim_var, "[", k - 1, "])")
                }
            }
        }
        return(
            paste0(
                "(", paste(terms, collapse = " + "),
                ")"
            )
        )
    }
    NULL
}

.mojor_indexlist_expr <- function(idx_exprs, rank, loop_vars = NULL, zero_based_vars = NULL) {
    if (length(idx_exprs) !=
        rank) {
        return(NULL)
    }
    if (is.null(zero_based_vars)) {
        zero_based_vars <- .mojor_state$current_zero_based_vars
        if (is.null(zero_based_vars))
            zero_based_vars <- loop_vars
    }
    idx_zero <- vapply(
        idx_exprs, function(x) {
            if (!is.null(zero_based_vars) &&
                x %in% zero_based_vars) {
                return(x)
            }
            paste0("(", x, " - 1)")
        }, character(1)
    )
    paste(idx_zero, collapse = ", ")
}

.mojor_dimnames_eval_env <- local({
    env <- new.env(parent = emptyenv())
    fn_names <- c(
        ":", "[", "c", "paste", "paste0", "sprintf",
        "rep", "rep_len", "seq", "seq_len", "seq.int",
        "as.character", "tolower", "toupper",
        "substr", "substring", "sort", "unique", "rev"
    )
    for (fn in fn_names) {
        assign(fn, get(fn, envir = baseenv()), envir = env)
    }
    assign("letters", letters, envir = env)
    assign("LETTERS", LETTERS, envir = env)
    assign("month.abb", month.abb, envir = env)
    assign("month.name", month.name, envir = env)
    env
})

.mojor_parse_literal_dimnames <- function(dimnames_expr, owner = "array", expected_len = NULL) {
    if (is.null(dimnames_expr)) {
        return(NULL)
    }
    if (is.name(dimnames_expr) &&
        as.character(dimnames_expr) ==
            "NULL") {
        return(NULL)
    }
    if (!is.call(dimnames_expr) ||
        as.character(dimnames_expr[[1]]) !=
            "list") {
        if (identical(owner, "matrix")) {
            stop("mojor_transpile: matrix(dimnames=...) must be a list(...) of character vectors evaluable at transpile time")
        }
        stop("mojor_transpile: array(dimnames=...) must be a list(...) of character vectors evaluable at transpile time")
    }
    dn_parts <- as.list(dimnames_expr)[-1]
    if (!is.null(expected_len) &&
        length(dn_parts) !=
            expected_len) {
        if (identical(owner, "matrix") && expected_len == 2L) {
            stop("mojor_transpile: matrix(dimnames=...) must have exactly 2 elements for rows and columns")
        }
        stop(sprintf("mojor_transpile: %s(dimnames=...) must have exactly %d elements", owner, expected_len))
    }
    parse_part <- function(part) {
        if (is.name(part) &&
            as.character(part) ==
                "NULL") {
            return(list(ok = TRUE, value = NULL))
        }
        if (is.character(part)) {
            return(list(ok = TRUE, value = as.character(part)))
        }
        vals <- tryCatch(
            eval(part, envir = .mojor_dimnames_eval_env),
            error = function(e) NULL
        )
        if (is.character(vals)) {
            return(list(ok = TRUE, value = as.character(vals)))
        }
        list(ok = FALSE, value = NULL)
    }
    parsed_parts <- lapply(dn_parts, parse_part)
    ok_all <- all(vapply(parsed_parts, function(x) isTRUE(x$ok), logical(1)))
    if (!ok_all) {
        if (identical(owner, "matrix")) {
            stop("mojor_transpile: matrix(dimnames=...) must be a list of character vectors (or NULL) evaluable at transpile time")
        }
        stop("mojor_transpile: array(dimnames=...) must be a list of character vectors (or NULL) evaluable at transpile time")
    }
    dim_names <- lapply(parsed_parts, function(x) x$value)
    out <- list(dim_names = dim_names)
    if (length(dim_names) >=
        1L) {
        out$row_names <- dim_names[[1L]]
    }
    if (length(dim_names) >=
        2L) {
        out$col_names <- dim_names[[2L]]
    }
    out
}

.mojor_parse_matrix_alloc <- function(expr, args, arg_specs) {
    if (!is.call(expr) ||
        as.character(expr[[1]]) !=
            "matrix") {
        stop("mojor_transpile: expected matrix() call")
    }
    parts <- as.list(expr)[-1]
    nms <- names(parts)
    get_arg <- function(nm, pos) {
        if (!is.null(nms) &&
            nm %in% nms) {
            return(parts[[which(nms == nm)[1]]])
        }
        if (length(parts) >=
            pos && (is.null(nms) ||
            is.null(nms[[pos]]) ||
            nms[[pos]] == "")) {
            return(parts[[pos]])
        }
        NULL
    }
    data_expr <- get_arg("data", 1)
    nrow_expr <- get_arg("nrow", 2)
    ncol_expr <- get_arg("ncol", 3)
    byrow_expr <- get_arg("byrow", 4)
    dimnames_expr <- get_arg("dimnames", 5)
    parse_byrow_literal <- function(value_expr) {
        if (is.null(value_expr)) {
            return(FALSE)
        }
        if (is.name(value_expr)) {
            nm <- as.character(value_expr)
            if (nm %in% c("TRUE", "FALSE")) {
                return(identical(nm, "TRUE"))
            }
            return(FALSE)
        }
        if (is.logical(value_expr) &&
            length(value_expr) == 1 &&
            !is.na(value_expr)) {
            return(isTRUE(value_expr))
        }
        if (is.numeric(value_expr) &&
            length(value_expr) == 1 &&
            !is.na(value_expr) &&
            value_expr %in% c(0, 1)) {
            return(isTRUE(as.logical(value_expr)))
        }
        FALSE
    }
    byrow <- parse_byrow_literal(byrow_expr)
    dimnames_list <- .mojor_parse_literal_dimnames(dimnames_expr, owner = "matrix", expected_len = 2L)
    if (is.null(nrow_expr) ||
        is.null(ncol_expr)) {
        stop("mojor_transpile: matrix() requires explicit nrow and ncol")
    }
    if (is.null(data_expr)) {
        stop("mojor_transpile: matrix() must provide a data value")
    }
    out_type <- "f64[]"
    if (is.integer(data_expr))
        out_type <- "i32[]"
    if (is.logical(data_expr))
        out_type <- "lgl[]"
    if (is.call(data_expr)) {
        op <- as.character(data_expr[[1]])
        if (op == "integer")
            out_type <- "i32[]"
        if (op == "logical")
            out_type <- "lgl[]"
        if (op == "numeric")
            out_type <- "f64[]"
    }
    if (is.name(data_expr)) {
        name <- as.character(data_expr)
        if (name %in% args && .mojor_is_array(arg_specs[[name]])) {
            spec <- arg_specs[[name]]
            if (spec == "i32[]")
                out_type <- "i32[]"
            if (spec == "lgl[]")
                out_type <- "lgl[]"
            if (spec == "f64[]" || spec == "f32[]")
                out_type <- "f64[]"
        }
    }
    .mojor_dim_src <- function(e, op_name) {
        if (!is.call(e) || length(e) != 2) {
            return(NULL)
        }
        op <- as.character(e[[1]])
        if (length(op) != 1 || !identical(op, op_name) || !is.name(e[[2]])) {
            return(NULL)
        }
        as.character(e[[2]])
    }
    fill_is_default_double <- is.numeric(data_expr) ||
        (is.call(data_expr) && identical(as.character(data_expr[[1]]), "numeric"))
    if (identical(out_type, "f64[]") && isTRUE(fill_is_default_double)) {
        nrow_src <- .mojor_dim_src(nrow_expr, "nrow")
        ncol_src <- .mojor_dim_src(ncol_expr, "ncol")
        if (!is.null(nrow_src) && identical(nrow_src, ncol_src) &&
            nrow_src %in% args) {
            src_spec <- arg_specs[[nrow_src]]
            if (!is.null(src_spec) && identical(src_spec, "f32[,]")) {
                out_type <- "f32[]"
            }
        }
    }
    data_name <- NULL
    if (is.name(data_expr)) {
        data_name <- as.character(data_expr)
    }
    list(
        type = out_type, nrow = nrow_expr, ncol = ncol_expr, dimnames = dimnames_list,
        data = data_name, byrow = byrow
    )
}

.mojor_parse_array_alloc <- function(expr, args, arg_specs) {
    if (!is.call(expr) ||
        as.character(expr[[1]]) !=
            "array") {
        stop("mojor_transpile: expected array() call")
    }
    parts <- as.list(expr)[-1]
    nms <- names(parts)
    get_arg <- function(nm, pos) {
        if (!is.null(nms) &&
            nm %in% nms) {
            return(parts[[which(nms == nm)[1]]])
        }
        if (length(parts) >=
            pos && (is.null(nms) ||
            is.null(nms[[pos]]) ||
            nms[[pos]] == "")) {
            return(parts[[pos]])
        }
        NULL
    }
    data_expr <- get_arg("data", 1)
    dim_expr <- get_arg("dim", 2)
    dimnames_expr <- get_arg("dimnames", 3)
    if (is.null(dim_expr)) {
        stop("mojor_transpile: array() requires dim=...")
    }
    dimnames_list <- .mojor_parse_literal_dimnames(dimnames_expr, owner = "array")
    if (is.null(data_expr)) {
        stop("mojor_transpile: array() must provide a data value")
    }
    out_type <- "f64[]"
    if (is.integer(data_expr))
        out_type <- "i32[]"
    if (is.logical(data_expr))
        out_type <- "lgl[]"
    if (is.call(data_expr)) {
        op <- as.character(data_expr[[1]])
        if (op == "integer")
            out_type <- "i32[]"
        if (op == "logical")
            out_type <- "lgl[]"
        if (op == "numeric")
            out_type <- "f64[]"
    }
    if (is.name(data_expr)) {
        name <- as.character(data_expr)
        if (name %in% args && .mojor_is_array(arg_specs[[name]])) {
            spec <- arg_specs[[name]]
            if (spec == "i32[]")
                out_type <- "i32[]"
            if (spec == "lgl[]")
                out_type <- "lgl[]"
            if (spec == "f64[]" || spec == "f32[]")
                out_type <- "f64[]"
        }
    }
    list(type = out_type, dim = dim_expr, dimnames = dimnames_list)
}

.mojor_dim_list_to_c <- function(expr, args, arg_specs) {
    if (is.call(expr) &&
        as.character(expr[[1]]) ==
            "c") {
        parts <- as.list(expr)[-1]
        if (length(parts) ==
            0)
            stop("mojor_transpile: dim=c() must have values")
        return(list(exprs = parts))
    }
    if (is.call(expr) &&
        as.character(expr[[1]]) ==
            "length") {
        return(list(exprs = list(expr)))
    }
    if (is.numeric(expr)) {
        val <- as.integer(expr)
        if (is.na(val) ||
            val < 0)
            stop("mojor_transpile: dim size must be non-negative integer")
        return(list(exprs = list(expr)))
    }
    if (is.name(expr)) {
        name <- as.character(expr)
        if (!(name %in% args))
            stop("mojor_transpile: array(dim=...) name must be a function argument")
        spec <- arg_specs[[name]]
        if (!is.null(spec) &&
            .mojor_is_array(spec)) {
            return(list(dim_name = name))
        }
    }
    stop("mojor_transpile: array(dim=...) must be c(...) or a dim name")
}

.mojor_dim_expr_to_target <- function(
    expr, args, arg_specs, len_var_map = NULL, n_source_name = NULL, target = c("c", "mojo")
) {
    target <- match.arg(target)
    out_key <- if (identical(target, "c"))
        "c_expr" else "mojo_expr"
    n_value <- if (identical(target, "c"))
        "__mojor_n" else "n_i"
    out <- function(value) stats::setNames(
        list(value),
        out_key
    )
    if_array <- function(name) {
        if (identical(target, "c"))
            paste0("LENGTH(", name, ")") else "n_i"
    }
    if_scalar <- function(name) {
        if (identical(target, "c"))
            paste0(name, "_val") else name
    }
    if_len_var <- function(name) {
        if (identical(target, "c"))
            if_array(name) else len_var_map[[name]]
    }
    resolve_array_alias <- function(name) .mojor_resolve_array_source_name(name, args, arg_specs)
    emit_minmax <- function(parts, op_name) {
        stopifnot(length(parts) >= 2L)
        if (identical(target, "mojo")) {
            fn <- if (op_name %in% c("min", "pmin")) "min" else "max"
            acc <- parts[[1L]]
            for (idx in 2:length(parts)) {
                acc <- paste0(fn, "(", acc, ", ", parts[[idx]], ")")
            }
            return(acc)
        }
        cmp <- if (op_name %in% c("min", "pmin")) "<" else ">"
        acc <- parts[[1L]]
        for (idx in 2:length(parts)) {
            rhs <- parts[[idx]]
            acc <- paste0("((", acc, ") ", cmp, " (", rhs, ") ? (", acc, ") : (", rhs, "))")
        }
        acc
    }

    if (is.numeric(expr)) {
        val <- as.integer(expr)
        if (is.na(val) ||
            val < 0)
            stop("mojor_transpile: dim size must be non-negative integer")
        return(out(as.character(val)))
    }
    if (is.call(expr) &&
        as.character(expr[[1]]) ==
            "(" && length(expr) ==
        2) {
        return(
            .mojor_dim_expr_to_target(
                expr[[2]], args, arg_specs, len_var_map = len_var_map,
                n_source_name = n_source_name, target = target
            )
        )
    }
    if (is.call(expr) &&
        as.character(expr[[1]]) %in%
            c("+", "-")) {
        lhs <- expr[[2]]
        rhs <- expr[[3]]
        base <- .mojor_dim_expr_to_target(
            lhs, args, arg_specs, len_var_map = len_var_map, n_source_name = n_source_name,
            target = target
        )[[out_key]]
        rhs_expr <- NULL
        if (is.numeric(rhs)) {
            val <- as.integer(rhs)
            if (is.na(val))
                stop("mojor_transpile: dim arithmetic requires integer literal")
            rhs_expr <- as.character(val)
        } else {
            rhs_expr <- .mojor_dim_expr_to_target(
                rhs, args, arg_specs, len_var_map = len_var_map, n_source_name = n_source_name,
                target = target
            )[[out_key]]
        }
        op <- as.character(expr[[1]])
        return(out(paste0("(", base, " ", op, " ", rhs_expr, ")")))
    }
    if (is.call(expr) &&
        as.character(expr[[1]]) %in%
            c("*", "/", "%/%", "%%")) {
        lhs <- expr[[2]]
        rhs <- expr[[3]]
        lhs_expr <- .mojor_dim_expr_to_target(
            lhs, args, arg_specs, len_var_map = len_var_map, n_source_name = n_source_name,
            target = target
        )[[out_key]]
        rhs_expr <- .mojor_dim_expr_to_target(
            rhs, args, arg_specs, len_var_map = len_var_map, n_source_name = n_source_name,
            target = target
        )[[out_key]]
        op <- as.character(expr[[1]])
        if (identical(op, "%/%")) {
            op <- "/"
        }
        return(out(paste0("(", lhs_expr, " ", op, " ", rhs_expr, ")")))
    }
    if (is.call(expr) &&
        as.character(expr[[1]]) %in%
            c("min", "max", "pmin", "pmax") &&
        length(expr) >= 2) {
        parts <- as.list(expr)[-1]
        expand_dim_arg <- function(part) {
            if (is.call(part) &&
                as.character(part[[1]]) == "dim" &&
                length(part) == 2 &&
                is.name(part[[2]])) {
                raw_name <- as.character(part[[2]])
                arr_name <- resolve_array_alias(raw_name)
                name <- if (!is.null(arr_name)) arr_name else raw_name
                if (!(name %in% args))
                    stop("mojor_transpile: dim() dim arg must be a function argument")
                spec <- arg_specs[[name]]
                if (!.mojor_is_array(spec))
                    stop("mojor_transpile: dim() dim arg must be an array")
                if (.mojor_is_matrix(spec)) {
                    if (identical(target, "c")) {
                        return(c(
                            paste0("Rf_nrows(", name, ")"),
                            paste0("Rf_ncols(", name, ")")
                        ))
                    }
                    return(c(
                        .mojor_nrow_var_name(name),
                        .mojor_ncol_var_name(name)
                    ))
                }
                rank <- .mojor_type_ndim(spec)
                if (is.na(rank) || rank <= 1L) {
                    stop("mojor_transpile: dim() reduction requires matrix or fixed-rank ND array argument")
                }
                if (identical(target, "c")) {
                    return(vapply(
                        seq_len(rank),
                        function(k) paste0("INTEGER(Rf_getAttrib(", name, ", R_DimSymbol))[", k - 1L, "]"),
                        character(1)
                    ))
                }
                dim_var <- .mojor_dim_var_name(name)
                return(vapply(
                    seq_len(rank),
                    function(k) paste0(dim_var, "[", k - 1L, "]"),
                    character(1)
                ))
            }
            .mojor_dim_expr_to_target(
                part, args, arg_specs, len_var_map = len_var_map, n_source_name = n_source_name,
                target = target
            )[[out_key]]
        }
        part_exprs <- unlist(lapply(parts, expand_dim_arg), use.names = FALSE)
        if (length(part_exprs) == 0L) {
            stop("mojor_transpile: min/max dim expression needs at least one argument")
        }
        if (length(part_exprs) == 1L) {
            return(out(part_exprs[[1L]]))
        }
        return(out(emit_minmax(as.list(part_exprs), as.character(expr[[1]]))))
    }
    if (is.name(expr)) {
        raw_name <- as.character(expr)
        if (!is.null(n_source_name) &&
            identical(raw_name, n_source_name)) {
            return(out(n_value))
        }
        arr_name <- resolve_array_alias(raw_name)
        name <- if (!is.null(arr_name))
            arr_name else raw_name
        spec <- if (name %in% args)
            arg_specs[[name]] else NULL
        has_len_var <- !is.null(len_var_map) &&
            !is.null(names(len_var_map)) &&
            name %in% names(len_var_map)
        if (has_len_var && !is.null(spec) &&
            .mojor_is_array(spec)) {
            return(out(if_len_var(name)))
        }
        if (!(name %in% args))
            stop("mojor_transpile: dim name must be a function argument")
        if (.mojor_is_array(spec)) {
            return(out(if_array(name)))
        }
        return(out(if_scalar(name)))
    }
    if (is.call(expr) &&
        as.character(expr[[1]]) ==
            "length") {
        arg <- expr[[2]]
        if (!is.name(arg))
            stop("mojor_transpile: length() dim arg must be a name")
        raw_name <- as.character(arg)
        if (!is.null(n_source_name) &&
            identical(raw_name, n_source_name)) {
            return(out(n_value))
        }
        arr_name <- resolve_array_alias(raw_name)
        name <- if (!is.null(arr_name))
            arr_name else raw_name
        spec <- if (name %in% args)
            arg_specs[[name]] else NULL
        has_len_var <- !is.null(len_var_map) &&
            !is.null(names(len_var_map)) &&
            name %in% names(len_var_map)
        if (has_len_var && !is.null(spec) &&
            .mojor_is_array(spec)) {
            return(out(if_len_var(name)))
        }
        if (!(name %in% args))
            stop("mojor_transpile: length() dim arg must be a function argument")
        if (!.mojor_is_array(spec))
            stop("mojor_transpile: length() dim arg must be an array")
        return(out(if_array(name)))
    }
    if (is.call(expr) &&
        as.character(expr[[1]]) %in%
            c("nrow", "ncol") &&
        length(expr) ==
            2) {
        arg <- expr[[2]]
        if (!is.name(arg))
            stop("mojor_transpile: nrow()/ncol() dim arg must be a name")
        name <- resolve_array_alias(as.character(arg))
        if (is.null(name))
            stop("mojor_transpile: nrow()/ncol() dim arg must be a function argument")
        spec <- arg_specs[[name]]
        if (!.mojor_is_array(spec))
            stop("mojor_transpile: nrow()/ncol() dim arg must be an array")
        op <- as.character(expr[[1]])
        if (identical(target, "c")) {
            return(
                out(
                  if (op == "nrow") paste0("Rf_nrows(", name, ")") else paste0("Rf_ncols(", name, ")")
              )
            )
        }
        return(
            out(
                if (op == "nrow") .mojor_nrow_var_name(name) else .mojor_ncol_var_name(name)
            )
        )
    }
    if (is.call(expr) &&
        as.character(expr[[1]]) %in%
            c("[", "[[") &&
        length(expr) ==
            3) {
        base <- expr[[2]]
        idx <- expr[[3]]
        if (is.call(base) &&
            as.character(base[[1]]) ==
                "dim" &&
            length(base) ==
                2 &&
            is.name(base[[2]])) {
            parse_dim_idx_literal <- function(node) {
                out <- NA_integer_
                if (is.integer(node) && length(node) == 1) {
                    out <- as.integer(node)
                } else if (is.numeric(node) &&
                    length(node) == 1 &&
                    abs(node - as.integer(node)) < 1e-12) {
                    out <- as.integer(node)
                }
                out
            }
            raw_name <- as.character(base[[2]])
            arr_name <- resolve_array_alias(raw_name)
            name <- if (!is.null(arr_name))
                arr_name else raw_name
            if (!(name %in% args))
                stop("mojor_transpile: dim() dim arg must be a function argument")
            spec <- arg_specs[[name]]
            if (!.mojor_is_array(spec))
                stop("mojor_transpile: dim() dim arg must be an array")
            dim_idx <- parse_dim_idx_literal(idx)
            if (!is.na(dim_idx) && dim_idx < 1L)
                stop("mojor_transpile: dim() index must be a positive integer literal or supported scalar expression")
            idx_expr_c <- NULL
            idx_expr_mojo <- NULL
                if (is.na(dim_idx)) {
                    if (identical(target, "c")) {
                    idx_expr_c <- .mojor_len_expr_to_c(idx, args, arg_specs, allow_scalar_cast = TRUE)
                } else {
                    idx_expr_mojo <- .mojor_scalar_range_expr_to_mojo(
                        idx,
                        arg_specs = arg_specs,
                        args = args,
                        len_var_map = len_var_map,
                        n_source_name = n_source_name,
                        scalar_inits = tryCatch(.mojor_state$current_scalar_inits, error = function(e) NULL),
                        nrow_var_map = tryCatch(.mojor_state$current_nrow_var_map, error = function(e) NULL),
                        ncol_var_map = tryCatch(.mojor_state$current_ncol_var_map, error = function(e) NULL),
                        dim_var_map = tryCatch(.mojor_state$current_dim_var_map, error = function(e) NULL),
                        allow_scalar_cast = TRUE
                    )
                }
            }
            if (.mojor_is_matrix(spec)) {
                if (!is.na(dim_idx) && dim_idx == 1L) {
                    return(
                        out(
                            if (identical(target, "c")) paste0("Rf_nrows(", name, ")") else .mojor_nrow_var_name(name)
                        )
                    )
                }
                if (!is.na(dim_idx) && dim_idx == 2L) {
                    return(
                        out(
                            if (identical(target, "c")) paste0("Rf_ncols(", name, ")") else .mojor_ncol_var_name(name)
                        )
                    )
                }
                if (!is.na(dim_idx))
                    stop("mojor_transpile: dim() index for matrix must be 1 or 2")
                if (identical(target, "c")) {
                    idx_int <- paste0("((int)(", idx_expr_c, "))")
                    return(
                        out(
                            paste0(
                                "((", idx_int, " >= 1 && ", idx_int,
                                " <= 2) ? INTEGER(Rf_getAttrib(", name, ", R_DimSymbol))[",
                                idx_int, " - 1] : (Rf_error(\"mojor_transpile: dim() index out of bounds\"), 0))"
                            )
                        )
                    )
                }
                return(
                    out(
                        paste0(
                            "((", .mojor_nrow_var_name(name), ") if (", idx_expr_mojo,
                            " == 1) else (", .mojor_ncol_var_name(name),
                            " if (", idx_expr_mojo, " == 2) else 0))"
                        )
                    )
                )
            }
            if (.mojor_type_ndim(spec) <= 1L)
                stop("mojor_transpile: dim() indexing requires matrix or fixed-rank ND array argument")
            rank <- .mojor_type_ndim(spec)
            if (!is.na(dim_idx) && dim_idx > rank)
                stop("mojor_transpile: dim() index exceeds declared array rank")
            if (identical(target, "c")) {
                if (is.na(dim_idx)) {
                    idx_int <- paste0("((int)(", idx_expr_c, "))")
                    return(
                        out(
                            paste0(
                                "((", idx_int, " >= 1 && ", idx_int,
                                " <= ", as.integer(rank), ") ? INTEGER(Rf_getAttrib(",
                                name, ", R_DimSymbol))[", idx_int,
                                " - 1] : (Rf_error(\"mojor_transpile: dim() index out of bounds\"), 0))"
                            )
                        )
                    )
                }
                return(
                    out(
                        paste0(
                            "INTEGER(Rf_getAttrib(", name, ", R_DimSymbol))[",
                            as.integer(dim_idx - 1L), "]"
                        )
                    )
                )
            }
            if (is.na(dim_idx)) {
                return(out(paste0(.mojor_dim_var_name(name), "[(", idx_expr_mojo, ") - 1]")))
            }
            return(out(paste0(.mojor_dim_var_name(name), "[", as.integer(dim_idx - 1L), "]")))
        }
    }
    stop(
        "mojor_transpile: unsupported matrix dim expression (use literal, name, length(name), nrow()/ncol(), or dim(name)[k])"
    )
}

.mojor_dim_expr_to_c <- function(expr, args, arg_specs, len_var_map = NULL, n_source_name = NULL) {
    .mojor_dim_expr_to_target(
        expr, args, arg_specs, len_var_map = len_var_map, n_source_name = n_source_name,
        target = "c"
    )
}

.mojor_len_expr_to_c <- function(expr, args, arg_specs, allow_scalar_cast = FALSE) {
    resolve_array_alias <- function(name) .mojor_resolve_array_source_name(name, args, arg_specs)
    local_scalar_type <- function(name) {
        local_scalars <- tryCatch(.mojor_state$current_scalar_inits, error = function(e) NULL)
        if (is.null(local_scalars) || is.null(local_scalars[[name]])) {
            return(NULL)
        }
        init_info <- local_scalars[[name]]
        init_info$type
    }
    is_local_i32_scalar <- function(name) {
        identical(local_scalar_type(name), "i32")
    }
    cond_expr_to_c <- function(cond_expr) {
        if (is.logical(cond_expr) &&
            length(cond_expr) == 1 &&
            !is.na(cond_expr)) {
            return(if (isTRUE(cond_expr)) "1" else "0")
        }
        if ((is.integer(cond_expr) || is.numeric(cond_expr)) &&
            length(cond_expr) == 1 &&
            !is.na(cond_expr)) {
            return(if (as.integer(cond_expr) == 0L) "0" else "1")
        }
        if (is.name(cond_expr)) {
            name <- as.character(cond_expr)
            arr_name <- resolve_array_alias(name)
            if (!is.null(arr_name)) {
                stop("mojor_transpile: loop-range condition cannot use array values")
            }
            if (is_local_i32_scalar(name)) {
                return(paste0("(", name, " != 0)"))
            }
            if (!(name %in% args)) {
                stop("mojor_transpile: loop-range condition must use function arguments")
            }
            spec <- arg_specs[[name]]
            if (.mojor_is_array(spec)) {
                stop("mojor_transpile: loop-range condition cannot use array values")
            }
            if (spec %in% c("lgl", "bool")) {
                return(paste0("(", name, "_val)"))
            }
            if (identical(spec, "i32")) {
                return(paste0("(", name, "_val != 0)"))
            }
            stop("mojor_transpile: loop-range condition must be scalar lgl/bool or i32")
        }
        if (!is.call(cond_expr)) {
            stop("mojor_transpile: loop-range condition must be a scalar logical expression")
        }
        op <- as.character(cond_expr[[1]])
        if (op == "(" && length(cond_expr) == 2) {
            return(cond_expr_to_c(cond_expr[[2]]))
        }
        if (op == "!" && length(cond_expr) == 2) {
            return(paste0("(!(", cond_expr_to_c(cond_expr[[2]]), "))"))
        }
        if (op %in% c("&&", "||") && length(cond_expr) == 3) {
            lhs <- cond_expr_to_c(cond_expr[[2]])
            rhs <- cond_expr_to_c(cond_expr[[3]])
            return(paste0("((", lhs, ") ", op, " (", rhs, "))"))
        }
        if (op %in% c("<", "<=", ">", ">=", "==", "!=") && length(cond_expr) == 3) {
            lhs <- .mojor_len_expr_to_c(cond_expr[[2]], args, arg_specs, allow_scalar_cast = allow_scalar_cast)
            rhs <- .mojor_len_expr_to_c(cond_expr[[3]], args, arg_specs, allow_scalar_cast = allow_scalar_cast)
            return(paste0("((", lhs, ") ", op, " (", rhs, "))"))
        }
        if (op == "as.logical" && length(cond_expr) == 2) {
            inner <- .mojor_len_expr_to_c(cond_expr[[2]], args, arg_specs, allow_scalar_cast = allow_scalar_cast)
            return(paste0("((", inner, ") != 0)"))
        }
        stop("mojor_transpile: loop-range condition must use !, &&, ||, comparisons, or as.logical()")
    }
    if (is.numeric(expr) &&
        length(expr) ==
            1) {
        val <- as.integer(expr)
        if (is.na(val))
            stop("mojor_transpile: length expression must be integer")
        return(as.character(val))
    }
        if (is.name(expr)) {
            name <- as.character(expr)
            arr_name <- resolve_array_alias(name)
            if (!is.null(arr_name)) {
                return(paste0("LENGTH(", arr_name, ")"))
            }
            local_spec <- local_scalar_type(name)
            if (!is.null(local_spec)) {
                if (identical(local_spec, "i32")) {
                    return(name)
                }
                if (isTRUE(allow_scalar_cast) && local_spec %in% c("f64", "f32")) {
                    return(paste0("((int)(", name, "))"))
                }
                if (isTRUE(allow_scalar_cast) && local_spec %in% c("lgl", "bool")) {
                    return(paste0("(((", name, ") != 0) ? 1 : 0)"))
                }
                stop("mojor_transpile: length expression scalar must be i32")
            }
            if (!(name %in% args))
                stop("mojor_transpile: length expression must use function arguments")
            spec <- arg_specs[[name]]
            if (.mojor_is_array(spec)) {
                return(paste0("LENGTH(", name, ")"))
            }
            if (identical(spec, "i32")) {
                return(paste0(name, "_val"))
            }
            if (isTRUE(allow_scalar_cast) && spec %in% c("f64", "f32")) {
                return(paste0("((int)(", name, "_val))"))
            }
            if (isTRUE(allow_scalar_cast) && spec %in% c("lgl", "bool")) {
                return(paste0("(((", name, "_val) != 0) ? 1 : 0)"))
            }
            if (spec != "i32")
                stop("mojor_transpile: length expression scalar must be i32")
            return(paste0(name, "_val"))
        }
        if (is.call(expr)) {
            op <- as.character(expr[[1]])
            if (op == "(" && length(expr) ==
                2) {
                return(.mojor_len_expr_to_c(expr[[2]], args, arg_specs, allow_scalar_cast = allow_scalar_cast))
            }
            if (op %in% c("ifelse", "if") && length(expr) == 4) {
                cond_c <- cond_expr_to_c(expr[[2]])
                yes_c <- .mojor_len_expr_to_c(expr[[3]], args, arg_specs, allow_scalar_cast = allow_scalar_cast)
                no_c <- .mojor_len_expr_to_c(expr[[4]], args, arg_specs, allow_scalar_cast = allow_scalar_cast)
                return(paste0("((", cond_c, ") ? (", yes_c, ") : (", no_c, "))"))
            }
            if (op == "as.integer" && length(expr) ==
                2) {
                return(.mojor_len_expr_to_c(expr[[2]], args, arg_specs, allow_scalar_cast = TRUE))
            }
            if (op %in% c("as.double", "as.single") && length(expr) ==
                2) {
                return(.mojor_len_expr_to_c(expr[[2]], args, arg_specs, allow_scalar_cast = TRUE))
            }
            if (op == "+" && length(expr) ==
                2) {
                return(.mojor_len_expr_to_c(expr[[2]], args, arg_specs, allow_scalar_cast = allow_scalar_cast))
            }
            if (op == "abs" && length(expr) ==
                2) {
                inner <- .mojor_len_expr_to_c(expr[[2]], args, arg_specs, allow_scalar_cast = allow_scalar_cast)
                return(paste0("(((", inner, ") < 0) ? (-(", inner, ")) : (", inner, "))"))
            }
            if (op == "-" && length(expr) ==
                2) {
                inner <- .mojor_len_expr_to_c(expr[[2]], args, arg_specs, allow_scalar_cast = allow_scalar_cast)
                return(paste0("(-", inner, ")"))
            }
            if (op == "length" && length(expr) ==
                2) {
                arg <- expr[[2]]
            if (!is.name(arg))
                stop("mojor_transpile: length() expression must use a name")
            name <- as.character(arg)
            arr_name <- resolve_array_alias(name)
            if (!is.null(arr_name)) {
                return(paste0("LENGTH(", arr_name, ")"))
            }
            if (!(name %in% args))
                stop("mojor_transpile: length() expression must use a function argument")
            spec <- arg_specs[[name]]
            if (!.mojor_is_array(spec))
                stop("mojor_transpile: length() expression requires an array argument")
            return(paste0("LENGTH(", name, ")"))
        }
        if (op %in% c("nrow", "ncol") &&
            length(expr) ==
                2) {
            arg <- expr[[2]]
            if (!is.name(arg))
                stop(paste0("mojor_transpile: ", op, "() expression must use a name"))
            name <- resolve_array_alias(as.character(arg))
            if (is.null(name))
                stop(
                  paste0(
                    "mojor_transpile: ", op, "() expression must use a function argument"
                )
              )
            spec <- arg_specs[[name]]
            if (!.mojor_is_array(spec))
                stop(
                  paste0("mojor_transpile: ", op, "() expression requires an array argument")
              )
            return(
                if (op == "nrow") paste0("Rf_nrows(", name, ")") else paste0("Rf_ncols(", name, ")")
            )
        }
        if (op %in% c("[", "[[") &&
            length(expr) ==
                3) {
            base <- expr[[2]]
            idx <- expr[[3]]
            if (is.call(base) &&
              as.character(base[[1]]) == "dim" &&
              length(base) == 2 &&
              is.name(base[[2]])) {
                parse_dim_idx_literal <- function(node) {
                  out <- NA_integer_
                  if (is.integer(node) && length(node) == 1) {
                    out <- as.integer(node)
                  } else if (is.numeric(node) &&
                    length(node) == 1 &&
                    abs(node - as.integer(node)) < 1e-12) {
                    out <- as.integer(node)
                  }
                  out
                }
                name <- resolve_array_alias(as.character(base[[2]]))
                if (is.null(name))
                  stop("mojor_transpile: dim() expression must use a function argument")
                spec <- arg_specs[[name]]
                if (!.mojor_is_array(spec))
                  stop("mojor_transpile: dim() expression requires an array argument")
                dim_idx <- parse_dim_idx_literal(idx)
                if (!is.na(dim_idx) && dim_idx < 1L)
                  stop("mojor_transpile: dim() index must be a positive integer literal or supported scalar expression")
                if (.mojor_is_matrix(spec)) {
                  if (!is.na(dim_idx) && dim_idx == 1L)
                    return(paste0("Rf_nrows(", name, ")"))
                  if (!is.na(dim_idx) && dim_idx == 2L)
                    return(paste0("Rf_ncols(", name, ")"))
                  if (!is.na(dim_idx))
                    stop("mojor_transpile: dim() index for matrix must be 1 or 2")
                }
                if (.mojor_type_ndim(spec) <= 1L)
                  stop("mojor_transpile: dim() indexing requires matrix or fixed-rank ND array argument")
                rank <- .mojor_type_ndim(spec)
                if (!is.na(dim_idx) && dim_idx > rank)
                  stop("mojor_transpile: dim() index exceeds declared array rank")
                if (is.na(dim_idx)) {
                    idx_expr <- .mojor_len_expr_to_c(idx, args, arg_specs, allow_scalar_cast = TRUE)
                    idx_int <- paste0("((int)(", idx_expr, "))")
                    return(
                        paste0(
                            "((", idx_int, " >= 1 && ", idx_int, " <= ", as.integer(rank),
                            ") ? INTEGER(Rf_getAttrib(", name, ", R_DimSymbol))[",
                            idx_int, " - 1] : (Rf_error(\"mojor_transpile: dim() index out of bounds\"), 0))"
                        )
                    )
                }
                return(
                    paste0(
                      "INTEGER(Rf_getAttrib(", name, ", R_DimSymbol))[",
                      as.integer(dim_idx - 1L), "]"
                  )
                )
            }
        }
        if (op %in% c("+", "-", "*") &&
            length(expr) ==
                3) {
            lhs <- .mojor_len_expr_to_c(expr[[2]], args, arg_specs, allow_scalar_cast = allow_scalar_cast)
            rhs <- .mojor_len_expr_to_c(expr[[3]], args, arg_specs, allow_scalar_cast = allow_scalar_cast)
            return(paste0("(", lhs, " ", op, " ", rhs, ")"))
        }
        if (op %in% c("%/%", "%%") &&
            length(expr) ==
                3) {
            lhs <- .mojor_len_expr_to_c(expr[[2]], args, arg_specs, allow_scalar_cast = allow_scalar_cast)
            rhs <- .mojor_len_expr_to_c(expr[[3]], args, arg_specs, allow_scalar_cast = allow_scalar_cast)
            c_op <- if (op == "%/%")
                "/" else "%"
            return(paste0("(", lhs, " ", c_op, " ", rhs, ")"))
        }
        if (op %in% c("min", "max", "pmin", "pmax") &&
            length(expr) >=
                2) {
            vals <- as.list(expr)[-1]
            expand_dim_arg <- function(v) {
                if (is.call(v) &&
                    as.character(v[[1]]) == "dim" &&
                    length(v) == 2 &&
                    is.name(v[[2]])) {
                    name <- resolve_array_alias(as.character(v[[2]]))
                    if (is.null(name)) {
                        stop("mojor_transpile: dim() expression must use a function argument")
                    }
                    spec <- arg_specs[[name]]
                    if (!.mojor_is_array(spec)) {
                        stop("mojor_transpile: dim() expression requires an array argument")
                    }
                    if (.mojor_is_matrix(spec)) {
                        return(c(
                            paste0("Rf_nrows(", name, ")"),
                            paste0("Rf_ncols(", name, ")")
                        ))
                    }
                    rank <- .mojor_type_ndim(spec)
                    if (rank <= 1L) {
                        stop("mojor_transpile: dim() reduction in loop range requires matrix or fixed-rank ND array")
                    }
                    return(vapply(
                        seq_len(rank),
                        function(k) paste0("INTEGER(Rf_getAttrib(", name, ", R_DimSymbol))[", k - 1L, "]"),
                        character(1)
                    ))
                }
                .mojor_len_expr_to_c(v, args, arg_specs, allow_scalar_cast = allow_scalar_cast)
            }
            parts <- unlist(lapply(vals, expand_dim_arg), use.names = FALSE)
            if (length(parts) == 0L) {
                stop("mojor_transpile: min/max loop range expression needs at least one scalar-compatible argument")
            }
            cmp <- if (op %in% c("min", "pmin"))
                "<" else ">"
            expr_c <- parts[[1]]
            if (length(parts) >
                1) {
                for (p in parts[-1]) {
                  expr_c <- paste0(
                    "((", expr_c, " ", cmp, " ", p, ") ? ", expr_c, " : ",
                    p, ")"
                )
                }
            }
            return(expr_c)
            }
        }
    stop(
        "mojor_transpile: unsupported length expression (use length(name), dim(name)[k], scalar name, literal, abs(), unary +/-, optional as.integer()/as.double()/as.single() casts, +, -, *, %/%, %%, min/max, or scalar ifelse()/if())"
    )
}

.mojor_float_expr_to_c <- function(expr, args, arg_specs) {
    resolve_array_alias <- function(name) .mojor_resolve_array_source_name(name, args, arg_specs)
    if (is.numeric(expr) &&
        length(expr) ==
            1) {
        return(as.character(as.double(expr)))
    }
    if (is.integer(expr) &&
        length(expr) ==
            1) {
        return(as.character(as.double(expr)))
    }
    if (is.name(expr)) {
        name <- as.character(expr)
        arr_name <- resolve_array_alias(name)
        if (!is.null(arr_name)) {
            return(paste0("(double)LENGTH(", arr_name, ")"))
        }
        if (!(name %in% args))
            stop("mojor_transpile: float expression must use function arguments")
        spec <- arg_specs[[name]]
        if (.mojor_is_array(spec)) {
            return(paste0("(double)LENGTH(", name, ")"))
        }
        if (!spec %in% c("i32", "f64", "f32"))
            stop("mojor_transpile: float expression scalar must be i32/f64/f32")
        return(paste0("(double)", name, "_val"))
    }
    if (is.call(expr)) {
        op <- as.character(expr[[1]])
        if (op == "(" && length(expr) ==
            2) {
            return(.mojor_float_expr_to_c(expr[[2]], args, arg_specs))
        }
        if (op == "as.double" && length(expr) ==
            2) {
            return(.mojor_float_expr_to_c(expr[[2]], args, arg_specs))
        }
        if (op == "as.single" && length(expr) ==
            2) {
            return(.mojor_float_expr_to_c(expr[[2]], args, arg_specs))
        }
        if (op == "as.integer" && length(expr) ==
            2) {
            inner <- .mojor_float_expr_to_c(expr[[2]], args, arg_specs)
            return(paste0("(double)((int)", inner, ")"))
        }
        if (op == "-" && length(expr) ==
            2) {
            inner <- .mojor_float_expr_to_c(expr[[2]], args, arg_specs)
            return(paste0("(-", inner, ")"))
        }
        if (op == "length" && length(expr) ==
            2) {
            arg <- expr[[2]]
            if (!is.name(arg))
                stop("mojor_transpile: length() expression must use a name")
            name <- as.character(arg)
            arr_name <- resolve_array_alias(name)
            if (!is.null(arr_name)) {
                return(paste0("(double)LENGTH(", arr_name, ")"))
            }
            if (!(name %in% args))
                stop("mojor_transpile: length() expression must use a function argument")
            spec <- arg_specs[[name]]
            if (!.mojor_is_array(spec))
                stop("mojor_transpile: length() expression requires an array argument")
            return(paste0("(double)LENGTH(", name, ")"))
        }
        if (op %in% c("nrow", "ncol") &&
            length(expr) ==
                2) {
            arg <- expr[[2]]
            if (!is.name(arg))
                stop(paste0("mojor_transpile: ", op, "() expression must use a name"))
            name <- resolve_array_alias(as.character(arg))
            if (is.null(name))
                stop(
                  paste0(
                    "mojor_transpile: ", op, "() expression must use a function argument"
                )
              )
            spec <- arg_specs[[name]]
            if (!.mojor_is_array(spec))
                stop(
                  paste0("mojor_transpile: ", op, "() expression requires an array argument")
              )
            return(
                if (op == "nrow") paste0("(double)Rf_nrows(", name, ")") else paste0("(double)Rf_ncols(", name, ")")
            )
        }
        if (op %in% c("[", "[[") &&
            length(expr) ==
                3) {
            base <- expr[[2]]
            if (is.call(base) &&
              as.character(base[[1]]) == "dim" &&
              length(base) == 2 &&
              is.name(base[[2]])) {
                return(paste0("(double)(", .mojor_len_expr_to_c(expr, args, arg_specs), ")"))
            }
        }
        if (op %in% c("+", "-", "*", "/") &&
            length(expr) ==
                3) {
            lhs <- .mojor_float_expr_to_c(expr[[2]], args, arg_specs)
            rhs <- .mojor_float_expr_to_c(expr[[3]], args, arg_specs)
            return(paste0("(", lhs, " ", op, " ", rhs, ")"))
        }
        if (op %in% c("%/%", "%%") &&
            length(expr) ==
                3) {
            lhs <- .mojor_float_expr_to_c(expr[[2]], args, arg_specs)
            rhs <- .mojor_float_expr_to_c(expr[[3]], args, arg_specs)
            if (op == "%/%") {
                return(paste0("floor((", lhs, ") / (", rhs, "))"))
            }
            return(paste0("fmod((", lhs, "), (", rhs, "))"))
        }
        if (op %in% c("min", "max", "pmin", "pmax") &&
            length(expr) >=
                3) {
            vals <- as.list(expr)[-1]
            parts <- vapply(
                vals, function(v) .mojor_float_expr_to_c(v, args, arg_specs),
                character(1)
            )
            cmp <- if (op %in% c("min", "pmin"))
                "<" else ">"
            expr_c <- parts[[1]]
            if (length(parts) >
                1) {
                for (p in parts[-1]) {
                  expr_c <- paste0(
                    "((", expr_c, " ", cmp, " ", p, ") ? ", expr_c, " : ",
                    p, ")"
                )
                }
            }
            return(expr_c)
        }
    }
    stop(
        "mojor_transpile: unsupported float expression (use literals, scalar names, length(name), nrow()/ncol(), dim(name)[k], +, -, *, /, %/%, %%, min/max)"
    )
}

.mojor_dim_expr_to_mojo <- function(expr, args, arg_specs, len_var_map = NULL, n_source_name = NULL) {
    .mojor_dim_expr_to_target(
        expr, args, arg_specs, len_var_map = len_var_map, n_source_name = n_source_name,
        target = "mojo"
    )
}

.mojor_collect_loop_seq_infos <- function(blocks, arg_specs, args, scalar_inits = list()) {
    infos <- list()
    # Get seq aliases from state
    seq_aliases <- tryCatch(.mojor_state$current_seq_aliases, error = function(e) NULL)
    for (b in blocks) {
        if (is.call(b) &&
            as.character(b[[1]]) ==
                "for") {
            loop_seq <- b[[3]]
            # Check if loop sequence is a seq alias
            if (is.name(loop_seq)) {
                seq_name <- as.character(loop_seq)
                if (!is.null(seq_aliases) && !is.null(seq_aliases[[seq_name]])) {
                    loop_seq <- seq_aliases[[seq_name]]
                }
            }
            info <- .mojor_parse_loop_seq(loop_seq, arg_specs, args, scalar_inits)
            loop_var <- as.character(b[[2]])
            child_arg_specs <- arg_specs
            child_args <- args
            child_scalar_inits <- scalar_inits
            if (!is.null(loop_var) &&
                nzchar(loop_var)) {
                child_arg_specs[[loop_var]] <- "i32"
                if (!(loop_var %in% child_args))
                  child_args <- c(child_args, loop_var)
                # Add loop variable to scalar_inits so it's recognized as i32
                if (is.null(child_scalar_inits[[loop_var]])) {
                    child_scalar_inits[[loop_var]] <- list(type = "i32", value = list(kind = "loop_var", name = loop_var))
                }
            }
            infos <- c(infos, list(info))
            body_blocks <- .mojor_extract_block(b[[4]])
            if (length(body_blocks) >
                0) {
                # Scan body blocks for local scalar assignments and add them to child_scalar_inits
                # This enables computed bounds like: inner_bound <- min(i * 2, m); for (j in seq_len(inner_bound))
                child_scalar_inits <- .mojor_collect_body_scalar_inits(body_blocks, child_arg_specs, child_args, child_scalar_inits)
                infos <- c(
                  infos, .mojor_collect_loop_seq_infos(body_blocks, child_arg_specs, child_args, child_scalar_inits)
              )
            }
        } else if (is.call(b) &&
            as.character(b[[1]]) ==
                "if") {
            then_blocks <- .mojor_extract_block(b[[3]])
            if (length(then_blocks) >
                0) {
                infos <- c(
                  infos, .mojor_collect_loop_seq_infos(then_blocks, arg_specs, args, scalar_inits)
              )
            }
            if (length(b) >=
                4) {
                else_blocks <- .mojor_extract_block(b[[4]])
                if (length(else_blocks) >
                  0) {
                  infos <- c(infos, .mojor_collect_loop_seq_infos(else_blocks, arg_specs, args, scalar_inits))
                }
            }
        } else if (is.call(b) &&
            as.character(b[[1]]) ==
                "while") {
            body_blocks <- .mojor_extract_block(b[[3]])
            if (length(body_blocks) >
                0) {
                infos <- c(infos, .mojor_collect_loop_seq_infos(body_blocks, arg_specs, args, scalar_inits))
            }
        }
    }
    infos
}

.mojor_constructor_info <- function(blocks, args, types) {
    if (length(blocks) ==
        0) {
        return(NULL)
    }
    is_ctor_call <- function(expr) {
        is.call(expr) &&
            as.character(expr[[1]]) %in%
                c("rep", "rep_len", "rep.int", "c")
    }
    parse_len_arg <- function(expr, label) {
        if (is.numeric(expr) &&
            length(expr) ==
                1) {
            return(expr)
        }
        if (is.name(expr)) {
            name <- as.character(expr)
            if (!(name %in% args))
                stop(
                  sprintf(
                    "mojor_transpile: %s must be a function argument",
                    label
                )
              )
            spec <- types[[name]]
            if (.mojor_is_array(spec))
                stop(sprintf("mojor_transpile: %s must be a scalar i32", label))
            if (spec != "i32")
                stop(sprintf("mojor_transpile: %s must be a scalar i32", label))
            return(expr)
        }
        stop(
            sprintf(
                "mojor_transpile: %s must be a scalar name or integer literal",
                label
            )
        )
    }
    len_of_value <- function(expr) {
        if (is.numeric(expr) ||
            is.integer(expr) ||
            is.logical(expr)) {
            return(1L)
        }
        if (is.name(expr)) {
            name <- as.character(expr)
            spec <- types[[name]]
            if (!is.null(spec) &&
                .mojor_is_array(spec)) {
                return(call("length", as.name(name)))
            }
            return(1L)
        }
        stop(
            "mojor_transpile: constructor values must be literals or typed names"
        )
    }
    collect_arrays <- function(expr) {
        arrays <- character(0)
        if (is.name(expr)) {
            name <- as.character(expr)
            spec <- types[[name]]
            if (!is.null(spec) &&
                .mojor_is_array(spec))
                arrays <- c(arrays, name)
        }
        arrays
    }
    add_expr <- function(lhs, rhs) {
        if (is.null(lhs)) {
            return(rhs)
        }
        if (is.null(rhs)) {
            return(lhs)
        }
        call("+", lhs, rhs)
    }
    mul_expr <- function(lhs, rhs) {
        if (is.null(lhs) ||
            is.null(rhs)) {
            return(NULL)
        }
        call("*", lhs, rhs)
    }
    parse_ctor_expr <- function(expr) {
        if (!is_ctor_call(expr)) {
            return(NULL)
        }
        op <- as.character(expr[[1]])
        if (op == "c") {
            parts <- as.list(expr)[-1]
            if (length(parts) ==
                0)
                stop("mojor_transpile: c() requires at least one argument")
            len_expr <- NULL
            len_arrays <- character(0)
            for (p in parts) {
                len_expr <- add_expr(len_expr, len_of_value(p))
                len_arrays <- unique(c(len_arrays, collect_arrays(p)))
            }
            return(list(expr = expr, len_expr = len_expr, len_arrays = len_arrays))
        }
        if (op %in% c("rep", "rep.int", "rep_len")) {
            if (length(expr) <
                2)
                stop(paste0("mojor_transpile: ", op, "() requires a value"))
            parts <- as.list(expr)[-1]
            nms <- names(parts)
            get_arg <- function(nm, pos) {
                if (!is.null(nms) &&
                  nm %in% nms) {
                  return(parts[[which(nms == nm)[1]]])
                }
                if (length(parts) >=
                  pos && (is.null(nms) ||
                  is.null(nms[[pos]]) ||
                  nms[[pos]] == "")) {
                  return(parts[[pos]])
                }
                NULL
            }
            val_expr <- get_arg("x", 1)
            if (is.null(val_expr))
                stop(paste0("mojor_transpile: ", op, "() requires a value"))
            len_base <- len_of_value(val_expr)
            len_arrays <- collect_arrays(val_expr)
            if (op == "rep_len") {
                len_out <- get_arg("length.out", 2)
                if (is.null(len_out))
                  stop("mojor_transpile: rep_len() requires length.out")
                len_expr <- parse_len_arg(len_out, "rep_len() length.out")
                return(list(expr = expr, len_expr = len_expr, len_arrays = len_arrays))
            }
            length_out <- get_arg("length.out", 3)
            each <- get_arg("each", 4)
            each_expr <- NULL
            if (!is.null(each)) {
                each_expr <- parse_len_arg(each, paste0(op, "() each"))
            }
            if (!is.null(length_out)) {
                len_expr <- parse_len_arg(length_out, paste0(op, "() length.out"))
                if (!is.null(each_expr) &&
                  isTRUE(.mojor_state$options$warn_constructor)) {
                  .mojor_warn(
                    paste0(op, "() length.out overrides each="),
                    expr, "each ignored when length.out is set"
                )
                }
                return(list(expr = expr, len_expr = len_expr, len_arrays = len_arrays))
            }
            times <- get_arg("times", 2)
            if (is.null(times)) {
                len_expr <- len_base
            } else {
                times_expr <- parse_len_arg(times, paste0(op, "() times"))
                len_expr <- mul_expr(len_base, times_expr)
            }
            if (!is.null(each_expr)) {
                len_expr <- mul_expr(len_expr, each_expr)
            }
            return(list(expr = expr, len_expr = len_expr, len_arrays = len_arrays))
        }
        NULL
    }
    parse_stmt <- function(stmt) {
        if (is.call(stmt) &&
            as.character(stmt[[1]]) %in%
                c("<-", "=")) {
            lhs <- stmt[[2]]
            rhs <- stmt[[3]]
            if (is.name(lhs)) {
                out_name <- as.character(lhs)
                ctor <- parse_ctor_expr(rhs)
                if (!is.null(ctor)) {
                  return(
                    c(
                      list(out_name = out_name),
                      ctor
                  )
                )
                }
            }
        }
        if (is.call(stmt) &&
            as.character(stmt[[1]]) ==
                "return" && length(stmt) >=
            2) {
            ctor <- parse_ctor_expr(stmt[[2]])
            if (!is.null(ctor)) {
                return(
                  c(
                    list(out_name = NULL),
                    ctor
                )
              )
            }
        }
        ctor <- parse_ctor_expr(stmt)
        if (!is.null(ctor)) {
            return(
                c(
                  list(out_name = NULL),
                  ctor
              )
            )
        }
        NULL
    }
    info <- NULL
    if (length(blocks) ==
        1) {
        info <- parse_stmt(blocks[[1]])
    } else if (length(blocks) ==
        2) {
        first <- parse_stmt(blocks[[1]])
        if (!is.null(first) &&
            !is.null(first$out_name)) {
            second <- blocks[[2]]
            if (is.name(second) &&
                as.character(second) ==
                  first$out_name) {
                info <- first
            } else if (is.call(second) &&
                as.character(second[[1]]) ==
                  "return" && length(second) >=
                2 && is.name(second[[2]]) &&
                as.character(second[[2]]) ==
                  first$out_name) {
                info <- first
            }
        }
    }
    if (is.null(info)) {
        return(NULL)
    }
    expr_elem_kind <- function(expr) {
        if (is.numeric(expr)) {
            return("float")
        }
        if (is.integer(expr)) {
            return("int")
        }
        if (is.logical(expr)) {
            return("bool")
        }
        if (is.name(expr)) {
            name <- as.character(expr)
            spec <- types[[name]]
            if (!is.null(spec) &&
                .mojor_is_array(spec)) {
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
            return(.mojor_expr_kind(expr, types))
        }
        if (is.call(expr)) {
            op <- as.character(expr[[1]])
            if (op %in% c("rep", "rep_len", "rep.int")) {
                if (length(expr) <
                  2) {
                  return("unknown")
                }
                parts <- as.list(expr)[-1]
                nms <- names(parts)
                get_arg <- function(nm, pos) {
                  if (!is.null(nms) &&
                    nm %in% nms) {
                    return(parts[[which(nms == nm)[1]]])
                  }
                  if (length(parts) >=
                    pos && (is.null(nms) ||
                    is.null(nms[[pos]]) ||
                    nms[[pos]] == "")) {
                    return(parts[[pos]])
                  }
                  NULL
                }
                val <- get_arg("x", 1)
                if (is.null(val)) {
                  return("unknown")
                }
                return(expr_elem_kind(val))
            }
            if (op == "c") {
                parts <- as.list(expr)[-1]
                if (length(parts) ==
                  0) {
                  return("unknown")
                }
                kinds <- vapply(parts, expr_elem_kind, character(1))
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
            }
        }
        .mojor_expr_kind(expr, types)
    }
    kind <- expr_elem_kind(info$expr)
    if (kind == "float") {
        out_type <- "f64[]"
    } else if (kind == "int") {
        out_type <- "i32[]"
    } else if (kind == "bool") {
        out_type <- "lgl[]"
    } else {
        stop(
            "mojor_transpile: constructor output type must be numeric, integer, or logical"
        )
    }
    info$out_type <- out_type
    info
}


# Helper: Collect all loop variables from blocks recursively
# Returns a character vector of loop variable names
.mojor_collect_loop_vars <- function(blocks) {
    vars <- character(0)
    
    collect_from_expr <- function(expr) {
        if (!is.call(expr)) {
            return(character(0))
        }
        op <- as.character(expr[[1]])
        if (length(op) != 1) op <- op[[1]]
        
        if (op == "for" && length(expr) >= 4) {
            loop_var <- as.character(expr[[2]])
            vars <<- c(vars, loop_var)
            # Also collect from body
            body_blocks <- .mojor_extract_block(expr[[4]])
            if (length(body_blocks) > 0) {
                vars <<- c(vars, .mojor_collect_loop_vars(body_blocks))
            }
            # Collect from sequence expression too
            vars <<- c(vars, collect_from_expr(expr[[3]]))
        } else if (op == "if" && length(expr) >= 3) {
            then_blocks <- .mojor_extract_block(expr[[3]])
            vars <<- c(vars, .mojor_collect_loop_vars(then_blocks))
            if (length(expr) >= 4) {
                else_blocks <- .mojor_extract_block(expr[[4]])
                vars <<- c(vars, .mojor_collect_loop_vars(else_blocks))
            }
        } else if (op == "while" && length(expr) >= 3) {
            body_blocks <- .mojor_extract_block(expr[[3]])
            if (length(body_blocks) > 0) {
                vars <<- c(vars, .mojor_collect_loop_vars(body_blocks))
            }
        } else if (op == "repeat" && length(expr) >= 2) {
            body_blocks <- .mojor_extract_block(expr[[2]])
            if (length(body_blocks) > 0) {
                vars <<- c(vars, .mojor_collect_loop_vars(body_blocks))
            }
        }
        vars
    }
    
    for (b in blocks) {
        collect_from_expr(b)
    }
    unique(vars)
}

# Helper: Scan body blocks for local scalar assignments
# Returns updated scalar_inits with local i32 variables added
.mojor_collect_body_scalar_inits <- function(blocks, arg_specs, args, scalar_inits = list()) {
    result <- scalar_inits
    resolve_array_source_name <- function(name) {
        .mojor_resolve_array_source_name(name, args, arg_specs)
    }
    is_dim_vector_expr <- function(expr) {
        if (!is.call(expr) ||
            as.character(expr[[1]]) != "dim" ||
            length(expr) != 2 ||
            !is.name(expr[[2]])) {
            return(FALSE)
        }
        arr_name <- resolve_array_source_name(as.character(expr[[2]]))
        if (is.null(arr_name)) {
            return(FALSE)
        }
        spec <- arg_specs[[arr_name]]
        if (is.null(spec) || !.mojor_is_array(spec)) {
            return(FALSE)
        }
        .mojor_is_matrix(spec) || .mojor_type_ndim(spec) > 1L
    }
    
    # Helper to check if an expression is a valid i32 scalar expression
    is_i32_expr <- function(expr) {
        if (is.integer(expr) && length(expr) == 1 && !is.na(expr)) {
            return(TRUE)
        }
        if (is.numeric(expr) && length(expr) == 1) {
            val <- as.integer(expr)
            return(!is.na(val) && abs(expr - val) < 1e-12)
        }
        if (is.name(expr)) {
            nm <- as.character(expr)
            if (nm %in% args) {
                spec <- arg_specs[[nm]]
                return(!is.null(spec) && !.mojor_is_array(spec) && spec == "i32")
            }
            if (!is.null(result[[nm]]) && !is.null(result[[nm]]$type)) {
                return(identical(result[[nm]]$type, "i32"))
            }
            return(FALSE)
        }
        if (is.call(expr)) {
            op <- as.character(expr[[1]])
            if (op == "(" && length(expr) == 2) {
                return(is_i32_expr(expr[[2]]))
            }
            if (op == "as.integer" && length(expr) == 2) {
                return(is_i32_expr(expr[[2]]))
            }
            if (op == "-" && length(expr) == 2) {
                return(is_i32_expr(expr[[2]]))
            }
            if (op %in% c("+", "-", "*", "%/%", "%%") && length(expr) == 3) {
                return(is_i32_expr(expr[[2]]) && is_i32_expr(expr[[3]]))
            }
            if (op %in% c("min", "max", "pmin", "pmax") && length(expr) >= 2) {
                vals <- as.list(expr)[-1]
                return(length(vals) > 0 && all(vapply(
                    vals,
                    function(v) is_i32_expr(v) || is_dim_vector_expr(v),
                    logical(1)
                )))
            }
        }
        FALSE
    }
    
    # Walk blocks looking for scalar assignments
    walk <- function(block_items) {
        for (stmt in block_items) {
            if (!is.call(stmt)) next
            
            op <- as.character(stmt[[1]])
            
            # Handle assignment
            if (op %in% c("<-", "=") && length(stmt) >= 3) {
                lhs <- stmt[[2]]
                rhs <- stmt[[3]]
                
                if (is.name(lhs)) {
                    lhs_name <- as.character(lhs)
                    # Skip if it's a function argument or already tracked
                    if (!(lhs_name %in% args) && is.null(result[[lhs_name]])) {
                        # Check if RHS is a valid i32 expression
                        if (is_i32_expr(rhs)) {
                            result[[lhs_name]] <<- list(type = "i32", value = list(kind = "expr", expr = rhs))
                        }
                    }
                }
                
                # Also walk the RHS for nested expressions
                walk(list(rhs))
            }
            # Handle for loops - don't recurse into body here, just note the loop var
            else if (op == "for" && length(stmt) >= 4) {
                # Loop var is already in scalar_inits from parent
                # Just walk the sequence expression
                walk(list(stmt[[3]]))
            }
            # Handle if statements
            else if (op == "if" && length(stmt) >= 3) {
                then_blocks <- .mojor_extract_block(stmt[[3]])
                walk(then_blocks)
                if (length(stmt) >= 4) {
                    else_blocks <- .mojor_extract_block(stmt[[4]])
                    walk(else_blocks)
                }
            }
            # Handle while loops
            else if (op == "while" && length(stmt) >= 3) {
                walk(list(stmt[[2]]))  # condition
                body_blocks <- .mojor_extract_block(stmt[[3]])
                walk(body_blocks)
            }
            # Handle repeat loops
            else if (op == "repeat" && length(stmt) >= 2) {
                body_blocks <- .mojor_extract_block(stmt[[2]])
                walk(body_blocks)
            }
            # Generic: walk all parts
            else {
                for (part in as.list(stmt)[-1]) {
                    is_missing_part <- tryCatch(
                        identical(part, quote(expr =)),
                        error = function(e) TRUE
                    )
                    if (isTRUE(is_missing_part)) {
                        next
                    }
                    if (is.call(part) || is.list(part)) {
                        walk(list(part))
                    }
                }
            }
        }
    }
    
    walk(blocks)
    result
}

#' Scan loop body statements for local array allocations and hoist them.
#'
#' Recursively walks loop body statements looking for `temp <- numeric(K)`,
#' `temp <- integer(K)`, or `temp <- logical(K)` patterns. When found, the
#' allocation is registered in `.mojor_state$current_local_vector_lengths`
#' so that the emission pass can allocate the array at function scope
#' (hoisted before the loop) and emit a re-initialization inside the loop body.
#'
#' @param stmts List of AST statements from the loop body.
#' @param args Character vector of function parameter names.
#' @param types Named list of type hints.
#' @param scalar_inits Named list of tracked scalar initializations.
.mojor_scan_loop_body_allocs <- function(stmts, args, types, scalar_inits) {
    for (s in stmts) {
        if (!is.call(s)) next
        op <- as.character(s[[1]])
        if (length(op) != 1) op <- op[[1]]

        # Assignment: temp <- numeric(K)
        if (op %in% c("<-", "=") && length(s) >= 3) {
            lhs_expr <- s[[2]]
            if (!is.name(lhs_expr)) next
            lhs <- as.character(lhs_expr)
            if (lhs %in% args) next
            rhs <- s[[3]]
            if (is.call(rhs) && length(rhs) >= 1) {
                rhs_op <- as.character(rhs[[1]])
                if (length(rhs_op) != 1) rhs_op <- rhs_op[[1]]
                if (rhs_op %in% c("numeric", "integer", "logical")) {
                    out_type <- switch(rhs_op,
                        integer = "i32[]",
                        logical = "lgl[]",
                        "f64[]"
                    )
                    len_expr <- if (length(rhs) >= 2) rhs[[2]] else 0L
                    # Resolve scalar inits (e.g., n <- length(x))
                    resolved_len <- len_expr
                    if (is.name(len_expr)) {
                        si_name <- as.character(len_expr)
                        if (!is.null(scalar_inits[[si_name]]) &&
                            is.list(scalar_inits[[si_name]]$value) &&
                            identical(scalar_inits[[si_name]]$value$kind, "len")) {
                            si_info <- scalar_inits[[si_name]]$value
                            resolved_len <- call("length", as.name(si_info$name))
                        }
                    }
                    if (is.null(.mojor_state$current_local_vector_lengths)) {
                        .mojor_state$current_local_vector_lengths <- list()
                    }
                    .mojor_state$current_local_vector_lengths[[lhs]] <- list(
                        length = resolved_len,
                        length_alloc = resolved_len,
                        length_runtime = resolved_len,
                        type = out_type,
                        scope = "loop_body",
                        origin = "ctor"
                    )
                }
            }
        }

        # Recurse into nested blocks, ifs, and loops
        if (op == "{") {
            .mojor_scan_loop_body_allocs(as.list(s)[-1], args, types, scalar_inits)
        } else if (op == "if") {
            if (length(s) >= 3) {
                then_stmts <- .mojor_extract_block(s[[3]])
                .mojor_scan_loop_body_allocs(then_stmts, args, types, scalar_inits)
            }
            if (length(s) >= 4) {
                else_stmts <- .mojor_extract_block(s[[4]])
                .mojor_scan_loop_body_allocs(else_stmts, args, types, scalar_inits)
            }
        } else if (op == "for" && length(s) >= 4) {
            .mojor_scan_loop_body_allocs(.mojor_extract_block(s[[4]]), args, types, scalar_inits)
        } else if (op == "while" && length(s) >= 3) {
            .mojor_scan_loop_body_allocs(.mojor_extract_block(s[[3]]), args, types, scalar_inits)
        } else if (op == "repeat" && length(s) >= 2) {
            .mojor_scan_loop_body_allocs(.mojor_extract_block(s[[2]]), args, types, scalar_inits)
        }
    }
}

.mojor_scan_named_vector_assignments <- function(blocks) {
    for (b in blocks) {
        if (!is.call(b) || !identical(as.character(b[[1]]), "<-")) next
        lhs <- b[[2]]
        if (!is.name(lhs)) next
        var_name <- as.character(lhs)
        rhs <- b[[3]]
        if (!is.call(rhs)) next
        op <- as.character(rhs[[1]])
        if (!identical(op, "c")) next
        parts <- as.list(rhs)[-1]
        if (length(parts) == 0L) next
        nms <- names(parts)
        if (is.null(nms) || !any(nms != "")) next
        if (is.null(.mojor_state$vector_names)) {
            .mojor_state$vector_names <- list()
        }
        .mojor_state$vector_names[[var_name]] <- nms
    }
}
