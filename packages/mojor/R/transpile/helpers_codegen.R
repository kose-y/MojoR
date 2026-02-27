# Transpile helpers: loop-seq parsing, statement/codegen helpers, GPU
# wrapper builders.

.mojor_fnv1a32_signed <- function(value) {
    to_u32 <- function(x) {
        x <- as.double(x)
        if (x < 0) x <- x + 4294967296
        x %% 4294967296
    }
    to_i32 <- function(x_u32) {
        if (x_u32 >= 2147483648) return(as.integer(x_u32 - 4294967296))
        as.integer(x_u32)
    }
    mul_u32 <- function(a, b) {
        a_u32 <- to_u32(a); b_u32 <- to_u32(b)
        a_lo <- a_u32 %% 65536; a_hi <- floor(a_u32 / 65536)
        b_lo <- b_u32 %% 65536; b_hi <- floor(b_u32 / 65536)
        low <- a_lo * b_lo; mid <- a_lo * b_hi + a_hi * b_lo
        (low + ((mid %% 65536) * 65536)) %% 4294967296
    }
    bytes <- as.integer(charToRaw(enc2utf8(as.character(value))))
    h <- 2166136261
    for (b in bytes) {
        h_i32 <- to_i32(to_u32(h))
        x <- bitwXor(h_i32, as.integer(b))
        h <- mul_u32(to_u32(x), 16777619)
    }
    to_i32(to_u32(h))
}

.mojor_strict_ir_enabled <- function() {
    isTRUE(tryCatch(.mojor_state$options$ir_only, error = function(e) FALSE))
}

.mojor_eval_seq_literal_expr <- function(expr) {
    eval(expr)
}

.mojor_call_arg <- function(node, i) {
    tryCatch(
        {
            val <- node[[i]]
            if (is.symbol(val)) {
                nm <- tryCatch(
                  as.character(val),
                  error = function(e) ""
              )
                if (identical(nm, "")) {
                  return(NULL)
                }
            }
            val
        }, error = function(e) {
            msg <- conditionMessage(e)
            if (grepl("argument.*missing|subscript out of bounds", msg, ignore.case = TRUE)) {
                return(NULL)
            }
            stop(e)
        }
    )
}

.mojor_parse_loop_seq <- function(seq_expr, arg_specs, args, scalar_inits = list()) {
    seq_expr_src <- seq_expr
    seq_expr_norm <- .mojor_ir_canonicalize_loop_index_ast(seq_expr)
    source_seq_op <- if (is.call(seq_expr_src) && length(seq_expr_src) >= 1L) {
        as.character(seq_expr_src[[1]])
    } else {
        ""
    }
    if (identical(source_seq_op, "seq_len")) {
        seq_expr <- seq_expr_src
    } else {
        seq_expr <- seq_expr_norm
    }
    strict_ir <- .mojor_strict_ir_enabled()
    scalar_expr_key <- function(expr) {
        paste(
            deparse(expr, width.cutoff = 500),
            collapse = ""
        )
    }
    is_local_i32_name <- function(name) {
        if (is.null(scalar_inits) || is.null(scalar_inits[[name]])) {
            return(FALSE)
        }
        init_info <- scalar_inits[[name]]
        if (!is.null(init_info$type) && init_info$type == "i32") {
            return(TRUE)
        }
        if (!is.null(init_info$type) &&
            init_info$type == "f64" &&
            !is.list(init_info$value)) {
            val <- init_info$value
            return(is.numeric(val) && abs(val - round(val)) < 1e-12)
        }
        FALSE
    }
    resolve_array_source_name <- function(name) {
        resolved <- .mojor_resolve_array_source_name(name, args, arg_specs)
        if (!is.null(resolved)) {
            return(resolved)
        }
        local_array_dims <- tryCatch(.mojor_state$current_local_array_dims, error = function(e) NULL)
        if (!is.null(local_array_dims) && !is.null(local_array_dims[[name]])) {
            return(name)
        }
        local_matrix_dims <- tryCatch(.mojor_state$current_local_matrix_dims, error = function(e) NULL)
        if (!is.null(local_matrix_dims) && !is.null(local_matrix_dims[[name]])) {
            return(name)
        }
        local_vector_lengths <- tryCatch(.mojor_state$current_local_vector_lengths, error = function(e) NULL)
        if (!is.null(local_vector_lengths) && !is.null(local_vector_lengths[[name]])) {
            return(name)
        }
        NULL
    }
    array_spec_for_name <- function(name) {
        if (is.null(name) || !nzchar(name)) {
            return(NULL)
        }
        spec <- arg_specs[[name]]
        if (!is.null(spec)) {
            return(spec)
        }
        local_array_dims <- tryCatch(.mojor_state$current_local_array_dims, error = function(e) NULL)
        if (!is.null(local_array_dims) && !is.null(local_array_dims[[name]]) &&
            !is.null(local_array_dims[[name]]$type)) {
            return(local_array_dims[[name]]$type)
        }
        local_matrix_dims <- tryCatch(.mojor_state$current_local_matrix_dims, error = function(e) NULL)
        if (!is.null(local_matrix_dims) && !is.null(local_matrix_dims[[name]]) &&
            !is.null(local_matrix_dims[[name]]$type)) {
            return(local_matrix_dims[[name]]$type)
        }
        local_vector_lengths <- tryCatch(.mojor_state$current_local_vector_lengths, error = function(e) NULL)
        if (!is.null(local_vector_lengths) && !is.null(local_vector_lengths[[name]]) &&
            !is.null(local_vector_lengths[[name]]$type)) {
            return(local_vector_lengths[[name]]$type)
        }
        NULL
    }
    is_integerish_num <- function(x) {
        if (!is.numeric(x) ||
            length(x) !=
                1) {
            return(FALSE)
        }
        val <- as.integer(x)
        if (is.na(val)) {
            return(FALSE)
        }
        abs(x - val) <
            1e-12
    }
    dim_vector_expr_ok <- function(expr) {
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
        spec <- array_spec_for_name(arr_name)
        if (is.null(spec) || !.mojor_is_array(spec)) {
            return(FALSE)
        }
        .mojor_is_matrix(spec) || .mojor_type_ndim(spec) > 1L
    }
    scalar_expr_float_ok <- function(expr) {
        if (is.numeric(expr) &&
            length(expr) ==
                1) {
            return(!is.na(expr))
        }
        if (is.integer(expr) &&
            length(expr) ==
                1) {
            return(TRUE)
        }
        if (is.name(expr)) {
            name <- as.character(expr)
            if (name %in% args) {
                spec <- arg_specs[[name]]
                if (.mojor_is_array(spec)) {
                    return(FALSE)
                }
                return(spec %in% c("i32", "f64", "f32"))
            }
            if (name %in% names(scalar_inits)) {
                init_info <- scalar_inits[[name]]
                return(
                  is.list(init_info) &&
                    !is.null(init_info$type) &&
                    init_info$type %in% c("i32", "f64", "f32")
                )
            }
            return(FALSE)
        }
        if (is.call(expr)) {
            op <- as.character(expr[[1]])
            if (op == "(" && length(expr) ==
                2) {
                return(scalar_expr_float_ok(expr[[2]]))
            }
            if (op == "as.double" && length(expr) ==
                2) {
                return(scalar_expr_float_ok(expr[[2]]))
            }
            if (op == "as.single" && length(expr) ==
                2) {
                return(scalar_expr_float_ok(expr[[2]]))
            }
            if (op == "as.integer" && length(expr) ==
                2) {
                return(scalar_expr_float_ok(expr[[2]]))
            }
            if (op == "-" && length(expr) ==
                2) {
                return(scalar_expr_float_ok(expr[[2]]))
            }
            if (op == "+" && length(expr) ==
                2) {
                return(scalar_expr_float_ok(expr[[2]]))
            }
            if (op %in% c("+", "-", "*", "/", "%/%", "%%") &&
                length(expr) ==
                  3) {
                return(
                  scalar_expr_float_ok(expr[[2]]) &&
                    scalar_expr_float_ok(expr[[3]])
              )
            }
            if (op %in% c("min", "max", "pmin", "pmax") &&
                length(expr) >=
                  2) {
                vals <- as.list(expr)[-1]
                return(all(vapply(
                    vals,
                    function(v) scalar_expr_float_ok(v) || dim_vector_expr_ok(v),
                    logical(1)
                )))
            }
            if (op == "length" && length(expr) ==
                2) {
                arg <- expr[[2]]
                if (!is.name(arg)) {
                  return(FALSE)
                }
                arr_name <- resolve_array_source_name(as.character(arg))
                if (is.null(arr_name)) {
                  return(FALSE)
                }
                spec <- arg_specs[[arr_name]]
                return(.mojor_is_array(spec))
            }
            if (op %in% c("nrow", "ncol") && length(expr) == 2) {
                arg <- expr[[2]]
                if (!is.name(arg)) {
                  return(FALSE)
                }
                arr_name <- resolve_array_source_name(as.character(arg))
                if (is.null(arr_name)) {
                  return(FALSE)
                }
                spec <- arg_specs[[arr_name]]
                return(.mojor_is_array(spec))
            }
        }
        FALSE
    }
    expr_has_float <- function(expr) {
        if (is.null(expr)) {
            return(FALSE)
        }
        if (is.numeric(expr) &&
            length(expr) ==
                1) {
            return(!is_integerish_num(expr))
        }
        if (is.integer(expr) &&
            length(expr) ==
                1) {
            return(FALSE)
        }
        if (is.name(expr)) {
            name <- as.character(expr)
            if (name %in% args) {
                spec <- arg_specs[[name]]
                return(
                    !.mojor_is_array(spec) &&
                      spec %in% c("f64", "f32")
                )
            }
            if (name %in% names(scalar_inits)) {
                init_info <- scalar_inits[[name]]
                return(
                  is.list(init_info) &&
                    !is.null(init_info$type) &&
                    init_info$type %in% c("f64", "f32")
                )
            }
            return(FALSE)
        }
        if (is.call(expr)) {
            op <- as.character(expr[[1]])
            if (op == "(" && length(expr) ==
                2) {
                return(expr_has_float(expr[[2]]))
            }
            if (op == "-" && length(expr) ==
                2) {
                return(expr_has_float(expr[[2]]))
            }
            if (op == "+" && length(expr) ==
                2) {
                return(expr_has_float(expr[[2]]))
            }
            if (op == "as.double" && length(expr) ==
                2) {
                return(TRUE)
            }
            if (op == "as.single" && length(expr) ==
                2) {
                return(TRUE)
            }
            if (op == "as.integer" && length(expr) ==
                2) {
                return(FALSE)
            }
            if (op == "abs" && length(expr) ==
                2) {
                return(expr_has_float(expr[[2]]))
            }
            if (op == "/" && length(expr) ==
                3) {
                return(TRUE)
            }
            if (op %in% c("+", "-", "*", "%/%", "%%", "min", "max", "pmin", "pmax") &&
                length(expr) >=
                  3) {
                vals <- as.list(expr)[-1]
                return(any(vapply(vals, expr_has_float, logical(1))))
            }
            if (op == "length" && length(expr) ==
                2) {
                return(FALSE)
            }
            if (op %in% c("nrow", "ncol") && length(expr) == 2) {
                return(FALSE)
            }
        }
        FALSE
    }
    cond_expr_ok <- NULL
    scalar_dim_index_ok <- function(expr) {
        if (is.numeric(expr) &&
            length(expr) ==
                1) {
            val <- as.integer(expr)
            return(!is.na(val))
        }
        if (is.integer(expr) &&
            length(expr) ==
                1) {
            return(TRUE)
        }
        if (is.name(expr)) {
            name <- as.character(expr)
            if (name %in% args) {
                spec <- arg_specs[[name]]
                if (.mojor_is_array(spec)) {
                    return(FALSE)
                }
                return(spec %in% c("i32", "f64", "f32", "lgl", "bool"))
            }
            if (name %in% names(scalar_inits)) {
                init_info <- scalar_inits[[name]]
                return(
                  is.list(init_info) &&
                    !is.null(init_info$type) &&
                    init_info$type %in% c("i32", "f64", "f32", "lgl", "bool")
                )
            }
            return(FALSE)
        }
        if (!is.call(expr)) {
            return(FALSE)
        }
        op <- as.character(expr[[1]])
        if (op == "(" && length(expr) ==
            2) {
            return(scalar_dim_index_ok(expr[[2]]))
        }
        if (op == "as.integer" && length(expr) ==
            2) {
            return(scalar_dim_index_ok(expr[[2]]))
        }
        if (op %in% c("as.double", "as.single") && length(expr) ==
            2) {
            return(scalar_dim_index_ok(expr[[2]]))
        }
        if (op == "+" && length(expr) ==
            2) {
            return(scalar_dim_index_ok(expr[[2]]))
        }
        if (op == "-" && length(expr) ==
            2) {
            return(scalar_dim_index_ok(expr[[2]]))
        }
        if (op == "abs" && length(expr) ==
            2) {
            return(scalar_dim_index_ok(expr[[2]]))
        }
        if (op %in% c("+", "-", "*", "%/%", "%%") &&
            length(expr) ==
                3) {
            return(
              scalar_dim_index_ok(expr[[2]]) &&
                scalar_dim_index_ok(expr[[3]])
            )
        }
        if (op %in% c("min", "max", "pmin", "pmax") &&
            length(expr) >=
                3) {
            vals <- as.list(expr)[-1]
            return(all(vapply(vals, scalar_dim_index_ok, logical(1))))
        }
        FALSE
    }
    scalar_expr_ok <- function(expr) {
        if (is.numeric(expr) &&
            length(expr) ==
                1) {
            val <- as.integer(expr)
            return(!is.na(val))
        }
        if (is.integer(expr) &&
            length(expr) ==
                1) {
            return(TRUE)
        }
        if (is.name(expr)) {
            name <- as.character(expr)
            if (name %in% args) {
                spec <- arg_specs[[name]]
                if (.mojor_is_array(spec)) {
                    return(FALSE)
                }
                return(spec == "i32")
            }
            return(is_local_i32_name(name))
        }
        if (is.call(expr)) {
            op <- as.character(expr[[1]])
            if (op == "(" && length(expr) ==
                2) {
                return(scalar_expr_ok(expr[[2]]))
            }
            if (op == "as.integer" && length(expr) ==
                2) {
                return(scalar_expr_ok(expr[[2]]) || scalar_expr_float_ok(expr[[2]]))
            }
            if (op %in% c("as.double", "as.single") && length(expr) ==
                2) {
                return(scalar_expr_float_ok(expr[[2]]))
            }
            if (op == "+" && length(expr) ==
                2) {
                return(scalar_expr_ok(expr[[2]]) || scalar_expr_float_ok(expr[[2]]))
            }
            if (op == "-" && length(expr) ==
                2) {
                return(scalar_expr_ok(expr[[2]]))
            }
            if (op == "abs" && length(expr) ==
                2) {
                return(scalar_expr_ok(expr[[2]]))
            }
            if (op %in% c("+", "-", "*", "%/%", "%%") &&
                length(expr) ==
                  3) {
                return(
                  scalar_expr_ok(expr[[2]]) &&
                    scalar_expr_ok(expr[[3]])
              )
            }
            if (op %in% c("min", "max", "pmin", "pmax") &&
                length(expr) >=
                  2) {
                vals <- as.list(expr)[-1]
                return(all(vapply(
                    vals,
                    function(v) scalar_expr_ok(v) || dim_vector_expr_ok(v),
                    logical(1)
                )))
            }
            if (op %in% c("ifelse", "if") && length(expr) == 4) {
                return(
                  cond_expr_ok(expr[[2]]) &&
                    scalar_expr_ok(expr[[3]]) &&
                    scalar_expr_ok(expr[[4]])
              )
            }
            if (op == "length" && length(expr) ==
                2) {
                arg <- expr[[2]]
                if (!is.name(arg)) {
                  return(FALSE)
                }
                arr_name <- resolve_array_source_name(as.character(arg))
                if (is.null(arr_name)) {
                  return(FALSE)
                }
                spec <- arg_specs[[arr_name]]
                return(.mojor_is_array(spec))
            }
            if (op %in% c("nrow", "ncol") && length(expr) == 2) {
                arg <- expr[[2]]
                if (!is.name(arg)) {
                  return(FALSE)
                }
                arr_name <- resolve_array_source_name(as.character(arg))
                if (is.null(arr_name)) {
                  return(FALSE)
                }
                spec <- arg_specs[[arr_name]]
                return(.mojor_is_array(spec))
            }
            if (op %in% c("[", "[[") && length(expr) == 3) {
                base <- expr[[2]]
                idx <- expr[[3]]
                if (!is.call(base) ||
                  as.character(base[[1]]) != "dim" ||
                  length(base) != 2 ||
                  !is.name(base[[2]])) {
                  return(FALSE)
                }
                if (!(scalar_expr_ok(idx) || scalar_dim_index_ok(idx))) {
                  return(FALSE)
                }
                name <- as.character(base[[2]])
                arr_name <- resolve_array_source_name(name)
                if (is.null(arr_name)) {
                  return(FALSE)
                }
                spec <- arg_specs[[arr_name]]
                if (!.mojor_is_array(spec)) {
                  return(FALSE)
                }
                if (.mojor_is_matrix(spec)) {
                  return(TRUE)
                }
                return(.mojor_type_ndim(spec) > 1L)
            }
        }
        FALSE
    }
    cond_expr_ok <- function(expr) {
        if (is.logical(expr) &&
            length(expr) ==
                1 && !is.na(expr)) {
            return(TRUE)
        }
        if ((is.integer(expr) || is.numeric(expr)) &&
            length(expr) ==
                1 && !is.na(expr)) {
            return(TRUE)
        }
        if (is.name(expr)) {
            name <- as.character(expr)
            if (name %in% args) {
                spec <- arg_specs[[name]]
                if (.mojor_is_array(spec)) {
                    return(FALSE)
                }
                return(spec %in% c("lgl", "bool", "i32"))
            }
            return(is_local_i32_name(name))
        }
        if (!is.call(expr)) {
            return(FALSE)
        }
        op <- as.character(expr[[1]])
        if (op == "(" && length(expr) ==
            2) {
            return(cond_expr_ok(expr[[2]]))
        }
        if (op == "!" && length(expr) ==
            2) {
            return(cond_expr_ok(expr[[2]]))
        }
        if (op %in% c("&&", "||") &&
            length(expr) ==
                3) {
            return(cond_expr_ok(expr[[2]]) && cond_expr_ok(expr[[3]]))
        }
        if (op %in% c("<", "<=", ">", ">=", "==", "!=") &&
            length(expr) ==
                3) {
            return(scalar_expr_ok(expr[[2]]) && scalar_expr_ok(expr[[3]]))
        }
        if (op == "as.logical" && length(expr) ==
            2) {
            return(scalar_expr_ok(expr[[2]]))
        }
        FALSE
    }
    parse_end <- function(end_expr) {
        while (is.call(end_expr) &&
            as.character(end_expr[[1]]) ==
                "(" && length(end_expr) ==
            2) {
            end_expr <- end_expr[[2]]
        }
        if (is.name(end_expr)) {
            return(
                list(
                  name = as.character(end_expr),
                  end_delta = 0L
              )
            )
        }
        if (is.call(end_expr) &&
            as.character(end_expr[[1]]) ==
                "length" && length(end_expr) ==
            2) {
            arg <- end_expr[[2]]
            if (is.name(arg)) {
                arr_name <- resolve_array_source_name(as.character(arg))
                if (!is.null(arr_name)) {
                  return(list(name = arr_name, end_delta = 0L))
                }
            }
        }
        if (is.call(end_expr) &&
            as.character(end_expr[[1]]) ==
                "-" && length(end_expr) ==
            3) {
            lhs <- end_expr[[2]]
            rhs <- end_expr[[3]]
            if (is.name(lhs) &&
                is.numeric(rhs) &&
                as.integer(rhs) ==
                  1L) {
                return(
                  list(
                    name = as.character(lhs),
                    end_delta = -1L
                )
              )
            }
            if (is.call(lhs) &&
                as.character(lhs[[1]]) ==
                  "length" && length(lhs) ==
                2) {
                arg <- lhs[[2]]
                if (is.name(arg) &&
                  is.numeric(rhs) &&
                  as.integer(rhs) ==
                    1L) {
                  arr_name <- resolve_array_source_name(as.character(arg))
                  if (!is.null(arr_name)) {
                    return(list(name = arr_name, end_delta = -1L))
                  }
                }
            }
        }
        if (is.numeric(end_expr) &&
            length(end_expr) ==
                1) {
            val <- as.integer(end_expr)
            if (is.na(val)) {
                return(NULL)
            }
            return(
                list(
                  name = NULL, end_expr = as.character(val),
                  end_delta = 0L
              )
            )
        }
        if (scalar_expr_ok(end_expr)) {
            return(
                list(
                  name = NULL, end_expr_ast = end_expr, end_delta = 0L,
                  expr_key = scalar_expr_key(end_expr)
              )
            )
        }
        NULL
    }
    parse_seq_arg <- function(expr, label, op_label) {
        if (is.null(expr)) {
            return(list(kind = "missing"))
        }
        if (is.numeric(expr) &&
            length(expr) ==
                1) {
            val <- as.integer(expr)
            if (is.na(val))
                stop(
                  sprintf(
                    "mojor_transpile: %s() %s must be integer", op_label,
                    label
                )
              )
            return(list(kind = "num", value = val))
        }
        if (is.call(expr) &&
            as.character(expr[[1]]) ==
                "-" && length(expr) ==
            2) {
            inner <- expr[[2]]
            if (is.numeric(inner) &&
                length(inner) ==
                  1) {
                val <- -as.integer(inner)
                if (is.na(val))
                  stop(
                    sprintf(
                      "mojor_transpile: %s() %s must be integer", op_label,
                      label
                  )
                )
                return(list(kind = "num", value = val))
            }
        }
        if (is.name(expr)) {
            name <- as.character(expr)
            if (name %in% args) {
                spec <- arg_specs[[name]]
                if (.mojor_is_array(spec))
                  stop(
                    sprintf(
                      "mojor_transpile: %s() %s must be a scalar", op_label,
                      label
                  )
                )
                if (spec != "i32")
                  stop(
                    sprintf(
                      "mojor_transpile: %s() %s must be i32", op_label,
                      label
                  )
                )
                return(list(kind = "name", name = name))
            }
            if (name %in% names(scalar_inits)) {
                init_info <- scalar_inits[[name]]
                if (init_info$type == "i32") {
                  return(list(kind = "name", name = name, init_info = init_info))
                }
                if (init_info$type == "f64" && !is.list(init_info$value)) {
                  val <- init_info$value
                  if (is.numeric(val) &&
                    abs(val - round(val)) <
                      1e-12) {
                    return(list(kind = "name", name = name, init_info = init_info))
                  }
                }
                stop(sprintf("mojor_transpile: %s() %s must be i32", op_label, label))
            }
            stop(
                sprintf(
                  "mojor_transpile: %s() %s must be a function argument",
                  op_label, label
              )
            )
        }
        if (scalar_expr_ok(expr)) {
            return(list(kind = "expr", expr = expr, expr_key = scalar_expr_key(expr)))
        }
        stop(
            sprintf(
                "mojor_transpile: %s() %s must be a scalar expression",
                op_label, label
            )
        )
    }
    parse_float_const <- function(expr) {
        if (is.null(expr)) {
            return(NULL)
        }
        if (is.numeric(expr) &&
            length(expr) ==
                1 && !is.na(expr)) {
            return(as.double(expr))
        }
        if (is.integer(expr) &&
            length(expr) ==
                1 && !is.na(expr)) {
            return(as.double(expr))
        }
        if (is.call(expr)) {
            op <- as.character(expr[[1]])
            if (op == "(" && length(expr) ==
                2) {
                return(parse_float_const(expr[[2]]))
            }
            if (op == "-" && length(expr) ==
                2) {
                inner <- parse_float_const(expr[[2]])
                if (!is.null(inner)) {
                  return(-inner)
                }
            }
            if (op %in% c("as.double", "as.single", "as.integer") &&
                length(expr) ==
                  2) {
                inner <- parse_float_const(expr[[2]])
                if (!is.null(inner)) {
                  return(as.double(inner))
                }
            }
        }
        NULL
    }
    seq_candidate <- function(expr) {
        expr <- .mojor_ir_normalize_selector_ast(expr, mode = "loop_seq")
        if (is.name(expr)) {
            return(TRUE)
        }
        if (!is.call(expr)) {
            return(FALSE)
        }
        op <- as.character(expr[[1]])
        op %in% c(
            "seq_along", "seq_len", "seq.int", "seq", ":", "mojor_seq_len",
            "rev", "+", "-", "*", "seq_parallel_along", "seq_parallel_len",
            "mojor_prange"
        )
    }
    seq_expr <- .mojor_ir_normalize_selector_ast(seq_expr, mode = "loop_seq")
    seq_start_ast <- function(info) {
        if (!is.null(info$start_expr_ast)) {
            return(info$start_expr_ast)
        }
        if (!is.null(info$start)) {
            return(as.integer(info$start))
        }
        1L
    }
    seq_end_ast <- function(info) {
        end_ast <- NULL
        if (!is.null(info$end_expr_ast)) {
            end_ast <- info$end_expr_ast
        } else if (!is.null(info$expr)) {
            end_ast <- info$expr
        } else if (!is.null(info$end_expr)) {
            val <- suppressWarnings(as.integer(info$end_expr))
            if (!is.na(val))
                end_ast <- val else end_ast <- as.name(info$end_expr)
        } else if (info$kind %in% c("array", "iter")) {
            end_ast <- call("length", as.name(info$name))
        } else if (identical(info$kind, "scalar")) {
            end_ast <- as.name(info$name)
        } else {
            end_ast <- 0L
        }
        if (!is.null(info$end_delta) &&
            info$end_delta != 0L) {
            delta <- info$end_delta
            if (delta > 0L) {
                end_ast <- call("+", end_ast, delta)
            } else {
                end_ast <- call("-", end_ast, abs(delta))
            }
        }
        end_ast
    }
    seq_by_ast <- function(info) {
        if (!is.null(info$by_expr_ast)) {
            return(info$by_expr_ast)
        }
        if (!is.null(info$by_expr)) {
            return(as.name(info$by_expr))
        }
        if (!is.null(info$by)) {
            return(as.integer(info$by))
        }
        1L
    }
    apply_seq_transform <- function(info, start_ast, end_ast, by_ast, expr_key) {
        info$start_expr_ast <- start_ast
        info$end_expr_ast <- end_ast
        info$by_expr_ast <- by_ast
        info$by <- NULL
        info$by_expr <- NULL
        info$end_expr <- NULL
        info$end_delta <- 0L
        info$expr_key <- expr_key
        info
    }
    apply_seq_affine <- function(info, seq_expr, op, scalar_ast, side) {
        start_ast <- seq_start_ast(info)
        end_ast <- seq_end_ast(info)
        by_ast <- seq_by_ast(info)
        if (op == "*") {
            start_ast <- call("*", start_ast, scalar_ast)
            end_ast <- call("*", end_ast, scalar_ast)
            by_ast <- call("*", by_ast, scalar_ast)
        } else if (op == "+") {
            if (side == "rhs") {
                start_ast <- call("+", start_ast, scalar_ast)
                end_ast <- call("+", end_ast, scalar_ast)
            } else {
                start_ast <- call("+", scalar_ast, start_ast)
                end_ast <- call("+", scalar_ast, end_ast)
            }
        } else if (op == "-") {
            if (side == "rhs") {
                start_ast <- call("-", start_ast, scalar_ast)
                end_ast <- call("-", end_ast, scalar_ast)
            } else {
                start_ast <- call("-", scalar_ast, start_ast)
                end_ast <- call("-", scalar_ast, end_ast)
                by_ast <- call("-", by_ast)
            }
        }
        apply_seq_transform(info, start_ast, end_ast, by_ast, scalar_expr_key(seq_expr))
    }
    apply_seq_rev <- function(info, seq_expr) {
        start_ast <- seq_start_ast(info)
        end_ast <- seq_end_ast(info)
        by_ast <- seq_by_ast(info)
        apply_seq_transform(
            info, end_ast, start_ast, call("-", by_ast),
            scalar_expr_key(seq_expr)
        )
    }
    scalar_value_ok <- function(expr) {
        if (is.numeric(expr) &&
            length(expr) ==
                1) {
            return(TRUE)
        }
        if (is.integer(expr) &&
            length(expr) ==
                1) {
            return(TRUE)
        }
        if (is.logical(expr) &&
            length(expr) ==
                1) {
            return(TRUE)
        }
        if (is.name(expr)) {
            name <- as.character(expr)
            if (!(name %in% args)) {
                return(FALSE)
            }
            spec <- arg_specs[[name]]
            if (.mojor_is_array(spec)) {
                return(FALSE)
            }
            return(spec %in% c("i32", "f64", "f32", "lgl", "bool"))
        }
        if (is.call(expr)) {
            op <- as.character(expr[[1]])
            if (op == "(" && length(expr) ==
                2) {
                return(scalar_value_ok(expr[[2]]))
            }
            if (op == "-" && length(expr) ==
                2) {
                return(scalar_value_ok(expr[[2]]))
            }
            if (op %in% c("+", "-", "*", "/", "%/%", "%%") &&
                length(expr) ==
                  3) {
                return(
                  scalar_value_ok(expr[[2]]) &&
                    scalar_value_ok(expr[[3]])
              )
            }
        }
        FALSE
    }
    value_kind <- function(expr) {
        if (is.logical(expr)) {
            return("bool")
        }
        if (is.integer(expr)) {
            return("int")
        }
        if (is.numeric(expr)) {
            return("float")
        }
        if (is.name(expr)) {
            name <- as.character(expr)
            spec <- arg_specs[[name]]
            if (spec %in% c("f64", "f32")) {
                return("float")
            }
            if (spec %in% c("i32")) {
                return("int")
            }
            if (spec %in% c("lgl", "bool")) {
                return("bool")
            }
        }
        if (is.call(expr)) {
            op <- as.character(expr[[1]])
            if (op == "(" && length(expr) ==
                2) {
                return(value_kind(expr[[2]]))
            }
            if (op == "-" && length(expr) ==
                2) {
                k <- value_kind(expr[[2]])
                return(if (k == "unknown") "unknown" else k)
            }
            if (op %in% c("+", "-", "*", "/", "%/%", "%%") &&
                length(expr) ==
                  3) {
                k1 <- value_kind(expr[[2]])
                k2 <- value_kind(expr[[3]])
                if (op == "/") {
                  return("float")
                }
                if (k1 == "float" || k2 == "float") {
                  return("float")
                }
                if (k1 == "int" || k2 == "int") {
                  return("int")
                }
                if (k1 == "bool" || k2 == "bool") {
                  return("int")
                }
            }
        }
        "unknown"
    }
    apply_seq_value_transform <- function(info, op, scalar_ast, side) {
 # If iter_expr_ast and scalar_ast are both literals, evaluate
 # statically e.g., c(1L, 2L) + 1L -> c(2, 3)
        iter_ast <- info$iter_expr_ast
        if (!is.null(iter_ast) &&
            is.call(iter_ast) &&
            as.character(iter_ast[[1]]) ==
                "c") {
            parts <- as.list(iter_ast)[-1]
            all_literals <- length(parts) >
                0 && all(
                vapply(
                  parts, function(p) {
                    is.numeric(p) ||
                      is.integer(p) ||
                      is.logical(p)
                  }, logical(1)
              )
            )
            scalar_is_literal <- is.numeric(scalar_ast) ||
                is.integer(scalar_ast) ||
                is.logical(scalar_ast)

            if (all_literals && scalar_is_literal && length(scalar_ast) ==
                1) {
 # Build the full expression and evaluate it
                full_expr <- if (side == "rhs") {
                  call(op, iter_ast, scalar_ast)
                } else {
                  call(op, scalar_ast, iter_ast)
                }

                evaluated <- tryCatch(
                  .mojor_eval_seq_literal_expr(full_expr),
                  error = function(e) e
              )
                if (inherits(evaluated, "error")) {
                  if (isTRUE(strict_ir)) {
                    stop(
                      sprintf(
                        "mojor_transpile: strict loop sequence literal folding failed: %s",
                        conditionMessage(evaluated)
                    ),
                      call. = FALSE
                  )
                  }
                  evaluated <- NULL
                }

                if (!is.null(evaluated) &&
                  (is.numeric(evaluated) ||
                    is.integer(evaluated)) &&
                  length(evaluated) <=
                    8) {
 # Successfully evaluated - replace iter_expr_ast
 # with the result
                  info$iter_expr_ast <- as.call(
                    c(
                      quote(c),
                      as.list(evaluated)
                  )
                )
 # Update spec based on result type
                  if (is.integer(evaluated) &&
                    all(evaluated == as.integer(evaluated))) {
                    info$iter_expr_spec <- "i32"
                  } else {
                    info$iter_expr_spec <- "f64"
                  }
                  return(info)  # Return early, no need to store transform
                }
            }
        }

 # Fallback: store transform to be applied later
        info$iter_expr_transform <- list(op = op, scalar = scalar_ast, side = side)
        k <- value_kind(scalar_ast)
        if (!is.null(info$iter_expr_spec)) {
            if (op == "/" || info$iter_expr_spec %in% c("f64", "f32") ||
                k == "float") {
                info$iter_expr_spec <- "f64"
            } else if (info$iter_expr_spec %in% c("i32") ||
                k == "int") {
                info$iter_expr_spec <- "i32"
            }
        }
        info
    }
    is_ctor_call <- function(expr) {
        is.call(expr) &&
            as.character(expr[[1]]) %in%
                c("c", "rep", "rep.int", "rep_len")
    }
    seq_elem_kind <- function(expr) {
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
            if (!(name %in% args))
                stop("mojor_transpile: loop sequence values must use function arguments")
            spec <- arg_specs[[name]]
            if (.mojor_is_array(spec)) {
                if (spec %in% c("f64[]", "f32[]")) {
                  return("float")
                }
                if (spec %in% c("i32[]")) {
                  return("int")
                }
                if (spec %in% c("lgl[]", "bool[]")) {
                  return("bool")
                }
            } else {
                if (spec %in% c("f64", "f32")) {
                  return("float")
                }
                if (spec %in% c("i32")) {
                  return("int")
                }
                if (spec %in% c("lgl", "bool")) {
                  return("bool")
                }
            }
            return("unknown")
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
                return(seq_elem_kind(val))
            }
            if (op == "c") {
                parts <- as.list(expr)[-1]
                if (length(parts) ==
                  0) {
                  return("unknown")
                }
                kinds <- vapply(parts, seq_elem_kind, character(1))
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
                  return("bool")
                }
            }
        }
        "unknown"
    }
    parse_ctor_len_arg <- function(expr, label) {
        if (is.numeric(expr) &&
            length(expr) ==
                1) {
            val <- as.integer(expr)
            if (is.na(val))
                stop(sprintf("mojor_transpile: %s must be integer", label))
            return(val)
        }
        if (is.integer(expr) &&
            length(expr) ==
                1) {
            return(expr)
        }
        if (is.name(expr)) {
            name <- as.character(expr)
            if (!(name %in% args))
                stop(
                  sprintf(
                    "mojor_transpile: %s must use a function argument",
                    label
                )
              )
            spec <- arg_specs[[name]]
            if (.mojor_is_array(spec))
                stop(sprintf("mojor_transpile: %s must be a scalar", label))
            if (spec != "i32")
                stop(sprintf("mojor_transpile: %s must be i32", label))
            return(expr)
        }
        if (scalar_expr_ok(expr)) {
            return(expr)
        }
        stop(
            sprintf(
                "mojor_transpile: %s must be a scalar integer expression",
                label
            )
        )
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
    ctor_len_value <- function(expr) {
        if (is.numeric(expr) ||
            is.integer(expr) ||
            is.logical(expr)) {
            return(
                list(
                  len_expr = 1L, len_arrays = character(0),
                  elem_kind = seq_elem_kind(expr)
              )
            )
        }
        if (is.name(expr)) {
            name <- as.character(expr)
            if (!(name %in% args))
                stop("mojor_transpile: loop sequence values must use function arguments")
            spec <- arg_specs[[name]]
            if (.mojor_is_array(spec)) {
                return(
                  list(
                    len_expr = call("length", as.name(name)),
                    len_arrays = name, elem_kind = seq_elem_kind(expr)
                )
              )
            }
            return(
                list(
                  len_expr = 1L, len_arrays = character(0),
                  elem_kind = seq_elem_kind(expr)
              )
            )
        }
        if (is.call(expr) &&
            is_ctor_call(expr)) {
            info <- ctor_len_info(expr)
            if (is.null(info))
                stop("mojor_transpile: unsupported constructor in loop sequence")
            return(info)
        }
        stop(
            "mojor_transpile: loop sequence values must be literals, names, or constructors"
        )
    }
    ctor_len_info <- function(expr) {
        if (!is.call(expr)) {
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
            elem_kinds <- character(0)
            for (p in parts) {
                part_info <- ctor_len_value(p)
                len_expr <- add_expr(len_expr, part_info$len_expr)
                len_arrays <- unique(c(len_arrays, part_info$len_arrays))
                elem_kinds <- c(elem_kinds, part_info$elem_kind)
            }
            elem_kind <- if (any(elem_kinds == "float")) {
                "float"
            } else if (any(elem_kinds == "int")) {
                "int"
            } else if (any(elem_kinds == "bool")) {
                "bool"
            } else {
                "unknown"
            }
            return(
                list(len_expr = len_expr, len_arrays = len_arrays, elem_kind = elem_kind)
            )
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
            base_info <- ctor_len_value(val_expr)
            len_expr <- base_info$len_expr
            len_arrays <- base_info$len_arrays
            elem_kind <- base_info$elem_kind
            if (op == "rep_len") {
                len_out <- get_arg("length.out", 2)
                if (is.null(len_out))
                  stop("mojor_transpile: rep_len() requires length.out")
                len_expr <- parse_ctor_len_arg(len_out, "rep_len() length.out")
                return(
                  list(len_expr = len_expr, len_arrays = len_arrays, elem_kind = elem_kind)
              )
            }
            length_out <- get_arg("length.out", 3)
            if (!is.null(length_out)) {
                len_expr <- parse_ctor_len_arg(length_out, paste0(op, "() length.out"))
                return(
                  list(len_expr = len_expr, len_arrays = len_arrays, elem_kind = elem_kind)
              )
            }
            times <- get_arg("times", 2)
            if (!is.null(times)) {
                times_expr <- parse_ctor_len_arg(times, paste0(op, "() times"))
                len_expr <- mul_expr(len_expr, times_expr)
            }
            each <- get_arg("each", 4)
            if (!is.null(each)) {
                each_expr <- parse_ctor_len_arg(each, paste0(op, "() each"))
                len_expr <- mul_expr(len_expr, each_expr)
            }
            return(
                list(len_expr = len_expr, len_arrays = len_arrays, elem_kind = elem_kind)
            )
        }
        NULL
    }
    parse_seq_constructor <- function(expr) {
        if (!is.call(expr) ||
            !is_ctor_call(expr)) {
            return(NULL)
        }
        info <- ctor_len_info(expr)
        if (is.null(info)) {
            return(NULL)
        }
        if (is.null(info$len_expr))
            stop("mojor_transpile: loop sequence length could not be determined")
        if (info$elem_kind == "unknown") {
            stop("mojor_transpile: loop sequence values must be numeric or logical")
        }
        iter_spec <- if (info$elem_kind == "float") {
            "f64"
        } else if (info$elem_kind == "int") {
            "i32"
        } else if (info$elem_kind == "bool") {
            "lgl"
        } else {
            "i32"
        }
        expr_c <- .mojor_len_expr_to_c(info$len_expr, args, arg_specs)
        list(
            kind = "expr", start = 1L, end_delta = 0L, end_expr_ast = info$len_expr,
            expr_key = scalar_expr_key(expr),
            expr_c = expr_c, iter_expr_ast = expr, iter_expr_spec = iter_spec,
            len_arrays = info$len_arrays
        )
    }
    if (is.call(seq_expr)) {
        op_wrap <- as.character(seq_expr[[1]])
        if (op_wrap == "rev" && length(seq_expr) ==
            2 && seq_candidate(seq_expr[[2]])) {
            base <- .mojor_parse_loop_seq(seq_expr[[2]], arg_specs, args, scalar_inits)
            return(apply_seq_rev(base, seq_expr))
        }
        if (op_wrap == "rev" && length(seq_expr) ==
            2 && is_ctor_call(seq_expr[[2]])) {
            base <- parse_seq_constructor(seq_expr[[2]])
            base$iter_expr_rev <- TRUE
            return(base)
        }
        if (op_wrap == "-" && length(seq_expr) ==
            2 && seq_candidate(seq_expr[[2]])) {
            base <- .mojor_parse_loop_seq(seq_expr[[2]], arg_specs, args, scalar_inits)
            return(apply_seq_affine(base, seq_expr, "*", -1L, side = "rhs"))
        }
        if (op_wrap == "-" && length(seq_expr) ==
            2 && is_ctor_call(seq_expr[[2]])) {
            base <- parse_seq_constructor(seq_expr[[2]])
            return(apply_seq_value_transform(base, "*", -1L, side = "rhs"))
        }
        if (op_wrap %in% c("+", "-", "*") &&
            length(seq_expr) ==
                3) {
            lhs <- seq_expr[[2]]
            rhs <- seq_expr[[3]]
            if (seq_candidate(lhs) &&
                scalar_expr_ok(rhs)) {
                base <- .mojor_parse_loop_seq(lhs, arg_specs, args, scalar_inits)
                return(apply_seq_affine(base, seq_expr, op_wrap, rhs, side = "rhs"))
            }
            if (seq_candidate(rhs) &&
                scalar_expr_ok(lhs)) {
                base <- .mojor_parse_loop_seq(rhs, arg_specs, args, scalar_inits)
                return(apply_seq_affine(base, seq_expr, op_wrap, lhs, side = "lhs"))
            }
            if (is_ctor_call(lhs) &&
                scalar_value_ok(rhs)) {
                base <- parse_seq_constructor(lhs)
                return(apply_seq_value_transform(base, op_wrap, rhs, side = "rhs"))
            }
            if (is_ctor_call(rhs) &&
                scalar_value_ok(lhs)) {
                base <- parse_seq_constructor(rhs)
                return(apply_seq_value_transform(base, op_wrap, lhs, side = "lhs"))
            }
        }
        if (op_wrap == "/" && length(seq_expr) ==
            3) {
            lhs <- seq_expr[[2]]
            rhs <- seq_expr[[3]]
            if (seq_candidate(lhs) &&
                scalar_expr_ok(rhs)) {
                base <- .mojor_parse_loop_seq(lhs, arg_specs, args, scalar_inits)
                if (is.null(base$iter_expr_spec))
                  base$iter_expr_spec <- "i32"
                base$iter_expr_from_loopvar <- TRUE
                return(apply_seq_value_transform(base, "/", rhs, side = "rhs"))
            }
            if (seq_candidate(rhs) &&
                scalar_expr_ok(lhs)) {
                base <- .mojor_parse_loop_seq(rhs, arg_specs, args, scalar_inits)
                if (is.null(base$iter_expr_spec))
                  base$iter_expr_spec <- "i32"
                base$iter_expr_from_loopvar <- TRUE
                return(apply_seq_value_transform(base, "/", lhs, side = "lhs"))
            }
            if (is_ctor_call(lhs) &&
                scalar_value_ok(rhs)) {
                base <- parse_seq_constructor(lhs)
                return(apply_seq_value_transform(base, "/", rhs, side = "rhs"))
            }
            if (is_ctor_call(rhs) &&
                scalar_value_ok(lhs)) {
                base <- parse_seq_constructor(rhs)
                return(apply_seq_value_transform(base, "/", lhs, side = "lhs"))
            }
        }
    }
    if (is.call(seq_expr)) {
        ctor_info <- parse_seq_constructor(seq_expr)
        if (!is.null(ctor_info)) {
            return(ctor_info)
        }
    }
    if (is.name(seq_expr)) {
        name <- as.character(seq_expr)
        source_name <- resolve_array_source_name(name)
        source_spec <- array_spec_for_name(source_name)
        if (!is.null(source_name) && !is.null(source_spec) && .mojor_is_array(source_spec)) {
            return(list(kind = "iter", name = source_name, start = 1L, end_delta = 0L))
        }
        .mojor_err(
            "unsupported loop sequence", seq_expr_src, "use seq_along/seq_len/seq.int/seq/1:n or rev()/+/-/* or / on those forms"
        )
    }
    if (!is.call(seq_expr)) {
        .mojor_err(
            "unsupported loop sequence", seq_expr_src, "use seq_along/seq_len/seq.int/seq/1:n or rev()/+/-/* or / on those forms"
        )
    }
    op <- as.character(seq_expr[[1]])
    if (op == "mojor_seq_len") {
        if (length(seq_expr) <
            2)
            stop("mojor_transpile: mojor_seq_len() requires a length expression")
        len_expr <- seq_expr[[2]]
        expr_c <- .mojor_len_expr_to_c(len_expr, args, arg_specs)
        return(
            list(
                kind = "expr", expr = len_expr, expr_c = expr_c, start = 1L,
                end_delta = 0L, expr_key = scalar_expr_key(len_expr)
            )
        )
    }
    if (op == "seq_along") {
        if (!is.name(seq_expr[[2]]))
            stop("mojor_transpile: seq_along() must take a name")
        source_name <- resolve_array_source_name(as.character(seq_expr[[2]]))
        source_spec <- array_spec_for_name(source_name)
        if (is.null(source_name) || is.null(source_spec) || !.mojor_is_array(source_spec)) {
            stop("mojor_transpile: seq_along() must use an array argument")
        }
        return(list(kind = "array", name = source_name, start = 1L, end_delta = 0L))
    }
    if (op == "seq_len") {
        arg <- seq_expr[[2]]
        if (is.call(arg) &&
            as.character(arg[[1]]) ==
                "-" && length(arg) ==
            3) {
            lhs <- arg[[2]]
            rhs <- arg[[3]]
            if (is.call(lhs) &&
                as.character(lhs[[1]]) ==
                  "length" && length(lhs) ==
                2) {
                inner <- lhs[[2]]
                if (is.name(inner) &&
                  is.numeric(rhs) &&
                  as.integer(rhs) ==
                    1L) {
                  name <- resolve_array_source_name(as.character(inner))
                  if (is.null(name))
                    stop(
                      "mojor_transpile: seq_len(length(x)-1) name must be a function argument"
                  )
                  spec <- array_spec_for_name(name)
                  if (!.mojor_is_array(spec))
                    stop("mojor_transpile: seq_len(length(x)-1) requires an array argument")
                  return(list(kind = "array", name = name, start = 1L, end_delta = -1L))
                }
            }
            if (is.name(lhs) &&
                is.numeric(rhs) &&
                as.integer(rhs) ==
                  1L) {
                name <- as.character(lhs)
                if (name %in% args) {
                  spec <- arg_specs[[name]]
                  if (.mojor_is_array(spec))
                    stop("mojor_transpile: seq_len(n-1) requires scalar i32")
                  if (spec != "i32")
                    stop("mojor_transpile: seq_len(n-1) scalar must be i32")
                } else if (!is_local_i32_name(name)) {
                  stop("mojor_transpile: seq_len(n-1) name must be a function argument")
                }
                return(list(kind = "scalar", name = name, start = 1L, end_delta = -1L))
            }
        }
        if (is.call(arg) &&
            as.character(arg[[1]]) ==
                "length" && length(arg) ==
            2) {
            inner <- arg[[2]]
            if (!is.name(inner))
                stop("mojor_transpile: seq_len(length(x)) must use a name")
            name <- resolve_array_source_name(as.character(inner))
            if (is.null(name))
                stop(
                  "mojor_transpile: seq_len(length(x)) name must be a function argument"
              )
            spec <- array_spec_for_name(name)
            if (!.mojor_is_array(spec))
                stop("mojor_transpile: seq_len(length(x)) requires an array argument")
            return(list(kind = "array", name = name, start = 1L, end_delta = 0L))
        }
        if (scalar_expr_ok(arg) &&
            !is.name(arg)) {
            expr_c <- .mojor_len_expr_to_c(arg, args, arg_specs)
            return(
                list(
                  kind = "expr", start = 1L, end_delta = 0L, end_expr_ast = arg,
                  expr_key = scalar_expr_key(seq_expr),
                  expr_c = expr_c
              )
            )
        }
        if (!is.name(arg))
            stop("mojor_transpile: seq_len() must take a name")
        name <- as.character(arg)
        if (name %in% args) {
            spec <- arg_specs[[name]]
            if (.mojor_is_array(spec)) {
                return(list(kind = "array", name = name, start = 1L, end_delta = 0L))
            }
            if (spec != "i32")
                stop("mojor_transpile: seq_len() scalar must be i32")
        } else if (!is_local_i32_name(name)) {
            stop("mojor_transpile: seq_len() arg must be a function argument")
        }
        return(list(kind = "scalar", name = name, start = 1L, end_delta = 0L))
    }
 # Parallel loop variants - mark loop as parallel-safe
    if (op == "seq_parallel_along") {
        if (!is.name(seq_expr[[2]]))
            stop("mojor_transpile: seq_parallel_along() must take a name")
        name <- resolve_array_source_name(as.character(seq_expr[[2]]))
        spec <- array_spec_for_name(name)
        if (is.null(name) || is.null(spec) || !.mojor_is_array(spec)) {
            stop("mojor_transpile: seq_parallel_along() must use an array argument")
        }
        return(
            list(
                kind = "array", name = name, start = 1L, end_delta = 0L,
                parallel = TRUE
            )
        )
    }
    if (op == "seq_parallel_len") {
        arg <- seq_expr[[2]]
        if (is.call(arg) &&
            as.character(arg[[1]]) ==
                "length" && length(arg) ==
            2) {
            inner <- arg[[2]]
            if (!is.name(inner))
                stop("mojor_transpile: seq_parallel_len(length(x)) must use a name")
            name <- resolve_array_source_name(as.character(inner))
            if (is.null(name))
                stop(
                  "mojor_transpile: seq_parallel_len(length(x)) name must be a function argument"
              )
            spec <- array_spec_for_name(name)
            if (!.mojor_is_array(spec))
                stop(
                  "mojor_transpile: seq_parallel_len(length(x)) requires an array argument"
              )
            return(
                list(
                  kind = "array", name = name, start = 1L, end_delta = 0L,
                  parallel = TRUE
              )
            )
        }
        if (!is.name(arg)) {
            expr_c <- tryCatch(
                .mojor_len_expr_to_c(arg, args, arg_specs),
                error = function(e) NULL
            )
            if (is.null(expr_c)) {
                stop("mojor_transpile: seq_parallel_len() must take an i32 scalar expression")
            }
            return(
                list(
                  kind = "expr", start = 1L, end_delta = 0L, end_expr_ast = arg,
                  expr_key = scalar_expr_key(seq_expr), expr_c = expr_c,
                  parallel = TRUE
              )
            )
        }
        name <- as.character(arg)
        if (name %in% args) {
            spec <- arg_specs[[name]]
            if (.mojor_is_array(spec)) {
                return(
                    list(
                      kind = "array", name = name, start = 1L, end_delta = 0L,
                      parallel = TRUE
                  )
                )
            }
            if (spec != "i32")
                stop("mojor_transpile: seq_parallel_len() scalar must be i32")
        } else if (!is_local_i32_name(name)) {
            stop(
                "mojor_transpile: seq_parallel_len() arg must be a function argument"
            )
        }
        return(
            list(
                kind = "scalar", name = name, start = 1L, end_delta = 0L,
                parallel = TRUE
            )
        )
    }
    if (op == "mojor_prange") {
        arg <- seq_expr[[2]]
        if (is.call(arg) &&
            as.character(arg[[1]]) ==
                "length" && length(arg) ==
            2) {
            inner <- arg[[2]]
            if (!is.name(inner))
                stop("mojor_transpile: mojor_prange(length(x)) must use a name")
            name <- resolve_array_source_name(as.character(inner))
            if (is.null(name))
                stop(
                  "mojor_transpile: mojor_prange(length(x)) name must be a function argument"
              )
            spec <- array_spec_for_name(name)
            if (!.mojor_is_array(spec))
                stop(
                  "mojor_transpile: mojor_prange(length(x)) requires an array argument"
              )
            return(
                list(
                  kind = "array", name = name, start = 1L, end_delta = 0L,
                  parallel = TRUE
              )
            )
        }
        if (!is.name(arg)) {
            expr_c <- tryCatch(
                .mojor_len_expr_to_c(arg, args, arg_specs),
                error = function(e) NULL
            )
            if (is.null(expr_c)) {
                stop("mojor_transpile: mojor_prange() must take an i32 scalar expression")
            }
            return(
                list(
                  kind = "expr", start = 1L, end_delta = 0L, end_expr_ast = arg,
                  expr_key = scalar_expr_key(seq_expr), expr_c = expr_c,
                  parallel = TRUE
              )
            )
        }
        name <- as.character(arg)
        if (name %in% args) {
            spec <- arg_specs[[name]]
            if (.mojor_is_array(spec)) {
                return(
                    list(
                      kind = "array", name = name, start = 1L, end_delta = 0L,
                      parallel = TRUE
                  )
                )
            }
            if (spec != "i32")
                stop("mojor_transpile: mojor_prange() scalar must be i32")
        } else if (!is_local_i32_name(name)) {
            stop("mojor_transpile: mojor_prange() arg must be a function argument")
        }
        return(
            list(
                kind = "scalar", name = name, start = 1L, end_delta = 0L,
                parallel = TRUE
            )
        )
    }
    if (op == ":") {
        if (length(seq_expr) !=
            3)
            stop("mojor_transpile: invalid ':' sequence")
        lhs <- seq_expr[[2]]
        rhs <- seq_expr[[3]]
        start_info <- parse_seq_arg(lhs, "start", ":")
        end_info <- parse_end(rhs)
        if (is.null(end_info))
            stop("mojor_transpile: ':' sequence must end with a scalar expression")
        end_is_scalar <- FALSE
        if (!is.null(end_info$name)) {
            name <- end_info$name
            if (name %in% names(scalar_inits)) {
 # Local variable tracked in scalar_inits takes
 # precedence (e.g., thin <- 10)
                init_info <- scalar_inits[[name]]
                if (is.list(init_info$value) &&
                  init_info$value$kind == "len") {
 # Derived from length(array_arg)
                  out <- list(
                    kind = "array", name = init_info$value$name, end_delta = end_info$end_delta +
                      init_info$value$delta, colon_dynamic = TRUE
                )
                } else if (is.list(init_info$value) &&
                  init_info$value$kind == "arg") {
 # Derived from scalar arg (e.g., N <- n where n is
 # i32)
                  if (init_info$type != "i32")
                    stop("mojor_transpile: ':' scalar must be i32")
                  out <- list(
                    kind = "scalar", name = name, end_delta = end_info$end_delta,
                    colon_dynamic = TRUE
                )
                  end_info$name <- NULL
                  end_info$end_expr_ast <- as.name(init_info$value$name)
                  end_is_scalar <- TRUE
                } else if (is.list(init_info$value) &&
                  init_info$value$kind == "dim") {
 # Derived from dim(x)[k] local scalar. Keep it as
 # a scalar loop bound and carry an explicit shape
 # expression.
                  arr_name <- init_info$value$name
                  dim_idx <- init_info$value$dim
                  if (!is.character(arr_name) ||
                    length(arr_name) !=
                      1 || !(arr_name %in% args)) {
                    stop(
                      "mojor_transpile: ':' sequence name must be a function argument or derived from one"
                  )
                  }
                  if (!isTRUE(dim_idx %in% c(1, 2))) {
                    stop(
                      "mojor_transpile: ':' scalar derived from dim() must use dim index 1 or 2"
                  )
                  }
                  out <- list(
                    kind = "scalar", name = name, end_delta = end_info$end_delta,
                    colon_dynamic = TRUE
                )
                  end_info$name <- NULL
                  end_info$end_expr_ast <- call(if (dim_idx == 1) "nrow" else "ncol", as.name(arr_name))
                  end_is_scalar <- TRUE
                } else if (is.list(init_info$value) &&
                  init_info$value$kind == "loop_var") {
 # Local loop vars (e.g. for (i in ...); for (j in 1:i)) are valid scalar bounds.
                  if (init_info$type != "i32")
                    stop("mojor_transpile: ':' scalar must be i32")
                  out <- list(
                    kind = "scalar", name = name, end_delta = end_info$end_delta,
                    colon_dynamic = TRUE
                )
                  end_info$name <- NULL
                  end_info$end_expr_ast <- as.name(name)
                  end_is_scalar <- TRUE
                } else if (is.list(init_info$value) &&
                  init_info$value$kind == "expr" &&
                  !is.null(init_info$value$expr)) {
                  if (init_info$type != "i32")
                    stop("mojor_transpile: ':' scalar must be i32")
                  out <- list(
                    kind = "scalar", name = name, end_delta = end_info$end_delta,
                    colon_dynamic = TRUE
                  )
                  end_info$name <- NULL
                  end_info$end_expr_ast <- init_info$value$expr
                  end_is_scalar <- TRUE
                } else if (!is.list(init_info$value)) {
 # Literal value (e.g., thin <- 10)
                  if (init_info$type == "i32") {
                    out <- list(
                      kind = "scalar", name = name, end_delta = end_info$end_delta,
                      colon_dynamic = TRUE
                  )
                    end_info$name <- NULL
                    end_info$end_expr <- as.character(as.integer(init_info$value))
                    end_is_scalar <- TRUE
                  } else if (init_info$type == "f64") {
 # Check if it's integer-ish
                    val <- init_info$value
                    if (is.numeric(val) &&
                      abs(val - round(val)) <
                        1e-12) {
                      out <- list(
                        kind = "scalar", name = name, end_delta = end_info$end_delta,
                        colon_dynamic = TRUE
                    )
                      end_info$name <- NULL
                      end_info$end_expr <- as.character(as.integer(round(val)))
                      end_is_scalar <- TRUE
                    } else {
                      stop("mojor_transpile: ':' scalar must be integer-like")
                    }
                  } else {
                    stop("mojor_transpile: ':' scalar must be i32")
                  }
                } else {
                  stop(
                    "mojor_transpile: ':' sequence name must be a function argument or derived from one"
                )
                }
            } else if (name %in% args) {
                spec <- arg_specs[[name]]
                if (.mojor_is_array(spec)) {
                  out <- list(
                    kind = "array", name = name, end_delta = end_info$end_delta,
                    colon_dynamic = TRUE
                )
                } else {
                  if (spec != "i32")
                    stop("mojor_transpile: ':' scalar must be i32")
                  out <- list(
                    kind = "scalar", name = name, end_delta = end_info$end_delta,
                    colon_dynamic = TRUE
                )
                  end_info$end_expr_ast <- as.name(name)
                  end_is_scalar <- TRUE
                }
            } else {
                stop("mojor_transpile: ':' sequence name must be a function argument")
            }
        } else {
            if (start_info$kind == "name" && !is.null(end_info$end_expr) &&
                is.null(end_info$end_expr_ast)) {
                out <- list(
                  kind = "scalar", name = start_info$name, end_delta = end_info$end_delta,
                  colon_dynamic = TRUE
              )
                out$end_expr <- end_info$end_expr
            } else {
                out <- list(
                  kind = "expr", end_delta = end_info$end_delta, colon_dynamic = TRUE,
                  expr_key = end_info$expr_key
              )
            }
            if (!is.null(end_info$end_expr))
                out$end_expr <- end_info$end_expr
            if (!is.null(end_info$end_expr_ast))
                out$end_expr_ast <- end_info$end_expr_ast
            end_is_scalar <- TRUE
        }
        if (start_info$kind == "num") {
            if (is.na(start_info$value))
                stop("mojor_transpile: ':' sequence start must be an integer")
            out$start <- start_info$value
        } else if (start_info$kind == "name") {
            out$start <- 1L
            out$start_expr_ast <- as.name(start_info$name)
        } else if (start_info$kind == "expr") {
            out$start <- 1L
            out$start_expr_ast <- start_info$expr
            if (is.null(out$expr_key))
                out$expr_key <- start_info$expr_key
        } else {
            stop("mojor_transpile: ':' sequence must use a scalar start expression")
        }
        if (identical(out$kind, "expr")) {
            out$expr_key <- scalar_expr_key(seq_expr)
        }
        if (identical(out$kind, "expr") &&
            is.null(out$expr_c)) {
            if (!is.null(out$end_expr_ast)) {
                out$expr_c <- .mojor_len_expr_to_c(out$end_expr_ast, args, arg_specs)
            } else if (!is.null(out$end_expr)) {
                out$expr_c <- as.character(out$end_expr)
            }
        }
            if (isTRUE(out$colon_dynamic) &&
                isTRUE(end_is_scalar)) {
            start_c <- NULL
            end_c <- NULL
            if (start_info$kind == "num") {
                start_c <- as.character(start_info$value)
            } else if (start_info$kind == "name") {
                if (!is.null(start_info$init_info)) {
                  init_info <- start_info$init_info
                  if (is.list(init_info$value) &&
                    init_info$value$kind == "len") {
                    end_ast <- call("length", as.name(init_info$value$name))
                    delta <- init_info$value$delta
                    if (!is.null(delta) &&
                      delta != 0L) {
                      end_ast <- if (delta > 0L)
                        call("+", end_ast, delta) else call("-", end_ast, abs(delta))
                    }
                    start_c <- .mojor_len_expr_to_c(end_ast, args, arg_specs)
                  } else if (is.list(init_info$value) &&
                    init_info$value$kind == "expr" &&
                    !is.null(init_info$value$expr)) {
                    start_c <- .mojor_len_expr_to_c(init_info$value$expr, args, arg_specs)
                  } else if (is.list(init_info$value) &&
                    init_info$value$kind == "name" &&
                    !is.null(init_info$value$name)) {
                    start_c <- paste0(init_info$value$name, "_val")
                  } else if (is.list(init_info$value) &&
                    init_info$value$kind == "arg") {
                    start_c <- paste0(init_info$value$name, "_val")
                  } else if (!is.list(init_info$value)) {
                    val <- init_info$value
                    start_c <- as.character(as.integer(if (init_info$type == "f64") round(val) else val))
                  }
                } else {
                  start_c <- paste0(start_info$name, "_val")
                }
            } else if (start_info$kind == "expr") {
                start_c <- .mojor_len_expr_to_c(start_info$expr, args, arg_specs)
            }
            if (!is.null(end_info$name)) {
                end_c <- paste0(end_info$name, "_val")
            } else if (!is.null(end_info$end_expr_ast)) {
                end_c <- .mojor_len_expr_to_c(end_info$end_expr_ast, args, arg_specs)
            } else if (!is.null(end_info$end_expr)) {
                end_c <- as.character(end_info$end_expr)
            }
            if (is.null(start_c)) {
                stop(
                  "mojor_transpile: ':' start must be a scalar derived from arguments or length()"
              )
            }
            if (!is.null(start_c) &&
                !is.null(end_c)) {
                out$expr_c <- paste0(
                  "((", start_c, " > ", end_c, ") ? ", start_c, " : ",
                  end_c, ")"
              )
            }
        }
        return(out)
    }
    if (op %in% c("seq.int", "seq")) {
        op_label <- if (source_seq_op %in% c("seq_len", "seq.int", "seq")) {
            source_seq_op
        } else {
            op
        }
        parts <- as.list(seq_expr)[-1]
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
        from_expr <- get_arg("from", 1)
        to_expr <- get_arg("to", 2)
        if (length(parts) ==
            1 && (is.null(nms) ||
            nms[[1]] == "")) {
            to_expr <- from_expr
            from_expr <- NULL
        }
        by_expr <- get_arg("by", 3)
        along_expr <- get_arg("along.with", 4)
        length_expr <- get_arg("length.out", 4)
        is_float_scalar_name <- function(expr) {
            if (!is.name(expr)) {
                return(FALSE)
            }
            nm <- as.character(expr)
            if (nm %in% args) {
                spec <- arg_specs[[nm]]
                return(
                  is.character(spec) &&
                    spec %in% c("f64", "f32")
              )
            }
            if (nm %in% names(scalar_inits)) {
                init_info <- scalar_inits[[nm]]
                return(
                  is.list(init_info) &&
                    !is.null(init_info$type) &&
                    init_info$type %in% c("f64", "f32")
              )
            }
            FALSE
        }
        float_seq_candidate <- identical(op_label, "seq") &&
            !is.null(by_expr) &&
            scalar_expr_float_ok(by_expr) &&
            (expr_has_float(from_expr) ||
                expr_has_float(to_expr) ||
                expr_has_float(by_expr) ||
                is_float_scalar_name(from_expr) ||
                is_float_scalar_name(to_expr) ||
                is_float_scalar_name(by_expr) ||
                !scalar_expr_ok(by_expr))
        if (is.null(from_expr) &&
            !float_seq_candidate) {
            if (is.name(to_expr)) {
                name <- as.character(to_expr)
                if (name %in% args) {
                  spec <- arg_specs[[name]]
                  if (.mojor_is_array(spec)) {
                    return(list(kind = "array", name = name, start = 1L, end_delta = 0L))
                  }
                  if (spec != "i32")
                    stop(sprintf("mojor_transpile: %s() scalar must be i32", op_label))
                } else if (!is_local_i32_name(name)) {
                  stop(
                    sprintf(
                      "mojor_transpile: %s() arg must be a function argument",
                      op_label
                  )
                )
                }
                return(list(kind = "scalar", name = name, start = 1L, end_delta = 0L))
            }
            if (!is.null(to_expr) &&
                scalar_expr_ok(to_expr)) {
                from_expr <- 1L
            }
        }
 # Removed float_seq restrictions - allow any seq() form in
 # any loop context The loop variable type is determined by
 # the expression type, not by how it's used Float values will
 # be converted to Int when used for indexing
        if (!is.null(along_expr)) {
            if (!is.name(along_expr))
                stop(
                  sprintf(
                    "mojor_transpile: %s() along.with must be a name",
                    op_label
                )
              )
            name <- as.character(along_expr)
            if (!(name %in% args) || !.mojor_is_array(arg_specs[[name]])) {
                stop(
                  sprintf(
                    "mojor_transpile: %s() along.with must be an array argument",
                    op_label
                )
              )
            }
            return(list(kind = "array", name = name, start = 1L, end_delta = 0L))
        }
        if (!is.null(length_expr)) {
            if (is.call(length_expr) &&
                as.character(length_expr[[1]]) ==
                  "length" && length(length_expr) ==
                2) {
                arg <- length_expr[[2]]
                if (is.name(arg)) {
                  name <- as.character(arg)
                  if (name %in% args && .mojor_is_array(arg_specs[[name]])) {
                    return(list(kind = "array", name = name, start = 1L, end_delta = 0L))
                  }
                }
                stop(
                  sprintf(
                    "mojor_transpile: %s() length.out must be length(array_arg)",
                    op_label
                )
              )
            }
            len_info <- parse_seq_arg(length_expr, "length.out", op_label)
            if (len_info$kind == "num") {
                stop(
                  sprintf(
                    "mojor_transpile: %s() length.out must be a scalar i32 argument",
                    op_label
                )
              )
            }
            if (len_info$kind == "expr") {
                expr_c <- .mojor_len_expr_to_c(len_info$expr, args, arg_specs)
                return(
                  list(
                    kind = "expr", start = 1L, end_delta = 0L, end_expr_ast = len_info$expr,
                    expr_key = scalar_expr_key(seq_expr),
                    expr_c = expr_c
                )
              )
            }
            return(
                list(kind = "scalar", name = len_info$name, start = 1L, end_delta = 0L)
            )
        }
        if (float_seq_candidate) {
            if (is.null(from_expr))
                from_expr <- 1L
            if (is.null(to_expr))
                stop(
                  sprintf(
                    "mojor_transpile: %s() to must be a scalar expression",
                    op_label
                )
              )
            if (!scalar_expr_float_ok(from_expr))
                stop(
                  sprintf(
                    "mojor_transpile: %s() from must be a scalar expression",
                    op_label
                )
              )
            if (!scalar_expr_float_ok(to_expr))
                stop(
                  sprintf(
                    "mojor_transpile: %s() to must be a scalar expression",
                    op_label
                )
              )
            if (!scalar_expr_float_ok(by_expr))
                stop(
                  sprintf(
                    "mojor_transpile: %s() by must be a scalar expression",
                    op_label
                )
              )
            by_const <- parse_float_const(by_expr)
            if (!is.null(by_const) &&
                abs(by_const) <
                  1e-12) {
                stop(sprintf("mojor_transpile: %s() by must be non-zero", op_label))
            }
            from_c <- .mojor_float_expr_to_c(from_expr, args, arg_specs)
            to_c <- .mojor_float_expr_to_c(to_expr, args, arg_specs)
            by_c <- .mojor_float_expr_to_c(by_expr, args, arg_specs)
            len_expr_ast <- call(
                "as.integer", call(
                  "+", call(
                    "floor", call(
                      "/", call(
                        "-", call("as.double", to_expr),
                        call("as.double", from_expr)
                    ),
                      call("as.double", by_expr)
                  )
                ),
                  1L
              )
            )
            expr_c <- paste0(
                "(int)(floor(((", to_c, ") - (", from_c, ")) / (", by_c,
                ")) + 1.0)"
            )
            return(
                list(
                  kind = "expr", start = 1L, end_delta = 0L, end_expr_ast = len_expr_ast,
                  expr_key = scalar_expr_key(seq_expr),
                  expr_c = expr_c, float_seq = TRUE, iter_expr_float = list(from = from_expr, by = by_expr),
                  iter_expr_spec = "f64", float_seq_check = list(from_c = from_c, to_c = to_c, by_c = by_c)
              )
            )
        }
        if (!is.null(by_expr)) {
            by_info <- parse_seq_arg(by_expr, "by", op_label)
            if (by_info$kind == "num") {
                if (by_info$value == 0L)
                  stop(sprintf("mojor_transpile: %s() by must be non-zero", op_label))
            } else if (by_info$kind == "name" || by_info$kind == "expr") {
 # ok
            } else {
                stop(
                  sprintf(
                    "mojor_transpile: %s() by must be a scalar expression",
                    op_label
                )
              )
            }
        } else {
            by_info <- list(kind = "num", value = 1L)
        }
        start_info <- parse_seq_arg(from_expr, "from", op_label)
        end_info <- parse_end(to_expr)
        if (is.null(end_info))
            stop(
                sprintf(
                  "mojor_transpile: %s() end must be a scalar expression",
                  op_label
              )
            )
        name <- end_info$name
        end_expr <- end_info$end_expr
        if (!is.null(name)) {
            if (name %in% args) {
                spec <- arg_specs[[name]]
                kind <- if (.mojor_is_array(spec))
                    "array" else "scalar"
                if (kind == "scalar" && spec != "i32")
                    stop(sprintf("mojor_transpile: %s() scalar must be i32", op_label))
            } else if (is_local_i32_name(name)) {
                kind <- "scalar"
            } else {
                stop(
                  sprintf(
                    "mojor_transpile: %s() end must be a function argument",
                    op_label
                )
              )
            }
            if (is.null(by_expr) &&
                kind == "scalar") {
                end_info$end_expr_ast <- as.name(name)
            }
        } else {
            kind <- "expr"
        }
        by_val <- if (by_info$kind == "num")
            by_info$value else NULL
        by_name <- if (by_info$kind == "name")
            by_info$name else NULL
        by_expr_ast <- if (by_info$kind == "expr")
            by_info$expr else NULL
        by_expr_key <- if (by_info$kind == "expr")
            by_info$expr_key else NULL
        out <- list(
            kind = kind, name = name, start = 1L, end_delta = end_info$end_delta,
            end_expr = end_expr, by = by_val, by_expr = by_name
        )
        if (is.null(by_expr) &&
            op_label %in% c("seq.int", "seq")) {
            out$colon_dynamic <- TRUE
            start_c <- NULL
            end_c <- NULL
            if (start_info$kind == "num") {
                start_c <- as.character(start_info$value)
            } else if (start_info$kind == "name") {
                start_c <- paste0(start_info$name, "_val")
            } else if (start_info$kind == "expr") {
                start_c <- .mojor_len_expr_to_c(start_info$expr, args, arg_specs)
            }
            if (!is.null(end_info$name)) {
                end_spec <- arg_specs[[end_info$name]]
                if (!.mojor_is_array(end_spec)) {
                  end_c <- paste0(end_info$name, "_val")
                }
            } else if (!is.null(end_info$end_expr_ast)) {
                end_c <- .mojor_len_expr_to_c(end_info$end_expr_ast, args, arg_specs)
            } else if (!is.null(end_info$end_expr)) {
                end_c <- as.character(end_info$end_expr)
            }
            if (!is.null(start_c) &&
                !is.null(end_c)) {
                out$expr_c <- paste0(
                  "((", start_c, " >= ", end_c, ") ? (", start_c, " - ",
                  end_c, " + 1) : (", end_c, " - ", start_c, " + 1))"
              )
                out$expr_key <- scalar_expr_key(seq_expr)
                out$kind <- "expr"
                out$name <- NULL
            }
        }
        if (kind == "expr")
            out$expr_key <- end_info$expr_key
        if (!is.null(end_info$end_expr_ast))
            out$end_expr_ast <- end_info$end_expr_ast
        if (!is.null(by_expr_ast)) {
            out$by_expr_ast <- by_expr_ast
            if (is.null(out$expr_key))
                out$expr_key <- by_expr_key
        }
        if (identical(out$kind, "expr")) {
            out$expr_key <- scalar_expr_key(seq_expr)
            if (is.null(out$expr_c)) {
                if (!is.null(out$end_expr_ast)) {
                  out$expr_c <- .mojor_len_expr_to_c(out$end_expr_ast, args, arg_specs)
                } else if (!is.null(out$end_expr)) {
                  out$expr_c <- as.character(out$end_expr)
                }
            }
        }
        if (start_info$kind == "num") {
            start_val <- start_info$value
            if (is.na(start_val) ||
                start_val < 1L)
                stop(
                  sprintf(
                    "mojor_transpile: %s() must start at 1 or higher",
                    op_label
                )
              )
            out$start <- start_val
            return(out)
        }
        if (is.null(by_expr) &&
            !is.null(end_expr) &&
            end_expr == "1") {
            out$by <- -1L
            out$by_expr <- NULL
        }
        if (start_info$kind == "name") {
            out$start_expr_ast <- as.name(start_info$name)
        } else if (start_info$kind == "expr") {
            out$start_expr_ast <- start_info$expr
            if (is.null(out$expr_key))
                out$expr_key <- start_info$expr_key
        } else {
            stop(
                sprintf(
                  "mojor_transpile: %s() from must be a scalar expression",
                  op_label
              )
            )
        }
        return(out)
    }
    .mojor_err(
        "unsupported loop sequence", seq_expr, "use seq_along/seq_len/seq.int/seq/1:n or rev()/+/-/* on those forms"
    )
}

.mojor_scalar_range_expr_to_mojo <- function(
    expr, arg_specs, args, len_var_map, n_source_name, scalar_inits = NULL,
    nrow_var_map = NULL, ncol_var_map = NULL, dim_var_map = NULL,
    allow_scalar_cast = FALSE
) {
    resolve_array_source_name <- function(name) {
        .mojor_resolve_array_source_name(name, args, arg_specs)
    }
    local_scalars <- scalar_inits
    if (is.null(local_scalars))
        local_scalars <- .mojor_state$current_scalar_inits
    scalar_type_of_name <- function(name) {
        if (name %in% args) {
            spec <- arg_specs[[name]]
            if (.mojor_is_array(spec))
                return("array")
            return(spec)
        }
        if (!is.null(local_scalars) &&
            !is.null(local_scalars[[name]]) &&
            !is.null(local_scalars[[name]]$type)) {
            return(local_scalars[[name]]$type)
        }
        NULL
    }
    emit_cond <- NULL
    emit <- function(e, allow_cast_override = FALSE) {
        allow_cast <- isTRUE(allow_scalar_cast) || isTRUE(allow_cast_override)
        if (is.numeric(e) &&
            length(e) ==
                1) {
            val <- as.integer(e)
            if (is.na(val))
                stop("mojor_transpile: loop range expression must be integer-valued")
            return(as.character(val))
        }
        if (is.integer(e) &&
            length(e) ==
                1) {
            return(as.character(e))
        }
        if (is.name(e)) {
            name <- as.character(e)
            spec <- scalar_type_of_name(name)
            if (identical(spec, "array"))
                stop("mojor_transpile: loop range expression cannot use array values")
            if (!is.null(spec)) {
                if (identical(spec, "i32")) {
                  return(name)
                }
                if (allow_cast && spec %in% c("f64", "f32")) {
                  return(paste0("Int(", name, ")"))
                }
                if (allow_cast && spec %in% c("lgl", "bool")) {
                  return(paste0("(1 if ", name, " else 0)"))
                }
                stop("mojor_transpile: loop range scalar must be i32")
            }
            stop("mojor_transpile: loop range expression must use function arguments")
        }
        if (is.call(e)) {
            op <- as.character(e[[1]])
            if (op == "(" && length(e) ==
                2) {
                return(emit(e[[2]], allow_cast_override = allow_cast_override))
            }
            if (op %in% c("ifelse", "if") &&
                length(e) ==
                    4) {
                cond_expr <- emit_cond(e[[2]])
                yes_expr <- emit(e[[3]])
                no_expr <- emit(e[[4]])
                return(
                  paste0(
                    "((", yes_expr, ") if (", cond_expr, ") else (", no_expr, "))"
                )
              )
            }
            if (op == "as.integer" && length(e) ==
                2) {
                return(
                  paste0(
                    "Int(", emit(e[[2]], allow_cast_override = TRUE),
                    ")"
                )
              )
            }
            if (op %in% c("as.double", "as.single") && length(e) ==
                2) {
                return(emit(e[[2]], allow_cast_override = TRUE))
            }
            if (op == "abs" && length(e) ==
                2) {
                inner <- emit(e[[2]], allow_cast_override = allow_cast_override)
                return(
                  paste0(
                    "((", inner, ") if ((", inner, ") >= 0) else (-(", inner, ")))"
                )
              )
            }
            if (op == "+" && length(e) ==
                2) {
                return(emit(e[[2]], allow_cast_override = allow_cast_override))
            }
            if (op == "-" && length(e) ==
                2) {
                return(
                  paste0(
                    "(-", emit(e[[2]], allow_cast_override = allow_cast_override),
                    ")"
                )
              )
            }
            if (op %in% c("+", "-", "*") &&
                length(e) ==
                  3) {
                return(
                  paste0(
                    "(", emit(e[[2]], allow_cast_override = allow_cast_override),
                    " ", op, " ", emit(e[[3]], allow_cast_override = allow_cast_override),
                    ")"
                )
              )
            }
            if (op %in% c("%/%", "%%") &&
                length(e) ==
                  3) {
                lhs <- emit(e[[2]], allow_cast_override = allow_cast_override)
                rhs <- emit(e[[3]], allow_cast_override = allow_cast_override)
                if (op == "%/%") {
                  return(paste0("Int((", lhs, ") / (", rhs, "))"))
                }
                return(paste0("((", lhs, ") % (", rhs, "))"))
            }
            if (op %in% c("min", "max", "pmin", "pmax") &&
                length(e) >=
                  2) {
                vals <- as.list(e)[-1]
                expand_dim_arg <- function(v) {
                    if (is.call(v) &&
                        as.character(v[[1]]) == "dim" &&
                        length(v) == 2 &&
                        is.name(v[[2]])) {
                        name <- resolve_array_source_name(as.character(v[[2]]))
                        if (is.null(name)) {
                            stop("mojor_transpile: dim() in loop range must use a function argument")
                        }
                        spec <- arg_specs[[name]]
                        if (!.mojor_is_array(spec)) {
                            stop("mojor_transpile: dim() in loop range requires an array argument")
                        }
                        if (.mojor_is_matrix(spec)) {
                            row_map <- if (is.null(nrow_var_map))
                                .mojor_state$current_nrow_var_map else nrow_var_map
                            col_map <- if (is.null(ncol_var_map))
                                .mojor_state$current_ncol_var_map else ncol_var_map
                            row_var <- .mojor_state$`%||%`(row_map[[name]], .mojor_nrow_var_name(name))
                            col_var <- .mojor_state$`%||%`(col_map[[name]], .mojor_ncol_var_name(name))
                            return(c(row_var, col_var))
                        }
                        rank <- .mojor_type_ndim(spec)
                        if (rank <= 1L) {
                            stop("mojor_transpile: dim() reduction in loop range requires matrix or fixed-rank ND array")
                        }
                        dim_map <- if (is.null(dim_var_map))
                            .mojor_state$current_dim_var_map else dim_var_map
                        dim_var <- .mojor_state$`%||%`(dim_map[[name]], .mojor_dim_var_name(name))
                        if (is.null(dim_var)) {
                            .mojor_err(
                                sprintf("loop range uses dim(%s) reduction not available", name),
                                v,
                                "use fixed-rank ND args with dim metadata or pass explicit scalar bounds"
                            )
                        }
                        return(vapply(
                            seq_len(rank),
                            function(k) paste0(dim_var, "[", k - 1L, "]"),
                            character(1)
                        ))
                    }
                    emit(v, allow_cast_override = allow_cast_override)
                }
                parts <- unlist(lapply(vals, expand_dim_arg), use.names = FALSE)
                if (length(parts) == 0L) {
                    stop("mojor_transpile: min/max loop range expression needs at least one scalar-compatible argument")
                }
                cmp <- if (op %in% c("min", "pmin"))
                  "<" else ">"
                expr_mojo <- parts[[1]]
                if (length(parts) >
                  1) {
                  for (p in parts[-1]) {
                    expr_mojo <- paste0(
                      "(", expr_mojo, " if (", expr_mojo, " ", cmp, " ",
                      p, ") else ", p, ")"
                  )
                  }
                }
                return(expr_mojo)
            }
            if (op == "length" && length(e) ==
                2) {
                arg <- e[[2]]
                if (!is.name(arg))
                  stop("mojor_transpile: length() in loop range must use a name")
                name <- resolve_array_source_name(as.character(arg))
                if (is.null(name))
                  stop(
                    "mojor_transpile: length() in loop range must use a function argument"
                )
                spec <- arg_specs[[name]]
                if (!.mojor_is_array(spec))
                  stop("mojor_transpile: length() in loop range requires an array argument")
                len_var <- .mojor_len_var_for(
                  list(kind = "array", name = name),
                  n_source_name, len_var_map
              )
                if (is.null(len_var)) {
                  .mojor_err(
                    "loop range uses array length not available", arg,
                    "use seq_len() with a scalar or add length params"
                )
                }
                return(len_var)
            }
            if (op %in% c("nrow", "ncol") && length(e) == 2) {
                arg <- e[[2]]
                if (!is.name(arg))
                  stop(sprintf("mojor_transpile: %s() in loop range must use a name", op))
                name <- resolve_array_source_name(as.character(arg))
                if (is.null(name))
                  stop(sprintf("mojor_transpile: %s() in loop range must use a function argument", op))
                spec <- arg_specs[[name]]
                if (!.mojor_is_array(spec))
                  stop(sprintf("mojor_transpile: %s() in loop range requires an array argument", op))
                dim_var_map <- if (op == "nrow") nrow_var_map else ncol_var_map
                if (is.null(dim_var_map)) {
                  dim_var_map <- if (op == "nrow")
                    .mojor_state$current_nrow_var_map else .mojor_state$current_ncol_var_map
                }
                dim_var <- .mojor_state$`%||%`(dim_var_map[[name]], NULL)
                if (is.null(dim_var)) {
                  .mojor_err(
                    sprintf("loop range uses %s(%s) not available", op, name),
                    arg,
                    "use matrix args with known dims or pass explicit scalar bounds"
                  )
                }
                return(dim_var)
            }
            if (op %in% c("[", "[[") && length(e) == 3) {
                base <- e[[2]]
                idx <- e[[3]]
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
                  name <- resolve_array_source_name(as.character(base[[2]]))
                  if (is.null(name))
                    stop("mojor_transpile: dim() in loop range must use a function argument")
                  spec <- arg_specs[[name]]
                  if (!.mojor_is_array(spec))
                    stop("mojor_transpile: dim() in loop range requires an array argument")
                  dim_idx <- parse_dim_idx_literal(idx)
                  if (!is.na(dim_idx) && dim_idx < 1L)
                    stop("mojor_transpile: dim() index in loop range must be a positive integer literal or supported scalar expression")
                  if (.mojor_is_matrix(spec)) {
                    if (!is.na(dim_idx) && dim_idx == 1L) {
                      row_map <- if (is.null(nrow_var_map))
                        .mojor_state$current_nrow_var_map else nrow_var_map
                      row_var <- .mojor_state$`%||%`(row_map[[name]], .mojor_nrow_var_name(name))
                      return(row_var)
                    }
                    if (!is.na(dim_idx) && dim_idx == 2L) {
                      col_map <- if (is.null(ncol_var_map))
                        .mojor_state$current_ncol_var_map else ncol_var_map
                      col_var <- .mojor_state$`%||%`(col_map[[name]], .mojor_ncol_var_name(name))
                      return(col_var)
                    }
                    if (is.na(dim_idx)) {
                      row_map <- if (is.null(nrow_var_map))
                        .mojor_state$current_nrow_var_map else nrow_var_map
                      col_map <- if (is.null(ncol_var_map))
                        .mojor_state$current_ncol_var_map else ncol_var_map
                      row_var <- .mojor_state$`%||%`(row_map[[name]], .mojor_nrow_var_name(name))
                      col_var <- .mojor_state$`%||%`(col_map[[name]], .mojor_ncol_var_name(name))
                      if (!is.null(row_var) && !is.null(col_var)) {
                        idx_expr <- .mojor_scalar_range_expr_to_mojo(
                          idx,
                          arg_specs = arg_specs,
                          args = args,
                          len_var_map = len_var_map,
                          n_source_name = n_source_name,
                          scalar_inits = scalar_inits,
                          nrow_var_map = nrow_var_map,
                          ncol_var_map = ncol_var_map,
                          dim_var_map = dim_var_map,
                          allow_scalar_cast = TRUE
                        )
                        return(
                          paste0(
                            "((", row_var, ") if (", idx_expr, " == 1) else (",
                            col_var, " if (", idx_expr, " == 2) else 0))"
                          )
                        )
                      }
                    }
                    if (!is.na(dim_idx))
                      stop("mojor_transpile: dim() index for matrix in loop range must be 1 or 2")
                  }
                  if (.mojor_type_ndim(spec) <= 1L)
                    stop("mojor_transpile: dim() indexing in loop range requires matrix or fixed-rank ND array argument")
                  rank <- .mojor_type_ndim(spec)
                  if (!is.na(dim_idx) && dim_idx > rank)
                    stop("mojor_transpile: dim() index in loop range exceeds declared array rank")
                  dim_map <- if (is.null(dim_var_map))
                    .mojor_state$current_dim_var_map else dim_var_map
                  dim_var <- .mojor_state$`%||%`(dim_map[[name]], .mojor_dim_var_name(name))
                  if (is.null(dim_var)) {
                    .mojor_err(
                      if (is.na(dim_idx)) sprintf("loop range uses dim(%s)[k] not available", name) else sprintf("loop range uses dim(%s)[%d] not available", name, dim_idx),
                      e,
                      "use fixed-rank ND args with dim metadata or pass explicit scalar bounds"
                    )
                  }
                  if (is.na(dim_idx)) {
                    idx_expr <- .mojor_scalar_range_expr_to_mojo(
                      idx,
                      arg_specs = arg_specs,
                      args = args,
                      len_var_map = len_var_map,
                      n_source_name = n_source_name,
                      scalar_inits = scalar_inits,
                      nrow_var_map = nrow_var_map,
                      ncol_var_map = ncol_var_map,
                      dim_var_map = dim_var_map,
                      allow_scalar_cast = TRUE
                    )
                    return(paste0(dim_var, "[(", idx_expr, ") - 1]"))
                  }
                  return(paste0(dim_var, "[", as.integer(dim_idx - 1L), "]"))
                }
            }
        }
        stop(
            "mojor_transpile: loop range expression must use scalar args, length(), nrow()/ncol(), dim(name)[k], dim(name) reductions via min/max, abs(), unary +/- and + - * %/% %%, optional as.integer()/as.double()/as.single() casts, min/max, or scalar ifelse()/if() expressions"
        )
    }
    emit_cond <- function(e) {
        if (is.logical(e) &&
            length(e) ==
                1 && !is.na(e)) {
            return(if (isTRUE(e)) "True" else "False")
        }
        if ((is.integer(e) || is.numeric(e)) &&
            length(e) ==
                1 && !is.na(e)) {
            return(paste0("(Int(", as.integer(e), ") != 0)"))
        }
        if (is.name(e)) {
            name <- as.character(e)
            spec <- scalar_type_of_name(name)
            if (is.null(spec))
                stop("mojor_transpile: loop range condition must use function arguments")
            if (identical(spec, "array"))
                stop("mojor_transpile: loop range condition cannot use array values")
            if (spec %in% c("lgl", "bool"))
                return(name)
            if (identical(spec, "i32"))
                return(paste0("(", name, " != 0)"))
            stop("mojor_transpile: loop range condition must be scalar lgl/bool or i32")
        }
        if (!is.call(e)) {
            stop("mojor_transpile: loop range condition must be a scalar logical expression")
        }
        op <- as.character(e[[1]])
        if (length(op) !=
            1)
            op <- op[[1]]
        if (op == "(" && length(e) == 2) {
            return(emit_cond(e[[2]]))
        }
        if (op == "!" && length(e) == 2) {
            return(paste0("not (", emit_cond(e[[2]]), ")"))
        }
        if (op %in% c("&&", "||") && length(e) == 3) {
            op_mojo <- if (op == "&&")
                "and" else "or"
            return(
              paste0(
                "(", emit_cond(e[[2]]), " ", op_mojo, " ", emit_cond(e[[3]]), ")"
            )
          )
        }
        if (op %in% c("<", "<=", ">", ">=", "==", "!=") &&
            length(e) == 3) {
            return(
              paste0(
                "(", emit(e[[2]]), " ", op, " ", emit(e[[3]]), ")"
            )
          )
        }
        if (op == "as.logical" && length(e) == 2) {
            return(paste0("((", emit(e[[2]]), ") != 0)"))
        }
        stop(
            "mojor_transpile: loop range condition must use scalar lgl/bool/i32 args with !, &&, ||, comparisons, or as.logical()"
        )
    }
    out <- emit(expr)
    if (!grepl("^Int\\(", out)) {
        out <- paste0("Int(", out, ")")
    }
    out
}

.mojor_range_expr <- function(seq_info) {
    canonicalize_end_expr <- function(expr) {
        if (!is.character(expr) || length(expr) != 1L || !nzchar(expr)) {
            return(expr)
        }
        sub(
            "^\\(\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*\\+\\s*1\\s*\\)$",
            "Int(\\1) + 1",
            expr,
            perl = TRUE
        )
    }
    start <- .mojor_state$`%||%`(seq_info$start, 1L)
    end_delta <- .mojor_state$`%||%`(seq_info$end_delta, 0L)
    len_var <- .mojor_state$`%||%`(seq_info$len_var, "n_i")
    start_expr <- .mojor_state$`%||%`(seq_info$start_expr, NULL)
    by_expr <- .mojor_state$`%||%`(seq_info$by_expr, NULL)
    by_val <- .mojor_state$`%||%`(seq_info$by, 1L)
    start_expr_val <- if (is.null(start_expr))
        as.character(start) else start_expr
    step_expr <- if (!is.null(by_expr))
        by_expr else as.character(by_val)
    end_expr <- if (!is.null(seq_info$end_expr)) {
        seq_info$end_expr
    } else if (end_delta == 0L) {
        len_var
    } else if (end_delta < 0L) {
        paste0(len_var, " - ", abs(end_delta))
    } else {
        paste0(len_var, " + ", end_delta)
    }
    if (isTRUE(seq_info$colon_dynamic)) {
        as_int <- function(x) {
            if (grepl("^Int\\(", x)) {
                return(x)
            }
            paste0("Int(", x, ")")
        }
        start_cmp <- as_int(start_expr_val)
        end_cmp <- as_int(end_expr)
        neg_cond <- paste0("(", start_cmp, " > ", end_cmp, ")")
        step_expr <- paste0("(-1 if ", neg_cond, " else 1)")
        end_expr <- paste0("(", end_cmp, " - 1 if ", neg_cond, " else ", end_cmp, " + 1)")
    } else if (!is.null(by_expr) &&
        is.null(seq_info$by)) {
        end_expr <- paste0(
            "(", end_expr, " - 1 if (", by_expr, " < 0) else ", end_expr,
            " + 1)"
        )
    } else if (!is.null(by_val) &&
        by_val < 0L) {
        end_expr <- paste0("(", end_expr, " - 1)")
    } else {
        end_expr <- paste0("(", end_expr, " + 1)")
    }
    end_expr <- canonicalize_end_expr(end_expr)
    if (step_expr == "1") {
        return(
            list(
                range = paste0("range(", start_expr_val, ", ", end_expr, ")"),
                start_expr = start_expr
            )
        )
    }
    list(
        range = paste0(
            "range(", start_expr_val, ", ", end_expr, ", ", step_expr,
            ")"
        ),
        start_expr = start_expr
    )
}

.mojor_stmt_to_mojo <- function(
    stmt, loop_vars, seq_info, types, out_name, out_type, scalar_name = NULL,
    scalar_type = NULL, indent = "    ", na_mode = "forbid", na_guard = "forbid",
    zero_based_vars = NULL
) {
    opt_level <- .mojor_state$`%||%`(.mojor_state$options$opt_level, 2L)
    strict_ir <- isTRUE(.mojor_state$options$ir_only)
    ir_build_res <- tryCatch(
        .mojor_ir_build_stmt(stmt),
        error = function(e) e
    )
    if (inherits(ir_build_res, "error")) {
        if (isTRUE(strict_ir)) {
            stop(
                "mojor_transpile: strict IR build failed: ", conditionMessage(ir_build_res),
                call. = FALSE
            )
        }
        ir_build_res <- NULL
    }
    ir <- ir_build_res
    if (is.null(ir)) {
        .mojor_err("IR failed to build statement", stmt, "simplify the statement")
    }
    ir <- .mojor_ir_normalize(ir)
    .mojor_ir_verify(
        ir, ctx = list(
            ir_only = isTRUE(.mojor_state$options$ir_only),
            type_env = types
        )
    )
    ir <- .mojor_ir_optimize(ir, opt_level = opt_level)
    layout_ctx <- list(
        n_var = "n_i", out_name = out_name, out_type = out_type, out_matrix = isTRUE(.mojor_state$current_out_is_matrix),
        out_array = isTRUE(.mojor_state$current_out_is_array),
        out_nrow_var = .mojor_state$current_out_nrow_var, out_ncol_var = .mojor_state$current_out_ncol_var,
        out_dim_var = .mojor_state$current_out_dim_var, out_ndim_var = .mojor_state$current_out_ndim_var,
        n_source_name = .mojor_state$current_n_source_name, len_var_map = .mojor_state$current_len_var_map,
        dim_var_map = .mojor_state$current_dim_var_map, ndim_var_map = .mojor_state$current_ndim_var_map,
        nrow_var_map = .mojor_state$current_nrow_var_map, ncol_var_map = .mojor_state$current_ncol_var_map,
        tensor_map = .mojor_state$current_tensor_map, type_env = types,
        index_base = .mojor_state$current_index_base, array_layout = .mojor_state$current_array_layout
    )
    ir <- .mojor_ir_lower(ir, ctx = layout_ctx)
    guard_mode <- if (!is.null(na_mode))
        na_mode else na_guard
    zero_based_vars <- .mojor_state$`%||%`(zero_based_vars, character(0))
    emit_res <- .mojor_ir_service_type_and_emit_stmt(
        ir = ir, local_types = types, out_name = out_name, na_mode = guard_mode,
        bounds_check = isTRUE(.mojor_state$options$index_bounds),
        scalar_name = scalar_name, schedule = NULL, loop_var = NULL, unroll = NULL,
        indent = indent, zero_based_vars = zero_based_vars
    )
    lines <- emit_res$mojo_lines
    if (is.null(lines) ||
        length(lines) ==
            0 || any(!nzchar(lines))) {
        .mojor_err(
            "IR failed to emit statement", stmt, "simplify the statement or move it inside the loop"
        )
    }
    lines
}

.mojor_detect_rng_calls <- function(expr) {
    strict_ir <- .mojor_strict_ir_enabled()
    is_missing_call_arg <- function(arg_node) {
        tryCatch(
            is.symbol(arg_node) &&
                identical(
                  as.character(arg_node),
                  ""
              ),
            error = function(e) {
                grepl(
                  "argument.*missing|subscript out of bounds", conditionMessage(e),
                  ignore.case = TRUE
              )
            }
        )
    }
    if (!is.call(expr)) {
        return(FALSE)
    }
    op <- as.character(expr[[1]])
    rng_fns <- .mojor_ir_rng_call_fns()
    if (length(op) ==
        1 && (op %in% rng_fns || op %in% c("sample", "sample.int"))) {
        return(TRUE)
    }
 # Check all arguments recursively (skip missing args from mat[i,
 # ])
    if (length(expr) >
        1) {
        for (i in 2:length(expr)) {
            arg <- tryCatch(
                .mojor_call_arg(expr, i),
                error = function(e) e
            )
            if (inherits(arg, "error")) {
                if (isTRUE(strict_ir)) {
                  stop(
                    sprintf(
                      "mojor_transpile: strict RNG call analysis failed: %s",
                      conditionMessage(arg)
                  ),
                    call. = FALSE
                )
                }
                next
            }
 # Skip NULL and empty symbol (missing indices)
            has_rng <- FALSE
            if (!is.null(arg) &&
                !is_missing_call_arg(arg)) {
                has_rng <- .mojor_detect_rng_calls(arg)
            }
            if (has_rng) {
                return(TRUE)
            }
        }
    }
    FALSE
}

.mojor_blocks_need_rng <- function(blocks) {
    for (blk in blocks) {
        if (.mojor_detect_rng_calls(blk)) {
            return(TRUE)
        }
    }
    FALSE
}

.mojor_collect_rng_calls <- function(expr) {
    strict_ir <- .mojor_strict_ir_enabled()
    rng_fns <- .mojor_ir_rng_call_fns()
    out <- character(0)
    is_missing_call_arg <- function(arg_node) {
        tryCatch(
            is.symbol(arg_node) &&
                identical(
                  as.character(arg_node),
                  ""
              ),
            error = function(e) {
                grepl(
                  "argument.*missing|subscript out of bounds", conditionMessage(e),
                  ignore.case = TRUE
              )
            }
        )
    }
    walk <- function(node) {
        if (!is.call(node)) {
            return()
        }
        op <- as.character(node[[1]])
        if (length(op) ==
            1 && op %in% rng_fns)
            out <<- c(out, op)
        if (length(op) ==
            1 && op %in% c("sample", "sample.int"))
            out <<- c(out, "runif")
        if (length(node) >
            1) {
            for (i in 2:length(node)) {
                arg <- tryCatch(
                  .mojor_call_arg(node, i),
                  error = function(e) e
              )
                if (inherits(arg, "error")) {
                  if (isTRUE(strict_ir)) {
                    stop(
                      sprintf(
                        "mojor_transpile: strict RNG call analysis failed: %s",
                        conditionMessage(arg)
                    ),
                      call. = FALSE
                  )
                  }
                  next
                }
                if (!is.null(arg) &&
                  !is_missing_call_arg(arg)) {
                  walk(arg)
                }
            }
        }
    }
    walk(expr)
    unique(out)
}

.mojor_rng_all_helper_symbols <- function() {
    map <- .mojor_ir_rng_helper_symbol_map()
    unique(unlist(map, use.names = FALSE))
}

.mojor_rng_helper_symbols_for_calls <- function(calls) {
    calls <- unique(as.character(calls))
    calls <- calls[!is.na(calls) &
        nzchar(calls)]
    if (length(calls) ==
        0) {
        return(.mojor_rng_all_helper_symbols())
    }

    map <- .mojor_ir_rng_helper_symbol_map()
    supported <- names(map)

    out <- character(0)
    for (fn in calls) {
        syms <- map[[fn]]
        if (is.null(syms)) {
            stop(
                sprintf(
                  ".mojor_rng_helper_symbols_for_calls: unknown RNG call '%s' (supported: %s)",
                  fn, paste(supported, collapse = ", ")
              )
            )
        }
        out <- c(out, syms)
    }
    unique(out)
}

.mojor_blocks_collect_rng_calls <- function(blocks) {
    seen <- character(0)
    for (blk in blocks) {
        seen <- c(seen, .mojor_collect_rng_calls(blk))
    }
    unique(seen)
}

.mojor_blocks_need_rng_tables <- function(blocks) {
    table_rng_fns <- .mojor_ir_rng_table_fns()
    seen <- character(0)
    for (blk in blocks) {
        seen <- c(seen, .mojor_collect_rng_calls(blk))
    }
    length(
        intersect(
            unique(seen),
            table_rng_fns
        )
    ) >
        0
}

# Scan blocks for mojor_c_call usage
.mojor_blocks_need_ffi <- function(blocks) {
    .detect_ffi <- function(expr) {
        if (!is.call(expr) &&
            !is.recursive(expr)) {
            return(FALSE)
        }
        if (is.call(expr) &&
            identical(
                as.character(expr[[1]]),
                "mojor_c_call"
            )) {
            return(TRUE)
        }
        for (i in seq_along(expr)) {
            if (.detect_ffi(expr[[i]])) {
                return(TRUE)
            }
        }
        FALSE
    }
    for (blk in blocks) {
        if (.detect_ffi(blk)) {
            return(TRUE)
        }
    }
    FALSE
}

.mojor_collect_pre_loop_out_assigns <- function(blocks, out_name, scalar_inits) {
    if (is.null(out_name)) {
        return(list())
    }
    is_loop_stmt <- function(stmt) {
        is.call(stmt) &&
            as.character(stmt[[1]]) %in%
                c("for", "while", "repeat")
    }
    is_out_assign <- function(stmt) {
        is.call(stmt) &&
            as.character(stmt[[1]]) %in%
                c("<-", "=") &&
            is.call(stmt[[2]]) &&
            as.character(stmt[[2]][[1]]) ==
                "[" && identical(
            as.character(stmt[[2]][[2]]),
            out_name
        )
    }
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
            if (!is.null(out_name) &&
                identical(lhs, out_name)) {
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
            }
            if (!is.null(scalar_inits[[lhs]])) {
                return(TRUE)
            }
        }
        FALSE
    }
    is_preloop_stmt <- function(stmt) {
        if (is_out_assign(stmt)) {
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
    pre_loop_out_assigns <- list()
    for (b in blocks) {
        if (is_loop_stmt(b))
            break
        if (is_preloop_stmt(b)) {
            pre_loop_out_assigns[[length(pre_loop_out_assigns) +
                1L]] <- b
            next
        }
        if (is_ignorable_stmt(b)) {
            next
        }
        break
    }
    pre_loop_out_assigns
}

.mojor_detect_mask_output_assign <- function(blocks, out_name, types) {
    if (is.null(out_name)) {
        return(NULL)
    }
    found <- NULL
    walk <- function(expr) {
        if (missing(expr) ||
            is.null(expr)) {
            return()
        }
        if (is.call(expr)) {
            if (length(expr) >=
                1) {
                op <- as.character(expr[[1]])
 # Use length check to ensure scalar comparison
                if (length(op) ==
                  1 && op %in% c("<-", "=") &&
                  length(expr) >=
                    3) {
                  lhs <- expr[[2]]
                  rhs <- expr[[3]]
                  rhs_op <- if (is.call(rhs) &&
                    length(rhs) >=
                      1)
                    as.character(rhs[[1]]) else character(0)
 # Ensure all conditions return scalar logicals
                  if (is.name(lhs) &&
                    length(lhs) ==
                      1 && identical(
                    as.character(lhs),
                    out_name
                ) &&
                    length(rhs_op) ==
                      1 && rhs_op %in% c("[", "[[") &&
                    length(rhs) ==
                      3) {
                    idx <- .mojor_call_arg(rhs, 3L)
                    if (!is.null(idx) && is.name(idx)) {
                      idx_name <- as.character(idx)
                      if (length(idx_name) ==
                        1 && !is.null(types[[idx_name]]) &&
                        .mojor_is_logical_mask_type(types[[idx_name]])) {
                        found <<- idx_name
                      }
                    }
                  }
                }
            }
            for (p in as.list(expr)[-1]) walk(p)
        }
    }
    for (b in blocks) walk(b)
    found
}

.mojor_validate_char_indexing <- function(blocks, matrix_dimnames) {
    if (is.null(matrix_dimnames))
        matrix_dimnames <- list()
    is_control_arg <- function(arg_name, op) {
        if (is.null(arg_name) ||
            !nzchar(arg_name)) {
            return(FALSE)
        }
        low <- tolower(arg_name)
        if (identical(low, "drop")) {
            return(TRUE)
        }
        identical(op, "[[") &&
            identical(low, "exact")
    }
    dimnames_for_pos <- function(dn, idx_pos) {
        dim_names <- dn$dim_names
        if (is.null(dim_names)) {
            dim_names <- list(dn$row_names, dn$col_names)
        }
        if (idx_pos < 1L || idx_pos >
            length(dim_names)) {
            return(NULL)
        }
        dim_names[[idx_pos]]
    }
    resolve_idx <- function(var, idx_expr, idx_pos, src) {
        if (!is.character(idx_expr) ||
            length(idx_expr) !=
                1) {
            return()
        }
        names_vec <- NULL
        dn <- matrix_dimnames[[var]]
        if (!is.null(dn)) {
            names_vec <- dimnames_for_pos(dn, idx_pos)
        }
        if (is.null(names_vec) && idx_pos == 1L) {
            vn_map <- .mojor_state$vector_names
            if (!is.null(vn_map) && !is.null(vn_map[[var]])) {
                names_vec <- vn_map[[var]]
            }
        }
        if (is.null(names_vec)) {
            guessed <- tryCatch(
                .mojor_ir_guess_dimname_positions(idx_expr, idx_pos),
                error = function(e) NULL
            )
            if (!is.null(guessed)) {
                return()
            }
            .mojor_err(
                "character indexing requires dimnames/names", src, paste0("object '", var, "' has no dimnames/names")
            )
        }
        if (is.na(match(idx_expr, names_vec))) {
            .mojor_err("index name not found in dimnames", src)
        }
    }
    walk <- function(expr) {
        if (missing(expr) ||
            is.null(expr)) {
            return()
        }
        if (is.call(expr)) {
            op <- as.character(expr[[1]])
            op <- op[1]
            if (op %in% c("[", "[[") &&
                length(expr) >=
                  3) {
                base <- expr[[2]]
                if (is.name(base)) {
                  var <- as.character(base)
                  call_parts <- as.list(expr)
                  part_names <- names(call_parts)
                  idx_pos <- 0L
                  for (k in seq.int(3L, length(expr))) {
                    arg_name <- if (is.null(part_names))
                      "" else part_names[[k]]
                    if (is_control_arg(arg_name, op)) {
                      next
                    }
                    idx_pos <- idx_pos + 1L
                    idx_expr <- .mojor_call_arg(expr, k)
                    if (is.null(idx_expr)) {
                      next
                    }
                    resolve_idx(var, idx_expr, idx_pos, expr)
                  }
                }
            }
            for (p in as.list(expr)[-1]) walk(p)
        }
    }
    for (b in blocks) walk(b)
}

.mojor_collect_chr_index_arg_vars <- function(blocks, arg_specs) {
    if (length(blocks) == 0 || is.null(arg_specs) || !is.list(arg_specs)) {
        return(character(0))
    }
    found <- character(0)
    is_control_arg <- function(arg_name, op) {
        if (is.null(arg_name) || !nzchar(arg_name)) {
            return(FALSE)
        }
        low <- tolower(arg_name)
        if (identical(low, "drop")) {
            return(TRUE)
        }
        identical(op, "[[") && identical(low, "exact")
    }
    walk <- function(expr) {
        if (missing(expr) || is.null(expr)) {
            return()
        }
        if (!is.call(expr)) {
            return()
        }
        op <- as.character(expr[[1]])[1]
        if (op %in% c("[", "[[") && length(expr) >= 3) {
            call_parts <- as.list(expr)
            part_names <- names(call_parts)
            for (k in seq.int(3L, length(expr))) {
                arg_name <- if (is.null(part_names))
                  "" else part_names[[k]]
                if (is_control_arg(arg_name, op)) {
                    next
                }
                idx_expr <- .mojor_call_arg(expr, k)
                if (is.null(idx_expr)) {
                    next
                }
                if (is.name(idx_expr)) {
                    nm <- as.character(idx_expr)
                    if (!is.null(arg_specs[[nm]]) && identical(arg_specs[[nm]], "chr[]")) {
                        found <<- unique(c(found, nm))
                    }
                }
            }
        }
        for (part in as.list(expr)[-1]) {
            walk(part)
        }
    }
    for (b in blocks) {
        walk(b)
    }
    found
}

.mojor_collect_chr_index_local_vars <- function(blocks) {
    local_chr_vars <- list()
    used_as_index <- character(0)
    for (b in blocks) {
        if (!is.call(b) || !identical(as.character(b[[1]]), "<-")) next
        lhs <- b[[2]]
        if (!is.name(lhs)) next
        rhs <- b[[3]]
        if (!is.call(rhs) || !identical(as.character(rhs[[1]]), "c")) next
        vals <- as.list(rhs)[-1]
        if (length(vals) == 0L) next
        if (!all(vapply(vals, is.character, logical(1)))) next
        local_chr_vars[[as.character(lhs)]] <- vapply(vals, as.character, character(1))
    }
    if (length(local_chr_vars) == 0L) {
        return(list(vars = list(), used = character(0)))
    }
    # Only flag vars used element-wise inside loops (e.g. mat[rows[i], ...])
    # not whole-vector uses like mat[rows, cols] which are handled by
    # the existing Dict[String, Int] path.
    # Also capture target array + dimension for compile-time position resolution.
    target_info <- list()  # name -> list(target, dim)
    walk_loop <- function(expr) {
        if (missing(expr) || is.null(expr)) return()
        if (!is.call(expr)) return()
        op <- as.character(expr[[1]])[1]
        if (op %in% c("[", "[[") && length(expr) >= 3) {
            target_name <- if (is.name(expr[[2]])) as.character(expr[[2]]) else NULL
            for (k in seq.int(3L, length(expr))) {
                idx_expr <- .mojor_call_arg(expr, k)
                if (is.null(idx_expr)) next
                # Check for element-wise subscript: rows[i]
                if (is.call(idx_expr) && identical(as.character(idx_expr[[1]]), "[") &&
                    length(idx_expr) >= 3 && is.name(idx_expr[[2]])) {
                    nm <- as.character(idx_expr[[2]])
                    if (nm %in% names(local_chr_vars) && !is.null(target_name)) {
                        used_as_index <<- unique(c(used_as_index, nm))
                        dim_pos <- k - 2L  # 1-based dimension position
                        target_info[[nm]] <<- list(target = target_name, dim = dim_pos)
                    }
                }
            }
        }
        for (part in as.list(expr)[-1]) walk_loop(part)
    }
    # Walk only loop bodies
    walk_find_loops <- function(expr) {
        if (missing(expr) || is.null(expr)) return()
        if (!is.call(expr)) return()
        op <- as.character(expr[[1]])[1]
        if (op == "for" && length(expr) >= 4) {
            walk_loop(expr[[4]])
        } else if (op == "while" && length(expr) >= 3) {
            walk_loop(expr[[3]])
        } else if (op == "repeat" && length(expr) >= 2) {
            walk_loop(expr[[2]])
        }
        for (part in as.list(expr)[-1]) walk_find_loops(part)
    }
    for (b in blocks) walk_find_loops(b)
    used_names <- intersect(names(local_chr_vars), used_as_index)
    list(vars = local_chr_vars[used_names],
         used = used_names,
         target_info = target_info[used_names])
}

.mojor_extract_loop_infos <- function(loops) {
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
 # indexed loops Detect: for (v in array_name) where
 # array_name is just a variable
            if (is.name(loop_seq)) {
                array_name <- as.character(loop_seq)
 # Transform to: for (__mojor_iter_i in
 # seq_along(array_name)) with v <-
 # array_name[__mojor_iter_i]
                iter_var <- "__mojor_iter_i"
                loop_seq <- call("seq_along", as.name(array_name))
 # Prepend: v <- array_name[__mojor_iter_i]
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
 # Use the internal iterator variable for the loop
                loop_var <- iter_var
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

.mojor_transpile_layout_ctx <- function(type_env = NULL, reduction_mode = NULL) {
    layout_ctx <- .mojor_ir_layout_ctx(
        n_var = "n_i", nrow_var = .mojor_state$current_out_nrow_var, ncol_var = .mojor_state$current_out_ncol_var,
        dim_var_map = .mojor_state$current_dim_var_map, ndim_var_map = .mojor_state$current_ndim_var_map,
        nrow_var_map = .mojor_state$current_nrow_var_map, ncol_var_map = .mojor_state$current_ncol_var_map,
        len_var_map = .mojor_state$current_len_var_map, tensor_map = .mojor_state$current_tensor_map,
        index_base = .mojor_state$current_index_base, array_layout = .mojor_state$current_array_layout,
        fusion_allow_control_flow_simple = isTRUE(.mojor_state$options$fusion_allow_control_flow_simple),
        fusion_allow_broadcast_nd_identity = isTRUE(.mojor_state$options$fusion_allow_broadcast_nd_identity)
    )
    layout_ctx$n_source_name <- .mojor_state$current_n_source_name
    layout_ctx$type_env <- type_env
    layout_ctx$reduction_mode <- reduction_mode
    layout_ctx
}

.mojor_transpile_schedule_ctx <- function(schedule, type_env = NULL) {
    schedule_ctx <- schedule
    schedule_ctx$type_env <- type_env
    schedule_ctx
}

.mojor_indent_lines <- function(lines, indent) {
    paste0(indent, lines)
}

.mojor_emit_argext_lines <- function(
    layout_runtime, arg_dtype, scalar_reduction_arg, want_min, scalar_init
) {
    c(
        paste0(
            "var _mojor_arg_layout = ", layout_runtime("_MOJOR_ARGEXT_LAYOUT", "IndexList[1](n_i)")
        ),
        paste0(
            "var _mojor_arg_tensor = LayoutTensor[mut=False, ", arg_dtype,
            ", _MOJOR_ARGEXT_LAYOUT, MutAnyOrigin](", scalar_reduction_arg,
            ", _mojor_arg_layout)"
        ),
        "var _mojor_out_ptr = alloc[Int64](1)", paste0(
            "var _mojor_out_layout = ", layout_runtime("_MOJOR_ARGEXT_LAYOUT", "IndexList[1](1)")
        ),
        "var _mojor_out_tensor = LayoutTensor[mut=True, DType.int64, _MOJOR_ARGEXT_LAYOUT, MutAnyOrigin](_mojor_out_ptr, _mojor_out_layout)",
        paste0(
            if (want_min) "argmin" else "argmax", "(_mojor_arg_tensor, 0, _mojor_out_tensor)"
        ),
        paste0(scalar_init, " = Int32(_mojor_out_ptr[0] + 1)"),
        "_mojor_out_ptr.free()"
    )
}

.mojor_emit_status_return_lines <- function(status, ret = 0L, indent = "        ") {
    c(
        paste0(indent, "_mojor_set_status(status_ptr, ", status, ")"),
        paste0(indent, "return ", ret)
    )
}

.mojor_emit_gpu_dim_stage_lines <- function(dim_var, ndim_var, loop_var, indent = "        ") {
    loop_body_indent <- paste0(indent, "    ")
    c(
        paste0(
            indent, "var ", dim_var, "_stage = ctx.enqueue_create_host_buffer[DType.int32](",
            ndim_var, ")"
        ),
        paste0(indent, "for ", loop_var, " in range(", ndim_var, "):"),
        paste0(
            loop_body_indent, dim_var, "_stage[", loop_var, "] = ", dim_var,
            "_host[", loop_var, "]"
        ),
        paste0(
            indent, "var ", dim_var, " = ctx.enqueue_create_buffer[DType.int32](",
            ndim_var, ")"
        ),
        paste0(
            indent, "ctx.enqueue_copy(dst_buf=", dim_var, ", src_buf=",
            dim_var, "_stage)"
        )
    )
}

.mojor_emit_gpu_dim_host_setup_lines <- function(
    dim_var, dim_param, ndim_var, ndim_param, status = -3L, indent = "    "
) {
    c(
        paste0(
            indent, "var ", dim_var, "_host: ImmutInt32Ptr = ", dim_param,
            "_ptr.bitcast[Int32]()"
        ),
        paste0(indent, "var ", ndim_var, " = Int(", ndim_param, ")"),
        paste0(indent, "if ", ndim_var, " < 0:"),
        .mojor_emit_status_return_lines(status, indent = paste0(indent, "    "))
    )
}

.mojor_append_gpu_wrapper_broadcast_sig <- function(gpu_sig, dim_arrays) {
    gpu_sig <- c(
        gpu_sig, paste0(.mojor_out_dim_param_name(), "_ptr: ImmutOpaqueAny"),
        paste0(.mojor_out_ndim_param_name(), ": Int32")
    )
    for (a in dim_arrays) {
        gpu_sig <- c(
            gpu_sig, paste0(
                .mojor_dim_param_name(a),
                "_ptr: ImmutOpaqueAny"
            ),
            paste0(
                .mojor_ndim_param_name(a),
                ": Int32"
            )
        )
    }
    gpu_sig
}

.mojor_append_gpu_kernel_broadcast_sig <- function(kernel_sig_generic, kernel_sig_wrapper, kernel_call, dim_arrays) {
    kernel_sig_generic <- c(
        kernel_sig_generic, paste0(.mojor_out_dim_var_name(), ": ImmutInt32Ptr"),
        paste0(.mojor_out_ndim_var_name(), ": Int")
    )
    kernel_sig_wrapper <- c(
        kernel_sig_wrapper, paste0(.mojor_out_dim_var_name(), ": ImmutInt32Ptr"),
        paste0(.mojor_out_ndim_var_name(), ": Int")
    )
    kernel_call <- c(kernel_call, .mojor_out_dim_var_name(), .mojor_out_ndim_var_name())
    for (a in dim_arrays) {
        kernel_sig_generic <- c(
            kernel_sig_generic, paste0(
                .mojor_dim_var_name(a),
                ": ImmutInt32Ptr"
            ),
            paste0(
                .mojor_ndim_var_name(a),
                ": Int"
            )
        )
        kernel_sig_wrapper <- c(
            kernel_sig_wrapper, paste0(
                .mojor_dim_var_name(a),
                ": ImmutInt32Ptr"
            ),
            paste0(
                .mojor_ndim_var_name(a),
                ": Int"
            )
        )
        kernel_call <- c(
            kernel_call, .mojor_dim_var_name(a),
            .mojor_ndim_var_name(a)
        )
    }
    list(
        kernel_sig_generic = kernel_sig_generic, kernel_sig_wrapper = kernel_sig_wrapper,
        kernel_call = kernel_call
    )
}

.mojor_append_gpu_dim_host_setup_lines <- function(gpu_lines, dim_arrays) {
    gpu_lines <- c(
        gpu_lines, .mojor_emit_gpu_dim_host_setup_lines(
            .mojor_out_dim_var_name(), .mojor_out_dim_param_name(), .mojor_out_ndim_var_name(),
            .mojor_out_ndim_param_name()
        )
    )
    for (a in dim_arrays) {
        gpu_lines <- c(
            gpu_lines, .mojor_emit_gpu_dim_host_setup_lines(
                .mojor_dim_var_name(a),
                .mojor_dim_param_name(a),
                .mojor_ndim_var_name(a),
                .mojor_ndim_param_name(a)
            )
        )
    }
    gpu_lines
}

.mojor_append_gpu_dim_stage_lines <- function(gpu_lines, dim_arrays) {
    gpu_lines <- c(
        gpu_lines, .mojor_emit_gpu_dim_stage_lines(
            .mojor_out_dim_var_name(), .mojor_out_ndim_var_name(), "__mojor_di_out"
        )
    )
    for (a in dim_arrays) {
        gpu_lines <- c(
            gpu_lines, .mojor_emit_gpu_dim_stage_lines(
                .mojor_dim_var_name(a),
                .mojor_ndim_var_name(a),
                paste0("__mojor_di_", a)
            )
        )
    }
    gpu_lines
}

.mojor_append_gpu_kernel_broadcast_len_prelude <- function(raw_kernel_defs, dim_arrays) {
    for (a in dim_arrays) {
        len_var <- .mojor_len_var_name(a)
        dim_var <- .mojor_dim_var_name(a)
        ndim_var <- .mojor_ndim_var_name(a)
        idx_var <- paste0("__mojor_di_", a)
        raw_kernel_defs <- c(
            raw_kernel_defs, paste0("        var ", len_var, ": Int = 1"),
            paste0("        for ", idx_var, " in range(", ndim_var, "):"),
            paste0(
                "            ", len_var, " *= Int(", dim_var, "[", idx_var,
                "])"
            )
        )
    }
    raw_kernel_defs
}

.mojor_build_gpu_signature_parts <- function(
    gpu, arg_specs, use_broadcast_nd, dim_arrays, buf_ptr_type, mut_ptr_type,
    immut_ptr_type, scalar_type, gpu_dtype_tag
) {
    matrix2d_mode <- identical(gpu$index_mode, "matrix2d")
    gpu_sig <- c(
        "ctxp: MutCtxPtr", paste0("out_bufp: ", buf_ptr_type),
        "n: Int32"
    )
    for (a in gpu$args) {
        spec <- arg_specs[[a]]
        if (.mojor_is_array(spec)) {
            gpu_sig <- c(gpu_sig, paste0(a, "_bufp: ", buf_ptr_type))
        } else {
            gpu_sig <- c(gpu_sig, paste0(a, ": ", .mojor_mojo_type(spec)))
        }
    }
    if (isTRUE(matrix2d_mode)) {
        gpu_sig <- c(gpu_sig, "__mojor_m2_nrow: Int32", "__mojor_m2_ncol: Int32")
    }
    if (use_broadcast_nd && length(dim_arrays) >
        0) {
        gpu_sig <- .mojor_append_gpu_wrapper_broadcast_sig(gpu_sig, dim_arrays)
    }
    gpu_sig <- c(gpu_sig, "status_ptr: MutOpaqueAny")

    kernel_sig_generic <- c(
        "out_ptr: UnsafePointer[mut=True, type=Scalar[dtype], origin=MutAnyOrigin]",
        "n: Int32"
    )
    kernel_sig_wrapper <- c(
        paste0("out_ptr: ", mut_ptr_type),
        "n: Int32"
    )
    kernel_call <- c("out_bufp[0]", "n")
    for (a in gpu$array_args) {
        kernel_sig_generic <- c(
            kernel_sig_generic, paste0(
                a, ": UnsafePointer[mut=False, type=Scalar[dtype], origin=ImmutAnyOrigin]"
            )
        )
        kernel_sig_wrapper <- c(kernel_sig_wrapper, paste0(a, ": ", immut_ptr_type))
        kernel_call <- c(kernel_call, paste0(a, "_bufp[0]"))
    }
    if (isTRUE(matrix2d_mode)) {
        kernel_sig_generic <- c(
            kernel_sig_generic,
            "__mojor_m2_nrow: Int32",
            "__mojor_m2_ncol: Int32"
        )
        kernel_sig_wrapper <- c(
            kernel_sig_wrapper,
            "__mojor_m2_nrow: Int32",
            "__mojor_m2_ncol: Int32"
        )
        kernel_call <- c(kernel_call, "__mojor_m2_nrow", "__mojor_m2_ncol")
    }
    if (length(gpu$scalar_f32) >
        0 && gpu_dtype_tag == "float32") {
        for (sn in gpu$scalar_f32) {
            kernel_sig_generic <- c(kernel_sig_generic, paste0(sn, ": Scalar[dtype]"))
            kernel_sig_wrapper <- c(kernel_sig_wrapper, paste0(sn, ": ", scalar_type))
            kernel_call <- c(kernel_call, sn)
        }
    }
    if (length(gpu$scalar_f64) >
        0) {
        for (sn in gpu$scalar_f64) {
            if (gpu_dtype_tag == "float32") {
                kernel_sig_generic <- c(kernel_sig_generic, paste0(sn, "_f32: Scalar[dtype]"))
                kernel_sig_wrapper <- c(kernel_sig_wrapper, paste0(sn, "_f32: Float32"))
                kernel_call <- c(kernel_call, paste0(sn, "_f32"))
            } else {
                kernel_sig_generic <- c(kernel_sig_generic, paste0(sn, ": Scalar[dtype]"))
                kernel_sig_wrapper <- c(kernel_sig_wrapper, paste0(sn, ": Float64"))
                kernel_call <- c(kernel_call, sn)
            }
        }
    }
    if (use_broadcast_nd && length(dim_arrays) >
        0) {
        kernel_sig_parts <- .mojor_append_gpu_kernel_broadcast_sig(kernel_sig_generic, kernel_sig_wrapper, kernel_call, dim_arrays)
        kernel_sig_generic <- kernel_sig_parts$kernel_sig_generic
        kernel_sig_wrapper <- kernel_sig_parts$kernel_sig_wrapper
        kernel_call <- kernel_sig_parts$kernel_call
    }
    list(
        gpu_sig = gpu_sig, kernel_sig_generic = kernel_sig_generic, kernel_sig_wrapper = kernel_sig_wrapper,
        kernel_call = kernel_call
    )
}

.mojor_build_gpu_raw_kernel_parts <- function(
    gpu, gpu_dtype_tag, kernel_sig_generic, kernel_sig_wrapper, kernel_call,
    use_broadcast_nd, dim_arrays
) {
    gpu_ew_scalar <- gpu$ew_scalar
    loop_var <- if (!is.null(gpu$linear_loop_var) && nzchar(gpu$linear_loop_var)) gpu$linear_loop_var else gpu$loop_var
    matrix2d_mode <- identical(gpu$index_mode, "matrix2d")
    matrix2d_use_coords <- isTRUE(matrix2d_mode) &&
        (isTRUE(gpu$matrix2d_uses_control_flow) || isTRUE(gpu$matrix2d_uses_neighbors))
    kernel_sig_wrapper_names <- sub(":.*$", "", kernel_sig_wrapper)
    raw_kernel_defs <- c(
        "comptime block_size = 256", "var grid_size = (n_i + block_size - 1) // block_size",
        "@parameter", paste0(
            "fn _mojor_ew_kernel[dtype: DType](", paste(kernel_sig_generic, collapse = ", "),
            ") -> None:"
        ),
        "    var tid = block_idx.x * block_dim.x + thread_idx.x", "    if tid < UInt(n):",
        paste0("        var ", loop_var, " = Int(tid)")
    )
    if (isTRUE(matrix2d_mode)) {
        raw_kernel_defs <- c(
            raw_kernel_defs,
            "        var __mojor_m2_nrow_i: Int = Int(__mojor_m2_nrow)",
            "        var __mojor_m2_ncol_i: Int = Int(__mojor_m2_ncol)"
        )
        if (isTRUE(matrix2d_use_coords)) {
            raw_kernel_defs <- c(
                raw_kernel_defs,
                paste0("        var __mojor_m2_i: Int = (", loop_var, " % __mojor_m2_nrow_i) + 1"),
                paste0("        var __mojor_m2_j: Int = (", loop_var, " // __mojor_m2_nrow_i) + 1")
            )
        }
    }
    if (use_broadcast_nd && length(dim_arrays) >
        0) {
        raw_kernel_defs <- .mojor_append_gpu_kernel_broadcast_len_prelude(raw_kernel_defs, dim_arrays)
    }
    for (expr in gpu_ew_scalar) {
        raw_kernel_defs <- c(raw_kernel_defs, paste0("        out_ptr[", loop_var, "] = ", expr))
    }
    raw_kernel_defs <- c(
        raw_kernel_defs, "@parameter", paste0(
            "fn _mojor_ew_kernel_", gpu_dtype_tag, "(", paste(kernel_sig_wrapper, collapse = ", "),
            ") -> None:"
        ),
        paste0(
            "    _mojor_ew_kernel[DType.", gpu_dtype_tag, "](", paste(kernel_sig_wrapper_names, collapse = ", "),
            ")"
        )
    )
    raw_kernel_call <- c(
        paste0(
            "ctx.enqueue_function[_mojor_ew_kernel_", gpu_dtype_tag, ", _mojor_ew_kernel_",
            gpu_dtype_tag, "]("
        ),
        paste0(
            "    ", paste(kernel_call, collapse = ", "),
            ","
        ),
        "    grid_dim=grid_size,", "    block_dim=block_size,", ")", "ctx.synchronize()",
        .mojor_emit_status_return_lines(1L, ret = 1L, indent = "")
    )
    list(raw_kernel_defs = raw_kernel_defs, raw_kernel_call = raw_kernel_call)
}

.mojor_build_gpu_wrapper_body_lines <- function(
    gpu, gpu_name, gpu_sig, buf_null, bytes_per, use_broadcast_nd, dim_arrays,
    raw_kernel_defs, raw_kernel_call, gpu_dtype_tag, elementwise_gpu_layouttensor
) {
    matrix2d_mode <- identical(gpu$index_mode, "matrix2d")
    gpu_lines <- c(
        "", paste0("@export(\"", gpu_name, "\", ABI=\"C\")"),
        paste0(
            "fn ", gpu_name, "(", paste(gpu_sig, collapse = ", "),
            ") -> Int32:"
        ),
        paste0("    if ctxp == NULL_CTX or out_bufp == ", buf_null, ":"),
        .mojor_emit_status_return_lines(-1L),
        "    @parameter", "    if not has_accelerator():", .mojor_emit_status_return_lines(-1L),
        "    var n_i = Int(n)", "    if n_i <= 0:", .mojor_emit_status_return_lines(0L)
    )
    for (a in gpu$array_args) {
        gpu_lines <- c(
            gpu_lines, paste0("    if ", a, "_bufp == ", buf_null, ":"),
            .mojor_emit_status_return_lines(-1L)
        )
    }
    if (!is.null(gpu$elementwise_size)) {
        gpu_lines <- c(
            gpu_lines, paste0("    if n_i != ", gpu$elementwise_size, ":"),
            .mojor_emit_status_return_lines(-3L)
        )
    }
    if (isTRUE(matrix2d_mode)) {
        gpu_lines <- c(
            gpu_lines,
            "    if Int(__mojor_m2_nrow) <= 0 or Int(__mojor_m2_ncol) <= 0:",
            .mojor_emit_status_return_lines(-3L)
        )
    }
    if (use_broadcast_nd && length(dim_arrays) >
        0) {
        gpu_lines <- .mojor_append_gpu_dim_host_setup_lines(gpu_lines, dim_arrays)
    }
    gpu_lines <- c(
        gpu_lines, paste0("    if not _mojor_gpu_limit_ok(n_i, 1, ", bytes_per, "):"),
        .mojor_emit_status_return_lines(-8L),
        "    try:", "        var ctx = ctxp[0]"
    )
    if (length(gpu$scalar_f64) >
        0 && gpu_dtype_tag == "float32") {
        for (sn in gpu$scalar_f64) {
            gpu_lines <- c(
                gpu_lines, paste0("        var ", sn, "_f32: Float32 = Float32(", sn, ")")
            )
        }
    }
    if (use_broadcast_nd && length(dim_arrays) >
        0) {
        gpu_lines <- .mojor_append_gpu_dim_stage_lines(gpu_lines, dim_arrays)
    }
    gpu_lines <- c(gpu_lines, .mojor_indent_lines(raw_kernel_defs, "        "))

    use_layouttensor <- isTRUE(elementwise_gpu_layouttensor) &&
        !isTRUE(gpu$broadcast_nd) &&
        !identical(gpu$index_mode, "matrix2d")
    if (use_layouttensor) {
        gpu_lines <- c(
            gpu_lines, paste0("        comptime _MOJOR_EW_DTYPE = DType.", gpu_dtype_tag),
            "        comptime _MOJOR_EW_LAYOUT = Layout.row_major(1)",
            "        var layout = RuntimeLayout[_MOJOR_EW_LAYOUT].row_major(IndexList[1](n_i))"
        )
        for (a in gpu$array_args) {
            gpu_lines <- c(
                gpu_lines, paste0("        var ", a, "_ptr = ", a, "_bufp[0].unsafe_ptr()"),
                paste0(
                  "        var ", a, "_dev = LayoutTensor[mut=False, _MOJOR_EW_DTYPE, _MOJOR_EW_LAYOUT, MutAnyOrigin](",
                  a, "_ptr, layout)"
              )
            )
        }
        gpu_lines <- c(
            gpu_lines, paste0("        var ", gpu$out_name, "_ptr = out_bufp[0].unsafe_ptr()"),
            paste0(
                "        var ", gpu$out_name, "_dev = LayoutTensor[mut=True, _MOJOR_EW_DTYPE, _MOJOR_EW_LAYOUT, MutAnyOrigin](",
                gpu$out_name, "_ptr, layout)"
            ),
            "        @parameter", "        @always_inline", "        fn _mojor_elementwise[dtype: DType, simd_width: Int, rank: Int, alignment: Int = 1](indices: IndexList[rank]) capturing -> None:"
        )
        for (a in gpu$array_args) {
            gpu_lines <- c(
                gpu_lines, paste0(
                  "            var ", a, "_chunk = ", a, "_dev.load[simd_width](indices)"
              )
            )
        }
        for (expr in gpu$ew_specs) {
            gpu_lines <- c(
                gpu_lines, paste0(
                  "            ", gpu$out_name, "_dev.store[simd_width](indices, ",
                  expr, ")"
              )
            )
        }
        gpu_lines <- c(
            gpu_lines, "        @parameter", "        @always_inline",
            paste0(
                "        fn _mojor_elementwise_", gpu_dtype_tag, "[simd_width: Int, rank: Int, alignment: Int = 1](indices: IndexList[rank]) capturing -> None:"
            ),
            paste0(
                "            _mojor_elementwise[DType.", gpu_dtype_tag,
                ", simd_width, rank, alignment](indices)"
            ),
            "        comptime _MOJOR_EW_SIMD = simd_width_of[_MOJOR_EW_DTYPE, target=get_gpu_target()]()",
            "        if (n_i % Int(_MOJOR_EW_SIMD)) != 0:"
        )
        gpu_lines <- c(gpu_lines, .mojor_indent_lines(raw_kernel_call, "            "))
        gpu_lines <- c(
            gpu_lines, paste0(
                "        elementwise[_mojor_elementwise_", gpu_dtype_tag,
                ", _MOJOR_EW_SIMD, target=\"gpu\"](n_i, ctx)"
            ),
            "        ctx.synchronize()", .mojor_emit_status_return_lines(1L, ret = 1L),
            "    except:", .mojor_emit_status_return_lines(-2L)
        )
    } else {
        gpu_lines <- c(
            gpu_lines, .mojor_indent_lines(raw_kernel_call, "        "),
            "    except:", .mojor_emit_status_return_lines(-2L)
        )
    }
    gpu_lines
}

.mojor_emit_out_matrix_tensor_wrapper <- function(mojo_lines, out_name, out_type, layout_runtime) {
    if (is.null(out_name)) {
        return(mojo_lines)
    }
    if (!isTRUE(.mojor_state$current_out_is_matrix) ||
        is.null(.mojor_state$current_out_nrow_var) ||
        is.null(.mojor_state$current_out_ncol_var)) {
        return(mojo_lines)
    }
    nrow_var <- .mojor_state$current_out_nrow_var
    ncol_var <- .mojor_state$current_out_ncol_var
    dtype_str <- if (out_type %in% c("i32[]", "i32[,]"))
        "DType.int32" else if (out_type %in% c("lgl[]", "lgl[,]"))
        "DType.int32" else if (out_type %in% c("f32[]", "f32[,]"))
        "DType.float32" else "DType.float64"
    tensor_name <- paste0(out_name, "_tensor")
    default_tensor_name <- paste0("__mojor_tensor_", out_name)
    current_tensor <- NULL
    if (!is.null(.mojor_state$current_tensor_map)) {
        current_tensor <- .mojor_state$current_tensor_map[[out_name]]
    }
    if (is.null(current_tensor) ||
        identical(current_tensor, default_tensor_name)) {
        mojo_lines <- c(
            mojo_lines, paste0(
                "    var ", out_name, "_layout = ", layout_runtime(
                  "_MOJOR_MATRIX_LAYOUT", paste0(
                    "IndexList[2](Int(", nrow_var, "), Int(", ncol_var,
                    "))"
                )
              )
            )
        )
        mojo_lines <- c(
            mojo_lines, paste0(
                "    var ", tensor_name, " = LayoutTensor[mut=True, ",
                dtype_str, ", _MOJOR_MATRIX_LAYOUT, MutAnyOrigin](", out_name,
                ", ", out_name, "_layout)"
            )
        )
        if (is.null(.mojor_state$current_tensor_map)) {
            .mojor_state$current_tensor_map <- list()
        }
        .mojor_state$current_tensor_map[[out_name]] <- tensor_name
    }
    mojo_lines
}
