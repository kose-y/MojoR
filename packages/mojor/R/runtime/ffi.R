# =============================================================================
# MojoR FFI ()
# =============================================================================
# Foreign Function Interface for calling C functions from MojoR
# kernels and exporting MojoR kernels as C functions. API:
# mojor_declare_c(name, args, returns, library) mojor_c_call(name,
# ...) mojor_export_c_header(built, header_path)
# =============================================================================

# =============================================================================
# Constants
# =============================================================================

MOJOR_FFI_VERSION <- "1.0.0"

# =============================================================================
# C Function Declaration
# =============================================================================

#' Declare an external C function for use in MojoR kernels
#'
#' @param name Function name
#' @param args Named list of argument types (e.g., `list(x = 'f64*', n = 'i32')`)
#' @param returns Return type (e.g., `'f64'`)
#' @param library Path to shared library (optional)
#'
#' @return Invisibly returns the declared function info
#'
#' @examples
#' \dontrun{
#' # Declare a C function from libfft.so
#' mojor_declare_c(
#' name = 'fft_compute',
#' args = list(x = 'f64*', n = 'i32'),
#' returns = 'f64',
#' library = 'libfft.so'
#' )
#'
#' # Use in a kernel
#' f <- function(x) {
#' out <- numeric(length(x))
#' for (i in seq_along(x)) {
#' out[i] <- mojor_c_call('fft_compute', x[i])
#' }
#' out
#' }
#' }
#'
#' @export
mojor_declare_c <- function(name, args, returns, library = NULL) {
 # Validate inputs
    if (!is.character(name) ||
        length(name) !=
            1 || !nzchar(name)) {
        stop("mojor_declare_c: name must be a non-empty character string")
    }
    if (!is.list(args)) {
        stop("mojor_declare_c: args must be a named list")
    }
    if (is.null(names(args)) ||
        any(
            names(args) ==
                ""
        )) {
        stop("mojor_declare_c: args must have named elements")
    }
    if (!is.character(returns) ||
        length(returns) !=
            1 || !nzchar(returns)) {
        stop("mojor_declare_c: returns must be a non-empty character string")
    }
    if (!is.null(library) &&
        (!is.character(library) ||
            length(library) !=
                1 || !nzchar(library))) {
        stop(
            "mojor_declare_c: library must be a non-empty character string or NULL"
        )
    }

 # Validate argument types
    valid_types <- c("f64*", "f32*", "i32*", "lgl*", "f64", "f32", "i32", "lgl")
    for (arg_type in args) {
        if (!arg_type %in% valid_types) {
            stop(
                "mojor_declare_c: invalid argument type '", arg_type, "' (valid: ",
                paste(valid_types, collapse = ", "),
                ")"
            )
        }
    }

 # Validate return type
    if (!returns %in% valid_types) {
        stop(
            "mojor_declare_c: invalid return type '", returns, "' (valid: ",
            paste(valid_types, collapse = ", "),
            ")"
        )
    }

 # Store declaration in state
    .mojor_state$declared_c_functions[[name]] <- list(
        name = name, args = args, returns = returns, library = library,
        declared_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    )

    invisible(.mojor_state$declared_c_functions[[name]])
}

#' Call a declared C function from a MojoR kernel
#'
#' @param name Function name (must be declared with `mojor_declare_c()`)
#' @param ... Arguments to pass to the C function
#'
#' @return The result of the C function call
#'
#' @export
mojor_c_call <- function(name, ...) {
    if (!is.character(name) ||
        length(name) !=
            1 || !nzchar(name)) {
        stop("mojor_c_call: name must be a non-empty character string")
    }

 # Check if function is declared
    if (is.null(.mojor_state$declared_c_functions) ||
        !name %in% names(.mojor_state$declared_c_functions)) {
        stop(
            "mojor_c_call: function '", name, "' not declared. Use mojor_declare_c() first."
        )
    }

 # Get declaration
    decl <- .mojor_state$declared_c_functions[[name]]
    call_args <- list(...)
    expected_arity <- length(decl$args)
    actual_arity <- length(call_args)
    if (!identical(actual_arity, expected_arity)) {
        stop(
            "mojor_c_call: argument count mismatch for '", name, "': expected ",
            expected_arity, ", got ", actual_arity
        )
    }
    decl_arg_types <- as.character(unname(unlist(decl$args, use.names = FALSE)))
    decl_arg_names <- names(decl$args)
    for (i in seq_along(call_args)) {
        expected_type <- decl_arg_types[[i]]
        if (is.null(expected_type) ||
            !is.character(expected_type) ||
            length(expected_type) !=
                1 || !nzchar(expected_type)) {
            stop(
                "mojor_c_call: declaration for '", name, "' has invalid type at argument #",
                i
            )
        }
        if (!.mojor_ffi_arg_value_compatible(call_args[[i]], expected_type)) {
            arg_label <- if (!is.null(decl_arg_names) &&
                length(decl_arg_names) >=
                  i && nzchar(decl_arg_names[[i]])) {
                decl_arg_names[[i]]
            } else {
                paste0("#", i)
            }
            stop(
                "mojor_c_call: argument type mismatch for '", name, "' argument '",
                arg_label, "' (position ", i, "): expected ", expected_type,
                ", got ", .mojor_ffi_describe_arg_value(call_args[[i]])
            )
        }
    }

 # Create IR node for C call
    .mojor_ir_c_call(
        name = name, args = call_args, returns = decl$returns, library = decl$library,
        expected_arity = expected_arity, arg_types = decl_arg_types, arg_names = decl_arg_names
    )
}

# =============================================================================
# C Call IR Node
# =============================================================================

#' Create a C call IR node
#'
#' @param name Function name
#' @param args List of arguments
#' @param returns Return type
#' @param library Library path (optional)
#' @param expected_arity Optional expected argument count (for verifier checks)
#' @param arg_types Optional declared argument type vector
#' @param arg_names Optional declared argument name vector
#'
#' @return C call IR node
#'
#' @noexport
.mojor_ir_c_call <- function(
    name, args, returns, library = NULL, expected_arity = NULL, arg_types = NULL,
    arg_names = NULL
) {
    list(
        kind = "c_call", name = name, args = args, returns = returns, library = library,
        expected_arity = expected_arity, arg_types = arg_types, arg_names = arg_names,
        src = NULL
    )
}

# =============================================================================
# C Call Emitter
# =============================================================================

#' Emit Mojo code for a C call
#'
#' @param node C call IR node
#' @param ctx Emission context
#'
#' @return Character vector of Mojo code lines
#'
#' @noexport
.mojor_emit_c_call <- function(node, ctx) {
    if (node$kind != "c_call") {
        stop(".mojor_emit_c_call: node must be a c_call IR node")
    }

 # Get declaration
    decl <- .mojor_state$declared_c_functions[[node$name]]
    if (is.null(decl)) {
        stop(".mojor_emit_c_call: function '", node$name, "' not declared")
    }

 # Build argument list
    arg_list <- character(0)
    for (i in seq_along(node$args)) {
        arg <- node$args[[i]]
        arg_name <- names(node$args)[[i]]
        if (is.null(arg_name))
            arg_name <- paste0("arg", i)

 # Emit argument expression
        arg_code <- .mojor_emit_expr(arg, ctx)
        arg_list <- c(arg_list, arg_code)
    }

 # Build function call
    call_lines <- character(0)

 # Import library if specified
    if (!is.null(decl$library)) {
        call_lines <- c(call_lines, paste0("from ", decl$library, " import ", node$name))
    }

 # Emit call
    if (decl$returns == "void" || decl$returns == "None") {
        call_lines <- c(
            call_lines, paste0(
                node$name, "(", paste(arg_list, collapse = ", "),
                ")"
            )
        )
    } else {
        result_var <- ctx$unique_var("c_result")
        call_lines <- c(
            call_lines, paste0(
                "var ", result_var, " = ", node$name, "(", paste(arg_list, collapse = ", "),
                ")"
            )
        )
        call_lines <- c(call_lines, paste0("return ", result_var))
    }

    call_lines
}

# =============================================================================
# C Header Export
# =============================================================================

#' Export a compiled kernel as a C header file
#'
#' @param built The built kernel object returned by `mojor_build()`
#' @param header_path Path to output header file
#' @param namespace Optional namespace for the exported functions
#'
#' @return Invisibly returns the header content
#'
#' @examples
#' \dontrun{
#' f <- function(x) {
#' out <- numeric(length(x))
#' for (i in seq_along(x)) {
#' out[i] <- x[i] * 2
#' }
#' out
#' }
#'
#' built <- mojor_build(f, x = 'f64[]')
#' mojor_export_c_header(built, 'my_kernel.h')
#' }
#'
#' @export
mojor_export_c_header <- function(built, header_path, namespace = NULL) {
 # Validate inputs
    if (is.null(built)) {
        stop("mojor_export_c_header: built cannot be NULL")
    }
    if (is.null(built$kernel)) {
        stop("mojor_export_c_header: built object missing 'kernel' field")
    }
    if (is.null(built$trans)) {
        stop("mojor_export_c_header: built object missing 'trans' field")
    }
    if (is.null(header_path)) {
        stop("mojor_export_c_header: header_path cannot be NULL")
    }
    if (!is.null(namespace) &&
        (!is.character(namespace) ||
            length(namespace) !=
                1 || !nzchar(namespace))) {
        stop(
            "mojor_export_c_header: namespace must be a non-empty character string or NULL"
        )
    }

    trans <- built$trans
    kernel_name <- built$kernel

 # Get function signature (raw kernel ABI).
    abi_types <- trans$kernel_types
    if (is.null(abi_types) ||
        !is.list(abi_types)) {
        abi_types <- trans$types
    }
    args <- names(abi_types)
    arg_specs <- abi_types[args]

 # Build argument declarations (must match raw kernel ABI from
 # mojor_build()).
    arg_decls <- character(0)
    for (i in seq_along(args)) {
        arg_name <- args[[i]]
        arg_spec <- arg_specs[[i]]

        c_type <- .mojor_c_type(arg_spec)
        if (is.null(c_type)) {
            stop(
                "mojor_export_c_header: unsupported type '", arg_spec,
                "' for argument '", arg_name, "'"
            )
        }
        arg_decls <- c(arg_decls, paste0(c_type, " ", arg_name))
    }

 # Output and metadata parameters mirror the generated extern
 # declaration.
    out_type <- trans$out_type
    out_ptr_type <- .mojor_c_ptr_type(out_type)
    if (is.null(out_ptr_type)) {
        stop(
            "mojor_export_c_header: unsupported output type '", out_type,
            "'"
        )
    }
    out_ptr_name <- "out_ptr"
    if (!is.null(trans$out_name) &&
        is.character(trans$out_name) &&
        length(trans$out_name) ==
            1 && nzchar(trans$out_name)) {
        out_ptr_name <- paste0(trans$out_name, "_ptr")
    }

    len_arrays <- trans$len_arrays
    if (is.null(len_arrays))
        len_arrays <- character(0)
    nrow_arrays <- trans$nrow_arrays
    if (is.null(nrow_arrays))
        nrow_arrays <- character(0)
    ncol_arrays <- trans$ncol_arrays
    if (is.null(ncol_arrays))
        ncol_arrays <- character(0)
    dim_arrays <- trans$dim_arrays
    if (is.null(dim_arrays))
        dim_arrays <- character(0)
    out_matrix <- isTRUE(trans$out_matrix)
    out_array <- isTRUE(trans$out_array)
    broadcast_nd <- isTRUE(trans$broadcast_nd)
    rng_needed <- isTRUE(trans$rng_needed)

    sig_decls <- c(
        arg_decls, paste0(out_ptr_type, " ", out_ptr_name),
        "int __mojor_n"
    )

    if (isTRUE(rng_needed)) {
        sig_decls <- c(sig_decls, "uint64_t* __mojor_rng_state")
    }
    if (length(len_arrays) >
        0) {
        sig_decls <- c(
            sig_decls, vapply(
                len_arrays, function(a) paste0("int ", .mojor_len_param_name(a)),
                character(1)
            )
        )
    }
    if (length(nrow_arrays) >
        0) {
        sig_decls <- c(
            sig_decls, vapply(
                nrow_arrays, function(a) paste0("int ", .mojor_nrow_param_name(a)),
                character(1)
            )
        )
    }
    if (length(ncol_arrays) >
        0) {
        extra_ncol <- ncol_arrays[!(ncol_arrays %in% nrow_arrays)]
        if (length(extra_ncol) >
            0) {
            sig_decls <- c(
                sig_decls, vapply(
                  extra_ncol, function(a) paste0("int ", .mojor_ncol_param_name(a)),
                  character(1)
              )
            )
        }
    }
    if (length(dim_arrays) >
        0) {
        dim_decls <- as.vector(
            rbind(
                vapply(
                  dim_arrays, function(a) paste0(
                    "int* ", .mojor_dim_param_name(a),
                    "_ptr"
                ),
                  character(1)
              ),
                vapply(
                  dim_arrays, function(a) paste0("int ", .mojor_ndim_param_name(a)),
                  character(1)
              )
            )
        )
        sig_decls <- c(sig_decls, dim_decls)
    }
    if (isTRUE(out_matrix)) {
        sig_decls <- c(
            sig_decls, paste0("int ", .mojor_out_nrow_param_name()),
            paste0("int ", .mojor_out_ncol_param_name())
        )
    }
    if (isTRUE(out_array) ||
        isTRUE(broadcast_nd)) {
        sig_decls <- c(
            sig_decls, paste0("int* ", .mojor_out_dim_param_name(), "_ptr"),
            paste0("int ", .mojor_out_ndim_param_name())
        )
    }
    na_mode <- trans$na_mode
    has_na_count <- is.character(na_mode) &&
        length(na_mode) ==
            1 && !is.na(na_mode) &&
        na_mode %in% c("forbid", "propagate")
    if (has_na_count) {
        sig_decls <- c(sig_decls, "int* __mojor_na_count_ptr")
    }

    func_name <- kernel_name
    func_sig <- paste0(
        "void ", func_name, "(", paste(sig_decls, collapse = ", "),
        ")"
    )
    alias_name <- if (!is.null(namespace))
        paste0(namespace, "_", kernel_name) else NULL

 # Build header content
    header_lines <- character(0)

 # Include guard
    header_guard <- paste0(
        "MOJOR_", toupper(gsub("[^A-Za-z0-9]", "_", kernel_name)),
        "_H"
    )
    header_lines <- c(header_lines, paste0("#ifndef ", header_guard))
    header_lines <- c(header_lines, paste0("#define ", header_guard))
    header_lines <- c(header_lines, "")

 # Include stdint for fixed-width types
    header_lines <- c(header_lines, "#include <stdint.h>")
    header_lines <- c(header_lines, "")

 # Function documentation
    header_lines <- c(header_lines, paste0("/* MojoR kernel: ", kernel_name, " */"))
    header_lines <- c(header_lines, "/* Raw kernel ABI generated by mojor_build() */")
    header_lines <- c(
        header_lines, paste0(
            "/* Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            " */"
        )
    )
    header_lines <- c(header_lines, "")
    if (!is.null(alias_name)) {
        header_lines <- c(
            header_lines, paste0("#define ", alias_name, " ", kernel_name),
            ""
        )
    }

 # Function declaration
    header_lines <- c(header_lines, paste0(func_sig, ";"))
    header_lines <- c(header_lines, "")

 # End include guard
    header_lines <- c(header_lines, paste0("#endif /* ", header_guard, " */"))

 # Write to file
    dir.create(
        dirname(header_path),
        recursive = TRUE, showWarnings = FALSE
    )
    writeLines(header_lines, header_path)

    invisible(header_lines)
}

#' Convert Mojo type to C type
#'
#' @param mojo_type Mojo type string
#'
#' @return C type string or NULL if unsupported
#'
#' @noexport
.mojor_mojo_to_c_type <- function(mojo_type) {
    type_map <- list(
        f64 = "double", `f64[]` = "double*", f32 = "float", `f32[]` = "float*",
        i32 = "int32_t", `i32[]` = "int32_t*", lgl = "int32_t", `lgl[]` = "int32_t*"
    )

    if (mojo_type %in% names(type_map)) {
        type_map[[mojo_type]]
    } else {
        NULL
    }
}

#' Convert FFI type to Mojo type for external_call emission
#'
#' @param ffi_type FFI type string (e.g., 'f64', 'f64*')
#'
#' @return Mojo type string (e.g., 'Float64', 'UnsafePointer[Float64]')
#'
#' @noexport
.mojor_ffi_to_mojo_type <- function(ffi_type) {
    type_map <- list(
        f64 = "Float64", f32 = "Float32", i32 = "Int32", lgl = "Int32",
        `f64*` = "UnsafePointer[Float64]", `f32*` = "UnsafePointer[Float32]",
        `i32*` = "UnsafePointer[Int32]", `lgl*` = "UnsafePointer[Int32]"
    )
    if (!ffi_type %in% names(type_map)) {
        stop(".mojor_ffi_to_mojo_type: unsupported type '", ffi_type, "'")
    }
    type_map[[ffi_type]]
}

.mojor_ffi_is_pointer_type <- function(ffi_type) {
    is.character(ffi_type) &&
        length(ffi_type) ==
            1 && grepl("\\*$", ffi_type)
}

.mojor_ffi_is_i32_scalar_value <- function(value) {
    if (is.integer(value) &&
        length(value) ==
            1 && !is.na(value)) {
        return(TRUE)
    }
    if (!is.numeric(value) ||
        is.logical(value) ||
        length(value) !=
            1 || is.na(value)) {
        return(FALSE)
    }
    if (!is.finite(value)) {
        return(FALSE)
    }
    rounded <- round(as.numeric(value))
    if (abs(
        as.numeric(value) -
            rounded
    ) >
        1e-08) {
        return(FALSE)
    }
    rounded >= -2147483648 && rounded <= 2147483647
}

.mojor_ffi_is_i32_vector_value <- function(value) {
    if (is.integer(value)) {
        return(TRUE)
    }
    if (!is.numeric(value) ||
        is.logical(value)) {
        return(FALSE)
    }
    if (length(value) ==
        0) {
        return(TRUE)
    }
    finite <- is.finite(value) |
        is.na(value)
    if (!all(finite)) {
        return(FALSE)
    }
    non_na <- value[!is.na(value)]
    if (length(non_na) ==
        0) {
        return(TRUE)
    }
    rounded <- round(non_na)
    if (any(
        abs(non_na - rounded) >
            1e-08
    )) {
        return(FALSE)
    }
    all(rounded >= -2147483648 & rounded <= 2147483647)
}

.mojor_ffi_describe_arg_value <- function(value) {
    if (is.null(value)) {
        return("NULL")
    }
    type_tag <- typeof(value)
    len <- length(value)
    if (!is.null(dim(value))) {
        dims <- paste(
            dim(value),
            collapse = "x"
        )
        return(paste0(type_tag, "[", dims, "]"))
    }
    paste0(type_tag, "[", len, "]")
}

.mojor_ffi_arg_value_compatible <- function(value, ffi_type) {
    if (!is.character(ffi_type) ||
        length(ffi_type) !=
            1 || !nzchar(ffi_type)) {
        return(FALSE)
    }
    if (.mojor_ffi_is_pointer_type(ffi_type)) {
        if (!is.atomic(value) ||
            !is.null(dim(value))) {
            return(FALSE)
        }
        if (identical(ffi_type, "f64*") ||
            identical(ffi_type, "f32*")) {
            return(
                is.numeric(value) &&
                  !is.logical(value)
            )
        }
        if (identical(ffi_type, "i32*")) {
            return(.mojor_ffi_is_i32_vector_value(value))
        }
        if (identical(ffi_type, "lgl*")) {
            if (is.logical(value)) {
                return(TRUE)
            }
            if (is.numeric(value) ||
                is.integer(value)) {
                if (length(value) ==
                  0) {
                  return(TRUE)
                }
                return(
                  all(
                    is.na(value) |
                      value %in% c(0, 1)
                )
              )
            }
            return(FALSE)
        }
        return(FALSE)
    }

    if (identical(ffi_type, "f64") ||
        identical(ffi_type, "f32")) {
        return(
            is.numeric(value) &&
                !is.logical(value) &&
                length(value) ==
                  1 && !is.na(value)
        )
    }
    if (identical(ffi_type, "i32")) {
        return(.mojor_ffi_is_i32_scalar_value(value))
    }
    if (identical(ffi_type, "lgl")) {
        if (is.logical(value) &&
            length(value) ==
                1 && !is.na(value)) {
            return(TRUE)
        }
        if ((is.integer(value) ||
            is.numeric(value)) &&
            length(value) ==
                1 && !is.na(value)) {
            return(value %in% c(0, 1))
        }
        return(FALSE)
    }
    FALSE
}

.mojor_ffi_ir_type_compatible <- function(ffi_type, ir_type) {
    if (!is.character(ffi_type) ||
        length(ffi_type) !=
            1 || !nzchar(ffi_type)) {
        return(FALSE)
    }
    if (!is.character(ir_type) ||
        length(ir_type) !=
            1 || !nzchar(ir_type)) {
        return(FALSE)
    }
    array_rank <- function(type_str) {
        if (!grepl("\\[", type_str)) {
            return(NA_integer_)
        }
        if (grepl("\\[\\]$", type_str)) {
            return(1L)
        }
        m_nd <- regmatches(type_str, regexpr("\\[([0-9]+)d\\]$", type_str))
        if (length(m_nd) == 1 && nchar(m_nd) > 0) {
            return(as.integer(sub(".*\\[([0-9]+)d\\]$", "\\1", m_nd)))
        }
        m_commas <- regmatches(type_str, regexpr("\\[([,]+)\\]$", type_str))
        if (length(m_commas) == 1 && nchar(m_commas) > 0) {
            comma_run <- sub("^\\[([,]+)\\]$", "\\1", m_commas)
            return(as.integer(nchar(comma_run) + 1L))
        }
        NA_integer_
    }
    array_base <- function(type_str) {
        sub("\\[.*\\]$", "", type_str)
    }
    pointer_elem_allowed <- switch(
        ffi_type,
        `f64*` = c("f64"),
        `f32*` = c("f32"),
        `i32*` = c("i32", "lgl", "bool"),
        `lgl*` = c("lgl", "bool", "i32"),
        NULL
    )
    if (!is.null(pointer_elem_allowed)) {
        rank <- array_rank(ir_type)
        if (is.na(rank) || rank < 1L) {
            return(FALSE)
        }
        return(array_base(ir_type) %in% pointer_elem_allowed)
    }
    allowed <- switch(
        ffi_type, f64 = c("f64", "f32", "i32"),
        f32 = c("f32", "i32"),
        i32 = c("i32", "lgl", "bool"),
        lgl = c("lgl", "bool", "i32"),
        NULL
    )
    !is.null(allowed) &&
        ir_type %in% allowed
}
