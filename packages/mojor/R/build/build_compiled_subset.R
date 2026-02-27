# =============================================================================
# Compiled Subset Build Helpers
# =============================================================================
# Shared compiled-subset build path used when transpile emits
# prelowered/runtime-bridge payloads.

.mojor_build_compiled_subset_kernel <- function(
    trans, fn, name, build_dir, load, verbose, cache, cache_dir, fast_math,
    ...
) {
    if (!isTRUE(.mojor_needs_compiled_subset_build(trans))) {
        return(NULL)
    }

 # Compiled subset kernels: scalar, matrix, or vector-returning
 # payloads.
    rng_needed <- isTRUE(trans$rng_needed)

    if (is.null(cache_dir)) {
        cache_dir <- tryCatch(
            tools::R_user_dir("mojor", "cache"),
            error = function(e) tempdir()
        )
    }

 # Generate cache key
    mojo_version <- tryCatch(
        system("mojo --version", intern = TRUE),
        error = function(e) "unknown"
    )
    build_flags <- .mojor_fast_math_flags(fast_math)
    literal_arrays_key <- ""
    if (!is.null(trans$literal_array_args) &&
        length(trans$literal_array_args) >
            0) {
        literal_arrays_key <- paste(
            vapply(
                names(trans$literal_array_args),
                function(nm) {
                  desc <- trans$literal_array_args[[nm]]
                  vals <- as.numeric(desc$values)
                  vals_str <- paste(
                    formatC(vals, digits = 17, format = "g"),
                    collapse = ","
                )
                  paste0(nm, ":", desc$type, ":", vals_str)
                }, character(1)
            ),
            collapse = ";"
        )
    }
    tier9_runtime_plan_key <- ""
    if (!is.null(trans$tier9_runtime_plan) &&
        is.list(trans$tier9_runtime_plan)) {
        tier9_runtime_plan_key <- paste(
            capture.output(dput(trans$tier9_runtime_plan)),
            collapse = ""
        )
    }
    key_input <- paste(
        trans$mojo, paste(
            names(trans$types),
            trans$types, sep = "=", collapse = ";"
        ),
        paste0("expression_kernel=TRUE"),
        paste0("return_type=", trans$return_type),
        paste0("is_matrix_output=", isTRUE(trans$is_matrix_output)),
        paste0("is_vector_output=", isTRUE(trans$is_vector_output)),
        paste0(
            "out_type=", if (!is.null(trans$out_type))
                trans$out_type else ""
        ),
        paste0("set_match_needed=", isTRUE(trans$set_match_needed)),
        paste0(
            "set_match_op=", if (!is.null(trans$set_match_op))
                trans$set_match_op else ""
        ),
        paste0("quantile_needed=", isTRUE(trans$quantile_needed)),
        paste0(
            "quantile_op=", if (!is.null(trans$quantile_op))
                trans$quantile_op else ""
        ),
        paste0("rng_needed=", rng_needed),
        paste0("string_builtin=", isTRUE(trans$string_builtin)),
        paste0(
            "string_op=", if (!is.null(trans$string_op))
                trans$string_op else ""
        ),
        paste0("tier9_runtime_plan=", tier9_runtime_plan_key),
        paste0(
            "string_sep=", if (!is.null(trans$string_sep))
                trans$string_sep else ""
        ),
        paste0(
            "string_args=", if (!is.null(trans$string_arg_names))
                paste(trans$string_arg_names, collapse = ",") else ""
        ),
        paste0(
            "string_start_kind=", if (!is.null(trans$string_start) &&
                !is.null(trans$string_start$kind))
                trans$string_start$kind else ""
        ),
        paste0(
            "string_start_name=", if (!is.null(trans$string_start) &&
                !is.null(trans$string_start$name))
                trans$string_start$name else ""
        ),
        paste0(
            "string_start_value=", if (!is.null(trans$string_start) &&
                !is.null(trans$string_start$value))
                as.integer(trans$string_start$value) else ""
        ),
        paste0(
            "string_stop_kind=", if (!is.null(trans$string_stop) &&
                !is.null(trans$string_stop$kind))
                trans$string_stop$kind else ""
        ),
        paste0(
            "string_stop_name=", if (!is.null(trans$string_stop) &&
                !is.null(trans$string_stop$name))
                trans$string_stop$name else ""
        ),
        paste0(
            "string_stop_value=", if (!is.null(trans$string_stop) &&
                !is.null(trans$string_stop$value))
                as.integer(trans$string_stop$value) else ""
        ),
        paste0(
            "vector_len_const=", if (!is.null(trans$vector_len_const))
                as.integer(trans$vector_len_const) else ""
        ),
        paste0(
            "vector_len_scale=", if (!is.null(trans$vector_len_scale))
                as.integer(trans$vector_len_scale) else ""
        ),
        paste0("literal_array_args=", literal_arrays_key),
        paste0("fast_math=", isTRUE(fast_math)),
        "compiled_subset_wrapper_abi=v2",
        paste0("build_flags=", paste(build_flags, collapse = " ")),
        paste0("transpile_version=", .mojor_state$transpile_version),
        paste(mojo_version, collapse = "\n"),
        sep = "\n"
    )
    key_file <- tempfile(pattern = "mojor_key_")
    writeLines(key_input, key_file)
    key <- as.character(tools::md5sum(key_file))
    unlink(key_file)
    trans$kernel_hash <- key

 # Make kernel name unique to avoid symbol collisions
    unique_name <- paste0(name, "_", substr(key, 1, 8))

 # Update Mojo code to use unique name
    mojo_code <- gsub(
        paste0("@export\\(\"", name, "\""),
        paste0("@export(\"", unique_name, "\""),
        trans$mojo, fixed = FALSE
    )
    mojo_code <- gsub(
        paste0("fn ", name, "\\("),
        paste0("fn ", unique_name, "("),
        mojo_code, fixed = FALSE
    )

    dir <- if (cache)
        file.path(cache_dir, key) else file.path(build_dir, paste0(unique_name, "_", as.integer(Sys.time())))
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)

# Write Mojo file
    mojo_file <- file.path(dir, paste0(unique_name, ".mojo"))
    writeLines(mojo_code, mojo_file)

    if (isTRUE(rng_needed)) {
        .mojor_copy_helper_mojo_to_dir("abi_types.mojo", dir)
        .mojor_copy_helper_mojo_to_dir("rng_helpers.mojo", dir)
        .mojor_copy_helper_mojo_to_dir("ziggurat_constants.mojo", dir)
    }

 # expression kernels may depend on set/match helpers.
    if (isTRUE(trans$set_match_needed)) {
        .mojor_copy_helper_mojo_to_dir("abi_types.mojo", dir)
        .mojor_copy_helper_mojo_to_dir("set_match_helpers.mojo", dir)
    }

 # expression kernels may depend on quantile helpers.
    if (isTRUE(trans$quantile_needed)) {
        .mojor_copy_helper_mojo_to_dir("abi_types.mojo", dir)
        .mojor_copy_helper_mojo_to_dir("quantile_helpers.mojo", dir)
    }

 # Build shared library
    lib_ext <- if (Sys.info()[["sysname"]] == "Darwin")
        "dylib" else "so"
    lib_path <- file.path(dir, paste0("lib", unique_name, ".", lib_ext))

    if (!file.exists(lib_path)) {
        flags <- paste(build_flags, collapse = " ")
        extra <- if (nzchar(flags))
            paste0(" ", flags) else ""
        cmd <- sprintf(
            "mojo build --emit shared-lib -o %s%s %s", shQuote(lib_path),
            extra, shQuote(mojo_file)
        )
        if (verbose)
            message("Mojo build: ", cmd)
        status <- system(cmd)
        if (status != 0)
            stop("Mojo build failed")
    }

 # Generate C wrapper for expression kernel
    wrapper_c <- file.path(dir, paste0(unique_name, "_wrapper.c"))
    user_args <- names(formals(fn))
    arg_specs <- list(...)
    kernel_args <- if (!is.null(trans$kernel_args) &&
        length(trans$kernel_args) >
            0)
        trans$kernel_args else user_args
    literal_array_args <- if (!is.null(trans$literal_array_args))
        trans$literal_array_args else list()
    literal_arg_names <- names(literal_array_args)
    if (is.null(literal_arg_names))
        literal_arg_names <- character(0)

    if (length(setdiff(literal_arg_names, kernel_args)) >
        0) {
        stop(
            "mojor_build: literal array metadata contains args not present in kernel_args"
        )
    }

    kernel_spec_for <- function(arg_name) {
        if (arg_name %in% user_args) {
            return(arg_specs[[arg_name]])
        }
        if (arg_name %in% literal_arg_names) {
            return(literal_array_args[[arg_name]]$type)
        }
        stop(
            "mojor_build: missing type metadata for kernel arg '", arg_name,
            "'"
        )
    }

    c_escape <- function(txt) {
        txt <- gsub("\\\\", "\\\\\\\\", txt)
        txt <- gsub("\"", "\\\\\"", txt)
        txt <- gsub("\n", "\\\\n", txt, fixed = TRUE)
        txt <- gsub("\r", "\\\\r", txt, fixed = TRUE)
        txt <- gsub("\t", "\\\\t", txt, fixed = TRUE)
        txt
    }

    .mojor_compile_compiled_subset_wrapper <- function(wrapper_so_path, wrapper_c_path, linked_lib_path) {
        if (file.exists(wrapper_so_path)) {
            return(invisible(NULL))
        }
        lib_dir <- dirname(linked_lib_path)
        lib_name <- sub("^lib", "", sub("\\..*$", "", basename(linked_lib_path)))
        r_bin_sh <- shQuote(.mojor_r_binary_path())
        if (Sys.info()[["sysname"]] == "Darwin") {
            compile_cmd <- sprintf(
                "%s CMD SHLIB -o %s %s -L%s -l%s -Wl,-rpath,@loader_path", r_bin_sh, shQuote(wrapper_so_path),
                shQuote(wrapper_c_path),
                shQuote(lib_dir),
                lib_name
            )
        } else {
            compile_cmd <- sprintf(
                "%s CMD SHLIB -o %s %s -L%s -l%s -Wl,-rpath,%s", r_bin_sh, shQuote(wrapper_so_path),
                shQuote(wrapper_c_path),
                shQuote(lib_dir),
                lib_name, shQuote(lib_dir)
            )
        }
        if (verbose)
            message("C build: ", compile_cmd)
        status <- system(compile_cmd)
        if (status != 0)
            stop("C wrapper compilation failed")
        invisible(NULL)
    }

    .mojor_resolve_compiled_subset_wrapper_pkg <- function(wrapper_so_path) {
        wrapper_pkg <- .mojor_find_loaded_dll_pkg(wrapper_so_path)
        if (isTRUE(load)) {
            dyn_loader <- get0(
                ".mojor_dyn_load_with_recovery",
                mode = "function",
                inherits = TRUE
            )
            if (is.function(dyn_loader)) {
                dll <- dyn_loader(
                    wrapper_so_path,
                    lib_dir = dirname(wrapper_so_path)
                )
            } else {
                lib_dir <- dirname(wrapper_so_path)
                wrapper_base <- basename(wrapper_so_path)
                lib_ext <- tools::file_ext(wrapper_base)
                kernel_name <- sub(
                    paste0("_[0-9a-f]{32}\\.", lib_ext, "$"),
                    "",
                    wrapper_base,
                    perl = TRUE
                )
                linked_lib_path <- file.path(
                    lib_dir,
                    paste0("lib", kernel_name, ".", lib_ext)
                )
                if (file.exists(linked_lib_path)) {
                    dll_paths <- vapply(
                        getLoadedDLLs(),
                        function(dll) getElement(dll, "path"),
                        character(1)
                    )
                    linked_norm <- normalizePath(
                        linked_lib_path,
                        winslash = "/",
                        mustWork = FALSE
                    )
                    is_loaded <- any(vapply(dll_paths, function(path) {
                        identical(
                            normalizePath(path, winslash = "/", mustWork = FALSE),
                            linked_norm
                        )
                    }, logical(1)))
                    if (!isTRUE(is_loaded)) {
                        dyn.load(linked_lib_path, local = FALSE)
                    }
                }
                with_lib_path <- get0(
                    ".mojor_with_ld_library_path",
                    mode = "function",
                    inherits = TRUE
                )
                if (is.function(with_lib_path)) {
                    dll <- with_lib_path(lib_dir, function() dyn.load(wrapper_so_path))
                } else {
                    sysname <- Sys.info()[["sysname"]]
                    if (identical(sysname, "Linux")) {
                        old <- Sys.getenv("LD_LIBRARY_PATH", "")
                        new <- if (nzchar(old))
                            paste(lib_dir, old, sep = ":") else lib_dir
                        Sys.setenv(LD_LIBRARY_PATH = new)
                        on.exit(
                            Sys.setenv(LD_LIBRARY_PATH = old),
                            add = TRUE
                        )
                    } else if (identical(sysname, "Darwin")) {
                        old <- Sys.getenv("DYLD_LIBRARY_PATH", "")
                        new <- if (nzchar(old))
                            paste(lib_dir, old, sep = ":") else lib_dir
                        Sys.setenv(DYLD_LIBRARY_PATH = new)
                        on.exit(
                            Sys.setenv(DYLD_LIBRARY_PATH = old),
                            add = TRUE
                        )
                    }
                    dll <- dyn.load(wrapper_so_path)
                }
            }
            wrapper_pkg <- .mojor_resolve_loaded_dll_pkg(wrapper_so_path, dll)
            if (!is.null(wrapper_pkg) &&
                nzchar(wrapper_pkg)) {
                .mojor_register_loaded_wrapper(wrapper_pkg, wrapper_so_path, kind = "expression")
            }
        }
        wrapper_pkg
    }

 # Strict runtime bridge plan (no operation-specific
 # generated wrappers).
    if (!is.null(trans$tier9_runtime_plan) &&
        is.list(trans$tier9_runtime_plan)) {
        tier9_plan <- trans$tier9_runtime_plan
        if (is.null(tier9_plan$kind) ||
            !is.character(tier9_plan$kind) ||
            length(tier9_plan$kind) !=
                1) {
            stop("mojor_build: runtime plan metadata requires a valid 'kind'")
        }
        if (!(tier9_plan$kind %in% c("regex_native", "expand_grid_native"))) {
            stop(
                "mojor_build: unsupported runtime plan kind: ", tier9_plan$kind
            )
        }

        .mojor_name_call_args <- function(args_in) {
            if (length(args_in) >
                0 && length(user_args) >
                0) {
                if (is.null(names(args_in))) {
                  names(args_in) <- user_args[seq_along(args_in)]
                } else {
                  blank <- names(args_in) ==
                    ""
                  names(args_in)[blank] <- user_args[seq_along(args_in)][blank]
                }
            }
            args_in
        }

        runtime_func <- function(...) {
            args_in <- .mojor_name_call_args(list(...))
            .mojor_tier9_runtime_execute_plan(
                plan = tier9_plan, args_in = args_in, arg_specs = arg_specs,
                context = "mojor_build"
            )
        }

        return(
            list(
                lib = NA_character_, mojo = trans$mojo, name = unique_name,
                args = user_args, types = trans$types, is_expression_kernel = TRUE,
                return_type = trans$return_type, kernel_hash = key, tier9_runtime_plan = tier9_plan,
                func = runtime_func
            )
        )
    }

 # strict native nchar/nzchar/substr/paste wrappers.
    if (isTRUE(trans$string_builtin)) {
        c_lines <- c(
            "#include <R.h>", "#include <Rinternals.h>", "#include <R_ext/Rdynload.h>",
            "#include <string.h>", ""
        )
        if (length(user_args) >
            0) {
            c_lines <- c(
                c_lines, "", paste0(
                  "SEXP ", unique_name, "_call(SEXP ", paste(user_args, collapse = ", SEXP "),
                  ") {"
              )
            )
        } else {
            c_lines <- c(c_lines, "", paste0("SEXP ", unique_name, "_call(void) {"))
        }

        for (a in user_args) {
            spec <- arg_specs[[a]]
            if (is.null(spec)) {
                stop(
                  "mojor_build: missing type metadata for string wrapper arg '",
                  a, "'"
              )
            }
            if (identical(spec, "chr[]")) {
                c_lines <- c(
                  c_lines, sprintf(
                    "  if (TYPEOF(%s) != STRSXP) error(\"%s: expected character vector\");",
                    a, a
                ),
                  sprintf("  int %s_len = LENGTH(%s);", a, a),
                  sprintf("  (void)%s_len;", a)
              )
            } else if (identical(spec, "chr")) {
                c_lines <- c(
                  c_lines, sprintf(
                    "  if (TYPEOF(%s) != STRSXP || LENGTH(%s) != 1) error(\"%s: expected character scalar\");",
                    a, a, a
                ),
                  sprintf("  int %s_len = 1;", a),
                  sprintf("  (void)%s_len;", a)
              )
            } else if (identical(spec, "i32")) {
                c_lines <- c(
                  c_lines, sprintf(
                    "  if (TYPEOF(%s) != INTSXP || LENGTH(%s) != 1) error(\"%s: expected integer scalar\");",
                    a, a, a
                ),
                  sprintf("  int %s_val = INTEGER(%s)[0];", a, a),
                  sprintf("  (void)%s_val;", a)
              )
            } else {
                stop(
                  "mojor_build: compiled subset string wrapper supports only chr, chr[] and i32 args; got ",
                  a, ": ", spec
              )
            }
        }

        op <- trans$string_op
        if (identical(op, "nchar")) {
            x_name <- trans$string_x_name
            if (is.null(x_name) ||
                !(x_name %in% user_args)) {
                stop("mojor_build: nchar string wrapper requires x argument metadata")
            }
            x_spec <- arg_specs[[x_name]]
            if (identical(x_spec, "chr[]")) {
                c_lines <- c(
                    c_lines, sprintf(
                      "  SEXP __mojor_out = PROTECT(allocVector(INTSXP, %s_len));",
                      x_name
                  ),
                    sprintf(
                      "  for (int __mojor_i = 0; __mojor_i < %s_len; __mojor_i++) {",
                      x_name
                  ),
                    sprintf("    SEXP __mojor_ch = STRING_ELT(%s, __mojor_i);", x_name),
                    "    INTEGER(__mojor_out)[__mojor_i] = R_nchar(__mojor_ch, Chars, FALSE, TRUE, \"nchar\");",
                    "  }", "  UNPROTECT(1);", "  return __mojor_out;", "}"
                )
            } else if (identical(x_spec, "chr")) {
                c_lines <- c(
                    c_lines,
                    sprintf("  SEXP __mojor_ch = STRING_ELT(%s, 0);", x_name),
                    "  SEXP __mojor_out = PROTECT(Rf_ScalarInteger(R_nchar(__mojor_ch, Chars, FALSE, TRUE, \"nchar\")));",
                    "  UNPROTECT(1);",
                    "  return __mojor_out;",
                    "}"
                )
            } else {
                stop("mojor_build: nchar string wrapper requires x to be chr[] or chr")
            }
        } else if (identical(op, "nzchar")) {
            x_name <- trans$string_x_name
            if (is.null(x_name) ||
                !(x_name %in% user_args)) {
                stop("mojor_build: nzchar string wrapper requires x argument metadata")
            }
            x_spec <- arg_specs[[x_name]]
            if (identical(x_spec, "chr[]")) {
                c_lines <- c(
                    c_lines, sprintf(
                      "  SEXP __mojor_out = PROTECT(allocVector(LGLSXP, %s_len));",
                      x_name
                  ),
                    sprintf(
                      "  for (int __mojor_i = 0; __mojor_i < %s_len; __mojor_i++) {",
                      x_name
                  ),
                    sprintf("    SEXP __mojor_ch = STRING_ELT(%s, __mojor_i);", x_name),
                    "    LOGICAL(__mojor_out)[__mojor_i] = (R_nchar(__mojor_ch, Chars, FALSE, FALSE, \"nzchar\") > 0);",
                    "  }", "  UNPROTECT(1);", "  return __mojor_out;", "}"
                )
            } else if (identical(x_spec, "chr")) {
                c_lines <- c(
                    c_lines,
                    sprintf("  SEXP __mojor_ch = STRING_ELT(%s, 0);", x_name),
                    "  SEXP __mojor_out = PROTECT(Rf_ScalarLogical((R_nchar(__mojor_ch, Chars, FALSE, FALSE, \"nzchar\") > 0) ? 1 : 0));",
                    "  UNPROTECT(1);",
                    "  return __mojor_out;",
                    "}"
                )
            } else {
                stop("mojor_build: nzchar string wrapper requires x to be chr[] or chr")
            }
        } else if (identical(op, "substr")) {
            x_name <- trans$string_x_name
            if (is.null(x_name) ||
                !(x_name %in% user_args)) {
                stop("mojor_build: substr string wrapper requires x argument metadata")
            }
            start_info <- trans$string_start
            stop_info <- trans$string_stop
            if (is.null(start_info) ||
                is.null(start_info$kind) ||
                is.null(stop_info) ||
                is.null(stop_info$kind)) {
                stop("mojor_build: substr string wrapper requires start/stop metadata")
            }

            start_expr <- NULL
            stop_expr <- NULL

            if (identical(start_info$kind, "arg")) {
                if (is.null(start_info$name) ||
                  !(start_info$name %in% user_args)) {
                  stop("mojor_build: substr start arg metadata is invalid")
                }
                start_expr <- paste0(start_info$name, "_val")
            } else if (identical(start_info$kind, "const")) {
                if (is.null(start_info$value) ||
                  is.na(start_info$value)) {
                  stop("mojor_build: substr start constant metadata is invalid")
                }
                start_expr <- as.character(as.integer(start_info$value))
            } else {
                stop(
                  "mojor_build: unsupported substr start metadata kind: ",
                  start_info$kind
              )
            }

            if (identical(stop_info$kind, "arg")) {
                if (is.null(stop_info$name) ||
                  !(stop_info$name %in% user_args)) {
                  stop("mojor_build: substr stop arg metadata is invalid")
                }
                stop_expr <- paste0(stop_info$name, "_val")
            } else if (identical(stop_info$kind, "const")) {
                if (is.null(stop_info$value) ||
                  is.na(stop_info$value)) {
                  stop("mojor_build: substr stop constant metadata is invalid")
                }
                stop_expr <- as.character(as.integer(stop_info$value))
            } else {
                stop(
                  "mojor_build: unsupported substr stop metadata kind: ",
                  stop_info$kind
              )
            }
            x_spec <- arg_specs[[x_name]]
            if (identical(x_spec, "chr[]")) {
                c_lines <- c(
                    c_lines, sprintf(
                      "  SEXP __mojor_out = PROTECT(allocVector(STRSXP, %s_len));",
                      x_name
                  ),
                    sprintf(
                      "  for (int __mojor_i = 0; __mojor_i < %s_len; __mojor_i++) {",
                      x_name
                  ),
                    sprintf("    SEXP __mojor_ch = STRING_ELT(%s, __mojor_i);", x_name),
                    "    if (__mojor_ch == NA_STRING) {", "      SET_STRING_ELT(__mojor_out, __mojor_i, NA_STRING);",
                    "      continue;", "    }", sprintf("    int __mojor_start = %s;", start_expr),
                    sprintf("    int __mojor_stop = %s;", stop_expr),
                    "    int __mojor_len = R_nchar(__mojor_ch, Chars, FALSE, TRUE, \"substr\");",
                    "    if (__mojor_start < 1) __mojor_start = 1;", "    if (__mojor_stop < __mojor_start || __mojor_start > __mojor_len || __mojor_len <= 0) {",
                    "      SET_STRING_ELT(__mojor_out, __mojor_i, Rf_mkCharLen(\"\", 0));",
                    "      continue;", "    }", "    if (__mojor_stop > __mojor_len) __mojor_stop = __mojor_len;",
                    "    int __mojor_from = __mojor_start - 1;", "    int __mojor_n = __mojor_stop - __mojor_from;",
                    "    const char* __mojor_raw = CHAR(__mojor_ch);", "    SET_STRING_ELT(__mojor_out, __mojor_i, Rf_mkCharLen(__mojor_raw + __mojor_from, __mojor_n));",
                    "  }", "  UNPROTECT(1);", "  return __mojor_out;", "}"
                )
            } else if (identical(x_spec, "chr")) {
                c_lines <- c(
                    c_lines,
                    sprintf("  SEXP __mojor_ch = STRING_ELT(%s, 0);", x_name),
                    "  if (__mojor_ch == NA_STRING) return Rf_ScalarString(NA_STRING);",
                    sprintf("  int __mojor_start = %s;", start_expr),
                    sprintf("  int __mojor_stop = %s;", stop_expr),
                    "  int __mojor_len = R_nchar(__mojor_ch, Chars, FALSE, TRUE, \"substr\");",
                    "  if (__mojor_start < 1) __mojor_start = 1;",
                    "  if (__mojor_stop < __mojor_start || __mojor_start > __mojor_len || __mojor_len <= 0) return Rf_ScalarString(Rf_mkCharLen(\"\", 0));",
                    "  if (__mojor_stop > __mojor_len) __mojor_stop = __mojor_len;",
                    "  int __mojor_from = __mojor_start - 1;",
                    "  int __mojor_n = __mojor_stop - __mojor_from;",
                    "  const char* __mojor_raw = CHAR(__mojor_ch);",
                    "  return Rf_ScalarString(Rf_mkCharLen(__mojor_raw + __mojor_from, __mojor_n));",
                    "}"
                )
            } else {
                stop("mojor_build: substr string wrapper requires x to be chr[] or chr")
            }
        } else if (op %in% c("paste", "paste0")) {
            x_name <- trans$string_x_name
            y_name <- trans$string_y_name
            if (is.null(x_name) ||
                is.null(y_name) ||
                !(x_name %in% user_args) || !(y_name %in% user_args)) {
                stop(
                  "mojor_build: ", op, " string wrapper requires x/y argument metadata"
              )
            }
            sep_value <- if (identical(op, "paste"))
                trans$string_sep else ""
            collapse_value <- if (!is.null(trans$string_collapse))
                trans$string_collapse else NULL
            if (!(is.character(sep_value) &&
                length(sep_value) ==
                  1 && !is.na(sep_value))) {
                stop(
                  "mojor_build: ", op, " string wrapper requires scalar character sep metadata"
              )
            }
            if (!(is.null(collapse_value) ||
                (is.character(collapse_value) &&
                  length(collapse_value) ==
                    1 && !is.na(collapse_value)))) {
                stop(
                  "mojor_build: ", op, " string wrapper requires scalar character collapse metadata or NULL"
              )
            }
            sep_escaped <- c_escape(as.character(sep_value[[1]]))
            if (is.null(collapse_value)) {
                c_lines <- c(
                  c_lines, sprintf("  const char* __mojor_sep = \"%s\";", sep_escaped),
                  "  int __mojor_sep_len = (int)strlen(__mojor_sep);",
                  sprintf(
                    "  int __mojor_out_len = (%s_len == 0 || %s_len == 0) ? 0 : ((%s_len > %s_len) ? %s_len : %s_len);",
                    x_name, y_name, x_name, y_name, x_name, y_name
                ),
                  "  SEXP __mojor_out = PROTECT(allocVector(STRSXP, __mojor_out_len));",
                  "  for (int __mojor_i = 0; __mojor_i < __mojor_out_len; __mojor_i++) {",
                  sprintf(
                    "    SEXP __mojor_x = STRING_ELT(%s, __mojor_i %% %s_len);",
                    x_name, x_name
                ),
                  sprintf(
                    "    SEXP __mojor_y = STRING_ELT(%s, __mojor_i %% %s_len);",
                    y_name, y_name
                ),
                  "    const char* __mojor_xs = (__mojor_x == NA_STRING) ? \"NA\" : CHAR(__mojor_x);",
                  "    const char* __mojor_ys = (__mojor_y == NA_STRING) ? \"NA\" : CHAR(__mojor_y);",
                  "    int __mojor_xn = (__mojor_x == NA_STRING) ? 2 : (int)strlen(__mojor_xs);",
                  "    int __mojor_yn = (__mojor_y == NA_STRING) ? 2 : (int)strlen(__mojor_ys);",
                  "    int __mojor_total = __mojor_xn + __mojor_sep_len + __mojor_yn;",
                  "    char* __mojor_buf = (char*)R_alloc((size_t)__mojor_total + 1u, sizeof(char));",
                  "    memcpy(__mojor_buf, __mojor_xs, (size_t)__mojor_xn);",
                  "    if (__mojor_sep_len > 0) memcpy(__mojor_buf + __mojor_xn, __mojor_sep, (size_t)__mojor_sep_len);",
                  "    memcpy(__mojor_buf + __mojor_xn + __mojor_sep_len, __mojor_ys, (size_t)__mojor_yn);",
                  "    __mojor_buf[__mojor_total] = '\\0';", "    SET_STRING_ELT(__mojor_out, __mojor_i, Rf_mkCharLen(__mojor_buf, __mojor_total));",
                  "  }", "  UNPROTECT(1);", "  return __mojor_out;", "}"
              )
            } else {
                collapse_escaped <- c_escape(as.character(collapse_value[[1]]))
                c_lines <- c(
                  c_lines, sprintf("  const char* __mojor_sep = \"%s\";", sep_escaped),
                  "  int __mojor_sep_len = (int)strlen(__mojor_sep);",
                  sprintf("  const char* __mojor_collapse = \"%s\";", collapse_escaped),
                  "  int __mojor_collapse_len = (int)strlen(__mojor_collapse);",
                  sprintf(
                    "  int __mojor_out_len = (%s_len == 0 || %s_len == 0) ? 0 : ((%s_len > %s_len) ? %s_len : %s_len);",
                    x_name, y_name, x_name, y_name, x_name, y_name
                ),
                  "  int __mojor_total = (__mojor_out_len <= 0) ? 0 : ((__mojor_out_len - 1) * __mojor_collapse_len);",
                  "  for (int __mojor_i = 0; __mojor_i < __mojor_out_len; __mojor_i++) {",
                  sprintf(
                    "    SEXP __mojor_x = STRING_ELT(%s, __mojor_i %% %s_len);",
                    x_name, x_name
                ),
                  sprintf(
                    "    SEXP __mojor_y = STRING_ELT(%s, __mojor_i %% %s_len);",
                    y_name, y_name
                ),
                  "    int __mojor_xn = (__mojor_x == NA_STRING) ? 2 : (int)strlen(CHAR(__mojor_x));",
                  "    int __mojor_yn = (__mojor_y == NA_STRING) ? 2 : (int)strlen(CHAR(__mojor_y));",
                  "    __mojor_total += __mojor_xn + __mojor_sep_len + __mojor_yn;",
                  "  }", "  char* __mojor_buf = (char*)R_alloc((size_t)__mojor_total + 1u, sizeof(char));",
                  "  int __mojor_off = 0;", "  for (int __mojor_i = 0; __mojor_i < __mojor_out_len; __mojor_i++) {",
                  sprintf(
                    "    SEXP __mojor_x = STRING_ELT(%s, __mojor_i %% %s_len);",
                    x_name, x_name
                ),
                  sprintf(
                    "    SEXP __mojor_y = STRING_ELT(%s, __mojor_i %% %s_len);",
                    y_name, y_name
                ),
                  "    const char* __mojor_xs = (__mojor_x == NA_STRING) ? \"NA\" : CHAR(__mojor_x);",
                  "    const char* __mojor_ys = (__mojor_y == NA_STRING) ? \"NA\" : CHAR(__mojor_y);",
                  "    int __mojor_xn = (__mojor_x == NA_STRING) ? 2 : (int)strlen(__mojor_xs);",
                  "    int __mojor_yn = (__mojor_y == NA_STRING) ? 2 : (int)strlen(__mojor_ys);",
                  "    memcpy(__mojor_buf + __mojor_off, __mojor_xs, (size_t)__mojor_xn);",
                  "    __mojor_off += __mojor_xn;", "    if (__mojor_sep_len > 0) {",
                  "      memcpy(__mojor_buf + __mojor_off, __mojor_sep, (size_t)__mojor_sep_len);",
                  "      __mojor_off += __mojor_sep_len;", "    }", "    memcpy(__mojor_buf + __mojor_off, __mojor_ys, (size_t)__mojor_yn);",
                  "    __mojor_off += __mojor_yn;", "    if (__mojor_i + 1 < __mojor_out_len && __mojor_collapse_len > 0) {",
                  "      memcpy(__mojor_buf + __mojor_off, __mojor_collapse, (size_t)__mojor_collapse_len);",
                  "      __mojor_off += __mojor_collapse_len;", "    }",
                  "  }", "  __mojor_buf[__mojor_total] = '\\0';", "  SEXP __mojor_out = PROTECT(Rf_ScalarString(Rf_mkCharLen(__mojor_buf, __mojor_total)));",
                  "  UNPROTECT(1);", "  return __mojor_out;", "}"
              )
            }
        } else {
            stop(
                "mojor_build: unsupported compiled subset string wrapper operation: ",
                op
            )
        }

        writeLines(c_lines, wrapper_c)

        wrapper_so <- file.path(dir, paste0(unique_name, "_", key, ".so"))
        .mojor_compile_compiled_subset_wrapper(wrapper_so, wrapper_c, lib_path)
        wrapper_pkg <- .mojor_resolve_compiled_subset_wrapper_pkg(wrapper_so)

        return(
            list(
                lib = wrapper_so, mojo = trans$mojo, name = unique_name,
                args = user_args, types = trans$types, is_expression_kernel = TRUE,
                return_type = trans$return_type, kernel_hash = key, wrapper_pkg = wrapper_pkg,
                func = .mojor_make_package_scoped_call(
                  symbol = paste0(unique_name, "_call"),
                  so_path = wrapper_so, pkg = wrapper_pkg, wrapper_kind = "expression",
                  missing_message = paste0(
                    "mojor_build: expression wrapper library is not loaded: ",
                    wrapper_so
                )
              )
            )
        )
    }

 # strict native dim(x) metadata wrapper.
    if (isTRUE(trans$dim_builtin)) {
        dim_mode <- .mojor_state$`%||%`(trans$dim_builtin_mode, "vector")
        if (!(dim_mode %in% c("vector", "index", "ndim"))) {
            stop("mojor_build: dim wrapper has unsupported mode: ", dim_mode)
        }
        x_name <- trans$dim_x_name
        if (is.null(x_name) || !is.character(x_name) || length(x_name) != 1 || !nzchar(x_name)) {
            stop("mojor_build: dim wrapper requires dim_x_name metadata")
        }
        if (!(x_name %in% user_args)) {
            stop("mojor_build: dim wrapper argument '", x_name, "' is not a user argument")
        }
        expected_ndim <- suppressWarnings(as.integer(trans$dim_expected_ndim))
        if (is.na(expected_ndim) || expected_ndim < 2L) {
            stop("mojor_build: dim wrapper requires dim_expected_ndim >= 2")
        }
        x_spec <- arg_specs[[x_name]]
        if (is.null(x_spec) || !.mojor_is_array(x_spec)) {
            stop("mojor_build: dim wrapper requires array-typed x argument")
        }
        x_sexp_type <- .mojor_sexp_type(x_spec)
        if (is.null(x_sexp_type) && is.character(x_spec) && length(x_spec) == 1 && !is.na(x_spec)) {
            x_base <- sub("\\[.*\\]$", "", x_spec)
            x_sexp_type <- switch(x_base,
                f64 = "REALSXP",
                f32 = "REALSXP",
                i32 = "INTSXP",
                lgl = "LGLSXP",
                bool = "LGLSXP",
                chr = "STRSXP",
                NULL
            )
        }
        if (is.null(x_sexp_type)) {
            stop("mojor_build: dim wrapper could not resolve SEXP type for x: ", x_spec)
        }
        x_type_msg <- if (identical(x_sexp_type, "INTSXP")) {
            "integer array"
        } else if (identical(x_sexp_type, "LGLSXP")) {
            "logical array"
        } else if (identical(x_sexp_type, "STRSXP")) {
            "character array"
        } else {
            "numeric array"
        }
        ndim_msg <- if (expected_ndim == 2L) "matrix (2D array)" else paste0(expected_ndim, "D array")
        dim_idx_supported_spec <- function(spec) {
            identical(spec, "i32") || spec %in% c("f64", "f32", "lgl", "bool")
        }
        dim_idx_extract_lines <- function(arg_name, arg_spec, target_name) {
            if (identical(arg_spec, "i32")) {
                return(c(
                    sprintf("  if (TYPEOF(%s) != INTSXP || LENGTH(%s) != 1) error(\"%s: expected scalar i32\");", arg_name, arg_name, arg_name),
                    sprintf("  const int %s = INTEGER(%s)[0];", target_name, arg_name)
                ))
            }
            if (arg_spec %in% c("f64", "f32")) {
                return(c(
                    sprintf("  if (TYPEOF(%s) != REALSXP || LENGTH(%s) != 1) error(\"%s: expected scalar numeric\");", arg_name, arg_name, arg_name),
                    sprintf("  if (ISNA(REAL(%s)[0]) || ISNAN(REAL(%s)[0])) error(\"%s: expected finite numeric scalar\");", arg_name, arg_name, arg_name),
                    sprintf("  const int %s = (int) REAL(%s)[0];", target_name, arg_name)
                ))
            }
            if (arg_spec %in% c("lgl", "bool")) {
                return(c(
                    sprintf("  if (TYPEOF(%s) != LGLSXP || LENGTH(%s) != 1) error(\"%s: expected scalar logical\");", arg_name, arg_name, arg_name),
                    sprintf("  if (LOGICAL(%s)[0] == NA_LOGICAL) error(\"%s: expected non-NA logical scalar\");", arg_name, arg_name),
                    sprintf("  const int %s = (LOGICAL(%s)[0] != 0);", target_name, arg_name)
                ))
            }
            stop("mojor_build: dim index argument '", arg_name, "' must be scalar i32/f64/f32/lgl/bool")
        }

        c_lines <- c(
            "#include <R.h>", "#include <Rinternals.h>", "#include <R_ext/Rdynload.h>",
            "#include <string.h>", ""
        )
        if (length(user_args) > 0) {
            c_lines <- c(
                c_lines, "", paste0(
                    "SEXP ", unique_name, "_call(SEXP ", paste(user_args, collapse = ", SEXP "),
                    ") {"
                )
            )
        } else {
            c_lines <- c(c_lines, "", paste0("SEXP ", unique_name, "_call(void) {"))
        }

        c_lines <- c(
            c_lines,
            sprintf(
                "  if (TYPEOF(%s) != %s) error(\"%s: expected %s\");",
                x_name, x_sexp_type, x_name, x_type_msg
            ),
            sprintf("  SEXP %s_dim = Rf_getAttrib(%s, R_DimSymbol);", x_name, x_name),
            sprintf(
                "  if (%s_dim == R_NilValue || TYPEOF(%s_dim) != INTSXP || LENGTH(%s_dim) != %d) error(\"%s: expected %s\");",
                x_name, x_name, x_name, expected_ndim, x_name, ndim_msg
            )
        )
        if (identical(dim_mode, "vector")) {
            c_lines <- c(
                c_lines,
                sprintf("  SEXP out = PROTECT(Rf_allocVector(INTSXP, %d));", expected_ndim),
                sprintf("  memcpy(INTEGER(out), INTEGER(%s_dim), (size_t)%d * sizeof(int));", x_name, expected_ndim),
                "  UNPROTECT(1);",
                "  return out;"
            )
        } else if (identical(dim_mode, "index")) {
            idx_kind <- .mojor_state$`%||%`(trans$dim_index_kind, NULL)
            if (is.null(idx_kind) || !(idx_kind %in% c("const", "arg", "expr"))) {
                stop("mojor_build: dim index wrapper requires dim_index_kind metadata")
            }
            if (identical(idx_kind, "const")) {
                idx_value <- suppressWarnings(as.integer(trans$dim_index_value))
                if (is.na(idx_value) || idx_value < 1L || idx_value > expected_ndim) {
                    stop(
                        "mojor_build: dim index constant must be in [1, ",
                        expected_ndim, "]"
                    )
                }
                c_lines <- c(
                    c_lines,
                    sprintf("  const int __mojor_dim_idx = %d;", idx_value)
                )
            } else if (identical(idx_kind, "arg")) {
                idx_name <- .mojor_state$`%||%`(trans$dim_index_name, NULL)
                if (is.null(idx_name) || !is.character(idx_name) || length(idx_name) != 1 || !nzchar(idx_name)) {
                    stop("mojor_build: dim index arg wrapper requires dim_index_name metadata")
                }
                if (!(idx_name %in% user_args)) {
                    stop("mojor_build: dim index argument '", idx_name, "' is not a user argument")
                }
                idx_spec <- arg_specs[[idx_name]]
                if (!dim_idx_supported_spec(idx_spec)) {
                    stop("mojor_build: dim index argument '", idx_name, "' must be scalar i32/f64/f32/lgl/bool")
                }
                c_lines <- c(
                    c_lines,
                    dim_idx_extract_lines(idx_name, idx_spec, "__mojor_dim_idx")
                )
            } else if (identical(idx_kind, "expr")) {
                idx_c_expr <- .mojor_state$`%||%`(trans$dim_index_c_expr, NULL)
                if (is.null(idx_c_expr) || !is.character(idx_c_expr) || length(idx_c_expr) != 1 || !nzchar(idx_c_expr)) {
                    stop("mojor_build: dim index expression wrapper requires dim_index_c_expr metadata")
                }
                idx_arg_names <- .mojor_state$`%||%`(trans$dim_index_arg_names, character(0))
                if (!is.character(idx_arg_names)) {
                    stop("mojor_build: dim index expression wrapper requires character dim_index_arg_names metadata")
                }
                idx_arg_names <- unique(idx_arg_names)
                for (idx_name in idx_arg_names) {
                    if (!(idx_name %in% user_args)) {
                        stop("mojor_build: dim index expression argument '", idx_name, "' is not a user argument")
                    }
                    idx_spec <- arg_specs[[idx_name]]
                    if (!dim_idx_supported_spec(idx_spec)) {
                        stop("mojor_build: dim index expression argument '", idx_name, "' must be scalar i32/f64/f32/lgl/bool")
                    }
                    c_lines <- c(
                        c_lines,
                        dim_idx_extract_lines(
                            idx_name,
                            idx_spec,
                            paste0("__mojor_dim_idx_", idx_name)
                        )
                    )
                }
                c_lines <- c(
                    c_lines,
                    sprintf("  const int __mojor_dim_idx = (int)(%s);", idx_c_expr)
                )
            } else {
                stop("mojor_build: unsupported dim index kind: ", idx_kind)
            }
            c_lines <- c(
                c_lines,
                sprintf("  if (__mojor_dim_idx < 1 || __mojor_dim_idx > %d) return Rf_ScalarInteger(0);", expected_ndim),
                sprintf("  return Rf_ScalarInteger(INTEGER(%s_dim)[__mojor_dim_idx - 1]);", x_name)
            )
        } else {
            c_lines <- c(
                c_lines,
                sprintf("  return Rf_ScalarInteger(%d);", expected_ndim)
            )
        }
        c_lines <- c(c_lines, "}")

        writeLines(c_lines, wrapper_c)

        wrapper_so <- file.path(dir, paste0(unique_name, "_", key, ".so"))
        .mojor_compile_compiled_subset_wrapper(wrapper_so, wrapper_c, lib_path)
        wrapper_pkg <- .mojor_resolve_compiled_subset_wrapper_pkg(wrapper_so)

        return(
            list(
                lib = wrapper_so, mojo = trans$mojo, name = unique_name,
                args = user_args, types = trans$types, is_expression_kernel = TRUE,
                return_type = trans$return_type, kernel_hash = key, wrapper_pkg = wrapper_pkg,
                func = .mojor_make_package_scoped_call(
                    symbol = paste0(unique_name, "_call"),
                    so_path = wrapper_so, pkg = wrapper_pkg, wrapper_kind = "expression",
                    missing_message = paste0(
                        "mojor_build: expression wrapper library is not loaded: ",
                        wrapper_so
                    )
                )
            )
        )
    }

 # Detect output mode
    is_matrix_output <- isTRUE(trans$is_matrix_output)
    is_vector_output <- isTRUE(trans$is_vector_output)
    matrix_out_spec <- if (is_matrix_output && !is.null(trans$out_type) &&
        nzchar(trans$out_type))
        trans$out_type else "f64[,]"
    if (is_matrix_output && !.mojor_is_matrix(matrix_out_spec)) {
        stop(
            "mojor_build: expression matrix output requires matrix out_type; got: ",
            matrix_out_spec
        )
    }
    matrix_out_sexp <- if (is_matrix_output)
        .mojor_sexp_type(matrix_out_spec) else NULL
    matrix_out_ptr_c_type <- if (is_matrix_output)
        .mojor_c_type(matrix_out_spec) else NULL
    if (is_matrix_output && (is.null(matrix_out_sexp) ||
        is.null(matrix_out_ptr_c_type))) {
        stop(
            "mojor_build: unsupported expression matrix output type: ",
            matrix_out_spec
        )
    }
    if (isTRUE(rng_needed) &&
        !isTRUE(is_vector_output)) {
        stop(
            "mojor_build: expression-kernel RNG support currently requires vector output"
        )
    }

 # Generate extern declaration
    c_lines <- c(
        "#include <R.h>", "#include <Rinternals.h>", "#include <R_ext/Rdynload.h>",
        "#include <stdint.h>", "#include <string.h>", ""
    )

    if (is_matrix_output) {
 # Matrix output: void return with output parameters
        extern_params <- unlist(
            lapply(
                kernel_args, function(a) {
                  spec <- kernel_spec_for(a)
                  if (.mojor_is_matrix(spec)) {
                    c(
                      paste0(
                        .mojor_c_type(spec),
                        ""
                    ),
                      "int", "int*"
                  )  # ptr, len, dim
                  } else if (.mojor_is_array(spec)) {
                    c(
                      paste0(
                        .mojor_c_type(spec),
                        ""
                    ),
                      "int"
                  )  # ptr, len
                  } else {
                    .mojor_c_type(spec)
                  }
                }
            )
        )
 # Add output parameters
        extern_params <- c(extern_params, matrix_out_ptr_c_type, "int", "int")

        c_lines <- c(
            c_lines, paste0(
                "extern void ", unique_name, "(", paste(extern_params, collapse = ", "),
                ");"
            )
        )
    } else if (is_vector_output) {
 # Vector output: Int32 return value carries produced length
 # for dynamic outputs.
        extern_params <- unlist(
            lapply(
                kernel_args, function(a) {
                  spec <- kernel_spec_for(a)
                  if (.mojor_is_array(spec)) {
                    c(
                      paste0(
                        .mojor_c_type(spec),
                        ""
                    ),
                      "int"
                  )  # ptr, len
                  } else {
                    .mojor_c_type(spec)
                  }
                }
            )
        )

        out_ptr_c_type <- if (identical(trans$out_type, "f64[]")) {
            "double*"
        } else if (identical(trans$out_type, "i32[]") ||
            identical(trans$out_type, "lgl[]")) {
            "int*"
        } else {
            stop(
                "mojor_build: expression vector output currently supports f64[], i32[], lgl[]; got: ",
                trans$out_type
            )
        }
        if (isTRUE(rng_needed)) {
            extern_params <- c(extern_params, "uint64_t*")
        }
        extern_params <- c(extern_params, out_ptr_c_type)

        c_lines <- c(
            c_lines, paste0(
                "extern int ", unique_name, "(", paste(extern_params, collapse = ", "),
                ");"
            )
        )
    } else {
 # Scalar output: return value
        c_return_type <- if (trans$return_type == "Float64")
            "double" else if (trans$return_type == "Int32")
            "int" else "double"
        extern_params <- unlist(
            lapply(
                kernel_args, function(a) {
                  spec <- kernel_spec_for(a)
                  if (.mojor_is_array(spec)) {
                    c(
                      paste0(
                        .mojor_c_type(spec),
                        ""
                    ),
                      "int"
                  )  # ptr, len
                  } else {
                    .mojor_c_type(spec)
                  }
                }
            )
        )

        c_lines <- c(
            c_lines, paste0(
                "extern ", c_return_type, " ", unique_name, "(", paste(extern_params, collapse = ", "),
                ");"
            )
        )
    }

    if (isTRUE(rng_needed)) {
        c_lines <- c(
            c_lines, "", "static uint64_t __mojor_rng_state[4] = {0, 0, 0, 0};",
            "static int __mojor_rng_seeded = 0;", "static uint64_t __mojor_splitmix64(uint64_t* x) {",
            "  uint64_t z = (*x += 0x9e3779b97f4a7c15ULL);", "  z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9ULL;",
            "  z = (z ^ (z >> 27)) * 0x94d049bb133111ebULL;", "  return z ^ (z >> 31);",
            "}", "static void __mojor_rng_seed_state(uint64_t seed) {",
            "  uint64_t x = seed;", "  __mojor_rng_state[0] = __mojor_splitmix64(&x);",
            "  __mojor_rng_state[1] = __mojor_splitmix64(&x);", "  __mojor_rng_state[2] = __mojor_splitmix64(&x);",
            "  __mojor_rng_state[3] = __mojor_splitmix64(&x);", "  __mojor_rng_seeded = 1;",
            "}", "static void __mojor_rng_ensure_seeded(void) {", "  if (!__mojor_rng_seeded) {",
            "    __mojor_rng_seed_state(0x106689d45497fdb5ULL);", "  }",
            "}"
        )
    }

 # Generate R wrapper function
    c_lines <- c(
        c_lines, "", paste0(
            "SEXP ", unique_name, "_call(SEXP ", paste(user_args, collapse = ", SEXP "),
            ") {"
        )
    )

 # Argument unpacking
    for (a in user_args) {
        spec <- arg_specs[[a]]
        if (.mojor_is_array(spec)) {
            sexp_type <- .mojor_sexp_type(spec)
            type_msg <- if (sexp_type == "INTSXP")
                "integer vector" else if (sexp_type == "LGLSXP")
                "logical vector" else "numeric vector"
            c_lines <- c(
                c_lines, sprintf(
                  "  if (TYPEOF(%s) != %s) error(\"%s: expected %s\");",
                  a, sexp_type, a, type_msg
              )
            )

            if (identical(spec, "f64[]")) {
                c_lines <- c(c_lines, sprintf("  double* %s_ptr = REAL(%s);", a, a))
            } else if (identical(spec, "f32[]")) {
                c_lines <- c(c_lines, sprintf("  float* %s_ptr = (float*) REAL(%s);", a, a))
            } else if (.mojor_is_matrix(spec)) {
 # Matrix type
                if (identical(spec, "f64[,]")) {
                  c_lines <- c(c_lines, sprintf("  double* %s_ptr = REAL(%s);", a, a))
                } else if (identical(spec, "f32[,]")) {
                  c_lines <- c(c_lines, sprintf("  float* %s_ptr = (float*) REAL(%s);", a, a))
                } else if (identical(spec, "i32[,]")) {
                  c_lines <- c(c_lines, sprintf("  int* %s_ptr = INTEGER(%s);", a, a))
                } else {
                  c_lines <- c(c_lines, sprintf("  int* %s_ptr = LOGICAL(%s);", a, a))
                }
            } else if (sexp_type == "LGLSXP") {
                c_lines <- c(c_lines, sprintf("  int* %s_ptr = LOGICAL(%s);", a, a))
            } else {
                c_lines <- c(c_lines, sprintf("  int* %s_ptr = INTEGER(%s);", a, a))
            }
            c_lines <- c(
                c_lines, sprintf("  int %s_len = LENGTH(%s);", a, a),
                sprintf("  (void)%s_ptr;", a),
                sprintf("  (void)%s_len;", a)
            )

 # Add dimension handling for matrices
            if (.mojor_is_matrix(spec)) {
                c_lines <- c(
                  c_lines, sprintf(
                    "  SEXP %s_dim_sexp = Rf_getAttrib(%s, R_DimSymbol);",
                    a, a
                ),
                  sprintf(
                    "  if (TYPEOF(%s_dim_sexp) != INTSXP || LENGTH(%s_dim_sexp) != 2) error(\"%s: expected matrix (2D array)\");",
                    a, a, a
                ),
                  sprintf("  int* %s_dim = INTEGER(%s_dim_sexp);", a, a),
                  sprintf("  (void)%s_dim;", a)
              )
            }
        } else {
            sexp_type <- .mojor_sexp_type(spec)
            type_msg <- if (sexp_type == "INTSXP")
                "integer scalar" else if (sexp_type == "LGLSXP")
                "logical scalar" else "numeric scalar"
            c_lines <- c(
                c_lines, sprintf(
                  "  if (TYPEOF(%s) != %s || LENGTH(%s) != 1) error(\"%s: expected %s\");",
                  a, sexp_type, a, a, type_msg
              )
            )

            if (identical(spec, "f64")) {
                c_lines <- c(c_lines, sprintf("  double %s_val = REAL(%s)[0];", a, a))
            } else if (sexp_type == "LGLSXP") {
                c_lines <- c(c_lines, sprintf("  int %s_val = LOGICAL(%s)[0];", a, a))
            } else {
                c_lines <- c(c_lines, sprintf("  int %s_val = INTEGER(%s)[0];", a, a))
            }
            c_lines <- c(c_lines, sprintf("  (void)%s_val;", a))
        }
    }

    if (length(literal_arg_names) >
        0) {
        for (a in literal_arg_names) {
            spec <- literal_array_args[[a]]$type
            values <- as.numeric(literal_array_args[[a]]$values)
            if (!identical(spec, "f64[]")) {
                stop(
                  "mojor_build: literal array arg '", a, "' currently supports only f64[]"
              )
            }
            if (length(values) ==
                0) {
                stop("mojor_build: literal array arg '", a, "' must be non-empty")
            }
            if (any(!is.finite(values))) {
                stop(
                  "mojor_build: literal array arg '", a, "' must contain finite numeric values"
              )
            }
            values_str <- paste(
                formatC(values, digits = 17, format = "g"),
                collapse = ", "
            )
            c_lines <- c(
                c_lines, sprintf(
                  "  double %s_data[%d] = { %s };", a, length(values),
                  values_str
              ),
                sprintf("  double* %s_ptr = %s_data;", a, a),
                sprintf("  int %s_len = %d;", a, length(values)),
                sprintf("  (void)%s_ptr;", a),
                sprintf("  (void)%s_len;", a)
            )
        }
    }

 # Handle matrix output vs vector output vs scalar output
    if (is_matrix_output) {
 # Matrix output: compute dimensions and allocate output
 # matrix
        operation <- trans$operation

 # Compute output dimensions based on operation
        if (operation == "matmul") {
 # A[m<U+00D7>n] %*% B[n<U+00D7>p] -> C[m<U+00D7>p]
            lhs <- user_args[1]
            rhs <- user_args[2]
            c_lines <- c(
                c_lines, sprintf("  int out_nrow = %s_dim[0];  /* m */", lhs),
                sprintf("  int out_ncol = %s_dim[1];  /* p */", rhs)
            )
        } else if (operation == "crossprod") {
 # t(A) %*% B -> C[n<U+00D7>p] or t(A) %*% A ->
 # C[n<U+00D7>n]
            lhs <- user_args[1]
            if (length(user_args) ==
                2) {
                rhs <- user_args[2]
                c_lines <- c(
                  c_lines, sprintf("  int out_nrow = %s_dim[1];  /* n */", lhs),
                  sprintf("  int out_ncol = %s_dim[1];  /* p */", rhs)
              )
            } else {
                c_lines <- c(
                  c_lines, sprintf("  int out_nrow = %s_dim[1];  /* n */", lhs),
                  sprintf("  int out_ncol = %s_dim[1];  /* n */", lhs)
              )
            }
        } else if (operation == "tcrossprod") {
 # A %*% t(B) -> C[m<U+00D7>p] or A %*% t(A) ->
 # C[m<U+00D7>m]
            lhs <- user_args[1]
            if (length(user_args) ==
                2) {
                rhs <- user_args[2]
                c_lines <- c(
                  c_lines, sprintf("  int out_nrow = %s_dim[0];  /* m */", lhs),
                  sprintf("  int out_ncol = %s_dim[0];  /* p */", rhs)
              )
            } else {
                c_lines <- c(
                  c_lines, sprintf("  int out_nrow = %s_dim[0];  /* m */", lhs),
                  sprintf("  int out_ncol = %s_dim[0];  /* m */", lhs)
              )
            }
        } else if (operation %in% c("cov", "cor")) {
 # cov/cor matrix lane:
 # - cov(X)/cor(X)     -> p x p where X is m x p
 # - cov(X, Y)/cor(X,Y)-> p x q where Y is m x q (same rows m)
            cov_args <- if (!is.null(trans$kernel_args) &&
                length(trans$kernel_args) >
                    0)
                trans$kernel_args else user_args
            x_arg <- cov_args[1]
            if (length(cov_args) >= 2) {
                y_arg <- cov_args[2]
                c_lines <- c(
                    c_lines, sprintf(
                      "  if (%s_dim[0] != %s_dim[0]) error(\"%s: matrix inputs must have same number of rows\");",
                      x_arg, y_arg, operation
                  ),
                    sprintf("  int out_nrow = %s_dim[1];  /* p */", x_arg),
                    sprintf("  int out_ncol = %s_dim[1];  /* q */", y_arg)
                )
            } else {
                c_lines <- c(
                    c_lines, sprintf("  int out_nrow = %s_dim[1];  /* p */", x_arg),
                    sprintf("  int out_ncol = %s_dim[1];  /* p */", x_arg)
                )
            }
        } else if (operation %in% c("row_matrix", "col_matrix")) {
            x_arg <- user_args[1]
            c_lines <- c(
                c_lines, sprintf("  int out_nrow = %s_dim[0];", x_arg),
                sprintf("  int out_ncol = %s_dim[1];", x_arg)
            )
        } else {
            stop(
                "mojor_build: unsupported expression matrix operation: ",
                operation
            )
        }

 # Allocate output matrix
        c_lines <- c(
            c_lines, sprintf(
                "  SEXP out = PROTECT(Rf_allocMatrix(%s, out_nrow, out_ncol));",
                matrix_out_sexp
            )
        )
        if (identical(matrix_out_sexp, "REALSXP")) {
            c_lines <- c(c_lines, "  double* out_ptr = REAL(out);")
        } else if (identical(matrix_out_sexp, "INTSXP")) {
            c_lines <- c(c_lines, "  int* out_ptr = INTEGER(out);")
        } else {
            c_lines <- c(c_lines, "  int* out_ptr = LOGICAL(out);")
        }

 # Build call arguments including matrix dimensions
        call_args <- unlist(
            lapply(
                kernel_args, function(a) {
                  spec <- kernel_spec_for(a)
                  if (.mojor_is_matrix(spec)) {
                    c(
                      paste0(a, "_ptr"),
                      paste0(a, "_len"),
                      paste0(a, "_dim")
                  )
                  } else if (.mojor_is_array(spec)) {
                    c(
                      paste0(a, "_ptr"),
                      paste0(a, "_len")
                  )
                  } else {
                    paste0(a, "_val")
                  }
                }
            )
        )

 # Add output arguments
        call_args <- c(call_args, "out_ptr", "out_nrow", "out_ncol")

 # Call Mojo kernel
        c_lines <- c(
            c_lines, paste0(
                "  ", unique_name, "(", paste(call_args, collapse = ", "),
                ");"
            ),
            "  UNPROTECT(1);", "  return out;", "}"
        )
    } else if (is_vector_output) {
        len_const <- if (!is.null(trans$vector_len_const))
            as.integer(trans$vector_len_const) else NA_integer_
        len_scale <- if (!is.null(trans$vector_len_scale))
            as.integer(trans$vector_len_scale) else 1L
        if (!is.na(len_const) &&
            len_const < 0L) {
            stop(
                "mojor_build: expression vector output requires non-negative vector_len_const"
            )
        }
        if (is.na(len_scale) || len_scale < 1L) {
            stop(
                "mojor_build: expression vector output requires positive vector_len_scale"
            )
        }
        len_name <- if (!is.null(trans$vector_len_arg) &&
            nzchar(trans$vector_len_arg)) {
            trans$vector_len_arg
        } else if (!is.null(trans$set_match_x_name) &&
            nzchar(trans$set_match_x_name)) {
            trans$set_match_x_name
        } else {
            user_args[1]
        }
        if (is.na(len_const) &&
            !(len_name %in% kernel_args)) {
            stop(
                "mojor_build: expression vector output missing valid source length argument"
            )
        }
        out_sexp <- if (identical(trans$out_type, "f64[]")) {
            "REALSXP"
        } else if (identical(trans$out_type, "i32[]")) {
            "INTSXP"
        } else if (identical(trans$out_type, "lgl[]")) {
            "LGLSXP"
        } else {
            stop(
                "mojor_build: unsupported expression vector output type: ",
                trans$out_type
            )
        }

        out_n_expr <- if (!is.na(len_const)) {
            as.character(as.integer(len_const * len_scale))
        } else {
            len_spec <- kernel_spec_for(len_name)
            source_len_expr <- if (.mojor_is_array(len_spec))
                paste0(len_name, "_len") else paste0(len_name, "_val")
            if (identical(len_scale, 1L)) {
                source_len_expr
            } else {
                paste0("(", source_len_expr, " * ", len_scale, ")")
            }
        }
        preserve_dim_source <- NULL
        if (is.na(len_const)) {
            len_spec_for_dim <- kernel_spec_for(len_name)
            if (.mojor_is_array(len_spec_for_dim)) {
                preserve_dim_source <- len_name
            }
        }
        c_lines <- c(
            c_lines, paste0("  int out_n = ", out_n_expr, ";"),
            paste0("  SEXP out = PROTECT(Rf_allocVector(", out_sexp, ", out_n));")
        )
        if (identical(trans$out_type, "f64[]")) {
            c_lines <- c(c_lines, "  double* out_ptr = REAL(out);")
        } else if (identical(trans$out_type, "i32[]")) {
            c_lines <- c(c_lines, "  int* out_ptr = INTEGER(out);")
        } else {
            c_lines <- c(c_lines, "  int* out_ptr = LOGICAL(out);")
        }

        call_args <- unlist(
            lapply(
                kernel_args, function(a) {
                  spec <- kernel_spec_for(a)
                  if (.mojor_is_array(spec)) {
                    c(
                      paste0(a, "_ptr"),
                      paste0(a, "_len")
                  )
                  } else {
                    paste0(a, "_val")
                  }
                }
            )
        )
        if (isTRUE(rng_needed)) {
            call_args <- c(call_args, "__mojor_rng_state")
            c_lines <- c(c_lines, "  __mojor_rng_ensure_seeded();")
        }
        call_args <- c(call_args, "out_ptr")
        c_lines <- c(
            c_lines, paste0(
                "  int used_n = ", unique_name, "(", paste(call_args, collapse = ", "),
                ");"
            )
        )

        if (identical(trans$set_match_op, "unique")) {
            c_lines <- c(
                c_lines, "  if (used_n < 0 || used_n > out_n) error(\"unique: invalid output length from kernel\");",
                "  if (used_n != out_n) {", paste0(
                  "    SEXP out_trim = PROTECT(Rf_allocVector(", out_sexp,
                  ", used_n));"
              )
            )
            if (identical(trans$out_type, "f64[]")) {
                c_lines <- c(
                  c_lines, "    memcpy(REAL(out_trim), REAL(out), (size_t)used_n * sizeof(double));"
              )
            } else if (identical(trans$out_type, "i32[]")) {
                c_lines <- c(
                  c_lines, "    memcpy(INTEGER(out_trim), INTEGER(out), (size_t)used_n * sizeof(int));"
              )
            } else {
                c_lines <- c(
                  c_lines, "    memcpy(LOGICAL(out_trim), LOGICAL(out), (size_t)used_n * sizeof(int));"
              )
            }
            c_lines <- c(c_lines, "    UNPROTECT(2);", "    return out_trim;", "  }")
        } else {
            c_lines <- c(
                c_lines, "  if (used_n != out_n) error(\"expression kernel returned unexpected output length\");"
            )
        }

        if (!is.null(preserve_dim_source)) {
            c_lines <- c(
                c_lines,
                sprintf("  SEXP __mojor_dim = getAttrib(%s, R_DimSymbol);", preserve_dim_source),
                "  if (__mojor_dim != R_NilValue) {",
                "    setAttrib(out, R_DimSymbol, __mojor_dim);",
                sprintf("    SEXP __mojor_dimnames = getAttrib(%s, R_DimNamesSymbol);", preserve_dim_source),
                "    if (__mojor_dimnames != R_NilValue) setAttrib(out, R_DimNamesSymbol, __mojor_dimnames);",
                "  }"
            )
        }

        c_lines <- c(c_lines, "  UNPROTECT(1);", "  return out;", "}")
    } else {
 # Scalar output (original path)
        call_args <- unlist(
            lapply(
                kernel_args, function(a) {
                  spec <- kernel_spec_for(a)
                  if (.mojor_is_array(spec)) {
                    c(
                      paste0(a, "_ptr"),
                      paste0(a, "_len")
                  )
                  } else {
                    paste0(a, "_val")
                  }
                }
            )
        )

        c_lines <- c(
            c_lines, paste0(
                "  ", c_return_type, " result = ", unique_name, "(", paste(call_args, collapse = ", "),
                ");"
            ),
            if (identical(trans$return_type, "Int32")) "  return Rf_ScalarInteger(result);" else "  return Rf_ScalarReal(result);",
            "}"
        )
    }

    writeLines(c_lines, wrapper_c)

 # Compile C wrapper using R CMD SHLIB
    wrapper_so <- file.path(dir, paste0(unique_name, "_", key, ".so"))

    .mojor_compile_compiled_subset_wrapper(wrapper_so, wrapper_c, lib_path)
    wrapper_pkg <- .mojor_resolve_compiled_subset_wrapper_pkg(wrapper_so)

 # Return build result (matching loop kernel structure)
    list(
        lib = wrapper_so, mojo = trans$mojo, name = unique_name, args = user_args,
        types = trans$types, is_expression_kernel = TRUE, return_type = trans$return_type,
        kernel_hash = key, wrapper_pkg = wrapper_pkg, func = .mojor_make_package_scoped_call(
            symbol = paste0(unique_name, "_call"),
            so_path = wrapper_so, pkg = wrapper_pkg, wrapper_kind = "expression",
            missing_message = paste0(
                "mojor_build: expression wrapper library is not loaded: ",
                wrapper_so
            )
        )
    )
}

cat("Compiled subset build function loaded.\n")
