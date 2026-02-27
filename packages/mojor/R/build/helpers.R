# Shared build helper utilities.

.mojor_apply_expression_compat_fields <- function(result, trans) {
    if (!is.list(result) || !is.list(trans)) {
        return(result)
    }
    if (!is.null(trans$mojo)) {
        result$mojo <- trans$mojo
    }
    if (isTRUE(trans$is_expression_kernel)) {
        result$is_expression_kernel <- TRUE
    }
    if (!is.null(trans$return_type)) {
        result$return_type <- trans$return_type
    }
    if (!is.null(trans$out_type)) {
        result$out_type <- trans$out_type
    }
    if (!is.null(trans$is_vector_output)) {
        result$is_vector_output <- isTRUE(trans$is_vector_output)
    }
    result
}

.mojor_needs_compiled_subset_build <- function(trans) {
    if (!is.list(trans)) {
        return(FALSE)
    }
    (isTRUE(trans$is_expression_kernel) &&
        (is.null(trans$n_source) || !is.list(trans$n_source) || is.null(trans$n_source$kind))) ||
        isTRUE(trans$string_builtin) ||
        isTRUE(trans$dim_builtin) ||
        (is.list(trans$tier9_runtime_plan) && length(trans$tier9_runtime_plan) > 0L) ||
        (!is.null(trans$kernel_args) && length(trans$kernel_args) > 0L) ||
        (!is.null(trans$literal_array_args) && length(trans$literal_array_args) > 0L)
}

# Backward-compatible wrapper retained for existing build call sites.
.mojor_use_expression_kernel_build_path <- function(trans) {
    .mojor_needs_compiled_subset_build(trans)
}

.mojor_r_binary_path <- function() {
    r_home <- Sys.getenv("R_HOME", unset = "")
    if (nzchar(r_home)) {
        r_home_bin <- file.path(r_home, "bin", "R")
        if (file.exists(r_home_bin)) {
            return(normalizePath(r_home_bin, winslash = "/", mustWork = FALSE))
        }
    }
    r_bin <- Sys.which("R")
    if (nzchar(r_bin)) {
        return(r_bin)
    }
    "R"
}

.mojor_copy_helper_mojo_to_dir <- function(helper_file, dir) {
    helper_src <- .mojor_find_upward(file.path("packages/mojor/src", helper_file))
    if (is.null(helper_src) || !file.exists(helper_src)) {
        helper_src <- .mojor_find_upward(file.path("src", helper_file))
    }
    if (is.null(helper_src) || !file.exists(helper_src)) {
        installed <- tryCatch(
            system.file("mojo_helpers", helper_file, package = "mojor"),
            error = function(e) ""
        )
        if (is.character(installed) && length(installed) == 1L && nzchar(installed)) {
            helper_src <- installed
        }
    }
    if (is.null(helper_src) || !file.exists(helper_src)) {
        return(FALSE)
    }
    file.copy(helper_src, file.path(dir, helper_file), overwrite = TRUE)
}

.mojor_copy_helper_mojo <- function(dir, helper_file) {
    ok <- .mojor_copy_helper_mojo_to_dir(helper_file, dir)
    if (!isTRUE(ok)) {
        stop("mojor_build: helper Mojo file not found: ", helper_file, call. = FALSE)
    }
    invisible(TRUE)
}

.mojor_build_expression_kernel <- function(
    trans, fn, name, build_dir, load, verbose, cache, cache_dir, fast_math,
    ...
) {
    out <- .mojor_build_compiled_subset_kernel(
        trans = trans,
        fn = fn,
        name = name,
        build_dir = build_dir,
        load = load,
        verbose = verbose,
        cache = cache,
        cache_dir = cache_dir,
        fast_math = fast_math,
        ...
    )
    if (!is.null(out)) {
        return(out)
    }
    stop(
        "mojor_build: expression kernel build path requested but transpile output is not expression-compatible",
        call. = FALSE
    )
}

.mojor_mojo_version_string <- function() {
    out <- tryCatch(
        system("mojo --version", intern = TRUE, ignore.stderr = TRUE),
        error = function(e) character(0)
    )
    if (length(out) == 0L) {
        return("unknown")
    }
    paste(out, collapse = "\n")
}

.mojor_build_hash_from_key_input <- function(key_input) {
    key_file <- tempfile(pattern = "mojor_key_")
    on.exit(unlink(key_file), add = TRUE)
    writeLines(as.character(key_input), key_file, useBytes = TRUE)
    as.character(tools::md5sum(key_file))
}

.mojor_build_shared_lib_if_missing <- function(
    lib_path,
    mojo_file,
    flags = "",
    verbose = FALSE
) {
    if (file.exists(lib_path)) {
        return(invisible(NULL))
    }
    flags <- as.character(flags[[1L]])
    build_cmd <- function(extra_flags = "") {
        extra <- if (nzchar(trimws(extra_flags))) {
            paste0(" ", trimws(extra_flags))
        } else {
            ""
        }
        sprintf(
            "mojo build --emit shared-lib -o %s%s %s",
            shQuote(lib_path),
            extra,
            shQuote(mojo_file)
        )
    }
    cmd <- build_cmd(flags)
    if (isTRUE(verbose)) {
        message("Mojo build: ", cmd)
    }
    status <- system(cmd)
    if (!identical(as.integer(status), 0L) || !file.exists(lib_path)) {
        sysname <- tolower(Sys.info()[["sysname"]])
        should_retry_o0 <- identical(sysname, "linux")
        if (isTRUE(should_retry_o0)) {
            retry_flags <- gsub("(^|[[:space:]])-O[0-3]([[:space:]]|$)", " ", flags, perl = TRUE)
            retry_flags <- paste(trimws(retry_flags), "-O0")
            retry_cmd <- build_cmd(retry_flags)
            if (isTRUE(verbose)) {
                message("Mojo build retry (-O0 Linux fallback): ", retry_cmd)
            }
            retry_status <- system(retry_cmd)
            if (identical(as.integer(retry_status), 0L) && file.exists(lib_path)) {
                return(invisible(NULL))
            }
        }
        stop("Mojo build failed", call. = FALSE)
    }
    invisible(NULL)
}
