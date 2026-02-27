# =============================================================================
# MojoR Serialization ()
# =============================================================================
# Functions to save and load compiled kernels for reproducibility and
# caching. API: mojor_save(built, path, embed_source = FALSE)
# mojor_load_kernel(path, load_library = TRUE)
# mojor_check_compatibility(saved, version = NULL)
# =============================================================================

# =============================================================================
# Constants
# =============================================================================

MOJOR_SERIAL_VERSION <- "1.0.0"
MOJOR_SERIAL_FORMAT <- "rds"

# =============================================================================
# Core Functions
# =============================================================================

#' Save a compiled kernel to disk
#'
#' @param built The built kernel object returned by `mojor_build()`
#' @param path Path to save the serialized kernel (`.mojor` extension recommended)
#' @param embed_source If TRUE, embeds the Mojo source code in the saved file
#' for portability (increases file size)
#'
#' @return Invisibly returns the saved object
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
#' mojor_save(built, 'my_kernel.mojor')
#'
#' # Later...
#' built <- mojor_load_kernel('my_kernel.mojor')
#' result <- built$func(x)
#' }
#'
#' @export
mojor_save <- function(built, path, embed_source = FALSE) {
 # Validate inputs
    if (is.null(built)) {
        stop("mojor_save: built cannot be NULL")
    }
    if (is.null(built$kernel)) {
        stop(
            "mojor_save: built object missing 'kernel' field (not a valid built kernel)"
        )
    }
    if (is.null(built$trans)) {
        stop(
            "mojor_save: built object missing 'trans' field (not a valid built kernel)"
        )
    }
    if (isTRUE(built$object_mode) ||
        isTRUE(built$trans$object_mode)) {
        mode_kind <- if (!is.null(built$object_mode_kind))
            built$object_mode_kind else if (!is.null(built$trans$object_mode_kind))
            built$trans$object_mode_kind else "object"
        stop(
            "mojor_save: object-mode kernels are not serializable yet (mode=",
            mode_kind, ")"
        )
    }
    if (is.null(built$build_dir)) {
        stop(
            "mojor_save: built object missing 'build_dir' field (not a valid built kernel)"
        )
    }
    if (is.null(built$wrapper_so)) {
        stop(
            "mojor_save: built object missing 'wrapper_so' field (not a valid built kernel)"
        )
    }
    if (is.null(built$cache_key)) {
        stop(
            "mojor_save: built object missing 'cache_key' field (not a valid built kernel)"
        )
    }

 # Extract trans object
    trans <- built$trans

 # Build serialization object
    serial <- list(
        version = MOJOR_SERIAL_VERSION, format = MOJOR_SERIAL_FORMAT, kernel = built$kernel,
        cache_key = built$cache_key, build_dir = built$build_dir, wrapper_so = built$wrapper_so,
        build_timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
        trans = trans
    )

 # Optionally embed Mojo source
    if (isTRUE(embed_source)) {
        serial$mojo_source <- trans$mojo
    }

 # Ensure directory exists
    dir.create(
        dirname(path),
        recursive = TRUE, showWarnings = FALSE
    )

 # Save to file
    saveRDS(serial, path)

    invisible(serial)
}

#' Load a compiled kernel from disk
#'
#' @param path Path to the saved kernel file
#' @param load_library If TRUE, loads the shared library and creates func wrapper
#' @param force_load If TRUE, loads library even if build_dir doesn't exist
#'
#' @return A built kernel object with `func` and other fields
#'
#' @examples
#' \dontrun{
#' built <- mojor_load_kernel('my_kernel.mojor')
#' result <- built$func(x)
#' }
#'
#' @export
mojor_load_kernel <- function(path, load_library = TRUE, force_load = FALSE) {
 # Validate path
    if (!file.exists(path)) {
        stop("mojor_load: file not found: ", path)
    }

 # Read serialized object
    serial <- tryCatch(
        readRDS(path),
        error = function(e) {
            stop("mojor_load: failed to read RDS file: ", e$message)
        }
    )

 # Check version compatibility
    mojor_check_compatibility(serial)

 # Validate required fields
    if (is.null(serial$kernel)) {
        stop("mojor_load: saved object missing 'kernel' field")
    }
    if (is.null(serial$trans)) {
        stop("mojor_load: saved object missing 'trans' field")
    }
    if (is.null(serial$build_dir)) {
        stop("mojor_load: saved object missing 'build_dir' field")
    }
    if (is.null(serial$wrapper_so)) {
        stop("mojor_load: saved object missing 'wrapper_so' field")
    }
    if (is.null(serial$cache_key)) {
        stop("mojor_load: saved object missing 'cache_key' field")
    }

 # Check if build artifacts exist
    build_dir_exists <- dir.exists(serial$build_dir)
    wrapper_exists <- file.exists(serial$wrapper_so)

 # Build result object
    result <- list(
        kernel = serial$kernel, cache_key = serial$cache_key, build_dir = serial$build_dir,
        wrapper_so = serial$wrapper_so, trans = serial$trans, build_timestamp = serial$build_timestamp,
        serial_version = serial$version, success = TRUE
    )

 # Optionally load library and create func wrapper
    if (isTRUE(load_library)) {
        if (!isTRUE(force_load) &&
            !isTRUE(build_dir_exists)) {
            warning(
                "mojor_load: build directory does not exist: ", serial$build_dir,
                "\n  Use force_load = TRUE to attempt loading anyway"
            )
        }

        if (!isTRUE(force_load) &&
            !isTRUE(wrapper_exists)) {
            warning(
                "mojor_load: wrapper shared library does not exist: ",
                serial$wrapper_so, "\n  Use force_load = TRUE to attempt loading anyway"
            )
        }

        if (isTRUE(build_dir_exists) &&
            isTRUE(wrapper_exists)) {
 # Load the shared library
            dll <- tryCatch(
                .mojor_dyn_load_with_recovery(
                    so_path = serial$wrapper_so,
                    lib_dir = serial$build_dir,
                    retry_on_limit = TRUE
                ),
                error = function(e) {
                  stop("mojor_load: failed to load shared library: ", e$message)
                }
            )
            pkg <- dll[["name"]]

 # Get trans object
            trans <- serial$trans

 # Check if RNG is needed
            rng_needed <- isTRUE(trans$rng_needed)

 # Create func wrapper
            na_preflight <- function(...) {
                args_in <- list(...)
                .mojor_check_no_na_args(args_in, trans$types, names(trans$types))
                .mojor_check_no_na_reduction_arg(
                  args_in, trans$types, names(trans$types),
                  reduction_op = trans$scalar_reduction_op, reduction_arg = trans$scalar_reduction_arg
              )
            }
            f <- .mojor_make_kernel_call_wrapper(
                kernel = serial$kernel, pkg = pkg, rng_needed = rng_needed,
                na_mode = trans$na_mode, na_preflight = na_preflight, error_state = NULL,
                wrapper_so = serial$wrapper_so
            )

            result$func <- f

 # Check for GPU functions
            if (!is.null(trans$gpu_buf_dtype) &&
                !is.null(trans$gpu_call_name)) {
                gpu_buf_class <- paste0("mojor_gpu_buf_", trans$gpu_buf_dtype)
                gpu_buf_len_call <- paste0("mojor_gpu_buf_", trans$gpu_buf_dtype, "_len")
                gpu_call_name <- trans$gpu_call_name

                gpu_raw <- function(...) {
 # GPU implementation (simplified - matches
 # mojor_build pattern)
                  stop("GPU functions not fully implemented in serialization v1.0")
                }

                result$gpu_func_raw <- gpu_raw
            }

            result$success <- TRUE
        } else {
            result$success <- FALSE
            result$load_error <- "Build artifacts not found"
        }
    }

    result
}

#' Check version compatibility of a saved kernel
#'
#' @param saved The saved kernel object (from readRDS or mojor_load)
#' @param version The expected version (NULL uses current MOJOR_SERIAL_VERSION)
#' @param strict If TRUE, error on version mismatch; if FALSE, warn
#'
#' @return TRUE if compatible, throws error or returns FALSE otherwise
#'
#' @examples
#' \dontrun{
#' serial <- readRDS('my_kernel.mojor')
#' mojor_check_compatibility(serial)
#' }
#'
#' @export
mojor_check_compatibility <- function(saved, version = NULL, strict = FALSE) {
    if (is.null(version)) {
        version <- MOJOR_SERIAL_VERSION
    }

 # Check if saved object has version field
    if (is.null(saved$version)) {
        if (isTRUE(strict)) {
            stop("mojor_check_compatibility: saved object has no version field")
        }
        warning("mojor_check_compatibility: saved object has no version field")
        return(FALSE)
    }

 # Parse versions
    saved_ver <- saved$version
    expected_ver <- version

 # Simple version comparison (major.minor.patch)
    saved_parts <- as.integer(strsplit(saved_ver, "\\.")[[1]])
    expected_parts <- as.integer(strsplit(expected_ver, "\\.")[[1]])

 # Pad to same length
    max_len <- max(
        length(saved_parts),
        length(expected_parts)
    )
    saved_parts <- c(saved_parts, rep(0, max_len - length(saved_parts)))
    expected_parts <- c(expected_parts, rep(0, max_len - length(expected_parts)))

 # Compare major version (must match exactly)
    if (saved_parts[1] != expected_parts[1]) {
        msg <- sprintf(
            "mojor_check_compatibility: major version mismatch (saved: %s, expected: %s)",
            saved_ver, expected_ver
        )
        if (isTRUE(strict)) {
            stop(msg)
        }
        warning(msg)
        return(FALSE)
    }

 # Minor version mismatch is a warning
    if (saved_parts[2] != expected_parts[2]) {
        msg <- sprintf(
            "mojor_check_compatibility: minor version mismatch (saved: %s, expected: %s)",
            saved_ver, expected_ver
        )
        if (isTRUE(strict)) {
            stop(msg)
        }
        warning(msg)
        return(FALSE)
    }

    TRUE
}

# =============================================================================
# Helper Functions
# =============================================================================

#' Get information about a saved kernel without loading it
#'
#' @param path Path to the saved kernel file
#'
#' @return A list with kernel metadata
#'
#' @examples
#' \dontrun{
#' info <- mojor_serialize('my_kernel.mojor')
#' print(info)
#' }
#'
#' @export
mojor_serialize <- function(path) {
    if (!file.exists(path)) {
        stop("mojor_serialize: file not found: ", path)
    }

 # Read just the metadata (first part of RDS) We need to read the
 # full file but only return metadata
    serial <- tryCatch(
        readRDS(path),
        error = function(e) {
            stop("mojor_serialize: failed to read RDS file: ", e$message)
        }
    )

    list(
        path = path, version = serial$version, format = serial$format,
        kernel = serial$kernel, cache_key = serial$cache_key, build_timestamp = serial$build_timestamp,
        has_mojo_source = !is.null(serial$mojo_source),
        trans_fields = names(serial$trans)
    )
}
