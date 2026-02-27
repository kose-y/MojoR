# ----------------------------------------------------------------------------
# Parallel Loop Subprocess Execution
# ----------------------------------------------------------------------------
# Workaround for parallelize segfault when loaded via dyn.load.
# Generates standalone Mojo executables instead of shared libraries.

.mojor_parallel_seed_spec <- function(type_annot) {
    base <- sub("(\\[\\]|\\[1d\\])$", "", type_annot)
    switch(
        base, f64 = list(
            mojo_type = "Float64", scalar_init = "2.0", vector_init = "Float64(i + 1)",
            zero_init = "0.0"
        ),
        f32 = list(
            mojo_type = "Float32", scalar_init = "2.0", vector_init = "Float32(i + 1)",
            zero_init = "0.0"
        ),
        i32 = list(
            mojo_type = "Int32", scalar_init = "Int32(2)", vector_init = "Int32(i + 1)",
            zero_init = "Int32(0)"
        ),
        bool = list(
            mojo_type = "Bool", scalar_init = "True", vector_init = "((i % 2) == 0)",
            zero_init = "False"
        ),
        lgl = list(
            mojo_type = "Int32", scalar_init = "Int32(1)", vector_init = "Int32((i + 1) % 2)",
            zero_init = "Int32(0)"
        ),
        stop(
            "mojor_export_parallel: unsupported type annotation for standalone runner: ",
            type_annot
        )
    )
}

.mojor_parallel_runtime_mode <- function() {
    env_mode <- tolower(trimws(Sys.getenv("MOJOR_PARALLEL_RUNTIME", "")))
    if (env_mode %in% c("inprocess", "subprocess")) {
        return(env_mode)
    }
    sys <- tolower(Sys.info()[["sysname"]])
    if (identical(sys, "darwin")) {
        return("subprocess")
    }
    "inprocess"
}

.mojor_parallel_inprocess_enabled <- function() {
    identical(.mojor_parallel_runtime_mode(), "inprocess")
}

.mojor_parallel_runtime_reason <- function(mode = .mojor_parallel_runtime_mode()) {
    if (identical(mode, "subprocess")) {
        return(
            "in-process parallel runtime disabled on this platform; subprocess fallback required"
        )
    }
    "in-process parallel runtime enabled"
}

.mojor_trans_uses_parallelize <- function(trans) {
    if (is.null(trans) ||
        !is.list(trans)) {
        return(FALSE)
    }
    if (is.list(trans$parallel) &&
        !is.null(trans$parallel$effective)) {
        return(isTRUE(trans$parallel$effective))
    }
    if (is.list(trans$parallel) &&
        !is.null(trans$parallel$uses_parallelize)) {
        return(isTRUE(trans$parallel$uses_parallelize))
    }
    if (is.null(trans$mojo)) {
        return(FALSE)
    }
    mojo <- as.character(trans$mojo)
    nzchar(mojo) &&
        grepl("parallelize", mojo, fixed = TRUE)
}

.mojor_parallel_runtime_fallback_result <- function(fn, trans, name, build_dir, cache_key, reason) {
    wrapped <- function(...) fn(...)
    attr(wrapped, "parallel_runtime_mode") <- "subprocess_fallback"
    attr(wrapped, "parallel_runtime_reason") <- reason
    list(
        func = wrapped, gpu_func = NULL, gpu_func_raw = NULL, build_dir = build_dir,
        kernel = name, cache_key = cache_key, trans = trans, wrapper_so = NULL,
        success = TRUE, compiled = FALSE, parallel_runtime = list(
            mode = "subprocess_fallback", reason = reason, inprocess_enabled = FALSE
        )
    )
}

#' Export parallel function to standalone Mojo executable
#'
#' This function generates a complete, standalone Mojo program that can be
#' compiled and run separately. This avoids the dyn.load issues with parallelize.
#'
#' The generated program reads input from files, runs the parallel kernel,
#' and writes output to a file.
#'
#' @param fn R function to transpile
#' @param ... Type annotations (e.g., x = 'f64[]')
#' @param name Output filename (without extension)
#' @param output_dir Directory to write the .mojo file
#' @return Path to generated .mojo file
#' @export
mojor_export_parallel <- function(fn, ..., name = "parallel_kernel", output_dir = getwd()) {
 # Get the parallel transpilation
    trans <- mojor_transpile(
        fn, ..., name = paste0(name, "_kernel"),
        parallel = TRUE
    )

    arg_types <- unlist(trans$types, use.names = TRUE)
    if (!length(arg_types) ||
        is.null(names(arg_types)) ||
        any(!nzchar(names(arg_types)))) {
        stop(
            "mojor_export_parallel: transpiler did not return named argument type metadata"
        )
    }
    if (any(
        grepl("\\[", arg_types) &
            !grepl("\\[\\]$", arg_types)
    )) {
        stop(
            "mojor_export_parallel: standalone runner currently supports only scalar and 1D array arguments"
        )
    }
    if (!nzchar(trans$out_type) ||
        (grepl("\\[", trans$out_type) &&
            !grepl("\\[\\]$", trans$out_type))) {
        stop(
            "mojor_export_parallel: standalone runner currently supports only scalar or 1D array outputs"
        )
    }

    sig_names <- trimws(sub(":.*$", "", trans$signature))
    if (!length(sig_names)) {
        stop("mojor_export_parallel: empty transpiler signature")
    }

    input_array_names <- names(arg_types)[grepl("\\[\\]$", arg_types)]
    input_scalar_names <- names(arg_types)[!grepl("\\[\\]$", arg_types)]
    input_ptr_names <- paste0(input_array_names, "_ptr")

    out_ptr_candidates <- sig_names[grepl("_ptr$", sig_names) &
        !(sig_names %in% c(input_ptr_names, "__mojor_na_flag_ptr")) &
        !grepl("^__mojor_len_", sig_names)]
    if (length(out_ptr_candidates) !=
        1L) {
        stop(
            "mojor_export_parallel: expected exactly one output pointer in generated signature"
        )
    }
    out_ptr_name <- out_ptr_candidates[[1]]

    array_specs <- lapply(arg_types[input_array_names], .mojor_parallel_seed_spec)
    scalar_specs <- lapply(arg_types[input_scalar_names], .mojor_parallel_seed_spec)
    out_spec <- .mojor_parallel_seed_spec(trans$out_type)
    out_is_array <- grepl("\\[\\]$", trans$out_type)
    has_na_flag <- "__mojor_na_flag_ptr" %in% sig_names

    main_lines <- c(
        "", "# Standalone executable entry point", "from sys import argv",
        "", "fn main():", "    var n = 1024", "    if len(argv()) > 1:",
        "        print(\"Note: standalone runner currently ignores CLI args and uses synthetic inputs\")"
    )

    for (nm in input_scalar_names) {
        main_lines <- c(
            main_lines, paste0("    var ", nm, " = ", scalar_specs[[nm]]$scalar_init)
        )
    }
    for (nm in input_array_names) {
        ptr_nm <- paste0(nm, "_ptr")
        spec <- array_specs[[nm]]
        main_lines <- c(
            main_lines, paste0("    var ", ptr_nm, " = alloc[", spec$mojo_type, "](n)"),
            "    for i in range(n):", paste0("        ", ptr_nm, "[i] = ", spec$vector_init)
        )
    }

    out_len <- if (out_is_array)
        "n" else "1"
    main_lines <- c(
        main_lines, paste0(
            "    var ", out_ptr_name, " = alloc[", out_spec$mojo_type,
            "](", out_len, ")"
        )
    )
    if (out_is_array) {
        main_lines <- c(
            main_lines, "    for i in range(n):", paste0("        ", out_ptr_name, "[i] = ", out_spec$zero_init)
        )
    } else {
        main_lines <- c(
            main_lines, paste0("    ", out_ptr_name, "[0] = ", out_spec$zero_init)
        )
    }
    if (has_na_flag) {
        main_lines <- c(
            main_lines, "    var __mojor_na_flag_raw = alloc[Int32](1)",
            "    __mojor_na_flag_raw[0] = 0"
        )
    }

    call_args <- vapply(
        sig_names, function(param) {
            if (param %in% input_ptr_names) {
                return(paste0(param, ".bitcast[NoneType]()"))
            }
            if (param %in% input_scalar_names) {
                return(param)
            }
            if (identical(param, out_ptr_name)) {
                return(paste0(out_ptr_name, ".bitcast[NoneType]()"))
            }
            if (identical(param, "__mojor_n")) {
                return("Int32(n)")
            }
            if (grepl("^__mojor_len_", param)) {
                return("Int32(n)")
            }
            if (identical(param, "__mojor_na_flag_ptr")) {
                return("__mojor_na_flag_raw.bitcast[NoneType]()")
            }
            stop(
                "mojor_export_parallel: unsupported generated signature parameter: ",
                param
            )
        }, character(1)
    )

    main_lines <- c(
        main_lines, paste0(
            "    ", trans$name, "(", paste(call_args, collapse = ", "),
            ")"
        ),
        "    print(\"STANDALONE_OK\")", if (has_na_flag) "    print(\"NA_FLAG\", __mojor_na_flag_raw[0])",
        paste0("    print(\"OUT0\", ", out_ptr_name, "[0])")
    )

    for (nm in input_array_names) {
        main_lines <- c(main_lines, paste0("    ", nm, "_ptr.free()"))
    }
    main_lines <- c(main_lines, paste0("    ", out_ptr_name, ".free()"))
    if (has_na_flag) {
        main_lines <- c(main_lines, "    __mojor_na_flag_raw.free()")
    }

    mojo_lines <- strsplit(trans$mojo, "\n", fixed = TRUE)[[1]]
    mojo_lines <- gsub(
        "^comptime ImmutOpaqueAny = OpaquePointer\\[mut=False, origin=ImmutAnyOrigin\\]$",
        "comptime ImmutOpaqueAny = UnsafePointer[mut=False, type=NoneType]",
        mojo_lines
    )
    mojo_lines <- gsub(
        "^comptime MutOpaqueAny = OpaquePointer\\[mut=True, origin=MutAnyOrigin\\]$",
        "comptime MutOpaqueAny = UnsafePointer[mut=True, type=NoneType]",
        mojo_lines
    )
    mojo_lines <- mojo_lines[!grepl("^@export\\(", mojo_lines)]

    full_code <- paste(
        c(mojo_lines, main_lines),
        collapse = "\n"
    )

    output_file <- file.path(output_dir, paste0(name, ".mojo"))
    writeLines(full_code, output_file)

    message("Generated standalone runner: ", output_file)
    message("Run with: mojo ", shQuote(output_file))

    invisible(output_file)
}

#' Run parallel Mojo code via subprocess
#'
#' Executes Mojo code in a subprocess, avoiding dyn.load issues.
#' This is useful for testing parallel kernels.
#'
#' @param mojo_code Complete Mojo source code
#' @param args Command line arguments
#' @return Output from mojo run
#' @export
mojor_run_subprocess <- function(mojo_code, args = character()) {
 # Write code to temp file
    temp_file <- tempfile("mojor_", fileext = ".mojo")
    writeLines(mojo_code, temp_file)
    on.exit(
        unlink(temp_file),
        add = TRUE
    )

 # Run via mojo
    cmd <- paste(
        "mojo run", shQuote(temp_file),
        paste(
            shQuote(args),
            collapse = " "
        )
    )
    result <- system(cmd, intern = TRUE)

    result
}

#' Compile Mojo code to executable
#'
#' Compiles Mojo source to a standalone executable.
#'
#' @param mojo_code Mojo source code
#' @param output_path Output executable path
#' @return Path to compiled executable
#' @export
mojor_compile_executable <- function(mojo_code, output_path) {
 # Write code to temp file
    temp_file <- tempfile("mojor_", fileext = ".mojo")
    writeLines(mojo_code, temp_file)
    on.exit(
        unlink(temp_file),
        add = TRUE
    )

 # Build
    cmd <- paste(
        "mojo build", shQuote(temp_file),
        "-o", shQuote(output_path)
    )
    status <- system(cmd)

    if (status != 0) {
        stop("Compilation failed")
    }

    invisible(output_path)
}

#' Run independent chains in parallel workers
#'
#' Executes independent MCMC chains with deterministic per-chain seeds.
#' This is intended for kernels where each chain is sequential but chains are
#' independent and can be distributed across workers.
#'
#' @param chain_fun Function that runs one chain.
#' @param n_chains Number of chains.
#' @param chain_args Named list of arguments passed to `chain_fun`.
#' @param seed Base seed. Chain `i` uses `seed + i - 1`.
#' @param workers Number of workers to use.
#' @param backend One of `'auto'`, `'fork'`, or `'sequential'`.
#' @param use_mojor_rng If `TRUE`, seed each chain with `mojor_rng_seed()`;
#' otherwise use `set.seed()`.
#' @return A list of chain outputs.
#' @export
mojor_run_chains_parallel <- function(
    chain_fun, n_chains, chain_args = list(), seed = 1L, workers = getOption("mc.cores", 2L),
    backend = c("auto", "fork", "sequential"),
    use_mojor_rng = TRUE
) {
    if (!is.function(chain_fun))
        stop("mojor_run_chains_parallel: chain_fun must be a function")
    if (!is.numeric(n_chains) ||
        length(n_chains) !=
            1 || is.na(n_chains) ||
        n_chains < 1) {
        stop("mojor_run_chains_parallel: n_chains must be a positive integer")
    }
    n_chains <- as.integer(n_chains)
    if (!is.list(chain_args))
        stop("mojor_run_chains_parallel: chain_args must be a list")
    if (!is.numeric(seed) ||
        length(seed) !=
            1 || is.na(seed)) {
        stop("mojor_run_chains_parallel: seed must be a finite scalar")
    }
    seed <- as.integer(seed)
    if (!is.numeric(workers) ||
        length(workers) !=
            1 || is.na(workers) ||
        workers < 1) {
        stop("mojor_run_chains_parallel: workers must be a positive integer")
    }
    workers <- as.integer(workers)
    backend <- match.arg(backend)

    if (identical(backend, "auto")) {
        backend <- if (.Platform$OS.type == "windows" || workers <= 1L ||
            n_chains <= 1L) {
            "sequential"
        } else {
            "fork"
        }
    }

    run_one <- function(chain_id) {
        chain_seed <- as.integer(seed + chain_id - 1L)
        if (isTRUE(use_mojor_rng)) {
            mojor_rng_seed(chain_seed)
        } else {
            set.seed(chain_seed)
        }
        do.call(chain_fun, chain_args)
    }

    if (identical(backend, "sequential") ||
        n_chains == 1L || workers == 1L) {
        return(
            lapply(
                seq_len(n_chains),
                run_one
            )
        )
    }

    if (identical(backend, "fork")) {
        if (.Platform$OS.type == "windows") {
            stop(
                "mojor_run_chains_parallel: backend='fork' is not supported on Windows"
            )
        }
        mc_cores <- max(1L, min(workers, n_chains))
        return(
            parallel::mclapply(
                seq_len(n_chains),
                run_one, mc.cores = mc_cores, mc.set.seed = FALSE
            )
        )
    }

    stop("mojor_run_chains_parallel: unsupported backend")
}
