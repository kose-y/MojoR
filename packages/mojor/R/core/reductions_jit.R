.mojor_call_scalar <- function(kernel, x) {
    if (!mojor_is_loaded())
        stop("Mojo backend not loaded")
    .mojor_call_bridge(kernel, x)
}

.mojor_reduce_empty <- function(op) {
    op <- match.arg(op, c("sum", "min", "max", "mean", "sd", "var", "prod", "which"))
    if (op == "sum") {
        return(sum(numeric(0)))
    }
    if (op == "min") {
        return(min(numeric(0)))
    }
    if (op == "max") {
        return(max(numeric(0)))
    }
    if (op == "mean") {
        return(mean(numeric(0)))
    }
    if (op == "prod") {
        return(prod(numeric(0)))
    }
    if (op == "which") {
        return(integer(0))
    }
    if (op == "sd") {
        return(sd(numeric(0)))
    }
    var(numeric(0))
}

.mojor_reduce_input <- function(x, dtype, na_mode = NULL) {
    dtype <- match.arg(dtype, c("f64", "f32"))
    mode <- .mojor_effective_na_mode(na_mode)
    if (dtype == "f64") {
        if (mode == "forbid") {
            .mojor_check_no_na(x, "f64[]", "x")
        }
        return(list(values = x, n = length(x)))
    }
    x_f <- .mojor_as_float32(x)
    if (mode == "forbid") {
        .mojor_check_no_na(x_f, "f32[]", "x")
    }
    list(values = methods::slot(x_f, "Data"), n = length(x_f))
}

.mojor_reduce_bridge_is_unsafe <- function() {
    sys <- tolower(Sys.info()[["sysname"]])
    machine <- tolower(Sys.info()[["machine"]])
    if (!identical(sys, "linux") || !(machine %in% c("aarch64", "arm64"))) {
        return(FALSE)
    }
    pkg <- tryCatch(.mojor_bridge_pkg(required = FALSE), error = function(e) NULL)
    if (is.null(pkg) || !is.character(pkg) || !nzchar(pkg)) {
        return(FALSE)
    }
    dlls <- getLoadedDLLs()
    if (!(pkg %in% names(dlls))) {
        return(FALSE)
    }
    dll_path <- tryCatch(as.character(getElement(dlls[[pkg]], "path")), error = function(e) "")
    if (!is.character(dll_path) || !nzchar(dll_path)) {
        return(FALSE)
    }
    identical(basename(dll_path), paste0("mojor", .Platform$dynlib.ext))
}

.mojor_reduce_call <- function(x, kernel, op, dtype, na_mode = NULL) {
    input <- .mojor_reduce_input(x, dtype, na_mode)
    if (input$n ==
        0) {
        empty_op <- if (op == "which")
            "which" else op
        if (empty_op %in% c("which", "min", "max", "mean", "prod", "sd", "var")) {
            return(.mojor_reduce_empty(empty_op))
        }
    }
    if (isTRUE(.mojor_reduce_bridge_is_unsafe()) &&
        op %in% c("sum", "min", "max", "mean", "sd", "var", "prod")) {
        values <- as.numeric(input$values)
        return(switch(op,
            sum = sum(values),
            min = min(values),
            max = max(values),
            mean = mean(values),
            sd = sd(values),
            var = var(values),
            prod = prod(values)
        ))
    }
    out <- .mojor_call_scalar(kernel, input$values)
    if (op == "which") {
        return(as.integer(out))
    }
    as.numeric(out)
}

.mojor_reduce_f64 <- function(x, kernel, op, na_mode = NULL) {
    .mojor_reduce_call(x, kernel, op, "f64", na_mode)
}

.mojor_reduce_f32 <- function(x, kernel, op, na_mode = NULL) {
    .mojor_reduce_call(x, kernel, op, "f32", na_mode)
}

.mojor_reduce_specs <- list(
    prod = list(
        op = "prod", f64 = "mojor_prod_f64_nomiss", f32 = "mojor_prod_f32_nomiss_int"
    ),
    min = list(
        op = "min", f64 = "mojor_min_f64_nomiss", f32 = "mojor_min_f32_nomiss_int"
    ),
    max = list(
        op = "max", f64 = "mojor_max_f64_nomiss", f32 = "mojor_max_f32_nomiss_int"
    ),
    mean = list(
        op = "mean", f64 = "mojor_mean_f64_nomiss", f32 = "mojor_mean_f32_nomiss_int"
    )
)

.mojor_which_specs <- list(
    min = list(
        f64 = "mojor_which_min_f64_nomiss", f32 = "mojor_which_min_f32_nomiss_int"
    ),
    max = list(
        f64 = "mojor_which_max_f64_nomiss", f32 = "mojor_which_max_f32_nomiss_int"
    )
)

.mojor_sdvar_specs <- list(
    sd = list(
        f64 = list(
            welford = "mojor_sd_f64_nomiss", two_pass = "mojor_sd_f64_nomiss_twopass"
        ),
        f32 = list(
            welford = "mojor_sd_f32_nomiss_int", two_pass = "mojor_sd_f32_nomiss_twopass_int"
        )
    ),
    var = list(
        f64 = list(
            welford = "mojor_var_f64_nomiss", two_pass = "mojor_var_f64_nomiss_twopass"
        ),
        f32 = list(
            welford = "mojor_var_f32_nomiss_int", two_pass = "mojor_var_f32_nomiss_twopass_int"
        )
    )
)

.mojor_reduce_fn <- function(kernel, op, dtype) {
    force(kernel)
    force(op)
    force(dtype)
    if (dtype == "f64") {
        function(x) .mojor_reduce_f64(x, kernel, op)
    } else {
        function(x) .mojor_reduce_f32(x, kernel, op)
    }
}

.mojor_sdvar_fn <- function(kind, dtype, kernels) {
    force(kind)
    force(dtype)
    force(kernels)
    function(x, sd_mode = NULL) {
        if (isTRUE(.mojor_reduce_bridge_is_unsafe())) {
            values <- as.numeric(x)
            if (identical(kind, "sd")) {
                return(sd(values))
            }
            return(var(values))
        }
        mode <- if (is.null(sd_mode))
            .mojor_state$options$sd_mode else match.arg(sd_mode, c("welford", "two_pass"))
        kernel <- if (identical(mode, "two_pass"))
            kernels$two_pass else kernels$welford
        if (dtype == "f64") {
            .mojor_reduce_f64(x, kernel, kind)
        } else {
            .mojor_reduce_f32(x, kernel, kind)
        }
    }
}

mojor_sum_f64_std <- function(x) .mojor_reduce_f64(x, "mojor_sum_f64_nomiss", "sum")
 .mojor_sum_f64_kernel <- function(x, kernel, label) {
    if (!mojor_is_loaded())
        stop("Mojo backend not loaded")
    if (!is.numeric(x))
        stop(label, ": expected numeric vector")
    if (identical(
        .mojor_effective_na_mode(NULL),
        "forbid"
    )) {
        .mojor_check_no_na(x, "f64[]", "x")
    }
    if (isTRUE(.mojor_reduce_bridge_is_unsafe())) {
        return(sum(as.double(x)))
    }
    .mojor_call_bridge(kernel, as.double(x))
 }
mojor_sum_f64_stdlib <- function(x) .mojor_sum_f64_kernel(x, "mojor_sum_f64_std", "mojor_sum_f64_stdlib")
mojor_sum_f64_manual <- function(x) .mojor_reduce_f64(x, "mojor_sum_f64_nomiss_manual", "sum")
mojor_sum_f64_pairwise <- function(x) .mojor_sum_f64_kernel(
    x,
    "mojor_sum_f64_pairwise",
    "mojor_sum_f64_pairwise"
)
mojor_sum_f64 <- function(x, method = c("std", "manual", "stdlib")) {
    method <- match.arg(method)
    if (method == "std") {
        mojor_sum_f64_std(x)
    } else if (method == "stdlib") {
        mojor_sum_f64_stdlib(x)
    } else {
        mojor_sum_f64_manual(x)
    }
}

{
    .mojor_reduce_env <- environment()
    for (nm in names(.mojor_reduce_specs)) {
        spec <- .mojor_reduce_specs[[nm]]
        assign(
            paste0("mojor_", nm, "_f64"),
            .mojor_reduce_fn(spec$f64, spec$op, "f64"),
            envir = .mojor_reduce_env
        )
        assign(
            paste0("mojor_", nm, "_f32"),
            .mojor_reduce_fn(spec$f32, spec$op, "f32"),
            envir = .mojor_reduce_env
        )
    }
    for (nm in names(.mojor_which_specs)) {
        spec <- .mojor_which_specs[[nm]]
        assign(
            paste0("mojor_which_", nm, "_f64"),
            .mojor_reduce_fn(spec$f64, "which", "f64"),
            envir = .mojor_reduce_env
        )
        assign(
            paste0("mojor_which_", nm, "_f32"),
            .mojor_reduce_fn(spec$f32, "which", "f32"),
            envir = .mojor_reduce_env
        )
    }
    for (nm in names(.mojor_sdvar_specs)) {
        spec <- .mojor_sdvar_specs[[nm]]
        assign(
            paste0("mojor_", nm, "_f64"),
            .mojor_sdvar_fn(nm, "f64", spec$f64),
            envir = .mojor_reduce_env
        )
        assign(
            paste0("mojor_", nm, "_f32"),
            .mojor_sdvar_fn(nm, "f32", spec$f32),
            envir = .mojor_reduce_env
        )
    }
}

mojor_sum_f32_stdlib <- function(x) {
    if (!mojor_is_loaded())
        stop("Mojo backend not loaded")
    x_f <- .mojor_as_float32(x)
    data <- methods::slot(x_f, "Data")
    if (identical(
        .mojor_effective_na_mode(NULL),
        "forbid"
    )) {
        .mojor_check_no_na(x_f, "f32[]", "x")
    }
    .mojor_call_bridge("mojor_sum_f32_std_int", data)
}
mojor_sum_f32_pairwise <- function(x) .mojor_reduce_f32(x, "mojor_sum_f32_pairwise_int", "sum")
mojor_sum_f32 <- function(x, method = c("nomiss", "stdlib", "pairwise")) {
    method <- match.arg(method)
    if (method == "stdlib") {
        mojor_sum_f32_stdlib(x)
    } else if (method == "pairwise") {
        mojor_sum_f32_pairwise(x)
    } else {
        .mojor_reduce_f32(x, "mojor_sum_f32_nomiss_int", "sum")
    }
}
.validate_types <- function(args, types) {
    for (name in names(types)) {
        if (!name %in% names(args)) {
            stop("Missing argument in call: ", name)
        }

        spec <- types[[name]]
        reg <- .mojor_type_registry[[spec]]
        if (is.null(reg)) {
            stop("Unknown type spec: ", spec)
        }

        if (!reg$check(args[[name]])) {
            stop("Argument '", name, "' does not match type ", spec)
        }
    }
    TRUE
}

mojor_compile <- function(fn, kernel, ..., .validate = TRUE) {
    if (!is.function(fn)) {
        stop("mojor_compile: fn must be a function")
    }
    if (!is.character(kernel) ||
        length(kernel) !=
            1) {
        stop("mojor_compile: kernel must be a single string name")
    }

    types <- list(...)
    if (length(types) ==
        0) {
        stop("mojor_compile: provide type annotations like x = 'f64[]'")
    }
    arg_names <- names(formals(fn))

    function(...) {
        args <- list(...)
        if (length(args) >
            0 && length(arg_names) >
            0) {
            if (is.null(names(args))) {
                names(args) <- arg_names[seq_along(args)]
            } else {
                blank <- names(args) ==
                  ""
                names(args)[blank] <- arg_names[seq_along(args)][blank]
            }
        }

        if (.validate) {
            .validate_types(args, types)
        }
        if (identical(
            .mojor_effective_na_mode(NULL),
            "forbid"
        )) {
            .mojor_check_no_na_args(args, types, arg_names)
        }
        if (!mojor_is_loaded()) {
            stop("Mojo backend not loaded; call mojor_load() first")
        }

        do.call(
            .mojor_call_bridge, c(
                list(kernel),
                args
            )
        )
    }
}

.mojor_infer_type_one <- function(x, broadcast = "none") {
    if (requireNamespace("float", quietly = TRUE) &&
        float::is.float(x)) {
        if (length(x) ==
            1 && broadcast != "none") {
            return("f32[]")
        }
        return(
            if (length(x) ==
                1) "f32" else "f32[]"
        )
    }
    if (is.logical(x)) {
        if (length(x) ==
            1 && broadcast != "none") {
            return("lgl[]")
        }
        return(
            if (length(x) ==
                1) "lgl" else "lgl[]"
        )
    }
    if (is.integer(x)) {
        if (length(x) ==
            1 && broadcast != "none") {
            return("i32[]")
        }
        return(
            if (length(x) ==
                1) "i32" else "i32[]"
        )
    }
    if (is.double(x) ||
        is.numeric(x)) {
        if (length(x) ==
            1 && broadcast != "none") {
            return("f64[]")
        }
        return(
            if (length(x) ==
                1) "f64" else "f64[]"
        )
    }
    NULL
}

.mojor_infer_types <- function(args, arg_names, broadcast = "none") {
    types <- list()
    has_f64 <- FALSE
    has_f32 <- FALSE
    has_float <- FALSE
    for (name in arg_names) {
        if (!name %in% names(args))
            next
        spec <- .mojor_infer_type_one(args[[name]], broadcast = broadcast)
        if (is.null(spec)) {
            stop("mojor_jit: cannot infer type for argument ", name)
        }
        types[[name]] <- spec
        if (spec %in% c("f64", "f64[]"))
            has_f64 <- TRUE
        if (spec %in% c("f32", "f32[]"))
            has_f32 <- TRUE
        if (spec %in% c("f64", "f64[]", "f32", "f32[]"))
            has_float <- TRUE
    }
    promotions <- list()
    float_target <- if (has_f64)
        "f64" else if (has_f32)
        "f32" else NULL
    if (!is.null(float_target)) {
        for (nm in names(types)) {
            spec <- types[[nm]]
            if (float_target == "f64" && spec %in% c("f32", "f32[]")) {
                types[[nm]] <- if (spec == "f32")
                  "f64" else "f64[]"
                promotions[[nm]] <- "f64"
                next
            }
            if (has_float && spec %in% c("i32[]", "lgl[]", "bool[]")) {
                types[[nm]] <- paste0(float_target, "[]")
                promotions[[nm]] <- float_target
            }
        }
    }
    attr(types, "promotions") <- promotions
    types
}

.mojor_promote_to_f64 <- function(x) {
    if (requireNamespace("float", quietly = TRUE) &&
        float::is.float(x)) {
        return(float::dbl(x))
    }
    as.double(x)
}

.mojor_promote_to_f32 <- function(x) {
    if (requireNamespace("float", quietly = TRUE) &&
        float::is.float(x)) {
        return(x)
    }
    if (!requireNamespace("float", quietly = TRUE)) {
        stop("mojor_jit: float package required for f32 promotion")
    }
    float::fl(as.double(x))
}

.mojor_apply_promotions <- function(args, promotions) {
    if (length(promotions) ==
        0) {
        return(args)
    }
    for (nm in names(promotions)) {
        if (!nm %in% names(args))
            next
        target <- promotions[[nm]]
        if (identical(target, "f64")) {
            args[[nm]] <- .mojor_promote_to_f64(args[[nm]])
        } else if (identical(target, "f32")) {
            args[[nm]] <- .mojor_promote_to_f32(args[[nm]])
        }
    }
    args
}

.mojor_signature_key <- function(types, opts) {
    parts <- c(
        paste(
            names(types),
            types, sep = "=", collapse = ";"
        ),
        paste(
            names(opts),
            opts, sep = "=", collapse = ";"
        )
    )
    paste(parts, collapse = "|")
}

.mojor_jit_fn_hash <- function(fn) {
    if (!is.function(fn)) {
        return(.mojor_hash_serialized(fn))
    }
    deparse_stable <- function(x) {
        paste(
            deparse(x, width.cutoff = 500L, control = c("keepInteger", "showAttributes")),
            collapse = "\n"
        )
    }
    env <- environment(fn)
    env_tag <- "closure"
    if (!is.null(env) &&
        is.environment(env) &&
        isNamespace(env)) {
        env_tag <- paste0("namespace:", getNamespaceName(env))
    } else if (!is.null(env) &&
        identical(env, baseenv())) {
        env_tag <- "baseenv"
    }
    .mojor_hash(
        paste(
            "formals", deparse_stable(formals(fn)),
            "body", deparse_stable(body(fn)),
            "env", env_tag, sep = "|"
        )
    )
}

.mojor_default_jit_cache_dir <- function() {
    tryCatch(
        tools::R_user_dir("mojor", "cache"),
        error = function(e) tempdir()
    )
}

.mojor_new_jit_dispatcher_state <- function(name) {
    state <- new.env(parent = emptyenv())
    state$name <- as.character(name[[1]])
    state$created_at <- as.POSIXct(Sys.time(), tz = "")
    state$stats <- list(
        calls_total = 0L,
        cache_hits_memory = 0L,
        cache_hits_disk = 0L,
        cache_misses_compile = 0L,
        cache_incompatible_skips = 0L,
        compile_count = 0L,
        compile_time_total_sec = 0,
        failed_compiles = 0L,
        signature_rejects = 0L,
        eager_compiles_attempted = 0L,
        eager_compiles_succeeded = 0L,
        api_compiles_attempted = 0L,
        api_compiles_succeeded = 0L,
        api_compiles_failed = 0L
    )
    state$signatures <- new.env(parent = emptyenv())
    state$declared_signatures <- list()
    state$normalize_signatures <- NULL
    state$resolve_compiled <- NULL
    state$last_error <- NULL
    state
}

.mojor_jit_signature_entry_init <- function(key_hash, signature_key, types, options) {
    list(
        key_hash = key_hash,
        signature_key = signature_key,
        types = as.list(types),
        options = as.list(options),
        calls = 0L,
        hits_memory = 0L,
        hits_disk = 0L,
        compiles = 0L,
        compile_time_sec = 0,
        failed_compiles = 0L,
        compiled_via = NULL,
        last_error = NULL,
        last_event = NULL,
        last_used_at = as.POSIXct(NA_real_, origin = "1970-01-01", tz = "")
    )
}

.mojor_jit_signature_get <- function(dispatcher_state, key_hash, signature_key, types, options) {
    sigs <- dispatcher_state$signatures
    if (!exists(key_hash, envir = sigs, inherits = FALSE)) {
        assign(
            key_hash,
            .mojor_jit_signature_entry_init(key_hash, signature_key, types, options),
            envir = sigs
        )
    }
    get(key_hash, envir = sigs, inherits = FALSE)
}

.mojor_jit_signature_put <- function(dispatcher_state, signature) {
    assign(signature$key_hash, signature, envir = dispatcher_state$signatures)
    invisible(TRUE)
}

.mojor_jit_dispatcher_note_call <- function(dispatcher_state, key_hash, signature_key, types, options) {
    stats <- dispatcher_state$stats
    stats$calls_total <- stats$calls_total + 1L
    dispatcher_state$stats <- stats
    sig <- .mojor_jit_signature_get(dispatcher_state, key_hash, signature_key, types, options)
    sig$calls <- sig$calls + 1L
    sig$last_used_at <- as.POSIXct(Sys.time(), tz = "")
    .mojor_jit_signature_put(dispatcher_state, sig)
    invisible(TRUE)
}

.mojor_jit_dispatcher_record_event <- function(
    dispatcher_state, key_hash, signature_key, types, options, event, compile_time_sec = 0,
    compiled_via = NULL
) {
    stats <- dispatcher_state$stats
    sig <- .mojor_jit_signature_get(dispatcher_state, key_hash, signature_key, types, options)
    if (identical(event, "memory_hit")) {
        stats$cache_hits_memory <- stats$cache_hits_memory + 1L
        sig$hits_memory <- sig$hits_memory + 1L
    } else if (identical(event, "disk_hit")) {
        stats$cache_hits_disk <- stats$cache_hits_disk + 1L
        sig$hits_disk <- sig$hits_disk + 1L
    } else if (identical(event, "compiled") || identical(event, "api_compiled")) {
        compile_time_sec <- suppressWarnings(as.double(compile_time_sec))
        if (length(compile_time_sec) !=
            1 || is.na(compile_time_sec) || compile_time_sec < 0) {
            compile_time_sec <- 0
        }
        stats$cache_misses_compile <- stats$cache_misses_compile + 1L
        stats$compile_count <- stats$compile_count + 1L
        stats$compile_time_total_sec <- stats$compile_time_total_sec + compile_time_sec
        sig$compiles <- sig$compiles + 1L
        sig$compile_time_sec <- sig$compile_time_sec + compile_time_sec
        if (!is.null(compiled_via)) {
            sig$compiled_via <- as.character(compiled_via[[1]])
        } else if (identical(event, "api_compiled")) {
            sig$compiled_via <- "api"
        }
    } else {
        stop(".mojor_jit_dispatcher_record_event: unknown event")
    }
    sig$last_event <- event
    sig$last_used_at <- as.POSIXct(Sys.time(), tz = "")
    dispatcher_state$stats <- stats
    .mojor_jit_signature_put(dispatcher_state, sig)
    invisible(TRUE)
}

.mojor_jit_error_payload <- function(message, key_hash, signature_key, stage) {
    list(
        message = as.character(message[[1]]),
        when = as.POSIXct(Sys.time(), tz = ""),
        key_hash = as.character(key_hash[[1]]),
        signature_key = as.character(signature_key[[1]]),
        stage = as.character(stage[[1]])
    )
}

.mojor_jit_dispatcher_record_error <- function(
    dispatcher_state, key_hash, signature_key, types, options, stage, message,
    count_failed_compile = TRUE
) {
    stats <- dispatcher_state$stats
    sig <- .mojor_jit_signature_get(dispatcher_state, key_hash, signature_key, types, options)
    payload <- .mojor_jit_error_payload(message, key_hash, signature_key, stage)
    if (isTRUE(count_failed_compile)) {
        stats$failed_compiles <- stats$failed_compiles + 1L
        sig$failed_compiles <- sig$failed_compiles + 1L
    }
    dispatcher_state$last_error <- payload
    sig$last_error <- payload
    sig$last_used_at <- as.POSIXct(Sys.time(), tz = "")
    dispatcher_state$stats <- stats
    .mojor_jit_signature_put(dispatcher_state, sig)
    invisible(payload)
}

.mojor_jit_dispatcher_note_cache_incompatible <- function(dispatcher_state) {
    stats <- dispatcher_state$stats
    stats$cache_incompatible_skips <- stats$cache_incompatible_skips + 1L
    dispatcher_state$stats <- stats
    invisible(TRUE)
}

.mojor_jit_dispatcher_note_eager_attempt <- function(dispatcher_state, success = FALSE) {
    stats <- dispatcher_state$stats
    stats$eager_compiles_attempted <- stats$eager_compiles_attempted + 1L
    if (isTRUE(success)) {
        stats$eager_compiles_succeeded <- stats$eager_compiles_succeeded + 1L
    }
    dispatcher_state$stats <- stats
    invisible(TRUE)
}

.mojor_jit_dispatcher_note_api_compile <- function(dispatcher_state, success = FALSE) {
    stats <- dispatcher_state$stats
    stats$api_compiles_attempted <- stats$api_compiles_attempted + 1L
    if (isTRUE(success)) {
        stats$api_compiles_succeeded <- stats$api_compiles_succeeded + 1L
    } else {
        stats$api_compiles_failed <- stats$api_compiles_failed + 1L
    }
    dispatcher_state$stats <- stats
    invisible(TRUE)
}

.mojor_jit_dispatcher_note_signature_reject <- function(
    dispatcher_state, key_hash, signature_key, types, options, message
) {
    stats <- dispatcher_state$stats
    stats$signature_rejects <- stats$signature_rejects + 1L
    sig <- .mojor_jit_signature_get(dispatcher_state, key_hash, signature_key, types, options)
    payload <- .mojor_jit_error_payload(message, key_hash, signature_key, "signature_reject")
    dispatcher_state$last_error <- payload
    sig$last_error <- payload
    sig$last_event <- "signature_reject"
    sig$last_used_at <- as.POSIXct(Sys.time(), tz = "")
    dispatcher_state$stats <- stats
    .mojor_jit_signature_put(dispatcher_state, sig)
    invisible(payload)
}

.mojor_jit_dispatcher_snapshot <- function(dispatcher_state) {
    if (is.null(dispatcher_state) ||
        !is.environment(dispatcher_state)) {
        stop("mojor_jit_info: jit_fn must be a function returned by mojor_jit/mojor_njit/mojor_vectorize")
    }
    if (is.null(dispatcher_state$stats) ||
        is.null(dispatcher_state$signatures) ||
        !is.environment(dispatcher_state$signatures)) {
        stop("mojor_jit_info: jit_fn has no dispatcher metadata")
    }
    stats <- dispatcher_state$stats
    signatures <- list()
    keys <- ls(dispatcher_state$signatures, all.names = TRUE)
    if (length(keys) >
        0) {
        signatures <- lapply(
            keys, function(key) {
                get(key, envir = dispatcher_state$signatures, inherits = FALSE)
            }
        )
        names(signatures) <- keys
    }
    list(
        name = dispatcher_state$name,
        created_at = dispatcher_state$created_at,
        stats = list(
            calls_total = as.integer(stats$calls_total),
            cache_hits_memory = as.integer(stats$cache_hits_memory),
            cache_hits_disk = as.integer(stats$cache_hits_disk),
            cache_misses_compile = as.integer(stats$cache_misses_compile),
            cache_incompatible_skips = as.integer(stats$cache_incompatible_skips),
            compile_count = as.integer(stats$compile_count),
            compile_time_total_sec = as.double(stats$compile_time_total_sec),
            failed_compiles = as.integer(stats$failed_compiles),
            signature_rejects = as.integer(stats$signature_rejects),
            eager_compiles_attempted = as.integer(stats$eager_compiles_attempted),
            eager_compiles_succeeded = as.integer(stats$eager_compiles_succeeded),
            api_compiles_attempted = as.integer(stats$api_compiles_attempted),
            api_compiles_succeeded = as.integer(stats$api_compiles_succeeded),
            api_compiles_failed = as.integer(stats$api_compiles_failed)
        ),
        last_error = dispatcher_state$last_error,
        signatures = signatures
    )
}

.mojor_jit_known_type_specs <- function() {
    if (!exists(".mojor_type_registry", inherits = TRUE)) {
        return(character(0))
    }
    specs <- names(.mojor_type_registry)
    specs[nzchar(specs)]
}

.mojor_jit_normalize_signature_one <- function(signature, arg_names) {
    if (is.character(signature)) {
        if (length(signature) !=
            1 || !nzchar(signature)) {
            stop("mojor_jit: signature strings must be non-empty scalar strings")
        }
        if (!grepl(":", signature, fixed = TRUE)) {
            stop("mojor_jit: signature strings must contain type annotations (e.g. x: f64[])")
        }
        signature <- .mojor_parse_signature(signature)$types
    }
    if (!is.list(signature)) {
        stop("mojor_jit: each signature must be a named list or a signature string")
    }
    if (is.null(names(signature)) ||
        any(!nzchar(names(signature)))) {
        stop("mojor_jit: each signature must provide named argument type specs")
    }
    unknown <- setdiff(names(signature), arg_names)
    if (length(unknown) >
        0) {
        stop(
            "mojor_jit: signature contains unknown argument(s): ",
            paste(unknown, collapse = ", ")
        )
    }
    missing <- setdiff(arg_names, names(signature))
    if (length(missing) >
        0) {
        stop(
            "mojor_jit: signature missing argument type(s): ",
            paste(missing, collapse = ", ")
        )
    }
    known_specs <- .mojor_jit_known_type_specs()
    out <- list()
    for (nm in arg_names) {
        spec <- signature[[nm]]
        if (!is.character(spec) ||
            length(spec) !=
                1 || !nzchar(spec)) {
            stop("mojor_jit: signature type spec for '", nm, "' must be a non-empty scalar string")
        }
        if (length(known_specs) >
            0 && !(spec %in% known_specs)) {
            stop("mojor_jit: unknown type spec for '", nm, "': ", spec)
        }
        out[[nm]] <- spec
    }
    out
}

.mojor_jit_signature_identity <- function(types) {
    paste(names(types), unlist(types, use.names = FALSE), sep = "=", collapse = ";")
}

.mojor_jit_signature_to_string <- function(types) {
    if (length(types) ==
        0) {
        return("()")
    }
    paste0(
        "(",
        paste0(names(types), ": ", unlist(types, use.names = FALSE), collapse = ", "),
        ")"
    )
}

.mojor_jit_validate_dispatcher_fn <- function(jit_fn, caller = "mojor_jit_info") {
    if (!is.function(jit_fn)) {
        stop(
            caller, ": jit_fn must be a function returned by mojor_jit/mojor_njit/mojor_vectorize"
        )
    }
    dispatcher_state <- attr(jit_fn, "mojor_jit_dispatcher_state", exact = TRUE)
    if (is.null(dispatcher_state) ||
        !is.environment(dispatcher_state)) {
        stop(
            caller, ": jit_fn must be a function returned by mojor_jit/mojor_njit/mojor_vectorize"
        )
    }
    dispatcher_state
}

.mojor_jit_signature_map_compiled <- function(dispatcher_state) {
    out <- list()
    keys <- ls(dispatcher_state$signatures, all.names = TRUE)
    if (length(keys) ==
        0) {
        return(out)
    }
    for (key_hash in keys) {
        entry <- get(key_hash, envir = dispatcher_state$signatures, inherits = FALSE)
        sig_key <- entry$signature_key
        if (!is.character(sig_key) ||
            length(sig_key) !=
                1 || !nzchar(sig_key)) {
            next
        }
        if (!sig_key %in% names(out)) {
            out[[sig_key]] <- as.list(entry$types)
        }
    }
    if (length(out) >
        0) {
        out <- out[order(names(out))]
    }
    out
}

.mojor_jit_signature_map_declared <- function(dispatcher_state) {
    declared <- dispatcher_state$declared_signatures
    if (is.null(declared) ||
        !is.list(declared) ||
        length(declared) ==
            0) {
        return(list())
    }
    if (is.null(names(declared))) {
        return(list())
    }
    declared <- declared[nzchar(names(declared))]
    if (length(declared) ==
        0) {
        return(list())
    }
    declared[order(names(declared))]
}

.mojor_jit_signature_map_union <- function(lhs, rhs) {
    out <- lhs
    for (key in names(rhs)) {
        if (!key %in% names(out)) {
            out[[key]] <- rhs[[key]]
        }
    }
    if (length(out) >
        0) {
        out <- out[order(names(out))]
    }
    out
}

.mojor_jit_signature_map_format <- function(signature_map, format = c("key", "string", "typed_list")) {
    format <- match.arg(format)
    if (length(signature_map) ==
        0) {
        if (identical(format, "typed_list")) {
            return(list())
        }
        return(character(0))
    }
    keys <- names(signature_map)
    if (identical(format, "key")) {
        return(keys)
    }
    if (identical(format, "string")) {
        return(vapply(signature_map, .mojor_jit_signature_to_string, character(1)))
    }
    signature_map
}

.mojor_jit_normalize_signatures <- function(signatures, arg_names) {
    if (is.null(signatures)) {
        return(list())
    }
    out <- list()
    if (is.character(signatures)) {
        if (length(signatures) ==
            0) {
            return(list())
        }
        out <- lapply(signatures, .mojor_jit_normalize_signature_one, arg_names = arg_names)
    } else if (is.list(signatures)) {
        if (length(signatures) ==
            0) {
            return(list())
        }
        single_named <- !is.null(names(signatures)) &&
            all(nzchar(names(signatures))) &&
            setequal(names(signatures), arg_names) &&
            all(vapply(
                signatures, function(x) is.character(x) &&
                    length(x) ==
                        1 && nzchar(x),
                logical(1)
            ))
        if (isTRUE(single_named)) {
            out <- list(.mojor_jit_normalize_signature_one(signatures, arg_names = arg_names))
        } else {
            out <- lapply(signatures, .mojor_jit_normalize_signature_one, arg_names = arg_names)
        }
    } else {
        stop("mojor_jit: signatures must be NULL, a character vector, or a list of signatures")
    }
    ids <- vapply(out, .mojor_jit_signature_identity, character(1))
    out[!duplicated(ids)]
}

mojor_jit <- function(
    fn, ..., name = "mojor_jit_kernel", elementwise = FALSE, elementwise_target = c("cpu", "gpu"),
    elementwise_size = NULL, elementwise_cpu = NULL, broadcast = c(
        "none", "scalar", "recycle", "recycle_warn", "broadcast_nd", "broadcast_nd_warn"
    ),
    disk_cache = NULL, cache_dir = NULL, signatures = NULL, eager = FALSE, strict_signatures = NULL,
    parallel = FALSE, object_mode = c("off", "fallback", "hybrid"),
    semantics = c("r", "raw"), fast_math = NULL, bounds_check = NULL
) {
    if (!is.function(fn))
        stop("mojor_jit: fn must be a function")
    elementwise_target <- match.arg(elementwise_target)
    broadcast <- match.arg(broadcast)
    object_mode <- match.arg(object_mode)
    semantics <- match.arg(semantics)
    if (!is.null(fast_math) &&
        (!is.logical(fast_math) ||
            length(fast_math) !=
                1 || is.na(fast_math))) {
        stop("mojor_jit: fast_math must be TRUE, FALSE, or NULL")
    }
    if (!is.null(bounds_check) &&
        (!is.logical(bounds_check) ||
            length(bounds_check) !=
                1 || is.na(bounds_check))) {
        stop("mojor_jit: bounds_check must be TRUE, FALSE, or NULL")
    }
    broadcast_nd <- broadcast %in% c("broadcast_nd", "broadcast_nd_warn")
    .mojor_state$current_broadcast_nd <- isTRUE(broadcast_nd)
    cache <- new.env(parent = emptyenv())
    arg_names <- names(formals(fn))
    if (is.null(arg_names)) {
        arg_names <- character(0)
    }
    jit_arg_names <- setdiff(arg_names, "...")
    dispatcher_state <- .mojor_new_jit_dispatcher_state(name)
    fn_hash <- .mojor_jit_fn_hash(fn)
    key_prefix <- paste(
        "fn", fn_hash, "ver", .mojor_state$transpile_version,
        sep = "|"
    )
    sig_ids_cache <- new.env(parent = emptyenv())
    opts <- list(
        elementwise = isTRUE(elementwise),
        elementwise_target = elementwise_target,
        elementwise_size = if (is.null(elementwise_size)) "" else as.character(elementwise_size),
        elementwise_cpu = if (is.null(elementwise_cpu)) "" else as.character(elementwise_cpu),
        broadcast = broadcast,
        parallel = isTRUE(parallel),
        object_mode = object_mode,
        semantics = semantics,
        fast_math = if (is.null(fast_math)) "auto" else as.character(isTRUE(fast_math)),
        bounds_check = if (is.null(bounds_check)) "auto" else as.character(isTRUE(bounds_check))
    )
    disk_cache_explicit <- !is.null(disk_cache)
    .mojor_jit_use_disk_cache <- function() {
        .mojor_effective_jit_disk_cache(
            if (isTRUE(disk_cache_explicit)) disk_cache else NULL,
            context = "mojor_jit"
        )
    }
    .mojor_jit_cache_dir <- function(use_disk_cache_now) {
        if (!isTRUE(use_disk_cache_now)) {
            return(NULL)
        }
        if (!is.null(cache_dir)) {
            return(cache_dir)
        }
        .mojor_default_jit_cache_dir()
    }
    eager_signatures <- .mojor_jit_normalize_signatures(signatures, jit_arg_names)
    if (!is.logical(eager) ||
        length(eager) !=
            1 || is.na(eager)) {
        stop("mojor_jit: eager must be TRUE or FALSE")
    }
    if (!is.null(strict_signatures) &&
        (!is.logical(strict_signatures) ||
            length(strict_signatures) !=
                1 || is.na(strict_signatures))) {
        stop("mojor_jit: strict_signatures must be TRUE, FALSE, or NULL")
    }
    strict_signatures_explicit <- !is.null(strict_signatures)
    .mojor_jit_use_strict_signatures <- function() {
        .mojor_effective_jit_strict_signatures(
            if (isTRUE(strict_signatures_explicit)) strict_signatures else NULL,
            context = "mojor_jit"
        )
    }
    declared_sig_keys <- unique(vapply(
        eager_signatures,
        function(types) .mojor_signature_key(types, opts),
        character(1)
    ))
    declared_sig_types <- list()
    if (length(eager_signatures) >
        0) {
        for (sig_types in eager_signatures) {
            key <- .mojor_signature_key(sig_types, opts)
            if (!key %in% names(declared_sig_types)) {
                declared_sig_types[[key]] <- as.list(sig_types)
            }
        }
        declared_sig_types <- declared_sig_types[order(names(declared_sig_types))]
        declared_sig_keys <- names(declared_sig_types)
    }
    declared_sig_set <- new.env(parent = emptyenv())
    if (length(declared_sig_keys) >
        0) {
        for (key in declared_sig_keys) {
            assign(key, TRUE, envir = declared_sig_set)
        }
    }
    .mojor_jit_is_declared_signature <- function(sig_key) {
        exists(sig_key, envir = declared_sig_set, inherits = FALSE)
    }
    if (isTRUE(.mojor_jit_use_strict_signatures()) &&
        length(declared_sig_keys) ==
            0) {
        stop("mojor_jit: strict_signatures=TRUE requires signatures")
    }

    .mojor_jit_signature_ids <- function(types) {
        sig_key <- .mojor_signature_key(types, opts)
        if (exists(sig_key, envir = sig_ids_cache, inherits = FALSE)) {
            return(get(sig_key, envir = sig_ids_cache, inherits = FALSE))
        }
        key_input <- paste(
            key_prefix, sig_key,
            sep = "|"
        )
        ids <- list(
            sig_key = sig_key,
            key_input = key_input,
            key_hash = .mojor_hash(key_input)
        )
        assign(sig_key, ids, envir = sig_ids_cache)
        ids
    }

    .mojor_jit_build_callable <- function(kernel_name, wrapper_so, types) {
        dll <- .mojor_dyn_load_with_recovery(
            so_path = wrapper_so,
            lib_dir = dirname(wrapper_so),
            retry_on_limit = TRUE
        )
        pkg <- dll[["name"]]
        kernel_call_symbol <- paste0(kernel_name, "_call")
        if (!is.null(pkg) &&
            nzchar(pkg)) {
            .mojor_register_loaded_wrapper(pkg, wrapper_so, kind = "jit")
        }
        call_kernel <- .mojor_make_package_scoped_call(
            symbol = kernel_call_symbol,
            so_path = wrapper_so,
            pkg = pkg,
            missing_message = paste0(
                "mojor_jit: shared library is not loaded: ",
                wrapper_so
            ),
            wrapper_kind = "jit",
            autoload = TRUE
        )
        function(...) {
            if (identical(
                .mojor_effective_na_mode(NULL),
                "forbid"
            )) {
                .mojor_check_no_na_args(
                    list(...),
                    types, arg_names
                )
            }
            call_kernel(...)
        }
    }

    .mojor_jit_resolve_compiled <- function(types, stage = c("runtime", "eager", "api")) {
        stage <- match.arg(stage, c("runtime", "eager", "api"))
        ids <- .mojor_jit_signature_ids(types)
        key_hash <- ids$key_hash
        sig_key <- ids$sig_key
        key_input <- ids$key_input
        if (exists(key_hash, envir = cache, inherits = FALSE)) {
            return(list(
                ok = TRUE,
                key_hash = key_hash,
                sig_key = sig_key,
                key_input = key_input,
                func = get(key_hash, envir = cache, inherits = FALSE),
                source = "memory"
            ))
        }
        use_disk_cache_now <- .mojor_jit_use_disk_cache()
        effective_cache_dir_now <- .mojor_jit_cache_dir(use_disk_cache_now)
        cache_compat_now <- if (isTRUE(use_disk_cache_now)) .mojor_jit_cache_compat_current() else NULL
        if (isTRUE(use_disk_cache_now)) {
            cache_index <- .mojor_jit_cache_read(effective_cache_dir_now)
            entry <- cache_index[[key_hash]]
            compat_check <- .mojor_jit_cache_compat_check(if (is.list(entry)) entry$compat else NULL, current = cache_compat_now)
            if (!isTRUE(compat_check$ok)) {
                .mojor_jit_dispatcher_note_cache_incompatible(dispatcher_state)
            } else {
                wrapper_so <- NULL
                kernel_name <- NULL
                if (is.list(entry)) {
                    if (is.character(entry$wrapper_so) &&
                        length(entry$wrapper_so) ==
                            1 && nzchar(entry$wrapper_so)) {
                        wrapper_so <- entry$wrapper_so
                    }
                    if (is.character(entry$kernel) &&
                        length(entry$kernel) ==
                            1 && nzchar(entry$kernel)) {
                        kernel_name <- entry$kernel
                    }
                }
                if (!is.null(wrapper_so) &&
                    !is.null(kernel_name) &&
                    isTRUE(file.exists(wrapper_so))) {
                    f <- .mojor_jit_build_callable(kernel_name, wrapper_so, types)
                    assign(key_hash, f, envir = cache)
                    return(list(
                        ok = TRUE,
                        key_hash = key_hash,
                        sig_key = sig_key,
                        key_input = key_input,
                        func = f,
                        source = "disk"
                    ))
                }
            }
        }
        compile_start <- proc.time()[["elapsed"]]
        build <- tryCatch(
            {
                do.call(
                    mojor_build, c(
                        list(
                            fn = fn, name = paste0(
                                .mojor_safe_name(name),
                                "_", substr(key_hash, 1, 8)
                            ),
                            elementwise = elementwise, elementwise_target = elementwise_target,
                            elementwise_size = elementwise_size, elementwise_cpu = elementwise_cpu,
                            broadcast = broadcast, object_mode = object_mode,
                            semantics = semantics, fast_math = fast_math, bounds_check = bounds_check,
                            cache = TRUE, cache_dir = effective_cache_dir_now
                        ),
                        types
                    )
                )
            }, error = function(e) e
        )
        if (inherits(build, "error")) {
            error_stage <- if (identical(stage, "eager")) {
                "eager_compile"
            } else if (identical(stage, "api")) {
                "api_compile"
            } else {
                "runtime_compile"
            }
            .mojor_jit_dispatcher_record_error(
                dispatcher_state = dispatcher_state, key_hash = key_hash, signature_key = sig_key,
                types = types, options = opts, stage = error_stage,
                message = build$message, count_failed_compile = TRUE
            )
            return(list(
                ok = FALSE,
                key_hash = key_hash,
                sig_key = sig_key,
                key_input = key_input,
                error = build
            ))
        }
        compile_elapsed <- proc.time()[["elapsed"]] - compile_start
        assign(key_hash, build$func, envir = cache)
        .mojor_jit_dispatcher_record_event(
            dispatcher_state, key_hash, sig_key, types, opts, if (identical(stage, "api")) "api_compiled" else "compiled",
            compile_time_sec = compile_elapsed,
            compiled_via = if (identical(stage, "eager")) {
                "eager"
            } else if (identical(stage, "api")) {
                "api"
            } else {
                "runtime"
            }
        )
        if (isTRUE(use_disk_cache_now)) {
            .mojor_jit_cache_write(
                effective_cache_dir_now, key_hash, list(
                    build_dir = build$build_dir,
                    kernel = build$kernel,
                    wrapper_so = build$wrapper_so,
                    fn_hash = fn_hash,
                    signature = key_input,
                    compat = cache_compat_now
                )
            )
            .mojor_jit_cache_enforce(effective_cache_dir_now)
        }
        list(
            ok = TRUE,
            key_hash = key_hash,
            sig_key = sig_key,
            key_input = key_input,
            func = build$func,
            source = "compiled"
        )
    }
    dispatcher_state$declared_signatures <- declared_sig_types
    dispatcher_state$normalize_signatures <- function(signature_specs) {
        .mojor_jit_normalize_signatures(signature_specs, jit_arg_names)
    }
    dispatcher_state$resolve_compiled <- function(types, stage = c("runtime", "eager", "api")) {
        .mojor_jit_resolve_compiled(types, stage = stage)
    }

    if (isTRUE(eager)) {
        if (length(eager_signatures) ==
            0) {
            stop("mojor_jit: eager=TRUE requires signatures")
        }
        for (sig_types in eager_signatures) {
            resolved <- .mojor_jit_resolve_compiled(sig_types, stage = "eager")
            .mojor_jit_dispatcher_note_eager_attempt(dispatcher_state, success = isTRUE(resolved$ok))
            if (!isTRUE(resolved$ok)) {
                stop(
                    "mojor_jit: eager compile failed for signature ",
                    resolved$sig_key, ": ", resolved$error$message
                )
            }
        }
    }

    out <- function(...) {
        args <- list(...)
        if (length(args) >
            0 && length(arg_names) >
            0) {
            if (is.null(names(args))) {
                names(args) <- arg_names[seq_along(args)]
            } else {
                blank <- names(args) ==
                  ""
                names(args)[blank] <- arg_names[seq_along(args)][blank]
            }
        }
        types <- .mojor_infer_types(args, arg_names, broadcast = broadcast)
        promotions <- attr(types, "promotions")
        args <- .mojor_apply_promotions(args, promotions)
        ids <- .mojor_jit_signature_ids(types)
        key_hash <- ids$key_hash
        sig_key <- ids$sig_key
        .mojor_jit_dispatcher_note_call(
            dispatcher_state, key_hash, sig_key, types, opts
        )
        strict_now <- .mojor_jit_use_strict_signatures()
        if (isTRUE(strict_now)) {
            declared_text <- if (length(declared_sig_keys) ==
                0) "<none>" else paste(declared_sig_keys, collapse = " || ")
            if (length(declared_sig_keys) ==
                0) {
                msg <- paste0(
                    "mojor_jit: strict_signatures enabled but no declared signatures are available. ",
                    "Inferred signature: ", sig_key
                )
                .mojor_jit_dispatcher_note_signature_reject(
                    dispatcher_state = dispatcher_state, key_hash = key_hash, signature_key = sig_key,
                    types = types, options = opts, message = msg
                )
                stop(msg)
            }
            if (!isTRUE(.mojor_jit_is_declared_signature(sig_key))) {
                msg <- paste0(
                    "mojor_jit: strict_signatures rejected runtime signature: ", sig_key,
                    ". Declared signatures: ", declared_text
                )
                .mojor_jit_dispatcher_note_signature_reject(
                    dispatcher_state = dispatcher_state, key_hash = key_hash, signature_key = sig_key,
                    types = types, options = opts, message = msg
                )
                stop(msg)
            }
        }
        resolved <- .mojor_jit_resolve_compiled(types, stage = "runtime")
        if (!isTRUE(resolved$ok)) {
            stop(resolved$error$message)
        }
        f <- resolved$func
        if (identical(resolved$source, "memory")) {
            .mojor_jit_dispatcher_record_event(
                dispatcher_state, key_hash, sig_key, types, opts, "memory_hit"
            )
        } else if (identical(resolved$source, "disk")) {
            .mojor_jit_dispatcher_record_event(
                dispatcher_state, key_hash, sig_key, types, opts, "disk_hit"
            )
        }
        out_val <- tryCatch(
            do.call(f, args),
            error = function(e) e
        )
        if (inherits(out_val, "error")) {
            .mojor_jit_dispatcher_record_error(
                dispatcher_state = dispatcher_state, key_hash = key_hash, signature_key = sig_key,
                types = types, options = opts, stage = "runtime_execute",
                message = out_val$message, count_failed_compile = FALSE
            )
            stop(out_val$message)
        }
        out_val
    }
    attr(out, "mojor_jit_dispatcher_state") <- dispatcher_state
    out
}

#' Nopython-style strict JIT wrapper
#'
#' @param fn Function to specialize and compile.
#' @param ... Optional runtime type hints passed through to `mojor_jit`.
#' @param name Kernel name prefix.
#' @param elementwise Enable elementwise lowering.
#' @param elementwise_target Elementwise target backend.
#' @param elementwise_size Optional elementwise size hint.
#' @param elementwise_cpu Optional CPU elementwise override.
#' @param broadcast Broadcast mode.
#' @param disk_cache Enable persistent cache index (`NULL` follows `mojor_options("jit_disk_cache")`).
#' @param cache_dir Cache directory.
#' @param signatures Optional declared signatures for eager precompile (named type-list signatures or signature strings).
#' @param eager If `TRUE`, compile all declared `signatures` at wrapper creation.
#' @param strict_signatures If `TRUE`, runtime signatures must match declared `signatures`.
#' @param parallel Enable parallel loop routing.
#' @param semantics Semantics mode forwarded to `mojor_jit`.
#' @param fast_math Fast-math toggle forwarded to `mojor_jit`.
#' @param bounds_check Bounds-check toggle forwarded to `mojor_jit`.
#' @return A callable runtime-specializing function.
#' @export
mojor_njit <- function(
    fn, ..., name = "mojor_njit_kernel", elementwise = FALSE, elementwise_target = c("cpu", "gpu"),
    elementwise_size = NULL, elementwise_cpu = NULL, broadcast = c(
        "none", "scalar", "recycle", "recycle_warn", "broadcast_nd", "broadcast_nd_warn"
    ),
    disk_cache = NULL, cache_dir = NULL, signatures = NULL, eager = FALSE, strict_signatures = NULL,
    parallel = FALSE, semantics = c("r", "raw"), fast_math = NULL, bounds_check = NULL
) {
    mojor_jit(
        fn = fn, ..., name = name, elementwise = elementwise, elementwise_target = elementwise_target,
        elementwise_size = elementwise_size, elementwise_cpu = elementwise_cpu,
        broadcast = broadcast, disk_cache = disk_cache, cache_dir = cache_dir,
        signatures = signatures, eager = eager, strict_signatures = strict_signatures, parallel = parallel,
        semantics = semantics, fast_math = fast_math, bounds_check = bounds_check, object_mode = "off"
    )
}

#' Elementwise vectorize wrapper
#'
#' @param fn Function to compile.
#' @param ... Optional runtime type hints passed through to `mojor_jit`.
#' @param name Kernel name prefix.
#' @param target Vectorize target backend.
#' @param broadcast Broadcast mode.
#' @param disk_cache Enable persistent cache index (`NULL` follows `mojor_options("jit_disk_cache")`).
#' @param cache_dir Cache directory.
#' @param signatures Optional declared signatures for eager precompile (named type-list signatures or signature strings).
#' @param eager If `TRUE`, compile all declared `signatures` at wrapper creation.
#' @param strict_signatures If `TRUE`, runtime signatures must match declared `signatures`.
#' @param parallel Enable parallel loop routing.
#' @param elementwise_size Optional elementwise size hint.
#' @param elementwise_cpu Optional CPU elementwise override.
#' @param semantics Semantics mode forwarded to `mojor_jit`.
#' @param fast_math Fast-math toggle forwarded to `mojor_jit`.
#' @param bounds_check Bounds-check toggle forwarded to `mojor_jit`.
#' @return A callable runtime-specializing function.
#' @export
mojor_vectorize <- function(
    fn, ..., name = "mojor_vectorize_kernel", target = c("cpu", "gpu"),
    broadcast = c(
        "none", "scalar", "recycle", "recycle_warn", "broadcast_nd", "broadcast_nd_warn"
    ),
    disk_cache = NULL, cache_dir = NULL, signatures = NULL, eager = FALSE, strict_signatures = NULL, parallel = FALSE, elementwise_size = NULL,
    elementwise_cpu = NULL, semantics = c("r", "raw"), fast_math = NULL, bounds_check = NULL
) {
    target <- match.arg(target)
    mojor_jit(
        fn = fn, ..., name = name, elementwise = TRUE, elementwise_target = target,
        elementwise_size = elementwise_size, elementwise_cpu = elementwise_cpu,
        broadcast = broadcast, disk_cache = disk_cache, cache_dir = cache_dir,
        signatures = signatures, eager = eager, strict_signatures = strict_signatures, parallel = parallel,
        semantics = semantics, fast_math = fast_math, bounds_check = bounds_check, object_mode = "off"
    )
}

#' Inspect runtime JIT dispatcher state
#'
#' @param jit_fn A function returned by `mojor_jit()`, `mojor_njit()`, or `mojor_vectorize()`.
#' @return Named list containing dispatcher metadata and per-signature runtime stats.
#' @export
mojor_jit_info <- function(jit_fn) {
    dispatcher_state <- .mojor_jit_validate_dispatcher_fn(jit_fn, caller = "mojor_jit_info")
    .mojor_jit_dispatcher_snapshot(dispatcher_state)
}

#' Explicitly compile JIT signatures
#'
#' @param jit_fn A function returned by `mojor_jit()`, `mojor_njit()`, or `mojor_vectorize()`.
#' @param signatures Signature declarations in named type-list form or signature-string form.
#' @param stop_on_error If `TRUE`, stop on first compile failure; if `FALSE`, continue and return aggregated results.
#' @return Named list with compile attempt counters and per-signature result records.
#' @export
mojor_jit_compile <- function(jit_fn, signatures, stop_on_error = TRUE) {
    dispatcher_state <- .mojor_jit_validate_dispatcher_fn(jit_fn, caller = "mojor_jit_compile")
    if (!is.logical(stop_on_error) ||
        length(stop_on_error) !=
            1 || is.na(stop_on_error)) {
        stop("mojor_jit_compile: stop_on_error must be TRUE or FALSE")
    }
    normalize <- dispatcher_state$normalize_signatures
    resolve_compiled <- dispatcher_state$resolve_compiled
    if (!is.function(normalize) ||
        !is.function(resolve_compiled)) {
        stop("mojor_jit_compile: jit_fn has no compile metadata")
    }
    normalized <- normalize(signatures)
    if (length(normalized) ==
        0) {
        stop("mojor_jit_compile: signatures must declare at least one signature")
    }
    results <- vector("list", length(normalized))
    succeeded <- 0L
    failed <- 0L
    for (i in seq_along(normalized)) {
        types <- normalized[[i]]
        resolved <- resolve_compiled(types, stage = "api")
        ok <- isTRUE(resolved$ok)
        .mojor_jit_dispatcher_note_api_compile(dispatcher_state, success = ok)
        if (ok) {
            succeeded <- succeeded + 1L
            results[[i]] <- list(
                ok = TRUE,
                key_hash = resolved$key_hash,
                signature_key = resolved$sig_key,
                source = resolved$source
            )
        } else {
            failed <- failed + 1L
            results[[i]] <- list(
                ok = FALSE,
                key_hash = resolved$key_hash,
                signature_key = resolved$sig_key,
                source = "error",
                message = resolved$error$message
            )
            if (isTRUE(stop_on_error)) {
                stop(
                    "mojor_jit_compile: compile failed for signature ",
                    resolved$sig_key, ": ", resolved$error$message
                )
            }
        }
    }
    if (length(results) >
        0) {
        names(results) <- vapply(results, function(x) x$signature_key, character(1))
    }
    list(
        attempted = as.integer(length(normalized)),
        succeeded = as.integer(succeeded),
        failed = as.integer(failed),
        results = results
    )
}

#' List JIT dispatcher signatures
#'
#' @param jit_fn A function returned by `mojor_jit()`, `mojor_njit()`, or `mojor_vectorize()`.
#' @param kind Signature set to return: compiled signatures, declared signatures, or their union.
#' @param format Output format for signatures (`key`, `string`, or `typed_list`).
#' @return Signature collection in the requested format and deterministic ordering.
#' @export
mojor_jit_signatures <- function(
    jit_fn,
    kind = c("compiled", "declared", "all"),
    format = c("key", "string", "typed_list")
) {
    dispatcher_state <- .mojor_jit_validate_dispatcher_fn(jit_fn, caller = "mojor_jit_signatures")
    kind <- match.arg(kind)
    format <- match.arg(format)
    compiled <- .mojor_jit_signature_map_compiled(dispatcher_state)
    declared <- .mojor_jit_signature_map_declared(dispatcher_state)
    selected <- if (identical(kind, "compiled")) {
        compiled
    } else if (identical(kind, "declared")) {
        declared
    } else {
        .mojor_jit_signature_map_union(declared, compiled)
    }
    .mojor_jit_signature_map_format(selected, format = format)
}

.mojor_guvectorize_signature_invalid <- function() {
    stop("mojor_guvectorize: signature must be a non-empty named list or signature string")
}

.mojor_guvectorize_split_signature_args <- function(args_str) {
    if (!is.character(args_str) || length(args_str) != 1L || is.na(args_str)) {
        .mojor_guvectorize_signature_invalid()
    }
    chars <- strsplit(args_str, "", fixed = TRUE)[[1L]]
    if (length(chars) == 0L) {
        return(character(0))
    }
    out <- character(0)
    cur <- character(0)
    depth <- 0L
    for (ch in chars) {
        if (identical(ch, "[")) {
            depth <- depth + 1L
            cur <- c(cur, ch)
            next
        }
        if (identical(ch, "]")) {
            depth <- depth - 1L
            if (depth < 0L) {
                .mojor_guvectorize_signature_invalid()
            }
            cur <- c(cur, ch)
            next
        }
        if (identical(ch, ",") && depth == 0L) {
            out <- c(out, trimws(paste0(cur, collapse = "")))
            cur <- character(0)
            next
        }
        cur <- c(cur, ch)
    }
    if (depth != 0L) {
        .mojor_guvectorize_signature_invalid()
    }
    c(out, trimws(paste0(cur, collapse = "")))
}

.mojor_guvectorize_parse_signature <- function(signature) {
    if (!is.character(signature) ||
        length(signature) != 1L ||
        is.na(signature) ||
        !nzchar(trimws(signature))) {
        .mojor_guvectorize_signature_invalid()
    }
    sig <- trimws(signature)
    sig <- sub("^fn\\s+", "", sig)
    if (grepl("->", sig, fixed = TRUE)) {
        parts <- strsplit(sig, "->", fixed = TRUE)[[1L]]
        if (length(parts) < 1L) {
            .mojor_guvectorize_signature_invalid()
        }
        sig <- trimws(parts[[1L]])
    }

    args_str <- NULL
    m <- regexec("^([A-Za-z][A-Za-z0-9_]*)\\s*\\((.*)\\)$", sig)
    mm <- regmatches(sig, m)[[1L]]
    if (length(mm) >= 3L) {
        args_str <- trimws(mm[[3L]])
    } else {
        m2 <- regexec("^\\((.*)\\)$", sig)
        mm2 <- regmatches(sig, m2)[[1L]]
        if (length(mm2) >= 2L) {
            args_str <- trimws(mm2[[2L]])
        } else if (grepl(":", sig, fixed = TRUE)) {
            args_str <- sig
        }
    }
    if (is.null(args_str)) {
        .mojor_guvectorize_signature_invalid()
    }
    if (!nzchar(args_str)) {
        return(list(types = list()))
    }

    parts <- .mojor_guvectorize_split_signature_args(args_str)
    types <- list()
    for (p in parts) {
        p <- trimws(p)
        if (!nzchar(p)) {
            next
        }
        sep <- regexpr(":", p, fixed = TRUE)[[1L]]
        if (sep < 1L) {
            .mojor_guvectorize_signature_invalid()
        }
        arg <- trimws(substr(p, 1L, sep - 1L))
        ty <- trimws(substr(p, sep + 1L, nchar(p)))
        if (!nzchar(arg) ||
            !grepl("^[A-Za-z][A-Za-z0-9_]*$", arg) ||
            !nzchar(ty) ||
            arg %in% names(types)) {
            .mojor_guvectorize_signature_invalid()
        }
        types[[arg]] <- ty
    }
    list(types = types)
}

.mojor_guvectorize_canonicalize_rank_spec <- function(spec) {
    if (!is.character(spec) || length(spec) != 1L || is.na(spec) || !nzchar(spec)) {
        return(spec)
    }
    compact <- gsub("[[:space:]]+", "", spec, perl = TRUE)
    m <- regexec("^([^\\[]+)\\[(.*)\\]$", compact, perl = TRUE)
    got <- regmatches(compact, m)[[1L]]
    if (length(got) < 3L) {
        return(compact)
    }
    base <- got[[2L]]
    inner <- got[[3L]]
    if (!nzchar(inner)) {
        return(paste0(base, "[]"))
    }
    if (grepl("^[,]+$", inner, perl = TRUE)) {
        rank <- nchar(inner) + 1L
        if (rank >= 3L) {
            return(paste0(base, "[", rank, "d]"))
        }
        return(compact)
    }
    if (grepl("^[0-9]+[dD]$", inner, perl = TRUE)) {
        rank <- suppressWarnings(as.integer(sub("[dD]$", "", inner, perl = TRUE)))
        if (!is.na(rank) && rank >= 1L) {
            return(paste0(base, "[", rank, "d]"))
        }
    }
    compact
}

.mojor_guvectorize_canonicalize_specs <- function(specs) {
    out <- specs
    for (nm in names(out)) {
        out[[nm]] <- .mojor_guvectorize_canonicalize_rank_spec(out[[nm]])
    }
    out
}

.mojor_as_signature_specs <- function(signature) {
    if (is.character(signature) &&
        length(signature) ==
            1 && grepl(":", signature, fixed = TRUE)) {
        parsed <- .mojor_guvectorize_parse_signature(signature)
        signature <- parsed$types
    }
    if (!is.list(signature)) {
        if (is.atomic(signature))
            signature <- as.list(signature)
    }
    if (!is.list(signature) ||
        length(signature) ==
            0) {
        stop(
            "mojor_guvectorize: signature must be a non-empty named list or signature string"
        )
    }
    if (is.null(names(signature)) ||
        any(!nzchar(names(signature)))) {
        stop("mojor_guvectorize: signature must provide named argument specs")
    }
    invalid <- names(signature)[!vapply(
        signature, function(x) is.character(x) &&
            length(x) ==
                1 && nzchar(x),
        logical(1)
    )]
    if (length(invalid) >
        0) {
        stop(
            "mojor_guvectorize: signature entries must be non-empty scalar strings: ",
            paste(invalid, collapse = ", ")
        )
    }
    .mojor_guvectorize_canonicalize_specs(signature)
}

.mojor_validate_guvectorize_single_output_shape <- function(value, expected) {
    expected <- as.integer(expected)
    actual_dim <- dim(value)
    if (is.null(actual_dim)) {
        if (length(expected) !=
            1L || length(value) != expected[[1L]]) {
            stop(
                "mojor_guvectorize: output_shape mismatch (expected length ",
                expected[[1L]], ", got ", length(value), ")"
            )
        }
        return(invisible(TRUE))
    }
    actual <- as.integer(actual_dim)
    if (!identical(actual, expected)) {
        stop(
            "mojor_guvectorize: output_shape mismatch (expected ",
            paste(expected, collapse = "x"), ", got ", paste(actual, collapse = "x"), ")"
        )
    }
    invisible(TRUE)
}

.mojor_validate_guvectorize_output_shape <- function(value, output_shape) {
    if (is.null(output_shape)) {
        return(invisible(TRUE))
    }
    if (is.list(output_shape)) {
        if (!is.list(value) || is.data.frame(value) || length(value) != length(output_shape)) {
            stop("mojor_guvectorize: output_shape mismatch")
        }
        for (i in seq_along(output_shape)) {
            .mojor_validate_guvectorize_single_output_shape(value[[i]], output_shape[[i]])
        }
        return(invisible(TRUE))
    }
    .mojor_validate_guvectorize_single_output_shape(value, output_shape)
}

.mojor_guvectorize_normalize_output_shape <- function(output_shape, output_arity) {
    if (is.null(output_shape)) {
        return(NULL)
    }
    if (output_arity <=
        1L) {
        out_shape <- as.integer(output_shape)
        if (length(out_shape) ==
            0L || any(is.na(out_shape)) ||
            any(out_shape < 1L)) {
            stop("mojor_guvectorize: output_shape must contain positive integers")
        }
        return(out_shape)
    }
    if (!is.list(output_shape) || length(output_shape) != output_arity) {
        stop("mojor_guvectorize: output_shape for multi-output must be a list of length ", output_arity)
    }
    lapply(
        output_shape,
        function(one_shape) {
            out_shape <- as.integer(one_shape)
            if (length(out_shape) ==
                0L || any(is.na(out_shape)) ||
                any(out_shape < 1L)) {
                stop("mojor_guvectorize: output_shape must contain positive integers")
            }
            out_shape
        }
    )
}

.mojor_guvectorize_core_dims_invalid <- function() {
    stop("mojor_guvectorize: core_dims must be a non-empty string like '(n),(n)->(n)'")
}

.mojor_guvectorize_core_dim_is_literal <- function(token) {
    is.character(token) &&
        length(token) == 1L &&
        !is.na(token) &&
        grepl("^[1-9][0-9]*$", token)
}

.mojor_guvectorize_core_dim_literal_value <- function(token) {
    if (!.mojor_guvectorize_core_dim_is_literal(token)) {
        return(NULL)
    }
    as.integer(token)
}

.mojor_parse_guvectorize_core_dims_side <- function(side, allow_empty = FALSE) {
    side <- trimws(side)
    if (!nzchar(side)) {
        if (isTRUE(allow_empty)) {
            return(list())
        }
        .mojor_guvectorize_core_dims_invalid()
    }
    tuples <- regmatches(side, gregexpr("\\([^()]*\\)", side, perl = TRUE))[[1]]
    rem <- gsub("\\([^()]*\\)", "", side, perl = TRUE)
    rem <- gsub("[[:space:],]", "", rem, perl = TRUE)
    if (nzchar(rem)) {
        .mojor_guvectorize_core_dims_invalid()
    }
    if (length(tuples) == 0L) {
        if (isTRUE(allow_empty)) {
            return(list())
        }
        .mojor_guvectorize_core_dims_invalid()
    }
    parsed <- lapply(
        tuples,
        function(tok) {
            inner <- trimws(sub("\\)$", "", sub("^\\(", "", tok)))
            if (!nzchar(inner)) {
                return(character(0))
            }
            dims <- trimws(strsplit(inner, ",", fixed = TRUE)[[1]])
            if (length(dims) == 0L || any(!nzchar(dims))) {
                .mojor_guvectorize_core_dims_invalid()
            }
            bad <- dims[!grepl("^([A-Za-z_][A-Za-z0-9_]*|[1-9][0-9]*)$", dims)]
            if (length(bad) > 0L) {
                .mojor_guvectorize_core_dims_invalid()
            }
            dims
        }
    )
    parsed
}

.mojor_parse_guvectorize_core_dims <- function(core_dims) {
    if (!is.character(core_dims) || length(core_dims) != 1L || is.na(core_dims)) {
        .mojor_guvectorize_core_dims_invalid()
    }
    text <- trimws(core_dims)
    if (!nzchar(text)) {
        .mojor_guvectorize_core_dims_invalid()
    }
    arrow_locs <- gregexpr("->", text, fixed = TRUE)[[1]]
    if (length(arrow_locs) != 1L || arrow_locs[[1L]] < 0L) {
        .mojor_guvectorize_core_dims_invalid()
    }
    if (length(arrow_locs) > 1L) {
        .mojor_guvectorize_core_dims_invalid()
    }
    cut <- arrow_locs[[1L]]
    lhs <- if (cut > 1L) substr(text, 1L, cut - 1L) else ""
    rhs <- if ((cut + 2L) <= nchar(text)) substr(text, cut + 2L, nchar(text)) else ""
    inputs <- .mojor_parse_guvectorize_core_dims_side(lhs, allow_empty = TRUE)
    outputs <- .mojor_parse_guvectorize_core_dims_side(rhs, allow_empty = TRUE)
    if (length(inputs) < 1L || length(outputs) < 1L) {
        stop("mojor_guvectorize: core_dims must declare at least one input tuple and at least one output tuple")
    }
    list(
        raw = text,
        inputs = inputs,
        outputs = outputs,
        output = if (length(outputs) == 1L) outputs[[1L]] else NULL
    )
}

.mojor_guvectorize_type_rank <- function(spec) {
    if (!is.character(spec) || length(spec) != 1L || !nzchar(spec)) {
        return(NA_integer_)
    }
    compact <- gsub("[[:space:]]+", "", spec, perl = TRUE)
    if (!grepl("[", compact, fixed = TRUE)) {
        return(0L)
    }
    match <- regexec("^([^\\[]+)\\[(.*)\\]$", compact, perl = TRUE)
    got <- regmatches(compact, match)[[1L]]
    if (length(got) < 3L) {
        return(NA_integer_)
    }
    inner <- got[[3L]]
    if (!nzchar(inner)) {
        return(1L)
    }
    if (grepl("^[0-9]+[dD]$", inner, perl = TRUE)) {
        rank <- suppressWarnings(as.integer(sub("[dD]$", "", inner, perl = TRUE)))
        if (is.na(rank) || rank < 1L) {
            return(NA_integer_)
        }
        return(rank)
    }
    if (grepl("^[,]+$", inner, perl = TRUE)) {
        return(nchar(inner) + 1L)
    }
    NA_integer_
}

.mojor_guvectorize_core_dims_max_rank <- function(parsed) {
    if (is.null(parsed)) {
        return(0L)
    }
    tuples <- c(parsed$inputs, parsed$outputs)
    if (length(tuples) == 0L) {
        return(0L)
    }
    max(vapply(tuples, length, integer(1)))
}

.mojor_validate_guvectorize_core_dims <- function(parsed, specs) {
    arg_names <- names(specs)
    if (length(parsed$inputs) != length(arg_names)) {
        stop("mojor_guvectorize: core_dims input tuple count must match signature arguments")
    }
    for (i in seq_along(arg_names)) {
        nm <- arg_names[[i]]
        spec_rank <- .mojor_guvectorize_type_rank(specs[[nm]])
        if (is.na(spec_rank)) {
            stop("mojor_guvectorize: invalid rank syntax in signature spec for '", nm, "'")
        }
        if (spec_rank != length(parsed$inputs[[i]])) {
            stop("mojor_guvectorize: core_dims/input rank mismatch for '", nm, "'")
        }
    }
    in_syms <- unique(unlist(parsed$inputs, use.names = FALSE))
    in_syms <- in_syms[
        !vapply(
            in_syms,
            .mojor_guvectorize_core_dim_is_literal,
            logical(1)
        )
    ]
    for (out_dims in parsed$outputs) {
        for (d in out_dims) {
            if (.mojor_guvectorize_core_dim_is_literal(d)) {
                next
            }
            if (!d %in% in_syms) {
                stop("mojor_guvectorize: output core dimension '", d, "' is not bound by inputs")
            }
        }
    }
    parsed
}

.mojor_bind_guvectorize_args <- function(args, arg_names) {
    if (is.null(names(args))) {
        names(args) <- rep.int("", length(args))
    }
    if (length(args) > length(arg_names)) {
        stop("mojor_guvectorize: too many arguments")
    }
    blank <- !nzchar(names(args))
    if (any(blank)) {
        idx <- which(blank)
        if (max(idx) > length(arg_names)) {
            stop("mojor_guvectorize: too many arguments")
        }
        names(args)[idx] <- arg_names[idx]
    }
    if (anyDuplicated(names(args))) {
        stop("mojor_guvectorize: duplicate argument names in call")
    }
    unknown <- setdiff(names(args), arg_names)
    if (length(unknown) > 0L) {
        stop("mojor_guvectorize: unknown argument(s): ", paste(unknown, collapse = ", "))
    }
    missing <- setdiff(arg_names, names(args))
    if (length(missing) > 0L) {
        stop("mojor_guvectorize: missing argument(s): ", paste(missing, collapse = ", "))
    }
    args[arg_names]
}

.mojor_guvectorize_runtime_shape <- function(value, core_rank, arg_name) {
    dims <- dim(value)
    if (is.null(dims)) {
        if (core_rank == 0L && length(value) == 1L) {
            return(integer(0))
        }
        shape <- as.integer(length(value))
    } else {
        shape <- as.integer(dims)
    }
    if (any(is.na(shape)) || any(shape < 0L) || length(shape) < core_rank) {
        stop("mojor_guvectorize: core_dims/input rank mismatch for '", arg_name, "'")
    }
    shape
}

.mojor_guvectorize_broadcast_shape <- function(shapes) {
    if (length(shapes) == 0L) {
        return(integer(0))
    }
    rank <- max(vapply(shapes, length, integer(1)))
    out <- rep.int(1L, rank)
    for (shape in shapes) {
        if (length(shape) == 0L) {
            next
        }
        pad <- c(rep.int(1L, rank - length(shape)), as.integer(shape))
        for (i in seq_len(rank)) {
            cur <- out[[i]]
            nxt <- pad[[i]]
            if (cur == nxt) {
                next
            }
            if (cur == 1L) {
                out[[i]] <- nxt
                next
            }
            if (nxt == 1L) {
                next
            }
            stop("mojor_guvectorize: batch dimensions are not broadcast-compatible")
        }
    }
    out
}

.mojor_guvectorize_bind_core_sizes <- function(arg_shapes, parsed, arg_names) {
    sizes <- list()
    for (i in seq_along(arg_names)) {
        nm <- arg_names[[i]]
        dims <- parsed$inputs[[i]]
        if (length(dims) == 0L) {
            next
        }
        shape <- arg_shapes[[nm]]
        core_part <- tail(shape, length(dims))
        for (j in seq_along(dims)) {
            key <- dims[[j]]
            val <- as.integer(core_part[[j]])
            literal_val <- .mojor_guvectorize_core_dim_literal_value(key)
            if (!is.null(literal_val)) {
                if (!identical(as.integer(literal_val), val)) {
                    stop(
                        "mojor_guvectorize: core dimension '", key, "' mismatch (expected ",
                        as.integer(literal_val), ", got ", val, ")"
                    )
                }
                next
            }
            cur <- sizes[[key]]
            if (is.null(cur)) {
                sizes[[key]] <- val
            } else if (!identical(as.integer(cur), val)) {
                stop(
                    "mojor_guvectorize: core dimension '", key, "' mismatch (expected ",
                    as.integer(cur), ", got ", val, ")"
                )
            }
        }
    }
    sizes
}

.mojor_guvectorize_output_core_shapes <- function(parsed, core_sizes) {
    lapply(
        parsed$outputs,
        function(out_dims) {
            out <- integer(length(out_dims))
            for (i in seq_along(out_dims)) {
                key <- out_dims[[i]]
                literal_val <- .mojor_guvectorize_core_dim_literal_value(key)
                if (!is.null(literal_val)) {
                    out[[i]] <- as.integer(literal_val)
                    next
                }
                val <- core_sizes[[key]]
                if (is.null(val)) {
                    stop("mojor_guvectorize: output core dimension '", key, "' is not bound by inputs")
                }
                out[[i]] <- as.integer(val)
            }
            out
        }
    )
}

.mojor_guvectorize_align_batch_index <- function(batch_idx, outer_shape, batch_rank) {
    outer_rank <- length(outer_shape)
    if (outer_rank == 0L) {
        return(integer(0))
    }
    out <- integer(outer_rank)
    start <- batch_rank - outer_rank
    for (i in seq_len(outer_rank)) {
        pos <- start + i
        pick <- if (outer_shape[[i]] == 1L) 1L else as.integer(batch_idx[[pos]])
        out[[i]] <- pick
    }
    out
}

.mojor_guvectorize_slice_core <- function(value, shape, outer_idx, core_shape, core_rank, arg_name) {
    if (core_rank == 0L) {
        if (length(shape) == 0L) {
            return(value[[1L]])
        }
        if (is.null(dim(value))) {
            if (length(outer_idx) == 0L) {
                return(value[[1L]])
            }
            return(value[[outer_idx[[1L]]]])
        }
        idx <- as.list(outer_idx)
        picked <- if (length(idx) == 0L) value else do.call(`[`, c(list(value), idx, list(drop = TRUE)))
        if (length(picked) != 1L) {
            stop("mojor_guvectorize: core_dims/input rank mismatch for '", arg_name, "'")
        }
        return(picked[[1L]])
    }
    if (length(outer_idx) == 0L) {
        core <- value
    } else {
        core_idx <- c(
            as.list(outer_idx),
            replicate(core_rank, TRUE, simplify = FALSE),
            list(drop = FALSE)
        )
        core <- do.call(`[`, c(list(value), core_idx))
    }
    if (length(core) != prod(core_shape)) {
        stop("mojor_guvectorize: core_dims/input rank mismatch for '", arg_name, "'")
    }
    if (core_rank == 1L) {
        return(as.vector(core))
    }
    array(core, dim = core_shape)
}

.mojor_guvectorize_validate_core_output <- function(value, core_shape, output_index = NULL) {
    shape_err <- function() {
        if (is.null(output_index)) {
            stop("mojor_guvectorize: output core shape mismatch")
        }
        stop("mojor_guvectorize: output #", as.integer(output_index), " core shape mismatch")
    }
    rank <- length(core_shape)
    if (rank == 0L) {
        if (length(value) != 1L) {
            shape_err()
        }
        return(value[[1L]])
    }
    if (rank == 1L) {
        if (length(value) != core_shape[[1L]]) {
            shape_err()
        }
        return(as.vector(value))
    }
    actual <- dim(value)
    if (is.null(actual) || !identical(as.integer(actual), as.integer(core_shape))) {
        if (length(value) != prod(core_shape)) {
            shape_err()
        }
    }
    array(value, dim = core_shape)
}

.mojor_guvectorize_alloc_output <- function(first_value, full_shape) {
    if (length(full_shape) == 0L) {
        return(first_value)
    }
    fill <- if (is.logical(first_value)) {
        FALSE
    } else if (is.integer(first_value)) {
        0L
    } else {
        0
    }
    array(fill, dim = full_shape)
}

.mojor_guvectorize_assign_output <- function(out, batch_idx, core_value, core_rank) {
    idx <- as.list(batch_idx)
    if (core_rank > 0L) {
        idx <- c(idx, replicate(core_rank, TRUE, simplify = FALSE))
    }
    do.call(`[<-`, c(list(out), idx, list(value = core_value)))
}

.mojor_guvectorize_core_dims_cache_key <- function(parsed) {
    if (is.null(parsed)) {
        return("none")
    }
    fmt_side <- function(side) {
        paste(vapply(side, function(one) paste0("(", paste(one, collapse = ","), ")"), character(1)), collapse = ",")
    }
    paste0(fmt_side(parsed$inputs), "->", fmt_side(parsed$outputs))
}

.mojor_guvectorize_reduce_values <- function(values, op) {
    if (identical(op, "sum")) {
        return(sum(values))
    }
    if (identical(op, "mean")) {
        return(mean(values))
    }
    if (identical(op, "min")) {
        return(min(values))
    }
    if (identical(op, "max")) {
        return(max(values))
    }
    stop("mojor_guvectorize: core_dims rank-n reduction op is not supported in this release")
}

.mojor_guvectorize_reduce_core_value <- function(
    value,
    core_shape,
    keep_axes,
    op
) {
    rank <- length(core_shape)
    if (rank == 0L) {
        return(value[[1L]])
    }
    expected_len <- as.integer(prod(core_shape))
    if (length(value) != expected_len) {
        stop("mojor_guvectorize: output core shape mismatch")
    }
    core_arr <- array(value, dim = core_shape)
    if (is.null(op) || length(keep_axes) == rank) {
        return(core_arr)
    }
    if (length(keep_axes) == 0L) {
        return(.mojor_guvectorize_reduce_values(as.vector(core_arr), op))
    }
    apply(core_arr, MARGIN = keep_axes, FUN = function(v) .mojor_guvectorize_reduce_values(v, op))
}

.mojor_make_guvectorize_core_dims_wrapper <- function(
    core_fns,
    specs,
    parsed,
    output_shape,
    output_names = NULL,
    engine = "direct_core",
    vector_arg_names = NULL,
    strict_has_indexed = FALSE,
    reduction_outputs = NULL,
    reduction_primary_tuple = NULL,
    target = "cpu"
) {
    arg_names <- names(specs)
    if (is.function(core_fns)) {
        core_fns <- list(core_fns)
    }
    output_arity <- length(core_fns)
    if (output_arity != length(parsed$outputs)) {
        stop("mojor_guvectorize: core_dims multi-output return length mismatch (expected ", length(parsed$outputs), ", got ", output_arity, ")")
    }
    if (is.null(output_names) || length(output_names) != output_arity) {
        output_names <- paste0("output", seq_len(output_arity))
    }
    output_names <- as.character(output_names)
    output_names[!nzchar(output_names)] <- paste0("output", which(!nzchar(output_names)))

    force(core_fns)
    force(specs)
    force(parsed)
    force(output_shape)
    force(output_names)
    force(engine)
    force(vector_arg_names)
    force(strict_has_indexed)
    force(reduction_outputs)
    force(reduction_primary_tuple)
    force(target)
    compiled_batch_cache <- new.env(parent = emptyenv())
    compiled_batch_cache$key <- NULL
    function(...) {
        args <- .mojor_bind_guvectorize_args(list(...), arg_names)

        arg_shapes <- setNames(vector("list", length(arg_names)), arg_names)
        outer_shapes <- setNames(vector("list", length(arg_names)), arg_names)
        for (i in seq_along(arg_names)) {
            nm <- arg_names[[i]]
            core_rank <- length(parsed$inputs[[i]])
            shape <- .mojor_guvectorize_runtime_shape(args[[nm]], core_rank, nm)
            arg_shapes[[nm]] <- shape
            if (core_rank == 0L) {
                outer_shapes[[nm]] <- shape
            } else {
                outer_n <- length(shape) - core_rank
                outer_shapes[[nm]] <- if (outer_n > 0L) shape[seq_len(outer_n)] else integer(0)
            }
        }

        core_sizes <- .mojor_guvectorize_bind_core_sizes(arg_shapes, parsed, arg_names)
        out_core_shapes <- .mojor_guvectorize_output_core_shapes(parsed, core_sizes)
        has_reduction <- !is.null(reduction_outputs)
        reduction_core_shape <- NULL
        if (isTRUE(has_reduction)) {
            if (is.null(reduction_primary_tuple) || length(reduction_primary_tuple) == 0L) {
                stop("mojor_guvectorize: core_dims rank-n reduction/output tuple contract is not satisfiable")
            }
            reduction_core_shape <- as.integer(
                vapply(
                    reduction_primary_tuple,
                    function(d) {
                        lit <- .mojor_guvectorize_core_dim_literal_value(d)
                        if (!is.null(lit)) {
                            return(as.integer(lit))
                        }
                        core_sizes[[d]]
                    },
                    integer(1)
                )
            )
        }
        batch_shape <- .mojor_guvectorize_broadcast_shape(unname(outer_shapes))
        batch_rank <- length(batch_shape)
        n_batch <- if (batch_rank == 0L) 1L else as.integer(prod(batch_shape))

        if (n_batch == 0L) {
            empty_values <- lapply(
                out_core_shapes,
                function(one_shape) array(numeric(0), dim = c(batch_shape, one_shape))
            )
            value <- if (output_arity == 1L) empty_values[[1L]] else empty_values
            if (output_arity > 1L) {
                names(value) <- output_names
            }
            .mojor_validate_guvectorize_output_shape(value, output_shape)
            return(value)
        }

        use_compiled_batch <- FALSE
        compiled_batch_mode <- "r_loop"
        compiled_batch_cache_hit <- FALSE
        if (identical(engine, "strict_elementwise") &&
            !isTRUE(strict_has_indexed) &&
            batch_rank > 0L &&
            !is.null(vector_arg_names) &&
            length(vector_arg_names) > 0L) {
            can_compile <- TRUE
            for (i in seq_along(arg_names)) {
                nm <- arg_names[[i]]
                core_rank_i <- length(parsed$inputs[[i]])
                if (core_rank_i > 0L) {
                    if (!identical(outer_shapes[[nm]], batch_shape)) {
                        can_compile <- FALSE
                        break
                    }
                } else if (length(args[[nm]]) != 1L) {
                    can_compile <- FALSE
                    break
                }
            }
            if (can_compile) {
                compiled_batch_mode <- if (isTRUE(has_reduction)) "strict_reduction_batch" else "strict_elementwise_batch"
                sig_key <- paste(vapply(arg_names, function(nm) paste0(nm, ":", specs[[nm]]), character(1)), collapse = ",")
                core_dims_key <- .mojor_guvectorize_core_dims_cache_key(parsed)
                cache_key <- paste(
                    c(
                        paste0("target=", target),
                        paste0("engine=", engine),
                        paste0("mode=", compiled_batch_mode),
                        paste0("arity=", output_arity),
                        paste0("batch_rank=", batch_rank),
                        paste0("n_batch=", n_batch),
                        paste0("signature=", sig_key),
                        paste0("core_dims=", core_dims_key),
                        batch_shape,
                        unlist(out_core_shapes),
                        vapply(arg_names, function(nm) paste(arg_shapes[[nm]], collapse = "x"), character(1))
                    ),
                    collapse = "|"
                )
                compiled_batch_cache_hit <- identical(compiled_batch_cache$key, cache_key)
                compiled_batch_cache$key <- cache_key
                use_compiled_batch <- TRUE
            }
        }

        if (use_compiled_batch) {
            eval_args <- args
            for (nm in vector_arg_names) {
                eval_args[[nm]] <- as.vector(args[[nm]])
            }
            core_values <- vector("list", output_arity)
            for (k in seq_len(output_arity)) {
                core_values[[k]] <- do.call(core_fns[[k]], eval_args)
            }
            value <- NULL
            if (!isTRUE(has_reduction)) {
                value <- if (output_arity == 1L) {
                    full_shape <- c(batch_shape, out_core_shapes[[1L]])
                    expected_len <- if (length(full_shape) == 0L) 1L else as.integer(prod(full_shape))
                    if (length(core_values[[1L]]) != expected_len) {
                        stop("mojor_guvectorize: output core shape mismatch")
                    }
                    array(core_values[[1L]], dim = full_shape)
                } else {
                    out_list <- vector("list", output_arity)
                    for (k in seq_len(output_arity)) {
                        full_shape <- c(batch_shape, out_core_shapes[[k]])
                        expected_len <- if (length(full_shape) == 0L) 1L else as.integer(prod(full_shape))
                        if (length(core_values[[k]]) != expected_len) {
                            stop("mojor_guvectorize: output #", as.integer(k), " core shape mismatch")
                        }
                        out_list[[k]] <- array(core_values[[k]], dim = full_shape)
                    }
                    names(out_list) <- output_names
                    out_list
                }
            } else {
                out_reduced <- vector("list", output_arity)
                batch_idx_mat <- arrayInd(seq_len(n_batch), .dim = batch_shape)
                for (k in seq_len(output_arity)) {
                    full_shape <- c(batch_shape, reduction_core_shape)
                    expected_len <- if (length(full_shape) == 0L) 1L else as.integer(prod(full_shape))
                    if (length(core_values[[k]]) != expected_len) {
                        if (output_arity > 1L) {
                            stop("mojor_guvectorize: output #", as.integer(k), " core shape mismatch")
                        }
                        stop("mojor_guvectorize: output core shape mismatch")
                    }
                    core_arr <- array(core_values[[k]], dim = full_shape)
                    for (row_i in seq_len(nrow(batch_idx_mat))) {
                        batch_idx <- as.integer(batch_idx_mat[row_i, ])
                        core_slice <- .mojor_guvectorize_slice_core(
                            value = core_arr,
                            shape = full_shape,
                            outer_idx = batch_idx,
                            core_shape = reduction_core_shape,
                            core_rank = length(reduction_core_shape),
                            arg_name = "core"
                        )
                        reduced <- .mojor_guvectorize_reduce_core_value(
                            value = core_slice,
                            core_shape = reduction_core_shape,
                            keep_axes = reduction_outputs[[k]]$keep_axes,
                            op = reduction_outputs[[k]]$op
                        )
                        reduced <- .mojor_guvectorize_validate_core_output(
                            reduced,
                            out_core_shapes[[k]],
                            if (output_arity > 1L) k else NULL
                        )
                        if (row_i == 1L) {
                            out_reduced[[k]] <- .mojor_guvectorize_alloc_output(
                                reduced,
                                c(batch_shape, out_core_shapes[[k]])
                            )
                        }
                        out_reduced[[k]] <- .mojor_guvectorize_assign_output(
                            out_reduced[[k]],
                            batch_idx,
                            reduced,
                            length(out_core_shapes[[k]])
                        )
                    }
                }
                value <- if (output_arity == 1L) out_reduced[[1L]] else out_reduced
                if (output_arity > 1L) {
                    names(value) <- output_names
                }
            }
            .mojor_validate_guvectorize_output_shape(value, output_shape)
            attr(compiled_batch_cache, "last_hit") <- isTRUE(compiled_batch_cache_hit)
            return(value)
        }

        batch_idx_mat <- if (batch_rank == 0L) {
            matrix(integer(0), nrow = 1L, ncol = 0L)
        } else {
            arrayInd(seq_len(n_batch), .dim = batch_shape)
        }

        out <- NULL
        single <- NULL
        for (row_i in seq_len(nrow(batch_idx_mat))) {
            batch_idx <- if (batch_rank == 0L) integer(0) else as.integer(batch_idx_mat[row_i, ])
            call_args <- setNames(vector("list", length(arg_names)), arg_names)
            for (i in seq_along(arg_names)) {
                nm <- arg_names[[i]]
                arg_outer <- outer_shapes[[nm]]
                arg_batch_idx <- .mojor_guvectorize_align_batch_index(batch_idx, arg_outer, batch_rank)
                call_args[[nm]] <- .mojor_guvectorize_slice_core(
                    args[[nm]],
                    arg_shapes[[nm]],
                    arg_batch_idx,
                    if (length(parsed$inputs[[i]]) > 0L) {
                        as.integer(
                            vapply(
                                parsed$inputs[[i]],
                                function(d) {
                                    lit <- .mojor_guvectorize_core_dim_literal_value(d)
                                    if (!is.null(lit)) {
                                        return(as.integer(lit))
                                    }
                                    core_sizes[[d]]
                                },
                                integer(1)
                            )
                        )
                    } else {
                        integer(0)
                    },
                    length(parsed$inputs[[i]]),
                    nm
                )
            }
            eval_args <- call_args
            if (identical(engine, "strict_elementwise")) {
                if (is.null(vector_arg_names) || length(vector_arg_names) == 0L) {
                    stop("mojor_guvectorize: core_dims rank-n strict indexed lane does not support this function body")
                }
                if (!isTRUE(strict_has_indexed)) {
                    eval_args <- call_args
                    for (nm in vector_arg_names) {
                        eval_args[[nm]] <- as.vector(call_args[[nm]])
                    }
                }
            }
            core_values <- vector("list", output_arity)
            for (k in seq_len(output_arity)) {
                core_val <- do.call(core_fns[[k]], eval_args)
                if (isTRUE(has_reduction)) {
                    core_val <- .mojor_guvectorize_reduce_core_value(
                        value = core_val,
                        core_shape = reduction_core_shape,
                        keep_axes = reduction_outputs[[k]]$keep_axes,
                        op = reduction_outputs[[k]]$op
                    )
                }
                core_values[[k]] <- .mojor_guvectorize_validate_core_output(
                    core_val,
                    out_core_shapes[[k]],
                    if (output_arity > 1L) k else NULL
                )
            }
            if (batch_rank == 0L) {
                single <- if (output_arity == 1L) core_values[[1L]] else core_values
                break
            }
            if (is.null(out)) {
                out <- lapply(
                    seq_len(output_arity),
                    function(k) .mojor_guvectorize_alloc_output(
                        core_values[[k]],
                        c(batch_shape, out_core_shapes[[k]])
                    )
                )
            }
            for (k in seq_len(output_arity)) {
                out[[k]] <- .mojor_guvectorize_assign_output(
                    out[[k]],
                    batch_idx,
                    core_values[[k]],
                    length(out_core_shapes[[k]])
                )
            }
        }

        value <- if (batch_rank == 0L) {
            single
        } else if (output_arity == 1L) {
            out[[1L]]
        } else {
            out
        }
        if (output_arity > 1L) {
            names(value) <- output_names
        }
        .mojor_validate_guvectorize_output_shape(value, output_shape)
        attr(compiled_batch_cache, "last_hit") <- FALSE
        value
    }
}

.mojor_guvectorize_terminal_expr <- function(expr) {
    if (is.call(expr) && identical(expr[[1L]], as.name("{"))) {
        if (length(expr) < 2L) {
            return(NULL)
        }
        return(expr[[length(expr)]])
    }
    expr
}

.mojor_guvectorize_extract_terminal_list_return <- function(fn, expected_len) {
    term <- .mojor_guvectorize_terminal_expr(body(fn))
    list_call <- NULL
    if (is.call(term) && identical(term[[1L]], as.name("return")) && length(term) >= 2L) {
        ret <- term[[2L]]
        if (is.call(ret) && identical(ret[[1L]], as.name("list"))) {
            list_call <- ret
        }
    } else if (is.call(term) && identical(term[[1L]], as.name("list"))) {
        list_call <- term
    }
    if (is.null(list_call)) {
        stop("mojor_guvectorize: core_dims multi-output requires terminal list(...) return")
    }
    exprs <- as.list(list_call)[-1L]
    if (length(exprs) != expected_len) {
        stop(
            "mojor_guvectorize: core_dims multi-output return length mismatch (expected ",
            expected_len, ", got ", length(exprs), ")"
        )
    }
    nm <- names(exprs)
    if (is.null(nm)) {
        nm <- rep.int("", length(exprs))
    }
    list(exprs = exprs, names = nm)
}

.mojor_guvectorize_make_output_fn <- function(fn, expr) {
    out <- fn
    body(out) <- expr
    out
}

.mojor_guvectorize_extract_terminal_single_expr <- function(fn) {
    term <- .mojor_guvectorize_terminal_expr(body(fn))
    if (is.call(term) && identical(term[[1L]], as.name("return")) && length(term) >= 2L) {
        return(term[[2L]])
    }
    term
}

.mojor_guvectorize_is_literal_scalar <- function(expr) {
    if (!is.atomic(expr) || is.object(expr) || length(expr) != 1L) {
        return(FALSE)
    }
    is.numeric(expr) || is.integer(expr) || is.logical(expr)
}

.mojor_guvectorize_core_index_names <- function(core_rank) {
    if (core_rank <= 0L) {
        return(character(0))
    }
    paste0("i", seq_len(core_rank))
}

.mojor_guvectorize_core_dim_names <- function(core_rank) {
    if (core_rank <= 0L) {
        return(character(0))
    }
    paste0("n", seq_len(core_rank))
}

.mojor_guvectorize_expr_uses_names <- function(expr, names) {
    if (length(names) == 0L) {
        return(FALSE)
    }
    if (is.name(expr)) {
        return(as.character(expr) %in% names)
    }
    if (!is.call(expr)) {
        return(FALSE)
    }
    any(vapply(as.list(expr), .mojor_guvectorize_expr_uses_names, logical(1), names = names))
}

.mojor_guvectorize_parse_literal_int <- function(expr) {
    if (!is.atomic(expr) || length(expr) != 1L || is.na(expr)) {
        return(NULL)
    }
    if (!(is.numeric(expr) || is.integer(expr))) {
        return(NULL)
    }
    val <- as.numeric(expr)
    if (!is.finite(val) || abs(val - round(val)) > 0) {
        return(NULL)
    }
    as.integer(round(val))
}

.mojor_guvectorize_parse_index_offset <- function(expr, index_names) {
    if (is.name(expr)) {
        nm <- as.character(expr)
        if (nm %in% index_names) {
            return(list(var = nm, offset = 0L))
        }
        return(NULL)
    }
    if (!is.call(expr)) {
        return(NULL)
    }
    op <- as.character(expr[[1L]])
    if (!(op %in% c("+", "-")) || length(expr) != 3L) {
        return(NULL)
    }
    lhs <- expr[[2L]]
    rhs <- expr[[3L]]
    if (!is.name(lhs) || !(as.character(lhs) %in% index_names)) {
        return(NULL)
    }
    off <- .mojor_guvectorize_parse_literal_int(rhs)
    if (is.null(off)) {
        stop("mojor_guvectorize: core_dims rank-n does not support non-literal neighbor offsets")
    }
    if (identical(op, "-")) {
        off <- -off
    }
    list(var = as.character(lhs), offset = as.integer(off))
}

.mojor_guvectorize_parse_dim_offset <- function(expr, dim_names) {
    if (is.name(expr)) {
        nm <- as.character(expr)
        if (nm %in% dim_names) {
            return(list(dim = nm, offset = 0L))
        }
        return(NULL)
    }
    if (!is.call(expr)) {
        return(NULL)
    }
    op <- as.character(expr[[1L]])
    if (!(op %in% c("+", "-")) || length(expr) != 3L) {
        return(NULL)
    }
    lhs <- expr[[2L]]
    rhs <- expr[[3L]]
    if (!is.name(lhs) || !(as.character(lhs) %in% dim_names)) {
        return(NULL)
    }
    off <- .mojor_guvectorize_parse_literal_int(rhs)
    if (is.null(off)) {
        return(NULL)
    }
    if (identical(op, "-")) {
        off <- -off
    }
    list(dim = as.character(lhs), offset = as.integer(off))
}

.mojor_guvectorize_guard_contains_complex_ops <- function(expr) {
    if (!is.call(expr)) {
        return(FALSE)
    }
    op <- as.character(expr[[1L]])
    if (op %in% c("||", "|", "!")) {
        return(TRUE)
    }
    any(vapply(as.list(expr)[-1L], .mojor_guvectorize_guard_contains_complex_ops, logical(1)))
}

.mojor_guvectorize_guard_terms <- function(expr) {
    if (is.call(expr) && identical(as.character(expr[[1L]]), "&&") && length(expr) == 3L) {
        return(c(.mojor_guvectorize_guard_terms(expr[[2L]]), .mojor_guvectorize_guard_terms(expr[[3L]])))
    }
    list(expr)
}

.mojor_guvectorize_parse_guard_term <- function(term, index_names, dim_names) {
    if (!is.call(term) || length(term) != 3L) {
        return(NULL)
    }
    op <- as.character(term[[1L]])
    if (!(op %in% c(">", ">=", "<", "<="))) {
        return(NULL)
    }
    lhs <- term[[2L]]
    rhs <- term[[3L]]

    lhs_idx <- if (is.name(lhs) && as.character(lhs) %in% index_names) as.character(lhs) else NULL
    rhs_idx <- if (is.name(rhs) && as.character(rhs) %in% index_names) as.character(rhs) else NULL

    if (!is.null(lhs_idx) && is.null(rhs_idx)) {
        rhs_lit <- .mojor_guvectorize_parse_literal_int(rhs)
        rhs_dim <- .mojor_guvectorize_parse_dim_offset(rhs, dim_names)
        if (!is.null(rhs_lit) && op %in% c(">", ">=")) {
            return(list(var = lhs_idx, side = "lower", bound = if (op == ">") rhs_lit + 1L else rhs_lit))
        }
        if (!is.null(rhs_dim) && op %in% c("<", "<=")) {
            off <- if (op == "<") rhs_dim$offset - 1L else rhs_dim$offset
            return(list(var = lhs_idx, side = "upper", dim = rhs_dim$dim, offset = off))
        }
    }

    if (is.null(lhs_idx) && !is.null(rhs_idx)) {
        lhs_lit <- .mojor_guvectorize_parse_literal_int(lhs)
        lhs_dim <- .mojor_guvectorize_parse_dim_offset(lhs, dim_names)
        if (!is.null(lhs_lit) && op %in% c("<", "<=")) {
            return(list(var = rhs_idx, side = "lower", bound = if (op == "<") lhs_lit + 1L else lhs_lit))
        }
        if (!is.null(lhs_dim) && op %in% c(">", ">=")) {
            off <- if (op == ">") lhs_dim$offset - 1L else lhs_dim$offset
            return(list(var = rhs_idx, side = "upper", dim = lhs_dim$dim, offset = off))
        }
    }

    NULL
}

.mojor_guvectorize_collect_index_usage <- function(expr, vector_arg_names, index_names, core_rank) {
    mins <- rep.int(0L, core_rank)
    maxs <- rep.int(0L, core_rank)
    has_index <- FALSE
    has_neighbor <- FALSE

    rec <- function(node) {
        if (!is.call(node)) {
            return(invisible(NULL))
        }
        op <- as.character(node[[1L]])
        if (identical(op, "[")) {
            base <- node[[2L]]
            if (is.name(base) && as.character(base) %in% vector_arg_names) {
                idx_args <- as.list(node)[-c(1L, 2L)]
                if (length(idx_args) != core_rank) {
                    stop("mojor_guvectorize: core_dims rank-n strict indexed lane requires canonical core index variables")
                }
                has_index <<- TRUE
                for (pos in seq_len(core_rank)) {
                    idx <- idx_args[[pos]]
                    parsed_idx <- .mojor_guvectorize_parse_index_offset(idx, index_names)
                    if (is.null(parsed_idx)) {
                        if (.mojor_guvectorize_expr_uses_names(idx, index_names)) {
                            stop("mojor_guvectorize: core_dims rank-n does not support non-literal neighbor offsets")
                        }
                        stop("mojor_guvectorize: core_dims rank-n strict indexed lane requires canonical core index variables")
                    }
                    expected <- index_names[[pos]]
                    if (!identical(parsed_idx$var, expected)) {
                        stop("mojor_guvectorize: core_dims rank-n strict indexed lane requires canonical core index variables")
                    }
                    mins[[pos]] <<- min(mins[[pos]], parsed_idx$offset)
                    maxs[[pos]] <<- max(maxs[[pos]], parsed_idx$offset)
                    if (parsed_idx$offset != 0L) {
                        has_neighbor <<- TRUE
                    }
                }
                return(invisible(NULL))
            }
        }
        kids <- as.list(node)[-1L]
        for (k in kids) {
            rec(k)
        }
        invisible(NULL)
    }

    rec(expr)
    list(
        has_index = has_index,
        has_neighbor = has_neighbor,
        min_offsets = mins,
        max_offsets = maxs
    )
}

.mojor_guvectorize_validate_neighbor_guard <- function(expr, usage, index_names, dim_names) {
    if (!isTRUE(usage$has_neighbor)) {
        return(invisible(NULL))
    }

    op <- if (is.call(expr)) as.character(expr[[1L]]) else ""
    if (!(op %in% c("if", "ifelse"))) {
        stop("mojor_guvectorize: core_dims rank-n neighbor indexing requires explicit in-bounds guard")
    }

    cond <- if (identical(op, "if")) expr[[2L]] else expr[[2L]]
    if (.mojor_guvectorize_guard_contains_complex_ops(cond)) {
        stop("mojor_guvectorize: core_dims rank-n does not support complex boolean guard operators")
    }
    terms <- .mojor_guvectorize_guard_terms(cond)
    parsed <- lapply(terms, .mojor_guvectorize_parse_guard_term, index_names = index_names, dim_names = dim_names)
    if (any(vapply(parsed, is.null, logical(1)))) {
        stop("mojor_guvectorize: core_dims rank-n guard must be canonical conjunction of bounds checks")
    }

    lower_seen <- setNames(rep.int(NA_integer_, length(index_names)), index_names)
    upper_seen <- setNames(rep.int(NA_integer_, length(index_names)), index_names)
    dim_expected <- setNames(dim_names, index_names)
    for (one in parsed) {
        var <- one$var
        if (identical(one$side, "lower")) {
            prev <- lower_seen[[var]]
            lower_seen[[var]] <- if (is.na(prev)) one$bound else max(prev, one$bound)
        } else {
            if (!identical(one$dim, dim_expected[[var]])) {
                stop("mojor_guvectorize: core_dims rank-n guard must be canonical conjunction of bounds checks")
            }
            prev <- upper_seen[[var]]
            upper_seen[[var]] <- if (is.na(prev)) one$offset else min(prev, one$offset)
        }
    }

    for (i in seq_along(index_names)) {
        nm <- index_names[[i]]
        min_off <- usage$min_offsets[[i]]
        max_off <- usage$max_offsets[[i]]
        if (min_off < 0L) {
            req <- as.integer(1L - min_off)
            if (is.na(lower_seen[[nm]]) || lower_seen[[nm]] < req) {
                stop("mojor_guvectorize: core_dims rank-n neighbor indexing requires explicit in-bounds guard")
            }
        }
        if (max_off > 0L) {
            req_off <- as.integer(-max_off)
            if (is.na(upper_seen[[nm]]) || upper_seen[[nm]] > req_off) {
                stop("mojor_guvectorize: core_dims rank-n neighbor indexing requires explicit in-bounds guard")
            }
        }
    }
    invisible(NULL)
}

.mojor_guvectorize_is_strict_elementwise_expr <- function(expr, arg_names, strict_ctx = NULL) {
    index_names <- if (is.null(strict_ctx)) character(0) else strict_ctx$index_names
    dim_names <- if (is.null(strict_ctx)) character(0) else strict_ctx$dim_names
    vector_arg_names <- if (is.null(strict_ctx)) arg_names else strict_ctx$vector_arg_names
    core_rank <- if (is.null(strict_ctx)) 0L else strict_ctx$core_rank

    if (is.name(expr)) {
        nm <- as.character(expr)
        return(nm %in% c(arg_names, index_names, dim_names))
    }
    if (.mojor_guvectorize_is_literal_scalar(expr)) {
        return(TRUE)
    }
    if (!is.call(expr)) {
        return(FALSE)
    }
    op <- as.character(expr[[1L]])
    if (op %in% c("+", "-", "*", "/")) {
        if (identical(op, "-") && length(expr) == 2L) {
            return(.mojor_guvectorize_is_strict_elementwise_expr(expr[[2L]], arg_names, strict_ctx))
        }
        if (length(expr) == 3L) {
            return(
                .mojor_guvectorize_is_strict_elementwise_expr(expr[[2L]], arg_names, strict_ctx) &&
                    .mojor_guvectorize_is_strict_elementwise_expr(expr[[3L]], arg_names, strict_ctx)
            )
        }
        return(FALSE)
    }
    if (op %in% c("as.double", "as.integer", "as.logical") && length(expr) == 2L) {
        return(.mojor_guvectorize_is_strict_elementwise_expr(expr[[2L]], arg_names, strict_ctx))
    }
    if (op %in% c(">", ">=", "<", "<=", "&&") && length(expr) == 3L) {
        return(
            .mojor_guvectorize_is_strict_elementwise_expr(expr[[2L]], arg_names, strict_ctx) &&
                .mojor_guvectorize_is_strict_elementwise_expr(expr[[3L]], arg_names, strict_ctx)
        )
    }
    if (op %in% c("||", "|") && length(expr) == 3L) {
        return(
            .mojor_guvectorize_is_strict_elementwise_expr(expr[[2L]], arg_names, strict_ctx) &&
                .mojor_guvectorize_is_strict_elementwise_expr(expr[[3L]], arg_names, strict_ctx)
        )
    }
    if (identical(op, "!") && length(expr) == 2L) {
        return(.mojor_guvectorize_is_strict_elementwise_expr(expr[[2L]], arg_names, strict_ctx))
    }
    if (identical(op, "if") && (length(expr) == 3L || length(expr) == 4L)) {
        ok <- .mojor_guvectorize_is_strict_elementwise_expr(expr[[2L]], arg_names, strict_ctx) &&
            .mojor_guvectorize_is_strict_elementwise_expr(expr[[3L]], arg_names, strict_ctx)
        if (length(expr) == 4L) {
            ok <- ok && .mojor_guvectorize_is_strict_elementwise_expr(expr[[4L]], arg_names, strict_ctx)
        }
        return(ok)
    }
    if (identical(op, "ifelse") && length(expr) == 4L) {
        return(
            .mojor_guvectorize_is_strict_elementwise_expr(expr[[2L]], arg_names, strict_ctx) &&
                .mojor_guvectorize_is_strict_elementwise_expr(expr[[3L]], arg_names, strict_ctx) &&
                .mojor_guvectorize_is_strict_elementwise_expr(expr[[4L]], arg_names, strict_ctx)
        )
    }
    if (identical(op, "[")) {
        if (is.null(strict_ctx) || core_rank <= 0L || length(expr) < 3L) {
            return(FALSE)
        }
        base <- expr[[2L]]
        if (!is.name(base) || !(as.character(base) %in% vector_arg_names)) {
            return(FALSE)
        }
        idx_args <- as.list(expr)[-c(1L, 2L)]
        if (length(idx_args) != core_rank) {
            stop("mojor_guvectorize: core_dims rank-n strict indexed lane requires canonical core index variables")
        }
        for (pos in seq_len(core_rank)) {
            idx <- idx_args[[pos]]
            parsed_idx <- .mojor_guvectorize_parse_index_offset(idx, index_names)
            if (is.null(parsed_idx)) {
                if (.mojor_guvectorize_expr_uses_names(idx, index_names)) {
                    stop("mojor_guvectorize: core_dims rank-n does not support non-literal neighbor offsets")
                }
                stop("mojor_guvectorize: core_dims rank-n strict indexed lane requires canonical core index variables")
            }
            if (!identical(parsed_idx$var, index_names[[pos]])) {
                stop("mojor_guvectorize: core_dims rank-n strict indexed lane requires canonical core index variables")
            }
        }
        return(TRUE)
    }
    FALSE
}

.mojor_guvectorize_make_linear_index_expr <- function(index_names, dim_names, offsets = NULL) {
    rank <- length(index_names)
    terms <- vector("list", rank)
    for (i in seq_len(rank)) {
        idx_expr <- as.name(index_names[[i]])
        off <- if (is.null(offsets)) 0L else as.integer(offsets[[i]])
        if (off != 0L) {
            idx_expr <- as.call(list(as.name(if (off > 0L) "+" else "-"), idx_expr, as.integer(abs(off))))
        }
        term <- as.call(list(as.name("-"), idx_expr, 1L))
        if (i > 1L) {
            stride <- as.name(dim_names[[1L]])
            if (i > 2L) {
                for (j in 2:(i - 1L)) {
                    stride <- as.call(list(as.name("*"), stride, as.name(dim_names[[j]])))
                }
            }
            term <- as.call(list(as.name("*"), term, stride))
        }
        terms[[i]] <- term
    }
    out <- 1L
    for (term in terms) {
        out <- as.call(list(as.name("+"), out, term))
    }
    out
}

.mojor_guvectorize_rewrite_strict_elementwise_expr <- function(
    expr,
    vector_arg_names,
    index_name,
    strict_ctx = NULL
) {
    if (is.name(expr)) {
        nm <- as.character(expr)
        if (nm %in% vector_arg_names) {
            return(as.call(list(as.name("["), as.name(nm), as.name(index_name))))
        }
        return(expr)
    }
    if (!is.call(expr)) {
        return(expr)
    }
    op <- as.character(expr[[1L]])
    if (identical(op, "[") && !is.null(strict_ctx)) {
        base <- expr[[2L]]
        if (is.name(base) && as.character(base) %in% vector_arg_names) {
            idx_args <- as.list(expr)[-c(1L, 2L)]
            if (length(idx_args) == strict_ctx$core_rank && strict_ctx$core_rank > 0L) {
                offsets <- integer(strict_ctx$core_rank)
                for (pos in seq_len(strict_ctx$core_rank)) {
                    parsed_idx <- .mojor_guvectorize_parse_index_offset(idx_args[[pos]], strict_ctx$index_names)
                    if (is.null(parsed_idx)) {
                        stop("mojor_guvectorize: core_dims rank-n strict indexed lane requires canonical core index variables")
                    }
                    if (!identical(parsed_idx$var, strict_ctx$index_names[[pos]])) {
                        stop("mojor_guvectorize: core_dims rank-n strict indexed lane requires canonical core index variables")
                    }
                    offsets[[pos]] <- parsed_idx$offset
                }
                lin <- .mojor_guvectorize_make_linear_index_expr(
                    index_names = strict_ctx$index_names,
                    dim_names = strict_ctx$dim_names,
                    offsets = offsets
                )
                return(as.call(list(as.name("["), base, lin)))
            }
        }
    }

    out <- expr
    for (i in seq_along(out)[-1L]) {
        out[[i]] <- .mojor_guvectorize_rewrite_strict_elementwise_expr(
            out[[i]],
            vector_arg_names = vector_arg_names,
            index_name = index_name,
            strict_ctx = strict_ctx
        )
    }
    out
}

.mojor_guvectorize_core_arg_base <- function(spec) {
    base <- sub("\\[.*\\]$", "", spec)
    if (identical(base, "logical")) {
        base <- "lgl"
    }
    base
}

.mojor_guvectorize_scalarized_specs <- function(specs, parsed) {
    out <- specs
    arg_names <- names(specs)
    for (i in seq_along(arg_names)) {
        nm <- arg_names[[i]]
        base <- .mojor_guvectorize_core_arg_base(specs[[nm]])
        if (length(parsed$inputs[[i]]) > 0L) {
            out[[nm]] <- paste0(base, "[]")
        } else {
            out[[nm]] <- base
        }
    }
    out
}

.mojor_guvectorize_elementwise_ctor_for_spec <- function(spec) {
    base <- .mojor_guvectorize_core_arg_base(spec)
    if (base %in% c("i32")) {
        return("integer")
    }
    if (base %in% c("lgl", "bool")) {
        return("logical")
    }
    "numeric"
}

.mojor_guvectorize_make_strict_elementwise_fn <- function(
    fn,
    expr,
    vector_arg_names,
    output_ctor = "numeric",
    strict_ctx = NULL
) {
    arg_names <- names(formals(fn))
    if (is.null(arg_names) || length(arg_names) == 0L) {
        stop("mojor_guvectorize: core_dims rank-n strict indexed lane does not support this function body")
    }
    if (length(vector_arg_names) == 0L) {
        stop("mojor_guvectorize: core_dims rank-n strict indexed lane does not support this function body")
    }
    base_arg <- vector_arg_names[[1L]]
    idx_name <- "__mojor_i"
    out_name <- "__mojor_out"
    rewritten <- .mojor_guvectorize_rewrite_strict_elementwise_expr(
        expr = expr,
        vector_arg_names = vector_arg_names,
        index_name = idx_name,
        strict_ctx = strict_ctx
    )
    len_call <- as.call(list(as.name("length"), as.name(base_arg)))
    out_ctor_call <- as.call(list(as.name(output_ctor), len_call))
    loop_call <- NULL

    indexed_mode <- !is.null(strict_ctx) && isTRUE(strict_ctx$has_indexed) && strict_ctx$core_rank > 0L
    if (!indexed_mode) {
        idx_seq <- as.call(list(as.name("seq_len"), len_call))
        out_index <- as.call(list(as.name("["), as.name(out_name), as.name(idx_name)))
        out_assign <- as.call(list(as.name("<-"), out_index, rewritten))
        loop_call <- as.call(list(as.name("for"), as.name(idx_name), idx_seq, out_assign))
    } else {
        dim_names <- strict_ctx$dim_names
        idx_names <- strict_ctx$index_names
        dim_obj <- "__mojor_dims"
        dim_prelude <- as.call(list(as.name("<-"), as.name(dim_obj), as.call(list(as.name("dim"), as.name(base_arg)))))
        dim_fix <- as.call(
            list(
                as.name("if"),
                as.call(list(as.name("is.null"), as.name(dim_obj))),
                as.call(
                    list(
                        as.name("<-"),
                        as.name(dim_obj),
                        as.call(list(as.name("c"), as.call(list(as.name("length"), as.name(base_arg)))))
                    )
                )
            )
        )
        dim_assigns <- vector("list", strict_ctx$core_rank)
        for (i in seq_len(strict_ctx$core_rank)) {
            dim_assigns[[i]] <- as.call(
                list(
                    as.name("<-"),
                    as.name(dim_names[[i]]),
                    as.call(list(as.name("as.integer"), as.call(list(as.name("["), as.name(dim_obj), as.integer(i)))))
                )
            )
        }

        lin_expr <- .mojor_guvectorize_make_linear_index_expr(idx_names, dim_names, offsets = rep.int(0L, strict_ctx$core_rank))
        out_index <- as.call(list(as.name("["), as.name(out_name), lin_expr))
        out_assign <- as.call(list(as.name("<-"), out_index, rewritten))

        body_loop <- out_assign
        for (i in rev(seq_len(strict_ctx$core_rank))) {
            loop_var <- as.name(idx_names[[i]])
            loop_seq <- as.call(list(as.name("seq_len"), as.name(dim_names[[i]])))
            body_loop <- as.call(list(as.name("for"), loop_var, loop_seq, body_loop))
        }
        loop_call <- as.call(c(list(as.name("{"), dim_prelude, dim_fix), dim_assigns, list(body_loop)))
    }

    out_fn <- fn
    body(out_fn) <- as.call(
        list(
            as.name("{"),
            as.call(list(as.name("<-"), as.name(out_name), out_ctor_call)),
            loop_call,
            as.name(out_name)
        )
    )
    out_fn
}

.mojor_prepare_guvectorize_strict_elementwise <- function(fn, specs, parsed) {
    arg_names <- names(specs)
    input_ranks <- vapply(parsed$inputs, length, integer(1))
    vector_idx <- which(input_ranks > 0L)
    if (length(vector_idx) == 0L) {
        return(list(enabled = FALSE, reason = "no-core-vector-input"))
    }
    primary_tuple <- parsed$inputs[[vector_idx[[1L]]]]
    if (length(primary_tuple) == 0L) {
        return(list(enabled = FALSE, reason = "no-core-vector-input"))
    }
    input_ok <- all(vapply(vector_idx, function(i) identical(parsed$inputs[[i]], primary_tuple), logical(1)))
    output_ok <- all(vapply(parsed$outputs, function(x) identical(x, primary_tuple), logical(1)))
    if (!input_ok || !output_ok) {
        return(list(enabled = FALSE, reason = "non-elementwise-core-shape"))
    }
    output_arity <- length(parsed$outputs)
    output_names <- paste0("output", seq_len(output_arity))
    exprs <- NULL
    if (output_arity > 1L) {
        parsed_return <- .mojor_guvectorize_extract_terminal_list_return(fn, expected_len = output_arity)
        output_names <- ifelse(
            nzchar(parsed_return$names),
            parsed_return$names,
            output_names
        )
        exprs <- parsed_return$exprs
    } else {
        exprs <- list(.mojor_guvectorize_extract_terminal_single_expr(fn))
    }
    core_rank <- length(primary_tuple)
    strict_ctx <- list(
        core_rank = core_rank,
        index_names = .mojor_guvectorize_core_index_names(core_rank),
        dim_names = .mojor_guvectorize_core_dim_names(core_rank),
        vector_arg_names = arg_names[vector_idx]
    )

    valid <- vapply(exprs, .mojor_guvectorize_is_strict_elementwise_expr, logical(1), arg_names = arg_names, strict_ctx = strict_ctx)
    if (!all(valid)) {
        return(list(enabled = FALSE, reason = "unsupported-body-expression"))
    }
    usage_info <- lapply(
        exprs,
        .mojor_guvectorize_collect_index_usage,
        vector_arg_names = strict_ctx$vector_arg_names,
        index_names = strict_ctx$index_names,
        core_rank = core_rank
    )
    for (idx in seq_along(usage_info)) {
        one_usage <- usage_info[[idx]]
        .mojor_guvectorize_validate_neighbor_guard(
            expr = exprs[[idx]],
            usage = one_usage,
            index_names = strict_ctx$index_names,
            dim_names = strict_ctx$dim_names
        )
    }

    has_indexed <- any(vapply(usage_info, function(x) isTRUE(x$has_index), logical(1)))
    has_neighbor <- any(vapply(usage_info, function(x) isTRUE(x$has_neighbor), logical(1)))
    vector_arg_names <- arg_names[vector_idx]
    strict_specs <- .mojor_guvectorize_scalarized_specs(specs = specs, parsed = parsed)
    output_ctors <- rep.int(
        .mojor_guvectorize_elementwise_ctor_for_spec(strict_specs[[vector_arg_names[[1L]]]]),
        output_arity
    )
    list(
        enabled = TRUE,
        reason = NULL,
        exprs = exprs,
        output_names = output_names,
        vector_arg_names = vector_arg_names,
        strict_specs = strict_specs,
        output_ctors = output_ctors,
        strict_ctx = c(strict_ctx, list(has_indexed = has_indexed, has_neighbor = has_neighbor))
    )
}

.mojor_prepare_guvectorize_rank_n_reduction <- function(fn, specs, parsed) {
    arg_names <- names(specs)
    input_ranks <- vapply(parsed$inputs, length, integer(1))
    vector_idx <- which(input_ranks > 0L)
    if (length(vector_idx) == 0L) {
        return(list(enabled = FALSE, reason = "no-core-vector-input"))
    }
    primary_tuple <- parsed$inputs[[vector_idx[[1L]]]]
    if (length(primary_tuple) == 0L) {
        return(list(enabled = FALSE, reason = "no-core-vector-input"))
    }
    if (!all(vapply(vector_idx, function(i) identical(parsed$inputs[[i]], primary_tuple), logical(1)))) {
        return(list(enabled = FALSE, reason = "non-elementwise-core-shape"))
    }

    output_arity <- length(parsed$outputs)
    output_names <- paste0("output", seq_len(output_arity))
    exprs <- if (output_arity > 1L) {
        parsed_return <- .mojor_guvectorize_extract_terminal_list_return(fn, expected_len = output_arity)
        output_names <- ifelse(
            nzchar(parsed_return$names),
            parsed_return$names,
            output_names
        )
        parsed_return$exprs
    } else {
        list(.mojor_guvectorize_extract_terminal_single_expr(fn))
    }

    core_rank <- length(primary_tuple)
    strict_ctx <- list(
        core_rank = core_rank,
        index_names = .mojor_guvectorize_core_index_names(core_rank),
        dim_names = .mojor_guvectorize_core_dim_names(core_rank),
        vector_arg_names = arg_names[vector_idx]
    )
    strict_specs <- .mojor_guvectorize_scalarized_specs(specs = specs, parsed = parsed)
    vector_arg_names <- arg_names[vector_idx]
    output_ctors <- rep.int(
        .mojor_guvectorize_elementwise_ctor_for_spec(strict_specs[[vector_arg_names[[1L]]]]),
        output_arity
    )
    supported_ops <- c("sum", "mean", "min", "max")
    reduction_outputs <- vector("list", output_arity)
    strict_exprs <- vector("list", output_arity)

    for (k in seq_len(output_arity)) {
        out_tuple <- parsed$outputs[[k]]
        if (!all(out_tuple %in% primary_tuple)) {
            return(list(enabled = FALSE, reason = "axis-unresolved"))
        }
        keep_axes <- if (length(out_tuple) == 0L) integer(0) else match(out_tuple, primary_tuple)
        if (any(is.na(keep_axes)) || length(unique(keep_axes)) != length(keep_axes)) {
            return(list(enabled = FALSE, reason = "axis-unresolved"))
        }
        expr <- exprs[[k]]
        is_reduction <- is.call(expr) &&
            length(expr) == 2L &&
            (as.character(expr[[1L]]) %in% supported_ops)
        if (is_reduction) {
            expr_inner <- expr[[2L]]
            if (!.mojor_guvectorize_is_strict_elementwise_expr(expr_inner, arg_names = arg_names, strict_ctx = strict_ctx)) {
                return(list(enabled = FALSE, reason = "non-strict-expression"))
            }
            strict_exprs[[k]] <- expr_inner
            reduction_outputs[[k]] <- list(
                op = as.character(expr[[1L]]),
                keep_axes = as.integer(keep_axes)
            )
            next
        }
        if (is.call(expr) && length(expr) == 2L) {
            return(list(enabled = FALSE, reason = "unsupported-op"))
        }
        if (!identical(out_tuple, primary_tuple)) {
            return(list(enabled = FALSE, reason = "unsatisfied-contract"))
        }
        if (!.mojor_guvectorize_is_strict_elementwise_expr(expr, arg_names = arg_names, strict_ctx = strict_ctx)) {
            return(list(enabled = FALSE, reason = "non-strict-expression"))
        }
        strict_exprs[[k]] <- expr
        reduction_outputs[[k]] <- list(op = NULL, keep_axes = as.integer(seq_len(core_rank)))
    }
    usage_info <- lapply(
        strict_exprs,
        .mojor_guvectorize_collect_index_usage,
        vector_arg_names = strict_ctx$vector_arg_names,
        index_names = strict_ctx$index_names,
        core_rank = core_rank
    )
    for (idx in seq_along(usage_info)) {
        .mojor_guvectorize_validate_neighbor_guard(
            expr = strict_exprs[[idx]],
            usage = usage_info[[idx]],
            index_names = strict_ctx$index_names,
            dim_names = strict_ctx$dim_names
        )
    }
    has_indexed <- any(vapply(usage_info, function(x) isTRUE(x$has_index), logical(1)))
    has_neighbor <- any(vapply(usage_info, function(x) isTRUE(x$has_neighbor), logical(1)))
    list(
        enabled = TRUE,
        reason = NULL,
        exprs = strict_exprs,
        output_names = output_names,
        vector_arg_names = vector_arg_names,
        strict_specs = strict_specs,
        output_ctors = output_ctors,
        strict_ctx = c(strict_ctx, list(has_indexed = has_indexed, has_neighbor = has_neighbor)),
        outputs = reduction_outputs,
        primary_tuple = primary_tuple
    )
}

.mojor_guvectorize_build_strict_only <- function(fn, name, build_args) {
    strict_err <- NULL
    strict_build <- tryCatch(
        do.call(
            mojor_build,
            c(list(fn = fn, name = name), build_args)
        ),
        error = function(e) {
            strict_err <<- e
            NULL
        }
    )
    list(
        build = strict_build,
        strict_error = if (is.null(strict_err)) NULL else conditionMessage(strict_err)
    )
}

.mojor_guvectorize_build_core_kernel <- function(
    fn,
    name,
    build_args,
    target,
    rank_n_mode = FALSE,
    output_index = NULL
) {
    strict_err <- NULL
    strict_build <- tryCatch(
        do.call(
            mojor_build,
            c(list(fn = fn, name = name), build_args)
        ),
        error = function(e) {
            strict_err <<- e
            NULL
        }
    )
    if (!is.null(strict_build)) {
        return(list(build = strict_build, used_retry = FALSE, strict_error = NULL))
    }

    if (identical(target, "gpu")) {
        if (!is.null(output_index)) {
            stop(
                "mojor_guvectorize: gpu target core kernel #",
                as.integer(output_index),
                " failed strict compilation for core_dims multi-output"
            )
        }
        if (isTRUE(rank_n_mode)) {
            stop("mojor_guvectorize: gpu target core kernel failed strict compilation for core_dims rank-n")
        }
        stop(conditionMessage(strict_err), call. = FALSE)
    }

    if (!isTRUE(rank_n_mode)) {
        stop(conditionMessage(strict_err), call. = FALSE)
    }

    if (!is.null(output_index)) {
        stop(
            "mojor_guvectorize: cpu target core kernel #",
            as.integer(output_index),
            " failed strict compilation for core_dims rank-n: ",
            conditionMessage(strict_err),
            call. = FALSE
        )
    }
    stop(
        "mojor_guvectorize: cpu target core kernel failed strict compilation for core_dims rank-n: ",
        conditionMessage(strict_err),
        call. = FALSE
    )
}

.mojor_guvectorize_append_rank_n_retry <- function(
    retry_info,
    output_index,
    strict_error,
    strict_error_message
) {
    retry_info[[length(retry_info) + 1L]] <- list(
        output_index = as.integer(output_index),
        strict_error = strict_error
    )
    if (is.null(strict_error_message) && !is.null(strict_error)) {
        strict_error_message <- strict_error
    }
    list(retry_info = retry_info, strict_error_message = strict_error_message)
}

#' Generalized vectorize build wrapper
#'
#' @param fn Function to compile.
#' @param signature Named signature spec list or signature string.
#' @param name Kernel name prefix.
#' @param target Vectorize target backend (`"cpu"` or `"gpu"`).
#' @param core_dims Optional core-dimension signature string (for example `"(n),(n)->(n)"`).
#'   Tuple rank is unbounded in this release.
#' @param output_shape Optional output metadata. For single-output wrappers provide
#'   an integer shape vector; for multi-output wrappers provide a list of integer
#'   shape vectors (one per output tuple).
#' @param load If TRUE, return callable function; otherwise return build result.
#' @param cache Enable build cache.
#' @param cache_dir Optional cache directory.
#' @param parallel Enable parallel loop routing.
#' @param broadcast Broadcast mode.
#' @return Compiled callable (`load=TRUE`) or full build result (`load=FALSE`).
#' @export
mojor_guvectorize <- function(
    fn, signature, name = "mojor_guvectorize_kernel", target = c("cpu", "gpu"), core_dims = NULL, output_shape = NULL,
    load = TRUE, cache = TRUE, cache_dir = NULL, parallel = FALSE, broadcast = c(
        "none", "scalar", "recycle", "recycle_warn", "broadcast_nd", "broadcast_nd_warn"
    )
) {
    specs <- .mojor_as_signature_specs(signature)
    target <- match.arg(target)
    broadcast <- match.arg(broadcast)
    core_dims_parsed <- NULL
    core_dims_raw <- NULL
    if (!is.null(core_dims)) {
        core_dims_parsed <- .mojor_validate_guvectorize_core_dims(
            .mojor_parse_guvectorize_core_dims(core_dims),
            specs
        )
        core_dims_raw <- core_dims_parsed$raw
    }
    rank_n_mode <- !is.null(core_dims_parsed) &&
        .mojor_guvectorize_core_dims_max_rank(core_dims_parsed) > 2L
    output_arity <- if (is.null(core_dims_parsed)) 1L else length(core_dims_parsed$outputs)
    output_names <- paste0("output", seq_len(output_arity))
    retry_info <- list()
    strict_error_message <- NULL
    engine <- "direct_core"
    strict_vector_arg_names <- NULL
    strict_has_indexed <- FALSE
    strict_plan <- NULL
    reduction_plan <- NULL
    reduction_outputs <- NULL
    reduction_primary_tuple <- NULL
    out_shape <- .mojor_guvectorize_normalize_output_shape(output_shape, output_arity)

    build_opts <- list(
        load = load, cache = cache, cache_dir = cache_dir,
        parallel = parallel, broadcast = broadcast, object_mode = "off",
        elementwise = identical(target, "gpu"), elementwise_target = target
    )
    build_args <- c(
        build_opts,
        specs
    )

    if (!is.null(core_dims_parsed) && isTRUE(rank_n_mode)) {
        strict_plan <- .mojor_prepare_guvectorize_strict_elementwise(fn = fn, specs = specs, parsed = core_dims_parsed)
        if (isTRUE(strict_plan$enabled)) {
            engine <- "strict_elementwise"
            output_names <- strict_plan$output_names
            strict_has_indexed <- isTRUE(strict_plan$strict_ctx$has_indexed)
        } else {
            if (identical(strict_plan$reason, "non-elementwise-core-shape")) {
                reduction_plan <- .mojor_prepare_guvectorize_rank_n_reduction(fn = fn, specs = specs, parsed = core_dims_parsed)
                if (isTRUE(reduction_plan$enabled)) {
                    engine <- "strict_elementwise"
                    strict_plan <- reduction_plan
                    output_names <- strict_plan$output_names
                    strict_has_indexed <- isTRUE(strict_plan$strict_ctx$has_indexed)
                    reduction_outputs <- strict_plan$outputs
                    reduction_primary_tuple <- strict_plan$primary_tuple
                } else if (identical(target, "gpu")) {
                    if (identical(reduction_plan$reason, "axis-unresolved")) {
                        stop("mojor_guvectorize: core_dims rank-n reduction axes cannot be resolved from output tuple")
                    }
                    if (identical(reduction_plan$reason, "unsupported-op")) {
                        stop("mojor_guvectorize: core_dims rank-n reduction op is not supported in this release")
                    }
                    if (identical(reduction_plan$reason, "non-strict-expression")) {
                        stop("mojor_guvectorize: core_dims rank-n reduction expression is not strict-compilable")
                    }
                    if (identical(reduction_plan$reason, "unsatisfied-contract")) {
                        stop("mojor_guvectorize: core_dims rank-n reduction/output tuple contract is not satisfiable")
                    }
                    stop("mojor_guvectorize: core_dims rank-n strict indexed lane does not support this function body")
                }
            } else if (identical(target, "gpu")) {
                stop("mojor_guvectorize: core_dims rank-n strict indexed lane does not support this function body")
            }
        }
    }

    build <- NULL
    core_fns <- NULL

    if (identical(engine, "strict_elementwise")) {
        strict_build_args <- c(build_opts, strict_plan$strict_specs)
        builds <- vector("list", output_arity)
        core_fns <- vector("list", output_arity)
        strict_fail_index <- NA_integer_
        for (k in seq_len(output_arity)) {
            fn_k <- .mojor_guvectorize_make_strict_elementwise_fn(
                fn = fn,
                expr = strict_plan$exprs[[k]],
                vector_arg_names = strict_plan$vector_arg_names,
                output_ctor = strict_plan$output_ctors[[k]],
                strict_ctx = strict_plan$strict_ctx
            )
            one <- .mojor_guvectorize_build_strict_only(
                fn = fn_k,
                name = if (output_arity > 1L) paste0(name, "_strict_out", k) else paste0(name, "_strict"),
                build_args = strict_build_args
            )
            if (is.null(one$build)) {
                strict_error_message <- one$strict_error
                strict_fail_index <- k
                break
            }
            builds[[k]] <- one$build
            core_fns[[k]] <- one$build$func
        }

        if (!is.na(strict_fail_index)) {
            if (identical(target, "gpu")) {
                if (output_arity > 1L) {
                    stop(
                        "mojor_guvectorize: gpu target core kernel #",
                        as.integer(strict_fail_index),
                        " failed strict compilation for core_dims multi-output"
                    )
                }
                stop("mojor_guvectorize: gpu target core kernel failed strict compilation for core_dims rank-n")
            }
            engine <- "direct_core"
            core_fns <- NULL
        } else {
            build <- builds[[1L]]
            if (output_arity > 1L) {
                build$core_dims_multi_output_builds <- builds
            }
            strict_vector_arg_names <- strict_plan$vector_arg_names
        }
    }

    if (identical(engine, "direct_core")) {
        if (!is.null(core_dims_parsed) && output_arity > 1L) {
            if (!isTRUE(load)) {
                stop("mojor_guvectorize: core_dims multi-output requires load=TRUE in this release")
            }
            parsed_return <- .mojor_guvectorize_extract_terminal_list_return(fn, expected_len = output_arity)
            output_names <- ifelse(
                nzchar(parsed_return$names),
                parsed_return$names,
                paste0("output", seq_len(output_arity))
            )
            builds <- vector("list", output_arity)
            core_fns <- vector("list", output_arity)
            for (k in seq_len(output_arity)) {
                fn_k <- .mojor_guvectorize_make_output_fn(fn, parsed_return$exprs[[k]])
                one <- .mojor_guvectorize_build_core_kernel(
                    fn = fn_k,
                    name = paste0(name, "_out", k),
                    build_args = build_args,
                    target = target,
                    rank_n_mode = rank_n_mode,
                    output_index = k
                )
                build_k <- one$build
                if (isTRUE(one$used_retry)) {
                    retry_update <- .mojor_guvectorize_append_rank_n_retry(
                        retry_info = retry_info,
                        output_index = k,
                        strict_error = one$strict_error,
                        strict_error_message = strict_error_message
                    )
                    retry_info <- retry_update$retry_info
                    strict_error_message <- retry_update$strict_error_message
                }
                builds[[k]] <- build_k
                core_fns[[k]] <- build_k$func
            }
            build <- builds[[1L]]
            build$core_dims_multi_output_builds <- builds
        } else {
            one <- .mojor_guvectorize_build_core_kernel(
                fn = fn,
                name = name,
                build_args = build_args,
                target = target,
                rank_n_mode = rank_n_mode
            )
            build <- one$build
            if (isTRUE(one$used_retry)) {
                retry_update <- .mojor_guvectorize_append_rank_n_retry(
                    retry_info = retry_info,
                    output_index = 1L,
                    strict_error = one$strict_error,
                    strict_error_message = strict_error_message
                )
                retry_info <- retry_update$retry_info
                strict_error_message <- retry_update$strict_error_message
            }
            core_fns <- list(build$func)
        }
    }

    retry_used <- length(retry_info) > 0L
    build$core_dims_rank_n_mode <- isTRUE(rank_n_mode)
    build$core_dims_rank_n_fallback_used <- retry_used
    build$core_dims_rank_n_fallback <- retry_info
    build$core_dims_engine <- engine
    build$core_dims_strict_error <- strict_error_message

    out <- if (isTRUE(load)) {
        force(out_shape)
        if (is.null(core_dims_parsed)) {
            f <- build$func
            function(...) {
                val <- f(...)
                .mojor_validate_guvectorize_output_shape(val, out_shape)
                val
            }
        } else {
            .mojor_make_guvectorize_core_dims_wrapper(
                core_fns = core_fns,
                specs = specs,
                parsed = core_dims_parsed,
                output_shape = out_shape,
                output_names = output_names,
                engine = engine,
                vector_arg_names = strict_vector_arg_names,
                strict_has_indexed = strict_has_indexed,
                reduction_outputs = reduction_outputs,
                reduction_primary_tuple = reduction_primary_tuple,
                target = target
            )
        }
    } else {
        build
    }
    attr(out, "mojor_signature") <- specs
    attr(out, "mojor_output_shape") <- out_shape
    attr(out, "mojor_target") <- target
    attr(out, "mojor_core_dims") <- core_dims_raw
    attr(out, "mojor_core_dims_parsed") <- core_dims_parsed
    attr(out, "mojor_output_arity") <- output_arity
    attr(out, "mojor_output_names") <- output_names
    attr(out, "mojor_core_dims_rank_n_mode") <- isTRUE(rank_n_mode)
    attr(out, "mojor_core_dims_rank_n_fallback_used") <- retry_used
    attr(out, "mojor_core_dims_rank_n_fallback") <- retry_info
    attr(out, "mojor_core_dims_engine") <- engine
    attr(out, "mojor_core_dims_strict_error") <- strict_error_message
    attr(out, "mojor_core_dims_batch_engine") <- if (identical(engine, "strict_elementwise") && !isTRUE(strict_has_indexed)) "compiled_batch" else "r_loop"
    attr(out, "mojor_core_dims_batch_cache_hit") <- FALSE
    out
}

#' Parallel range alias for transpiled loops
#'
#' @param n Loop bound (typically an `i32` scalar argument in transpiled code).
#' @return A `seq_len` sequence in interpreted mode.
#' @export
mojor_prange <- function(n) {
    seq_parallel_len(n)
}

#' Parallel sequence helper for transpiled loops
#'
#' @param x Vector-like object.
#' @return `seq_along(x)` in interpreted mode.
#' @export
seq_parallel_along <- function(x) {
    seq_along(x)
}

#' Parallel sequence helper for transpiled loops
#'
#' @param n Sequence length.
#' @return `seq_len(n)` in interpreted mode.
#' @export
seq_parallel_len <- function(n) {
    n_i <- as.integer(n)
    if (length(n_i) !=
        1 || is.na(n_i) ||
        n_i < 0) {
        stop("seq_parallel_len: n must be a non-negative scalar integer")
    }
    seq_len(n_i)
}

# ============================================================================
# BinCount (histogram) helper - C_BinCount equivalent
# ============================================================================

#' BinCount - Fast histogram binning
#'
#' Counts elements into bins using optimized Mojo implementation.
#' Equivalent to R's internal C_BinCount.
#'
#' @param x Numeric vector of data points
#' @param breaks Numeric vector of break points (sorted, length >= 2)
#' @param right If TRUE, use (a,b] intervals; if FALSE, use [a,b)
#' @param include.lowest If TRUE, include lowest value in first bin
#' @return Integer vector of counts (length = length(breaks) - 1)
#' @export
mojor_bincount <- function(x, breaks, right = TRUE, include.lowest = TRUE) {
    if (!mojor_is_loaded())
        stop("Mojo backend not loaded")
    if (!is.numeric(x))
        stop("x must be numeric")
    if (!is.numeric(breaks))
        stop("breaks must be numeric")
    if (length(breaks) <
        2)
        stop("breaks must have at least 2 elements")

 # Ensure breaks are sorted
    breaks <- sort(as.double(breaks))
    x <- as.double(x)

 # Linux ARM64 bridge lane can intermittently crash in bincount dispatch;
 # use deterministic base-R histogram fallback there until backend parity is stable.
    sysname <- tolower(as.character(Sys.info()[["sysname"]]))
    machine <- tolower(as.character(Sys.info()[["machine"]]))
    if (identical(sysname, "linux") &&
        grepl("aarch64|arm64", machine)) {
        nbins <- length(breaks) - 1L
        if (isTRUE(right)) {
            idx <- findInterval(x, breaks, left.open = TRUE, rightmost.closed = TRUE)
            valid <- x <= breaks[[length(breaks)]] &
                (x > breaks[[1L]] | (isTRUE(include.lowest) & x == breaks[[1L]]))
        } else {
            idx <- findInterval(x, breaks, left.open = FALSE, rightmost.closed = TRUE)
            valid <- x >= breaks[[1L]] &
                (x < breaks[[length(breaks)]] | (isTRUE(include.lowest) & x == breaks[[length(breaks)]]))
        }
        idx[!valid] <- 0L
        return(as.integer(tabulate(idx[idx >= 1L & idx <= nbins], nbins = nbins)))
    }

    nbins <- length(breaks) -
        1
    counts <- integer(nbins)  # Pre-allocate output

    .mojor_call_bridge(
        "mojor_bincount_f64", x, breaks, counts, as.integer(length(x)),
        as.integer(length(breaks)),
        as.integer(ifelse(right, 1, 0)),
        as.integer(ifelse(include.lowest, 1, 0))
    )

    counts
}

# ============================================================================
# Random Number Generation (RNG) support
# ============================================================================

#' Seed the MojoR Random Number Generator
#'
#' @param seed Integer seed value
#' @return NULL (invisible)
#' @export
#' @description
#' Set the seed for the MojoR random number generator.
#' This ensures deterministic behavior: the same seed will produce the same
#' sequence of random numbers across runs.
#'
#' @examples
#' mojor_rng_seed(123)
#' x <- mojor_runif(5)
#' print(x)
mojor_rng_seed <- function(seed) {
    if (!mojor_is_loaded())
        stop("Mojo backend not loaded")
    seed <- as.integer(seed)
    if (length(seed) !=
        1)
        stop("seed must be a single integer")
    if (is.na(seed))
        stop("seed must be a valid integer")
    .mojor_state$last_rng_seed <- seed
    token <- .mojor_state$last_rng_seed_token
    if (!is.integer(token) ||
        length(token) !=
            1L || is.na(token))
        token <- 0L
    .mojor_state$last_rng_seed_token <- token + 1L
    invisible(.mojor_call_bridge("mojor_rng_seed", seed))
}

#' Generate Uniform Random Numbers
#'
#' @param n Number of random values to generate
#' @param min Minimum value (default: 0)
#' @param max Maximum value (default: 1)
#' @return Numeric vector of length n with U(min, max) random values
#' @export
mojor_runif <- function(n, min = 0, max = 1) {
    if (!mojor_is_loaded())
        stop("Mojo backend not loaded")
    n <- as.integer(n)
    if (n < 0)
        stop("n must be non-negative")
    if (n == 0) {
        return(numeric(0))
    }

    if (min == 0 && max == 1) {
        .mojor_call_bridge("mojor_runif", n)
    } else {
        .mojor_call_bridge(
            "mojor_runif_range", n, as.double(min),
            as.double(max)
        )
    }
}

#' Generate Normal Random Numbers
#'
#' @param n Number of random values to generate
#' @param mean Mean of the distribution (default: 0)
#' @param sd Standard deviation (default: 1)
#' @return Numeric vector of length n with N(mean, sd^2) random values
#' @export
mojor_rnorm <- function(n, mean = 0, sd = 1) {
    if (!mojor_is_loaded())
        stop("Mojo backend not loaded")
    n <- as.integer(n)
    if (n < 0)
        stop("n must be non-negative")
    if (n == 0) {
        return(numeric(0))
    }
    if (sd < 0)
        stop("sd must be non-negative")

    if (mean == 0 && sd == 1) {
        .mojor_call_bridge("mojor_rnorm", n)
    } else {
        .mojor_call_bridge(
            "mojor_rnorm_mean_sd", n, as.double(mean),
            as.double(sd)
        )
    }
}

#' Generate Gamma Random Numbers
#'
#' @param n Number of random values to generate
#' @param shape Shape parameter (alpha) > 0
#' @param rate Rate parameter (beta) > 0 (inverse of scale)
#' @return Numeric vector of length n with Gamma(shape, rate) random values
#' @export
mojor_rgamma <- function(n, shape, rate = 1) {
    if (!mojor_is_loaded())
        stop("Mojo backend not loaded")
    n <- as.integer(n)
    if (n < 0)
        stop("n must be non-negative")
    if (n == 0) {
        return(numeric(0))
    }
    if (shape <= 0)
        stop("shape must be positive")
    if (rate <= 0)
        stop("rate must be positive")

    .mojor_call_bridge(
        "mojor_rgamma", n, as.double(shape),
        as.double(rate)
    )
}

#' Generate Binomial Random Numbers
#'
#' @param n Number of random values to generate
#' @param size Number of trials (must be positive integer)
#' @param prob Probability of success on each trial (0 <= prob <= 1)
#' @return Numeric vector of length n with Binomial(size, prob) random values
#' @export
mojor_rbinom <- function(n, size, prob) {
    if (!mojor_is_loaded())
        stop("Mojo backend not loaded")
    n <- as.integer(n)
    if (n < 0)
        stop("n must be non-negative")
    if (n == 0) {
        return(numeric(0))
    }
    size <- as.integer(size)
    if (size < 0)
        stop("size must be non-negative")
    if (prob < 0 || prob > 1)
        stop("prob must be between 0 and 1")

    .mojor_call_bridge("mojor_rbinom", n, size, as.double(prob))
}

#' Generate Exponential Random Numbers
#'
#' @param n Number of random values to generate
#' @param rate Rate parameter (> 0)
#' @return Numeric vector of length n with Exponential(rate) random values
#' @export
mojor_rexp <- function(n, rate = 1) {
    if (!mojor_is_loaded())
        stop("Mojo backend not loaded")
    n <- as.integer(n)
    if (n < 0)
        stop("n must be non-negative")
    if (n == 0) {
        return(numeric(0))
    }
    if (!(rate > 0))
        stop("rate must be positive")

    .mojor_call_bridge("mojor_rexp", n, as.double(rate))
}

#' Generate Poisson Random Numbers
#'
#' @param n Number of random values to generate
#' @param lambda Mean parameter (>= 0)
#' @return Numeric vector of length n with Poisson(lambda) random values
#' @export
mojor_rpois <- function(n, lambda) {
    if (!mojor_is_loaded())
        stop("Mojo backend not loaded")
    n <- as.integer(n)
    if (n < 0)
        stop("n must be non-negative")
    if (n == 0) {
        return(numeric(0))
    }
    if (lambda < 0)
        stop("lambda must be non-negative")

    .mojor_call_bridge("mojor_rpois", n, as.double(lambda))
}

#' Generate Log-Normal Random Numbers
#'
#' @param n Number of random values to generate
#' @param meanlog Mean on log scale
#' @param sdlog Standard deviation on log scale (>= 0)
#' @return Numeric vector of length n
#' @export
mojor_rlnorm <- function(n, meanlog = 0, sdlog = 1) {
    if (!mojor_is_loaded())
        stop("Mojo backend not loaded")
    n <- as.integer(n)
    if (n < 0)
        stop("n must be non-negative")
    if (n == 0) {
        return(numeric(0))
    }
    if (sdlog < 0)
        stop("sdlog must be non-negative")

    .mojor_call_bridge(
        "mojor_rlnorm", n, as.double(meanlog),
        as.double(sdlog)
    )
}

#' Generate Chi-Squared Random Numbers
#'
#' @param n Number of random values to generate
#' @param df Degrees of freedom (> 0)
#' @return Numeric vector of length n
#' @export
mojor_rchisq <- function(n, df) {
    if (!mojor_is_loaded())
        stop("Mojo backend not loaded")
    n <- as.integer(n)
    if (n < 0)
        stop("n must be non-negative")
    if (n == 0) {
        return(numeric(0))
    }
    if (!(df > 0))
        stop("df must be positive")

    .mojor_call_bridge("mojor_rchisq", n, as.double(df))
}

#' Generate Student t Random Numbers
#'
#' @param n Number of random values to generate
#' @param df Degrees of freedom (> 0)
#' @return Numeric vector of length n
#' @export
mojor_rt <- function(n, df) {
    if (!mojor_is_loaded())
        stop("Mojo backend not loaded")
    n <- as.integer(n)
    if (n < 0)
        stop("n must be non-negative")
    if (n == 0) {
        return(numeric(0))
    }
    if (!(df > 0))
        stop("df must be positive")

    .mojor_call_bridge("mojor_rt", n, as.double(df))
}

#' Generate F Random Numbers
#'
#' @param n Number of random values to generate
#' @param df1 Numerator degrees of freedom (> 0)
#' @param df2 Denominator degrees of freedom (> 0)
#' @return Numeric vector of length n
#' @export
mojor_rf <- function(n, df1, df2) {
    if (!mojor_is_loaded())
        stop("Mojo backend not loaded")
    n <- as.integer(n)
    if (n < 0)
        stop("n must be non-negative")
    if (n == 0) {
        return(numeric(0))
    }
    if (!(df1 > 0))
        stop("df1 must be positive")
    if (!(df2 > 0))
        stop("df2 must be positive")

    .mojor_call_bridge(
        "mojor_rf", n, as.double(df1),
        as.double(df2)
    )
}

#' Generate Beta Random Numbers
#'
#' @param n Number of random values to generate
#' @param shape1 First shape parameter (> 0)
#' @param shape2 Second shape parameter (> 0)
#' @return Numeric vector of length n
#' @export
mojor_rbeta <- function(n, shape1, shape2) {
    if (!mojor_is_loaded())
        stop("Mojo backend not loaded")
    n <- as.integer(n)
    if (n < 0)
        stop("n must be non-negative")
    if (n == 0) {
        return(numeric(0))
    }
    if (!(shape1 > 0))
        stop("shape1 must be positive")
    if (!(shape2 > 0))
        stop("shape2 must be positive")

    .mojor_call_bridge(
        "mojor_rbeta", n, as.double(shape1),
        as.double(shape2)
    )
}

#' Generate Weibull Random Numbers
#'
#' @param n Number of random values to generate
#' @param shape Shape parameter (> 0)
#' @param scale Scale parameter (> 0)
#' @return Numeric vector of length n
#' @export
mojor_rweibull <- function(n, shape, scale = 1) {
    if (!mojor_is_loaded())
        stop("Mojo backend not loaded")
    n <- as.integer(n)
    if (n < 0)
        stop("n must be non-negative")
    if (n == 0) {
        return(numeric(0))
    }
    if (!(shape > 0))
        stop("shape must be positive")
    if (!(scale > 0))
        stop("scale must be positive")

    .mojor_call_bridge(
        "mojor_rweibull", n, as.double(shape),
        as.double(scale)
    )
}

#' Generate Logistic Random Numbers
#'
#' @param n Number of random values to generate
#' @param location Location parameter
#' @param scale Scale parameter (> 0)
#' @return Numeric vector of length n
#' @export
mojor_rlogis <- function(n, location = 0, scale = 1) {
    if (!mojor_is_loaded())
        stop("Mojo backend not loaded")
    n <- as.integer(n)
    if (n < 0)
        stop("n must be non-negative")
    if (n == 0) {
        return(numeric(0))
    }
    if (!(scale > 0))
        stop("scale must be positive")

    .mojor_call_bridge(
        "mojor_rlogis", n, as.double(location),
        as.double(scale)
    )
}

#' Generate Cauchy Random Numbers
#'
#' @param n Number of random values to generate
#' @param location Location parameter
#' @param scale Scale parameter (> 0)
#' @return Numeric vector of length n
#' @export
mojor_rcauchy <- function(n, location = 0, scale = 1) {
    if (!mojor_is_loaded())
        stop("Mojo backend not loaded")
    n <- as.integer(n)
    if (n < 0)
        stop("n must be non-negative")
    if (n == 0) {
        return(numeric(0))
    }
    if (!(scale > 0))
        stop("scale must be positive")

    .mojor_call_bridge(
        "mojor_rcauchy", n, as.double(location),
        as.double(scale)
    )
}

#' Generate Geometric Random Numbers
#'
#' @param n Number of random values to generate
#' @param prob Success probability in (0, 1]
#' @return Numeric vector of length n
#' @export
mojor_rgeom <- function(n, prob) {
    if (!mojor_is_loaded())
        stop("Mojo backend not loaded")
    n <- as.integer(n)
    if (n < 0)
        stop("n must be non-negative")
    if (n == 0) {
        return(numeric(0))
    }
    if (!(prob > 0 && prob <= 1))
        stop("prob must be in (0, 1]")

    .mojor_call_bridge("mojor_rgeom", n, as.double(prob))
}

#' Generate Negative Binomial Random Numbers
#'
#' @param n Number of random values to generate
#' @param size Dispersion parameter (>= 0)
#' @param prob Success probability in (0, 1]
#' @return Numeric vector of length n
#' @export
mojor_rnbinom <- function(n, size, prob) {
    if (!mojor_is_loaded())
        stop("Mojo backend not loaded")
    n <- as.integer(n)
    if (n < 0)
        stop("n must be non-negative")
    if (n == 0) {
        return(numeric(0))
    }
    if (size < 0)
        stop("size must be non-negative")
    if (!(prob > 0 && prob <= 1))
        stop("prob must be in (0, 1]")

    .mojor_call_bridge(
        "mojor_rnbinom", n, as.double(size),
        as.double(prob)
    )
}

#' Generate Hypergeometric Random Numbers
#'
#' @param nn Number of random values to generate
#' @param m Number of white balls in urn (>= 0)
#' @param n Number of black balls in urn (>= 0)
#' @param k Number of draws without replacement (>= 0, <= m + n)
#' @return Numeric vector of length nn
#' @export
mojor_rhyper <- function(nn, m, n, k) {
    if (!mojor_is_loaded())
        stop("Mojo backend not loaded")
    nn <- as.integer(nn)
    if (nn < 0)
        stop("nn must be non-negative")
    if (nn == 0) {
        return(numeric(0))
    }
    m <- as.integer(m)
    n <- as.integer(n)
    k <- as.integer(k)
    if (m < 0)
        stop("m must be non-negative")
    if (n < 0)
        stop("n must be non-negative")
    if (k < 0)
        stop("k must be non-negative")
    if (k > (m + n))
        stop("k cannot exceed m + n")

    .mojor_call_bridge("mojor_rhyper", nn, m, n, k)
}

#' Generate Wilcoxon Signed-Rank Random Numbers
#'
#' @param nn Number of random values to generate
#' @param n Number of ranks (>= 0)
#' @return Numeric vector of length nn
#' @export
mojor_rsignrank <- function(nn, n) {
    if (!mojor_is_loaded())
        stop("Mojo backend not loaded")
    nn <- as.integer(nn)
    if (nn < 0)
        stop("nn must be non-negative")
    if (nn == 0) {
        return(numeric(0))
    }
    n <- as.integer(n)
    if (n < 0)
        stop("n must be non-negative")

    .mojor_call_bridge("mojor_rsignrank", nn, n)
}

#' Generate Wilcoxon Rank-Sum Random Numbers
#'
#' @param nn Number of random values to generate
#' @param m Size of first sample (>= 0)
#' @param n Size of second sample (>= 0)
#' @return Numeric vector of length nn
#' @export
mojor_rwilcox <- function(nn, m, n) {
    if (!mojor_is_loaded())
        stop("Mojo backend not loaded")
    nn <- as.integer(nn)
    if (nn < 0)
        stop("nn must be non-negative")
    if (nn == 0) {
        return(numeric(0))
    }
    m <- as.integer(m)
    n <- as.integer(n)
    if (m < 0)
        stop("m must be non-negative")
    if (n < 0)
        stop("n must be non-negative")

    .mojor_call_bridge("mojor_rwilcox", nn, m, n)
}

# Base-R distribution API parity: d*/p*/q* wrappers. These delegate
# to stats::* implementations.
.mojor_stats_dist_aliases <- c(
    mojor_dbeta = "dbeta", mojor_pbeta = "pbeta", mojor_qbeta = "qbeta",
    mojor_dbinom = "dbinom", mojor_pbinom = "pbinom", mojor_qbinom = "qbinom",
    mojor_dcauchy = "dcauchy", mojor_pcauchy = "pcauchy", mojor_qcauchy = "qcauchy",
    mojor_dchisq = "dchisq", mojor_pchisq = "pchisq", mojor_qchisq = "qchisq",
    mojor_dexp = "dexp", mojor_pexp = "pexp", mojor_qexp = "qexp", mojor_df = "df",
    mojor_pf = "pf", mojor_qf = "qf", mojor_dgamma = "dgamma", mojor_pgamma = "pgamma",
    mojor_qgamma = "qgamma", mojor_dgeom = "dgeom", mojor_pgeom = "pgeom",
    mojor_qgeom = "qgeom", mojor_dhyper = "dhyper", mojor_phyper = "phyper",
    mojor_qhyper = "qhyper", mojor_dlogis = "dlogis", mojor_plogis = "plogis",
    mojor_qlogis = "qlogis", mojor_dlnorm = "dlnorm", mojor_plnorm = "plnorm",
    mojor_qlnorm = "qlnorm", mojor_dnbinom = "dnbinom", mojor_pnbinom = "pnbinom",
    mojor_qnbinom = "qnbinom", mojor_dnorm = "dnorm", mojor_pnorm = "pnorm",
    mojor_qnorm = "qnorm", mojor_dpois = "dpois", mojor_ppois = "ppois",
    mojor_qpois = "qpois", mojor_dt = "dt", mojor_pt = "pt", mojor_qt = "qt",
    mojor_dunif = "dunif", mojor_punif = "punif", mojor_qunif = "qunif",
    mojor_dweibull = "dweibull", mojor_pweibull = "pweibull", mojor_qweibull = "qweibull",
    mojor_dsignrank = "dsignrank", mojor_psignrank = "psignrank", mojor_qsignrank = "qsignrank",
    mojor_dwilcox = "dwilcox", mojor_pwilcox = "pwilcox", mojor_qwilcox = "qwilcox",
    mojor_pbirthday = "pbirthday", mojor_qbirthday = "qbirthday", mojor_ptukey = "ptukey",
    mojor_qtukey = "qtukey"
)

for (.mojor_alias_name in names(.mojor_stats_dist_aliases)) {
    .mojor_base_name <- .mojor_stats_dist_aliases[[.mojor_alias_name]]
    assign(
        .mojor_alias_name, getFromNamespace(.mojor_base_name, "stats"),
        envir = environment()
    )
}
rm(.mojor_alias_name, .mojor_base_name)

.mojor_stats_dnorm <- getFromNamespace("dnorm", "stats")
.mojor_stats_dpois <- getFromNamespace("dpois", "stats")
.mojor_dist_fast_min_n <- 8L

.mojor_dist_fast_numeric_vector <- function(x) {
    is.atomic(x) &&
        is.numeric(x) &&
        is.null(dim(x)) &&
        !is.object(x)
}

.mojor_dist_fast_scalar_numeric <- function(x) {
    is.atomic(x) &&
        is.numeric(x) &&
        length(x) ==
            1L && !is.object(x)
}

.mojor_dist_fast_scalar_logical <- function(x) {
    is.logical(x) &&
        length(x) ==
            1L && !is.na(x)
}

# Fast path for common vector calls while preserving full stats::
# behavior for recycling, non-scalar params, and edge-case argument
# handling.
mojor_dnorm <- function(x, mean = 0, sd = 1, log = FALSE) {
    if (length(x) <
        .mojor_dist_fast_min_n) {
        return(.mojor_stats_dnorm(x, mean = mean, sd = sd, log = log))
    }
    use_fast <- .mojor_dist_fast_numeric_vector(x) &&
        .mojor_dist_fast_scalar_numeric(mean) &&
        .mojor_dist_fast_scalar_numeric(sd) &&
        .mojor_dist_fast_scalar_logical(log) &&
        mojor_is_loaded()
    if (use_fast) {
        fast_out <- tryCatch(
            .mojor_call_bridge(
                "mojor_dnorm_fast", as.double(x),
                as.double(mean),
                as.double(sd),
                log
            ),
            error = function(e) NULL
        )
        if (!is.null(fast_out)) {
            return(fast_out)
        }
    }
    .mojor_stats_dnorm(x, mean = mean, sd = sd, log = log)
}

mojor_dpois <- function(x, lambda = 1, log = FALSE) {
    if (length(x) <
        .mojor_dist_fast_min_n) {
        return(.mojor_stats_dpois(x, lambda = lambda, log = log))
    }
    use_fast <- .mojor_dist_fast_numeric_vector(x) &&
        .mojor_dist_fast_scalar_numeric(lambda) &&
        .mojor_dist_fast_scalar_logical(log) &&
        mojor_is_loaded()
    if (use_fast) {
        fast_out <- tryCatch(
            .mojor_call_bridge(
                "mojor_dpois_fast", as.double(x),
                as.double(lambda),
                log
            ),
            error = function(e) NULL
        )
        if (!is.null(fast_out)) {
            return(fast_out)
        }
    }
    .mojor_stats_dpois(x, lambda = lambda, log = log)
}
