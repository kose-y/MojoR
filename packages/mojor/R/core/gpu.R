.mojor_fast_math_flags <- function(fast_math = NULL) {
    use_fast <- if (is.null(fast_math))
        isTRUE(.mojor_state$options$fast_math) else isTRUE(fast_math)
    if (!use_fast) {
        return(character(0))
    }
    flag <- .mojor_state$options$fast_math_flag
    if (!is.null(flag) &&
        nzchar(flag)) {
        return(flag)
    }
    if (!isTRUE(.mojor_state$fast_math_warned)) {
        warning(
            "mojor: fast_math requested but no default mojo fast_math flag is configured; set fast_math_flag to override",
            call. = FALSE
        )
        .mojor_state$fast_math_warned <- TRUE
    }
    character(0)
}

# GPUArray object backend: R6 reference objects (replaces S4/list
# wrappers).
if (!exists(".mojor_gpu_array_r6_class", inherits = FALSE)) {
    .mojor_gpu_array_r6_class <- R6::R6Class(
        "mojor_gpu_array", public = list(
            handle = NULL, data = NULL, n = 0L, handle_epoch = 0L, api = "metal",
            dtype = NULL, dim = NULL, dimnames = NULL, index_plans = NULL,
            strides = NULL, initialize = function(
                handle = NULL, data = NULL, n = 0L, handle_epoch = 0L,
                api = "metal", dtype = NULL, dim = NULL, dimnames = NULL,
                index_plans = NULL, strides = NULL
            ) {
                attr(self, ".mojor_gpu_initializing") <- TRUE
                on.exit(attr(self, ".mojor_gpu_initializing") <- NULL, add = TRUE)
                self$handle <- handle
                self$data <- data
                self$n <- as.integer(n)
                self$handle_epoch <- as.integer(handle_epoch)
                self$api <- as.character(api)
                self$dtype <- dtype
                self$dim <- if (is.null(dim)) NULL else as.integer(dim)
                self$dimnames <- dimnames
                self$index_plans <- if (is.null(index_plans) ||
                  !is.environment(index_plans)) new.env(parent = emptyenv()) else index_plans
                self$strides <- if (is.null(strides)) NULL else as.integer(strides)
            }
        )
    )
}
if (!exists(".mojor_gpu_gpuarray_r6_class", inherits = FALSE)) {
    .mojor_gpu_gpuarray_r6_class <- R6::R6Class(
        "GPUArray", inherit = .mojor_gpu_array_r6_class, public = list(
            initialize = function(...) {
                super$initialize(...)
            }
        )
    )
}
if (!exists(".mojor_gpu_session_r6_class", inherits = FALSE)) {
    .mojor_gpu_session_r6_class <- R6::R6Class(
        "mojor_gpu_session", public = list(
            data = NULL, n = 0L, status = 0L, initialize = function(data = NULL, n = 0L, status = 0L) {
                self$data <- data
                self$n <- as.integer(n)
                self$status <- as.integer(status)
            }
        )
    )
}

.mojor_gpu_object_get <- function(obj, field, default = NULL) {
    if (is.environment(obj) &&
        exists(field, envir = obj, inherits = FALSE)) {
        return(get(field, envir = obj, inherits = FALSE))
    }
    default
}

.mojor_gpu_object_set <- function(obj, field, value) {
    if (is.environment(obj)) {
        assign(field, value, envir = obj)
        return(obj)
    }
    obj
}

.mojor_gpu_route_label <- function(route) {
    if (is.null(route) ||
        length(route) < 1L ||
        is.na(route[[1L]])) {
        return(NULL)
    }
    route_chr <- trimws(as.character(route[[1L]]))
    if (!nzchar(route_chr)) {
        return(NULL)
    }
    route_chr
}

.mojor_gpu_reject_cpu_fallback_enabled <- function() {
    isTRUE(mojor_options("gpu_reject_fallback")$gpu_reject_fallback)
}

.mojor_gpu_enforce_cpu_fallback_policy <- function(route, reason = NULL, reason_code = NULL) {
    route_chr <- .mojor_gpu_route_label(route)
    if (is.null(route_chr) ||
        !startsWith(route_chr, "cpu_") ||
        !isTRUE(.mojor_gpu_reject_cpu_fallback_enabled())) {
        return(invisible(NULL))
    }
    msg <- paste0("mojor gpu fallback rejected for route ", route_chr)
    if (!is.null(reason_code) &&
        length(reason_code) >= 1L &&
        !is.na(reason_code[[1L]])) {
        code_chr <- trimws(as.character(reason_code[[1L]]))
        if (nzchar(code_chr)) {
            msg <- paste0(msg, " [", code_chr, "]")
        }
    }
    if (!is.null(reason) &&
        length(reason) >= 1L &&
        !is.na(reason[[1L]])) {
        reason_chr <- trimws(as.character(reason[[1L]]))
        if (nzchar(reason_chr)) {
            msg <- paste0(msg, ": ", reason_chr)
        }
    }
    stop(msg, call. = FALSE)
}

.mojor_gpu_warn_cpu_fallback <- function(route, reason = NULL, reason_code = NULL) {
    route_chr <- .mojor_gpu_route_label(route)
    if (is.null(route_chr) ||
        !startsWith(route_chr, "cpu_")) {
        return(invisible(NULL))
    }
    msg <- paste0("mojor gpu fallback to CPU via ", route_chr)
    if (!is.null(reason_code) &&
        length(reason_code) >= 1L &&
        !is.na(reason_code[[1L]])) {
        code_chr <- trimws(as.character(reason_code[[1L]]))
        if (nzchar(code_chr)) {
            msg <- paste0(msg, " [", code_chr, "]")
        }
    }
    if (!is.null(reason) &&
        length(reason) >= 1L &&
        !is.na(reason[[1L]])) {
        reason_chr <- trimws(as.character(reason[[1L]]))
        if (nzchar(reason_chr)) {
            msg <- paste0(msg, ": ", reason_chr)
        }
    }
    warning(msg, call. = FALSE)
    invisible(NULL)
}

.mojor_gpu_route_tag <- function(x, route, reason = NULL, reason_code = NULL) {
    reason_chr <- NULL
    if (!is.null(reason) &&
        length(reason) >= 1L &&
        !is.na(reason[[1L]])) {
        reason_chr <- trimws(as.character(reason[[1L]]))
        if (!nzchar(reason_chr)) {
            reason_chr <- NULL
        }
    }
    reason_code_chr <- NULL
    if (!is.null(reason_code) &&
        length(reason_code) >= 1L &&
        !is.na(reason_code[[1L]])) {
        reason_code_chr <- trimws(as.character(reason_code[[1L]]))
        if (!nzchar(reason_code_chr)) {
            reason_code_chr <- NULL
        }
    }
    .mojor_gpu_enforce_cpu_fallback_policy(route, reason_chr, reason_code_chr)
    attr(x, "gpu_route") <- route
 # Keep compatibility for existing callers/tests that inspect this
 # attribute.
    attr(x, "gpu_fallback") <- route
    attr(x, "gpu_route_reason") <- reason_chr
    attr(x, "gpu_fallback_reason") <- reason_chr
    attr(x, "gpu_route_reason_code") <- reason_code_chr
    attr(x, "gpu_fallback_reason_code") <- reason_code_chr
    .mojor_gpu_warn_cpu_fallback(route, reason_chr, reason_code_chr)
    x
}

.mojor_gpu_route_value <- function(x) {
    route <- attr(x, "gpu_route", exact = TRUE)
    if (!is.null(route)) {
        return(route)
    }
    attr(x, "gpu_fallback", exact = TRUE)
}

.mojor_gpu_route_reason_value <- function(x) {
    reason <- attr(x, "gpu_route_reason", exact = TRUE)
    if (!is.null(reason)) {
        return(reason)
    }
    attr(x, "gpu_fallback_reason", exact = TRUE)
}

.mojor_gpu_route_reason_code_value <- function(x) {
    code <- attr(x, "gpu_route_reason_code", exact = TRUE)
    if (!is.null(code)) {
        return(code)
    }
    attr(x, "gpu_fallback_reason_code", exact = TRUE)
}

.mojor_gpu_clear_route_attrs <- function(x) {
    attr(x, "gpu_route") <- NULL
    attr(x, "gpu_fallback") <- NULL
    attr(x, "gpu_route_reason") <- NULL
    attr(x, "gpu_fallback_reason") <- NULL
    attr(x, "gpu_route_reason_code") <- NULL
    attr(x, "gpu_fallback_reason_code") <- NULL
    x
}

.mojor_gpu_copy_on_modify_enabled <- function() {
    isTRUE(getOption("mojor.gpu.copy_on_modify", FALSE))
}

.mojor_gpu_copy_on_modify_marker <- function() {
    ".mojor_gpu_copy_on_modify_prepared"
}

.mojor_gpu_copy_on_modify_is_prepared <- function(x) {
    isTRUE(attr(x, .mojor_gpu_copy_on_modify_marker(), exact = TRUE))
}

.mojor_gpu_copy_on_modify_mark <- function(x) {
    attr(x, .mojor_gpu_copy_on_modify_marker()) <- TRUE
    x
}

.mojor_gpu_copy_on_modify_clear <- function(x) {
    attr(x, .mojor_gpu_copy_on_modify_marker()) <- NULL
    invisible(x)
}

.mojor_gpu_copy_on_modify_clone <- function(x) {
    if (!inherits(x, "mojor_gpu_array")) {
        return(x)
    }
    out <- .mojor_gpu_clone_device_via_gather(x, route_label = "gpu_copy_on_modify")
    if (inherits(out, "mojor_gpu_array")) {
        return(.mojor_gpu_clear_route_attrs(out))
    }
    host <- mojor_gpu_array_read(x)
    out <- mojor_gpu_array(
        host,
        api = .mojor_gpu_array_api(x),
        dtype = .mojor_gpu_array_dtype(x)
    )
    .mojor_gpu_clear_route_attrs(out)
}

.mojor_gpu_copy_on_modify_prepare <- function(x) {
    if (!isTRUE(.mojor_gpu_copy_on_modify_enabled())) {
        return(x)
    }
    if (!inherits(x, "mojor_gpu_array")) {
        return(x)
    }
    if (isTRUE(attr(x, ".mojor_gpu_initializing", exact = TRUE))) {
        return(x)
    }
    if (isTRUE(.mojor_gpu_copy_on_modify_is_prepared(x))) {
        return(x)
    }
    .mojor_gpu_copy_on_modify_mark(.mojor_gpu_copy_on_modify_clone(x))
}

.mojor_gpu_new_object <- function(payload, class = "mojor_gpu_array") {
    class <- as.character(class)
    primary <- if ("GPUArray" %in% class)
        "GPUArray" else class[[1]]
    payload <- as.list(payload)
    if (identical(primary, "GPUArray")) {
        return(do.call(.mojor_gpu_gpuarray_r6_class$new, payload))
    }
    if (identical(primary, "mojor_gpu_array")) {
        return(do.call(.mojor_gpu_array_r6_class$new, payload))
    }
    if (identical(primary, "mojor_gpu_session")) {
        return(do.call(.mojor_gpu_session_r6_class$new, payload))
    }
    stop(".mojor_gpu_new_object: unsupported class '", primary, "'")
}

mojor_gpu_check_release <- function(fn, ...) {
    if (!is.function(fn))
        stop("mojor_gpu_check_release: fn must be a function")
    if (!mojor_is_loaded()) {
        out1 <- fn(...)
        out2 <- fn(...)
        return(invisible(list(first = out1, second = out2)))
    }
    before <- mojor_gpu_buf_f32_live_count()
    out1 <- fn(...)
    after1 <- mojor_gpu_buf_f32_live_count()
    if (!identical(after1, before)) {
        stop(
            "mojor_gpu_check_release: live GPU buffer count changed after first call",
            call. = FALSE
        )
    }
    out2 <- fn(...)
    after2 <- mojor_gpu_buf_f32_live_count()
    if (!identical(after2, before)) {
        stop(
            "mojor_gpu_check_release: live GPU buffer count changed after second call",
            call. = FALSE
        )
    }
    invisible(list(first = out1, second = out2))
}

.mojor_gpu_buf_info <- function(dtype) {
    dtype <- match.arg(dtype, c("f32", "f64", "i32"))
    if (dtype == "f32") {
        return(
            list(
                dtype = "f32", class = "mojor_gpu_buf_f32", label = "mojor_gpu_buf_f32",
                bytes_per = 4L, alloc = "mojor_gpu_buf_f32_alloc", free = "mojor_gpu_buf_f32_free",
                read = "mojor_gpu_buf_f32_read", write = "mojor_gpu_buf_f32_write",
                len = "mojor_gpu_buf_f32_len"
            )
        )
    }
    if (dtype == "f64") {
        return(
            list(
                dtype = "f64", class = "mojor_gpu_buf_f64", label = "mojor_gpu_buf_f64",
                bytes_per = 8L, alloc = "mojor_gpu_buf_f64_alloc", free = "mojor_gpu_buf_f64_free",
                read = "mojor_gpu_buf_f64_read", write = "mojor_gpu_buf_f64_write",
                len = "mojor_gpu_buf_f64_len"
            )
        )
    }
    list(
        dtype = "i32", class = "mojor_gpu_buf_i32", label = "mojor_gpu_buf_i32",
        bytes_per = 4L, alloc = "mojor_gpu_buf_i32_alloc", free = "mojor_gpu_buf_i32_free",
        read = "mojor_gpu_buf_i32_read", write = "mojor_gpu_buf_i32_write",
        len = "mojor_gpu_buf_i32_len"
    )
}

.mojor_gpu_cast_values <- function(values, dtype) {
    dtype <- match.arg(dtype, c("f32", "f64", "i32"))
    if (identical(dtype, "i32")) {
        return(as.integer(values))
    }
    as.numeric(values)
}

.mojor_gpu_buf_new <- function(dtype, x = NULL, n = NULL, api = c("auto", "metal", "cuda", "amd")) {
    info <- .mojor_gpu_buf_info(dtype)
    .mojor_gpu_api_resolve(api)
    if (!mojor_is_loaded()) {
        stop(info$label, ": Mojo backend not loaded")
    }
    if (!mojor_has_gpu()) {
        stop(info$label, ": GPU not available")
    }
    if (!is.null(x)) {
        x <- .mojor_gpu_cast_values(x, dtype)
        n <- length(x)
    } else if (is.null(n)) {
        stop(info$label, ": provide x or n")
    }
    n <- as.integer(n)
    .mojor_gpu_check_limit(
        paste0(info$label, "_alloc"),
        n, buffers = 1L, bytes_per = info$bytes_per
    )
    buf <- .mojor_call_bridge(info$alloc, .mojor_gpu_ctx_get(), n)
    class(buf) <- c(info$class, class(buf))
    if (!is.null(x) &&
        n > 0L) {
        .mojor_gpu_buf_write(info, buf, x)
    }
    buf
}

.mojor_gpu_buf_write <- function(info, buf, values) {
    if (!inherits(buf, info$class)) {
        stop(info$label, "_write: buf must be ", info$class)
    }
    values <- .mojor_gpu_cast_values(values, info$dtype)
    .mojor_call_bridge(info$write, buf, values)
    invisible(buf)
}

.mojor_gpu_buf_read <- function(info, buf) {
    if (!inherits(buf, info$class)) {
        stop(info$label, "_read: buf must be ", info$class)
    }
    .mojor_call_bridge(info$read, buf)
}

.mojor_gpu_buf_free <- function(info, buf) {
    if (!inherits(buf, info$class)) {
        stop(info$label, "_free: buf must be ", info$class)
    }
    .mojor_call_bridge(info$free, buf)
    invisible(TRUE)
}

.mojor_gpu_buf_print <- function(info, x) {
    n <- attr(x, "n")
    if (is.null(n))
        n <- .mojor_call_bridge(info$len, x)
    cat(info$label, "<", n, ">\n", sep = "")
    invisible(x)
}

.mojor_gpu_buf_register_dtype <- function(
    dtype, with_live_count = FALSE, with_as_integer = FALSE, numeric_cast = FALSE,
    env = parent.frame()
) {
    info <- .mojor_gpu_buf_info(dtype)
    base_name <- info$label

    assign(
        base_name, eval(
            bquote(
                function(x = NULL, n = NULL, api = c("auto", "metal", "cuda", "amd")) {
                  .mojor_gpu_buf_new(
                    .(dtype),
                    x = x, n = n, api = api
                )
                }
            )
        ),
        envir = env
    )
    assign(
        paste0(base_name, "_write"),
        eval(
            bquote(
                function(buf, values) {
                  .mojor_gpu_buf_write(
                    .mojor_gpu_buf_info(.(dtype)),
                    buf, values
                )
                }
            )
        ),
        envir = env
    )
    assign(
        paste0(base_name, "_read"),
        eval(
            bquote(
                function(buf) {
                  .mojor_gpu_buf_read(
                    .mojor_gpu_buf_info(.(dtype)),
                    buf
                )
                }
            )
        ),
        envir = env
    )
    assign(
        paste0(base_name, "_free"),
        eval(
            bquote(
                function(buf) {
                  .mojor_gpu_buf_free(
                    .mojor_gpu_buf_info(.(dtype)),
                    buf
                )
                }
            )
        ),
        envir = env
    )
    assign(
        paste0("print.", info$class),
        eval(
            bquote(
                function(x, ...) {
                  .mojor_gpu_buf_print(
                    .mojor_gpu_buf_info(.(dtype)),
                    x
                )
                }
            )
        ),
        envir = env
    )

    if (isTRUE(with_live_count)) {
        assign(
            paste0(base_name, "_live_count"),
            function() .mojor_call_bridge(paste0(base_name, "_live_count")),
            envir = env
        )
    }

    assign(
        paste0("as.numeric.", info$class),
        if (isTRUE(numeric_cast)) {
            eval(
                bquote(
                  function(x, ...) {
                    as.numeric(
                      .mojor_gpu_buf_read(
                        .mojor_gpu_buf_info(.(dtype)),
                        x
                    )
                  )
                  }
              )
            )
        } else {
            eval(
                bquote(
                  function(x, ...) {
                    .mojor_gpu_buf_read(
                      .mojor_gpu_buf_info(.(dtype)),
                      x
                  )
                  }
              )
            )
        }, envir = env
    )

    if (isTRUE(with_as_integer)) {
        assign(
            paste0("as.integer.", info$class),
            eval(
                bquote(
                  function(x, ...) {
                    .mojor_gpu_buf_read(
                      .mojor_gpu_buf_info(.(dtype)),
                      x
                  )
                  }
              )
            ),
            envir = env
        )
    }

    invisible(NULL)
}

.mojor_gpu_buf_register_dtype("f32", with_live_count = TRUE)
.mojor_gpu_buf_register_dtype("f64")
.mojor_gpu_buf_register_dtype("i32", with_as_integer = TRUE, numeric_cast = TRUE)

.mojor_gpu_api_override <- function() {
    opt <- getOption("mojor_gpu_api", NA_character_)
    if (!is.na(opt) &&
        nzchar(opt)) {
        return(tolower(as.character(opt)))
    }
    env <- Sys.getenv("MOJOR_GPU_API", "")
    if (nzchar(env)) {
        return(tolower(env))
    }
    ""
}

.mojor_gpu_linux_api_detect <- local({
    cached <- NULL
    function(force_refresh = FALSE) {
        if (!isTRUE(force_refresh) &&
            is.character(cached) &&
            length(cached) == 1L &&
            nzchar(cached)) {
            return(cached)
        }
        api <- "cuda"
        if (nzchar(Sys.which("nvidia-smi"))) {
            api <- "cuda"
        } else if (
            nzchar(Sys.which("rocm-smi")) ||
            nzchar(Sys.getenv("ROCM_PATH", "")) ||
            nzchar(Sys.getenv("ROCM_HOME", "")) ||
            file.exists("/dev/kfd")
        ) {
            api <- "amd"
        } else {
            lspci <- Sys.which("lspci")
            if (nzchar(lspci)) {
                out <- tryCatch(
                    system2(lspci, stdout = TRUE, stderr = FALSE),
                    error = function(e) character(0)
                )
                pci <- paste(out, collapse = "\n")
                if (grepl("NVIDIA", pci, ignore.case = TRUE)) {
                    api <- "cuda"
                } else if (grepl("AMD|Advanced Micro Devices|Radeon|ATI", pci, ignore.case = TRUE)) {
                    api <- "amd"
                }
            }
        }
        cached <<- api
        api
    }
})

.mojor_gpu_api_platform_default <- function() {
    sysname <- tolower(as.character(Sys.info()[["sysname"]]))
    if (identical(sysname, "linux")) {
        return(.mojor_gpu_linux_api_detect())
    }
    "metal"
}

.mojor_gpu_api_resolve <- function(api) {
    api <- match.arg(api, c("auto", "metal", "cuda", "amd"))
    requested_api <- api
    default_api <- .mojor_gpu_api_platform_default()
    override <- .mojor_gpu_api_override()
    if (nzchar(override)) {
        if (override %in% c("auto", "metal", "cuda", "amd")) {
            api <- override
        } else {
            warning(
                "mojor: ignoring invalid MOJOR_GPU_API value: ", override,
                call. = FALSE
            )
        }
    }
    if (identical(api, "auto")) {
        api <- default_api
    }
    if (!identical(requested_api, "auto") &&
        !identical(api, default_api) &&
        !nzchar(override)) {
        warning(
            "mojor: API '", api, "' requested; ensure the backend was built with ",
            "MOJOR_GPU_API=", api, call. = FALSE
        )
    }
    api
}

.mojor_gpu_api_name <- function(api = "auto") {
    api_name <- tolower(as.character(api[[1L]]))
    if (!nzchar(api_name) ||
        is.na(api_name) ||
        identical(api_name, "auto")) {
        override <- .mojor_gpu_api_override()
        if (nzchar(override) &&
            override %in% c("metal", "cuda", "amd")) {
            api_name <- override
        } else {
            api_name <- .mojor_gpu_api_platform_default()
        }
    }
    if (!api_name %in% c("metal", "cuda", "amd")) {
        api_name <- .mojor_gpu_api_platform_default()
    }
    api_name
}

.mojor_gpu_force_f64_cpu_fallback <- function(api = "auto") {
    sysname <- tolower(as.character(Sys.info()[["sysname"]]))
    identical(sysname, "darwin") &&
        identical(.mojor_gpu_api_name(api), "metal")
}

.mojor_gpu_use_host_staged_index_route <- function(api, dtype = NULL) {
    api_name <- tolower(as.character(api[[1L]]))
    if (!api_name %in% c("cuda", "amd")) {
        return(FALSE)
    }
    tolower(Sys.getenv("MOJOR_GPU_UNSAFE_INDEX_KERNELS", unset = "")) %in% c("", "0", "false", "no", "off")
}

.mojor_gpu_array_handle <- function(buf) {
    if (inherits(buf, "mojor_gpu_buf_f32") ||
        inherits(buf, "mojor_gpu_buf_f64") ||
        inherits(buf, "mojor_gpu_buf_i32")) {
        return(buf)
    }
    handle <- .mojor_gpu_object_get(buf, "handle", default = NULL)
    if (!is.null(handle)) {
        return(handle)
    }
    NULL
}

.mojor_gpu_array_handle_epoch <- function(buf) {
    epoch <- .mojor_gpu_object_get(buf, "handle_epoch", default = NULL)
    if (is.null(epoch) ||
        length(epoch) !=
            1L || is.na(epoch)) {
        return(0L)
    }
    as.integer(epoch)
}

.mojor_gpu_idx_plan_symbols <- function(dtype) {
    dtype <- match.arg(dtype, c("f32", "f64"))
    list(
        dtype = dtype, class = paste0("mojor_gpu_idx_plan_", dtype),
        create = paste0("mojor_gpu_idx_plan_", dtype, "_create"),
        free = paste0("mojor_gpu_idx_plan_", dtype, "_free"),
        gather = paste0("mojor_gpu_buf_", dtype, "_gather_plan"),
        scatter = paste0("mojor_gpu_buf_", dtype, "_scatter_plan")
    )
}

.mojor_gpu_idx_plan_symbols_from_array <- function(x) {
    dtype <- .mojor_gpu_array_dtype(x)
    if (!dtype %in% c("f32", "f64")) {
        return(NULL)
    }
    .mojor_gpu_idx_plan_symbols(dtype)
}

.mojor_gpu_idx_plan_free <- function(plan) {
    if (inherits(plan, "mojor_gpu_idx_plan_f32")) {
        try(
            .mojor_call_bridge("mojor_gpu_idx_plan_f32_free", plan),
            silent = TRUE
        )
        return(invisible(NULL))
    }
    if (inherits(plan, "mojor_gpu_idx_plan_f64")) {
        try(
            .mojor_call_bridge("mojor_gpu_idx_plan_f64_free", plan),
            silent = TRUE
        )
        return(invisible(NULL))
    }
    invisible(NULL)
}

.mojor_gpu_array_clear_index_plans <- function(buf) {
    plans <- .mojor_gpu_object_get(buf, "index_plans", default = NULL)
    if (is.environment(plans)) {
        for (k in ls(plans, all.names = TRUE)) {
            entry <- get(k, envir = plans, inherits = FALSE)
            if (is.list(entry) &&
                !is.null(entry$plan)) {
                .mojor_gpu_idx_plan_free(entry$plan)
            }
        }
    }
    .mojor_gpu_object_set(buf, "index_plans", new.env(parent = emptyenv()))
}

.mojor_gpu_array_set_handle <- function(buf, handle, free_old = FALSE, invalidate_plans = TRUE) {
    old_handle <- .mojor_gpu_array_handle(buf)
    if (isTRUE(free_old) &&
        !is.null(old_handle) &&
        !identical(old_handle, handle)) {
        info <- .mojor_gpu_buf_info_from_handle(old_handle)
        try(
            .mojor_gpu_buf_free(info, old_handle),
            silent = TRUE
        )
    }
    buf <- .mojor_gpu_object_set(buf, "handle", handle)
    buf <- .mojor_gpu_object_set(
        buf, "handle_epoch", as.integer(
            .mojor_gpu_array_handle_epoch(buf) +
                1L
        )
    )
    if (isTRUE(invalidate_plans)) {
        buf <- .mojor_gpu_array_clear_index_plans(buf)
    }
    buf
}

.mojor_gpu_array_api <- function(buf) {
    api <- .mojor_gpu_object_get(buf, "api", default = NULL)
    if (!is.null(api)) {
        return(as.character(api))
    }
    "metal"
}

.mojor_dim_strides <- function(dim) {
    if (is.null(dim) ||
        length(dim) ==
            0) {
        return(NULL)
    }
    dim <- as.integer(dim)
    strides <- integer(length(dim))
    strides[1] <- 1L
    if (length(dim) >
        1L) {
        for (i in 2:length(dim)) {
            strides[i] <- strides[i - 1L] * dim[i - 1L]
        }
    }
    strides
}

.mojor_gpu_dtype_from_input <- function(x, dtype = c("auto", "f32", "f64", "i32")) {
    dtype <- match.arg(dtype)
    if (dtype != "auto") {
        return(dtype)
    }
    if (!is.null(x) &&
        .mojor_float_available() && float::is.float(x)) {
        return("f32")
    }
    "f64"
}

.mojor_gpu_array_dtype <- function(buf) {
    dtype <- .mojor_gpu_object_get(buf, "dtype", default = NULL)
    if (!is.null(dtype)) {
        return(dtype)
    }
    handle <- .mojor_gpu_array_handle(buf)
    if (inherits(handle, "mojor_gpu_buf_i32")) {
        return("i32")
    }
    if (inherits(handle, "mojor_gpu_buf_f64")) {
        return("f64")
    }
    "f32"
}

.mojor_gpu_buf_info_from_dtype <- function(dtype) {
    .mojor_gpu_buf_info(match.arg(dtype, c("f32", "f64", "i32")))
}

.mojor_gpu_value_dtype <- function(x) {
    if (inherits(x, "GPUArray") ||
        inherits(x, "mojor_gpu_array")) {
        return(.mojor_gpu_array_dtype(x))
    }
    if (!is.null(x) &&
        .mojor_float_available() && float::is.float(x)) {
        return("f32")
    }
    if (is.integer(x) &&
        !is.double(x)) {
        return("i32")
    }
    "f64"
}

.mojor_gpu_promote_dtype <- function(dtype_x, dtype_y) {
    dx <- match.arg(dtype_x, c("f32", "f64", "i32"))
    dy <- match.arg(dtype_y, c("f32", "f64", "i32"))
    if (identical(dx, "f64") ||
        identical(dy, "f64")) {
        return("f64")
    }
    if (identical(dx, "f32") ||
        identical(dy, "f32")) {
        return("f32")
    }
    "i32"
}

.mojor_gpu_broadcast_shape <- function(shape, fn_name = "gpu_broadcast") {
    if (!is.numeric(shape) ||
        length(shape) ==
            0L) {
        stop(fn_name, ": shape must be a non-empty integer vector")
    }
    shape_i <- as.integer(shape)
    if (anyNA(shape_i) ||
        any(shape_i <= 0L)) {
        stop(fn_name, ": shape entries must be positive integers")
    }
    shape_i
}

.mojor_gpu_broadcast_host <- function(host, out_dim, fn_name = "gpu_broadcast") {
    out_dim <- .mojor_gpu_broadcast_shape(out_dim, fn_name = fn_name)
    out_nd <- length(out_dim)

    src_dim <- dim(host)
    if (is.null(src_dim)) {
        src_dim <- as.integer(length(host))
    } else {
        src_dim <- as.integer(src_dim)
    }
    src_nd <- length(src_dim)
    if (src_nd > out_nd) {
        stop(fn_name, ": source rank exceeds target shape rank")
    }

    src_dim_pad <- c(
        rep.int(1L, out_nd - src_nd),
        src_dim
    )
    ok <- (src_dim_pad == out_dim) | (src_dim_pad == 1L)
    if (!all(ok)) {
        stop(
            fn_name, ": source shape is not broadcast-compatible with target shape"
        )
    }

    src_vals <- as.vector(host)
    src_strides <- .mojor_dim_strides(src_dim_pad)
    out_n <- as.integer(prod(out_dim))
    out_vals <- vector(
        mode = typeof(src_vals),
        length = out_n
    )

    for (oi in seq_len(out_n)) {
        rem <- as.integer(oi - 1L)
        src_off <- 0L
        for (axis in seq_len(out_nd)) {
            od <- out_dim[[axis]]
            coord <- if (od > 1L)
                rem%%od else 0L
            if (od > 1L)
                rem <- rem%/%od
            sd <- src_dim_pad[[axis]]
            src_coord <- if (sd == 1L)
                0L else coord
            src_off <- src_off + src_coord * src_strides[[axis]]
        }
        out_vals[[oi]] <- src_vals[[src_off + 1L]]
    }

    if (out_nd == 1L) {
        return(out_vals)
    }
    array(out_vals, dim = out_dim)
}

.mojor_gpu_buf_info_from_handle <- function(handle) {
    if (inherits(handle, "mojor_gpu_buf_i32")) {
        return(.mojor_gpu_buf_info("i32"))
    }
    if (inherits(handle, "mojor_gpu_buf_f64")) {
        return(.mojor_gpu_buf_info("f64"))
    }
    if (inherits(handle, "mojor_gpu_buf_f32")) {
        return(.mojor_gpu_buf_info("f32"))
    }
    stop(
        ".mojor_gpu_buf_info_from_handle: unsupported GPU buffer handle",
        call. = FALSE
    )
}

.mojor_gpu_buf_call <- function(
    dtype, op, ..., require = TRUE, as_handle = FALSE, require_label = NULL
) {
    info <- .mojor_gpu_buf_info_from_dtype(dtype)
    bridge <- paste0("mojor_gpu_buf_", info$dtype, "_", op)
    out <- .mojor_call_bridge(bridge, .mojor_gpu_ctx_get(), ...)
    if (isTRUE(require)) {
        label <- if (is.null(require_label))
            paste0("gpu_buf_", info$dtype, "_", op) else require_label
        .mojor_gpu_require(label, out)
    }
    if (isTRUE(as_handle)) {
        class(out) <- c(info$class, class(out))
    }
    out
}

.mojor_gpu_f64_matmul_mode <- function(m, n) {
    m_i <- as.integer(m)
    n_i <- as.integer(n)
    if (is.na(m_i) ||
        is.na(n_i) ||
        m_i <= 0L || n_i <= 0L) {
        return("matmul")
    }
    if (n_i == 1L) {
        return("gemv")
    }
    if (m_i == 1L) {
        return("gevm")
    }
    "matmul"
}

.mojor_gpu_probe_values_close <- function(observed, expected, tol = 1e-09) {
    obs <- as.numeric(observed)
    exp <- as.numeric(expected)
    if (length(obs) !=
        length(exp) ||
        any(!is.finite(obs)) ||
        any(!is.finite(exp))) {
        return(FALSE)
    }
    scale <- pmax(1, abs(exp))
    rel_err <- abs(obs - exp)/scale
    max(rel_err) <=
        tol
}

.mojor_gpu_f64_probe_mode <- function(force_refresh = FALSE) {
    env_mode <- tolower(trimws(Sys.getenv("MOJOR_GPU_F64_PROBE_MODE", "")))
    if (env_mode %in% c("inprocess", "subprocess")) {
        return(env_mode)
    }
    if (isTRUE(force_refresh)) {
        return("subprocess")
    }
    "inprocess"
}

.mojor_gpu_f64_probe_timeout_sec <- function() {
    env_timeout <- suppressWarnings(as.numeric(Sys.getenv("MOJOR_GPU_F64_PROBE_TIMEOUT_SEC", "")))
    if (is.finite(env_timeout) &&
        env_timeout > 0) {
        return(as.integer(max(1, ceiling(env_timeout))))
    }
    45L
}

.mojor_gpu_f64_probe_max_shapes <- function() {
    env_max <- suppressWarnings(as.integer(trimws(Sys.getenv("MOJOR_GPU_F64_PROBE_MAX_SHAPES", ""))))
    if (is.na(env_max) ||
        env_max < 1L) {
        return(1L)
    }
    env_max
}

.mojor_gpu_probe_mojor_script <- function() {
    candidates <- c(
        .mojor_find_upward(file.path("packages", "mojor", "R", "mojor.R")),
        .mojor_find_upward(file.path("R", "mojor.R"))
    )
    hits <- candidates[!vapply(candidates, is.null, logical(1L))]
    if (!length(hits)) {
        return(NULL)
    }
    hit <- hits[[1L]]
    if (!file.exists(hit)) {
        return(NULL)
    }
    normalizePath(hit, winslash = "/", mustWork = TRUE)
}

.mojor_gpu_probe_reason_text <- function(reason) {
    if (is.null(reason) ||
        !length(reason) ||
        is.na(reason[[1L]])) {
        return(NULL)
    }
    reason_chr <- trimws(as.character(reason[[1L]]))
    if (!nzchar(reason_chr)) {
        return(NULL)
    }
    reason_chr
}

.mojor_gpu_probe_reason_code <- function(reason) {
    msg <- .mojor_gpu_probe_reason_text(reason)
    if (is.null(msg)) {
        return(NULL)
    }
    if (grepl(
        "probe subprocess unavailable: mojor.R not found",
        msg,
        fixed = TRUE
    )) {
        return("subprocess_script_missing")
    }
    if (grepl(
        "probe subprocess unavailable: Rscript binary not found",
        msg,
        fixed = TRUE
    )) {
        return("subprocess_rscript_missing")
    }
    if (grepl("probe subprocess failed (status=", msg, fixed = TRUE)) {
        return("subprocess_failed")
    }
    if (grepl("probe returned invalid result shape", msg, fixed = TRUE)) {
        return("invalid_result_shape")
    }
    if (grepl("probe requires GPU context", msg, fixed = TRUE)) {
        return("probe_context_missing")
    }
    if (grepl("disabled on metal backend", msg, fixed = TRUE)) {
        return("disabled_on_metal_backend")
    }
    if (grepl("probe unavailable (", msg, fixed = TRUE)) {
        return("probe_unavailable")
    }
    if (grepl("capability precheck call failed:", msg, fixed = TRUE)) {
        return("capability_precheck_call_failed")
    }
    if (grepl(" capability precheck failed", msg, fixed = TRUE)) {
        return("capability_precheck_unavailable")
    }
    if (grepl("probe failed across", msg, fixed = TRUE)) {
        return("probe_validation_failed")
    }
    if (grepl("probe kernel call failed", msg, fixed = TRUE)) {
        return("probe_kernel_call_failed")
    }
    if (grepl("unexpected ", msg, fixed = TRUE) &&
        grepl("probe result", msg, fixed = TRUE)) {
        return("probe_unexpected_result")
    }
    if (grepl("unused argument", msg, fixed = TRUE)) {
        return("probe_signature_mismatch")
    }
    "probe_error"
}

.mojor_gpu_probe_result <- function(available, reason = NULL, code = NULL) {
    available_flag <- isTRUE(available)
    reason_chr <- .mojor_gpu_probe_reason_text(reason)
    code_chr <- .mojor_gpu_probe_reason_text(code)
    if (available_flag) {
        code_chr <- NULL
    } else if (is.null(code_chr)) {
        code_chr <- .mojor_gpu_probe_reason_code(reason_chr)
    }
    list(
        available = available_flag,
        reason = reason_chr,
        code = code_chr
    )
}

.mojor_gpu_probe_result_normalize <- function(res, label) {
    if (!is.list(res) ||
        is.null(res$available)) {
        return(.mojor_gpu_probe_result(
            available = FALSE,
            reason = paste0("f64 ", label, " probe returned invalid result shape"),
            code = "invalid_result_shape"
        ))
    }
    .mojor_gpu_probe_result(
        available = isTRUE(res$available),
        reason = res$reason,
        code = res$code
    )
}

.mojor_gpu_probe_run_subprocess <- function(
    kind = c("matmul", "reduce"),
    timeout_sec = .mojor_gpu_f64_probe_timeout_sec(), ...
) {
    kind <- match.arg(kind, c("matmul", "reduce"))
    timeout_sec <- as.integer(timeout_sec)
    if (is.na(timeout_sec) ||
        timeout_sec < 1L) {
        timeout_sec <- 45L
    }

    mojor_script <- .mojor_gpu_probe_mojor_script()
    if (is.null(mojor_script) ||
        !nzchar(mojor_script)) {
        return(.mojor_gpu_probe_result(
            available = FALSE,
            reason = "f64 probe subprocess unavailable: mojor.R not found",
            code = "subprocess_script_missing"
        ))
    }
    rscript_bin <- Sys.which("Rscript")
    if (!nzchar(rscript_bin)) {
        return(.mojor_gpu_probe_result(
            available = FALSE,
            reason = "f64 probe subprocess unavailable: Rscript binary not found",
            code = "subprocess_rscript_missing"
        ))
    }

    bridge_path <- .mojor_state$bridge_path
    mojor_lib_path <- Sys.getenv("MOJOR_LIB_PATH", "")
    if ((!nzchar(mojor_lib_path)) &&
        is.character(bridge_path) &&
        length(bridge_path) ==
            1L && nzchar(bridge_path)) {
        mojor_lib_path <- dirname(bridge_path)
    }

    probe_dir <- tempfile("mojor_gpu_probe_")
    dir.create(probe_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(
        unlink(probe_dir, recursive = TRUE, force = TRUE),
        add = TRUE
    )
    req_path <- file.path(probe_dir, "probe_request.rds")
    out_path <- file.path(probe_dir, "probe_result.rds")
    runner_path <- file.path(probe_dir, "probe_runner.R")

    saveRDS(
        list(
            kind = kind, params = list(...),
            mojor_script = mojor_script, mojor_lib_path = mojor_lib_path
        ),
        req_path
    )

    writeLines(
        c(
            "args <- commandArgs(trailingOnly = TRUE)", "if (length(args) < 2L) stop('probe runner requires request/result paths')",
            "req <- readRDS(args[[1L]])", "out_path <- args[[2L]]", "res <- list(available = FALSE, reason = 'probe runner did not execute')",
            "tryCatch({", "  source(req$mojor_script)", "  if (is.character(req$mojor_lib_path) && length(req$mojor_lib_path) == 1L && nzchar(req$mojor_lib_path)) {",
            "    if (!nzchar(Sys.getenv('MOJOR_LIB_PATH', ''))) Sys.setenv(MOJOR_LIB_PATH = req$mojor_lib_path)",
            "  }", "  if (!isTRUE(mojor_is_loaded())) try(mojor_load(), silent = TRUE)",
            "  if (!isTRUE(mojor_is_loaded()) || !isTRUE(mojor_has_gpu())) stop('GPU not available for f64 probe subprocess', call. = FALSE)",
            "  ctx <- .mojor_gpu_ctx_get()", "  kind <- as.character(req$kind[[1L]])",
            "  params <- req$params", "  if (identical(kind, 'matmul')) {",
            "    mode <- if (!is.null(params$mode)) as.character(params$mode[[1L]]) else 'matmul'",
            "    out <- tryCatch({", "      .mojor_gpu_probe_f64_matmul(ctx, mode = mode)",
            "    }, error = function(e) {", "      msg <- conditionMessage(e)",
            "      if (grepl('unused argument', msg, fixed = TRUE)) {",
            "        .mojor_gpu_probe_f64_matmul(ctx)", "      } else {",
            "        stop(e)", "      }", "    })", "  } else if (identical(kind, 'reduce')) {",
            "    dims_mode <- if (!is.null(params$dims_mode)) as.character(params$dims_mode[[1L]]) else 'scalar'",
            "    op_class <- if (!is.null(params$op_class)) as.character(params$op_class[[1L]]) else 'value'",
            "    out <- tryCatch({", "      .mojor_gpu_probe_f64_reduce(ctx, dims_mode = dims_mode, op_class = op_class)",
            "    }, error = function(e) {", "      msg <- conditionMessage(e)",
            "      if (grepl('unused argument', msg, fixed = TRUE)) {",
            "        .mojor_gpu_probe_f64_reduce(ctx, dims_mode = dims_mode)",
            "      } else {", "        stop(e)", "      }", "    })", "  } else {",
            "    stop('unsupported probe kind: ', kind)", "  }", "  if (!is.list(out) || is.null(out$available)) stop('probe subprocess produced invalid result')",
            "  res <- list(available = isTRUE(out$available), reason = out$reason, code = out$code)",
            "}, error = function(e) {", "  res <<- list(available = FALSE, reason = conditionMessage(e), code = 'subprocess_runner_error')",
            "})", "saveRDS(res, out_path)"
        ),
        con = runner_path
    )

    output <- tryCatch(
        system2(
            rscript_bin, c("--vanilla", runner_path, req_path, out_path),
            stdout = TRUE, stderr = TRUE, timeout = timeout_sec
        ),
        error = function(e) {
            structure(
                conditionMessage(e),
                status = 1L
            )
        }
    )
    status <- attr(output, "status")
    if (is.null(status)) {
        status <- 0L
    }

    if (file.exists(out_path)) {
        res <- tryCatch(
            readRDS(out_path),
            error = function(e) NULL
        )
        if (is.list(res) &&
            !is.null(res$available)) {
            return(.mojor_gpu_probe_result_normalize(res, kind))
        }
    }

    out_tail <- character()
    if (length(output)) {
        out_tail <- tail(
            as.character(output),
            3L
        )
    }
    reason_parts <- c(
        paste0(
            "f64 ", kind, " probe subprocess failed (status=", as.integer(status),
            ")"
        ),
        out_tail
    )
    .mojor_gpu_probe_result(
        available = FALSE,
        reason = paste(
            reason_parts[nzchar(reason_parts)],
            collapse = " | "
        ),
        code = "subprocess_failed"
    )
}

.mojor_gpu_probe_f64_matmul_dispatch <- function(
    ctx, mode = c("matmul", "gemv", "gevm"),
    force_refresh = FALSE
) {
    mode <- match.arg(mode, c("matmul", "gemv", "gevm"))
    probe_mode <- .mojor_gpu_f64_probe_mode(force_refresh = force_refresh)
    if (identical(probe_mode, "subprocess")) {
        return(.mojor_gpu_probe_run_subprocess("matmul", mode = mode))
    }
    if (missing(ctx) ||
        is.null(ctx)) {
        return(.mojor_gpu_probe_result(
            available = FALSE,
            reason = "f64 matmul probe requires GPU context",
            code = "probe_context_missing"
        ))
    }
    tryCatch(
        {
            .mojor_gpu_probe_f64_matmul(ctx, mode = mode)
        }, error = function(e) {
            msg <- conditionMessage(e)
            if (grepl("unused argument", msg, fixed = TRUE)) {
                .mojor_gpu_probe_f64_matmul(ctx)
            } else {
                .mojor_gpu_probe_result(
                    available = FALSE,
                    reason = msg,
                    code = "probe_dispatch_error"
                )
            }
        }
    )
}

.mojor_gpu_probe_f64_reduce_dispatch <- function(
    ctx, dims_mode = c("scalar", "dims"),
    op_class = c("value", "arg"),
    force_refresh = FALSE
) {
    dims_mode <- match.arg(dims_mode, c("scalar", "dims"))
    op_class <- match.arg(op_class, c("value", "arg"))
    probe_mode <- .mojor_gpu_f64_probe_mode(force_refresh = force_refresh)
    if (identical(probe_mode, "subprocess")) {
        return(
            .mojor_gpu_probe_run_subprocess("reduce", dims_mode = dims_mode, op_class = op_class)
        )
    }
    if (missing(ctx) ||
        is.null(ctx)) {
        return(.mojor_gpu_probe_result(
            available = FALSE,
            reason = "f64 reduce probe requires GPU context",
            code = "probe_context_missing"
        ))
    }
    tryCatch(
        {
            .mojor_gpu_probe_f64_reduce(ctx, dims_mode = dims_mode, op_class = op_class)
        }, error = function(e) {
            msg <- conditionMessage(e)
            if (grepl("unused argument", msg, fixed = TRUE)) {
                .mojor_gpu_probe_f64_reduce(ctx, dims_mode = dims_mode)
            } else {
                .mojor_gpu_probe_result(
                    available = FALSE,
                    reason = msg,
                    code = "probe_dispatch_error"
                )
            }
        }
    )
}

.mojor_gpu_f64_matmul_probe_configs <- function(
    mode = c("matmul", "gemv", "gevm"),
    max_shapes = NULL
) {
    mode <- match.arg(mode, c("matmul", "gemv", "gevm"))
    pairs <- switch(
        mode, gemv = list(
            list(
                a = matrix(
                  c(1, 2, 3, 4, 5, 6),
                  nrow = 3L, ncol = 2L
              ),
                b = matrix(
                  c(2, 5),
                  nrow = 2L, ncol = 1L
              )
            ),
            list(
                a = matrix(
                  c(1, 2, 3, 4, 5, 6, 7, 8),
                  nrow = 4L, ncol = 2L
              ),
                b = matrix(
                  c(3, 1),
                  nrow = 2L, ncol = 1L
              )
            )
        ),
        gevm = list(
            list(
                a = matrix(
                  c(2, 3, 4),
                  nrow = 1L, ncol = 3L
              ),
                b = matrix(
                  c(1, 4, 2, 5, 3, 6),
                  nrow = 3L, ncol = 2L
              )
            ),
            list(
                a = matrix(
                  c(1, 3, 5, 7),
                  nrow = 1L, ncol = 4L
              ),
                b = matrix(
                  c(2, 4, 6, 8, 1, 3, 5, 7),
                  nrow = 4L, ncol = 2L
              )
            )
        ),
        matmul = list(
            list(
                a = matrix(
                  c(1, 2, 3, 4),
                  nrow = 2L, ncol = 2L
              ),
                b = matrix(
                  c(5, 6, 7, 8),
                  nrow = 2L, ncol = 2L
              )
            ),
            list(
                a = matrix(
                  c(1, 4, 2, 5, 3, 6),
                  nrow = 3L, ncol = 2L
              ),
                b = matrix(
                  c(2, 1, 3, 5, 4, 6),
                  nrow = 2L, ncol = 3L
              )
            )
        )
    )

    configs <- lapply(
        pairs, function(pair) {
            a <- pair$a
            b <- pair$b
            expected <- a %*% b
            list(
                mode = mode, m = as.integer(nrow(a)),
                k = as.integer(ncol(a)),
                n = as.integer(ncol(b)),
                lhs = as.numeric(a),
                rhs = as.numeric(b),
                expected = as.numeric(expected)
            )
        }
    )

    if (is.null(max_shapes)) {
        return(configs)
    }
    max_shapes <- suppressWarnings(as.integer(max_shapes[1L]))
    if (is.na(max_shapes) ||
        max_shapes < 1L) {
        return(configs[1L])
    }
    configs[seq_len(
        min(
            length(configs),
            max_shapes
        )
    )]
}

.mojor_gpu_f64_matmul_probe_config <- function(mode = c("matmul", "gemv", "gevm")) {
    .mojor_gpu_f64_matmul_probe_configs(mode = mode)[[1L]]
}

.mojor_gpu_capability_precheck <- function(symbol, ..., reason_label = symbol) {
    args <- list(...)
    if (length(args) >= 1L &&
        !identical(typeof(args[[1L]]), "externalptr")) {
        return(list(checked = FALSE, available = TRUE, reason = NULL, code = NULL))
    }
    pkg <- tryCatch(
        .mojor_bridge_pkg_for_symbol(symbol),
        error = function(e) NULL
    )
    if (is.null(pkg) ||
        !isTRUE(is.loaded(symbol, PACKAGE = pkg))) {
        return(list(checked = FALSE, available = TRUE, reason = NULL, code = NULL))
    }
    out <- tryCatch(
        .mojor_runtime_dispatch_call(
            symbol = symbol,
            pkg = pkg,
            deterministic = TRUE,
            ...
        ),
        error = function(e) e
    )
    if (inherits(out, "error")) {
        return(
            list(
                checked = TRUE,
                available = FALSE,
                reason = paste0(
                    reason_label,
                    " capability precheck call failed: ",
                    conditionMessage(out)
                ),
                code = "capability_precheck_call_failed"
            )
        )
    }
    if (!isTRUE(out)) {
        return(
            list(
                checked = TRUE,
                available = FALSE,
                reason = paste0(reason_label, " capability precheck failed"),
                code = "capability_precheck_unavailable"
            )
        )
    }
    list(checked = TRUE, available = TRUE, reason = NULL, code = NULL)
}

.mojor_gpu_probe_f64_matmul <- function(ctx, mode = c("matmul", "gemv", "gevm")) {
    mode <- match.arg(mode, c("matmul", "gemv", "gevm"))
    cap_pre <- .mojor_gpu_capability_precheck(
        "mojor_gpu_cap_f64_matmul",
        ctx,
        reason_label = paste0("f64 ", mode)
    )
    if (!isTRUE(cap_pre$available)) {
        return(.mojor_gpu_probe_result(
            available = FALSE,
            reason = cap_pre$reason,
            code = cap_pre$code
        ))
    }
    probe_max_shapes <- .mojor_gpu_f64_probe_max_shapes()
    all_configs <- .mojor_gpu_f64_matmul_probe_configs(mode = mode)
    configs <- .mojor_gpu_f64_matmul_probe_configs(mode = mode, max_shapes = probe_max_shapes)
    total_shapes <- length(all_configs)
    attempted_shapes <- length(configs)

    probe_one <- function(cfg) {
        a <- NULL
        b <- NULL
        out <- NULL
        on.exit(
            {
                if (!is.null(out)) try(
                  .mojor_call_bridge("mojor_gpu_buf_f64_free", out),
                  silent = TRUE
              )
                if (!is.null(b)) try(
                  .mojor_call_bridge("mojor_gpu_buf_f64_free", b),
                  silent = TRUE
              )
                if (!is.null(a)) try(
                  .mojor_call_bridge("mojor_gpu_buf_f64_free", a),
                  silent = TRUE
              )
            }, add = TRUE
        )

        a <- .mojor_call_bridge("mojor_gpu_buf_f64_alloc", ctx, as.integer(length(cfg$lhs)))
        b <- .mojor_call_bridge("mojor_gpu_buf_f64_alloc", ctx, as.integer(length(cfg$rhs)))
        .mojor_call_bridge("mojor_gpu_buf_f64_write", a, cfg$lhs)
        .mojor_call_bridge("mojor_gpu_buf_f64_write", b, cfg$rhs)
        out <- .mojor_call_bridge(
            "mojor_gpu_buf_f64_matmul", ctx, a, b, cfg$m, cfg$k, cfg$n,
            0L, 0L
        )
        vals <- .mojor_call_bridge("mojor_gpu_buf_f64_read", out)
        if (!is.numeric(vals) ||
            !.mojor_gpu_probe_values_close(vals, cfg$expected, tol = 1e-09)) {
            stop(
                sprintf("unexpected f64 %s probe result", cfg$mode),
                call. = FALSE
            )
        }
        invisible(TRUE)
    }

    errors <- character()
    for (i in seq_along(configs)) {
        attempt <- tryCatch(
            {
                probe_one(configs[[i]])
                list(ok = TRUE, reason = NULL)
            }, error = function(e) {
                list(ok = FALSE, reason = conditionMessage(e))
            }
        )
        if (isTRUE(attempt$ok)) {
            return(.mojor_gpu_probe_result(available = TRUE, reason = NULL))
        }
        errors <- c(errors, attempt$reason)
    }

    .mojor_gpu_probe_result(
        available = FALSE,
        reason = paste0(
            "f64 ", mode, " probe failed across ", attempted_shapes, "/",
            total_shapes, " shapes: ", paste(
                unique(errors),
                collapse = " | "
            )
        ),
        code = "probe_validation_failed"
    )
}

.mojor_gpu_probe_i32_scatter <- function(ctx) {
    cap_pre <- .mojor_gpu_capability_precheck(
        "mojor_gpu_cap_i32_scatter",
        ctx,
        reason_label = "i32 scatter"
    )
    if (!isTRUE(cap_pre$available)) {
        return(.mojor_gpu_probe_result(
            available = FALSE,
            reason = cap_pre$reason,
            code = cap_pre$code
        ))
    }
    target <- NULL
    values <- NULL
    on.exit(
        {
            if (!is.null(values)) try(
                .mojor_call_bridge("mojor_gpu_buf_i32_free", values),
                silent = TRUE
            )
            if (!is.null(target)) try(
                .mojor_call_bridge("mojor_gpu_buf_i32_free", target),
                silent = TRUE
            )
        }, add = TRUE
    )

    res <- tryCatch(
        {
            target <- .mojor_call_bridge("mojor_gpu_buf_i32_alloc", ctx, 4L)
            values <- .mojor_call_bridge("mojor_gpu_buf_i32_alloc", ctx, 2L)
            .mojor_call_bridge("mojor_gpu_buf_i32_write", target, as.integer(c(0L, 0L, 0L, 0L)))
            .mojor_call_bridge("mojor_gpu_buf_i32_write", values, as.integer(c(11L, 22L)))
            ok <- .mojor_call_bridge(
                "mojor_gpu_buf_i32_scatter", ctx, target, values, as.integer(c(2L, 4L)),
                as.integer(0L),
                as.integer(2L),
                as.integer(4L)
            )
            if (!isTRUE(ok)) {
                stop("i32 scatter probe kernel call failed", call. = FALSE)
            }
            host <- .mojor_call_bridge("mojor_gpu_buf_i32_read", target)
            if (!is.numeric(host) ||
                length(host) !=
                  4L || !identical(
                as.integer(host),
                as.integer(c(0L, 11L, 0L, 22L))
            )) {
                stop("unexpected i32 scatter probe result", call. = FALSE)
            }
            list(available = TRUE, reason = NULL, code = NULL)
        }, error = function(e) {
            list(available = FALSE, reason = conditionMessage(e), code = NULL)
        }
    )

    .mojor_gpu_probe_result(
        available = res$available,
        reason = res$reason,
        code = res$code
    )
}

.mojor_gpu_cache_stats_defaults <- function() {
    list(
        lookups = 0L,
        hits = 0L,
        misses = 0L,
        negative_hits = 0L,
        stores = 0L
    )
}

.mojor_gpu_cache_stats_get <- function(field) {
    stats <- .mojor_state[[field]]
    defaults <- .mojor_gpu_cache_stats_defaults()
    if (!is.list(stats)) {
        stats <- defaults
    }
    for (nm in names(defaults)) {
        val <- stats[[nm]]
        val_i <- suppressWarnings(
            as.integer(if (is.null(val) || length(val) == 0L)
                NA_integer_ else val[[1L]])
        )
        if (is.null(val) || length(val) == 0L || is.na(val_i) || val_i < 0L) {
            stats[[nm]] <- defaults[[nm]]
        } else {
            stats[[nm]] <- val_i
        }
    }
    stats
}

.mojor_gpu_cache_stats_touch <- function(field, event = c("lookup", "hit", "miss", "negative_hit", "store")) {
    event <- match.arg(event, c("lookup", "hit", "miss", "negative_hit", "store"))
    stats <- .mojor_gpu_cache_stats_get(field)
    key <- switch(
        event,
        lookup = "lookups",
        hit = "hits",
        miss = "misses",
        negative_hit = "negative_hits",
        store = "stores"
    )
    stats[[key]] <- as.integer(stats[[key]] + 1L)
    .mojor_state[[field]] <- stats
    invisible(stats)
}

.mojor_gpu_cache_stats_reset <- function(field) {
    stats <- .mojor_gpu_cache_stats_defaults()
    .mojor_state[[field]] <- stats
    invisible(stats)
}

.mojor_gpu_capability_cache_lookup <- function(cache, key, ctx, ctx_epoch) {
    .mojor_gpu_cache_stats_touch("gpu_capability_cache_stats", "lookup")
    cached <- cache[[key]]
    if (!is.list(cached) ||
        is.null(cached$available) ||
        !identical(cached$ctx, ctx)) {
        .mojor_gpu_cache_stats_touch("gpu_capability_cache_stats", "miss")
        return(NULL)
    }
    cached_epoch <- cached$ctx_epoch
    if (is.null(cached_epoch) ||
        length(cached_epoch) !=
            1L || is.na(cached_epoch)) {
        .mojor_gpu_cache_stats_touch("gpu_capability_cache_stats", "miss")
        return(NULL)
    }
    if (!identical(
        as.integer(cached_epoch),
        as.integer(ctx_epoch)
    )) {
        .mojor_gpu_cache_stats_touch("gpu_capability_cache_stats", "miss")
        return(NULL)
    }
    .mojor_gpu_cache_stats_touch("gpu_capability_cache_stats", "hit")
    cached
}

.mojor_gpu_capability_cache_store <- function(
    cache, key, ctx, ctx_epoch, available, reason = NULL,
    reason_code = NULL
) {
    cache[[key]] <- list(
        ctx = ctx, ctx_epoch = as.integer(ctx_epoch),
        available = isTRUE(available),
        reason = reason,
        reason_code = reason_code
    )
    .mojor_gpu_cache_stats_touch("gpu_capability_cache_stats", "store")
    cache
}

.mojor_gpu_capability_cache_diag <- function(include_entries = TRUE, reset = FALSE) {
    cache <- .mojor_state$gpu_capability_cache
    if (!is.list(cache)) {
        cache <- list()
    }
    stats <- .mojor_gpu_cache_stats_get("gpu_capability_cache_stats")
    entries <- data.frame(
        key = character(),
        available = logical(),
        ctx_epoch = integer(),
        reason_code = character(),
        reason = character(),
        stringsAsFactors = FALSE
    )
    if (isTRUE(include_entries) && length(cache) > 0L) {
        rows <- lapply(names(cache), function(key) {
            row <- cache[[key]]
            if (!is.list(row)) {
                return(NULL)
            }
            epoch_val <- NA_integer_
            if (!is.null(row$ctx_epoch) && length(row$ctx_epoch) > 0L) {
                epoch_val <- suppressWarnings(as.integer(row$ctx_epoch[[1L]]))
            }
            reason_code_val <- ""
            if (!is.null(row$reason_code) && length(row$reason_code) > 0L) {
                reason_code_val <- as.character(row$reason_code[[1L]])
            }
            reason_val <- ""
            if (!is.null(row$reason) && length(row$reason) > 0L) {
                reason_val <- as.character(row$reason[[1L]])
            }
            data.frame(
                key = as.character(key),
                available = isTRUE(row$available),
                ctx_epoch = epoch_val,
                reason_code = reason_code_val,
                reason = reason_val,
                stringsAsFactors = FALSE
            )
        })
        rows <- Filter(Negate(is.null), rows)
        if (length(rows) > 0L) {
            entries <- do.call(rbind, rows)
            entries <- entries[order(entries$key), , drop = FALSE]
        }
    }
    if (isTRUE(reset)) {
        .mojor_gpu_cache_stats_reset("gpu_capability_cache_stats")
    }
    list(
        stats = stats,
        entries = entries
    )
}

.mojor_gpu_capability_cache_reason_detail <- function(
    cache, key, ctx = NULL, ctx_epoch = NULL, default_reason = NULL,
    default_code = NULL
) {
    cached <- NULL
    if (!is.null(cache) &&
        is.list(cache)) {
        if (!is.null(ctx) &&
            !is.null(ctx_epoch)) {
            cached <- .mojor_gpu_capability_cache_lookup(
                cache,
                key,
                ctx,
                ctx_epoch
            )
        } else {
            cached <- cache[[key]]
            if (!is.list(cached) ||
                is.null(cached$available)) {
                cached <- NULL
            }
        }
    }
    reason <- .mojor_gpu_probe_reason_text(
        if (is.list(cached))
            cached$reason else NULL
    )
    code <- .mojor_gpu_probe_reason_text(
        if (is.list(cached))
            cached$reason_code else NULL
    )
    if (is.null(reason)) {
        reason <- .mojor_gpu_probe_reason_text(default_reason)
    }
    if (is.null(code)) {
        code <- .mojor_gpu_probe_reason_text(default_code)
    }
    if (is.null(code)) {
        code <- .mojor_gpu_probe_reason_code(reason)
    }
    list(
        reason = reason,
        code = code
    )
}

.mojor_gpu_f64_matmul_probe_diag <- function(mode, ctx = NULL, ctx_epoch = NULL) {
    mode <- match.arg(mode, c("matmul", "gemv", "gevm"))
    cache <- .mojor_state$gpu_capability_cache
    .mojor_gpu_capability_cache_reason_detail(
        cache = cache,
        key = paste0("f64_matmul_", mode),
        ctx = ctx,
        ctx_epoch = ctx_epoch,
        default_reason = paste0(
            "f64 matmul probe unavailable (",
            mode,
            ")"
        ),
        default_code = "probe_unavailable"
    )
}

.mojor_gpu_f64_reduce_probe_diag <- function(
    dims_mode = c("scalar", "dims"),
    op_class = c("value", "arg"), ctx = NULL, ctx_epoch = NULL
) {
    dims_mode <- match.arg(dims_mode, c("scalar", "dims"))
    op_class <- match.arg(op_class, c("value", "arg"))
    cache <- .mojor_state$gpu_capability_cache
    .mojor_gpu_capability_cache_reason_detail(
        cache = cache,
        key = paste0("f64_reduce_", dims_mode, "_", op_class),
        ctx = ctx,
        ctx_epoch = ctx_epoch,
        default_reason = paste0(
            "f64 reduce probe unavailable (",
            dims_mode,
            ",",
            op_class,
            ")"
        ),
        default_code = "probe_unavailable"
    )
}

.mojor_gpu_f64_matmul_capable <- function(force_refresh = FALSE, mode = c("matmul", "gemv", "gevm")) {
    mode <- match.arg(mode, c("matmul", "gemv", "gevm"))
    if (!mojor_is_loaded() || !mojor_has_gpu()) {
        return(FALSE)
    }
    ctx <- tryCatch(.mojor_gpu_ctx_get(), error = function(e) NULL)
    if (is.null(ctx)) {
        return(FALSE)
    }
    ctx_epoch <- .mojor_gpu_ctx_epoch_get()
    cache <- .mojor_state$gpu_capability_cache
    if (is.null(cache) ||
        !is.list(cache)) {
        cache <- list()
    }
    cache_key <- paste0("f64_matmul_", mode)
    if (!isTRUE(force_refresh)) {
        cached <- .mojor_gpu_capability_cache_lookup(cache, cache_key, ctx, ctx_epoch)
        if (!is.null(cached)) {
            return(isTRUE(cached$available))
        }
    }
    probed <- .mojor_gpu_probe_f64_matmul_dispatch(ctx = ctx, mode = mode, force_refresh = force_refresh)
    probed <- .mojor_gpu_probe_result_normalize(probed, "matmul")
    cache <- .mojor_gpu_capability_cache_store(
        cache, cache_key, ctx, ctx_epoch, available = probed$available,
        reason = probed$reason,
        reason_code = probed$code
    )
    .mojor_state$gpu_capability_cache <- cache
    isTRUE(probed$available)
}

.mojor_gpu_probe_f64_reduce <- function(
    ctx, dims_mode = c("scalar", "dims"),
    op_class = c("value", "arg")
) {
    dims_mode <- match.arg(dims_mode, c("scalar", "dims"))
    op_class <- match.arg(op_class, c("value", "arg"))
    op_code <- if (identical(op_class, "arg"))
        as.integer(6L) else as.integer(0L)
    cap_pre <- .mojor_gpu_capability_precheck(
        "mojor_gpu_cap_f64_reduce",
        ctx,
        op_code,
        reason_label = paste0("f64 ", op_class, " reduce")
    )
    if (!isTRUE(cap_pre$available)) {
        return(.mojor_gpu_probe_result(
            available = FALSE,
            reason = cap_pre$reason,
            code = cap_pre$code
        ))
    }
    src <- NULL
    out <- NULL
    on.exit(
        {
            if (!is.null(out)) try(
                .mojor_call_bridge("mojor_gpu_buf_f64_free", out),
                silent = TRUE
            )
            if (!is.null(src)) try(
                .mojor_call_bridge("mojor_gpu_buf_f64_free", src),
                silent = TRUE
            )
        }, add = TRUE
    )

    res <- tryCatch(
        {
            src <- .mojor_call_bridge("mojor_gpu_buf_f64_alloc", ctx, 4L)
            .mojor_call_bridge("mojor_gpu_buf_f64_write", src, c(1, 2, 3, 4))
            dims_arg <- if (identical(dims_mode, "dims"))
                as.integer(c(-2L, 2L)) else NULL
            out <- .mojor_call_bridge(
                "mojor_gpu_buf_f64_reduce", ctx, src, op_code, dims_arg,
                as.integer(0L)
            )
            vals <- .mojor_call_bridge("mojor_gpu_buf_f64_read", out)
            if (!is.numeric(vals)) {
                stop("unexpected f64 reduce probe result", call. = FALSE)
            }
            if (identical(dims_mode, "scalar")) {
                expected <- if (identical(op_class, "arg"))
                  4 else 10
                if (length(vals) !=
                  1L || !is.finite(vals[[1L]]) ||
                  !.mojor_gpu_probe_values_close(vals[[1L]], expected, tol = 1e-09)) {
                  stop(
                    sprintf("unexpected f64 %s scalar reduce probe result", op_class),
                    call. = FALSE
                )
                }
            } else {
                expected <- if (identical(op_class, "arg"))
                  c(2, 2) else c(3, 7)
                if (length(vals) !=
                  2L || !.mojor_gpu_probe_values_close(vals, expected, tol = 1e-09)) {
                  stop(
                    sprintf("unexpected f64 %s dims reduce probe result", op_class),
                    call. = FALSE
                )
                }
            }
            list(available = TRUE, reason = NULL, code = NULL)
        }, error = function(e) {
            list(available = FALSE, reason = conditionMessage(e), code = NULL)
        }
    )

    .mojor_gpu_probe_result(
        available = res$available,
        reason = res$reason,
        code = res$code
    )
}

.mojor_gpu_f64_reduce_capable <- function(
    force_refresh = FALSE, dims_mode = c("scalar", "dims"),
    op_class = c("value", "arg")
) {
    dims_mode <- match.arg(dims_mode, c("scalar", "dims"))
    op_class <- match.arg(op_class, c("value", "arg"))
    if (!mojor_is_loaded() || !mojor_has_gpu()) {
        return(FALSE)
    }
    ctx <- tryCatch(.mojor_gpu_ctx_get(), error = function(e) NULL)
    if (is.null(ctx)) {
        return(FALSE)
    }
    ctx_epoch <- .mojor_gpu_ctx_epoch_get()
    cache <- .mojor_state$gpu_capability_cache
    if (is.null(cache) ||
        !is.list(cache)) {
        cache <- list()
    }
    cache_key <- paste0("f64_reduce_", dims_mode, "_", op_class)
    if (!isTRUE(force_refresh)) {
        cached <- .mojor_gpu_capability_cache_lookup(cache, cache_key, ctx, ctx_epoch)
        if (!is.null(cached)) {
            return(isTRUE(cached$available))
        }
    }
    probed <- .mojor_gpu_probe_f64_reduce_dispatch(
        ctx = ctx, dims_mode = dims_mode, op_class = op_class, force_refresh = force_refresh
    )
    probed <- .mojor_gpu_probe_result_normalize(probed, paste0("reduce(", dims_mode, ",", op_class, ")"))
    cache <- .mojor_gpu_capability_cache_store(
        cache, cache_key, ctx, ctx_epoch, available = probed$available,
        reason = probed$reason,
        reason_code = probed$code
    )
    .mojor_state$gpu_capability_cache <- cache
    isTRUE(probed$available)
}

.mojor_gpu_i32_scatter_capable <- function(api = "metal", force_refresh = FALSE) {
    api_name <- tolower(as.character(api[[1L]]))
    if (!identical(api_name, "metal")) {
        return(TRUE)
    }
    force_refresh <- isTRUE(force_refresh) ||
        isTRUE(getOption("mojor.gpu.i32_scatter_on_metal", FALSE))
    if (!mojor_is_loaded() || !mojor_has_gpu()) {
        return(FALSE)
    }
    ctx <- tryCatch(.mojor_gpu_ctx_get(), error = function(e) NULL)
    if (is.null(ctx)) {
        return(FALSE)
    }
    ctx_epoch <- .mojor_gpu_ctx_epoch_get()
    cache <- .mojor_state$gpu_capability_cache
    if (is.null(cache) ||
        !is.list(cache)) {
        cache <- list()
    }
    if (!isTRUE(force_refresh)) {
        cached <- .mojor_gpu_capability_cache_lookup(cache, "i32_scatter_metal", ctx, ctx_epoch)
        if (!is.null(cached)) {
            return(isTRUE(cached$available))
        }
    }
    probed <- .mojor_gpu_probe_i32_scatter(ctx)
    cache <- .mojor_gpu_capability_cache_store(
        cache, "i32_scatter_metal", ctx, ctx_epoch, available = probed$available,
        reason = probed$reason,
        reason_code = probed$code
    )
    .mojor_state$gpu_capability_cache <- cache
    isTRUE(probed$available)
}

.mojor_gpu_wrap_buffer_result <- function(
    out_handle, n, api, dtype, include_shape = FALSE, dim = NULL, dimnames = NULL,
    strides = NULL, class = "mojor_gpu_array"
) {
    payload <- list(handle = out_handle, data = NULL, n = n, api = api, dtype = dtype)
    if (isTRUE(include_shape)) {
        payload$dim <- dim
        payload$dimnames <- dimnames
        payload$strides <- strides
    }
    out <- .mojor_gpu_new_object(payload, class = class)
    attr(out, "gpu_status") <- attr(out_handle, "gpu_status")
    out
}

.mojor_gpu_reduce_call <- function(
    dtype, handle, op_code, dims = NULL, keepdims_i = 0L, require_label = NULL
) {
    info <- .mojor_gpu_buf_info_from_dtype(dtype)
    out_handle <- .mojor_call_bridge(
        paste0("mojor_gpu_buf_", info$dtype, "_reduce"),
        .mojor_gpu_ctx_get(), handle, as.integer(op_code),
        dims, if (is.null(dims))
            0L else as.integer(length(dims)),
        as.integer(keepdims_i)
    )
    if (is.null(out_handle)) {
        stop(
            sprintf("mojor_gpu_reduce: gpu_buf_%s_reduce failed", info$dtype),
            call. = FALSE
        )
    }
    attr(out_handle, "gpu_status") <- 1L
    label <- if (is.null(require_label))
        paste0("gpu_buf_", info$dtype, "_reduce") else require_label
    .mojor_gpu_require(label, out_handle)
    class(out_handle) <- c(info$class, class(out_handle))
    out_n <- as.integer(.mojor_call_bridge(info$len, out_handle))
    list(handle = out_handle, n = out_n)
}

mojor_gpu_array <- function(
    x = NULL, n = NULL, api = c("auto", "metal", "cuda", "amd"),
    dtype = c("auto", "f32", "f64", "i32")
) {
    api <- .mojor_gpu_api_resolve(api)
    dtype <- .mojor_gpu_dtype_from_input(x, dtype)
    dims <- NULL
    dimn <- NULL
    strides <- NULL
    if (!is.null(x)) {
        dims <- dim(x)
        if (!is.null(dims))
            dims <- as.integer(dims)
        dimn <- dimnames(x)
        if (!is.null(dims)) {
            strides <- .mojor_dim_strides(dims)
        }
        x <- .mojor_gpu_cast_values(x, dtype)
        n <- length(x)
    } else if (is.null(n)) {
        stop("mojor_gpu_array: provide x or n")
    } else {
        n <- as.integer(n)
    }
    handle <- switch(
        dtype, f64 = mojor_gpu_buf_f64(x = x, n = n),
        i32 = mojor_gpu_buf_i32(x = x, n = n),
        mojor_gpu_buf_f32(x = x, n = n)
    )
    .mojor_gpu_new_object(
        list(
            handle = handle, data = NULL, n = n, api = api, dtype = dtype,
            dim = dims, dimnames = dimn, strides = strides
        ),
        class = "GPUArray"
    )
}

GPUArray <- function(
    x = NULL, n = NULL, api = c("auto", "metal", "cuda", "amd"),
    dtype = c("auto", "f32", "f64", "i32")
) {
    mojor_gpu_array(x = x, n = n, api = api, dtype = dtype)
}

gpu_array <- function(
    x = NULL, n = NULL, api = c("auto", "metal", "cuda", "amd"),
    dtype = c("auto", "f32", "f64", "i32")
) {
    mojor_gpu_array(x = x, n = n, api = api, dtype = dtype)
}

gpu_zeros <- function(
    dim = NULL, n = NULL, api = c("auto", "metal", "cuda", "amd"),
    dtype = c("auto", "f32", "f64", "i32")
) {
    api <- match.arg(api)
    dtype <- match.arg(dtype)
    if (!is.null(dim)) {
        dim <- as.integer(dim)
        if (length(dim) ==
            0L)
            stop("gpu_zeros: dim must be non-empty")
        if (any(dim <= 0L))
            stop("gpu_zeros: dim entries must be positive")
        x <- array(0, dim = dim)
        return(mojor_gpu_array(x = x, api = api, dtype = dtype))
    }
    if (is.null(n)) {
        stop("gpu_zeros: provide dim or n")
    }
    n <- as.integer(n)
    if (n <= 0L)
        stop("gpu_zeros: n must be positive")
    x <- rep(0, n)
    mojor_gpu_array(x = x, api = api, dtype = dtype)
}

gpu_ones <- function(
    dim = NULL, n = NULL, api = c("auto", "metal", "cuda", "amd"),
    dtype = c("auto", "f32", "f64", "i32")
) {
    api <- match.arg(api)
    dtype <- match.arg(dtype)
    if (!is.null(dim)) {
        dim <- as.integer(dim)
        if (length(dim) ==
            0L)
            stop("gpu_ones: dim must be non-empty")
        if (any(dim <= 0L))
            stop("gpu_ones: dim entries must be positive")
        x <- array(1, dim = dim)
        return(mojor_gpu_array(x = x, api = api, dtype = dtype))
    }
    if (is.null(n)) {
        stop("gpu_ones: provide dim or n")
    }
    n <- as.integer(n)
    if (n <= 0L)
        stop("gpu_ones: n must be positive")
    x <- rep(1, n)
    mojor_gpu_array(x = x, api = api, dtype = dtype)
}

gpu_full <- function(
    value, dim = NULL, n = NULL, api = c("auto", "metal", "cuda", "amd"),
    dtype = c("auto", "f32", "f64", "i32")
) {
    api <- match.arg(api)
    dtype <- match.arg(dtype)
    if (!is.numeric(value) ||
        length(value) !=
            1L || is.na(value)) {
        stop("gpu_full: value must be a non-NA numeric scalar")
    }
    if (!is.null(dim)) {
        dim <- as.integer(dim)
        if (length(dim) ==
            0L)
            stop("gpu_full: dim must be non-empty")
        if (any(dim <= 0L))
            stop("gpu_full: dim entries must be positive")
        x <- array(
            as.numeric(value),
            dim = dim
        )
        return(mojor_gpu_array(x = x, api = api, dtype = dtype))
    }
    if (is.null(n)) {
        stop("gpu_full: provide dim or n")
    }
    n <- as.integer(n)
    if (n <= 0L)
        stop("gpu_full: n must be positive")
    x <- rep(
        as.numeric(value),
        n
    )
    mojor_gpu_array(x = x, api = api, dtype = dtype)
}

.mojor_gpu_validate_rand_bounds <- function(min, max, fn_name) {
    if (!is.numeric(min) ||
        length(min) !=
            1L || is.na(min)) {
        stop(fn_name, ": min must be a non-NA numeric scalar")
    }
    if (!is.numeric(max) ||
        length(max) !=
            1L || is.na(max)) {
        stop(fn_name, ": max must be a non-NA numeric scalar")
    }
    min <- as.numeric(min)
    max <- as.numeric(max)
    if (max < min) {
        stop(fn_name, ": max must be >= min")
    }
    list(min = min, max = max)
}

.mojor_gpu_validate_randn_params <- function(mean, sd, fn_name) {
    if (!is.numeric(mean) ||
        length(mean) !=
            1L || is.na(mean)) {
        stop(fn_name, ": mean must be a non-NA numeric scalar")
    }
    if (!is.numeric(sd) ||
        length(sd) !=
            1L || is.na(sd)) {
        stop(fn_name, ": sd must be a non-NA numeric scalar")
    }
    mean <- as.numeric(mean)
    sd <- as.numeric(sd)
    if (sd < 0) {
        stop(fn_name, ": sd must be >= 0")
    }
    list(mean = mean, sd = sd)
}

.mojor_gpu_validate_randi_bounds <- function(low, high, fn_name) {
    if (!is.numeric(low) ||
        length(low) !=
            1L || is.na(low) ||
        !is.finite(low) ||
        (low != trunc(low))) {
        stop(fn_name, ": low must be a finite integer scalar")
    }
    if (!is.numeric(high) ||
        length(high) !=
            1L || is.na(high) ||
        !is.finite(high) ||
        (high != trunc(high))) {
        stop(fn_name, ": high must be a finite integer scalar")
    }
    low_i <- as.integer(low)
    high_i <- as.integer(high)
    if (high_i <= low_i) {
        stop(fn_name, ": high must be > low (high is exclusive)")
    }
    list(low = low_i, high = high_i, span = as.integer(high_i - low_i))
}

gpu_rand <- function(
    dim = NULL, n = NULL, min = 0, max = 1, api = c("auto", "metal", "cuda", "amd"),
    dtype = c("auto", "f32", "f64", "i32")
) {
    api <- match.arg(api)
    dtype <- match.arg(dtype)
    bounds <- .mojor_gpu_validate_rand_bounds(min, max, "gpu_rand")

    if (!is.null(dim)) {
        dim <- as.integer(dim)
        if (length(dim) ==
            0L)
            stop("gpu_rand: dim must be non-empty")
        if (any(dim <= 0L))
            stop("gpu_rand: dim entries must be positive")
        host <- array(
            stats::runif(
                as.integer(prod(dim)),
                min = bounds$min, max = bounds$max
            ),
            dim = dim
        )
        return(mojor_gpu_array(x = host, api = api, dtype = dtype))
    }
    if (is.null(n)) {
        stop("gpu_rand: provide dim or n")
    }
    n <- as.integer(n)
    if (n <= 0L)
        stop("gpu_rand: n must be positive")
    host <- stats::runif(n, min = bounds$min, max = bounds$max)
    mojor_gpu_array(x = host, api = api, dtype = dtype)
}

gpu_randn <- function(
    dim = NULL, n = NULL, mean = 0, sd = 1, api = c("auto", "metal", "cuda", "amd"),
    dtype = c("auto", "f32", "f64", "i32")
) {
    api <- match.arg(api)
    dtype <- match.arg(dtype)
    params <- .mojor_gpu_validate_randn_params(mean, sd, "gpu_randn")

    if (!is.null(dim)) {
        dim <- as.integer(dim)
        if (length(dim) ==
            0L)
            stop("gpu_randn: dim must be non-empty")
        if (any(dim <= 0L))
            stop("gpu_randn: dim entries must be positive")
        host <- array(
            stats::rnorm(
                as.integer(prod(dim)),
                mean = params$mean, sd = params$sd
            ),
            dim = dim
        )
        return(mojor_gpu_array(x = host, api = api, dtype = dtype))
    }
    if (is.null(n)) {
        stop("gpu_randn: provide dim or n")
    }
    n <- as.integer(n)
    if (n <= 0L)
        stop("gpu_randn: n must be positive")
    host <- stats::rnorm(n, mean = params$mean, sd = params$sd)
    mojor_gpu_array(x = host, api = api, dtype = dtype)
}

gpu_randi <- function(
    dim = NULL, n = NULL, low = 0L, high = 10L, api = c("auto", "metal", "cuda", "amd"),
    dtype = c("auto", "f32", "f64", "i32")
) {
    api <- match.arg(api)
    dtype <- match.arg(dtype)
    bounds <- .mojor_gpu_validate_randi_bounds(low, high, "gpu_randi")

    if (!is.null(dim)) {
        dim <- as.integer(dim)
        if (length(dim) ==
            0L)
            stop("gpu_randi: dim must be non-empty")
        if (any(dim <= 0L))
            stop("gpu_randi: dim entries must be positive")
        size <- as.integer(prod(dim))
        vals <- sample.int(bounds$span, size = size, replace = TRUE) +
            bounds$low - 1L
        host <- array(vals, dim = dim)
        return(mojor_gpu_array(x = host, api = api, dtype = dtype))
    }
    if (is.null(n)) {
        stop("gpu_randi: provide dim or n")
    }
    n <- as.integer(n)
    if (n <= 0L)
        stop("gpu_randi: n must be positive")
    host <- sample.int(bounds$span, size = n, replace = TRUE) +
        bounds$low - 1L
    mojor_gpu_array(x = host, api = api, dtype = dtype)
}

gpu_empty <- function(
    dim = NULL, n = NULL, api = c("auto", "metal", "cuda", "amd"),
    dtype = c("auto", "f32", "f64", "i32")
) {
    api <- match.arg(api)
    dtype <- match.arg(dtype)
    if (!is.null(dim)) {
        dim <- as.integer(dim)
        if (length(dim) ==
            0L)
            stop("gpu_empty: dim must be non-empty")
        if (any(dim <= 0L))
            stop("gpu_empty: dim entries must be positive")
        out <- mojor_gpu_array(
            n = as.integer(prod(dim)),
            api = api, dtype = dtype
        )
        out <- .mojor_gpu_object_set(out, "dim", dim)
        out <- .mojor_gpu_object_set(out, "dimnames", NULL)
        out <- .mojor_gpu_object_set(out, "strides", .mojor_dim_strides(dim))
        return(out)
    }
    if (is.null(n)) {
        stop("gpu_empty: provide dim or n")
    }
    n <- as.integer(n)
    if (n <= 0L)
        stop("gpu_empty: n must be positive")
    mojor_gpu_array(n = n, api = api, dtype = dtype)
}

.mojor_gpu_like_source_meta <- function(x, api, dtype, fn_name) {
    api <- match.arg(api, c("auto", "metal", "cuda", "amd"))
    dtype <- match.arg(dtype, c("auto", "f32", "f64", "i32"))
    if (inherits(x, "GPUArray") ||
        inherits(x, "mojor_gpu_array")) {
        if (identical(api, "auto")) {
            api <- .mojor_gpu_array_api(x)
        }
        if (identical(dtype, "auto")) {
            dtype <- .mojor_gpu_array_dtype(x)
        }
        x_dim <- .mojor_gpu_array_dim(x)
        x_dimnames <- .mojor_gpu_array_dimnames(x)
        x_n <- .mojor_gpu_array_length(x)
    } else {
        if (is.null(x)) {
            stop(fn_name, ": x must not be NULL")
        }
        x_dim <- dim(x)
        if (!is.null(x_dim))
            x_dim <- as.integer(x_dim)
        x_dimnames <- dimnames(x)
        x_n <- as.integer(length(x))
    }
    list(
        api = api, dtype = dtype, x_dim = x_dim, x_dimnames = x_dimnames,
        x_n = x_n
    )
}

gpu_rand_like <- function(
    x, min = 0, max = 1, api = c("auto", "metal", "cuda", "amd"),
    dtype = c("auto", "f32", "f64", "i32")
) {
    bounds <- .mojor_gpu_validate_rand_bounds(min, max, "gpu_rand_like")
    meta <- .mojor_gpu_like_source_meta(x, api, dtype, "gpu_rand_like")
    if (meta$x_n <= 0L) {
        stop("gpu_rand_like: x must have positive length")
    }
    host <- stats::runif(meta$x_n, min = bounds$min, max = bounds$max)
    if (!is.null(meta$x_dim)) {
        host <- array(host, dim = meta$x_dim, dimnames = meta$x_dimnames)
    }
    mojor_gpu_array(x = host, api = meta$api, dtype = meta$dtype)
}

gpu_randn_like <- function(
    x, mean = 0, sd = 1, api = c("auto", "metal", "cuda", "amd"),
    dtype = c("auto", "f32", "f64", "i32")
) {
    params <- .mojor_gpu_validate_randn_params(mean, sd, "gpu_randn_like")
    meta <- .mojor_gpu_like_source_meta(x, api, dtype, "gpu_randn_like")
    if (meta$x_n <= 0L) {
        stop("gpu_randn_like: x must have positive length")
    }
    host <- stats::rnorm(meta$x_n, mean = params$mean, sd = params$sd)
    if (!is.null(meta$x_dim)) {
        host <- array(host, dim = meta$x_dim, dimnames = meta$x_dimnames)
    }
    mojor_gpu_array(x = host, api = meta$api, dtype = meta$dtype)
}

gpu_randi_like <- function(
    x, low = 0L, high = 10L, api = c("auto", "metal", "cuda", "amd"),
    dtype = c("auto", "f32", "f64", "i32")
) {
    bounds <- .mojor_gpu_validate_randi_bounds(low, high, "gpu_randi_like")
    meta <- .mojor_gpu_like_source_meta(x, api, dtype, "gpu_randi_like")
    if (meta$x_n <= 0L) {
        stop("gpu_randi_like: x must have positive length")
    }
    host <- sample.int(bounds$span, size = meta$x_n, replace = TRUE) +
        bounds$low - 1L
    if (!is.null(meta$x_dim)) {
        host <- array(host, dim = meta$x_dim, dimnames = meta$x_dimnames)
    }
    mojor_gpu_array(x = host, api = meta$api, dtype = meta$dtype)
}

.mojor_gpu_make_like <- function(x, value, api, dtype, fn_name) {
    if (!is.numeric(value) ||
        length(value) !=
            1L || is.na(value)) {
        stop(fn_name, ": value must be a non-NA numeric scalar")
    }
    meta <- .mojor_gpu_like_source_meta(x, api, dtype, fn_name)
    if (!is.null(meta$x_dim)) {
        host <- array(
            as.numeric(value),
            dim = meta$x_dim, dimnames = meta$x_dimnames
        )
    } else {
        if (meta$x_n <= 0L) {
            stop(fn_name, ": x must have positive length")
        }
        host <- rep(
            as.numeric(value),
            meta$x_n
        )
    }
    mojor_gpu_array(x = host, api = meta$api, dtype = meta$dtype)
}

gpu_zeros_like <- function(
    x, api = c("auto", "metal", "cuda", "amd"),
    dtype = c("auto", "f32", "f64", "i32")
) {
    .mojor_gpu_make_like(x, value = 0, api = api, dtype = dtype, fn_name = "gpu_zeros_like")
}

gpu_ones_like <- function(
    x, api = c("auto", "metal", "cuda", "amd"),
    dtype = c("auto", "f32", "f64", "i32")
) {
    .mojor_gpu_make_like(x, value = 1, api = api, dtype = dtype, fn_name = "gpu_ones_like")
}

gpu_full_like <- function(
    x, value, api = c("auto", "metal", "cuda", "amd"),
    dtype = c("auto", "f32", "f64", "i32")
) {
    .mojor_gpu_make_like(
        x, value = value, api = api, dtype = dtype, fn_name = "gpu_full_like"
    )
}

gpu_empty_like <- function(
    x, api = c("auto", "metal", "cuda", "amd"),
    dtype = c("auto", "f32", "f64", "i32")
) {
    meta <- .mojor_gpu_like_source_meta(x, api, dtype, "gpu_empty_like")
    if (meta$x_n <= 0L) {
        stop("gpu_empty_like: x must have positive length")
    }
    out <- mojor_gpu_array(
        n = as.integer(meta$x_n),
        api = meta$api, dtype = meta$dtype
    )
    if (!is.null(meta$x_dim)) {
        out <- .mojor_gpu_object_set(out, "dim", as.integer(meta$x_dim))
        out <- .mojor_gpu_object_set(out, "dimnames", meta$x_dimnames)
        out <- .mojor_gpu_object_set(out, "strides", .mojor_dim_strides(as.integer(meta$x_dim)))
    }
    out
}

as.GPUArray <- function(x, ...) {
    if (inherits(x, "GPUArray")) {
        return(x)
    }
    mojor_gpu_array(x, ...)
}

.mojor_gpu_cast_bridge_symbol <- function(src_dtype, dst_dtype) {
    key <- paste0(src_dtype, "_", dst_dtype)
    switch(
        key, f32_f64 = "mojor_gpu_buf_f32_cast_f64", f32_i32 = "mojor_gpu_buf_f32_cast_i32",
        f64_f32 = "mojor_gpu_buf_f64_cast_f32", f64_i32 = "mojor_gpu_buf_f64_cast_i32",
        i32_f32 = "mojor_gpu_buf_i32_cast_f32", i32_f64 = "mojor_gpu_buf_i32_cast_f64",
        NULL
    )
}

.mojor_gpu_clone_device_via_gather <- function(x, route_label = "gpu_cast") {
    if (!inherits(x, "GPUArray") &&
        !inherits(x, "mojor_gpu_array")) {
        return(NULL)
    }
    dtype <- .mojor_gpu_array_dtype(x)
    api <- .mojor_gpu_array_api(x)
    if (isTRUE(.mojor_gpu_use_host_staged_index_route(api, dtype))) {
        out <- mojor_gpu_array(
            mojor_gpu_array_read(x),
            api = api,
            dtype = dtype
        )
        return(
            .mojor_gpu_route_tag(
                out,
                route_label,
                reason = "host-staged gather clone on this backend",
                reason_code = "host_stage"
            )
        )
    }
    handle <- .mojor_gpu_array_handle(x)
    if (is.null(handle)) {
        return(NULL)
    }
    bridge <- paste0("mojor_gpu_buf_", dtype, "_gather")
    x_dim <- .mojor_gpu_array_dim(x)
    if (is.null(x_dim)) {
        x_dim <- as.integer(.mojor_gpu_array_length(x))
        out_dim <- NULL
    } else {
        x_dim <- as.integer(x_dim)
        out_dim <- x_dim
    }
    idx_vals <- lapply(x_dim, seq_len)
    payload <- .mojor_gpu_index_payload(idx_vals, x_dim)
    out_handle <- tryCatch(
        .mojor_call_bridge(
            bridge, .mojor_gpu_ctx_get(), handle, payload$idx_data, payload$idx_offsets,
            payload$idx_lens, x_dim
        ),
        error = function(e) NULL
    )
    if (is.null(out_handle)) {
        return(NULL)
    }
    .mojor_gpu_require(
        paste0("gpu_buf_", dtype, "_gather"),
        out_handle
    )
    info <- .mojor_gpu_buf_info_from_dtype(dtype)
    class(out_handle) <- c(info$class, class(out_handle))
    out <- .mojor_gpu_wrap_buffer_result(
        out_handle, n = as.integer(payload$out_n),
        api = .mojor_gpu_array_api(x),
        dtype = dtype, include_shape = !is.null(out_dim),
        dim = out_dim, dimnames = .mojor_gpu_array_dimnames(x),
        strides = if (is.null(out_dim))
            NULL else .mojor_dim_strides(out_dim),
        class = if (inherits(x, "GPUArray"))
            "GPUArray" else "mojor_gpu_array"
    )
    out <- .mojor_gpu_route_tag(out, route_label)
    out
}

.mojor_gpu_cast_device <- function(x, dtype) {
    if (!inherits(x, "GPUArray") &&
        !inherits(x, "mojor_gpu_array")) {
        return(NULL)
    }
    src_dtype <- .mojor_gpu_array_dtype(x)
    if (identical(src_dtype, dtype)) {
        return(.mojor_gpu_clone_device_via_gather(x, route_label = "gpu_cast"))
    }
    bridge <- .mojor_gpu_cast_bridge_symbol(src_dtype, dtype)
    if (is.null(bridge)) {
        return(NULL)
    }
    handle <- .mojor_gpu_array_handle(x)
    if (is.null(handle)) {
        return(NULL)
    }
    out_handle <- tryCatch(
        .mojor_call_bridge(bridge, .mojor_gpu_ctx_get(), handle),
        error = function(e) NULL
    )
    if (is.null(out_handle)) {
        return(NULL)
    }
    .mojor_gpu_require(
        paste0("gpu_buf_", src_dtype, "_cast_", dtype),
        out_handle
    )

    info <- .mojor_gpu_buf_info_from_dtype(dtype)
    class(out_handle) <- c(info$class, class(out_handle))
    out_n <- tryCatch(
        as.integer(.mojor_call_bridge(info$len, out_handle)),
        error = function(e) as.integer(.mojor_gpu_array_length(x))
    )
    out_dim <- .mojor_gpu_array_dim(x)
    if (!is.null(out_dim))
        out_dim <- as.integer(out_dim)
    out_dimnames <- .mojor_gpu_array_dimnames(x)
    out_strides <- if (is.null(out_dim))
        NULL else .mojor_dim_strides(out_dim)
    out_class <- if (inherits(x, "GPUArray"))
        "GPUArray" else "mojor_gpu_array"
    out <- .mojor_gpu_wrap_buffer_result(
        out_handle, n = out_n, api = .mojor_gpu_array_api(x),
        dtype = dtype, include_shape = TRUE, dim = out_dim, dimnames = out_dimnames,
        strides = out_strides, class = out_class
    )
    out <- .mojor_gpu_route_tag(out, "gpu_cast")
    out
}

.mojor_gpu_broadcast_plan <- function(src_dim, out_dim, fn_name = "gpu_broadcast") {
    out_nd <- length(out_dim)
    src_nd <- length(src_dim)
    if (src_nd > out_nd) {
        stop(fn_name, ": source rank exceeds target shape rank")
    }
    src_dim_pad <- c(
        rep.int(1L, out_nd - src_nd),
        as.integer(src_dim)
    )
    ok <- (src_dim_pad == out_dim) | (src_dim_pad == 1L)
    if (!all(ok)) {
        stop(
            fn_name, ": source shape is not broadcast-compatible with target shape"
        )
    }
    idx_vals <- lapply(
        seq_len(out_nd),
        function(axis) {
            sd <- src_dim_pad[[axis]]
            if (sd == 1L)
                rep.int(1L, out_dim[[axis]]) else seq_len(out_dim[[axis]])
        }
    )
    list(src_dim_pad = src_dim_pad, idx_vals = idx_vals)
}

.mojor_gpu_broadcast_device <- function(x, out_dim, plan, fn_name = "gpu_broadcast") {
    if (!inherits(x, "GPUArray") &&
        !inherits(x, "mojor_gpu_array")) {
        return(NULL)
    }
    dtype <- .mojor_gpu_array_dtype(x)
    api <- .mojor_gpu_array_api(x)
    if (isTRUE(.mojor_gpu_use_host_staged_index_route(api))) {
        out_host <- .mojor_gpu_broadcast_host(
            mojor_gpu_array_read(x),
            out_dim,
            fn_name = fn_name
        )
        out <- mojor_gpu_array(
            out_host,
            api = api,
            dtype = dtype
        )
        return(
            .mojor_gpu_route_tag(
                out,
                "gpu_broadcast",
                reason = "host-staged broadcast on this backend",
                reason_code = "host_stage"
            )
        )
    }
    handle <- .mojor_gpu_array_handle(x)
    if (is.null(handle)) {
        return(NULL)
    }
    bridge <- paste0("mojor_gpu_buf_", dtype, "_gather")
    payload <- .mojor_gpu_index_payload(plan$idx_vals, plan$src_dim_pad)
    out_handle <- tryCatch(
        .mojor_call_bridge(
            bridge, .mojor_gpu_ctx_get(), handle, payload$idx_data, payload$idx_offsets,
            payload$idx_lens, plan$src_dim_pad
        ),
        error = function(e) NULL
    )
    if (is.null(out_handle)) {
        return(NULL)
    }
    .mojor_gpu_require(
        paste0("gpu_buf_", dtype, "_gather"),
        out_handle
    )
    info <- .mojor_gpu_buf_info_from_dtype(dtype)
    class(out_handle) <- c(info$class, class(out_handle))
    out_dim_obj <- if (length(out_dim) ==
        1L)
        NULL else as.integer(out_dim)
    out <- .mojor_gpu_wrap_buffer_result(
        out_handle, n = as.integer(payload$out_n),
        api = .mojor_gpu_array_api(x),
        dtype = dtype, include_shape = TRUE, dim = out_dim_obj, dimnames = NULL,
        strides = if (is.null(out_dim_obj))
            NULL else .mojor_dim_strides(out_dim_obj),
        class = if (inherits(x, "GPUArray"))
            "GPUArray" else "mojor_gpu_array"
    )
    out <- .mojor_gpu_route_tag(out, "gpu_broadcast")
    out
}

gpu_cast <- function(x, dtype = c("f32", "f64", "i32")) {
    dtype <- match.arg(dtype, c("f32", "f64", "i32"))
    if (inherits(x, "GPUArray") ||
        inherits(x, "mojor_gpu_array")) {
        out_gpu <- .mojor_gpu_cast_device(x, dtype)
        if (!is.null(out_gpu)) {
            return(out_gpu)
        }
        out <- mojor_gpu_array(
            mojor_gpu_array_read(x),
            api = .mojor_gpu_array_api(x),
            dtype = dtype
        )
        out <- .mojor_gpu_route_tag(out, "cpu_cast")
        return(out)
    }
    if (!is.numeric(x) &&
        !is.integer(x)) {
        stop("gpu_cast: x must be numeric/integer or mojor_gpu_array")
    }
    out <- mojor_gpu_array(x, dtype = dtype)
    out <- .mojor_gpu_route_tag(out, "host_cast")
    out
}

gpu_promote <- function(x, y) {
    dtype_x <- .mojor_gpu_value_dtype(x)
    dtype_y <- .mojor_gpu_value_dtype(y)
    target <- .mojor_gpu_promote_dtype(dtype_x, dtype_y)

    x_gpu <- if ((inherits(x, "GPUArray") ||
        inherits(x, "mojor_gpu_array")) &&
        identical(dtype_x, target)) {
        x
    } else {
        gpu_cast(x, dtype = target)
    }
    y_gpu <- if ((inherits(y, "GPUArray") ||
        inherits(y, "mojor_gpu_array")) &&
        identical(dtype_y, target)) {
        y
    } else {
        gpu_cast(y, dtype = target)
    }

    if (!inherits(x_gpu, "GPUArray"))
        x_gpu <- as.GPUArray(x_gpu)
    if (!inherits(y_gpu, "GPUArray"))
        y_gpu <- as.GPUArray(y_gpu)

    structure(
        list(x = x_gpu, y = y_gpu, dtype = target),
        class = "mojor_gpu_promoted"
    )
}

gpu_broadcast <- function(x, shape) {
    shape_i <- .mojor_gpu_broadcast_shape(shape, fn_name = "gpu_broadcast")
    if (inherits(x, "GPUArray") ||
        inherits(x, "mojor_gpu_array")) {
        src_dim <- .mojor_gpu_array_dim(x)
        if (is.null(src_dim)) {
            src_dim <- as.integer(.mojor_gpu_array_length(x))
        } else {
            src_dim <- as.integer(src_dim)
        }
        plan <- .mojor_gpu_broadcast_plan(src_dim, shape_i, fn_name = "gpu_broadcast")
        out_gpu <- .mojor_gpu_broadcast_device(x, shape_i, plan, fn_name = "gpu_broadcast")
        if (!is.null(out_gpu)) {
            return(out_gpu)
        }
        host <- mojor_gpu_array_read(x)
        out_host <- .mojor_gpu_broadcast_host(host, shape_i, fn_name = "gpu_broadcast")
        out <- mojor_gpu_array(
            out_host, api = .mojor_gpu_array_api(x),
            dtype = .mojor_gpu_array_dtype(x)
        )
        out <- .mojor_gpu_route_tag(out, "cpu_broadcast")
        return(out)
    }
    if (!is.numeric(x) &&
        !is.integer(x)) {
        stop("gpu_broadcast: x must be numeric/integer or mojor_gpu_array")
    }
    out_host <- .mojor_gpu_broadcast_host(x, shape_i, fn_name = "gpu_broadcast")
    out <- mojor_gpu_array(out_host, dtype = .mojor_gpu_value_dtype(x))
    out <- .mojor_gpu_route_tag(out, "host_broadcast")
    out
}

.mojor_gpu_array_length <- function(x) {
    n <- .mojor_gpu_object_get(x, "n", default = NULL)
    if (!is.null(n)) {
        return(as.integer(n))
    }
    handle <- .mojor_gpu_array_handle(x)
    if (is.null(handle)) {
        return(0L)
    }
    info <- .mojor_gpu_buf_info_from_handle(handle)
    as.integer(.mojor_call_bridge(info$len, handle))
}

.mojor_gpu_array_dim <- function(x) {
    .mojor_gpu_object_get(x, "dim", default = NULL)
}

.mojor_gpu_array_dimnames <- function(x) {
    .mojor_gpu_object_get(x, "dimnames", default = NULL)
}

.mojor_gpu_as_array_dispatch <- function(x, ...) {
    mojor_gpu_array_read(x)
}

.mojor_gpu_as_matrix_dispatch <- function(x, ...) {
    as.matrix(mojor_gpu_array_read(x))
}

.mojor_gpu_as_numeric_dispatch <- function(x, ...) {
    as.numeric(mojor_gpu_array_read(x))
}

mojor_gpu_array_free <- function(buf) {
    if (!inherits(buf, "mojor_gpu_array")) {
        stop("mojor_gpu_array_free: buf must be mojor_gpu_array")
    }
    buf <- .mojor_gpu_array_clear_index_plans(buf)
    handle <- .mojor_gpu_array_handle(buf)
    if (!is.null(handle)) {
        info <- .mojor_gpu_buf_info_from_handle(handle)
        .mojor_gpu_buf_free(info, handle)
    }
    buf <- .mojor_gpu_array_set_handle(buf, NULL, free_old = FALSE, invalidate_plans = FALSE)
    buf <- .mojor_gpu_object_set(buf, "n", 0L)
    buf <- .mojor_gpu_object_set(buf, "dtype", NULL)
    buf <- .mojor_gpu_object_set(buf, "dim", NULL)
    buf <- .mojor_gpu_object_set(buf, "dimnames", NULL)
    buf <- .mojor_gpu_object_set(buf, "strides", NULL)
    buf
}

mojor_gpu_array_write <- function(buf, values) {
    if (!inherits(buf, "mojor_gpu_array")) {
        stop("mojor_gpu_array_write: buf must be mojor_gpu_array")
    }
    if (inherits(values, "mojor_gpu_array") ||
        inherits(values, "GPUArray")) {
        values <- mojor_gpu_array_read(values)
    }
    dtype <- .mojor_gpu_array_dtype(buf)
    values_dim <- dim(values)
    values_dimnames <- dimnames(values)
    values <- .mojor_gpu_cast_values(values, dtype)
    n <- .mojor_gpu_object_get(buf, "n", default = NULL)
    if (!is.null(n) &&
        length(values) !=
            n) {
        stop("mojor_gpu_array_write: length mismatch")
    }
    old_dim <- .mojor_gpu_object_get(buf, "dim", default = NULL)
    if (!is.null(values_dim)) {
        new_dim <- as.integer(values_dim)
        buf <- .mojor_gpu_object_set(buf, "dim", new_dim)
        buf <- .mojor_gpu_object_set(buf, "dimnames", values_dimnames)
        buf <- .mojor_gpu_object_set(buf, "strides", .mojor_dim_strides(new_dim))
    }
    new_dim <- .mojor_gpu_object_get(buf, "dim", default = NULL)
    if (!identical(old_dim, new_dim)) {
        buf <- .mojor_gpu_array_clear_index_plans(buf)
    }
    handle <- .mojor_gpu_array_handle(buf)
    if (is.null(handle))
        stop("mojor_gpu_array_write: missing handle")
    info <- .mojor_gpu_buf_info_from_dtype(dtype)
    .mojor_gpu_buf_write(info, handle, values)
    invisible(buf)
}

mojor_gpu_array_read <- function(buf) {
    if (!inherits(buf, "mojor_gpu_array")) {
        stop("mojor_gpu_array_read: buf must be mojor_gpu_array")
    }
    handle <- .mojor_gpu_array_handle(buf)
    if (is.null(handle))
        stop("mojor_gpu_array_read: missing handle")
    info <- .mojor_gpu_buf_info_from_dtype(.mojor_gpu_array_dtype(buf))
    out <- .mojor_gpu_buf_read(info, handle)
    out_dim <- .mojor_gpu_object_get(buf, "dim", default = NULL)
    out_dimnames <- .mojor_gpu_object_get(buf, "dimnames", default = NULL)
    if (!is.null(out_dim)) {
        dim(out) <- out_dim
        if (!is.null(out_dimnames)) {
            dimnames(out) <- out_dimnames
        }
    }
    out
}

print.mojor_gpu_array <- function(x, ...) {
    n <- .mojor_gpu_object_get(x, "n", default = 0L)
    api <- .mojor_gpu_array_api(x)
    dtype <- .mojor_gpu_array_dtype(x)
    x_dim <- .mojor_gpu_object_get(x, "dim", default = NULL)
    dim_str <- ""
    if (!is.null(x_dim)) {
        dim_str <- paste0(" dim=", paste(x_dim, collapse = "x"))
    }
    cat(
        "mojor_gpu_array<", n, "> [", api, ", ", dtype, "]", dim_str, "\n",
        sep = ""
    )
    invisible(x)
}

print.GPUArray <- function(x, ...) {
    print.mojor_gpu_array(x, ...)
}

.mojor_gpu_is_array <- function(x) {
    inherits(x, "mojor_gpu_array") ||
        inherits(x, "GPUArray")
}

.mojor_gpu_is_scalar <- function(x) {
    is.numeric(x) &&
        length(x) ==
            1L && is.null(dim(x))
}

.mojor_gpu_scalar_cast <- function(dtype, x) {
    if (!.mojor_gpu_is_scalar(x))
        stop("mojor_gpu: scalar value required")
    if (identical(dtype, "i32")) {
        return(as.integer(x))
    }
    if (identical(dtype, "f32")) {
        if (!requireNamespace("float", quietly = TRUE)) {
            stop("mojor_gpu: float package required for f32 scalars")
        }
        return(float::fl(x))
    }
    as.numeric(x)
}

.mojor_gpu_dtype_from_args <- function(x, y = NULL) {
    dx <- if (.mojor_gpu_is_array(x))
        .mojor_gpu_array_dtype(x) else NULL
    dy <- if (!is.null(y) &&
        .mojor_gpu_is_array(y))
        .mojor_gpu_array_dtype(y) else NULL
    if (!is.null(dx) &&
        !is.null(dy) &&
        !identical(dx, dy)) {
        stop("mojor_gpu: gpu array dtype mismatch")
    }
    if (!is.null(dx)) {
        return(dx)
    }
    if (!is.null(dy)) {
        return(dy)
    }
    if (!is.null(x)) {
        return(.mojor_gpu_dtype_from_input(x, "auto"))
    }
    if (!is.null(y)) {
        return(.mojor_gpu_dtype_from_input(y, "auto"))
    }
    "f64"
}

.mojor_gpu_linalg_dtype_supported <- function(dtype) {
    identical(dtype, "f32") ||
        identical(dtype, "f64") ||
        identical(dtype, "i32")
}

.mojor_gpu_stop_unsupported_linalg_dtype <- function(op, dtype) {
    stop(op, ": dtype '", dtype, "' is not supported (use f32, f64, or i32)")
}

.mojor_gpu_free_temp_arrays <- function(bufs) {
    if (length(bufs) == 0L) {
        return(invisible(NULL))
    }
    for (tmp in bufs) {
        if (inherits(tmp, "GPUArray") || inherits(tmp, "mojor_gpu_array")) {
            try(mojor_gpu_array_free(tmp), silent = TRUE)
        }
    }
    invisible(NULL)
}

.mojor_gpu_promote_linalg_pair <- function(x, y) {
    tmp <- list()
    if (.mojor_gpu_is_array(x) &&
        .mojor_gpu_is_array(y)) {
        d1 <- .mojor_gpu_array_dtype(x)
        d2 <- .mojor_gpu_array_dtype(y)
        if (!identical(d1, d2)) {
            promoted <- gpu_promote(x, y)
            x_new <- promoted$x
            y_new <- promoted$y
            if (!identical(x_new, x)) {
                tmp <- c(tmp, list(x_new))
            }
            if (!identical(y_new, y) &&
                !any(vapply(tmp, identical, logical(1), y_new))) {
                tmp <- c(tmp, list(y_new))
            }
            x <- x_new
            y <- y_new
        }
    }
    list(x = x, y = y, dtype = .mojor_gpu_dtype_from_args(x, y), tmp = tmp)
}

.mojor_gpu_pick_api <- function(x, y = NULL) {
    if (.mojor_gpu_is_array(x)) {
        return(.mojor_gpu_array_api(x))
    }
    if (!is.null(y) &&
        .mojor_gpu_is_array(y)) {
        return(.mojor_gpu_array_api(y))
    }
    .mojor_gpu_api_resolve("auto")
}

.mojor_gpu_kernel_cache_get <- function(key) {
    cache <- .mojor_state$gpu_kernel_cache
    .mojor_gpu_cache_stats_touch("gpu_kernel_cache_stats", "lookup")
    if (is.null(cache)) {
        .mojor_gpu_cache_stats_touch("gpu_kernel_cache_stats", "miss")
        return(NULL)
    }
    cached <- cache[[key]]
    if (is.null(cached)) {
        .mojor_gpu_cache_stats_touch("gpu_kernel_cache_stats", "miss")
        return(NULL)
    }
    if (identical(cached, FALSE)) {
        .mojor_gpu_cache_stats_touch("gpu_kernel_cache_stats", "negative_hit")
    } else {
        .mojor_gpu_cache_stats_touch("gpu_kernel_cache_stats", "hit")
    }
    cached
}

.mojor_gpu_kernel_cache_set <- function(key, val) {
    cache <- .mojor_state$gpu_kernel_cache
    if (is.null(cache))
        cache <- list()
    cache[[key]] <- val
    .mojor_state$gpu_kernel_cache <- cache
    .mojor_gpu_cache_stats_touch("gpu_kernel_cache_stats", "store")
    val
}

.mojor_gpu_kernel_cache_diag <- function(include_entries = TRUE, reset = FALSE) {
    cache <- .mojor_state$gpu_kernel_cache
    if (!is.list(cache)) {
        cache <- list()
    }
    stats <- .mojor_gpu_cache_stats_get("gpu_kernel_cache_stats")
    entries <- data.frame(
        key = character(),
        status = character(),
        stringsAsFactors = FALSE
    )
    if (isTRUE(include_entries) && length(cache) > 0L) {
        keys <- sort(names(cache))
        entries <- data.frame(
            key = keys,
            status = vapply(keys, function(key) {
                val <- cache[[key]]
                if (identical(val, FALSE)) {
                    "negative"
                } else if (is.null(val)) {
                    "missing"
                } else {
                    "positive"
                }
            }, FUN.VALUE = character(1L)),
            stringsAsFactors = FALSE
        )
    }
    if (isTRUE(reset)) {
        .mojor_gpu_cache_stats_reset("gpu_kernel_cache_stats")
    }
    list(
        stats = stats,
        entries = entries
    )
}

.mojor_gpu_cache_diag <- function(include_entries = TRUE, reset = FALSE) {
    out <- list(
        capability = .mojor_gpu_capability_cache_diag(
            include_entries = include_entries,
            reset = FALSE
        ),
        kernel = .mojor_gpu_kernel_cache_diag(
            include_entries = include_entries,
            reset = FALSE
        )
    )
    if (isTRUE(reset)) {
        .mojor_gpu_cache_stats_reset("gpu_capability_cache_stats")
        .mojor_gpu_cache_stats_reset("gpu_kernel_cache_stats")
    }
    out
}

.mojor_gpu_binop_fn <- function(op, dtype, pattern) {
    if (identical(dtype, "f32") &&
        !requireNamespace("float", quietly = TRUE)) {
        stop("mojor_gpu: float package required for f32 kernels")
    }
    alloc <- if (identical(dtype, "f32"))
        "float::float32" else "numeric"
    if (identical(pattern, "arr_arr")) {
        txt <- sprintf(
            "function(x, y) { out <- %s(length(x)); for (i in seq_along(x)) { out[i] <- x[i] %s y[i] }; out }",
            alloc, op
        )
    } else if (identical(pattern, "arr_sca")) {
        txt <- sprintf(
            "function(x, y) { out <- %s(length(x)); for (i in seq_along(x)) { out[i] <- x[i] %s y }; out }",
            alloc, op
        )
    } else if (identical(pattern, "sca_arr")) {
        txt <- sprintf(
            "function(x, y) { out <- %s(length(y)); for (i in seq_along(y)) { out[i] <- x %s y[i] }; out }",
            alloc, op
        )
    } else {
        stop("mojor_gpu: unknown binop pattern")
    }
    eval(parse(text = txt))
}

.mojor_gpu_get_binop_kernel <- function(op, dtype, pattern) {
    key <- paste0("gpu_binop:", op, ":", dtype, ":", pattern)
    cached <- .mojor_gpu_kernel_cache_get(key)
    if (!is.null(cached)) {
        return(cached)
    }
    f <- .mojor_gpu_binop_fn(op, dtype, pattern)
    args <- if (identical(pattern, "arr_arr")) {
        list(
            x = paste0(dtype, "[]"),
            y = paste0(dtype, "[]")
        )
    } else if (identical(pattern, "arr_sca")) {
        list(
            x = paste0(dtype, "[]"),
            y = dtype
        )
    } else {
        list(x = dtype, y = paste0(dtype, "[]"))
    }
    name <- paste0(
        "mojor_gpu_binop_", gsub("[^a-z0-9]+", "_", op),
        "_", dtype, "_", pattern
    )
    built <- do.call(
        mojor_build, c(
            list(
                f, name = name, elementwise = TRUE, elementwise_target = "gpu",
                cache = TRUE, load = TRUE
            ),
            args
        )
    )
    .mojor_gpu_kernel_cache_set(key, built)
}

.mojor_gpu_unary_math_fn <- function(op, dtype, include_digits = FALSE) {
    if (identical(dtype, "f32") &&
        !requireNamespace("float", quietly = TRUE)) {
        stop("mojor_gpu: float package required for f32 kernels")
    }
    alloc <- if (identical(dtype, "f32"))
        "float::float32" else "numeric"
    unary_expr <- switch(
        op,
        "tan" = "(sin(x[i]) / cos(x[i]))",
        "expm1" = "(exp(x[i]) - exp(x[i] * 0))",
        "round" = "if (x[i] >= 0) floor(x[i] + 0.5) else ceiling(x[i] - 0.5)",
        "log10" = "log(x[i]) / log(10)",
        "log2" = "log(x[i]) / log(2)",
        "sign" = "if (x[i] > 0) (x[i] * 0 + 1) else if (x[i] < 0) (x[i] * 0 - 1) else x[i]",
        sprintf("%s(x[i])", op)
    )
    txt <- if (isTRUE(include_digits)) {
        sprintf(
            "function(x, digits) { out <- %s(length(x)); for (i in seq_along(x)) { out[i] <- round(x[i], digits = digits) }; out }",
            alloc
        )
    } else {
        sprintf(
            "function(x) { out <- %s(length(x)); for (i in seq_along(x)) { out[i] <- %s }; out }",
            alloc,
            unary_expr
        )
    }
    eval(parse(text = txt))
}

.mojor_gpu_unary_math_elementwise_size <- function(x) {
    if (!.mojor_gpu_is_array(x)) {
        return(NULL)
    }
    n <- .mojor_gpu_array_length(x)
    n_i <- suppressWarnings(as.integer(n[[1L]]))
    if (is.na(n_i) || n_i <= 0L) {
        return(NULL)
    }
    n_i
}

.mojor_gpu_unary_math_kernel_rev <- function(op, include_digits = FALSE) {
    if (isTRUE(include_digits)) {
        return("v1")
    }
    if (op %in% c("tan", "expm1")) {
        return("v2")
    }
    if (identical(op, "round")) {
        return("v3")
    }
    "v1"
}

.mojor_gpu_get_unary_math_kernel <- function(op, dtype, include_digits = FALSE, elementwise_size = NULL) {
    kernel_rev <- .mojor_gpu_unary_math_kernel_rev(op, include_digits = include_digits)
    size_key <- "dyn"
    if (!is.null(elementwise_size)) {
        elementwise_size <- suppressWarnings(as.integer(elementwise_size[[1L]]))
        if (!is.na(elementwise_size) && elementwise_size > 0L) {
            size_key <- as.character(elementwise_size)
        } else {
            elementwise_size <- NULL
        }
    }
    key <- paste0(
        "gpu_unary_math:",
        op,
        ":",
        dtype,
        ":",
        include_digits,
        ":rev=",
        kernel_rev,
        ":n=",
        size_key
    )
    cached <- .mojor_gpu_kernel_cache_get(key)
    if (identical(cached, FALSE)) {
        return(NULL)
    }
    if (!is.null(cached)) {
        return(cached)
    }
    f <- .mojor_gpu_unary_math_fn(op, dtype, include_digits = include_digits)
    args <- if (isTRUE(include_digits)) {
        list(
            x = paste0(dtype, "[]"),
            digits = "i32"
        )
    } else {
        list(x = paste0(dtype, "[]"))
    }
    name <- paste0(
        "mojor_gpu_unary_math_",
        gsub("[^a-z0-9]+", "_", op),
        "_", dtype,
        if (isTRUE(include_digits))
            "_digits" else "",
        "_",
        kernel_rev
    )
    build_args <- list(
        f,
        name = name,
        elementwise = TRUE,
        elementwise_target = "gpu",
        elementwise_gpu_layouttensor = FALSE,
        cache = TRUE,
        load = TRUE
    )
    if (!is.null(elementwise_size)) {
        build_args$elementwise_size <- elementwise_size
    }
    built <- tryCatch(
        do.call(mojor_build, c(build_args, args)),
        error = function(e) NULL
    )
    if (is.null(built)) {
        .mojor_gpu_kernel_cache_set(key, FALSE)
        return(NULL)
    }
    .mojor_gpu_kernel_cache_set(key, built)
}

.mojor_gpu_call_kernel <- function(built, ..., allow_host_wrapper = TRUE) {
    if (!is.null(built$gpu_func_raw)) {
        out <- tryCatch(
            built$gpu_func_raw(...),
            error = function(e) NULL
        )
        if (!is.null(out)) {
            return(out)
        }
    }
    if (isTRUE(allow_host_wrapper) && !is.null(built$gpu_func)) {
        return(built$gpu_func(...))
    }
    stop("mojor_gpu: gpu kernel not available")
}

.mojor_gpu_call_kernel_raw_status <- function(built, ...) {
    if (is.null(built$gpu_func_raw)) {
        return(
            list(
                ok = FALSE,
                out = NULL,
                reason = "gpu raw unary kernel unavailable",
                reason_code = "raw_kernel_unavailable"
            )
        )
    }
    out <- tryCatch(
        built$gpu_func_raw(...),
        error = function(e) NULL
    )
    if (is.null(out)) {
        return(
            list(
                ok = FALSE,
                out = NULL,
                reason = "gpu unary kernel dispatch failed",
                reason_code = "kernel_dispatch_failed"
            )
        )
    }
    list(ok = TRUE, out = out, reason = NULL, reason_code = NULL)
}

.mojor_gpu_call_unary_wrapper_status <- function(built, x, include_digits = FALSE, digits = 0L) {
    if (is.null(built$gpu_func)) {
        return(
            list(
                ok = FALSE,
                out = NULL,
                reason = "gpu unary host wrapper unavailable",
                reason_code = "kernel_wrapper_unavailable"
            )
        )
    }
    out <- tryCatch(
        {
            if (isTRUE(include_digits)) {
                built$gpu_func(x, digits)
            } else {
                built$gpu_func(x)
            }
        },
        error = function(e) NULL
    )
    if (is.null(out)) {
        return(
            list(
                ok = FALSE,
                out = NULL,
                reason = "gpu unary host wrapper dispatch failed",
                reason_code = "kernel_wrapper_dispatch_failed"
            )
        )
    }
    list(ok = TRUE, out = out, reason = NULL, reason_code = NULL)
}

.mojor_gpu_set_dim <- function(out, ref) {
    ref_dim <- .mojor_gpu_object_get(ref, "dim", default = NULL)
    if (!is.null(ref_dim)) {
        out <- .mojor_gpu_object_set(out, "dim", ref_dim)
        out <- .mojor_gpu_object_set(
            out, "dimnames", .mojor_gpu_object_get(ref, "dimnames", default = NULL)
        )
        out <- .mojor_gpu_object_set(out, "strides", .mojor_dim_strides(ref_dim))
    }
    out
}

.mojor_gpu_drop_dim <- function(out) {
    out <- .mojor_gpu_object_set(out, "dim", NULL)
    out <- .mojor_gpu_object_set(out, "dimnames", NULL)
    out <- .mojor_gpu_object_set(out, "strides", NULL)
    out
}

.mojor_gpu_host_binop <- function(op, lhs, rhs) {
    switch(
        op,
        "+" = lhs + rhs,
        "-" = lhs - rhs,
        "*" = lhs * rhs,
        "/" = lhs / rhs,
        "^" = lhs ^ rhs,
        "%%" = lhs %% rhs,
        "%/%" = lhs %/% rhs,
        stop("mojor_gpu: unsupported arithmetic operator '", op, "'")
    )
}

.mojor_gpu_host_unary_math <- function(op, x, digits = 0) {
    switch(
        op,
        "sin" = sin(x),
        "cos" = cos(x),
        "tan" = tan(x),
        "exp" = exp(x),
        "log" = log(x),
        "log10" = log10(x),
        "log2" = log2(x),
        "log1p" = log1p(x),
        "expm1" = expm1(x),
        "sqrt" = sqrt(x),
        "abs" = abs(x),
        "sign" = sign(x),
        "trunc" = trunc(x),
        "round" = round(x, digits = digits),
        "floor" = floor(x),
        "ceiling" = ceiling(x),
        stop("mojor_gpu: unsupported Math operator '", op, "'")
    )
}

.mojor_gpu_unary_math_dispatch <- function(op, x, digits = 0) {
    if (!.mojor_gpu_is_array(x)) {
        stop("mojor_gpu: unary math expects GPUArray input")
    }
    api <- .mojor_gpu_pick_api(x)
    dtype <- .mojor_gpu_array_dtype(x)
    x_eval <- x
    tmp <- list()
    on.exit(.mojor_gpu_free_temp_arrays(tmp), add = TRUE)

    include_digits <- FALSE
    try_gpu <- TRUE
    digits_i <- 0L
    cpu_reason <- "gpu unary math kernel unavailable"
    cpu_reason_code <- "kernel_dispatch_failed"

    if (isTRUE(try_gpu) &&
        dtype == "i32" &&
        op %in% c(
            "sin", "cos", "tan",
            "exp", "log", "log1p", "expm1",
            "sqrt"
        )) {
        x_cast <- gpu_cast(x, dtype = "f32")
        if (!identical(x_cast, x)) {
            tmp <- c(tmp, list(x_cast))
            x_eval <- x_cast
            dtype <- .mojor_gpu_array_dtype(x_eval)
        }
    }

    if (identical(op, "round")) {
        if (length(digits) != 1L ||
            is.na(digits[[1L]]) ||
            !is.finite(as.numeric(digits[[1L]]))) {
            try_gpu <- FALSE
            cpu_reason <- "round digits must be a finite scalar; using host fallback"
            cpu_reason_code <- "invalid_round_digits"
        } else {
            digits_i <- suppressWarnings(as.integer(digits[[1L]]))
            if (is.na(digits_i)) {
                try_gpu <- FALSE
                cpu_reason <- "round digits cast failed; using host fallback"
                cpu_reason_code <- "invalid_round_digits"
            } else if (digits_i != 0L) {
                try_gpu <- FALSE
                cpu_reason <- "round with non-zero digits uses host fallback"
                cpu_reason_code <- "round_digits_host_fallback"
            }
        }
    }

    if (isTRUE(try_gpu) &&
        identical(.mojor_gpu_api_name(api), "metal") &&
        op %in% c("log", "log10", "log2")) {
        try_gpu <- FALSE
        cpu_reason <- "gpu unary kernel dispatch failed"
        cpu_reason_code <- "kernel_dispatch_failed"
    }

    if (isTRUE(try_gpu)) {
        built <- tryCatch(
            {
                .mojor_gpu_get_unary_math_kernel(
                    op,
                    dtype,
                    include_digits = include_digits,
                    elementwise_size = .mojor_gpu_unary_math_elementwise_size(x_eval)
                )
            },
            error = function(e) NULL
        )
        if (!is.null(built)) {
            raw_status <- .mojor_gpu_call_kernel_raw_status(built, x_eval)
            if (isTRUE(raw_status$ok)) {
                return(.mojor_gpu_set_dim(raw_status$out, x))
            }
            allow_wrapper <- !(op %in% c("log", "log10", "log2", "log1p"))
            if (isTRUE(allow_wrapper)) {
                wrapper_status <- .mojor_gpu_call_unary_wrapper_status(
                    built,
                    x_eval,
                    include_digits = include_digits,
                    digits = digits_i
                )
                if (isTRUE(wrapper_status$ok)) {
                    return(.mojor_gpu_set_dim(wrapper_status$out, x))
                }
            }
            cpu_reason <- raw_status$reason
            cpu_reason_code <- raw_status$reason_code
        }
    }

    out_host <- .mojor_gpu_host_unary_math(
        op,
        mojor_gpu_array_read(x),
        digits = digits
    )
    .mojor_gpu_wrap_host_op_result(
        out_host,
        api = api,
        route = "cpu_arith",
        reason = cpu_reason,
        reason_code = cpu_reason_code
    )
}

.mojor_gpu_binary_math_fn <- function(op, dtype, pattern) {
    if (identical(dtype, "f32") &&
        !requireNamespace("float", quietly = TRUE)) {
        stop("mojor_gpu: float package required for f32 kernels")
    }
    alloc <- if (identical(dtype, "f32"))
        "float::float32" else "numeric"
    is_minmax <- op %in% c("pmin", "pmax")
    rhs_expr <- if (is_minmax) {
        if (identical(pattern, "arr_arr")) {
            sprintf(
                "if (is.na(y[i])) y[i] else if (is.na(x[i])) x[i] else %s(x[i], y[i])",
                op
            )
        } else if (identical(pattern, "arr_sca")) {
            sprintf(
                "if (is.na(y)) y else if (is.na(x[i])) x[i] else %s(x[i], y)",
                op
            )
        } else if (identical(pattern, "sca_arr")) {
            sprintf(
                "if (is.na(y[i])) y[i] else if (is.na(x)) x else %s(x, y[i])",
                op
            )
        } else {
            stop("mojor_gpu: unknown binary math pattern")
        }
    } else {
        if (identical(pattern, "arr_arr")) {
            sprintf("%s(x[i], y[i])", op)
        } else if (identical(pattern, "arr_sca")) {
            sprintf("%s(x[i], y)", op)
        } else if (identical(pattern, "sca_arr")) {
            sprintf("%s(x, y[i])", op)
        } else {
            stop("mojor_gpu: unknown binary math pattern")
        }
    }
    if (identical(pattern, "arr_arr")) {
        txt <- sprintf(
            "function(x, y) { out <- %s(length(x)); for (i in seq_along(x)) { out[i] <- %s }; out }",
            alloc,
            rhs_expr
        )
    } else if (identical(pattern, "arr_sca")) {
        txt <- sprintf(
            "function(x, y) { out <- %s(length(x)); for (i in seq_along(x)) { out[i] <- %s }; out }",
            alloc,
            rhs_expr
        )
    } else if (identical(pattern, "sca_arr")) {
        txt <- sprintf(
            "function(x, y) { out <- %s(length(y)); for (i in seq_along(y)) { out[i] <- %s }; out }",
            alloc,
            rhs_expr
        )
    } else {
        stop("mojor_gpu: unknown binary math pattern")
    }
    eval(parse(text = txt))
}

.mojor_gpu_binary_math_elementwise_size <- function(op, dtype, pattern, e1 = NULL, e2 = NULL) {
    n <- NULL
    if (identical(pattern, "arr_arr")) {
        if (.mojor_gpu_is_array(e1)) {
            n <- .mojor_gpu_array_length(e1)
        } else if (.mojor_gpu_is_array(e2)) {
            n <- .mojor_gpu_array_length(e2)
        }
    } else if (identical(pattern, "arr_sca") && .mojor_gpu_is_array(e1)) {
        n <- .mojor_gpu_array_length(e1)
    } else if (identical(pattern, "sca_arr") && .mojor_gpu_is_array(e2)) {
        n <- .mojor_gpu_array_length(e2)
    }
    if (is.null(n)) {
        return(NULL)
    }
    n_i <- suppressWarnings(as.integer(n[[1L]]))
    if (is.na(n_i) || n_i <= 0L) {
        return(NULL)
    }
    n_i
}

.mojor_gpu_get_binary_math_kernel <- function(op, dtype, pattern, elementwise_size = NULL) {
    size_key <- "dyn"
    if (!is.null(elementwise_size)) {
        elementwise_size <- suppressWarnings(as.integer(elementwise_size[[1L]]))
        if (!is.na(elementwise_size) && elementwise_size > 0L) {
            size_key <- as.character(elementwise_size)
        } else {
            elementwise_size <- NULL
        }
    }
    key <- paste0("gpu_binary_math:", op, ":", dtype, ":", pattern, ":n=", size_key)
    cached <- .mojor_gpu_kernel_cache_get(key)
    if (identical(cached, FALSE)) {
        return(NULL)
    }
    if (!is.null(cached)) {
        return(cached)
    }
    f <- .mojor_gpu_binary_math_fn(op, dtype, pattern)
    args <- if (identical(pattern, "arr_arr")) {
        list(
            x = paste0(dtype, "[]"),
            y = paste0(dtype, "[]")
        )
    } else if (identical(pattern, "arr_sca")) {
        list(
            x = paste0(dtype, "[]"),
            y = dtype
        )
    } else {
        list(
            x = dtype,
            y = paste0(dtype, "[]")
        )
    }
    name <- paste0(
        "mojor_gpu_binary_math_",
        gsub("[^a-z0-9]+", "_", op),
        "_",
        dtype,
        "_",
        pattern
    )
    build_args <- list(
        f,
        name = name,
        elementwise = TRUE,
        elementwise_target = "gpu",
        cache = TRUE,
        load = TRUE
    )
    if (!is.null(elementwise_size)) {
        build_args$elementwise_size <- elementwise_size
    }
    if (op %in% c("pmin", "pmax", "atan2")) {
        build_args$elementwise_gpu_layouttensor <- FALSE
    }
    built <- tryCatch(
        do.call(mojor_build, c(build_args, args)),
        error = function(e) NULL
    )
    if (is.null(built)) {
        .mojor_gpu_kernel_cache_set(key, FALSE)
        return(NULL)
    }
    attr(built, "mojor_kernel_cache_key") <- key
    .mojor_gpu_kernel_cache_set(key, built)
}

.mojor_gpu_mark_kernel_dispatch_failure <- function(built) {
    if (is.null(built)) {
        return(invisible(NULL))
    }
    key <- attr(built, "mojor_kernel_cache_key", exact = TRUE)
    if (is.null(key) ||
        length(key) != 1L ||
        is.na(key[[1L]])) {
        return(invisible(NULL))
    }
    key_chr <- trimws(as.character(key[[1L]]))
    if (!nzchar(key_chr)) {
        return(invisible(NULL))
    }
    .mojor_gpu_kernel_cache_set(key_chr, FALSE)
    invisible(NULL)
}

.mojor_gpu_binary_math_kernel_scalar <- function(dtype, value) {
    if (identical(dtype, "i32")) {
        return(as.integer(value[[1L]]))
    }
    as.numeric(value[[1L]])
}

.mojor_gpu_host_binary_math <- function(op, lhs, rhs, na_rm = FALSE) {
    switch(
        op,
        "atan2" = atan2(lhs, rhs),
        "pmin" = pmin(lhs, rhs, na.rm = na_rm),
        "pmax" = pmax(lhs, rhs, na.rm = na_rm),
        stop("mojor_gpu: unsupported binary math function '", op, "'")
    )
}

.mojor_gpu_binary_math_apply <- function(op, e1, e2, na_rm = FALSE) {
    if (!op %in% c("atan2", "pmin", "pmax")) {
        stop("mojor_gpu: unsupported binary math function '", op, "'")
    }
    e1_is_arr <- .mojor_gpu_is_array(e1)
    e2_is_arr <- .mojor_gpu_is_array(e2)
    if (!isTRUE(e1_is_arr) &&
        !isTRUE(e2_is_arr)) {
        stop("mojor_gpu: binary math expects at least one GPUArray operand")
    }
    promoted_tmp <- list()
    add_tmp <- function(buf) {
        if (!inherits(buf, "GPUArray") &&
            !inherits(buf, "mojor_gpu_array")) {
            return(invisible(NULL))
        }
        if (any(vapply(promoted_tmp, identical, logical(1), buf))) {
            return(invisible(NULL))
        }
        promoted_tmp[[length(promoted_tmp) + 1L]] <<- buf
        invisible(NULL)
    }
    target_dtype <- .mojor_gpu_promote_dtype(
        .mojor_gpu_value_dtype(e1),
        .mojor_gpu_value_dtype(e2)
    )
    if (op %in% c("pmin", "pmax")) {
        if (e1_is_arr && !e2_is_arr &&
            identical(.mojor_gpu_array_dtype(e1), "f32")) {
            target_dtype <- "f32"
        } else if (e2_is_arr && !e1_is_arr &&
            identical(.mojor_gpu_array_dtype(e2), "f32")) {
            target_dtype <- "f32"
        }
    }
    if (identical(op, "atan2") &&
        identical(target_dtype, "i32")) {
        target_dtype <- "f32"
    }
    if (e1_is_arr &&
        !identical(.mojor_gpu_array_dtype(e1), target_dtype)) {
        e1_new <- gpu_cast(e1, dtype = target_dtype)
        if (!identical(e1_new, e1)) {
            add_tmp(e1_new)
        }
        e1 <- e1_new
    }
    if (e2_is_arr &&
        !identical(.mojor_gpu_array_dtype(e2), target_dtype)) {
        e2_new <- gpu_cast(e2, dtype = target_dtype)
        if (!identical(e2_new, e2)) {
            add_tmp(e2_new)
        }
        e2 <- e2_new
    }
    if (length(promoted_tmp) > 0L) {
        on.exit(
            .mojor_gpu_free_temp_arrays(promoted_tmp),
            add = TRUE
        )
    }
    dtype <- target_dtype
    api <- .mojor_gpu_pick_api(e1, e2)
    api_name <- .mojor_gpu_api_name(api)
    e1_is_arr <- .mojor_gpu_is_array(e1)
    e2_is_arr <- .mojor_gpu_is_array(e2)
    e1_is_sca <- .mojor_gpu_is_scalar(e1)
    e2_is_sca <- .mojor_gpu_is_scalar(e2)
    gpu_supported <- !(op %in% c("pmin", "pmax") && isTRUE(na_rm))
    if (identical(op, "atan2") &&
        identical(api_name, "metal")) {
        gpu_supported <- FALSE
    }
    host_disabled_diag <- function() {
        if (identical(op, "atan2") &&
            identical(api_name, "metal")) {
            return(
                list(
                    reason = "binary math gpu kernel dispatch failed",
                    reason_code = "kernel_dispatch_failed"
                )
            )
        }
        list(
            reason = "binary math with na.rm routes through host parity",
            reason_code = "na_rm_host_parity"
        )
    }
    host_wrap <- function(lhs_host, rhs_host, reason = NULL, reason_code = NULL) {
        out_host <- .mojor_gpu_host_binary_math(
            op,
            lhs_host,
            rhs_host,
            na_rm = na_rm
        )
        .mojor_gpu_wrap_host_op_result(
            out_host,
            api = api,
            route = "cpu_arith",
            reason = reason,
            reason_code = reason_code
        )
    }

    if (e1_is_arr && e2_is_arr) {
        dim_mismatch <- FALSE
        if (!is.null(e1$dim) ||
            !is.null(e2$dim)) {
            dim_mismatch <- !identical(e1$dim, e2$dim)
        } else if (!identical(
            as.integer(.mojor_gpu_array_length(e1)),
            as.integer(.mojor_gpu_array_length(e2))
        )) {
            dim_mismatch <- TRUE
        }
        if (isTRUE(dim_mismatch)) {
            return(
                host_wrap(
                    mojor_gpu_array_read(e1),
                    mojor_gpu_array_read(e2),
                    reason = "binary math shape mismatch uses host parity",
                    reason_code = "shape_mismatch_host_parity"
                )
            )
        }
        if (isTRUE(gpu_supported)) {
            built <- NULL
            out <- tryCatch(
                {
                    built <- .mojor_gpu_get_binary_math_kernel(
                        op,
                        dtype,
                        "arr_arr",
                        elementwise_size = .mojor_gpu_binary_math_elementwise_size(
                            op,
                            dtype,
                            "arr_arr",
                            e1,
                            e2
                        )
                    )
                    if (is.null(built)) {
                        NULL
                    } else {
                        .mojor_gpu_call_kernel(built, e1, e2, allow_host_wrapper = FALSE)
                    }
                },
                error = function(e) NULL
            )
            if (!is.null(out)) {
                return(.mojor_gpu_set_dim(out, e1))
            }
            .mojor_gpu_mark_kernel_dispatch_failure(built)
            return(
                host_wrap(
                    mojor_gpu_array_read(e1),
                    mojor_gpu_array_read(e2),
                    reason = "binary math gpu kernel dispatch failed",
                    reason_code = "kernel_dispatch_failed"
                )
            )
        }
        disabled_diag <- host_disabled_diag()
        return(
            host_wrap(
                mojor_gpu_array_read(e1),
                mojor_gpu_array_read(e2),
                reason = disabled_diag$reason,
                reason_code = disabled_diag$reason_code
            )
        )
    }

    if (e1_is_arr && e2_is_sca) {
        if (isTRUE(gpu_supported)) {
            built <- NULL
            out <- tryCatch(
                {
                    built <- .mojor_gpu_get_binary_math_kernel(
                        op,
                        dtype,
                        "arr_sca",
                        elementwise_size = .mojor_gpu_binary_math_elementwise_size(
                            op,
                            dtype,
                            "arr_sca",
                            e1,
                            e2
                        )
                    )
                    if (is.null(built)) {
                        NULL
                    } else {
                        .mojor_gpu_call_kernel(
                            built,
                            e1,
                            .mojor_gpu_binary_math_kernel_scalar(dtype, e2),
                            allow_host_wrapper = FALSE
                        )
                    }
                },
                error = function(e) NULL
            )
            if (!is.null(out)) {
                return(.mojor_gpu_set_dim(out, e1))
            }
            .mojor_gpu_mark_kernel_dispatch_failure(built)
            return(
                host_wrap(
                    mojor_gpu_array_read(e1),
                    e2,
                    reason = "binary math gpu kernel dispatch failed",
                    reason_code = "kernel_dispatch_failed"
                )
            )
        }
        disabled_diag <- host_disabled_diag()
        return(
            host_wrap(
                mojor_gpu_array_read(e1),
                e2,
                reason = disabled_diag$reason,
                reason_code = disabled_diag$reason_code
            )
        )
    }

    if (e1_is_sca && e2_is_arr) {
        if (isTRUE(gpu_supported)) {
            built <- NULL
            out <- tryCatch(
                {
                    built <- .mojor_gpu_get_binary_math_kernel(
                        op,
                        dtype,
                        "sca_arr",
                        elementwise_size = .mojor_gpu_binary_math_elementwise_size(
                            op,
                            dtype,
                            "sca_arr",
                            e1,
                            e2
                        )
                    )
                    if (is.null(built)) {
                        NULL
                    } else {
                        .mojor_gpu_call_kernel(
                            built,
                            .mojor_gpu_binary_math_kernel_scalar(dtype, e1),
                            e2,
                            allow_host_wrapper = FALSE
                        )
                    }
                },
                error = function(e) NULL
            )
            if (!is.null(out)) {
                if (op %in% c("pmin", "pmax")) {
                    return(.mojor_gpu_drop_dim(out))
                }
                return(.mojor_gpu_set_dim(out, e2))
            }
            .mojor_gpu_mark_kernel_dispatch_failure(built)
            return(
                host_wrap(
                    e1,
                    mojor_gpu_array_read(e2),
                    reason = "binary math gpu kernel dispatch failed",
                    reason_code = "kernel_dispatch_failed"
                )
            )
        }
        disabled_diag <- host_disabled_diag()
        return(
            host_wrap(
                e1,
                mojor_gpu_array_read(e2),
                reason = disabled_diag$reason,
                reason_code = disabled_diag$reason_code
            )
        )
    }

    if (e1_is_arr && is.numeric(e2)) {
        if (isTRUE(gpu_supported)) {
            out <- NULL
            e2_arr <- NULL
            built <- NULL
            out <- tryCatch(
                {
                    e2_arr <- mojor_gpu_array(e2, api = api, dtype = dtype)
                    built <- .mojor_gpu_get_binary_math_kernel(
                        op,
                        dtype,
                        "arr_arr",
                        elementwise_size = .mojor_gpu_binary_math_elementwise_size(
                            op,
                            dtype,
                            "arr_arr",
                            e1,
                            e2_arr
                        )
                    )
                    if (is.null(built)) {
                        NULL
                    } else {
                        .mojor_gpu_call_kernel(
                            built,
                            e1,
                            e2_arr,
                            allow_host_wrapper = FALSE
                        )
                    }
                },
                error = function(e) NULL
            )
            if (inherits(e2_arr, "mojor_gpu_array")) {
                mojor_gpu_array_free(e2_arr)
            }
            if (!is.null(out)) {
                return(.mojor_gpu_set_dim(out, e1))
            }
            .mojor_gpu_mark_kernel_dispatch_failure(built)
            return(
                host_wrap(
                    mojor_gpu_array_read(e1),
                    e2,
                    reason = "binary math gpu kernel dispatch failed",
                    reason_code = "kernel_dispatch_failed"
                )
            )
        }
        disabled_diag <- host_disabled_diag()
        return(
            host_wrap(
                mojor_gpu_array_read(e1),
                e2,
                reason = disabled_diag$reason,
                reason_code = disabled_diag$reason_code
            )
        )
    }

    if (e2_is_arr && is.numeric(e1)) {
        if (isTRUE(gpu_supported)) {
            out <- NULL
            e1_arr <- NULL
            built <- NULL
            out <- tryCatch(
                {
                    e1_arr <- mojor_gpu_array(e1, api = api, dtype = dtype)
                    built <- .mojor_gpu_get_binary_math_kernel(
                        op,
                        dtype,
                        "arr_arr",
                        elementwise_size = .mojor_gpu_binary_math_elementwise_size(
                            op,
                            dtype,
                            "arr_arr",
                            e1_arr,
                            e2
                        )
                    )
                    if (is.null(built)) {
                        NULL
                    } else {
                        .mojor_gpu_call_kernel(
                            built,
                            e1_arr,
                            e2,
                            allow_host_wrapper = FALSE
                        )
                    }
                },
                error = function(e) NULL
            )
            if (inherits(e1_arr, "mojor_gpu_array")) {
                mojor_gpu_array_free(e1_arr)
            }
            if (!is.null(out)) {
                if (op %in% c("pmin", "pmax")) {
                    return(.mojor_gpu_drop_dim(out))
                }
                return(.mojor_gpu_set_dim(out, e2))
            }
            .mojor_gpu_mark_kernel_dispatch_failure(built)
            return(
                host_wrap(
                    e1,
                    mojor_gpu_array_read(e2),
                    reason = "binary math gpu kernel dispatch failed",
                    reason_code = "kernel_dispatch_failed"
                )
            )
        }
        disabled_diag <- host_disabled_diag()
        return(
            host_wrap(
                e1,
                mojor_gpu_array_read(e2),
                reason = disabled_diag$reason,
                reason_code = disabled_diag$reason_code
            )
        )
    }

    stop("mojor_gpu: unsupported operand types")
}

.mojor_gpu_binop_apply <- function(op, e1, e2) {
    e1_is_arr <- .mojor_gpu_is_array(e1)
    e2_is_arr <- .mojor_gpu_is_array(e2)
    promoted_tmp <- list()
    add_tmp <- function(buf) {
        if (!inherits(buf, "GPUArray") &&
            !inherits(buf, "mojor_gpu_array")) {
            return(invisible(NULL))
        }
        if (any(vapply(promoted_tmp, identical, logical(1), buf))) {
            return(invisible(NULL))
        }
        promoted_tmp[[length(promoted_tmp) + 1L]] <<- buf
        invisible(NULL)
    }
    target_dtype <- .mojor_gpu_promote_dtype(
        .mojor_gpu_value_dtype(e1),
        .mojor_gpu_value_dtype(e2)
    )
    if (e1_is_arr &&
        !identical(.mojor_gpu_array_dtype(e1), target_dtype)) {
        e1_new <- gpu_cast(e1, dtype = target_dtype)
        if (!identical(e1_new, e1)) {
            add_tmp(e1_new)
        }
        e1 <- e1_new
    }
    if (e2_is_arr &&
        !identical(.mojor_gpu_array_dtype(e2), target_dtype)) {
        e2_new <- gpu_cast(e2, dtype = target_dtype)
        if (!identical(e2_new, e2)) {
            add_tmp(e2_new)
        }
        e2 <- e2_new
    }
    if (length(promoted_tmp) > 0L) {
        on.exit(
            .mojor_gpu_free_temp_arrays(promoted_tmp),
            add = TRUE
        )
    }
    dtype <- target_dtype
    api <- .mojor_gpu_pick_api(e1, e2)
    e1_is_arr <- .mojor_gpu_is_array(e1)
    e2_is_arr <- .mojor_gpu_is_array(e2)
    e1_is_sca <- .mojor_gpu_is_scalar(e1)
    e2_is_sca <- .mojor_gpu_is_scalar(e2)

    if (e1_is_arr && e2_is_arr) {
        if (!is.null(e1$dim) ||
            !is.null(e2$dim)) {
            if (!identical(e1$dim, e2$dim)) {
                lhs <- mojor_gpu_array_read(e1)
                rhs <- mojor_gpu_array_read(e2)
                out_host <- .mojor_gpu_host_binop(op, lhs, rhs)
                return(
                    .mojor_gpu_wrap_host_op_result(
                        out_host,
                        api = api,
                        route = "cpu_arith"
                    )
                )
            }
        }
        built <- .mojor_gpu_get_binop_kernel(op, dtype, "arr_arr")
        out <- .mojor_gpu_call_kernel(built, e1, e2)
        return(.mojor_gpu_set_dim(out, e1))
    }

    if (e1_is_arr && e2_is_sca) {
        built <- .mojor_gpu_get_binop_kernel(op, dtype, "arr_sca")
        out <- .mojor_gpu_call_kernel(built, e1, .mojor_gpu_scalar_cast(dtype, e2))
        return(.mojor_gpu_set_dim(out, e1))
    }

    if (e1_is_sca && e2_is_arr) {
        built <- .mojor_gpu_get_binop_kernel(op, dtype, "sca_arr")
        out <- .mojor_gpu_call_kernel(
            built, .mojor_gpu_scalar_cast(dtype, e1),
            e2
        )
        return(.mojor_gpu_set_dim(out, e2))
    }

    if (e1_is_arr && is.numeric(e2)) {
        out <- NULL
        e2_arr <- NULL
        out <- tryCatch(
            {
                e2_arr <- mojor_gpu_array(e2, api = api, dtype = dtype)
                built <- .mojor_gpu_get_binop_kernel(op, dtype, "arr_arr")
                .mojor_gpu_call_kernel(built, e1, e2_arr)
            },
            error = function(e) NULL
        )
        if (inherits(e2_arr, "mojor_gpu_array")) {
            mojor_gpu_array_free(e2_arr)
        }
        if (!is.null(out)) {
            return(.mojor_gpu_set_dim(out, e1))
        }
        out_host <- .mojor_gpu_host_binop(op, mojor_gpu_array_read(e1), e2)
        return(
            .mojor_gpu_wrap_host_op_result(
                out_host,
                api = api,
                route = "cpu_arith"
            )
        )
    }

    if (e2_is_arr && is.numeric(e1)) {
        out <- NULL
        e1_arr <- NULL
        out <- tryCatch(
            {
                e1_arr <- mojor_gpu_array(e1, api = api, dtype = dtype)
                built <- .mojor_gpu_get_binop_kernel(op, dtype, "arr_arr")
                .mojor_gpu_call_kernel(built, e1_arr, e2)
            },
            error = function(e) NULL
        )
        if (inherits(e1_arr, "mojor_gpu_array")) {
            mojor_gpu_array_free(e1_arr)
        }
        if (!is.null(out)) {
            return(.mojor_gpu_set_dim(out, e2))
        }
        out_host <- .mojor_gpu_host_binop(op, e1, mojor_gpu_array_read(e2))
        return(
            .mojor_gpu_wrap_host_op_result(
                out_host,
                api = api,
                route = "cpu_arith"
            )
        )
    }

    stop("mojor_gpu: unsupported operand types")
}

.mojor_gpu_host_values <- function(x) {
    if (.mojor_gpu_is_array(x)) {
        return(mojor_gpu_array_read(x))
    }
    x
}

.mojor_gpu_wrap_host_op_result <- function(
    out_host, api, route, reason = NULL, reason_code = NULL
) {
    if (is.logical(out_host)) {
        out_vals <- as.integer(out_host)
        out_dtype <- "i32"
    } else {
        out_vals <- out_host
        out_dtype <- .mojor_gpu_dtype_from_input(out_vals, "auto")
    }
    out <- mojor_gpu_array(out_vals, api = api, dtype = out_dtype)
    out_dim <- dim(out_host)
    if (!is.null(out_dim)) {
        out_dim_i <- as.integer(out_dim)
        out <- .mojor_gpu_object_set(out, "dim", out_dim_i)
        out <- .mojor_gpu_object_set(out, "dimnames", dimnames(out_host))
        out <- .mojor_gpu_object_set(out, "strides", .mojor_dim_strides(out_dim_i))
    }
    .mojor_gpu_route_tag(
        out,
        route,
        reason = reason,
        reason_code = reason_code
    )
}

.mojor_gpu_compare_dispatch <- function(op, e1, e2) {
    lhs <- .mojor_gpu_host_values(e1)
    rhs <- .mojor_gpu_host_values(e2)
    out_host <- switch(
        op,
        "<" = lhs < rhs,
        ">" = lhs > rhs,
        "<=" = lhs <= rhs,
        ">=" = lhs >= rhs,
        "==" = lhs == rhs,
        "!=" = lhs != rhs,
        stop("mojor_gpu: unsupported comparison operator '", op, "'")
    )
    .mojor_gpu_wrap_host_op_result(
        out_host,
        api = .mojor_gpu_pick_api(e1, e2),
        route = "cpu_compare"
    )
}

.mojor_gpu_logic_dispatch <- function(op, e1, e2 = NULL, unary = FALSE) {
    out_host <- NULL
    lhs <- .mojor_gpu_host_values(e1)
    if (isTRUE(unary)) {
        out_host <- !lhs
    } else if (identical(op, "&")) {
        rhs <- .mojor_gpu_host_values(e2)
        out_host <- lhs & rhs
    } else if (identical(op, "|")) {
        rhs <- .mojor_gpu_host_values(e2)
        out_host <- lhs | rhs
    } else {
        stop("mojor_gpu: unsupported logical operator '", op, "'")
    }
    .mojor_gpu_wrap_host_op_result(
        out_host,
        api = .mojor_gpu_pick_api(e1, e2),
        route = "cpu_logic"
    )
}

.mojor_gpu_arith_dispatch <- function(op, e1, e2) {
    if (op %in% c("+", "-", "*", "/", "^", "%%", "%/%")) {
        if (missing(e2)) {
            if (op == "+")
                return(e1)
            if (op != "-")
                stop("mojor_gpu: unary operation not supported")
            return(.mojor_gpu_binop_apply("*", e1, -1))
        }
        return(.mojor_gpu_binop_apply(op, e1, e2))
    }
    if (op %in% c("<", ">", "<=", ">=", "==", "!=")) {
        if (missing(e2)) {
            stop("mojor_gpu: unary comparison is not supported")
        }
        return(.mojor_gpu_compare_dispatch(op, e1, e2))
    }
    if (identical(op, "!")) {
        if (!missing(e2)) {
            stop("mojor_gpu: unary ! expects one operand")
        }
        return(.mojor_gpu_logic_dispatch(op, e1, unary = TRUE))
    }
    if (op %in% c("&", "|")) {
        if (missing(e2)) {
            stop("mojor_gpu: binary logical operator requires two operands")
        }
        return(.mojor_gpu_logic_dispatch(op, e1, e2, unary = FALSE))
    }
    stop("mojor_gpu: unsupported operator '", op, "'")
}

mojor_gpu_kernel <- function(
    fn, ..., name = NULL, cache = TRUE, load = TRUE, broadcast = "none",
    elementwise_size = NULL, elementwise_gpu_layouttensor = NULL
) {
    if (!is.function(fn))
        stop("mojor_gpu_kernel: fn must be a function")
    built <- do.call(
        mojor_build, c(
            list(
                fn, name = name, cache = cache, load = TRUE, elementwise = TRUE,
                elementwise_target = "gpu", broadcast = broadcast, elementwise_size = elementwise_size,
                elementwise_gpu_layouttensor = elementwise_gpu_layouttensor
            ),
            list(...)
        )
    )
    if (!isTRUE(load)) {
        return(built)
    }
    out <- if (!is.null(built$gpu_func_raw))
        built$gpu_func_raw else built$gpu_func
    if (is.null(out))
        stop("mojor_gpu_kernel: GPU kernel not available")
    attr(out, "mojor_build") <- built
    out
}

gpu_kernel <- function(
    fn, ..., name = NULL, cache = TRUE, load = TRUE, broadcast = "none",
    elementwise_size = NULL, elementwise_gpu_layouttensor = NULL
) {
    mojor_gpu_kernel(
        fn, ..., name = name, cache = cache, load = load, broadcast = broadcast,
        elementwise_size = elementwise_size, elementwise_gpu_layouttensor = elementwise_gpu_layouttensor
    )
}

.mojor_gpu_matmul_dispatch <- function(x, y) {
    x_is_arr <- .mojor_gpu_is_array(x)
    y_is_arr <- .mojor_gpu_is_array(y)
    if (!isTRUE(x_is_arr) &&
        !isTRUE(y_is_arr)) {
        stop("%*%.GPUArray: at least one operand must be GPUArray")
    }
    if (!isTRUE(x_is_arr) &&
        isTRUE(y_is_arr)) {
        prep <- .mojor_gpu_promote_linalg_host_rhs(y, x)
        y <- prep$x
        if (length(prep$tmp) > 0L) {
            on.exit(
                .mojor_gpu_free_temp_arrays(prep$tmp),
                add = TRUE
            )
        }
        api <- .mojor_gpu_pick_api(y, x)
        dtype <- prep$dtype
        if (!.mojor_gpu_linalg_dtype_supported(dtype)) {
            .mojor_gpu_stop_unsupported_linalg_dtype("mojor_gpu_matmul", dtype)
        }
        x_info <- .mojor_gpu_matmul_shape_info_host(x)
        y_info <- .mojor_gpu_matmul_shape_info(y)
        dims <- .mojor_gpu_matmul_dispatch_dims_from_info(x_info, y_info)
        try_gpu <- !is.null(dims)
        cpu_route_reason <- NULL
        cpu_route_reason_code <- NULL
        if (isTRUE(try_gpu) &&
            identical(dtype, "f64")) {
            if (isTRUE(.mojor_gpu_force_f64_cpu_fallback(api))) {
                try_gpu <- FALSE
                cpu_route_reason <- "f64 matmul disabled on metal backend"
                cpu_route_reason_code <- "disabled_on_metal_backend"
            } else {
                f64_mode <- .mojor_gpu_f64_matmul_mode(dims$x_dim2[[1L]], dims$y_dim2[[2L]])
                f64_ctx <- tryCatch(
                    .mojor_gpu_ctx_get(),
                    error = function(e) NULL
                )
                f64_ctx_epoch <- if (is.null(f64_ctx))
                    NULL else .mojor_gpu_ctx_epoch_get()
                f64_probe_ok <- isTRUE(.mojor_gpu_f64_matmul_capable(mode = f64_mode))
                try_gpu <- f64_probe_ok
                if (!isTRUE(f64_probe_ok)) {
                    probe_diag <- .mojor_gpu_f64_matmul_probe_diag(
                        mode = f64_mode,
                        ctx = f64_ctx,
                        ctx_epoch = f64_ctx_epoch
                    )
                    cpu_route_reason <- probe_diag$reason
                    cpu_route_reason_code <- probe_diag$code
                }
            }
        }
        x_host <- NULL
        if (isTRUE(try_gpu)) {
            x_host <- .mojor_gpu_linalg_host_rhs_values(
                x,
                dtype,
                op = "mojor_gpu_matmul"
            )
            x_arr <- mojor_gpu_array(x_host, api = api, dtype = dtype)
            on.exit(
                mojor_gpu_array_free(x_arr),
                add = TRUE
            )
            lhs <- .mojor_gpu_matmul_view(x_arr, dims$x_dim2)
            rhs <- .mojor_gpu_matmul_view(y, dims$y_dim2)
            out_gpu <- tryCatch(
                .mojor_gpu_matmul(lhs, rhs),
                error = function(e) NULL
            )
            if (!is.null(out_gpu)) {
                return(out_gpu)
            }
            cpu_route_reason <- "gpu matmul dispatch failed"
            cpu_route_reason_code <- "dispatch_failed"
        }
        if (is.null(x_host)) {
            x_host <- .mojor_gpu_linalg_host_rhs_values(
                x,
                dtype,
                op = "mojor_gpu_matmul"
            )
        }
        out_host <- x_host %*% mojor_gpu_array_read(y)
        out <- mojor_gpu_array(out_host, api = api, dtype = dtype)
        out <- .mojor_gpu_route_tag(
            out,
            "cpu_matmul",
            reason = cpu_route_reason,
            reason_code = cpu_route_reason_code
        )
        return(out)
    }
    if (!isTRUE(x_is_arr))
        stop("%*%.GPUArray: left operand must be GPUArray")
    y_is_host <- !is.null(y) &&
        !isTRUE(y_is_arr)
    y_arr <- if (isTRUE(y_is_arr))
        y else NULL
    prep <- if (isTRUE(y_is_host)) {
        .mojor_gpu_promote_linalg_host_rhs(x, y)
    } else {
        .mojor_gpu_promote_linalg_pair(x, y_arr)
    }
    x <- prep$x
    if (!isTRUE(y_is_host)) {
        y_arr <- prep$y
    }
    if (length(prep$tmp) > 0L) {
        on.exit(
            .mojor_gpu_free_temp_arrays(prep$tmp),
            add = TRUE
        )
    }
    api <- .mojor_gpu_pick_api(
        x,
        if (isTRUE(y_is_arr))
            y_arr else y
    )
    dtype <- prep$dtype
    if (!.mojor_gpu_linalg_dtype_supported(dtype)) {
        .mojor_gpu_stop_unsupported_linalg_dtype("mojor_gpu_matmul", dtype)
    }
    x_info <- .mojor_gpu_matmul_shape_info(x)
    y_info <- if (isTRUE(y_is_arr))
        .mojor_gpu_matmul_shape_info(y_arr) else .mojor_gpu_matmul_shape_info_host(y)
    dims <- .mojor_gpu_matmul_dispatch_dims_from_info(x_info, y_info)
    try_gpu <- !is.null(dims)
    cpu_route_reason <- NULL
    cpu_route_reason_code <- NULL
    if (isTRUE(try_gpu) &&
        identical(dtype, "f64")) {
        if (isTRUE(.mojor_gpu_force_f64_cpu_fallback(api))) {
            try_gpu <- FALSE
            cpu_route_reason <- "f64 matmul disabled on metal backend"
            cpu_route_reason_code <- "disabled_on_metal_backend"
        } else {
            f64_mode <- .mojor_gpu_f64_matmul_mode(dims$x_dim2[[1L]], dims$y_dim2[[2L]])
            f64_ctx <- tryCatch(
                .mojor_gpu_ctx_get(),
                error = function(e) NULL
            )
            f64_ctx_epoch <- if (is.null(f64_ctx))
                NULL else .mojor_gpu_ctx_epoch_get()
            f64_probe_ok <- isTRUE(.mojor_gpu_f64_matmul_capable(mode = f64_mode))
            try_gpu <- f64_probe_ok
            if (!isTRUE(f64_probe_ok)) {
                probe_diag <- .mojor_gpu_f64_matmul_probe_diag(
                    mode = f64_mode,
                    ctx = f64_ctx,
                    ctx_epoch = f64_ctx_epoch
                )
                cpu_route_reason <- probe_diag$reason
                cpu_route_reason_code <- probe_diag$code
            }
        }
    }
    y_host <- NULL
    if (isTRUE(y_is_host)) {
        if (isTRUE(try_gpu)) {
            if (identical(dtype, "i32")) {
                y_host <- .mojor_gpu_linalg_host_rhs_values(
                    y,
                    dtype,
                    op = "mojor_gpu_matmul"
                )
                y_arr <- mojor_gpu_array(y_host, api = api, dtype = dtype)
            } else {
                y_arr <- mojor_gpu_array(y, api = api, dtype = dtype)
            }
            on.exit(
                mojor_gpu_array_free(y_arr),
                add = TRUE
            )
        }
    }
    if (isTRUE(try_gpu) &&
        !is.null(y_arr)) {
        lhs <- .mojor_gpu_matmul_view(x, dims$x_dim2)
        rhs <- .mojor_gpu_matmul_view(y_arr, dims$y_dim2)
        out_gpu <- tryCatch(
            .mojor_gpu_matmul(lhs, rhs),
            error = function(e) NULL
        )
        if (!is.null(out_gpu)) {
            return(out_gpu)
        }
        cpu_route_reason <- "gpu matmul dispatch failed"
        cpu_route_reason_code <- "dispatch_failed"
    }

    hx <- mojor_gpu_array_read(x)
    if (isTRUE(y_is_arr)) {
        hy <- mojor_gpu_array_read(y_arr)
    } else {
        if (is.null(y_host)) {
            y_host <- .mojor_gpu_linalg_host_rhs_values(
                y,
                dtype,
                op = "mojor_gpu_matmul"
            )
        }
        hy <- y_host
    }
    out_host <- hx %*% hy
    out <- mojor_gpu_array(out_host, api = api, dtype = dtype)
    out <- .mojor_gpu_route_tag(
        out,
        "cpu_matmul",
        reason = cpu_route_reason,
        reason_code = cpu_route_reason_code
    )
    out
}

.mojor_gpu_require_i32_host_values <- function(y, op = "mojor_gpu_matmul") {
    y_vec <- as.numeric(as.vector(y))
    if (length(y_vec) == 0L) {
        return(invisible(NULL))
    }
    if (anyNA(y_vec) ||
        any(!is.finite(y_vec))) {
        stop(op, ": i32 host values must be finite integers")
    }
    if (any(y_vec != trunc(y_vec))) {
        stop(op, ": i32 host values must be integer-valued (no fractional coercion)")
    }
    int_min <- -as.double(.Machine$integer.max) - 1
    int_max <- as.double(.Machine$integer.max)
    if (any(y_vec < int_min) ||
        any(y_vec > int_max)) {
        stop(op, ": i32 host values must be within 32-bit integer range")
    }
    invisible(NULL)
}

.mojor_gpu_linalg_host_value_dtype <- function(y) {
    if (!is.null(y) &&
        .mojor_float_available() &&
        float::is.float(y)) {
        return("f32")
    }
    if (is.integer(y) &&
        !is.double(y)) {
        return("i32")
    }
    if (is.numeric(y) &&
        is.atomic(y)) {
        y_vec <- as.numeric(as.vector(y))
        if (length(y_vec) == 0L) {
            return("i32")
        }
        if (all(is.finite(y_vec)) &&
            all(y_vec == trunc(y_vec))) {
            int_min <- -as.double(.Machine$integer.max) - 1
            int_max <- as.double(.Machine$integer.max)
            if (all(y_vec >= int_min) &&
                all(y_vec <= int_max)) {
                return("i32")
            }
        }
        return("f64")
    }
    "f64"
}

.mojor_gpu_promote_linalg_host_rhs <- function(x, y) {
    tmp <- list()
    dtype <- .mojor_gpu_promote_dtype(
        .mojor_gpu_array_dtype(x),
        .mojor_gpu_linalg_host_value_dtype(y)
    )
    if (!identical(.mojor_gpu_array_dtype(x), dtype)) {
        x_new <- gpu_cast(x, dtype = dtype)
        if (!identical(x_new, x)) {
            tmp <- c(tmp, list(x_new))
        }
        x <- x_new
    }
    list(x = x, dtype = dtype, tmp = tmp)
}

.mojor_gpu_linalg_host_rhs_values <- function(y, dtype, op = "mojor_gpu_matmul") {
    if (identical(dtype, "i32")) {
        .mojor_gpu_require_i32_host_values(y, op = op)
    }
    y_host <- .mojor_gpu_cast_values(
        as.vector(y),
        dtype
    )
    y_dim <- dim(y)
    if (!is.null(y_dim)) {
        y_host <- array(y_host, dim = y_dim)
    }
    y_host
}

.mojor_gpu_matmul_shape_info <- function(x) {
    if (!inherits(x, "mojor_gpu_array")) {
        return(NULL)
    }
    n <- as.integer(.mojor_gpu_object_get(x, "n", default = 0L))
    if (n < 0L) {
        return(NULL)
    }

    dim_obj <- .mojor_gpu_object_get(x, "dim", default = NULL)
    if (is.null(dim_obj)) {
        return(list(kind = "vector", len = n))
    }

    dim_i <- as.integer(dim_obj)
    if (anyNA(dim_i) ||
        length(dim_i) ==
            0L) {
        return(NULL)
    }
    if (length(dim_i) ==
        1L) {
        if (!identical(
            as.integer(dim_i[[1L]]),
            n
        )) {
            return(NULL)
        }
        return(list(kind = "vector", len = as.integer(dim_i[[1L]])))
    }
    if (length(dim_i) ==
        2L) {
        if (!identical(
            as.integer(prod(dim_i)),
            n
        )) {
            return(NULL)
        }
        return(list(kind = "matrix", dim = dim_i))
    }
    NULL
}

.mojor_gpu_matmul_shape_info_host <- function(x) {
    if (!is.numeric(x)) {
        return(NULL)
    }
    n <- as.integer(length(x))
    if (n < 0L) {
        return(NULL)
    }

    dim_obj <- dim(x)
    if (is.null(dim_obj)) {
        return(list(kind = "vector", len = n))
    }

    dim_i <- as.integer(dim_obj)
    if (anyNA(dim_i) ||
        length(dim_i) ==
            0L) {
        return(NULL)
    }
    if (length(dim_i) ==
        1L) {
        if (!identical(
            as.integer(dim_i[[1L]]),
            n
        )) {
            return(NULL)
        }
        return(list(kind = "vector", len = as.integer(dim_i[[1L]])))
    }
    if (length(dim_i) ==
        2L) {
        if (!identical(
            as.integer(prod(dim_i)),
            n
        )) {
            return(NULL)
        }
        return(list(kind = "matrix", dim = dim_i))
    }
    NULL
}

.mojor_gpu_matmul_dispatch_dims_from_info <- function(x_info, y_info) {
    if (is.null(x_info) ||
        is.null(y_info)) {
        return(NULL)
    }

    if (identical(x_info$kind, "matrix") &&
        identical(y_info$kind, "matrix")) {
        return(
            list(
                x_dim2 = as.integer(x_info$dim),
                y_dim2 = as.integer(y_info$dim)
            )
        )
    }

    if (identical(x_info$kind, "matrix") &&
        identical(y_info$kind, "vector")) {
        if (!identical(
            as.integer(x_info$dim[[2L]]),
            as.integer(y_info$len)
        )) {
            return(NULL)
        }
        return(
            list(
                x_dim2 = as.integer(x_info$dim),
                y_dim2 = as.integer(c(y_info$len, 1L))
            )
        )
    }

    if (identical(x_info$kind, "vector") &&
        identical(y_info$kind, "matrix")) {
        if (!identical(
            as.integer(x_info$len),
            as.integer(y_info$dim[[1L]])
        )) {
            return(NULL)
        }
        return(
            list(
                x_dim2 = as.integer(c(1L, x_info$len)),
                y_dim2 = as.integer(y_info$dim)
            )
        )
    }

    if (identical(x_info$kind, "vector") &&
        identical(y_info$kind, "vector")) {
        if (!identical(
            as.integer(x_info$len),
            as.integer(y_info$len)
        )) {
            return(NULL)
        }
        return(
            list(
                x_dim2 = as.integer(c(1L, x_info$len)),
                y_dim2 = as.integer(c(y_info$len, 1L))
            )
        )
    }

    NULL
}

.mojor_gpu_matmul_dispatch_dims <- function(x, y) {
    x_info <- .mojor_gpu_matmul_shape_info(x)
    y_info <- if (inherits(y, "mojor_gpu_array"))
        .mojor_gpu_matmul_shape_info(y) else .mojor_gpu_matmul_shape_info_host(y)
    .mojor_gpu_matmul_dispatch_dims_from_info(x_info, y_info)
}

.mojor_gpu_matmul_view_dim <- function(x) {
    if (!inherits(x, "mojor_gpu_array")) {
        return(NULL)
    }
    n <- as.integer(.mojor_gpu_object_get(x, "n", default = 0L))
    if (n <= 0L) {
        return(NULL)
    }
    dim_obj <- .mojor_gpu_object_get(x, "dim", default = NULL)
    if (is.null(dim_obj)) {
        return(as.integer(c(n, 1L)))
    }
    dim_i <- as.integer(dim_obj)
    if (anyNA(dim_i) ||
        length(dim_i) ==
            0L) {
        return(NULL)
    }
    if (length(dim_i) ==
        1L) {
        dim2 <- as.integer(c(dim_i[[1L]], 1L))
    } else if (length(dim_i) ==
        2L) {
        dim2 <- dim_i
    } else {
        return(NULL)
    }
    if (as.integer(prod(dim2)) !=
        n) {
        return(NULL)
    }
    dim2
}

.mojor_gpu_matmul_view <- function(x, dim2) {
    .mojor_gpu_new_object(
        list(
            handle = .mojor_gpu_array_handle(x),
            data = NULL, n = as.integer(.mojor_gpu_object_get(x, "n", default = 0L)),
            handle_epoch = as.integer(.mojor_gpu_array_handle_epoch(x)),
            api = .mojor_gpu_array_api(x),
            dtype = .mojor_gpu_array_dtype(x),
            dim = as.integer(dim2),
            dimnames = NULL, index_plans = new.env(parent = emptyenv()),
            strides = .mojor_dim_strides(as.integer(dim2))
        ),
        class = "mojor_gpu_array"
    )
}

.mojor_gpu_crossprod_dispatch <- function(x, y = NULL) {
    y_is_arr <- .mojor_gpu_is_array(y)
    y_is_host <- !is.null(y) &&
        !isTRUE(y_is_arr)
    y_arr <- if (isTRUE(y_is_arr))
        y else NULL
    prep <- if (isTRUE(y_is_host)) {
        .mojor_gpu_promote_linalg_host_rhs(x, y)
    } else {
        .mojor_gpu_promote_linalg_pair(x, y_arr)
    }
    x <- prep$x
    if (!isTRUE(y_is_host)) {
        y_arr <- prep$y
    }
    if (length(prep$tmp) > 0L) {
        on.exit(
            .mojor_gpu_free_temp_arrays(prep$tmp),
            add = TRUE
        )
    }
    api <- .mojor_gpu_pick_api(
        x,
        if (isTRUE(y_is_arr))
            y_arr else y
    )
    dtype <- prep$dtype
    if (!.mojor_gpu_linalg_dtype_supported(dtype)) {
        .mojor_gpu_stop_unsupported_linalg_dtype("mojor_gpu_crossprod", dtype)
    }
    x_info <- .mojor_gpu_matmul_shape_info(x)
    y_info <- if (is.null(y)) {
        x_info
    } else if (isTRUE(y_is_arr)) {
        .mojor_gpu_matmul_shape_info(y_arr)
    } else {
        .mojor_gpu_matmul_shape_info_host(y)
    }
    dims <- .mojor_gpu_matmul_dispatch_dims_from_info(x_info, y_info)
    x_dim2 <- if (is.null(dims))
        NULL else dims$x_dim2
    y_dim2 <- if (is.null(dims))
        NULL else dims$y_dim2
    y_host <- NULL
    cpu_route_reason <- NULL
    cpu_route_reason_code <- NULL
    try_gpu <- !is.null(x_dim2) &&
        !is.null(y_dim2)
    if (isTRUE(try_gpu) &&
        identical(dtype, "f64")) {
        if (isTRUE(.mojor_gpu_force_f64_cpu_fallback(api))) {
            try_gpu <- FALSE
            cpu_route_reason <- "f64 matmul disabled on metal backend"
            cpu_route_reason_code <- "disabled_on_metal_backend"
        } else {
 # crossprod uses transpose_a=TRUE, transpose_b=FALSE
            f64_mode <- .mojor_gpu_f64_matmul_mode(x_dim2[[2L]], y_dim2[[2L]])
            f64_ctx <- tryCatch(
                .mojor_gpu_ctx_get(),
                error = function(e) NULL
            )
            f64_ctx_epoch <- if (is.null(f64_ctx))
                NULL else .mojor_gpu_ctx_epoch_get()
            f64_probe_ok <- isTRUE(.mojor_gpu_f64_matmul_capable(mode = f64_mode))
            try_gpu <- f64_probe_ok
            if (!isTRUE(f64_probe_ok)) {
                probe_diag <- .mojor_gpu_f64_matmul_probe_diag(
                    mode = f64_mode,
                    ctx = f64_ctx,
                    ctx_epoch = f64_ctx_epoch
                )
                cpu_route_reason <- probe_diag$reason
                cpu_route_reason_code <- probe_diag$code
            }
        }
    }
    if (isTRUE(y_is_host)) {
        if (isTRUE(try_gpu)) {
            if (identical(dtype, "i32")) {
                y_host <- .mojor_gpu_linalg_host_rhs_values(
                    y,
                    dtype,
                    op = "mojor_gpu_crossprod"
                )
                y_arr <- mojor_gpu_array(y_host, api = api, dtype = dtype)
            } else {
                y_arr <- mojor_gpu_array(y, api = api, dtype = dtype)
            }
            on.exit(
                mojor_gpu_array_free(y_arr),
                add = TRUE
            )
        }
    }
    if (isTRUE(try_gpu)) {
        lhs <- .mojor_gpu_matmul_view(x, x_dim2)
        rhs_src <- if (is.null(y))
            x else y_arr
        rhs_dim2 <- if (is.null(y))
            x_dim2 else y_dim2
        rhs <- .mojor_gpu_matmul_view(rhs_src, rhs_dim2)
        out_gpu <- tryCatch(
            .mojor_gpu_matmul(lhs, rhs, transpose_a = TRUE, transpose_b = FALSE),
            error = function(e) NULL
        )
        if (!is.null(out_gpu)) {
            out_route <- .mojor_gpu_route_value(out_gpu)
            out_reason <- .mojor_gpu_route_reason_value(out_gpu)
            out_reason_code <- .mojor_gpu_route_reason_code_value(out_gpu)
            if (identical(out_route, "cpu_matmul")) {
                out_gpu <- .mojor_gpu_route_tag(
                    out_gpu,
                    "cpu_crossprod",
                    reason = out_reason,
                    reason_code = out_reason_code
                )
            } else {
                out_gpu <- .mojor_gpu_route_tag(out_gpu, "gpu_crossprod")
            }
            return(out_gpu)
        }
        cpu_route_reason <- "gpu crossprod dispatch failed"
        cpu_route_reason_code <- "dispatch_failed"
    }

    hx <- mojor_gpu_array_read(x)
    if (is.null(y)) {
        hy <- NULL
    } else if (isTRUE(y_is_arr)) {
        hy <- mojor_gpu_array_read(y_arr)
    } else {
        if (is.null(y_host)) {
            y_host <- .mojor_gpu_linalg_host_rhs_values(
                y,
                dtype,
                op = "mojor_gpu_crossprod"
            )
        }
        hy <- y_host
    }
    out_host <- if (is.null(hy))
        crossprod(hx) else crossprod(hx, hy)
    out <- mojor_gpu_array(out_host, api = api, dtype = dtype)
    out <- .mojor_gpu_route_tag(
        out,
        "cpu_crossprod",
        reason = cpu_route_reason,
        reason_code = cpu_route_reason_code
    )
    out
}

.mojor_gpu_tcrossprod_dispatch <- function(x, y = NULL) {
    y_is_arr <- .mojor_gpu_is_array(y)
    y_is_host <- !is.null(y) &&
        !isTRUE(y_is_arr)
    y_arr <- if (isTRUE(y_is_arr))
        y else NULL
    prep <- if (isTRUE(y_is_host)) {
        .mojor_gpu_promote_linalg_host_rhs(x, y)
    } else {
        .mojor_gpu_promote_linalg_pair(x, y_arr)
    }
    x <- prep$x
    if (!isTRUE(y_is_host)) {
        y_arr <- prep$y
    }
    if (length(prep$tmp) > 0L) {
        on.exit(
            .mojor_gpu_free_temp_arrays(prep$tmp),
            add = TRUE
        )
    }
    api <- .mojor_gpu_pick_api(
        x,
        if (isTRUE(y_is_arr))
            y_arr else y
    )
    dtype <- prep$dtype
    if (!.mojor_gpu_linalg_dtype_supported(dtype)) {
        .mojor_gpu_stop_unsupported_linalg_dtype("mojor_gpu_tcrossprod", dtype)
    }
    x_info <- .mojor_gpu_matmul_shape_info(x)
    y_info <- if (is.null(y)) {
        x_info
    } else if (isTRUE(y_is_arr)) {
        .mojor_gpu_matmul_shape_info(y_arr)
    } else {
        .mojor_gpu_matmul_shape_info_host(y)
    }
    dims <- .mojor_gpu_matmul_dispatch_dims_from_info(x_info, y_info)
    x_dim2 <- if (is.null(dims))
        NULL else dims$x_dim2
    y_dim2 <- if (is.null(dims))
        NULL else dims$y_dim2
    y_host <- NULL
    cpu_route_reason <- NULL
    cpu_route_reason_code <- NULL
    try_gpu <- !is.null(x_dim2) &&
        !is.null(y_dim2)
    if (isTRUE(try_gpu) &&
        identical(dtype, "f64")) {
        if (isTRUE(.mojor_gpu_force_f64_cpu_fallback(api))) {
            try_gpu <- FALSE
            cpu_route_reason <- "f64 matmul disabled on metal backend"
            cpu_route_reason_code <- "disabled_on_metal_backend"
        } else {
 # tcrossprod uses transpose_a=FALSE, transpose_b=TRUE
            f64_mode <- .mojor_gpu_f64_matmul_mode(x_dim2[[1L]], y_dim2[[1L]])
            f64_ctx <- tryCatch(
                .mojor_gpu_ctx_get(),
                error = function(e) NULL
            )
            f64_ctx_epoch <- if (is.null(f64_ctx))
                NULL else .mojor_gpu_ctx_epoch_get()
            f64_probe_ok <- isTRUE(.mojor_gpu_f64_matmul_capable(mode = f64_mode))
            try_gpu <- f64_probe_ok
            if (!isTRUE(f64_probe_ok)) {
                probe_diag <- .mojor_gpu_f64_matmul_probe_diag(
                    mode = f64_mode,
                    ctx = f64_ctx,
                    ctx_epoch = f64_ctx_epoch
                )
                cpu_route_reason <- probe_diag$reason
                cpu_route_reason_code <- probe_diag$code
            }
        }
    }
    if (isTRUE(y_is_host)) {
        if (isTRUE(try_gpu)) {
            if (identical(dtype, "i32")) {
                y_host <- .mojor_gpu_linalg_host_rhs_values(
                    y,
                    dtype,
                    op = "mojor_gpu_tcrossprod"
                )
                y_arr <- mojor_gpu_array(y_host, api = api, dtype = dtype)
            } else {
                y_arr <- mojor_gpu_array(y, api = api, dtype = dtype)
            }
            on.exit(
                mojor_gpu_array_free(y_arr),
                add = TRUE
            )
        }
    }
    if (isTRUE(try_gpu)) {
        lhs <- .mojor_gpu_matmul_view(x, x_dim2)
        rhs_src <- if (is.null(y))
            x else y_arr
        rhs_dim2 <- if (is.null(y))
            x_dim2 else y_dim2
        rhs <- .mojor_gpu_matmul_view(rhs_src, rhs_dim2)
        out_gpu <- tryCatch(
            .mojor_gpu_matmul(lhs, rhs, transpose_a = FALSE, transpose_b = TRUE),
            error = function(e) NULL
        )
        if (!is.null(out_gpu)) {
            out_route <- .mojor_gpu_route_value(out_gpu)
            out_reason <- .mojor_gpu_route_reason_value(out_gpu)
            out_reason_code <- .mojor_gpu_route_reason_code_value(out_gpu)
            if (identical(out_route, "cpu_matmul")) {
                out_gpu <- .mojor_gpu_route_tag(
                    out_gpu,
                    "cpu_tcrossprod",
                    reason = out_reason,
                    reason_code = out_reason_code
                )
            } else {
                out_gpu <- .mojor_gpu_route_tag(out_gpu, "gpu_tcrossprod")
            }
            return(out_gpu)
        }
        cpu_route_reason <- "gpu tcrossprod dispatch failed"
        cpu_route_reason_code <- "dispatch_failed"
    }

    hx <- mojor_gpu_array_read(x)
    if (is.null(y)) {
        hy <- NULL
    } else if (isTRUE(y_is_arr)) {
        hy <- mojor_gpu_array_read(y_arr)
    } else {
        if (is.null(y_host)) {
            y_host <- .mojor_gpu_linalg_host_rhs_values(
                y,
                dtype,
                op = "mojor_gpu_tcrossprod"
            )
        }
        hy <- y_host
    }
    out_host <- if (is.null(hy))
        tcrossprod(hx) else tcrossprod(hx, hy)
    out <- mojor_gpu_array(out_host, api = api, dtype = dtype)
    out <- .mojor_gpu_route_tag(
        out,
        "cpu_tcrossprod",
        reason = cpu_route_reason,
        reason_code = cpu_route_reason_code
    )
    out
}

.mojor_gpu_slice_dispatch <- function(x, starts, ends, strides = NULL) {
    if (!inherits(x, "mojor_gpu_array")) {
        stop("gpu_slice: x must be mojor_gpu_array")
    }
    x_dim_obj <- .mojor_gpu_object_get(x, "dim", default = NULL)
    if (is.null(x_dim_obj)) {
        x_dim <- as.integer(.mojor_gpu_object_get(x, "n", default = 0L))
    } else {
        x_dim <- as.integer(x_dim_obj)
    }
    ndim <- length(x_dim)

    starts_i <- as.integer(starts)
    ends_i <- as.integer(ends)
    if (length(starts_i) !=
        ndim || length(ends_i) !=
        ndim || anyNA(starts_i) ||
        anyNA(ends_i)) {
        stop("gpu_slice: starts/ends must match rank")
    }

    if (is.null(strides)) {
        strides_i <- rep.int(1L, ndim)
    } else {
        strides_i <- as.integer(strides)
    }
    if (length(strides_i) !=
        ndim || anyNA(strides_i)) {
        stop("gpu_slice: strides must match rank")
    }
    if (any(strides_i == 0L)) {
        stop("gpu_slice: strides must be non-zero")
    }
    if (any(starts_i < 1L) ||
        any(starts_i > x_dim) ||
        any(ends_i < 1L) ||
        any(ends_i > x_dim)) {
        stop("gpu_slice: invalid slice bounds")
    }
    slice_seq <- function(start_i, end_i, stride_i) {
        if ((stride_i > 0L &&
            start_i > end_i) ||
            (stride_i < 0L &&
                start_i < end_i)) {
            return(integer(0L))
        }
        as.integer(seq.int(start_i, end_i, by = stride_i))
    }
    idx_vals <- lapply(
        seq_len(ndim),
        function(i) {
            slice_seq(starts_i[[i]], ends_i[[i]], strides_i[[i]])
        }
    )
    direction_empty <- (strides_i > 0L &
        starts_i > ends_i) |
        (strides_i < 0L &
            starts_i < ends_i)
    unit_stride <- all(strides_i == 1L) &&
        !any(direction_empty)
    if (isTRUE(unit_stride)) {
        sizes_i <- as.integer(ends_i - starts_i + 1L)
        dtype <- .mojor_gpu_array_dtype(x)
        if (dtype %in% c("f32", "f64", "i32")) {
            info <- .mojor_gpu_buf_info_from_dtype(dtype)
            handle <- .mojor_gpu_array_handle(x)
            if (inherits(handle, info$class)) {
                out_handle <- tryCatch(
                  .mojor_call_bridge(
                    paste0("mojor_gpu_buf_", dtype, "_slice"),
                    .mojor_gpu_ctx_get(), handle, as.integer(starts_i - 1L),
                    sizes_i, x_dim
                ),
                  error = function(e) NULL
              )
                if (!is.null(out_handle)) {
                  .mojor_gpu_require(
                    paste0("gpu_buf_", dtype, "_slice"),
                    out_handle
                )
                  class(out_handle) <- c(info$class, class(out_handle))
                  out <- .mojor_gpu_wrap_buffer_result(
                    out_handle, n = as.integer(prod(sizes_i)),
                    api = .mojor_gpu_array_api(x),
                    dtype = .mojor_gpu_array_dtype(x),
                    include_shape = !is.null(x_dim_obj),
                    dim = if (is.null(x_dim_obj))
                      NULL else sizes_i, dimnames = NULL, strides = if (is.null(x_dim_obj))
                      NULL else .mojor_dim_strides(sizes_i),
                    class = "mojor_gpu_array"
                )
                  out <- .mojor_gpu_route_tag(out, "gpu_slice")
                  return(out)
                }
            }
        }
    }
    gather_out <- .mojor_gpu_try_gather(
        x, idx_vals = idx_vals, missing_flags = rep.int(FALSE, ndim),
        drop = FALSE, allow_direct_fallback = TRUE
    )
    if (!is.null(gather_out)) {
        gather_out <- .mojor_gpu_route_tag(gather_out, "gpu_slice")
        return(gather_out)
    }

    host <- mojor_gpu_array_read(x)
    idx <- lapply(
        seq_len(ndim),
        function(i) slice_seq(starts_i[[i]], ends_i[[i]], strides_i[[i]])
    )
    if (is.null(x_dim_obj)) {
        out_host <- host[idx[[1L]]]
    } else {
        out_host <- do.call(
            `[`, c(
                list(host),
                idx, list(drop = FALSE)
            )
        )
    }
    out <- mojor_gpu_array(
        out_host, api = .mojor_gpu_array_api(x),
        dtype = .mojor_gpu_array_dtype(x)
    )
    out <- .mojor_gpu_route_tag(out, "cpu_slice")
    out
}

gpu_slice <- function(x, starts, ends, strides = NULL) {
    .mojor_gpu_slice_dispatch(x, starts = starts, ends = ends, strides = strides)
}

.mojor_gpu_axis_dimname_match_one <- function(axis_names_chr, key, context, arg_name) {
    exact_idx <- which(axis_names_chr == key)
    if (length(exact_idx) >= 1L) {
        return(as.integer(exact_idx[[1L]]))
    }
    partial_idx <- which(startsWith(axis_names_chr, key))
    if (length(partial_idx) == 1L) {
        return(as.integer(partial_idx[[1L]]))
    }
    if (length(partial_idx) > 1L) {
        stop(
            context, ": ", arg_name,
            " has ambiguous partial dimname label '",
            key,
            "'"
        )
    }
    NA_integer_
}

.mojor_gpu_logical_mask_mode <- function() {
    mode_raw <- getOption("mojor.gpu.logical_mask_mode", "strict")
    if (is.null(mode_raw) ||
        length(mode_raw) < 1L) {
        return("strict")
    }
    mode <- tolower(as.character(mode_raw[[1L]]))
    if (!mode %in% c("strict", "compat", "compat_warn")) {
        warning(
            "mojor_gpu: unknown logical mask mode '",
            mode,
            "'; using strict (expected strict/compat/compat_warn)",
            call. = FALSE
        )
        return("strict")
    }
    mode
}

.mojor_gpu_logical_mask_normalize <- function(
    mask, extent, arg_name = NULL, context = "[.mojor_gpu_array",
    extent_label = "extent"
) {
    mask_lgl <- as.logical(mask)
    extent_i <- as.integer(extent)
    if (is.na(extent_i) ||
        extent_i < 0L) {
        stop(context, ": invalid logical mask extent")
    }

    arg_prefix <- ""
    if (!is.null(arg_name)) {
        arg_chr <- as.character(arg_name[[1L]])
        if (length(arg_chr) == 1L &&
            nzchar(arg_chr)) {
            arg_prefix <- paste0(arg_chr, " ")
        }
    }

    mode <- .mojor_gpu_logical_mask_mode()
    if (identical(mode, "strict")) {
        if (anyNA(mask_lgl)) {
            stop(context, ": ", arg_prefix, "logical mask must not contain NA")
        }
        if (length(mask_lgl) != extent_i) {
            stop(
                context, ": ", arg_prefix,
                "logical mask length must equal ", extent_label
            )
        }
        return(mask_lgl)
    }

    if (length(mask_lgl) == 0L) {
        return(logical(0L))
    }

    recycled <- FALSE
    if (length(mask_lgl) != extent_i) {
        mask_lgl <- rep_len(mask_lgl, extent_i)
        recycled <- TRUE
    }
    had_na <- anyNA(mask_lgl)
    if (had_na) {
        mask_lgl[is.na(mask_lgl)] <- FALSE
    }

    if (identical(mode, "compat_warn")) {
        if (recycled) {
            warning(
                context, ": ", arg_prefix,
                "logical mask length recycled to ", extent_label,
                call. = FALSE
            )
        }
        if (had_na) {
            warning(
                context, ": ", arg_prefix,
                "logical mask NA treated as FALSE",
                call. = FALSE
            )
        }
    }
    mask_lgl
}

.mojor_gpu_index_values <- function(
    idx, extent, arg_name, missing_index = FALSE, context = "[.mojor_gpu_array",
    axis_names = NULL
) {
    if (isTRUE(missing_index)) {
        return(seq_len(as.integer(extent)))
    }
    if (is.character(idx) &&
        is.atomic(idx) &&
        is.null(dim(idx))) {
        idx_chr <- as.character(idx)
        if (length(idx_chr) == 0L) {
            return(integer(0L))
        }
        if (anyNA(idx_chr)) {
            stop(context, ": ", arg_name, " character indices must not contain NA")
        }
        if (is.null(axis_names)) {
            stop(context, ": ", arg_name, " character indices require dimnames")
        }
        axis_names_chr <- as.character(axis_names)
        if (length(axis_names_chr) != as.integer(extent)) {
            stop(context, ": ", arg_name, " dimnames extent mismatch")
        }
        idx_match <- vapply(
            idx_chr,
            function(lbl) {
                .mojor_gpu_axis_dimname_match_one(
                    axis_names_chr,
                    key = lbl,
                    context = context,
                    arg_name = arg_name
                )
            },
            integer(1)
        )
        if (anyNA(idx_match)) {
            stop(context, ": ", arg_name, " contains unknown dimname labels")
        }
        return(as.integer(idx_match))
    }
    if (is.logical(idx) &&
        is.atomic(idx) &&
        is.null(dim(idx))) {
        mask <- .mojor_gpu_logical_mask_normalize(
            idx,
            extent = extent,
            arg_name = arg_name,
            context = context,
            extent_label = "extent"
        )
        idx_i <- which(mask)
        return(as.integer(idx_i))
    }
    if (!is.numeric(idx) ||
        !is.atomic(idx) ||
        !is.null(dim(idx))) {
        stop(context, ": ", arg_name, " must be numeric indices")
    }
    idx_num <- as.numeric(idx)
    if (length(idx_num) ==
        0L) {
        return(integer(0L))
    }
    if (anyNA(idx_num) ||
        any(!is.finite(idx_num)) ||
        any(idx_num != floor(idx_num))) {
        stop(context, ": ", arg_name, " must contain finite integers")
    }
    has_pos <- any(idx_num > 0)
    has_neg <- any(idx_num < 0)
    if (has_pos && has_neg) {
        stop(
            context, ": ", arg_name,
            " cannot mix positive and negative indices"
        )
    }
    if (has_neg) {
        idx_neg <- as.integer(abs(idx_num[idx_num != 0]))
        if (any(idx_neg > extent)) {
            stop(context, ": ", arg_name, " indices out of bounds")
        }
        keep <- setdiff(seq_len(as.integer(extent)), unique(idx_neg))
        return(as.integer(keep))
    }
    idx_pos <- as.integer(idx_num[idx_num > 0])
    if (length(idx_pos) ==
        0L) {
        return(integer(0L))
    }
    if (any(idx_pos > extent)) {
        stop(context, ": ", arg_name, " indices out of bounds")
    }
    idx_pos
}

.mojor_gpu_index_is_contiguous <- function(idx_vals) {
    if (length(idx_vals) <=
        1L) {
        return(TRUE)
    }
    if (anyDuplicated(idx_vals)) {
        return(FALSE)
    }
    if (!identical(idx_vals, sort.int(idx_vals))) {
        return(FALSE)
    }
    all(
        diff(idx_vals) ==
            1L
    )
}

.mojor_gpu_index_regular_stride <- function(idx_vals) {
    n <- length(idx_vals)
    if (n <= 1L) {
        return(1L)
    }
    if (anyDuplicated(idx_vals)) {
        return(NULL)
    }
    deltas <- as.integer(diff(as.integer(idx_vals)))
    if (any(deltas == 0L)) {
        return(NULL)
    }
    if (length(unique(deltas)) !=
        1L) {
        return(NULL)
    }
    as.integer(deltas[[1L]])
}

.mojor_gpu_missing_arg <- new.env(parent = emptyenv())

.mojor_gpu_is_missing_arg <- function(x) {
    identical(x, .mojor_gpu_missing_arg)
}

.mojor_gpu_eval_dot_indices <- function(dots_raw, dot_env, enclos_env) {
    if (length(dots_raw) == 0L) {
        return(list())
    }
    lapply(
        dots_raw,
        function(dot_expr) {
            tryCatch(
                eval(dot_expr, envir = dot_env, enclos = enclos_env),
                error = function(e) {
                    msg <- conditionMessage(e)
                    if (grepl("is missing, with no default", msg, fixed = TRUE)) {
                        return(.mojor_gpu_missing_arg)
                    }
                    stop(e)
                }
            )
        }
    )
}

.mojor_gpu_selector_uniform_logical <- function(x, value) {
    value_lgl <- isTRUE(value)
    is.logical(x) &&
        is.atomic(x) &&
        is.null(dim(x)) &&
        length(x) > 0L &&
        !anyNA(x) &&
        all(x == value_lgl)
}

.mojor_gpu_selector_uniform_numeric <- function(x, value) {
    value_i <- as.integer(value)
    is.numeric(x) &&
        is.atomic(x) &&
        is.null(dim(x)) &&
        length(x) > 0L &&
        !anyNA(x) &&
        all(is.finite(x)) &&
        all(x == floor(x)) &&
        all(as.integer(x) == value_i)
}

.mojor_gpu_selector_noop_empty_state <- function(x) {
    if (is.null(x)) {
        return("noop")
    }
    if (is.logical(x) &&
        is.atomic(x) &&
        is.null(dim(x))) {
        if (length(x) == 0L) {
            return("noop")
        }
        if (anyNA(x)) {
            return("invalid")
        }
        if (all(x)) {
            return("noop")
        }
        return("empty")
    }
    if (is.numeric(x) &&
        is.atomic(x) &&
        is.null(dim(x))) {
        if (length(x) == 0L) {
            return("noop")
        }
        x_num <- as.numeric(x)
        if (anyNA(x_num) ||
            any(!is.finite(x_num)) ||
            any(x_num != floor(x_num))) {
            return("invalid")
        }
        x_int <- as.integer(x_num)
        if (any(x_int < 0L)) {
            return("invalid")
        }
        if (all(x_int == 0L)) {
            return("empty")
        }
        # Treat any strictly-positive extra selectors as compatibility no-op
        # selectors; mixed zero/non-zero selectors remain empty selectors.
        if (all(x_int > 0L)) {
            return("noop")
        }
        return("empty")
    }
    "invalid"
}

.mojor_gpu_rank1_secondary_ok <- function(j) {
    state <- .mojor_gpu_selector_noop_empty_state(j)
    identical(state, "noop") || identical(state, "empty")
}

.mojor_gpu_rank1_secondary_empty <- function(j) {
    identical(.mojor_gpu_selector_noop_empty_state(j), "empty")
}

.mojor_gpu_rank_noop_selector_ok <- function(j) {
    identical(.mojor_gpu_selector_noop_empty_state(j), "noop")
}

.mojor_gpu_rank_extra_selector_state <- function(idx) {
    if (.mojor_gpu_is_missing_arg(idx)) {
        return("noop")
    }
    .mojor_gpu_selector_noop_empty_state(idx)
}

.mojor_gpu_collect_indices <- function(i, j, dots_raw, rank, missing_i, missing_j, context) {
    dot_env <- parent.frame()
    dot_vals <- .mojor_gpu_eval_dot_indices(
        dots_raw,
        dot_env = dot_env,
        enclos_env = parent.frame()
    )

    if (rank == 1L) {
        if (!isTRUE(missing_j) &&
            !isTRUE(.mojor_gpu_rank1_secondary_ok(j))) {
            rank1_msg <- if (startsWith(context, "[<-")) {
                "rank-1 assignment does not accept j"
            } else {
                "rank-1 indexing does not accept j"
            }
            stop(context, ": ", rank1_msg)
        }
        if (length(dot_vals) > 0L &&
            any(
                !vapply(
                    dot_vals,
                    function(v) {
                        if (.mojor_gpu_is_missing_arg(v)) {
                            return(TRUE)
                        }
                        .mojor_gpu_rank1_secondary_ok(v)
                    },
                    logical(1)
                )
            )) {
            rank1_msg <- if (startsWith(context, "[<-")) {
                "rank-1 assignment does not accept j"
            } else {
                "rank-1 indexing does not accept j"
            }
            stop(context, ": ", rank1_msg)
        }
        secondary_empty <- FALSE
        if (!isTRUE(missing_j) &&
            isTRUE(.mojor_gpu_rank1_secondary_empty(j))) {
            secondary_empty <- TRUE
        }
        if (length(dot_vals) > 0L) {
            non_missing_dot <- vapply(dot_vals, function(v) !.mojor_gpu_is_missing_arg(v), logical(1))
            if (any(non_missing_dot) &&
                any(vapply(dot_vals[non_missing_dot], .mojor_gpu_rank1_secondary_empty, logical(1)))) {
                secondary_empty <- TRUE
            }
        }
        if (isTRUE(secondary_empty)) {
            return(list(integer(0L)))
        }
        return(list(if (isTRUE(missing_i)) .mojor_gpu_missing_arg else i))
    }

    max_extra <- rank - 2L
    if (length(dot_vals) > max_extra) {
        extra_vals <- dot_vals[seq.int(max_extra + 1L, length(dot_vals))]
        extra_states <- vapply(extra_vals, .mojor_gpu_rank_extra_selector_state, character(1))
        if (any(extra_states == "invalid")) {
            stop(context, ": additional indices are not supported")
        }
        if (any(extra_states == "empty")) {
            return(c(
                list(integer(0L)),
                rep.int(list(.mojor_gpu_missing_arg), rank - 1L)
            ))
        }
        if (max_extra > 0L) {
            dot_vals <- dot_vals[seq_len(max_extra)]
        } else {
            dot_vals <- list()
        }
    }

    idx_inputs <- vector("list", rank)
    idx_inputs[1L] <- list(if (isTRUE(missing_i))
        .mojor_gpu_missing_arg else i)
    idx_inputs[2L] <- list(if (isTRUE(missing_j))
        .mojor_gpu_missing_arg else j)
    if (max_extra > 0L) {
        for (k in seq_len(max_extra)) {
            if (k <= length(dot_vals)) {
                idx_inputs[k + 2L] <- list(dot_vals[[k]])
            } else {
                idx_inputs[k + 2L] <- list(.mojor_gpu_missing_arg)
            }
        }
    }
    idx_inputs
}

.mojor_gpu_materialize_indices <- function(idx_inputs, x_dim, context, x_dimnames = NULL) {
    rank <- length(x_dim)
    idx_vals <- vector("list", rank)
    missing_flags <- logical(rank)
    for (k in seq_len(rank)) {
        is_missing <- .mojor_gpu_is_missing_arg(idx_inputs[[k]])
        missing_flags[[k]] <- is_missing
        arg_name <- if (k == 1L)
            "i" else if (k == 2L)
            "j" else paste0("dim", k)
        axis_names <- NULL
        if (is.list(x_dimnames) && length(x_dimnames) >= k) {
            axis_names <- x_dimnames[[k]]
        }
        idx_vals[[k]] <- .mojor_gpu_index_values(
            if (is_missing)
                NULL else idx_inputs[[k]], x_dim[[k]], arg_name = arg_name, missing_index = is_missing,
            context = context, axis_names = axis_names
        )
    }
    list(values = idx_vals, missing = missing_flags)
}

.mojor_gpu_index_payload <- function(idx_vals, x_dim) {
    rank <- length(x_dim)
    if (length(idx_vals) !=
        rank) {
        stop(".mojor_gpu_index_payload: rank mismatch")
    }
    idx_lens <- as.integer(vapply(idx_vals, length, integer(1)))
    if (any(idx_lens <= 0L) ||
        anyNA(idx_lens)) {
        stop(".mojor_gpu_index_payload: invalid index lengths")
    }
    idx_offsets <- integer(rank)
    if (rank > 1L) {
        idx_offsets[2:rank] <- cumsum(idx_lens)[seq_len(rank - 1L)]
    }
    idx_data <- as.integer(unlist(idx_vals, use.names = FALSE))
    idx_data0 <- as.integer(idx_data - 1L)
    if (any(idx_data0 < 0L) ||
        anyNA(idx_data0)) {
        stop(".mojor_gpu_index_payload: invalid index data")
    }
    list(
        idx_data = idx_data0, idx_offsets = as.integer(idx_offsets),
        idx_lens = idx_lens, out_n = as.integer(prod(idx_lens))
    )
}

.mojor_gpu_linear_indices <- function(idx_vals, x_dim) {
    rank <- length(x_dim)
    if (rank <= 1L || length(idx_vals) !=
        rank) {
        return(NULL)
    }
    idx_lens <- as.integer(vapply(idx_vals, length, integer(1)))
    if (any(idx_lens <= 0L) ||
        anyNA(idx_lens)) {
        return(NULL)
    }
    total <- as.numeric(prod(idx_lens))
    if (!is.finite(total) ||
        total <= 0 || total > .Machine$integer.max) {
        return(NULL)
    }
    total_i <- as.integer(total)
    strides <- .mojor_dim_strides(as.integer(x_dim))
    if (is.null(strides) ||
        length(strides) !=
            rank) {
        return(NULL)
    }
    linear0 <- rep.int(0, total_i)
    for (axis in seq_len(rank)) {
        each_n <- if (axis == 1L) {
            1L
        } else {
            as.integer(prod(idx_lens[seq_len(axis - 1L)]))
        }
        times_n <- if (axis == rank) {
            1L
        } else {
            as.integer(prod(idx_lens[(axis + 1L):rank]))
        }
        coord <- rep(
            as.integer(idx_vals[[axis]]),
            each = each_n, times = times_n
        )
        linear0 <- linear0 + (as.numeric(coord) -
            1) * as.numeric(strides[[axis]])
    }
    linear1 <- linear0 + 1
    if (any(!is.finite(linear1)) ||
        any(linear1 < 1) ||
        any(linear1 > .Machine$integer.max)) {
        return(NULL)
    }
    as.integer(linear1)
}

.mojor_gpu_index_plan_cache <- function(x) {
    plans <- .mojor_gpu_object_get(x, "index_plans", default = NULL)
    if (!is.environment(plans)) {
        plans <- new.env(parent = emptyenv())
        x <- .mojor_gpu_object_set(x, "index_plans", plans)
    }
    plans
}

.mojor_gpu_index_plan_cache_find <- function(plans, x_dim, idx_vals, handle_ref, handle_epoch, dtype = NULL) {
    keys <- ls(plans, all.names = TRUE)
    if (length(keys) ==
        0L) {
        return(NULL)
    }
    for (k in keys) {
        entry <- get(k, envir = plans, inherits = FALSE)
        if (!is.list(entry) ||
            is.null(entry$plan)) {
            next
        }
        if (!identical(entry$handle_ref, handle_ref) ||
            !identical(entry$handle_epoch, as.integer(handle_epoch))) {
            next
        }
        if (!identical(entry$x_dim, as.integer(x_dim))) {
            next
        }
        entry_dtype <- entry$dtype
        if (is.null(entry_dtype)) {
            if (inherits(entry$plan, "mojor_gpu_idx_plan_f32")) {
                entry_dtype <- "f32"
            } else if (inherits(entry$plan, "mojor_gpu_idx_plan_f64")) {
                entry_dtype <- "f64"
            }
        }
        if (!is.null(dtype) &&
            !identical(entry_dtype, dtype)) {
            next
        }
        if (length(entry$idx_vals) !=
            length(idx_vals)) {
            next
        }
        same <- TRUE
        for (axis in seq_along(idx_vals)) {
            if (!identical(entry$idx_vals[[axis]], as.integer(idx_vals[[axis]]))) {
                same <- FALSE
                break
            }
        }
        if (isTRUE(same)) {
            entry$dtype <- entry_dtype
            return(entry)
        }
    }
    NULL
}

.mojor_gpu_index_plan_cache_prune <- function(plans, handle_ref, handle_epoch) {
    keys <- ls(plans, all.names = TRUE)
    if (length(keys) ==
        0L) {
        return(invisible(NULL))
    }
    for (k in keys) {
        entry <- get(k, envir = plans, inherits = FALSE)
        if (!is.list(entry) ||
            is.null(entry$plan)) {
            rm(list = k, envir = plans)
            next
        }
        if (!identical(entry$handle_ref, handle_ref) ||
            !identical(entry$handle_epoch, as.integer(handle_epoch))) {
            .mojor_gpu_idx_plan_free(entry$plan)
            rm(list = k, envir = plans)
        }
    }
    invisible(NULL)
}

.mojor_gpu_index_plan_cache_drop_one <- function(plans) {
    keys <- ls(plans, all.names = TRUE)
    if (length(keys) ==
        0L) {
        return(invisible(NULL))
    }
    key <- keys[[1L]]
    entry <- get(key, envir = plans, inherits = FALSE)
    if (is.list(entry) &&
        !is.null(entry$plan)) {
        .mojor_gpu_idx_plan_free(entry$plan)
    }
    rm(list = key, envir = plans)
    invisible(NULL)
}

.mojor_gpu_index_plan_cache_put <- function(plans, entry, max_entries = 8L) {
    keys <- ls(plans, all.names = TRUE)
    while (length(keys) >=
        max_entries) {
        .mojor_gpu_index_plan_cache_drop_one(plans)
        keys <- ls(plans, all.names = TRUE)
    }
    ids <- suppressWarnings(as.integer(sub("^p", "", keys)))
    next_id <- if (length(ids) >
        0L && any(!is.na(ids)))
        max(ids, na.rm = TRUE) +
            1L else 1L
    key <- sprintf("p%08d", as.integer(next_id))
    assign(key, entry, envir = plans)
    invisible(entry)
}

.mojor_gpu_index_plan_cached <- function(x, idx_vals, x_dim) {
    plan_symbols <- .mojor_gpu_idx_plan_symbols_from_array(x)
    if (is.null(plan_symbols)) {
        return(NULL)
    }
    if (isTRUE(.mojor_gpu_use_host_staged_index_route(.mojor_gpu_array_api(x), plan_symbols$dtype))) {
        return(NULL)
    }
    plans <- .mojor_gpu_index_plan_cache(x)
    handle_ref <- .mojor_gpu_array_handle(x)
    handle_epoch <- .mojor_gpu_array_handle_epoch(x)
    .mojor_gpu_index_plan_cache_prune(plans, handle_ref, handle_epoch)
    cached <- .mojor_gpu_index_plan_cache_find(
        plans, x_dim, idx_vals, handle_ref, handle_epoch, dtype = plan_symbols$dtype
    )
    if (!is.null(cached) &&
        identical(cached$dtype, plan_symbols$dtype)) {
        return(cached)
    }

    payload <- .mojor_gpu_index_payload(idx_vals, x_dim)
    plan <- tryCatch(
        .mojor_call_bridge(
            plan_symbols$create, .mojor_gpu_ctx_get(), payload$idx_data,
            payload$idx_offsets, payload$idx_lens, x_dim
        ),
        error = function(e) NULL
    )
    if (is.null(plan)) {
        return(NULL)
    }
    .mojor_gpu_require(
        paste0("gpu_idx_plan_", plan_symbols$dtype, "_create"),
        plan
    )
    class(plan) <- c(plan_symbols$class, class(plan))
    entry <- list(
        dtype = plan_symbols$dtype, plan = plan, out_n = payload$out_n,
        idx_lens = payload$idx_lens, handle_ref = handle_ref, handle_epoch = as.integer(handle_epoch),
        x_dim = as.integer(x_dim),
        idx_vals = lapply(idx_vals, as.integer)
    )
    .mojor_gpu_index_plan_cache_put(plans, entry)
    entry
}

.mojor_gpu_indices_have_duplicates <- function(idx_vals) {
    any(
        vapply(
            idx_vals, function(v) anyDuplicated(v) >
                0L, logical(1)
        )
    )
}

.mojor_gpu_apply_subset_drop <- function(out, idx_vals, missing_flags, drop = TRUE) {
    rank <- length(idx_vals)
    if (rank <= 1L || !isTRUE(drop)) {
        return(out)
    }

    scalar_flags <- vapply(
        seq_len(rank),
        function(k) {
            length(idx_vals[[k]]) ==
                1L && !isTRUE(missing_flags[[k]])
        }, logical(1)
    )
    if (!any(scalar_flags)) {
        return(out)
    }
    if (all(scalar_flags)) {
        scalar <- as.numeric(mojor_gpu_array_read(out)[[1L]])
        mojor_gpu_array_free(out)
        return(scalar)
    }

    kept_lens <- as.integer(vapply(idx_vals[!scalar_flags], length, integer(1)))
    out <- .mojor_gpu_object_set(out, "n", as.integer(prod(kept_lens)))
    if (length(kept_lens) ==
        1L) {
        out <- .mojor_gpu_object_set(out, "dim", NULL)
    } else {
        out <- .mojor_gpu_object_set(out, "dim", kept_lens)
    }
    out <- .mojor_gpu_object_set(out, "dimnames", NULL)
    out <- .mojor_gpu_object_set(out, "strides", NULL)
    out
}

.mojor_gpu_empty_subset_result <- function(
    x, idx_vals, missing_flags, drop = TRUE, force_vector = FALSE, route = "gpu_gather"
) {
    x_dim_obj <- .mojor_gpu_object_get(x, "dim", default = NULL)
    if (isTRUE(force_vector)) {
        x_dim_obj <- NULL
    }
    idx_lens <- as.integer(vapply(idx_vals, length, integer(1)))
    out <- mojor_gpu_array(
        n = 0L,
        api = .mojor_gpu_array_api(x),
        dtype = .mojor_gpu_array_dtype(x)
    )
    if (is.null(x_dim_obj)) {
        out <- .mojor_gpu_object_set(out, "dim", NULL)
        out <- .mojor_gpu_object_set(out, "dimnames", NULL)
        out <- .mojor_gpu_object_set(out, "strides", NULL)
    } else {
        out <- .mojor_gpu_object_set(out, "dim", idx_lens)
        out <- .mojor_gpu_object_set(out, "dimnames", NULL)
        out <- .mojor_gpu_object_set(out, "strides", .mojor_dim_strides(idx_lens))
    }
    out <- .mojor_gpu_route_tag(out, route)
    .mojor_gpu_apply_subset_drop(out, idx_vals, missing_flags, drop = drop)
}

.mojor_gpu_try_gather <- function(
    x, idx_vals, missing_flags, drop = TRUE, force_vector = FALSE, allow_direct_fallback = FALSE
) {
    idx_lens <- as.integer(vapply(idx_vals, length, integer(1)))
    if (any(idx_lens == 0L)) {
        return(
            .mojor_gpu_empty_subset_result(
                x,
                idx_vals = idx_vals,
                missing_flags = missing_flags,
                drop = drop,
                force_vector = force_vector,
                route = "gpu_gather"
            )
        )
    }
    dtype <- .mojor_gpu_array_dtype(x)
    if (!dtype %in% c("f32", "f64", "i32")) {
        return(NULL)
    }
    info <- .mojor_gpu_buf_info_from_dtype(dtype)
    handle <- .mojor_gpu_array_handle(x)
    if (!inherits(handle, info$class)) {
        return(NULL)
    }
    x_dim_obj <- .mojor_gpu_object_get(x, "dim", default = NULL)
    if (isTRUE(force_vector)) {
        x_dim_obj <- NULL
    }
    if (is.null(x_dim_obj)) {
        x_dim <- as.integer(.mojor_gpu_object_get(x, "n", default = 0L))
    } else {
        x_dim <- as.integer(x_dim_obj)
    }
    if (isTRUE(.mojor_gpu_use_host_staged_index_route(.mojor_gpu_array_api(x), dtype))) {
        host <- mojor_gpu_array_read(x)
        out_host <- .mojor_gpu_host_subset(host, idx_vals, missing_flags, drop = drop)
        return(
            .mojor_gpu_wrap_subset_host_result(
                x,
                out_host,
                drop = drop,
                route = "cpu_gather"
            )
        )
    }
    if (identical(dtype, "i32")) {
        payload <- .mojor_gpu_index_payload(idx_vals, x_dim)
        out_handle <- tryCatch(
            .mojor_call_bridge(
                "mojor_gpu_buf_i32_gather", .mojor_gpu_ctx_get(), handle,
                payload$idx_data, payload$idx_offsets, payload$idx_lens,
                x_dim
            ),
            error = function(e) NULL
        )
        if (is.null(out_handle)) {
            return(NULL)
        }
        .mojor_gpu_require("gpu_buf_i32_gather", out_handle)
        class(out_handle) <- c(info$class, class(out_handle))
        out <- .mojor_gpu_wrap_buffer_result(
            out_handle, n = payload$out_n, api = .mojor_gpu_array_api(x),
            dtype = .mojor_gpu_array_dtype(x),
            include_shape = !is.null(x_dim_obj),
            dim = if (is.null(x_dim_obj))
                NULL else payload$idx_lens, dimnames = NULL, strides = if (is.null(x_dim_obj))
                NULL else .mojor_dim_strides(payload$idx_lens),
            class = "mojor_gpu_array"
        )
        out <- .mojor_gpu_route_tag(out, "gpu_gather")
        return(
            .mojor_gpu_apply_subset_drop(out, idx_vals, missing_flags, drop = drop)
        )
    }
    plan_symbols <- .mojor_gpu_idx_plan_symbols(dtype)
    entry <- tryCatch(
        .mojor_gpu_index_plan_cached(x, idx_vals, x_dim),
        error = function(e) NULL
    )
    out_handle <- NULL
    idx_lens <- NULL
    out_n <- NA_integer_
    if (!is.null(entry) &&
        !is.null(entry$plan)) {
        out_handle <- tryCatch(
            .mojor_call_bridge(plan_symbols$gather, .mojor_gpu_ctx_get(), handle, entry$plan),
            error = function(e) NULL
        )
        if (!is.null(out_handle)) {
            idx_lens <- entry$idx_lens
            out_n <- as.integer(entry$out_n)
        }
    }
    if (is.null(out_handle) &&
        isTRUE(allow_direct_fallback)) {
        payload <- tryCatch(
            .mojor_gpu_index_payload(idx_vals, x_dim),
            error = function(e) NULL
        )
        if (!is.null(payload)) {
            out_handle <- tryCatch(
                .mojor_call_bridge(
                  paste0("mojor_gpu_buf_", dtype, "_gather"),
                  .mojor_gpu_ctx_get(), handle, payload$idx_data, payload$idx_offsets,
                  payload$idx_lens, x_dim
              ),
                error = function(e) NULL
            )
            if (!is.null(out_handle)) {
                idx_lens <- payload$idx_lens
                out_n <- as.integer(payload$out_n)
            }
        }
    }
    if (is.null(out_handle)) {
        return(NULL)
    }

    .mojor_gpu_require(
        paste0("gpu_buf_", dtype, "_gather"),
        out_handle
    )
    class(out_handle) <- c(info$class, class(out_handle))
    out <- .mojor_gpu_wrap_buffer_result(
        out_handle, n = out_n, api = .mojor_gpu_array_api(x),
        dtype = .mojor_gpu_array_dtype(x),
        include_shape = !is.null(x_dim_obj),
        dim = if (is.null(x_dim_obj))
            NULL else idx_lens, dimnames = NULL, strides = if (is.null(x_dim_obj))
            NULL else .mojor_dim_strides(idx_lens),
        class = "mojor_gpu_array"
    )
    out <- .mojor_gpu_route_tag(out, "gpu_gather")
    .mojor_gpu_apply_subset_drop(out, idx_vals, missing_flags, drop = drop)
}

.mojor_gpu_try_logical_gather <- function(x, mask, drop = TRUE) {
    idx_i <- which(mask)
    .mojor_gpu_try_gather(
        x, idx_vals = list(as.integer(idx_i)),
        missing_flags = list(FALSE),
        drop = drop, force_vector = TRUE
    )
}

.mojor_gpu_try_gather_linearized <- function(x, idx_vals, missing_flags, drop = TRUE) {
    x_dim <- .mojor_gpu_object_get(x, "dim", default = NULL)
    if (is.null(x_dim) ||
        length(x_dim) <=
            1L) {
        return(NULL)
    }
    x_dim <- as.integer(x_dim)
    linear_idx <- .mojor_gpu_linear_indices(idx_vals, x_dim)
    if (is.null(linear_idx)) {
        return(NULL)
    }
    out <- .mojor_gpu_try_gather(
        x, idx_vals = list(as.integer(linear_idx)),
        missing_flags = list(FALSE),
        drop = TRUE, force_vector = TRUE, allow_direct_fallback = TRUE
    )
    if (is.null(out)) {
        return(NULL)
    }
    idx_lens <- as.integer(vapply(idx_vals, length, integer(1)))
    out <- .mojor_gpu_object_set(out, "n", as.integer(prod(idx_lens)))
    out <- .mojor_gpu_object_set(out, "dim", idx_lens)
    out <- .mojor_gpu_object_set(out, "dimnames", NULL)
    out <- .mojor_gpu_object_set(out, "strides", .mojor_dim_strides(idx_lens))
    .mojor_gpu_apply_subset_drop(out, idx_vals, missing_flags, drop = drop)
}

.mojor_gpu_assign_target_shape <- function(idx_vals, missing_flags) {
    rank <- length(idx_vals)
    idx_lens <- as.integer(vapply(idx_vals, length, integer(1)))
    target_len <- as.integer(prod(idx_lens))
    if (rank <= 1L) {
        return(list(target_len = target_len, target_dim = NULL))
    }
    scalar_flags <- vapply(
        seq_len(rank),
        function(k) {
            idx_lens[[k]] == 1L && !isTRUE(missing_flags[[k]])
        }, logical(1)
    )
    kept_lens <- as.integer(idx_lens[!scalar_flags])
    if (length(kept_lens) <=
        1L) {
        return(list(target_len = target_len, target_dim = NULL))
    }
    list(target_len = target_len, target_dim = kept_lens)
}

.mojor_gpu_try_i32_slice_assign <- function(x, values, idx_vals, x_dim) {
    if (!identical(
        .mojor_gpu_array_dtype(x),
        "i32"
    )) {
        return(FALSE)
    }
    if (isTRUE(.mojor_gpu_use_host_staged_index_route(.mojor_gpu_array_api(x), "i32"))) {
        return(FALSE)
    }
    if (length(idx_vals) !=
        length(x_dim)) {
        return(FALSE)
    }
    if (!all(vapply(idx_vals, .mojor_gpu_index_is_contiguous, logical(1)))) {
        return(FALSE)
    }
    info <- .mojor_gpu_buf_info_from_dtype("i32")
    handle <- .mojor_gpu_array_handle(x)
    if (!inherits(handle, info$class)) {
        return(FALSE)
    }
    starts <- as.integer(
        vapply(
            idx_vals, function(v) v[[1L]] -
                1L, integer(1)
        )
    )
    sizes <- as.integer(vapply(idx_vals, length, integer(1)))
    dims_i <- as.integer(x_dim)
    if (any(starts < 0L) ||
        any(sizes <= 0L)) {
        return(FALSE)
    }
    values_buf <- tryCatch(
        .mojor_gpu_buf_new("i32", x = values),
        error = function(e) NULL
    )
    if (is.null(values_buf)) {
        return(FALSE)
    }
    on.exit(
        try(
            .mojor_gpu_buf_free(info, values_buf),
            silent = TRUE
        ),
        add = TRUE
    )
    ok <- tryCatch(
        .mojor_call_bridge(
            "mojor_gpu_buf_i32_slice_assign", .mojor_gpu_ctx_get(), handle,
            values_buf, starts, sizes, dims_i
        ),
        error = function(e) NULL
    )
    isTRUE(ok)
}

.mojor_gpu_try_scatter <- function(x, values, idx_vals, x_dim) {
    dtype <- .mojor_gpu_array_dtype(x)
    if (!dtype %in% c("f32", "f64", "i32")) {
        return(FALSE)
    }
    if (isTRUE(.mojor_gpu_use_host_staged_index_route(.mojor_gpu_array_api(x), dtype))) {
        return(FALSE)
    }
    if (.mojor_gpu_indices_have_duplicates(idx_vals)) {
        return(FALSE)
    }
    info <- .mojor_gpu_buf_info_from_dtype(dtype)
    handle <- .mojor_gpu_array_handle(x)
    if (!inherits(handle, info$class)) {
        return(FALSE)
    }
    if (identical(dtype, "i32")) {
        if (!isTRUE(.mojor_gpu_i32_scatter_capable(.mojor_gpu_array_api(x)))) {
            return(FALSE)
        }
        payload <- .mojor_gpu_index_payload(idx_vals, x_dim)
        values_buf <- tryCatch(
            .mojor_gpu_buf_new(dtype, x = values),
            error = function(e) NULL
        )
        if (is.null(values_buf)) {
            return(FALSE)
        }
        on.exit(
            try(
                .mojor_gpu_buf_free(info, values_buf),
                silent = TRUE
            ),
            add = TRUE
        )
        ok <- tryCatch(
            .mojor_call_bridge(
                "mojor_gpu_buf_i32_scatter", .mojor_gpu_ctx_get(), handle,
                values_buf, payload$idx_data, payload$idx_offsets, payload$idx_lens,
                x_dim
            ),
            error = function(e) NULL
        )
        return(isTRUE(ok))
    }
    plan_symbols <- .mojor_gpu_idx_plan_symbols(dtype)
    entry <- tryCatch(
        .mojor_gpu_index_plan_cached(x, idx_vals, x_dim),
        error = function(e) NULL
    )
    values_buf <- tryCatch(
        .mojor_gpu_buf_new(dtype, x = values),
        error = function(e) NULL
    )
    if (is.null(values_buf)) {
        return(FALSE)
    }
    on.exit(
        try(
            .mojor_gpu_buf_free(info, values_buf),
            silent = TRUE
        ),
        add = TRUE
    )

    if (!is.null(entry) &&
        !is.null(entry$plan)) {
        ok <- tryCatch(
            .mojor_call_bridge(
                plan_symbols$scatter, .mojor_gpu_ctx_get(), handle, values_buf,
                entry$plan
            ),
            error = function(e) NULL
        )
        if (isTRUE(ok)) {
            return(TRUE)
        }
    }
    FALSE
}

.mojor_gpu_try_scatter_linearized <- function(x, values, idx_vals, x_dim) {
    if (length(x_dim) <=
        1L) {
        if (length(idx_vals) !=
            1L || length(idx_vals[[1L]]) !=
            length(values)) {
            return(FALSE)
        }
        linear_idx <- as.integer(idx_vals[[1L]])
    } else {
        linear_idx <- .mojor_gpu_linear_indices(idx_vals, x_dim)
        if (is.null(linear_idx) ||
            length(linear_idx) !=
                length(values)) {
            return(FALSE)
        }
    }
    keep_last <- !duplicated(linear_idx, fromLast = TRUE)
    linear_idx <- as.integer(linear_idx[keep_last])
    values <- as.numeric(values[keep_last])
    x_n <- as.integer(.mojor_gpu_array_length(x))
    .mojor_gpu_try_scatter(
        x, values = values, idx_vals = list(linear_idx),
        x_dim = x_n
    )
}

.mojor_gpu_bracket_eval <- function(x, idx_vals, drop = TRUE) {
    idx_exprs <- lapply(
        idx_vals,
        function(idx) {
            if (is.null(idx))
                quote(expr =) else idx
        }
    )
    call <- as.call(
        c(
            list(
                as.name("["),
                quote(x)
            ),
            idx_exprs,
            list(drop = drop)
        )
    )
    eval(
        call,
        envir = list(x = x),
        enclos = parent.frame()
    )
}

.mojor_gpu_bracket_assign <- function(x, idx_vals, value) {
    idx_exprs <- lapply(
        idx_vals,
        function(idx) {
            if (is.null(idx) || .mojor_gpu_is_missing_arg(idx)) {
                quote(expr =)
            } else {
                idx
            }
        }
    )
    call <- as.call(
        c(
            list(
                as.name("[<-"),
                quote(x)
            ),
            idx_exprs,
            list(value = quote(value))
        )
    )
    eval(
        call,
        envir = list(x = x, value = value),
        enclos = parent.frame()
    )
}

.mojor_gpu_host_subset <- function(host, idx_vals, missing_flags, drop = TRUE) {
    idx_exprs <- lapply(
        seq_along(idx_vals),
        function(k) {
            if (isTRUE(missing_flags[[k]]))
                quote(expr =) else idx_vals[[k]]
        }
    )
    call <- as.call(
        c(
            list(
                as.name("["),
                quote(host)
            ),
            idx_exprs, list(drop = drop)
        )
    )
    eval(
        call, envir = list(host = host),
        enclos = parent.frame()
    )
}

.mojor_gpu_host_assign <- function(host, idx_vals, missing_flags, value) {
    idx_exprs <- lapply(
        seq_along(idx_vals),
        function(k) {
            if (isTRUE(missing_flags[[k]]))
                quote(expr =) else idx_vals[[k]]
        }
    )
    call <- as.call(
        c(
            list(
                as.name("[<-"),
                quote(host)
            ),
            idx_exprs, list(value = quote(value))
        )
    )
    eval(
        call, envir = list(host = host, value = value),
        enclos = parent.frame()
    )
}

.mojor_gpu_collect_indices_raw <- function(i, j, dots_raw, missing_i, missing_j) {
    dot_env <- parent.frame()
    dot_vals <- .mojor_gpu_eval_dot_indices(
        dots_raw,
        dot_env = dot_env,
        enclos_env = parent.frame()
    )
    idx_inputs <- list(if (isTRUE(missing_i))
        .mojor_gpu_missing_arg else i)
    if (!isTRUE(missing_j) || length(dot_vals) > 0L) {
        idx_inputs[length(idx_inputs) + 1L] <- list(if (isTRUE(missing_j))
            .mojor_gpu_missing_arg else j)
    }
    if (length(dot_vals) > 0L) {
        idx_inputs <- c(idx_inputs, dot_vals)
    }
    idx_inputs
}

.mojor_gpu_host_subset_raw <- function(host, idx_inputs, drop = TRUE) {
    idx_exprs <- vector("list", length(idx_inputs))
    eval_vals <- list(host = host)
    idx_counter <- 0L
    for (k in seq_along(idx_inputs)) {
        idx <- idx_inputs[[k]]
        if (.mojor_gpu_is_missing_arg(idx)) {
            idx_exprs[[k]] <- quote(expr =)
        } else {
            idx_counter <- idx_counter + 1L
            nm <- paste0(".idx", idx_counter)
            # Preserve NULL selectors in the evaluation frame.
            eval_vals[nm] <- list(idx)
            idx_exprs[[k]] <- as.name(nm)
        }
    }
    call <- as.call(
        c(
            list(
                as.name("["),
                quote(host)
            ),
            idx_exprs,
            list(drop = drop)
        )
    )
    eval(
        call,
        envir = list2env(eval_vals, parent = parent.frame()),
        enclos = parent.frame()
    )
}

.mojor_gpu_host_assign_raw <- function(host, idx_inputs, value) {
    idx_exprs <- vector("list", length(idx_inputs))
    eval_vals <- list(host = host, value = value)
    idx_counter <- 0L
    for (k in seq_along(idx_inputs)) {
        idx <- idx_inputs[[k]]
        if (.mojor_gpu_is_missing_arg(idx)) {
            idx_exprs[[k]] <- quote(expr =)
        } else {
            idx_counter <- idx_counter + 1L
            nm <- paste0(".idx", idx_counter)
            # Preserve NULL selectors in the evaluation frame.
            eval_vals[nm] <- list(idx)
            idx_exprs[[k]] <- as.name(nm)
        }
    }
    call <- as.call(
        c(
            list(
                as.name("[<-"),
                quote(host)
            ),
            idx_exprs,
            list(value = quote(value))
        )
    )
    eval(
        call,
        envir = list2env(eval_vals, parent = parent.frame()),
        enclos = parent.frame()
    )
}

.mojor_gpu_strict_logical_mask_error <- function(err) {
    if (!inherits(err, "error")) {
        return(FALSE)
    }
    msg <- conditionMessage(err)
    grepl(
        "logical mask must not contain NA|logical mask length must equal extent|mask length must equal length\\(x\\)",
        msg,
        perl = TRUE
    )
}

.mojor_gpu_host_get2_raw <- function(host, i, dots = list(), exact = TRUE) {
    do.call(
        "[[",
        c(
            list(host, i),
            dots,
            list(exact = exact)
        )
    )
}

.mojor_gpu_host_set2_raw <- function(host, i, dots = list(), value) {
    do.call(
        "[[<-",
        c(
            list(host, i),
            dots,
            list(value = value)
        )
    )
}

.mojor_gpu_wrap_subset_host_result <- function(x, out_host, drop = TRUE, route = "cpu_gather") {
    if (isTRUE(drop) &&
        is.atomic(out_host) &&
        length(out_host) == 1L &&
        is.null(dim(out_host))) {
        return(as.numeric(out_host[[1L]]))
    }
    out <- mojor_gpu_array(
        out_host, api = .mojor_gpu_array_api(x),
        dtype = .mojor_gpu_array_dtype(x)
    )
    .mojor_gpu_route_tag(out, route)
}

.mojor_gpu_index_error_fallback_ok <- function(msg) {
    any(vapply(
        c(
            "must be numeric indices",
            "must contain finite integers",
            "rank-1 indexing does not accept j",
            "rank-1 assignment does not accept j",
            "additional indices are not supported"
        ),
        function(pat) grepl(pat, msg, fixed = TRUE),
        logical(1)
    ))
}

.mojor_gpu_index_span <- function(idx_vals, missing_index = FALSE) {
    list(
        start = idx_vals[[1L]], end = idx_vals[[length(idx_vals)]],
        len = as.integer(length(idx_vals)),
        is_scalar = (length(idx_vals) ==
            1L && !isTRUE(missing_index))
    )
}

.mojor_gpu_normalize_drop <- function(drop, context = "[.mojor_gpu_array") {
    if (length(drop) != 1L) {
        stop(context, ": drop must be a non-NA scalar coercible to TRUE or FALSE")
    }
    drop_lgl <- suppressWarnings(as.logical(drop))
    if (length(drop_lgl) != 1L || is.na(drop_lgl)) {
        stop(context, ": drop must be a non-NA scalar coercible to TRUE or FALSE")
    }
    isTRUE(drop_lgl)
}

.mojor_gpu_bracket_matrix_index_ok <- function(i, x_dim) {
    if (!is.matrix(i)) {
        return(FALSE)
    }
    rank <- length(x_dim)
    if (rank == 1L) {
        return(FALSE)
    }
    if (is.logical(i)) {
        return(identical(as.integer(dim(i)), as.integer(x_dim)))
    }
    if (is.numeric(i) || is.character(i)) {
        return(ncol(i) == rank)
    }
    FALSE
}

`[.mojor_gpu_array` <- function(x, i, j, ..., drop = TRUE) {
    drop <- .mojor_gpu_normalize_drop(drop, context = "[.mojor_gpu_array")
    dots_raw <- as.list(substitute(list(...)))[-1L]

    x_dim <- .mojor_gpu_object_get(x, "dim", default = NULL)
    if (is.null(x_dim)) {
        x_dim <- as.integer(.mojor_gpu_object_get(x, "n", default = 0L))
    } else {
        x_dim <- as.integer(x_dim)
    }
    rank <- length(x_dim)
    if (rank < 1L) {
        stop("[.mojor_gpu_array: invalid rank")
    }

    if (!missing(i) &&
        missing(j) &&
        length(dots_raw) == 0L &&
        .mojor_gpu_bracket_matrix_index_ok(i, x_dim)) {
        host <- mojor_gpu_array_read(x)
        out_host <- host[i]
        return(
            .mojor_gpu_wrap_subset_host_result(
                x,
                out_host,
                drop = drop,
                route = "cpu_gather"
            )
        )
    }

    if (!missing(i) &&
        missing(j) &&
        length(dots_raw) ==
            0L && is.logical(i) &&
        is.atomic(i) &&
        is.null(dim(i))) {
        x_n <- .mojor_gpu_array_length(x)
        mask <- .mojor_gpu_logical_mask_normalize(
            i,
            extent = x_n,
            arg_name = NULL,
            context = "[.mojor_gpu_array",
            extent_label = "length(x)"
        )
        gather_out <- .mojor_gpu_try_logical_gather(x, mask, drop = drop)
        if (!is.null(gather_out)) {
            return(gather_out)
        }
        host <- mojor_gpu_array_read(x)
        out_host <- as.vector(host)[mask]
        return(
            .mojor_gpu_wrap_subset_host_result(
                x,
                out_host,
                drop = drop,
                route = "cpu_gather"
            )
        )
    }

    idx_state <- tryCatch(
        {
            idx_inputs <- .mojor_gpu_collect_indices(
                i = i, j = j, dots_raw = dots_raw, rank = rank, missing_i = missing(i),
                missing_j = missing(j),
                context = "[.mojor_gpu_array"
            )
            x_dimnames <- .mojor_gpu_object_get(x, "dimnames", default = NULL)
            idx_mat <- .mojor_gpu_materialize_indices(
                idx_inputs,
                x_dim,
                context = "[.mojor_gpu_array",
                x_dimnames = x_dimnames
            )
            list(idx_inputs = idx_inputs, idx_mat = idx_mat)
        },
        error = function(e) e
    )
    if (inherits(idx_state, "error")) {
        if (isTRUE(.mojor_gpu_strict_logical_mask_error(idx_state))) {
            stop(idx_state)
        }
        host <- mojor_gpu_array_read(x)
        idx_raw <- .mojor_gpu_collect_indices_raw(
            i = i,
            j = j,
            dots_raw = dots_raw,
            missing_i = missing(i),
            missing_j = missing(j)
        )
        out_host <- tryCatch(
            .mojor_gpu_host_subset_raw(
                host,
                idx_raw,
                drop = drop
            ),
            error = function(e) e
        )
        if (!inherits(out_host, "error")) {
            return(
                .mojor_gpu_wrap_subset_host_result(
                    x,
                    out_host,
                    drop = drop,
                    route = "cpu_gather"
                )
            )
        }
        stop(idx_state)
    }
    idx_inputs <- idx_state$idx_inputs
    idx_mat <- idx_state$idx_mat
    idx_vals <- idx_mat$values
    missing_flags <- idx_mat$missing
    if (any(vapply(idx_vals, length, integer(1)) == 0L)) {
        gather_out <- .mojor_gpu_try_gather(
            x,
            idx_vals = idx_vals,
            missing_flags = missing_flags,
            drop = drop
        )
        if (!is.null(gather_out)) {
            return(gather_out)
        }
        host <- mojor_gpu_array_read(x)
        out_host <- .mojor_gpu_host_subset(host, idx_vals, missing_flags, drop = drop)
        return(
            .mojor_gpu_wrap_subset_host_result(
                x,
                out_host,
                drop = drop,
                route = "cpu_gather"
            )
        )
    }

    if (rank == 1L) {
        idx_i <- idx_vals[[1L]]
        idx_stride <- .mojor_gpu_index_regular_stride(idx_i)
        if (!is.null(idx_stride) &&
            abs(as.integer(idx_stride)) ==
                1L) {
            span_i <- .mojor_gpu_index_span(idx_i, missing_index = missing_flags[[1L]])
            return(
                gpu_slice(
                    x,
                    starts = span_i$start,
                    ends = span_i$end,
                    strides = as.integer(idx_stride)
                )
            )
        }
        gather_out <- .mojor_gpu_try_gather(x, idx_vals = idx_vals, missing_flags = missing_flags, drop = drop)
        if (!is.null(gather_out)) {
            return(gather_out)
        }
        host <- mojor_gpu_array_read(x)
        out <- mojor_gpu_array(
            host[idx_i], api = .mojor_gpu_array_api(x),
            dtype = .mojor_gpu_array_dtype(x)
        )
        out <- .mojor_gpu_route_tag(out, "cpu_gather")
        return(out)
    }

    slice_strides <- lapply(idx_vals, .mojor_gpu_index_regular_stride)
    use_slice <- all(
        vapply(
            slice_strides,
            function(s) {
                !is.null(s) &&
                    abs(as.integer(s)) ==
                        1L
            },
            logical(1)
        )
    )
    if (isTRUE(use_slice)) {
        spans <- lapply(
            seq_len(rank),
            function(k) {
                .mojor_gpu_index_span(idx_vals[[k]], missing_index = missing_flags[[k]])
            }
        )
        starts <- as.integer(
            vapply(
                spans, function(s) s$start,
                integer(1)
            )
        )
        ends <- as.integer(
            vapply(
                spans, function(s) s$end,
                integer(1)
            )
        )
        strides <- as.integer(vapply(slice_strides, as.integer, integer(1)))
        out <- gpu_slice(x, starts = starts, ends = ends, strides = strides)
        return(
            .mojor_gpu_apply_subset_drop(out, idx_vals, missing_flags, drop = drop)
        )
    }

    gather_out <- .mojor_gpu_try_gather(x, idx_vals = idx_vals, missing_flags = missing_flags, drop = drop)
    if (!is.null(gather_out)) {
        return(gather_out)
    }
    if (rank > 1L) {
        gather_out <- .mojor_gpu_try_gather_linearized(x, idx_vals = idx_vals, missing_flags = missing_flags, drop = drop)
        if (!is.null(gather_out)) {
            return(gather_out)
        }
    }

    host <- mojor_gpu_array_read(x)
    out_host <- .mojor_gpu_host_subset(host, idx_vals, missing_flags, drop = drop)
    out <- .mojor_gpu_wrap_subset_host_result(
        x,
        out_host,
        drop = drop,
        route = "cpu_gather"
    )
    if (!isTRUE(drop)) {
        return(out)
    }
    out
}

`[.GPUArray` <- function(x, i, j, ..., drop = TRUE) {
    `[.mojor_gpu_array`(x, i, j, ..., drop = drop)
}

.mojor_gpu_prepare_assign_value <- function(value, target_len, target_dim = NULL, target_dtype = NULL) {
    value_src <- value
    if (inherits(value, "mojor_gpu_array")) {
        value_src <- mojor_gpu_array_read(value)
    }
    if (!is.numeric(value_src) ||
        !is.atomic(value_src)) {
        stop("[<-.mojor_gpu_array: value must be numeric")
    }
    target_len <- as.integer(target_len)
    if (is.na(target_len) || target_len < 0L) {
        stop("[<-.mojor_gpu_array: invalid assignment target length")
    }
    vals <- as.numeric(value_src)
    if (target_len == 0L) {
        if (is.null(target_dim)) {
            return(numeric(0))
        }
        return(array(numeric(0), dim = as.integer(target_dim)))
    }
    value_len <- length(vals)
    if (value_len == 0L) {
        stop("[<-.mojor_gpu_array: replacement has length zero")
    }

    vals_out <- NULL
    if (value_len == 1L) {
        vals_out <- rep(vals, target_len)
    } else if (value_len == target_len) {
        vals_out <- vals
    } else if (target_len %% value_len == 0L) {
        vals_out <- rep(vals, length.out = target_len)
    } else {
        stop(
            "[<-.mojor_gpu_array: number of items to replace is not a multiple of replacement length"
        )
    }
    if (!is.null(target_dtype)) {
        vals_out <- .mojor_gpu_cast_values(vals_out, target_dtype)
    }
    if (!is.null(target_dim)) {
        return(array(vals_out, dim = as.integer(target_dim)))
    }
    vals_out
}

`[<-.mojor_gpu_array` <- function(x, i, j, ..., value) {
    x <- .mojor_gpu_copy_on_modify_prepare(x)
    on.exit(.mojor_gpu_copy_on_modify_clear(x), add = TRUE)

    dots_raw <- as.list(substitute(list(...)))[-1L]
    x_dim <- .mojor_gpu_object_get(x, "dim", default = NULL)
    if (is.null(x_dim)) {
        x_dim <- as.integer(.mojor_gpu_object_get(x, "n", default = 0L))
    } else {
        x_dim <- as.integer(x_dim)
    }
    rank <- length(x_dim)
    if (rank < 1L) {
        stop("[<-.mojor_gpu_array: invalid rank")
    }

    if (!missing(i) &&
        missing(j) &&
        length(dots_raw) == 0L &&
        .mojor_gpu_bracket_matrix_index_ok(i, x_dim)) {
        host <- mojor_gpu_array_read(x)
        target_len <- length(host[i])
        vals <- .mojor_gpu_prepare_assign_value(
            value,
            target_len = target_len,
            target_dtype = .mojor_gpu_array_dtype(x)
        )
        host[i] <- as.vector(vals)
        mojor_gpu_array_write(x, host)
        x <- .mojor_gpu_route_tag(x, "cpu_scatter")
        return(x)
    }

    idx_state <- tryCatch(
        {
            idx_inputs <- .mojor_gpu_collect_indices(
                i = i, j = j, dots_raw = dots_raw, rank = rank, missing_i = missing(i),
                missing_j = missing(j),
                context = "[<-.mojor_gpu_array"
            )
            x_dimnames <- .mojor_gpu_object_get(x, "dimnames", default = NULL)
            idx_mat <- .mojor_gpu_materialize_indices(
                idx_inputs,
                x_dim,
                context = "[<-.mojor_gpu_array",
                x_dimnames = x_dimnames
            )
            list(idx_inputs = idx_inputs, idx_mat = idx_mat)
        },
        error = function(e) e
    )
    if (inherits(idx_state, "error")) {
        if (isTRUE(.mojor_gpu_strict_logical_mask_error(idx_state))) {
            stop(idx_state)
        }
        host <- mojor_gpu_array_read(x)
        idx_raw <- .mojor_gpu_collect_indices_raw(
            i = i,
            j = j,
            dots_raw = dots_raw,
            missing_i = missing(i),
            missing_j = missing(j)
        )
        value_raw <- if (inherits(value, "mojor_gpu_array")) {
            mojor_gpu_array_read(value)
        } else {
            value
        }
        host_new <- tryCatch(
            .mojor_gpu_host_assign_raw(host, idx_raw, value_raw),
            error = function(e) e
        )
        if (!inherits(host_new, "error")) {
            mojor_gpu_array_write(x, host_new)
            x <- .mojor_gpu_route_tag(x, "cpu_scatter")
            return(x)
        }
        stop(idx_state)
    }
    idx_inputs <- idx_state$idx_inputs
    idx_mat <- idx_state$idx_mat
    idx_vals <- idx_mat$values
    missing_flags <- idx_mat$missing
    if (any(vapply(idx_vals, length, integer(1)) == 0L)) {
        x <- .mojor_gpu_route_tag(x, "cpu_scatter")
        return(x)
    }
    all_contiguous <- all(vapply(idx_vals, .mojor_gpu_index_is_contiguous, logical(1)))

    if (!isTRUE(all_contiguous)) {
        target_info <- .mojor_gpu_assign_target_shape(idx_vals, missing_flags)
        vals <- .mojor_gpu_prepare_assign_value(
            value,
            target_info$target_len,
            target_dim = target_info$target_dim,
            target_dtype = .mojor_gpu_array_dtype(x)
        )
        vals_num <- as.numeric(vals)
        scatter_ok <- .mojor_gpu_try_scatter(x, values = vals_num, idx_vals = idx_vals, x_dim = x_dim)
        if (isTRUE(scatter_ok)) {
            x <- .mojor_gpu_route_tag(x, "gpu_scatter")
            return(x)
        }
        scatter_ok <- .mojor_gpu_try_scatter_linearized(x, values = vals_num, idx_vals = idx_vals, x_dim = x_dim)
        if (isTRUE(scatter_ok)) {
            x <- .mojor_gpu_route_tag(x, "gpu_scatter")
            return(x)
        }
        host <- mojor_gpu_array_read(x)
        if (rank == 1L) {
            host[idx_vals[[1L]]] <- vals
        } else {
            host <- .mojor_gpu_host_assign(host, idx_vals, missing_flags, vals)
        }
        mojor_gpu_array_write(x, host)
        x <- .mojor_gpu_route_tag(x, "cpu_scatter")
        return(x)
    }

    target_info <- .mojor_gpu_assign_target_shape(idx_vals, missing_flags)
    vals <- .mojor_gpu_prepare_assign_value(
        value,
        target_info$target_len,
        target_dim = target_info$target_dim,
        target_dtype = .mojor_gpu_array_dtype(x)
    )
    vals_num <- as.numeric(vals)
    slice_assign_ok <- .mojor_gpu_try_i32_slice_assign(x, values = vals_num, idx_vals = idx_vals, x_dim = x_dim)
    if (isTRUE(slice_assign_ok)) {
        x <- .mojor_gpu_route_tag(x, "gpu_scatter")
        return(x)
    }
    scatter_ok <- .mojor_gpu_try_scatter(x, values = vals_num, idx_vals = idx_vals, x_dim = x_dim)
    if (!isTRUE(scatter_ok)) {
        scatter_ok <- .mojor_gpu_try_scatter_linearized(x, values = vals_num, idx_vals = idx_vals, x_dim = x_dim)
    }
    if (isTRUE(scatter_ok)) {
        x <- .mojor_gpu_route_tag(x, "gpu_scatter")
        return(x)
    }

    host <- mojor_gpu_array_read(x)
    if (rank == 1L) {
        host[idx_vals[[1L]]] <- vals
    } else {
        host <- .mojor_gpu_host_assign(host, idx_vals, missing_flags, vals)
    }
    mojor_gpu_array_write(x, host)
    x <- .mojor_gpu_route_tag(x, "cpu_slice_assign")
    x
}

`[<-.GPUArray` <- function(x, i, j, ..., value) {
    `[<-.mojor_gpu_array`(x, i, j, ..., value = value)
}

length.mojor_gpu_array <- function(x) .mojor_gpu_array_length(x)
dim.mojor_gpu_array <- function(x) .mojor_gpu_array_dim(x)
dimnames.mojor_gpu_array <- function(x) .mojor_gpu_array_dimnames(x)
length.GPUArray <- function(x) .mojor_gpu_array_length(x)
dim.GPUArray <- function(x) .mojor_gpu_array_dim(x)
dimnames.GPUArray <- function(x) .mojor_gpu_array_dimnames(x)

.mojor_gpu_field_names <- function() {
    c(
        "handle", "data", "n", "handle_epoch", "api", "dtype", "dim", "dimnames",
        "index_plans", "strides"
    )
}

.mojor_gpu_field_match <- function(key, exact = FALSE) {
    fields <- .mojor_gpu_field_names()
    exact_idx <- which(fields == key)
    if (length(exact_idx) >= 1L) {
        return(
            list(
                status = "unique",
                field = fields[[exact_idx[[1L]]]],
                kind = "exact"
            )
        )
    }
    if (isTRUE(exact)) {
        return(list(status = "none", field = NULL, kind = NULL))
    }
    partial_idx <- which(startsWith(fields, key))
    if (length(partial_idx) == 1L) {
        return(
            list(
                status = "unique",
                field = fields[[partial_idx[[1L]]]],
                kind = "partial"
            )
        )
    }
    if (length(partial_idx) > 1L) {
        return(list(status = "ambiguous", field = NULL, kind = NULL))
    }
    list(status = "none", field = NULL, kind = NULL)
}

.mojor_gpu_dimname_axis_match_status <- function(x, key, exact = FALSE) {
    x_dim <- .mojor_gpu_object_get(x, "dim", default = NULL)
    if (is.null(x_dim)) {
        return(list(status = "none", hit = NULL, kind = NULL, matched = NULL))
    }
    x_dimnames <- .mojor_gpu_object_get(x, "dimnames", default = NULL)
    if (!is.list(x_dimnames) ||
        length(x_dimnames) == 0L) {
        return(list(status = "none", hit = NULL, kind = NULL, matched = NULL))
    }
    rank <- length(as.integer(x_dim))
    if (length(x_dimnames) < rank) {
        return(list(status = "none", hit = NULL, kind = NULL, matched = NULL))
    }
    axis_hits <- list()
    for (axis in seq_len(rank)) {
        axis_names <- x_dimnames[[axis]]
        if (is.null(axis_names)) {
            next
        }
        axis_names <- as.character(axis_names)
        idx <- integer(0L)
        kind <- NULL
        exact_idx <- which(axis_names == key)
        if (length(exact_idx) > 1L) {
            return(
                list(
                    status = "ambiguous_within_axis",
                    hit = NULL,
                    kind = "exact",
                    matched = NULL
                )
            )
        }
        if (length(exact_idx) == 1L) {
            idx <- exact_idx
            kind <- "exact"
        } else if (!isTRUE(exact)) {
            partial_idx <- which(startsWith(axis_names, key))
            if (length(partial_idx) > 1L) {
                return(
                    list(
                        status = "ambiguous_within_axis",
                        hit = NULL,
                        kind = "partial",
                        matched = NULL
                    )
                )
            }
            if (length(partial_idx) == 1L) {
                idx <- partial_idx
                kind <- "partial"
            }
        }
        if (length(idx) == 1L) {
            axis_hits[[length(axis_hits) + 1L]] <- list(
                axis = as.integer(axis),
                index = as.integer(idx[[1L]]),
                kind = kind,
                matched = axis_names[[idx[[1L]]]]
            )
        }
    }
    if (length(axis_hits) == 0L) {
        return(list(status = "none", hit = NULL, kind = NULL, matched = NULL))
    }
    if (length(axis_hits) > 1L) {
        return(list(status = "ambiguous_axes", hit = NULL, kind = NULL, matched = NULL))
    }
    list(
        status = "unique",
        hit = list(axis = axis_hits[[1L]]$axis, index = axis_hits[[1L]]$index),
        kind = axis_hits[[1L]]$kind,
        matched = axis_hits[[1L]]$matched
    )
}

.mojor_gpu_dimname_axis_match <- function(x, key, exact = FALSE) {
    status <- .mojor_gpu_dimname_axis_match_status(
        x,
        key = key,
        exact = exact
    )
    if (!identical(status$status, "unique")) {
        return(NULL)
    }
    status$hit
}

.mojor_gpu_dimname_axis_match_prefer_exact <- function(
    x,
    key,
    allow_partial = TRUE,
    return_status = FALSE
) {
    exact_status <- .mojor_gpu_dimname_axis_match_status(
        x,
        key = key,
        exact = TRUE
    )
    if (!identical(exact_status$status, "none")) {
        if (isTRUE(return_status)) {
            return(exact_status)
        }
        if (identical(exact_status$status, "unique")) {
            return(exact_status$hit)
        }
        return(NULL)
    }
    if (!isTRUE(allow_partial)) {
        if (isTRUE(return_status)) {
            return(exact_status)
        }
        return(NULL)
    }
    partial_status <- .mojor_gpu_dimname_axis_match_status(
        x,
        key = key,
        exact = FALSE
    )
    if (isTRUE(return_status)) {
        return(partial_status)
    }
    if (identical(partial_status$status, "unique")) {
        return(partial_status$hit)
    }
    NULL
}

.mojor_gpu_dimname_axis_stop_ambiguous <- function(context, key, status) {
    if (!identical(status$status, "ambiguous_within_axis") &&
        !identical(status$status, "ambiguous_axes")) {
        return(invisible(NULL))
    }
    if (identical(status$status, "ambiguous_within_axis")) {
        if (identical(status$kind, "partial")) {
            stop(context, ": ambiguous partial dimname label '", key, "'")
        }
        stop(context, ": ambiguous dimname label '", key, "'")
    }
    stop(context, ": dimname label '", key, "' matches multiple axes")
}

.mojor_gpu_get2_exact_mode <- function(exact, context = "[[.mojor_gpu_array") {
    exact_lgl <- suppressWarnings(as.logical(exact))
    if (length(exact_lgl) != 1L ||
        (is.na(exact_lgl) &&
            !anyNA(exact))) {
        stop(context, ": exact must be TRUE, FALSE, or NA")
    }
    if (isTRUE(exact_lgl)) {
        return(list(allow_partial = FALSE, warn_partial = FALSE))
    }
    if (isFALSE(exact_lgl)) {
        return(list(allow_partial = TRUE, warn_partial = FALSE))
    }
    list(allow_partial = TRUE, warn_partial = TRUE)
}

.mojor_gpu_get2_warn_partial <- function(
    context,
    key,
    matched,
    source = "dimname label"
) {
    warning(
        context,
        ": partial match of '",
        key,
        "' to ",
        source,
        " '",
        matched,
        "'",
        call. = FALSE
    )
}

.mojor_gpu_dollar_partial_mode <- function() {
    mode_raw <- getOption("mojor.gpu.dollar_partial", "allow")
    if (is.null(mode_raw) ||
        length(mode_raw) < 1L) {
        mode_raw <- "allow"
    }
    mode <- tolower(as.character(mode_raw[[1L]]))
    if (!mode %in% c("allow", "warn", "exact")) {
        warning(
            "mojor_gpu: unknown dollar partial mode '",
            mode,
            "'; using allow (expected allow/warn/exact)",
            call. = FALSE
        )
        mode <- "allow"
    }
    list(
        allow_partial = !identical(mode, "exact"),
        warn_partial = identical(mode, "warn")
    )
}

.mojor_gpu_get2_tuple_char_indices <- function(
    x,
    keys,
    exact_mode,
    context = "[[.mojor_gpu_array"
) {
    x_dim <- .mojor_gpu_object_get(x, "dim", default = NULL)
    rank <- if (is.null(x_dim))
        1L else length(as.integer(x_dim))
    keys_chr <- as.character(keys)
    if (length(keys_chr) != rank) {
        stop(context, ": tuple index length must match rank")
    }
    if (anyNA(keys_chr) || any(!nzchar(keys_chr))) {
        stop(context, ": tuple character indices must be non-empty scalars")
    }
    x_dimnames <- .mojor_gpu_object_get(x, "dimnames", default = NULL)
    if (!is.list(x_dimnames) ||
        length(x_dimnames) < rank) {
        stop(context, ": character indices require dimnames")
    }
    idx_list <- vector("list", rank)
    for (axis in seq_len(rank)) {
        axis_names <- x_dimnames[[axis]]
        if (is.null(axis_names)) {
            stop(context, ": character indices require dimnames")
        }
        axis_names_chr <- as.character(axis_names)
        key <- keys_chr[[axis]]
        exact_idx <- which(axis_names_chr == key)
        if (length(exact_idx) >= 1L) {
            idx_list[[axis]] <- as.integer(exact_idx[[1L]])
            next
        }
        if (!isTRUE(exact_mode$allow_partial)) {
            stop(context, ": tuple contains unknown dimname labels")
        }
        partial_idx <- which(startsWith(axis_names_chr, key))
        if (length(partial_idx) > 1L) {
            stop(context, ": ambiguous partial dimname label '", key, "'")
        }
        if (length(partial_idx) == 0L) {
            stop(context, ": tuple contains unknown dimname labels")
        }
        hit <- as.integer(partial_idx[[1L]])
        if (isTRUE(exact_mode$warn_partial)) {
            .mojor_gpu_get2_warn_partial(
                context = context,
                key = key,
                matched = axis_names_chr[[hit]]
            )
        }
        idx_list[[axis]] <- hit
    }
    idx_list
}

.mojor_gpu_extract_scalar_value <- function(out, context = "[[.mojor_gpu_array") {
    if (inherits(out, "mojor_gpu_array")) {
        host <- mojor_gpu_array_read(out)
        on.exit(
            try(
                mojor_gpu_array_free(out),
                silent = TRUE
            ),
            add = TRUE
        )
        vals <- as.numeric(host)
        if (length(vals) != 1L) {
            stop(context, ": index must select exactly one element")
        }
        return(vals[[1L]])
    }
    vals <- as.numeric(out)
    if (length(vals) != 1L) {
        stop(context, ": index must select exactly one element")
    }
    vals[[1L]]
}

.mojor_gpu_scalar_index_list <- function(x, i, dots, context) {
    if (length(dots) > 0L) {
        return(c(list(i), dots))
    }
    if (is.list(i) &&
        !is.data.frame(i)) {
        if (length(i) == 0L) {
            stop(context, ": missing index")
        }
        return(i)
    }
    if (is.numeric(i) &&
        is.atomic(i) &&
        is.null(dim(i))) {
        x_dim <- .mojor_gpu_object_get(x, "dim", default = NULL)
        rank <- if (is.null(x_dim))
            1L else length(as.integer(x_dim))
        i_num <- as.numeric(i)
        if (rank > 1L &&
            length(i_num) > 1L) {
            if (length(i_num) != rank) {
                stop(context, ": tuple index length must match rank")
            }
            return(as.list(i_num))
        }
    }
    if (is.character(i) &&
        is.atomic(i) &&
        is.null(dim(i))) {
        x_dim <- .mojor_gpu_object_get(x, "dim", default = NULL)
        rank <- if (is.null(x_dim))
            1L else length(as.integer(x_dim))
        i_chr <- as.character(i)
        if (rank > 1L &&
            length(i_chr) > 1L) {
            if (length(i_chr) != rank) {
                stop(context, ": tuple index length must match rank")
            }
            if (anyNA(i_chr) || any(!nzchar(i_chr))) {
                stop(context, ": tuple character indices must be non-empty scalars")
            }
            return(as.list(i_chr))
        }
    }
    list(i)
}

.mojor_gpu_get2_scalar_linear_index <- function(i) {
    if (is.factor(i)) {
        return(length(i) == 1L)
    }
    (is.numeric(i) || is.logical(i)) &&
        is.atomic(i) &&
        is.null(dim(i)) &&
        length(i) == 1L
}

`[[.mojor_gpu_array` <- function(x, i, ..., exact = TRUE) {
    dots <- list(...)
    if (missing(i)) {
        stop("[[.mojor_gpu_array: missing index")
    }
    x_dim <- .mojor_gpu_object_get(x, "dim", default = NULL)
    rank <- if (is.null(x_dim))
        1L else length(as.integer(x_dim))
    exact_mode <- .mojor_gpu_get2_exact_mode(
        exact,
        context = "[[.mojor_gpu_array"
    )

    if (length(dots) == 0L &&
        rank > 1L &&
        !is.character(i) &&
        isTRUE(.mojor_gpu_get2_scalar_linear_index(i))) {
        host <- mojor_gpu_array_read(x)
        out <- tryCatch(
            host[[i]],
            error = function(e) e
        )
        if (inherits(out, "error")) {
            stop("[[.mojor_gpu_array: ", conditionMessage(out))
        }
        return(as.numeric(out))
    }

    if (is.character(i) &&
        length(i) > 1L &&
        length(dots) == 0L) {
        idx_list <- .mojor_gpu_get2_tuple_char_indices(
            x,
            keys = i,
            exact_mode = exact_mode,
            context = "[[.mojor_gpu_array"
        )
        out <- .mojor_gpu_bracket_eval(
            x,
            idx_vals = idx_list,
            drop = TRUE
        )
        return(
            .mojor_gpu_extract_scalar_value(out, context = "[[.mojor_gpu_array")
        )
    }

    if (is.character(i) &&
        length(i) == 1L &&
        length(dots) == 0L) {
        i_chr <- as.character(i[[1L]])
        if (length(i_chr) != 1L || is.na(i_chr) || !nzchar(i_chr)) {
            stop("[[.mojor_gpu_array: character index must be a non-empty scalar")
        }
        field_status <- .mojor_gpu_field_match(
            i_chr,
            exact = !isTRUE(exact_mode$allow_partial)
        )
        if (identical(field_status$status, "unique")) {
            if (isTRUE(exact_mode$warn_partial) &&
                identical(field_status$kind, "partial")) {
                .mojor_gpu_get2_warn_partial(
                    context = "[[.mojor_gpu_array",
                    key = i_chr,
                    matched = field_status$field,
                    source = "field"
                )
            }
            return(
                .mojor_gpu_object_get(
                    x,
                    field_status$field,
                    default = NULL
                )
            )
        }
        if (identical(field_status$status, "ambiguous")) {
            return(NULL)
        }
        axis_status <- .mojor_gpu_dimname_axis_match_prefer_exact(
            x,
            key = i_chr,
            allow_partial = isTRUE(exact_mode$allow_partial),
            return_status = TRUE
        )
        if (identical(axis_status$status, "unique")) {
            if (isTRUE(exact_mode$warn_partial) &&
                identical(axis_status$kind, "partial")) {
                .mojor_gpu_get2_warn_partial(
                    context = "[[.mojor_gpu_array",
                    key = i_chr,
                    matched = axis_status$matched
                )
            }
            x_dim <- .mojor_gpu_object_get(x, "dim", default = NULL)
            if (is.null(x_dim)) {
                return(NULL)
            }
            rank <- length(as.integer(x_dim))
            idx_list <- vector("list", rank)
            idx_list[[axis_status$hit$axis]] <- axis_status$hit$index
            return(
                .mojor_gpu_bracket_eval(
                    x,
                    idx_vals = idx_list,
                    drop = TRUE
                )
            )
        }
        if (identical(axis_status$status, "ambiguous_within_axis") ||
            identical(axis_status$status, "ambiguous_axes")) {
            return(NULL)
        }
        return(NULL)
    }

    if (length(dots) > 0L) {
        idx_list <- c(list(i), dots)
        out <- .mojor_gpu_bracket_eval(
            x,
            idx_vals = idx_list,
            drop = TRUE
        )
        return(
            .mojor_gpu_extract_scalar_value(out, context = "[[.mojor_gpu_array")
        )
    }

    if (is.list(i) &&
        !is.data.frame(i)) {
        if (length(i) == 0L) {
            stop("[[.mojor_gpu_array: missing index")
        }
        out <- .mojor_gpu_bracket_eval(
            x,
            idx_vals = i,
            drop = TRUE
        )
        return(
            .mojor_gpu_extract_scalar_value(out, context = "[[.mojor_gpu_array")
        )
    }

    if (is.numeric(i) &&
        is.atomic(i) &&
        is.null(dim(i))) {
        x_dim <- .mojor_gpu_object_get(x, "dim", default = NULL)
        rank <- if (is.null(x_dim))
            1L else length(as.integer(x_dim))
        i_num <- as.numeric(i)
        if (rank > 1L &&
            length(i_num) > 1L) {
            if (length(i_num) != rank) {
                stop("[[.mojor_gpu_array: tuple index length must match rank")
            }
            out <- .mojor_gpu_bracket_eval(
                x,
                idx_vals = as.list(i_num),
                drop = TRUE
            )
            return(
                .mojor_gpu_extract_scalar_value(out, context = "[[.mojor_gpu_array")
            )
        }
    }

    out <- tryCatch(
        `[.mojor_gpu_array`(x, i, drop = TRUE),
        error = function(e) e
    )
    if (!inherits(out, "error")) {
        return(
            .mojor_gpu_extract_scalar_value(
                out,
                context = "[[.mojor_gpu_array"
            )
        )
    }
    host <- mojor_gpu_array_read(x)
    host_out <- tryCatch(
        .mojor_gpu_host_get2_raw(
            host,
            i = i,
            dots = dots,
            exact = exact
        ),
        error = function(e) e
    )
    if (!inherits(host_out, "error")) {
        return(
            .mojor_gpu_extract_scalar_value(
                host_out,
                context = "[[.mojor_gpu_array"
            )
        )
    }
    stop(out)
}

`[[.GPUArray` <- function(x, i, ..., exact = TRUE) {
    `[[.mojor_gpu_array`(x, i, ..., exact = exact)
}

`[[<-.mojor_gpu_array` <- function(x, i, ..., value) {
    x <- .mojor_gpu_copy_on_modify_prepare(x)
    on.exit(.mojor_gpu_copy_on_modify_clear(x), add = TRUE)

    dots <- list(...)
    if (missing(i)) {
        stop("[[<-.mojor_gpu_array: missing index")
    }
    x_dim <- .mojor_gpu_object_get(x, "dim", default = NULL)
    rank <- if (is.null(x_dim))
        1L else length(as.integer(x_dim))

    if (length(dots) == 0L &&
        rank > 1L &&
        !is.character(i) &&
        isTRUE(.mojor_gpu_get2_scalar_linear_index(i))) {
        host <- mojor_gpu_array_read(x)
        value_scalar <- .mojor_gpu_prepare_assign_value(
            value,
            target_len = 1L,
            target_dtype = .mojor_gpu_array_dtype(x)
        )[[1L]]
        host_new <- tryCatch(
            {
                host[[i]] <- value_scalar
                host
            },
            error = function(e) e
        )
        if (inherits(host_new, "error")) {
            stop("[[<-.mojor_gpu_array: ", conditionMessage(host_new))
        }
        mojor_gpu_array_write(x, host_new)
        return(.mojor_gpu_route_tag(x, "cpu_scatter"))
    }

    if (is.character(i) &&
        length(i) == 1L &&
        length(dots) == 0L) {
        i_chr <- as.character(i)
        if (length(i_chr) != 1L || is.na(i_chr) || !nzchar(i_chr)) {
            stop("[[<-.mojor_gpu_array: character index must be a non-empty scalar")
        }
        dollar_mode <- .mojor_gpu_dollar_partial_mode()
        field_status <- .mojor_gpu_field_match(
            i_chr,
            exact = !isTRUE(dollar_mode$allow_partial)
        )
        if (identical(field_status$status, "unique")) {
            if (isTRUE(dollar_mode$warn_partial) &&
                identical(field_status$kind, "partial")) {
                .mojor_gpu_get2_warn_partial(
                    context = "[[<-.mojor_gpu_array",
                    key = i_chr,
                    matched = field_status$field,
                    source = "field"
                )
            }
            return(.mojor_gpu_object_set(x, field_status$field, value))
        }
        if (identical(field_status$status, "ambiguous")) {
            stop(
                "[[<-.mojor_gpu_array: ambiguous partial field name '",
                i_chr,
                "'"
            )
        }
        axis_status <- .mojor_gpu_dimname_axis_match_prefer_exact(
            x,
            key = i_chr,
            allow_partial = isTRUE(dollar_mode$allow_partial),
            return_status = TRUE
        )
        if (!identical(axis_status$status, "unique")) {
            .mojor_gpu_dimname_axis_stop_ambiguous(
                context = "[[<-.mojor_gpu_array",
                key = i_chr,
                status = axis_status
            )
            stop("[[<-.mojor_gpu_array: unknown field or dimname axis")
        }
        if (isTRUE(dollar_mode$warn_partial) &&
            identical(axis_status$kind, "partial")) {
            .mojor_gpu_get2_warn_partial(
                context = "[[<-.mojor_gpu_array",
                key = i_chr,
                matched = axis_status$matched
            )
        }
        x_dim <- .mojor_gpu_object_get(x, "dim", default = NULL)
        if (is.null(x_dim)) {
            stop("[[<-.mojor_gpu_array: dimname-axis replacement requires array dimensions")
        }
        rank <- length(as.integer(x_dim))
        idx_list <- vector("list", rank)
        idx_list[[axis_status$hit$axis]] <- axis_status$hit$index
        return(
            .mojor_gpu_bracket_assign(
                x,
                idx_vals = idx_list,
                value = value
            )
        )
    }
    host_fallback <- function(err = NULL) {
        host <- mojor_gpu_array_read(x)
        value_raw <- if (inherits(value, "mojor_gpu_array")) {
            mojor_gpu_array_read(value)
        } else {
            value
        }
        host_new <- tryCatch(
            .mojor_gpu_host_set2_raw(
                host,
                i = i,
                dots = dots,
                value = value_raw
            ),
            error = function(e) e
        )
        if (!inherits(host_new, "error")) {
            mojor_gpu_array_write(x, host_new)
            return(.mojor_gpu_route_tag(x, "cpu_scatter"))
        }
        if (!is.null(err)) {
            stop(err)
        }
        stop(host_new)
    }

    idx_list <- tryCatch(
        .mojor_gpu_scalar_index_list(
            x,
            i,
            dots = dots,
            context = "[[<-.mojor_gpu_array"
        ),
        error = function(e) e
    )
    if (inherits(idx_list, "error")) {
        return(host_fallback(idx_list))
    }

    probe <- tryCatch(
        .mojor_gpu_bracket_eval(
            x,
            idx_vals = idx_list,
            drop = TRUE
        ),
        error = function(e) e
    )
    if (inherits(probe, "error")) {
        return(host_fallback(probe))
    }
    probe_ok <- tryCatch(
        {
            .mojor_gpu_extract_scalar_value(
                probe,
                context = "[[<-.mojor_gpu_array"
            )
            TRUE
        },
        error = function(e) e
    )
    if (inherits(probe_ok, "error")) {
        return(host_fallback(probe_ok))
    }
    assigned <- tryCatch(
        .mojor_gpu_bracket_assign(
            x,
            idx_vals = idx_list,
            value = value
        ),
        error = function(e) e
    )
    if (inherits(assigned, "error")) {
        return(host_fallback(assigned))
    }
    assigned
}

`[[<-.GPUArray` <- function(x, i, ..., value) {
    `[[<-.mojor_gpu_array`(x, i, ..., value = value)
}

`$.mojor_gpu_array` <- function(x, name) {
    if (missing(name)) {
        return(NULL)
    }
    name_chr <- as.character(name)
    if (length(name_chr) != 1L || is.na(name_chr) || !nzchar(name_chr)) {
        return(NULL)
    }
    dollar_mode <- .mojor_gpu_dollar_partial_mode()
    field_status <- .mojor_gpu_field_match(
        name_chr,
        exact = !isTRUE(dollar_mode$allow_partial)
    )
    if (identical(field_status$status, "unique")) {
        if (isTRUE(dollar_mode$warn_partial) &&
            identical(field_status$kind, "partial")) {
            .mojor_gpu_get2_warn_partial(
                context = "$.mojor_gpu_array",
                key = name_chr,
                matched = field_status$field,
                source = "field"
            )
        }
        return(
            .mojor_gpu_object_get(
                x,
                field_status$field,
                default = NULL
            )
        )
    }
    if (identical(field_status$status, "ambiguous")) {
        return(NULL)
    }
    axis_status <- .mojor_gpu_dimname_axis_match_prefer_exact(
        x,
        key = name_chr,
        allow_partial = isTRUE(dollar_mode$allow_partial),
        return_status = TRUE
    )
    if (!identical(axis_status$status, "unique")) {
        return(NULL)
    }
    if (isTRUE(dollar_mode$warn_partial) &&
        identical(axis_status$kind, "partial")) {
        .mojor_gpu_get2_warn_partial(
            context = "$.mojor_gpu_array",
            key = name_chr,
            matched = axis_status$matched
        )
    }
    x_dim <- .mojor_gpu_object_get(x, "dim", default = NULL)
    if (is.null(x_dim)) {
        return(NULL)
    }
    rank <- length(as.integer(x_dim))
    idx_list <- vector("list", rank)
    idx_list[[axis_status$hit$axis]] <- axis_status$hit$index
    .mojor_gpu_bracket_eval(
        x,
        idx_vals = idx_list,
        drop = TRUE
    )
}

`$.GPUArray` <- function(x, name) {
    `$.mojor_gpu_array`(x, name)
}

`$<-.mojor_gpu_array` <- function(x, name, value) {
    x <- .mojor_gpu_copy_on_modify_prepare(x)
    on.exit(.mojor_gpu_copy_on_modify_clear(x), add = TRUE)

    if (missing(name)) {
        return(x)
    }
    name_chr <- as.character(name)
    if (length(name_chr) != 1L || is.na(name_chr) || !nzchar(name_chr)) {
        stop("$<-.mojor_gpu_array: name must be a non-empty scalar")
    }
    dollar_mode <- .mojor_gpu_dollar_partial_mode()
    field_status <- .mojor_gpu_field_match(
        name_chr,
        exact = !isTRUE(dollar_mode$allow_partial)
    )
    if (identical(field_status$status, "unique")) {
        if (isTRUE(dollar_mode$warn_partial) &&
            identical(field_status$kind, "partial")) {
            .mojor_gpu_get2_warn_partial(
                context = "$<-.mojor_gpu_array",
                key = name_chr,
                matched = field_status$field,
                source = "field"
            )
        }
        return(.mojor_gpu_object_set(x, field_status$field, value))
    }
    if (identical(field_status$status, "ambiguous")) {
        stop(
            "$<-.mojor_gpu_array: ambiguous partial field name '",
            name_chr,
            "'"
        )
    }
    axis_status <- .mojor_gpu_dimname_axis_match_prefer_exact(
        x,
        key = name_chr,
        allow_partial = isTRUE(dollar_mode$allow_partial),
        return_status = TRUE
    )
    if (!identical(axis_status$status, "unique")) {
        .mojor_gpu_dimname_axis_stop_ambiguous(
            context = "$<-.mojor_gpu_array",
            key = name_chr,
            status = axis_status
        )
        stop("$<-.mojor_gpu_array: unknown field or dimname axis")
    }
    if (isTRUE(dollar_mode$warn_partial) &&
        identical(axis_status$kind, "partial")) {
        .mojor_gpu_get2_warn_partial(
            context = "$<-.mojor_gpu_array",
            key = name_chr,
            matched = axis_status$matched
        )
    }
    x_dim <- .mojor_gpu_object_get(x, "dim", default = NULL)
    if (is.null(x_dim)) {
        stop("$<-.mojor_gpu_array: dimname-axis replacement requires array dimensions")
    }
    rank <- length(as.integer(x_dim))
    idx_list <- vector("list", rank)
    idx_list[[axis_status$hit$axis]] <- axis_status$hit$index
    .mojor_gpu_bracket_assign(
        x,
        idx_vals = idx_list,
        value = value
    )
}

`$<-.GPUArray` <- function(x, name, value) {
    `$<-.mojor_gpu_array`(x, name, value)
}

.mojor_gpu_summary_host_values <- function(x) {
    if (.mojor_gpu_is_array(x)) {
        return(mojor_gpu_array_read(x))
    }
    x
}

.mojor_gpu_reduce_scalar_value <- function(reducer_out) {
    vals <- as.vector(mojor_gpu_array_read(reducer_out))
    if (inherits(reducer_out, "mojor_gpu_array") ||
        inherits(reducer_out, "GPUArray")) {
        try(mojor_gpu_array_free(reducer_out), silent = TRUE)
    }
    if (length(vals) < 1L) {
        return(NA_real_)
    }
    vals[[1L]]
}

.mojor_gpu_summary_fastpath_ok <- function(x) {
    if (!.mojor_gpu_is_array(x)) {
        return(FALSE)
    }
    n <- suppressWarnings(as.integer(.mojor_gpu_array_length(x)))
    if (is.na(n) || n < 1L) {
        return(FALSE)
    }
    !identical(.mojor_gpu_array_dtype(x), "i32")
}

.mojor_gpu_minmax_suspect_na_sentinel <- function(dtype, value) {
    if (!is.finite(value)) {
        return(FALSE)
    }
    if (identical(dtype, "f32")) {
        return(abs(as.numeric(value)) >= 3.402823e+38)
    }
    if (identical(dtype, "f64")) {
        return(abs(as.numeric(value)) >= (.Machine$double.xmax * 0.99))
    }
    FALSE
}

Summary.mojor_gpu_array <- function(x, ..., na.rm = FALSE) {
    op <- .Generic
    if (!op %in% c("sum", "prod", "min", "max")) {
        stop("mojor_gpu: unsupported Summary operator '", op, "'")
    }
    args <- c(list(x), list(...))
    has_extra <- length(args) > 1L
    if (!isTRUE(has_extra) &&
        !isTRUE(na.rm) &&
        isTRUE(.mojor_gpu_summary_fastpath_ok(x))) {
        dtype <- .mojor_gpu_array_dtype(x)
        reduce_out <- switch(
            op,
            sum = gpu_sum(x),
            prod = gpu_prod(x),
            min = gpu_min(x),
            max = gpu_max(x)
        )
        out_val <- .mojor_gpu_reduce_scalar_value(reduce_out)
        suspect_na <- is.nan(out_val)
        if (op %in% c("min", "max")) {
            suspect_na <- isTRUE(suspect_na) ||
                isTRUE(.mojor_gpu_minmax_suspect_na_sentinel(dtype, out_val))
        }
        if (isTRUE(suspect_na)) {
            host_x <- .mojor_gpu_summary_host_values(x)
            if (anyNA(host_x)) {
                return(
                    do.call(
                        match.fun(op),
                        list(host_x, na.rm = FALSE)
                    )
                )
            }
        }
        return(out_val)
    }
    host_args <- lapply(args, .mojor_gpu_summary_host_values)
    do.call(
        match.fun(op),
        c(host_args, list(na.rm = isTRUE(na.rm)))
    )
}

Summary.GPUArray <- Summary.mojor_gpu_array

mean.mojor_gpu_array <- function(x, trim = 0, na.rm = FALSE, ...) {
    if (!is.numeric(trim) ||
        length(trim) != 1L ||
        is.na(trim[[1L]]) ||
        trim[[1L]] != 0 ||
        isTRUE(na.rm) ||
        !isTRUE(.mojor_gpu_summary_fastpath_ok(x))) {
        return(mean(.mojor_gpu_summary_host_values(x), trim = trim, na.rm = na.rm, ...))
    }
    reduce_out <- gpu_mean(x)
    out_val <- .mojor_gpu_reduce_scalar_value(reduce_out)
    if (is.nan(out_val)) {
        host_x <- .mojor_gpu_summary_host_values(x)
        if (anyNA(host_x)) {
            return(mean(host_x, trim = trim, na.rm = na.rm, ...))
        }
    }
    out_val
}

mean.GPUArray <- function(x, trim = 0, na.rm = FALSE, ...) {
    mean.mojor_gpu_array(x, trim = trim, na.rm = na.rm, ...)
}

which.min.mojor_gpu_array <- function(x) {
    host <- .mojor_gpu_summary_host_values(x)
    expected <- which.min(host)
    if (length(expected) < 1L) {
        return(expected)
    }
    if (anyNA(host)) {
        return(expected)
    }
    out <- gpu_argmin(x)
    idx <- as.integer(as.vector(mojor_gpu_array_read(out))[[1L]])
    try(mojor_gpu_array_free(out), silent = TRUE)
    if (identical(idx, as.integer(expected))) {
        return(as.integer(idx))
    }
    if (identical(as.integer(idx + 1L), as.integer(expected))) {
        return(as.integer(idx + 1L))
    }
    as.integer(expected)
}

which.min.GPUArray <- function(x) which.min.mojor_gpu_array(x)

which.max.mojor_gpu_array <- function(x) {
    host <- .mojor_gpu_summary_host_values(x)
    expected <- which.max(host)
    if (length(expected) < 1L) {
        return(expected)
    }
    if (anyNA(host)) {
        return(expected)
    }
    out <- gpu_argmax(x)
    idx <- as.integer(as.vector(mojor_gpu_array_read(out))[[1L]])
    try(mojor_gpu_array_free(out), silent = TRUE)
    if (identical(idx, as.integer(expected))) {
        return(as.integer(idx))
    }
    if (identical(as.integer(idx + 1L), as.integer(expected))) {
        return(as.integer(idx + 1L))
    }
    as.integer(expected)
}

which.max.GPUArray <- function(x) which.max.mojor_gpu_array(x)

gpu_which_min <- function(x) {
    if (.mojor_gpu_is_array(x)) {
        return(which.min.mojor_gpu_array(x))
    }
    which.min(x)
}

gpu_which_max <- function(x) {
    if (.mojor_gpu_is_array(x)) {
        return(which.max.mojor_gpu_array(x))
    }
    which.max(x)
}

Math.mojor_gpu_array <- function(x, ...) {
    op <- .Generic
    if (!op %in% c(
        "sin", "cos", "tan",
        "exp", "log", "log10", "log2", "log1p", "expm1",
        "sqrt", "abs", "sign", "trunc", "floor", "ceiling"
    )) {
        stop("mojor_gpu: unsupported Math operator '", op, "'")
    }
    .mojor_gpu_unary_math_dispatch(op, x)
}

Math.GPUArray <- function(x, ...) {
    op <- .Generic
    if (!op %in% c(
        "sin", "cos", "tan",
        "exp", "log", "log10", "log2", "log1p", "expm1",
        "sqrt", "abs", "sign", "trunc", "floor", "ceiling"
    )) {
        stop("mojor_gpu: unsupported Math operator '", op, "'")
    }
    .mojor_gpu_unary_math_dispatch(op, x)
}

round.mojor_gpu_array <- function(x, digits = 0, ...) {
    .mojor_gpu_unary_math_dispatch("round", x, digits = digits)
}

round.GPUArray <- function(x, digits = 0, ...) {
    round.mojor_gpu_array(x, digits = digits, ...)
}

gpu_atan2 <- function(y, x) {
    .mojor_gpu_binary_math_apply("atan2", y, x, na_rm = FALSE)
}

gpu_minimum <- function(..., na.rm = FALSE) {
    args <- list(...)
    if (length(args) < 1L) {
        stop("gpu_minimum: at least one argument is required")
    }
    if (length(args) == 1L) {
        return(args[[1L]])
    }
    out <- args[[1L]]
    for (k in 2:length(args)) {
        prev <- out
        out <- .mojor_gpu_binary_math_apply(
            "pmin",
            out,
            args[[k]],
            na_rm = isTRUE(na.rm)
        )
        prev_is_input <- any(vapply(args, identical, logical(1), prev))
        if (inherits(prev, "mojor_gpu_array") &&
            !isTRUE(prev_is_input) &&
            !identical(prev, out)) {
            try(mojor_gpu_array_free(prev), silent = TRUE)
        }
    }
    out
}

gpu_maximum <- function(..., na.rm = FALSE) {
    args <- list(...)
    if (length(args) < 1L) {
        stop("gpu_maximum: at least one argument is required")
    }
    if (length(args) == 1L) {
        return(args[[1L]])
    }
    out <- args[[1L]]
    for (k in 2:length(args)) {
        prev <- out
        out <- .mojor_gpu_binary_math_apply(
            "pmax",
            out,
            args[[k]],
            na_rm = isTRUE(na.rm)
        )
        prev_is_input <- any(vapply(args, identical, logical(1), prev))
        if (inherits(prev, "mojor_gpu_array") &&
            !isTRUE(prev_is_input) &&
            !identical(prev, out)) {
            try(mojor_gpu_array_free(prev), silent = TRUE)
        }
    }
    out
}

Ops.mojor_gpu_array <- function(e1, e2) .mojor_gpu_arith_dispatch(.Generic, e1, e2)
`%*%.mojor_gpu_array` <- function(x, y) .mojor_gpu_matmul_dispatch(x, y)
crossprod.mojor_gpu_array <- function(x, y = NULL, ...) .mojor_gpu_crossprod_dispatch(x, y)
tcrossprod.mojor_gpu_array <- function(x, y = NULL, ...) .mojor_gpu_tcrossprod_dispatch(x, y)
Ops.GPUArray <- function(e1, e2) .mojor_gpu_arith_dispatch(.Generic, e1, e2)
`%*%.GPUArray` <- function(x, y) .mojor_gpu_matmul_dispatch(x, y)
crossprod.GPUArray <- function(x, y = NULL, ...) .mojor_gpu_crossprod_dispatch(x, y)
tcrossprod.GPUArray <- function(x, y = NULL, ...) .mojor_gpu_tcrossprod_dispatch(x, y)
as.array.mojor_gpu_array <- function(x, ...) .mojor_gpu_as_array_dispatch(x, ...)
as.matrix.mojor_gpu_array <- function(x, ...) .mojor_gpu_as_matrix_dispatch(x, ...)
as.numeric.mojor_gpu_array <- function(x, ...) .mojor_gpu_as_numeric_dispatch(x, ...)
as.array.GPUArray <- function(x, ...) .mojor_gpu_as_array_dispatch(x, ...)
as.matrix.GPUArray <- function(x, ...) .mojor_gpu_as_matrix_dispatch(x, ...)
as.numeric.GPUArray <- function(x, ...) .mojor_gpu_as_numeric_dispatch(x, ...)

.mojor_gpu_input_numeric <- function(x, chunk_size = NULL) {
    if (inherits(x, "mojor_gpu_array")) {
        return(mojor_gpu_array_read(x))
    }
    x
}

mojor_gpu_sigmoid <- function(x, api = c("auto", "metal", "cuda", "amd")) {
    if (inherits(x, "mojor_gpu_array")) {
        api <- .mojor_gpu_array_api(x)
    } else {
        api <- .mojor_gpu_api_resolve(api)
    }
    x <- .mojor_gpu_input_numeric(x)
    if (.mojor_float_available() && float::is.float(x)) {
        out <- mojor_sigmoid_f32_gpu(x)
        buf <- mojor_gpu_array(
            float::dbl(out),
            api = api
        )
    } else {
        out <- mojor_sigmoid_f64_gpu(x)
        buf <- mojor_gpu_array(out, api = api, dtype = "f64")
    }
    buf$data <- out
    buf
}

mojor_gpu_affine <- function(x, scale = 1, bias = 0, api = c("auto", "metal", "cuda", "amd")) {
    if (inherits(x, "mojor_gpu_array")) {
        api <- .mojor_gpu_array_api(x)
    } else {
        api <- .mojor_gpu_api_resolve(api)
    }
    x <- .mojor_gpu_input_numeric(x)
    out <- mojor_sigmoid_affine_f32_gpu(x, scale = scale, bias = bias)
    buf <- mojor_gpu_array(
        float::dbl(out),
        api = api
    )
    buf$data <- out
    buf
}

mojor_gpu_linear <- function(x, scale = 1, bias = 0, api = c("auto", "metal", "cuda", "amd")) {
    if (inherits(x, "mojor_gpu_array")) {
        api <- .mojor_gpu_array_api(x)
        dtype <- .mojor_gpu_array_dtype(x)
        bytes_per <- if (identical(dtype, "f64"))
            8L else 4L
        .mojor_gpu_check_limit("gpu_buf_affine", x$n, buffers = 2L, bytes_per = bytes_per)
        handle <- .mojor_gpu_array_handle(x)
        out_handle <- .mojor_gpu_buf_call(
            dtype, "affine", handle, as.numeric(scale),
            as.numeric(bias),
            as_handle = TRUE
        )
        return(
            .mojor_gpu_wrap_buffer_result(out_handle, n = x$n, api = api, dtype = dtype)
        )
    }
    api <- .mojor_gpu_api_resolve(api)
    buf <- mojor_gpu_array(x, api = api)
    out <- mojor_gpu_linear(buf, scale = scale, bias = bias, api = api)
    mojor_gpu_array_free(buf)
    out
}

mojor_gpu_chain_array <- function(
    x, iters = 5L, scale = 1, bias = 0, post_scale = 1, post_bias = 0,
    post_iters = 0L, api = c("auto", "metal", "cuda", "amd")
) {
    if (inherits(x, "mojor_gpu_array")) {
        api <- .mojor_gpu_array_api(x)
        dtype <- .mojor_gpu_array_dtype(x)
        bytes_per <- if (identical(dtype, "f64"))
            8L else 4L
        .mojor_gpu_check_limit("gpu_buf_chain", x$n, buffers = 2L, bytes_per = bytes_per)
        handle <- .mojor_gpu_array_handle(x)
        out_handle <- .mojor_gpu_buf_call(
            dtype, "chain", handle, as.integer(iters),
            as.numeric(scale),
            as.numeric(bias),
            as.numeric(post_scale),
            as.numeric(post_bias),
            as.integer(post_iters),
            as_handle = TRUE
        )
        return(
            .mojor_gpu_wrap_buffer_result(out_handle, n = x$n, api = api, dtype = dtype)
        )
    }
    api <- .mojor_gpu_api_resolve(api)
    x <- .mojor_gpu_input_numeric(x)
    out <- mojor_gpu_chain(
        x, iters = iters, op = "sigmoid", scale = scale, bias = bias, post_scale = post_scale,
        post_bias = post_bias, post_iters = post_iters
    )
    buf <- mojor_gpu_array(
        float::dbl(out),
        api = api
    )
    buf$data <- out
    buf
}

mojor_gpu_sum <- function(
    x, iters = 5L, scale = 1, bias = 0, post_scale = 1, post_bias = 0,
    post_iters = 0L
) {
    if (inherits(x, "mojor_gpu_array")) {
        handle <- .mojor_gpu_array_handle(x)
        dtype <- .mojor_gpu_array_dtype(x)
        out <- .mojor_gpu_buf_call(
            dtype, "chain_sum", handle, as.integer(iters),
            as.numeric(scale),
            as.numeric(bias),
            as.numeric(post_scale),
            as.numeric(post_bias),
            as.integer(post_iters),
            require = TRUE
        )
        return(out)
    }
    x <- .mojor_gpu_input_numeric(x)
    mojor_sigmoid_affine_f32_gpu_chain_sum(
        x, iters = iters, scale = scale, bias = bias, post_scale = post_scale,
        post_bias = post_bias, post_iters = post_iters
    )
}

mojor_gpu_session <- function(x) {
    x <- .mojor_gpu_input_numeric(x)
    ga <- .mojor_as_float32(x)
    data <- methods::slot(ga, "Data")
    n <- length(data)
    .mojor_gpu_check_limit("gpu_session_create", n, buffers = 4L, bytes_per = 4L)
    status <- .mojor_call_bridge("mojor_gpu_session_create", .mojor_gpu_ctx_get(), as.integer(n))
    if (!is.numeric(status) ||
        length(status) !=
            1 || status <= 0) {
        stop(
            "mojor_gpu_session: GPU session init failed (status=", status,
            ")", call. = FALSE
        )
    }
    sess <- .mojor_gpu_new_object(
        list(data = ga, n = n, status = status),
        class = "mojor_gpu_session"
    )
    attr(sess, "gpu_status") <- status
    sess
}

print.mojor_gpu_session <- function(x, ...) {
    cat("mojor_gpu_session<", x$n, ">\n", sep = "")
    invisible(x)
}

mojor_gpu_session_run <- function(
    session, iters = 5L, scale = 1, bias = 0, post_scale = 1, post_bias = 0,
    post_iters = 0L
) {
    if (!inherits(session, "mojor_gpu_session")) {
        stop("mojor_gpu_session_run: session must be mojor_gpu_session")
    }
    data <- methods::slot(session$data, "Data")
    out_data <- .mojor_call_bridge(
        "mojor_gpu_session_chain_int", .mojor_gpu_ctx_get(), data, as.integer(iters),
        as.numeric(scale),
        as.numeric(bias),
        as.numeric(post_scale),
        as.numeric(post_bias),
        as.integer(post_iters)
    )
    .mojor_gpu_require("gpu_session_chain", out_data)
    status <- attr(out_data, "gpu_status")
    out <- float::float32(out_data)
    sess <- .mojor_gpu_new_object(
        list(
            data = out, n = length(out),
            status = status
        ),
        class = "mojor_gpu_session"
    )
    attr(sess, "gpu_status") <- status
    sess
}

mojor_gpu_session_sum <- function(
    session, iters = 5L, scale = 1, bias = 0, post_scale = 1, post_bias = 0,
    post_iters = 0L
) {
    if (!inherits(session, "mojor_gpu_session")) {
        stop("mojor_gpu_session_sum: session must be mojor_gpu_session")
    }
    data <- methods::slot(session$data, "Data")
    out <- .mojor_call_bridge(
        "mojor_gpu_session_chain_sum_int", .mojor_gpu_ctx_get(), data,
        as.integer(iters),
        as.numeric(scale),
        as.numeric(bias),
        as.numeric(post_scale),
        as.numeric(post_bias),
        as.integer(post_iters)
    )
    .mojor_gpu_require("gpu_session_chain_sum", out)
    out
}

mojor_gpu_session_free <- function(session) {
    if (!inherits(session, "mojor_gpu_session")) {
        stop("mojor_gpu_session_free: session must be mojor_gpu_session")
    }
    .mojor_call_bridge("mojor_gpu_session_free", .mojor_gpu_ctx_get())
    session$data <- float::float32(integer(0))
    session$n <- 0L
    session$status <- 0L
    invisible(session)
}

# =============================================================================
# Step 24: GPU Array - Reduction
# =============================================================================

# GPU Reduction - parallel reduction on GPU op: one of 'sum', 'mean',
# 'prod', 'min', 'max' dims: reduction dimensions (`NULL` for all dimensions)
# keepdims: whether to keep reduced dimensions

.mojor_gpu_reduce_host <- function(host, op_norm, dims = NULL, keepdims = FALSE) {
    reduce_fun <- switch(
        op_norm, sum = sum, mean = mean, prod = prod, min = min, max = max, argmin = which.min,
        argmax = which.max, NULL
    )
    if (is.null(reduce_fun)) {
        stop(
            "mojor_gpu_reduce: unsupported op (use sum/prod/min/max/mean/argmin/argmax)"
        )
    }

    x_dim <- dim(host)
    if (is.null(x_dim)) {
        x_dim <- as.integer(length(host))
    } else {
        x_dim <- as.integer(x_dim)
    }
    rank <- length(x_dim)
    dims_int <- if (is.null(dims) ||
        length(dims) ==
            0L) {
        NULL
    } else {
        as.integer(unique(dims))
    }
    if (!is.null(dims_int)) {
        if (anyNA(dims_int) ||
            any(dims_int < 1L) ||
            any(dims_int > rank)) {
            stop("mojor_gpu_reduce: invalid dims")
        }
    }

    if (is.null(dims_int)) {
        out <- reduce_fun(as.vector(host))
        if (isTRUE(keepdims) &&
            !is.null(dim(host))) {
            return(array(out, dim = rep.int(1L, rank)))
        }
        return(out)
    }

    keep_axes <- setdiff(
        seq_len(rank),
        dims_int
    )
    if (length(keep_axes) ==
        0L) {
        out <- reduce_fun(as.vector(host))
        if (isTRUE(keepdims)) {
            out_dim <- x_dim
            out_dim[dims_int] <- 1L
            return(array(out, dim = out_dim))
        }
        return(out)
    }

    out_apply <- apply(host, keep_axes, reduce_fun)
    if (isTRUE(keepdims)) {
        out_dim <- x_dim
        out_dim[dims_int] <- 1L
        return(
            array(
                as.vector(out_apply),
                dim = out_dim
            )
        )
    }
    if (length(keep_axes) ==
        1L) {
        return(as.vector(out_apply))
    }
    array(
        as.vector(out_apply),
        dim = x_dim[keep_axes]
    )
}

.mojor_gpu_reduce <- function(x, op, dims = NULL, keepdims = FALSE) {
    if (!inherits(x, "mojor_gpu_array")) {
        stop("mojor_gpu_reduce: x must be mojor_gpu_array")
    }

    handle <- .mojor_gpu_array_handle(x)
    dtype <- .mojor_gpu_array_dtype(x)
    api <- .mojor_gpu_array_api(x)

    if (!.mojor_gpu_linalg_dtype_supported(dtype)) {
        .mojor_gpu_stop_unsupported_linalg_dtype("mojor_gpu_reduce", dtype)
    }

    if (is.null(handle)) {
        stop("mojor_gpu_reduce: missing handle")
    }

    op_norm <- NULL
    if (is.character(op) &&
        length(op) ==
            1L && !is.na(op)) {
        op_norm <- tolower(op)
    } else if (is.name(op) &&
        length(op) ==
            1L) {
        op_norm <- tolower(as.character(op))
    }
    if (is.null(op_norm)) {
        stop("mojor_gpu_reduce: op must be a single character string")
    }
    if (identical(op_norm, "product")) {
        op_norm <- "prod"
    } else if (identical(op_norm, "pmin")) {
        op_norm <- "min"
    } else if (identical(op_norm, "pmax")) {
        op_norm <- "max"
    }

    op_map <- c(sum = 0L, min = 1L, max = 2L, mean = 3L, prod = 4L, argmin = 5L, argmax = 6L)
    op_code <- unname(op_map[[op_norm]])
    if (is.null(op_code)) {
        stop(
            "mojor_gpu_reduce: unsupported op (use sum/prod/min/max/mean/argmin/argmax)"
        )
    }
    f64_reduce_op_class <- if (identical(op_norm, "argmin") ||
        identical(op_norm, "argmax"))
        "arg" else "value"
    op_fun <- switch(
        op_norm, sum = TRUE, min = TRUE, max = TRUE, mean = TRUE, prod = TRUE, argmin = TRUE,
        argmax = TRUE, FALSE
    )
    f64_force_cpu <- identical(dtype, "f64") &&
        isTRUE(.mojor_gpu_force_f64_cpu_fallback(api))
    f64_ctx <- if (identical(dtype, "f64"))
        tryCatch(.mojor_gpu_ctx_get(), error = function(e) NULL) else NULL
    f64_ctx_epoch <- if (is.null(f64_ctx))
        NULL else .mojor_gpu_ctx_epoch_get()
    cpu_reduce_fallback <- function(dims_arg, reason = NULL, reason_code = NULL) {
        host <- mojor_gpu_array_read(x)
        out_host <- .mojor_gpu_reduce_host(host, op_norm, dims = dims_arg, keepdims = keepdims)
        out <- mojor_gpu_array(out_host, api = api, dtype = dtype)
        out <- .mojor_gpu_route_tag(
            out,
            "cpu_reduce",
            reason = reason,
            reason_code = reason_code
        )
        out
    }
    out_n <- NA_integer_
    dims_int <- NULL
    reduce_all_dims <- FALSE

    if (!is.null(dims) &&
        length(dims) >
            0L) {
        dims_int <- as.integer(dims)
        if (anyNA(dims_int)) {
            stop("mojor_gpu_reduce: invalid dims")
        }
        dims_int <- unique(dims_int)
        ndim <- if (is.null(x$dim))
            1L else as.integer(length(x$dim))
        if (any(dims_int < 1L) ||
            any(dims_int > ndim)) {
            stop("mojor_gpu_reduce: invalid dims")
        }
        reduce_all_dims <- identical(
            sort.int(dims_int),
            seq_len(ndim)
        )
        if (!isTRUE(op_fun)) {
            stop(
                "mojor_gpu_reduce: unsupported op (use sum/prod/min/max/mean/argmin/argmax)"
            )
        }

        if (!isTRUE(reduce_all_dims)) {
            full_dim <- if (is.null(x$dim))
                as.integer(x$n) else as.integer(x$dim)
            encoded_dims <- full_dim
            encoded_dims[dims_int] <- -encoded_dims[dims_int]

            if (identical(dtype, "f64")) {
                if (isTRUE(f64_force_cpu)) {
                    return(cpu_reduce_fallback(
                        dims_int,
                        reason = "f64 reduce disabled on metal backend",
                        reason_code = "disabled_on_metal_backend"
                    ))
                }
                if (!isTRUE(
                    .mojor_gpu_f64_reduce_capable(
                        dims_mode = "dims",
                        op_class = f64_reduce_op_class
                    )
                )) {
                    probe_diag <- .mojor_gpu_f64_reduce_probe_diag(
                        dims_mode = "dims",
                        op_class = f64_reduce_op_class,
                        ctx = f64_ctx,
                        ctx_epoch = f64_ctx_epoch
                    )
                    return(cpu_reduce_fallback(
                        dims_int,
                        reason = probe_diag$reason,
                        reason_code = probe_diag$code
                    ))
                }
            }

            keepdims_i <- if (isTRUE(keepdims))
                1L else 0L
            require_label <- switch(
                dtype, f64 = "gpu_buf_f64_reduce", i32 = "gpu_buf_i32_reduce",
                "gpu_buf_f32_reduce"
            )
            reduce_res <- tryCatch(
                .mojor_gpu_reduce_call(
                  dtype, handle, op_code, dims = encoded_dims, keepdims_i = keepdims_i,
                  require_label = require_label
              ),
                error = function(e) NULL
            )
            if (is.null(reduce_res)) {
                return(cpu_reduce_fallback(
                    dims_int,
                    reason = paste0("gpu_", dtype, "_reduce kernel call failed"),
                    reason_code = "reduce_kernel_call_failed_dims"
                ))
            }
            out_handle <- reduce_res$handle
            out_n <- reduce_res$n
        }
    }

    if (is.null(dims) ||
        length(dims) ==
            0L || isTRUE(reduce_all_dims)) {
        if (identical(dtype, "f64")) {
            if (isTRUE(f64_force_cpu)) {
                return(cpu_reduce_fallback(
                    NULL,
                    reason = "f64 reduce disabled on metal backend",
                    reason_code = "disabled_on_metal_backend"
                ))
            }
            if (!isTRUE(
                .mojor_gpu_f64_reduce_capable(
                    dims_mode = "scalar",
                    op_class = f64_reduce_op_class
                )
            )) {
                probe_diag <- .mojor_gpu_f64_reduce_probe_diag(
                    dims_mode = "scalar",
                    op_class = f64_reduce_op_class,
                    ctx = f64_ctx,
                    ctx_epoch = f64_ctx_epoch
                )
                return(cpu_reduce_fallback(
                    NULL,
                    reason = probe_diag$reason,
                    reason_code = probe_diag$code
                ))
            }
        }

        require_label <- switch(
            dtype, f64 = "gpu_buf_f64_reduce", i32 = "gpu_buf_i32_reduce",
            "gpu_buf_f32_reduce"
        )
        reduce_res <- tryCatch(
            .mojor_gpu_reduce_call(
                dtype, handle, op_code, dims = NULL, keepdims_i = as.integer(keepdims),
                require_label = require_label
            ),
            error = function(e) NULL
        )
        if (is.null(reduce_res)) {
            return(cpu_reduce_fallback(
                NULL,
                reason = paste0("gpu_", dtype, "_reduce kernel call failed"),
                reason_code = "reduce_kernel_call_failed_scalar"
            ))
        }
        out_handle <- reduce_res$handle
        out_n <- reduce_res$n
    }

    out <- .mojor_gpu_wrap_buffer_result(
        out_handle, n = out_n, api = api, dtype = dtype, include_shape = TRUE,
        dim = NULL, dimnames = NULL, strides = NULL, class = "mojor_gpu_array"
    )

    if (!is.null(x$dim)) {
        if (isTRUE(keepdims)) {
            if (!is.null(dims) &&
                length(dims) >
                  0L) {
                out$dim <- x$dim
                out$dim[dims_int] <- 1L
            } else {
                out$dim <- rep.int(1L, length(x$dim))
            }
        } else if (!is.null(dims) &&
            length(dims) >
                0L) {
            keep_dims <- setdiff(
                seq_along(x$dim),
                dims_int
            )
            if (length(keep_dims) >
                1L) {
                out$dim <- x$dim[keep_dims]
            }
        }
        if (!is.null(out$dim)) {
            out$strides <- .mojor_dim_strides(out$dim)
        }
    }

    out <- .mojor_gpu_route_tag(out, "gpu_reduce")
    out
}

gpu_reduce <- function(x, op, dims = NULL, keepdims = FALSE) {
    .mojor_gpu_reduce(x, op, dims, keepdims)
}

gpu_sum <- function(x, dims = NULL, keepdims = FALSE) .mojor_gpu_reduce(x, "sum", dims, keepdims)
gpu_mean <- function(x, dims = NULL, keepdims = FALSE) .mojor_gpu_reduce(x, "mean", dims, keepdims)
gpu_prod <- function(x, dims = NULL, keepdims = FALSE) .mojor_gpu_reduce(x, "prod", dims, keepdims)
gpu_min <- function(x, dims = NULL, keepdims = FALSE) .mojor_gpu_reduce(x, "min", dims, keepdims)
gpu_max <- function(x, dims = NULL, keepdims = FALSE) .mojor_gpu_reduce(x, "max", dims, keepdims)
gpu_pmin <- function(x, dims = NULL, keepdims = FALSE) .mojor_gpu_reduce(x, "min", dims, keepdims)
gpu_pmax <- function(x, dims = NULL, keepdims = FALSE) .mojor_gpu_reduce(x, "max", dims, keepdims)
gpu_argmin <- function(x, dims = NULL) .mojor_gpu_reduce(x, "argmin", dims, keepdims = FALSE)
gpu_argmax <- function(x, dims = NULL) .mojor_gpu_reduce(x, "argmax", dims, keepdims = FALSE)

# GPU Matmul - matrix multiplication on GPU

.mojor_gpu_matmul <- function(x, y, transpose_a = FALSE, transpose_b = FALSE, out = NULL) {
    if (!inherits(x, "mojor_gpu_array")) {
        stop("mojor_gpu_matmul: x must be mojor_gpu_array")
    }
    if (!inherits(y, "mojor_gpu_array")) {
        stop("mojor_gpu_matmul: y must be mojor_gpu_array")
    }

    prep <- .mojor_gpu_promote_linalg_pair(x, y)
    x <- prep$x
    y <- prep$y
    tmp_arrays <- prep$tmp
    if (!is.null(out)) {
        if (!inherits(out, "mojor_gpu_array")) {
            stop("mojor_gpu_matmul: out must be mojor_gpu_array")
        }
        out_dtype <- .mojor_gpu_array_dtype(out)
        if (!.mojor_gpu_linalg_dtype_supported(out_dtype)) {
            .mojor_gpu_stop_unsupported_linalg_dtype("mojor_gpu_matmul", out_dtype)
        }
        if (!identical(out_dtype, prep$dtype)) {
            if (!identical(.mojor_gpu_array_dtype(x), out_dtype)) {
                x_cast <- gpu_cast(x, dtype = out_dtype)
                if (!identical(x_cast, x) &&
                    !any(vapply(tmp_arrays, identical, logical(1), x_cast))) {
                    tmp_arrays <- c(tmp_arrays, list(x_cast))
                }
                x <- x_cast
            }
            if (!identical(.mojor_gpu_array_dtype(y), out_dtype)) {
                y_cast <- gpu_cast(y, dtype = out_dtype)
                if (!identical(y_cast, y) &&
                    !any(vapply(tmp_arrays, identical, logical(1), y_cast))) {
                    tmp_arrays <- c(tmp_arrays, list(y_cast))
                }
                y <- y_cast
            }
        }
    }
    if (length(tmp_arrays) > 0L) {
        on.exit(
            .mojor_gpu_free_temp_arrays(tmp_arrays),
            add = TRUE
        )
    }

    handle_x <- .mojor_gpu_array_handle(x)
    handle_y <- .mojor_gpu_array_handle(y)
    dtype <- .mojor_gpu_dtype_from_args(x, y)
    api <- .mojor_gpu_pick_api(x, y)

    if (!.mojor_gpu_linalg_dtype_supported(dtype)) {
        .mojor_gpu_stop_unsupported_linalg_dtype("mojor_gpu_matmul", dtype)
    }

    if (is.null(handle_x) ||
        is.null(handle_y)) {
        stop("mojor_gpu_matmul: missing handle")
    }

 # Get dimensions
    dim_x <- x$dim
    dim_y <- y$dim

    if (is.null(dim_x) ||
        length(dim_x) !=
            2) {
        stop("mojor_gpu_matmul: x must be 2D")
    }
    if (is.null(dim_y) ||
        length(dim_y) !=
            2) {
        stop("mojor_gpu_matmul: y must be 2D")
    }

 # Apply transpose flags
    m <- if (transpose_a)
        dim_x[2] else dim_x[1]
    k_x <- if (transpose_a)
        dim_x[1] else dim_x[2]
    k_y <- if (transpose_b)
        dim_y[2] else dim_y[1]
    n <- if (transpose_b)
        dim_y[1] else dim_y[2]

    if (k_x != k_y) {
        stop("mojor_gpu_matmul: inner dimensions must match")
    }

    k <- k_x
    out_dim <- c(
        as.integer(m),
        as.integer(n)
    )
    matmul_bridge <- switch(
        dtype, f64 = "mojor_gpu_buf_f64_matmul", i32 = "mojor_gpu_buf_i32_matmul",
        "mojor_gpu_buf_f32_matmul"
    )
    matmul_into_bridge <- switch(
        dtype, f64 = "mojor_gpu_buf_f64_matmul_into", i32 = "mojor_gpu_buf_i32_matmul_into",
        "mojor_gpu_buf_f32_matmul_into"
    )
    matmul_require <- switch(
        dtype, f64 = "gpu_buf_f64_matmul", i32 = "gpu_buf_i32_matmul",
        "gpu_buf_f32_matmul"
    )
    out_info <- .mojor_gpu_buf_info_from_dtype(dtype)
    cpu_matmul_host <- function() {
        host_x <- mojor_gpu_array_read(x)
        host_y <- mojor_gpu_array_read(y)
        if (isTRUE(transpose_a)) {
            host_x <- t(host_x)
        }
        if (isTRUE(transpose_b)) {
            host_y <- t(host_y)
        }
        host_x %*% host_y
    }
    f64_force_cpu <- identical(dtype, "f64") &&
        isTRUE(.mojor_gpu_force_f64_cpu_fallback(api))
    f64_mode <- if (identical(dtype, "f64"))
        .mojor_gpu_f64_matmul_mode(m, n) else NULL
    f64_probe_ok <- !identical(dtype, "f64") ||
        isTRUE(.mojor_gpu_f64_matmul_capable(mode = f64_mode))
    f64_matmul_gpu <- !identical(dtype, "f64") ||
        (!isTRUE(f64_force_cpu) && isTRUE(f64_probe_ok))
    f64_cpu_reason <- NULL
    f64_cpu_reason_code <- NULL
    if (identical(dtype, "f64")) {
        if (isTRUE(f64_force_cpu)) {
            f64_cpu_reason <- "f64 matmul disabled on metal backend"
            f64_cpu_reason_code <- "disabled_on_metal_backend"
        } else if (!isTRUE(f64_probe_ok)) {
            f64_ctx <- tryCatch(
                .mojor_gpu_ctx_get(),
                error = function(e) NULL
            )
            f64_ctx_epoch <- if (is.null(f64_ctx))
                NULL else .mojor_gpu_ctx_epoch_get()
            probe_diag <- .mojor_gpu_f64_matmul_probe_diag(
                mode = f64_mode,
                ctx = f64_ctx,
                ctx_epoch = f64_ctx_epoch
            )
            f64_cpu_reason <- probe_diag$reason
            f64_cpu_reason_code <- probe_diag$code
        }
    }

    if (!is.null(out)) {
        if (!inherits(out, "mojor_gpu_array")) {
            stop("mojor_gpu_matmul: out must be mojor_gpu_array")
        }
        handle_out <- .mojor_gpu_array_handle(out)
        if (is.null(handle_out)) {
            stop("mojor_gpu_matmul: missing output handle")
        }
        if (!identical(
            .mojor_gpu_array_dtype(out),
            dtype
        )) {
            stop("mojor_gpu_matmul: output dtype mismatch")
        }
        if (!is.null(out$dim)) {
            if (length(out$dim) !=
                2L || !identical(
                as.integer(out$dim),
                out_dim
            )) {
                stop("mojor_gpu_matmul: output dimensions mismatch")
            }
        } else {
            if (!is.null(out$n) &&
                as.integer(out$n) !=
                  as.integer(m * n)) {
                stop("mojor_gpu_matmul: output length mismatch")
            }
            out$dim <- out_dim
            out$dimnames <- NULL
            out$strides <- .mojor_dim_strides(out_dim)
        }
        if (!isTRUE(f64_matmul_gpu)) {
            mojor_gpu_array_write(out, cpu_matmul_host())
            out <- .mojor_gpu_route_tag(
                out,
                "cpu_matmul",
                reason = f64_cpu_reason,
                reason_code = f64_cpu_reason_code
            )
            return(out)
        }
        status <- tryCatch(
            .mojor_call_bridge(
                matmul_into_bridge, .mojor_gpu_ctx_get(), handle_out, handle_x,
                handle_y, as.integer(m),
                as.integer(k),
                as.integer(n),
                as.integer(transpose_a),
                as.integer(transpose_b)
            ),
            error = function(e) FALSE
        )
        if (!isTRUE(status)) {
            mojor_gpu_array_write(out, cpu_matmul_host())
            out <- .mojor_gpu_route_tag(out, "cpu_matmul", reason = "gpu matmul_into kernel call failed")
            return(out)
        }
        attr(out, "gpu_status") <- attr(handle_out, "gpu_status")
        out <- .mojor_gpu_route_tag(out, "gpu_matmul")
        return(out)
    }

    if (!isTRUE(f64_matmul_gpu)) {
        out <- GPUArray(cpu_matmul_host(), dtype = dtype)
        out <- .mojor_gpu_route_tag(
            out,
            "cpu_matmul",
            reason = f64_cpu_reason,
            reason_code = f64_cpu_reason_code
        )
        return(out)
    }

    out_handle <- tryCatch(
        .mojor_call_bridge(
            matmul_bridge, .mojor_gpu_ctx_get(), handle_x, handle_y, as.integer(m),
            as.integer(k),
            as.integer(n),
            as.integer(transpose_a),
            as.integer(transpose_b)
        ),
        error = function(e) NULL
    )
    if (!is.null(out_handle)) {
        ok <- tryCatch(
            {
                .mojor_gpu_require(matmul_require, out_handle)
                TRUE
            }, error = function(e) FALSE
        )
        if (isTRUE(ok)) {
            class(out_handle) <- c(out_info$class, class(out_handle))
            out <- .mojor_gpu_wrap_buffer_result(
                out_handle, n = as.integer(m * n),
                api = api, dtype = dtype, include_shape = TRUE, dim = out_dim,
                dimnames = NULL, strides = .mojor_dim_strides(out_dim),
                class = "GPUArray"
            )
            out <- .mojor_gpu_route_tag(out, "gpu_matmul")
            return(out)
        }
    }

    out <- GPUArray(cpu_matmul_host(), dtype = dtype)
    out <- .mojor_gpu_route_tag(out, "cpu_matmul", reason = "gpu matmul kernel call failed")
    out
}

gpu_matmul <- function(x, y, transpose_a = FALSE, transpose_b = FALSE, out = NULL) {
    tmp_arrays <- list()
    api <- if (inherits(out, "mojor_gpu_array")) {
        .mojor_gpu_array_api(out)
    } else {
        .mojor_gpu_pick_api(x, y)
    }

    if (!inherits(x, "mojor_gpu_array")) {
        if (!is.numeric(x)) {
            stop("gpu_matmul: x must be numeric/integer or mojor_gpu_array")
        }
        x <- mojor_gpu_array(
            x,
            api = api,
            dtype = .mojor_gpu_value_dtype(x)
        )
        tmp_arrays <- c(tmp_arrays, list(x))
    }
    if (!inherits(y, "mojor_gpu_array")) {
        if (!is.numeric(y)) {
            stop("gpu_matmul: y must be numeric/integer or mojor_gpu_array")
        }
        y <- mojor_gpu_array(
            y,
            api = api,
            dtype = .mojor_gpu_value_dtype(y)
        )
        if (!any(vapply(tmp_arrays, identical, logical(1), y))) {
            tmp_arrays <- c(tmp_arrays, list(y))
        }
    }

    if (length(tmp_arrays) > 0L) {
        on.exit(
            .mojor_gpu_free_temp_arrays(tmp_arrays),
            add = TRUE
        )
    }

    .mojor_gpu_matmul(x, y, transpose_a, transpose_b, out)
}

gpu_matmul_into <- function(out, x, y, transpose_a = FALSE, transpose_b = FALSE) {
    gpu_matmul(
        x, y,
        transpose_a = transpose_a,
        transpose_b = transpose_b,
        out = out
    )
}
