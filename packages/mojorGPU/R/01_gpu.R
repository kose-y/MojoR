.mojor_mojo_supports_flag <- function(flag) {
  cache <- .mojor_state$mojo_flag_cache
  if (!is.null(cache[[flag]])) return(isTRUE(cache[[flag]]))
  out <- tryCatch(system("mojo build --help-hidden", intern = TRUE), error = function(e) character(0))
  supported <- any(grepl(flag, out, fixed = TRUE))
  cache[[flag]] <- supported
  .mojor_state$mojo_flag_cache <- cache
  supported
}

.mojor_fast_math_flags <- function(fast_math = NULL) {
  use_fast <- if (is.null(fast_math)) isTRUE(.mojor_state$options$fast_math) else isTRUE(fast_math)
  if (!use_fast) return(character(0))
  flag <- .mojor_state$options$fast_math_flag
  if (!is.null(flag) && nzchar(flag)) return(flag)
  if (.mojor_mojo_supports_flag("--fast-math")) return("--fast-math")
  if (!isTRUE(.mojor_state$fast_math_warned)) {
    warning("mojor: fast_math requested but no known mojo flag; set fast_math_flag to override", call. = FALSE)
    .mojor_state$fast_math_warned <- TRUE
  }
  character(0)
}

if (!methods::isClass("mojor_gpu_array")) {
  methods::setClass("mojor_gpu_array", contains = "list")
}
if (!methods::isClass("mojor_gpu_session")) {
  methods::setClass("mojor_gpu_session", contains = "list")
}

.mojor_gpu_new_object <- function(payload, class = "mojor_gpu_array") {
  class <- as.character(class)
  primary <- class[[1]]
  if (methods::isClass(primary)) {
    return(methods::new(primary, .Data = payload))
  }
  structure(payload, class = class)
}

mojor_gpu_check_release <- function(fn, ...) {
  if (!is.function(fn)) stop("mojor_gpu_check_release: fn must be a function")
  if (!mojor_is_loaded()) {
    out1 <- fn(...)
    out2 <- fn(...)
    return(invisible(list(first = out1, second = out2)))
  }
  before <- mojor_gpu_buf_f32_live_count()
  out1 <- fn(...)
  after1 <- mojor_gpu_buf_f32_live_count()
  if (!identical(after1, before)) {
    stop("mojor_gpu_check_release: live GPU buffer count changed after first call", call. = FALSE)
  }
  out2 <- fn(...)
  after2 <- mojor_gpu_buf_f32_live_count()
  if (!identical(after2, before)) {
    stop("mojor_gpu_check_release: live GPU buffer count changed after second call", call. = FALSE)
  }
  invisible(list(first = out1, second = out2))
}

.mojor_gpu_buf_info <- function(dtype) {
  dtype <- match.arg(dtype, c("f32", "f64"))
  if (dtype == "f32") {
    return(list(
      dtype = "f32",
      class = "mojor_gpu_buf_f32",
      label = "mojor_gpu_buf_f32",
      bytes_per = 4L,
      alloc = "mojor_gpu_buf_f32_alloc",
      free = "mojor_gpu_buf_f32_free",
      read = "mojor_gpu_buf_f32_read",
      write = "mojor_gpu_buf_f32_write",
      len = "mojor_gpu_buf_f32_len"
    ))
  }
  list(
    dtype = "f64",
    class = "mojor_gpu_buf_f64",
    label = "mojor_gpu_buf_f64",
    bytes_per = 8L,
    alloc = "mojor_gpu_buf_f64_alloc",
    free = "mojor_gpu_buf_f64_free",
    read = "mojor_gpu_buf_f64_read",
    write = "mojor_gpu_buf_f64_write",
    len = "mojor_gpu_buf_f64_len"
  )
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
    x <- as.numeric(x)
    n <- length(x)
  } else if (is.null(n)) {
    stop(info$label, ": provide x or n")
  }
  n <- as.integer(n)
  .mojor_gpu_check_limit(paste0(info$label, "_alloc"), n, buffers = 1L, bytes_per = info$bytes_per)
  buf <- .mojor_call(info$alloc, .mojor_gpu_ctx_get(), n)
  class(buf) <- c(info$class, class(buf))
  if (!is.null(x) && n > 0L) {
    .mojor_gpu_buf_write(info, buf, x)
  }
  buf
}

.mojor_gpu_buf_write <- function(info, buf, values) {
  if (!inherits(buf, info$class)) {
    stop(info$label, "_write: buf must be ", info$class)
  }
  values <- as.numeric(values)
  .mojor_call(info$write, buf, values)
  invisible(buf)
}

.mojor_gpu_buf_read <- function(info, buf) {
  if (!inherits(buf, info$class)) {
    stop(info$label, "_read: buf must be ", info$class)
  }
  .mojor_call(info$read, buf)
}

.mojor_gpu_buf_free <- function(info, buf) {
  if (!inherits(buf, info$class)) {
    stop(info$label, "_free: buf must be ", info$class)
  }
  .mojor_call(info$free, buf)
  invisible(TRUE)
}

.mojor_gpu_buf_print <- function(info, x) {
  n <- attr(x, "n")
  if (is.null(n)) n <- .mojor_call(info$len, x)
  cat(info$label, "<", n, ">\n", sep = "")
  invisible(x)
}

mojor_gpu_buf_f32 <- function(x = NULL, n = NULL, api = c("auto", "metal", "cuda", "amd")) {
  .mojor_gpu_buf_new("f32", x = x, n = n, api = api)
}

mojor_gpu_buf_f32_write <- function(buf, values) {
  .mojor_gpu_buf_write(.mojor_gpu_buf_info("f32"), buf, values)
}

mojor_gpu_buf_f32_read <- function(buf) {
  .mojor_gpu_buf_read(.mojor_gpu_buf_info("f32"), buf)
}

mojor_gpu_buf_f32_free <- function(buf) {
  .mojor_gpu_buf_free(.mojor_gpu_buf_info("f32"), buf)
}

mojor_gpu_buf_f32_live_count <- function() {
  .mojor_call("mojor_gpu_buf_f32_live_count")
}

print.mojor_gpu_buf_f32 <- function(x, ...) {
  .mojor_gpu_buf_print(.mojor_gpu_buf_info("f32"), x)
}

as.numeric.mojor_gpu_buf_f32 <- function(x, ...) {
  mojor_gpu_buf_f32_read(x)
}

mojor_gpu_buf_f64 <- function(x = NULL, n = NULL, api = c("auto", "metal", "cuda", "amd")) {
  .mojor_gpu_buf_new("f64", x = x, n = n, api = api)
}

mojor_gpu_buf_f64_write <- function(buf, values) {
  .mojor_gpu_buf_write(.mojor_gpu_buf_info("f64"), buf, values)
}

mojor_gpu_buf_f64_read <- function(buf) {
  .mojor_gpu_buf_read(.mojor_gpu_buf_info("f64"), buf)
}

mojor_gpu_buf_f64_free <- function(buf) {
  .mojor_gpu_buf_free(.mojor_gpu_buf_info("f64"), buf)
}

print.mojor_gpu_buf_f64 <- function(x, ...) {
  .mojor_gpu_buf_print(.mojor_gpu_buf_info("f64"), x)
}

as.numeric.mojor_gpu_buf_f64 <- function(x, ...) {
  mojor_gpu_buf_f64_read(x)
}

.mojor_gpu_api_override <- function() {
  opt <- getOption("mojor_gpu_api", NA_character_)
  if (!is.na(opt) && nzchar(opt)) return(tolower(as.character(opt)))
  env <- Sys.getenv("MOJOR_GPU_API", "")
  if (nzchar(env)) return(tolower(env))
  ""
}

.mojor_gpu_api_resolve <- function(api) {
  api <- match.arg(api, c("auto", "metal", "cuda", "amd"))
  override <- .mojor_gpu_api_override()
  if (nzchar(override)) {
    if (override %in% c("auto", "metal", "cuda", "amd")) {
      api <- override
    } else {
      warning("mojor: ignoring invalid MOJOR_GPU_API value: ", override, call. = FALSE)
    }
  }
  if (identical(api, "auto")) {
    api <- "metal"
  }
  if (!identical(api, "metal") && !nzchar(override)) {
    warning(
      "mojor: API '", api, "' requested; ensure the backend was built with ",
      "MOJOR_GPU_API=", api,
      call. = FALSE
    )
  }
  api
}

.mojor_gpu_array_handle <- function(buf) {
  if (inherits(buf, "mojor_gpu_buf_f32") || inherits(buf, "mojor_gpu_buf_f64")) return(buf)
  if (is.list(buf) && !is.null(buf$handle)) return(buf$handle)
  NULL
}

.mojor_gpu_array_api <- function(buf) {
  if (is.list(buf) && !is.null(buf$api)) return(as.character(buf$api))
  "metal"
}

.mojor_dim_strides <- function(dim) {
  if (is.null(dim) || length(dim) == 0) return(NULL)
  dim <- as.integer(dim)
  strides <- integer(length(dim))
  strides[1] <- 1L
  if (length(dim) > 1L) {
    for (i in 2:length(dim)) {
      strides[i] <- strides[i - 1L] * dim[i - 1L]
    }
  }
  strides
}

.mojor_gpu_dtype_from_input <- function(x, dtype = c("auto", "f32", "f64")) {
  dtype <- match.arg(dtype)
  if (dtype != "auto") return(dtype)
  if (!is.null(x) && .mojor_float_available() && float::is.float(x)) {
    return("f32")
  }
  "f64"
}

.mojor_gpu_array_dtype <- function(buf) {
  if (is.list(buf) && !is.null(buf$dtype)) return(buf$dtype)
  handle <- .mojor_gpu_array_handle(buf)
  if (inherits(handle, "mojor_gpu_buf_f64")) return("f64")
  "f32"
}

mojor_gpu_array <- function(x = NULL, n = NULL, api = c("auto", "metal", "cuda", "amd"), dtype = c("auto", "f32", "f64")) {
  api <- .mojor_gpu_api_resolve(api)
  dtype <- .mojor_gpu_dtype_from_input(x, dtype)
  dims <- NULL
  dimn <- NULL
  strides <- NULL
  if (!is.null(x)) {
    dims <- dim(x)
    dimn <- dimnames(x)
    if (!is.null(dims)) {
      strides <- .mojor_dim_strides(dims)
    }
    x <- as.numeric(x)
    n <- length(x)
  } else if (is.null(n)) {
    stop("mojor_gpu_array: provide x or n")
  } else {
    n <- as.integer(n)
  }
  handle <- if (identical(dtype, "f64")) {
    mojor_gpu_buf_f64(x = x, n = n)
  } else {
    mojor_gpu_buf_f32(x = x, n = n)
  }
  .mojor_gpu_new_object(
    list(handle = handle, data = NULL, n = n, api = api, dtype = dtype, dim = dims, dimnames = dimn, strides = strides),
    class = "mojor_gpu_array"
  )
}

.mojor_gpu_array_length <- function(x) {
  if (is.list(x) && !is.null(x$n)) return(as.integer(x$n))
  handle <- .mojor_gpu_array_handle(x)
  if (is.null(handle)) return(0L)
  if (.mojor_gpu_array_dtype(x) == "f64") {
    return(as.integer(.mojor_call("mojor_gpu_buf_f64_len", handle)))
  }
  as.integer(.mojor_call("mojor_gpu_buf_f32_len", handle))
}

.mojor_gpu_array_dim <- function(x) {
  if (is.list(x)) return(x$dim)
  NULL
}

.mojor_gpu_array_dimnames <- function(x) {
  if (is.list(x)) return(x$dimnames)
  NULL
}

length.mojor_gpu_array <- function(x) .mojor_gpu_array_length(x)

dim.mojor_gpu_array <- function(x) .mojor_gpu_array_dim(x)

dimnames.mojor_gpu_array <- function(x) .mojor_gpu_array_dimnames(x)

mojor_gpu_array_free <- function(buf) {
  if (!inherits(buf, "mojor_gpu_array")) {
    stop("mojor_gpu_array_free: buf must be mojor_gpu_array")
  }
  handle <- .mojor_gpu_array_handle(buf)
  if (!is.null(handle)) {
    if (inherits(handle, "mojor_gpu_buf_f64")) {
      mojor_gpu_buf_f64_free(handle)
    } else {
      mojor_gpu_buf_f32_free(handle)
    }
  }
  if (is.list(buf)) {
    buf$handle <- NULL
    buf$n <- 0L
    buf$dtype <- NULL
    buf$dim <- NULL
    buf$dimnames <- NULL
    buf$strides <- NULL
  }
  buf
}

mojor_gpu_array_write <- function(buf, values) {
  if (!inherits(buf, "mojor_gpu_array")) {
    stop("mojor_gpu_array_write: buf must be mojor_gpu_array")
  }
  values <- as.numeric(values)
  if (is.list(buf) && !is.null(buf$n) && length(values) != buf$n) {
    stop("mojor_gpu_array_write: length mismatch")
  }
  if (is.list(buf) && !is.null(dim(values))) {
    buf$dim <- dim(values)
    buf$dimnames <- dimnames(values)
    buf$strides <- .mojor_dim_strides(buf$dim)
  }
  handle <- .mojor_gpu_array_handle(buf)
  if (is.null(handle)) stop("mojor_gpu_array_write: missing handle")
  if (.mojor_gpu_array_dtype(buf) == "f64") {
    mojor_gpu_buf_f64_write(handle, values)
  } else {
    mojor_gpu_buf_f32_write(handle, values)
  }
  invisible(buf)
}

mojor_gpu_array_read <- function(buf) {
  if (!inherits(buf, "mojor_gpu_array")) {
    stop("mojor_gpu_array_read: buf must be mojor_gpu_array")
  }
  handle <- .mojor_gpu_array_handle(buf)
  if (is.null(handle)) stop("mojor_gpu_array_read: missing handle")
  out <- if (.mojor_gpu_array_dtype(buf) == "f64") {
    mojor_gpu_buf_f64_read(handle)
  } else {
    mojor_gpu_buf_f32_read(handle)
  }
  if (is.list(buf) && !is.null(buf$dim)) {
    dim(out) <- buf$dim
    if (!is.null(buf$dimnames)) {
      dimnames(out) <- buf$dimnames
    }
  }
  out
}

print.mojor_gpu_array <- function(x, ...) {
  n <- x$n
  api <- .mojor_gpu_array_api(x)
  dtype <- .mojor_gpu_array_dtype(x)
  dim_str <- ""
  if (is.list(x) && !is.null(x$dim)) {
    dim_str <- paste0(" dim=", paste(x$dim, collapse = "x"))
  }
  cat("mojor_gpu_array<", n, "> [", api, ", ", dtype, "]", dim_str, "\n", sep = "")
  invisible(x)
}

as.numeric.mojor_gpu_array <- function(x, ...) {
  mojor_gpu_array_read(x)
}

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
    buf <- mojor_gpu_array(float::dbl(out), api = api)
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
  buf <- mojor_gpu_array(float::dbl(out), api = api)
  buf$data <- out
  buf
}

mojor_gpu_linear <- function(x, scale = 1, bias = 0, api = c("auto", "metal", "cuda", "amd")) {
  if (inherits(x, "mojor_gpu_array")) {
    api <- .mojor_gpu_array_api(x)
    dtype <- .mojor_gpu_array_dtype(x)
    bytes_per <- if (identical(dtype, "f64")) 8L else 4L
    .mojor_gpu_check_limit("gpu_buf_affine", x$n, buffers = 2L, bytes_per = bytes_per)
    handle <- .mojor_gpu_array_handle(x)
    if (identical(dtype, "f64")) {
      out_handle <- .mojor_call(
        "mojor_gpu_buf_f64_affine",
        .mojor_gpu_ctx_get(),
        handle,
        as.numeric(scale),
        as.numeric(bias)
      )
      .mojor_gpu_require("gpu_buf_f64_affine", out_handle)
      class(out_handle) <- c("mojor_gpu_buf_f64", class(out_handle))
    } else {
      out_handle <- .mojor_call(
        "mojor_gpu_buf_f32_affine",
        .mojor_gpu_ctx_get(),
        handle,
        as.numeric(scale),
        as.numeric(bias)
      )
      .mojor_gpu_require("gpu_buf_f32_affine", out_handle)
      class(out_handle) <- c("mojor_gpu_buf_f32", class(out_handle))
    }
    out <- .mojor_gpu_new_object(
      list(handle = out_handle, data = NULL, n = x$n, api = api, dtype = dtype),
      class = "mojor_gpu_array"
    )
    attr(out, "gpu_status") <- attr(out_handle, "gpu_status")
    return(out)
  }
  api <- .mojor_gpu_api_resolve(api)
  buf <- mojor_gpu_array(x, api = api)
  out <- mojor_gpu_linear(buf, scale = scale, bias = bias, api = api)
  mojor_gpu_array_free(buf)
  out
}

mojor_gpu_chain_array <- function(
  x,
  iters = 5L,
  scale = 1,
  bias = 0,
  post_scale = 1,
  post_bias = 0,
  post_iters = 0L,
  api = c("auto", "metal", "cuda", "amd")
) {
  if (inherits(x, "mojor_gpu_array")) {
    api <- .mojor_gpu_array_api(x)
    dtype <- .mojor_gpu_array_dtype(x)
    bytes_per <- if (identical(dtype, "f64")) 8L else 4L
    .mojor_gpu_check_limit("gpu_buf_chain", x$n, buffers = 2L, bytes_per = bytes_per)
    handle <- .mojor_gpu_array_handle(x)
    if (identical(dtype, "f64")) {
      out_handle <- .mojor_call(
        "mojor_gpu_buf_f64_chain",
        .mojor_gpu_ctx_get(),
        handle,
        as.integer(iters),
        as.numeric(scale),
        as.numeric(bias),
        as.numeric(post_scale),
        as.numeric(post_bias),
        as.integer(post_iters)
      )
      .mojor_gpu_require("gpu_buf_f64_chain", out_handle)
      class(out_handle) <- c("mojor_gpu_buf_f64", class(out_handle))
    } else {
      out_handle <- .mojor_call(
        "mojor_gpu_buf_f32_chain",
        .mojor_gpu_ctx_get(),
        handle,
        as.integer(iters),
        as.numeric(scale),
        as.numeric(bias),
        as.numeric(post_scale),
        as.numeric(post_bias),
        as.integer(post_iters)
      )
      .mojor_gpu_require("gpu_buf_f32_chain", out_handle)
      class(out_handle) <- c("mojor_gpu_buf_f32", class(out_handle))
    }
    out <- .mojor_gpu_new_object(
      list(handle = out_handle, data = NULL, n = x$n, api = api, dtype = dtype),
      class = "mojor_gpu_array"
    )
    attr(out, "gpu_status") <- attr(out_handle, "gpu_status")
    return(out)
  }
  api <- .mojor_gpu_api_resolve(api)
  x <- .mojor_gpu_input_numeric(x)
  out <- mojor_gpu_chain(
    x,
    iters = iters,
    op = "sigmoid",
    scale = scale,
    bias = bias,
    post_scale = post_scale,
    post_bias = post_bias,
    post_iters = post_iters
  )
  buf <- mojor_gpu_array(float::dbl(out), api = api)
  buf$data <- out
  buf
}

mojor_gpu_sum <- function(x, iters = 5L, scale = 1, bias = 0, post_scale = 1, post_bias = 0, post_iters = 0L) {
  if (inherits(x, "mojor_gpu_array")) {
    handle <- .mojor_gpu_array_handle(x)
    dtype <- .mojor_gpu_array_dtype(x)
    if (identical(dtype, "f64")) {
      out <- .mojor_call(
        "mojor_gpu_buf_f64_chain_sum",
        .mojor_gpu_ctx_get(),
        handle,
        as.integer(iters),
        as.numeric(scale),
        as.numeric(bias),
        as.numeric(post_scale),
        as.numeric(post_bias),
        as.integer(post_iters)
      )
      .mojor_gpu_require("gpu_buf_f64_chain_sum", out)
      return(out)
    }
    out <- .mojor_call(
      "mojor_gpu_buf_f32_chain_sum",
      .mojor_gpu_ctx_get(),
      handle,
      as.integer(iters),
      as.numeric(scale),
      as.numeric(bias),
      as.numeric(post_scale),
      as.numeric(post_bias),
      as.integer(post_iters)
    )
    .mojor_gpu_require("gpu_buf_f32_chain_sum", out)
    return(out)
  }
  x <- .mojor_gpu_input_numeric(x)
  mojor_sigmoid_affine_f32_gpu_chain_sum(
    x,
    iters = iters,
    scale = scale,
    bias = bias,
    post_scale = post_scale,
    post_bias = post_bias,
    post_iters = post_iters
  )
}

mojor_gpu_session <- function(x) {
  x <- .mojor_gpu_input_numeric(x)
  ga <- .mojor_as_float32(x)
  data <- methods::slot(ga, "Data")
  n <- length(data)
  .mojor_gpu_check_limit("gpu_session_create", n, buffers = 4L, bytes_per = 4L)
  status <- .mojor_call("mojor_gpu_session_create", .mojor_gpu_ctx_get(), as.integer(n))
  if (!is.numeric(status) || length(status) != 1 || status <= 0) {
    stop("mojor_gpu_session: GPU session init failed (status=", status, ")", call. = FALSE)
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

.mojor_gpu_register_s4_methods <- function() {
  if (methods::isClass("mojor_gpu_array")) {
    if (is.null(methods::getMethod("show", "mojor_gpu_array", optional = TRUE))) {
      methods::setMethod("show", "mojor_gpu_array", function(object) {
        print.mojor_gpu_array(object)
        invisible(object)
      })
    }
    if (is.null(methods::getMethod("length", "mojor_gpu_array", optional = TRUE))) {
      methods::setMethod("length", "mojor_gpu_array", function(x) .mojor_gpu_array_length(x))
    }
    if (is.null(methods::getMethod("dim", "mojor_gpu_array", optional = TRUE))) {
      methods::setMethod("dim", "mojor_gpu_array", function(x) .mojor_gpu_array_dim(x))
    }
    if (is.null(methods::getMethod("dimnames", "mojor_gpu_array", optional = TRUE))) {
      methods::setMethod("dimnames", "mojor_gpu_array", function(x) .mojor_gpu_array_dimnames(x))
    }
  }

  if (methods::isClass("mojor_gpu_session") && is.null(methods::getMethod("show", "mojor_gpu_session", optional = TRUE))) {
    methods::setMethod("show", "mojor_gpu_session", function(object) {
      print.mojor_gpu_session(object)
      invisible(object)
    })
  }
}

.mojor_gpu_register_s4_methods()

mojor_gpu_session_run <- function(
  session,
  iters = 5L,
  scale = 1,
  bias = 0,
  post_scale = 1,
  post_bias = 0,
  post_iters = 0L
) {
  if (!inherits(session, "mojor_gpu_session")) {
    stop("mojor_gpu_session_run: session must be mojor_gpu_session")
  }
  data <- methods::slot(session$data, "Data")
  out_data <- .mojor_call(
    "mojor_gpu_session_chain_int",
    .mojor_gpu_ctx_get(),
    data,
    as.integer(iters),
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
    list(data = out, n = length(out), status = status),
    class = "mojor_gpu_session"
  )
  attr(sess, "gpu_status") <- status
  sess
}

mojor_gpu_session_sum <- function(
  session,
  iters = 5L,
  scale = 1,
  bias = 0,
  post_scale = 1,
  post_bias = 0,
  post_iters = 0L
) {
  if (!inherits(session, "mojor_gpu_session")) {
    stop("mojor_gpu_session_sum: session must be mojor_gpu_session")
  }
  data <- methods::slot(session$data, "Data")
  out <- .mojor_call(
    "mojor_gpu_session_chain_sum_int",
    .mojor_gpu_ctx_get(),
    data,
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
  .mojor_call("mojor_gpu_session_free", .mojor_gpu_ctx_get())
  session$data <- float::float32(integer(0))
  session$n <- 0L
  session$status <- 0L
  invisible(session)
}
