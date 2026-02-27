.mojor_state <- getFromNamespace(".mojor_state", "mojor")
.mojor_find_upward <- getFromNamespace(".mojor_find_upward", "mojor")
.mojor_float_available <- getFromNamespace(".mojor_float_available", "mojor")
.mojor_as_float32 <- getFromNamespace(".mojor_as_float32", "mojor")

mojor_is_loaded <- function() {
  mojor::mojor_is_loaded()
}

mojor_load <- function(...) {
  mojor::mojor_load(...)
}

.mojor_call <- function(...) {
  .Call(..., PACKAGE = "mojor")
}

mojor_has_gpu <- function() {
  if (!mojor_is_loaded()) return(FALSE)
  isTRUE(.mojor_call("mojor_has_gpu"))
}

.mojor_gpu_ctx_get <- function() {
  if (!mojor_is_loaded()) stop("Mojo backend not loaded")
  ctx <- .mojor_state$gpu_ctx
  if (is.null(ctx) || !inherits(ctx, "mojor_gpu_ctx")) {
    ctx <- .mojor_call("mojor_gpu_ctx_create")
    if (is.null(ctx)) {
      stop("mojor: GPU context unavailable", call. = FALSE)
    }
    class(ctx) <- c("mojor_gpu_ctx", class(ctx))
    .mojor_state$gpu_ctx <- ctx
  }
  ctx
}

mojor_gpu_ctx_free <- function() {
  ctx <- .mojor_state$gpu_ctx
  if (is.null(ctx) || !inherits(ctx, "mojor_gpu_ctx")) {
    return(invisible(FALSE))
  }
  if (mojor_gpu_buf_f32_live_count() > 0L) {
    stop("mojor_gpu_ctx_free: GPU buffers still live", call. = FALSE)
  }
  .mojor_call("mojor_gpu_ctx_free", ctx)
  .mojor_state$gpu_ctx <- NULL
  invisible(TRUE)
}

mojor_gpu_ctx_reset <- function(mode = c("soft", "hard")) {
  mode <- match.arg(mode)
  ctx <- .mojor_state$gpu_ctx
  if (is.null(ctx) || !inherits(ctx, "mojor_gpu_ctx")) {
    return(invisible(FALSE))
  }
  if (mojor_gpu_buf_f32_live_count() > 0L) {
    stop("mojor_gpu_ctx_reset: GPU buffers still live", call. = FALSE)
  }
  if (mode == "soft") {
    .mojor_state$gpu_ctx <- NULL
    warning("mojor_gpu_ctx_reset: soft reset drops cached context without freeing; restart R for a full reset", call. = FALSE)
    return(invisible(TRUE))
  }
  stop("mojor_gpu_ctx_reset: hard reset not supported in-process; restart R for a full reset", call. = FALSE)
}

mojor_gpu_meminfo <- function() {
  if (!mojor_is_loaded()) {
    stop("mojor_gpu_meminfo: Mojo backend not loaded")
  }
  if (!mojor_has_gpu()) {
    stop("mojor_gpu_meminfo: GPU not available")
  }
  raw <- .mojor_call("mojor_gpu_meminfo", .mojor_gpu_ctx_get())
  status <- attr(raw, "status")
  free <- unname(raw[1])
  total <- unname(raw[2])
  if (!is.numeric(status) || length(status) != 1 || status <= 0) {
    free <- NA_real_
    total <- NA_real_
  }
  list(
    free_bytes = free,
    total_bytes = total,
    free_gb = free / 1024^3,
    total_gb = total / 1024^3,
    status = status
  )
}


mojor_gpu_ctx_smoke <- function(script = NULL, strict = FALSE) {
  if (is.null(script) || !nzchar(script)) {
    script <- .mojor_find_upward(file.path("prototype", "tools", "gpu_ctx_smoke.mojo"))
    if (is.null(script)) {
      script <- .mojor_find_upward(file.path("tools", "gpu_ctx_smoke.mojo"))
    }
  }
  if (is.null(script) || !nzchar(script) || !file.exists(script)) {
    stop("mojor_gpu_ctx_smoke: gpu_ctx_smoke.mojo not found", call. = FALSE)
  }
  mojo_bin <- Sys.which("mojo")
  if (!nzchar(mojo_bin)) {
    alt_bin <- "/opt/anaconda3/bin/mojo"
    if (file.exists(alt_bin)) {
      mojo_bin <- alt_bin
    }
  }
  if (!nzchar(mojo_bin)) {
    stop("mojor_gpu_ctx_smoke: mojo binary not found", call. = FALSE)
  }
  out <- tryCatch(system2(mojo_bin, script, stdout = TRUE, stderr = TRUE), error = function(e) {
    stop("mojor_gpu_ctx_smoke: failed to run mojo", call. = FALSE)
  })
  ok <- any(grepl("gpu_ctx_smoke: ok", out, fixed = TRUE))
  if (!ok) {
    msg <- "mojor_gpu_ctx_smoke: failed (see output)"
    if (isTRUE(strict)) {
      stop(msg, call. = FALSE)
    }
    warning(msg, call. = FALSE)
  }
  invisible(list(ok = ok, output = out, script = script))
}



.mojor_gpu_require <- function(label, out_data) {
  gpu_status <- attr(out_data, "gpu_status")
  if (is.numeric(gpu_status) && length(gpu_status) == 1 && gpu_status > 0) {
    return(invisible(TRUE))
  }
  status <- if (length(gpu_status)) as.character(gpu_status) else "unknown"
  stop("mojor: GPU execution failed for ", label, " (status=", status, ")", call. = FALSE)
}

.mojor_gpu_estimate_bytes <- function(n, buffers = 4L, bytes_per = 4L) {
  if (is.null(n) || length(n) != 1L || is.na(n)) return(NA_real_)
  n <- as.double(n)
  buffers <- as.double(buffers)
  bytes_per <- as.double(bytes_per)
  n * buffers * bytes_per
}

.mojor_gpu_max_bytes <- function() {
  env_bytes <- Sys.getenv("MOJOR_GPU_MAX_BYTES", "")
  if (nzchar(env_bytes)) {
    val <- suppressWarnings(as.double(env_bytes))
    if (!is.na(val) && val > 0) return(val)
  }
  env_gb <- Sys.getenv("MOJOR_GPU_MAX_GB", "")
  if (nzchar(env_gb)) {
    val <- suppressWarnings(as.double(env_gb))
    if (!is.na(val) && val > 0) return(val * 1024^3)
  }
  opt <- .mojor_state$options$gpu_max_bytes
  if (!is.null(opt)) {
    val <- suppressWarnings(as.double(opt))
    if (!is.na(val) && val > 0) return(val)
  }
  16 * 1024^3
}

.mojor_gpu_check_limit <- function(label, n, buffers = 4L, bytes_per = 4L) {
  cap <- .mojor_gpu_max_bytes()
  if (is.na(cap) || cap <= 0) return(invisible(TRUE))
  est <- .mojor_gpu_estimate_bytes(n, buffers, bytes_per)
  if (!is.na(est) && est > cap) {
    msg <- sprintf(
      "mojor: %s estimated %.2f MB exceeds gpu_max_bytes (%.2f MB)",
      label,
      est / (1024^2),
      cap / (1024^2)
    )
    msg <- paste0(msg, "; set MOJOR_GPU_MAX_GB or mojor_options(gpu_max_bytes=...)")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.mojor_gpu_log <- function(label, n, buffers = 4L, bytes_per = 4L) {
  .mojor_gpu_check_limit(label, n, buffers = buffers, bytes_per = bytes_per)
  if (!isTRUE(.mojor_state$options$gpu_debug)) return(invisible(NULL))
  est <- .mojor_gpu_estimate_bytes(n, buffers, bytes_per)
  cap <- .mojor_gpu_max_bytes()
  mem_note <- ""
  if (mojor_is_loaded() && mojor_has_gpu()) {
    info <- tryCatch(mojor_gpu_meminfo(), error = function(e) NULL)
    if (is.list(info) && is.numeric(info$status) && info$status > 0) {
      mem_note <- sprintf(
        ", free=%.0f MB total=%.0f MB",
        info$free_bytes / (1024^2),
        info$total_bytes / (1024^2)
      )
    }
  }
  msg <- sprintf(
    "mojor: gpu est for %s: %.2f MB (buffers=%d, bytes=%d, cap=%.0f MB%s)",
    label,
    est / (1024^2),
    as.integer(buffers),
    as.integer(bytes_per),
    cap / (1024^2),
    mem_note
  )
  message(msg)
  invisible(est)
}

.mojor_wrap_float32_gpu <- function(out_data, label = "kernel") {
  .mojor_gpu_require(label, out_data)
  gpu_status <- attr(out_data, "gpu_status")
  out <- float::float32(out_data)
  attr(out, "gpu_status") <- gpu_status
  out
}

mojor_sigmoid_f32_gpu <- function(x) {
  if (!mojor_is_loaded()) stop("Mojo backend not loaded")
  x <- .mojor_as_float32(x)
  data <- methods::slot(x, "Data")
  .mojor_gpu_log("sigmoid_f32_gpu", length(data), buffers = 4L, bytes_per = 4L)
  out_data <- .mojor_call("mojor_sigmoid_f32_gpu_int", .mojor_gpu_ctx_get(), data)
  .mojor_wrap_float32_gpu(out_data, label = "sigmoid_f32_gpu")
}

mojor_sigmoid_f64_gpu <- function(x) {
  if (!mojor_is_loaded()) stop("Mojo backend not loaded")
  if (!is.numeric(x)) {
    stop("mojor_sigmoid_f64_gpu: x must be numeric")
  }
  data <- as.double(x)
  .mojor_gpu_log("sigmoid_f64_gpu", length(data), buffers = 4L, bytes_per = 8L)
  out_data <- .mojor_call("mojor_sigmoid_f64_gpu", .mojor_gpu_ctx_get(), data)
  .mojor_gpu_require("sigmoid_f64_gpu", out_data)
  out_data
}

mojor_sigmoid_f32_gpu_iters <- function(x, iters = 5L) {
  if (!mojor_is_loaded()) stop("Mojo backend not loaded")
  x <- .mojor_as_float32(x)
  data <- methods::slot(x, "Data")
  .mojor_gpu_log("sigmoid_f32_gpu_iters", length(data), buffers = 4L, bytes_per = 4L)
  out_data <- .mojor_call("mojor_sigmoid_f32_gpu_iters_int", .mojor_gpu_ctx_get(), data, as.integer(iters))
  .mojor_wrap_float32_gpu(out_data, label = "sigmoid_f32_gpu_iters")
}

mojor_sigmoid_affine_f32_gpu <- function(x, scale = 1, bias = 0) {
  if (!mojor_is_loaded()) stop("Mojo backend not loaded")
  x <- .mojor_as_float32(x)
  data <- methods::slot(x, "Data")
  .mojor_gpu_log("sigmoid_affine_f32_gpu", length(data), buffers = 4L, bytes_per = 4L)
  out_data <- .mojor_call("mojor_sigmoid_affine_f32_gpu_int", .mojor_gpu_ctx_get(), data, as.numeric(scale), as.numeric(bias))
  out <- .mojor_wrap_float32_gpu(out_data, label = "sigmoid_affine_f32_gpu")
  attr(out, "gpu_scale") <- scale
  attr(out, "gpu_bias") <- bias
  out
}

mojor_sigmoid_affine_f32_gpu_iters <- function(x, iters = 5L, scale = 1, bias = 0) {
  if (!mojor_is_loaded()) stop("Mojo backend not loaded")
  x <- .mojor_as_float32(x)
  data <- methods::slot(x, "Data")
  .mojor_gpu_log("sigmoid_affine_f32_gpu_iters", length(data), buffers = 4L, bytes_per = 4L)
  out_data <- .mojor_call(
    "mojor_sigmoid_affine_f32_gpu_iters_int",
    .mojor_gpu_ctx_get(),
    data,
    as.integer(iters),
    as.numeric(scale),
    as.numeric(bias)
  )
  out <- .mojor_wrap_float32_gpu(out_data, label = "sigmoid_affine_f32_gpu_iters")
  attr(out, "gpu_scale") <- scale
  attr(out, "gpu_bias") <- bias
  attr(out, "gpu_iters") <- as.integer(iters)
  out
}

mojor_sigmoid_affine_f32_gpu_chain <- function(
  x,
  iters = 5L,
  scale = 1,
  bias = 0,
  post_scale = 1,
  post_bias = 0,
  post_iters = 0L
) {
  if (!mojor_is_loaded()) stop("Mojo backend not loaded")
  x <- .mojor_as_float32(x)
  data <- methods::slot(x, "Data")
  .mojor_gpu_log("sigmoid_affine_f32_gpu_chain", length(data), buffers = 4L, bytes_per = 4L)
  out_data <- .mojor_call(
    "mojor_sigmoid_affine_f32_gpu_chain_int",
    .mojor_gpu_ctx_get(),
    data,
    as.integer(iters),
    as.numeric(scale),
    as.numeric(bias),
    as.numeric(post_scale),
    as.numeric(post_bias),
    as.integer(post_iters)
  )
  out <- .mojor_wrap_float32_gpu(out_data, label = "sigmoid_affine_f32_gpu_chain")
  attr(out, "gpu_scale") <- scale
  attr(out, "gpu_bias") <- bias
  attr(out, "gpu_iters") <- as.integer(iters)
  attr(out, "gpu_post_scale") <- post_scale
  attr(out, "gpu_post_bias") <- post_bias
  attr(out, "gpu_post_iters") <- as.integer(post_iters)
  out
}

mojor_sigmoid_affine_f32_gpu_chain_sum <- function(
  x,
  iters = 5L,
  scale = 1,
  bias = 0,
  post_scale = 1,
  post_bias = 0,
  post_iters = 0L,
  reduction = c("block", "warp")
) {
  if (!mojor_is_loaded()) stop("Mojo backend not loaded")
  reduction <- match.arg(reduction)
  x <- .mojor_as_float32(x)
  data <- methods::slot(x, "Data")
  log_label <- if (reduction == "warp") {
    "sigmoid_affine_f32_gpu_chain_sum_warp"
  } else {
    "sigmoid_affine_f32_gpu_chain_sum"
  }
  .mojor_gpu_log(log_label, length(data), buffers = 5L, bytes_per = 4L)
  out <- if (reduction == "warp") {
    .mojor_call(
      "mojor_sigmoid_affine_f32_gpu_chain_sum_warp_int",
      .mojor_gpu_ctx_get(),
      data,
      as.integer(iters),
      as.numeric(scale),
      as.numeric(bias),
      as.numeric(post_scale),
      as.numeric(post_bias),
      as.integer(post_iters)
    )
  } else {
    .mojor_call(
      "mojor_sigmoid_affine_f32_gpu_chain_sum_int",
      .mojor_gpu_ctx_get(),
      data,
      as.integer(iters),
      as.numeric(scale),
      as.numeric(bias),
      as.numeric(post_scale),
      as.numeric(post_bias),
      as.integer(post_iters)
    )
  }
  .mojor_gpu_require(log_label, out)
  out
}

mojor_gpu_chain <- function(
  x,
  iters = 5L,
  op = "sigmoid",
  scale = 1,
  bias = 0,
  post_scale = 1,
  post_bias = 0,
  post_iters = 0L
) {
  if (!mojor_is_loaded()) stop("Mojo backend not loaded")
  op <- match.arg(op, c("sigmoid"))
  x <- .mojor_as_float32(x)
  data <- methods::slot(x, "Data")
  iters <- as.integer(iters)
  scale <- as.numeric(scale)
  bias <- as.numeric(bias)
  post_scale <- as.numeric(post_scale)
  post_bias <- as.numeric(post_bias)
  post_iters <- as.integer(post_iters)
  use_post <- post_iters > 0L || !isTRUE(all.equal(post_scale, 1)) || !isTRUE(all.equal(post_bias, 0))

  out_data <- NULL
  label <- NULL
  if (op == "sigmoid") {
    if (use_post) {
      .mojor_gpu_log("gpu_chain_sigmoid_affine_chain", length(data), buffers = 4L, bytes_per = 4L)
      out_data <- .mojor_call(
        "mojor_sigmoid_affine_f32_gpu_chain_int",
        .mojor_gpu_ctx_get(),
        data,
        iters,
        scale,
        bias,
        post_scale,
        post_bias,
        post_iters
      )
      label <- "gpu_chain_sigmoid_affine_chain"
    } else if (iters <= 1L && isTRUE(all.equal(scale, 1)) && isTRUE(all.equal(bias, 0))) {
      .mojor_gpu_log("gpu_chain_sigmoid", length(data), buffers = 4L, bytes_per = 4L)
      out_data <- .mojor_call("mojor_sigmoid_f32_gpu_int", .mojor_gpu_ctx_get(), data)
      label <- "gpu_chain_sigmoid"
    } else if (iters <= 1L) {
      .mojor_gpu_log("gpu_chain_sigmoid_affine", length(data), buffers = 4L, bytes_per = 4L)
      out_data <- .mojor_call("mojor_sigmoid_affine_f32_gpu_int", .mojor_gpu_ctx_get(), data, scale, bias)
      label <- "gpu_chain_sigmoid_affine"
    } else {
      .mojor_gpu_log("gpu_chain_sigmoid_affine_iters", length(data), buffers = 4L, bytes_per = 4L)
      out_data <- .mojor_call("mojor_sigmoid_affine_f32_gpu_iters_int", .mojor_gpu_ctx_get(), data, iters, scale, bias)
      label <- "gpu_chain_sigmoid_affine_iters"
    }
  }

  out <- .mojor_wrap_float32_gpu(out_data, label = label)
  attr(out, "gpu_op") <- op
  attr(out, "gpu_scale") <- scale
  attr(out, "gpu_bias") <- bias
  attr(out, "gpu_iters") <- iters
  attr(out, "gpu_post_scale") <- post_scale
  attr(out, "gpu_post_bias") <- post_bias
  attr(out, "gpu_post_iters") <- post_iters
  out
}
