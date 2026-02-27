# Shared helpers extracted from test_gpu_array.R for split test files.
# Keep helper- prefix so testthat loads this for file-level and dir-level runs.

.mojor_gpu_truthy_env <- function(name) {
  val <- tolower(Sys.getenv(name, unset = ""))
  val %in% c("1", "true", "yes", "y", "on")
}

.skip_if_no_gpu_backend <- function(require_float = FALSE, require_mojo = TRUE) {
  if (!.mojor_gpu_truthy_env("MOJOR_TEST_RUN_GPU_BACKEND_TESTS")) {
    skip("set MOJOR_TEST_RUN_GPU_BACKEND_TESTS=1 to run GPU backend runtime tests")
  }
  if (isTRUE(require_mojo)) {
    skip_if_no_mojo()
  }
  if (isTRUE(require_float) && !requireNamespace("float", quietly = TRUE)) {
    skip("float package not available")
  }
  if (!mojor_is_loaded()) {
    skip("Mojo backend not loaded")
  }
  if (!mojor_has_gpu()) {
    skip("GPU not available")
  }
  if (!exists(".mojor_test_gpu_ctx_probe_ok", envir = .GlobalEnv, inherits = FALSE)) {
    assign(".mojor_test_gpu_ctx_probe_ok", NULL, envir = .GlobalEnv)
  }
  cached_ctx_ok <- get(".mojor_test_gpu_ctx_probe_ok", envir = .GlobalEnv, inherits = FALSE)
  cur_epoch <- tryCatch(as.integer(.mojor_gpu_ctx_epoch_get()), error = function(e) NA_integer_)
  if (is.list(cached_ctx_ok) &&
      isTRUE(cached_ctx_ok$ok) &&
      isTRUE(identical(cached_ctx_ok$pid, Sys.getpid())) &&
      !is.na(cur_epoch) &&
      isTRUE(identical(as.integer(cached_ctx_ok$epoch), cur_epoch))) {
    return(invisible(NULL))
  }
  .probe_ctx <- function() {
    tryCatch(
      {
        probe <- mojor_gpu_array(0, dtype = "f32")
        try(mojor_gpu_array_free(probe), silent = TRUE)
        TRUE
      },
      error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("invalid context|gpu context unavailable|probe requires GPU context", msg, ignore.case = TRUE)) {
          FALSE
        } else {
          stop(e)
        }
      }
    )
  }
  .ctx_invalidate <- function() {
    old_epoch <- .mojor_state$gpu_ctx_epoch
    if (is.null(old_epoch) || length(old_epoch) != 1L || is.na(old_epoch)) {
      old_epoch <- 0L
    }
    .mojor_state$gpu_ctx <- NULL
    .mojor_state$gpu_capability_cache <- list()
    .mojor_state$gpu_ctx_epoch <- as.integer(as.integer(old_epoch) + 1L)
    invisible(NULL)
  }
  ctx_ok <- .probe_ctx()
  if (!isTRUE(ctx_ok)) {
    try(mojor_gpu_ctx_reset(mode = "hard"), silent = TRUE)
    ctx_ok <- .probe_ctx()
  }
  if (!isTRUE(ctx_ok)) {
    .ctx_invalidate()
    try(mojor_load(), silent = TRUE)
    ctx_ok <- .probe_ctx()
  }
  if (!isTRUE(ctx_ok)) {
    .ctx_invalidate()
    mojor_lib <- Sys.getenv("MOJOR_LIB_PATH", unset = "")
    if (nzchar(mojor_lib)) {
      try(mojor_load(path = mojor_lib), silent = TRUE)
    } else {
      try(mojor_load(), silent = TRUE)
    }
    ctx_ok <- .probe_ctx()
  }
  if (isTRUE(ctx_ok)) {
    assign(
      ".mojor_test_gpu_ctx_probe_ok",
      list(
        ok = TRUE,
        pid = Sys.getpid(),
        epoch = tryCatch(as.integer(.mojor_gpu_ctx_epoch_get()), error = function(e) NA_integer_)
      ),
      envir = .GlobalEnv
    )
  }
  if (!isTRUE(ctx_ok)) {
    if (exists(".mojor_test_gpu_ctx_probe_ok", envir = .GlobalEnv, inherits = FALSE)) {
      rm(list = ".mojor_test_gpu_ctx_probe_ok", envir = .GlobalEnv)
    }
    skip("GPU context invalid in this runtime")
  }
}
.mojor_gpu_array_free_all <- function(...) {
  bufs <- list(...)
  for (buf in bufs) {
    if (is.null(buf)) {
      next
    }
    try(mojor_gpu_array_free(buf), silent = TRUE)
  }
  invisible(NULL)
}
.mojor_gpu_src_matrix <- function() {
  matrix(
    as.numeric(1:12),
    nrow = 3L,
    ncol = 4L,
    dimnames = list(paste0("r", 1:3), paste0("c", 1:4))
  )
}
.mojor_expect_gpu_like_meta <- function(buf, ref, expect_dtype = NULL, expect_api = NULL) {
  if (!is.null(expect_dtype)) {
    expect_equal(.mojor_gpu_array_dtype(buf), expect_dtype)
  }
  if (!is.null(expect_api)) {
    expect_equal(.mojor_gpu_array_api(buf), expect_api)
  }
  expect_equal(dim(buf), dim(ref))
  expect_equal(dimnames(buf), dimnames(ref))
}
.mojor_expect_call_errors <- function(cases) {
  for (case in cases) {
    expect_error(do.call(case$fn, case$args), case$pattern)
  }
}
.mojor_build_or_skip_expr_subset <- function(expr) {
  built <- tryCatch(force(expr), error = function(e) e)
  if (inherits(built, "error")) {
    if (grepl("unsupported expression", conditionMessage(built), fixed = TRUE)) {
      skip("expression subset unsupported in this runtime")
    }
    stop(built)
  }
  built
}
.mojor_gpu_write_cases <- function() {
  list(
    list(
      name = "shape_change",
      vals = matrix(
        as.numeric(101:120),
        nrow = 2L,
        ncol = 10L,
        dimnames = list(c("r1", "r2"), paste0("c", seq_len(10L)))
      ),
      expect_dim = as.integer(c(2L, 10L)),
      expect_dimnames = function(m, vals) dimnames(vals),
      expect_host = function(m, vals) vals,
      clear_plans = TRUE
    ),
    list(
      name = "vector_preserve",
      vals = as.numeric(201:220),
      expect_dim = as.integer(c(4L, 5L)),
      expect_dimnames = function(m, vals) dimnames(m),
      expect_host = function(m, vals) matrix(vals, nrow = 4L, ncol = 5L, dimnames = dimnames(m)),
      clear_plans = FALSE
    ),
    list(
      name = "same_shape_matrix",
      vals = matrix(
        as.numeric(301:320),
        nrow = 4L,
        ncol = 5L,
        dimnames = list(paste0("rr", 1:4), paste0("cc", 1:5))
      ),
      expect_dim = as.integer(c(4L, 5L)),
      expect_dimnames = function(m, vals) dimnames(vals),
      expect_host = function(m, vals) vals,
      clear_plans = FALSE
    )
  )
}
.mojor_run_gpu_array_write_cases <- function(dtype = c("f32", "f64"), track_f32_count = FALSE) {
  dtype <- match.arg(dtype)
  m <- matrix(as.numeric(1:20), nrow = 4L, ncol = 5L, dimnames = list(paste0("r", 1:4), paste0("c", 1:5)))
  for (case in .mojor_gpu_write_cases()) {
    count_before <- if (isTRUE(track_f32_count)) mojor_gpu_buf_f32_live_count() else NULL
    gx <- GPUArray(m, dtype = dtype)
    if (isTRUE(track_f32_count)) {
      expect_equal(mojor_gpu_buf_f32_live_count(), count_before + 1L, info = paste(dtype, case$name))
    }

    plans <- new.env(parent = emptyenv())
    assign("sentinel", list(plan = NULL, marker = TRUE), envir = plans)
    gx <- .mojor_gpu_object_set(gx, "index_plans", plans)
    mojor_gpu_array_write(gx, case$vals)

    expect_equal(.mojor_gpu_array_dim(gx), case$expect_dim, info = paste(dtype, case$name))
    expect_equal(.mojor_gpu_array_dimnames(gx), case$expect_dimnames(m, case$vals), info = paste(dtype, case$name))
    if (isTRUE(case$clear_plans)) {
      expect_equal(
        .mojor_gpu_object_get(gx, "strides", default = NULL),
        .mojor_dim_strides(case$expect_dim),
        info = paste(dtype, case$name)
      )
      plans_after <- .mojor_gpu_object_get(gx, "index_plans", default = NULL)
      expect_true(is.environment(plans_after), info = paste(dtype, case$name))
      expect_equal(length(ls(plans_after, all.names = TRUE)), 0L, info = paste(dtype, case$name))
    } else {
      expect_true(identical(.mojor_gpu_object_get(gx, "index_plans", default = NULL), plans), info = paste(dtype, case$name))
      expect_true(exists("sentinel", envir = plans, inherits = FALSE), info = paste(dtype, case$name))
    }

    host <- mojor_gpu_array_read(gx)
    expect_equal(host, case$expect_host(m, case$vals), info = paste(dtype, case$name))
    expect_equal(dimnames(host), case$expect_dimnames(m, case$vals), info = paste(dtype, case$name))

    if (isTRUE(track_f32_count)) {
      expect_equal(mojor_gpu_buf_f32_live_count(), count_before + 1L, info = paste(dtype, case$name))
    }
    mojor_gpu_array_free(gx)
    if (isTRUE(track_f32_count)) {
      expect_equal(mojor_gpu_buf_f32_live_count(), count_before, info = paste(dtype, case$name))
    }
  }
}
