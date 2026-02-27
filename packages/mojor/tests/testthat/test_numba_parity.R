test_that("numba parity v0.1: jit cache and broadcast", {  if (!mojor_is_loaded()) {
    skip("Mojo backend not loaded")
  }
  if (!mojor_has_gpu()) {
    skip("GPU not available")
  }
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + y[i]
    out
  }
  jit <- mojor_jit(f, broadcast = "recycle")
  x <- runif(16)
  y <- 2
  out1 <- jit(x, y)
  out2 <- jit(x, y)
  expect_equal(out1, x + y)
  expect_equal(out2, x + y)
})

test_that("numba parity v0.1: jit disk cache reuses wrapper", {  if (!mojor_is_loaded()) {
    skip("Mojo backend not loaded")
  }
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + y[i]
    out
  }
  cache_dir <- file.path(tempdir(), paste0("mojor_jit_cache_", Sys.getpid(), "_", as.integer(Sys.time())))
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  jit1 <- mojor_jit(f, broadcast = "recycle", disk_cache = TRUE, cache_dir = cache_dir)
  x <- runif(16)
  y <- 2
  out1 <- jit1(x, y)
  expect_equal(out1, x + y)
  index_path <- file.path(cache_dir, "jit_index.rds")
  expect_true(file.exists(index_path))
  index <- readRDS(index_path)
  expect_true(length(index) > 0)
  entry <- index[[1]]
  expect_true(file.exists(entry$wrapper_so))
  mtime_before <- file.info(entry$wrapper_so)$mtime
  Sys.sleep(1.1)
  jit2 <- mojor_jit(f, broadcast = "recycle", disk_cache = TRUE, cache_dir = cache_dir)
  out2 <- jit2(x, y)
  expect_equal(out2, x + y)
  mtime_after <- file.info(entry$wrapper_so)$mtime
  expect_true(identical(mtime_before, mtime_after))
})

test_that("numba parity v0.1: jit dispatcher introspection exposes signatures", {  skip_if_no_mojo()
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2
    out
  }
  jit <- mojor_jit(f, disk_cache = FALSE)
  x <- runif(12)
  expect_equal(jit(x), f(x), tolerance = 1e-12)
  expect_equal(jit(x), f(x), tolerance = 1e-12)

  info <- mojor_jit_info(jit)
  expect_identical(info$name, "mojor_jit_kernel")
  expect_identical(info$stats$calls_total, 2L)
  expect_identical(info$stats$compile_count, 1L)
  expect_identical(info$stats$cache_hits_memory, 1L)
  expect_length(info$signatures, 1L)
})

test_that("numba parity v0.1: eager signatures remove first-call compile hit", {  skip_if_no_mojo()
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + y[i]
    out
  }
  jit <- mojor_jit(
    f,
    signatures = list(list(x = "f64[]", y = "f64[]")),
    eager = TRUE,
    disk_cache = FALSE
  )
  x <- runif(16)
  y <- runif(16)
  out <- jit(x, y)
  expect_equal(out, f(x, y), tolerance = 1e-12)

  info <- mojor_jit_info(jit)
  expect_identical(info$stats$eager_compiles_attempted, 1L)
  expect_identical(info$stats$eager_compiles_succeeded, 1L)
  expect_identical(info$stats$compile_count, 1L)
  expect_identical(info$stats$cache_hits_memory, 1L)
  expect_identical(info$signatures[[1]]$compiled_via, "eager")
})

test_that("numba parity v0.1: explicit compile and signature listing APIs", {  skip_if_no_mojo()
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + y[i]
    out
  }
  jit <- mojor_jit(
    f,
    signatures = list(
      list(x = "f64[]", y = "f64[]"),
      list(x = "i32[]", y = "i32[]")
    ),
    disk_cache = FALSE
  )

  comp <- mojor_jit_compile(
    jit,
    signatures = "(x: f64[], y: f64[])"
  )
  expect_identical(comp$attempted, 1L)
  expect_identical(comp$succeeded, 1L)
  expect_identical(comp$failed, 0L)

  declared <- mojor_jit_signatures(jit, kind = "declared", format = "key")
  compiled <- mojor_jit_signatures(jit, kind = "compiled", format = "key")
  all_sigs <- mojor_jit_signatures(jit, kind = "all", format = "typed_list")
  expect_length(declared, 2L)
  expect_length(compiled, 1L)
  expect_true(compiled[[1]] %in% declared)
  expect_identical(names(all_sigs), declared)

  info <- mojor_jit_info(jit)
  expect_identical(info$stats$api_compiles_attempted, 1L)
  expect_identical(info$stats$api_compiles_succeeded, 1L)
  expect_identical(info$stats$api_compiles_failed, 0L)
  expect_identical(info$signatures[[1]]$last_event, "api_compiled")
})

test_that("numba parity v0.1: elementwise emits elementwise", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2 + 1
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", elementwise = TRUE, elementwise_cpu = TRUE)
  expect_true(grepl("elementwise\\[", trans$mojo))
})

test_that("numba parity v0.1: reductions vs base R", {  if (!mojor_is_loaded()) {
    skip("Mojo backend not loaded")
  }
  x <- runif(1000)
  expect_equal(mojor_sum_f64(x), sum(x), tolerance = 1e-8)
  expect_equal(mojor_mean_f64(x), mean(x), tolerance = 1e-8)
})

test_that("numba parity v0.1: gpu chain sum", {  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not available")
  }
  if (!mojor_is_loaded()) {
    skip("Mojo backend not loaded")
  }
  if (!mojor_has_gpu()) {
    skip("GPU not available")
  }
  x <- float::fl(runif(1024))
  cpu <- sum(float::dbl(mojor_gpu_chain_array(
    x,
    iters = 3L,
    scale = 1.1,
    bias = 0.05,
    post_scale = 0.9,
    post_bias = -0.02,
    post_iters = 1L
  )$data))
  gpu <- mojor_sigmoid_affine_f32_gpu_chain_sum(
    x,
    iters = 3L,
    scale = 1.1,
    bias = 0.05,
    post_scale = 0.9,
    post_bias = -0.02,
    post_iters = 1L
  )
  expect_true(abs(as.numeric(gpu) - cpu) < 1.0)
})
