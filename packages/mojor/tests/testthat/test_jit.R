test_that("mojor_jit infers and caches signatures", {  if (!mojor_is_loaded()) {
    skip("Mojo backend not loaded")
  }
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + y[i]
    out
  }
  jit <- mojor_jit(f, broadcast = "recycle")
  x <- runif(32)
  y <- 2
  out1 <- jit(x, y)
  out2 <- jit(x, y)
  expect_equal(out1, x + y)
  expect_equal(out2, x + y)
})

test_that("mojor_jit eager signatures compile at creation and hit memory on first call", {  skip_if_no_mojo()
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + y[i]
    out
  }
  sigs <- list(list(x = "f64[]", y = "f64[]"))
  jit <- mojor_jit(f, signatures = sigs, eager = TRUE, disk_cache = FALSE)
  info0 <- mojor_jit_info(jit)
  expect_identical(info0$stats$compile_count, 1L)
  expect_identical(info0$stats$eager_compiles_attempted, 1L)
  expect_identical(info0$stats$eager_compiles_succeeded, 1L)
  expect_identical(info0$stats$calls_total, 0L)
  expect_identical(info0$signatures[[1]]$compiled_via, "eager")

  x <- runif(10)
  y <- runif(10)
  expect_equal(jit(x, y), f(x, y), tolerance = 1e-12)
  info1 <- mojor_jit_info(jit)
  expect_identical(info1$stats$calls_total, 1L)
  expect_identical(info1$stats$cache_hits_memory, 1L)
  expect_identical(info1$signatures[[1]]$last_event, "memory_hit")
})

test_that("mojor_jit eager signatures accept signature strings", {  skip_if_no_mojo()
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2 + y[i]
    out
  }
  jit <- mojor_jit(
    f,
    signatures = c("(x: f64[], y: f64[])", "(x: i32[], y: i32[])"),
    eager = TRUE,
    disk_cache = FALSE
  )
  info <- mojor_jit_info(jit)
  expect_identical(info$stats$compile_count, 2L)
  expect_length(info$signatures, 2L)
  expect_true(all(vapply(info$signatures, function(s) identical(s$compiled_via, "eager"), logical(1))))
})

test_that("mojor_jit eager signatures fail fast with signature context", {  skip_if_no_mojo()
  foo_local <- function(v) v + 1
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- foo_local(x[i])
    out
  }
  expect_error(
    mojor_jit(
      f,
      signatures = list(list(x = "f64[]")),
      eager = TRUE,
      disk_cache = FALSE
    ),
    "eager compile failed for signature"
  )
})

test_that("mojor_jit strict signatures require declared signatures", {  skip_if_no_mojo()
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + 1
    out
  }

  expect_error(
    mojor_jit(f, strict_signatures = TRUE, disk_cache = FALSE),
    "strict_signatures=TRUE requires signatures"
  )
})

test_that("mojor_jit strict signatures reject undeclared runtime signatures with diagnostics", {  skip_if_no_mojo()
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + y[i]
    out
  }
  jit <- mojor_jit(
    f,
    signatures = list(list(x = "f64[]", y = "f64[]")),
    strict_signatures = TRUE,
    disk_cache = FALSE
  )

  xd <- as.double(seq_len(6))
  xi <- as.integer(seq_len(6))
  expect_equal(jit(xd, xd), f(xd, xd), tolerance = 1e-12)

  expect_error(
    jit(xi, xi),
    "strict_signatures rejected runtime signature"
  )

  info <- mojor_jit_info(jit)
  expect_identical(info$stats$compile_count, 1L)
  expect_identical(info$stats$signature_rejects, 1L)
  expect_true(is.list(info$last_error))
  expect_identical(info$last_error$stage, "signature_reject")

  reject_entries <- Filter(function(s) identical(s$last_event, "signature_reject"), info$signatures)
  expect_true(length(reject_entries) >= 1L)
  expect_identical(reject_entries[[1]]$last_error$stage, "signature_reject")
})

test_that("mojor_jit records runtime compile failure diagnostics", {  skip_if_no_mojo()
  foo_local <- function(v) v + 1
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- foo_local(x[i])
    out
  }
  jit <- mojor_jit(f, disk_cache = FALSE)
  x <- as.double(seq_len(4))
  expect_error(jit(x), "unsupported|failed|code generation|strict mode")

  info <- mojor_jit_info(jit)
  expect_identical(info$stats$failed_compiles, 1L)
  expect_true(is.list(info$last_error))
  expect_identical(info$last_error$stage, "runtime_compile")
  expect_true(is.character(info$last_error$message))
  expect_length(info$signatures, 1L)
  sig <- info$signatures[[1]]
  expect_identical(sig$failed_compiles, 1L)
  expect_true(is.list(sig$last_error))
  expect_identical(sig$last_error$stage, "runtime_compile")
})

test_that("mojor_jit default disk cache follows jit_disk_cache option", {  skip_if_no_mojo()
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + 1
    out
  }
  x <- as.double(seq_len(8))

  .mojor_test_local_options(jit_disk_cache = TRUE)
  cache_dir_on <- file.path(
    tempdir(),
    paste0("mojor_jit_default_disk_on_", Sys.getpid(), "_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
  )
  dir.create(cache_dir_on, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir_on, recursive = TRUE, force = TRUE), add = TRUE)

  jit_on <- mojor_jit(f, cache_dir = cache_dir_on)
  expect_equal(jit_on(x), f(x), tolerance = 1e-12)
  expect_true(file.exists(file.path(cache_dir_on, "jit_index.rds")))

  .mojor_test_local_options(jit_disk_cache = FALSE)
  cache_dir_off <- file.path(
    tempdir(),
    paste0("mojor_jit_default_disk_off_", Sys.getpid(), "_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
  )
  dir.create(cache_dir_off, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir_off, recursive = TRUE, force = TRUE), add = TRUE)

  jit_off <- mojor_jit(f, cache_dir = cache_dir_off)
  expect_equal(jit_off(x), f(x), tolerance = 1e-12)
  expect_false(file.exists(file.path(cache_dir_off, "jit_index.rds")))
})

test_that("mojor_jit explicit disk_cache overrides global option", {  skip_if_no_mojo()
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 2
    out
  }
  x <- as.double(seq_len(6))

  .mojor_test_local_options(jit_disk_cache = TRUE)
  cache_dir_forced_off <- file.path(
    tempdir(),
    paste0("mojor_jit_override_off_", Sys.getpid(), "_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
  )
  dir.create(cache_dir_forced_off, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir_forced_off, recursive = TRUE, force = TRUE), add = TRUE)

  jit_forced_off <- mojor_jit(f, disk_cache = FALSE, cache_dir = cache_dir_forced_off)
  expect_equal(jit_forced_off(x), f(x), tolerance = 1e-12)
  expect_false(file.exists(file.path(cache_dir_forced_off, "jit_index.rds")))

  .mojor_test_local_options(jit_disk_cache = FALSE)
  cache_dir_forced_on <- file.path(
    tempdir(),
    paste0("mojor_jit_override_on_", Sys.getpid(), "_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
  )
  dir.create(cache_dir_forced_on, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir_forced_on, recursive = TRUE, force = TRUE), add = TRUE)

  jit_forced_on <- mojor_jit(f, disk_cache = TRUE, cache_dir = cache_dir_forced_on)
  expect_equal(jit_forced_on(x), f(x), tolerance = 1e-12)
  expect_true(file.exists(file.path(cache_dir_forced_on, "jit_index.rds")))
})

test_that("mojor_jit resolves disk_cache at call time when NULL", {  skip_if_no_mojo()
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + 2
    out
  }
  x <- as.double(seq_len(5))
  cache_dir <- file.path(
    tempdir(),
    paste0("mojor_jit_dynamic_option_", Sys.getpid(), "_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  .mojor_test_local_options(jit_disk_cache = FALSE)
  jit <- mojor_jit(f, cache_dir = cache_dir)

  mojor_options(jit_disk_cache = TRUE)
  expect_equal(jit(x), f(x), tolerance = 1e-12)
  expect_true(file.exists(file.path(cache_dir, "jit_index.rds")))
})

test_that("mojor_jit_info reports compile, memory-hit, and disk-hit events", {  skip_if_no_mojo()
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + 3
    out
  }
  x <- as.double(seq_len(7))
  cache_dir <- file.path(
    tempdir(),
    paste0("mojor_jit_info_stats_", Sys.getpid(), "_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  .mojor_test_local_options(jit_disk_cache = TRUE)

  jit1 <- mojor_jit(f, disk_cache = TRUE, cache_dir = cache_dir)
  expect_equal(jit1(x), f(x), tolerance = 1e-12)

  info1 <- mojor_jit_info(jit1)
  expect_identical(info1$name, "mojor_jit_kernel")
  expect_s3_class(info1$created_at, "POSIXct")
  expect_identical(info1$stats$calls_total, 1L)
  expect_identical(info1$stats$cache_misses_compile, 1L)
  expect_identical(info1$stats$compile_count, 1L)
  expect_identical(info1$stats$cache_hits_memory, 0L)
  expect_identical(info1$stats$cache_hits_disk, 0L)
  expect_identical(info1$stats$failed_compiles, 0L)
  expect_identical(info1$stats$eager_compiles_attempted, 0L)
  expect_identical(info1$stats$eager_compiles_succeeded, 0L)
  expect_true(is.null(info1$last_error))
  expect_true(info1$stats$compile_time_total_sec >= 0)
  expect_length(info1$signatures, 1L)
  sig1 <- info1$signatures[[1]]
  expect_identical(sig1$calls, 1L)
  expect_identical(sig1$compiles, 1L)
  expect_identical(sig1$hits_memory, 0L)
  expect_identical(sig1$hits_disk, 0L)
  expect_identical(sig1$failed_compiles, 0L)
  expect_identical(sig1$compiled_via, "runtime")
  expect_true(is.null(sig1$last_error))
  expect_identical(sig1$last_event, "compiled")
  expect_s3_class(sig1$last_used_at, "POSIXct")
  expect_true(sig1$compile_time_sec >= 0)

  expect_equal(jit1(x), f(x), tolerance = 1e-12)
  info2 <- mojor_jit_info(jit1)
  expect_identical(info2$stats$calls_total, 2L)
  expect_identical(info2$stats$cache_hits_memory, 1L)
  expect_identical(info2$stats$compile_count, 1L)
  expect_identical(info2$signatures[[1]]$hits_memory, 1L)
  expect_identical(info2$signatures[[1]]$last_event, "memory_hit")

  jit2 <- mojor_jit(f, disk_cache = TRUE, cache_dir = cache_dir)
  expect_equal(jit2(x), f(x), tolerance = 1e-12)
  info3 <- mojor_jit_info(jit2)
  expect_identical(info3$stats$calls_total, 1L)
  expect_identical(info3$stats$cache_hits_disk, 1L)
  expect_identical(info3$stats$compile_count, 0L)
  expect_identical(info3$signatures[[1]]$hits_disk, 1L)
  expect_identical(info3$signatures[[1]]$last_event, "disk_hit")
})

test_that("mojor_jit_info exposes multiple signatures and validates input", {  skip_if_no_mojo()
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + y[i]
    out
  }
  jit <- mojor_jit(f, disk_cache = FALSE)

  xi <- as.integer(seq_len(4))
  xd <- as.double(seq_len(4))
  expect_equal(jit(xi, xi), as.double(xi + xi))
  expect_equal(jit(xd, xd), xd + xd)

  info <- mojor_jit_info(jit)
  expect_identical(info$stats$compile_count, 2L)
  expect_length(info$signatures, 2L)

  required <- c(
    "key_hash", "signature_key", "types", "options", "calls", "hits_memory",
    "hits_disk", "compiles", "compile_time_sec", "failed_compiles",
    "compiled_via", "last_error", "last_event", "last_used_at"
  )
  for (entry in info$signatures) {
    expect_true(all(required %in% names(entry)))
  }

  expect_error(
    mojor_jit_info(function(x) x),
    "jit_fn must be a function returned by mojor_jit/mojor_njit/mojor_vectorize"
  )
})

test_that("mojor_jit_compile precompiles signatures and updates API compile counters", {  skip_if_no_mojo()
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + y[i]
    out
  }
  jit <- mojor_jit(f, disk_cache = FALSE)

  comp <- mojor_jit_compile(
    jit,
    signatures = list(list(x = "f64[]", y = "f64[]"))
  )
  expect_identical(comp$attempted, 1L)
  expect_identical(comp$succeeded, 1L)
  expect_identical(comp$failed, 0L)
  expect_identical(comp$results[[1]]$ok, TRUE)

  info0 <- mojor_jit_info(jit)
  expect_identical(info0$stats$api_compiles_attempted, 1L)
  expect_identical(info0$stats$api_compiles_succeeded, 1L)
  expect_identical(info0$stats$api_compiles_failed, 0L)
  expect_identical(info0$stats$compile_count, 1L)
  expect_identical(info0$signatures[[1]]$compiled_via, "api")
  expect_identical(info0$signatures[[1]]$last_event, "api_compiled")

  x <- as.double(seq_len(6))
  y <- as.double(seq_len(6))
  expect_equal(jit(x, y), f(x, y), tolerance = 1e-12)
  info1 <- mojor_jit_info(jit)
  expect_identical(info1$stats$cache_hits_memory, 1L)
})

test_that("mojor_jit_compile supports stop_on_error controls and api_compile diagnostics", {  skip_if_no_mojo()
  foo_local <- function(v) v + 1
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- foo_local(x[i])
    out
  }
  jit <- mojor_jit(f, disk_cache = FALSE)

  expect_error(
    mojor_jit_compile(
      jit,
      signatures = list(list(x = "f64[]")),
      stop_on_error = TRUE
    ),
    "compile failed for signature"
  )

  out <- mojor_jit_compile(
    jit,
    signatures = list(list(x = "f64[]")),
    stop_on_error = FALSE
  )
  expect_identical(out$attempted, 1L)
  expect_identical(out$succeeded, 0L)
  expect_identical(out$failed, 1L)
  expect_identical(out$results[[1]]$ok, FALSE)
  expect_true(grepl("unsupported|failed|code generation|strict mode", out$results[[1]]$message))

  info <- mojor_jit_info(jit)
  expect_identical(info$stats$api_compiles_attempted, 2L)
  expect_identical(info$stats$api_compiles_succeeded, 0L)
  expect_identical(info$stats$api_compiles_failed, 2L)
  expect_identical(info$last_error$stage, "api_compile")
})

test_that("mojor_jit_signatures returns declared/compiled/all sets in all formats", {  skip_if_no_mojo()
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

  declared_keys <- mojor_jit_signatures(jit, kind = "declared", format = "key")
  compiled_keys0 <- mojor_jit_signatures(jit, kind = "compiled", format = "key")
  expect_length(declared_keys, 2L)
  expect_identical(compiled_keys0, character(0))

  mojor_jit_compile(jit, signatures = list(list(x = "f64[]", y = "f64[]")))

  compiled_keys1 <- mojor_jit_signatures(jit, kind = "compiled", format = "key")
  all_keys <- mojor_jit_signatures(jit, kind = "all", format = "key")
  expect_length(compiled_keys1, 1L)
  expect_length(all_keys, 2L)
  expect_true(all(compiled_keys1 %in% all_keys))
  expect_true(all(declared_keys %in% all_keys))

  compiled_strings <- mojor_jit_signatures(jit, kind = "compiled", format = "string")
  expect_length(compiled_strings, 1L)
  expect_true(grepl("x: f64\\[\\]", compiled_strings[[1]]))
  expect_true(grepl("y: f64\\[\\]", compiled_strings[[1]]))

  all_typed <- mojor_jit_signatures(jit, kind = "all", format = "typed_list")
  expect_true(is.list(all_typed))
  expect_identical(names(all_typed), all_keys)
  expect_true(all(vapply(all_typed, function(s) s$x %in% c("f64[]", "i32[]"), logical(1))))
  expect_true(all(vapply(all_typed, function(s) s$y %in% c("f64[]", "i32[]"), logical(1))))

  expect_error(
    mojor_jit_compile(function(x) x, signatures = list(list(x = "f64[]"))),
    "jit_fn must be a function returned by mojor_jit/mojor_njit/mojor_vectorize"
  )
  expect_error(
    mojor_jit_signatures(function(x) x),
    "jit_fn must be a function returned by mojor_jit/mojor_njit/mojor_vectorize"
  )
})

test_that("mojor_jit promotes f32 inputs when mixed with f64 (scalar + array)", {  if (!mojor_is_loaded()) {
    skip("Mojo backend not loaded")
  }
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not installed")
  }
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + y
    out
  }
  jit <- mojor_jit(f)
  x <- float::fl(c(1, 2, 3))
  y <- 0.5
  out1 <- jit(x, y)
  expect_equal(out1, float::dbl(x) + y)

  x2 <- as.double(c(1, 2, 3))
  y2 <- float::fl(0.5)
  out2 <- jit(x2, y2)
  expect_equal(out2, x2 + as.double(y2))
})

test_that("mojor_jit promotes f32 inputs when mixed with f64 (array + array)", {  if (!mojor_is_loaded()) {
    skip("Mojo backend not loaded")
  }
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not installed")
  }
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + y[i]
    out
  }
  jit <- mojor_jit(f)
  x <- float::fl(c(1, 2, 3))
  y <- as.double(c(10, 20, 30))
  out1 <- jit(x, y)
  expect_equal(out1, float::dbl(x) + y)

  x2 <- as.double(c(4, 5, 6))
  y2 <- float::fl(c(7, 8, 9))
  out2 <- jit(x2, y2)
  expect_equal(out2, x2 + float::dbl(y2))
})

test_that("mojor_jit promotes integer arrays when mixed with f64", {  if (!mojor_is_loaded()) {
    skip("Mojo backend not loaded")
  }
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + y[i]
    out
  }
  jit <- mojor_jit(f)
  x <- 1:5
  y <- as.double(seq_len(5)) * 0.5
  out1 <- jit(x, y)
  expect_equal(out1, as.double(x) + y)

  x2 <- as.double(seq_len(5)) * 2
  y2 <- 1:5
  out2 <- jit(x2, y2)
  expect_equal(out2, x2 + as.double(y2))
})

test_that("mojor_jit promotes logical arrays when mixed with f64", {  if (!mojor_is_loaded()) {
    skip("Mojo backend not loaded")
  }
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + y[i]
    out
  }
  jit <- mojor_jit(f)
  x <- as.double(c(1, 2, 3, 4))
  y <- c(TRUE, FALSE, TRUE, FALSE)
  out1 <- jit(x, y)
  expect_equal(out1, x + as.double(y))
})

test_that("mojor_jit promotes logical arrays when mixed with f32", {  if (!mojor_is_loaded()) {
    skip("Mojo backend not loaded")
  }
  if (!requireNamespace("float", quietly = TRUE)) {
    skip("float package not installed")
  }
  f <- function(x, y) {
    out <- float::float32(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + y[i]
    out
  }
  jit <- mojor_jit(f)
  x <- float::fl(c(1, 2, 3, 4))
  y <- c(TRUE, FALSE, TRUE, FALSE)
  out1 <- jit(x, y)
  expect_equal(float::dbl(out1), float::dbl(x) + as.double(y))
})

test_that("mojor_jit supports object_mode hybrid for unsupported functions", {  skip_if_no_mojo()
  .mojor_test_local_options(ir_only = FALSE)

  foo_local <- function(v) v + 1
  f <- function(x) {
    y <- x * 2
    s <- sum(y)
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- foo_local(x[i])
    }
    out + s
  }

  jit <- mojor_jit(f, object_mode = "hybrid")
  x <- c(-1, 0, 1, 2)
  out1 <- jit(x)
  out2 <- jit(x)
  expect_equal(out1, f(x), tolerance = 1e-12)
  expect_equal(out2, f(x), tolerance = 1e-12)
})
