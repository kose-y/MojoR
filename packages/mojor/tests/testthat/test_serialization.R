# =============================================================================
# MojoR Serialization Tests (Phase 6.2)
# =============================================================================

test_that("mojor_save creates valid RDS file", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }

  built <- mojor_build(f, x = "f64[]")
  expect_true(built$success)

  tmp_file <- tempfile(fileext = ".mojor")
  on.exit(unlink(tmp_file), add = TRUE)

  result <- mojor_save(built, tmp_file)
  expect_true(file.exists(tmp_file))
  expect_is(result, "list")
  expect_equal(result$version, "1.0.0")
})

test_that("mojor_load reads valid RDS file", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }

  built <- mojor_build(f, x = "f64[]")
  expect_true(built$success)

  tmp_file <- tempfile(fileext = ".mojor")
  on.exit(unlink(tmp_file), add = TRUE)

  mojor_save(built, tmp_file)

  # Load without loading library
  serial <- mojor_load_kernel(tmp_file, load_library = FALSE)
  expect_equal(serial$kernel, built$kernel)
  expect_equal(serial$cache_key, built$cache_key)
  expect_equal(serial$trans$mojo, built$trans$mojo)
})

test_that("mojor_load with load_library = TRUE creates func", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }

  built <- mojor_build(f, x = "f64[]")
  expect_true(built$success)

  tmp_file <- tempfile(fileext = ".mojor")
  on.exit(unlink(tmp_file), add = TRUE)

  mojor_save(built, tmp_file)

  # Load with library
  loaded <- mojor_load_kernel(tmp_file, load_library = TRUE)
  expect_true(loaded$success)
  expect_is(loaded$func, "function")

  # Test the function
  x <- c(1.0, 2.0, 3.0)
  result <- loaded$func(x)
  expected <- c(2.0, 4.0, 6.0)
  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("mojor_check_compatibility validates version", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }

  built <- mojor_build(f, x = "f64[]")
  tmp_file <- tempfile(fileext = ".mojor")
  on.exit(unlink(tmp_file), add = TRUE)

  mojor_save(built, tmp_file)

  # Valid version should pass
  expect_true(mojor_check_compatibility(readRDS(tmp_file)))

  # Strict mode with mismatch should error
  serial <- readRDS(tmp_file)
  serial$version <- "0.9.0"
  expect_error(
    mojor_check_compatibility(serial, strict = TRUE),
    "major version mismatch"
  )
})

test_that("mojor_info returns metadata", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }

  built <- mojor_build(f, x = "f64[]")
  tmp_file <- tempfile(fileext = ".mojor")
  on.exit(unlink(tmp_file), add = TRUE)

  mojor_save(built, tmp_file)

  info <- mojor_serialize(tmp_file)
  expect_equal(info$version, "1.0.0")
  expect_equal(info$kernel, built$kernel)
  expect_true("mojo" %in% info$trans_fields)
})

test_that("mojor_save handles missing fields", {  # Test with invalid built object
  expect_error(
    mojor_save(NULL, tempfile()),
    "built cannot be NULL"
  )

  expect_error(
    mojor_save(list(), tempfile()),
    "missing 'kernel' field"
  )

  expect_error(
    mojor_save(list(kernel = "test"), tempfile()),
    "missing 'trans' field"
  )
})

test_that("mojor_save rejects object-mode kernels explicitly", {  .mojor_test_local_options(ir_only = FALSE)
  foo_local <- function(v) v + 1
  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- foo_local(x[i])
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", object_mode = "fallback")
  expect_true(isTRUE(built$object_mode))
  expect_error(
    mojor_save(built, tempfile(fileext = ".mojor")),
    "object-mode kernels are not serializable yet"
  )
})

test_that("mojor_load handles missing file", {  expect_error(
    mojor_load_kernel("nonexistent.mojor"),
    "file not found"
  )
})

test_that("mojor_save with embed_source = TRUE", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }

  built <- mojor_build(f, x = "f64[]")
  tmp_file <- tempfile(fileext = ".mojor")
  on.exit(unlink(tmp_file), add = TRUE)

  mojor_save(built, tmp_file, embed_source = TRUE)

  serial <- readRDS(tmp_file)
  expect_true("mojo_source" %in% names(serial))
  expect_equal(serial$mojo_source, built$trans$mojo)
})

test_that("round-trip preserves kernel functionality", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2 + 1
    }
    out
  }

  built <- mojor_build(f, x = "f64[]")
  expect_true(built$success)

  tmp_file <- tempfile(fileext = ".mojor")
  on.exit(unlink(tmp_file), add = TRUE)

  # Save
  mojor_save(built, tmp_file)

  # Load
  loaded <- mojor_load_kernel(tmp_file, load_library = TRUE)
  expect_true(loaded$success)

  # Test with different inputs
  test_cases <- list(
    c(1.0, 2.0, 3.0),
    c(0.0, 0.0, 0.0),
    c(-1.0, 2.0, -3.0),
    runif(10)
  )

  for (x in test_cases) {
    result <- loaded$func(x)
    expected <- x * 2 + 1
    expect_equal(result, expected, tolerance = 1e-10)
  }
})

test_that("mojor_load with force_load = TRUE", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }

  built <- mojor_build(f, x = "f64[]")
  tmp_file <- tempfile(fileext = ".mojor")
  on.exit(unlink(tmp_file), add = TRUE)

  mojor_save(built, tmp_file)

  # Load with force_load when build_dir doesn't exist
  serial <- readRDS(tmp_file)
  serial$build_dir <- "/nonexistent/path"
  saveRDS(serial, tmp_file)

  loaded <- mojor_load_kernel(tmp_file, load_library = TRUE, force_load = TRUE)
  expect_false(loaded$success)
  expect_equal(loaded$load_error, "Build artifacts not found")
})
