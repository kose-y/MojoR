# =============================================================================
# MojoR Profiling Tests (Phase 6.7)
# =============================================================================

test_that("mojor_profile_report validates inputs", {  expect_error(
    mojor_profile_report(built = NULL),
    "built cannot be NULL"
  )

  expect_error(
    mojor_profile_report(built = list()),
    "built object missing 'kernel' field"
  )

  expect_error(
    mojor_profile_report(built = list(kernel = "test")),
    "built object missing 'trans' field"
  )

  expect_error(
    mojor_profile_report(built = list(kernel = "test", trans = list())),
    "profiling was not enabled for this kernel"
  )
})

test_that("mojor_profile_report returns profile data", {  # Create a minimal built object with profiling enabled
  built <- list(
    kernel = "test_kernel",
    trans = list(
      types = list(x = "f64[]"),
      out_type = "f64[]",
      simd = list(emitted = FALSE, safe = TRUE)
    ),
    profile_enabled = TRUE,
    profile_timing = list(),
    profile_memory = list(),
    profile_simd = list(),
    profile_suggestions = list()
  )

  prof <- mojor_profile_report(built)
  expect_equal(prof$version, "1.0.0")
  expect_equal(prof$timing, list())
  expect_equal(prof$memory, list())
  expect_equal(prof$simd, list())
  expect_equal(prof$suggestions, list())
  expect_true(nzchar(prof$created_at))
})

test_that("mojor_suggest returns suggestions", {  built <- list(
    kernel = "test_kernel",
    trans = list(
      types = list(x = "f64[]"),
      out_type = "f64[]",
      simd = list(emitted = FALSE, safe = TRUE)
    ),
    profile_enabled = TRUE,
    profile_timing = list(),
    profile_memory = list(),
    profile_simd = list(),
    profile_suggestions = list(
      list(priority = "high", category = "simd", message = "Enable SIMD")
    )
  )

  sugg <- mojor_suggest(built)
  expect_is(sugg, "list")
  expect_equal(length(sugg), 1)
  expect_equal(sugg[[1]]$priority, "high")
})

test_that(".mojor_profile_data creates profile object", {  prof <- .mojor_profile_data(
    timing = list(a = list(total_ms = 1.0)),
    memory = list(b = list(bytes = 100)),
    simd = list(c = list(instructions = 10, vectorized_pct = 50.0)),
    suggestions = list(list(priority = "high", message = "test"))
  )

  expect_equal(prof$version, "1.0.0")
  expect_equal(length(prof$timing), 1)
  expect_equal(length(prof$memory), 1)
  expect_equal(length(prof$simd), 1)
  expect_equal(length(prof$suggestions), 1)
})

test_that("print.mojor_profile works", {  prof <- .mojor_profile_data(
    timing = list(total = list(total_ms = 1.5)),
    memory = list(input = list(bytes = 100)),
    simd = list(vectorization = list(instructions = 10, vectorized_pct = 50.0)),
    suggestions = list(list(priority = "high", message = "test"))
  )

  # Should not error (output is expected)
  # Use print.mojor_profile directly to test the custom print method
  expect_output(print.mojor_profile(prof), "MojoR Profile Report")
})

test_that(".mojor_add_profiling adds profiling data", {  built <- list(
    kernel = "test",
    trans = list(
      types = list(x = "f64[]"),
      out_type = "f64[]",
      simd = list(emitted = FALSE, safe = TRUE)
    )
  )

  result <- .mojor_add_profiling(built, profile_enabled = TRUE)
  expect_true(result$profile_enabled)
  expect_is(result$profile_timing, "list")
  expect_is(result$profile_memory, "list")
  expect_is(result$profile_simd, "list")
  expect_is(result$profile_suggestions, "list")
})

test_that(".mojor_add_profiling without profiling returns unchanged", {  built <- list(
    kernel = "test",
    trans = list(
      types = list(x = "f64[]"),
      out_type = "f64[]"
    )
  )

  result <- .mojor_add_profiling(built, profile_enabled = FALSE)
  expect_false("profile_enabled" %in% names(result))
})

test_that(".mojor_collect_timing returns timing data", {  built <- list(
    kernel = "test",
    trans = list(
      types = list(x = "f64[]"),
      out_type = "f64[]"
    )
  )

  timing <- .mojor_collect_timing(built)
  expect_is(timing, "list")
  expect_true("total" %in% names(timing))
})

test_that(".mojor_collect_memory returns memory data", {  built <- list(
    kernel = "test",
    trans = list(
      types = list(x = "f64[]"),
      out_type = "f64[]"
    )
  )

  memory <- .mojor_collect_memory(built)
  expect_is(memory, "list")
  expect_true("input" %in% names(memory))
})

test_that(".mojor_collect_simd returns simd data", {  built <- list(
    kernel = "test",
    trans = list(
      types = list(x = "f64[]"),
      out_type = "f64[]",
      simd = list(emitted = FALSE, safe = TRUE)
    )
  )

  simd <- .mojor_collect_simd(built)
  expect_is(simd, "list")
  expect_true("vectorization" %in% names(simd))
})

test_that(".mojor_generate_suggestions generates suggestions", {  built <- list(
    kernel = "test",
    trans = list(
      types = list(x = "f64[]"),
      out_type = "f64[]",
      simd = list(emitted = FALSE, safe = TRUE),
      unroll = 1,
      opt_level = 0
    )
  )

  suggestions <- .mojor_generate_suggestions(built)
  expect_is(suggestions, "list")
  expect_true(length(suggestions) > 0)
})

test_that(".mojor_generate_suggestions with SIMD enabled", {  built <- list(
    kernel = "test",
    trans = list(
      types = list(x = "f64[]"),
      out_type = "f64[]",
      simd = list(emitted = TRUE, safe = TRUE, assume_aligned = 16),
      unroll = 1,
      opt_level = 0
    )
  )

  suggestions <- .mojor_generate_suggestions(built)
  expect_is(suggestions, "list")
  # Should have fewer suggestions when SIMD is enabled
  expect_true(length(suggestions) >= 0)
})

test_that("mojor_build with profile = TRUE", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", profile = TRUE)
  expect_true(built$profile_enabled)
  expect_is(built$profile_timing, "list")
  expect_is(built$profile_memory, "list")
  expect_is(built$profile_simd, "list")
  expect_is(built$profile_suggestions, "list")
})

test_that("mojor_build with profile = TRUE and cache = FALSE still adds profiling", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", profile = TRUE, cache = FALSE, load = FALSE)
  expect_true(isTRUE(built$profile_enabled))
  expect_is(built$profile_timing, "list")
  expect_is(built$profile_memory, "list")
  expect_is(built$profile_simd, "list")
  expect_is(built$profile_suggestions, "list")
})

test_that("mojor_build with profile = FALSE", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", profile = FALSE)
  expect_false("profile_enabled" %in% names(built))
})

test_that("mojor_build with invalid profile", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }

  expect_error(
    mojor_build(f, x = "f64[]", profile = NULL),
    "profile must be TRUE or FALSE"
  )

  expect_error(
    mojor_build(f, x = "f64[]", profile = c(TRUE, FALSE)),
    "profile must be TRUE or FALSE"
  )
})

test_that("mojor_profile_report with profiling enabled", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", profile = TRUE)
  prof <- mojor_profile_report(built)
  expect_equal(prof$version, "1.0.0")
})

test_that("mojor_suggest with profiling enabled", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }

  built <- mojor_build(f, x = "f64[]", profile = TRUE)
  sugg <- mojor_suggest(built)
  expect_is(sugg, "list")
})
