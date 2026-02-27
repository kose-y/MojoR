library(testthat)

test_that("mojor_options follows base-style semantics", {  current <- mojor_options()
  expect_true("na_mode" %in% names(current))
  expect_true("simd_mode" %in% names(current))
  expect_true("assume_aligned" %in% names(current))
  expect_true("warn_ifelse" %in% names(current))
  expect_true("warn_parallel" %in% names(current))
  expect_true("sd_mode" %in% names(current))
  expect_true("fast_math" %in% names(current))
  expect_true("fast_math_flag" %in% names(current))
  expect_true("index_base" %in% names(current))
  expect_true("array_layout" %in% names(current))
  expect_true("gpu_jit_mode" %in% names(current))
  expect_true("gpu_reject_fallback" %in% names(current))
  expect_true("r_jit_level" %in% names(current))
  expect_true("jit_disk_cache" %in% names(current))
  expect_true("fusion_allow_control_flow_simple" %in% names(current))
  expect_true("fusion_allow_broadcast_nd_identity" %in% names(current))

  old <- mojor_options(na_mode = "unsafe")
  expect_equal(old$na_mode, "forbid")
  expect_equal(mojor_options("na_mode")$na_mode, "unsafe")

  on.exit(mojor_options(na_mode = "forbid"), add = TRUE)

  expect_error(mojor_options(bogus = 1), "unknown option")
  expect_error(mojor_options(1), "unnamed arguments")
})

test_that("mojor_options validates simd_mode", {  old <- mojor_options(simd_mode = "auto")
  expect_equal(old$simd_mode, "explicit")
  expect_equal(mojor_options("simd_mode")$simd_mode, "auto")
  expect_error(mojor_options(simd_mode = "fast"), "should be one of")
  mojor_options(simd_mode = "explicit")
  expect_equal(mojor_options("simd_mode")$simd_mode, "explicit")
})

test_that("mojor_options validates warn_ifelse", {  old <- mojor_options(warn_ifelse = FALSE)
  expect_true(old$warn_ifelse)
  expect_false(mojor_options("warn_ifelse")$warn_ifelse)
  expect_error(mojor_options(warn_ifelse = 1), "warn_ifelse must be TRUE or FALSE")
  mojor_options(warn_ifelse = TRUE)
  expect_true(mojor_options("warn_ifelse")$warn_ifelse)
})

test_that("mojor_options validates warn_parallel", {  old <- mojor_options(warn_parallel = FALSE)
  on.exit(mojor_options(warn_parallel = old$warn_parallel), add = TRUE)
  expect_true(is.logical(old$warn_parallel))
  expect_false(mojor_options("warn_parallel")$warn_parallel)
  expect_error(mojor_options(warn_parallel = 1), "warn_parallel must be TRUE or FALSE")
  mojor_options(warn_parallel = TRUE)
  expect_true(mojor_options("warn_parallel")$warn_parallel)
})

test_that("mojor_options validates sd_mode", {  old <- mojor_options(sd_mode = "two_pass")
  expect_equal(old$sd_mode, "welford")
  expect_equal(mojor_options("sd_mode")$sd_mode, "two_pass")
  expect_error(mojor_options(sd_mode = "fast"), "should be one of")
  mojor_options(sd_mode = "welford")
  expect_equal(mojor_options("sd_mode")$sd_mode, "welford")
})

test_that("sd_mode override does not mutate global options", {  old <- mojor_options(sd_mode = "welford")
  on.exit(mojor_options(sd_mode = old$sd_mode), add = TRUE)
  x <- as.double(1:10)
  suppressWarnings(try(mojor_sd_f64(x, sd_mode = "two_pass"), silent = TRUE))
  expect_equal(mojor_options("sd_mode")$sd_mode, "welford")
})

test_that("assume_aligned option is validated", {  old <- mojor_options(assume_aligned = 32L)
  expect_true(is.null(old$assume_aligned))
  expect_equal(mojor_options("assume_aligned")$assume_aligned, 32L)
  expect_error(mojor_options(assume_aligned = -1L), "assume_aligned must be a positive integer")
  mojor_options(assume_aligned = NULL)
  expect_true(is.null(mojor_options("assume_aligned")$assume_aligned))
})

test_that("mojor_options validates na_mode and fast_math", {  old <- mojor_options(na_mode = "unsafe", fast_math = TRUE, fast_math_flag = "--fast-math")
  expect_equal(old$na_mode, "forbid")
  expect_equal(mojor_options("na_mode")$na_mode, "unsafe")
  expect_true(isTRUE(mojor_options("fast_math")$fast_math))
  expect_equal(mojor_options("fast_math_flag")$fast_math_flag, "--fast-math")
  expect_error(mojor_options(na_mode = "bad"), "should be one of")
  expect_error(mojor_options(na_mode = "fallback"), "should be one of")
  expect_error(mojor_options(fast_math = 1), "fast_math must be TRUE or FALSE")
  expect_error(mojor_options(fast_math_flag = 1), "fast_math_flag must be NULL or a single string")
  mojor_options(na_mode = "forbid", fast_math = FALSE, fast_math_flag = NULL)
})

test_that("fast_math uses only explicit flag override", {
  old_opts <- mojor_options(
    fast_math = FALSE,
    fast_math_flag = NULL
  )
  old_warned <- .mojor_state$fast_math_warned
  on.exit({
    mojor_options(
      fast_math = old_opts$fast_math,
      fast_math_flag = old_opts$fast_math_flag
    )
    .mojor_state$fast_math_warned <- old_warned
  }, add = TRUE)

  .mojor_state$fast_math_warned <- FALSE
  expect_warning(
    expect_identical(.mojor_fast_math_flags(TRUE), character(0)),
    "no default mojo fast_math flag is configured"
  )
  expect_true(isTRUE(.mojor_state$fast_math_warned))

  mojor_options(fast_math_flag = "-ffast-math")
  .mojor_state$fast_math_warned <- FALSE
  expect_identical(.mojor_fast_math_flags(TRUE), "-ffast-math")
})

test_that("mojor_options validates index_base and array_layout", {  old <- mojor_options(index_base = "zero_based", array_layout = "row_major")
  on.exit(mojor_options(index_base = old$index_base, array_layout = old$array_layout), add = TRUE)
  expect_equal(old$index_base, "one_based")
  expect_equal(mojor_options("index_base")$index_base, "zero_based")
  expect_equal(mojor_options("array_layout")$array_layout, "row_major")
  expect_error(mojor_options(index_base = "base1"), "should be one of")
  expect_error(mojor_options(array_layout = "fortran"), "should be one of")
})

test_that("mojor_options validates fusion legality opt-in flags", {  old <- mojor_options(
    fusion_allow_control_flow_simple = TRUE,
    fusion_allow_broadcast_nd_identity = TRUE
  )
  on.exit(
    mojor_options(
      fusion_allow_control_flow_simple = old$fusion_allow_control_flow_simple,
      fusion_allow_broadcast_nd_identity = old$fusion_allow_broadcast_nd_identity
    ),
    add = TRUE
  )

  expect_false(isTRUE(old$fusion_allow_control_flow_simple))
  expect_false(isTRUE(old$fusion_allow_broadcast_nd_identity))
  expect_true(isTRUE(mojor_options("fusion_allow_control_flow_simple")$fusion_allow_control_flow_simple))
  expect_true(isTRUE(mojor_options("fusion_allow_broadcast_nd_identity")$fusion_allow_broadcast_nd_identity))

  expect_error(
    mojor_options(fusion_allow_control_flow_simple = 1),
    "fusion_allow_control_flow_simple must be TRUE or FALSE"
  )
  expect_error(
    mojor_options(fusion_allow_broadcast_nd_identity = "yes"),
    "fusion_allow_broadcast_nd_identity must be TRUE or FALSE"
  )
})

test_that("mojor_options validates gpu_jit_mode", {
  old <- mojor_options("gpu_jit_mode")
  on.exit(mojor_options(gpu_jit_mode = old$gpu_jit_mode), add = TRUE)

  mojor_options(gpu_jit_mode = "auto")
  expect_equal(mojor_options("gpu_jit_mode")$gpu_jit_mode, "auto")

  mojor_options(gpu_jit_mode = "unified_preview")
  expect_equal(mojor_options("gpu_jit_mode")$gpu_jit_mode, "unified_preview")

  expect_error(
    mojor_options(gpu_jit_mode = "legacy"),
    "should be one of"
  )
})

test_that("mojor_options validates gpu_reject_fallback", {
  old <- mojor_options(gpu_reject_fallback = TRUE)
  on.exit(mojor_options(gpu_reject_fallback = old$gpu_reject_fallback), add = TRUE)

  expect_false(isTRUE(old$gpu_reject_fallback))
  expect_true(isTRUE(mojor_options("gpu_reject_fallback")$gpu_reject_fallback))

  mojor_options(gpu_reject_fallback = FALSE)
  expect_false(isTRUE(mojor_options("gpu_reject_fallback")$gpu_reject_fallback))

  expect_error(
    mojor_options(gpu_reject_fallback = NA),
    "gpu_reject_fallback must be TRUE or FALSE"
  )
  expect_error(
    mojor_options(gpu_reject_fallback = "yes"),
    "gpu_reject_fallback must be TRUE or FALSE"
  )
})

test_that("mojor_options validates r_jit_level", {  old <- mojor_options(r_jit_level = 3L)
  on.exit(mojor_options(r_jit_level = old$r_jit_level), add = TRUE)

  expect_equal(old$r_jit_level, 0L)
  expect_equal(mojor_options("r_jit_level")$r_jit_level, 3L)

  expect_error(
    mojor_options(r_jit_level = -1L),
    "r_jit_level must be an integer between 0 and 3"
  )
  expect_error(
    mojor_options(r_jit_level = 4L),
    "r_jit_level must be an integer between 0 and 3"
  )
  expect_error(
    mojor_options(r_jit_level = "fast"),
    "r_jit_level must be an integer between 0 and 3"
  )
})

test_that("mojor_options validates jit_disk_cache", {  old <- mojor_options(jit_disk_cache = FALSE)
  on.exit(mojor_options(jit_disk_cache = old$jit_disk_cache), add = TRUE)

  expect_true(isTRUE(old$jit_disk_cache))
  expect_false(isTRUE(mojor_options("jit_disk_cache")$jit_disk_cache))

  mojor_options(jit_disk_cache = NULL)
  expect_true(is.null(mojor_options("jit_disk_cache")$jit_disk_cache))

  mojor_options(jit_disk_cache = TRUE)
  expect_true(isTRUE(mojor_options("jit_disk_cache")$jit_disk_cache))

  expect_error(
    mojor_options(jit_disk_cache = "yes"),
    "jit_disk_cache must be TRUE, FALSE, or NULL"
  )
  expect_error(
    mojor_options(jit_disk_cache = c(TRUE, FALSE)),
    "jit_disk_cache must be TRUE, FALSE, or NULL"
  )
})

test_that("mojor_options validates jit_strict_signatures", {  old <- mojor_options(jit_strict_signatures = TRUE)
  on.exit(mojor_options(jit_strict_signatures = old$jit_strict_signatures), add = TRUE)

  expect_false(isTRUE(old$jit_strict_signatures))
  expect_true(isTRUE(mojor_options("jit_strict_signatures")$jit_strict_signatures))

  mojor_options(jit_strict_signatures = FALSE)
  expect_false(isTRUE(mojor_options("jit_strict_signatures")$jit_strict_signatures))

  expect_error(
    mojor_options(jit_strict_signatures = NULL),
    "jit_strict_signatures must be TRUE or FALSE"
  )
  expect_error(
    mojor_options(jit_strict_signatures = "yes"),
    "jit_strict_signatures must be TRUE or FALSE"
  )
})

test_that("mojor_options validates jit_profile and applies profile bundle", {  old <- mojor_options(
  c("jit_profile", "jit_disk_cache", "jit_strict_signatures", "r_jit_level")
)
  on.exit(do.call(mojor_options, old), add = TRUE)

  mojor_options(jit_profile = "dev")
  dev <- mojor_options(c("jit_profile", "jit_disk_cache", "jit_strict_signatures", "r_jit_level"))
  expect_identical(dev$jit_profile, "dev")
  expect_true(isTRUE(dev$jit_disk_cache))
  expect_false(isTRUE(dev$jit_strict_signatures))
  expect_identical(dev$r_jit_level, 0L)

  mojor_options(jit_profile = "bench")
  bench <- mojor_options(c("jit_profile", "jit_disk_cache", "jit_strict_signatures", "r_jit_level"))
  expect_identical(bench$jit_profile, "bench")
  expect_true(isTRUE(bench$jit_disk_cache))
  expect_true(isTRUE(bench$jit_strict_signatures))
  expect_identical(bench$r_jit_level, 0L)

  mojor_options(jit_profile = NULL)
  expect_true(is.null(mojor_options("jit_profile")$jit_profile))

  expect_error(
    mojor_options(jit_profile = "ci"),
    "should be one of"
  )
})

test_that("mojor_options applies jit_profile before explicit overrides in one call", {  old <- mojor_options(
  c("jit_profile", "jit_disk_cache", "jit_strict_signatures", "r_jit_level")
)
  on.exit(do.call(mojor_options, old), add = TRUE)

  mojor_options(
    jit_profile = "bench",
    jit_disk_cache = FALSE,
    jit_strict_signatures = FALSE,
    r_jit_level = 2L
  )
  out <- mojor_options(c("jit_profile", "jit_disk_cache", "jit_strict_signatures", "r_jit_level"))
  expect_identical(out$jit_profile, "bench")
  expect_false(isTRUE(out$jit_disk_cache))
  expect_false(isTRUE(out$jit_strict_signatures))
  expect_identical(out$r_jit_level, 2L)
})

test_that("mojor_build applies and restores r_jit_level around transpile", {
  if (!requireNamespace("compiler", quietly = TRUE)) {
    skip("compiler package unavailable")
  }

  old_jit <- compiler::enableJIT(-1L)
  on.exit(invisible(compiler::enableJIT(old_jit)), add = TRUE)

  env <- environment(mojor_build)
  old_transpile <- get("mojor_transpile", envir = env, inherits = FALSE)
  on.exit(assign("mojor_transpile", old_transpile, envir = env), add = TRUE)

  .mojor_test_local_options(r_jit_level = 0L)

  seen <- NA_integer_
  assign("mojor_transpile", function(...) {
    seen <<- compiler::enableJIT(-1L)
    stop("jit_probe")
  }, envir = env)

  expect_error(
    mojor_build(function(x) x, x = "f64[]", cache = FALSE, load = FALSE),
    "jit_probe"
  )
  expect_equal(seen, 0L)
  expect_equal(compiler::enableJIT(-1L), old_jit)

  seen <- NA_integer_
  expect_error(
    mojor_build(function(x) x, x = "f64[]", cache = FALSE, load = FALSE, r_jit_level = 2L),
    "jit_probe"
  )
  expect_equal(seen, 2L)
  expect_equal(compiler::enableJIT(-1L), old_jit)
})
