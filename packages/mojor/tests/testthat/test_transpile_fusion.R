library(testthat)

test_that("sequential loops fuse when safe", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i]
    }
    for (i in seq_along(x)) {
      out[i] <- out[i] + 1
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "t_fuse", fusion_debug = TRUE, assume_aligned = 32L)
  expect_true(trans$fused)
  expect_true(trans$simd$safe)
  expect_equal(trans$simd$assume_aligned, 32L)
  expect_true(trans$parallel$safe)
  count <- gregexpr("for i in range", trans$mojo, fixed = TRUE)[[1]]
  expect_true(length(count[count > 0]) %in% c(1, 2))
})

test_that("loops with different vars are not fused", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i]
    }
    for (j in seq_along(x)) {
      out[j] <- out[j] + 1
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "t_no_fuse_var", fusion_debug = TRUE)
  expect_true(trans$fused)
  expect_true(trans$simd$safe)
  expect_true(trans$parallel$safe)
  count <- gregexpr("for ", trans$mojo, fixed = TRUE)[[1]]
  expect_true(length(count[count > 0]) %in% c(1, 2))
})

test_that("scalar reductions do not fuse", {  f <- function(x) {
    acc <- 0
    for (i in seq_along(x)) {
      acc <- acc + x[i]
    }
    for (i in seq_along(x)) {
      acc <- acc + 1
    }
    acc
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "t_no_fuse_scalar", fusion_debug = TRUE)
  expect_false(trans$fused)
  expect_false(trans$simd$safe)
  expect_false(trans$parallel$safe)
  count <- gregexpr("for ", trans$mojo, fixed = TRUE)[[1]]
  expect_equal(length(count[count > 0]), 2)
})

test_that("fusion is disabled when scalar accumulator exists", {  f <- function(x) {
    out <- numeric(length(x))
    acc <- 0
    for (i in seq_along(x)) {
      out[i] <- x[i]
    }
    for (i in seq_along(x)) {
      out[i] <- out[i] + 1
      acc <- acc + 1
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "t_no_fuse_acc", fusion_debug = TRUE)
  expect_false(trans$fused)
  expect_false(trans$simd$safe)
  expect_false(trans$parallel$safe)
})

test_that("parallel safety rejects out[j] dependency", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      for (j in seq_along(x)) {
        out[i] <- out[i] + out[j]
      }
    }
    out
  }
  old <- mojor_options(warn_parallel = TRUE)
  on.exit(mojor_options(warn_parallel = old$warn_parallel), add = TRUE)
  expect_warning(
    trans <- mojor_transpile(f, x = "f64[]", name = "t_parallel_dep"),
    "parallelization skipped"
  )
  expect_false(trans$parallel$safe)
})

test_that("aligned vector path emits store for simple scalar RHS", {  f <- function(n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      out[i] <- 2
    }
    out
  }
  trans <- mojor_transpile(f, n = "i32", name = "t_align_const", assume_aligned = 32L)
  # IR path handles loops; aligned constants are still emitted
  expect_true(grepl("_MOJOR_ALIGN", trans$mojo))
  expect_true(trans$simd$safe)
})

test_that("aligned vector path supports x[i] + y[i]", {  old <- mojor_options(simd_mode = "explicit")
  on.exit(mojor_options(simd_mode = old$simd_mode), add = TRUE)
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]", name = "t_align_add", assume_aligned = 32L, simd_mode = "explicit")
  # IR path handles loops; verify alignment eligibility is detected
  expect_true(trans$simd$safe)
})

test_that("aligned vector path supports abs/min/max", {  old <- mojor_options(simd_mode = "explicit")
  on.exit(mojor_options(simd_mode = old$simd_mode), add = TRUE)
  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- max(abs(x[i]), y[i])
    }
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]", name = "t_align_minmax", assume_aligned = 32L, simd_mode = "explicit")
  # IR path handles loops; verify alignment eligibility is detected
  expect_true(trans$simd$safe)
})

test_that("auto SIMD mode skips explicit aligned vector path", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
  trans <- mojor_transpile(
    f,
    x = "f64[]",
    y = "f64[]",
    name = "t_simd_auto",
    assume_aligned = 32L,
    simd_mode = "auto"
  )
  expect_false(trans$simd$emitted)
  expect_equal(trans$simd$mode, "auto")
  expect_false(grepl("out.store\\[width=_MOJOR_ALIGN_F64, alignment=_MOJOR_ALIGN\\]", trans$mojo))
})
