simple_add <- function(x, y) {
  out <- numeric(length(x))
  for (i in seq_along(x)) {
    out[i] <- x[i] + y[i]
  }
  out
}

test_that("mojor_simd_report prints simd info", {  old <- mojor_options(simd_mode = "auto")
  on.exit(mojor_options(simd_mode = old$simd_mode), add = TRUE)
  out <- capture.output(
    mojor_simd_report(simple_add, x = "f64[]", y = "f64[]", assume_aligned = 32L, simd_mode = "auto")
  )
  expect_true(any(grepl("simd.mode:", out, fixed = TRUE)))
  expect_true(any(grepl("simd.safe:", out, fixed = TRUE)))
  expect_true(any(grepl("simd.emitted:", out, fixed = TRUE)))
})

test_that("fusion_debug emits simd report", {  old <- mojor_options(simd_mode = "auto")
  on.exit(mojor_options(simd_mode = old$simd_mode), add = TRUE)
  expect_message(
    mojor_transpile(simple_add, x = "f64[]", y = "f64[]", assume_aligned = 32L, simd_mode = "auto", fusion_debug = TRUE),
    "simd.mode:"
  )
})

test_that("mojor_build emits simd report when fusion_debug is TRUE", {  old <- mojor_options(simd_mode = "auto")
  on.exit(mojor_options(simd_mode = old$simd_mode), add = TRUE)
  skip_if_not(mojor_is_loaded(), "Mojo backend not loaded")
  expect_message(
    mojor_build(simple_add, x = "f64[]", y = "f64[]", name = "t_build_simd", assume_aligned = 32L, simd_mode = "auto", fusion_debug = TRUE),
    "simd.mode:"
  )
})
