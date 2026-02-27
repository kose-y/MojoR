library(testthat)


test_that("math helpers transpile", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- abs(x[i]) + sqrt(y[i]) + exp(x[i]) + log(y[i])
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", y = "f64[]", name = "t_helpers")
  expect_match(res$mojo, "abs", fixed = TRUE)
  expect_match(res$mojo, "sqrt", fixed = TRUE)
  expect_match(res$mojo, "exp", fixed = TRUE)
  expect_match(res$mojo, "log", fixed = TRUE)
})


test_that("min/max transpile", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- min(x[i], y[i]) + max(x[i], y[i])
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", y = "f64[]", name = "t_minmax")
  expect_match(res$mojo, "min", fixed = TRUE)
  expect_match(res$mojo, "max", fixed = TRUE)
})

test_that("pow/trig/floor/ceiling/pmin/pmax transpile", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- (x[i] ^ 2) + sin(x[i]) + cos(y[i]) +
        floor(x[i]) + ceiling(y[i]) + pmin(x[i], y[i]) + pmax(x[i], y[i])
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", y = "f64[]", name = "t_helpers_more")
  expect_match(res$mojo, "from math import", fixed = TRUE)
  expect_match(res$mojo, "pow", fixed = TRUE)
  expect_match(res$mojo, "sin", fixed = TRUE)
  expect_match(res$mojo, "cos", fixed = TRUE)
  expect_match(res$mojo, "floor", fixed = TRUE)
  expect_match(res$mojo, "ceil", fixed = TRUE)
  expect_match(res$mojo, "min", fixed = TRUE)
  expect_match(res$mojo, "max", fixed = TRUE)
})

test_that("pmin/pmax accept more than two args in loops", {  f <- function(x, y, z) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- pmin(x[i], y[i], z[i]) + pmax(x[i], y[i], z[i])
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", y = "f64[]", z = "f64[]", name = "t_pmin3")
  expect_match(res$mojo, "min", fixed = TRUE)
  expect_match(res$mojo, "max", fixed = TRUE)
})

test_that("extended math helpers transpile", {  f <- function(x, y) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- sinh(x[i]) + cosh(y[i]) + tanh(x[i]) +
        log1p(x[i]) + expm1(y[i]) +
        trunc(x[i]) + round(y[i]) + sign(x[i]) +
        atan2(x[i], y[i]) + abs2(x[i]) +
        hypot(x[i], y[i]) + cbrt(x[i]) + lgamma(y[i]) + erf(x[i])
    }
    out
  }

  res <- mojor_transpile(f, x = "f64[]", y = "f64[]", name = "t_helpers_ext")
  expect_match(res$mojo, "sinh", fixed = TRUE)
  expect_match(res$mojo, "cosh", fixed = TRUE)
  expect_match(res$mojo, "tanh", fixed = TRUE)
  expect_match(res$mojo, "log1p", fixed = TRUE)
  expect_match(res$mojo, "expm1", fixed = TRUE)
  expect_match(res$mojo, "trunc", fixed = TRUE)
  expect_match(res$mojo, "round", fixed = TRUE)
  expect_true(
    grepl("sign", res$mojo, fixed = TRUE) ||
      grepl("Float64\\(-1\\.0\\) if", res$mojo)
  )
  expect_match(res$mojo, "atan2", fixed = TRUE)
  expect_match(res$mojo, "*", fixed = TRUE)
  expect_match(res$mojo, "hypot", fixed = TRUE)
  expect_match(res$mojo, "cbrt", fixed = TRUE)
  expect_match(res$mojo, "lgamma", fixed = TRUE)
  expect_match(res$mojo, "erf", fixed = TRUE)
})

test_that("postamble wires emitted GPU helper shims", {  lines <- c(
    "from memory import OpaquePointer, UnsafePointer, alloc",
    "fn _internal() -> Int32:",
    "    var r = _mojor_gpu_reduce(x, \"sum\", Int(0), False, True, Int(10))",
    "    var m = _mojor_gpu_matmul(a, b, False, False)",
    "    var m2 = _mojor_gpu_matmul_into(out, a, b, False, False, Int(2), Int(3), Int(3), Int(4), Int(2), Int(4))",
    "    _ = r",
    "    _ = m",
    "    _ = m2",
    "    return Int32(0)",
    "@export(\"t_gpu_wire\", ABI=\"C\")",
    "fn t_gpu_wire() -> Int32:",
    "    return Int32(1)"
  )

  out <- .mojor_finalize_mojo_postamble(lines, index_bounds = FALSE)
  expect_true(any(grepl("^fn _mojor_gpu_reduce\\[", out)))
  expect_true(any(grepl("^fn _mojor_gpu_matmul\\[", out)))
  expect_true(any(grepl("^fn _mojor_gpu_matmul_into\\(", out)))
  expect_true(any(grepl("^fn _mojor_gpu_reduce\\[.*\\) ->", out)))
  expect_true(any(grepl("^fn _mojor_gpu_matmul\\[.*\\) raises ->", out)))
  expect_true(any(grepl("^fn _mojor_gpu_matmul_into\\(out: MutF64Ptr, a: ImmutF64Ptr, b: ImmutF64Ptr", out)))
  expect_true(any(grepl("fn _mojor_gpu_reduce\\[dims_t: AnyType\\]\\(value: ImmutF64Ptr", out, fixed = FALSE)))
  expect_true(any(grepl("fn _mojor_gpu_reduce\\[dims_t: AnyType\\]\\(value: ImmutF32Ptr", out, fixed = FALSE)))
  expect_true(any(grepl("fn _mojor_gpu_matmul_into\\(out: MutF64Ptr, a: ImmutF64Ptr, b: ImmutF64Ptr", out, fixed = FALSE)))
  expect_true(any(grepl("fn _mojor_gpu_matmul_into\\(out: MutF32Ptr, a: ImmutF32Ptr, b: ImmutF32Ptr", out, fixed = FALSE)))
  expect_true(any(grepl("if op == \"sum\":", out, fixed = TRUE)))
  expect_true(any(grepl("if op == \"mean\":", out, fixed = TRUE)))
  expect_true(any(grepl("if op == \"min\":", out, fixed = TRUE)))
  expect_true(any(grepl("if op == \"max\":", out, fixed = TRUE)))
  expect_true(any(grepl("if op == \"argmin\":", out, fixed = TRUE)))
  expect_true(any(grepl("if op == \"argmax\":", out, fixed = TRUE)))
  expect_true(any(grepl("if not dims_default or keepdims:", out, fixed = TRUE)))
  expect_true(any(grepl("if n_i <= 0:", out, fixed = TRUE)))
  expect_true(any(grepl("return _MOJOR_NAN", out, fixed = TRUE)))
  expect_true(any(grepl("return _MOJOR_NAN_F32", out, fixed = TRUE)))
  expect_true(any(grepl("return out", out, fixed = TRUE)))
  expect_true(any(grepl("raise Error\\(\"_mojor_gpu_matmul expression form is not executable", out, fixed = FALSE)))
  expect_true(any(grepl("acc += a[a_idx] * b[b_idx]", out, fixed = TRUE)))

  helper_idx <- which(grepl("^fn _mojor_gpu_reduce\\[", out))[1]
  export_idx <- which(grepl("^@export", out))[1]
  expect_true(helper_idx < export_idx)
})

test_that("gpu jit telemetry detects helper shim coverage", {  lines <- c(
    "from memory import OpaquePointer, UnsafePointer, alloc",
    "fn _internal() -> Int32:",
    "    var r = _mojor_gpu_reduce(x, \"sum\", Int(0), False, True, Int(10))",
    "    var m = _mojor_gpu_matmul(a, b, False, False)",
    "    var m2 = _mojor_gpu_matmul_into(out, a, b, False, False, Int(2), Int(3), Int(3), Int(4), Int(2), Int(4))",
    "    _ = r",
    "    _ = m",
    "    _ = m2",
    "    return Int32(0)",
    "@export(\"t_gpu_wire\", ABI=\"C\")",
    "fn t_gpu_wire() -> Int32:",
    "    return Int32(1)"
  )
  out <- .mojor_finalize_mojo_postamble(lines, index_bounds = FALSE)
  info <- .mojor_detect_gpu_jit_coverage(
    out,
    elementwise_gpu_emitted = FALSE,
    elementwise_target = "off"
  )
  expect_identical(info$route, "loop")
  expect_true(isTRUE(info$helper_backed_used))
  expect_false(isTRUE(info$unified_loop_used))
  expect_identical(info$helper_calls$reduce, 1L)
  expect_identical(info$helper_calls$matmul, 1L)
  expect_identical(info$helper_calls$matmul_into, 1L)
  expect_true(info$helper_defs$reduce >= 1L)
  expect_true(info$helper_defs$matmul >= 1L)
  expect_true(info$helper_defs$matmul_into >= 1L)
  expect_true(is.list(info$preview_blockers))
  expect_identical(info$preview_blockers$reduce_calls, 1L)
  expect_identical(info$preview_blockers$matmul_calls, 1L)
  expect_identical(info$preview_blockers$matmul_into_calls, 1L)
  expect_identical(info$preview_blockers$reduce_ops, "sum")
})

test_that("gpu jit telemetry reports unified loop lane and is exposed in transpile output", {  unified_info <- .mojor_detect_gpu_jit_coverage(
    c(
      "fn t_gpu_unified() -> None:",
      "    pass"
    ),
    elementwise_gpu_emitted = TRUE,
    elementwise_target = "gpu"
  )
  expect_identical(unified_info$route, "loop")
  expect_true(isTRUE(unified_info$unified_loop_used))

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + 1
    out
  }
  trans <- mojor_transpile(f, x = "f64[]", name = "t_gpu_jit_telemetry_none")
  expect_true(is.list(trans$gpu_jit))
  expect_identical(trans$gpu_jit$route, "loop")
  expect_false(isTRUE(trans$gpu_jit$helper_backed_used))
  expect_false(isTRUE(trans$gpu_jit$unified_loop_used))
  expect_true(is.list(trans$gpu_jit$helper_calls))
})

test_that("gpu jit unified preview gate rejects helper-backed routes", {  lines <- c(
    "from memory import OpaquePointer, UnsafePointer, alloc",
    "fn _internal() -> Int32:",
    "    var r = _mojor_gpu_reduce(x, \"mean\", Int(0), False, True, Int(10))",
    "    var m = _mojor_gpu_matmul_into(out, a, b, False, False, Int(2), Int(3), Int(3), Int(4), Int(2), Int(4))",
    "    _ = r",
    "    _ = m",
    "    return Int32(0)",
    "@export(\"t_gpu_wire\", ABI=\"C\")",
    "fn t_gpu_wire() -> Int32:",
    "    return Int32(1)"
  )
  out <- .mojor_finalize_mojo_postamble(lines, index_bounds = FALSE)
  expect_error(
    .mojor_enforce_gpu_jit_mode(
      out,
      gpu_jit_mode = "unified_preview",
      elementwise_gpu_emitted = FALSE,
      elementwise_target = "off"
    ),
    "blocked by _mojor_gpu_reduce\\(op=mean\\), _mojor_gpu_matmul_into\\(assign\\)"
  )
  auto_mode <- .mojor_enforce_gpu_jit_mode(
    out,
    gpu_jit_mode = "auto",
    elementwise_gpu_emitted = FALSE,
    elementwise_target = "off"
  )
  expect_identical(auto_mode$mode, "auto")
  expect_identical(auto_mode$route, "loop")
  expect_true(isTRUE(auto_mode$helper_backed_used))
  expect_false(isTRUE(auto_mode$unified_loop_used))
})

test_that("gpu jit telemetry reports configured mode with unified elementwise promotion", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + 1
    out
  }
  trans <- mojor_transpile(
    f,
    x = "f64[]",
    name = "t_gpu_jit_mode_unified",
    gpu_jit_mode = "unified_preview"
  )
  expect_true(is.list(trans$gpu_jit))
  expect_identical(trans$gpu_jit$mode, "unified_preview")
  expect_identical(trans$gpu_jit$route, "loop")
  expect_true(isTRUE(trans$elementwise$gpu_buf_emitted))
})

test_that("gpu jit unified preview lowers supported gpu_reduce assign forms", {  f_sum <- function(x) {
    out <- 0
    out <- gpu_sum(x)
    out
  }
  trans_sum <- mojor_transpile(
    f_sum,
    x = "f64[]",
    name = "t_gpu_jit_preview_reduce_sum",
    gpu_jit_mode = "unified_preview"
  )
  expect_false(grepl("_mojor_gpu_reduce\\(", trans_sum$mojo))
  expect_identical(trans_sum$gpu_jit$route, "loop")
  expect_true(isTRUE(trans_sum$gpu_jit$preview_lowering_used))
  expect_true("reduce" %in% trans_sum$gpu_jit$preview_lowered_ops)

  f_nested <- function(x) {
    out <- 0
    out <- 1 + gpu_sum(x)
    out
  }
  trans_nested <- mojor_transpile(
    f_nested,
    x = "f64[]",
    name = "t_gpu_jit_preview_reduce_nested_sum",
    gpu_jit_mode = "unified_preview"
  )
  expect_false(grepl("_mojor_gpu_reduce\\(", trans_nested$mojo))
  expect_identical(trans_nested$gpu_jit$route, "loop")

  f_mean <- function(x) {
    out <- 0
    out <- gpu_mean(x)
    out
  }
  trans_mean <- mojor_transpile(
    f_mean,
    x = "f64[]",
    name = "t_gpu_jit_preview_reduce_mean",
    gpu_jit_mode = "unified_preview"
  )
  expect_false(grepl("_mojor_gpu_reduce\\(", trans_mean$mojo))
  expect_identical(trans_mean$gpu_jit$route, "loop")
  expect_true(grepl("Float64\\(", trans_mean$mojo))

  f_prod <- function(x) {
    out <- 0
    out <- gpu_prod(x)
    out
  }
  trans_prod <- mojor_transpile(
    f_prod,
    x = "f64[]",
    name = "t_gpu_jit_preview_reduce_prod",
    gpu_jit_mode = "unified_preview"
  )
  expect_false(grepl("_mojor_gpu_reduce\\(", trans_prod$mojo))
  expect_identical(trans_prod$gpu_jit$route, "loop")
})

test_that("gpu jit unified preview lowers assign gpu_matmul subset helper-free", {  f <- function(a, b) {
    out <- matrix(0, 2, 2)
    out <- gpu_matmul(a, b)
    out
  }
  trans <- mojor_transpile(
    f,
    a = "f64[,]",
    b = "f64[,]",
    name = "t_gpu_jit_preview_matmul_assign",
    gpu_jit_mode = "unified_preview"
  )
  expect_identical(trans$gpu_jit$route, "loop")
  expect_true(isTRUE(trans$gpu_jit$preview_lowering_used))
  expect_true("matmul" %in% trans$gpu_jit$preview_lowered_ops)
  expect_false(grepl("_mojor_gpu_matmul\\(", trans$mojo))
  expect_false(grepl("_mojor_gpu_matmul_into\\(", trans$mojo))
  expect_true(grepl("__mojor_gpu_mm_inner_n == __mojor_gpu_mm_b_inner_n", trans$mojo, fixed = TRUE))
})

test_that("gpu jit unified preview lowers return gpu_matmul when output contract exists", {  f <- function(a, b) {
    out <- matrix(0, 2, 2)
    return(gpu_matmul(a, b))
  }
  trans <- mojor_transpile(
    f,
    a = "f64[,]",
    b = "f64[,]",
    name = "t_gpu_jit_preview_matmul_return",
    gpu_jit_mode = "unified_preview"
  )
  expect_identical(trans$gpu_jit$route, "loop")
  expect_false(grepl("_mojor_gpu_matmul\\(", trans$mojo))
  expect_false(grepl("_mojor_gpu_matmul_into\\(", trans$mojo))
  expect_true(grepl("__mojor_gpu_mm_inner_n == __mojor_gpu_mm_b_inner_n", trans$mojo, fixed = TRUE))
})

test_that("gpu jit unified preview lowers return gpu_matmul without predeclared output", {  f <- function(a, b) {
    return(gpu_matmul(a, b))
  }
  trans <- mojor_transpile(
    f,
    a = "f64[,]",
    b = "f64[,]",
    name = "t_gpu_jit_preview_matmul_return_no_out",
    gpu_jit_mode = "unified_preview"
  )
  expect_identical(trans$gpu_jit$route, "loop")
  expect_false(grepl("_mojor_gpu_matmul\\(", trans$mojo))
  expect_false(grepl("_mojor_gpu_matmul_into\\(", trans$mojo))
  expect_true(grepl("__mojor_gpu_mm_inner_n == __mojor_gpu_mm_b_inner_n", trans$mojo, fixed = TRUE))
})

test_that("gpu jit telemetry marks helper-free preview lowerings on loop route", {  info_reduce <- .mojor_detect_gpu_jit_coverage(
    c(
      "fn t() -> None:",
      "    var __mojor_gpu_reduce_assign_1 = Float64(0)"
    ),
    elementwise_gpu_emitted = FALSE,
    elementwise_target = "off"
  )
  expect_identical(info_reduce$route, "loop")
  expect_true(isTRUE(info_reduce$preview_lowering_used))
  expect_true("reduce" %in% info_reduce$preview_lowered_ops)

  info_matmul <- .mojor_detect_gpu_jit_coverage(
    c(
      "fn t() -> None:",
      "    var __mojor_gpu_mm_row = Int(0)"
    ),
    elementwise_gpu_emitted = FALSE,
    elementwise_target = "off"
  )
  expect_identical(info_matmul$route, "loop")
  expect_true("matmul" %in% info_matmul$preview_lowered_ops)
})

test_that("gpu jit unified preview matmul strict lane keeps mixed-type diagnostic stable", {  f <- function(a, b) {
    out <- matrix(0.0, 2, 2)
    out <- gpu_matmul(a, b)
    out
  }
  expect_error(
    mojor_transpile(
      f,
      a = "f32[,]",
      b = "f64[,]",
      name = "t_gpu_jit_preview_matmul_type_mismatch",
      gpu_jit_mode = "unified_preview",
      ir_only = TRUE
    ),
    "a and b must have matching element type in strict mode"
  )
})

test_that("gpu jit auto lowers supported gpu_reduce assign forms helper-free", {  f <- function(x) {
    out <- 0
    out <- gpu_mean(x)
    out
  }
  trans <- mojor_transpile(
    f,
    x = "f64[]",
    name = "t_gpu_jit_auto_reduce_mean",
    gpu_jit_mode = "auto"
  )
  expect_identical(trans$gpu_jit$mode, "auto")
  expect_identical(trans$gpu_jit$route, "loop")
  expect_false(grepl("_mojor_gpu_reduce\\(", trans$mojo))
  expect_true(isTRUE(trans$gpu_jit$preview_lowering_used))
  expect_true("reduce" %in% trans$gpu_jit$preview_lowered_ops)
})

test_that("gpu jit auto lowers return gpu_matmul helper-free", {  f <- function(a, b) {
    return(gpu_matmul(a, b))
  }
  trans <- mojor_transpile(
    f,
    a = "f64[,]",
    b = "f64[,]",
    name = "t_gpu_jit_auto_matmul_return",
    gpu_jit_mode = "auto"
  )
  expect_identical(trans$gpu_jit$mode, "auto")
  expect_identical(trans$gpu_jit$route, "loop")
  expect_false(grepl("_mojor_gpu_matmul\\(", trans$mojo))
  expect_false(grepl("_mojor_gpu_matmul_into\\(", trans$mojo))
  expect_true(isTRUE(trans$gpu_jit$preview_lowering_used))
  expect_true("matmul" %in% trans$gpu_jit$preview_lowered_ops)
})

test_that("gpu jit auto promotes simple elementwise loop to unified gpu lane", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2 + 1
    }
    out
  }
  trans <- mojor_transpile(
    f,
    x = "f64[]",
    name = "t_gpu_jit_auto_elementwise_loop",
    gpu_jit_mode = "auto"
  )
  expect_identical(trans$gpu_jit$mode, "auto")
  expect_identical(trans$gpu_jit$route, "loop")
  expect_true(isTRUE(trans$elementwise$enabled))
  expect_true(isTRUE(trans$elementwise$gpu_buf_emitted))
  expect_true(grepl("t_gpu_jit_auto_elementwise_loop_gpu_buf_f64", trans$mojo, fixed = TRUE))
})

test_that("gpu jit invalid mode is rejected", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2 + 1
    }
    out
  }
  expect_error(
    mojor_transpile(
      f,
      x = "f64[]",
      name = "t_gpu_jit_invalid_mode_elementwise_loop",
      gpu_jit_mode = "legacy"
    ),
    "should be one of"
  )
})
