library(testthat)

# Test workarounds for parallelize + dyn.load issue
.with_mocked_export_transpile <- function(fake_transpile, expr) {
  mojor_env <- environment(mojor_export_parallel)
  old_transpile <- get("mojor_transpile", envir = mojor_env)
  on.exit(assign("mojor_transpile", old_transpile, envir = mojor_env), add = TRUE)
  assign("mojor_transpile", fake_transpile, envir = mojor_env)
  eval.parent(substitute(expr))
}

.mk_vec_double_fn <- function() {
  function(x) {
    out <- numeric(length(x))
    for (i in seq_parallel_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }
}

.mk_vec_mul_add_scalar_fn <- function() {
  function(x, a) {
    out <- numeric(length(x))
    for (i in seq_parallel_along(x)) {
      out[i] <- x[i] * a + 1
    }
    out
  }
}

.mk_vec_add_fn <- function() {
  function(x, y) {
    out <- numeric(length(x))
    for (i in seq_parallel_along(x)) {
      out[i] <- x[i] + y[i]
    }
    out
  }
}

.mk_vec_plus1_fn <- function() {
  function(x) {
    out <- numeric(length(x))
    for (i in seq_parallel_along(x)) {
      out[i] <- x[i] + 1
    }
    out
  }
}

test_that("parallel runtime mode honors env override and platform default", {  old_mode <- Sys.getenv("MOJOR_PARALLEL_RUNTIME", unset = "")
  on.exit(Sys.setenv(MOJOR_PARALLEL_RUNTIME = old_mode), add = TRUE)

  Sys.setenv(MOJOR_PARALLEL_RUNTIME = "subprocess")
  expect_identical(.mojor_parallel_runtime_mode(), "subprocess")
  expect_false(.mojor_parallel_inprocess_enabled())

  Sys.setenv(MOJOR_PARALLEL_RUNTIME = "inprocess")
  expect_identical(.mojor_parallel_runtime_mode(), "inprocess")
  expect_true(.mojor_parallel_inprocess_enabled())

  Sys.setenv(MOJOR_PARALLEL_RUNTIME = "invalid")
  expected <- if (identical(tolower(Sys.info()[["sysname"]]), "darwin")) "subprocess" else "inprocess"
  expect_identical(.mojor_parallel_runtime_mode(), expected)
})

test_that("parallel runtime fallback wrapper and detection are deterministic", {  expect_false(.mojor_trans_uses_parallelize(NULL))
  expect_false(.mojor_trans_uses_parallelize(list()))
  expect_false(.mojor_trans_uses_parallelize(list(mojo = "")))
  expect_true(.mojor_trans_uses_parallelize(list(mojo = "from algorithm import parallelize")))
  expect_true(.mojor_trans_uses_parallelize(list(
    mojo = "",
    parallel = list(effective = TRUE)
  )))
  expect_false(.mojor_trans_uses_parallelize(list(
    mojo = "from algorithm import parallelize",
    parallel = list(effective = FALSE)
  )))

  fallback <- .mojor_parallel_runtime_fallback_result(
    fn = function(x) x + 1,
    trans = list(mojo = "parallelize"),
    name = "t_parallel_fallback",
    build_dir = tempdir(),
    cache_key = "cache_key",
    reason = "forced subprocess fallback"
  )
  expect_false(isTRUE(fallback$compiled))
  expect_true(isTRUE(fallback$success))
  expect_identical(fallback$kernel, "t_parallel_fallback")
  expect_identical(fallback$cache_key, "cache_key")
  expect_identical(fallback$trans$mojo, "parallelize")
  expect_null(fallback$wrapper_so)
  expect_null(fallback$gpu_func)
  expect_null(fallback$gpu_func_raw)
  expect_identical(attr(fallback$func, "parallel_runtime_mode"), "subprocess_fallback")
  expect_identical(attr(fallback$func, "parallel_runtime_reason"), "forced subprocess fallback")
  expect_identical(fallback$parallel_runtime$mode, "subprocess_fallback")
  expect_identical(fallback$parallel_runtime$reason, "forced subprocess fallback")
  expect_false(isTRUE(fallback$parallel_runtime$inprocess_enabled))
  expect_equal(fallback$func(2), 3)
})

test_that("parallel runtime reason strings are deterministic", {  expect_identical(
    .mojor_parallel_runtime_reason("inprocess"),
    "in-process parallel runtime enabled"
  )
  expect_identical(
    .mojor_parallel_runtime_reason("subprocess"),
    "in-process parallel runtime disabled on this platform; subprocess fallback required"
  )
})

test_that("mojor_run_subprocess executes Mojo code", {  skip_if_no_mojo()
  
  mojo_code <- '
from algorithm import parallelize
from memory import alloc

fn main():
    var n = 10
    var out = alloc[Float64](n)
    
    @always_inline
    fn body(i: Int) capturing:
        out[i] = Float64(i) * 2.0
    
    parallelize[body](n)
    
    for i in range(n):
        print(out[i])
    
    out.free()
'
  
  result <- mojor_run_subprocess(mojo_code)
  
  # Check that we got 10 lines of output
  expect_equal(length(result), 10)
  # Check first value is 0.0
  expect_equal(as.numeric(result[1]), 0)
  # Check last value is 18.0 (9 * 2)
  expect_equal(as.numeric(result[10]), 18)
})

test_that("mojor_compile_executable creates runnable binary", {  skip_if_no_mojo()
  
  mojo_code <- '
from algorithm import parallelize
from memory import alloc

fn main():
    var n = 5
    var out = alloc[Float64](n)
    
    @always_inline
    fn body(i: Int) capturing:
        out[i] = Float64(i) + 100.0
    
    parallelize[body](n)
    
    for i in range(n):
        print(out[i])
    
    out.free()
'
  
  exe_path <- tempfile("mojor_test_", fileext = "")
  mojor_compile_executable(mojo_code, exe_path)
  
  # Check executable exists
  expect_true(file.exists(exe_path))
  
  # Run it
  result <- system2(exe_path, stdout = TRUE, stderr = TRUE)
  
  # Check output
  expect_equal(length(result), 5)
  expect_equal(as.numeric(result), c(100, 101, 102, 103, 104))
})

test_that("mojor_export_parallel generates standalone file", {  f <- .mk_vec_double_fn()
  
  temp_dir <- tempdir()
  output_file <- mojor_export_parallel(f, x = "f64[]", name = "test_export", output_dir = temp_dir)
  
  # Check file was created
  expect_true(file.exists(output_file))
  
  # Check content
  content <- readLines(output_file)
  expect_true(any(grepl("parallelize", content, fixed = TRUE)))
  expect_true(any(grepl("seq_parallel_along", content, fixed = TRUE) | 
                  grepl("from algorithm import parallelize", content, fixed = TRUE)))
  expect_false(any(grepl("Standalone execution not yet implemented", content, fixed = TRUE)))
  expect_true(any(grepl("STANDALONE_OK", content, fixed = TRUE)))
  expect_true(any(grepl("bitcast\\[NoneType\\]", content)))
})

test_that("mojor_export_parallel standalone runner executes", {  skip_if_no_mojo()

  f <- .mk_vec_double_fn()

  temp_dir <- tempdir()
  output_file <- mojor_export_parallel(f, x = "f64[]", name = "test_export_run", output_dir = temp_dir)
  result <- system2("mojo", output_file, stdout = TRUE, stderr = TRUE)

  expect_true(any(grepl("STANDALONE_OK", result, fixed = TRUE)),
    info = paste(result, collapse = "\n"))
  expect_true(any(grepl("NA_FLAG", result, fixed = TRUE)),
    info = paste(result, collapse = "\n"))
})

test_that("mojor_export_parallel handles mixed signatures", {  skip_if_no_mojo()

  cases <- list(
    list(
      tag = "arr_scalar_f64",
      fn = .mk_vec_mul_add_scalar_fn(),
      types = list(x = "f64[]", a = "f64"),
      out0_pattern = "^OUT0 3\\.0$"
    ),
    list(
      tag = "arr_arr_f64",
      fn = .mk_vec_add_fn(),
      types = list(x = "f64[]", y = "f64[]"),
      out0_pattern = "^OUT0 2\\.0$"
    ),
    list(
      tag = "arr_f32",
      fn = .mk_vec_plus1_fn(),
      types = list(x = "f32[]"),
      out0_pattern = "^OUT0 2\\.0$"
    ),
    list(
      tag = "arr_i32",
      fn = .mk_vec_plus1_fn(),
      types = list(x = "i32[]"),
      out0_pattern = "^OUT0 2\\.0$"
    ),
    list(
      tag = "arr_lgl",
      fn = .mk_vec_plus1_fn(),
      types = list(x = "lgl[]"),
      out0_pattern = "^OUT0 2\\.0$"
    )
  )

  for (case in cases) {
    export_args <- c(
      list(fn = case$fn, name = paste0("test_export_mixed_", case$tag), output_dir = tempdir()),
      case$types
    )
    output_file <- do.call(mojor_export_parallel, export_args)
    result <- system2("mojo", output_file, stdout = TRUE, stderr = TRUE)

    expect_true(any(grepl("STANDALONE_OK", result, fixed = TRUE)),
      info = paste(case$tag, paste(result, collapse = "\n"), sep = ": "))
    expect_true(any(grepl("NA_FLAG 0", result, fixed = TRUE)),
      info = paste(case$tag, paste(result, collapse = "\n"), sep = ": "))
    expect_true(any(grepl(case$out0_pattern, result)),
      info = paste(case$tag, paste(result, collapse = "\n"), sep = ": "))
  }
})

test_that("mojor_export_parallel rejects unsupported matrix signatures", {  f <- .mk_vec_plus1_fn()

  expect_error(
    mojor_export_parallel(f, x = "f64[, ]", name = "test_export_matrix", output_dir = tempdir()),
    "standalone runner currently supports only scalar and 1D array arguments"
  )

  f_mixed <- .mk_vec_add_fn()

  expect_error(
    mojor_export_parallel(
      f_mixed,
      x = "f64[]",
      y = "f64[, ]",
      name = "test_export_matrix_mixed",
      output_dir = tempdir()
    ),
    "standalone runner currently supports only scalar and 1D array arguments"
  )
})

test_that("mojor_export_parallel validates transpiler metadata guards", {  f <- function(x) x
  cases <- list(
    list(
      tag = "out_matrix",
      transpile = function(...) {
        list(
          types = list(x = "f64[]"),
          out_type = "f64[, ]"
        )
      },
      error = "standalone runner currently supports only scalar or 1D array outputs"
    ),
    list(
      tag = "bad_types",
      transpile = function(...) {
        list(
          types = list("f64[]"),
          out_type = "f64[]"
        )
      },
      error = "transpiler did not return named argument type metadata"
    ),
    list(
      tag = "bad_sig",
      transpile = function(...) {
        list(
          types = list(x = "f64[]"),
          out_type = "f64[]",
          signature = c(
            "x_ptr: ImmutOpaqueAny",
            "out_a_ptr: MutOpaqueAny",
            "out_b_ptr: MutOpaqueAny",
            "__mojor_n: Int32"
          )
        )
      },
      error = "expected exactly one output pointer in generated signature"
    ),
    list(
      tag = "bad_param",
      transpile = function(...) {
        list(
          types = list(x = "f64[]"),
          out_type = "f64[]",
          signature = c(
            "x_ptr: ImmutOpaqueAny",
            "out_ptr: MutOpaqueAny",
            "__mojor_n: Int32",
            "__mojor_unknown_flag: Int32"
          )
        )
      },
      error = "unsupported generated signature parameter: __mojor_unknown_flag"
    )
  )

  for (case in cases) {
    .with_mocked_export_transpile(case$transpile, {
      expect_error(
        mojor_export_parallel(f, x = "f64[]", name = paste0("test_export_", case$tag), output_dir = tempdir()),
        case$error
      )
    })
  }
})
