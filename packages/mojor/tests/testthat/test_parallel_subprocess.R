library(testthat)

# Subprocess test for parallel loops
# parallelize has issues with dynamic library loading from R,
# so we test by running a standalone Mojo process
.run_mojo_program <- function(code, filename) {
  mojo_file <- file.path(tempdir(), filename)
  writeLines(code, mojo_file)
  system2("mojo", mojo_file, stdout = TRUE, stderr = TRUE)
}

.build_parallel_affine_program <- function(dtype, ptr_alias, kernel_name, success_line) {
  paste(
    c(
      "from algorithm import parallelize",
      "from memory import UnsafePointer, alloc",
      "",
      sprintf("comptime %s = UnsafePointer[mut=True, type=%s, origin=MutAnyOrigin]", ptr_alias, dtype),
      "",
      sprintf("fn %s(x_data: %s, out_data: %s, n: Int) -> None:", kernel_name, ptr_alias, ptr_alias),
      "    @always_inline",
      "    fn body(__mojor_idx: Int) capturing:",
      "        var i = __mojor_idx + 1",
      "        out_data[i - 1] = (x_data[i - 1] * 2.0) + 1.0",
      "    parallelize[body](n)",
      "",
      "fn main():",
      "    var n = 100",
      sprintf("    var x_ptr = alloc[%s](n)", dtype),
      sprintf("    var out_ptr = alloc[%s](n)", dtype),
      "",
      "    for i in range(n):",
      sprintf("        x_ptr[i] = %s(i + 1)", dtype),
      "        out_ptr[i] = 0.0",
      "",
      sprintf("    %s(x_ptr, out_ptr, n)", kernel_name),
      "",
      "    var ok = True",
      "    for i in range(n):",
      sprintf("        var expected = (%s(i + 1) * 2.0) + 1.0", dtype),
      "        if out_ptr[i] != expected:",
      "            print(\"MISMATCH\")",
      "            ok = False",
      "            break",
      "",
      "    if ok:",
      sprintf("        %s", success_line),
      "    else:",
      "        print(\"FAILURE\")",
      "",
      "    x_ptr.free()",
      "    out_ptr.free()"
    ),
    collapse = "\n"
  )
}

.build_parallel_workload_program <- function(n) {
  c(
    "from algorithm import parallelize",
    "from memory import alloc",
    "",
    "fn main():",
    sprintf("    var n = %d", n),
    "    var out = alloc[Float64](n)",
    "    ",
    "    @always_inline",
    "    fn body(i: Int) capturing:",
    "        out[i] = Float64(i) * 2.0",
    "    ",
    "    parallelize[body](n)",
    "    ",
    "    var ok = True",
    "    for i in range(n):",
    "        if out[i] != Float64(i) * 2.0:",
    "            ok = False",
    "            break",
    "    ",
    "    if ok:",
    sprintf("        print(\"PASS_%d\")", n),
    "    else:",
    sprintf("        print(\"FAIL_%d\")", n),
    "    out.free()"
  )
}

.mk_parallel_struct_fn <- function() {
  function(x) {
    out <- numeric(length(x))
    for (i in seq_parallel_along(x)) {
      out[i] <- x[i] * 2 + 1
    }
    out
  }
}

test_that("parallel loop code compiles and runs via subprocess", {  skip_if_no_mojo()

  mojo_code <- .build_parallel_affine_program(
    dtype = "Float64",
    ptr_alias = "MutF64Ptr",
    kernel_name = "parallel_kernel",
    success_line = 'print("SUCCESS: All ", n, " values correct")'
  )
  result <- .run_mojo_program(mojo_code, "test_parallel_subprocess.mojo")
  
  # Check result
  expect_true(any(grepl("SUCCESS", result, fixed = TRUE)), 
    info = paste("Mojo output:", paste(result, collapse = "\n")))
})

test_that("generated parallel code structure is correct", {  # This test verifies the generated Mojo code has the right structure
  # without actually running it (which may segfault via dyn.load)

  f <- .mk_parallel_struct_fn()
  
  trans <- mojor_transpile(f, x = "f64[]", name = "t_parallel_struct")
  mojo <- trans$mojo

  # Newer codegen may conservatively lower to a regular loop while still
  # recognizing the parallel form as safe.
  has_parallel_body <- grepl("fn _mojor_parallel_body_i", mojo, fixed = TRUE)
  if (has_parallel_body) {
    expect_true(grepl("from algorithm import parallelize", mojo, fixed = TRUE))
    expect_true(grepl("__mojor_idx: Int", mojo, fixed = TRUE))
    expect_true(grepl("capturing", mojo, fixed = TRUE))
    expect_true(grepl("parallelize[_mojor_parallel_body_i]", mojo, fixed = TRUE))
    expect_true(grepl("var i = __mojor_idx + 1", mojo, fixed = TRUE))
  } else {
    expect_true(isTRUE(trans$parallel$safe))
    expect_true(grepl("for i in range", mojo))
    expect_true(grepl("out\\[Int\\(\\(i - 1\\)\\)\\]", mojo))
  }
})

test_that("parallel loop with float32 works", {  skip_if_no_mojo()

  mojo_code <- .build_parallel_affine_program(
    dtype = "Float32",
    ptr_alias = "MutF32Ptr",
    kernel_name = "parallel_kernel_f32",
    success_line = 'print("SUCCESS: f32 parallel loop works")'
  )
  result <- .run_mojo_program(mojo_code, "test_parallel_f32.mojo")
  expect_true(any(grepl("SUCCESS", result, fixed = TRUE)))
})

test_that("parallel loop with different workloads", {  skip_if_no_mojo()
  
  # Test with different sizes to ensure parallelize handles various workloads
  test_sizes <- c(1, 10, 100, 1000)
  
  for (n in test_sizes) {
    code_lines <- .build_parallel_workload_program(n)
    result <- .run_mojo_program(code_lines, sprintf("test_parallel_n%d.mojo", n))
    expect_true(any(grepl(sprintf("PASS_%d", n), result, fixed = TRUE)),
      info = sprintf("n=%d failed: %s", n, paste(result, collapse = "\n")))
  }
})
