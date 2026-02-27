# =============================================================================
# MojoR FFI Tests (Phase 6.3)
# =============================================================================

.with_declared_c_registry <- function(code) {
  .mojor_test_local_state("declared_c_functions", list())
  on.exit(.mojor_test_local_state("declared_c_functions", list()), add = TRUE)
  force(code)
}

.with_declared_c_and_emit_state <- function(code) {
  .mojor_test_local_state("declared_c_functions", list())
  .mojor_test_local_state("needs_ffi", FALSE)
  on.exit({
    .mojor_test_local_state("declared_c_functions", list())
    .mojor_test_local_state("needs_ffi", FALSE)
  }, add = TRUE)
  force(code)
}

test_that("mojor_declare_c validates inputs", {  # Test invalid name
  expect_error(
    mojor_declare_c(name = NULL, args = list(x = "f64*"), returns = "f64"),
    "name must be a non-empty character string"
  )

  expect_error(
    mojor_declare_c(name = "", args = list(x = "f64*"), returns = "f64"),
    "name must be a non-empty character string"
  )

  # Test invalid args
  expect_error(
    mojor_declare_c(name = "test", args = NULL, returns = "f64"),
    "args must be a named list"
  )

  expect_error(
    mojor_declare_c(name = "test", args = list("f64*"), returns = "f64"),
    "args must have named elements"
  )

  # Test invalid returns
  expect_error(
    mojor_declare_c(name = "test", args = list(x = "f64*"), returns = NULL),
    "returns must be a non-empty character string"
  )

  # Test invalid library
  expect_error(
    mojor_declare_c(name = "test", args = list(x = "f64*"), returns = "f64", library = ""),
    "library must be a non-empty character string or NULL"
  )

  # Test invalid argument type
  expect_error(
    mojor_declare_c(name = "test", args = list(x = "invalid*"), returns = "f64"),
    "invalid argument type"
  )

  # Test invalid return type
  expect_error(
    mojor_declare_c(name = "test", args = list(x = "f64*"), returns = "invalid"),
    "invalid return type"
  )
})

test_that("mojor_declare_c stores declaration", {  mojor_declare_c(
    name = "test_func",
    args = list(x = "f64*", n = "i32"),
    returns = "f64",
    library = "libtest.so"
  )

  expect_true("test_func" %in% names(.mojor_state$declared_c_functions))
  decl <- .mojor_state$declared_c_functions[["test_func"]]
  expect_equal(decl$name, "test_func")
  expect_equal(decl$args, list(x = "f64*", n = "i32"))
  expect_equal(decl$returns, "f64")
  expect_equal(decl$library, "libtest.so")
})

test_that("mojor_c_call validates function is declared", {  .with_declared_c_registry({
    expect_error(
      mojor_c_call("undeclared_func"),
      "function 'undeclared_func' not declared"
    )
  })
})

test_that("mojor_c_call creates IR node", {  mojor_declare_c(
    name = "test_call",
    args = list(x = "f64*"),
    returns = "f64"
  )

  node <- mojor_c_call("test_call", 1.0)
  expect_equal(node$kind, "c_call")
  expect_equal(node$name, "test_call")
  expect_equal(node$returns, "f64")
})

test_that("mojor_export_c_header validates inputs", {  expect_error(
    mojor_export_c_header(built = NULL, header_path = "test.h"),
    "built cannot be NULL"
  )

  expect_error(
    mojor_export_c_header(built = list(), header_path = "test.h"),
    "built object missing 'kernel' field"
  )

  expect_error(
    mojor_export_c_header(built = list(kernel = "test"), header_path = "test.h"),
    "built object missing 'trans' field"
  )

  expect_error(
    mojor_export_c_header(built = list(kernel = "test", trans = list()), header_path = NULL),
    "header_path cannot be NULL"
  )

  expect_error(
    mojor_export_c_header(
      built = list(kernel = "test", trans = list(types = list(x = "f64[]"), out_type = "f64[]")),
      header_path = "test.h",
      namespace = ""
    ),
    "namespace must be a non-empty character string or NULL"
  )
})

test_that("mojor_export_c_header generates valid header", {  # Create a minimal built object
  built <- list(
    kernel = "test_kernel",
    trans = list(
      types = list(x = "f64[]"),
      out_type = "f64[]"
    )
  )

  tmp_file <- tempfile(fileext = ".h")
  on.exit(unlink(tmp_file), add = TRUE)

  header <- mojor_export_c_header(built, tmp_file)

  # Check header content
  expect_true(length(header) > 0)
  expect_true(any(grepl("#ifndef", header)))
  expect_true(any(grepl("#define", header)))
  expect_true(any(grepl("#endif", header)))
  expect_true(any(grepl("void test_kernel\\(", header)))
  expect_true(any(grepl("double\\* x", header)))
  expect_true(any(grepl("double\\* out_ptr", header)))
  expect_true(any(grepl("int __mojor_n", header)))
})

test_that("mojor_export_c_header with namespace", {  built <- list(
    kernel = "test_kernel",
    trans = list(
      types = list(x = "f64[]"),
      out_type = "f64[]"
    )
  )

  tmp_file <- tempfile(fileext = ".h")
  on.exit(unlink(tmp_file), add = TRUE)

  header <- mojor_export_c_header(built, tmp_file, namespace = "myns")

  expect_true(any(grepl("#define myns_test_kernel test_kernel", header)))
})

test_that("mojor_export_c_header handles different types", {  built <- list(
    kernel = "mixed_types",
    trans = list(
      types = list(
        x = "f64[]",
        y = "i32",
        z = "lgl[]"
      ),
      out_type = "f32"
    )
  )

  tmp_file <- tempfile(fileext = ".h")
  on.exit(unlink(tmp_file), add = TRUE)

  header <- mojor_export_c_header(built, tmp_file)

  expect_true(any(grepl("double\\* x", header)))
  expect_true(any(grepl("int y", header)))
  expect_true(any(grepl("int\\* z", header)))
  expect_true(any(grepl("void mixed_types\\(", header)))
  expect_true(any(grepl("float\\* out_ptr", header)))
})

test_that("mojor_export_c_header handles empty or missing-like na_mode", {  na_mode_cases <- list(character(0), NULL, NA_character_)
  kernel_names <- c("empty_na_mode", "null_na_mode", "na_char_na_mode")
  tmp_files <- character(0)
  on.exit(unlink(tmp_files), add = TRUE)

  for (i in seq_along(na_mode_cases)) {
    built <- list(
      kernel = kernel_names[[i]],
      trans = list(
        types = list(x = "f64[]"),
        out_type = "f64[]",
        na_mode = na_mode_cases[[i]]
      )
    )

    tmp_file <- tempfile(fileext = ".h")
    tmp_files <- c(tmp_files, tmp_file)

    header <- mojor_export_c_header(built, tmp_file)

    expect_true(any(grepl(paste0("void ", kernel_names[[i]], "\\("), header)))
    expect_false(any(grepl("__mojor_na_count_ptr", header)))
  }
})

test_that("mojor_export_c_header creates directory", {  built <- list(
    kernel = "nested_test",
    trans = list(
      types = list(x = "f64[]"),
      out_type = "f64[]"
    )
  )

  tmp_dir <- tempfile()
  tmp_file <- file.path(tmp_dir, "subdir", "test.h")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  header <- mojor_export_c_header(built, tmp_file)

  expect_true(file.exists(tmp_file))
})

test_that(".mojor_mojo_to_c_type converts types", {  mapping <- list(
    f64 = "double",
    "f64[]" = "double*",
    f32 = "float",
    "f32[]" = "float*",
    i32 = "int32_t",
    "i32[]" = "int32_t*",
    lgl = "int32_t",
    "lgl[]" = "int32_t*"
  )
  for (type in names(mapping)) {
    expect_equal(.mojor_mojo_to_c_type(type), mapping[[type]])
  }

  expect_null(.mojor_mojo_to_c_type("unsupported"))
})

test_that("mojor_declare_c returns declared info", {  result <- mojor_declare_c(
    name = "return_test",
    args = list(x = "f64*"),
    returns = "f64"
  )

  expect_equal(result$name, "return_test")
  expect_equal(result$args, list(x = "f64*"))
  expect_equal(result$returns, "f64")
})

test_that("mojor_c_call passes arguments", {  mojor_declare_c(
    name = "multi_arg",
    args = list(a = "f64", b = "i32", c = "f64*"),
    returns = "f64"
  )

  node <- mojor_c_call("multi_arg", 1.0, 2, c(1.0, 2.0, 3.0))
  expect_equal(node$kind, "c_call")
  expect_equal(length(node$args), 3)
})

test_that("mojor_c_call validates declared argument count", {  .with_declared_c_registry({
    mojor_declare_c(
      name = "arity_test",
      args = list(a = "f64", b = "f64"),
      returns = "f64"
    )

    expect_error(
      mojor_c_call("arity_test", 1.0),
      "argument count mismatch"
    )
    expect_error(
      mojor_c_call("arity_test", 1.0, 2.0, 3.0),
      "argument count mismatch"
    )
  })
})

test_that("mojor_c_call validates declared argument types", {  .with_declared_c_registry({
    mojor_declare_c(
      name = "type_test",
      args = list(a = "i32", b = "f64*"),
      returns = "f64"
    )

    expect_error(
      mojor_c_call("type_test", 1.5, c(1.0, 2.0)),
      "argument type mismatch"
    )
    expect_error(
      mojor_c_call("type_test", 1L, TRUE),
      "argument type mismatch"
    )
  })
})

# ============================================================================
# Phase 6.3: FFI-to-Mojo type mapping
# ============================================================================

test_that(".mojor_ffi_to_mojo_type maps scalar types correctly", {  mapping <- list(
    f64 = "Float64",
    f32 = "Float32",
    i32 = "Int32",
    lgl = "Int32"
  )
  for (type in names(mapping)) {
    expect_equal(.mojor_ffi_to_mojo_type(type), mapping[[type]])
  }
})

test_that(".mojor_ffi_to_mojo_type maps pointer types correctly", {  mapping <- list(
    "f64*" = "UnsafePointer[Float64]",
    "f32*" = "UnsafePointer[Float32]",
    "i32*" = "UnsafePointer[Int32]",
    "lgl*" = "UnsafePointer[Int32]"
  )
  for (type in names(mapping)) {
    expect_equal(.mojor_ffi_to_mojo_type(type), mapping[[type]])
  }
})

test_that(".mojor_ffi_to_mojo_type rejects unsupported types", {  expect_error(.mojor_ffi_to_mojo_type("bool"), "unsupported type")
  expect_error(.mojor_ffi_to_mojo_type("string"), "unsupported type")
})

test_that(".mojor_ffi_ir_type_compatible supports ND pointer ranks", {
  expect_true(.mojor_ffi_ir_type_compatible("f64*", "f64[]"))
  expect_true(.mojor_ffi_ir_type_compatible("f64*", "f64[,]"))
  expect_true(.mojor_ffi_ir_type_compatible("f64*", "f64[,,]"))
  expect_true(.mojor_ffi_ir_type_compatible("f64*", "f64[4d]"))
  expect_true(.mojor_ffi_ir_type_compatible("i32*", "lgl[3d]"))
  expect_true(.mojor_ffi_ir_type_compatible("lgl*", "bool[5d]"))

  expect_false(.mojor_ffi_ir_type_compatible("f64*", "f64"))
  expect_false(.mojor_ffi_ir_type_compatible("f64*", "i32[3d]"))
  expect_false(.mojor_ffi_ir_type_compatible("i32*", "f64[3d]"))
})

# ============================================================================
# Phase 6.3: AST builder recognizes mojor_c_call
# ============================================================================

test_that("AST builder creates c_call IR node from mojor_c_call", {  .with_declared_c_registry({
    mojor_declare_c("my_add", args = list(a = "f64", b = "f64"), returns = "f64")
    expr <- quote(mojor_c_call("my_add", x[i], 1.0))
    node <- .mojor_ir_expr_build(expr)

    expect_equal(node$kind, "c_call")
    expect_equal(node$name, "my_add")
    expect_equal(node$returns, "f64")
    expect_equal(length(node$args), 2)
    expect_equal(node$args[[1]]$kind, "index")
    expect_equal(node$args[[2]]$kind, "const")
  })
})

test_that("AST builder errors on undeclared mojor_c_call", {  .with_declared_c_registry({
    expr <- quote(mojor_c_call("nonexistent", x[i]))
    expect_error(.mojor_ir_expr_build(expr), "not declared")
  })
})

test_that("AST builder errors on mojor_c_call argument count mismatch", {  .with_declared_c_registry({
    mojor_declare_c("my_mul", args = list(a = "f64", b = "f64"), returns = "f64")

    expect_error(
      .mojor_ir_expr_build(quote(mojor_c_call("my_mul", x[i]))),
      "argument count mismatch"
    )
    expect_error(
      .mojor_ir_expr_build(quote(mojor_c_call("my_mul", x[i], 1.0, 2.0))),
      "argument count mismatch"
    )
  })
})

# ============================================================================
# Phase 6.3: Expression emission for c_call
# ============================================================================

test_that("c_call emits external_call with correct syntax", {  .with_declared_c_and_emit_state({
    mojor_declare_c("my_sqrt", args = list(x = "f64"), returns = "f64")

    node <- .mojor_ir_c_call("my_sqrt", list(.mojor_ir_var("val")), "f64", NULL)
    result <- .mojor_ir_expr_emit(node)

    expect_true(grepl('external_call\\["my_sqrt"', result))
    expect_true(grepl("Float64", result))
    expect_true(grepl("val", result))
    expect_true(.mojor_state$needs_ffi)
  })
})

test_that("c_call emits correct Mojo types for different return types", {  .with_declared_c_and_emit_state({
    mojor_declare_c("fn_f32", args = list(x = "f32"), returns = "f32")
    node <- .mojor_ir_c_call("fn_f32", list(.mojor_ir_var("v")), "f32", NULL)
    result <- .mojor_ir_expr_emit(node)
    expect_true(grepl("Float32", result))

    mojor_declare_c("fn_i32", args = list(x = "i32"), returns = "i32")
    node <- .mojor_ir_c_call("fn_i32", list(.mojor_ir_var("v")), "i32", NULL)
    result <- .mojor_ir_expr_emit(node)
    expect_true(grepl("Int32", result))
  })
})

test_that("c_call emits multiple arguments correctly", {  .with_declared_c_and_emit_state({
    mojor_declare_c("add3", args = list(a = "f64", b = "f64", c = "f64"), returns = "f64")

    node <- .mojor_ir_c_call("add3", list(
      .mojor_ir_var("x"), .mojor_ir_var("y"), .mojor_ir_const("1.0")
    ), "f64", NULL)
    result <- .mojor_ir_expr_emit(node)

    expect_true(grepl('external_call\\["add3"', result))
    expect_true(grepl("x, y, 1\\.0", result))
  })
})

test_that("c_call verifier rejects invalid metadata and arguments", {  verifier_cases <- list(
    list(
      name = "verify_add2",
      decl_args = list(a = "f64", b = "f64"),
      returns = "f64",
      node = function() {
        .mojor_ir_c_call("verify_add2", list(.mojor_ir_var("x")), "f64", NULL, expected_arity = 2L)
      },
      err = "argument count mismatch"
    ),
    list(
      name = "verify_i32",
      decl_args = list(a = "i32"),
      returns = "i32",
      node = function() {
        .mojor_ir_c_call(
          "verify_i32",
          list(.mojor_ir_const("1.5")),
          "i32",
          NULL,
          expected_arity = 1L,
          arg_types = c("i32"),
          arg_names = c("a")
        )
      },
      err = "argument type mismatch"
    ),
    list(
      name = "verify_ret",
      decl_args = list(a = "f64"),
      returns = "f64",
      node = function() {
        .mojor_ir_c_call(
          "verify_ret",
          list(.mojor_ir_var("x")),
          "i32",
          NULL,
          expected_arity = 1L,
          arg_types = c("f64"),
          arg_names = c("a")
        )
      },
      err = "return type mismatch"
    ),
    list(
      name = "verify_meta",
      decl_args = list(a = "f64"),
      returns = "f64",
      node = function() {
        .mojor_ir_c_call(
          "verify_meta",
          list(.mojor_ir_var("x")),
          "f64",
          NULL,
          expected_arity = 1L,
          arg_types = c("i32"),
          arg_names = c("a")
        )
      },
      err = "arg_types metadata mismatch"
    )
  )

  for (case in verifier_cases) {
    .with_declared_c_registry({
      mojor_declare_c(case$name, args = case$decl_args, returns = case$returns)
      expect_error(.mojor_ir_verify(case$node()), case$err, info = case$name)
    })
  }
})

test_that("c_call emitter rejects invalid metadata and arguments", {  emitter_cases <- list(
    list(
      name = "emit_add2",
      decl_args = list(a = "f64", b = "f64"),
      returns = "f64",
      node = function() .mojor_ir_c_call("emit_add2", list(.mojor_ir_var("x")), "f64", NULL),
      err = "argument count mismatch"
    ),
    list(
      name = "emit_i32",
      decl_args = list(a = "i32"),
      returns = "i32",
      node = function() .mojor_ir_c_call("emit_i32", list(.mojor_ir_const("1.5")), "i32", NULL),
      err = "argument type mismatch"
    ),
    list(
      name = "emit_ret",
      decl_args = list(a = "f64"),
      returns = "f64",
      node = function() .mojor_ir_c_call("emit_ret", list(.mojor_ir_var("x")), "i32", NULL),
      err = "return type mismatch"
    ),
    list(
      name = "emit_meta",
      decl_args = list(a = "f64"),
      returns = "f64",
      node = function() {
        .mojor_ir_c_call(
          "emit_meta",
          list(.mojor_ir_var("x")),
          "f64",
          NULL,
          arg_types = c("i32"),
          arg_names = c("a")
        )
      },
      err = "arg_types metadata mismatch"
    )
  )

  for (case in emitter_cases) {
    .with_declared_c_and_emit_state({
      mojor_declare_c(case$name, args = case$decl_args, returns = case$returns)
      expect_error(.mojor_ir_expr_emit(case$node()), case$err, info = case$name)
    })
  }
})

# ============================================================================
# Phase 6.3: Transpilation integration
# ============================================================================

test_that("mojor_transpile with mojor_c_call emits external_call and FFI import", {  .with_declared_c_registry({
    mojor_declare_c("my_scale", args = list(x = "f64", factor = "f64"), returns = "f64")

    f <- function(x) {
      out <- numeric(length(x))
      for (i in seq_along(x)) {
        out[i] <- mojor_c_call("my_scale", x[i], 2.0)
      }
      out
    }

    trans <- mojor_transpile(f, x = "f64[]")
    mojo <- trans$mojo

    expect_true(grepl('external_call\\["my_scale"', mojo))
    expect_true(grepl("Float64", mojo))
    expect_true(grepl("from sys\\.ffi import external_call", mojo))
  })
})

test_that("mojor_transpile without FFI does not emit FFI import", {  .with_declared_c_registry({
    f <- function(x) {
      out <- numeric(length(x))
      for (i in seq_along(x)) {
        out[i] <- x[i] * 2.0
      }
      out
    }

    trans <- mojor_transpile(f, x = "f64[]")
    expect_false(grepl("from sys\\.ffi import external_call", trans$mojo))
  })
})
