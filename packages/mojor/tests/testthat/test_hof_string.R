# =============================================================================
# Higher-Order Functions and String Basics
# =============================================================================
# Tests for vapply/sapply/lapply/mapply and nchar/nzchar/substr/paste
# =============================================================================

tier8_fun_unary <- quote(function(v) v)
tier8_fun_binary <- quote(function(a, b) a + b)

context("Higher-Order Functions - IR Nodes")
tier8_make_vapply <- function(fun = tier8_fun_unary, fun_value_type = "f64") {
  .mojor_ir_vapply(x = .mojor_ir_var("x"), fun = fun, fun_value_type = fun_value_type)
}

tier8_make_sapply <- function(fun = tier8_fun_unary) {
  .mojor_ir_sapply(x = .mojor_ir_var("x"), fun = fun)
}

tier8_make_lapply <- function(fun = tier8_fun_unary) {
  .mojor_ir_lapply(x = .mojor_ir_var("x"), fun = fun)
}

tier8_make_mapply <- function(args = list(.mojor_ir_var("x"), .mojor_ir_var("y"))) {
  .mojor_ir_mapply(fun = tier8_fun_binary, args = args)
}

test_that("IR node creation", {  skip_if_no_mojo()

  cases <- list(
    list(label = "vapply", node = tier8_make_vapply(), kind = "vapply", extra = function(n) expect_identical(n$fun_value_type, "f64")),
    list(label = "sapply", node = tier8_make_sapply(), kind = "sapply"),
    list(label = "lapply", node = tier8_make_lapply(), kind = "lapply"),
    list(label = "mapply", node = tier8_make_mapply(), kind = "mapply", extra = function(n) expect_length(n$args, 2))
  )

  for (case in cases) {
    expect_identical(case$node$kind, case$kind, info = case$label)
    if (!is.null(case$extra)) case$extra(case$node)
  }
})

context("Higher-Order Functions - Verifier")
test_that("verifier core contracts", {  skip_if_no_mojo()

  expect_silent(.mojor_ir_verify(tier8_make_vapply()))
  expect_silent(.mojor_ir_verify(tier8_make_sapply()))
  expect_silent(.mojor_ir_verify(tier8_make_mapply()))

  expect_error(
    .mojor_ir_verify(tier8_make_vapply(fun_value_type = c("f64", "i32"))),
    "fun_value_type must be a single character string"
  )
  expect_error(
    .mojor_ir_verify(tier8_make_vapply(fun_value_type = "str")),
    "fun_value_type must be numeric\\(1\\), integer\\(1\\), or logical\\(1\\)"
  )
  expect_error(
    .mojor_ir_verify(tier8_make_sapply(fun = quote(identity))),
    "named function 'identity' is unsupported"
  )
  expect_error(
    .mojor_ir_verify(tier8_make_mapply(args = "not_a_list")),
    "args must be a list"
  )
  expect_error(
    .mojor_ir_verify(tier8_make_mapply(args = list(.mojor_ir_var("x")))),
    "requires at least two vector arguments"
  )
  expect_error(
    .mojor_ir_verify(tier8_make_mapply(args = list(.mojor_ir_var("x"), .mojor_ir_const("1")))),
    "arguments must be direct vector variables"
  )
})

test_that("verifier accepts supported unary named FUN", {  skip_if_no_mojo()

  expect_silent(.mojor_ir_verify(
    .mojor_ir_vapply(
      x = .mojor_ir_var("x"),
      fun = quote(abs),
      fun_value_type = "f64"
    )
  ))
  expect_silent(.mojor_ir_verify(
    .mojor_ir_sapply(
      x = .mojor_ir_var("x"),
      fun = quote(sqrt)
    )
  ))
  expect_silent(.mojor_ir_verify(
    .mojor_ir_lapply(
      x = .mojor_ir_var("x"),
      fun = quote(sign)
    )
  ))
})

test_that("verifier enforces vector typing in strict ir_only mode", {  skip_if_no_mojo()

  expect_silent(.mojor_ir_verify(
    tier8_make_vapply(),
    ctx = list(ir_only = TRUE, type_env = list(x = "f64[]"))
  ))

  expect_error(
    .mojor_ir_verify(
      tier8_make_sapply(),
      ctx = list(ir_only = TRUE, type_env = list(x = "f64"))
    ),
    "x must have type 'f64\\[\\]' or 'i32\\[\\]' or 'lgl\\[\\]' or 'bool\\[\\]'"
  )

  expect_error(
    .mojor_ir_verify(
      tier8_make_mapply(),
      ctx = list(ir_only = TRUE, type_env = list(x = "f64[]", y = "unknown"))
    ),
    "args\\[\\[2\\]\\] must have type 'f64\\[\\]' or 'i32\\[\\]' or 'lgl\\[\\]' or 'bool\\[\\]'"
  )
})

context("Higher-Order Functions - Effects & Resources")
test_that("vapply() effect tracking", {  skip_if_no_mojo()
  
  node <- .mojor_ir_vapply(
    x = .mojor_ir_var("x"),
    fun = tier8_fun_unary,
    fun_value_type = "f64"
  )
  effects <- .mojor_ir_expr_effects(node)
  # vapply reads from x, so should have ReadsMem effect
  expect_true("ReadsMem" %in% effects || "Pure" %in% effects)
})

test_that("vapply() resource tracking", {  skip_if_no_mojo()
  
  node <- .mojor_ir_vapply(
    x = .mojor_ir_var("x"),
    fun = tier8_fun_unary,
    fun_value_type = "f64"
  )
  resources <- .mojor_ir_expr_resources(node)
  # Should have resource for x
  expect_length(resources, 1)
  expect_identical(resources[[1]]$resource_id, "var:x")
})

test_that("mapply() resource tracking", {  skip_if_no_mojo()
  
  node <- .mojor_ir_mapply(
    fun = tier8_fun_binary,
    args = list(.mojor_ir_var("x"), .mojor_ir_var("y"))
  )
  resources <- .mojor_ir_expr_resources(node)
  # Should have resources for both x and y
  expect_length(resources, 2)
  resource_ids <- sapply(resources, function(r) r$resource_id)
  expect_true("var:x" %in% resource_ids)
  expect_true("var:y" %in% resource_ids)
})

context("Higher-Order Functions - Key Generation")
test_that("vapply() key generation", {  skip_if_no_mojo()
  
  node <- .mojor_ir_vapply(
    x = .mojor_ir_var("x"),
    fun = tier8_fun_unary,
    fun_value_type = "f64"
  )
  key <- .mojor_ir_expr_key(node)
  expect_true(nzchar(key))
  expect_true(grepl("^vapply", key))
})

test_that("sapply() key generation", {  skip_if_no_mojo()
  
  node <- .mojor_ir_sapply(
    x = .mojor_ir_var("x"),
    fun = tier8_fun_unary
  )
  key <- .mojor_ir_expr_key(node)
  expect_true(nzchar(key))
  expect_true(grepl("^sapply", key))
})

context("Higher-Order Functions - Type Inference")
test_that("vapply() type inference", {  skip_if_no_mojo()
  
  node <- .mojor_ir_vapply(
    x = .mojor_ir_var("x"),
    fun = tier8_fun_unary,
    fun_value_type = "f64"
  )
  type <- .mojor_ir_infer_type(node, list(x = "f64[]"))
  expect_identical(type, "f64[]")
})

test_that("sapply/lapply/mapply type inference follows vector subset", {  skip_if_no_mojo()

  node_sapply <- .mojor_ir_sapply(
    x = .mojor_ir_var("x"),
    fun = tier8_fun_unary
  )
  node_lapply <- .mojor_ir_lapply(
    x = .mojor_ir_var("x"),
    fun = tier8_fun_unary
  )
  node_mapply <- .mojor_ir_mapply(
    fun = tier8_fun_binary,
    args = list(.mojor_ir_var("x"), .mojor_ir_var("y"))
  )

  expect_identical(.mojor_ir_infer_type(node_sapply, list(x = "i32[]")), "i32[]")
  expect_identical(.mojor_ir_infer_type(node_lapply, list(x = "lgl[]")), "lgl[]")
  expect_identical(.mojor_ir_infer_type(node_mapply, list(x = "f64[]", y = "f64[]")), "f64[]")
  expect_identical(.mojor_ir_infer_type(node_sapply, list(x = "bool[]")), "bool[]")
  expect_identical(.mojor_ir_infer_type(node_mapply, list(x = "bool[]", y = "bool[]")), "bool[]")
})

test_that("type query call inference returns chr[]", {  skip_if_no_mojo()

  node_typeof <- .mojor_ir_call("typeof", list(.mojor_ir_var("x")))
  node_mode <- .mojor_ir_call("mode", list(.mojor_ir_var("x")))
  node_class <- .mojor_ir_call("class", list(.mojor_ir_var("x")))

  expect_identical(.mojor_ir_infer_type(node_typeof, list(x = "f64[]")), "chr[]")
  expect_identical(.mojor_ir_infer_type(node_mode, list(x = "f64[]")), "chr[]")
  expect_identical(.mojor_ir_infer_type(node_class, list(x = "f64[]")), "chr[]")
})

test_that("strict type checker rejects compiled subset contract violations", {  skip_if_no_mojo()

  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = TRUE)

  bad_rhs <- .mojor_ir_sample(
    x = .mojor_ir_var("x"),
    size = .mojor_ir_var("size"),
    replace = FALSE
  )
  bad_assign <- .mojor_ir_assign(lhs = .mojor_ir_var("y"), rhs = bad_rhs)
  expect_error(
    .mojor_ir_type_check_stmt(
      bad_assign,
      list(y = "f64[]", x = "chr[]", size = "i32")
    ),
    "compiled subset node 'sample' type contract failed in assignment RHS: IR verify \\[sample\\]: x must have type 'f64\\[\\]' or 'i32\\[\\]' or 'lgl\\[\\]' or 'bool\\[\\]' in strict mode"
  )

  bad_return <- .mojor_ir_return(bad_rhs)
  expect_error(
    .mojor_ir_type_check_stmt(
      bad_return,
      list(x = "chr[]", size = "i32")
    ),
    "compiled subset node 'sample' type contract failed in return: IR verify \\[sample\\]: x must have type 'f64\\[\\]' or 'i32\\[\\]' or 'lgl\\[\\]' or 'bool\\[\\]' in strict mode"
  )

  mojor_options(ir_only = FALSE)
  expect_silent(.mojor_ir_type_check_stmt(
    bad_assign,
    list(y = "f64[]", x = "chr[]", size = "i32")
  ))
})

test_that("vapply() expression builder parses FUN.VALUE constructor forms", {  skip_if_no_mojo()

  node_i32 <- .mojor_ir_expr_build(quote(vapply(x, function(v) v + 1L, integer(1))))
  expect_false(is.null(node_i32))
  expect_identical(node_i32$kind, "vapply")
  expect_identical(node_i32$fun_value_type, "i32")

  node_lgl <- .mojor_ir_expr_build(quote(vapply(x, function(v) v > 0, FUN.VALUE = logical(1))))
  expect_false(is.null(node_lgl))
  expect_identical(node_lgl$kind, "vapply")
  expect_identical(node_lgl$fun_value_type, "lgl")
})

test_that("HOF assignment emitter supports unary named FUN", {  skip_if_no_mojo()

  lines <- .mojor_ir_tier8_hof_assign_emit(
    .mojor_ir_sapply(
      x = .mojor_ir_var("x"),
      fun = quote(abs)
    ),
    out_name = "out",
    type_env = list(x = "f64[]", out = "f64[]")
  )
  expect_false(is.null(lines))
  expect_true(any(grepl("abs\\(", lines)))
})

test_that("expression-only helper supports unary named FUN", {  skip_if_no_mojo()

  info <- .mojor_extract_hof_inline_fun(quote(abs), "sapply", expected_arity = 1L)
  expect_identical(info$param_names, c("__mojor_hof_arg1"))
  expect_identical(info$body_ir$kind, "call")
  expect_identical(info$body_ir$fn, "abs")
})

context("String Basics - Expression Builder")
test_that("substring() expression builder parses named and aliased arguments", {  skip_if_no_mojo()

  node_named <- .mojor_ir_expr_build(quote(substring(text = x, first = start, last = stop)))
  expect_false(is.null(node_named))
  expect_identical(node_named$kind, "substr")
  expect_identical(node_named$x$kind, "var")
  expect_identical(node_named$x$name, "x")
  expect_identical(node_named$start$kind, "var")
  expect_identical(node_named$start$name, "start")
  expect_identical(node_named$stop$kind, "var")
  expect_identical(node_named$stop$name, "stop")

  node_alias <- .mojor_ir_expr_build(quote(substring(x = x, start = start, stop = stop)))
  expect_false(is.null(node_alias))
  expect_identical(node_alias$kind, "substr")
})

test_that("paste()/paste0() expression builder excludes named args from concat inputs", {  skip_if_no_mojo()

  node_paste <- .mojor_ir_expr_build(quote(paste(x, y, sep = "-", collapse = NULL)))
  expect_false(is.null(node_paste))
  expect_identical(node_paste$kind, "paste")
  expect_length(node_paste$args, 2)
  expect_identical(node_paste$sep, "\"-\"")
  expect_identical(node_paste$collapse, "NULL")

  node_paste0 <- .mojor_ir_expr_build(quote(paste0(x, y, collapse = ":")))
  expect_false(is.null(node_paste0))
  expect_identical(node_paste0$kind, "paste")
  expect_length(node_paste0$args, 2)
  expect_identical(node_paste0$sep, "\"\"")
  expect_identical(node_paste0$collapse, "\":\"")
})

test_that("expression builder rejects malformed named argument forms", {  skip_if_no_mojo()

  expect_null(.mojor_ir_expr_build(quote(substring(x, first = start, last = stop, extra = 1L))))
  expect_null(.mojor_ir_expr_build(quote(paste(x, y, sep = sep_var))))
  expect_null(.mojor_ir_expr_build(quote(paste0(x, y, collapse = sep_var))))
})

context("String Basics - IR Nodes")
tier8_make_nchar <- function() .mojor_ir_nchar(x = .mojor_ir_var("x"))
tier8_make_nzchar <- function() .mojor_ir_nzchar(x = .mojor_ir_var("x"))
tier8_make_substr <- function() .mojor_ir_substr(
  x = .mojor_ir_var("x"),
  start = .mojor_ir_const("1"),
  stop = .mojor_ir_const("5")
)
tier8_make_paste <- function() .mojor_ir_paste(
  args = list(.mojor_ir_var("x"), .mojor_ir_var("y")),
  sep = "\" \"",
  collapse = NULL
)
tier8_make_paste0 <- function() .mojor_ir_paste0(
  args = list(.mojor_ir_var("x"), .mojor_ir_var("y")),
  collapse = NULL
)

test_that("IR node creation", {  skip_if_no_mojo()

  cases <- list(
    list(label = "nchar", node = tier8_make_nchar(), kind = "nchar"),
    list(label = "nzchar", node = tier8_make_nzchar(), kind = "nzchar"),
    list(label = "substr", node = tier8_make_substr(), kind = "substr"),
    list(label = "paste", node = tier8_make_paste(), kind = "paste", extra = function(n) expect_length(n$args, 2)),
    list(label = "paste0", node = tier8_make_paste0(), kind = "paste", extra = function(n) expect_identical(n$sep, "\"\""))
  )

  for (case in cases) {
    expect_identical(case$node$kind, case$kind, info = case$label)
    if (!is.null(case$extra)) case$extra(case$node)
  }
})

context("String Basics - Verifier")
test_that("verifier valid inputs", {  skip_if_no_mojo()

  expect_silent(.mojor_ir_verify(tier8_make_nchar()))
  expect_silent(.mojor_ir_verify(tier8_make_substr()))
  expect_silent(.mojor_ir_verify(tier8_make_paste()))
})

test_that("paste() verifier - invalid sep", {  skip_if_no_mojo()
  
  node <- .mojor_ir_paste(
    args = list(.mojor_ir_var("x")),
    sep = c("\" \"", "\"-\""),
    collapse = NULL
  )
  expect_error(.mojor_ir_verify(node), "sep must be a single character string")
})

test_that("verifier enforces chr[] inputs in strict ir_only mode", {  skip_if_no_mojo()

  expect_error(
    .mojor_ir_verify(
      .mojor_ir_nchar(x = .mojor_ir_var("x")),
      ctx = list(ir_only = TRUE)
    ),
    "x must have type 'chr\\[\\]'"
  )

  expect_silent(.mojor_ir_verify(
    .mojor_ir_nchar(x = .mojor_ir_var("x")),
    ctx = list(ir_only = TRUE, type_env = list(x = "chr[]"))
  ))

  expect_error(
    .mojor_ir_verify(
      .mojor_ir_nchar(x = .mojor_ir_var("x")),
      ctx = list(ir_only = TRUE, type_env = list(x = "f64[]"))
    ),
    "x must have type 'chr\\[\\]'"
  )

  expect_error(
    .mojor_ir_verify(
      .mojor_ir_paste(args = list(.mojor_ir_var("x"), .mojor_ir_var("y")), sep = "\" \""),
      ctx = list(ir_only = TRUE, type_env = list(x = "chr[]", y = "i32[]"))
    ),
    "args\\[\\[2\\]\\] must have type 'chr\\[\\]'"
  )

  expect_silent(
    .mojor_ir_verify(
      .mojor_ir_substr(
        x = .mojor_ir_var("x"),
        start = .mojor_ir_var("start"),
        stop = .mojor_ir_var("stop")
      ),
      ctx = list(ir_only = TRUE, type_env = list(x = "chr[]", start = "i32", stop = "i32"))
    )
  )

  expect_error(
    .mojor_ir_verify(
      .mojor_ir_substr(
        x = .mojor_ir_var("x"),
        start = .mojor_ir_var("start"),
        stop = .mojor_ir_var("stop")
      ),
      ctx = list(ir_only = TRUE, type_env = list(x = "chr[]", start = "f64", stop = "i32"))
    ),
    "start must have type 'i32'"
  )

  expect_error(
    .mojor_ir_verify(
      .mojor_ir_substr(
        x = .mojor_ir_var("x"),
        start = .mojor_ir_var("start"),
        stop = .mojor_ir_var("stop")
      ),
      ctx = list(ir_only = TRUE, type_env = list(x = "chr[]", start = "i32", stop = "f64"))
    ),
    "stop must have type 'i32'"
  )
})

test_that("strict transpile IR preparation enforces typing contracts", {  skip_if_no_mojo()

  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)

  node <- .mojor_ir_nchar(x = .mojor_ir_var("x"))
  dim_map <- list()

  mojor_options(ir_only = TRUE)
  expect_error(
    .mojor_prepare_transpile_ir_stmt(
      node,
      local_types = list(x = "f64[]"),
      dim_map = dim_map,
      verify_strict = TRUE
    ),
    "x must have type 'chr\\[\\]'"
  )

  mojor_options(ir_only = FALSE)
  prepared <- .mojor_prepare_transpile_ir_stmt(
    node,
    local_types = list(x = "f64[]"),
    dim_map = dim_map,
    verify_strict = TRUE
  )
  expect_false(is.null(prepared))
  expect_identical(prepared$kind, "nchar")
})

test_that("strict transpile IR preparation surfaces normalize failures", {  skip_if_no_mojo()

  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = TRUE)

  old_normalize <- .mojor_ir_normalize
  on.exit(assign(".mojor_ir_normalize", old_normalize, envir = .GlobalEnv), add = TRUE)
  assign(".mojor_ir_normalize", function(node) stop("normalize boom"), envir = .GlobalEnv)

  expect_error(
    .mojor_prepare_transpile_ir_stmt(
      .mojor_ir_nchar(x = .mojor_ir_var("x")),
      local_types = list(x = "chr[]"),
      dim_map = list(),
      verify_strict = TRUE
    ),
    "strict IR normalize failed: normalize boom"
  )
})

test_that("strict transpile type+emit helper surfaces type and emission diagnostics", {  skip_if_no_mojo()

  old_ir_only <- isTRUE(mojor_options("ir_only")$ir_only)
  on.exit(mojor_options(ir_only = old_ir_only), add = TRUE)
  mojor_options(ir_only = TRUE)

  idx_assign <- .mojor_ir_assign(
    lhs = .mojor_ir_index(
      base = .mojor_ir_var("out"),
      indices = list(.mojor_ir_const("1"))
    ),
    rhs = .mojor_ir_call("sin", list(.mojor_ir_const("1.0")))
  )
  expect_error(
    .mojor_transpile_type_and_emit_stmt(
      ir = idx_assign,
      local_types = list(out = "i32[]"),
      out_name = "out",
      na_mode = "forbid",
      bounds_check = FALSE,
      scalar_name = NULL,
      schedule = NULL
    ),
    "strict IR type check failed: mojor_transpile: integer output requires explicit cast"
  )

  nchar_expr <- .mojor_ir_nchar(x = .mojor_ir_var("x"))
  expect_error(
    .mojor_transpile_type_and_emit_stmt(
      ir = nchar_expr,
      local_types = list(x = "chr[]"),
      out_name = "out",
      na_mode = "forbid",
      bounds_check = FALSE,
      scalar_name = NULL,
      schedule = NULL
    ),
    "strict IR emission produced empty output"
  )
})

context("String Basics - Effects & Resources")
test_that("nchar() effect tracking", {  skip_if_no_mojo()
  
  node <- .mojor_ir_nchar(
    x = .mojor_ir_var("x")
  )
  effects <- .mojor_ir_expr_effects(node)
  expect_true("ReadsMem" %in% effects || "Pure" %in% effects)
})

test_that("substr() resource tracking", {  skip_if_no_mojo()
  
  node <- .mojor_ir_substr(
    x = .mojor_ir_var("x"),
    start = .mojor_ir_const("1"),
    stop = .mojor_ir_const("5")
  )
  resources <- .mojor_ir_expr_resources(node)
  # Should have resources for x only (start/stop are constants, no resources)
  expect_length(resources, 1)
  expect_identical(resources[[1]]$resource_id, "var:x")
})

context("String Basics - Type Inference")
test_that("type inference on chr[] inputs", {  skip_if_no_mojo()

  cases <- list(
    list(label = "nchar", node = tier8_make_nchar(), env = list(x = "chr[]"), expected = "i32[]"),
    list(label = "nzchar", node = tier8_make_nzchar(), env = list(x = "chr[]"), expected = "lgl[]"),
    list(label = "substr", node = tier8_make_substr(), env = list(x = "chr[]"), expected = "chr[]"),
    list(label = "paste", node = tier8_make_paste(), env = list(x = "chr[]", y = "chr[]"), expected = "chr[]")
  )
  for (case in cases) {
    expect_identical(.mojor_ir_infer_type(case$node, case$env), case$expected, info = case$label)
  }
})

test_that("type inference returns unknown for non-chr inputs", {  skip_if_no_mojo()

  node_nchar <- .mojor_ir_nchar(x = .mojor_ir_var("x"))
  expect_identical(.mojor_ir_infer_type(node_nchar, list(x = "f64[]")), "unknown")

  node_substr <- .mojor_ir_substr(
    x = .mojor_ir_var("x"),
    start = .mojor_ir_const("1"),
    stop = .mojor_ir_const("2")
  )
  expect_identical(.mojor_ir_infer_type(node_substr, list(x = "i32[]")), "unknown")

  node_paste <- .mojor_ir_paste(args = list(.mojor_ir_var("x"), .mojor_ir_var("y")), sep = "\" \"")
  expect_identical(.mojor_ir_infer_type(node_paste, list(x = "chr[]", y = "f64[]")), "unknown")
})

context("String Basics - Emit")
test_that("scalar string emit requires loop/index context", {  skip_if_no_mojo()

  cases <- list(
    list(node = tier8_make_nchar(), err = "nchar\\(\\) scalar expression emission is unsupported outside loop/index context"),
    list(node = tier8_make_nzchar(), err = "nzchar\\(\\) scalar expression emission is unsupported outside loop/index context"),
    list(node = tier8_make_substr(), err = "substr\\(\\) scalar expression emission is unsupported outside loop/index context")
  )
  for (case in cases) {
    expect_error(
      .mojor_ir_expr_emit(case$node, zero_based_vars = NULL, type_env = list(x = "chr[]"), loop_vars = NULL),
      case$err
    )
  }
})

test_that("paste()/paste0() emit requires loop context and supports indexed element emission", {  skip_if_no_mojo()

  node_paste <- .mojor_ir_paste(
    args = list(.mojor_ir_var("x"), .mojor_ir_var("y")),
    sep = "\"|\"",
    collapse = NULL
  )
  expect_error(
    .mojor_ir_expr_emit(node_paste, zero_based_vars = NULL, type_env = list(x = "chr[]", y = "chr[]"), loop_vars = NULL),
    "paste\\(\\)/paste0\\(\\) scalar expression emission is unsupported outside loop/index context"
  )
  emit_paste <- .mojor_ir_expr_emit(node_paste, zero_based_vars = NULL, type_env = list(x = "chr[]", y = "chr[]"), loop_vars = "i")
  expect_true(grepl("x[Int(i - 1)]", emit_paste, fixed = TRUE))
  expect_true(grepl("+ \"|\" +", emit_paste, fixed = TRUE))
  expect_true(grepl("y[Int(i - 1)]", emit_paste, fixed = TRUE))

  node_paste0 <- .mojor_ir_paste0(
    args = list(.mojor_ir_var("x"), .mojor_ir_var("y")),
    collapse = NULL
  )
  emit_paste0 <- .mojor_ir_expr_emit(node_paste0, zero_based_vars = NULL, type_env = list(x = "chr[]", y = "chr[]"), loop_vars = "i")
  expect_true(grepl("+ \"\" +", emit_paste0, fixed = TRUE))
})

test_that("whole-vector string assign emit supports paste()/paste0() with and without collapse", {  skip_if_no_mojo()

  old_len_var_map <- .mojor_state$current_len_var_map
  on.exit(.mojor_state$current_len_var_map <- old_len_var_map, add = TRUE)
  .mojor_state$current_len_var_map <- list(x = "n_x", y = "n_y", out = "n_out")

  node_paste <- .mojor_ir_paste(
    args = list(.mojor_ir_var("x"), .mojor_ir_var("y")),
    sep = "\"|\"",
    collapse = NULL
  )
  lines_paste <- .mojor_ir_tier8_string_assign_emit(
    node_paste,
    out_name = "out",
    type_env = list(x = "chr[]", y = "chr[]", out = "chr[]")
  )
  expect_false(is.null(lines_paste))
  expect_true(any(grepl("for __mojor_str_i", lines_paste, fixed = TRUE)))
  expect_true(any(grepl("out[__mojor_str_i", lines_paste, fixed = TRUE)))
  expect_true(any(grepl("+ \"|\" +", lines_paste, fixed = TRUE)))

  node_paste0 <- .mojor_ir_paste0(
    args = list(.mojor_ir_var("x"), .mojor_ir_var("y")),
    collapse = NULL
  )
  lines_paste0 <- .mojor_ir_tier8_string_assign_emit(
    node_paste0,
    out_name = "out",
    type_env = list(x = "chr[]", y = "chr[]", out = "chr[]")
  )
  expect_false(is.null(lines_paste0))
  expect_true(any(grepl("+ \"\" +", lines_paste0, fixed = TRUE)))

  node_paste_collapse <- .mojor_ir_paste(
    args = list(.mojor_ir_var("x"), .mojor_ir_var("y")),
    sep = "\"|\"",
    collapse = "\"::\""
  )
  lines_paste_collapse <- .mojor_ir_tier8_string_assign_emit(
    node_paste_collapse,
    out_name = "out",
    type_env = list(x = "chr[]", y = "chr[]", out = "chr[]")
  )
  expect_false(is.null(lines_paste_collapse))
  expect_true(any(grepl("var __mojor_str_acc", lines_paste_collapse, fixed = TRUE)))
  expect_true(any(grepl("if __mojor_str_i > 0", lines_paste_collapse, fixed = TRUE)))
  expect_true(any(grepl("+ \"::\"", lines_paste_collapse, fixed = TRUE)))
  expect_true(any(grepl("out[Int(0)] = ", lines_paste_collapse, fixed = TRUE)))
})

context("Sampling")
test_that("sample.int() IR node creation", {  skip_if_no_mojo()
  
  node <- .mojor_ir_sample_int(
    n = .mojor_ir_const("10"),
    size = .mojor_ir_const("5"),
    replace = FALSE
  )
  expect_identical(node$kind, "sample_int")
})

test_that("sample() IR node creation", {  skip_if_no_mojo()
  
  node <- .mojor_ir_sample(
    x = .mojor_ir_var("x"),
    size = .mojor_ir_const("5"),
    replace = FALSE
  )
  expect_identical(node$kind, "sample")
})

test_that("verifier enforces strict subset in ir_only mode", {  skip_if_no_mojo()

  expect_silent(.mojor_ir_verify(
    .mojor_ir_sample_int(
      n = .mojor_ir_var("n"),
      size = .mojor_ir_var("size"),
      replace = FALSE
    ),
    ctx = list(ir_only = TRUE, type_env = list(n = "i32", size = "i32"))
  ))

  expect_silent(
    .mojor_ir_verify(
      .mojor_ir_sample_int(
        n = .mojor_ir_var("n"),
        size = .mojor_ir_var("size"),
        replace = TRUE,
        prob = .mojor_ir_var("p")
      ),
      ctx = list(ir_only = TRUE, type_env = list(n = "i32", size = "i32", p = "f64[]"))
    )
  )
  expect_silent(
    .mojor_ir_verify(
      .mojor_ir_sample_int(
        n = .mojor_ir_var("n"),
        size = .mojor_ir_var("size"),
        replace = FALSE,
        prob = .mojor_ir_var("p")
      ),
      ctx = list(ir_only = TRUE, type_env = list(n = "i32", size = "i32", p = "f64[]"))
    )
  )

  expect_silent(.mojor_ir_verify(
    .mojor_ir_sample(
      x = .mojor_ir_var("x"),
      size = .mojor_ir_var("size"),
      replace = TRUE
    ),
    ctx = list(ir_only = TRUE, type_env = list(x = "f64[]", size = "i32"))
  ))

  expect_silent(.mojor_ir_verify(
    .mojor_ir_sample(
      x = .mojor_ir_var("x"),
      size = .mojor_ir_var("size"),
      replace = TRUE
    ),
    ctx = list(ir_only = TRUE, type_env = list(x = "bool[]", size = "i32"))
  ))

  expect_error(
    .mojor_ir_verify(
      .mojor_ir_sample(
        x = .mojor_ir_index(base = .mojor_ir_var("x"), indices = list(.mojor_ir_const("1"))),
        size = .mojor_ir_var("size"),
        replace = TRUE
      ),
      ctx = list(ir_only = TRUE, type_env = list(x = "f64[]", size = "i32"))
    ),
    "direct vector variable"
  )

  expect_silent(
    .mojor_ir_verify(
      .mojor_ir_sample(
        x = .mojor_ir_var("x"),
        size = .mojor_ir_var("size"),
        replace = TRUE,
        prob = .mojor_ir_var("p")
      ),
      ctx = list(ir_only = TRUE, type_env = list(x = "f64[]", size = "i32", p = "f64[]"))
    )
  )
  expect_silent(
    .mojor_ir_verify(
      .mojor_ir_sample(
        x = .mojor_ir_var("x"),
        size = .mojor_ir_var("size"),
        replace = FALSE,
        prob = .mojor_ir_var("p")
      ),
      ctx = list(ir_only = TRUE, type_env = list(x = "f64[]", size = "i32", p = "f64[]"))
    )
  )
})

test_that("type inference tracks sampling outputs", {  skip_if_no_mojo()

  node_sample_int <- .mojor_ir_sample_int(
    n = .mojor_ir_var("n"),
    size = .mojor_ir_var("size"),
    replace = FALSE
  )
  expect_identical(
    .mojor_ir_infer_type(node_sample_int, type_env = list(n = "i32", size = "i32")),
    "i32[]"
  )

  node_sample <- .mojor_ir_sample(
    x = .mojor_ir_var("x"),
    size = .mojor_ir_var("size"),
    replace = FALSE
  )
  expect_identical(
    .mojor_ir_infer_type(node_sample, type_env = list(x = "f64[]", size = "i32")),
    "f64[]"
  )
  expect_identical(
    .mojor_ir_infer_type(node_sample, type_env = list(x = "i32[]", size = "i32")),
    "i32[]"
  )
  expect_identical(
    .mojor_ir_infer_type(node_sample, type_env = list(x = "lgl[]", size = "i32")),
    "lgl[]"
  )
  expect_identical(
    .mojor_ir_infer_type(node_sample, type_env = list(x = "bool[]", size = "i32")),
    "bool[]"
  )
  expect_identical(
    .mojor_ir_infer_type(node_sample, type_env = list(x = "chr[]", size = "i32")),
    "unknown"
  )
  expect_identical(
    .mojor_ir_infer_type(node_sample, type_env = list(x = "f64[]", size = "f64")),
    "unknown"
  )
})

context("Higher-Order Functions - Compiled Subset")
test_that("lapply/mapply compiled subset matches base R", {  skip_if_no_mojo()

  f_lapply <- mojor_fn(
    function(x) lapply(x, function(v) v * v),
    x = "f64[]",
    object_mode = "off",
    cache = FALSE
  )
  x <- c(1, 2, 3, 4)
  expect_equal(unname(f_lapply(x)), unname(unlist(lapply(x, function(v) v * v))))

  f_mapply <- mojor_fn(
    function(x, y) mapply(function(a, b) a - b, x, y),
    x = "f64[]",
    y = "f64[]",
    object_mode = "off",
    cache = FALSE
  )
  expect_equal(unname(f_mapply(c(4, 5, 6), c(1, 2, 3))), unname(mapply(function(a, b) a - b, c(4, 5, 6), c(1, 2, 3))))
})

test_that("strict subset rejects unsupported forms under ir_only", {  skip_if_no_mojo()

  expect_error(
    mojor_fn(
      function(x, y) mapply(function(a, b) list(a, b), x, y),
      x = "f64[]",
      y = "f64[]",
      object_mode = "off",
      ir_only = TRUE,
      cache = FALSE
    ),
    "list\\(\\.\\.\\.\\) return values are unsupported|FUN body is not inlineable"
  )

  expect_error(
    mojor_fn(
      function(x, y) mapply(function(a, b) a + b, x[1:2], y),
      x = "f64[]",
      y = "f64[]",
      object_mode = "off",
      ir_only = TRUE,
      cache = FALSE
    ),
    "direct vector arguments"
  )
})
