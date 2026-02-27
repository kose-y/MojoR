# Test suite for IR verifier invariant checks
# This file validates that .mojor_ir_verify() catches violations of IR invariants

test_that("break statement only allowed inside loops", {  # Invalid: break outside loop
  ir <- .mojor_ir_block(list(.mojor_ir_break()))

  expect_error(
    .mojor_ir_verify(ir, list(in_loop = FALSE)),
    "break statement outside loop"
  )

  # Valid: break inside loop
  ir <- .mojor_ir_loop(
    var = "i",
    range = .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10")),
    body = .mojor_ir_block(list(.mojor_ir_break()))
  )
  expect_silent(.mojor_ir_verify(ir))
})

test_that("next statement only allowed inside loops", {  # Invalid: next outside loop
  ir <- .mojor_ir_block(list(.mojor_ir_next()))

  expect_error(
    .mojor_ir_verify(ir, list(in_loop = FALSE)),
    "next statement outside loop"
  )

  # Valid: next inside loop
  ir <- .mojor_ir_loop(
    var = "i",
    range = .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10")),
    body = .mojor_ir_block(list(.mojor_ir_next()))
  )
  expect_silent(.mojor_ir_verify(ir))
})

test_that("break allowed in while loops", {  ir <- .mojor_ir_while(
    cond = .mojor_ir_const("TRUE"),
    body = .mojor_ir_block(list(.mojor_ir_break()))
  )
  expect_silent(.mojor_ir_verify(ir))
})

test_that("next allowed in repeat loops", {  ir <- .mojor_ir_repeat(
    body = .mojor_ir_block(list(.mojor_ir_next()))
  )
  expect_silent(.mojor_ir_verify(ir))
})

test_that("unreachable code after return is detected", {  # Invalid: statement after return
  ir <- .mojor_ir_block(list(
    .mojor_ir_return(.mojor_ir_const("0")),
    .mojor_ir_assign(
      .mojor_ir_var("x"),
      .mojor_ir_const("1")
    )
  ))

  expect_error(
    .mojor_ir_verify(ir, list(check_scope = TRUE, defined_vars = character())),
    "unreachable statement at position 2 after return"
  )
})

test_that("unreachable code with multiple statements", {  # Invalid: multiple statements after return
  ir <- .mojor_ir_block(list(
    .mojor_ir_assign(.mojor_ir_var("x"), .mojor_ir_const("1")),
    .mojor_ir_return(.mojor_ir_var("x")),
    .mojor_ir_assign(.mojor_ir_var("y"), .mojor_ir_const("2")),
    .mojor_ir_assign(.mojor_ir_var("z"), .mojor_ir_const("3"))
  ))

  expect_error(
    .mojor_ir_verify(ir, list(check_scope = TRUE, defined_vars = character())),
    "unreachable statement at position 3 after return"
  )
})

test_that("redundant cast warning", {  # Cast from f64 to f64 should warn
  ir <- .mojor_ir_cast(
    to = "f64",
    expr = .mojor_ir_var("x")
  )

  type_env <- list(x = "f64")

  expect_warning(
    .mojor_ir_verify(ir, list(type_env = type_env)),
    "redundant cast from 'f64' to 'f64'"
  )
})

test_that("redundant cast with array types", {  # Cast from f64[] to f64[] should warn (base types match)
  ir <- .mojor_ir_cast(
    to = "f64[]",
    expr = .mojor_ir_var("x")
  )

  type_env <- list(x = "f64[]")

  expect_warning(
    .mojor_ir_verify(ir, list(type_env = type_env)),
    "redundant cast.*same base type"
  )
})

test_that("non-redundant cast is silent", {  # Cast from i32 to f64 is valid
  ir <- .mojor_ir_cast(
    to = "f64",
    expr = .mojor_ir_var("x")
  )

  type_env <- list(x = "i32")

  expect_silent(.mojor_ir_verify(ir, list(type_env = type_env)))
})

test_that("call arity validation for unary functions", {  # Invalid: sin() with 2 arguments
  ir <- .mojor_ir_call(
    fn = "sin",
    args = list(.mojor_ir_var("x"), .mojor_ir_var("y"))
  )

  expect_error(
    .mojor_ir_verify(ir),
    "sin.*expects 1 argument, got 2"
  )
})

test_that("call arity validation for binary functions", {  # Invalid: min() with 1 argument
  ir <- .mojor_ir_call(
    fn = "min",
    args = list(.mojor_ir_var("x"))
  )

  # In R, min() supports 1 or 2 scalar arguments; this should pass.
  expect_silent(.mojor_ir_verify(ir))

  # Invalid: max() with 3 arguments
  ir <- .mojor_ir_call(
    fn = "max",
    args = list(.mojor_ir_var("x"), .mojor_ir_var("y"), .mojor_ir_var("z"))
  )

  expect_error(
    .mojor_ir_verify(ir),
    "max.*expects 1 or 2 arguments, got 3"
  )
})

test_that("valid unary call passes", {  ir <- .mojor_ir_call(fn = "sqrt", args = list(.mojor_ir_var("x")))
  expect_silent(.mojor_ir_verify(ir))
})

test_that("valid binary call passes", {  ir <- .mojor_ir_call(
    fn = "min",
    args = list(.mojor_ir_var("x"), .mojor_ir_var("y"))
  )
  expect_silent(.mojor_ir_verify(ir))
})

test_that("unknown function names are allowed (no arity check)", {  # Unknown functions don't have arity validation
  ir <- .mojor_ir_call(
    fn = "custom_fn",
    args = list(.mojor_ir_var("x"), .mojor_ir_var("y"), .mojor_ir_var("z"))
  )
  expect_silent(.mojor_ir_verify(ir))
})

test_that("empty index list is rejected", {  # Invalid: x[] with no indices
  ir <- .mojor_ir_index(
    base = .mojor_ir_var("x"),
    indices = list()
  )

  expect_error(
    .mojor_ir_verify(ir),
    "indices list cannot be empty"
  )
})

test_that("array-valued index expression allows selector vectors", {  # Valid: x[y] where y is an integer selector vector
  ir <- .mojor_ir_index(
    base = .mojor_ir_var("x"),
    indices = list(.mojor_ir_var("y"))
  )

  type_env <- list(x = "f64[]", y = "i32[]")

  expect_silent(.mojor_ir_verify(ir, list(type_env = type_env)))
})

test_that("logical mask index is allowed", {  # Valid: x[mask] where mask is logical
  ir <- .mojor_ir_index(
    base = .mojor_ir_var("x"),
    indices = list(.mojor_ir_var("mask"))
  )

  type_env <- list(x = "f64[]", mask = "lgl[]")

  expect_silent(.mojor_ir_verify(ir, list(type_env = type_env)))
})

test_that("scalar index is allowed", {  # Valid: x[i] where i is scalar
  ir <- .mojor_ir_index(
    base = .mojor_ir_var("x"),
    indices = list(.mojor_ir_var("i"))
  )

  type_env <- list(x = "f64[]", i = "i32")

  expect_silent(.mojor_ir_verify(ir, list(type_env = type_env)))
})

test_that("range with zero step is rejected", {  # Invalid: range with step = 0
  ir <- .mojor_ir_range(
    start = .mojor_ir_const("1"),
    end = .mojor_ir_const("10"),
    step = .mojor_ir_const("0")
  )

  expect_error(
    .mojor_ir_verify(ir),
    "step cannot be zero"
  )
})

test_that("empty range with positive step warns", {  # Warning: start > end with positive step
  ir <- .mojor_ir_range(
    start = .mojor_ir_const("10"),
    end = .mojor_ir_const("1"),
    step = .mojor_ir_const("1")
  )

  expect_warning(
    .mojor_ir_verify(ir),
    "empty range.*start=10.*end=1.*positive step"
  )
})

test_that("empty range with negative step warns", {  # Warning: start < end with negative step
  ir <- .mojor_ir_range(
    start = .mojor_ir_const("1"),
    end = .mojor_ir_const("10"),
    step = .mojor_ir_const("-1")
  )

  expect_warning(
    .mojor_ir_verify(ir),
    "empty range.*start=1.*end=10.*negative step"
  )
})

test_that("valid descending range is silent", {  # Valid: start > end with negative step
  ir <- .mojor_ir_range(
    start = .mojor_ir_const("10"),
    end = .mojor_ir_const("1"),
    step = .mojor_ir_const("-1")
  )

  expect_silent(.mojor_ir_verify(ir))
})

test_that("valid ascending range is silent", {  # Valid: start < end with positive step (default)
  ir <- .mojor_ir_range(
    start = .mojor_ir_const("1"),
    end = .mojor_ir_const("10")
  )

  expect_silent(.mojor_ir_verify(ir))
})

test_that("loop variable shadowing is rejected", {  # Invalid: nested loop with same variable name
  inner_loop <- .mojor_ir_loop(
    var = "i",
    range = .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("5")),
    body = .mojor_ir_block(list())
  )

  outer_loop <- .mojor_ir_loop(
    var = "i",
    range = .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10")),
    body = inner_loop
  )

  expect_error(
    .mojor_ir_verify(outer_loop),
    "var 'i' shadows outer loop variable"
  )
})

test_that("different loop variable names are allowed", {  # Valid: nested loops with different variable names
  inner_loop <- .mojor_ir_loop(
    var = "j",
    range = .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("5")),
    body = .mojor_ir_block(list())
  )

  outer_loop <- .mojor_ir_loop(
    var = "i",
    range = .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10")),
    body = inner_loop
  )

  expect_silent(.mojor_ir_verify(outer_loop))
})

test_that("use before define is caught with check_scope", {  # Invalid: use x before it's defined
  ir <- .mojor_ir_block(list(
    .mojor_ir_assign(
      .mojor_ir_var("y"),
      .mojor_ir_var("x")  # x not defined yet
    ),
    .mojor_ir_assign(
      .mojor_ir_var("x"),
      .mojor_ir_const("1")
    )
  ))

  expect_error(
    .mojor_ir_verify(ir, list(check_scope = TRUE, defined_vars = character())),
    "IR verify \\[var\\]: 'x' not in scope"
  )
})

test_that("sequential definition tracking works", {  # Valid: define x, then use it
  ir <- .mojor_ir_block(list(
    .mojor_ir_assign(
      .mojor_ir_var("x"),
      .mojor_ir_const("1")
    ),
    .mojor_ir_assign(
      .mojor_ir_var("y"),
      .mojor_ir_var("x")
    )
  ))

  expect_silent(
    .mojor_ir_verify(ir, list(check_scope = TRUE, defined_vars = character()))
  )
})

test_that("loop variable is in scope inside loop body", {  # Valid: use loop variable inside body
  ir <- .mojor_ir_loop(
    var = "i",
    range = .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10")),
    body = .mojor_ir_block(list(
      .mojor_ir_assign(
        .mojor_ir_index(
          .mojor_ir_var("out"),
          list(.mojor_ir_var("i"))
        ),
        .mojor_ir_var("i")
      )
    ))
  )

  expect_silent(
    .mojor_ir_verify(ir, list(check_scope = TRUE, defined_vars = c("out")))
  )
})

test_that("complex invariant combination: nested loops with break", {  # Valid: break inside nested loop
  inner_body <- .mojor_ir_block(list(
    .mojor_ir_if(
      cond = .mojor_ir_binop(">", .mojor_ir_var("j"), .mojor_ir_const("5")),
      then = .mojor_ir_block(list(.mojor_ir_break()))
    )
  ))

  inner_loop <- .mojor_ir_loop(
    var = "j",
    range = .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10")),
    body = inner_body
  )

  outer_loop <- .mojor_ir_loop(
    var = "i",
    range = .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10")),
    body = inner_loop
  )

  expect_silent(.mojor_ir_verify(outer_loop))
})

test_that("invalid: break in if outside loop", {  # Invalid: break in if-statement outside any loop
  ir <- .mojor_ir_if(
    cond = .mojor_ir_const("TRUE"),
    then = .mojor_ir_block(list(.mojor_ir_break()))
  )

  expect_error(
    .mojor_ir_verify(ir, list(in_loop = FALSE)),
    "break statement outside loop"
  )
})

test_that("all math functions validated for arity", {  math_fns <- c("sin", "cos", "tan", "asin", "acos", "atan",
                "log", "log10", "log1p", "log2", "exp", "expm1",
                "sqrt", "abs", "floor", "ceiling", "trunc", "round")

  for (fn in math_fns) {
    # Should error with 0 args
    ir0 <- .mojor_ir_call(fn = fn, args = list())
    expect_error(.mojor_ir_verify(ir0), paste0(fn, ".*expects 1 argument"))

    # Should pass with 1 arg
    ir1 <- .mojor_ir_call(fn = fn, args = list(.mojor_ir_var("x")))
    expect_silent(.mojor_ir_verify(ir1))

    # Should error with 2 args
    ir2 <- .mojor_ir_call(fn = fn, args = list(.mojor_ir_var("x"), .mojor_ir_var("y")))
    expect_error(.mojor_ir_verify(ir2), paste0(fn, ".*expects 1 argument"))
  }
})

test_that("type_env is preserved through nested structures", {  # Valid: type_env should be threaded through if/loop/block
  type_env <- list(x = "f64[]", i = "i32", out = "f64[]")

  ir <- .mojor_ir_loop(
    var = "i",
    range = .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10")),
    body = .mojor_ir_if(
      cond = .mojor_ir_binop(">", .mojor_ir_var("i"), .mojor_ir_const("5")),
      then = .mojor_ir_block(list(
        .mojor_ir_assign(
          .mojor_ir_index(.mojor_ir_var("out"), list(.mojor_ir_var("i"))),
          .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i")))
        )
      ))
    )
  )

  expect_silent(
    .mojor_ir_verify(ir, list(
      check_scope = TRUE,
      defined_vars = c("x", "out"),
      type_env = type_env
    ))
  )
})

test_that("verifier context fields are documented", {  # Test that all documented ctx fields work
  ctx <- list(
    loop_vars = c("i", "j"),
    defined_vars = c("x", "y", "z"),
    check_scope = TRUE,
    ir_only = FALSE,
    in_loop = TRUE,
    type_env = list(x = "f64", y = "i32"),
    has_returned = FALSE
  )

  # Simple var reference with all context
  ir <- .mojor_ir_var("x")
  expect_silent(.mojor_ir_verify(ir, ctx))
})
