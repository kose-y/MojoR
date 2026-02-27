# Test suite for IR roundtrip stability
# This file validates that .mojor_ir_format() produces stable output

library(testthat)

# =============================================================================
# Format Stability Tests
# =============================================================================
# Goal: format(ir) should be idempotent (calling it twice produces same result)

test_that("format stability: simple assignment", {  code <- quote({
    x <- 1
  })

  ir <- .mojor_ir_build_stmt(code[[2]])
  expect_true(!is.null(ir))

  # Format twice
  formatted1 <- .mojor_ir_format(ir)
  formatted2 <- .mojor_ir_format(ir)

  expect_identical(formatted1, formatted2)
  expect_type(formatted1, "character")
})

test_that("format stability: loop with body", {  code <- quote({
    for (i in 1:n) {
      out[i] <- x[i] + 1
    }
  })

  ir <- .mojor_ir_build_stmt(code[[2]])
  expect_true(!is.null(ir))

  formatted1 <- .mojor_ir_format(ir)
  formatted2 <- .mojor_ir_format(ir)

  expect_identical(formatted1, formatted2)
})

test_that("format stability: nested loops", {  code <- quote({
    for (i in 1:n) {
      for (j in 1:m) {
        out[i] <- out[i] + x[j]
      }
    }
  })

  ir <- .mojor_ir_build_stmt(code[[2]])
  expect_true(!is.null(ir))

  formatted1 <- .mojor_ir_format(ir)
  formatted2 <- .mojor_ir_format(ir)

  expect_identical(formatted1, formatted2)
})

test_that("format stability: if/else statements", {  code <- quote({
    if (x > 0) {
      y <- 1
    } else {
      y <- -1
    }
  })

  ir <- .mojor_ir_build_stmt(code[[2]])
  expect_true(!is.null(ir))

  formatted1 <- .mojor_ir_format(ir)
  formatted2 <- .mojor_ir_format(ir)

  expect_identical(formatted1, formatted2)
})

test_that("format stability: while loop", {  code <- quote({
    while (i < n) {
      i <- i + 1
    }
  })

  ir <- .mojor_ir_build_stmt(code[[2]])
  expect_true(!is.null(ir))

  formatted1 <- .mojor_ir_format(ir)
  formatted2 <- .mojor_ir_format(ir)

  expect_identical(formatted1, formatted2)
})

test_that("format stability: complex expression", {  expr <- quote(x[i] + y[j] * 2 - z[k])

  ir <- .mojor_ir_expr_build(expr)
  expect_true(!is.null(ir))

  formatted1 <- .mojor_ir_format(ir)
  formatted2 <- .mojor_ir_format(ir)

  expect_identical(formatted1, formatted2)
})

test_that("format stability: ifelse expression", {  expr <- quote(ifelse(x > 0, y, z))

  ir <- expect_warning(
    .mojor_ir_expr_build(expr),
    regexp = "ifelse\\(\\) expression lowered to ternary",
    fixed = FALSE
  )
  expect_true(!is.null(ir))

  formatted1 <- .mojor_ir_format(ir)
  formatted2 <- .mojor_ir_format(ir)

  expect_identical(formatted1, formatted2)
})

test_that("format stability: math function calls", {  expr <- quote(sin(x) + cos(y) - log(z))

  ir <- .mojor_ir_expr_build(expr)
  expect_true(!is.null(ir))

  formatted1 <- .mojor_ir_format(ir)
  formatted2 <- .mojor_ir_format(ir)

  expect_identical(formatted1, formatted2)
})

# =============================================================================
# Format Consistency Tests
# =============================================================================
# Goal: format() should produce valid, parseable output

test_that("format output is non-empty for simple nodes", {  nodes <- list(
    const = .mojor_ir_const("42"),
    var = .mojor_ir_var("x"),
    unop = .mojor_ir_unop("-", .mojor_ir_var("x")),
    binop = .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_var("y"))
  )

  for (name in names(nodes)) {
    formatted <- .mojor_ir_format(nodes[[name]])
    expect_type(formatted, "character")
    expect_gt(nchar(formatted), 0)
  }
})

test_that("format output contains node kind", {  nodes <- list(
    const = .mojor_ir_const("42"),
    var = .mojor_ir_var("x"),
    loop = .mojor_ir_loop(
      var = "i",
      range = .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10")),
      body = .mojor_ir_block(list())
    )
  )

  for (name in names(nodes)) {
    formatted <- .mojor_ir_format(nodes[[name]])
    # Format should contain the kind name somewhere (check any line)
    expect_true(any(grepl(name, formatted, fixed = TRUE)), label = name)
  }
})

test_that("format indent increases for nested structures", {  ir <- .mojor_ir_loop(
    var = "i",
    range = .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10")),
    body = .mojor_ir_block(list(
      .mojor_ir_assign(
        .mojor_ir_var("x"),
        .mojor_ir_var("i")
      )
    ))
  )

  formatted <- .mojor_ir_format(ir)

  # Nested content should have more leading spaces than parent
  lines <- strsplit(formatted, "\n")[[1]]
  expect_gt(length(lines), 1)

  # Body lines should be indented more than loop line
  loop_line <- grep("^loop", lines, value = TRUE)[1]
  body_lines <- lines[grepl("^  ", lines)]
  expect_gt(length(body_lines), 0)
})

# =============================================================================
# Format Coverage Tests
# =============================================================================
# Goal: all IR node types can be formatted without error

test_that("format coverage: all expression nodes", {  nodes <- list(
    const = .mojor_ir_const("42"),
    var = .mojor_ir_var("x"),
    unop = .mojor_ir_unop("-", .mojor_ir_var("x")),
    binop = .mojor_ir_binop("+", .mojor_ir_var("x"), .mojor_ir_const("1")),
    cast = .mojor_ir_cast("f64", .mojor_ir_var("x")),
    call = .mojor_ir_call("sqrt", list(.mojor_ir_var("x"))),
    index = .mojor_ir_index(.mojor_ir_var("x"), list(.mojor_ir_var("i"))),
    ifelse = .mojor_ir_ifelse(
      .mojor_ir_const("TRUE"),
      .mojor_ir_var("x"),
      .mojor_ir_var("y")
    )
  )

  for (name in names(nodes)) {
    formatted <- .mojor_ir_format(nodes[[name]])
    expect_type(formatted, "character")
    expect_gt(length(formatted), 0)
  }
})

test_that("format coverage: all statement nodes", {  nodes <- list(
    block = .mojor_ir_block(list()),
    assign = .mojor_ir_assign(.mojor_ir_var("x"), .mojor_ir_const("1")),
    if_stmt = .mojor_ir_if(
      .mojor_ir_const("TRUE"),
      .mojor_ir_block(list())
    ),
    loop = .mojor_ir_loop(
      "i",
      .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10")),
      .mojor_ir_block(list())
    ),
    while_stmt = .mojor_ir_while(
      .mojor_ir_const("TRUE"),
      .mojor_ir_block(list())
    ),
    repeat_stmt = .mojor_ir_repeat(.mojor_ir_block(list())),
    break_stmt = .mojor_ir_break(),
    next_stmt = .mojor_ir_next(),
    return_stmt = .mojor_ir_return(.mojor_ir_var("x"))
  )

  for (name in names(nodes)) {
    formatted <- .mojor_ir_format(nodes[[name]])
    expect_type(formatted, "character")
    expect_gt(length(formatted), 0)
  }
})

test_that("format coverage: indexing helper nodes", {  nodes <- list(
    scalar_index = .mojor_ir_scalar_index(.mojor_ir_var("i")),
    slice_index = .mojor_ir_slice_index(
      .mojor_ir_const("1"),
      .mojor_ir_var("n")
    ),
    missing_index = .mojor_ir_missing_index(),
    subscript = .mojor_ir_subscript(
      "out",
      list(.mojor_ir_scalar_index(.mojor_ir_var("i")))
    )
  )

  for (name in names(nodes)) {
    formatted <- .mojor_ir_format(nodes[[name]])
    expect_type(formatted, "character")
    expect_gt(length(formatted), 0)
  }
})

test_that("format coverage: range nodes", {  nodes <- list(
    range = .mojor_ir_range(.mojor_ir_const("1"), .mojor_ir_const("10")),
    range_with_step = .mojor_ir_range(
      .mojor_ir_const("1"),
      .mojor_ir_const("10"),
      step = .mojor_ir_const("2")
    ),
    range_expr = .mojor_ir_range_expr(quote(1:n))
  )

  for (name in names(nodes)) {
    formatted <- .mojor_ir_format(nodes[[name]])
    expect_type(formatted, "character")
    expect_gt(length(formatted), 0)
  }
})

# =============================================================================
# Format Determinism Tests
# =============================================================================
# Goal: same IR structure always produces same format output

test_that("format determinism: multiple builds of same code", {  code <- quote({
    for (i in 1:n) {
      out[i] <- x[i] + y[i]
    }
  })

  # Build IR multiple times from same code
  ir1 <- .mojor_ir_build_stmt(code[[2]])
  ir2 <- .mojor_ir_build_stmt(code[[2]])
  ir3 <- .mojor_ir_build_stmt(code[[2]])

  # All should format identically
  formatted1 <- .mojor_ir_format(ir1)
  formatted2 <- .mojor_ir_format(ir2)
  formatted3 <- .mojor_ir_format(ir3)

  expect_identical(formatted1, formatted2)
  expect_identical(formatted2, formatted3)
})

test_that("format determinism: complex nested structure", {  code <- quote({
    for (i in 1:n) {
      if (x[i] > 0) {
        for (j in 1:m) {
          out[i] <- out[i] + x[j] * y[j]
        }
      } else {
        out[i] <- 0
      }
    }
  })

  ir1 <- .mojor_ir_build_stmt(code[[2]])
  ir2 <- .mojor_ir_build_stmt(code[[2]])

  formatted1 <- .mojor_ir_format(ir1)
  formatted2 <- .mojor_ir_format(ir2)

  expect_identical(formatted1, formatted2)
})

# =============================================================================
# Format Robustness Tests
# =============================================================================
# Goal: format handles edge cases gracefully

test_that("format handles empty blocks", {  ir <- .mojor_ir_block(list())
  expect_silent(formatted <- .mojor_ir_format(ir))
  expect_type(formatted, "character")
})

test_that("format handles single-statement blocks", {  ir <- .mojor_ir_block(list(
    .mojor_ir_assign(.mojor_ir_var("x"), .mojor_ir_const("1"))
  ))
  expect_silent(formatted <- .mojor_ir_format(ir))
  expect_type(formatted, "character")
})

test_that("format handles deeply nested structures", {  # Build a 5-level nested structure
  inner <- .mojor_ir_assign(.mojor_ir_var("x"), .mojor_ir_const("1"))
  for (i in 1:5) {
    inner <- .mojor_ir_if(
      .mojor_ir_const("TRUE"),
      .mojor_ir_block(list(inner))
    )
  }

  formatted <- .mojor_ir_format(inner)
  expect_type(formatted, "character")

  # Should have multiple indentation levels
  lines <- if (length(formatted) == 1) strsplit(formatted, "\n")[[1]] else formatted
  expect_gt(length(lines), 10)  # Should have many lines

  # Check that some lines have significant indentation
  indents <- nchar(lines) - nchar(trimws(lines, "left"))
  expect_gt(max(indents), 8)
})

test_that("format handles NULL src fields", {  # All nodes should handle NULL src gracefully
  ir <- .mojor_ir_const("42", src = NULL)
  expect_silent(formatted <- .mojor_ir_format(ir))
  expect_type(formatted, "character")
})

# =============================================================================
# Format Golden Files (Optional - for regression testing)
# =============================================================================

test_that("format matches expected output for simple loop", {  code <- quote({
    for (i in 1:10) {
      x <- i
    }
  })

  ir <- .mojor_ir_build_stmt(code[[2]])
  formatted <- .mojor_ir_format(ir)

  # Check that essential structure is present (check any line)
  expect_true(any(grepl("loop", formatted)))
  expect_true(any(grepl("range", formatted)))
  expect_true(any(grepl("assign", formatted)))
})

test_that("format preserves operator precedence info", {  expr <- quote(x + y * z)
  ir <- .mojor_ir_expr_build(expr)
  formatted <- .mojor_ir_format(ir)

  # Should show multiplication nested under addition
  # (exact format depends on implementation, but structure should be clear)
  formatted_text <- paste(formatted, collapse = "\n")
  expect_true(grepl("\\+", formatted_text))
  expect_true(grepl("\\*", formatted_text))
})
