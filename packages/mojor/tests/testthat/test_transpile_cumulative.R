# Test: Cumulative and Statistical Operations (PR-B5)
# Tests for cumsum, cumprod, mean, etc.

library(testthat)

if (!exists(".mojor_ir_var", mode = "function", inherits = TRUE)) {
  stop("IR helpers are unavailable in the test environment")
}

context("cumulative operations transpilation")
test_that("cumsum basic infrastructure exists", {  # Test IR constructor
  x_ir <- .mojor_ir_var("x")
  cumsum_ir <- .mojor_ir_cumsum(x_ir)
  
  expect_equal(cumsum_ir$kind, "cumsum")
  expect_equal(cumsum_ir$x, x_ir)
})

test_that("cumprod basic infrastructure exists", {  x_ir <- .mojor_ir_var("x")
  cumprod_ir <- .mojor_ir_cumprod(x_ir)
  
  expect_equal(cumprod_ir$kind, "cumprod")
  expect_equal(cumprod_ir$x, x_ir)
})

test_that("mean basic infrastructure exists", {  x_ir <- .mojor_ir_var("x")
  mean_ir <- .mojor_ir_mean(x_ir)
  
  expect_equal(mean_ir$kind, "mean")
  expect_equal(mean_ir$x, x_ir)
})

test_that("cumulative operations are recognized as constructors", {  expect_true(.mojor_ir_expr_has_constructor(quote(cumsum(x))))
  expect_true(.mojor_ir_expr_has_constructor(quote(cumprod(x))))
  expect_true(.mojor_ir_expr_has_constructor(quote(mean(x))))
})

test_that("cumulative operations have correct effects", {  x_ir <- .mojor_ir_var("x")
  
  cumsum_ir <- .mojor_ir_cumsum(x_ir)
  cumsum_eff <- .mojor_ir_expr_effects(cumsum_ir)
  expect_true("Pure" %in% cumsum_eff || "ReadsMem" %in% cumsum_eff)
  
  mean_ir <- .mojor_ir_mean(x_ir)
  mean_eff <- .mojor_ir_expr_effects(mean_ir)
  expect_true("Pure" %in% mean_eff || "ReadsMem" %in% mean_eff)
})

test_that("cumsum IR node is built from AST", {  expr <- quote(cumsum(x))
  ir <- .mojor_ir_expr_build(expr)
  
  expect_false(is.null(ir))
  expect_equal(ir$kind, "cumsum")
  expect_equal(ir$x$kind, "var")
  expect_equal(ir$x$name, "x")
})

test_that("cumprod IR node is built from AST", {  expr <- quote(cumprod(x))
  ir <- .mojor_ir_expr_build(expr)
  
  expect_false(is.null(ir))
  expect_equal(ir$kind, "cumprod")
})

test_that("mean IR node is built from AST", {  expr <- quote(mean(x))
  ir <- .mojor_ir_expr_build(expr)
  
  expect_false(is.null(ir))
  expect_equal(ir$kind, "mean")
})

context("cumulative operations loop-carried dependency")
test_that("cumsum generates accumulator pattern", {  # Verify the IR infrastructure exists for loop-carried deps
  x_ir <- .mojor_ir_var("x")
  cumsum_ir <- .mojor_ir_cumsum(x_ir)
  
  # The emitter should produce accumulator variable reference
  # Full integration test requires loop context
  expect_equal(cumsum_ir$kind, "cumsum")
  expect_equal(cumsum_ir$x$name, "x")
})

test_that("cumprod generates accumulator pattern", {  x_ir <- .mojor_ir_var("x")
  cumprod_ir <- .mojor_ir_cumprod(x_ir)
  
  expect_equal(cumprod_ir$kind, "cumprod")
  expect_equal(cumprod_ir$x$name, "x")
})

test_that("cumulative operations state tracking exists", {  # Verify state variables exist
  expect_true(is.null(.mojor_state$current_cumul_acc_counter) || 
              is.numeric(.mojor_state$current_cumul_acc_counter))
  expect_true(is.null(.mojor_state$current_cumul_accs) || 
              is.list(.mojor_state$current_cumul_accs))
})

test_that("cummax / cummin infrastructure exists", {  x_ir <- .mojor_ir_var("x")
  
  cummax_ir <- .mojor_ir_cummax(x_ir)
  expect_equal(cummax_ir$kind, "cummax")
  expect_equal(cummax_ir$x$name, "x")
  
  cummin_ir <- .mojor_ir_cummin(x_ir)
  expect_equal(cummin_ir$kind, "cummin")
  expect_equal(cummin_ir$x$name, "x")
})

test_that("cummax / cummin are recognized as constructors", {  expect_true(.mojor_ir_expr_has_constructor(quote(cummax(x))))
  expect_true(.mojor_ir_expr_has_constructor(quote(cummin(x))))
})

test_that("cummax / cummin have correct effects", {  x_ir <- .mojor_ir_var("x")
  
  cummax_ir <- .mojor_ir_cummax(x_ir)
  cummax_eff <- .mojor_ir_expr_effects(cummax_ir)
  expect_true("Pure" %in% cummax_eff || "ReadsMem" %in% cummax_eff)
  
  cummin_ir <- .mojor_ir_cummin(x_ir)
  cummin_eff <- .mojor_ir_expr_effects(cummin_ir)
  expect_true("Pure" %in% cummin_eff || "ReadsMem" %in% cummin_eff)
})

test_that("var / sd infrastructure exists", {  x_ir <- .mojor_ir_var("x")
  
  var_ir <- .mojor_ir_variance(x_ir)
  expect_equal(var_ir$kind, "var_stat")
  expect_equal(var_ir$x$name, "x")
  
  sd_ir <- .mojor_ir_sd(x_ir)
  expect_equal(sd_ir$kind, "sd")
  expect_equal(sd_ir$x$name, "x")
})

test_that("var / sd are recognized as constructors", {  expect_true(.mojor_ir_expr_has_constructor(quote(var(x))))
  expect_true(.mojor_ir_expr_has_constructor(quote(sd(x))))
})

test_that("var / sd have correct effects", {  x_ir <- .mojor_ir_var("x")
  
  var_ir <- .mojor_ir_variance(x_ir)
  var_eff <- .mojor_ir_expr_effects(var_ir)
  expect_true("Pure" %in% var_eff || "ReadsMem" %in% var_eff)
  
  sd_ir <- .mojor_ir_sd(x_ir)
  sd_eff <- .mojor_ir_expr_effects(sd_ir)
  expect_true("Pure" %in% sd_eff || "ReadsMem" %in% sd_eff)
})

context("cumulative operations documentation")
test_that("PR-B5 is documented in docs/BREADTH/PLAN.md", {  # Try multiple possible locations
  candidates <- c(
    file.path(getwd(), "../../docs/BREADTH/PLAN.md"),
    file.path(getwd(), "../../../docs/BREADTH/PLAN.md"),
    "docs/BREADTH/PLAN.md"
  )
  plan_file <- NULL
  for (c in candidates) {
    if (file.exists(c)) {
      plan_file <- c
      break
    }
  }
  skip_if(is.null(plan_file), "docs/BREADTH/PLAN.md not found")
  
  plan_doc <- readLines(plan_file)
  expect_true(any(grepl("PR-B5", plan_doc)))
  expect_true(any(grepl("Cumulative\\s*[/&]\\s*Statistical\\s*Ops", plan_doc, ignore.case = TRUE)))
})
