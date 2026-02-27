test_that("[1d] alias works in type annotations", {
  skip_if_no_mojo()
  
  # Test basic [1d] syntax
  f <- function(x) {
    sum(x)
  }
  
  # Should work with [1d] notation
  tr1 <- mojor_transpile(f, x = "f64[1d]")
  expect_true(!is.null(tr1))
  expect_identical(tr1$types$x, "f64[]")  # normalized internally
  
  # Should produce same result as [] notation
  tr2 <- mojor_transpile(f, x = "f64[]")
  expect_identical(tr1$mojo_code, tr2$mojo_code)
  
  # Test with multiple types
  g <- function(x, y) {
    s <- 0.0
    for (i in 1:length(x)) {
      s <- s + x[i] + y[i]
    }
    s
  }
  
  tr3 <- mojor_transpile(g, x = "f64[1d]", y = "i32[1d]")
  expect_identical(tr3$types$x, "f64[]")
  expect_identical(tr3$types$y, "i32[]")
  
  # Test build pipeline
  b <- mojor_build(f, x = "f64[1d]", load = FALSE)
  expect_true(!is.null(b))
  expect_identical(b$types$x, "f64[]")
})

test_that("[1d] normalization in helpers_core", {
  # Test .mojor_normalize_type_spec
  expect_equal(.mojor_normalize_type_spec("f64[1d]"), "f64[]")
  expect_equal(.mojor_normalize_type_spec("i32[1d]"), "i32[]")
  expect_equal(.mojor_normalize_type_spec("lgl[1d]"), "lgl[]")
  
  # Test with character alias
  expect_equal(.mojor_normalize_type_spec("logical[1d]"), "lgl[]")
})
