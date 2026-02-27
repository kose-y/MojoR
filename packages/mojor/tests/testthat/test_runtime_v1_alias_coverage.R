library(testthat)

test_that("runtime v1 aliases exist for bridge-managed symbols", {
  bridge_pkg <- .mojor_bridge_pkg(required = FALSE)
  if (is.null(bridge_pkg) || !nzchar(bridge_pkg)) {
    skip("bridge package not loaded in this test environment")
  }

  symbols <- c(
    "mojor_dnorm_fast",
    "mojor_dpois_fast",
    "mojor_tier9_regex_runtime",
    "mojor_tier9_expand_grid_runtime"
  )
  for (symbol in symbols) {
    candidates <- .mojor_runtime_symbol_v1_candidates(symbol)
    loaded <- any(vapply(
      candidates,
      function(candidate) isTRUE(is.loaded(candidate, PACKAGE = bridge_pkg)),
      logical(1)
    ))
    expect_true(
      loaded,
      info = paste0(
        "missing runtime v1 alias for symbol=",
        symbol,
        " candidates=",
        paste(candidates, collapse = ",")
      )
    )
  }
})
