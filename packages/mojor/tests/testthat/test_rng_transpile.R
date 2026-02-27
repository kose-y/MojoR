library(testthat)

test_that("additional RNG calls transpile with RNG helpers", {  f <- function(N, p, lam, a, b) {
    out <- numeric(N)
    for (i in seq_len(N)) {
      out[i] <- rexp(1, 2) + rpois(1, lam) +
        rbeta(1, a, b) + rgeom(1, p) + rnbinom(1, a, p)
    }
    out
  }

  trans <- mojor_transpile(
    f,
    N = "i32",
    p = "f64",
    lam = "f64",
    a = "f64",
    b = "f64",
    name = "t_rng_extra_transpile_1",
    emit_ir = TRUE,
    ir_only = TRUE
  )

  expect_true(isTRUE(trans$rng_needed))
  expect_match(trans$mojo, "_random_poisson", fixed = TRUE)
  expect_match(trans$mojo, "_random_beta", fixed = TRUE)
  expect_match(trans$mojo, "_random_geometric", fixed = TRUE)
})

test_that("hypergeometric RNG transpiles with helper", {  f <- function(N, m, n_bad, k) {
    out <- numeric(N)
    for (i in seq_len(N)) {
      out[i] <- rhyper(1, m, n_bad, k)
    }
    out
  }

  trans <- mojor_transpile(
    f,
    N = "i32",
    m = "i32",
    n_bad = "i32",
    k = "i32",
    name = "t_rng_extra_transpile_3",
    emit_ir = TRUE,
    ir_only = TRUE
  )

  expect_true(isTRUE(trans$rng_needed))
  expect_match(trans$mojo, "_random_hypergeometric", fixed = TRUE)
})

test_that("signrank and wilcox RNG transpile with helpers", {  f <- function(N, n_sign, m, n_w) {
    out <- numeric(N)
    for (i in seq_len(N)) {
      out[i] <- rsignrank(1, n_sign) + rwilcox(1, m, n_w)
    }
    out
  }

  trans <- mojor_transpile(
    f,
    N = "i32",
    n_sign = "i32",
    m = "i32",
    n_w = "i32",
    name = "t_rng_extra_transpile_4",
    emit_ir = TRUE,
    ir_only = TRUE
  )

  expect_true(isTRUE(trans$rng_needed))
  expect_match(trans$mojo, "_random_signrank", fixed = TRUE)
  expect_match(trans$mojo, "_random_wilcox", fixed = TRUE)
})

test_that("distribution transforms transpile for scalar draws", {  f <- function(N, df, shape, scale) {
    out <- numeric(N)
    for (i in seq_len(N)) {
      out[i] <- rlnorm(1, 0, 1) + rchisq(1, df) + rt(1, df) +
        rf(1, df, df + 1) + rweibull(1, shape, scale) +
        rlogis(1, 0, 1) + rcauchy(1, 0, 1)
    }
    out
  }

  trans <- mojor_transpile(
    f,
    N = "i32",
    df = "f64",
    shape = "f64",
    scale = "f64",
    name = "t_rng_extra_transpile_2",
    emit_ir = TRUE,
    ir_only = TRUE
  )

  expect_true(isTRUE(trans$rng_needed))
  expect_match(trans$mojo, "_random_chisq", fixed = TRUE)
  expect_match(trans$mojo, "_random_weibull", fixed = TRUE)
  expect_match(trans$mojo, "_random_logistic", fixed = TRUE)
  expect_match(trans$mojo, "_random_cauchy", fixed = TRUE)
})
