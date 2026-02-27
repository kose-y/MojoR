library(testthat)

# Tests for Random Number Generation (RNG)

# ============================================================================
# Phase 4.0: Determinism Tests
# ============================================================================

test_that("mojor_rng_seed works and is reproducible", {  skip_if_no_mojo()
  
  mojor_rng_seed(42)
  r1 <- mojor_runif(10)
  
  mojor_rng_seed(42)
  r2 <- mojor_runif(10)
  
  expect_equal(r1, r2)
})

test_that("mojor_rng_seed produces identical sequences across multiple runs", {  skip_if_no_mojo()
  
  # Run 1
  mojor_rng_seed(12345)
  r1 <- mojor_runif(100)
  
  # Run 2 - same seed should produce identical sequence
  mojor_rng_seed(12345)
  r2 <- mojor_runif(100)
  
  expect_equal(r1, r2, info = "Same seed should produce identical sequences")
})

test_that("mojor_rng_seed advances state correctly", {  skip_if_no_mojo()
  
  mojor_rng_seed(999)
  x1 <- mojor_runif(1)
  
  mojor_rng_seed(999)
  x2 <- mojor_runif(1)
  x3 <- mojor_runif(1)
  
  # First call should match
  expect_equal(x1, x2)
  # Second call should be different (state advanced)
  expect_true(x2 != x3)
})

test_that("mojor_rng_seed resets sequence properly", {  skip_if_no_mojo()
  
  mojor_rng_seed(777)
  seq1 <- mojor_runif(5)
  
  # Advance state
  mojor_runif(3)
  
  # Reset to same seed - should restart sequence
  mojor_rng_seed(777)
  seq2 <- mojor_runif(5)
  
  expect_equal(seq1, seq2)
})

# ============================================================================
# Phase 4.0: Distribution Sanity Checks
# ============================================================================

test_that("mojor_runif generates values in [0, 1)", {  skip_if_no_mojo()
  
  x <- mojor_runif(1000)
  
  expect_true(all(x >= 0))
  expect_true(all(x < 1))
  expect_equal(length(x), 1000)
})

test_that("mojor_runif has uniform distribution properties", {  skip_if_no_mojo()
  
  x <- mojor_runif(10000)
  
  # Mean should be approximately 0.5 (uniform distribution property)
  expect_true(abs(mean(x) - 0.5) < 0.03)
  
  # Variance should be approximately 1/12 ≈ 0.0833 for uniform [0,1)
  expected_var <- 1/12
  actual_var <- var(x)
  expect_true(abs(actual_var - expected_var) < 0.01)
})

test_that("mojor_runif with range generates values in [min, max)", {  skip_if_no_mojo()
  
  x <- mojor_runif(1000, min = 10, max = 20)
  
  expect_true(all(x >= 10))
  expect_true(all(x < 20))
  expect_equal(length(x), 1000)
})

test_that("mojor_runif range scaling is correct", {  skip_if_no_mojo()
  
  x <- mojor_runif(10000, min = 5, max = 15)
  
  # Mean should be approximately (min + max) / 2 = 10
  expected_mean <- 10
  actual_mean <- mean(x)
  expect_true(abs(actual_mean - expected_mean) < 0.05)
  
  # Variance should be approximately (max-min)^2 / 12 = 100/12 ≈ 8.33
  expected_var <- 100/12
  actual_var <- var(x)
  expect_true(abs(actual_var - expected_var) < 0.5)
})

test_that("mojor_rnorm generates standard normal values", {  skip_if_no_mojo()
  
  z <- mojor_rnorm(1000)
  
  expect_equal(length(z), 1000)
  # Mean should be approximately 0
  expect_true(abs(mean(z)) < 0.1)
  # SD should be approximately 1
  expect_true(abs(sd(z) - 1) < 0.1)
})

test_that("mojor_rnorm has normal distribution properties", {  skip_if_no_mojo()
  
  z <- mojor_rnorm(10000)
  
  # Mean should be approximately 0
  expect_true(abs(mean(z)) < 0.05)
  
  # SD should be approximately 1
  expect_true(abs(sd(z) - 1) < 0.05)
  
  # Skewness should be approximately 0 (standard normal)
  # Using simple approximation: E[X^3] ≈ 0 for standard normal
  skewness <- mean(z^3)
  expect_true(abs(skewness) < 0.2)
})

test_that("mojor_rnorm with mean and sd works", {  skip_if_no_mojo()
  
  z <- mojor_rnorm(1000, mean = 50, sd = 10)
  
  expect_equal(length(z), 1000)
  # Mean should be approximately 50
  expect_true(abs(mean(z) - 50) < 1)
  # SD should be approximately 10
  expect_true(abs(sd(z) - 10) < 1)
})

test_that("mojor_rnorm handles edge cases", {  skip_if_no_mojo()
  
  # Zero observations
  expect_equal(mojor_rnorm(0), numeric(0))
  
  # Zero SD (all values should be equal to mean)
  z <- mojor_rnorm(10, mean = 5, sd = 0)
  expect_true(all(z == 5))
})

test_that("mojor_runif handles edge cases", {  skip_if_no_mojo()
  
  # Zero observations
  expect_equal(mojor_runif(0), numeric(0))
  
  # Single observation
  x <- mojor_runif(1)
  expect_equal(length(x), 1)
  expect_true(x >= 0 && x < 1)
})

test_that("additional RNG distributions generate expected domains", {  skip_if_no_mojo()

  e <- mojor_rexp(5000, rate = 2)
  expect_true(all(e >= 0))
  expect_true(abs(mean(e) - 0.5) < 0.06)

  p <- mojor_rpois(5000, lambda = 3)
  expect_true(all(p >= 0))
  expect_true(abs(mean(p) - 3) < 0.15)

  ln <- mojor_rlnorm(5000, meanlog = 0, sdlog = 0.5)
  expect_true(all(ln > 0))

  cs <- mojor_rchisq(5000, df = 4)
  expect_true(all(cs >= 0))
  expect_true(abs(mean(cs) - 4) < 0.25)

  tt <- mojor_rt(5000, df = 10)
  expect_true(abs(mean(tt)) < 0.12)

  ff <- mojor_rf(5000, df1 = 5, df2 = 10)
  expect_true(all(ff >= 0))

  bb <- mojor_rbeta(5000, shape1 = 2, shape2 = 5)
  expect_true(all(bb >= 0 & bb <= 1))

  ww <- mojor_rweibull(5000, shape = 2, scale = 3)
  expect_true(all(ww >= 0))

  lg <- mojor_rlogis(5000, location = 1, scale = 2)
  expect_true(abs(mean(lg) - 1) < 0.2)

  cc <- mojor_rcauchy(2000, location = 0, scale = 1)
  expect_equal(length(cc), 2000)
  expect_true(all(is.finite(cc)))

  gg <- mojor_rgeom(5000, prob = 0.25)
  expect_true(all(gg >= 0))
  expect_true(abs(mean(gg) - 3) < 0.25)

  nb <- mojor_rnbinom(5000, size = 4, prob = 0.5)
  expect_true(all(nb >= 0))
  expect_true(abs(mean(nb) - 4) < 0.35)

  hy <- mojor_rhyper(5000, m = 30, n = 20, k = 10)
  expect_true(all(hy >= 0))
  expect_true(all(hy <= 10))
  expect_true(abs(mean(hy) - 6) < 0.2)

  sr <- mojor_rsignrank(5000, n = 10)
  expect_true(all(sr >= 0))
  expect_true(all(sr <= 55))
  expect_true(abs(mean(sr) - 27.5) < 0.8)

  wx <- mojor_rwilcox(5000, m = 8, n = 12)
  expect_true(all(wx >= 0))
  expect_true(all(wx <= 96))
  expect_true(abs(mean(wx) - 48) < 1.0)
})

test_that("additional RNG distributions validate parameters", {  skip_if_no_mojo()

  expect_error(mojor_rexp(10, rate = 0), "rate must be positive")
  expect_error(mojor_rpois(10, lambda = -1), "lambda must be non-negative")
  expect_error(mojor_rlnorm(10, sdlog = -1), "sdlog must be non-negative")
  expect_error(mojor_rchisq(10, df = 0), "df must be positive")
  expect_error(mojor_rt(10, df = 0), "df must be positive")
  expect_error(mojor_rf(10, df1 = 0, df2 = 1), "df1 must be positive")
  expect_error(mojor_rbeta(10, shape1 = 0, shape2 = 1), "shape1 must be positive")
  expect_error(mojor_rweibull(10, shape = 0), "shape must be positive")
  expect_error(mojor_rlogis(10, scale = 0), "scale must be positive")
  expect_error(mojor_rcauchy(10, scale = 0), "scale must be positive")
  expect_error(mojor_rgeom(10, prob = 0), "prob must be in \\(0, 1\\]")
  expect_error(mojor_rnbinom(10, size = -1, prob = 0.5), "size must be non-negative")
  expect_error(mojor_rhyper(10, m = -1, n = 5, k = 3), "m must be non-negative")
  expect_error(mojor_rhyper(10, m = 5, n = -1, k = 3), "n must be non-negative")
  expect_error(mojor_rhyper(10, m = 5, n = 3, k = -1), "k must be non-negative")
  expect_error(mojor_rhyper(10, m = 5, n = 3, k = 9), "k cannot exceed m \\+ n")
  expect_error(mojor_rsignrank(10, n = -1), "n must be non-negative")
  expect_error(mojor_rwilcox(10, m = -1, n = 2), "m must be non-negative")
  expect_error(mojor_rwilcox(10, m = 2, n = -1), "n must be non-negative")
})
