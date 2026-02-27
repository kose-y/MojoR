source("helper-mojo.R")

test_that("mojor_run_chains_parallel is deterministic with base RNG", {  chain_fun <- function(n) stats::rnorm(n)

  res1 <- mojor_run_chains_parallel(
    chain_fun = chain_fun,
    n_chains = 4L,
    chain_args = list(n = 16L),
    seed = 123L,
    workers = 1L,
    backend = "sequential",
    use_mojor_rng = FALSE
  )
  res2 <- mojor_run_chains_parallel(
    chain_fun = chain_fun,
    n_chains = 4L,
    chain_args = list(n = 16L),
    seed = 123L,
    workers = 1L,
    backend = "sequential",
    use_mojor_rng = FALSE
  )

  expect_equal(res1, res2, tolerance = 1e-12)
  expect_equal(length(res1), 4L)
})

test_that("mojor_run_chains_parallel fork backend matches sequential seeds", {
  skip_if(.Platform$OS.type == "windows")
  skip_if(tolower(Sys.info()[["sysname"]]) == "darwin")

  chain_fun <- function(n) stats::rnorm(n)
  seq_res <- mojor_run_chains_parallel(
    chain_fun = chain_fun,
    n_chains = 4L,
    chain_args = list(n = 16L),
    seed = 777L,
    workers = 1L,
    backend = "sequential",
    use_mojor_rng = FALSE
  )
  fork_res <- mojor_run_chains_parallel(
    chain_fun = chain_fun,
    n_chains = 4L,
    chain_args = list(n = 16L),
    seed = 777L,
    workers = 2L,
    backend = "fork",
    use_mojor_rng = FALSE
  )

  expect_equal(fork_res, seq_res, tolerance = 1e-12)
})

test_that("mojor_run_chains_parallel is deterministic with MojoR RNG", {  skip_if_no_mojo()

  chain_fun <- function(n) mojor_rnorm(n)

  res1 <- mojor_run_chains_parallel(
    chain_fun = chain_fun,
    n_chains = 3L,
    chain_args = list(n = 32L),
    seed = 2026L,
    workers = 1L,
    backend = "sequential",
    use_mojor_rng = TRUE
  )
  res2 <- mojor_run_chains_parallel(
    chain_fun = chain_fun,
    n_chains = 3L,
    chain_args = list(n = 32L),
    seed = 2026L,
    workers = 1L,
    backend = "sequential",
    use_mojor_rng = TRUE
  )

  expect_equal(res1, res2, tolerance = 1e-12)
})
