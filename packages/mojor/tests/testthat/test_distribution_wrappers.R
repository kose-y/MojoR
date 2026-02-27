library(testthat)

test_that("distribution d/p/q wrappers are registered", {  expect_true(exists(".mojor_stats_dist_aliases", mode = "character"))
  aliases <- get(".mojor_stats_dist_aliases")
  expect_true(is.character(aliases))
  expect_true(length(aliases) >= 61)
  for (alias in names(aliases)) {
    expect_true(exists(alias, mode = "function"), info = alias)
  }
})

.mojor_dist_sample_args <- function(base_name) {
  if (base_name == "pbirthday") {
    return(list(n = 23, classes = 365, coincident = 2))
  }
  if (base_name == "qbirthday") {
    return(list(prob = 0.5, classes = 365, coincident = 2))
  }
  if (base_name == "ptukey") {
    return(list(q = 2.0, nmeans = 4, df = 20, nranges = 1, lower.tail = TRUE, log.p = FALSE))
  }
  if (base_name == "qtukey") {
    return(list(p = 0.95, nmeans = 4, df = 20, nranges = 1, lower.tail = TRUE, log.p = FALSE))
  }

  prefix <- substr(base_name, 1, 1)
  dist <- substring(base_name, 2)
  discrete_dists <- c("binom", "geom", "hyper", "nbinom", "pois", "signrank", "wilcox")

  arg_name <- switch(prefix, d = "x", p = "q", q = "p")
  arg_val <- if (prefix == "q") 0.5 else if (dist %in% discrete_dists) 2 else 0.5
  args <- setNames(list(arg_val), arg_name)

  params <- switch(dist,
    beta = list(shape1 = 2, shape2 = 3),
    binom = list(size = 10, prob = 0.4),
    cauchy = list(location = 0, scale = 1),
    chisq = list(df = 5),
    exp = list(rate = 2),
    f = list(df1 = 5, df2 = 10),
    gamma = list(shape = 3, rate = 2),
    geom = list(prob = 0.3),
    hyper = list(m = 15, n = 10, k = 6),
    logis = list(location = 0, scale = 1),
    lnorm = list(meanlog = 0, sdlog = 1),
    nbinom = list(size = 4, prob = 0.6),
    norm = list(mean = 0, sd = 1),
    pois = list(lambda = 3),
    t = list(df = 8),
    unif = list(min = 0, max = 1),
    weibull = list(shape = 2, scale = 1.5),
    signrank = list(n = 7),
    wilcox = list(m = 4, n = 6),
    stop("Unhandled distribution in test: ", base_name)
  )

  c(args, params)
}

test_that("distribution d/p/q wrappers match stats implementations", {  aliases <- get(".mojor_stats_dist_aliases")
  for (alias in names(aliases)) {
    base_name <- aliases[[alias]]
    args <- .mojor_dist_sample_args(base_name)
    got <- do.call(get(alias, mode = "function"), args)
    expected <- do.call(getFromNamespace(base_name, "stats"), args)
    expect_equal(got, expected, tolerance = 1e-12, info = alias)
  }
})

test_that("mojor_dnorm and mojor_dpois fast path match stats for scalar params", {  skip_if_no_mojo()

  x_norm <- seq(-3, 3, length.out = 2048)
  expect_equal(
    mojor_dnorm(x_norm, mean = 0.25, sd = 1.5),
    stats::dnorm(x_norm, mean = 0.25, sd = 1.5),
    tolerance = 1e-12
  )
  expect_equal(
    mojor_dnorm(x_norm, mean = -0.4, sd = 0.9, log = TRUE),
    stats::dnorm(x_norm, mean = -0.4, sd = 0.9, log = TRUE),
    tolerance = 1e-12
  )

  x_pois <- seq(0, 1024)
  expect_equal(
    mojor_dpois(x_pois, lambda = 4.2),
    stats::dpois(x_pois, lambda = 4.2),
    tolerance = 1e-12
  )
  expect_equal(
    mojor_dpois(x_pois, lambda = 2.7, log = TRUE),
    stats::dpois(x_pois, lambda = 2.7, log = TRUE),
    tolerance = 1e-12
  )
})

test_that("mojor_dnorm and mojor_dpois preserve stats fallback semantics", {  x <- 0:9

  expect_equal(
    mojor_dnorm(x, mean = c(0, 1), sd = c(1, 2), log = FALSE),
    stats::dnorm(x, mean = c(0, 1), sd = c(1, 2), log = FALSE),
    tolerance = 1e-12
  )
  expect_equal(
    mojor_dpois(x, lambda = c(1, 2, 3), log = FALSE),
    stats::dpois(x, lambda = c(1, 2, 3), log = FALSE),
    tolerance = 1e-12
  )
})
