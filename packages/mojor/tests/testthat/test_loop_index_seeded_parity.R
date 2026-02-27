library(testthat)

test_that("seeded parity matrix: scalar-in-loop selector wrappers match base across strict/non-strict and bounds", {
  f_loop <- function(x, n) {
    out <- numeric(n)
    for (i in +(seq.int(1L, n))) {
      out[(i)] <- x[(i)] + 1.0
    }
    out
  }
  f_read_scalar <- function(x, idx, n) {
    out <- numeric(n)
    for (i in +(seq.int(1L, n))) {
      out[(i)] <- x[+(idx[i])]
    }
    out
  }
  f_write_scalar <- function(x, idx, n) {
    out <- numeric(n)
    for (i in +(seq.int(1L, n))) {
      out[+(idx[i])] <- x[(i)]
    }
    out
  }

  configs <- expand.grid(
    strict = c(TRUE, FALSE),
    bounds = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  seeds <- c(101L, 211L, 307L, 401L, 503L, 601L)

  skip_if_no_mojo()
  for (cfg_i in seq_len(nrow(configs))) {
    cfg <- configs[cfg_i, ]
    mode_tag <- if (isTRUE(cfg$strict)) "strict" else "non_strict"
    bounds_tag <- if (isTRUE(cfg$bounds)) "bounds_on" else "bounds_off"
    .mojor_test_local_options(ir_only = isTRUE(cfg$strict), index_bounds = isTRUE(cfg$bounds))

    built_loop <- mojor_build(
      f_loop,
      x = "f64[]",
      n = "i32",
      bounds_check = isTRUE(cfg$bounds),
      name = paste0("idx_seed_loop_", mode_tag, "_", bounds_tag)
    )
    built_read <- mojor_build(
      f_read_scalar,
      x = "f64[]",
      idx = "i32[]",
      n = "i32",
      bounds_check = isTRUE(cfg$bounds),
      name = paste0("idx_seed_read_scalar_", mode_tag, "_", bounds_tag)
    )
    built_write <- mojor_build(
      f_write_scalar,
      x = "f64[]",
      idx = "i32[]",
      n = "i32",
      bounds_check = isTRUE(cfg$bounds),
      name = paste0("idx_seed_write_scalar_", mode_tag, "_", bounds_tag)
    )

    for (seed in seeds) {
      set.seed(seed)
      n <- sample(3L:8L, size = 1L)
      x <- as.numeric(stats::runif(n, min = -5, max = 5))
      idx <- sample(seq_len(n), size = n, replace = FALSE)

      expect_equal(built_loop$func(x, n), f_loop(x, n), info = paste(mode_tag, bounds_tag, "loop", seed))
      expect_equal(built_read$func(x, idx, n), f_read_scalar(x, idx, n), info = paste(mode_tag, bounds_tag, "read", seed))
      expect_equal(built_write$func(x, idx, n), f_write_scalar(x, idx, n), info = paste(mode_tag, bounds_tag, "write", seed))
    }
  }
})

test_that("seeded OOB matrix: scalar-in-loop selectors respect bounds mode", {
  f_read_scalar <- function(x, idx, n) {
    out <- numeric(n)
    for (i in +(seq.int(1L, n))) {
      out[(i)] <- x[+(idx[i])]
    }
    out
  }
  f_write_scalar <- function(x, idx, n) {
    out <- numeric(n)
    for (i in +(seq.int(1L, n))) {
      out[+(idx[i])] <- x[(i)]
    }
    out
  }
  emulate_bounds_off_write <- function(x, idx, n) {
    out <- numeric(n)
    for (i in seq_len(n)) {
      k <- idx[[i]]
      if (!is.na(k) && k >= 1L && k <= n) {
        out[[k]] <- x[[i]]
      }
    }
    out
  }

  configs <- expand.grid(
    strict = c(TRUE, FALSE),
    bounds = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  seeds <- c(13L, 31L, 79L)

  skip_if_no_mojo()
  for (cfg_i in seq_len(nrow(configs))) {
    cfg <- configs[cfg_i, ]
    mode_tag <- if (isTRUE(cfg$strict)) "strict" else "non_strict"
    bounds_tag <- if (isTRUE(cfg$bounds)) "bounds_on" else "bounds_off"
    .mojor_test_local_options(ir_only = isTRUE(cfg$strict), index_bounds = isTRUE(cfg$bounds))

    built_read <- mojor_build(
      f_read_scalar,
      x = "f64[]",
      idx = "i32[]",
      n = "i32",
      bounds_check = isTRUE(cfg$bounds),
      na_mode = "unsafe",
      name = paste0("idx_seed_oob_read_scalar_", mode_tag, "_", bounds_tag)
    )
    built_write <- mojor_build(
      f_write_scalar,
      x = "f64[]",
      idx = "i32[]",
      n = "i32",
      bounds_check = isTRUE(cfg$bounds),
      na_mode = "unsafe",
      name = paste0("idx_seed_oob_write_scalar_", mode_tag, "_", bounds_tag)
    )

    for (seed in seeds) {
      set.seed(seed)
      n <- sample(4L:9L, size = 1L)
      x <- as.numeric(stats::runif(n, min = -3, max = 3))
      idx <- seq_len(n)
      idx[[2L]] <- n + 1L
      idx[[3L]] <- 0L

      if (isTRUE(cfg$bounds)) {
        expect_error(
          built_read$func(x, idx, n),
          "Index out of bounds",
          info = paste(mode_tag, bounds_tag, "read", seed)
        )
        expect_error(
          built_write$func(x, idx, n),
          "Index out of bounds",
          info = paste(mode_tag, bounds_tag, "write", seed)
        )
      } else {
        read_res <- built_read$func(x, idx, n)
        expect_equal(length(read_res), n, info = paste(mode_tag, bounds_tag, "read_len", seed))
        expect_equal(read_res[[1L]], x[[1L]], info = paste(mode_tag, bounds_tag, "read_first", seed))
        expect_true(is.nan(read_res[[2L]]) || is.na(read_res[[2L]]), info = paste(mode_tag, bounds_tag, "read_oob_hi", seed))
        expect_true(is.nan(read_res[[3L]]) || is.na(read_res[[3L]]), info = paste(mode_tag, bounds_tag, "read_oob_zero", seed))

        write_res <- built_write$func(x, idx, n)
        expect_equal(
          write_res,
          emulate_bounds_off_write(x, idx, n),
          info = paste(mode_tag, bounds_tag, "write", seed)
        )
      }
    }
  }
})
