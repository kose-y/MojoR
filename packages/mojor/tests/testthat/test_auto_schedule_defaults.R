library(testthat)

test_that("auto schedule picks simd/tree reduction deterministically", {  base_args <- list(
    schedule = list(n_var = "n_i"),
    reduction_mode = "auto",
    unroll = NULL,
    tile = NULL,
    opt_level = 2L,
    loop_infos = list(list(blocks = list())),
    out_kind = "scalar",
    out_matrix = FALSE,
    parallel_safe = TRUE,
    scalar_reduction_eligible = TRUE,
    scalar_reduction_op = "sum",
    scalar_reduction_arg = "x",
    arg_specs = list(x = "f64[]"),
    broadcast_nd = FALSE
  )

  simd_policy <- do.call(.mojor_apply_auto_schedule_policy, c(base_args, list(simd_safe = TRUE)))
  expect_identical(simd_policy$reduction_effective, "simd")
  expect_identical(simd_policy$schedule$reduction, "simd")
  expect_identical(simd_policy$unroll_effective, 2L)

  tree_policy <- do.call(.mojor_apply_auto_schedule_policy, c(base_args, list(simd_safe = FALSE)))
  expect_identical(tree_policy$reduction_effective, "tree")
  expect_identical(tree_policy$schedule$reduction, "tree")
  expect_identical(tree_policy$unroll_effective, 2L)
})

test_that("auto schedule preserves explicit knobs", {  policy <- .mojor_apply_auto_schedule_policy(
    schedule = list(n_var = "n_i"),
    reduction_mode = "tree",
    unroll = 8L,
    tile = c(16L, 16L),
    opt_level = 3L,
    loop_infos = list(list(blocks = list(quote(for (j in 1:n) x <- x)))),
    out_kind = "vector",
    out_matrix = TRUE,
    parallel_safe = TRUE,
    simd_safe = TRUE,
    scalar_reduction_eligible = FALSE,
    scalar_reduction_op = NULL,
    scalar_reduction_arg = NULL,
    arg_specs = list(),
    broadcast_nd = FALSE,
    unroll_explicit = TRUE,
    reduction_explicit = TRUE,
    tile_explicit = TRUE
  )

  expect_identical(policy$reduction_effective, "tree")
  expect_identical(policy$unroll_effective, 8L)
  expect_equal(policy$tile_effective, c(16L, 16L))
})

test_that("auto schedule tiles nested matrix loops and respects broadcast guard", {  base_args <- list(
    schedule = list(n_var = "n_i"),
    reduction_mode = NULL,
    unroll = NULL,
    tile = NULL,
    opt_level = 3L,
    loop_infos = list(list(blocks = list(quote(for (j in 1:n) x <- x)))),
    out_kind = "vector",
    out_matrix = TRUE,
    parallel_safe = TRUE,
    simd_safe = TRUE,
    scalar_reduction_eligible = FALSE,
    scalar_reduction_op = NULL,
    scalar_reduction_arg = NULL,
    arg_specs = list()
  )

  tiled_policy <- do.call(.mojor_apply_auto_schedule_policy, c(base_args, list(broadcast_nd = FALSE)))
  expect_equal(tiled_policy$tile_effective, c(32L, 32L))
  expect_equal(tiled_policy$schedule$tile, c(32L, 32L))
  expect_identical(tiled_policy$policy$tile$mode, "auto")
  expect_identical(tiled_policy$policy$tile$reason, "nested matrix loop at high opt level")

  bnd_policy <- do.call(.mojor_apply_auto_schedule_policy, c(base_args, list(broadcast_nd = TRUE)))
  expect_null(bnd_policy$tile_effective)
  expect_null(bnd_policy$schedule$tile)
})

test_that("mojor_transpile reports requested vs effective schedule settings", {  f_sum <- function(x) {
    acc <- 0
    for (i in seq_along(x)) {
      acc <- acc + x[i]
    }
    acc
  }
  trans_sum <- mojor_transpile(f_sum, x = "f64[]", reduction = "auto", opt_level = 2L)
  expect_identical(trans_sum$reduction, "auto")
  expect_true(trans_sum$reduction_effective %in% c("auto", "tree", "simd", "linear"))
  expect_null(trans_sum$unroll)
  expect_true(is.null(trans_sum$unroll_effective) || is.numeric(trans_sum$unroll_effective))
  expect_true(!is.null(trans_sum$schedule))
  expect_true(!is.null(trans_sum$auto_schedule))
  if (!is.null(trans_sum$scalar_reduction_op) &&
      trans_sum$scalar_reduction_op %in% c("sum", "product", "min", "max", "which.min", "which.max")) {
    expect_true(trans_sum$reduction_effective %in% c("tree", "simd"))
    expect_identical(trans_sum$unroll_effective, 2L)
  }

  f_vec <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) {
      out[i] <- x[i] * 2
    }
    out
  }
  trans_vec <- mojor_transpile(f_vec, x = "f64[]", parallel = TRUE, opt_level = 2L)
  expect_null(trans_vec$unroll)
  expect_identical(trans_vec$unroll_effective, 4L)
})
