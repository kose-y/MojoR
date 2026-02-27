# Split from test_gpu_array.R (chunk 05).
# Shared utilities are in helper-gpu-array.R.

.mojor_skip_gpu_runtime_file_unless_opt_in()

test_that("GPUArray bracket assignment supports strict per-axis logical masks", {  .skip_if_no_gpu_backend(require_float = TRUE)

  m_ref <- matrix(as.numeric(1:20), nrow = 4L, ncol = 5L)
  gx <- GPUArray(m_ref, dtype = "f32")

  vals1 <- matrix(c(101, 201, 301, 401), nrow = 2L, ncol = 2L)
  gx[c(TRUE, FALSE, TRUE, FALSE), c(2L, 5L)] <- vals1
  m_ref[c(TRUE, FALSE, TRUE, FALSE), c(2L, 5L)] <- vals1
  route1 <- attr(gx, "gpu_fallback")
  expect_true(route1 %in% c("cpu_scatter", "gpu_scatter"))

  vals2 <- matrix(as.numeric(501:506), nrow = 2L, ncol = 3L)
  gx[c(1L, 4L), c(TRUE, FALSE, TRUE, FALSE, TRUE)] <- vals2
  m_ref[c(1L, 4L), c(TRUE, FALSE, TRUE, FALSE, TRUE)] <- vals2
  route2 <- attr(gx, "gpu_fallback")
  expect_true(route2 %in% c("cpu_scatter", "gpu_scatter"))
  expect_equal(mojor_gpu_array_read(gx), m_ref)

  a_ref <- array(as.numeric(1:60), dim = c(3L, 4L, 5L))
  ga <- GPUArray(a_ref, dtype = "f32")
  vals3 <- array(as.numeric(801:812), dim = c(2L, 2L, 3L))
  ga[c(TRUE, FALSE, TRUE), c(1L, 4L), c(TRUE, FALSE, TRUE, FALSE, TRUE)] <- vals3
  a_ref[c(TRUE, FALSE, TRUE), c(1L, 4L), c(TRUE, FALSE, TRUE, FALSE, TRUE)] <- vals3
  route3 <- attr(ga, "gpu_fallback")
  expect_true(route3 %in% c("cpu_scatter", "gpu_scatter"))
  expect_equal(mojor_gpu_array_read(ga), a_ref)

  expect_error({
    gx[c(TRUE, NA, FALSE, TRUE), c(1L, 2L)] <- 1
  }, "logical mask must not contain NA")
  expect_error({
    gx[c(TRUE, FALSE), c(1L, 2L)] <- 1
  }, "logical mask length must equal extent")
  gx_before_empty <- mojor_gpu_array_read(gx)
  gx[c(FALSE, FALSE, FALSE, FALSE), c(1L, 2L)] <- 1
  expect_equal(mojor_gpu_array_read(gx), gx_before_empty)
  gx[matrix(c(TRUE, FALSE, TRUE, FALSE), nrow = 2L), c(1L, 2L)] <- 1
  m_ref[matrix(c(TRUE, FALSE, TRUE, FALSE), nrow = 2L), c(1L, 2L)] <- 1
  expect_equal(mojor_gpu_array_read(gx), m_ref)
  expect_equal(attr(gx, "gpu_fallback"), "cpu_scatter")

  .mojor_gpu_array_free_all(ga, gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray bracket assignment supports strict per-axis logical masks for rank-4/5", {  .skip_if_no_gpu_backend(require_float = TRUE)

  ref4 <- array(as.numeric(1:120), dim = c(2L, 3L, 4L, 5L))
  g4 <- GPUArray(ref4, dtype = "f32")
  vals4 <- array(as.numeric(1001:1008), dim = c(2L, 2L, 2L))
  g4[c(TRUE, FALSE), c(1L, 3L), c(TRUE, FALSE, TRUE, FALSE), c(2L, 5L)] <- vals4
  ref4[c(TRUE, FALSE), c(1L, 3L), c(TRUE, FALSE, TRUE, FALSE), c(2L, 5L)] <- vals4
  route4 <- attr(g4, "gpu_fallback")
  expect_true(route4 %in% c("cpu_scatter", "gpu_scatter"))
  expect_equal(mojor_gpu_array_read(g4), ref4)

  ref5 <- array(as.numeric(1:144), dim = c(2L, 3L, 2L, 4L, 3L))
  g5 <- GPUArray(ref5, dtype = "f32")
  vals5 <- array(as.numeric(2001:2008), dim = c(2L, 2L, 2L))
  g5[c(TRUE, FALSE), c(1L, 3L), c(TRUE, FALSE), c(2L, 4L), c(TRUE, FALSE, TRUE)] <- vals5
  ref5[c(TRUE, FALSE), c(1L, 3L), c(TRUE, FALSE), c(2L, 4L), c(TRUE, FALSE, TRUE)] <- vals5
  route5 <- attr(g5, "gpu_fallback")
  expect_true(route5 %in% c("cpu_scatter", "gpu_scatter"))
  expect_equal(mojor_gpu_array_read(g5), ref5)

  expect_error({
    g4[c(TRUE, NA), c(1L, 3L), c(1L, 2L), c(1L, 2L)] <- 1
  }, "logical mask must not contain NA")
  expect_error({
    g4[c(TRUE, FALSE, TRUE), c(1L, 3L), c(1L, 2L), c(1L, 2L)] <- 1
  }, "logical mask length must equal extent")
  g4_before_empty <- mojor_gpu_array_read(g4)
  g4[c(FALSE, FALSE), c(1L, 3L), c(1L, 2L), c(1L, 2L)] <- 1
  expect_equal(mojor_gpu_array_read(g4), g4_before_empty)
  g4[matrix(c(TRUE, FALSE), nrow = 1L), c(1L, 3L), c(1L, 2L), c(1L, 2L)] <- 1
  ref4[matrix(c(TRUE, FALSE), nrow = 1L), c(1L, 3L), c(1L, 2L), c(1L, 2L)] <- 1
  expect_equal(mojor_gpu_array_read(g4), ref4)
  expect_equal(attr(g4, "gpu_fallback"), "cpu_scatter")

  .mojor_gpu_array_free_all(g5, g4)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray bracket assignment supports character dimname selectors", {  .skip_if_no_gpu_backend(require_float = TRUE)

  m_ref <- matrix(
    as.numeric(1:12),
    nrow = 3L,
    ncol = 4L,
    dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3", "c4"))
  )
  gx <- GPUArray(m_ref, dtype = "f32")

  gx[c("r1", "r3"), c("c2", "c4")] <- matrix(c(101, 201, 301, 401), nrow = 2L, ncol = 2L)
  m_ref[c("r1", "r3"), c("c2", "c4")] <- matrix(c(101, 201, 301, 401), nrow = 2L, ncol = 2L)
  expect_equal(mojor_gpu_array_read(gx), m_ref)
  expect_true(attr(gx, "gpu_fallback") %in% c("gpu_scatter", "cpu_scatter", "cpu_slice_assign"))

  expect_error({
    gx[c("r1", "r_missing"), "c2"] <- 1
  }, "contains unknown dimname labels")

  gx_no_names <- GPUArray(unname(m_ref), dtype = "f32")
  expect_error({
    gx_no_names["r1", 1L] <- 1
  }, "character indices require dimnames")

  m_partial <- matrix(
    as.numeric(1:12),
    nrow = 3L,
    ncol = 4L,
    dimnames = list(c("r_alpha", "r_beta", "r_gamma"), c("c_alpha", "c_beta", "c_gamma", "c_delta"))
  )
  gx_partial <- GPUArray(m_partial, dtype = "f32")
  gx_partial[c("r_g", "r_a"), c("c_d", "c_b")] <- matrix(c(1001, 2001, 3001, 4001), nrow = 2L, ncol = 2L)
  m_partial[c("r_gamma", "r_alpha"), c("c_delta", "c_beta")] <- matrix(c(1001, 2001, 3001, 4001), nrow = 2L, ncol = 2L)
  expect_equal(mojor_gpu_array_read(gx_partial), m_partial)
  expect_true(attr(gx_partial, "gpu_fallback") %in% c("gpu_scatter", "cpu_scatter", "cpu_slice_assign"))

  expect_error({
    gx_partial["r", "c_b"] <- 1
  }, "ambiguous partial dimname label")
  expect_error({
    gx_partial["r_missing", "c_b"] <- 1
  }, "contains unknown dimname labels")

  .mojor_gpu_array_free_all(gx_partial, gx_no_names, gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray scatter plan cache evicts beyond capacity", {  .skip_if_no_gpu_backend(require_float = TRUE)

  m_ref <- matrix(as.numeric(1:20), nrow = 4L, ncol = 5L)
  gx <- GPUArray(m_ref, dtype = "f32")
  count_start <- mojor_gpu_buf_f32_live_count()
  gpu_hits <- 0L

  patterns <- list(
    list(rows = c(1L, 3L), cols = c(2L, 5L)),
    list(rows = c(2L, 4L), cols = c(1L, 4L)),
    list(rows = c(4L, 1L), cols = c(3L, 5L)),
    list(rows = c(1L, 4L), cols = c(2L, 3L)),
    list(rows = c(3L, 1L), cols = c(5L, 2L)),
    list(rows = c(2L, 1L), cols = c(4L, 2L)),
    list(rows = c(4L, 2L), cols = c(1L, 3L)),
    list(rows = c(1L, 3L), cols = c(5L, 3L)),
    list(rows = c(2L, 4L), cols = c(2L, 5L)),
    list(rows = c(3L, 1L), cols = c(4L, 1L))
  )

  for (k in seq_along(patterns)) {
    p <- patterns[[k]]
    vals <- matrix(
      as.numeric((k * 100L + 1L):(k * 100L + length(p$rows) * length(p$cols))),
      nrow = length(p$rows),
      ncol = length(p$cols)
    )
    gx[p$rows, p$cols] <- vals
    m_ref[p$rows, p$cols] <- vals
    expect_equal(mojor_gpu_array_read(gx), m_ref)
    route <- attr(gx, "gpu_fallback")
    expect_true(route %in% c("cpu_scatter", "gpu_scatter"))
    if (identical(route, "gpu_scatter")) {
      gpu_hits <- gpu_hits + 1L
    }
  }

  plans <- .mojor_gpu_object_get(gx, "index_plans", default = NULL)
  plan_n <- if (is.environment(plans)) length(ls(plans, all.names = TRUE)) else 0L
  if (gpu_hits > 0L) {
    expect_true(is.environment(plans))
    expect_gte(plan_n, 1L)
    expect_lte(plan_n, 8L)
    expect_equal(mojor_gpu_buf_f32_live_count(), count_start + plan_n)
  } else {
    expect_equal(plan_n, 0L)
    expect_equal(mojor_gpu_buf_f32_live_count(), count_start)
  }

  mojor_gpu_array_free(gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), count_start - 1L)
})

test_that("GPUArray alternating gather/scatter uses shared bounded plan cache", {  .skip_if_no_gpu_backend(require_float = TRUE)

  m_ref <- matrix(as.numeric(1:20), nrow = 4L, ncol = 5L)
  gx <- GPUArray(m_ref, dtype = "f32")
  count_start <- mojor_gpu_buf_f32_live_count()

  patterns <- list(
    list(rows = c(1L, 3L), cols = c(2L, 5L)),
    list(rows = c(2L, 4L), cols = c(1L, 4L)),
    list(rows = c(4L, 1L), cols = c(3L, 5L)),
    list(rows = c(1L, 4L), cols = c(2L, 3L)),
    list(rows = c(3L, 1L), cols = c(5L, 2L)),
    list(rows = c(2L, 1L), cols = c(4L, 2L))
  )
  gpu_pattern_flags <- logical(length(patterns))
  gather_gpu_hits <- 0L
  scatter_gpu_hits <- 0L

  for (k in seq_along(patterns)) {
    p <- patterns[[k]]
    vals <- matrix(
      as.numeric((k * 1000L + 1L):(k * 1000L + length(p$rows) * length(p$cols))),
      nrow = length(p$rows),
      ncol = length(p$cols)
    )

    gx[p$rows, p$cols] <- vals
    m_ref[p$rows, p$cols] <- vals
    route_s <- attr(gx, "gpu_fallback")
    expect_true(route_s %in% c("cpu_scatter", "gpu_scatter"))
    if (identical(route_s, "gpu_scatter")) {
      scatter_gpu_hits <- scatter_gpu_hits + 1L
      gpu_pattern_flags[[k]] <- TRUE
    }
    expect_equal(mojor_gpu_array_read(gx), m_ref)

    g <- gx[p$rows, p$cols]
    expect_equal(mojor_gpu_array_read(g), m_ref[p$rows, p$cols, drop = TRUE])
    route_g <- attr(g, "gpu_fallback")
    expect_true(route_g %in% c("cpu_gather", "gpu_gather"))
    if (identical(route_g, "gpu_gather")) {
      gather_gpu_hits <- gather_gpu_hits + 1L
      gpu_pattern_flags[[k]] <- TRUE
    }
    mojor_gpu_array_free(g)
  }

  plans <- .mojor_gpu_object_get(gx, "index_plans", default = NULL)
  plan_n <- if (is.environment(plans)) length(ls(plans, all.names = TRUE)) else 0L
  gpu_pattern_n <- as.integer(sum(gpu_pattern_flags))
  if ((gather_gpu_hits + scatter_gpu_hits) > 0L) {
    expect_true(is.environment(plans))
    expect_gte(plan_n, 1L)
    expect_lte(plan_n, 8L)
    expect_equal(plan_n, gpu_pattern_n)
    expect_equal(mojor_gpu_buf_f32_live_count(), count_start + plan_n)
  } else {
    expect_equal(plan_n, 0L)
    expect_equal(mojor_gpu_buf_f32_live_count(), count_start)
  }

  mojor_gpu_array_free(gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), count_start - 1L)
})

test_that("GPUArray alternating gather/scatter cache invalidates on handle swap", {  .skip_if_no_gpu_backend(require_float = TRUE)

  rows <- c(1L, 3L)
  cols <- c(2L, 5L)
  m1 <- matrix(as.numeric(1:20), nrow = 4L, ncol = 5L)
  m2 <- matrix(as.numeric(201:220), nrow = 4L, ncol = 5L)
  gx <- GPUArray(m1, dtype = "f32")

  vals1 <- matrix(c(701, 702, 703, 704), nrow = 2L, ncol = 2L)
  gx[rows, cols] <- vals1
  m1[rows, cols] <- vals1
  route_s1 <- attr(gx, "gpu_fallback")
  g1 <- gx[rows, cols]
  route_g1 <- attr(g1, "gpu_fallback")
  expect_equal(mojor_gpu_array_read(g1), m1[rows, cols, drop = TRUE])
  mojor_gpu_array_free(g1)

  epoch_before <- .mojor_gpu_array_handle_epoch(gx)
  new_handle <- mojor_gpu_buf_f32(as.numeric(m2))
  gx <- .mojor_gpu_array_set_handle(gx, new_handle, free_old = TRUE, invalidate_plans = FALSE)
  gx <- .mojor_gpu_object_set(gx, "n", as.integer(length(m2)))
  gx <- .mojor_gpu_object_set(gx, "dim", as.integer(dim(m2)))
  gx <- .mojor_gpu_object_set(gx, "dimnames", NULL)
  gx <- .mojor_gpu_object_set(gx, "strides", .mojor_dim_strides(as.integer(dim(m2))))
  expect_equal(.mojor_gpu_array_handle_epoch(gx), epoch_before + 1L)

  g2 <- gx[rows, cols]
  route_g2 <- attr(g2, "gpu_fallback")
  expect_equal(mojor_gpu_array_read(g2), m2[rows, cols, drop = TRUE])
  mojor_gpu_array_free(g2)

  vals2 <- matrix(c(801, 802, 803, 804), nrow = 2L, ncol = 2L)
  gx[rows, cols] <- vals2
  m2[rows, cols] <- vals2
  route_s2 <- attr(gx, "gpu_fallback")
  expect_equal(mojor_gpu_array_read(gx), m2)

  routes <- c(route_s1, route_g1, route_g2, route_s2)
  expect_true(all(routes %in% c("cpu_scatter", "gpu_scatter", "cpu_gather", "gpu_gather")))

  plans <- .mojor_gpu_object_get(gx, "index_plans", default = NULL)
  if (is.environment(plans)) {
    keys <- ls(plans, all.names = TRUE)
    for (k in keys) {
      entry <- get(k, envir = plans, inherits = FALSE)
      if (is.list(entry) && !is.null(entry$plan)) {
        expect_true(identical(entry$handle_ref, .mojor_gpu_array_handle(gx)))
        expect_equal(entry$handle_epoch, .mojor_gpu_array_handle_epoch(gx))
      }
    }
  }

  mojor_gpu_array_free(gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray explicit index plan clear frees cached plans", {  .skip_if_no_gpu_backend(require_float = TRUE)

  m1 <- matrix(as.numeric(1:20), nrow = 4L, ncol = 5L)
  gx <- GPUArray(m1, dtype = "f32")

  g <- gx[c(1L, 3L), c(2L, 5L)]
  route <- attr(g, "gpu_fallback")
  expect_true(route %in% c("cpu_gather", "gpu_gather"))
  mojor_gpu_array_free(g)

  plans_before <- .mojor_gpu_object_get(gx, "index_plans", default = NULL)
  plan_n_before <- if (is.environment(plans_before)) length(ls(plans_before, all.names = TRUE)) else 0L
  live_before <- mojor_gpu_buf_f32_live_count()

  gx <- .mojor_gpu_array_clear_index_plans(gx)
  plans_after <- .mojor_gpu_object_get(gx, "index_plans", default = NULL)
  plan_n_after <- if (is.environment(plans_after)) length(ls(plans_after, all.names = TRUE)) else 0L
  expect_equal(plan_n_after, 0L)
  expect_equal(mojor_gpu_array_read(gx), m1)

  if (identical(route, "gpu_gather") && plan_n_before > 0L) {
    expect_equal(mojor_gpu_buf_f32_live_count(), live_before - plan_n_before)
  } else {
    expect_equal(mojor_gpu_buf_f32_live_count(), live_before)
  }

  mojor_gpu_array_free(gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("mojor_gpu_array_write updates metadata and index plan behavior for f32", {  .skip_if_no_gpu_backend(require_float = TRUE)
  .mojor_run_gpu_array_write_cases("f32", track_f32_count = TRUE)
})

test_that("mojor_gpu_array_write updates metadata and index plan behavior for f64", {  skip_if_no_gpu_f64()
  .mojor_run_gpu_array_write_cases("f64", track_f32_count = FALSE)
})

test_that("mojor_gpu_array_write accepts GPUArray sources with mixed dtypes", {  .skip_if_no_gpu_backend(require_float = TRUE)
  count0 <- mojor_gpu_buf_f32_live_count()
  target_host <- matrix(
    as.numeric(1:6),
    nrow = 2L,
    ncol = 3L,
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
  gx <- GPUArray(target_host, dtype = "f32")

  src_i32_host <- matrix(
    as.integer(101:106),
    nrow = 3L,
    ncol = 2L,
    dimnames = list(c("u1", "u2", "u3"), c("v1", "v2"))
  )
  gi <- GPUArray(src_i32_host, dtype = "i32")
  mojor_gpu_array_write(gx, gi)
  expect_equal(.mojor_gpu_array_dim(gx), as.integer(c(3L, 2L)))
  expect_equal(.mojor_gpu_array_dimnames(gx), dimnames(src_i32_host))
  expect_equal(
    mojor_gpu_array_read(gx),
    matrix(as.numeric(src_i32_host), nrow = 3L, ncol = 2L, dimnames = dimnames(src_i32_host))
  )

  src_vec <- GPUArray(as.numeric(1:6), dtype = "f32")
  mojor_gpu_array_write(gx, src_vec)
  expect_equal(.mojor_gpu_array_dim(gx), as.integer(c(3L, 2L)))
  expect_equal(.mojor_gpu_array_dimnames(gx), dimnames(src_i32_host))
  expect_equal(
    mojor_gpu_array_read(gx),
    matrix(as.numeric(1:6), nrow = 3L, ncol = 2L, dimnames = dimnames(src_i32_host))
  )

  .mojor_gpu_array_free_all(src_vec, gi, gx)
  expect_lte(mojor_gpu_buf_f32_live_count(), count0)
})

test_that("GPUArray handle epoch increments on handle updates", {  .skip_if_no_gpu_backend(require_float = TRUE)
  count0 <- mojor_gpu_buf_f32_live_count()

  m <- matrix(as.numeric(1:20), nrow = 4L, ncol = 5L)
  gx <- GPUArray(m, dtype = "f32")
  e0 <- .mojor_gpu_array_handle_epoch(gx)

  h1 <- mojor_gpu_buf_f32(as.numeric(m + 100))
  gx <- .mojor_gpu_array_set_handle(gx, h1, free_old = TRUE, invalidate_plans = FALSE)
  gx <- .mojor_gpu_object_set(gx, "n", as.integer(length(m)))
  gx <- .mojor_gpu_object_set(gx, "dim", as.integer(dim(m)))
  gx <- .mojor_gpu_object_set(gx, "dimnames", NULL)
  gx <- .mojor_gpu_object_set(gx, "strides", .mojor_dim_strides(as.integer(dim(m))))
  e1 <- .mojor_gpu_array_handle_epoch(gx)

  h2 <- mojor_gpu_buf_f32(as.numeric(m + 200))
  gx <- .mojor_gpu_array_set_handle(gx, h2, free_old = TRUE, invalidate_plans = FALSE)
  gx <- .mojor_gpu_object_set(gx, "n", as.integer(length(m)))
  gx <- .mojor_gpu_object_set(gx, "dim", as.integer(dim(m)))
  gx <- .mojor_gpu_object_set(gx, "dimnames", NULL)
  gx <- .mojor_gpu_object_set(gx, "strides", .mojor_dim_strides(as.integer(dim(m))))
  e2 <- .mojor_gpu_array_handle_epoch(gx)

  expect_equal(e1, e0 + 1L)
  expect_equal(e2, e1 + 1L)
  expect_equal(mojor_gpu_array_read(gx), m + 200)

  mojor_gpu_array_free(gx)
  expect_lte(mojor_gpu_buf_f32_live_count(), count0)
})

test_that("GPUArray same-handle epoch bump prunes stale plans on next use", {  .skip_if_no_gpu_backend(require_float = TRUE)
  count0 <- mojor_gpu_buf_f32_live_count()

  m <- matrix(as.numeric(1:20), nrow = 4L, ncol = 5L)
  gx <- GPUArray(m, dtype = "f32")
  rows <- c(1L, 3L)
  cols <- c(2L, 5L)

  g1 <- gx[rows, cols]
  route1 <- attr(g1, "gpu_fallback")
  expect_true(route1 %in% c("cpu_gather", "gpu_gather"))
  expect_equal(mojor_gpu_array_read(g1), m[rows, cols, drop = TRUE])
  mojor_gpu_array_free(g1)

  plans1 <- .mojor_gpu_object_get(gx, "index_plans", default = NULL)
  plan_n1 <- if (is.environment(plans1)) length(ls(plans1, all.names = TRUE)) else 0L
  live1 <- mojor_gpu_buf_f32_live_count()
  epoch1 <- .mojor_gpu_array_handle_epoch(gx)

  same_handle <- .mojor_gpu_array_handle(gx)
  gx <- .mojor_gpu_array_set_handle(gx, same_handle, free_old = FALSE, invalidate_plans = FALSE)
  epoch2 <- .mojor_gpu_array_handle_epoch(gx)
  expect_equal(epoch2, epoch1 + 1L)

  g2 <- gx[rows, cols]
  route2 <- attr(g2, "gpu_fallback")
  expect_true(route2 %in% c("cpu_gather", "gpu_gather"))
  expect_equal(mojor_gpu_array_read(g2), m[rows, cols, drop = TRUE])
  mojor_gpu_array_free(g2)

  plans2 <- .mojor_gpu_object_get(gx, "index_plans", default = NULL)
  plan_n2 <- if (is.environment(plans2)) length(ls(plans2, all.names = TRUE)) else 0L
  if (identical(route1, "gpu_gather") || identical(route2, "gpu_gather")) {
    expect_lte(plan_n2, 1L)
    if (is.environment(plans2) && plan_n2 > 0L) {
      entry <- get(ls(plans2, all.names = TRUE)[[1L]], envir = plans2, inherits = FALSE)
      expect_equal(entry$handle_epoch, epoch2)
    }
    expect_equal(mojor_gpu_buf_f32_live_count(), live1)
    if (identical(route1, "gpu_gather")) {
      expect_lte(plan_n1, 1L)
    }
  } else {
    expect_equal(plan_n1, 0L)
    expect_equal(plan_n2, 0L)
  }

  mojor_gpu_array_free(gx)
  expect_lte(mojor_gpu_buf_f32_live_count(), count0)
})

test_that("GPUArray bracket assignment supports n-d shorthand fallback", {  .skip_if_no_gpu_backend(require_float = TRUE)
  count0 <- mojor_gpu_buf_f32_live_count()

  a_ref <- array(as.numeric(1:24), dim = c(2L, 3L, 4L))
  ga <- GPUArray(a_ref, dtype = "f32")

  v1 <- matrix(as.numeric(101:112), nrow = 3L, ncol = 4L)
  ga[1, , ] <- v1
  a_ref[1, , ] <- v1

  v2 <- matrix(as.numeric(201:206), nrow = 2L, ncol = 3L)
  ga[, , 2] <- v2
  a_ref[, , 2] <- v2

  v3 <- matrix(as.numeric(301:308), nrow = 2L, ncol = 4L)
  ga[1, c(1L, 3L), ] <- v3
  a_ref[1, c(1L, 3L), ] <- v3

  expect_equal(mojor_gpu_array_read(ga), a_ref)
  expect_true(attr(ga, "gpu_fallback") %in% c("cpu_scatter", "gpu_scatter"))

  ga[c(1L, 1L), 2L, ] <- matrix(as.numeric(401:408), nrow = 2L, ncol = 4L)
  a_ref[c(1L, 1L), 2L, ] <- matrix(as.numeric(401:408), nrow = 2L, ncol = 4L)
  expect_equal(mojor_gpu_array_read(ga), a_ref)
  expect_true(attr(ga, "gpu_fallback") %in% c("cpu_scatter", "gpu_scatter"))

  mojor_gpu_array_free(ga)
  expect_lte(mojor_gpu_buf_f32_live_count(), count0)
})

test_that("GPUArray n-d gather falls back to linearized path when plan route is unavailable", {  .skip_if_no_gpu_backend(require_float = TRUE)
  count0 <- mojor_gpu_buf_f32_live_count()

  a_ref <- array(as.numeric(1:24), dim = c(2L, 3L, 4L))
  ga <- GPUArray(a_ref, dtype = "f32")
  idx_i <- 1L
  idx_j <- c(1L, 3L)

  gpu_env <- environment(.mojor_gpu_try_gather)
  orig_plan_cached <- get(".mojor_gpu_index_plan_cached", envir = gpu_env, inherits = FALSE)
  assign(".mojor_gpu_index_plan_cached", function(...) NULL, envir = gpu_env)
  on.exit(assign(".mojor_gpu_index_plan_cached", orig_plan_cached, envir = gpu_env), add = TRUE)

  g <- ga[idx_i, idx_j, ]
  expect_equal(mojor_gpu_array_read(g), a_ref[idx_i, idx_j, , drop = TRUE])
  expect_true(attr(g, "gpu_fallback") %in% c("gpu_gather", "cpu_gather"))

  .mojor_gpu_array_free_all(g, ga)
  expect_lte(mojor_gpu_buf_f32_live_count(), count0)
})

test_that("GPUArray bracket assignment enforces strict supported forms", {  .skip_if_no_gpu_backend(require_float = TRUE)

  gx <- GPUArray(matrix(as.numeric(1:12), nrow = 3L, ncol = 4L), dtype = "f32")
  expect_error({
    gx[1, ] <- c(1, 2, 3)
  }, "number of items to replace is not a multiple of replacement length")
  expect_error({
    gx[1:2, 1:2] <- matrix(1:3, nrow = 1L)
  }, "number of items to replace is not a multiple of replacement length")
  gx_before_empty <- mojor_gpu_array_read(gx)
  gx[integer(0), 1] <- 1
  expect_equal(mojor_gpu_array_read(gx), gx_before_empty)
  gx[0, 1] <- 1
  expect_equal(mojor_gpu_array_read(gx), gx_before_empty)
  gx[1, 2, ] <- 77
  expect_equal(mojor_gpu_array_read(gx)[1, 2], 77)
  gx[1, 1, 1] <- 101
  expect_equal(mojor_gpu_array_read(gx)[1, 1], 101)
  gx[1, 3, TRUE] <- 303
  expect_equal(mojor_gpu_array_read(gx)[1, 3], 303)
  gx[1, 1, 2] <- 1
  expect_equal(mojor_gpu_array_read(gx)[1, 1], 1)
  expect_error({
    gx[matrix(c(TRUE, FALSE, TRUE, FALSE), nrow = 2L), 1] <- 1
  })

  gv <- GPUArray(as.numeric(1:8), dtype = "f32")
  gv[1, 1] <- 1
  expect_equal(mojor_gpu_array_read(gv)[1], 1)
  gv[2, 1, ] <- 22
  expect_equal(mojor_gpu_array_read(gv)[2], 22)
  gv[3, 1, 1] <- 33
  expect_equal(mojor_gpu_array_read(gv)[3], 33)
  gv[4, NULL] <- 44
  expect_equal(mojor_gpu_array_read(gv)[4], 44)
  gv[5, integer(0)] <- 55
  expect_equal(mojor_gpu_array_read(gv)[5], 55)
  gv[6, 1, NULL] <- 66
  expect_equal(mojor_gpu_array_read(gv)[6], 66)
  gv[7, 1, integer(0)] <- 77
  expect_equal(mojor_gpu_array_read(gv)[7], 77)
  gv[8, logical(0)] <- 88
  expect_equal(mojor_gpu_array_read(gv)[8], 88)
  gv_before_rank1_empty <- mojor_gpu_array_read(gv)
  gv[2, 0] <- 777
  expect_equal(mojor_gpu_array_read(gv), gv_before_rank1_empty)
  gv[2, FALSE] <- 888
  expect_equal(mojor_gpu_array_read(gv), gv_before_rank1_empty)
  gv[2, 1, 0] <- 999
  expect_equal(mojor_gpu_array_read(gv), gv_before_rank1_empty)
  gv[1, c(TRUE, FALSE)] <- 123
  expect_equal(mojor_gpu_array_read(gv), gv_before_rank1_empty)
  gx_before_extra_mixed <- mojor_gpu_array_read(gx)
  gx[1, 1, c(TRUE, FALSE)] <- 321
  expect_equal(mojor_gpu_array_read(gx), gx_before_extra_mixed)
  gv[1, 2] <- 111
  expect_equal(mojor_gpu_array_read(gv)[1], 111)
  gv[1, 1, 2] <- 1
  expect_equal(mojor_gpu_array_read(gv)[1], 1)
  expect_error({
    gv[c(-1L, 2L)] <- 1
  }, "cannot mix positive and negative indices")

  .mojor_gpu_array_free_all(gv, gx)
})

test_that("GPUArray bracket assignment falls back to host parity for factor selectors", {  .skip_if_no_gpu_backend(require_float = TRUE)

  m_ref <- matrix(as.numeric(1:12), nrow = 3L, ncol = 4L)
  gx <- GPUArray(m_ref, dtype = "f32")

  idx_factor <- factor(c(3L, 1L), levels = c(1L, 2L, 3L))
  vals <- matrix(c(101, 201, 301, 401), nrow = 2L, ncol = 2L)
  gx[idx_factor, c(2L, 4L)] <- vals
  m_ref[idx_factor, c(2L, 4L)] <- vals

  expect_equal(mojor_gpu_array_read(gx), m_ref)
  expect_equal(attr(gx, "gpu_fallback"), "cpu_scatter")

  .mojor_gpu_array_free_all(gx)
})

test_that("GPUArray bracket assignment falls back to host parity for ND NULL selectors", {  .skip_if_no_gpu_backend(require_float = TRUE)

  ref <- array(
    as.numeric(1:24),
    dim = c(2L, 3L, 4L),
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"), c("z1", "z2", "z3", "z4"))
  )
  ga <- GPUArray(ref, dtype = "f32")

  ga[c(TRUE, FALSE), NULL, c(TRUE, FALSE)] <- 999
  ref[c(TRUE, FALSE), NULL, c(TRUE, FALSE)] <- 999
  expect_equal(mojor_gpu_array_read(ga), ref)
  expect_equal(attr(ga, "gpu_fallback"), "cpu_scatter")

  ga[1L, c(0L, 2L), NULL] <- 555
  ref[1L, c(0L, 2L), NULL] <- 555
  expect_equal(mojor_gpu_array_read(ga), ref)
  expect_equal(attr(ga, "gpu_fallback"), "cpu_scatter")

  ga[NULL, -1L, c(TRUE, FALSE, TRUE, FALSE)] <- 777
  ref[NULL, -1L, c(TRUE, FALSE, TRUE, FALSE)] <- 777
  expect_equal(mojor_gpu_array_read(ga), ref)
  expect_equal(attr(ga, "gpu_fallback"), "cpu_scatter")

  .mojor_gpu_array_free_all(ga)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray bracket assignment supports zero and negative integer subscripts", {  .skip_if_no_gpu_backend(require_float = TRUE)

  v_ref <- as.numeric(1:10)
  gv <- GPUArray(v_ref, dtype = "f32")

  gv[c(0L, 2L, 0L, 5L)] <- c(20, 50)
  v_ref[c(0L, 2L, 0L, 5L)] <- c(20, 50)
  expect_equal(mojor_gpu_array_read(gv), v_ref)

  gv[-c(1L, 4L, 8L)] <- 99
  v_ref[-c(1L, 4L, 8L)] <- 99
  expect_equal(mojor_gpu_array_read(gv), v_ref)

  m_ref <- matrix(as.numeric(1:12), nrow = 3L, ncol = 4L)
  gx <- GPUArray(m_ref, dtype = "f32")

  gx[-1L, -c(2L, 4L)] <- matrix(c(100, 200, 300, 400), nrow = 2L, ncol = 2L)
  m_ref[-1L, -c(2L, 4L)] <- matrix(c(100, 200, 300, 400), nrow = 2L, ncol = 2L)
  expect_equal(mojor_gpu_array_read(gx), m_ref)

  .mojor_gpu_array_free_all(gx, gv)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray bracket assignment supports exact-multiple recycling", {  .skip_if_no_gpu_backend(require_float = TRUE)

  ref <- matrix(as.numeric(1:12), nrow = 3L, ncol = 4L)
  gx <- GPUArray(ref, dtype = "f32")

  gx[1, ] <- c(10, 20)
  ref[1, ] <- c(10, 20)

  gx[1:2, 1:2] <- matrix(c(7, 9), nrow = 1L, ncol = 2L)
  ref[1:2, 1:2] <- matrix(c(7, 9), nrow = 1L, ncol = 2L)

  expect_equal(mojor_gpu_array_read(gx), ref)

  mojor_gpu_array_free(gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})

test_that("GPUArray bracket assignment supports matrix and logical-matrix selectors via cpu scatter parity", {  .skip_if_no_gpu_backend(require_float = TRUE)

  ref <- matrix(as.numeric(1:12), nrow = 3L, ncol = 4L)
  gx <- GPUArray(ref, dtype = "f32")

  idx_num <- rbind(c(1L, 1L), c(3L, 4L), c(2L, 2L))
  gx[idx_num] <- c(101, 202, 303)
  ref[idx_num] <- c(101, 202, 303)
  expect_equal(mojor_gpu_array_read(gx), ref)
  expect_equal(attr(gx, "gpu_fallback"), "cpu_scatter")

  idx_lgl <- matrix(rep(c(TRUE, FALSE, FALSE), 4L), nrow = 3L, ncol = 4L)
  gx[idx_lgl] <- 9
  ref[idx_lgl] <- 9
  expect_equal(mojor_gpu_array_read(gx), ref)
  expect_equal(attr(gx, "gpu_fallback"), "cpu_scatter")

  mojor_gpu_array_free(gx)
  expect_equal(mojor_gpu_buf_f32_live_count(), 0L)
})
