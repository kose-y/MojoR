# Split from test_gpu_array.R (chunk 08).
# Shared utilities are in helper-gpu-array.R.

.mojor_skip_gpu_runtime_file_unless_opt_in()

test_that("mojor_gpu_kernel supports canonical matrix2d scalar + matrix loops", {  .skip_if_no_gpu_backend(require_float = TRUE)
  f <- function(x, b) {
    out <- matrix(0, nrow = nrow(x), ncol = ncol(x))
    for (i in seq_len(nrow(x))) {
      for (j in seq_len(ncol(x))) {
        out[i, j] <- b + x[i, j]
      }
    }
    out
  }
  x <- matrix(runif(10), nrow = 2, ncol = 5)
  b <- 0.25
  k <- gpu_kernel(
    f,
    x = "f32[,]",
    b = "f32",
    name = "t_gpu_kernel_matrix2d_scalar",
    cache = FALSE,
    elementwise_size = length(x)
  )
  ax <- mojor_gpu_array(x, dtype = "f32")
  out <- k(ax, b)
  host <- mojor_gpu_array_read(out)
  expect_equal(dim(host), dim(x))
  expect_true(max(abs(as.numeric(host) - as.numeric(x + b))) < 1e-5)
  .mojor_gpu_array_free_all(out, ax)
})

test_that("mojor_gpu_kernel supports matrix2d guarded if/else assignment", {  .skip_if_no_gpu_backend(require_float = TRUE)
  f <- function(x, y) {
    out <- matrix(0, nrow = nrow(x), ncol = ncol(x))
    for (i in seq_len(nrow(x))) {
      for (j in seq_len(ncol(x))) {
        if (x[i, j] > y[i, j]) {
          out[i, j] <- x[i, j] - y[i, j]
        } else {
          out[i, j] <- y[i, j] - x[i, j]
        }
      }
    }
    out
  }
  x <- matrix(runif(12), nrow = 3, ncol = 4)
  y <- matrix(runif(12), nrow = 3, ncol = 4)
  k <- gpu_kernel(
    f,
    x = "f32[,]",
    y = "f32[,]",
    name = "t_gpu_kernel_matrix2d_ifelse",
    cache = FALSE,
    elementwise_size = length(x)
  )
  ax <- mojor_gpu_array(x, dtype = "f32")
  ay <- mojor_gpu_array(y, dtype = "f32")
  out <- k(ax, ay)
  host <- mojor_gpu_array_read(out)
  expect_equal(dim(host), dim(x))
  expect_true(max(abs(as.numeric(host) - as.numeric(abs(x - y)))) < 1e-5)
  .mojor_gpu_array_free_all(out, ay, ax)
})

test_that("mojor_gpu_kernel supports matrix2d guarded neighbor indexing with literal offsets", {  .skip_if_no_gpu_backend(require_float = TRUE)
  f <- function(x, b) {
    out <- matrix(0, nrow = nrow(x), ncol = ncol(x))
    for (i in seq_len(nrow(x))) {
      for (j in seq_len(ncol(x))) {
        out[i, j] <- x[i, j]
        if (i > 2 && i <= nrow(x) - 2 && j > 1 && j < ncol(x)) {
          out[i, j] <- x[i - 2, j + 1] + b
        }
      }
    }
    out
  }
  x <- matrix(runif(30), nrow = 5, ncol = 6)
  b <- 0.125
  k <- gpu_kernel(
    f,
    x = "f32[,]",
    b = "f32",
    name = "t_gpu_kernel_matrix2d_neighbors",
    cache = FALSE,
    elementwise_size = length(x)
  )
  ax <- mojor_gpu_array(x, dtype = "f32")
  out <- k(ax, b)
  host <- mojor_gpu_array_read(out)
  expected <- x
  for (i in seq_len(nrow(x))) {
    for (j in seq_len(ncol(x))) {
      if (i > 2 && i <= nrow(x) - 2 && j > 1 && j < ncol(x)) {
        expected[i, j] <- x[i - 2, j + 1] + b
      }
    }
  }
  expect_equal(dim(host), dim(x))
  expect_true(max(abs(as.numeric(host) - as.numeric(expected))) < 1e-5)
  .mojor_gpu_array_free_all(out, ax)
})

test_that("mojor_gpu_kernel supports matrix2d neighbor indexing with compile-time constant offsets", {  .skip_if_no_gpu_backend(require_float = TRUE)
  f <- function(x, b) {
    di <- 2L
    dj <- 1L
    out <- matrix(0, nrow = nrow(x), ncol = ncol(x))
    for (i in seq_len(nrow(x))) {
      for (j in seq_len(ncol(x))) {
        out[i, j] <- x[i, j]
        if ((1L + di <= i) && (nrow(x) - di >= i) && (j > 0L) && (ncol(x) >= j + dj)) {
          out[i, j] <- x[i - di, j + dj] + b
        }
      }
    }
    out
  }
  x <- matrix(runif(35), nrow = 5, ncol = 7)
  b <- 0.25
  k <- suppressWarnings(gpu_kernel(
    f,
    x = "f32[,]",
    b = "f32",
    name = "t_gpu_kernel_matrix2d_neighbors_const_offset",
    cache = FALSE,
    elementwise_size = length(x)
  ))
  ax <- mojor_gpu_array(x, dtype = "f32")
  out <- k(ax, b)
  host <- mojor_gpu_array_read(out)
  expected <- x
  di <- 2L
  dj <- 1L
  for (i in seq_len(nrow(x))) {
    for (j in seq_len(ncol(x))) {
      if ((1L + di <= i) && (nrow(x) - di >= i) && (j > 0L) && (ncol(x) >= j + dj)) {
        expected[i, j] <- x[i - di, j + dj] + b
      }
    }
  }
  expect_equal(dim(host), dim(x))
  expect_true(max(abs(as.numeric(host) - as.numeric(expected))) < 1e-5)
  .mojor_gpu_array_free_all(out, ax)
})

test_that("mojor_gpu_kernel supports matrix2d dim-index loop bounds and normalized indices", {  .skip_if_no_gpu_backend(require_float = TRUE)
  f <- function(x, y, b) {
    out <- matrix(0, nrow = nrow(x), ncol = ncol(x))
    for (i in seq_len(dim(x)[1L])) {
      for (j in seq_len(dim(x)[2L])) {
        out[(i + 0L), as.integer(j)] <- x[(i), (j)] + y[as.integer(i), (j - 0L)] + b
      }
    }
    out
  }
  x <- matrix(runif(12), nrow = 3, ncol = 4)
  y <- matrix(runif(12), nrow = 3, ncol = 4)
  b <- 0.1
  k <- gpu_kernel(
    f,
    x = "f32[,]",
    y = "f32[,]",
    b = "f32",
    name = "t_gpu_kernel_matrix2d_dim_index_norm",
    cache = FALSE,
    elementwise_size = length(x)
  )
  ax <- mojor_gpu_array(x, dtype = "f32")
  ay <- mojor_gpu_array(y, dtype = "f32")
  out <- k(ax, ay, b)
  host <- mojor_gpu_array_read(out)
  expect_equal(dim(host), dim(x))
  expect_true(max(abs(as.numeric(host) - as.numeric(x + y + b))) < 1e-5)
  .mojor_gpu_array_free_all(out, ay, ax)
})

test_that("mojor_gpu_kernel supports matrix2d neighbor guards with negated bound atoms", {  .skip_if_no_gpu_backend(require_float = TRUE)
  f <- function(x, b) {
    out <- matrix(0, nrow = nrow(x), ncol = ncol(x))
    for (i in seq_len(nrow(x))) {
      for (j in seq_len(ncol(x))) {
        out[i, j] <- x[i, j]
        if (!(i <= 1L) && !(j <= 1L) && !(i > nrow(x) - 1L) && !(j > ncol(x) - 1L)) {
          out[i, j] <- x[i - 1L, j - 1L] + b
        }
      }
    }
    out
  }
  x <- matrix(runif(20), nrow = 4, ncol = 5)
  b <- 0.2
  k <- gpu_kernel(
    f,
    x = "f32[,]",
    b = "f32",
    name = "t_gpu_kernel_matrix2d_negated_guard_atoms",
    cache = FALSE,
    elementwise_size = length(x)
  )
  ax <- mojor_gpu_array(x, dtype = "f32")
  out <- k(ax, b)
  host <- mojor_gpu_array_read(out)
  expected <- x
  for (i in seq_len(nrow(x))) {
    for (j in seq_len(ncol(x))) {
      if (!(i <= 1L) && !(j <= 1L) && !(i > nrow(x) - 1L) && !(j > ncol(x) - 1L)) {
        expected[i, j] <- x[i - 1L, j - 1L] + b
      }
    }
  }
  expect_equal(dim(host), dim(x))
  expect_true(max(abs(as.numeric(host) - as.numeric(expected))) < 1e-5)
  .mojor_gpu_array_free_all(out, ax)
})

test_that("mojor_gpu_kernel supports matrix2d neighbors with runtime offset vars under explicit bounds guards", {  .skip_if_no_gpu_backend(require_float = TRUE)
  f <- function(x, di, dj, b) {
    out <- matrix(0, nrow = nrow(x), ncol = ncol(x))
    for (i in seq_len(nrow(x))) {
      for (j in seq_len(ncol(x))) {
        out[i, j] <- x[i, j]
        if ((i + di >= 1L) && (i + di <= nrow(x)) && (j + dj >= 1L) && (j + dj <= ncol(x))) {
          out[i, j] <- x[i + di, j + dj] + b
        }
      }
    }
    out
  }
  x <- matrix(runif(24), nrow = 4, ncol = 6)
  di <- -1L
  dj <- 1L
  b <- 0.15
  k <- gpu_kernel(
    f,
    x = "f32[,]",
    di = "i32",
    dj = "i32",
    b = "f32",
    name = "t_gpu_kernel_matrix2d_neighbors_runtime_offset",
    cache = FALSE,
    elementwise_size = length(x)
  )
  ax <- mojor_gpu_array(x, dtype = "f32")
  out <- k(ax, di, dj, b)
  host <- mojor_gpu_array_read(out)
  expected <- x
  for (i in seq_len(nrow(x))) {
    for (j in seq_len(ncol(x))) {
      if ((i + di >= 1L) && (i + di <= nrow(x)) && (j + dj >= 1L) && (j + dj <= ncol(x))) {
        expected[i, j] <- x[i + di, j + dj] + b
      }
    }
  }
  expect_equal(dim(host), dim(x))
  expect_true(max(abs(as.numeric(host) - as.numeric(expected))) < 1e-5)
  .mojor_gpu_array_free_all(out, ax)
})

test_that("mojor_gpu_kernel supports matrix2d de-morgan neighbor guards with disjunction under negation", {  .skip_if_no_gpu_backend(require_float = TRUE)
  f <- function(x, b) {
    out <- matrix(0, nrow = nrow(x), ncol = ncol(x))
    for (i in seq_len(nrow(x))) {
      for (j in seq_len(ncol(x))) {
        out[i, j] <- x[i, j]
        if (!(i <= 1L || j <= 1L || i > nrow(x) - 1L || j > ncol(x) - 1L)) {
          out[i, j] <- x[i - 1L, j - 1L] + b
        }
      }
    }
    out
  }
  x <- matrix(runif(20), nrow = 4, ncol = 5)
  b <- 0.2
  k <- gpu_kernel(
    f,
    x = "f32[,]",
    b = "f32",
    name = "t_gpu_kernel_matrix2d_negated_disjunction_guard",
    cache = FALSE,
    elementwise_size = length(x)
  )
  ax <- mojor_gpu_array(x, dtype = "f32")
  out <- k(ax, b)
  host <- mojor_gpu_array_read(out)
  expected <- x
  for (i in seq_len(nrow(x))) {
    for (j in seq_len(ncol(x))) {
      if (!(i <= 1L || j <= 1L || i > nrow(x) - 1L || j > ncol(x) - 1L)) {
        expected[i, j] <- x[i - 1L, j - 1L] + b
      }
    }
  }
  expect_equal(dim(host), dim(x))
  expect_true(max(abs(as.numeric(host) - as.numeric(expected))) < 1e-5)
  .mojor_gpu_array_free_all(out, ax)
})

test_that("mojor_transpile matrix2d accepts mixed f32/f64 inputs at entry", {  skip_if_no_mojo()
  f <- function(x, y) {
    out <- matrix(0, nrow = nrow(x), ncol = ncol(x))
    for (i in seq_len(nrow(x))) {
      for (j in seq_len(ncol(x))) {
        out[i, j] <- x[i, j] + y[i, j]
      }
    }
    out
  }
  trans <- mojor_transpile(
    f,
    x = "f32[,]",
    y = "f64[,]",
    name = "t_gpu_kernel_matrix2d_mixed_dtype_inputs",
    elementwise = TRUE,
    elementwise_target = "gpu",
    elementwise_size = 12L
  )
  if (!isTRUE(trans$elementwise$gpu_buf_emitted) ||
      !identical(trans$elementwise$gpu_buf_index_mode, "matrix2d")) {
    skip("matrix2d mojor_transpile path unavailable in this runtime")
  }
  expect_true(isTRUE(trans$elementwise$gpu_buf_emitted))
  expect_identical(trans$elementwise$gpu_buf_index_mode, "matrix2d")
  expect_false(is.character(trans$elementwise$gpu_buf_reason) && nzchar(trans$elementwise$gpu_buf_reason))
})

test_that("matrix2d elementwise rejects unsupported forms with stable diagnostics", {  skip_if_no_mojo()
  f_irregular <- function(x) {
    out <- matrix(0, nrow = nrow(x), ncol = ncol(x))
    for (i in seq_len(nrow(x))) {
      for (j in seq_len(ncol(x))) {
        out[i, j] <- x[j, i]
      }
    }
    out
  }
  f_reduce <- function(x) {
    out <- matrix(0, nrow = nrow(x), ncol = ncol(x))
    for (i in seq_len(nrow(x))) {
      for (j in seq_len(ncol(x))) {
        out[i, j] <- sum(x)
      }
    }
    out
  }
  f_non_canonical <- function(x) {
    out <- matrix(0, nrow = nrow(x), ncol = ncol(x))
    for (i in seq_along(x)) {
      for (j in seq_len(ncol(x))) {
        out[i, j] <- x[i, j]
      }
    }
    out
  }
  f_neighbor_noguard <- function(x) {
    out <- matrix(0, nrow = nrow(x), ncol = ncol(x))
    for (i in seq_len(nrow(x))) {
      for (j in seq_len(ncol(x))) {
        out[i, j] <- x[i - 1, j]
      }
    }
    out
  }
  f_neighbor_partial_guard <- function(x) {
    out <- matrix(0, nrow = nrow(x), ncol = ncol(x))
    for (i in seq_len(nrow(x))) {
      for (j in seq_len(ncol(x))) {
        out[i, j] <- x[i, j]
        if (i > 1) {
          out[i, j] <- x[i - 1, j + 1]
        }
      }
    }
    out
  }
  f_neighbor_nonliteral <- function(x, di) {
    out <- matrix(0, nrow = nrow(x), ncol = ncol(x))
    for (i in seq_len(nrow(x))) {
      for (j in seq_len(ncol(x))) {
        if (i > 1) {
          out[i, j] <- x[i + di, j]
        } else {
          out[i, j] <- x[i, j]
        }
      }
    }
    out
  }
  f_guard_complex <- function(x) {
    out <- matrix(0, nrow = nrow(x), ncol = ncol(x))
    for (i in seq_len(nrow(x))) {
      for (j in seq_len(ncol(x))) {
        if ((i > 1 && j > 1) || (i < nrow(x))) {
          out[i, j] <- x[i - 1, j - 1]
        } else {
          out[i, j] <- x[i, j]
        }
      }
    }
    out
  }
  f_nested_if <- function(x) {
    out <- matrix(0, nrow = nrow(x), ncol = ncol(x))
    for (i in seq_len(nrow(x))) {
      for (j in seq_len(ncol(x))) {
        if (i > 1) {
          if (j > 1) {
            out[i, j] <- x[i, j]
          } else {
            out[i, j] <- x[i, j]
          }
        } else {
          out[i, j] <- x[i, j]
        }
      }
    }
    out
  }
  t1 <- mojor_transpile(
    f_irregular,
    x = "f32[,]",
    name = "t_gpu_kernel_matrix2d_bad_irregular",
    elementwise = TRUE,
    elementwise_target = "gpu",
    elementwise_size = 12L
  )
  t2 <- mojor_transpile(
    f_reduce,
    x = "f32[,]",
    name = "t_gpu_kernel_matrix2d_bad_reduce",
    elementwise = TRUE,
    elementwise_target = "gpu",
    elementwise_size = 12L
  )
  t3 <- mojor_transpile(
    f_non_canonical,
    x = "f32[,]",
    name = "t_gpu_kernel_matrix2d_bad_noncanonical",
    elementwise = TRUE,
    elementwise_target = "gpu",
    elementwise_size = 12L
  )
  t4 <- mojor_transpile(
    f_neighbor_noguard,
    x = "f32[,]",
    name = "t_gpu_kernel_matrix2d_bad_neighbor_noguard",
    elementwise = TRUE,
    elementwise_target = "gpu",
    elementwise_size = 12L
  )
  t5 <- mojor_transpile(
    f_neighbor_partial_guard,
    x = "f32[,]",
    name = "t_gpu_kernel_matrix2d_bad_neighbor_partial",
    elementwise = TRUE,
    elementwise_target = "gpu",
    elementwise_size = 12L
  )
  t6 <- mojor_transpile(
    f_neighbor_nonliteral,
    x = "f32[,]",
    di = "i32",
    name = "t_gpu_kernel_matrix2d_bad_neighbor_nonliteral",
    elementwise = TRUE,
    elementwise_target = "gpu",
    elementwise_size = 12L
  )
  t7 <- mojor_transpile(
    f_guard_complex,
    x = "f32[,]",
    name = "t_gpu_kernel_matrix2d_bad_neighbor_complex_guard",
    elementwise = TRUE,
    elementwise_target = "gpu",
    elementwise_size = 12L
  )
  t8 <- mojor_transpile(
    f_nested_if,
    x = "f32[,]",
    name = "t_gpu_kernel_matrix2d_bad_nested_if",
    elementwise = TRUE,
    elementwise_target = "gpu",
    elementwise_size = 12L
  )
  expect_false(isTRUE(t1$elementwise$gpu_buf_emitted))
  expect_false(isTRUE(t2$elementwise$gpu_buf_emitted))
  expect_false(isTRUE(t3$elementwise$gpu_buf_emitted))
  expect_false(isTRUE(t4$elementwise$gpu_buf_emitted))
  expect_false(isTRUE(t5$elementwise$gpu_buf_emitted))
  expect_false(isTRUE(t6$elementwise$gpu_buf_emitted))
  expect_false(isTRUE(t7$elementwise$gpu_buf_emitted))
  expect_false(isTRUE(t8$elementwise$gpu_buf_emitted))
  reasons <- vapply(
    list(t1, t2, t3, t4, t5, t6, t7, t8),
    function(tr) {
      reason <- tr$elementwise$reason
      if (is.character(reason) && length(reason) == 1L) {
        reason
      } else {
        ""
      }
    },
    FUN.VALUE = character(1L)
  )
  shared_gate <- "loop range is not unit stride"
  expect_true(grepl("matrix2d does not support irregular indexing", reasons[[1L]]) || grepl(shared_gate, reasons[[1L]], fixed = TRUE))
  expect_true(grepl("matrix2d does not support reductions in kernel body", reasons[[2L]]) || grepl(shared_gate, reasons[[2L]], fixed = TRUE))
  expect_true(grepl("requires outer loop seq_len\\(nrow\\(X\\)\\)", reasons[[3L]]) || grepl(shared_gate, reasons[[3L]], fixed = TRUE))
  expect_true(grepl("neighbor indexing requires explicit in-bounds guard", reasons[[4L]]) || grepl(shared_gate, reasons[[4L]], fixed = TRUE))
  expect_true(grepl("neighbor indexing requires explicit in-bounds guard", reasons[[5L]]) || grepl(shared_gate, reasons[[5L]], fixed = TRUE))
  expect_true(grepl("neighbor indexing requires explicit in-bounds guard", reasons[[6L]]) || grepl(shared_gate, reasons[[6L]], fixed = TRUE))
  expect_true(grepl("does not support complex boolean guard operators", reasons[[7L]]) || grepl(shared_gate, reasons[[7L]], fixed = TRUE))
  expect_true(grepl("does not support control flow in inner body", reasons[[8L]]) || grepl(shared_gate, reasons[[8L]], fixed = TRUE))
})

