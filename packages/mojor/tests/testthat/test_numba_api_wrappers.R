library(testthat)

.with_mock_binding <- function(name, value, expr) {
  env <- .GlobalEnv
  had_old <- exists(name, envir = env, inherits = FALSE)
  if (had_old) old <- get(name, envir = env, inherits = FALSE)
  assign(name, value, envir = env)
  on.exit({
    if (had_old) {
      assign(name, old, envir = env)
    } else if (exists(name, envir = env, inherits = FALSE)) {
      rm(list = name, envir = env)
    }
  }, add = TRUE)
  eval.parent(substitute(expr))
}

test_that("mojor_njit forwards strict object_mode off to mojor_jit", {  captured <- NULL
  f <- function(x) x

  .with_mock_binding("mojor_jit", function(...) {
    captured <<- list(...)
    function(...) NULL
  }, {
    out <- mojor_njit(
      f,
      name = "mock_njit",
      parallel = TRUE,
      broadcast = "recycle",
      semantics = "raw",
      fast_math = TRUE,
      bounds_check = FALSE
    )
    expect_true(is.function(out))
  })

  expect_identical(captured$fn, f)
  expect_identical(captured$name, "mock_njit")
  expect_true(isTRUE(captured$parallel))
  expect_identical(captured$broadcast, "recycle")
  expect_true(is.null(captured$disk_cache))
  expect_true(is.null(captured$signatures))
  expect_false(isTRUE(captured$eager))
  expect_identical(captured$semantics, "raw")
  expect_identical(captured$fast_math, TRUE)
  expect_identical(captured$bounds_check, FALSE)
  expect_identical(captured$object_mode, "off")
})

test_that("mojor_vectorize enforces elementwise target and strict mode", {  captured <- NULL
  f <- function(x) x

  .with_mock_binding("mojor_jit", function(...) {
    captured <<- list(...)
    function(...) NULL
  }, {
    out <- mojor_vectorize(
      f,
      name = "mock_vectorize",
      target = "gpu",
      parallel = TRUE,
      broadcast = "broadcast_nd_warn",
      elementwise_size = 128L
    )
    expect_true(is.function(out))
  })

  expect_identical(captured$fn, f)
  expect_identical(captured$name, "mock_vectorize")
  expect_true(isTRUE(captured$elementwise))
  expect_identical(captured$elementwise_target, "gpu")
  expect_identical(captured$elementwise_size, 128L)
  expect_true(isTRUE(captured$parallel))
  expect_identical(captured$broadcast, "broadcast_nd_warn")
  expect_true(is.null(captured$disk_cache))
  expect_true(is.null(captured$signatures))
  expect_false(isTRUE(captured$eager))
  expect_identical(captured$object_mode, "off")
})

test_that("mojor_njit and mojor_vectorize forward eager signature controls", {  captured_njit <- NULL
  captured_vec <- NULL
  f <- function(x) x
  sigs <- list(list(x = "f64[]"))

  .with_mock_binding("mojor_jit", function(...) {
    args <- list(...)
    if (identical(args$name, "mock_njit_eager")) {
      captured_njit <<- args
    } else {
      captured_vec <<- args
    }
    function(...) NULL
  }, {
    out1 <- mojor_njit(
      f,
      name = "mock_njit_eager",
      signatures = sigs,
      eager = TRUE
    )
    out2 <- mojor_vectorize(
      f,
      name = "mock_vectorize_eager",
      signatures = sigs,
      eager = TRUE
    )
    expect_true(is.function(out1))
    expect_true(is.function(out2))
  })

  expect_identical(captured_njit$signatures, sigs)
  expect_true(isTRUE(captured_njit$eager))
  expect_identical(captured_njit$object_mode, "off")
  expect_identical(captured_vec$signatures, sigs)
  expect_true(isTRUE(captured_vec$eager))
  expect_identical(captured_vec$object_mode, "off")
})

test_that("mojor_njit and mojor_vectorize forward strict_signatures", {  captured_njit <- NULL
  captured_vec <- NULL
  f <- function(x) x
  sigs <- list(list(x = "f64[]"))

  .with_mock_binding("mojor_jit", function(...) {
    args <- list(...)
    if (identical(args$name, "mock_njit_strict")) {
      captured_njit <<- args
    } else {
      captured_vec <<- args
    }
    function(...) NULL
  }, {
    out1 <- mojor_njit(
      f,
      name = "mock_njit_strict",
      signatures = sigs,
      strict_signatures = TRUE
    )
    out2 <- mojor_vectorize(
      f,
      name = "mock_vectorize_strict",
      signatures = sigs,
      strict_signatures = TRUE
    )
    expect_true(is.function(out1))
    expect_true(is.function(out2))
  })

  expect_identical(captured_njit$strict_signatures, TRUE)
  expect_identical(captured_njit$object_mode, "off")
  expect_identical(captured_vec$strict_signatures, TRUE)
  expect_identical(captured_vec$object_mode, "off")
})

test_that("mojor_guvectorize forwards signature and strict mode to build", {  captured <- NULL
  fake_build <- list(
    func = function(...) NULL,
    trans = list(types = list(x = "f64[]", n = "i32"))
  )

  .with_mock_binding("mojor_build", function(...) {
    captured <<- list(...)
    fake_build
  }, {
    out <- mojor_guvectorize(
      function(x, n) x,
      signature = list(x = "f64[]", n = "i32"),
      name = "mock_guvectorize",
      load = FALSE,
      cache = FALSE,
      parallel = TRUE,
      broadcast = "recycle_warn",
      output_shape = c(1L)
    )
    expect_true(is.list(out))
  })

  expect_identical(captured$name, "mock_guvectorize")
  expect_identical(captured$x, "f64[]")
  expect_identical(captured$n, "i32")
  expect_identical(captured$load, FALSE)
  expect_identical(captured$cache, FALSE)
  expect_true(isTRUE(captured$parallel))
  expect_identical(captured$broadcast, "recycle_warn")
  expect_false(isTRUE(captured$elementwise))
  expect_identical(captured$elementwise_target, "cpu")
  expect_identical(captured$object_mode, "off")
})

test_that("mojor_guvectorize target gpu forces gpu elementwise lane", {  captured <- NULL
  fake_build <- list(
    func = function(...) c(1, 2),
    trans = list(types = list(x = "f64[]", n = "i32"))
  )

  .with_mock_binding("mojor_build", function(...) {
    captured <<- list(...)
    fake_build
  }, {
    out <- mojor_guvectorize(
      function(x, n) x,
      signature = list(x = "f64[]", n = "i32"),
      name = "mock_guvectorize_gpu",
      target = "gpu",
      load = FALSE,
      cache = FALSE
    )
    expect_true(is.list(out))
  })

  expect_true(isTRUE(captured$elementwise))
  expect_identical(captured$elementwise_target, "gpu")
  expect_identical(captured$object_mode, "off")
})

test_that("mojor_guvectorize core_dims metadata and gpu forwarding", {  captured <- NULL
  fake_build <- list(
    func = function(x, y) x + y,
    trans = list(types = list(x = "f64[]", y = "f64[]"))
  )

  .with_mock_binding("mojor_build", function(...) {
    captured <<- list(...)
    fake_build
  }, {
    out <- mojor_guvectorize(
      function(x, y) x + y,
      signature = list(x = "f64[]", y = "f64[]"),
      core_dims = "(n),(n)->(n)",
      target = "gpu",
      load = FALSE,
      cache = FALSE
    )
    expect_true(is.list(out))
    expect_identical(attr(out, "mojor_core_dims"), "(n),(n)->(n)")
    parsed <- attr(out, "mojor_core_dims_parsed")
    expect_identical(parsed$inputs, list(c("n"), c("n")))
    expect_identical(parsed$outputs, list(c("n")))
    expect_identical(parsed$output, c("n"))
    expect_identical(attr(out, "mojor_core_dims_engine"), "direct_core")
    expect_null(attr(out, "mojor_core_dims_strict_error"))
    expect_identical(attr(out, "mojor_output_arity"), 1L)
    expect_identical(attr(out, "mojor_output_names"), "output1")
  })

  expect_true(isTRUE(captured$elementwise))
  expect_identical(captured$elementwise_target, "gpu")
  expect_identical(captured$object_mode, "off")
})

test_that("mojor_guvectorize core_dims multi-output supports n-input metadata and gpu forwarding", {  captured <- list()
  fake_build <- list(
    func = function(...) 0,
    trans = list(types = list(x = "f64[]", y = "f64[]", z = "f64[]"))
  )

  .with_mock_binding("mojor_build", function(...) {
    captured[[length(captured) + 1L]] <<- list(...)
    fake_build
  }, {
    out <- mojor_guvectorize(
      function(x, y, z) list(sum = x + y, diff = x - z),
      signature = list(x = "f64[]", y = "f64[]", z = "f64[]"),
      core_dims = "(n),(n),(n)->(n),(n)",
      target = "gpu",
      load = TRUE,
      cache = FALSE
    )
    expect_true(is.function(out))
    expect_length(captured, 2L)
    for (i in seq_along(captured)) {
      expect_true(isTRUE(captured[[i]]$elementwise))
      expect_identical(captured[[i]]$elementwise_target, "gpu")
      expect_identical(captured[[i]]$object_mode, "off")
    }
    parsed <- attr(out, "mojor_core_dims_parsed")
    expect_identical(parsed$inputs, list(c("n"), c("n"), c("n")))
    expect_identical(parsed$outputs, list(c("n"), c("n")))
    expect_null(parsed$output)
    expect_identical(attr(out, "mojor_core_dims_engine"), "direct_core")
    expect_identical(attr(out, "mojor_output_arity"), 2L)
    expect_identical(attr(out, "mojor_output_names"), c("sum", "diff"))
  })
})

test_that("mojor_guvectorize core_dims unary supports no-batch and batched calls", {  fake_build <- list(
  func = function(x) x + 1,
  trans = list(types = list(x = "f64[]"))
)

  .with_mock_binding("mojor_build", function(...) fake_build, {
    out <- mojor_guvectorize(
      function(x) x,
      signature = list(x = "f64[]"),
      core_dims = "(n)->(n)",
      load = TRUE,
      cache = FALSE
    )
    expect_equal(out(c(1, 2, 3)), c(2, 3, 4))

    x_batch <- array(as.double(1:6), dim = c(2, 3))
    expect_equal(out(x_batch), x_batch + 1)
  })
})

test_that("mojor_guvectorize core_dims rank-n supports no-batch and batched calls", {
  fake_build <- list(
    func = function(x) x + 1,
    trans = list(types = list(x = "f64[3d]"))
  )

  .with_mock_binding("mojor_build", function(...) fake_build, {
    out <- mojor_guvectorize(
      function(x) x,
      signature = list(x = "f64[3d]"),
      core_dims = "(a,b,c)->(a,b,c)",
      load = TRUE,
      cache = FALSE
    )
    expect_true(isTRUE(attr(out, "mojor_core_dims_rank_n_mode")))
    expect_false(isTRUE(attr(out, "mojor_core_dims_rank_n_fallback_used")))
    expect_identical(attr(out, "mojor_core_dims_engine"), "strict_elementwise")

    x <- array(as.double(seq_len(24)), dim = c(2, 3, 4))
    expect_equal(out(x), x + 1)

    x_batch <- array(as.double(seq_len(120)), dim = c(5, 2, 3, 4))
    expect_equal(out(x_batch), x_batch + 1)
  })
})

test_that("mojor_guvectorize core_dims rank-n accepts dual signature syntax", {
  fake_build <- list(
    func = function(x, y) x + y,
    trans = list(types = list(x = "f64[,,]", y = "f64[3d]"))
  )

  .with_mock_binding("mojor_build", function(...) fake_build, {
    out <- mojor_guvectorize(
      function(x, y) x + y,
      signature = "(x: f64[,,], y: f64[3d])",
      core_dims = "(a,b,c),(a,b,c)->(a,b,c)",
      load = TRUE,
      cache = FALSE
    )
    expect_identical(attr(out, "mojor_signature"), list(x = "f64[3d]", y = "f64[3d]"))
    x <- array(as.double(seq_len(24)), dim = c(2, 3, 4))
    y <- array(as.double(seq_len(24)), dim = c(2, 3, 4))
    expect_equal(out(x, y), x + y)
  })
})

test_that("mojor_guvectorize core_dims rank-n canonicalizes signature specs before direct-core build", {
  captured <- list()
  fake_build <- list(
    func = function(x) x,
    trans = list(types = list(x = "f64[3d]"))
  )

  .with_mock_binding("mojor_build", function(...) {
    captured[[length(captured) + 1L]] <<- list(...)
    fake_build
  }, {
    out <- mojor_guvectorize(
      function(x) {
        if (TRUE) x else x
      },
      signature = "(x: f64[,,])",
      core_dims = "(a,b,c)->(a,b,c)",
      target = "cpu",
      load = FALSE,
      cache = FALSE
    )
    expect_identical(attr(out, "mojor_signature"), list(x = "f64[3d]"))
    expect_identical(attr(out, "mojor_core_dims_engine"), "strict_elementwise")
  })

  expect_length(captured, 1L)
  expect_identical(captured[[1L]]$x, "f64[]")
})

test_that("mojor_guvectorize core_dims binary supports batch broadcasting", {  fake_build <- list(
  func = function(x, y) x + y,
  trans = list(types = list(x = "f64[]", y = "f64[]"))
)

  .with_mock_binding("mojor_build", function(...) fake_build, {
    out <- mojor_guvectorize(
      function(x, y) x + y,
      signature = list(x = "f64[]", y = "f64[]"),
      core_dims = "(n),(n)->(n)",
      load = TRUE,
      cache = FALSE
    )

    x <- array(as.double(1:6), dim = c(2, 3))
    y <- as.double(c(10, 20, 30))
    expected <- array(0, dim = c(2, 3))
    for (i in seq_len(2)) expected[i, ] <- x[i, ] + y
    expect_equal(out(x, y), expected)
  })
})

test_that("mojor_guvectorize core_dims matrix form supports batched matmul", {  fake_build <- list(
  func = function(A, B) A %*% B,
  trans = list(types = list(A = "f64[,]", B = "f64[,]"))
)

  .with_mock_binding("mojor_build", function(...) fake_build, {
    out <- mojor_guvectorize(
      function(A, B) A %*% B,
      signature = list(A = "f64[,]", B = "f64[,]"),
      core_dims = "(m,n),(n,p)->(m,p)",
      load = TRUE,
      cache = FALSE
    )

    A <- array(as.double(1:12), dim = c(2, 2, 3))
    B <- matrix(as.double(1:12), nrow = 3, ncol = 4)
    expected <- array(0, dim = c(2, 2, 4))
    for (b in seq_len(2)) expected[b, , ] <- A[b, , ] %*% B
    expect_equal(out(A, B), expected)
  })
})

test_that("mojor_guvectorize core_dims multi-output supports no-batch and batched calls", {
  .with_mock_binding("mojor_build", function(...) {
    dots <- list(...)
    list(
      func = function(...) do.call(dots$fn, list(...)),
      trans = list(types = dots[names(dots) %in% names(formals(dots$fn))])
    )
  }, {
    out <- mojor_guvectorize(
      function(x, y) list(sum = x + y, prod = x * y),
      signature = list(x = "f64[]", y = "f64[]"),
      core_dims = "(n),(n)->(n),(n)",
      load = TRUE,
      cache = FALSE
    )
    res <- out(c(1, 2, 3), c(10, 20, 30))
    expect_type(res, "list")
    expect_identical(names(res), c("sum", "prod"))
    expect_equal(res$sum, c(11, 22, 33))
    expect_equal(res$prod, c(10, 40, 90))

    x <- array(as.double(1:6), dim = c(2, 3))
    y <- as.double(c(10, 20, 30))
    expected_sum <- array(0, dim = c(2, 3))
    expected_prod <- array(0, dim = c(2, 3))
    for (i in seq_len(2)) {
      expected_sum[i, ] <- x[i, ] + y
      expected_prod[i, ] <- x[i, ] * y
    }
    batched <- out(x, y)
    expect_equal(batched$sum, expected_sum)
    expect_equal(batched$prod, expected_prod)
  })
})

test_that("mojor_guvectorize core_dims rank-n strict elementwise supports multi-output and gpu lane metadata", {
  captured <- list()
  .with_mock_binding("mojor_build", function(...) {
    dots <- list(...)
    captured[[length(captured) + 1L]] <<- dots
    list(
      func = dots$fn,
      trans = list(types = dots[names(dots) %in% names(formals(dots$fn))])
    )
  }, {
    out <- mojor_guvectorize(
      function(x, y) list(sum = x + y, diff = x - y),
      signature = list(x = "f64[3d]", y = "f64[3d]"),
      core_dims = "(a,b,c),(a,b,c)->(a,b,c),(a,b,c)",
      target = "gpu",
      load = TRUE,
      cache = FALSE
    )
    expect_identical(attr(out, "mojor_core_dims_engine"), "strict_elementwise")

    x <- array(as.double(seq_len(120)), dim = c(5, 2, 3, 4))
    y <- array(as.double(seq_len(24)), dim = c(2, 3, 4))
    res <- out(x, y)
    expected_sum <- array(0, dim = dim(x))
    expected_diff <- array(0, dim = dim(x))
    for (b in seq_len(dim(x)[1])) {
      expected_sum[b, , , ] <- x[b, , , ] + y
      expected_diff[b, , , ] <- x[b, , , ] - y
    }
    expect_equal(res$sum, expected_sum)
    expect_equal(res$diff, expected_diff)
  })

  expect_length(captured, 2L)
  for (k in seq_along(captured)) {
    expect_true(isTRUE(captured[[k]]$elementwise))
    expect_identical(captured[[k]]$elementwise_target, "gpu")
    expect_identical(captured[[k]]$object_mode, "off")
    expect_identical(captured[[k]]$x, "f64[]")
    expect_identical(captured[[k]]$y, "f64[]")
  }
})

test_that("mojor_guvectorize core_dims rank-n strict indexed reads support canonical index vars", {
  .with_mock_binding("mojor_build", function(...) {
    dots <- list(...)
    list(
      func = dots$fn,
      trans = list(types = dots[names(dots) %in% names(formals(dots$fn))])
    )
  }, {
    out <- mojor_guvectorize(
      function(x) x[i1, i2, i3] + 1,
      signature = list(x = "f64[3d]"),
      core_dims = "(a,b,c)->(a,b,c)",
      target = "gpu",
      load = TRUE,
      cache = FALSE
    )
    expect_identical(attr(out, "mojor_core_dims_engine"), "strict_elementwise")

    x <- array(as.double(seq_len(24)), dim = c(2, 3, 4))
    expect_equal(out(x), x + 1)

    x_batch <- array(as.double(seq_len(120)), dim = c(5, 2, 3, 4))
    expect_equal(out(x_batch), x_batch + 1)
  })
})

test_that("mojor_guvectorize core_dims rank-n strict indexed lane enforces neighbor guard grammar", {
  .with_mock_binding("mojor_build", function(...) {
    dots <- list(...)
    list(
      func = dots$fn,
      trans = list(types = dots[names(dots) %in% names(formals(dots$fn))])
    )
  }, {
    guarded <- mojor_guvectorize(
      function(x) if (i1 >= 2L && i1 <= n1 && i2 >= 1L && i2 <= n2 && i3 >= 1L && i3 <= n3) x[i1 - 1L, i2, i3] else x[i1, i2, i3],
      signature = list(x = "f64[3d]"),
      core_dims = "(a,b,c)->(a,b,c)",
      target = "cpu",
      load = TRUE,
      cache = FALSE
    )

    x <- array(as.double(seq_len(24)), dim = c(2, 3, 4))
    expected <- x
    expected[2, , ] <- x[1, , ]
    expect_equal(guarded(x), expected)
  })

  expect_error(
    mojor_guvectorize(
      function(x) x[i1 + 1L, i2, i3],
      signature = list(x = "f64[3d]"),
      core_dims = "(a,b,c)->(a,b,c)",
      target = "gpu",
      load = TRUE,
      cache = FALSE
    ),
    "core_dims rank-n neighbor indexing requires explicit in-bounds guard"
  )

  expect_error(
    mojor_guvectorize(
      function(x, k) if (i1 >= 1L && i1 <= n1 && i2 >= 1L && i2 <= n2 && i3 >= 1L && i3 <= n3) x[i1 + k, i2, i3] else x[i1, i2, i3],
      signature = list(x = "f64[3d]", k = "i32"),
      core_dims = "(a,b,c),()->(a,b,c)",
      target = "gpu",
      load = TRUE,
      cache = FALSE
    ),
    "core_dims rank-n does not support non-literal neighbor offsets"
  )

  expect_error(
    mojor_guvectorize(
      function(x) if (i1 >= 2L || i1 <= n1) x[i1 + 1L, i2, i3] else x[i1, i2, i3],
      signature = list(x = "f64[3d]"),
      core_dims = "(a,b,c)->(a,b,c)",
      target = "gpu",
      load = TRUE,
      cache = FALSE
    ),
    "core_dims rank-n does not support complex boolean guard operators"
  )
})

test_that("mojor_guvectorize core_dims rank-n strict lane uses compiled_batch metadata when eligible", {
  .with_mock_binding("mojor_build", function(...) {
    dots <- list(...)
    list(
      func = dots$fn,
      trans = list(types = dots[names(dots) %in% names(formals(dots$fn))])
    )
  }, {
    out <- mojor_guvectorize(
      function(x, y) x + y,
      signature = list(x = "f64[3d]", y = "f64[3d]"),
      core_dims = "(a,b,c),(a,b,c)->(a,b,c)",
      target = "gpu",
      load = TRUE,
      cache = FALSE
    )
    expect_identical(attr(out, "mojor_core_dims_batch_engine"), "compiled_batch")
    expect_false(isTRUE(attr(out, "mojor_core_dims_batch_cache_hit")))
    x <- array(as.double(seq_len(48)), dim = c(2, 2, 3, 4))
    y <- array(as.double(seq_len(48)), dim = c(2, 2, 3, 4))
    v1 <- out(x, y)
    expect_equal(v1, x + y)
    v2 <- out(x, y)
    expect_equal(v2, x + y)
    expect_equal(v2, x + y)
  })
})

test_that("mojor_guvectorize core_dims rank-n reduction subset reports stable diagnostics on gpu", {
  expect_error(
    mojor_guvectorize(
      function(x) prod(x),
      signature = list(x = "f64[3d]"),
      core_dims = "(a,b,c)->()",
      target = "gpu",
      load = TRUE,
      cache = FALSE
    ),
    "core_dims rank-n reduction op is not supported in this release"
  )

  expect_error(
    mojor_guvectorize(
      function(x) sum(sin(x)),
      signature = list(x = "f64[3d]"),
      core_dims = "(a,b,c)->()",
      target = "gpu",
      load = TRUE,
      cache = FALSE
    ),
    "core_dims rank-n reduction expression is not strict-compilable"
  )

  expect_error(
    mojor_guvectorize(
      function(x) x,
      signature = list(x = "f64[3d]"),
      core_dims = "(a,b,c)->(a,b)",
      target = "gpu",
      load = TRUE,
      cache = FALSE
    ),
    "core_dims rank-n reduction/output tuple contract is not satisfiable"
  )
})

test_that("mojor_guvectorize core_dims rank-n reductions run with deterministic axis semantics", {
  .with_mock_binding("mojor_build", function(...) {
    dots <- list(...)
    list(
      func = dots$fn,
      trans = list(types = dots[names(dots) %in% names(formals(dots$fn))])
    )
  }, {
    red_m <- mojor_guvectorize(
      function(x) sum(x),
      signature = list(x = "f64[3d]"),
      core_dims = "(a,b,c)->(a,b)",
      target = "gpu",
      load = TRUE,
      cache = FALSE
    )
    red_n <- mojor_guvectorize(
      function(x) sum(x),
      signature = list(x = "f64[3d]"),
      core_dims = "(a,b,c)->(b,c)",
      target = "gpu",
      load = TRUE,
      cache = FALSE
    )
    red_s <- mojor_guvectorize(
      function(x) sum(x),
      signature = list(x = "f64[3d]"),
      core_dims = "(a,b,c)->()",
      target = "gpu",
      load = TRUE,
      cache = FALSE
    )

    x <- array(as.double(seq_len(24)), dim = c(2, 3, 4))
    expect_equal(red_m(x), apply(x, c(1, 2), sum))
    expect_equal(red_n(x), apply(x, c(2, 3), sum))
    expect_equal(as.double(red_s(x)), sum(x))
  })
})

test_that("mojor_guvectorize core_dims rank-n supports mixed multi-output reduced and non-reduced tuples", {
  .with_mock_binding("mojor_build", function(...) {
    dots <- list(...)
    list(
      func = dots$fn,
      trans = list(types = dots[names(dots) %in% names(formals(dots$fn))])
    )
  }, {
    out <- mojor_guvectorize(
      function(x) list(raw = x, total = sum(x)),
      signature = list(x = "f64[3d]"),
      core_dims = "(a,b,c)->(a,b,c),()",
      target = "gpu",
      load = TRUE,
      cache = FALSE
    )

    x <- array(as.double(seq_len(24)), dim = c(2, 3, 4))
    res <- out(x)
    expect_equal(res$raw, x)
    expect_equal(as.double(res$total), sum(x))
  })
})

test_that("mojor_guvectorize core_dims matrix form supports batched multi-output", {
  .with_mock_binding("mojor_build", function(...) {
    dots <- list(...)
    list(
      func = function(...) do.call(dots$fn, list(...)),
      trans = list(types = dots[names(dots) %in% names(formals(dots$fn))])
    )
  }, {
    out <- mojor_guvectorize(
      function(A, B) list(sum = A + B, diff = A - B),
      signature = list(A = "f64[,]", B = "f64[,]"),
      core_dims = "(m,n),(m,n)->(m,n),(m,n)",
      load = TRUE,
      cache = FALSE
    )

    A <- array(as.double(1:12), dim = c(2, 2, 3))
    B <- matrix(as.double(seq_len(6)), nrow = 2, ncol = 3)
    expected_sum <- array(0, dim = c(2, 2, 3))
    expected_diff <- array(0, dim = c(2, 2, 3))
    for (b in seq_len(2)) {
      expected_sum[b, , ] <- A[b, , ] + B
      expected_diff[b, , ] <- A[b, , ] - B
    }
    res <- out(A, B)
    expect_equal(res$sum, expected_sum)
    expect_equal(res$diff, expected_diff)
  })
})

test_that("mojor_guvectorize core_dims validates grammar and shape contracts", {  f <- function(x) x

  expect_error(
    mojor_guvectorize(f, signature = list(x = "f64[]"), core_dims = "n->n", load = FALSE, cache = FALSE),
    "core_dims must be a non-empty string like"
  )
  expect_error(
    mojor_guvectorize(f, signature = list(x = "f64[]"), core_dims = "(n)->", load = FALSE, cache = FALSE),
    "core_dims must declare at least one input tuple and at least one output tuple"
  )
  expect_error(
    mojor_guvectorize(f, signature = list(x = "f64[]"), core_dims = "(m,n)->(m,n)", load = FALSE, cache = FALSE),
    "core_dims/input rank mismatch for 'x'"
  )
  expect_error(
    mojor_guvectorize(f, signature = list(x = "f64[2x]"), core_dims = "(n)->(n)", load = FALSE, cache = FALSE),
    "invalid rank syntax in signature spec for 'x'"
  )
  expect_error(
    mojor_guvectorize(f, signature = list(x = "f64[]"), core_dims = "(n)->(m)", load = FALSE, cache = FALSE),
    "output core dimension 'm' is not bound by inputs"
  )
  expect_error(
    mojor_guvectorize(
      function(x, y) list(x + y, x - y),
      signature = list(x = "f64[]", y = "f64[]"),
      core_dims = "(n),(n)->(n),(n)",
      output_shape = c(3L),
      load = TRUE,
      cache = FALSE
    ),
    "output_shape for multi-output must be a list of length 2"
  )
  expect_error(
    mojor_guvectorize(
      function(x, y) list(x + y, x - y),
      signature = list(x = "f64[]", y = "f64[]"),
      core_dims = "(n),(n)->(n),(n)",
      load = FALSE,
      cache = FALSE
    ),
    "core_dims multi-output requires load=TRUE in this release"
  )
  expect_error(
    mojor_guvectorize(
      function(x, y) x + y,
      signature = list(x = "f64[]", y = "f64[]"),
      core_dims = "(n),(n)->(n),(n)",
      load = TRUE,
      cache = FALSE
    ),
    "core_dims multi-output requires terminal list\\(\\.\\.\\.\\) return"
  )
  expect_error(
    mojor_guvectorize(
      function(x, y) list(x + y),
      signature = list(x = "f64[]", y = "f64[]"),
      core_dims = "(n),(n)->(n),(n)",
      load = TRUE,
      cache = FALSE
    ),
    "core_dims multi-output return length mismatch \\(expected 2, got 1\\)"
  )
})

test_that("mojor_guvectorize core_dims accepts literal and underscore dimension tokens", {
  .with_mock_binding("mojor_build", function(...) {
    dots <- list(...)
    list(
      func = dots$fn,
      trans = list(types = dots[names(dots) %in% names(formals(dots$fn))])
    )
  }, {
    out <- mojor_guvectorize(
      function(x) x,
      signature = list(x = "f64[,]"),
      core_dims = "(_m,3)->(_m,3)",
      load = FALSE,
      cache = FALSE
    )
    parsed <- attr(out, "mojor_core_dims_parsed")
    expect_identical(parsed$inputs, list(c("_m", "3")))
    expect_identical(parsed$outputs, list(c("_m", "3")))
  })
})

test_that("mojor_guvectorize core_dims enforces literal dimension sizes at runtime", {
  .with_mock_binding("mojor_build", function(...) {
    dots <- list(...)
    list(
      func = dots$fn,
      trans = list(types = dots[names(dots) %in% names(formals(dots$fn))])
    )
  }, {
    g <- mojor_guvectorize(
      function(x) x,
      signature = list(x = "f64[,]"),
      core_dims = "(n,3)->(n,3)",
      load = TRUE,
      cache = FALSE
    )

    x_ok <- matrix(as.double(seq_len(6)), nrow = 2, ncol = 3)
    expect_equal(g(x_ok), x_ok)

    x_bad <- matrix(as.double(seq_len(8)), nrow = 2, ncol = 4)
    expect_error(
      g(x_bad),
      "core dimension '3' mismatch \\(expected 3, got 4\\)"
    )
  })
})

test_that("mojor_guvectorize core_dims rank-n cpu strict compile failure is explicit and never retries object mode", {
  old_ir_only <- .mojor_state$options$ir_only
  .mojor_state$options$ir_only <- FALSE
  on.exit(.mojor_state$options$ir_only <- old_ir_only, add = TRUE)
  calls <- list()
  .with_mock_binding("mojor_build", function(...) {
    dots <- list(...)
    calls[[length(calls) + 1L]] <<- dots
    stop("forced strict failure")
  }, {
    expect_error(
      mojor_guvectorize(
        function(x) x,
        signature = list(x = "f64[3d]"),
        core_dims = "(a,b,c)->(a,b,c)",
        target = "cpu",
        load = TRUE,
        cache = FALSE
      ),
      "cpu target core kernel failed strict compilation for core_dims rank-n: forced strict failure"
    )
  })
  expect_false(any(vapply(calls, function(one) identical(one$object_mode, "fallback"), logical(1))))
})

test_that("mojor_guvectorize core_dims rank-n cpu strict compile failure is unchanged when ir_only=TRUE", {
  old_ir_only <- .mojor_state$options$ir_only
  .mojor_state$options$ir_only <- TRUE
  on.exit(.mojor_state$options$ir_only <- old_ir_only, add = TRUE)
  calls <- list()
  .with_mock_binding("mojor_build", function(...) {
    dots <- list(...)
    calls[[length(calls) + 1L]] <<- dots
    stop("forced strict failure")
  }, {
    expect_error(
      mojor_guvectorize(
        function(x) x + 1,
        signature = list(x = "f64[3d]"),
        core_dims = "(a,b,c)->(a,b,c)",
        target = "cpu",
        load = TRUE,
        cache = FALSE
      ),
      "cpu target core kernel failed strict compilation for core_dims rank-n: forced strict failure"
    )
  })
  expect_false(any(vapply(calls, function(one) identical(one$object_mode, "fallback"), logical(1))))
})

test_that("mojor_guvectorize core_dims rank-n gpu strict compile failure stays explicit", {
  .with_mock_binding("mojor_build", function(...) {
    stop("forced strict failure")
  }, {
    expect_error(
      mojor_guvectorize(
        function(x) x,
        signature = list(x = "f64[3d]"),
        core_dims = "(a,b,c)->(a,b,c)",
        target = "gpu",
        load = TRUE,
        cache = FALSE
      ),
      "gpu target core kernel failed strict compilation for core_dims rank-n"
    )
  })
})

test_that("mojor_guvectorize core_dims rank-n gpu rejects unsupported strict elementwise body", {
  expect_error(
    mojor_guvectorize(
      function(x) sin(x),
      signature = list(x = "f64[3d]"),
      core_dims = "(a,b,c)->(a,b,c)",
      target = "gpu",
      load = TRUE,
      cache = FALSE
    ),
    "core_dims rank-n strict indexed lane does not support this function body"
  )
})

test_that("mojor_guvectorize core_dims catches runtime dim mismatch and non-broadcastable batches", {  fake_build <- list(
  func = function(x, y) x + y,
  trans = list(types = list(x = "f64[]", y = "f64[]"))
)

  .with_mock_binding("mojor_build", function(...) fake_build, {
    out <- mojor_guvectorize(
      function(x, y) x + y,
      signature = list(x = "f64[]", y = "f64[]"),
      core_dims = "(n),(n)->(n)",
      load = TRUE,
      cache = FALSE
    )
    expect_error(out(c(1, 2, 3), c(1, 2, 3, 4)), "core dimension 'n' mismatch")

    x <- array(as.double(1:6), dim = c(2, 3))
    y <- array(as.double(1:9), dim = c(3, 3))
    expect_error(out(x, y), "batch dimensions are not broadcast-compatible")
  })
})

test_that("mojor_guvectorize core_dims still enforces output_shape on assembled output", {  fake_build <- list(
  func = function(x) x,
  trans = list(types = list(x = "f64[]"))
)

  .with_mock_binding("mojor_build", function(...) fake_build, {
    out <- mojor_guvectorize(
      function(x) x,
      signature = list(x = "f64[]"),
      core_dims = "(n)->(n)",
      output_shape = c(2L, 3L),
      load = TRUE,
      cache = FALSE
    )
    expect_equal(out(array(as.double(1:6), dim = c(2, 3))), array(as.double(1:6), dim = c(2, 3)))

    bad <- mojor_guvectorize(
      function(x) x,
      signature = list(x = "f64[]"),
      core_dims = "(n)->(n)",
      output_shape = c(3L, 3L),
      load = TRUE,
      cache = FALSE
    )
    expect_error(bad(array(as.double(1:6), dim = c(2, 3))), "output_shape mismatch")
  })
})

test_that("mojor_guvectorize core_dims multi-output enforces per-output contracts", {
  .with_mock_binding("mojor_build", function(...) {
    dots <- list(...)
    list(
      func = function(...) do.call(dots$fn, list(...)),
      trans = list(types = dots[names(dots) %in% names(formals(dots$fn))])
    )
  }, {
    out <- mojor_guvectorize(
      function(x, y) list(sum = x + y, diff = x - y),
      signature = list(x = "f64[]", y = "f64[]"),
      core_dims = "(n),(n)->(n),(n)",
      output_shape = list(c(2L, 3L), c(2L, 3L)),
      load = TRUE,
      cache = FALSE
    )
    x <- array(as.double(1:6), dim = c(2, 3))
    y <- array(as.double(6:1), dim = c(2, 3))
    res <- out(x, y)
    expect_equal(res$sum, x + y)
    expect_equal(res$diff, x - y)

    bad_shape <- mojor_guvectorize(
      function(x, y) list(sum = x + y, diff = x - y),
      signature = list(x = "f64[]", y = "f64[]"),
      core_dims = "(n),(n)->(n),(n)",
      output_shape = list(c(2L, 3L), c(3L, 3L)),
      load = TRUE,
      cache = FALSE
    )
    expect_error(bad_shape(x, y), "output_shape mismatch")

    bad_core <- mojor_guvectorize(
      function(x, y) list(sum = x + y, diff = 1),
      signature = list(x = "f64[]", y = "f64[]"),
      core_dims = "(n),(n)->(n),(n)",
      load = TRUE,
      cache = FALSE
    )
    expect_error(bad_core(x, y), "output #2 core shape mismatch")
  })
})

test_that("mojor_guvectorize core_dims multi-output gpu compilation stays strict", {
  .with_mock_binding("mojor_build", function(...) {
    dots <- list(...)
    if (identical(dots$name, "mock_multi_gpu_out2")) {
      stop("forced compile failure")
    }
    list(
      func = function(...) do.call(dots$fn, list(...)),
      trans = list(types = dots[names(dots) %in% names(formals(dots$fn))])
    )
  }, {
    expect_error(
      mojor_guvectorize(
        function(x, y) list(sum = x + y, diff = x - y),
        signature = list(x = "f64[]", y = "f64[]"),
        core_dims = "(n),(n)->(n),(n)",
        target = "gpu",
        name = "mock_multi_gpu",
        load = TRUE,
        cache = FALSE
      ),
      "gpu target core kernel #2 failed strict compilation for core_dims multi-output"
    )
  })
})

test_that("mojor_prange lowers to parallel loop routing", {  f <- function(x) {
    out <- numeric(length(x))
    for (i in mojor_prange(length(x))) {
      out[i] <- x[i] * 2
    }
    out
  }

  trans <- mojor_transpile(f, x = "f64[]", name = "t_mojor_prange")
  expect_true(grepl("parallelize", trans$mojo, fixed = TRUE))
})

test_that("mojor_njit enforces strict object_mode off", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] + 1
    out
  }

  jit <- mojor_njit(f, name = "t_mojor_njit")
  x <- c(1, 2, 3)
  expect_equal(jit(x), f(x), tolerance = 1e-12)
})

test_that("mojor_vectorize routes through elementwise JIT path", {  skip_if_no_mojo()

  f <- function(x) {
    out <- numeric(length(x))
    for (i in seq_along(x)) out[i] <- x[i] * 3 + 1
    out
  }

  vec <- mojor_vectorize(f, name = "t_mojor_vectorize", target = "cpu")
  x <- as.double(1:8)
  expect_equal(vec(x), f(x), tolerance = 1e-12)
})

test_that("mojor_guvectorize accepts explicit signature and output shape metadata", {  skip_if_no_mojo()

  f <- function(x, n) {
    out <- numeric(n)
    for (i in seq_len(n)) out[i] <- x[i] + 2
    out
  }

  built <- mojor_guvectorize(
    f,
    signature = list(x = "f64[]", n = "i32"),
    name = "t_mojor_guvectorize",
    output_shape = c(1L),
    load = FALSE,
    cache = FALSE
  )

  expect_true(is.list(built))
  expect_identical(attr(built, "mojor_signature"), list(x = "f64[]", n = "i32"))
  expect_identical(attr(built, "mojor_output_shape"), c(1L))
  expect_identical(attr(built, "mojor_target"), "cpu")
  expect_identical(built$trans$types$x, "f64[]")
  expect_identical(built$trans$types$n, "i32")
})

test_that("mojor_guvectorize enforces output_shape on loaded function", {  fake_build <- list(
  func = function(...) c(1, 2, 3),
  trans = list(types = list(x = "f64[]"))
)

  .with_mock_binding("mojor_build", function(...) fake_build, {
    out <- mojor_guvectorize(
      function(x) x,
      signature = list(x = "f64[]"),
      output_shape = c(2L),
      load = TRUE,
      cache = FALSE
    )
    expect_error(out(c(1, 2, 3)), "output_shape mismatch")
  })
})

test_that("mojor_guvectorize validates output_shape", {  f <- function(x) x
  expect_error(
    mojor_guvectorize(f, signature = list(x = "f64[]"), output_shape = c(0L), load = FALSE),
    "output_shape must contain positive integers"
  )
})
