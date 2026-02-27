# Test covariance and correlation transpilation (PR-B7.2)

test_that("cov(x, y) operation is detected", {  f <- function(x, y) cov(x, y)
  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]")

  expect_true(trans$is_expression_kernel)
  expect_false(trans$is_matrix_output)
  expect_equal(trans$operation, "cov")
})

test_that("cor(x, y) operation is detected", {  f <- function(x, y) cor(x, y)
  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]")

  expect_true(trans$is_expression_kernel)
  expect_false(trans$is_matrix_output)
  expect_equal(trans$operation, "cor")
})

test_that("cov(x, y) generates valid Mojo code", {  f <- function(x, y) cov(x, y)
  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]")

  # Check for means computation
  expect_true(grepl("Compute means", trans$mojo))
  expect_true(grepl("sum_x", trans$mojo))
  expect_true(grepl("sum_y", trans$mojo))
  expect_true(grepl("mean_x", trans$mojo))
  expect_true(grepl("mean_y", trans$mojo))

  # Check for covariance computation
  expect_true(grepl("Compute covariance", trans$mojo))
  expect_true(grepl("sum_prod", trans$mojo))
  expect_true(grepl("dev_x", trans$mojo))
  expect_true(grepl("dev_y", trans$mojo))
})

test_that("cor(x, y) generates valid Mojo code", {  f <- function(x, y) cor(x, y)
  trans <- mojor_transpile(f, x = "f64[]", y = "f64[]")

  # Check for mean computation
  expect_true(grepl("Compute means", trans$mojo))

  # Check for covariance and variance computation
  expect_true(grepl("Compute covariance and variances", trans$mojo))
  expect_true(grepl("sum_sq_x", trans$mojo))
  expect_true(grepl("sum_sq_y", trans$mojo))

  # Check for standard deviation computation
  expect_true(grepl("sqrt", trans$mojo))
  expect_true(grepl("sd_x", trans$mojo))
  expect_true(grepl("sd_y", trans$mojo))

  # Check for correlation formula
  expect_true(grepl("cov_xy / \\(sd_x \\* sd_y\\)", trans$mojo))
})

test_that("cov/cor reject single vector argument (R behavior)", {  f_cov <- function(x) cov(x)
  f_cor <- function(x) cor(x)

  expect_error(
    mojor_transpile(f_cov, x = "f64[]"),
    "requires a matrix.*Use var\\(x\\) for variance"
  )

  expect_error(
    mojor_transpile(f_cor, x = "f64[]"),
    "requires a matrix.*Use var\\(x\\) for variance"
  )
})

test_that("cov(X) matrix lane is detected", {  f <- function(X) cov(X)
  trans <- mojor_transpile(f, X = "f64[,]")

  expect_true(trans$is_expression_kernel)
  expect_true(trans$is_matrix_output)
  expect_equal(trans$out_type, "f64[,]")
  expect_equal(trans$return_type, "Matrix")
  expect_equal(trans$operation, "cov")
  expect_equal(trans$kernel_args, "X")
})

test_that("cor(X, Y) matrix lane is detected", {  f <- function(X, Y) cor(X, Y)
  trans <- mojor_transpile(f, X = "f64[,]", Y = "f64[,]")

  expect_true(trans$is_expression_kernel)
  expect_true(trans$is_matrix_output)
  expect_equal(trans$out_type, "f64[,]")
  expect_equal(trans$return_type, "Matrix")
  expect_equal(trans$operation, "cor")
  expect_equal(trans$kernel_args, c("X", "Y"))
})

test_that("cov/cor matrix lanes generate matrix kernel code", {  trans_cov <- mojor_transpile(
    function(X) cov(X),
    X = "f64[,]"
  )
  trans_cor <- mojor_transpile(
    function(X, Y) cor(X, Y),
    X = "f64[,]",
    Y = "f64[,]"
  )

  expect_true(grepl("covariance/correlation matrix lane", trans_cov$mojo))
  expect_true(grepl("out_ptr: MutOpaqueAny", trans_cov$mojo))
  expect_true(grepl("for i in range\\(p\\)", trans_cov$mojo))
  expect_true(grepl("for j in range\\(q\\)", trans_cov$mojo))
  expect_true(grepl("out\\[i \\+ j \\* p\\]", trans_cov$mojo))

  expect_true(grepl("q = Int\\(Y_dim\\[1\\]\\)", trans_cor$mojo))
  expect_true(grepl("sum_sq_x", trans_cor$mojo))
  expect_true(grepl("sum_sq_y", trans_cor$mojo))
})

test_that("cov/cor reject mixed vector/matrix ranks", {  expect_error(
    mojor_transpile(function(x, Y) cov(x, Y), x = "f64[]", Y = "f64[,]"),
    "matching rank"
  )

  expect_error(
    mojor_transpile(function(X, y) cor(X, y), X = "f64[,]", y = "f64[]"),
    "matching rank"
  )
})

test_that("cov/cor validate arguments are names", {  expect_error(
    mojor_transpile(function(x) cov(x + 1), x = "f64[]"),
    "must be a simple variable name"
  )

  expect_error(
    mojor_transpile(function(x) cor(x * 2), x = "f64[]"),
    "must be a simple variable name"
  )
})

test_that("cov/cor validate arguments are function parameters", {  expect_error(
    mojor_transpile(function(x) cov(y), x = "f64[]"),
    "is not a function parameter"
  )

  expect_error(
    mojor_transpile(function(x) cor(z, x), x = "f64[]"),
    "is not a function parameter"
  )
})
