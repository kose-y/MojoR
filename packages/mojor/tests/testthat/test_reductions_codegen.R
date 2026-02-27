test_that("reductions share common reducer helpers", {  f64_fns <- c(
    "mojor_prod_f64",
    "mojor_min_f64",
    "mojor_max_f64",
    "mojor_mean_f64",
    "mojor_which_min_f64",
    "mojor_which_max_f64"
  )
  f32_fns <- c(
    "mojor_prod_f32",
    "mojor_min_f32",
    "mojor_max_f32",
    "mojor_mean_f32",
    "mojor_which_min_f32",
    "mojor_which_max_f32"
  )
  for (nm in f64_fns) {
    fn <- get(nm, mode = "function")
    body_txt <- paste(deparse(body(fn)), collapse = "\n")
    expect_true(grepl("\\.mojor_reduce_f64", body_txt))
  }
  for (nm in f32_fns) {
    fn <- get(nm, mode = "function")
    body_txt <- paste(deparse(body(fn)), collapse = "\n")
    expect_true(grepl("\\.mojor_reduce_f32", body_txt))
  }
})

test_that("sd/var use shared sd_mode path", {  for (nm in c("mojor_sd_f64", "mojor_sd_f32", "mojor_var_f64", "mojor_var_f32")) {
    fn <- get(nm, mode = "function")
    expect_true("sd_mode" %in% names(formals(fn)))
    body_txt <- paste(deparse(body(fn)), collapse = "\n")
    expect_true(grepl("\\.mojor_state\\$options\\$sd_mode", body_txt))
  }
})
