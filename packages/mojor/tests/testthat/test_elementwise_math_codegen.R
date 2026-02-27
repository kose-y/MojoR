extract_ew_kernel <- function(mojo) {
  start <- regexpr("fn _mojor_elementwise\\[", mojo)
  if (start < 0) return(NULL)
  sub <- substr(mojo, start, nchar(mojo))
  end_rel <- regexpr("fn _mojor_elementwise_[A-Za-z0-9]+", sub)
  if (end_rel < 0) return(sub)
  substr(sub, 1, end_rel - 1)
}

test_that("elementwise math ops share the same mapping as scalar path", {  blocked_cpu_elementwise <- c("tan", "expm1")
  ops <- c(
    sin = "sin(",
    cos = "cos(",
    tan = "tan(",
    asin = "asin(",
    acos = "acos(",
    atan = "atan(",
    sinh = "sinh(",
    cosh = "cosh(",
    tanh = "tanh(",
    log1p = "log1p(",
    expm1 = "expm1(",
    floor = "floor(",
    ceiling = "ceil(",
    trunc = "trunc(",
    round = "round(",
    sign = "sign(",
    abs = "abs(",
    sqrt = "sqrt(",
    exp = "exp(",
    log = "log(",
    cbrt = "cbrt(",
    lgamma = "lgamma(",
    erf = "erf("
  )

  for (op in names(ops)) {
    f_txt <- paste0(
      "function(x) { out <- numeric(length(x)); ",
      "for (i in seq_along(x)) { out[i] <- ", op, "(x[i]) }; out }"
    )
    f <- eval(parse(text = f_txt))
    trans_ew <- mojor_transpile(
      f,
      x = "f64[]",
      elementwise = TRUE,
      elementwise_cpu = TRUE,
      name = paste0("t_ew_", op)
    )
    expect_true(isTRUE(trans_ew$elementwise$emitted))
    expect_true(grepl(ops[[op]], trans_ew$mojo, fixed = TRUE))
    kernel <- extract_ew_kernel(trans_ew$mojo)
    if (op %in% blocked_cpu_elementwise) {
      expect_null(kernel)
      expect_false(grepl("fn _mojor_elementwise\\[", trans_ew$mojo))
      expect_true(
        grepl("for _mojor_i in range", trans_ew$mojo, fixed = TRUE) ||
          grepl("for i in range", trans_ew$mojo, fixed = TRUE)
      )
    } else {
      expect_true(is.character(kernel))
      expect_true(grepl("var i = Int\\(indices\\[0\\]\\)", kernel))
      expect_true(
        grepl("out[i]", kernel, fixed = TRUE) ||
          grepl("out[Int(i)]", kernel, fixed = TRUE)
      )
      expect_true(
        grepl("x[i]", kernel, fixed = TRUE) ||
          grepl("_mojor_read_f64\\(x, i", kernel) ||
          grepl("_mojor_read_f64\\(x, Int\\(i\\)", kernel) ||
          grepl("_mojor_read_f64\\(x, Int\\(", kernel)
      )
      expect_false(grepl("\\(i - 1\\)", kernel, fixed = TRUE))
    }

    trans_std <- mojor_transpile(
      f,
      x = "f64[]",
      elementwise = FALSE,
      name = paste0("t_std_", op)
    )
    expect_true(grepl(ops[[op]], trans_std$mojo, fixed = TRUE))
  }
})
