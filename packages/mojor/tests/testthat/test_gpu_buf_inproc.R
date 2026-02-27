.mojor_skip_gpu_runtime_file_unless_opt_in()

test_that("in-process gpu buf f32 read/write", {  skip_if_no_mojo()
  if (!mojor_is_loaded()) {
    skip("Mojo backend not loaded")
  }
  if (!mojor_has_gpu()) {
    skip("GPU not available")
  }
  mojor_path <- find_mojor()
  proto_root <- dirname(dirname(mojor_path))
  build_path <- file.path(proto_root, "build")
  cmd <- paste0(
    "source(\"", mojor_path, "\");",
    "mojor_load(\"", build_path, "\");",
    "if (!mojor_is_loaded()) quit(status=0);",
    "if (!mojor_has_gpu()) quit(status=0);",
    "x <- runif(32);",
    "buf <- mojor_gpu_buf_f32(x);",
    "out <- mojor_gpu_buf_f32_read(buf);",
    "ok <- (length(out) == length(x));",
    "mojor_gpu_buf_f32_free(buf);",
    "invisible(gc());",
    "ok <- ok && (mojor_gpu_buf_f32_live_count() == 0L);",
    "if (!ok) quit(status=1);"
  )
  status <- system2("Rscript", c("--vanilla", "-e", shQuote(cmd)))
  expect_equal(status, 0L)
})
