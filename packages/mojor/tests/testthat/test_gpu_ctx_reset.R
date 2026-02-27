library(testthat)

.mojor_skip_gpu_runtime_file_unless_opt_in()

test_that("gpu ctx reset hard frees context in-process", {
  env <- environment(mojor_gpu_ctx_reset)
  old_bridge <- get(".mojor_call_bridge", envir = env, inherits = TRUE)
  old_live_count <- get("mojor_gpu_buf_f32_live_count", envir = env, inherits = TRUE)
  old_ctx <- .mojor_state$gpu_ctx
  old_cache <- .mojor_state$gpu_capability_cache
  bridge_calls <- character(0)

  on.exit({
    assign(".mojor_call_bridge", old_bridge, envir = env)
    assign("mojor_gpu_buf_f32_live_count", old_live_count, envir = env)
    .mojor_state$gpu_ctx <- old_ctx
    .mojor_state$gpu_capability_cache <- old_cache
  }, add = TRUE)

  assign(
    ".mojor_call_bridge",
    function(name, ...) {
      bridge_calls <<- c(bridge_calls, as.character(name))
      1L
    },
    envir = env
  )
  assign("mojor_gpu_buf_f32_live_count", function() 0L, envir = env)

  .mojor_state$gpu_ctx <- structure(1L, class = "mojor_gpu_ctx")
  .mojor_state$gpu_capability_cache <- list(mock = TRUE)

  expect_true(isTRUE(mojor_gpu_ctx_reset("hard")))
  expect_equal(bridge_calls, "mojor_gpu_ctx_free")
  expect_null(.mojor_state$gpu_ctx)
  expect_equal(.mojor_state$gpu_capability_cache, list())
})

test_that("gpu ctx reset soft clears cache without bridge free", {
  env <- environment(mojor_gpu_ctx_reset)
  old_bridge <- get(".mojor_call_bridge", envir = env, inherits = TRUE)
  old_live_count <- get("mojor_gpu_buf_f32_live_count", envir = env, inherits = TRUE)
  old_ctx <- .mojor_state$gpu_ctx
  old_cache <- .mojor_state$gpu_capability_cache
  bridge_calls <- character(0)

  on.exit({
    assign(".mojor_call_bridge", old_bridge, envir = env)
    assign("mojor_gpu_buf_f32_live_count", old_live_count, envir = env)
    .mojor_state$gpu_ctx <- old_ctx
    .mojor_state$gpu_capability_cache <- old_cache
  }, add = TRUE)

  assign(
    ".mojor_call_bridge",
    function(name, ...) {
      bridge_calls <<- c(bridge_calls, as.character(name))
      1L
    },
    envir = env
  )
  assign("mojor_gpu_buf_f32_live_count", function() 0L, envir = env)

  .mojor_state$gpu_ctx <- structure(1L, class = "mojor_gpu_ctx")
  .mojor_state$gpu_capability_cache <- list(mock = TRUE)

  expect_warning(
    mojor_gpu_ctx_reset("soft"),
    "use mode='hard' for a full in-process reset"
  )
  expect_equal(bridge_calls, character(0))
  expect_null(.mojor_state$gpu_ctx)
  expect_equal(.mojor_state$gpu_capability_cache, list())
})
