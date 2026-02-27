# =============================================================================
# MojoR Context Management
# =============================================================================
# Centralized context management for MojoR
#
# Organization:
# 1. Context Class Definition
# 2. Context Constructors
# 3. Context Accessors
# 4. Context Mutators
# 5. Context Serialization
# =============================================================================

# =============================================================================
# Section 1: Context Class Definition
# =============================================================================

#' MojoR Context Class
#'
#' Immutable context object for state management
#'
#' @field options Options list
#' @field diagnostics Diagnostics list
#' @field diagnostics_sink Diagnostics sink functions
#' @field current_srcref Current source reference
#' @field current_function_name Current function name
#' @field type_env Type environment
#' @field var_map Variable map
#' @field layout_ctx Layout context
#' @field index_ctx Index context
#' @field effect_ctx Effect context
#' @field rng_state RNG state
#'
#' @noRd
setClass("MojoRContext", representation(
  options = "list",
  diagnostics = "list",
  diagnostics_sink = "list",
  current_srcref = "ANY",
  current_function_name = "character",
  type_env = "list",
  var_map = "list",
  layout_ctx = "list",
  index_ctx = "list",
  effect_ctx = "list",
  rng_state = "ANY"
))

# =============================================================================
# Section 2: Context Constructors
# =============================================================================

#' Create a new MojoR context
#'
#' @param options Options list
#' @param diagnostics Diagnostics list
#' @param diagnostics_sink Diagnostics sink functions
#' @param current_srcref Current source reference
#' @param current_function_name Current function name
#' @param type_env Type environment
#' @param var_map Variable map
#' @param layout_ctx Layout context
#' @param index_ctx Index context
#' @param effect_ctx Effect context
#' @param rng_state RNG state
#'
#' @noRd
mojor_context_new <- function(
    options = list(),
    diagnostics = list(),
    diagnostics_sink = list(),
    current_srcref = NULL,
    current_function_name = character(0),
    type_env = list(),
    var_map = list(),
    layout_ctx = list(),
    index_ctx = list(),
    effect_ctx = list(),
    rng_state = NULL) {
  new("MojoRContext",
    options = options,
    diagnostics = diagnostics,
    diagnostics_sink = diagnostics_sink,
    current_srcref = current_srcref,
    current_function_name = current_function_name,
    type_env = type_env,
    var_map = var_map,
    layout_ctx = layout_ctx,
    index_ctx = index_ctx,
    effect_ctx = effect_ctx,
    rng_state = rng_state
  )
}

#' Create context from state list
#'
#' @param state State list
#'
#' @noRd
mojor_context_from_state <- function(state) {
  mojor_context_new(
    options = state$options,
    diagnostics = state$diagnostics,
    diagnostics_sink = state$diagnostics_sink,
    current_srcref = state$current_srcref,
    current_function_name = state$current_function_name,
    type_env = state$type_env,
    var_map = state$var_map,
    layout_ctx = state$layout_ctx,
    index_ctx = state$index_ctx,
    effect_ctx = state$effect_ctx,
    rng_state = state$rng_state
  )
}

# =============================================================================
# Section 3: Context Accessors
# =============================================================================

#' Get options from context
#'
#' @param ctx Context object
#'
#' @noRd
mojor_context_get_options <- function(ctx) {
  ctx$options
}

#' Get option value from context
#'
#' @param ctx Context object
#' @param name Option name
#' @param default Default value
#'
#' @noRd
mojor_context_get_option <- function(ctx, name, default = NULL) {
  if (is.null(ctx$options[[name]])) {
    default
  } else {
    ctx$options[[name]]
  }
}

#' Get diagnostics from context
#'
#' @param ctx Context object
#'
#' @noRd
mojor_context_get_diagnostics <- function(ctx) {
  ctx$diagnostics
}

#' Get diagnostics sink from context
#'
#' @param ctx Context object
#'
#' @noRd
mojor_context_get_diagnostics_sink <- function(ctx) {
  ctx$diagnostics_sink
}

#' Get current srcref from context
#'
#' @param ctx Context object
#'
#' @noRd
mojor_context_get_srcref <- function(ctx) {
  ctx$current_srcref
}

#' Get current function name from context
#'
#' @param ctx Context object
#'
#' @noRd
mojor_context_get_function_name <- function(ctx) {
  ctx$current_function_name
}

#' Get type environment from context
#'
#' @param ctx Context object
#'
#' @noRd
mojor_context_get_type_env <- function(ctx) {
  ctx$type_env
}

#' Get variable map from context
#'
#' @param ctx Context object
#'
#' @noRd
mojor_context_get_var_map <- function(ctx) {
  ctx$var_map
}

#' Get layout context from context
#'
#' @param ctx Context object
#'
#' @noRd
mojor_context_get_layout_ctx <- function(ctx) {
  ctx$layout_ctx
}

#' Get index context from context
#'
#' @param ctx Context object
#'
#' @noRd
mojor_context_get_index_ctx <- function(ctx) {
  ctx$index_ctx
}

#' Get effect context from context
#'
#' @param ctx Context object
#'
#' @noRd
mojor_context_get_effect_ctx <- function(ctx) {
  ctx$effect_ctx
}

#' Get RNG state from context
#'
#' @param ctx Context object
#'
#' @noRd
mojor_context_get_rng_state <- function(ctx) {
  ctx$rng_state
}

# =============================================================================
# Section 4: Context Mutators
# =============================================================================

#' Update context with new options
#'
#' @param ctx Context object
#' @param options New options list
#'
#' @noRd
mojor_context_with_options <- function(ctx, options) {
  new("MojoRContext",
    options = options,
    diagnostics = ctx$diagnostics,
    diagnostics_sink = ctx$diagnostics_sink,
    current_srcref = ctx$current_srcref,
    current_function_name = ctx$current_function_name,
    type_env = ctx$type_env,
    var_map = ctx$var_map,
    layout_ctx = ctx$layout_ctx,
    index_ctx = ctx$index_ctx,
    effect_ctx = ctx$effect_ctx,
    rng_state = ctx$rng_state
  )
}

#' Update context with new option
#'
#' @param ctx Context object
#' @param name Option name
#' @param value Option value
#'
#' @noRd
mojor_context_with_option <- function(ctx, name, value) {
  new_options <- ctx$options
  new_options[[name]] <- value
  mojor_context_with_options(ctx, new_options)
}

#' Update context with new diagnostics
#'
#' @param ctx Context object
#' @param diagnostics New diagnostics list
#'
#' @noRd
mojor_context_with_diagnostics <- function(ctx, diagnostics) {
  new("MojoRContext",
    options = ctx$options,
    diagnostics = diagnostics,
    diagnostics_sink = ctx$diagnostics_sink,
    current_srcref = ctx$current_srcref,
    current_function_name = ctx$current_function_name,
    type_env = ctx$type_env,
    var_map = ctx$var_map,
    layout_ctx = ctx$layout_ctx,
    index_ctx = ctx$index_ctx,
    effect_ctx = ctx$effect_ctx,
    rng_state = ctx$rng_state
  )
}

#' Update context with new diagnostics sink
#'
#' @param ctx Context object
#' @param sink New diagnostics sink
#'
#' @noRd
mojor_context_with_diagnostics_sink <- function(ctx, sink) {
  new("MojoRContext",
    options = ctx$options,
    diagnostics = ctx$diagnostics,
    diagnostics_sink = sink,
    current_srcref = ctx$current_srcref,
    current_function_name = ctx$current_function_name,
    type_env = ctx$type_env,
    var_map = ctx$var_map,
    layout_ctx = ctx$layout_ctx,
    index_ctx = ctx$index_ctx,
    effect_ctx = ctx$effect_ctx,
    rng_state = ctx$rng_state
  )
}

#' Update context with new srcref
#'
#' @param ctx Context object
#' @param srcref New source reference
#'
#' @noRd
mojor_context_with_srcref <- function(ctx, srcref) {
  new("MojoRContext",
    options = ctx$options,
    diagnostics = ctx$diagnostics,
    diagnostics_sink = ctx$diagnostics_sink,
    current_srcref = srcref,
    current_function_name = ctx$current_function_name,
    type_env = ctx$type_env,
    var_map = ctx$var_map,
    layout_ctx = ctx$layout_ctx,
    index_ctx = ctx$index_ctx,
    effect_ctx = ctx$effect_ctx,
    rng_state = ctx$rng_state
  )
}

#' Update context with new function name
#'
#' @param ctx Context object
#' @param name New function name
#'
#' @noRd
mojor_context_with_function_name <- function(ctx, name) {
  new("MojoRContext",
    options = ctx$options,
    diagnostics = ctx$diagnostics,
    diagnostics_sink = ctx$diagnostics_sink,
    current_srcref = ctx$current_srcref,
    current_function_name = name,
    type_env = ctx$type_env,
    var_map = ctx$var_map,
    layout_ctx = ctx$layout_ctx,
    index_ctx = ctx$index_ctx,
    effect_ctx = ctx$effect_ctx,
    rng_state = ctx$rng_state
  )
}

#' Update context with new type environment
#'
#' @param ctx Context object
#' @param type_env New type environment
#'
#' @noRd
mojor_context_with_type_env <- function(ctx, type_env) {
  new("MojoRContext",
    options = ctx$options,
    diagnostics = ctx$diagnostics,
    diagnostics_sink = ctx$diagnostics_sink,
    current_srcref = ctx$current_srcref,
    current_function_name = ctx$current_function_name,
    type_env = type_env,
    var_map = ctx$var_map,
    layout_ctx = ctx$layout_ctx,
    index_ctx = ctx$index_ctx,
    effect_ctx = ctx$effect_ctx,
    rng_state = ctx$rng_state
  )
}

#' Update context with new variable map
#'
#' @param ctx Context object
#' @param var_map New variable map
#'
#' @noRd
mojor_context_with_var_map <- function(ctx, var_map) {
  new("MojoRContext",
    options = ctx$options,
    diagnostics = ctx$diagnostics,
    diagnostics_sink = ctx$diagnostics_sink,
    current_srcref = ctx$current_srcref,
    current_function_name = ctx$current_function_name,
    type_env = ctx$type_env,
    var_map = var_map,
    layout_ctx = ctx$layout_ctx,
    index_ctx = ctx$index_ctx,
    effect_ctx = ctx$effect_ctx,
    rng_state = ctx$rng_state
  )
}

#' Update context with new layout context
#'
#' @param ctx Context object
#' @param layout_ctx New layout context
#'
#' @noRd
mojor_context_with_layout_ctx <- function(ctx, layout_ctx) {
  new("MojoRContext",
    options = ctx$options,
    diagnostics = ctx$diagnostics,
    diagnostics_sink = ctx$diagnostics_sink,
    current_srcref = ctx$current_srcref,
    current_function_name = ctx$current_function_name,
    type_env = ctx$type_env,
    var_map = ctx$var_map,
    layout_ctx = layout_ctx,
    index_ctx = ctx$index_ctx,
    effect_ctx = ctx$effect_ctx,
    rng_state = ctx$rng_state
  )
}

#' Update context with new index context
#'
#' @param ctx Context object
#' @param index_ctx New index context
#'
#' @noRd
mojor_context_with_index_ctx <- function(ctx, index_ctx) {
  new("MojoRContext",
    options = ctx$options,
    diagnostics = ctx$diagnostics,
    diagnostics_sink = ctx$diagnostics_sink,
    current_srcref = ctx$current_srcref,
    current_function_name = ctx$current_function_name,
    type_env = ctx$type_env,
    var_map = ctx$var_map,
    layout_ctx = ctx$layout_ctx,
    index_ctx = index_ctx,
    effect_ctx = ctx$effect_ctx,
    rng_state = ctx$rng_state
  )
}

#' Update context with new effect context
#'
#' @param ctx Context object
#' @param effect_ctx New effect context
#'
#' @noRd
mojor_context_with_effect_ctx <- function(ctx, effect_ctx) {
  new("MojoRContext",
    options = ctx$options,
    diagnostics = ctx$diagnostics,
    diagnostics_sink = ctx$diagnostics_sink,
    current_srcref = ctx$current_srcref,
    current_function_name = ctx$current_function_name,
    type_env = ctx$type_env,
    var_map = ctx$var_map,
    layout_ctx = ctx$layout_ctx,
    index_ctx = ctx$index_ctx,
    effect_ctx = effect_ctx,
    rng_state = ctx$rng_state
  )
}

#' Update context with new RNG state
#'
#' @param ctx Context object
#' @param rng_state New RNG state
#'
#' @noRd
mojor_context_with_rng_state <- function(ctx, rng_state) {
  new("MojoRContext",
    options = ctx$options,
    diagnostics = ctx$diagnostics,
    diagnostics_sink = ctx$diagnostics_sink,
    current_srcref = ctx$current_srcref,
    current_function_name = ctx$current_function_name,
    type_env = ctx$type_env,
    var_map = ctx$var_map,
    layout_ctx = ctx$layout_ctx,
    index_ctx = ctx$index_ctx,
    effect_ctx = ctx$effect_ctx,
    rng_state = rng_state
  )
}

# =============================================================================
# Section 5: Context Serialization
# =============================================================================

#' Convert context to list
#'
#' @param ctx Context object
#'
#' @noRd
mojor_context_to_list <- function(ctx) {
  list(
    options = ctx$options,
    diagnostics = ctx$diagnostics,
    diagnostics_sink = ctx$diagnostics_sink,
    current_srcref = ctx$current_srcref,
    current_function_name = ctx$current_function_name,
    type_env = ctx$type_env,
    var_map = ctx$var_map,
    layout_ctx = ctx$layout_ctx,
    index_ctx = ctx$index_ctx,
    effect_ctx = ctx$effect_ctx,
    rng_state = ctx$rng_state
  )
}

#' Convert list to context
#'
#' @param state State list
#'
#' @noRd
mojor_context_from_list <- function(state) {
  mojor_context_from_state(state)
}
