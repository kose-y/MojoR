# =============================================================================
# MojoR Constants
# =============================================================================
# Centralized constants for MojoR
#
# Organization:
# 1. Error Codes
# 2. IR Node Kinds
# 3. Effect Classes
# 4. Type Constants
# 5. Mojo Type Mappings
# 6. Special Values
# 7. Index Base
# 8. Layout
# 9. Broadcast Modes
# 10. NA Modes
# 11. Reduction Modes
# 12. SIMD Modes
# 13. Optimization Levels
# 14. Loop Unroll
# 15. Tile Dimensions
# 16. File Paths
# 17. Runtime Constants
# 18. Default Values
# 19. Version
# =============================================================================

# =============================================================================
# Section 1: Error Codes
# =============================================================================

# Invalid input
MOJOR_ERR_INVALID_INPUT <- 1001

# Unsupported feature
MOJOR_ERR_UNSUPPORTED_FEATURE <- 1002

# Type mismatch
MOJOR_ERR_TYPE_MISMATCH <- 1003

# Bounds violation
MOJOR_ERR_BOUNDS_VIOLATION <- 1004

# Invalid IR node
MOJOR_ERR_IR_INVALID <- 1005

# Build failure
MOJOR_ERR_BUILD_FAILED <- 1006

# Runtime error
MOJOR_ERR_RUNTIME_ERROR <- 1007

# =============================================================================
# Section 2: IR Node Kinds
# =============================================================================

# Expression nodes
MOJOR_IR_CONST <- "const"
MOJOR_IR_VAR <- "var"
MOJOR_IR_UNOP <- "unop"
MOJOR_IR_BINOP <- "binop"
MOJOR_IR_CAST <- "cast"
MOJOR_IR_CALL <- "call"
MOJOR_IR_INDEX <- "index"
MOJOR_IR_IFELSE <- "ifelse"

# Helper nodes
MOJOR_IR_SCALAR_INDEX <- "scalar_index"
MOJOR_IR_SLICE_INDEX <- "slice_index"
MOJOR_IR_MISSING_INDEX <- "missing_index"
MOJOR_IR_SUBSCRIPT <- "subscript"

# Constructor nodes
MOJOR_IR_REP <- "rep"
MOJOR_IR_REP_LEN <- "rep_len"
MOJOR_IR_C <- "c"
MOJOR_IR_SEQ <- "seq"
MOJOR_IR_TRANSPOSE <- "transpose"
MOJOR_IR_CBIND <- "cbind"
MOJOR_IR_RBIND <- "rbind"
MOJOR_IR_DIAG <- "diag"

# Cumulative operations
MOJOR_IR_CUMSUM <- "cumsum"
MOJOR_IR_CUMPROD <- "cumprod"
MOJOR_IR_CUMMAX <- "cummax"
MOJOR_IR_CUMMIN <- "cummin"

# Statistical operations
MOJOR_IR_MEAN <- "mean"
MOJOR_IR_VARIANCE <- "var_stat"
MOJOR_IR_SD <- "sd"

# Reduction operations
MOJOR_IR_SCALAR_REDUCE <- "scalar_reduce"
MOJOR_IR_SCHEDULED_REDUCE <- "scheduled_reduce"

# Matrix operations
MOJOR_IR_MATMUL <- "matmul"
MOJOR_IR_CROSSPROD <- "crossprod"
MOJOR_IR_TCROSSPROD <- "tcrossprod"

# Statement nodes
MOJOR_IR_BLOCK <- "block"
MOJOR_IR_ASSIGN <- "assign"
MOJOR_IR_IF <- "if"
MOJOR_IR_LOOP <- "loop"
MOJOR_IR_WHILE <- "while"
MOJOR_IR_REPEAT <- "repeat"
MOJOR_IR_BREAK <- "break"
MOJOR_IR_NEXT <- "next"
MOJOR_IR_RETURN <- "return"

# SSA nodes
MOJOR_IR_SSA_BLOCK <- "ssa_block"
MOJOR_IR_SSA_BR <- "br"
MOJOR_IR_SSA_COND_BR <- "condbr"
MOJOR_IR_SSA_RET <- "ret"

# =============================================================================
# Section 3: Effect Classes
# =============================================================================

# Pure: No side effects
MOJOR_EFFECT_NONE <- "None"
MOJOR_EFFECT_PURE <- "Pure"

# Memory effects
MOJOR_EFFECT_READS_MEM <- "ReadsMem"
MOJOR_EFFECT_WRITES_MEM <- "WritesMem"

# Special effects
MOJOR_EFFECT_RNG <- "RNG"
MOJOR_EFFECT_STATUS <- "Status"
MOJOR_EFFECT_UNKNOWN <- "Unknown"

# =============================================================================
# Section 4: Type Constants
# =============================================================================

# Scalar types
MOJOR_TYPE_I32 <- "i32"
MOJOR_TYPE_F64 <- "f64"
MOJOR_TYPE_F32 <- "f32"
MOJOR_TYPE_BOOL <- "bool"
MOJOR_TYPE_LGL <- "lgl"

# Array types
MOJOR_TYPE_I32_ARR <- "i32[]"
MOJOR_TYPE_F64_ARR <- "f64[]"
MOJOR_TYPE_F32_ARR <- "f32[]"
MOJOR_TYPE_BOOL_ARR <- "bool[]"
MOJOR_TYPE_LGL_ARR <- "lgl[]"

# =============================================================================
# Section 5: Mojo Type Mappings
# =============================================================================

MOJOR_MOJO_TYPE_I32 <- "Int32"
MOJOR_MOJO_TYPE_F64 <- "Float64"
MOJOR_MOJO_TYPE_F32 <- "Float32"
MOJOR_MOJO_TYPE_BOOL <- "Int32" # R logical stored as Int32

MOJOR_MOJO_DTYPE_I32 <- "DType.int32"
MOJOR_MOJO_DTYPE_F64 <- "DType.float64"
MOJOR_MOJO_DTYPE_F32 <- "DType.float32"

# =============================================================================
# Section 6: Special Values
# =============================================================================

# Inf/NaN tokens
MOJOR_INF <- "_MOJOR_INF"
MOJOR_NINF <- "_MOJOR_NINF"
MOJOR_NAN <- "_MOJOR_NAN"

# =============================================================================
# Section 7: Index Base
# =============================================================================

MOJOR_INDEX_ONE_BASED <- "one_based"
MOJOR_INDEX_ZERO_BASED <- "zero_based"

# =============================================================================
# Section 8: Layout
# =============================================================================

MOJOR_LAYOUT_COL_MAJOR <- "col_major"
MOJOR_LAYOUT_ROW_MAJOR <- "row_major"

# =============================================================================
# Section 9: Broadcast Modes
# =============================================================================

MOJOR_BROADCAST_NONE <- "none"
MOJOR_BROADCAST_SCALAR <- "scalar"
MOJOR_BROADCAST_RECYCLE <- "recycle"
MOJOR_BROADCAST_RECYCLE_WARN <- "recycle_warn"
MOJOR_BROADCAST_ND <- "broadcast_nd"
MOJOR_BROADCAST_ND_WARN <- "broadcast_nd_warn"

# =============================================================================
# Section 10: NA Modes
# =============================================================================

MOJOR_NA_MODE_DEFAULT <- "default"
MOJOR_NA_MODE_FORBID <- "forbid"
MOJOR_NA_MODE_UNSAFE <- "unsafe"

# =============================================================================
# Section 11: Reduction Modes
# =============================================================================

MOJOR_REDUCTION_AUTO <- "auto"
MOJOR_REDUCTION_LINEAR <- "linear"
MOJOR_REDUCTION_TREE <- "tree"
MOJOR_REDUCTION_SIMD <- "simd"

# =============================================================================
# Section 12: SIMD Modes
# =============================================================================

MOJOR_SIMD_EXPLICIT <- "explicit"
MOJOR_SIMD_AUTO <- "auto"
MOJOR_SIMD_OFF <- "off"

# =============================================================================
# Section 13: Optimization Levels
# =============================================================================

MOJOR_OPT_LEVEL_0 <- 0 # No optimization
MOJOR_OPT_LEVEL_1 <- 1 # Basic optimization
MOJOR_OPT_LEVEL_2 <- 2 # Moderate optimization
MOJOR_OPT_LEVEL_3 <- 3 # Aggressive optimization

# =============================================================================
# Section 14: Loop Unroll
# =============================================================================

MOJOR_UNROLL_MIN <- 1
MOJOR_UNROLL_MAX <- 16

# =============================================================================
# Section 15: Tile Dimensions
# =============================================================================

MOJOR_TILE_MIN <- 1
MOJOR_TILE_MAX <- 3 # 1D, 2D, or 3D

# =============================================================================
# Section 16: File Paths
# =============================================================================

# Source files
MOJOR_SRC_ABI_TYPES <- "src/abi_types.mojo"
MOJOR_SRC_BACKEND <- "src/backend.mojo"
MOJOR_SRC_BACKEND_RNG <- "src/backend_rng.mojo"
MOJOR_SRC_BACKEND_GAMMA <- "src/backend_gamma.mojo"
MOJOR_SRC_RNG_HELPERS <- "src/rng_helpers.mojo"
MOJOR_SRC_ZIGGURAT <- "src/ziggurat_constants.mojo"

# =============================================================================
# Section 17: Runtime Constants
# =============================================================================

# Alignment
MOJOR_ALIGN_F32 <- 4
MOJOR_ALIGN_F64 <- 8

# NA value for R logical (stored as NA_integer_ in R)
MOJOR_NA_LOGICAL <- NA_integer_

# =============================================================================
# Section 18: Default Values
# =============================================================================

MOJOR_DEFAULT_N_VAR <- "n_i"
MOJOR_DEFAULT_OUT_NAME <- "out"
MOJOR_DEFAULT_OPT_LEVEL <- 2L
MOJOR_DEFAULT_BOUNDS_CHECK <- TRUE
MOJOR_DEFAULT_INDEX_BASE <- "one_based"
MOJOR_DEFAULT_ARRAY_LAYOUT <- "col_major"

# =============================================================================
# Section 19: Version
# =============================================================================

MOJOR_VERSION <- "0.1.0"
MOJOR_IR_VERSION <- "1.0"
