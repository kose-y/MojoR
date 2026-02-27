#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/RS.h>
#include <Rmath.h>
#include <string.h>
#include <stdint.h>
#include <limits.h>
#include <stdio.h>
#include "backend_stub.h"

// Backend ABI
// mojor_sum_f64
// mojor_scale_f64
// mojor_abs_f64
// mojor_running_max_f64

#define MOJOR_GPU_BUF_STRUCT(tag) \
    typedef struct { \
        void* ctx; \
        void* handle; \
        int n; \
    } tag;

MOJOR_GPU_BUF_STRUCT(mojor_gpu_buf_f32_t)
MOJOR_GPU_BUF_STRUCT(mojor_gpu_buf_f64_t)
MOJOR_GPU_BUF_STRUCT(mojor_gpu_buf_i32_t)

typedef struct {
    void* ctx;
    void* handle;
    int ndim;
    int idx_data_n;
    int out_n;
    int full_n;
} mojor_gpu_idx_plan_f32_t;

typedef struct {
    void* ctx;
    void* handle;
    int ndim;
    int idx_data_n;
    int out_n;
    int full_n;
} mojor_gpu_idx_plan_f64_t;

typedef struct mojor_gpu_handle_node {
    void* handle;
    struct mojor_gpu_handle_node* next;
} mojor_gpu_handle_node;

static mojor_gpu_handle_node* mojor_gpu_handles = NULL;
static int mojor_gpu_handle_count = 0;

static uint64_t mojor_rng_state[4] = {0, 0, 0, 0};
static int mojor_rng_seeded = 0;

static uint64_t mojor_splitmix64(uint64_t* x) {
    uint64_t z = (*x += 0x9e3779b97f4a7c15ULL);
    z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9ULL;
    z = (z ^ (z >> 27)) * 0x94d049bb133111ebULL;
    return z ^ (z >> 31);
}

static void mojor_rng_seed_state(uint64_t seed) {
    uint64_t x = seed;
    mojor_rng_state[0] = mojor_splitmix64(&x);
    mojor_rng_state[1] = mojor_splitmix64(&x);
    mojor_rng_state[2] = mojor_splitmix64(&x);
    mojor_rng_state[3] = mojor_splitmix64(&x);
    mojor_rng_seeded = 1;
}

static void mojor_rng_ensure_seeded(void) {
    if (!mojor_rng_seeded) {
        mojor_rng_seed_state(0x106689d45497fdb5ULL);
    }
}

static void mojor_gpu_handle_add(void* handle) {
    if (!handle) {
        return;
    }
    mojor_gpu_handle_node* node = (mojor_gpu_handle_node*) malloc(sizeof(mojor_gpu_handle_node));
    if (!node) {
        return;
    }
    node->handle = handle;
    node->next = mojor_gpu_handles;
    mojor_gpu_handles = node;
    mojor_gpu_handle_count += 1;
}

static int mojor_gpu_handle_remove(void* handle) {
    mojor_gpu_handle_node* prev = NULL;
    mojor_gpu_handle_node* cur = mojor_gpu_handles;
    while (cur) {
        if (cur->handle == handle) {
            if (prev) {
                prev->next = cur->next;
            } else {
                mojor_gpu_handles = cur->next;
            }
            free(cur);
            mojor_gpu_handle_count -= 1;
            return 1;
        }
        prev = cur;
        cur = cur->next;
    }
    return 0;
}

void mojor_gpu_handle_add_export(void* handle) {
    mojor_gpu_handle_add(handle);
}

int mojor_gpu_handle_remove_export(void* handle) {
    return mojor_gpu_handle_remove(handle);
}

static void* mojor_gpu_ctx_get(SEXP ext) {
    if (ext == R_NilValue) {
        return NULL;
    }
    if (TYPEOF(ext) != EXTPTRSXP) {
        error("mojor_gpu_ctx: invalid context");
    }
    return R_ExternalPtrAddr(ext);
}

static void mojor_gpu_ctx_finalizer(SEXP ext) {
    if (TYPEOF(ext) != EXTPTRSXP) {
        return;
    }
    void* ctx = R_ExternalPtrAddr(ext);
    if (ctx) {
        mojor_gpu_ctx_free(ctx);
        R_ClearExternalPtr(ext);
    }
}

#define MOJOR_GPU_BUF_GETTER(fn_name, type) \
    static type* fn_name(SEXP ext) { \
        if (TYPEOF(ext) != EXTPTRSXP) { \
            return NULL; \
        } \
        return (type*) R_ExternalPtrAddr(ext); \
    }

#define MOJOR_GPU_BUF_FINALIZER(fn_name, getter, type, free_fn) \
    static void fn_name(SEXP ext) { \
        type* buf = getter(ext); \
        if (!buf) { \
            return; \
        } \
        if (buf->handle) { \
            free_fn(buf->ctx, buf->handle); \
            mojor_gpu_handle_remove(buf->handle); \
            buf->handle = NULL; \
        } \
        buf->ctx = NULL; \
        R_Free(buf); \
        R_ClearExternalPtr(ext); \
    }

MOJOR_GPU_BUF_GETTER(mojor_gpu_buf_f32_get, mojor_gpu_buf_f32_t)
MOJOR_GPU_BUF_GETTER(mojor_gpu_buf_f64_get, mojor_gpu_buf_f64_t)
MOJOR_GPU_BUF_GETTER(mojor_gpu_buf_i32_get, mojor_gpu_buf_i32_t)
MOJOR_GPU_BUF_GETTER(mojor_gpu_idx_plan_f32_get, mojor_gpu_idx_plan_f32_t)
MOJOR_GPU_BUF_GETTER(mojor_gpu_idx_plan_f64_get, mojor_gpu_idx_plan_f64_t)

MOJOR_GPU_BUF_FINALIZER(mojor_gpu_buf_f32_finalizer, mojor_gpu_buf_f32_get, mojor_gpu_buf_f32_t, mojor_gpu_buf_f32_free)
MOJOR_GPU_BUF_FINALIZER(mojor_gpu_buf_f64_finalizer, mojor_gpu_buf_f64_get, mojor_gpu_buf_f64_t, mojor_gpu_buf_f64_free)
MOJOR_GPU_BUF_FINALIZER(mojor_gpu_buf_i32_finalizer, mojor_gpu_buf_i32_get, mojor_gpu_buf_i32_t, mojor_gpu_buf_i32_free)
MOJOR_GPU_BUF_FINALIZER(mojor_gpu_idx_plan_f32_finalizer, mojor_gpu_idx_plan_f32_get, mojor_gpu_idx_plan_f32_t, mojor_gpu_idx_plan_f32_free)
MOJOR_GPU_BUF_FINALIZER(mojor_gpu_idx_plan_f64_finalizer, mojor_gpu_idx_plan_f64_get, mojor_gpu_idx_plan_f64_t, mojor_gpu_idx_plan_f64_free)

// Common R wrappers for reduction helpers
#define MOJOR_REDUCE_F64_CALL(RFN, CFN, ERRNAME) \
    SEXP RFN(SEXP r_vec) { \
        if (TYPEOF(r_vec) != REALSXP) { \
            error(ERRNAME ": expected numeric vector"); \
        } \
        PROTECT(r_vec); \
        double* ptr = REAL(r_vec); \
        int n = LENGTH(r_vec); \
        double result = CFN(ptr, n); \
        UNPROTECT(1); \
        return ScalarReal(result); \
    }

#define MOJOR_REDUCE_F64_WHICH_CALL(RFN, CFN, ERRNAME) \
    SEXP RFN(SEXP r_vec) { \
        if (TYPEOF(r_vec) != REALSXP) { \
            error(ERRNAME ": expected numeric vector"); \
        } \
        PROTECT(r_vec); \
        double* ptr = REAL(r_vec); \
        int n = LENGTH(r_vec); \
        if (n == 0) { \
            UNPROTECT(1); \
            return allocVector(INTSXP, 0); \
        } \
        int result = CFN(ptr, n); \
        UNPROTECT(1); \
        return ScalarInteger(result); \
    }

#define MOJOR_REDUCE_F32_INT_COPY_CALL(RFN, CFN, ERRNAME) \
    SEXP RFN(SEXP r_int) { \
        if (TYPEOF(r_int) != INTSXP) { \
            error(ERRNAME ": expected integer vector (float bits)"); \
        } \
        int n = LENGTH(r_int); \
        PROTECT(r_int); \
        float* buf = (float*) R_alloc((size_t) n, sizeof(float)); \
        memcpy(buf, INTEGER(r_int), (size_t) n * sizeof(float)); \
        float result = CFN(buf, n); \
        UNPROTECT(1); \
        return ScalarReal((double) result); \
    }

#define MOJOR_REDUCE_F32_INT_WHICH_COPY_CALL(RFN, CFN, ERRNAME) \
    SEXP RFN(SEXP r_int) { \
        if (TYPEOF(r_int) != INTSXP) { \
            error(ERRNAME ": expected integer vector (float bits)"); \
        } \
        int n = LENGTH(r_int); \
        PROTECT(r_int); \
        if (n == 0) { \
            UNPROTECT(1); \
            return allocVector(INTSXP, 0); \
        } \
        float* buf = (float*) R_alloc((size_t) n, sizeof(float)); \
        memcpy(buf, INTEGER(r_int), (size_t) n * sizeof(float)); \
        int result = CFN(buf, n); \
        UNPROTECT(1); \
        return ScalarInteger(result); \
    }

#define MOJOR_REDUCE_F32_INT_BITS_CALL(RFN, CFN, ERRNAME) \
    SEXP RFN(SEXP r_int) { \
        if (TYPEOF(r_int) != INTSXP) { \
            error(ERRNAME ": expected integer vector (float bits)"); \
        } \
        PROTECT(r_int); \
        int* bits = INTEGER(r_int); \
        int n = LENGTH(r_int); \
        float* buf = (float*) bits; \
        float result = CFN(buf, n); \
        UNPROTECT(1); \
        return ScalarReal((double) result); \
    }

MOJOR_REDUCE_F64_CALL(mojor_sum_f64_call, mojor_sum_f64, "mojor_sum_f64")
MOJOR_REDUCE_F64_CALL(mojor_sum_f64_std_call, mojor_sum_f64_std, "mojor_sum_f64_std")
MOJOR_REDUCE_F64_CALL(mojor_sum_f64_pairwise_call, mojor_sum_f64_pairwise, "mojor_sum_f64_pairwise")
MOJOR_REDUCE_F64_CALL(mojor_sum_f64_nomiss_call, mojor_sum_f64_nomiss, "mojor_sum_f64_nomiss")
MOJOR_REDUCE_F64_CALL(mojor_sum_f64_nomiss_manual_call, mojor_sum_f64_nomiss_manual, "mojor_sum_f64_nomiss_manual")
MOJOR_REDUCE_F64_CALL(mojor_prod_f64_call, mojor_prod_f64, "mojor_prod_f64")
MOJOR_REDUCE_F64_CALL(mojor_prod_f64_nomiss_call, mojor_prod_f64_nomiss, "mojor_prod_f64_nomiss")
MOJOR_REDUCE_F64_WHICH_CALL(mojor_which_min_f64_nomiss_call, mojor_which_min_f64_nomiss, "mojor_which_min_f64_nomiss")
MOJOR_REDUCE_F64_WHICH_CALL(mojor_which_max_f64_nomiss_call, mojor_which_max_f64_nomiss, "mojor_which_max_f64_nomiss")
MOJOR_REDUCE_F64_CALL(mojor_min_f64_call, mojor_min_f64, "mojor_min_f64")
MOJOR_REDUCE_F64_CALL(mojor_min_f64_nomiss_call, mojor_min_f64_nomiss, "mojor_min_f64_nomiss")
MOJOR_REDUCE_F64_CALL(mojor_max_f64_call, mojor_max_f64, "mojor_max_f64")
MOJOR_REDUCE_F64_CALL(mojor_max_f64_nomiss_call, mojor_max_f64_nomiss, "mojor_max_f64_nomiss")
MOJOR_REDUCE_F64_CALL(mojor_mean_f64_call, mojor_mean_f64, "mojor_mean_f64")
MOJOR_REDUCE_F64_CALL(mojor_mean_f64_nomiss_call, mojor_mean_f64_nomiss, "mojor_mean_f64_nomiss")

MOJOR_REDUCE_F32_INT_COPY_CALL(mojor_sum_f32_int_call, mojor_sum_f32, "mojor_sum_f32")
MOJOR_REDUCE_F32_INT_COPY_CALL(mojor_sum_f32_std_int_call, mojor_sum_f32_std, "mojor_sum_f32_std")
MOJOR_REDUCE_F32_INT_COPY_CALL(mojor_sum_f32_pairwise_int_call, mojor_sum_f32_pairwise, "mojor_sum_f32_pairwise")
MOJOR_REDUCE_F32_INT_COPY_CALL(mojor_sum_f32_nomiss_int_call, mojor_sum_f32_nomiss, "mojor_sum_f32_nomiss")
MOJOR_REDUCE_F32_INT_COPY_CALL(mojor_prod_f32_int_call, mojor_prod_f32, "mojor_prod_f32")
MOJOR_REDUCE_F32_INT_COPY_CALL(mojor_prod_f32_nomiss_int_call, mojor_prod_f32_nomiss, "mojor_prod_f32_nomiss")
MOJOR_REDUCE_F32_INT_WHICH_COPY_CALL(mojor_which_min_f32_nomiss_int_call, mojor_which_min_f32_nomiss, "mojor_which_min_f32_nomiss")
MOJOR_REDUCE_F32_INT_WHICH_COPY_CALL(mojor_which_max_f32_nomiss_int_call, mojor_which_max_f32_nomiss, "mojor_which_max_f32_nomiss")
MOJOR_REDUCE_F32_INT_COPY_CALL(mojor_min_f32_int_call, mojor_min_f32, "mojor_min_f32")
MOJOR_REDUCE_F32_INT_COPY_CALL(mojor_min_f32_nomiss_int_call, mojor_min_f32_nomiss, "mojor_min_f32_nomiss")
MOJOR_REDUCE_F32_INT_COPY_CALL(mojor_max_f32_int_call, mojor_max_f32, "mojor_max_f32")
MOJOR_REDUCE_F32_INT_COPY_CALL(mojor_max_f32_nomiss_int_call, mojor_max_f32_nomiss, "mojor_max_f32_nomiss")
MOJOR_REDUCE_F32_INT_COPY_CALL(mojor_mean_f32_int_call, mojor_mean_f32, "mojor_mean_f32")
MOJOR_REDUCE_F32_INT_COPY_CALL(mojor_mean_f32_nomiss_int_call, mojor_mean_f32_nomiss, "mojor_mean_f32_nomiss")

MOJOR_REDUCE_F64_CALL(mojor_sd_f64_nomiss_call, mojor_sd_f64_nomiss, "mojor_sd_f64_nomiss")
MOJOR_REDUCE_F32_INT_BITS_CALL(mojor_sd_f32_nomiss_int_call, mojor_sd_f32_nomiss, "mojor_sd_f32_nomiss")
MOJOR_REDUCE_F64_CALL(mojor_var_f64_nomiss_call, mojor_var_f64_nomiss, "mojor_var_f64_nomiss")
MOJOR_REDUCE_F32_INT_BITS_CALL(mojor_var_f32_nomiss_int_call, mojor_var_f32_nomiss, "mojor_var_f32_nomiss")
MOJOR_REDUCE_F64_CALL(mojor_var_f64_nomiss_twopass_call, mojor_var_f64_nomiss_twopass, "mojor_var_f64_nomiss_twopass")
MOJOR_REDUCE_F32_INT_BITS_CALL(mojor_var_f32_nomiss_twopass_int_call, mojor_var_f32_nomiss_twopass, "mojor_var_f32_nomiss_twopass")
MOJOR_REDUCE_F64_CALL(mojor_sd_f64_nomiss_twopass_call, mojor_sd_f64_nomiss_twopass, "mojor_sd_f64_nomiss_twopass")
MOJOR_REDUCE_F32_INT_BITS_CALL(mojor_sd_f32_nomiss_twopass_int_call, mojor_sd_f32_nomiss_twopass, "mojor_sd_f32_nomiss_twopass")


// Common R wrappers for elementwise-style CPU helpers
#define MOJOR_REAL_UNARY_SCALAR_CALL(RFN, CFN, ERRNAME) \
    SEXP RFN(SEXP r_vec, SEXP r_scalar) { \
        if (TYPEOF(r_vec) != REALSXP) { \
            error(ERRNAME ": expected numeric vector"); \
        } \
        if (TYPEOF(r_scalar) != REALSXP || LENGTH(r_scalar) != 1) { \
            error(ERRNAME ": expected numeric scalar"); \
        } \
        PROTECT(r_vec); \
        PROTECT(r_scalar); \
        double* in_ptr = REAL(r_vec); \
        int n = LENGTH(r_vec); \
        double scalar = REAL(r_scalar)[0]; \
        SEXP result = PROTECT(allocVector(REALSXP, n)); \
        double* out_ptr = REAL(result); \
        CFN(in_ptr, out_ptr, n, scalar); \
        UNPROTECT(3); \
        return result; \
    }

#define MOJOR_REAL_UNARY_OUT_CALL(RFN, CFN, ERRNAME) \
    SEXP RFN(SEXP r_vec) { \
        if (TYPEOF(r_vec) != REALSXP) { \
            error(ERRNAME ": expected numeric vector"); \
        } \
        PROTECT(r_vec); \
        double* in_ptr = REAL(r_vec); \
        int n = LENGTH(r_vec); \
        SEXP result = PROTECT(allocVector(REALSXP, n)); \
        double* out_ptr = REAL(result); \
        CFN(in_ptr, out_ptr, n); \
        UNPROTECT(2); \
        return result; \
    }

#define MOJOR_REAL_UNARY_INPLACE_CALL(RFN, CFN, ERRNAME) \
    SEXP RFN(SEXP r_vec) { \
        if (TYPEOF(r_vec) != REALSXP) { \
            error(ERRNAME ": expected numeric vector"); \
        } \
        if (MAYBE_SHARED(r_vec)) { \
            r_vec = PROTECT(duplicate(r_vec)); \
        } else { \
            PROTECT(r_vec); \
        } \
        double* ptr = REAL(r_vec); \
        int n = LENGTH(r_vec); \
        CFN(ptr, n); \
        UNPROTECT(1); \
        return r_vec; \
    }

#define MOJOR_REAL_VEC_SCALAR_INT2_CALL(RFN, CFN, ERRNAME) \
    SEXP RFN(SEXP r_vec, SEXP r_scalar) { \
        if (TYPEOF(r_vec) != REALSXP) { \
            error(ERRNAME ": expected numeric vector"); \
        } \
        if (TYPEOF(r_scalar) != REALSXP || LENGTH(r_scalar) != 1) { \
            error(ERRNAME ": expected numeric threshold"); \
        } \
        PROTECT(r_vec); \
        PROTECT(r_scalar); \
        double* in_ptr = REAL(r_vec); \
        int n = LENGTH(r_vec); \
        double scalar = REAL(r_scalar)[0]; \
        SEXP result = PROTECT(allocVector(INTSXP, 2)); \
        int* out_ptr = INTEGER(result); \
        CFN(in_ptr, n, scalar, out_ptr); \
        UNPROTECT(3); \
        return result; \
    }

// R wrapper: scale
MOJOR_REAL_UNARY_SCALAR_CALL(mojor_scale_f64_call, mojor_scale_f64, "mojor_scale_f64")
MOJOR_REAL_UNARY_INPLACE_CALL(mojor_abs_f64_inplace_call, mojor_abs_f64, "mojor_abs_f64_inplace")
MOJOR_REAL_UNARY_OUT_CALL(mojor_running_max_f64_call, mojor_running_max_f64, "mojor_running_max_f64")
MOJOR_REAL_VEC_SCALAR_INT2_CALL(mojor_count_runs_f64_call, mojor_count_runs_f64, "mojor_count_runs_f64")

// R wrapper: sigmoid (returns new vector)
MOJOR_REAL_UNARY_OUT_CALL(mojor_sigmoid_f64_call, mojor_sigmoid_f64, "mojor_sigmoid_f64")

// R wrapper: GPU context
SEXP mojor_gpu_ctx_create_call() {
    void* ctx = mojor_gpu_ctx_create();
    if (!ctx) {
        return R_NilValue;
    }
    SEXP ext = PROTECT(R_MakeExternalPtr(ctx, install("mojor_gpu_ctx"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_ctx_finalizer, TRUE);
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_ctx_free_call(SEXP r_ctx) {
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        return ScalarInteger(0);
    }
    int ok = mojor_gpu_ctx_free(ctx);
    R_ClearExternalPtr(r_ctx);
    return ScalarInteger(ok);
}

SEXP mojor_gpu_meminfo_call(SEXP r_ctx) {
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    uint64_t mem[2] = {0, 0};
    int status = -1;
    if (ctx) {
        status = mojor_gpu_meminfo(ctx, mem);
    }
    SEXP out = PROTECT(allocVector(REALSXP, 2));
    REAL(out)[0] = (status > 0) ? (double)mem[0] : NA_REAL;
    REAL(out)[1] = (status > 0) ? (double)mem[1] : NA_REAL;
    SEXP names = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(names, 0, mkChar("free_bytes"));
    SET_STRING_ELT(names, 1, mkChar("total_bytes"));
    setAttrib(out, R_NamesSymbol, names);
    setAttrib(out, install("status"), ScalarInteger(status));
    UNPROTECT(2);
    return out;
}

// R wrapper: sigmoid GPU (returns new vector, attr gpu_status)
SEXP mojor_sigmoid_f64_gpu_call(SEXP r_ctx, SEXP r_vec) {
    if (TYPEOF(r_vec) != REALSXP) {
        error("mojor_sigmoid_f64_gpu: expected numeric vector");
    }

    void* ctx = mojor_gpu_ctx_get(r_ctx);
    PROTECT(r_vec);

    double* in_ptr = REAL(r_vec);
    int n = LENGTH(r_vec);

    SEXP result = PROTECT(allocVector(REALSXP, n));
    double* out_ptr = REAL(result);

    int status = ctx ? mojor_sigmoid_f64_gpu(ctx, in_ptr, out_ptr, n) : -1;
    setAttrib(result, install("gpu_status"), ScalarInteger(status));

    UNPROTECT(2);
    return result;
}

// R wrapper: sigmoid GPU (float32 raw in/out)
SEXP mojor_sigmoid_f32_gpu_call(SEXP r_ctx, SEXP r_raw) {
    if (TYPEOF(r_raw) != RAWSXP) {
        error("mojor_sigmoid_f32_gpu: expected raw vector");
    }

    R_xlen_t n_bytes = XLENGTH(r_raw);
    if (n_bytes % 4 != 0) {
        error("mojor_sigmoid_f32_gpu: raw length must be multiple of 4");
    }

    int n = (int)(n_bytes / 4);
    PROTECT(r_raw);

    SEXP result = PROTECT(allocVector(RAWSXP, n_bytes));

    float* in_ptr = (float*) RAW(r_raw);
    float* out_ptr = (float*) RAW(result);

    void* ctx = mojor_gpu_ctx_get(r_ctx);
    int status = ctx ? mojor_sigmoid_f32_gpu(ctx, in_ptr, out_ptr, n) : -1;
    setAttrib(result, install("gpu_status"), ScalarInteger(status));

    UNPROTECT(2);
    return result;
}

// R wrapper: sigmoid GPU (float package Data slot, INT bits)
SEXP mojor_sigmoid_f32_gpu_int_call(SEXP r_ctx, SEXP r_int) {
    if (TYPEOF(r_int) != INTSXP) {
        error("mojor_sigmoid_f32_gpu_int: expected integer vector");
    }

    int n = LENGTH(r_int);
    PROTECT(r_int);

    SEXP result = PROTECT(allocVector(INTSXP, n));

    float* in_buf = (float*) R_alloc((size_t) n, sizeof(float));
    float* out_buf = (float*) R_alloc((size_t) n, sizeof(float));

    memcpy(in_buf, INTEGER(r_int), (size_t) n * sizeof(float));

    void* ctx = mojor_gpu_ctx_get(r_ctx);
    int status = ctx ? mojor_sigmoid_f32_gpu(ctx, in_buf, out_buf, n) : -1;
    memcpy(INTEGER(result), out_buf, (size_t) n * sizeof(float));

    setAttrib(result, install("gpu_status"), ScalarInteger(status));

    UNPROTECT(2);
    return result;
}

// R wrapper: has GPU
SEXP mojor_has_gpu_call() {
    int has = mojor_has_gpu();
    return ScalarLogical(has != 0);
}

SEXP mojor_gpu_cap_f64_matmul_call(SEXP r_ctx) {
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        return ScalarLogical(0);
    }
    int ok = mojor_gpu_cap_f64_matmul(ctx);
    return ScalarLogical(ok != 0);
}

SEXP mojor_gpu_cap_f64_reduce_call(SEXP r_ctx, SEXP r_op) {
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        return ScalarLogical(0);
    }
    int op = (TYPEOF(r_op) == INTSXP) ? INTEGER(r_op)[0] : (int) REAL(r_op)[0];
    int ok = mojor_gpu_cap_f64_reduce(ctx, op);
    return ScalarLogical(ok != 0);
}

SEXP mojor_gpu_cap_i32_scatter_call(SEXP r_ctx) {
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        return ScalarLogical(0);
    }
    int ok = mojor_gpu_cap_i32_scatter(ctx);
    return ScalarLogical(ok != 0);
}

// GPU sigmoid with iterations (float32 raw in/out)
SEXP mojor_sigmoid_f32_gpu_iters_int_call(SEXP r_ctx, SEXP r_int, SEXP r_iters) {
    if (TYPEOF(r_int) != INTSXP) {
        error("mojor_sigmoid_f32_gpu_iters: expected integer vector (float bits)");
    }
    int iters = 1;
    if (TYPEOF(r_iters) == INTSXP && LENGTH(r_iters) == 1) {
        iters = INTEGER(r_iters)[0];
    } else if (TYPEOF(r_iters) == REALSXP && LENGTH(r_iters) == 1) {
        iters = (int) REAL(r_iters)[0];
    }
    int n = LENGTH(r_int);
    PROTECT(r_int);
    SEXP result = PROTECT(allocVector(INTSXP, n));
    float* in_ptr = (float*) INTEGER(r_int);
    float* out_ptr = (float*) INTEGER(result);
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    int status = ctx ? mojor_sigmoid_f32_gpu_iters(ctx, in_ptr, out_ptr, n, iters) : -1;
    setAttrib(result, install("gpu_status"), ScalarInteger(status));
    UNPROTECT(2);
    return result;
}

// GPU sigmoid affine (float package Data slot, INT bits)
SEXP mojor_sigmoid_affine_f32_gpu_int_call(SEXP r_ctx, SEXP r_int, SEXP r_scale, SEXP r_bias) {
    if (TYPEOF(r_int) != INTSXP) {
        error("mojor_sigmoid_affine_f32_gpu_int: expected integer vector");
    }
    float scale = 1.0f;
    float bias = 0.0f;
    if (TYPEOF(r_scale) == REALSXP && LENGTH(r_scale) == 1) {
        scale = (float) REAL(r_scale)[0];
    } else if (TYPEOF(r_scale) == INTSXP && LENGTH(r_scale) == 1) {
        scale = (float) INTEGER(r_scale)[0];
    }
    if (TYPEOF(r_bias) == REALSXP && LENGTH(r_bias) == 1) {
        bias = (float) REAL(r_bias)[0];
    } else if (TYPEOF(r_bias) == INTSXP && LENGTH(r_bias) == 1) {
        bias = (float) INTEGER(r_bias)[0];
    }
    int n = LENGTH(r_int);
    PROTECT(r_int);
    SEXP result = PROTECT(allocVector(INTSXP, n));
    float* in_ptr = (float*) INTEGER(r_int);
    float* out_ptr = (float*) INTEGER(result);
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    int status = ctx ? mojor_sigmoid_affine_f32_gpu(ctx, in_ptr, out_ptr, n, scale, bias) : -1;
    setAttrib(result, install("gpu_status"), ScalarInteger(status));
    UNPROTECT(2);
    return result;
}

// GPU sigmoid affine with iterations (float package Data slot, INT bits)
SEXP mojor_sigmoid_affine_f32_gpu_iters_int_call(
    SEXP r_ctx,
    SEXP r_int,
    SEXP r_iters,
    SEXP r_scale,
    SEXP r_bias
) {
    if (TYPEOF(r_int) != INTSXP) {
        error("mojor_sigmoid_affine_f32_gpu_iters: expected integer vector (float bits)");
    }
    int iters = 1;
    if (TYPEOF(r_iters) == INTSXP && LENGTH(r_iters) == 1) {
        iters = INTEGER(r_iters)[0];
    } else if (TYPEOF(r_iters) == REALSXP && LENGTH(r_iters) == 1) {
        iters = (int) REAL(r_iters)[0];
    }
    float scale = 1.0f;
    float bias = 0.0f;
    if (TYPEOF(r_scale) == REALSXP && LENGTH(r_scale) == 1) {
        scale = (float) REAL(r_scale)[0];
    } else if (TYPEOF(r_scale) == INTSXP && LENGTH(r_scale) == 1) {
        scale = (float) INTEGER(r_scale)[0];
    }
    if (TYPEOF(r_bias) == REALSXP && LENGTH(r_bias) == 1) {
        bias = (float) REAL(r_bias)[0];
    } else if (TYPEOF(r_bias) == INTSXP && LENGTH(r_bias) == 1) {
        bias = (float) INTEGER(r_bias)[0];
    }
    int n = LENGTH(r_int);
    PROTECT(r_int);
    SEXP result = PROTECT(allocVector(INTSXP, n));
    float* in_ptr = (float*) INTEGER(r_int);
    float* out_ptr = (float*) INTEGER(result);
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    int status = ctx ? mojor_sigmoid_affine_f32_gpu_iters(ctx, in_ptr, out_ptr, n, iters, scale, bias) : -1;
    setAttrib(result, install("gpu_status"), ScalarInteger(status));
    UNPROTECT(2);
    return result;
}

// GPU sigmoid affine chain (iters + post-affine), float package Data slot (INT bits)
SEXP mojor_sigmoid_affine_f32_gpu_chain_int_call(
    SEXP r_ctx,
    SEXP r_int,
    SEXP r_iters,
    SEXP r_scale,
    SEXP r_bias,
    SEXP r_post_scale,
    SEXP r_post_bias,
    SEXP r_post_iters
) {
    if (TYPEOF(r_int) != INTSXP) {
        error("mojor_sigmoid_affine_f32_gpu_chain: expected integer vector (float bits)");
    }
    int iters = 1;
    if (TYPEOF(r_iters) == INTSXP && LENGTH(r_iters) == 1) {
        iters = INTEGER(r_iters)[0];
    } else if (TYPEOF(r_iters) == REALSXP && LENGTH(r_iters) == 1) {
        iters = (int) REAL(r_iters)[0];
    }
    int post_iters = 0;
    if (TYPEOF(r_post_iters) == INTSXP && LENGTH(r_post_iters) == 1) {
        post_iters = INTEGER(r_post_iters)[0];
    } else if (TYPEOF(r_post_iters) == REALSXP && LENGTH(r_post_iters) == 1) {
        post_iters = (int) REAL(r_post_iters)[0];
    }
    float scale = 1.0f;
    float bias = 0.0f;
    float post_scale = 1.0f;
    float post_bias = 0.0f;
    if (TYPEOF(r_scale) == REALSXP && LENGTH(r_scale) == 1) {
        scale = (float) REAL(r_scale)[0];
    } else if (TYPEOF(r_scale) == INTSXP && LENGTH(r_scale) == 1) {
        scale = (float) INTEGER(r_scale)[0];
    }
    if (TYPEOF(r_bias) == REALSXP && LENGTH(r_bias) == 1) {
        bias = (float) REAL(r_bias)[0];
    } else if (TYPEOF(r_bias) == INTSXP && LENGTH(r_bias) == 1) {
        bias = (float) INTEGER(r_bias)[0];
    }
    if (TYPEOF(r_post_scale) == REALSXP && LENGTH(r_post_scale) == 1) {
        post_scale = (float) REAL(r_post_scale)[0];
    } else if (TYPEOF(r_post_scale) == INTSXP && LENGTH(r_post_scale) == 1) {
        post_scale = (float) INTEGER(r_post_scale)[0];
    }
    if (TYPEOF(r_post_bias) == REALSXP && LENGTH(r_post_bias) == 1) {
        post_bias = (float) REAL(r_post_bias)[0];
    } else if (TYPEOF(r_post_bias) == INTSXP && LENGTH(r_post_bias) == 1) {
        post_bias = (float) INTEGER(r_post_bias)[0];
    }
    int n = LENGTH(r_int);
    PROTECT(r_int);
    SEXP result = PROTECT(allocVector(INTSXP, n));
    float* in_ptr = (float*) INTEGER(r_int);
    float* out_ptr = (float*) INTEGER(result);
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    int status = ctx ? mojor_sigmoid_affine_f32_gpu_chain(
        ctx,
        in_ptr,
        out_ptr,
        n,
        iters,
        scale,
        bias,
        post_scale,
        post_bias,
        post_iters
    ) : -1;
    setAttrib(result, install("gpu_status"), ScalarInteger(status));
    UNPROTECT(2);
    return result;
}

// GPU sigmoid affine chain + reduction (float package Data slot, INT bits)
SEXP mojor_sigmoid_affine_f32_gpu_chain_sum_int_call(
    SEXP r_ctx,
    SEXP r_int,
    SEXP r_iters,
    SEXP r_scale,
    SEXP r_bias,
    SEXP r_post_scale,
    SEXP r_post_bias,
    SEXP r_post_iters
) {
    if (TYPEOF(r_int) != INTSXP) {
        error("mojor_sigmoid_affine_f32_gpu_chain_sum: expected integer vector (float bits)");
    }
    int iters = 1;
    if (TYPEOF(r_iters) == INTSXP && LENGTH(r_iters) == 1) {
        iters = INTEGER(r_iters)[0];
    } else if (TYPEOF(r_iters) == REALSXP && LENGTH(r_iters) == 1) {
        iters = (int) REAL(r_iters)[0];
    }
    int post_iters = 0;
    if (TYPEOF(r_post_iters) == INTSXP && LENGTH(r_post_iters) == 1) {
        post_iters = INTEGER(r_post_iters)[0];
    } else if (TYPEOF(r_post_iters) == REALSXP && LENGTH(r_post_iters) == 1) {
        post_iters = (int) REAL(r_post_iters)[0];
    }
    float scale = 1.0f;
    float bias = 0.0f;
    float post_scale = 1.0f;
    float post_bias = 0.0f;
    if (TYPEOF(r_scale) == REALSXP && LENGTH(r_scale) == 1) {
        scale = (float) REAL(r_scale)[0];
    } else if (TYPEOF(r_scale) == INTSXP && LENGTH(r_scale) == 1) {
        scale = (float) INTEGER(r_scale)[0];
    }
    if (TYPEOF(r_bias) == REALSXP && LENGTH(r_bias) == 1) {
        bias = (float) REAL(r_bias)[0];
    } else if (TYPEOF(r_bias) == INTSXP && LENGTH(r_bias) == 1) {
        bias = (float) INTEGER(r_bias)[0];
    }
    if (TYPEOF(r_post_scale) == REALSXP && LENGTH(r_post_scale) == 1) {
        post_scale = (float) REAL(r_post_scale)[0];
    } else if (TYPEOF(r_post_scale) == INTSXP && LENGTH(r_post_scale) == 1) {
        post_scale = (float) INTEGER(r_post_scale)[0];
    }
    if (TYPEOF(r_post_bias) == REALSXP && LENGTH(r_post_bias) == 1) {
        post_bias = (float) REAL(r_post_bias)[0];
    } else if (TYPEOF(r_post_bias) == INTSXP && LENGTH(r_post_bias) == 1) {
        post_bias = (float) INTEGER(r_post_bias)[0];
    }
    int n = LENGTH(r_int);
    PROTECT(r_int);
    float* in_ptr = (float*) INTEGER(r_int);
    int status = 0;
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    float result = ctx ? mojor_sigmoid_affine_f32_gpu_chain_sum(
        ctx,
        in_ptr,
        n,
        iters,
        scale,
        bias,
        post_scale,
        post_bias,
        post_iters,
        &status
    ) : 0.0f;
    SEXP out = PROTECT(ScalarReal((double) result));
    setAttrib(out, install("gpu_status"), ScalarInteger(ctx ? status : -1));
    UNPROTECT(2);
    return out;
}

// GPU sigmoid affine chain + reduction (warp reduction, float package Data slot, INT bits)
SEXP mojor_sigmoid_affine_f32_gpu_chain_sum_warp_int_call(
    SEXP r_ctx,
    SEXP r_int,
    SEXP r_iters,
    SEXP r_scale,
    SEXP r_bias,
    SEXP r_post_scale,
    SEXP r_post_bias,
    SEXP r_post_iters
) {
    if (TYPEOF(r_int) != INTSXP) {
        error("mojor_sigmoid_affine_f32_gpu_chain_sum_warp: expected integer vector (float bits)");
    }
    int iters = 1;
    if (TYPEOF(r_iters) == INTSXP && LENGTH(r_iters) == 1) {
        iters = INTEGER(r_iters)[0];
    } else if (TYPEOF(r_iters) == REALSXP && LENGTH(r_iters) == 1) {
        iters = (int) REAL(r_iters)[0];
    }
    int post_iters = 0;
    if (TYPEOF(r_post_iters) == INTSXP && LENGTH(r_post_iters) == 1) {
        post_iters = INTEGER(r_post_iters)[0];
    } else if (TYPEOF(r_post_iters) == REALSXP && LENGTH(r_post_iters) == 1) {
        post_iters = (int) REAL(r_post_iters)[0];
    }
    float scale = 1.0f;
    float bias = 0.0f;
    float post_scale = 1.0f;
    float post_bias = 0.0f;
    if (TYPEOF(r_scale) == REALSXP && LENGTH(r_scale) == 1) {
        scale = (float) REAL(r_scale)[0];
    } else if (TYPEOF(r_scale) == INTSXP && LENGTH(r_scale) == 1) {
        scale = (float) INTEGER(r_scale)[0];
    }
    if (TYPEOF(r_bias) == REALSXP && LENGTH(r_bias) == 1) {
        bias = (float) REAL(r_bias)[0];
    } else if (TYPEOF(r_bias) == INTSXP && LENGTH(r_bias) == 1) {
        bias = (float) INTEGER(r_bias)[0];
    }
    if (TYPEOF(r_post_scale) == REALSXP && LENGTH(r_post_scale) == 1) {
        post_scale = (float) REAL(r_post_scale)[0];
    } else if (TYPEOF(r_post_scale) == INTSXP && LENGTH(r_post_scale) == 1) {
        post_scale = (float) INTEGER(r_post_scale)[0];
    }
    if (TYPEOF(r_post_bias) == REALSXP && LENGTH(r_post_bias) == 1) {
        post_bias = (float) REAL(r_post_bias)[0];
    } else if (TYPEOF(r_post_bias) == INTSXP && LENGTH(r_post_bias) == 1) {
        post_bias = (float) INTEGER(r_post_bias)[0];
    }
    int n = LENGTH(r_int);
    PROTECT(r_int);
    float* in_ptr = (float*) INTEGER(r_int);
    int status = 0;
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    float result = ctx ? mojor_sigmoid_affine_f32_gpu_chain_sum_warp(
        ctx,
        in_ptr,
        n,
        iters,
        scale,
        bias,
        post_scale,
        post_bias,
        post_iters,
        &status
    ) : 0.0f;
    SEXP out = PROTECT(ScalarReal((double) result));
    setAttrib(out, install("gpu_status"), ScalarInteger(ctx ? status : -1));
    UNPROTECT(2);
    return out;
}

SEXP mojor_gpu_buf_f32_alloc_call(SEXP r_ctx, SEXP r_n) {
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f32_alloc: GPU context unavailable");
    }
    int n = (TYPEOF(r_n) == INTSXP) ? INTEGER(r_n)[0] : (int) REAL(r_n)[0];
    if (n < 0) {
        error("mojor_gpu_buf_f32_alloc: n must be >= 0");
    }
    void* handle = mojor_gpu_buf_f32_alloc(ctx, n);
    if (!handle) {
        error("mojor_gpu_buf_f32_alloc: allocation failed");
    }
    mojor_gpu_handle_add(handle);
    mojor_gpu_buf_f32_t* buf = R_Calloc(1, mojor_gpu_buf_f32_t);
    buf->ctx = ctx;
    buf->handle = handle;
    buf->n = n;
    SEXP ext = PROTECT(R_MakeExternalPtr(buf, install("mojor_gpu_buf_f32"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_buf_f32_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(n));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_buf_f32_free_call(SEXP r_buf) {
    mojor_gpu_buf_f32_t* buf = mojor_gpu_buf_f32_get(r_buf);
    if (!buf) {
        return ScalarLogical(0);
    }
    int ok = 0;
    if (buf->handle) {
        ok = mojor_gpu_buf_f32_free(buf->ctx, buf->handle);
        if (ok) {
            mojor_gpu_handle_remove(buf->handle);
            buf->handle = NULL;
        }
    }
    buf->ctx = NULL;
    R_ClearExternalPtr(r_buf);
    return ScalarLogical(ok != 0);
}

SEXP mojor_gpu_buf_f32_len_call(SEXP r_buf) {
    mojor_gpu_buf_f32_t* buf = mojor_gpu_buf_f32_get(r_buf);
    if (!buf || !buf->handle) {
        return ScalarInteger(-1);
    }
    return ScalarInteger(buf->n);
}

SEXP mojor_gpu_buf_f32_write_call(SEXP r_buf, SEXP r_vec) {
    mojor_gpu_buf_f32_t* buf = mojor_gpu_buf_f32_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_f32_write: invalid buffer");
    }
    if (TYPEOF(r_vec) != REALSXP && TYPEOF(r_vec) != INTSXP) {
        error("mojor_gpu_buf_f32_write: expected numeric or integer vector");
    }
    int n = LENGTH(r_vec);
    if (n != buf->n) {
        error("mojor_gpu_buf_f32_write: length mismatch");
    }
    float* tmp = (float*) R_alloc((size_t) n, sizeof(float));
    if (TYPEOF(r_vec) == REALSXP) {
        double* src = REAL(r_vec);
        for (int i = 0; i < n; i++) {
            tmp[i] = (float) src[i];
        }
    } else {
        int* src = INTEGER(r_vec);
        for (int i = 0; i < n; i++) {
            tmp[i] = (float) src[i];
        }
    }
    int ok = mojor_gpu_buf_f32_write(buf->ctx, buf->handle, tmp, n);
    if (!ok) {
        error("mojor_gpu_buf_f32_write: write failed");
    }
    return ScalarLogical(1);
}

SEXP mojor_gpu_buf_f32_read_call(SEXP r_buf) {
    mojor_gpu_buf_f32_t* buf = mojor_gpu_buf_f32_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_f32_read: invalid buffer");
    }
    int n = buf->n;
    SEXP out = PROTECT(allocVector(REALSXP, n));
    float* tmp = (float*) R_alloc((size_t) n, sizeof(float));
    int ok = mojor_gpu_buf_f32_read(buf->ctx, buf->handle, tmp, n);
    if (!ok) {
        UNPROTECT(1);
        error("mojor_gpu_buf_f32_read: read failed");
    }
    double* dst = REAL(out);
    for (int i = 0; i < n; i++) {
        dst[i] = (double) tmp[i];
    }
    UNPROTECT(1);
    return out;
}

SEXP mojor_gpu_buf_f32_cast_f64_call(SEXP r_ctx, SEXP r_buf) {
    mojor_gpu_buf_f32_t* buf = mojor_gpu_buf_f32_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_f32_cast_f64: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f32_cast_f64: GPU context unavailable");
    }
    void* handle = mojor_gpu_buf_f32_cast_f64(ctx, buf->handle);
    if (!handle) {
        error("mojor_gpu_buf_f32_cast_f64: failed");
    }
    mojor_gpu_handle_add(handle);
    mojor_gpu_buf_f64_t* out = R_Calloc(1, mojor_gpu_buf_f64_t);
    out->ctx = ctx;
    out->handle = handle;
    out->n = mojor_gpu_buf_f64_len(ctx, handle);
    SEXP ext = PROTECT(R_MakeExternalPtr(out, install("mojor_gpu_buf_f64"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_buf_f64_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(out->n));
    setAttrib(ext, install("gpu_status"), ScalarInteger(1));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_buf_f32_cast_i32_call(SEXP r_ctx, SEXP r_buf) {
    mojor_gpu_buf_f32_t* buf = mojor_gpu_buf_f32_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_f32_cast_i32: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f32_cast_i32: GPU context unavailable");
    }
    void* handle = mojor_gpu_buf_f32_cast_i32(ctx, buf->handle);
    if (!handle) {
        error("mojor_gpu_buf_f32_cast_i32: failed");
    }
    mojor_gpu_handle_add(handle);
    mojor_gpu_buf_i32_t* out = R_Calloc(1, mojor_gpu_buf_i32_t);
    out->ctx = ctx;
    out->handle = handle;
    out->n = mojor_gpu_buf_i32_len(ctx, handle);
    SEXP ext = PROTECT(R_MakeExternalPtr(out, install("mojor_gpu_buf_i32"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_buf_i32_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(out->n));
    setAttrib(ext, install("gpu_status"), ScalarInteger(1));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_buf_f32_affine_call(
    SEXP r_ctx,
    SEXP r_buf,
    SEXP r_scale,
    SEXP r_bias
) {
    mojor_gpu_buf_f32_t* buf = mojor_gpu_buf_f32_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_f32_affine: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f32_affine: GPU context unavailable");
    }
    float scale = (TYPEOF(r_scale) == REALSXP) ? (float) REAL(r_scale)[0] : (float) INTEGER(r_scale)[0];
    float bias = (TYPEOF(r_bias) == REALSXP) ? (float) REAL(r_bias)[0] : (float) INTEGER(r_bias)[0];
    int status = 0;
    void* handle = mojor_gpu_buf_f32_affine(ctx, buf->handle, scale, bias, &status);
    if (!handle) {
        error("mojor_gpu_buf_f32_affine: failed (status=%d)", status);
    }
    mojor_gpu_handle_add(handle);
    mojor_gpu_buf_f32_t* out = R_Calloc(1, mojor_gpu_buf_f32_t);
    out->ctx = ctx;
    out->handle = handle;
    out->n = buf->n;
    SEXP ext = PROTECT(R_MakeExternalPtr(out, install("mojor_gpu_buf_f32"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_buf_f32_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(out->n));
    setAttrib(ext, install("gpu_status"), ScalarInteger(status));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_buf_f32_chain_call(
    SEXP r_ctx,
    SEXP r_buf,
    SEXP r_iters,
    SEXP r_scale,
    SEXP r_bias,
    SEXP r_post_scale,
    SEXP r_post_bias,
    SEXP r_post_iters
) {
    mojor_gpu_buf_f32_t* buf = mojor_gpu_buf_f32_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_f32_chain: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f32_chain: GPU context unavailable");
    }
    int iters = (TYPEOF(r_iters) == INTSXP) ? INTEGER(r_iters)[0] : (int) REAL(r_iters)[0];
    int post_iters = (TYPEOF(r_post_iters) == INTSXP) ? INTEGER(r_post_iters)[0] : (int) REAL(r_post_iters)[0];
    float scale = (TYPEOF(r_scale) == REALSXP) ? (float) REAL(r_scale)[0] : (float) INTEGER(r_scale)[0];
    float bias = (TYPEOF(r_bias) == REALSXP) ? (float) REAL(r_bias)[0] : (float) INTEGER(r_bias)[0];
    float post_scale = (TYPEOF(r_post_scale) == REALSXP) ? (float) REAL(r_post_scale)[0] : (float) INTEGER(r_post_scale)[0];
    float post_bias = (TYPEOF(r_post_bias) == REALSXP) ? (float) REAL(r_post_bias)[0] : (float) INTEGER(r_post_bias)[0];
    int status = 0;
    void* handle = mojor_gpu_buf_f32_chain(
        ctx,
        buf->handle,
        iters,
        scale,
        bias,
        post_scale,
        post_bias,
        post_iters,
        &status
    );
    if (!handle) {
        error("mojor_gpu_buf_f32_chain: failed (status=%d)", status);
    }
    mojor_gpu_handle_add(handle);
    mojor_gpu_buf_f32_t* out = R_Calloc(1, mojor_gpu_buf_f32_t);
    out->ctx = ctx;
    out->handle = handle;
    out->n = buf->n;
    SEXP ext = PROTECT(R_MakeExternalPtr(out, install("mojor_gpu_buf_f32"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_buf_f32_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(out->n));
    setAttrib(ext, install("gpu_status"), ScalarInteger(status));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_buf_f32_chain_sum_call(
    SEXP r_ctx,
    SEXP r_buf,
    SEXP r_iters,
    SEXP r_scale,
    SEXP r_bias,
    SEXP r_post_scale,
    SEXP r_post_bias,
    SEXP r_post_iters
) {
    mojor_gpu_buf_f32_t* buf = mojor_gpu_buf_f32_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_f32_chain_sum: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f32_chain_sum: GPU context unavailable");
    }
    int iters = (TYPEOF(r_iters) == INTSXP) ? INTEGER(r_iters)[0] : (int) REAL(r_iters)[0];
    int post_iters = (TYPEOF(r_post_iters) == INTSXP) ? INTEGER(r_post_iters)[0] : (int) REAL(r_post_iters)[0];
    float scale = (TYPEOF(r_scale) == REALSXP) ? (float) REAL(r_scale)[0] : (float) INTEGER(r_scale)[0];
    float bias = (TYPEOF(r_bias) == REALSXP) ? (float) REAL(r_bias)[0] : (float) INTEGER(r_bias)[0];
    float post_scale = (TYPEOF(r_post_scale) == REALSXP) ? (float) REAL(r_post_scale)[0] : (float) INTEGER(r_post_scale)[0];
    float post_bias = (TYPEOF(r_post_bias) == REALSXP) ? (float) REAL(r_post_bias)[0] : (float) INTEGER(r_post_bias)[0];
    int status = 0;
    float result = mojor_gpu_buf_f32_chain_sum(
        ctx,
        buf->handle,
        iters,
        scale,
        bias,
        post_scale,
        post_bias,
        post_iters,
        &status
    );
    SEXP out = PROTECT(ScalarReal((double) result));
    setAttrib(out, install("gpu_status"), ScalarInteger(status));
    UNPROTECT(1);
    return out;
}

SEXP mojor_gpu_buf_f32_chain_sum_warp_call(
    SEXP r_ctx,
    SEXP r_buf,
    SEXP r_iters,
    SEXP r_scale,
    SEXP r_bias,
    SEXP r_post_scale,
    SEXP r_post_bias,
    SEXP r_post_iters
) {
    mojor_gpu_buf_f32_t* buf = mojor_gpu_buf_f32_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_f32_chain_sum_warp: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f32_chain_sum_warp: GPU context unavailable");
    }
    int iters = (TYPEOF(r_iters) == INTSXP) ? INTEGER(r_iters)[0] : (int) REAL(r_iters)[0];
    int post_iters = (TYPEOF(r_post_iters) == INTSXP) ? INTEGER(r_post_iters)[0] : (int) REAL(r_post_iters)[0];
    float scale = (TYPEOF(r_scale) == REALSXP) ? (float) REAL(r_scale)[0] : (float) INTEGER(r_scale)[0];
    float bias = (TYPEOF(r_bias) == REALSXP) ? (float) REAL(r_bias)[0] : (float) INTEGER(r_bias)[0];
    float post_scale = (TYPEOF(r_post_scale) == REALSXP) ? (float) REAL(r_post_scale)[0] : (float) INTEGER(r_post_scale)[0];
    float post_bias = (TYPEOF(r_post_bias) == REALSXP) ? (float) REAL(r_post_bias)[0] : (float) INTEGER(r_post_bias)[0];
    int status = 0;
    float result = mojor_gpu_buf_f32_chain_sum_warp(
        ctx,
        buf->handle,
        iters,
        scale,
        bias,
        post_scale,
        post_bias,
        post_iters,
        &status
    );
    SEXP out = PROTECT(ScalarReal((double) result));
    setAttrib(out, install("gpu_status"), ScalarInteger(status));
    UNPROTECT(1);
    return out;
}

SEXP mojor_gpu_buf_f32_reduce_call(
    SEXP r_ctx,
    SEXP r_buf,
    SEXP r_op,
    SEXP r_dims,
    SEXP r_ndim,
    SEXP r_keepdims
) {
    mojor_gpu_buf_f32_t* buf = mojor_gpu_buf_f32_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_f32_reduce: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f32_reduce: GPU context unavailable");
    }
    int op = (TYPEOF(r_op) == INTSXP) ? INTEGER(r_op)[0] : (int) REAL(r_op)[0];
    int ndim = (TYPEOF(r_ndim) == INTSXP) ? INTEGER(r_ndim)[0] : (int) REAL(r_ndim)[0];
    int keepdims = (TYPEOF(r_keepdims) == INTSXP) ? INTEGER(r_keepdims)[0] : (int) REAL(r_keepdims)[0];
    const int* dims = NULL;
    if (ndim > 0 && r_dims != R_NilValue) {
        if (TYPEOF(r_dims) != INTSXP && TYPEOF(r_dims) != REALSXP) {
            error("mojor_gpu_buf_f32_reduce: dims must be integer or numeric");
        }
        int n = LENGTH(r_dims);
        if (n < ndim) ndim = n;
        int* tmp = (int*) R_alloc((size_t) ndim, sizeof(int));
        if (TYPEOF(r_dims) == INTSXP) {
            int* src = INTEGER(r_dims);
            for (int i = 0; i < ndim; i++) tmp[i] = src[i];
        } else {
            double* src = REAL(r_dims);
            for (int i = 0; i < ndim; i++) tmp[i] = (int) src[i];
        }
        dims = tmp;
    }
    void* handle = mojor_gpu_buf_f32_reduce(ctx, buf->handle, op, dims, ndim, keepdims);
    if (!handle) {
        error("mojor_gpu_buf_f32_reduce: failed");
    }
    mojor_gpu_handle_add(handle);
    mojor_gpu_buf_f32_t* out = R_Calloc(1, mojor_gpu_buf_f32_t);
    out->ctx = ctx;
    out->handle = handle;
    out->n = mojor_gpu_buf_f32_len(ctx, handle);
    SEXP ext = PROTECT(R_MakeExternalPtr(out, install("mojor_gpu_buf_f32"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_buf_f32_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(out->n));
    setAttrib(ext, install("gpu_status"), ScalarInteger(1));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_buf_f32_matmul_call(
    SEXP r_ctx,
    SEXP r_a,
    SEXP r_b,
    SEXP r_m,
    SEXP r_k,
    SEXP r_n,
    SEXP r_transpose_a,
    SEXP r_transpose_b
) {
    mojor_gpu_buf_f32_t* buf_a = mojor_gpu_buf_f32_get(r_a);
    mojor_gpu_buf_f32_t* buf_b = mojor_gpu_buf_f32_get(r_b);
    if (!buf_a || !buf_a->handle || !buf_b || !buf_b->handle) {
        error("mojor_gpu_buf_f32_matmul: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f32_matmul: GPU context unavailable");
    }
    int m = (TYPEOF(r_m) == INTSXP) ? INTEGER(r_m)[0] : (int) REAL(r_m)[0];
    int k = (TYPEOF(r_k) == INTSXP) ? INTEGER(r_k)[0] : (int) REAL(r_k)[0];
    int n = (TYPEOF(r_n) == INTSXP) ? INTEGER(r_n)[0] : (int) REAL(r_n)[0];
    int transpose_a = (TYPEOF(r_transpose_a) == INTSXP) ? INTEGER(r_transpose_a)[0] : (int) REAL(r_transpose_a)[0];
    int transpose_b = (TYPEOF(r_transpose_b) == INTSXP) ? INTEGER(r_transpose_b)[0] : (int) REAL(r_transpose_b)[0];
    void* handle = mojor_gpu_buf_f32_matmul(ctx, buf_a->handle, buf_b->handle, m, k, n, transpose_a, transpose_b);
    if (!handle) {
        error("mojor_gpu_buf_f32_matmul: failed");
    }
    mojor_gpu_handle_add(handle);
    mojor_gpu_buf_f32_t* out = R_Calloc(1, mojor_gpu_buf_f32_t);
    out->ctx = ctx;
    out->handle = handle;
    out->n = mojor_gpu_buf_f32_len(ctx, handle);
    SEXP ext = PROTECT(R_MakeExternalPtr(out, install("mojor_gpu_buf_f32"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_buf_f32_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(out->n));
    setAttrib(ext, install("gpu_status"), ScalarInteger(1));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_buf_f32_matmul_into_call(
    SEXP r_ctx,
    SEXP r_out,
    SEXP r_a,
    SEXP r_b,
    SEXP r_m,
    SEXP r_k,
    SEXP r_n,
    SEXP r_transpose_a,
    SEXP r_transpose_b
) {
    mojor_gpu_buf_f32_t* buf_out = mojor_gpu_buf_f32_get(r_out);
    mojor_gpu_buf_f32_t* buf_a = mojor_gpu_buf_f32_get(r_a);
    mojor_gpu_buf_f32_t* buf_b = mojor_gpu_buf_f32_get(r_b);
    if (!buf_out || !buf_out->handle || !buf_a || !buf_a->handle || !buf_b || !buf_b->handle) {
        error("mojor_gpu_buf_f32_matmul_into: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f32_matmul_into: GPU context unavailable");
    }
    int m = (TYPEOF(r_m) == INTSXP) ? INTEGER(r_m)[0] : (int) REAL(r_m)[0];
    int k = (TYPEOF(r_k) == INTSXP) ? INTEGER(r_k)[0] : (int) REAL(r_k)[0];
    int n = (TYPEOF(r_n) == INTSXP) ? INTEGER(r_n)[0] : (int) REAL(r_n)[0];
    int transpose_a = (TYPEOF(r_transpose_a) == INTSXP) ? INTEGER(r_transpose_a)[0] : (int) REAL(r_transpose_a)[0];
    int transpose_b = (TYPEOF(r_transpose_b) == INTSXP) ? INTEGER(r_transpose_b)[0] : (int) REAL(r_transpose_b)[0];
    int ok = mojor_gpu_buf_f32_matmul_into(ctx, buf_out->handle, buf_a->handle, buf_b->handle, m, k, n, transpose_a, transpose_b);
    if (!ok) {
        error("mojor_gpu_buf_f32_matmul_into: failed");
    }
    return ScalarLogical(1);
}

SEXP mojor_gpu_buf_i32_reduce_call(
    SEXP r_ctx,
    SEXP r_buf,
    SEXP r_op,
    SEXP r_dims,
    SEXP r_ndim,
    SEXP r_keepdims
) {
    mojor_gpu_buf_i32_t* buf = mojor_gpu_buf_i32_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_i32_reduce: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_i32_reduce: GPU context unavailable");
    }
    int op = (TYPEOF(r_op) == INTSXP) ? INTEGER(r_op)[0] : (int) REAL(r_op)[0];
    int ndim = (TYPEOF(r_ndim) == INTSXP) ? INTEGER(r_ndim)[0] : (int) REAL(r_ndim)[0];
    int keepdims = (TYPEOF(r_keepdims) == INTSXP) ? INTEGER(r_keepdims)[0] : (int) REAL(r_keepdims)[0];
    const int* dims = NULL;
    if (ndim > 0 && r_dims != R_NilValue) {
        if (TYPEOF(r_dims) != INTSXP && TYPEOF(r_dims) != REALSXP) {
            error("mojor_gpu_buf_i32_reduce: dims must be integer or numeric");
        }
        int n = LENGTH(r_dims);
        if (n < ndim) ndim = n;
        int* tmp = (int*) R_alloc((size_t) ndim, sizeof(int));
        if (TYPEOF(r_dims) == INTSXP) {
            int* src = INTEGER(r_dims);
            for (int i = 0; i < ndim; i++) tmp[i] = src[i];
        } else {
            double* src = REAL(r_dims);
            for (int i = 0; i < ndim; i++) tmp[i] = (int) src[i];
        }
        dims = tmp;
    }
    void* handle = mojor_gpu_buf_i32_reduce(ctx, buf->handle, op, dims, ndim, keepdims);
    if (!handle) {
        error("mojor_gpu_buf_i32_reduce: failed");
    }
    mojor_gpu_handle_add(handle);
    mojor_gpu_buf_i32_t* out = R_Calloc(1, mojor_gpu_buf_i32_t);
    out->ctx = ctx;
    out->handle = handle;
    out->n = mojor_gpu_buf_i32_len(ctx, handle);
    SEXP ext = PROTECT(R_MakeExternalPtr(out, install("mojor_gpu_buf_i32"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_buf_i32_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(out->n));
    setAttrib(ext, install("gpu_status"), ScalarInteger(1));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_buf_i32_matmul_call(
    SEXP r_ctx,
    SEXP r_a,
    SEXP r_b,
    SEXP r_m,
    SEXP r_k,
    SEXP r_n,
    SEXP r_transpose_a,
    SEXP r_transpose_b
) {
    mojor_gpu_buf_i32_t* buf_a = mojor_gpu_buf_i32_get(r_a);
    mojor_gpu_buf_i32_t* buf_b = mojor_gpu_buf_i32_get(r_b);
    if (!buf_a || !buf_a->handle || !buf_b || !buf_b->handle) {
        error("mojor_gpu_buf_i32_matmul: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_i32_matmul: GPU context unavailable");
    }
    int m = (TYPEOF(r_m) == INTSXP) ? INTEGER(r_m)[0] : (int) REAL(r_m)[0];
    int k = (TYPEOF(r_k) == INTSXP) ? INTEGER(r_k)[0] : (int) REAL(r_k)[0];
    int n = (TYPEOF(r_n) == INTSXP) ? INTEGER(r_n)[0] : (int) REAL(r_n)[0];
    int transpose_a = (TYPEOF(r_transpose_a) == INTSXP) ? INTEGER(r_transpose_a)[0] : (int) REAL(r_transpose_a)[0];
    int transpose_b = (TYPEOF(r_transpose_b) == INTSXP) ? INTEGER(r_transpose_b)[0] : (int) REAL(r_transpose_b)[0];
    void* handle = mojor_gpu_buf_i32_matmul(ctx, buf_a->handle, buf_b->handle, m, k, n, transpose_a, transpose_b);
    if (!handle) {
        error("mojor_gpu_buf_i32_matmul: failed");
    }
    mojor_gpu_handle_add(handle);
    mojor_gpu_buf_i32_t* out = R_Calloc(1, mojor_gpu_buf_i32_t);
    out->ctx = ctx;
    out->handle = handle;
    out->n = mojor_gpu_buf_i32_len(ctx, handle);
    SEXP ext = PROTECT(R_MakeExternalPtr(out, install("mojor_gpu_buf_i32"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_buf_i32_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(out->n));
    setAttrib(ext, install("gpu_status"), ScalarInteger(1));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_buf_i32_matmul_into_call(
    SEXP r_ctx,
    SEXP r_out,
    SEXP r_a,
    SEXP r_b,
    SEXP r_m,
    SEXP r_k,
    SEXP r_n,
    SEXP r_transpose_a,
    SEXP r_transpose_b
) {
    mojor_gpu_buf_i32_t* buf_out = mojor_gpu_buf_i32_get(r_out);
    mojor_gpu_buf_i32_t* buf_a = mojor_gpu_buf_i32_get(r_a);
    mojor_gpu_buf_i32_t* buf_b = mojor_gpu_buf_i32_get(r_b);
    if (!buf_out || !buf_out->handle || !buf_a || !buf_a->handle || !buf_b || !buf_b->handle) {
        error("mojor_gpu_buf_i32_matmul_into: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_i32_matmul_into: GPU context unavailable");
    }
    int m = (TYPEOF(r_m) == INTSXP) ? INTEGER(r_m)[0] : (int) REAL(r_m)[0];
    int k = (TYPEOF(r_k) == INTSXP) ? INTEGER(r_k)[0] : (int) REAL(r_k)[0];
    int n = (TYPEOF(r_n) == INTSXP) ? INTEGER(r_n)[0] : (int) REAL(r_n)[0];
    int transpose_a = (TYPEOF(r_transpose_a) == INTSXP) ? INTEGER(r_transpose_a)[0] : (int) REAL(r_transpose_a)[0];
    int transpose_b = (TYPEOF(r_transpose_b) == INTSXP) ? INTEGER(r_transpose_b)[0] : (int) REAL(r_transpose_b)[0];
    int ok = mojor_gpu_buf_i32_matmul_into(ctx, buf_out->handle, buf_a->handle, buf_b->handle, m, k, n, transpose_a, transpose_b);
    if (!ok) {
        error("mojor_gpu_buf_i32_matmul_into: failed");
    }
    return ScalarLogical(1);
}

SEXP mojor_gpu_buf_f32_slice_call(
    SEXP r_ctx,
    SEXP r_buf,
    SEXP r_starts,
    SEXP r_sizes,
    SEXP r_dims
) {
    mojor_gpu_buf_f32_t* buf = mojor_gpu_buf_f32_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_f32_slice: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f32_slice: GPU context unavailable");
    }
    if ((TYPEOF(r_starts) != INTSXP && TYPEOF(r_starts) != REALSXP) ||
        (TYPEOF(r_sizes) != INTSXP && TYPEOF(r_sizes) != REALSXP) ||
        (TYPEOF(r_dims) != INTSXP && TYPEOF(r_dims) != REALSXP)) {
        error("mojor_gpu_buf_f32_slice: starts/sizes/dims must be integer or numeric");
    }

    int ndim = LENGTH(r_starts);
    if (ndim <= 0) {
        error("mojor_gpu_buf_f32_slice: ndim must be >= 1");
    }
    if (LENGTH(r_sizes) != ndim || LENGTH(r_dims) != ndim) {
        error("mojor_gpu_buf_f32_slice: starts/sizes/dims must have equal length");
    }

    int* starts = (int*) R_alloc((size_t) ndim, sizeof(int));
    int* sizes = (int*) R_alloc((size_t) ndim, sizeof(int));
    int* dims = (int*) R_alloc((size_t) ndim, sizeof(int));

    for (int i = 0; i < ndim; i++) {
        starts[i] = (TYPEOF(r_starts) == INTSXP) ? INTEGER(r_starts)[i] : (int) REAL(r_starts)[i];
        sizes[i] = (TYPEOF(r_sizes) == INTSXP) ? INTEGER(r_sizes)[i] : (int) REAL(r_sizes)[i];
        dims[i] = (TYPEOF(r_dims) == INTSXP) ? INTEGER(r_dims)[i] : (int) REAL(r_dims)[i];
        if (starts[i] < 0 || sizes[i] <= 0 || dims[i] <= 0) {
            error("mojor_gpu_buf_f32_slice: invalid starts/sizes/dims values");
        }
        if (starts[i] + sizes[i] > dims[i]) {
            error("mojor_gpu_buf_f32_slice: slice bounds exceed dimension extent");
        }
    }

    void* handle = mojor_gpu_buf_f32_slice(ctx, buf->handle, starts, sizes, dims, ndim);
    if (!handle) {
        error("mojor_gpu_buf_f32_slice: failed");
    }
    mojor_gpu_handle_add(handle);
    mojor_gpu_buf_f32_t* out = R_Calloc(1, mojor_gpu_buf_f32_t);
    out->ctx = ctx;
    out->handle = handle;
    out->n = mojor_gpu_buf_f32_len(ctx, handle);
    SEXP ext = PROTECT(R_MakeExternalPtr(out, install("mojor_gpu_buf_f32"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_buf_f32_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(out->n));
    setAttrib(ext, install("gpu_status"), ScalarInteger(1));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_buf_f32_gather_call(
    SEXP r_ctx,
    SEXP r_buf,
    SEXP r_idx_data,
    SEXP r_idx_offsets,
    SEXP r_idx_lens,
    SEXP r_dims
) {
    mojor_gpu_buf_f32_t* buf = mojor_gpu_buf_f32_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_f32_gather: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f32_gather: GPU context unavailable");
    }
    if ((TYPEOF(r_idx_data) != INTSXP && TYPEOF(r_idx_data) != REALSXP) ||
        (TYPEOF(r_idx_offsets) != INTSXP && TYPEOF(r_idx_offsets) != REALSXP) ||
        (TYPEOF(r_idx_lens) != INTSXP && TYPEOF(r_idx_lens) != REALSXP) ||
        (TYPEOF(r_dims) != INTSXP && TYPEOF(r_dims) != REALSXP)) {
        error("mojor_gpu_buf_f32_gather: idx_data/idx_offsets/idx_lens/dims must be integer or numeric");
    }

    int ndim = LENGTH(r_idx_offsets);
    if (ndim <= 0) {
        error("mojor_gpu_buf_f32_gather: ndim must be >= 1");
    }
    if (LENGTH(r_idx_lens) != ndim || LENGTH(r_dims) != ndim) {
        error("mojor_gpu_buf_f32_gather: idx_offsets/idx_lens/dims must have equal length");
    }

    int* idx_offsets = (int*) R_alloc((size_t) ndim, sizeof(int));
    int* idx_lens = (int*) R_alloc((size_t) ndim, sizeof(int));
    int* dims = (int*) R_alloc((size_t) ndim, sizeof(int));
    int expected_idx_n = 0;

    for (int i = 0; i < ndim; i++) {
        idx_offsets[i] = (TYPEOF(r_idx_offsets) == INTSXP) ? INTEGER(r_idx_offsets)[i] : (int) REAL(r_idx_offsets)[i];
        idx_lens[i] = (TYPEOF(r_idx_lens) == INTSXP) ? INTEGER(r_idx_lens)[i] : (int) REAL(r_idx_lens)[i];
        dims[i] = (TYPEOF(r_dims) == INTSXP) ? INTEGER(r_dims)[i] : (int) REAL(r_dims)[i];
        if (idx_offsets[i] != expected_idx_n || idx_lens[i] <= 0 || dims[i] <= 0) {
            error("mojor_gpu_buf_f32_gather: invalid index metadata");
        }
        expected_idx_n += idx_lens[i];
    }
    if (LENGTH(r_idx_data) != expected_idx_n) {
        error("mojor_gpu_buf_f32_gather: idx_data length mismatch");
    }

    int* idx_data = (int*) R_alloc((size_t) expected_idx_n, sizeof(int));
    for (int i = 0; i < expected_idx_n; i++) {
        idx_data[i] = (TYPEOF(r_idx_data) == INTSXP) ? INTEGER(r_idx_data)[i] : (int) REAL(r_idx_data)[i];
    }

    void* handle = mojor_gpu_buf_f32_gather(ctx, buf->handle, idx_data, idx_offsets, idx_lens, dims, ndim);
    if (!handle) {
        error("mojor_gpu_buf_f32_gather: failed");
    }
    mojor_gpu_handle_add(handle);
    mojor_gpu_buf_f32_t* out = R_Calloc(1, mojor_gpu_buf_f32_t);
    out->ctx = ctx;
    out->handle = handle;
    out->n = mojor_gpu_buf_f32_len(ctx, handle);
    SEXP ext = PROTECT(R_MakeExternalPtr(out, install("mojor_gpu_buf_f32"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_buf_f32_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(out->n));
    setAttrib(ext, install("gpu_status"), ScalarInteger(1));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_buf_f32_scatter_call(
    SEXP r_ctx,
    SEXP r_buf,
    SEXP r_values,
    SEXP r_idx_data,
    SEXP r_idx_offsets,
    SEXP r_idx_lens,
    SEXP r_dims
) {
    mojor_gpu_buf_f32_t* buf = mojor_gpu_buf_f32_get(r_buf);
    mojor_gpu_buf_f32_t* values = mojor_gpu_buf_f32_get(r_values);
    if (!buf || !buf->handle || !values || !values->handle) {
        error("mojor_gpu_buf_f32_scatter: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f32_scatter: GPU context unavailable");
    }
    if ((TYPEOF(r_idx_data) != INTSXP && TYPEOF(r_idx_data) != REALSXP) ||
        (TYPEOF(r_idx_offsets) != INTSXP && TYPEOF(r_idx_offsets) != REALSXP) ||
        (TYPEOF(r_idx_lens) != INTSXP && TYPEOF(r_idx_lens) != REALSXP) ||
        (TYPEOF(r_dims) != INTSXP && TYPEOF(r_dims) != REALSXP)) {
        error("mojor_gpu_buf_f32_scatter: idx_data/idx_offsets/idx_lens/dims must be integer or numeric");
    }

    int ndim = LENGTH(r_idx_offsets);
    if (ndim <= 0) {
        error("mojor_gpu_buf_f32_scatter: ndim must be >= 1");
    }
    if (LENGTH(r_idx_lens) != ndim || LENGTH(r_dims) != ndim) {
        error("mojor_gpu_buf_f32_scatter: idx_offsets/idx_lens/dims must have equal length");
    }

    int* idx_offsets = (int*) R_alloc((size_t) ndim, sizeof(int));
    int* idx_lens = (int*) R_alloc((size_t) ndim, sizeof(int));
    int* dims = (int*) R_alloc((size_t) ndim, sizeof(int));
    int expected_idx_n = 0;
    int expected_out_n = 1;

    for (int i = 0; i < ndim; i++) {
        idx_offsets[i] = (TYPEOF(r_idx_offsets) == INTSXP) ? INTEGER(r_idx_offsets)[i] : (int) REAL(r_idx_offsets)[i];
        idx_lens[i] = (TYPEOF(r_idx_lens) == INTSXP) ? INTEGER(r_idx_lens)[i] : (int) REAL(r_idx_lens)[i];
        dims[i] = (TYPEOF(r_dims) == INTSXP) ? INTEGER(r_dims)[i] : (int) REAL(r_dims)[i];
        if (idx_offsets[i] != expected_idx_n || idx_lens[i] <= 0 || dims[i] <= 0) {
            error("mojor_gpu_buf_f32_scatter: invalid index metadata");
        }
        expected_idx_n += idx_lens[i];
        expected_out_n *= idx_lens[i];
    }
    if (LENGTH(r_idx_data) != expected_idx_n) {
        error("mojor_gpu_buf_f32_scatter: idx_data length mismatch");
    }
    if (values->n != expected_out_n) {
        error("mojor_gpu_buf_f32_scatter: values length mismatch");
    }

    int* idx_data = (int*) R_alloc((size_t) expected_idx_n, sizeof(int));
    for (int i = 0; i < expected_idx_n; i++) {
        idx_data[i] = (TYPEOF(r_idx_data) == INTSXP) ? INTEGER(r_idx_data)[i] : (int) REAL(r_idx_data)[i];
    }

    int ok = mojor_gpu_buf_f32_scatter(
        ctx,
        buf->handle,
        values->handle,
        idx_data,
        idx_offsets,
        idx_lens,
        dims,
        ndim
    );
    if (!ok) {
        error("mojor_gpu_buf_f32_scatter: failed");
    }
    return ScalarLogical(1);
}

SEXP mojor_gpu_idx_plan_f32_create_call(
    SEXP r_ctx,
    SEXP r_idx_data,
    SEXP r_idx_offsets,
    SEXP r_idx_lens,
    SEXP r_dims
) {
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_idx_plan_f32_create: GPU context unavailable");
    }
    if ((TYPEOF(r_idx_data) != INTSXP && TYPEOF(r_idx_data) != REALSXP) ||
        (TYPEOF(r_idx_offsets) != INTSXP && TYPEOF(r_idx_offsets) != REALSXP) ||
        (TYPEOF(r_idx_lens) != INTSXP && TYPEOF(r_idx_lens) != REALSXP) ||
        (TYPEOF(r_dims) != INTSXP && TYPEOF(r_dims) != REALSXP)) {
        error("mojor_gpu_idx_plan_f32_create: idx_data/idx_offsets/idx_lens/dims must be integer or numeric");
    }

    int ndim = LENGTH(r_idx_offsets);
    if (ndim <= 0) {
        error("mojor_gpu_idx_plan_f32_create: ndim must be >= 1");
    }
    if (LENGTH(r_idx_lens) != ndim || LENGTH(r_dims) != ndim) {
        error("mojor_gpu_idx_plan_f32_create: idx_offsets/idx_lens/dims must have equal length");
    }

    int* idx_offsets = (int*) R_alloc((size_t) ndim, sizeof(int));
    int* idx_lens = (int*) R_alloc((size_t) ndim, sizeof(int));
    int* dims = (int*) R_alloc((size_t) ndim, sizeof(int));
    int expected_idx_n = 0;
    int out_n = 1;
    int full_n = 1;

    for (int i = 0; i < ndim; i++) {
        idx_offsets[i] = (TYPEOF(r_idx_offsets) == INTSXP) ? INTEGER(r_idx_offsets)[i] : (int) REAL(r_idx_offsets)[i];
        idx_lens[i] = (TYPEOF(r_idx_lens) == INTSXP) ? INTEGER(r_idx_lens)[i] : (int) REAL(r_idx_lens)[i];
        dims[i] = (TYPEOF(r_dims) == INTSXP) ? INTEGER(r_dims)[i] : (int) REAL(r_dims)[i];
        if (idx_offsets[i] != expected_idx_n || idx_lens[i] <= 0 || dims[i] <= 0) {
            error("mojor_gpu_idx_plan_f32_create: invalid index metadata");
        }
        expected_idx_n += idx_lens[i];
        out_n *= idx_lens[i];
        full_n *= dims[i];
    }
    if (LENGTH(r_idx_data) != expected_idx_n) {
        error("mojor_gpu_idx_plan_f32_create: idx_data length mismatch");
    }

    int* idx_data = (int*) R_alloc((size_t) expected_idx_n, sizeof(int));
    for (int i = 0; i < expected_idx_n; i++) {
        idx_data[i] = (TYPEOF(r_idx_data) == INTSXP) ? INTEGER(r_idx_data)[i] : (int) REAL(r_idx_data)[i];
    }

    void* handle = mojor_gpu_idx_plan_f32_create(ctx, idx_data, idx_offsets, idx_lens, dims, ndim);
    if (!handle) {
        error("mojor_gpu_idx_plan_f32_create: failed");
    }
    mojor_gpu_handle_add(handle);

    mojor_gpu_idx_plan_f32_t* plan = R_Calloc(1, mojor_gpu_idx_plan_f32_t);
    plan->ctx = ctx;
    plan->handle = handle;
    plan->ndim = ndim;
    plan->idx_data_n = expected_idx_n;
    plan->out_n = out_n;
    plan->full_n = full_n;
    SEXP ext = PROTECT(R_MakeExternalPtr(plan, install("mojor_gpu_idx_plan_f32"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_idx_plan_f32_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(out_n));
    setAttrib(ext, install("gpu_status"), ScalarInteger(1));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_idx_plan_f32_free_call(SEXP r_plan) {
    mojor_gpu_idx_plan_f32_t* plan = mojor_gpu_idx_plan_f32_get(r_plan);
    if (!plan) {
        return ScalarInteger(0);
    }
    int ok = 0;
    if (plan->handle) {
        ok = mojor_gpu_idx_plan_f32_free(plan->ctx, plan->handle);
        mojor_gpu_handle_remove(plan->handle);
        plan->handle = NULL;
    }
    plan->ctx = NULL;
    R_Free(plan);
    R_ClearExternalPtr(r_plan);
    return ScalarInteger(ok);
}

SEXP mojor_gpu_buf_f32_gather_plan_call(
    SEXP r_ctx,
    SEXP r_buf,
    SEXP r_plan
) {
    mojor_gpu_buf_f32_t* buf = mojor_gpu_buf_f32_get(r_buf);
    mojor_gpu_idx_plan_f32_t* plan = mojor_gpu_idx_plan_f32_get(r_plan);
    if (!buf || !buf->handle || !plan || !plan->handle) {
        error("mojor_gpu_buf_f32_gather_plan: invalid buffer or plan");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f32_gather_plan: GPU context unavailable");
    }

    void* handle = mojor_gpu_buf_f32_gather_plan(ctx, buf->handle, plan->handle);
    if (!handle) {
        error("mojor_gpu_buf_f32_gather_plan: failed");
    }
    mojor_gpu_handle_add(handle);
    mojor_gpu_buf_f32_t* out = R_Calloc(1, mojor_gpu_buf_f32_t);
    out->ctx = ctx;
    out->handle = handle;
    out->n = plan->out_n;
    SEXP ext = PROTECT(R_MakeExternalPtr(out, install("mojor_gpu_buf_f32"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_buf_f32_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(out->n));
    setAttrib(ext, install("gpu_status"), ScalarInteger(1));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_buf_f32_scatter_plan_call(
    SEXP r_ctx,
    SEXP r_buf,
    SEXP r_values,
    SEXP r_plan
) {
    mojor_gpu_buf_f32_t* buf = mojor_gpu_buf_f32_get(r_buf);
    mojor_gpu_buf_f32_t* values = mojor_gpu_buf_f32_get(r_values);
    mojor_gpu_idx_plan_f32_t* plan = mojor_gpu_idx_plan_f32_get(r_plan);
    if (!buf || !buf->handle || !values || !values->handle || !plan || !plan->handle) {
        error("mojor_gpu_buf_f32_scatter_plan: invalid buffer or plan");
    }
    if (values->n != plan->out_n) {
        error("mojor_gpu_buf_f32_scatter_plan: values length mismatch");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f32_scatter_plan: GPU context unavailable");
    }

    int ok = mojor_gpu_buf_f32_scatter_plan(ctx, buf->handle, values->handle, plan->handle);
    if (!ok) {
        error("mojor_gpu_buf_f32_scatter_plan: failed");
    }
    return ScalarLogical(1);
}

SEXP mojor_gpu_buf_f64_slice_call(
    SEXP r_ctx,
    SEXP r_buf,
    SEXP r_starts,
    SEXP r_sizes,
    SEXP r_dims
) {
    mojor_gpu_buf_f64_t* buf = mojor_gpu_buf_f64_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_f64_slice: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f64_slice: GPU context unavailable");
    }
    if ((TYPEOF(r_starts) != INTSXP && TYPEOF(r_starts) != REALSXP) ||
        (TYPEOF(r_sizes) != INTSXP && TYPEOF(r_sizes) != REALSXP) ||
        (TYPEOF(r_dims) != INTSXP && TYPEOF(r_dims) != REALSXP)) {
        error("mojor_gpu_buf_f64_slice: starts/sizes/dims must be integer or numeric");
    }

    int ndim = LENGTH(r_starts);
    if (ndim <= 0) {
        error("mojor_gpu_buf_f64_slice: ndim must be >= 1");
    }
    if (LENGTH(r_sizes) != ndim || LENGTH(r_dims) != ndim) {
        error("mojor_gpu_buf_f64_slice: starts/sizes/dims must have equal length");
    }

    int* starts = (int*) R_alloc((size_t) ndim, sizeof(int));
    int* sizes = (int*) R_alloc((size_t) ndim, sizeof(int));
    int* dims = (int*) R_alloc((size_t) ndim, sizeof(int));

    for (int i = 0; i < ndim; i++) {
        starts[i] = (TYPEOF(r_starts) == INTSXP) ? INTEGER(r_starts)[i] : (int) REAL(r_starts)[i];
        sizes[i] = (TYPEOF(r_sizes) == INTSXP) ? INTEGER(r_sizes)[i] : (int) REAL(r_sizes)[i];
        dims[i] = (TYPEOF(r_dims) == INTSXP) ? INTEGER(r_dims)[i] : (int) REAL(r_dims)[i];
        if (starts[i] < 0 || sizes[i] <= 0 || dims[i] <= 0) {
            error("mojor_gpu_buf_f64_slice: invalid starts/sizes/dims values");
        }
        if (starts[i] + sizes[i] > dims[i]) {
            error("mojor_gpu_buf_f64_slice: slice bounds exceed dimension extent");
        }
    }

    void* handle = mojor_gpu_buf_f64_slice(ctx, buf->handle, starts, sizes, dims, ndim);
    if (!handle) {
        error("mojor_gpu_buf_f64_slice: failed");
    }
    mojor_gpu_handle_add(handle);
    mojor_gpu_buf_f64_t* out = R_Calloc(1, mojor_gpu_buf_f64_t);
    out->ctx = ctx;
    out->handle = handle;
    out->n = mojor_gpu_buf_f64_len(ctx, handle);
    SEXP ext = PROTECT(R_MakeExternalPtr(out, install("mojor_gpu_buf_f64"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_buf_f64_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(out->n));
    setAttrib(ext, install("gpu_status"), ScalarInteger(1));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_buf_f64_gather_call(
    SEXP r_ctx,
    SEXP r_buf,
    SEXP r_idx_data,
    SEXP r_idx_offsets,
    SEXP r_idx_lens,
    SEXP r_dims
) {
    mojor_gpu_buf_f64_t* buf = mojor_gpu_buf_f64_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_f64_gather: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f64_gather: GPU context unavailable");
    }
    if ((TYPEOF(r_idx_data) != INTSXP && TYPEOF(r_idx_data) != REALSXP) ||
        (TYPEOF(r_idx_offsets) != INTSXP && TYPEOF(r_idx_offsets) != REALSXP) ||
        (TYPEOF(r_idx_lens) != INTSXP && TYPEOF(r_idx_lens) != REALSXP) ||
        (TYPEOF(r_dims) != INTSXP && TYPEOF(r_dims) != REALSXP)) {
        error("mojor_gpu_buf_f64_gather: idx_data/idx_offsets/idx_lens/dims must be integer or numeric");
    }

    int ndim = LENGTH(r_idx_offsets);
    if (ndim <= 0) {
        error("mojor_gpu_buf_f64_gather: ndim must be >= 1");
    }
    if (LENGTH(r_idx_lens) != ndim || LENGTH(r_dims) != ndim) {
        error("mojor_gpu_buf_f64_gather: idx_offsets/idx_lens/dims must have equal length");
    }

    int* idx_offsets = (int*) R_alloc((size_t) ndim, sizeof(int));
    int* idx_lens = (int*) R_alloc((size_t) ndim, sizeof(int));
    int* dims = (int*) R_alloc((size_t) ndim, sizeof(int));
    int expected_idx_n = 0;

    for (int i = 0; i < ndim; i++) {
        idx_offsets[i] = (TYPEOF(r_idx_offsets) == INTSXP) ? INTEGER(r_idx_offsets)[i] : (int) REAL(r_idx_offsets)[i];
        idx_lens[i] = (TYPEOF(r_idx_lens) == INTSXP) ? INTEGER(r_idx_lens)[i] : (int) REAL(r_idx_lens)[i];
        dims[i] = (TYPEOF(r_dims) == INTSXP) ? INTEGER(r_dims)[i] : (int) REAL(r_dims)[i];
        if (idx_offsets[i] != expected_idx_n || idx_lens[i] <= 0 || dims[i] <= 0) {
            error("mojor_gpu_buf_f64_gather: invalid index metadata");
        }
        expected_idx_n += idx_lens[i];
    }
    if (LENGTH(r_idx_data) != expected_idx_n) {
        error("mojor_gpu_buf_f64_gather: idx_data length mismatch");
    }

    int* idx_data = (int*) R_alloc((size_t) expected_idx_n, sizeof(int));
    for (int i = 0; i < expected_idx_n; i++) {
        idx_data[i] = (TYPEOF(r_idx_data) == INTSXP) ? INTEGER(r_idx_data)[i] : (int) REAL(r_idx_data)[i];
    }

    void* handle = mojor_gpu_buf_f64_gather(ctx, buf->handle, idx_data, idx_offsets, idx_lens, dims, ndim);
    if (!handle) {
        error("mojor_gpu_buf_f64_gather: failed");
    }
    mojor_gpu_handle_add(handle);
    mojor_gpu_buf_f64_t* out = R_Calloc(1, mojor_gpu_buf_f64_t);
    out->ctx = ctx;
    out->handle = handle;
    out->n = mojor_gpu_buf_f64_len(ctx, handle);
    SEXP ext = PROTECT(R_MakeExternalPtr(out, install("mojor_gpu_buf_f64"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_buf_f64_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(out->n));
    setAttrib(ext, install("gpu_status"), ScalarInteger(1));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_buf_f64_scatter_call(
    SEXP r_ctx,
    SEXP r_buf,
    SEXP r_values,
    SEXP r_idx_data,
    SEXP r_idx_offsets,
    SEXP r_idx_lens,
    SEXP r_dims
) {
    mojor_gpu_buf_f64_t* buf = mojor_gpu_buf_f64_get(r_buf);
    mojor_gpu_buf_f64_t* values = mojor_gpu_buf_f64_get(r_values);
    if (!buf || !buf->handle || !values || !values->handle) {
        error("mojor_gpu_buf_f64_scatter: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f64_scatter: GPU context unavailable");
    }
    if ((TYPEOF(r_idx_data) != INTSXP && TYPEOF(r_idx_data) != REALSXP) ||
        (TYPEOF(r_idx_offsets) != INTSXP && TYPEOF(r_idx_offsets) != REALSXP) ||
        (TYPEOF(r_idx_lens) != INTSXP && TYPEOF(r_idx_lens) != REALSXP) ||
        (TYPEOF(r_dims) != INTSXP && TYPEOF(r_dims) != REALSXP)) {
        error("mojor_gpu_buf_f64_scatter: idx_data/idx_offsets/idx_lens/dims must be integer or numeric");
    }

    int ndim = LENGTH(r_idx_offsets);
    if (ndim <= 0) {
        error("mojor_gpu_buf_f64_scatter: ndim must be >= 1");
    }
    if (LENGTH(r_idx_lens) != ndim || LENGTH(r_dims) != ndim) {
        error("mojor_gpu_buf_f64_scatter: idx_offsets/idx_lens/dims must have equal length");
    }

    int* idx_offsets = (int*) R_alloc((size_t) ndim, sizeof(int));
    int* idx_lens = (int*) R_alloc((size_t) ndim, sizeof(int));
    int* dims = (int*) R_alloc((size_t) ndim, sizeof(int));
    int expected_idx_n = 0;
    int expected_out_n = 1;

    for (int i = 0; i < ndim; i++) {
        idx_offsets[i] = (TYPEOF(r_idx_offsets) == INTSXP) ? INTEGER(r_idx_offsets)[i] : (int) REAL(r_idx_offsets)[i];
        idx_lens[i] = (TYPEOF(r_idx_lens) == INTSXP) ? INTEGER(r_idx_lens)[i] : (int) REAL(r_idx_lens)[i];
        dims[i] = (TYPEOF(r_dims) == INTSXP) ? INTEGER(r_dims)[i] : (int) REAL(r_dims)[i];
        if (idx_offsets[i] != expected_idx_n || idx_lens[i] <= 0 || dims[i] <= 0) {
            error("mojor_gpu_buf_f64_scatter: invalid index metadata");
        }
        expected_idx_n += idx_lens[i];
        expected_out_n *= idx_lens[i];
    }
    if (LENGTH(r_idx_data) != expected_idx_n) {
        error("mojor_gpu_buf_f64_scatter: idx_data length mismatch");
    }
    if (values->n != expected_out_n) {
        error("mojor_gpu_buf_f64_scatter: values length mismatch");
    }

    int* idx_data = (int*) R_alloc((size_t) expected_idx_n, sizeof(int));
    for (int i = 0; i < expected_idx_n; i++) {
        idx_data[i] = (TYPEOF(r_idx_data) == INTSXP) ? INTEGER(r_idx_data)[i] : (int) REAL(r_idx_data)[i];
    }

    int ok = mojor_gpu_buf_f64_scatter(
        ctx,
        buf->handle,
        values->handle,
        idx_data,
        idx_offsets,
        idx_lens,
        dims,
        ndim
    );
    if (!ok) {
        error("mojor_gpu_buf_f64_scatter: failed");
    }
    return ScalarLogical(1);
}

SEXP mojor_gpu_idx_plan_f64_create_call(
    SEXP r_ctx,
    SEXP r_idx_data,
    SEXP r_idx_offsets,
    SEXP r_idx_lens,
    SEXP r_dims
) {
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_idx_plan_f64_create: GPU context unavailable");
    }
    if ((TYPEOF(r_idx_data) != INTSXP && TYPEOF(r_idx_data) != REALSXP) ||
        (TYPEOF(r_idx_offsets) != INTSXP && TYPEOF(r_idx_offsets) != REALSXP) ||
        (TYPEOF(r_idx_lens) != INTSXP && TYPEOF(r_idx_lens) != REALSXP) ||
        (TYPEOF(r_dims) != INTSXP && TYPEOF(r_dims) != REALSXP)) {
        error("mojor_gpu_idx_plan_f64_create: idx_data/idx_offsets/idx_lens/dims must be integer or numeric");
    }

    int ndim = LENGTH(r_idx_offsets);
    if (ndim <= 0) {
        error("mojor_gpu_idx_plan_f64_create: ndim must be >= 1");
    }
    if (LENGTH(r_idx_lens) != ndim || LENGTH(r_dims) != ndim) {
        error("mojor_gpu_idx_plan_f64_create: idx_offsets/idx_lens/dims must have equal length");
    }

    int* idx_offsets = (int*) R_alloc((size_t) ndim, sizeof(int));
    int* idx_lens = (int*) R_alloc((size_t) ndim, sizeof(int));
    int* dims = (int*) R_alloc((size_t) ndim, sizeof(int));
    int expected_idx_n = 0;
    int out_n = 1;
    int full_n = 1;

    for (int i = 0; i < ndim; i++) {
        idx_offsets[i] = (TYPEOF(r_idx_offsets) == INTSXP) ? INTEGER(r_idx_offsets)[i] : (int) REAL(r_idx_offsets)[i];
        idx_lens[i] = (TYPEOF(r_idx_lens) == INTSXP) ? INTEGER(r_idx_lens)[i] : (int) REAL(r_idx_lens)[i];
        dims[i] = (TYPEOF(r_dims) == INTSXP) ? INTEGER(r_dims)[i] : (int) REAL(r_dims)[i];
        if (idx_offsets[i] != expected_idx_n || idx_lens[i] <= 0 || dims[i] <= 0) {
            error("mojor_gpu_idx_plan_f64_create: invalid index metadata");
        }
        expected_idx_n += idx_lens[i];
        out_n *= idx_lens[i];
        full_n *= dims[i];
    }
    if (LENGTH(r_idx_data) != expected_idx_n) {
        error("mojor_gpu_idx_plan_f64_create: idx_data length mismatch");
    }

    int* idx_data = (int*) R_alloc((size_t) expected_idx_n, sizeof(int));
    for (int i = 0; i < expected_idx_n; i++) {
        idx_data[i] = (TYPEOF(r_idx_data) == INTSXP) ? INTEGER(r_idx_data)[i] : (int) REAL(r_idx_data)[i];
    }

    void* handle = mojor_gpu_idx_plan_f64_create(ctx, idx_data, idx_offsets, idx_lens, dims, ndim);
    if (!handle) {
        error("mojor_gpu_idx_plan_f64_create: failed");
    }
    mojor_gpu_handle_add(handle);

    mojor_gpu_idx_plan_f64_t* plan = R_Calloc(1, mojor_gpu_idx_plan_f64_t);
    plan->ctx = ctx;
    plan->handle = handle;
    plan->ndim = ndim;
    plan->idx_data_n = expected_idx_n;
    plan->out_n = out_n;
    plan->full_n = full_n;
    SEXP ext = PROTECT(R_MakeExternalPtr(plan, install("mojor_gpu_idx_plan_f64"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_idx_plan_f64_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(out_n));
    setAttrib(ext, install("gpu_status"), ScalarInteger(1));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_idx_plan_f64_free_call(SEXP r_plan) {
    mojor_gpu_idx_plan_f64_t* plan = mojor_gpu_idx_plan_f64_get(r_plan);
    if (!plan) {
        return ScalarInteger(0);
    }
    int ok = 0;
    if (plan->handle) {
        ok = mojor_gpu_idx_plan_f64_free(plan->ctx, plan->handle);
        mojor_gpu_handle_remove(plan->handle);
        plan->handle = NULL;
    }
    plan->ctx = NULL;
    R_Free(plan);
    R_ClearExternalPtr(r_plan);
    return ScalarInteger(ok);
}

SEXP mojor_gpu_buf_f64_gather_plan_call(
    SEXP r_ctx,
    SEXP r_buf,
    SEXP r_plan
) {
    mojor_gpu_buf_f64_t* buf = mojor_gpu_buf_f64_get(r_buf);
    mojor_gpu_idx_plan_f64_t* plan = mojor_gpu_idx_plan_f64_get(r_plan);
    if (!buf || !buf->handle || !plan || !plan->handle) {
        error("mojor_gpu_buf_f64_gather_plan: invalid buffer or plan");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f64_gather_plan: GPU context unavailable");
    }

    void* handle = mojor_gpu_buf_f64_gather_plan(ctx, buf->handle, plan->handle);
    if (!handle) {
        error("mojor_gpu_buf_f64_gather_plan: failed");
    }
    mojor_gpu_handle_add(handle);
    mojor_gpu_buf_f64_t* out = R_Calloc(1, mojor_gpu_buf_f64_t);
    out->ctx = ctx;
    out->handle = handle;
    out->n = plan->out_n;
    SEXP ext = PROTECT(R_MakeExternalPtr(out, install("mojor_gpu_buf_f64"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_buf_f64_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(out->n));
    setAttrib(ext, install("gpu_status"), ScalarInteger(1));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_buf_f64_scatter_plan_call(
    SEXP r_ctx,
    SEXP r_buf,
    SEXP r_values,
    SEXP r_plan
) {
    mojor_gpu_buf_f64_t* buf = mojor_gpu_buf_f64_get(r_buf);
    mojor_gpu_buf_f64_t* values = mojor_gpu_buf_f64_get(r_values);
    mojor_gpu_idx_plan_f64_t* plan = mojor_gpu_idx_plan_f64_get(r_plan);
    if (!buf || !buf->handle || !values || !values->handle || !plan || !plan->handle) {
        error("mojor_gpu_buf_f64_scatter_plan: invalid buffer or plan");
    }
    if (values->n != plan->out_n) {
        error("mojor_gpu_buf_f64_scatter_plan: values length mismatch");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f64_scatter_plan: GPU context unavailable");
    }

    int ok = mojor_gpu_buf_f64_scatter_plan(ctx, buf->handle, values->handle, plan->handle);
    if (!ok) {
        error("mojor_gpu_buf_f64_scatter_plan: failed");
    }
    return ScalarLogical(1);
}

SEXP mojor_gpu_buf_i32_slice_call(
    SEXP r_ctx,
    SEXP r_buf,
    SEXP r_starts,
    SEXP r_sizes,
    SEXP r_dims
) {
    mojor_gpu_buf_i32_t* buf = mojor_gpu_buf_i32_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_i32_slice: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_i32_slice: GPU context unavailable");
    }
    if ((TYPEOF(r_starts) != INTSXP && TYPEOF(r_starts) != REALSXP) ||
        (TYPEOF(r_sizes) != INTSXP && TYPEOF(r_sizes) != REALSXP) ||
        (TYPEOF(r_dims) != INTSXP && TYPEOF(r_dims) != REALSXP)) {
        error("mojor_gpu_buf_i32_slice: starts/sizes/dims must be integer or numeric");
    }

    int ndim = LENGTH(r_starts);
    if (ndim <= 0) {
        error("mojor_gpu_buf_i32_slice: ndim must be >= 1");
    }
    if (LENGTH(r_sizes) != ndim || LENGTH(r_dims) != ndim) {
        error("mojor_gpu_buf_i32_slice: starts/sizes/dims must have equal length");
    }

    int* starts = (int*) R_alloc((size_t) ndim, sizeof(int));
    int* sizes = (int*) R_alloc((size_t) ndim, sizeof(int));
    int* dims = (int*) R_alloc((size_t) ndim, sizeof(int));

    for (int i = 0; i < ndim; i++) {
        starts[i] = (TYPEOF(r_starts) == INTSXP) ? INTEGER(r_starts)[i] : (int) REAL(r_starts)[i];
        sizes[i] = (TYPEOF(r_sizes) == INTSXP) ? INTEGER(r_sizes)[i] : (int) REAL(r_sizes)[i];
        dims[i] = (TYPEOF(r_dims) == INTSXP) ? INTEGER(r_dims)[i] : (int) REAL(r_dims)[i];
        if (starts[i] < 0 || sizes[i] <= 0 || dims[i] <= 0) {
            error("mojor_gpu_buf_i32_slice: invalid starts/sizes/dims values");
        }
        if (starts[i] + sizes[i] > dims[i]) {
            error("mojor_gpu_buf_i32_slice: slice bounds exceed dimension extent");
        }
    }

    void* handle = mojor_gpu_buf_i32_slice(ctx, buf->handle, starts, sizes, dims, ndim);
    if (!handle) {
        error("mojor_gpu_buf_i32_slice: failed");
    }
    mojor_gpu_handle_add(handle);
    mojor_gpu_buf_i32_t* out = R_Calloc(1, mojor_gpu_buf_i32_t);
    out->ctx = ctx;
    out->handle = handle;
    out->n = mojor_gpu_buf_i32_len(ctx, handle);
    SEXP ext = PROTECT(R_MakeExternalPtr(out, install("mojor_gpu_buf_i32"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_buf_i32_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(out->n));
    setAttrib(ext, install("gpu_status"), ScalarInteger(1));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_buf_i32_slice_assign_call(
    SEXP r_ctx,
    SEXP r_buf,
    SEXP r_values,
    SEXP r_starts,
    SEXP r_sizes,
    SEXP r_dims
) {
    mojor_gpu_buf_i32_t* buf = mojor_gpu_buf_i32_get(r_buf);
    mojor_gpu_buf_i32_t* values = mojor_gpu_buf_i32_get(r_values);
    if (!buf || !buf->handle || !values || !values->handle) {
        error("mojor_gpu_buf_i32_slice_assign: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_i32_slice_assign: GPU context unavailable");
    }
    if ((TYPEOF(r_starts) != INTSXP && TYPEOF(r_starts) != REALSXP) ||
        (TYPEOF(r_sizes) != INTSXP && TYPEOF(r_sizes) != REALSXP) ||
        (TYPEOF(r_dims) != INTSXP && TYPEOF(r_dims) != REALSXP)) {
        error("mojor_gpu_buf_i32_slice_assign: starts/sizes/dims must be integer or numeric");
    }

    int ndim = LENGTH(r_starts);
    if (ndim <= 0) {
        error("mojor_gpu_buf_i32_slice_assign: ndim must be >= 1");
    }
    if (LENGTH(r_sizes) != ndim || LENGTH(r_dims) != ndim) {
        error("mojor_gpu_buf_i32_slice_assign: starts/sizes/dims must have equal length");
    }

    int* starts = (int*) R_alloc((size_t) ndim, sizeof(int));
    int* sizes = (int*) R_alloc((size_t) ndim, sizeof(int));
    int* dims = (int*) R_alloc((size_t) ndim, sizeof(int));
    int expected_values_n = 1;

    for (int i = 0; i < ndim; i++) {
        starts[i] = (TYPEOF(r_starts) == INTSXP) ? INTEGER(r_starts)[i] : (int) REAL(r_starts)[i];
        sizes[i] = (TYPEOF(r_sizes) == INTSXP) ? INTEGER(r_sizes)[i] : (int) REAL(r_sizes)[i];
        dims[i] = (TYPEOF(r_dims) == INTSXP) ? INTEGER(r_dims)[i] : (int) REAL(r_dims)[i];
        if (starts[i] < 0 || sizes[i] <= 0 || dims[i] <= 0) {
            error("mojor_gpu_buf_i32_slice_assign: invalid starts/sizes/dims values");
        }
        if (starts[i] + sizes[i] > dims[i]) {
            error("mojor_gpu_buf_i32_slice_assign: slice bounds exceed dimension extent");
        }
        expected_values_n *= sizes[i];
    }
    if (values->n != expected_values_n) {
        error("mojor_gpu_buf_i32_slice_assign: values length mismatch");
    }

    int ok = mojor_gpu_buf_i32_slice_assign(
        ctx,
        buf->handle,
        values->handle,
        starts,
        sizes,
        dims,
        ndim
    );
    if (!ok) {
        error("mojor_gpu_buf_i32_slice_assign: failed");
    }
    return ScalarLogical(1);
}

SEXP mojor_gpu_buf_i32_gather_call(
    SEXP r_ctx,
    SEXP r_buf,
    SEXP r_idx_data,
    SEXP r_idx_offsets,
    SEXP r_idx_lens,
    SEXP r_dims
) {
    mojor_gpu_buf_i32_t* buf = mojor_gpu_buf_i32_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_i32_gather: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_i32_gather: GPU context unavailable");
    }
    if ((TYPEOF(r_idx_data) != INTSXP && TYPEOF(r_idx_data) != REALSXP) ||
        (TYPEOF(r_idx_offsets) != INTSXP && TYPEOF(r_idx_offsets) != REALSXP) ||
        (TYPEOF(r_idx_lens) != INTSXP && TYPEOF(r_idx_lens) != REALSXP) ||
        (TYPEOF(r_dims) != INTSXP && TYPEOF(r_dims) != REALSXP)) {
        error("mojor_gpu_buf_i32_gather: idx_data/idx_offsets/idx_lens/dims must be integer or numeric");
    }

    int ndim = LENGTH(r_idx_offsets);
    if (ndim <= 0) {
        error("mojor_gpu_buf_i32_gather: ndim must be >= 1");
    }
    if (LENGTH(r_idx_lens) != ndim || LENGTH(r_dims) != ndim) {
        error("mojor_gpu_buf_i32_gather: idx_offsets/idx_lens/dims must have equal length");
    }

    int* idx_offsets = (int*) R_alloc((size_t) ndim, sizeof(int));
    int* idx_lens = (int*) R_alloc((size_t) ndim, sizeof(int));
    int* dims = (int*) R_alloc((size_t) ndim, sizeof(int));
    int expected_idx_n = 0;

    for (int i = 0; i < ndim; i++) {
        idx_offsets[i] = (TYPEOF(r_idx_offsets) == INTSXP) ? INTEGER(r_idx_offsets)[i] : (int) REAL(r_idx_offsets)[i];
        idx_lens[i] = (TYPEOF(r_idx_lens) == INTSXP) ? INTEGER(r_idx_lens)[i] : (int) REAL(r_idx_lens)[i];
        dims[i] = (TYPEOF(r_dims) == INTSXP) ? INTEGER(r_dims)[i] : (int) REAL(r_dims)[i];
        if (idx_offsets[i] != expected_idx_n || idx_lens[i] <= 0 || dims[i] <= 0) {
            error("mojor_gpu_buf_i32_gather: invalid index metadata");
        }
        expected_idx_n += idx_lens[i];
    }
    if (LENGTH(r_idx_data) != expected_idx_n) {
        error("mojor_gpu_buf_i32_gather: idx_data length mismatch");
    }

    int* idx_data = (int*) R_alloc((size_t) expected_idx_n, sizeof(int));
    for (int i = 0; i < expected_idx_n; i++) {
        idx_data[i] = (TYPEOF(r_idx_data) == INTSXP) ? INTEGER(r_idx_data)[i] : (int) REAL(r_idx_data)[i];
    }

    void* handle = mojor_gpu_buf_i32_gather(ctx, buf->handle, idx_data, idx_offsets, idx_lens, dims, ndim);
    if (!handle) {
        error("mojor_gpu_buf_i32_gather: failed");
    }
    mojor_gpu_handle_add(handle);
    mojor_gpu_buf_i32_t* out = R_Calloc(1, mojor_gpu_buf_i32_t);
    out->ctx = ctx;
    out->handle = handle;
    out->n = mojor_gpu_buf_i32_len(ctx, handle);
    SEXP ext = PROTECT(R_MakeExternalPtr(out, install("mojor_gpu_buf_i32"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_buf_i32_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(out->n));
    setAttrib(ext, install("gpu_status"), ScalarInteger(1));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_buf_i32_scatter_call(
    SEXP r_ctx,
    SEXP r_buf,
    SEXP r_values,
    SEXP r_idx_data,
    SEXP r_idx_offsets,
    SEXP r_idx_lens,
    SEXP r_dims
) {
    mojor_gpu_buf_i32_t* buf = mojor_gpu_buf_i32_get(r_buf);
    mojor_gpu_buf_i32_t* values = mojor_gpu_buf_i32_get(r_values);
    if (!buf || !buf->handle || !values || !values->handle) {
        error("mojor_gpu_buf_i32_scatter: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_i32_scatter: GPU context unavailable");
    }
    if ((TYPEOF(r_idx_data) != INTSXP && TYPEOF(r_idx_data) != REALSXP) ||
        (TYPEOF(r_idx_offsets) != INTSXP && TYPEOF(r_idx_offsets) != REALSXP) ||
        (TYPEOF(r_idx_lens) != INTSXP && TYPEOF(r_idx_lens) != REALSXP) ||
        (TYPEOF(r_dims) != INTSXP && TYPEOF(r_dims) != REALSXP)) {
        error("mojor_gpu_buf_i32_scatter: idx_data/idx_offsets/idx_lens/dims must be integer or numeric");
    }

    int ndim = LENGTH(r_idx_offsets);
    if (ndim <= 0) {
        error("mojor_gpu_buf_i32_scatter: ndim must be >= 1");
    }
    if (LENGTH(r_idx_lens) != ndim || LENGTH(r_dims) != ndim) {
        error("mojor_gpu_buf_i32_scatter: idx_offsets/idx_lens/dims must have equal length");
    }

    int* idx_offsets = (int*) R_alloc((size_t) ndim, sizeof(int));
    int* idx_lens = (int*) R_alloc((size_t) ndim, sizeof(int));
    int* dims = (int*) R_alloc((size_t) ndim, sizeof(int));
    int expected_idx_n = 0;
    int expected_out_n = 1;

    for (int i = 0; i < ndim; i++) {
        idx_offsets[i] = (TYPEOF(r_idx_offsets) == INTSXP) ? INTEGER(r_idx_offsets)[i] : (int) REAL(r_idx_offsets)[i];
        idx_lens[i] = (TYPEOF(r_idx_lens) == INTSXP) ? INTEGER(r_idx_lens)[i] : (int) REAL(r_idx_lens)[i];
        dims[i] = (TYPEOF(r_dims) == INTSXP) ? INTEGER(r_dims)[i] : (int) REAL(r_dims)[i];
        if (idx_offsets[i] != expected_idx_n || idx_lens[i] <= 0 || dims[i] <= 0) {
            error("mojor_gpu_buf_i32_scatter: invalid index metadata");
        }
        expected_idx_n += idx_lens[i];
        expected_out_n *= idx_lens[i];
    }
    if (LENGTH(r_idx_data) != expected_idx_n) {
        error("mojor_gpu_buf_i32_scatter: idx_data length mismatch");
    }
    if (values->n != expected_out_n) {
        error("mojor_gpu_buf_i32_scatter: values length mismatch");
    }

    int* idx_data = (int*) R_alloc((size_t) expected_idx_n, sizeof(int));
    for (int i = 0; i < expected_idx_n; i++) {
        idx_data[i] = (TYPEOF(r_idx_data) == INTSXP) ? INTEGER(r_idx_data)[i] : (int) REAL(r_idx_data)[i];
    }

    int ok = mojor_gpu_buf_i32_scatter(
        ctx,
        buf->handle,
        values->handle,
        idx_data,
        idx_offsets,
        idx_lens,
        dims,
        ndim
    );
    if (!ok) {
        error("mojor_gpu_buf_i32_scatter: failed");
    }
    return ScalarLogical(1);
}

SEXP mojor_gpu_buf_f64_alloc_call(SEXP r_ctx, SEXP r_n) {
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f64_alloc: GPU context unavailable");
    }
    int n = (TYPEOF(r_n) == INTSXP) ? INTEGER(r_n)[0] : (int) REAL(r_n)[0];
    if (n < 0) {
        error("mojor_gpu_buf_f64_alloc: n must be >= 0");
    }
    void* handle = mojor_gpu_buf_f64_alloc(ctx, n);
    if (!handle) {
        error("mojor_gpu_buf_f64_alloc: allocation failed");
    }
    mojor_gpu_handle_add(handle);
    mojor_gpu_buf_f64_t* buf = R_Calloc(1, mojor_gpu_buf_f64_t);
    buf->ctx = ctx;
    buf->handle = handle;
    buf->n = n;
    SEXP ext = PROTECT(R_MakeExternalPtr(buf, install("mojor_gpu_buf_f64"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_buf_f64_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(n));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_buf_f64_reduce_call(
    SEXP r_ctx,
    SEXP r_buf,
    SEXP r_op,
    SEXP r_dims,
    SEXP r_ndim,
    SEXP r_keepdims
) {
    mojor_gpu_buf_f64_t* buf = mojor_gpu_buf_f64_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_f64_reduce: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f64_reduce: GPU context unavailable");
    }
    int op = (TYPEOF(r_op) == INTSXP) ? INTEGER(r_op)[0] : (int) REAL(r_op)[0];
    int ndim = (TYPEOF(r_ndim) == INTSXP) ? INTEGER(r_ndim)[0] : (int) REAL(r_ndim)[0];
    int keepdims = (TYPEOF(r_keepdims) == INTSXP) ? INTEGER(r_keepdims)[0] : (int) REAL(r_keepdims)[0];
    const int* dims = NULL;
    if (ndim > 0 && r_dims != R_NilValue) {
        if (TYPEOF(r_dims) != INTSXP && TYPEOF(r_dims) != REALSXP) {
            error("mojor_gpu_buf_f64_reduce: dims must be integer or numeric");
        }
        int n = LENGTH(r_dims);
        if (n < ndim) ndim = n;
        int* tmp = (int*) R_alloc((size_t) ndim, sizeof(int));
        if (TYPEOF(r_dims) == INTSXP) {
            int* src = INTEGER(r_dims);
            for (int i = 0; i < ndim; i++) tmp[i] = src[i];
        } else {
            double* src = REAL(r_dims);
            for (int i = 0; i < ndim; i++) tmp[i] = (int) src[i];
        }
        dims = tmp;
    }
    void* handle = mojor_gpu_buf_f64_reduce(ctx, buf->handle, op, dims, ndim, keepdims);
    if (!handle) {
        error("mojor_gpu_buf_f64_reduce: failed");
    }
    mojor_gpu_handle_add(handle);
    mojor_gpu_buf_f64_t* out = R_Calloc(1, mojor_gpu_buf_f64_t);
    out->ctx = ctx;
    out->handle = handle;
    out->n = mojor_gpu_buf_f64_len(ctx, handle);
    SEXP ext = PROTECT(R_MakeExternalPtr(out, install("mojor_gpu_buf_f64"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_buf_f64_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(out->n));
    setAttrib(ext, install("gpu_status"), ScalarInteger(1));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_buf_f64_matmul_call(
    SEXP r_ctx,
    SEXP r_a,
    SEXP r_b,
    SEXP r_m,
    SEXP r_k,
    SEXP r_n,
    SEXP r_transpose_a,
    SEXP r_transpose_b
) {
    mojor_gpu_buf_f64_t* buf_a = mojor_gpu_buf_f64_get(r_a);
    mojor_gpu_buf_f64_t* buf_b = mojor_gpu_buf_f64_get(r_b);
    if (!buf_a || !buf_a->handle || !buf_b || !buf_b->handle) {
        error("mojor_gpu_buf_f64_matmul: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f64_matmul: GPU context unavailable");
    }
    int m = (TYPEOF(r_m) == INTSXP) ? INTEGER(r_m)[0] : (int) REAL(r_m)[0];
    int k = (TYPEOF(r_k) == INTSXP) ? INTEGER(r_k)[0] : (int) REAL(r_k)[0];
    int n = (TYPEOF(r_n) == INTSXP) ? INTEGER(r_n)[0] : (int) REAL(r_n)[0];
    int transpose_a = (TYPEOF(r_transpose_a) == INTSXP) ? INTEGER(r_transpose_a)[0] : (int) REAL(r_transpose_a)[0];
    int transpose_b = (TYPEOF(r_transpose_b) == INTSXP) ? INTEGER(r_transpose_b)[0] : (int) REAL(r_transpose_b)[0];
    void* handle = mojor_gpu_buf_f64_matmul(ctx, buf_a->handle, buf_b->handle, m, k, n, transpose_a, transpose_b);
    if (!handle) {
        error("mojor_gpu_buf_f64_matmul: failed");
    }
    mojor_gpu_handle_add(handle);
    mojor_gpu_buf_f64_t* out = R_Calloc(1, mojor_gpu_buf_f64_t);
    out->ctx = ctx;
    out->handle = handle;
    out->n = mojor_gpu_buf_f64_len(ctx, handle);
    SEXP ext = PROTECT(R_MakeExternalPtr(out, install("mojor_gpu_buf_f64"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_buf_f64_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(out->n));
    setAttrib(ext, install("gpu_status"), ScalarInteger(1));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_buf_f64_matmul_into_call(
    SEXP r_ctx,
    SEXP r_out,
    SEXP r_a,
    SEXP r_b,
    SEXP r_m,
    SEXP r_k,
    SEXP r_n,
    SEXP r_transpose_a,
    SEXP r_transpose_b
) {
    mojor_gpu_buf_f64_t* buf_out = mojor_gpu_buf_f64_get(r_out);
    mojor_gpu_buf_f64_t* buf_a = mojor_gpu_buf_f64_get(r_a);
    mojor_gpu_buf_f64_t* buf_b = mojor_gpu_buf_f64_get(r_b);
    if (!buf_out || !buf_out->handle || !buf_a || !buf_a->handle || !buf_b || !buf_b->handle) {
        error("mojor_gpu_buf_f64_matmul_into: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f64_matmul_into: GPU context unavailable");
    }
    int m = (TYPEOF(r_m) == INTSXP) ? INTEGER(r_m)[0] : (int) REAL(r_m)[0];
    int k = (TYPEOF(r_k) == INTSXP) ? INTEGER(r_k)[0] : (int) REAL(r_k)[0];
    int n = (TYPEOF(r_n) == INTSXP) ? INTEGER(r_n)[0] : (int) REAL(r_n)[0];
    int transpose_a = (TYPEOF(r_transpose_a) == INTSXP) ? INTEGER(r_transpose_a)[0] : (int) REAL(r_transpose_a)[0];
    int transpose_b = (TYPEOF(r_transpose_b) == INTSXP) ? INTEGER(r_transpose_b)[0] : (int) REAL(r_transpose_b)[0];
    int ok = mojor_gpu_buf_f64_matmul_into(ctx, buf_out->handle, buf_a->handle, buf_b->handle, m, k, n, transpose_a, transpose_b);
    if (!ok) {
        error("mojor_gpu_buf_f64_matmul_into: failed");
    }
    return ScalarLogical(1);
}

SEXP mojor_gpu_buf_f64_free_call(SEXP r_buf) {
    mojor_gpu_buf_f64_t* buf = mojor_gpu_buf_f64_get(r_buf);
    if (!buf) {
        return ScalarLogical(0);
    }
    int ok = 0;
    if (buf->handle) {
        ok = mojor_gpu_buf_f64_free(buf->ctx, buf->handle);
        if (ok) {
            mojor_gpu_handle_remove(buf->handle);
            buf->handle = NULL;
        }
    }
    buf->ctx = NULL;
    R_ClearExternalPtr(r_buf);
    return ScalarLogical(ok != 0);
}

SEXP mojor_gpu_buf_f64_len_call(SEXP r_buf) {
    mojor_gpu_buf_f64_t* buf = mojor_gpu_buf_f64_get(r_buf);
    if (!buf || !buf->handle) {
        return ScalarInteger(-1);
    }
    return ScalarInteger(buf->n);
}

SEXP mojor_gpu_buf_f64_write_call(SEXP r_buf, SEXP r_vec) {
    mojor_gpu_buf_f64_t* buf = mojor_gpu_buf_f64_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_f64_write: invalid buffer");
    }
    if (TYPEOF(r_vec) != REALSXP && TYPEOF(r_vec) != INTSXP) {
        error("mojor_gpu_buf_f64_write: expected numeric or integer vector");
    }
    int n = LENGTH(r_vec);
    if (n != buf->n) {
        error("mojor_gpu_buf_f64_write: length mismatch");
    }
    double* tmp = (double*) R_alloc((size_t) n, sizeof(double));
    if (TYPEOF(r_vec) == REALSXP) {
        double* src = REAL(r_vec);
        for (int i = 0; i < n; i++) {
            tmp[i] = src[i];
        }
    } else {
        int* src = INTEGER(r_vec);
        for (int i = 0; i < n; i++) {
            tmp[i] = (double) src[i];
        }
    }
    int ok = mojor_gpu_buf_f64_write(buf->ctx, buf->handle, tmp, n);
    if (!ok) {
        error("mojor_gpu_buf_f64_write: write failed");
    }
    return ScalarLogical(1);
}

SEXP mojor_gpu_buf_f64_read_call(SEXP r_buf) {
    mojor_gpu_buf_f64_t* buf = mojor_gpu_buf_f64_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_f64_read: invalid buffer");
    }
    int n = buf->n;
    SEXP out = PROTECT(allocVector(REALSXP, n));
    double* tmp = (double*) R_alloc((size_t) n, sizeof(double));
    int ok = mojor_gpu_buf_f64_read(buf->ctx, buf->handle, tmp, n);
    if (!ok) {
        UNPROTECT(1);
        error("mojor_gpu_buf_f64_read: read failed");
    }
    double* dst = REAL(out);
    for (int i = 0; i < n; i++) {
        dst[i] = tmp[i];
    }
    UNPROTECT(1);
    return out;
}

SEXP mojor_gpu_buf_f64_cast_f32_call(SEXP r_ctx, SEXP r_buf) {
    mojor_gpu_buf_f64_t* buf = mojor_gpu_buf_f64_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_f64_cast_f32: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f64_cast_f32: GPU context unavailable");
    }
    void* handle = mojor_gpu_buf_f64_cast_f32(ctx, buf->handle);
    if (!handle) {
        error("mojor_gpu_buf_f64_cast_f32: failed");
    }
    mojor_gpu_handle_add(handle);
    mojor_gpu_buf_f32_t* out = R_Calloc(1, mojor_gpu_buf_f32_t);
    out->ctx = ctx;
    out->handle = handle;
    out->n = mojor_gpu_buf_f32_len(ctx, handle);
    SEXP ext = PROTECT(R_MakeExternalPtr(out, install("mojor_gpu_buf_f32"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_buf_f32_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(out->n));
    setAttrib(ext, install("gpu_status"), ScalarInteger(1));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_buf_f64_cast_i32_call(SEXP r_ctx, SEXP r_buf) {
    mojor_gpu_buf_f64_t* buf = mojor_gpu_buf_f64_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_f64_cast_i32: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f64_cast_i32: GPU context unavailable");
    }
    void* handle = mojor_gpu_buf_f64_cast_i32(ctx, buf->handle);
    if (!handle) {
        error("mojor_gpu_buf_f64_cast_i32: failed");
    }
    mojor_gpu_handle_add(handle);
    mojor_gpu_buf_i32_t* out = R_Calloc(1, mojor_gpu_buf_i32_t);
    out->ctx = ctx;
    out->handle = handle;
    out->n = mojor_gpu_buf_i32_len(ctx, handle);
    SEXP ext = PROTECT(R_MakeExternalPtr(out, install("mojor_gpu_buf_i32"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_buf_i32_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(out->n));
    setAttrib(ext, install("gpu_status"), ScalarInteger(1));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_buf_i32_alloc_call(SEXP r_ctx, SEXP r_n) {
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_i32_alloc: GPU context unavailable");
    }
    int n = (TYPEOF(r_n) == INTSXP) ? INTEGER(r_n)[0] : (int) REAL(r_n)[0];
    if (n < 0) {
        error("mojor_gpu_buf_i32_alloc: n must be >= 0");
    }
    void* handle = mojor_gpu_buf_i32_alloc(ctx, n);
    if (!handle) {
        error("mojor_gpu_buf_i32_alloc: allocation failed");
    }
    mojor_gpu_handle_add(handle);
    mojor_gpu_buf_i32_t* buf = R_Calloc(1, mojor_gpu_buf_i32_t);
    buf->ctx = ctx;
    buf->handle = handle;
    buf->n = n;
    SEXP ext = PROTECT(R_MakeExternalPtr(buf, install("mojor_gpu_buf_i32"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_buf_i32_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(n));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_buf_i32_free_call(SEXP r_buf) {
    mojor_gpu_buf_i32_t* buf = mojor_gpu_buf_i32_get(r_buf);
    if (!buf) {
        return ScalarLogical(0);
    }
    int ok = 0;
    if (buf->handle) {
        ok = mojor_gpu_buf_i32_free(buf->ctx, buf->handle);
        if (ok) {
            mojor_gpu_handle_remove(buf->handle);
            buf->handle = NULL;
        }
    }
    buf->ctx = NULL;
    R_ClearExternalPtr(r_buf);
    return ScalarLogical(ok != 0);
}

SEXP mojor_gpu_buf_i32_len_call(SEXP r_buf) {
    mojor_gpu_buf_i32_t* buf = mojor_gpu_buf_i32_get(r_buf);
    if (!buf || !buf->handle) {
        return ScalarInteger(-1);
    }
    return ScalarInteger(buf->n);
}

SEXP mojor_gpu_buf_i32_write_call(SEXP r_buf, SEXP r_vec) {
    mojor_gpu_buf_i32_t* buf = mojor_gpu_buf_i32_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_i32_write: invalid buffer");
    }
    if (TYPEOF(r_vec) != INTSXP && TYPEOF(r_vec) != REALSXP) {
        error("mojor_gpu_buf_i32_write: expected integer or numeric vector");
    }
    int n = LENGTH(r_vec);
    if (n != buf->n) {
        error("mojor_gpu_buf_i32_write: length mismatch");
    }
    int32_t* tmp = (int32_t*) R_alloc((size_t) n, sizeof(int32_t));
    if (TYPEOF(r_vec) == INTSXP) {
        int* src = INTEGER(r_vec);
        for (int i = 0; i < n; i++) {
            tmp[i] = (int32_t) src[i];
        }
    } else {
        double* src = REAL(r_vec);
        for (int i = 0; i < n; i++) {
            tmp[i] = (int32_t) src[i];
        }
    }
    int ok = mojor_gpu_buf_i32_write(buf->ctx, buf->handle, tmp, n);
    if (!ok) {
        error("mojor_gpu_buf_i32_write: write failed");
    }
    return ScalarLogical(1);
}

SEXP mojor_gpu_buf_i32_read_call(SEXP r_buf) {
    mojor_gpu_buf_i32_t* buf = mojor_gpu_buf_i32_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_i32_read: invalid buffer");
    }
    int n = buf->n;
    SEXP out = PROTECT(allocVector(INTSXP, n));
    int32_t* tmp = (int32_t*) R_alloc((size_t) n, sizeof(int32_t));
    int ok = mojor_gpu_buf_i32_read(buf->ctx, buf->handle, tmp, n);
    if (!ok) {
        UNPROTECT(1);
        error("mojor_gpu_buf_i32_read: read failed");
    }
    int* dst = INTEGER(out);
    for (int i = 0; i < n; i++) {
        dst[i] = (int) tmp[i];
    }
    UNPROTECT(1);
    return out;
}

SEXP mojor_gpu_buf_i32_cast_f32_call(SEXP r_ctx, SEXP r_buf) {
    mojor_gpu_buf_i32_t* buf = mojor_gpu_buf_i32_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_i32_cast_f32: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_i32_cast_f32: GPU context unavailable");
    }
    void* handle = mojor_gpu_buf_i32_cast_f32(ctx, buf->handle);
    if (!handle) {
        error("mojor_gpu_buf_i32_cast_f32: failed");
    }
    mojor_gpu_handle_add(handle);
    mojor_gpu_buf_f32_t* out = R_Calloc(1, mojor_gpu_buf_f32_t);
    out->ctx = ctx;
    out->handle = handle;
    out->n = mojor_gpu_buf_f32_len(ctx, handle);
    SEXP ext = PROTECT(R_MakeExternalPtr(out, install("mojor_gpu_buf_f32"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_buf_f32_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(out->n));
    setAttrib(ext, install("gpu_status"), ScalarInteger(1));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_buf_i32_cast_f64_call(SEXP r_ctx, SEXP r_buf) {
    mojor_gpu_buf_i32_t* buf = mojor_gpu_buf_i32_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_i32_cast_f64: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_i32_cast_f64: GPU context unavailable");
    }
    void* handle = mojor_gpu_buf_i32_cast_f64(ctx, buf->handle);
    if (!handle) {
        error("mojor_gpu_buf_i32_cast_f64: failed");
    }
    mojor_gpu_handle_add(handle);
    mojor_gpu_buf_f64_t* out = R_Calloc(1, mojor_gpu_buf_f64_t);
    out->ctx = ctx;
    out->handle = handle;
    out->n = mojor_gpu_buf_f64_len(ctx, handle);
    SEXP ext = PROTECT(R_MakeExternalPtr(out, install("mojor_gpu_buf_f64"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_buf_f64_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(out->n));
    setAttrib(ext, install("gpu_status"), ScalarInteger(1));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_buf_f64_affine_call(
    SEXP r_ctx,
    SEXP r_buf,
    SEXP r_scale,
    SEXP r_bias
) {
    mojor_gpu_buf_f64_t* buf = mojor_gpu_buf_f64_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_f64_affine: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f64_affine: GPU context unavailable");
    }
    double scale = (TYPEOF(r_scale) == REALSXP) ? REAL(r_scale)[0] : (double) INTEGER(r_scale)[0];
    double bias = (TYPEOF(r_bias) == REALSXP) ? REAL(r_bias)[0] : (double) INTEGER(r_bias)[0];
    int status = 0;
    void* handle = mojor_gpu_buf_f64_affine(ctx, buf->handle, scale, bias, &status);
    if (!handle) {
        error("mojor_gpu_buf_f64_affine: failed (status=%d)", status);
    }
    mojor_gpu_handle_add(handle);
    mojor_gpu_buf_f64_t* out = R_Calloc(1, mojor_gpu_buf_f64_t);
    out->ctx = ctx;
    out->handle = handle;
    out->n = buf->n;
    SEXP ext = PROTECT(R_MakeExternalPtr(out, install("mojor_gpu_buf_f64"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_buf_f64_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(out->n));
    setAttrib(ext, install("gpu_status"), ScalarInteger(status));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_buf_f64_chain_call(
    SEXP r_ctx,
    SEXP r_buf,
    SEXP r_iters,
    SEXP r_scale,
    SEXP r_bias,
    SEXP r_post_scale,
    SEXP r_post_bias,
    SEXP r_post_iters
) {
    mojor_gpu_buf_f64_t* buf = mojor_gpu_buf_f64_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_f64_chain: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f64_chain: GPU context unavailable");
    }
    int iters = (TYPEOF(r_iters) == INTSXP) ? INTEGER(r_iters)[0] : (int) REAL(r_iters)[0];
    int post_iters = (TYPEOF(r_post_iters) == INTSXP) ? INTEGER(r_post_iters)[0] : (int) REAL(r_post_iters)[0];
    double scale = (TYPEOF(r_scale) == REALSXP) ? REAL(r_scale)[0] : (double) INTEGER(r_scale)[0];
    double bias = (TYPEOF(r_bias) == REALSXP) ? REAL(r_bias)[0] : (double) INTEGER(r_bias)[0];
    double post_scale = (TYPEOF(r_post_scale) == REALSXP) ? REAL(r_post_scale)[0] : (double) INTEGER(r_post_scale)[0];
    double post_bias = (TYPEOF(r_post_bias) == REALSXP) ? REAL(r_post_bias)[0] : (double) INTEGER(r_post_bias)[0];
    int status = 0;
    void* handle = mojor_gpu_buf_f64_chain(
        ctx,
        buf->handle,
        iters,
        scale,
        bias,
        post_scale,
        post_bias,
        post_iters,
        &status
    );
    if (!handle) {
        error("mojor_gpu_buf_f64_chain: failed (status=%d)", status);
    }
    mojor_gpu_handle_add(handle);
    mojor_gpu_buf_f64_t* out = R_Calloc(1, mojor_gpu_buf_f64_t);
    out->ctx = ctx;
    out->handle = handle;
    out->n = buf->n;
    SEXP ext = PROTECT(R_MakeExternalPtr(out, install("mojor_gpu_buf_f64"), R_NilValue));
    R_RegisterCFinalizerEx(ext, mojor_gpu_buf_f64_finalizer, TRUE);
    setAttrib(ext, install("n"), ScalarInteger(out->n));
    setAttrib(ext, install("gpu_status"), ScalarInteger(status));
    UNPROTECT(1);
    return ext;
}

SEXP mojor_gpu_buf_f64_chain_sum_call(
    SEXP r_ctx,
    SEXP r_buf,
    SEXP r_iters,
    SEXP r_scale,
    SEXP r_bias,
    SEXP r_post_scale,
    SEXP r_post_bias,
    SEXP r_post_iters
) {
    mojor_gpu_buf_f64_t* buf = mojor_gpu_buf_f64_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_f64_chain_sum: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f64_chain_sum: GPU context unavailable");
    }
    int iters = (TYPEOF(r_iters) == INTSXP) ? INTEGER(r_iters)[0] : (int) REAL(r_iters)[0];
    int post_iters = (TYPEOF(r_post_iters) == INTSXP) ? INTEGER(r_post_iters)[0] : (int) REAL(r_post_iters)[0];
    double scale = (TYPEOF(r_scale) == REALSXP) ? REAL(r_scale)[0] : (double) INTEGER(r_scale)[0];
    double bias = (TYPEOF(r_bias) == REALSXP) ? REAL(r_bias)[0] : (double) INTEGER(r_bias)[0];
    double post_scale = (TYPEOF(r_post_scale) == REALSXP) ? REAL(r_post_scale)[0] : (double) INTEGER(r_post_scale)[0];
    double post_bias = (TYPEOF(r_post_bias) == REALSXP) ? REAL(r_post_bias)[0] : (double) INTEGER(r_post_bias)[0];
    int status = 0;
    double result = mojor_gpu_buf_f64_chain_sum(
        ctx,
        buf->handle,
        iters,
        scale,
        bias,
        post_scale,
        post_bias,
        post_iters,
        &status
    );
    SEXP out = PROTECT(ScalarReal(result));
    setAttrib(out, install("gpu_status"), ScalarInteger(status));
    UNPROTECT(1);
    return out;
}

SEXP mojor_gpu_buf_f64_chain_sum_warp_call(
    SEXP r_ctx,
    SEXP r_buf,
    SEXP r_iters,
    SEXP r_scale,
    SEXP r_bias,
    SEXP r_post_scale,
    SEXP r_post_bias,
    SEXP r_post_iters
) {
    mojor_gpu_buf_f64_t* buf = mojor_gpu_buf_f64_get(r_buf);
    if (!buf || !buf->handle) {
        error("mojor_gpu_buf_f64_chain_sum_warp: invalid buffer");
    }
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        error("mojor_gpu_buf_f64_chain_sum_warp: GPU context unavailable");
    }
    int iters = (TYPEOF(r_iters) == INTSXP) ? INTEGER(r_iters)[0] : (int) REAL(r_iters)[0];
    int post_iters = (TYPEOF(r_post_iters) == INTSXP) ? INTEGER(r_post_iters)[0] : (int) REAL(r_post_iters)[0];
    double scale = (TYPEOF(r_scale) == REALSXP) ? REAL(r_scale)[0] : (double) INTEGER(r_scale)[0];
    double bias = (TYPEOF(r_bias) == REALSXP) ? REAL(r_bias)[0] : (double) INTEGER(r_bias)[0];
    double post_scale = (TYPEOF(r_post_scale) == REALSXP) ? REAL(r_post_scale)[0] : (double) INTEGER(r_post_scale)[0];
    double post_bias = (TYPEOF(r_post_bias) == REALSXP) ? REAL(r_post_bias)[0] : (double) INTEGER(r_post_bias)[0];
    int status = 0;
    double result = mojor_gpu_buf_f64_chain_sum_warp(
        ctx,
        buf->handle,
        iters,
        scale,
        bias,
        post_scale,
        post_bias,
        post_iters,
        &status
    );
    SEXP out = PROTECT(ScalarReal(result));
    setAttrib(out, install("gpu_status"), ScalarInteger(status));
    UNPROTECT(1);
    return out;
}

SEXP mojor_gpu_buf_f32_live_count_call() {
    return ScalarInteger(mojor_gpu_handle_count);
}

SEXP mojor_gpu_session_create_call(SEXP r_ctx, SEXP r_n) {
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        return ScalarInteger(-1);
    }
    int n = (TYPEOF(r_n) == INTSXP) ? INTEGER(r_n)[0] : (int) REAL(r_n)[0];
    int status = mojor_gpu_session_create(ctx, n);
    return ScalarInteger(status);
}

SEXP mojor_gpu_session_free_call(SEXP r_ctx) {
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    if (!ctx) {
        return ScalarInteger(0);
    }
    int status = mojor_gpu_session_free(ctx);
    return ScalarInteger(status);
}

SEXP mojor_gpu_session_chain_int_call(
    SEXP r_ctx,
    SEXP r_int,
    SEXP r_iters,
    SEXP r_scale,
    SEXP r_bias,
    SEXP r_post_scale,
    SEXP r_post_bias,
    SEXP r_post_iters
) {
    if (TYPEOF(r_int) != INTSXP) {
        error("mojor_gpu_session_chain: expected integer vector (float bits)");
    }
    int iters = (TYPEOF(r_iters) == INTSXP) ? INTEGER(r_iters)[0] : (int) REAL(r_iters)[0];
    int post_iters = (TYPEOF(r_post_iters) == INTSXP) ? INTEGER(r_post_iters)[0] : (int) REAL(r_post_iters)[0];
    float scale = (TYPEOF(r_scale) == REALSXP) ? (float) REAL(r_scale)[0] : (float) INTEGER(r_scale)[0];
    float bias = (TYPEOF(r_bias) == REALSXP) ? (float) REAL(r_bias)[0] : (float) INTEGER(r_bias)[0];
    float post_scale = (TYPEOF(r_post_scale) == REALSXP) ? (float) REAL(r_post_scale)[0] : (float) INTEGER(r_post_scale)[0];
    float post_bias = (TYPEOF(r_post_bias) == REALSXP) ? (float) REAL(r_post_bias)[0] : (float) INTEGER(r_post_bias)[0];
    int n = LENGTH(r_int);
    PROTECT(r_int);
    SEXP result = PROTECT(allocVector(INTSXP, n));
    float* in_ptr = (float*) INTEGER(r_int);
    float* out_ptr = (float*) INTEGER(result);
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    int status = ctx ? mojor_gpu_session_chain_f32(
        ctx,
        in_ptr,
        out_ptr,
        n,
        iters,
        scale,
        bias,
        post_scale,
        post_bias,
        post_iters
    ) : -1;
    setAttrib(result, install("gpu_status"), ScalarInteger(status));
    UNPROTECT(2);
    return result;
}

SEXP mojor_gpu_session_chain_sum_int_call(
    SEXP r_ctx,
    SEXP r_int,
    SEXP r_iters,
    SEXP r_scale,
    SEXP r_bias,
    SEXP r_post_scale,
    SEXP r_post_bias,
    SEXP r_post_iters
) {
    if (TYPEOF(r_int) != INTSXP) {
        error("mojor_gpu_session_chain_sum: expected integer vector (float bits)");
    }
    int iters = (TYPEOF(r_iters) == INTSXP) ? INTEGER(r_iters)[0] : (int) REAL(r_iters)[0];
    int post_iters = (TYPEOF(r_post_iters) == INTSXP) ? INTEGER(r_post_iters)[0] : (int) REAL(r_post_iters)[0];
    float scale = (TYPEOF(r_scale) == REALSXP) ? (float) REAL(r_scale)[0] : (float) INTEGER(r_scale)[0];
    float bias = (TYPEOF(r_bias) == REALSXP) ? (float) REAL(r_bias)[0] : (float) INTEGER(r_bias)[0];
    float post_scale = (TYPEOF(r_post_scale) == REALSXP) ? (float) REAL(r_post_scale)[0] : (float) INTEGER(r_post_scale)[0];
    float post_bias = (TYPEOF(r_post_bias) == REALSXP) ? (float) REAL(r_post_bias)[0] : (float) INTEGER(r_post_bias)[0];
    int n = LENGTH(r_int);
    PROTECT(r_int);
    float* in_ptr = (float*) INTEGER(r_int);
    int status = 0;
    void* ctx = mojor_gpu_ctx_get(r_ctx);
    float result = ctx ? mojor_gpu_session_chain_sum_f32(
        ctx,
        in_ptr,
        n,
        iters,
        scale,
        bias,
        post_scale,
        post_bias,
        post_iters,
        &status
    ) : 0.0f;
    SEXP out = PROTECT(ScalarReal((double) result));
    setAttrib(out, install("gpu_status"), ScalarInteger(ctx ? status : -1));
    UNPROTECT(2);
    return out;
}

#define MOJOR_SIMPLE_CALLS(X) \
    X(mojor_scale_f64_call, mojor_scale_f64, 2) \
    X(mojor_abs_f64_inplace_call, mojor_abs_f64_inplace, 1) \
    X(mojor_running_max_f64_call, mojor_running_max_f64, 1) \
    X(mojor_count_runs_f64_call, mojor_count_runs_f64, 2) \
    X(mojor_sigmoid_f64_call, mojor_sigmoid_f64, 1)

#define MOJOR_RNG_CALLS(X) \
    X(mojor_runif_call, mojor_runif, 1) \
    X(mojor_runif_range_call, mojor_runif_range, 3) \
    X(mojor_rnorm_call, mojor_rnorm, 1) \
    X(mojor_rnorm_mean_sd_call, mojor_rnorm_mean_sd, 3) \
    X(mojor_rgamma_call, mojor_rgamma, 3) \
    X(mojor_rbinom_call, mojor_rbinom, 3) \
    X(mojor_rexp_call, mojor_rexp, 2) \
    X(mojor_rpois_call, mojor_rpois, 2) \
    X(mojor_rlnorm_call, mojor_rlnorm, 3) \
    X(mojor_rchisq_call, mojor_rchisq, 2) \
    X(mojor_rt_call, mojor_rt, 2) \
    X(mojor_rf_call, mojor_rf, 3) \
    X(mojor_rbeta_call, mojor_rbeta, 3) \
    X(mojor_rweibull_call, mojor_rweibull, 3) \
    X(mojor_rlogis_call, mojor_rlogis, 3) \
    X(mojor_rcauchy_call, mojor_rcauchy, 3) \
    X(mojor_rgeom_call, mojor_rgeom, 2) \
    X(mojor_rnbinom_call, mojor_rnbinom, 3)

#define MOJOR_GPU_CAP_CALLS(X) \
    X(mojor_gpu_cap_f64_matmul_call, mojor_gpu_cap_f64_matmul, 1) \
    X(mojor_gpu_cap_f64_reduce_call, mojor_gpu_cap_f64_reduce, 2) \
    X(mojor_gpu_cap_i32_scatter_call, mojor_gpu_cap_i32_scatter, 1)

#define MOJOR_GPU_BUF_F32_CALLS(X) \
    X(mojor_gpu_buf_f32_alloc_call, mojor_gpu_buf_f32_alloc, 2) \
    X(mojor_gpu_buf_f32_free_call, mojor_gpu_buf_f32_free, 1) \
    X(mojor_gpu_buf_f32_len_call, mojor_gpu_buf_f32_len, 1) \
    X(mojor_gpu_buf_f32_write_call, mojor_gpu_buf_f32_write, 2) \
    X(mojor_gpu_buf_f32_read_call, mojor_gpu_buf_f32_read, 1) \
    X(mojor_gpu_buf_f32_cast_f64_call, mojor_gpu_buf_f32_cast_f64, 2) \
    X(mojor_gpu_buf_f32_cast_i32_call, mojor_gpu_buf_f32_cast_i32, 2) \
    X(mojor_gpu_buf_f32_affine_call, mojor_gpu_buf_f32_affine, 4) \
    X(mojor_gpu_buf_f32_chain_call, mojor_gpu_buf_f32_chain, 8) \
    X(mojor_gpu_buf_f32_chain_sum_call, mojor_gpu_buf_f32_chain_sum, 8) \
    X(mojor_gpu_buf_f32_chain_sum_warp_call, mojor_gpu_buf_f32_chain_sum_warp, 8) \
    X(mojor_gpu_buf_f32_reduce_call, mojor_gpu_buf_f32_reduce, 6) \
    X(mojor_gpu_buf_f32_matmul_call, mojor_gpu_buf_f32_matmul, 8) \
    X(mojor_gpu_buf_f32_matmul_into_call, mojor_gpu_buf_f32_matmul_into, 9) \
    X(mojor_gpu_buf_f32_slice_call, mojor_gpu_buf_f32_slice, 5) \
    X(mojor_gpu_idx_plan_f32_create_call, mojor_gpu_idx_plan_f32_create, 5) \
    X(mojor_gpu_idx_plan_f32_free_call, mojor_gpu_idx_plan_f32_free, 1) \
    X(mojor_gpu_buf_f32_gather_plan_call, mojor_gpu_buf_f32_gather_plan, 3) \
    X(mojor_gpu_buf_f32_scatter_plan_call, mojor_gpu_buf_f32_scatter_plan, 4) \
    X(mojor_gpu_buf_f32_gather_call, mojor_gpu_buf_f32_gather, 6) \
    X(mojor_gpu_buf_f32_scatter_call, mojor_gpu_buf_f32_scatter, 7)

#define MOJOR_GPU_BUF_F64_CALLS(X) \
    X(mojor_gpu_buf_f64_alloc_call, mojor_gpu_buf_f64_alloc, 2) \
    X(mojor_gpu_buf_f64_free_call, mojor_gpu_buf_f64_free, 1) \
    X(mojor_gpu_buf_f64_len_call, mojor_gpu_buf_f64_len, 1) \
    X(mojor_gpu_buf_f64_write_call, mojor_gpu_buf_f64_write, 2) \
    X(mojor_gpu_buf_f64_read_call, mojor_gpu_buf_f64_read, 1) \
    X(mojor_gpu_buf_f64_cast_f32_call, mojor_gpu_buf_f64_cast_f32, 2) \
    X(mojor_gpu_buf_f64_cast_i32_call, mojor_gpu_buf_f64_cast_i32, 2) \
    X(mojor_gpu_buf_f64_affine_call, mojor_gpu_buf_f64_affine, 4) \
    X(mojor_gpu_buf_f64_chain_call, mojor_gpu_buf_f64_chain, 8) \
    X(mojor_gpu_buf_f64_chain_sum_call, mojor_gpu_buf_f64_chain_sum, 8) \
    X(mojor_gpu_buf_f64_chain_sum_warp_call, mojor_gpu_buf_f64_chain_sum_warp, 8) \
    X(mojor_gpu_buf_f64_reduce_call, mojor_gpu_buf_f64_reduce, 6) \
    X(mojor_gpu_buf_f64_matmul_call, mojor_gpu_buf_f64_matmul, 8) \
    X(mojor_gpu_buf_f64_matmul_into_call, mojor_gpu_buf_f64_matmul_into, 9) \
    X(mojor_gpu_buf_f64_slice_call, mojor_gpu_buf_f64_slice, 5) \
    X(mojor_gpu_idx_plan_f64_create_call, mojor_gpu_idx_plan_f64_create, 5) \
    X(mojor_gpu_idx_plan_f64_free_call, mojor_gpu_idx_plan_f64_free, 1) \
    X(mojor_gpu_buf_f64_gather_plan_call, mojor_gpu_buf_f64_gather_plan, 3) \
    X(mojor_gpu_buf_f64_scatter_plan_call, mojor_gpu_buf_f64_scatter_plan, 4) \
    X(mojor_gpu_buf_f64_gather_call, mojor_gpu_buf_f64_gather, 6) \
    X(mojor_gpu_buf_f64_scatter_call, mojor_gpu_buf_f64_scatter, 7)

#define MOJOR_GPU_BUF_I32_CALLS(X) \
    X(mojor_gpu_buf_i32_alloc_call, mojor_gpu_buf_i32_alloc, 2) \
    X(mojor_gpu_buf_i32_free_call, mojor_gpu_buf_i32_free, 1) \
    X(mojor_gpu_buf_i32_len_call, mojor_gpu_buf_i32_len, 1) \
    X(mojor_gpu_buf_i32_write_call, mojor_gpu_buf_i32_write, 2) \
    X(mojor_gpu_buf_i32_read_call, mojor_gpu_buf_i32_read, 1) \
    X(mojor_gpu_buf_i32_cast_f32_call, mojor_gpu_buf_i32_cast_f32, 2) \
    X(mojor_gpu_buf_i32_cast_f64_call, mojor_gpu_buf_i32_cast_f64, 2) \
    X(mojor_gpu_buf_i32_reduce_call, mojor_gpu_buf_i32_reduce, 6) \
    X(mojor_gpu_buf_i32_matmul_call, mojor_gpu_buf_i32_matmul, 8) \
    X(mojor_gpu_buf_i32_matmul_into_call, mojor_gpu_buf_i32_matmul_into, 9) \
    X(mojor_gpu_buf_i32_slice_call, mojor_gpu_buf_i32_slice, 5) \
    X(mojor_gpu_buf_i32_slice_assign_call, mojor_gpu_buf_i32_slice_assign, 6) \
    X(mojor_gpu_buf_i32_gather_call, mojor_gpu_buf_i32_gather, 6) \
    X(mojor_gpu_buf_i32_scatter_call, mojor_gpu_buf_i32_scatter, 7)

#define MOJOR_CALL_ENTRY(fn, name, nargs) { #name, (DL_FUNC) &fn, nargs },
#define MOJOR_CALL_ENTRY_RT_V1(fn, name, nargs) { "mojor_rt_v1_" #name, (DL_FUNC) &fn, nargs },

// BinCount (histogram) wrapper
SEXP mojor_bincount_f64_call(SEXP r_x, SEXP r_breaks, SEXP r_counts, SEXP r_n, SEXP r_nbreaks, SEXP r_right, SEXP r_include_lowest) {
    if (TYPEOF(r_x) != REALSXP) {
        error("mojor_bincount_f64: x must be numeric");
    }
    if (TYPEOF(r_breaks) != REALSXP) {
        error("mojor_bincount_f64: breaks must be numeric");
    }
    if (TYPEOF(r_counts) != INTSXP) {
        error("mojor_bincount_f64: counts must be integer");
    }
    
    PROTECT(r_x);
    PROTECT(r_breaks);
    PROTECT(r_counts);
    
    double* x = REAL(r_x);
    double* breaks = REAL(r_breaks);
    int* counts = INTEGER(r_counts);
    int n = asInteger(r_n);
    int nbreaks = asInteger(r_nbreaks);
    int right = asInteger(r_right);
    int include_lowest = asInteger(r_include_lowest);
    
    // Zero the counts array
    int nbins = nbreaks - 1;
    for (int i = 0; i < nbins; i++) {
        counts[i] = 0;
    }
    
    mojor_bincount_f64(x, breaks, counts, n, nbreaks, right, include_lowest);
    
    UNPROTECT(3);
    return r_counts;
}

// RNG wrappers
SEXP mojor_rng_seed_call(SEXP r_seed) {
    uint64_t seed = (uint64_t) asInteger(r_seed);
    mojor_rng_seed_state(seed);
    return R_NilValue;
}

static void mojor_rng_validate_n(int n, const char* fn_name) {
    if (n < 0) {
        error("%s: n must be non-negative", fn_name);
    }
}

static int mojor_rng_prepare_result(int n, SEXP* out) {
    if (n == 0) {
        *out = allocVector(REALSXP, 0);
        return 0;
    }
    *out = PROTECT(allocVector(REALSXP, n));
    mojor_rng_ensure_seeded();
    return 1;
}

static void mojor_rng_validate_nonneg_double_lt(double value, const char* fn_name, const char* arg_name) {
    if (value < 0) {
        error("%s: %s must be non-negative", fn_name, arg_name);
    }
}

static void mojor_rng_validate_nonneg_int_lt(int value, const char* fn_name, const char* arg_name) {
    if (value < 0) {
        error("%s: %s must be non-negative", fn_name, arg_name);
    }
}

static void mojor_rng_validate_positive_leq(double value, const char* fn_name, const char* arg_name) {
    if (value <= 0) {
        error("%s: %s must be positive", fn_name, arg_name);
    }
}

static void mojor_rng_validate_positive_gt(double value, const char* fn_name, const char* arg_name) {
    if (!(value > 0)) {
        error("%s: %s must be positive", fn_name, arg_name);
    }
}

static void mojor_rng_validate_prob_0_1_closed(double prob, const char* fn_name) {
    if (prob < 0 || prob > 1) {
        error("%s: prob must be between 0 and 1", fn_name);
    }
}

static void mojor_rng_validate_prob_0_1_open_closed(double prob, const char* fn_name) {
    if (!(prob > 0 && prob <= 1)) {
        error("%s: prob must be in (0, 1]", fn_name);
    }
}

#define MOJOR_RNG_VALIDATE_NONE ((void)0)
#define MOJOR_RNG_VALIDATE_NONNEG_DBL(NAME, ARG) mojor_rng_validate_nonneg_double_lt((ARG), #NAME, #ARG)
#define MOJOR_RNG_VALIDATE_NONNEG_INT(NAME, ARG) mojor_rng_validate_nonneg_int_lt((ARG), #NAME, #ARG)
#define MOJOR_RNG_VALIDATE_POS_LEQ(NAME, ARG) mojor_rng_validate_positive_leq((ARG), #NAME, #ARG)
#define MOJOR_RNG_VALIDATE_POS_GT(NAME, ARG) mojor_rng_validate_positive_gt((ARG), #NAME, #ARG)
#define MOJOR_RNG_VALIDATE_PROB_CLOSED(NAME, ARG) mojor_rng_validate_prob_0_1_closed((ARG), #NAME)
#define MOJOR_RNG_VALIDATE_PROB_OPEN_CLOSED(NAME, ARG) mojor_rng_validate_prob_0_1_open_closed((ARG), #NAME)

#define MOJOR_RNG_CALL0(NAME, CALL_EXPR) \
SEXP NAME##_call(SEXP r_n) { \
    int n = asInteger(r_n); \
    SEXP result = R_NilValue; \
    mojor_rng_validate_n(n, #NAME); \
    if (!mojor_rng_prepare_result(n, &result)) { \
        return result; \
    } \
    CALL_EXPR; \
    UNPROTECT(1); \
    return result; \
}

#define MOJOR_RNG_CALL1_DBL(NAME, ARG1, VALIDATE_STMT, CALL_EXPR) \
SEXP NAME##_call(SEXP r_n, SEXP r_##ARG1) { \
    int n = asInteger(r_n); \
    double ARG1 = asReal(r_##ARG1); \
    SEXP result = R_NilValue; \
    mojor_rng_validate_n(n, #NAME); \
    VALIDATE_STMT; \
    if (!mojor_rng_prepare_result(n, &result)) { \
        return result; \
    } \
    CALL_EXPR; \
    UNPROTECT(1); \
    return result; \
}

#define MOJOR_RNG_CALL2_DBL(NAME, ARG1, ARG2, VALIDATE_STMT, CALL_EXPR) \
SEXP NAME##_call(SEXP r_n, SEXP r_##ARG1, SEXP r_##ARG2) { \
    int n = asInteger(r_n); \
    double ARG1 = asReal(r_##ARG1); \
    double ARG2 = asReal(r_##ARG2); \
    SEXP result = R_NilValue; \
    mojor_rng_validate_n(n, #NAME); \
    VALIDATE_STMT; \
    if (!mojor_rng_prepare_result(n, &result)) { \
        return result; \
    } \
    CALL_EXPR; \
    UNPROTECT(1); \
    return result; \
}

#define MOJOR_RNG_CALL1_INT1_DBL(NAME, ARG1_INT, ARG2_DBL, VALIDATE_STMT, CALL_EXPR) \
SEXP NAME##_call(SEXP r_n, SEXP r_##ARG1_INT, SEXP r_##ARG2_DBL) { \
    int n = asInteger(r_n); \
    int ARG1_INT = asInteger(r_##ARG1_INT); \
    double ARG2_DBL = asReal(r_##ARG2_DBL); \
    SEXP result = R_NilValue; \
    mojor_rng_validate_n(n, #NAME); \
    VALIDATE_STMT; \
    if (!mojor_rng_prepare_result(n, &result)) { \
        return result; \
    } \
    CALL_EXPR; \
    UNPROTECT(1); \
    return result; \
}

#define MOJOR_RNG_CALL1_INT(NAME, ARG1_INT, VALIDATE_STMT, CALL_EXPR) \
SEXP NAME##_call(SEXP r_n, SEXP r_##ARG1_INT) { \
    int n = asInteger(r_n); \
    int ARG1_INT = asInteger(r_##ARG1_INT); \
    SEXP result = R_NilValue; \
    mojor_rng_validate_n(n, #NAME); \
    VALIDATE_STMT; \
    if (!mojor_rng_prepare_result(n, &result)) { \
        return result; \
    } \
    CALL_EXPR; \
    UNPROTECT(1); \
    return result; \
}

#define MOJOR_RNG_CALL2_INT(NAME, ARG1_INT, ARG2_INT, VALIDATE_STMT, CALL_EXPR) \
SEXP NAME##_call(SEXP r_n, SEXP r_##ARG1_INT, SEXP r_##ARG2_INT) { \
    int n = asInteger(r_n); \
    int ARG1_INT = asInteger(r_##ARG1_INT); \
    int ARG2_INT = asInteger(r_##ARG2_INT); \
    SEXP result = R_NilValue; \
    mojor_rng_validate_n(n, #NAME); \
    VALIDATE_STMT; \
    if (!mojor_rng_prepare_result(n, &result)) { \
        return result; \
    } \
    CALL_EXPR; \
    UNPROTECT(1); \
    return result; \
}

#define MOJOR_RNG_CALL3_INT(NAME, ARG1_INT, ARG2_INT, ARG3_INT, VALIDATE_STMT, CALL_EXPR) \
SEXP NAME##_call(SEXP r_n, SEXP r_##ARG1_INT, SEXP r_##ARG2_INT, SEXP r_##ARG3_INT) { \
    int n = asInteger(r_n); \
    int ARG1_INT = asInteger(r_##ARG1_INT); \
    int ARG2_INT = asInteger(r_##ARG2_INT); \
    int ARG3_INT = asInteger(r_##ARG3_INT); \
    SEXP result = R_NilValue; \
    mojor_rng_validate_n(n, #NAME); \
    VALIDATE_STMT; \
    if (!mojor_rng_prepare_result(n, &result)) { \
        return result; \
    } \
    CALL_EXPR; \
    UNPROTECT(1); \
    return result; \
}

MOJOR_RNG_CALL0(mojor_runif, mojor_runif(REAL(result), n, mojor_rng_state))
MOJOR_RNG_CALL2_DBL(mojor_runif_range, min_val, max_val, MOJOR_RNG_VALIDATE_NONE, mojor_runif_range(REAL(result), n, min_val, max_val, mojor_rng_state))
MOJOR_RNG_CALL0(mojor_rnorm, mojor_rnorm(REAL(result), n, mojor_rng_state))
MOJOR_RNG_CALL2_DBL(mojor_rnorm_mean_sd, mean, sd,
    MOJOR_RNG_VALIDATE_NONNEG_DBL(mojor_rnorm_mean_sd, sd),
    mojor_rnorm_mean_sd(REAL(result), n, mean, sd, mojor_rng_state)
)
MOJOR_RNG_CALL2_DBL(mojor_rgamma, shape, rate,
    do {
        MOJOR_RNG_VALIDATE_POS_LEQ(mojor_rgamma, shape);
        MOJOR_RNG_VALIDATE_POS_LEQ(mojor_rgamma, rate);
    } while (0),
    mojor_rgamma(REAL(result), n, shape, rate, mojor_rng_state)
)
MOJOR_RNG_CALL1_INT1_DBL(mojor_rbinom, size, prob,
    do {
        MOJOR_RNG_VALIDATE_NONNEG_INT(mojor_rbinom, size);
        MOJOR_RNG_VALIDATE_PROB_CLOSED(mojor_rbinom, prob);
    } while (0),
    mojor_rbinom(REAL(result), n, size, prob, mojor_rng_state)
)
MOJOR_RNG_CALL1_DBL(mojor_rexp, rate,
    MOJOR_RNG_VALIDATE_POS_GT(mojor_rexp, rate),
    mojor_rexp(REAL(result), n, rate, mojor_rng_state)
)
MOJOR_RNG_CALL1_DBL(mojor_rpois, lambda,
    MOJOR_RNG_VALIDATE_NONNEG_DBL(mojor_rpois, lambda),
    mojor_rpois(REAL(result), n, lambda, mojor_rng_state)
)
MOJOR_RNG_CALL2_DBL(mojor_rlnorm, meanlog, sdlog,
    MOJOR_RNG_VALIDATE_NONNEG_DBL(mojor_rlnorm, sdlog),
    mojor_rlnorm(REAL(result), n, meanlog, sdlog, mojor_rng_state)
)
MOJOR_RNG_CALL1_DBL(mojor_rchisq, df,
    MOJOR_RNG_VALIDATE_POS_GT(mojor_rchisq, df),
    mojor_rchisq(REAL(result), n, df, mojor_rng_state)
)
MOJOR_RNG_CALL1_DBL(mojor_rt, df,
    MOJOR_RNG_VALIDATE_POS_GT(mojor_rt, df),
    mojor_rt(REAL(result), n, df, mojor_rng_state)
)
MOJOR_RNG_CALL2_DBL(mojor_rf, df1, df2,
    do {
        MOJOR_RNG_VALIDATE_POS_GT(mojor_rf, df1);
        MOJOR_RNG_VALIDATE_POS_GT(mojor_rf, df2);
    } while (0),
    mojor_rf(REAL(result), n, df1, df2, mojor_rng_state)
)
MOJOR_RNG_CALL2_DBL(mojor_rbeta, shape1, shape2,
    do {
        MOJOR_RNG_VALIDATE_POS_GT(mojor_rbeta, shape1);
        MOJOR_RNG_VALIDATE_POS_GT(mojor_rbeta, shape2);
    } while (0),
    mojor_rbeta(REAL(result), n, shape1, shape2, mojor_rng_state)
)
MOJOR_RNG_CALL2_DBL(mojor_rweibull, shape, scale,
    do {
        MOJOR_RNG_VALIDATE_POS_GT(mojor_rweibull, shape);
        MOJOR_RNG_VALIDATE_POS_GT(mojor_rweibull, scale);
    } while (0),
    mojor_rweibull(REAL(result), n, shape, scale, mojor_rng_state)
)
MOJOR_RNG_CALL2_DBL(mojor_rlogis, location, scale,
    MOJOR_RNG_VALIDATE_POS_GT(mojor_rlogis, scale),
    mojor_rlogis(REAL(result), n, location, scale, mojor_rng_state)
)
MOJOR_RNG_CALL2_DBL(mojor_rcauchy, location, scale,
    MOJOR_RNG_VALIDATE_POS_GT(mojor_rcauchy, scale),
    mojor_rcauchy(REAL(result), n, location, scale, mojor_rng_state)
)
MOJOR_RNG_CALL1_DBL(mojor_rgeom, prob,
    MOJOR_RNG_VALIDATE_PROB_OPEN_CLOSED(mojor_rgeom, prob),
    mojor_rgeom(REAL(result), n, prob, mojor_rng_state)
)
MOJOR_RNG_CALL2_DBL(mojor_rnbinom, size, prob,
    do {
        MOJOR_RNG_VALIDATE_NONNEG_DBL(mojor_rnbinom, size);
        MOJOR_RNG_VALIDATE_PROB_OPEN_CLOSED(mojor_rnbinom, prob);
    } while (0),
    mojor_rnbinom(REAL(result), n, size, prob, mojor_rng_state)
)
MOJOR_RNG_CALL3_INT(mojor_rhyper, m, n_bad, k,
    do {
        MOJOR_RNG_VALIDATE_NONNEG_INT(mojor_rhyper, m);
        MOJOR_RNG_VALIDATE_NONNEG_INT(mojor_rhyper, n_bad);
        MOJOR_RNG_VALIDATE_NONNEG_INT(mojor_rhyper, k);
        if (k > (m + n_bad)) {
            error("mojor_rhyper: k cannot exceed m + n_bad");
        }
    } while (0),
    mojor_rhyper(REAL(result), n, m, n_bad, k, mojor_rng_state)
)
MOJOR_RNG_CALL1_INT(mojor_rsignrank, n_sign,
    MOJOR_RNG_VALIDATE_NONNEG_INT(mojor_rsignrank, n_sign),
    mojor_rsignrank(REAL(result), n, n_sign, mojor_rng_state)
)
MOJOR_RNG_CALL2_INT(mojor_rwilcox, m, n_w,
    do {
        MOJOR_RNG_VALIDATE_NONNEG_INT(mojor_rwilcox, m);
        MOJOR_RNG_VALIDATE_NONNEG_INT(mojor_rwilcox, n_w);
    } while (0),
    mojor_rwilcox(REAL(result), n, m, n_w, mojor_rng_state)
)

#undef MOJOR_RNG_CALL3_INT
#undef MOJOR_RNG_CALL2_INT
#undef MOJOR_RNG_CALL1_INT
#undef MOJOR_RNG_CALL1_INT1_DBL
#undef MOJOR_RNG_CALL2_DBL
#undef MOJOR_RNG_CALL1_DBL
#undef MOJOR_RNG_CALL0
#undef MOJOR_RNG_VALIDATE_PROB_OPEN_CLOSED
#undef MOJOR_RNG_VALIDATE_PROB_CLOSED
#undef MOJOR_RNG_VALIDATE_POS_GT
#undef MOJOR_RNG_VALIDATE_POS_LEQ
#undef MOJOR_RNG_VALIDATE_NONNEG_INT
#undef MOJOR_RNG_VALIDATE_NONNEG_DBL
#undef MOJOR_RNG_VALIDATE_NONE

SEXP mojor_dnorm_fast_call(SEXP r_x, SEXP r_mean, SEXP r_sd, SEXP r_log) {
    if (TYPEOF(r_x) != REALSXP) {
        error("mojor_dnorm_fast: x must be numeric");
    }
    int give_log = asLogical(r_log);
    if (give_log == NA_LOGICAL) {
        error("mojor_dnorm_fast: log must be TRUE or FALSE");
    }

    int n = LENGTH(r_x);
    double mean = asReal(r_mean);
    double sd = asReal(r_sd);
    SEXP out = PROTECT(allocVector(REALSXP, n));
    double* x = REAL(r_x);
    double* y = REAL(out);
    for (int i = 0; i < n; ++i) {
        y[i] = dnorm4(x[i], mean, sd, give_log);
    }
    UNPROTECT(1);
    return out;
}

SEXP mojor_dpois_fast_call(SEXP r_x, SEXP r_lambda, SEXP r_log) {
    if (TYPEOF(r_x) != REALSXP) {
        error("mojor_dpois_fast: x must be numeric");
    }
    int give_log = asLogical(r_log);
    if (give_log == NA_LOGICAL) {
        error("mojor_dpois_fast: log must be TRUE or FALSE");
    }

    int n = LENGTH(r_x);
    double lambda = asReal(r_lambda);
    SEXP out = PROTECT(allocVector(REALSXP, n));
    double* x = REAL(r_x);
    double* y = REAL(out);
    for (int i = 0; i < n; ++i) {
        y[i] = dpois(x[i], lambda, give_log);
    }
    UNPROTECT(1);
    return out;
}

static int mojor_tier9_contains_literal(const char* haystack, const char* needle) {
    if (needle == NULL || needle[0] == '\0') {
        return 1;
    }
    return strstr(haystack, needle) != NULL;
}

static SEXP mojor_tier9_replace_literal_one(SEXP ch, const char* pattern, const char* replacement, int replace_all) {
    if (ch == NA_STRING) {
        return NA_STRING;
    }
    const char* src = CHAR(ch);
    size_t src_len = strlen(src);
    size_t pat_len = strlen(pattern);
    size_t rep_len = strlen(replacement);
    if (pat_len == 0) {
        return ch;
    }
    const char* first = strstr(src, pattern);
    if (first == NULL) {
        return ch;
    }
    if (!replace_all) {
        size_t prefix = (size_t)(first - src);
        size_t out_len = src_len - pat_len + rep_len;
        if (out_len > (size_t) INT_MAX) {
            error("Tier 9 regex runtime bridge: output too large");
        }
        char* out = (char*) R_alloc(out_len + 1, sizeof(char));
        memcpy(out, src, prefix);
        memcpy(out + prefix, replacement, rep_len);
        memcpy(out + prefix + rep_len, first + pat_len, src_len - prefix - pat_len);
        out[out_len] = '\0';
        return Rf_mkCharLen(out, (int) out_len);
    }
    size_t count = 0;
    const char* p = src;
    while ((p = strstr(p, pattern)) != NULL) {
        count++;
        p += pat_len;
    }
    if (count == 0) {
        return ch;
    }
    size_t out_len = src_len;
    if (rep_len >= pat_len) {
        out_len += (rep_len - pat_len) * count;
    } else {
        out_len -= (pat_len - rep_len) * count;
    }
    if (out_len > (size_t) INT_MAX) {
        error("Tier 9 regex runtime bridge: output too large");
    }
    char* out = (char*) R_alloc(out_len + 1, sizeof(char));
    const char* src_p = src;
    char* dst_p = out;
    const char* hit;
    while ((hit = strstr(src_p, pattern)) != NULL) {
        size_t n = (size_t)(hit - src_p);
        memcpy(dst_p, src_p, n);
        dst_p += n;
        memcpy(dst_p, replacement, rep_len);
        dst_p += rep_len;
        src_p = hit + pat_len;
    }
    {
        size_t tail = strlen(src_p);
        memcpy(dst_p, src_p, tail);
        dst_p += tail;
    }
    *dst_p = '\0';
    return Rf_mkCharLen(out, (int) out_len);
}

SEXP mojor_tier9_regex_runtime_call(SEXP r_op, SEXP r_pattern, SEXP r_replacement, SEXP r_x, SEXP r_fixed, SEXP r_perl) {
    if (TYPEOF(r_op) != STRSXP || XLENGTH(r_op) != 1) {
        error("Tier 9 regex runtime bridge: op must be a scalar character");
    }
    if (TYPEOF(r_pattern) != STRSXP || XLENGTH(r_pattern) != 1) {
        error("Tier 9 regex runtime bridge: pattern must be a scalar character");
    }
    if (TYPEOF(r_x) != STRSXP) {
        error("Tier 9 regex runtime bridge: x must be a character vector");
    }
    int fixed = asLogical(r_fixed);
    int perl = asLogical(r_perl);
    if (fixed == NA_LOGICAL || perl == NA_LOGICAL) {
        error("Tier 9 regex runtime bridge: fixed/perl must be TRUE or FALSE");
    }
    if (fixed && perl) {
        error("Tier 9 regex runtime bridge: fixed=TRUE with perl=TRUE is unsupported");
    }

    const char* op = CHAR(STRING_ELT(r_op, 0));
    if (!(strcmp(op, "grepl") == 0 || strcmp(op, "grep") == 0 || strcmp(op, "sub") == 0 || strcmp(op, "gsub") == 0)) {
        error("Tier 9 regex runtime bridge: unsupported op");
    }

    if (STRING_ELT(r_pattern, 0) == NA_STRING) {
        error("Tier 9 regex runtime bridge: NA pattern is not supported");
    }
    const char* pattern = CHAR(STRING_ELT(r_pattern, 0));
    if (pattern[0] == '\0') {
        error("Tier 9 regex runtime bridge: empty pattern is not supported");
    }

    R_xlen_t n = XLENGTH(r_x);
    if (strcmp(op, "grepl") == 0) {
        SEXP out = PROTECT(allocVector(LGLSXP, n));
        for (R_xlen_t i = 0; i < n; i++) {
            SEXP ch = STRING_ELT(r_x, i);
            if (ch == NA_STRING) {
                LOGICAL(out)[i] = NA_LOGICAL;
            } else {
                LOGICAL(out)[i] = mojor_tier9_contains_literal(CHAR(ch), pattern) ? TRUE : FALSE;
            }
        }
        UNPROTECT(1);
        return out;
    }

    if (strcmp(op, "grep") == 0) {
        SEXP tmp = PROTECT(allocVector(INTSXP, n));
        int out_n = 0;
        for (R_xlen_t i = 0; i < n; i++) {
            SEXP ch = STRING_ELT(r_x, i);
            if (ch == NA_STRING) {
                continue;
            }
            if (mojor_tier9_contains_literal(CHAR(ch), pattern)) {
                if (i >= (R_xlen_t) INT_MAX) {
                    error("Tier 9 regex runtime bridge: grep index overflow");
                }
                INTEGER(tmp)[out_n] = (int) (i + 1);
                out_n++;
            }
        }
        SEXP out = PROTECT(allocVector(INTSXP, out_n));
        if (out_n > 0) {
            memcpy(INTEGER(out), INTEGER(tmp), sizeof(int) * (size_t) out_n);
        }
        UNPROTECT(2);
        return out;
    }

    if (TYPEOF(r_replacement) != STRSXP || XLENGTH(r_replacement) != 1) {
        error("Tier 9 regex runtime bridge: replacement must be a scalar character");
    }
    if (STRING_ELT(r_replacement, 0) == NA_STRING) {
        error("Tier 9 regex runtime bridge: NA replacement is not supported");
    }
    const char* replacement = CHAR(STRING_ELT(r_replacement, 0));
    int replace_all = strcmp(op, "gsub") == 0;

    SEXP out = PROTECT(allocVector(STRSXP, n));
    for (R_xlen_t i = 0; i < n; i++) {
        SEXP ch = STRING_ELT(r_x, i);
        SET_STRING_ELT(out, i, mojor_tier9_replace_literal_one(ch, pattern, replacement, replace_all));
    }
    UNPROTECT(1);
    return out;
}

SEXP mojor_tier9_expand_grid_runtime_call(SEXP r_a1, SEXP r_a2, SEXP r_a3, SEXP r_a4, SEXP r_nargs) {
    int n_args = asInteger(r_nargs);
    if (n_args < 2 || n_args > 4) {
        error("Tier 9 expand.grid runtime bridge: nargs must be in [2, 4]");
    }
    SEXP args[4];
    args[0] = r_a1;
    args[1] = r_a2;
    args[2] = r_a3;
    args[3] = r_a4;
    R_xlen_t lens[4] = {0, 0, 0, 0};

    for (int i = 0; i < n_args; i++) {
        if (!(TYPEOF(args[i]) == REALSXP || TYPEOF(args[i]) == INTSXP || TYPEOF(args[i]) == LGLSXP)) {
            error("Tier 9 expand.grid runtime bridge: arguments must be numeric/integer/logical vectors");
        }
        SEXP dim = getAttrib(args[i], R_DimSymbol);
        if (dim != R_NilValue) {
            error("Tier 9 expand.grid runtime bridge: matrix/array args are unsupported");
        }
        lens[i] = XLENGTH(args[i]);
    }

    R_xlen_t total = 1;
    for (int i = 0; i < n_args; i++) {
        if (lens[i] == 0) {
            total = 0;
        } else if (total > 0 && total > ((R_xlen_t) INT_MAX) / lens[i]) {
            error("expand.grid: output too large");
        } else if (total > 0) {
            total *= lens[i];
        }
    }

    SEXP df = PROTECT(allocVector(VECSXP, n_args));
    for (int i = 0; i < n_args; i++) {
        R_xlen_t rep_each = 1;
        for (int k = 0; k < i; k++) {
            rep_each *= lens[k];
        }
        SEXP col = PROTECT(allocVector(TYPEOF(args[i]), total));
        for (R_xlen_t j = 0; j < total; j++) {
            R_xlen_t src_idx = (lens[i] == 0) ? 0 : ((j / rep_each) % lens[i]);
            if (TYPEOF(args[i]) == REALSXP) {
                REAL(col)[j] = REAL(args[i])[src_idx];
            } else if (TYPEOF(args[i]) == INTSXP) {
                INTEGER(col)[j] = INTEGER(args[i])[src_idx];
            } else {
                LOGICAL(col)[j] = LOGICAL(args[i])[src_idx];
            }
        }
        SET_VECTOR_ELT(df, i, col);
        UNPROTECT(1);
    }

    SEXP names = PROTECT(allocVector(STRSXP, n_args));
    for (int i = 0; i < n_args; i++) {
        char var_name[32];
        snprintf(var_name, sizeof(var_name), "Var%d", i + 1);
        SET_STRING_ELT(names, i, Rf_mkChar(var_name));
    }
    setAttrib(df, R_NamesSymbol, names);

    SEXP out_attrs = PROTECT(allocVector(VECSXP, 2));
    SEXP out_attrs_names = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(out_attrs_names, 0, Rf_mkChar("dim"));
    SET_STRING_ELT(out_attrs_names, 1, Rf_mkChar("dimnames"));
    setAttrib(out_attrs, R_NamesSymbol, out_attrs_names);
    SEXP out_dim = PROTECT(allocVector(INTSXP, n_args));
    SEXP out_dimnames = PROTECT(allocVector(VECSXP, n_args));

    for (int i = 0; i < n_args; i++) {
        if (lens[i] > (R_xlen_t) INT_MAX) {
            error("expand.grid: dimension too large");
        }
        INTEGER(out_dim)[i] = (int) lens[i];
        SEXP dimnames_i = PROTECT(allocVector(STRSXP, lens[i]));
        for (R_xlen_t j = 0; j < lens[i]; j++) {
            char valbuf[128];
            if (TYPEOF(args[i]) == INTSXP) {
                int v = INTEGER(args[i])[j];
                if (v == NA_INTEGER) {
                    snprintf(valbuf, sizeof(valbuf), "Var%d=NA", i + 1);
                } else {
                    snprintf(valbuf, sizeof(valbuf), "Var%d=%d", i + 1, v);
                }
            } else if (TYPEOF(args[i]) == LGLSXP) {
                int v = LOGICAL(args[i])[j];
                if (v == NA_LOGICAL) {
                    snprintf(valbuf, sizeof(valbuf), "Var%d=NA", i + 1);
                } else {
                    snprintf(valbuf, sizeof(valbuf), "Var%d=%s", i + 1, v ? "TRUE" : "FALSE");
                }
            } else {
                double v = REAL(args[i])[j];
                if (ISNA(v)) {
                    snprintf(valbuf, sizeof(valbuf), "Var%d=NA", i + 1);
                } else if (ISNAN(v)) {
                    snprintf(valbuf, sizeof(valbuf), "Var%d=NaN", i + 1);
                } else {
                    snprintf(valbuf, sizeof(valbuf), "Var%d=%.15g", i + 1, v);
                }
            }
            SET_STRING_ELT(dimnames_i, j, Rf_mkChar(valbuf));
        }
        SET_VECTOR_ELT(out_dimnames, i, dimnames_i);
        UNPROTECT(1);
    }

    SET_VECTOR_ELT(out_attrs, 0, out_dim);
    SET_VECTOR_ELT(out_attrs, 1, out_dimnames);
    setAttrib(out_dimnames, R_NamesSymbol, names);
    setAttrib(df, Rf_install("out.attrs"), out_attrs);

    SEXP rownames = PROTECT(allocVector(INTSXP, 2));
    INTEGER(rownames)[0] = NA_INTEGER;
    INTEGER(rownames)[1] = -(int) total;
    setAttrib(df, R_RowNamesSymbol, rownames);
    SEXP klass = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(klass, 0, Rf_mkChar("data.frame"));
    setAttrib(df, R_ClassSymbol, klass);

    UNPROTECT(8);
    return df;
}

SEXP mojor_ir_emit_stmt_pilot_call(SEXP r_payload, SEXP r_legacy_text) {
    if (TYPEOF(r_payload) != STRSXP || XLENGTH(r_payload) != 1) {
        error("IR emit pilot bridge: payload must be a scalar character");
    }
    if (TYPEOF(r_legacy_text) != STRSXP || XLENGTH(r_legacy_text) != 1) {
        error("IR emit pilot bridge: legacy_text must be a scalar character");
    }
    if (STRING_ELT(r_payload, 0) == NA_STRING || STRING_ELT(r_legacy_text, 0) == NA_STRING) {
        error("IR emit pilot bridge: payload and legacy_text must be non-NA strings");
    }

    const char* payload = CHAR(STRING_ELT(r_payload, 0));
    const char* legacy_text = CHAR(STRING_ELT(r_legacy_text, 0));
    size_t payload_len_raw = strlen(payload);
    size_t legacy_len_raw = strlen(legacy_text);
    if (payload_len_raw > (size_t)INT_MAX || legacy_len_raw > (size_t)INT_MAX) {
        error("IR emit pilot bridge: payload too large");
    }

    int mode_hint = 0;
    {
        const char* k = strstr(payload, "kind = \"");
        if (k != NULL) {
            k += 8;
            if (strncmp(k, "assign\"", 7) == 0) {
                mode_hint = 1;
            } else if (strncmp(k, "return\"", 7) == 0) {
                mode_hint = 2;
            } else if (strncmp(k, "block\"", 6) == 0) {
                mode_hint = 3;
            } else if (strncmp(k, "if\"", 3) == 0) {
                mode_hint = 4;
            } else if (strncmp(k, "loop\"", 5) == 0) {
                mode_hint = 5;
            }
        }
    }

    int out_len = 0;
    int status = mojor_ir_emit_stmt_pilot(
        payload,
        (int) payload_len_raw,
        legacy_text,
        (int) legacy_len_raw,
        mode_hint,
        NULL,
        0,
        &out_len
    );
    char* out_buf = NULL;
    if (status == -4 && out_len >= 0) {
        int cap = out_len + 1;
        out_buf = (char*) R_alloc((size_t) cap, sizeof(char));
        out_buf[0] = '\0';
        status = mojor_ir_emit_stmt_pilot(
            payload,
            (int) payload_len_raw,
            legacy_text,
            (int) legacy_len_raw,
            mode_hint,
            out_buf,
            cap,
            &out_len
        );
    }

    const char* emitted = legacy_text;
    int emitted_len = (int) legacy_len_raw;
    if (status == 0 && out_buf != NULL && out_len >= 0) {
        emitted = out_buf;
        emitted_len = out_len;
    }

    SEXP out = PROTECT(allocVector(VECSXP, 2));
    SEXP nms = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(nms, 0, Rf_mkChar("status"));
    SET_STRING_ELT(nms, 1, Rf_mkChar("emitted"));
    setAttrib(out, R_NamesSymbol, nms);
    SET_VECTOR_ELT(out, 0, ScalarInteger(status));
    SET_VECTOR_ELT(out, 1, ScalarString(Rf_mkCharLen(emitted, emitted_len)));
    UNPROTECT(2);
    return out;
}


static const R_CallMethodDef CallEntries[] = {
    {"mojor_sum_f64", (DL_FUNC) &mojor_sum_f64_call, 1},
    {"mojor_sum_f64_std", (DL_FUNC) &mojor_sum_f64_std_call, 1},
    {"mojor_sum_f64_pairwise", (DL_FUNC) &mojor_sum_f64_pairwise_call, 1},
    {"mojor_sum_f64_nomiss", (DL_FUNC) &mojor_sum_f64_nomiss_call, 1},
    {"mojor_sum_f64_nomiss_manual", (DL_FUNC) &mojor_sum_f64_nomiss_manual_call, 1},
    {"mojor_prod_f64", (DL_FUNC) &mojor_prod_f64_call, 1},
    {"mojor_prod_f64_nomiss", (DL_FUNC) &mojor_prod_f64_nomiss_call, 1},
    {"mojor_which_min_f64_nomiss", (DL_FUNC) &mojor_which_min_f64_nomiss_call, 1},
    {"mojor_which_max_f64_nomiss", (DL_FUNC) &mojor_which_max_f64_nomiss_call, 1},
    {"mojor_min_f64", (DL_FUNC) &mojor_min_f64_call, 1},
    {"mojor_min_f64_nomiss", (DL_FUNC) &mojor_min_f64_nomiss_call, 1},
    {"mojor_max_f64", (DL_FUNC) &mojor_max_f64_call, 1},
    {"mojor_max_f64_nomiss", (DL_FUNC) &mojor_max_f64_nomiss_call, 1},
    {"mojor_mean_f64", (DL_FUNC) &mojor_mean_f64_call, 1},
    {"mojor_mean_f64_nomiss", (DL_FUNC) &mojor_mean_f64_nomiss_call, 1},
    {"mojor_sum_f32_int", (DL_FUNC) &mojor_sum_f32_int_call, 1},
    {"mojor_sum_f32_std_int", (DL_FUNC) &mojor_sum_f32_std_int_call, 1},
    {"mojor_sum_f32_pairwise_int", (DL_FUNC) &mojor_sum_f32_pairwise_int_call, 1},
    {"mojor_sum_f32_nomiss_int", (DL_FUNC) &mojor_sum_f32_nomiss_int_call, 1},
    {"mojor_prod_f32_int", (DL_FUNC) &mojor_prod_f32_int_call, 1},
    {"mojor_prod_f32_nomiss_int", (DL_FUNC) &mojor_prod_f32_nomiss_int_call, 1},
    {"mojor_which_min_f32_nomiss_int", (DL_FUNC) &mojor_which_min_f32_nomiss_int_call, 1},
    {"mojor_which_max_f32_nomiss_int", (DL_FUNC) &mojor_which_max_f32_nomiss_int_call, 1},
    {"mojor_min_f32_int", (DL_FUNC) &mojor_min_f32_int_call, 1},
    {"mojor_min_f32_nomiss_int", (DL_FUNC) &mojor_min_f32_nomiss_int_call, 1},
    {"mojor_max_f32_int", (DL_FUNC) &mojor_max_f32_int_call, 1},
    {"mojor_max_f32_nomiss_int", (DL_FUNC) &mojor_max_f32_nomiss_int_call, 1},
    {"mojor_mean_f32_int", (DL_FUNC) &mojor_mean_f32_int_call, 1},
    {"mojor_mean_f32_nomiss_int", (DL_FUNC) &mojor_mean_f32_nomiss_int_call, 1},
    {"mojor_sd_f64_nomiss", (DL_FUNC) &mojor_sd_f64_nomiss_call, 1},
    {"mojor_sd_f32_nomiss_int", (DL_FUNC) &mojor_sd_f32_nomiss_int_call, 1},
    {"mojor_var_f64_nomiss", (DL_FUNC) &mojor_var_f64_nomiss_call, 1},
    {"mojor_var_f32_nomiss_int", (DL_FUNC) &mojor_var_f32_nomiss_int_call, 1},
    {"mojor_var_f64_nomiss_twopass", (DL_FUNC) &mojor_var_f64_nomiss_twopass_call, 1},
    {"mojor_var_f32_nomiss_twopass_int", (DL_FUNC) &mojor_var_f32_nomiss_twopass_int_call, 1},
    {"mojor_sd_f64_nomiss_twopass", (DL_FUNC) &mojor_sd_f64_nomiss_twopass_call, 1},
    {"mojor_sd_f32_nomiss_twopass_int", (DL_FUNC) &mojor_sd_f32_nomiss_twopass_int_call, 1},
    MOJOR_SIMPLE_CALLS(MOJOR_CALL_ENTRY)
    {"mojor_gpu_ctx_create", (DL_FUNC) &mojor_gpu_ctx_create_call, 0},
    {"mojor_gpu_ctx_free", (DL_FUNC) &mojor_gpu_ctx_free_call, 1},
    {"mojor_gpu_meminfo", (DL_FUNC) &mojor_gpu_meminfo_call, 1},
    {"mojor_sigmoid_f64_gpu", (DL_FUNC) &mojor_sigmoid_f64_gpu_call, 2},
    {"mojor_sigmoid_f32_gpu", (DL_FUNC) &mojor_sigmoid_f32_gpu_call, 2},
    {"mojor_sigmoid_f32_gpu_int", (DL_FUNC) &mojor_sigmoid_f32_gpu_int_call, 2},
    {"mojor_sigmoid_f32_gpu_iters_int", (DL_FUNC) &mojor_sigmoid_f32_gpu_iters_int_call, 3},
    {"mojor_sigmoid_affine_f32_gpu_int", (DL_FUNC) &mojor_sigmoid_affine_f32_gpu_int_call, 4},
    {"mojor_sigmoid_affine_f32_gpu_iters_int", (DL_FUNC) &mojor_sigmoid_affine_f32_gpu_iters_int_call, 5},
    {"mojor_sigmoid_affine_f32_gpu_chain_int", (DL_FUNC) &mojor_sigmoid_affine_f32_gpu_chain_int_call, 8},
    {"mojor_sigmoid_affine_f32_gpu_chain_sum_int", (DL_FUNC) &mojor_sigmoid_affine_f32_gpu_chain_sum_int_call, 8},
    {"mojor_sigmoid_affine_f32_gpu_chain_sum_warp_int", (DL_FUNC) &mojor_sigmoid_affine_f32_gpu_chain_sum_warp_int_call, 8},
    {"mojor_gpu_session_create", (DL_FUNC) &mojor_gpu_session_create_call, 2},
    {"mojor_gpu_session_free", (DL_FUNC) &mojor_gpu_session_free_call, 1},
    {"mojor_gpu_session_chain_int", (DL_FUNC) &mojor_gpu_session_chain_int_call, 8},
    {"mojor_gpu_session_chain_sum_int", (DL_FUNC) &mojor_gpu_session_chain_sum_int_call, 8},
    MOJOR_GPU_CAP_CALLS(MOJOR_CALL_ENTRY)
    MOJOR_GPU_BUF_F32_CALLS(MOJOR_CALL_ENTRY)
    MOJOR_GPU_BUF_F64_CALLS(MOJOR_CALL_ENTRY)
    MOJOR_GPU_BUF_I32_CALLS(MOJOR_CALL_ENTRY)
    {"mojor_gpu_buf_f32_live_count", (DL_FUNC) &mojor_gpu_buf_f32_live_count_call, 0},
    {"mojor_has_gpu", (DL_FUNC) &mojor_has_gpu_call, 0},
    {"mojor_bincount_f64", (DL_FUNC) &mojor_bincount_f64_call, 7},
    {"mojor_rng_seed", (DL_FUNC) &mojor_rng_seed_call, 1},
    MOJOR_RNG_CALLS(MOJOR_CALL_ENTRY)
    {"mojor_rhyper", (DL_FUNC) &mojor_rhyper_call, 4},
    {"mojor_rsignrank", (DL_FUNC) &mojor_rsignrank_call, 2},
    {"mojor_rwilcox", (DL_FUNC) &mojor_rwilcox_call, 3},
    {"mojor_dnorm_fast", (DL_FUNC) &mojor_dnorm_fast_call, 4},
    {"mojor_dpois_fast", (DL_FUNC) &mojor_dpois_fast_call, 3},
    {"mojor_tier9_regex_runtime", (DL_FUNC) &mojor_tier9_regex_runtime_call, 6},
    {"mojor_tier9_expand_grid_runtime", (DL_FUNC) &mojor_tier9_expand_grid_runtime_call, 5},
    {"mojor_ir_emit_stmt_pilot", (DL_FUNC) &mojor_ir_emit_stmt_pilot_call, 2},
    {"mojor_rt_v1_mojor_rng_seed", (DL_FUNC) &mojor_rng_seed_call, 1},
    MOJOR_RNG_CALLS(MOJOR_CALL_ENTRY_RT_V1)
    {"mojor_rt_v1_mojor_rhyper", (DL_FUNC) &mojor_rhyper_call, 4},
    {"mojor_rt_v1_mojor_rsignrank", (DL_FUNC) &mojor_rsignrank_call, 2},
    {"mojor_rt_v1_mojor_rwilcox", (DL_FUNC) &mojor_rwilcox_call, 3},
    {"mojor_rt_v1_mojor_dnorm_fast", (DL_FUNC) &mojor_dnorm_fast_call, 4},
    {"mojor_rt_v1_dnorm_fast", (DL_FUNC) &mojor_dnorm_fast_call, 4},
    {"mojor_rt_v1_mojor_dpois_fast", (DL_FUNC) &mojor_dpois_fast_call, 3},
    {"mojor_rt_v1_dpois_fast", (DL_FUNC) &mojor_dpois_fast_call, 3},
    {"mojor_rt_v1_mojor_tier9_regex_runtime", (DL_FUNC) &mojor_tier9_regex_runtime_call, 6},
    {"mojor_rt_v1_tier9_regex_runtime", (DL_FUNC) &mojor_tier9_regex_runtime_call, 6},
    {"mojor_rt_v1_mojor_tier9_expand_grid_runtime", (DL_FUNC) &mojor_tier9_expand_grid_runtime_call, 5},
    {"mojor_rt_v1_tier9_expand_grid_runtime", (DL_FUNC) &mojor_tier9_expand_grid_runtime_call, 5},
    {"mojor_rt_v1_mojor_gpu_session_create", (DL_FUNC) &mojor_gpu_session_create_call, 2},
    {"mojor_rt_v1_mojor_gpu_session_free", (DL_FUNC) &mojor_gpu_session_free_call, 1},
    {"mojor_rt_v1_mojor_gpu_session_chain_int", (DL_FUNC) &mojor_gpu_session_chain_int_call, 8},
    {"mojor_rt_v1_mojor_gpu_session_chain_sum_int", (DL_FUNC) &mojor_gpu_session_chain_sum_int_call, 8},
    {"mojor_rt_v1_mojor_sigmoid_f64_gpu", (DL_FUNC) &mojor_sigmoid_f64_gpu_call, 2},
    {"mojor_rt_v1_mojor_sigmoid_f32_gpu", (DL_FUNC) &mojor_sigmoid_f32_gpu_call, 2},
    {"mojor_rt_v1_mojor_sigmoid_f32_gpu_int", (DL_FUNC) &mojor_sigmoid_f32_gpu_int_call, 2},
    {"mojor_rt_v1_mojor_sigmoid_f32_gpu_iters_int", (DL_FUNC) &mojor_sigmoid_f32_gpu_iters_int_call, 3},
    {"mojor_rt_v1_mojor_sigmoid_affine_f32_gpu_int", (DL_FUNC) &mojor_sigmoid_affine_f32_gpu_int_call, 4},
    {"mojor_rt_v1_mojor_sigmoid_affine_f32_gpu_iters_int", (DL_FUNC) &mojor_sigmoid_affine_f32_gpu_iters_int_call, 5},
    {"mojor_rt_v1_mojor_sigmoid_affine_f32_gpu_chain_int", (DL_FUNC) &mojor_sigmoid_affine_f32_gpu_chain_int_call, 8},
    {"mojor_rt_v1_mojor_sigmoid_affine_f32_gpu_chain_sum_int", (DL_FUNC) &mojor_sigmoid_affine_f32_gpu_chain_sum_int_call, 8},
    {"mojor_rt_v1_mojor_sigmoid_affine_f32_gpu_chain_sum_warp_int", (DL_FUNC) &mojor_sigmoid_affine_f32_gpu_chain_sum_warp_int_call, 8},
    {"mojor_rt_v1_mojor_ir_emit_stmt_pilot", (DL_FUNC) &mojor_ir_emit_stmt_pilot_call, 2},
    {"mojor_rt_v1_sum_f64", (DL_FUNC) &mojor_sum_f64_call, 1},
    {"mojor_rt_v1_sum_f64_std", (DL_FUNC) &mojor_sum_f64_std_call, 1},
    {"mojor_rt_v1_sum_f64_pairwise", (DL_FUNC) &mojor_sum_f64_pairwise_call, 1},
    {"mojor_rt_v1_sum_f64_nomiss", (DL_FUNC) &mojor_sum_f64_nomiss_call, 1},
    {"mojor_rt_v1_sum_f64_nomiss_manual", (DL_FUNC) &mojor_sum_f64_nomiss_manual_call, 1},
    {"mojor_rt_v1_prod_f64", (DL_FUNC) &mojor_prod_f64_call, 1},
    {"mojor_rt_v1_prod_f64_nomiss", (DL_FUNC) &mojor_prod_f64_nomiss_call, 1},
    {"mojor_rt_v1_which_min_f64_nomiss", (DL_FUNC) &mojor_which_min_f64_nomiss_call, 1},
    {"mojor_rt_v1_which_max_f64_nomiss", (DL_FUNC) &mojor_which_max_f64_nomiss_call, 1},
    {"mojor_rt_v1_min_f64", (DL_FUNC) &mojor_min_f64_call, 1},
    {"mojor_rt_v1_min_f64_nomiss", (DL_FUNC) &mojor_min_f64_nomiss_call, 1},
    {"mojor_rt_v1_max_f64", (DL_FUNC) &mojor_max_f64_call, 1},
    {"mojor_rt_v1_max_f64_nomiss", (DL_FUNC) &mojor_max_f64_nomiss_call, 1},
    {"mojor_rt_v1_mean_f64", (DL_FUNC) &mojor_mean_f64_call, 1},
    {"mojor_rt_v1_mean_f64_nomiss", (DL_FUNC) &mojor_mean_f64_nomiss_call, 1},
    {"mojor_rt_v1_sum_f32_int", (DL_FUNC) &mojor_sum_f32_int_call, 1},
    {"mojor_rt_v1_sum_f32_std_int", (DL_FUNC) &mojor_sum_f32_std_int_call, 1},
    {"mojor_rt_v1_sum_f32_pairwise_int", (DL_FUNC) &mojor_sum_f32_pairwise_int_call, 1},
    {"mojor_rt_v1_sum_f32_nomiss_int", (DL_FUNC) &mojor_sum_f32_nomiss_int_call, 1},
    {"mojor_rt_v1_prod_f32_int", (DL_FUNC) &mojor_prod_f32_int_call, 1},
    {"mojor_rt_v1_prod_f32_nomiss_int", (DL_FUNC) &mojor_prod_f32_nomiss_int_call, 1},
    {"mojor_rt_v1_which_min_f32_nomiss_int", (DL_FUNC) &mojor_which_min_f32_nomiss_int_call, 1},
    {"mojor_rt_v1_which_max_f32_nomiss_int", (DL_FUNC) &mojor_which_max_f32_nomiss_int_call, 1},
    {"mojor_rt_v1_min_f32_int", (DL_FUNC) &mojor_min_f32_int_call, 1},
    {"mojor_rt_v1_min_f32_nomiss_int", (DL_FUNC) &mojor_min_f32_nomiss_int_call, 1},
    {"mojor_rt_v1_max_f32_int", (DL_FUNC) &mojor_max_f32_int_call, 1},
    {"mojor_rt_v1_max_f32_nomiss_int", (DL_FUNC) &mojor_max_f32_nomiss_int_call, 1},
    {"mojor_rt_v1_mean_f32_int", (DL_FUNC) &mojor_mean_f32_int_call, 1},
    {"mojor_rt_v1_mean_f32_nomiss_int", (DL_FUNC) &mojor_mean_f32_nomiss_int_call, 1},
    {"mojor_rt_v1_sd_f64_nomiss", (DL_FUNC) &mojor_sd_f64_nomiss_call, 1},
    {"mojor_rt_v1_sd_f32_nomiss_int", (DL_FUNC) &mojor_sd_f32_nomiss_int_call, 1},
    {"mojor_rt_v1_var_f64_nomiss", (DL_FUNC) &mojor_var_f64_nomiss_call, 1},
    {"mojor_rt_v1_var_f32_nomiss_int", (DL_FUNC) &mojor_var_f32_nomiss_int_call, 1},
    {"mojor_rt_v1_var_f64_nomiss_twopass", (DL_FUNC) &mojor_var_f64_nomiss_twopass_call, 1},
    {"mojor_rt_v1_var_f32_nomiss_twopass_int", (DL_FUNC) &mojor_var_f32_nomiss_twopass_int_call, 1},
    {"mojor_rt_v1_sd_f64_nomiss_twopass", (DL_FUNC) &mojor_sd_f64_nomiss_twopass_call, 1},
    {"mojor_rt_v1_sd_f32_nomiss_twopass_int", (DL_FUNC) &mojor_sd_f32_nomiss_twopass_int_call, 1},
    {"mojor_rt_v1_gpu_buf_f32_live_count", (DL_FUNC) &mojor_gpu_buf_f32_live_count_call, 0},
    {"mojor_rt_v1_bincount_f64", (DL_FUNC) &mojor_bincount_f64_call, 7},
    {"mojor_rt_v1_rng_seed", (DL_FUNC) &mojor_rng_seed_call, 1},
    {"mojor_rt_v1_rhyper", (DL_FUNC) &mojor_rhyper_call, 4},
    {"mojor_rt_v1_rsignrank", (DL_FUNC) &mojor_rsignrank_call, 2},
    {"mojor_rt_v1_rwilcox", (DL_FUNC) &mojor_rwilcox_call, 3},
    {"mojor_rt_v1_gpu_ctx_create", (DL_FUNC) &mojor_gpu_ctx_create_call, 0},
    {"mojor_rt_v1_gpu_ctx_free", (DL_FUNC) &mojor_gpu_ctx_free_call, 1},
    {"mojor_rt_v1_gpu_meminfo", (DL_FUNC) &mojor_gpu_meminfo_call, 1},
    {"mojor_rt_v1_has_gpu", (DL_FUNC) &mojor_has_gpu_call, 0},
    MOJOR_GPU_CAP_CALLS(MOJOR_CALL_ENTRY_RT_V1)
    MOJOR_GPU_BUF_F32_CALLS(MOJOR_CALL_ENTRY_RT_V1)
    MOJOR_GPU_BUF_F64_CALLS(MOJOR_CALL_ENTRY_RT_V1)
    MOJOR_GPU_BUF_I32_CALLS(MOJOR_CALL_ENTRY_RT_V1)
    MOJOR_SIMPLE_CALLS(MOJOR_CALL_ENTRY_RT_V1)

    {NULL, NULL, 0}
};

#undef MOJOR_SIMPLE_CALLS
#undef MOJOR_RNG_CALLS
#undef MOJOR_GPU_BUF_F32_CALLS
#undef MOJOR_GPU_BUF_F64_CALLS
#undef MOJOR_GPU_BUF_I32_CALLS
#undef MOJOR_CALL_ENTRY
#undef MOJOR_CALL_ENTRY_RT_V1

void R_init_mojor_bridge(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
