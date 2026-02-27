#ifndef MOJOR_BACKEND_H
#define MOJOR_BACKEND_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

double mojor_sum_f64(const double* x, int n);
double mojor_sum_f64_std(const double* x, int n);
double mojor_sum_f64_pairwise(const double* x, int n);
double mojor_sum_f64_nomiss(const double* x, int n);
double mojor_sum_f64_nomiss_manual(const double* x, int n);
double mojor_prod_f64(const double* x, int n);
double mojor_prod_f64_nomiss(const double* x, int n);
int mojor_which_min_f64_nomiss(const double* x, int n);
int mojor_which_max_f64_nomiss(const double* x, int n);
double mojor_min_f64(const double* x, int n);
double mojor_min_f64_nomiss(const double* x, int n);
double mojor_max_f64(const double* x, int n);
double mojor_max_f64_nomiss(const double* x, int n);
double mojor_mean_f64(const double* x, int n);
double mojor_mean_f64_nomiss(const double* x, int n);
float mojor_sum_f32(const float* x, int n);
float mojor_sum_f32_std(const float* x, int n);
float mojor_sum_f32_pairwise(const float* x, int n);
float mojor_sum_f32_nomiss(const float* x, int n);
float mojor_prod_f32(const float* x, int n);
float mojor_prod_f32_nomiss(const float* x, int n);
int mojor_which_min_f32_nomiss(const float* x, int n);
int mojor_which_max_f32_nomiss(const float* x, int n);
float mojor_min_f32(const float* x, int n);
float mojor_min_f32_nomiss(const float* x, int n);
float mojor_max_f32(const float* x, int n);
float mojor_max_f32_nomiss(const float* x, int n);
float mojor_mean_f32(const float* x, int n);
float mojor_mean_f32_nomiss(const float* x, int n);
double mojor_sd_f64_nomiss(const double* x, int n);
float mojor_sd_f32_nomiss(const float* x, int n);
double mojor_var_f64_nomiss(const double* x, int n);
float mojor_var_f32_nomiss(const float* x, int n);
double mojor_var_f64_nomiss_twopass(const double* x, int n);
float mojor_var_f32_nomiss_twopass(const float* x, int n);
double mojor_sd_f64_nomiss_twopass(const double* x, int n);
float mojor_sd_f32_nomiss_twopass(const float* x, int n);
void mojor_scale_f64(const double* x, double* out, int n, double scalar);
void mojor_abs_f64(double* x, int n);
void mojor_running_max_f64(const double* x, double* out, int n);
void mojor_count_runs_f64(const double* x, int n, double threshold, int* out);
void mojor_sigmoid_f64(const double* x, double* out, int n);
void* mojor_gpu_ctx_create(void);
int mojor_gpu_ctx_free(void* ctx);
int mojor_sigmoid_f64_gpu(void* ctx, const double* x, double* out, int n);
int mojor_sigmoid_f32_gpu(void* ctx, const float* x, float* out, int n);
int mojor_sigmoid_affine_f32_gpu(void* ctx, const float* x, float* out, int n, float scale, float bias);
int mojor_has_gpu(void);
int mojor_gpu_meminfo(void* ctx, uint64_t* out);

int mojor_sigmoid_f32_gpu_iters(void* ctx, const float* x, float* out, int n, int iters);
int mojor_sigmoid_affine_f32_gpu_iters(
    void* ctx,
    const float* x,
    float* out,
    int n,
    int iters,
    float scale,
    float bias
);
int mojor_sigmoid_affine_f32_gpu_chain(
    void* ctx,
    const float* x,
    float* out,
    int n,
    int iters,
    float scale,
    float bias,
    float post_scale,
    float post_bias,
    int post_iters
);
float mojor_sigmoid_affine_f32_gpu_chain_sum(
    void* ctx,
    const float* x,
    int n,
    int iters,
    float scale,
    float bias,
    float post_scale,
    float post_bias,
    int post_iters,
    int* status
);
float mojor_sigmoid_affine_f32_gpu_chain_sum_warp(
    void* ctx,
    const float* x,
    int n,
    int iters,
    float scale,
    float bias,
    float post_scale,
    float post_bias,
    int post_iters,
    int* status
);
int mojor_gpu_session_create(void* ctx, int n);
int mojor_gpu_session_free(void* ctx);
int mojor_gpu_session_chain_f32(
    void* ctx,
    const float* x,
    float* out,
    int n,
    int iters,
    float scale,
    float bias,
    float post_scale,
    float post_bias,
    int post_iters
);
float mojor_gpu_session_chain_sum_f32(
    void* ctx,
    const float* x,
    int n,
    int iters,
    float scale,
    float bias,
    float post_scale,
    float post_bias,
    int post_iters,
    int* status
);
void* mojor_gpu_buf_f32_alloc(void* ctx, int n);
int mojor_gpu_buf_f32_free(void* ctx, void* handle);
int mojor_gpu_buf_f32_len(void* ctx, void* handle);
void* mojor_gpu_buf_f32_ptr(void* handle);
int mojor_gpu_buf_f32_write(void* ctx, void* handle, const float* host, int n);
int mojor_gpu_buf_f32_read(void* ctx, void* handle, float* host, int n);
void* mojor_gpu_buf_f32_cast_f64(void* ctx, void* handle);
void* mojor_gpu_buf_f32_cast_i32(void* ctx, void* handle);
void* mojor_gpu_buf_f32_affine(
    void* ctx,
    void* handle,
    float scale,
    float bias,
    int* status
);
void* mojor_gpu_buf_f32_chain(
    void* ctx,
    void* handle,
    int iters,
    float scale,
    float bias,
    float post_scale,
    float post_bias,
    int post_iters,
    int* status
);
float mojor_gpu_buf_f32_chain_sum(
    void* ctx,
    void* handle,
    int iters,
    float scale,
    float bias,
    float post_scale,
    float post_bias,
    int post_iters,
    int* status
);
float mojor_gpu_buf_f32_chain_sum_warp(
    void* ctx,
    void* handle,
    int iters,
    float scale,
    float bias,
    float post_scale,
    float post_bias,
    int post_iters,
    int* status
);
void* mojor_gpu_buf_f64_alloc(void* ctx, int n);
int mojor_gpu_buf_f64_free(void* ctx, void* handle);
int mojor_gpu_buf_f64_len(void* ctx, void* handle);
void* mojor_gpu_buf_f64_ptr(void* handle);
int mojor_gpu_buf_f64_write(void* ctx, void* handle, const double* host, int n);
int mojor_gpu_buf_f64_read(void* ctx, void* handle, double* host, int n);
void* mojor_gpu_buf_f64_cast_f32(void* ctx, void* handle);
void* mojor_gpu_buf_f64_cast_i32(void* ctx, void* handle);
void* mojor_gpu_buf_i32_alloc(void* ctx, int n);
int mojor_gpu_buf_i32_free(void* ctx, void* handle);
int mojor_gpu_buf_i32_len(void* ctx, void* handle);
void* mojor_gpu_buf_i32_ptr(void* handle);
int mojor_gpu_buf_i32_write(void* ctx, void* handle, const int32_t* host, int n);
int mojor_gpu_buf_i32_read(void* ctx, void* handle, int32_t* host, int n);
void* mojor_gpu_buf_i32_cast_f32(void* ctx, void* handle);
void* mojor_gpu_buf_i32_cast_f64(void* ctx, void* handle);
void* mojor_gpu_buf_f64_affine(
    void* ctx,
    void* handle,
    double scale,
    double bias,
    int* status
);
void* mojor_gpu_buf_f64_chain(
    void* ctx,
    void* handle,
    int iters,
    double scale,
    double bias,
    double post_scale,
    double post_bias,
    int post_iters,
    int* status
);
double mojor_gpu_buf_f64_chain_sum(
    void* ctx,
    void* handle,
    int iters,
    double scale,
    double bias,
    double post_scale,
    double post_bias,
    int post_iters,
    int* status
);
double mojor_gpu_buf_f64_chain_sum_warp(
    void* ctx,
    void* handle,
    int iters,
    double scale,
    double bias,
    double post_scale,
    double post_bias,
    int post_iters,
    int* status
);
void* mojor_gpu_buf_f32_reduce(
    void* ctx,
    void* handle,
    int op,
    const int* dims,
    int ndim,
    int keepdims
);
void* mojor_gpu_buf_f32_matmul(
    void* ctx,
    void* handle_a,
    void* handle_b,
    int m,
    int k,
    int n,
    int transpose_a,
    int transpose_b
);
int mojor_gpu_buf_f32_matmul_into(
    void* ctx,
    void* handle_out,
    void* handle_a,
    void* handle_b,
    int m,
    int k,
    int n,
    int transpose_a,
    int transpose_b
);
void* mojor_gpu_buf_f32_slice(
    void* ctx,
    void* handle,
    const int* starts,
    const int* sizes,
    const int* dims,
    int ndim
);
void* mojor_gpu_buf_f32_gather(
    void* ctx,
    void* handle,
    const int* idx_data,
    const int* idx_offsets,
    const int* idx_lens,
    const int* dims,
    int ndim
);
int mojor_gpu_buf_f32_scatter(
    void* ctx,
    void* handle,
    void* values_handle,
    const int* idx_data,
    const int* idx_offsets,
    const int* idx_lens,
    const int* dims,
    int ndim
);
void* mojor_gpu_idx_plan_f32_create(
    void* ctx,
    const int* idx_data,
    const int* idx_offsets,
    const int* idx_lens,
    const int* dims,
    int ndim
);
int mojor_gpu_idx_plan_f32_free(
    void* ctx,
    void* plan_handle
);
void* mojor_gpu_buf_f32_gather_plan(
    void* ctx,
    void* handle,
    void* plan_handle
);
int mojor_gpu_buf_f32_scatter_plan(
    void* ctx,
    void* handle,
    void* values_handle,
    void* plan_handle
);
void* mojor_gpu_buf_f64_slice(
    void* ctx,
    void* handle,
    const int* starts,
    const int* sizes,
    const int* dims,
    int ndim
);
void* mojor_gpu_buf_f64_gather(
    void* ctx,
    void* handle,
    const int* idx_data,
    const int* idx_offsets,
    const int* idx_lens,
    const int* dims,
    int ndim
);
int mojor_gpu_buf_f64_scatter(
    void* ctx,
    void* handle,
    void* values_handle,
    const int* idx_data,
    const int* idx_offsets,
    const int* idx_lens,
    const int* dims,
    int ndim
);
void* mojor_gpu_buf_i32_slice(
    void* ctx,
    void* handle,
    const int* starts,
    const int* sizes,
    const int* dims,
    int ndim
);
int mojor_gpu_buf_i32_slice_assign(
    void* ctx,
    void* handle,
    void* values_handle,
    const int* starts,
    const int* sizes,
    const int* dims,
    int ndim
);
void* mojor_gpu_buf_i32_gather(
    void* ctx,
    void* handle,
    const int* idx_data,
    const int* idx_offsets,
    const int* idx_lens,
    const int* dims,
    int ndim
);
int mojor_gpu_buf_i32_scatter(
    void* ctx,
    void* handle,
    void* values_handle,
    const int* idx_data,
    const int* idx_offsets,
    const int* idx_lens,
    const int* dims,
    int ndim
);
int mojor_gpu_cap_f64_matmul(void* ctx);
int mojor_gpu_cap_f64_reduce(void* ctx, int op);
int mojor_gpu_cap_i32_scatter(void* ctx);
void* mojor_gpu_buf_i32_reduce(
    void* ctx,
    void* handle,
    int op,
    const int* dims,
    int ndim,
    int keepdims
);
void* mojor_gpu_buf_i32_matmul(
    void* ctx,
    void* handle_a,
    void* handle_b,
    int m,
    int k,
    int n,
    int transpose_a,
    int transpose_b
);
int mojor_gpu_buf_i32_matmul_into(
    void* ctx,
    void* handle_out,
    void* handle_a,
    void* handle_b,
    int m,
    int k,
    int n,
    int transpose_a,
    int transpose_b
);
void* mojor_gpu_idx_plan_f64_create(
    void* ctx,
    const int* idx_data,
    const int* idx_offsets,
    const int* idx_lens,
    const int* dims,
    int ndim
);
int mojor_gpu_idx_plan_f64_free(
    void* ctx,
    void* plan_handle
);
void* mojor_gpu_buf_f64_gather_plan(
    void* ctx,
    void* handle,
    void* plan_handle
);
int mojor_gpu_buf_f64_scatter_plan(
    void* ctx,
    void* handle,
    void* values_handle,
    void* plan_handle
);
void* mojor_gpu_buf_f64_reduce(
    void* ctx,
    void* handle,
    int op,
    const int* dims,
    int ndim,
    int keepdims
);
void* mojor_gpu_buf_f64_matmul(
    void* ctx,
    void* handle_a,
    void* handle_b,
    int m,
    int k,
    int n,
    int transpose_a,
    int transpose_b
);
int mojor_gpu_buf_f64_matmul_into(
    void* ctx,
    void* handle_out,
    void* handle_a,
    void* handle_b,
    int m,
    int k,
    int n,
    int transpose_a,
    int transpose_b
);

// Runtime v1 compatibility aliases (forward to existing ABI).
double mojor_rt_v1_sum_f64(const double* x, int n);
double mojor_rt_v1_prod_f64(const double* x, int n);
int mojor_rt_v1_has_gpu(void);
void* mojor_rt_v1_gpu_ctx_create(void);
int mojor_rt_v1_gpu_ctx_free(void* ctx);
int mojor_rt_v1_gpu_meminfo(void* ctx, uint64_t* out);
void* mojor_rt_v1_gpu_buf_f32_alloc(void* ctx, int n);
int mojor_rt_v1_gpu_buf_f32_free(void* ctx, void* handle);
int mojor_rt_v1_gpu_buf_f32_len(void* ctx, void* handle);
int mojor_rt_v1_gpu_buf_f32_write(void* ctx, void* handle, const float* host, int n);
int mojor_rt_v1_gpu_buf_f32_read(void* ctx, void* handle, float* host, int n);
void* mojor_rt_v1_gpu_buf_f64_alloc(void* ctx, int n);
int mojor_rt_v1_gpu_buf_f64_free(void* ctx, void* handle);
int mojor_rt_v1_gpu_buf_f64_len(void* ctx, void* handle);
int mojor_rt_v1_gpu_buf_f64_write(void* ctx, void* handle, const double* host, int n);
int mojor_rt_v1_gpu_buf_f64_read(void* ctx, void* handle, double* host, int n);
void* mojor_rt_v1_gpu_buf_f64_reduce(
    void* ctx,
    void* handle,
    int op,
    const int* dims,
    int ndim,
    int keepdims
);
void* mojor_rt_v1_gpu_buf_f32_matmul(
    void* ctx,
    void* handle_a,
    void* handle_b,
    int m,
    int k,
    int n,
    int transpose_a,
    int transpose_b
);
int mojor_rt_v1_gpu_buf_f32_matmul_into(
    void* ctx,
    void* handle_out,
    void* handle_a,
    void* handle_b,
    int m,
    int k,
    int n,
    int transpose_a,
    int transpose_b
);
int mojor_ir_emit_stmt_pilot(
    const char* typed_lir_payload,
    int payload_len,
    const char* legacy_text,
    int legacy_len,
    int mode_hint,
    char* out_text,
    int out_cap,
    int* out_len
);

// BinCount (histogram)
void mojor_bincount_f64(
    const double* x,
    const double* breaks,
    int* counts,
    int n,
    int nbreaks,
    int right,
    int include_lowest
);

// Random Number Generation (RNG)
void mojor_rng_seed(int seed);
void mojor_runif(double* out, int n, uint64_t* state);
void mojor_runif_range(double* out, int n, double min_val, double max_val, uint64_t* state);
void mojor_rnorm(double* out, int n, uint64_t* state);
void mojor_rnorm_mean_sd(double* out, int n, double mean, double sd, uint64_t* state);
void mojor_rgamma(double* out, int n, double shape, double rate, uint64_t* state);
void mojor_rbinom(double* out, int n, int size, double prob, uint64_t* state);
void mojor_rexp(double* out, int n, double rate, uint64_t* state);
void mojor_rpois(double* out, int n, double lambda, uint64_t* state);
void mojor_rlnorm(double* out, int n, double meanlog, double sdlog, uint64_t* state);
void mojor_rchisq(double* out, int n, double df, uint64_t* state);
void mojor_rt(double* out, int n, double df, uint64_t* state);
void mojor_rf(double* out, int n, double df1, double df2, uint64_t* state);
void mojor_rbeta(double* out, int n, double shape1, double shape2, uint64_t* state);
void mojor_rweibull(double* out, int n, double shape, double scale, uint64_t* state);
void mojor_rlogis(double* out, int n, double location, double scale, uint64_t* state);
void mojor_rcauchy(double* out, int n, double location, double scale, uint64_t* state);
void mojor_rgeom(double* out, int n, double prob, uint64_t* state);
void mojor_rnbinom(double* out, int n, double size, double prob, uint64_t* state);
void mojor_rhyper(double* out, int n, int m, int n_bad, int k, uint64_t* state);
void mojor_rsignrank(double* out, int n, int n_sign, uint64_t* state);
void mojor_rwilcox(double* out, int n, int m, int n_w, uint64_t* state);


#ifdef __cplusplus
}
#endif

#endif
