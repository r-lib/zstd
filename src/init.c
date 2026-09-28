#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP zstd_mem_compress_(SEXP x, SEXP level, SEXP dict);
SEXP zstd_mem_decompress_(SEXP x, SEXP dict);
SEXP zstd_info_(SEXP x);
SEXP zstd_min_clevel_(void);
SEXP zstd_max_clevel_(void);
SEXP zstd_default_clevel_(void);
SEXP zstd_train_dict_(SEXP samples, SEXP buffer_capacity);
SEXP zstd_compress_file_(SEXP input, SEXP output, SEXP level, SEXP dict);
SEXP zstd_decompress_file_(SEXP input, SEXP output, SEXP dict);

static const R_CallMethodDef CallEntries[] = {
  {"zstd_mem_compress_",       (DL_FUNC) &zstd_mem_compress_,       3},
  {"zstd_mem_decompress_",     (DL_FUNC) &zstd_mem_decompress_,     2},
  {"zstd_info_",           (DL_FUNC) &zstd_info_,           1},
  {"zstd_min_clevel_",     (DL_FUNC) &zstd_min_clevel_,     0},
  {"zstd_max_clevel_",     (DL_FUNC) &zstd_max_clevel_,     0},
  {"zstd_default_clevel_", (DL_FUNC) &zstd_default_clevel_, 0},
  {"zstd_train_dict_",     (DL_FUNC) &zstd_train_dict_,     2},
  {"zstd_compress_file_",   (DL_FUNC) &zstd_compress_file_,   4},
  {"zstd_decompress_file_", (DL_FUNC) &zstd_decompress_file_, 3},
  {NULL, NULL, 0}
};

void R_init_zstd(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
