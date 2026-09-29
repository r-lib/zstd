#define R_NO_REMAP
#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>

SEXP zstd_mem_compress_(SEXP x, SEXP level, SEXP dict, SEXP window_log,
                        SEXP checksum, SEXP strategy, SEXP nb_workers,
                        SEXP content_size, SEXP dict_id, SEXP ldm);
SEXP zstd_mem_decompress_(SEXP x, SEXP dict);
SEXP zstd_info_(SEXP x);
SEXP zstd_min_clevel_(void);
SEXP zstd_max_clevel_(void);
SEXP zstd_default_clevel_(void);
SEXP zstd_train_dict_(SEXP samples, SEXP buffer_capacity);
SEXP zstd_compress_file_(SEXP input, SEXP output, SEXP level, SEXP dict,
                         SEXP window_log, SEXP checksum, SEXP strategy,
                         SEXP nb_workers, SEXP content_size, SEXP dict_id,
                         SEXP ldm);
SEXP zstd_decompress_file_(SEXP input, SEXP output, SEXP dict);
SEXP zstd_tar_compress_(SEXP files, SEXP names, SEXP isdir, SEXP output,
                        SEXP level, SEXP dict, SEXP window_log, SEXP checksum,
                        SEXP strategy, SEXP nb_workers, SEXP content_size,
                        SEXP dict_id, SEXP ldm);
SEXP zstd_tar_decompress_(SEXP input, SEXP exdir, SEXP dict, SEXP tmp);
SEXP glue_(SEXP x, SEXP f, SEXP open_arg, SEXP close_arg, SEXP cli_arg);
SEXP trim_(SEXP x);

static const R_CallMethodDef CallEntries[] = {
    {"zstd_mem_compress_", (DL_FUNC)&zstd_mem_compress_, 10},
    {"zstd_mem_decompress_", (DL_FUNC)&zstd_mem_decompress_, 2},
    {"zstd_info_", (DL_FUNC)&zstd_info_, 1},
    {"zstd_min_clevel_", (DL_FUNC)&zstd_min_clevel_, 0},
    {"zstd_max_clevel_", (DL_FUNC)&zstd_max_clevel_, 0},
    {"zstd_default_clevel_", (DL_FUNC)&zstd_default_clevel_, 0},
    {"zstd_train_dict_", (DL_FUNC)&zstd_train_dict_, 2},
    {"zstd_compress_file_", (DL_FUNC)&zstd_compress_file_, 11},
    {"zstd_decompress_file_", (DL_FUNC)&zstd_decompress_file_, 3},
    {"zstd_tar_compress_", (DL_FUNC)&zstd_tar_compress_, 13},
    {"zstd_tar_decompress_", (DL_FUNC)&zstd_tar_decompress_, 4},
    {"glue_", (DL_FUNC)&glue_, 5},
    {"trim_", (DL_FUNC)&trim_, 1},
    {NULL, NULL, 0}};

void R_init_zstd(DllInfo* dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
