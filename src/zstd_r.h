#ifndef ZSTD_R_H
#define ZSTD_R_H

#include <Rinternals.h>
#include <stdio.h>
#define ZSTD_STATIC_LINKING_ONLY
#include "zstd.h"

/* Opens `path_utf8` (interpreted as UTF-8, as returned by
 * Rf_translateCharUTF8()), using a wide-char path on Windows so non-ASCII
 * paths work regardless of the current locale/codepage. */
FILE* zstd_fopen(const char* path_utf8, const char* mode);

#ifdef _WIN32
#include <wchar.h>
/* Converts a UTF-8 string to a newly R_alloc()-ed UTF-16 string, for the
 * wide-char Windows APIs. Returns NULL if `str_utf8` is not valid UTF-8. */
wchar_t* zstd_utf8_to_wide(const char* str_utf8);
#endif

/* Applies the common advanced compression parameters (window_log,
 * checksum, strategy, nb_workers, content_size, dict_id,
 * long_distance_matching) to `cctx`. Each SEXP is either R_NilValue
 * (leave at the zstd default) or a scalar of the appropriate type,
 * as validated on the R side. */
void zstd_set_common_cparams(ZSTD_CCtx* cctx, SEXP window_log, SEXP checksum,
                             SEXP strategy, SEXP nb_workers, SEXP content_size,
                             SEXP dict_id, SEXP ldm);

#endif
