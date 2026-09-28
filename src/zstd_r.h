#ifndef ZSTD_R_H
#define ZSTD_R_H

#include <Rinternals.h>
#define ZSTD_STATIC_LINKING_ONLY
#include "zstd.h"

/* Applies the common advanced compression parameters (window_log,
 * checksum, strategy, nb_workers, content_size, dict_id,
 * long_distance_matching) to `cctx`. Each SEXP is either R_NilValue
 * (leave at the zstd default) or a scalar of the appropriate type,
 * as validated on the R side. */
void zstd_set_common_cparams(
  ZSTD_CCtx *cctx,
  SEXP window_log,
  SEXP checksum,
  SEXP strategy,
  SEXP nb_workers,
  SEXP content_size,
  SEXP dict_id,
  SEXP ldm
);

#endif
