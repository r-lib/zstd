#define R_NO_REMAP
#include <string.h>
#include <R.h>
#include <Rinternals.h>
#include "zstd.h"

SEXP zstd_compress_(SEXP x, SEXP level) {
  if (TYPEOF(x) != RAWSXP) Rf_error("`x` must be a raw vector");
  size_t srcSize = (size_t) XLENGTH(x);
  int lvl = Rf_asInteger(level);

  size_t bound = ZSTD_compressBound(srcSize);
  if (ZSTD_isError(bound)) {
    Rf_error("zstd error: %s", ZSTD_getErrorName(bound));    // # nocov
  }

  SEXP out = PROTECT(Rf_allocVector(RAWSXP, (R_xlen_t) bound));
  size_t written = ZSTD_compress(
    RAW(out), bound,
    srcSize ? RAW(x) : NULL, srcSize,
    lvl
  );
  if (ZSTD_isError(written)) {
    // # nocov start
    UNPROTECT(1);
    Rf_error("zstd compression error: %s", ZSTD_getErrorName(written));
    // # nocov end
  }

  SEXP res = PROTECT(Rf_allocVector(RAWSXP, (R_xlen_t) written));
  memcpy(RAW(res), RAW(out), written);
  UNPROTECT(2);
  return res;
}

SEXP zstd_decompress_(SEXP x) {
  if (TYPEOF(x) != RAWSXP) Rf_error("`x` must be a raw vector");
  size_t srcSize = (size_t) XLENGTH(x);

  unsigned long long contentSize =
    ZSTD_getFrameContentSize(srcSize ? RAW(x) : NULL, srcSize);
  if (contentSize == ZSTD_CONTENTSIZE_ERROR) {
    Rf_error("Invalid or corrupt zstd frame, cannot determine decompressed size");
  }
  if (contentSize == ZSTD_CONTENTSIZE_UNKNOWN) {
    // # nocov start
    Rf_error(
      "Cannot determine decompressed size of this zstd frame "
      "(streaming frames are not supported)"
    );
    // # nocov end
  }

  SEXP out = PROTECT(Rf_allocVector(RAWSXP, (R_xlen_t) contentSize));
  size_t written = ZSTD_decompress(
    RAW(out), (size_t) contentSize,
    RAW(x), srcSize
  );
  if (ZSTD_isError(written)) {
    UNPROTECT(1);
    Rf_error("zstd decompression error: %s", ZSTD_getErrorName(written));
  }
  if (written != (size_t) contentSize) {
    // # nocov start
    UNPROTECT(1);
    Rf_error("zstd decompression size mismatch (corrupt frame?)");
    // # nocov end
  }

  UNPROTECT(1);
  return out;
}

SEXP zstd_min_clevel_(void) {
  return Rf_ScalarInteger(ZSTD_minCLevel());
}

SEXP zstd_max_clevel_(void) {
  return Rf_ScalarInteger(ZSTD_maxCLevel());
}

SEXP zstd_default_clevel_(void) {
  return Rf_ScalarInteger(ZSTD_defaultCLevel());
}
