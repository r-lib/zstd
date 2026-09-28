#define R_NO_REMAP
#include <string.h>
#include <R.h>
#include <Rinternals.h>
#define ZSTD_STATIC_LINKING_ONLY
#include "zstd.h"
#include "zdict.h"

SEXP zstd_compress_(SEXP x, SEXP level, SEXP dict) {
  if (TYPEOF(x) != RAWSXP) Rf_error("`x` must be a raw vector");
  if (dict != R_NilValue && TYPEOF(dict) != RAWSXP) {
    Rf_error("`dict` must be a raw vector or NULL");    // # nocov
  }
  size_t srcSize = (size_t) XLENGTH(x);
  int lvl = Rf_asInteger(level);

  size_t bound = ZSTD_compressBound(srcSize);
  if (ZSTD_isError(bound)) {
    Rf_error("zstd error: %s", ZSTD_getErrorName(bound));    // # nocov
  }

  SEXP out = PROTECT(Rf_allocVector(RAWSXP, (R_xlen_t) bound));
  size_t written;
  if (dict == R_NilValue) {
    written = ZSTD_compress(
      RAW(out), bound,
      srcSize ? RAW(x) : NULL, srcSize,
      lvl
    );
  } else {
    ZSTD_CCtx *cctx = ZSTD_createCCtx();
    if (cctx == NULL) Rf_error("cannot create zstd compression context");    // # nocov
    size_t dictSize = (size_t) XLENGTH(dict);
    written = ZSTD_compress_usingDict(
      cctx,
      RAW(out), bound,
      srcSize ? RAW(x) : NULL, srcSize,
      dictSize ? RAW(dict) : NULL, dictSize,
      lvl
    );
    ZSTD_freeCCtx(cctx);
  }
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

SEXP zstd_decompress_(SEXP x, SEXP dict) {
  if (TYPEOF(x) != RAWSXP) Rf_error("`x` must be a raw vector");
  if (dict != R_NilValue && TYPEOF(dict) != RAWSXP) {
    Rf_error("`dict` must be a raw vector or NULL");    // # nocov
  }
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
  size_t written;
  if (dict == R_NilValue) {
    written = ZSTD_decompress(
      RAW(out), (size_t) contentSize,
      RAW(x), srcSize
    );
  } else {
    ZSTD_DCtx *dctx = ZSTD_createDCtx();
    if (dctx == NULL) Rf_error("cannot create zstd decompression context");    // # nocov
    size_t dictSize = (size_t) XLENGTH(dict);
    written = ZSTD_decompress_usingDict(
      dctx,
      RAW(out), (size_t) contentSize,
      RAW(x), srcSize,
      dictSize ? RAW(dict) : NULL, dictSize
    );
    ZSTD_freeDCtx(dctx);
  }
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

SEXP zstd_train_dict_(SEXP samples, SEXP buffer_capacity) {
  if (TYPEOF(samples) != VECSXP) Rf_error("`samples` must be a list of raw vectors");    // # nocov
  R_xlen_t nbSamples = XLENGTH(samples);
  size_t capacity = (size_t) Rf_asReal(buffer_capacity);

  size_t totalSize = 0;
  for (R_xlen_t i = 0; i < nbSamples; i++) {
    SEXP el = VECTOR_ELT(samples, i);
    if (TYPEOF(el) != RAWSXP) Rf_error("`samples` must be a list of raw vectors");
    totalSize += (size_t) XLENGTH(el);
  }

  unsigned char *buffer = (unsigned char *) R_alloc(totalSize ? totalSize : 1, 1);
  size_t *sizes = (size_t *) R_alloc(nbSamples ? nbSamples : 1, sizeof(size_t));
  size_t offset = 0;
  for (R_xlen_t i = 0; i < nbSamples; i++) {
    SEXP el = VECTOR_ELT(samples, i);
    size_t len = (size_t) XLENGTH(el);
    if (len) memcpy(buffer + offset, RAW(el), len);
    sizes[i] = len;
    offset += len;
  }

  SEXP out = PROTECT(Rf_allocVector(RAWSXP, (R_xlen_t) capacity));
  size_t written = ZDICT_trainFromBuffer(
    RAW(out), capacity,
    buffer, sizes, (unsigned) nbSamples
  );
  if (ZDICT_isError(written)) {
    // # nocov start
    UNPROTECT(1);
    Rf_error("zstd dictionary training error: %s", ZDICT_getErrorName(written));
    // # nocov end
  }

  SEXP res = PROTECT(Rf_allocVector(RAWSXP, (R_xlen_t) written));
  memcpy(RAW(res), RAW(out), written);
  UNPROTECT(2);
  return res;
}

SEXP zstd_info_(SEXP x) {
  if (TYPEOF(x) != RAWSXP) Rf_error("`x` must be a raw vector");
  size_t size = (size_t) XLENGTH(x);
  const char *src = (const char*) (size ? RAW(x) : NULL);

  size_t offset = 0;
  R_xlen_t n = 0;
  while (offset < size) {
    size_t frameSize = ZSTD_findFrameCompressedSize(src + offset, size - offset);
    if (ZSTD_isError(frameSize)) {
      Rf_error(
        "Invalid or corrupt zstd data at offset %.0f: %s",
        (double) offset, ZSTD_getErrorName(frameSize)
      );
    }
    offset += frameSize;
    n++;
  }

  SEXP type            = PROTECT(Rf_allocVector(STRSXP,  n));
  SEXP compressed_size = PROTECT(Rf_allocVector(REALSXP, n));
  SEXP content_size    = PROTECT(Rf_allocVector(REALSXP, n));
  SEXP window_size     = PROTECT(Rf_allocVector(REALSXP, n));
  SEXP dict_id         = PROTECT(Rf_allocVector(INTSXP,  n));
  SEXP checksum        = PROTECT(Rf_allocVector(LGLSXP,  n));

  offset = 0;
  for (R_xlen_t i = 0; i < n; i++) {
    ZSTD_FrameHeader fh;
    size_t hret = ZSTD_getFrameHeader(&fh, src + offset, size - offset);
    if (ZSTD_isError(hret)) {
      // # nocov start
      UNPROTECT(6);
      Rf_error(
        "Invalid or corrupt zstd frame header at offset %.0f: %s",
        (double) offset, ZSTD_getErrorName(hret)
      );
      // # nocov end
    }
    if (hret != 0) {
      // # nocov start
      UNPROTECT(6);
      Rf_error("Truncated zstd frame header at offset %.0f", (double) offset);
      // # nocov end
    }

    size_t frameSize = ZSTD_findFrameCompressedSize(src + offset, size - offset);

    if (fh.frameType == ZSTD_skippableFrame) {
      SET_STRING_ELT(type, i, Rf_mkChar("skippable"));
      REAL(content_size)[i] = NA_REAL;
      REAL(window_size)[i] = NA_REAL;
      INTEGER(dict_id)[i] = NA_INTEGER;
      LOGICAL(checksum)[i] = NA_LOGICAL;
    } else {
      SET_STRING_ELT(type, i, Rf_mkChar("frame"));
      REAL(content_size)[i] = fh.frameContentSize == ZSTD_CONTENTSIZE_UNKNOWN ?
        NA_REAL : (double) fh.frameContentSize;
      REAL(window_size)[i] = (double) fh.windowSize;
      INTEGER(dict_id)[i] = (int) fh.dictID;
      LOGICAL(checksum)[i] = fh.checksumFlag ? TRUE : FALSE;
    }
    REAL(compressed_size)[i] = (double) frameSize;

    offset += frameSize;
  }

  const char *names[] = {
    "type", "compressed_size", "content_size", "window_size", "dict_id", "checksum"
  };
  SEXP res = PROTECT(Rf_allocVector(VECSXP, 6));
  SET_VECTOR_ELT(res, 0, type);
  SET_VECTOR_ELT(res, 1, compressed_size);
  SET_VECTOR_ELT(res, 2, content_size);
  SET_VECTOR_ELT(res, 3, window_size);
  SET_VECTOR_ELT(res, 4, dict_id);
  SET_VECTOR_ELT(res, 5, checksum);
  SEXP nm = PROTECT(Rf_allocVector(STRSXP, 6));
  for (int i = 0; i < 6; i++) SET_STRING_ELT(nm, i, Rf_mkChar(names[i]));
  Rf_setAttrib(res, R_NamesSymbol, nm);

  UNPROTECT(8);
  return res;
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
