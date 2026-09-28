#define R_NO_REMAP
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <R.h>
#include <Rinternals.h>
#define ZSTD_STATIC_LINKING_ONLY
#include "zstd.h"
#include "zstd_r.h"

#ifdef _WIN32
#include <windows.h>
static FILE *zstd_fopen(const char *path_utf8, const char *mode) {
  int wlen = MultiByteToWideChar(CP_UTF8, 0, path_utf8, -1, NULL, 0);
  if (wlen == 0) return NULL;
  wchar_t *wpath = (wchar_t *) R_alloc(wlen, sizeof(wchar_t));
  MultiByteToWideChar(CP_UTF8, 0, path_utf8, -1, wpath, wlen);
  wchar_t wmode[4];
  MultiByteToWideChar(CP_UTF8, 0, mode, -1, wmode, 4);
  return _wfopen(wpath, wmode);
}
#else
#define zstd_fopen fopen
#endif

SEXP zstd_compress_file_(
  SEXP input, SEXP output, SEXP level, SEXP dict,
  SEXP window_log, SEXP checksum, SEXP strategy, SEXP nb_workers,
  SEXP content_size, SEXP dict_id, SEXP ldm
) {
  if (TYPEOF(input) != STRSXP) Rf_error("`input` must be a string");
  if (TYPEOF(output) != STRSXP) Rf_error("`output` must be a string");
  if (dict != R_NilValue && TYPEOF(dict) != RAWSXP) {
    Rf_error("`dict` must be a raw vector or NULL");    // # nocov
  }
  const char *input_path = Rf_translateCharUTF8(STRING_ELT(input, 0));
  const char *output_path = Rf_translateCharUTF8(STRING_ELT(output, 0));
  int lvl = Rf_asInteger(level);

  FILE *fin = zstd_fopen(input_path, "rb");
  if (fin == NULL) {
    Rf_error("Cannot open input file '%s': %s", input_path, strerror(errno));
  }
  FILE *fout = zstd_fopen(output_path, "wb");
  if (fout == NULL) {
    fclose(fin);
    Rf_error("Cannot open output file '%s': %s", output_path, strerror(errno));
  }

  ZSTD_CCtx *cctx = ZSTD_createCCtx();
  if (cctx == NULL) {
    // # nocov start
    fclose(fin);
    fclose(fout);
    Rf_error("cannot create zstd compression context");
    // # nocov end
  }
  ZSTD_CCtx_setParameter(cctx, ZSTD_c_compressionLevel, lvl);
  zstd_set_common_cparams(cctx, window_log, checksum, strategy, nb_workers, content_size, dict_id, ldm);
  if (dict != R_NilValue) {
    size_t dictSize = (size_t) XLENGTH(dict);
    size_t dret = ZSTD_CCtx_loadDictionary(
      cctx, dictSize ? RAW(dict) : NULL, dictSize
    );
    if (ZSTD_isError(dret)) {
      // # nocov start
      ZSTD_freeCCtx(cctx);
      fclose(fin);
      fclose(fout);
      Rf_error("zstd error loading dictionary: %s", ZSTD_getErrorName(dret));
      // # nocov end
    }
  }

  size_t const inBufSize = ZSTD_CStreamInSize();
  size_t const outBufSize = ZSTD_CStreamOutSize();
  void *inBuf = R_alloc(inBufSize, 1);
  void *outBuf = R_alloc(outBufSize, 1);

  size_t nRead;
  int finished = 0;
  while (!finished) {
    nRead = fread(inBuf, 1, inBufSize, fin);
    if (nRead < inBufSize && ferror(fin)) {
      // # nocov start
      ZSTD_freeCCtx(cctx);
      fclose(fin);
      fclose(fout);
      Rf_error("Error reading input file '%s'", input_path);
      // # nocov end
    }
    int lastChunk = feof(fin);
    ZSTD_EndDirective const mode = lastChunk ? ZSTD_e_end : ZSTD_e_continue;

    ZSTD_inBuffer in = { inBuf, nRead, 0 };
    int finishedChunk = 0;
    while (!finishedChunk) {
      ZSTD_outBuffer out = { outBuf, outBufSize, 0 };
      size_t const ret = ZSTD_compressStream2(cctx, &out, &in, mode);
      if (ZSTD_isError(ret)) {
        // # nocov start
        ZSTD_freeCCtx(cctx);
        fclose(fin);
        fclose(fout);
        Rf_error("zstd compression error: %s", ZSTD_getErrorName(ret));
        // # nocov end
      }
      if (out.pos) {
        size_t written = fwrite(outBuf, 1, out.pos, fout);
        if (written != out.pos) {
          // # nocov start
          ZSTD_freeCCtx(cctx);
          fclose(fin);
          fclose(fout);
          Rf_error("Error writing output file '%s'", output_path);
          // # nocov end
        }
      }
      finishedChunk = lastChunk ? (ret == 0) : (in.pos == in.size);
    }
    finished = lastChunk;
  }

  ZSTD_freeCCtx(cctx);
  fclose(fin);
  if (fclose(fout) != 0) {
    // # nocov start
    Rf_error("Error closing output file '%s': %s", output_path, strerror(errno));
    // # nocov end
  }

  return R_NilValue;
}

SEXP zstd_decompress_file_(SEXP input, SEXP output, SEXP dict) {
  if (TYPEOF(input) != STRSXP) Rf_error("`input` must be a string");
  if (TYPEOF(output) != STRSXP) Rf_error("`output` must be a string");
  if (dict != R_NilValue && TYPEOF(dict) != RAWSXP) {
    Rf_error("`dict` must be a raw vector or NULL");    // # nocov
  }
  const char *input_path = Rf_translateCharUTF8(STRING_ELT(input, 0));
  const char *output_path = Rf_translateCharUTF8(STRING_ELT(output, 0));

  FILE *fin = zstd_fopen(input_path, "rb");
  if (fin == NULL) {
    Rf_error("Cannot open input file '%s': %s", input_path, strerror(errno));
  }
  FILE *fout = zstd_fopen(output_path, "wb");
  if (fout == NULL) {
    fclose(fin);
    Rf_error("Cannot open output file '%s': %s", output_path, strerror(errno));
  }

  ZSTD_DCtx *dctx = ZSTD_createDCtx();
  if (dctx == NULL) {
    // # nocov start
    fclose(fin);
    fclose(fout);
    Rf_error("cannot create zstd decompression context");
    // # nocov end
  }
  if (dict != R_NilValue) {
    size_t dictSize = (size_t) XLENGTH(dict);
    size_t dret = ZSTD_DCtx_loadDictionary(
      dctx, dictSize ? RAW(dict) : NULL, dictSize
    );
    if (ZSTD_isError(dret)) {
      // # nocov start
      ZSTD_freeDCtx(dctx);
      fclose(fin);
      fclose(fout);
      Rf_error("zstd error loading dictionary: %s", ZSTD_getErrorName(dret));
      // # nocov end
    }
  }

  size_t const inBufSize = ZSTD_DStreamInSize();
  size_t const outBufSize = ZSTD_DStreamOutSize();
  void *inBuf = R_alloc(inBufSize, 1);
  void *outBuf = R_alloc(outBufSize, 1);

  size_t lastRet = 0;
  int isEmpty = 1;
  size_t nRead;
  while ((nRead = fread(inBuf, 1, inBufSize, fin)) != 0) {
    isEmpty = 0;
    ZSTD_inBuffer in = { inBuf, nRead, 0 };
    while (in.pos < in.size) {
      ZSTD_outBuffer out = { outBuf, outBufSize, 0 };
      size_t const ret = ZSTD_decompressStream(dctx, &out, &in);
      if (ZSTD_isError(ret)) {
        // # nocov start
        ZSTD_freeDCtx(dctx);
        fclose(fin);
        fclose(fout);
        Rf_error("zstd decompression error: %s", ZSTD_getErrorName(ret));
        // # nocov end
      }
      if (out.pos) {
        size_t written = fwrite(outBuf, 1, out.pos, fout);
        if (written != out.pos) {
          // # nocov start
          ZSTD_freeDCtx(dctx);
          fclose(fin);
          fclose(fout);
          Rf_error("Error writing output file '%s'", output_path);
          // # nocov end
        }
      }
      lastRet = ret;
    }
  }
  if (ferror(fin)) {
    // # nocov start
    ZSTD_freeDCtx(dctx);
    fclose(fin);
    fclose(fout);
    Rf_error("Error reading input file '%s'", input_path);
    // # nocov end
  }
  if (!isEmpty && lastRet != 0) {
    ZSTD_freeDCtx(dctx);
    fclose(fin);
    fclose(fout);
    Rf_error("zstd decompression error: incomplete or truncated zstd frame");
  }

  ZSTD_freeDCtx(dctx);
  fclose(fin);
  if (fclose(fout) != 0) {
    // # nocov start
    Rf_error("Error closing output file '%s': %s", output_path, strerror(errno));
    // # nocov end
  }

  return R_NilValue;
}
