#define R_NO_REMAP
#include <errno.h>
#include <string.h>
#include <R.h>
#include <Rinternals.h>
#define ZSTD_STATIC_LINKING_ONLY
#include "zstd.h"
#include "zdict.h"
#include "zstd_r.h"

#ifdef _WIN32
#include <windows.h>
FILE *zstd_fopen(const char *path_utf8, const char *mode) {
  int wlen = MultiByteToWideChar(CP_UTF8, 0, path_utf8, -1, NULL, 0);
  if (wlen == 0) return NULL;
  wchar_t *wpath = (wchar_t *) R_alloc(wlen, sizeof(wchar_t));
  MultiByteToWideChar(CP_UTF8, 0, path_utf8, -1, wpath, wlen);
  wchar_t wmode[4];
  MultiByteToWideChar(CP_UTF8, 0, mode, -1, wmode, 4);
  return _wfopen(wpath, wmode);
}
#else
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>
FILE *zstd_fopen(const char *path_utf8, const char *mode) {
  return fopen(path_utf8, mode);
}
#endif

void zstd_set_common_cparams(
  ZSTD_CCtx *cctx,
  SEXP window_log,
  SEXP checksum,
  SEXP strategy,
  SEXP nb_workers,
  SEXP content_size,
  SEXP dict_id,
  SEXP ldm
) {
  size_t ret;
  if (window_log != R_NilValue) {
    ret = ZSTD_CCtx_setParameter(cctx, ZSTD_c_windowLog, Rf_asInteger(window_log));
    if (ZSTD_isError(ret)) Rf_error("zstd error setting window_log: %s", ZSTD_getErrorName(ret));
  }
  if (checksum != R_NilValue) {
    ret = ZSTD_CCtx_setParameter(cctx, ZSTD_c_checksumFlag, Rf_asLogical(checksum) ? 1 : 0);
    if (ZSTD_isError(ret)) Rf_error("zstd error setting checksum: %s", ZSTD_getErrorName(ret));    // # nocov
  }
  if (strategy != R_NilValue) {
    ret = ZSTD_CCtx_setParameter(cctx, ZSTD_c_strategy, Rf_asInteger(strategy));
    if (ZSTD_isError(ret)) Rf_error("zstd error setting strategy: %s", ZSTD_getErrorName(ret));
  }
  if (nb_workers != R_NilValue) {
    ret = ZSTD_CCtx_setParameter(cctx, ZSTD_c_nbWorkers, Rf_asInteger(nb_workers));
    if (ZSTD_isError(ret)) Rf_error("zstd error setting nb_workers: %s", ZSTD_getErrorName(ret));    // # nocov
  }
  if (content_size != R_NilValue) {
    ret = ZSTD_CCtx_setParameter(cctx, ZSTD_c_contentSizeFlag, Rf_asLogical(content_size) ? 1 : 0);
    if (ZSTD_isError(ret)) Rf_error("zstd error setting content_size: %s", ZSTD_getErrorName(ret));    // # nocov
  }
  if (dict_id != R_NilValue) {
    ret = ZSTD_CCtx_setParameter(cctx, ZSTD_c_dictIDFlag, Rf_asLogical(dict_id) ? 1 : 0);
    if (ZSTD_isError(ret)) Rf_error("zstd error setting dict_id: %s", ZSTD_getErrorName(ret));    // # nocov
  }
  if (ldm != R_NilValue) {
    ret = ZSTD_CCtx_setParameter(cctx, ZSTD_c_enableLongDistanceMatching, Rf_asLogical(ldm) ? 1 : 0);
    if (ZSTD_isError(ret)) Rf_error("zstd error setting long_distance_matching: %s", ZSTD_getErrorName(ret));    // # nocov
  }
}

SEXP zstd_mem_compress_(
  SEXP x, SEXP level, SEXP dict,
  SEXP window_log, SEXP checksum, SEXP strategy, SEXP nb_workers,
  SEXP content_size, SEXP dict_id, SEXP ldm
) {
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

  ZSTD_CCtx *cctx = ZSTD_createCCtx();
  if (cctx == NULL) Rf_error("cannot create zstd compression context");    // # nocov
  ZSTD_CCtx_setParameter(cctx, ZSTD_c_compressionLevel, lvl);
  zstd_set_common_cparams(cctx, window_log, checksum, strategy, nb_workers, content_size, dict_id, ldm);
  if (dict != R_NilValue) {
    size_t dictSize = (size_t) XLENGTH(dict);
    size_t dret = ZSTD_CCtx_loadDictionary(cctx, dictSize ? RAW(dict) : NULL, dictSize);
    if (ZSTD_isError(dret)) {
      // # nocov start
      ZSTD_freeCCtx(cctx);
      UNPROTECT(1);
      Rf_error("zstd error loading dictionary: %s", ZSTD_getErrorName(dret));
      // # nocov end
    }
  }

  size_t written = ZSTD_compress2(
    cctx,
    RAW(out), bound,
    srcSize ? RAW(x) : NULL, srcSize
  );
  ZSTD_freeCCtx(cctx);
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

SEXP zstd_mem_decompress_(SEXP x, SEXP dict) {
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

typedef struct {
  const char *data;
  size_t size;
#ifdef _WIN32
  HANDLE hFile;
  HANDLE hMap;
#else
  int fd;
#endif
} zstd_mmap_t;

static void zstd_mmap_close(zstd_mmap_t *m) {
#ifdef _WIN32
  if (m->data != NULL) UnmapViewOfFile((LPCVOID) m->data);
  if (m->hMap != NULL) CloseHandle(m->hMap);
  if (m->hFile != INVALID_HANDLE_VALUE) CloseHandle(m->hFile);
#else
  if (m->data != NULL) munmap((void *) m->data, m->size);
  if (m->fd >= 0) close(m->fd);
#endif
}

#ifdef _WIN32
static void zstd_mmap_open(const char *path, zstd_mmap_t *m) {
  m->data = NULL;
  m->size = 0;
  m->hFile = INVALID_HANDLE_VALUE;
  m->hMap = NULL;

  int wlen = MultiByteToWideChar(CP_UTF8, 0, path, -1, NULL, 0);
  if (wlen == 0) {
    Rf_error("Cannot convert path '%s' to UTF-16", path);    // # nocov
  }
  wchar_t *wpath = (wchar_t *) R_alloc(wlen, sizeof(wchar_t));
  MultiByteToWideChar(CP_UTF8, 0, path, -1, wpath, wlen);

  m->hFile = CreateFileW(
    wpath, GENERIC_READ, FILE_SHARE_READ, NULL,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL
  );
  if (m->hFile == INVALID_HANDLE_VALUE) {
    Rf_error("Cannot open input file '%s' (error %lu)", path, GetLastError());
  }

  LARGE_INTEGER fsize;
  if (!GetFileSizeEx(m->hFile, &fsize)) {
    // # nocov start
    zstd_mmap_close(m);
    Rf_error("Cannot get size of input file '%s' (error %lu)", path, GetLastError());
    // # nocov end
  }
  m->size = (size_t) fsize.QuadPart;

  if (m->size > 0) {
    m->hMap = CreateFileMappingW(m->hFile, NULL, PAGE_READONLY, 0, 0, NULL);
    if (m->hMap == NULL) {
      // # nocov start
      zstd_mmap_close(m);
      Rf_error("Cannot memory-map input file '%s' (error %lu)", path, GetLastError());
      // # nocov end
    }
    m->data = (const char *) MapViewOfFile(m->hMap, FILE_MAP_READ, 0, 0, 0);
    if (m->data == NULL) {
      // # nocov start
      zstd_mmap_close(m);
      Rf_error("Cannot memory-map input file '%s' (error %lu)", path, GetLastError());
      // # nocov end
    }
  }
}
#else
static void zstd_mmap_open(const char *path, zstd_mmap_t *m) {
  m->data = NULL;
  m->size = 0;
  m->fd = -1;

  m->fd = open(path, O_RDONLY);
  if (m->fd < 0) {
    Rf_error("Cannot open input file '%s': %s", path, strerror(errno));
  }

  struct stat st;
  if (fstat(m->fd, &st) != 0) {
    // # nocov start
    zstd_mmap_close(m);
    Rf_error("Cannot stat input file '%s': %s", path, strerror(errno));
    // # nocov end
  }
  m->size = (size_t) st.st_size;

  if (m->size > 0) {
    void *ptr = mmap(NULL, m->size, PROT_READ, MAP_PRIVATE, m->fd, 0);
    if (ptr == MAP_FAILED) {
      // # nocov start
      zstd_mmap_close(m);
      Rf_error("Cannot memory-map input file '%s': %s", path, strerror(errno));
      // # nocov end
    }
    m->data = (const char *) ptr;
  }
}
#endif

SEXP zstd_info_(SEXP path) {
  if (TYPEOF(path) != STRSXP || XLENGTH(path) != 1) {
    Rf_error("`path` must be a string");    // # nocov
  }
  const char *filepath = Rf_translateCharUTF8(STRING_ELT(path, 0));

  zstd_mmap_t m;
  zstd_mmap_open(filepath, &m);
  size_t size = m.size;
  const char *src = m.data;

  size_t offset = 0;
  R_xlen_t n = 0;
  while (offset < size) {
    size_t frameSize = ZSTD_findFrameCompressedSize(src + offset, size - offset);
    if (ZSTD_isError(frameSize)) {
      zstd_mmap_close(&m);
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
      zstd_mmap_close(&m);
      Rf_error(
        "Invalid or corrupt zstd frame header at offset %.0f: %s",
        (double) offset, ZSTD_getErrorName(hret)
      );
      // # nocov end
    }
    if (hret != 0) {
      // # nocov start
      UNPROTECT(6);
      zstd_mmap_close(&m);
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
  zstd_mmap_close(&m);
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
