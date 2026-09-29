#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define ZSTD_STATIC_LINKING_ONLY
#include "microtar.h"
#include "zstd.h"
#include "zstd_r.h"

#ifdef _WIN32
#include <direct.h>
#include <sys/stat.h>
#include <sys/utime.h>
#define zstd_mkdir(path) _mkdir(path)
#else
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <utime.h>
#define zstd_mkdir(path) mkdir(path, 0777)
#endif

/* Best-effort source file attributes to store in a tar entry: mode bits
 * and (outside Windows, which has no matching concept) uid/gid, so that
 * extracting the archive as the same user restores real ownership
 * instead of always recording uid/gid 0 (root). */
static void get_file_attrs(const char* path, unsigned* mode, unsigned* uid,
                           unsigned* gid) {
#ifdef _WIN32
  struct _stat st;
  *mode = (_stat(path, &st) == 0) ? (unsigned)(st.st_mode & 0777) : 0644;
  *uid = 0;
  *gid = 0;
#else
  struct stat st;
  if (stat(path, &st) == 0) {
    *mode = (unsigned)(st.st_mode & 07777);
    *uid = (unsigned)st.st_uid;
    *gid = (unsigned)st.st_gid;
  } else {
    // # nocov start
    *mode = 0644;
    *uid = 0;
    *gid = 0;
    // # nocov end
  }
#endif
}

/* ---- shared helpers ---------------------------------------------------- */

/* Sets mtime (and, outside Windows, uid/gid) on an already-extracted
 * entry. Best-effort: failures (e.g. chown() without root) are not
 * fatal, but flag `*chown_failed` so the caller can emit a single
 * summary warning for the whole archive instead of one per file. */
static void apply_attrs(const char* path, unsigned mtime, unsigned uid,
                        unsigned gid, int* chown_failed) {
#ifdef _WIN32
  struct _utimbuf ut;
  ut.actime = (time_t)mtime;
  ut.modtime = (time_t)mtime;
  if (_utime(path, &ut) != 0) *chown_failed = 1;
#else
  struct utimbuf ut;
  ut.actime = (time_t)mtime;
  ut.modtime = (time_t)mtime;
  if (utime(path, &ut) != 0) *chown_failed = 1;
  if (chown(path, (uid_t)uid, (gid_t)gid) != 0) *chown_failed = 1;
#endif
}

/* Creates `path` and all of its missing parent directories. */
static int mkdir_p(char* path) {
  size_t len = strlen(path);
  for (size_t i = 1; i < len; i++) {
    if (path[i] == '/') {
      path[i] = '\0';
      int ok = (zstd_mkdir(path) == 0) || (errno == EEXIST);
      path[i] = '/';
      if (!ok) return -1;
    }
  }
  return ((zstd_mkdir(path) == 0) || (errno == EEXIST)) ? 0 : -1;
}

/* Rejects absolute paths and paths containing a '..' segment, so archive
 * entries can never be extracted outside of the requested directory. */
static int path_is_unsafe(const char* name) {
  if (name[0] == '\0' || name[0] == '/') return 1;
#ifdef _WIN32
  if (strlen(name) >= 2 && name[1] == ':') return 1;
#endif
  const char* p = name;
  while (*p) {
    const char* seg_end = strchr(p, '/');
    size_t seg_len = seg_end ? (size_t)(seg_end - p) : strlen(p);
    if (seg_len == 0 || (seg_len == 2 && p[0] == '.' && p[1] == '.')) {
      return 1;
    }
    p += seg_len;
    if (*p == '/') p++;
  }
  return 0;
}

/* ---- compression: files -> tar stream -> zstd stream -> file ----------- */

typedef struct {
  ZSTD_CCtx* cctx;
  FILE* fout;
  const char* output_path;
  void* outBuf;
  size_t outBufSize;
  const char* last_error;
} tar_write_ctx_t;

static int tar_write_cb(mtar_t* tar, const void* data, unsigned size) {
  tar_write_ctx_t* ctx = (tar_write_ctx_t*)tar->stream;
  ZSTD_inBuffer in = {data, size, 0};
  while (in.pos < in.size) {
    ZSTD_outBuffer out = {ctx->outBuf, ctx->outBufSize, 0};
    size_t const ret =
        ZSTD_compressStream2(ctx->cctx, &out, &in, ZSTD_e_continue);
    if (ZSTD_isError(ret)) {
      ctx->last_error = ZSTD_getErrorName(ret);
      return MTAR_EWRITEFAIL;
    }
    if (out.pos) {
      size_t written = fwrite(ctx->outBuf, 1, out.pos, ctx->fout);
      if (written != out.pos) {
        ctx->last_error = "error writing output file";
        return MTAR_EWRITEFAIL;
      }
    }
  }
  return MTAR_ESUCCESS;
}

SEXP zstd_tar_compress_(SEXP files, SEXP names, SEXP isdir, SEXP output,
                        SEXP level, SEXP dict, SEXP window_log, SEXP checksum,
                        SEXP strategy, SEXP nb_workers, SEXP content_size,
                        SEXP dict_id, SEXP ldm) {
  if (TYPEOF(files) != STRSXP) Rf_error("`files` must be a character vector");
  if (TYPEOF(names) != STRSXP || XLENGTH(names) != XLENGTH(files)) {
    Rf_error(
        "`names` must be a character vector of the same length as `files`");  // # nocov
  }
  if (TYPEOF(isdir) != LGLSXP || XLENGTH(isdir) != XLENGTH(files)) {
    Rf_error(
        "`isdir` must be a logical vector of the same length as `files`");  // # nocov
  }
  if (TYPEOF(output) != STRSXP) Rf_error("`output` must be a string");
  if (dict != R_NilValue && TYPEOF(dict) != RAWSXP) {
    Rf_error("`dict` must be a raw vector or NULL");  // # nocov
  }
  const char* output_path = Rf_translateCharUTF8(STRING_ELT(output, 0));
  int lvl = Rf_asInteger(level);
  R_xlen_t n = XLENGTH(files);

  FILE* fout = zstd_fopen(output_path, "wb");
  if (fout == NULL) {
    Rf_error("Cannot open output file '%s': %s", output_path, strerror(errno));
  }

  ZSTD_CCtx* cctx = ZSTD_createCCtx();
  if (cctx == NULL) {
    // # nocov start
    fclose(fout);
    Rf_error("cannot create zstd compression context");
    // # nocov end
  }
  ZSTD_CCtx_setParameter(cctx, ZSTD_c_compressionLevel, lvl);
  zstd_set_common_cparams(cctx, window_log, checksum, strategy, nb_workers,
                          content_size, dict_id, ldm);
  if (dict != R_NilValue) {
    size_t dictSize = (size_t)XLENGTH(dict);
    size_t dret =
        ZSTD_CCtx_loadDictionary(cctx, dictSize ? RAW(dict) : NULL, dictSize);
    if (ZSTD_isError(dret)) {
      // # nocov start
      ZSTD_freeCCtx(cctx);
      fclose(fout);
      Rf_error("zstd error loading dictionary: %s", ZSTD_getErrorName(dret));
      // # nocov end
    }
  }

  size_t const inBufSize = ZSTD_CStreamInSize();
  size_t const outBufSize = ZSTD_CStreamOutSize();
  void* inBuf = R_alloc(inBufSize, 1);
  void* outBuf = R_alloc(outBufSize, 1);

  tar_write_ctx_t ctx = {cctx, fout, output_path, outBuf, outBufSize, NULL};
  mtar_t tar;
  memset(&tar, 0, sizeof(tar));
  tar.write = tar_write_cb;
  tar.stream = &ctx;

  for (R_xlen_t i = 0; i < n; i++) {
    const char* path = Rf_translateCharUTF8(STRING_ELT(files, i));
    const char* name = Rf_translateCharUTF8(STRING_ELT(names, i));
    int is_directory = LOGICAL(isdir)[i];
    size_t nlen = strlen(name);

    if (is_directory) {
      char* name_buf = (char*)R_alloc(nlen + 2, 1);
      memcpy(name_buf, name, nlen);
      if (nlen == 0 || name[nlen - 1] != '/') {
        name_buf[nlen] = '/';
        name_buf[nlen + 1] = '\0';
      } else {
        name_buf[nlen] = '\0';
      }
      unsigned mode, uid, gid;
      get_file_attrs(path, &mode, &uid, &gid);
      int err = mtar_write_dir_header(&tar, name_buf, mode, uid, gid);
      if (err) {
        ZSTD_freeCCtx(cctx);
        fclose(fout);
        Rf_error("Cannot write tar entry for '%s': %s", name,
                 mtar_strerror(err));
      }
      continue;
    }

    FILE* fin = zstd_fopen(path, "rb");
    if (fin == NULL) {
      ZSTD_freeCCtx(cctx);
      fclose(fout);
      Rf_error("Cannot open input file '%s': %s", path, strerror(errno));
    }
    if (fseek(fin, 0, SEEK_END) != 0) {
      // # nocov start
      fclose(fin);
      ZSTD_freeCCtx(cctx);
      fclose(fout);
      Rf_error("Cannot determine size of input file '%s'", path);
      // # nocov end
    }
    long fsize = ftell(fin);
    if (fsize < 0 || fseek(fin, 0, SEEK_SET) != 0) {
      // # nocov start
      fclose(fin);
      ZSTD_freeCCtx(cctx);
      fclose(fout);
      Rf_error("Cannot determine size of input file '%s'", path);
      // # nocov end
    }

    unsigned mode, uid, gid;
    get_file_attrs(path, &mode, &uid, &gid);
    int err =
        mtar_write_file_header(&tar, name, (unsigned)fsize, mode, uid, gid);
    if (err) {
      fclose(fin);
      ZSTD_freeCCtx(cctx);
      fclose(fout);
      Rf_error("Cannot write tar entry for '%s': %s", name, mtar_strerror(err));
    }

    size_t remaining = (size_t)fsize;
    while (remaining > 0) {
      size_t chunk = remaining < inBufSize ? remaining : inBufSize;
      size_t got = fread(inBuf, 1, chunk, fin);
      if (got != chunk) {
        // # nocov start
        fclose(fin);
        ZSTD_freeCCtx(cctx);
        fclose(fout);
        Rf_error("Error reading input file '%s'", path);
        // # nocov end
      }
      int werr = mtar_write_data(&tar, inBuf, (unsigned)got);
      if (werr) {
        fclose(fin);
        ZSTD_freeCCtx(cctx);
        fclose(fout);
        Rf_error("Error archiving '%s': %s", path,
                 ctx.last_error ? ctx.last_error : mtar_strerror(werr));
      }
      remaining -= got;
    }
    fclose(fin);
  }

  int ferr = mtar_finalize(&tar);
  if (ferr) {
    // # nocov start
    ZSTD_freeCCtx(cctx);
    fclose(fout);
    Rf_error("Error finalizing tar archive: %s", mtar_strerror(ferr));
    // # nocov end
  }

  {
    ZSTD_inBuffer in = {NULL, 0, 0};
    int finished = 0;
    while (!finished) {
      ZSTD_outBuffer out = {outBuf, outBufSize, 0};
      size_t const ret = ZSTD_compressStream2(cctx, &out, &in, ZSTD_e_end);
      if (ZSTD_isError(ret)) {
        // # nocov start
        ZSTD_freeCCtx(cctx);
        fclose(fout);
        Rf_error("zstd compression error: %s", ZSTD_getErrorName(ret));
        // # nocov end
      }
      if (out.pos) {
        size_t written = fwrite(outBuf, 1, out.pos, fout);
        if (written != out.pos) {
          // # nocov start
          ZSTD_freeCCtx(cctx);
          fclose(fout);
          Rf_error("Error writing output file '%s'", output_path);
          // # nocov end
        }
      }
      finished = (ret == 0);
    }
  }

  ZSTD_freeCCtx(cctx);
  if (fclose(fout) != 0) {
    // # nocov start
    Rf_error("Error closing output file '%s': %s", output_path,
             strerror(errno));
    // # nocov end
  }

  return R_NilValue;
}

/* ---- decompression: zstd file -> plain tar file -> extracted files ----- */

/* Overrides carried by a Pax extended header ('x': applies only to the
 * entry immediately following it; 'g': applies to all following entries,
 * until a later 'g' header overrides the same key again). Unset fields
 * (NULL name/linkname, has_* == 0) mean "fall back to the ustar header". */
typedef struct {
  char* name;
  char* linkname;
  int has_mtime;
  unsigned mtime;
  int has_uid;
  unsigned uid;
  int has_gid;
  unsigned gid;
  int has_size;
  unsigned size; /* only used for the mismatch check */
} pax_overrides_t;

/* Reads the data block of a Pax extended header ('x' or 'g', h->type) and
 * parses its "<len> key=value\n" records, updating only the fields in
 * `*out` whose key is present in this block. Unknown keys (atime, ctime,
 * comment, SCHILY.*, LIBARCHIVE.*, ...) are silently ignored, matching
 * this package's existing behavior of not applying ustar attributes it
 * doesn't otherwise consume. */
static void read_pax_records(mtar_t* tar, const mtar_header_t* h,
                             pax_overrides_t* out) {
  unsigned size = h->size;
  if (size == 0) return;

  char* data = (char*)R_alloc((size_t)size + 1, 1);
  unsigned remaining = size;
  size_t off = 0;
  size_t const bufSize = 1 << 16;
  while (remaining > 0) {
    unsigned chunk = remaining < bufSize ? remaining : (unsigned)bufSize;
    int rerr = mtar_read_data(tar, data + off, chunk);
    if (rerr) {
      mtar_close(tar);
      Rf_error("Error reading Pax extended header: %s", mtar_strerror(rerr));
    }
    off += chunk;
    remaining -= chunk;
  }
  data[size] = '\0';

  size_t pos = 0;
  while (pos < size) {
    size_t p = pos;
    size_t reclen = 0;
    while (p < size && data[p] >= '0' && data[p] <= '9') {
      reclen = reclen * 10 + (size_t)(data[p] - '0');
      p++;
    }
    if (p == pos || p >= size || data[p] != ' ' || reclen == 0 ||
        pos + reclen > size) {
      mtar_close(tar);
      Rf_error("Malformed Pax extended header record in tar archive");
    }
    size_t rec_end = pos + reclen;
    if (data[rec_end - 1] != '\n') {
      mtar_close(tar);
      Rf_error("Malformed Pax extended header record in tar archive");
    }

    size_t key_start = p + 1;
    size_t eq = key_start;
    while (eq < rec_end - 1 && data[eq] != '=') eq++;
    if (eq >= rec_end - 1) {
      mtar_close(tar);
      Rf_error("Malformed Pax extended header record in tar archive");
    }
    size_t key_len = eq - key_start;
    size_t val_start = eq + 1;
    size_t val_len = (rec_end - 1) - val_start;
    const char* key = data + key_start;
    const char* val = data + val_start;

    if (key_len == 4 && memcmp(key, "path", 4) == 0) {
      char* v = (char*)R_alloc(val_len + 1, 1);
      memcpy(v, val, val_len);
      v[val_len] = '\0';
      out->name = v;
    } else if (key_len == 8 && memcmp(key, "linkpath", 8) == 0) {
      char* v = (char*)R_alloc(val_len + 1, 1);
      memcpy(v, val, val_len);
      v[val_len] = '\0';
      out->linkname = v;
    } else if (key_len == 5 && memcmp(key, "mtime", 5) == 0) {
      out->has_mtime = 1;
      out->mtime = (unsigned)strtoul(val, NULL, 10);
    } else if (key_len == 3 && memcmp(key, "uid", 3) == 0) {
      out->has_uid = 1;
      out->uid = (unsigned)strtoul(val, NULL, 10);
    } else if (key_len == 3 && memcmp(key, "gid", 3) == 0) {
      out->has_gid = 1;
      out->gid = (unsigned)strtoul(val, NULL, 10);
    } else if (key_len == 4 && memcmp(key, "size", 4) == 0) {
      out->has_size = 1;
      out->size = (unsigned)strtoul(val, NULL, 10);
    }
    /* other keys intentionally ignored */

    pos = rec_end;
  }
}

static void decompress_to_file(const char* input_path, const char* output_path,
                               SEXP dict) {
  FILE* fin = zstd_fopen(input_path, "rb");
  if (fin == NULL) {
    Rf_error("Cannot open input file '%s': %s", input_path, strerror(errno));
  }
  FILE* fout = zstd_fopen(output_path, "wb");
  if (fout == NULL) {
    fclose(fin);
    Rf_error("Cannot open temporary file '%s': %s", output_path,
             strerror(errno));
  }

  ZSTD_DCtx* dctx = ZSTD_createDCtx();
  if (dctx == NULL) {
    // # nocov start
    fclose(fin);
    fclose(fout);
    Rf_error("cannot create zstd decompression context");
    // # nocov end
  }
  if (dict != R_NilValue) {
    size_t dictSize = (size_t)XLENGTH(dict);
    size_t dret =
        ZSTD_DCtx_loadDictionary(dctx, dictSize ? RAW(dict) : NULL, dictSize);
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
  void* inBuf = R_alloc(inBufSize, 1);
  void* outBuf = R_alloc(outBufSize, 1);

  size_t lastRet = 0;
  int isEmpty = 1;
  size_t nRead;
  while ((nRead = fread(inBuf, 1, inBufSize, fin)) != 0) {
    isEmpty = 0;
    ZSTD_inBuffer in = {inBuf, nRead, 0};
    while (in.pos < in.size) {
      ZSTD_outBuffer out = {outBuf, outBufSize, 0};
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
          Rf_error("Error writing temporary file '%s'", output_path);
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
    Rf_error("Error closing temporary file '%s': %s", output_path,
             strerror(errno));
    // # nocov end
  }
}

SEXP zstd_tar_decompress_(SEXP input, SEXP exdir, SEXP dict, SEXP tmp) {
  if (TYPEOF(input) != STRSXP) Rf_error("`input` must be a string");
  if (TYPEOF(exdir) != STRSXP) Rf_error("`exdir` must be a string");
  if (TYPEOF(tmp) != STRSXP) Rf_error("`tmp` must be a string");  // # nocov
  if (dict != R_NilValue && TYPEOF(dict) != RAWSXP) {
    Rf_error("`dict` must be a raw vector or NULL");  // # nocov
  }
  const char* input_path = Rf_translateCharUTF8(STRING_ELT(input, 0));
  const char* exdir_path = Rf_translateCharUTF8(STRING_ELT(exdir, 0));
  const char* tmp_path = Rf_translateCharUTF8(STRING_ELT(tmp, 0));

  decompress_to_file(input_path, tmp_path, dict);

  mtar_t tar;
  int err = mtar_open(&tar, tmp_path, "r");
  if (err) {
    Rf_error("Not a valid tar archive (after zstd decompression): %s",
             mtar_strerror(err));
  }

  size_t const bufSize = 1 << 16;
  void* buf = R_alloc(bufSize, 1);
  size_t exdir_len = strlen(exdir_path);

  pax_overrides_t pax = {0};
  pax_overrides_t global = {0};
  int chown_failed = 0;

  mtar_header_t h;
  while ((err = mtar_read_header(&tar, &h)) == MTAR_ESUCCESS) {
    if (h.type == 'x') {
      pax = (pax_overrides_t){0};
      read_pax_records(&tar, &h, &pax);
      err = mtar_next(&tar);
      if (err && err != MTAR_ENULLRECORD) {
        // # nocov start
        mtar_close(&tar);
        Rf_error("Error reading tar archive: %s", mtar_strerror(err));
        // # nocov end
      }
      continue;
    }
    if (h.type == 'g') {
      read_pax_records(&tar, &h, &global);
      err = mtar_next(&tar);
      if (err && err != MTAR_ENULLRECORD) {
        // # nocov start
        mtar_close(&tar);
        Rf_error("Error reading tar archive: %s", mtar_strerror(err));
        // # nocov end
      }
      continue;
    }
    /* 'L'/'K' are GNU long-name/long-linkname headers, a different (non-
     * Pax) extension this package doesn't understand; silently continuing
     * would misread the entry that follows (its real name is stored in
     * this header's *data*, not in the 'name' field microtar decoded).
     * Fail clearly instead. */
    if (h.type == 'L' || h.type == 'K') {
      mtar_close(&tar);
      Rf_error(
          "This tar archive uses a GNU long name/link extended header "
          "(type '%c') not supported by this package; cannot extract it "
          "safely.",
          h.type);
    }

    {
      const char* eff_name =
          pax.name ? pax.name : (global.name ? global.name : h.name);
      const char* eff_linkname =
          pax.linkname ? pax.linkname
                       : (global.linkname ? global.linkname : h.linkname);
      if (eff_name != h.name) {
        if (strlen(eff_name) + 1 > sizeof(h.name)) {
          mtar_close(&tar);
          Rf_error("Pax-overridden path too long: '%s'", eff_name);
        }
        strcpy(h.name, eff_name);
      }
      if (eff_linkname != h.linkname) {
        if (strlen(eff_linkname) + 1 > sizeof(h.linkname)) {
          mtar_close(&tar);
          Rf_error("Pax-overridden link path too long: '%s'", eff_linkname);
        }
        strcpy(h.linkname, eff_linkname);
      }
    }

    if (pax.has_size && pax.size != h.size) {
      Rf_warning(
          "Tar entry '%s': Pax 'size' (%u) does not match the ustar header "
          "size (%u); using the ustar size",
          h.name, pax.size, h.size);
    } else if (global.has_size && global.size != h.size) {
      Rf_warning(
          "Tar entry '%s': Pax global 'size' (%u) does not match the ustar "
          "header size (%u); using the ustar size",
          h.name, global.size, h.size);
    }

    unsigned eff_mtime =
        pax.has_mtime ? pax.mtime : (global.has_mtime ? global.mtime : h.mtime);
    unsigned eff_uid =
        pax.has_uid ? pax.uid : (global.has_uid ? global.uid : h.uid);
    unsigned eff_gid =
        pax.has_gid ? pax.gid : (global.has_gid ? global.gid : h.gid);
    pax = (pax_overrides_t){0};

    if (path_is_unsafe(h.name)) {
      mtar_close(&tar);
      Rf_error("Refusing to extract unsafe path from tar archive: '%s'",
               h.name);
    }

    size_t name_len = strlen(h.name);
    char* full_path = (char*)R_alloc(exdir_len + 1 + name_len + 1, 1);
    memcpy(full_path, exdir_path, exdir_len);
    full_path[exdir_len] = '/';
    memcpy(full_path + exdir_len + 1, h.name, name_len + 1);

    if (h.type == MTAR_TDIR) {
      if (mkdir_p(full_path) != 0) {
        // # nocov start
        mtar_close(&tar);
        Rf_error("Cannot create directory '%s': %s", full_path,
                 strerror(errno));
        // # nocov end
      }
      apply_attrs(full_path, eff_mtime, eff_uid, eff_gid, &chown_failed);
    } else if (h.type == MTAR_TREG) {
      char* slash = strrchr(full_path, '/');
      if (slash != NULL) {
        *slash = '\0';
        int mkerr = mkdir_p(full_path);
        *slash = '/';
        if (mkerr != 0) {
          // # nocov start
          mtar_close(&tar);
          Rf_error("Cannot create directory for '%s': %s", full_path,
                   strerror(errno));
          // # nocov end
        }
      }

      FILE* fout = zstd_fopen(full_path, "wb");
      if (fout == NULL) {
        mtar_close(&tar);
        Rf_error("Cannot create output file '%s': %s", full_path,
                 strerror(errno));
      }
      unsigned remaining = h.size;
      while (remaining > 0) {
        unsigned chunk = remaining < bufSize ? remaining : (unsigned)bufSize;
        int rerr = mtar_read_data(&tar, buf, chunk);
        if (rerr) {
          fclose(fout);
          mtar_close(&tar);
          Rf_error("Error reading tar entry '%s': %s", h.name,
                   mtar_strerror(rerr));
        }
        size_t written = fwrite(buf, 1, chunk, fout);
        if (written != chunk) {
          // # nocov start
          fclose(fout);
          mtar_close(&tar);
          Rf_error("Error writing output file '%s'", full_path);
          // # nocov end
        }
        remaining -= chunk;
      }
      if (fclose(fout) != 0) {
        // # nocov start
        mtar_close(&tar);
        Rf_error("Error closing output file '%s': %s", full_path,
                 strerror(errno));
        // # nocov end
      }
      apply_attrs(full_path, eff_mtime, eff_uid, eff_gid, &chown_failed);
    }
    /* other entry types (symlinks, devices, fifos) are skipped */

    err = mtar_next(&tar);
    if (err && err != MTAR_ENULLRECORD) {
      // # nocov start
      mtar_close(&tar);
      Rf_error("Error reading tar archive: %s", mtar_strerror(err));
      // # nocov end
    }
  }
  if (err != MTAR_ENULLRECORD) {
    mtar_close(&tar);
    Rf_error("Error reading tar archive: %s", mtar_strerror(err));
  }

  mtar_close(&tar);
  if (chown_failed) {
    Rf_warning(
        "Could not set the modification time and/or ownership of one or "
        "more extracted files (e.g. not running as root)");
  }
  return R_NilValue;
}
