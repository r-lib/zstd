/*
 * Copyright (c) 2017 rxi
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 *
 * ---
 *
 * Modified for the 'zstd' R package (see 'inst/COPYRIGHTS'):
 *  - the raw on-disk header now fills in the ustar 'magic'/'version' fields
 *    and the 155-byte 'prefix' field (previously unused padding), so entry
 *    names/paths up to ~255 bytes are supported (split across 'prefix' and
 *    'name', like GNU tar/bsdtar do for ustar-format archives), instead of
 *    upstream's 99-byte limit.
 *  - all name/linkname copies are now bounds-checked and return
 *    MTAR_ENAMETOOLONG on overflow, instead of upstream's unchecked
 *    strcpy() (which could overflow the fixed-size header fields).
 *  - numeric header fields are now filled with snprintf() instead of
 *    upstream's unchecked sprintf().
 */

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

#include "microtar.h"

typedef struct {
  char name[100];
  char mode[8];
  char owner[8];
  char group[8];
  char size[12];
  char mtime[12];
  char checksum[8];
  char type;
  char linkname[100];
  char magic[6];
  char version[2];
  char uname[32];
  char gname[32];
  char devmajor[8];
  char devminor[8];
  char prefix[155];
  char _padding[12];
} mtar_raw_header_t;


static unsigned round_up(unsigned n, unsigned incr) {
  return n + (incr - n % incr) % incr;
}


static unsigned checksum(const mtar_raw_header_t* rh) {
  unsigned i;
  unsigned char *p = (unsigned char*) rh;
  unsigned res = 256;
  for (i = 0; i < offsetof(mtar_raw_header_t, checksum); i++) {
    res += p[i];
  }
  for (i = offsetof(mtar_raw_header_t, type); i < sizeof(*rh); i++) {
    res += p[i];
  }
  return res;
}


static int tread(mtar_t *tar, void *data, unsigned size) {
  int err = tar->read(tar, data, size);
  tar->pos += size;
  return err;
}


static int twrite(mtar_t *tar, const void *data, unsigned size) {
  int err = tar->write(tar, data, size);
  tar->pos += size;
  return err;
}


static int write_null_bytes(mtar_t *tar, int n) {
  int i, err;
  char nul = '\0';
  for (i = 0; i < n; i++) {
    err = twrite(tar, &nul, 1);
    if (err) {
      return err;
    }
  }
  return MTAR_ESUCCESS;
}


/* Bounds-checked copy: returns MTAR_ENAMETOOLONG if `src` (plus terminating
 * nul) does not fit in a buffer of size `dstsize`, instead of overflowing
 * it like upstream's unchecked strcpy(). */
static int safe_copy(char *dst, size_t dstsize, const char *src) {
  size_t len = strlen(src);
  if (len + 1 > dstsize) {
    return MTAR_ENAMETOOLONG;
  }
  memcpy(dst, src, len + 1);
  return MTAR_ESUCCESS;
}


/* Splits `name` into a ustar 'prefix' and 'name' pair: `name_out` (up to 99
 * bytes + nul) and `prefix_out` (up to 154 bytes + nul), such that the
 * original path is `prefix_out + "/" + name_out`. Picks the rightmost '/'
 * that makes both halves fit. Returns MTAR_ENAMETOOLONG if no split works
 * (e.g. a single path component longer than 99 bytes, or the full path
 * longer than 254 bytes). */
static int split_name(const char *name, char *prefix_out, char *name_out) {
  size_t len = strlen(name);

  prefix_out[0] = '\0';
  if (len < sizeof(((mtar_raw_header_t *) 0)->name)) {
    return safe_copy(name_out, 100, name);
  }

  for (size_t i = len; i > 0; i--) {
    if (name[i - 1] != '/') {
      continue;
    }
    size_t prefix_len = i - 1;
    size_t suffix_len = len - i;
    if (suffix_len > 0 && suffix_len < 100 && prefix_len < 155) {
      memcpy(prefix_out, name, prefix_len);
      prefix_out[prefix_len] = '\0';
      memcpy(name_out, name + i, suffix_len + 1);
      return MTAR_ESUCCESS;
    }
  }

  return MTAR_ENAMETOOLONG;
}


static int raw_to_header(mtar_header_t *h, const mtar_raw_header_t *rh) {
  unsigned chksum1, chksum2;

  /* If the checksum starts with a null byte we assume the record is NULL */
  if (*rh->checksum == '\0') {
    return MTAR_ENULLRECORD;
  }

  /* Build and compare checksum */
  chksum1 = checksum(rh);
  sscanf(rh->checksum, "%o", &chksum2);
  if (chksum1 != chksum2) {
    return MTAR_EBADCHKSUM;
  }

  /* Load raw header into header */
  sscanf(rh->mode, "%o", &h->mode);
  sscanf(rh->owner, "%o", &h->owner);
  sscanf(rh->size, "%o", &h->size);
  sscanf(rh->mtime, "%o", &h->mtime);
  h->type = rh->type;

  /* Reconstruct the full name from 'prefix' + '/' + 'name' when the
   * archive is in ustar format and 'prefix' is set. */
  if (memcmp(rh->magic, "ustar", 5) == 0 && rh->prefix[0] != '\0') {
    /* rh->name/rh->prefix are not guaranteed to be nul-terminated if they
     * fill their entire field, so bound the length explicitly. */
    char name_buf[101], prefix_buf[156];
    size_t name_len = strnlen(rh->name, sizeof(rh->name));
    size_t prefix_len = strnlen(rh->prefix, sizeof(rh->prefix));
    memcpy(name_buf, rh->name, name_len);
    name_buf[name_len] = '\0';
    memcpy(prefix_buf, rh->prefix, prefix_len);
    prefix_buf[prefix_len] = '\0';
    if (prefix_len + 1 + name_len + 1 > sizeof(h->name)) {
      return MTAR_ENAMETOOLONG;
    }
    snprintf(h->name, sizeof(h->name), "%s/%s", prefix_buf, name_buf);
  } else {
    size_t name_len = strnlen(rh->name, sizeof(rh->name));
    if (name_len + 1 > sizeof(h->name)) {
      return MTAR_ENAMETOOLONG;    // # nocov
    }
    memcpy(h->name, rh->name, name_len);
    h->name[name_len] = '\0';
  }

  {
    size_t linkname_len = strnlen(rh->linkname, sizeof(rh->linkname));
    if (linkname_len + 1 > sizeof(h->linkname)) {
      return MTAR_ENAMETOOLONG;    // # nocov
    }
    memcpy(h->linkname, rh->linkname, linkname_len);
    h->linkname[linkname_len] = '\0';
  }

  return MTAR_ESUCCESS;
}


static int header_to_raw(mtar_raw_header_t *rh, const mtar_header_t *h) {
  unsigned chksum;
  int err;

  /* Load header into raw header */
  memset(rh, 0, sizeof(*rh));
  snprintf(rh->mode, sizeof(rh->mode), "%o", h->mode);
  snprintf(rh->owner, sizeof(rh->owner), "%o", h->owner);
  snprintf(rh->size, sizeof(rh->size), "%o", h->size);
  snprintf(rh->mtime, sizeof(rh->mtime), "%o", h->mtime);
  rh->type = h->type ? h->type : MTAR_TREG;
  memcpy(rh->magic, "ustar", 5);
  memcpy(rh->version, "00", 2);

  err = split_name(h->name, rh->prefix, rh->name);
  if (err) {
    return err;
  }
  err = safe_copy(rh->linkname, sizeof(rh->linkname), h->linkname);
  if (err) {
    return err;
  }

  /* Calculate and write checksum */
  chksum = checksum(rh);
  snprintf(rh->checksum, sizeof(rh->checksum), "%06o", chksum);
  rh->checksum[7] = ' ';

  return MTAR_ESUCCESS;
}


const char* mtar_strerror(int err) {
  switch (err) {
    case MTAR_ESUCCESS     : return "success";
    case MTAR_EFAILURE     : return "failure";
    case MTAR_EOPENFAIL    : return "could not open";
    case MTAR_EREADFAIL    : return "could not read";
    case MTAR_EWRITEFAIL   : return "could not write";
    case MTAR_ESEEKFAIL    : return "could not seek";
    case MTAR_EBADCHKSUM   : return "bad checksum";
    case MTAR_ENULLRECORD  : return "null record";
    case MTAR_ENOTFOUND    : return "file not found";
    case MTAR_ENAMETOOLONG : return "name too long";
  }
  return "unknown error";
}


static int file_write(mtar_t *tar, const void *data, unsigned size) {
  unsigned res = fwrite(data, 1, size, tar->stream);
  return (res == size) ? MTAR_ESUCCESS : MTAR_EWRITEFAIL;
}

static int file_read(mtar_t *tar, void *data, unsigned size) {
  unsigned res = fread(data, 1, size, tar->stream);
  return (res == size) ? MTAR_ESUCCESS : MTAR_EREADFAIL;
}

static int file_seek(mtar_t *tar, unsigned offset) {
  int res = fseek(tar->stream, offset, SEEK_SET);
  return (res == 0) ? MTAR_ESUCCESS : MTAR_ESEEKFAIL;
}

static int file_close(mtar_t *tar) {
  fclose(tar->stream);
  return MTAR_ESUCCESS;
}


int mtar_open(mtar_t *tar, const char *filename, const char *mode) {
  int err;
  mtar_header_t h;

  /* Init tar struct and functions */
  memset(tar, 0, sizeof(*tar));
  tar->write = file_write;
  tar->read = file_read;
  tar->seek = file_seek;
  tar->close = file_close;

  /* Assure mode is always binary */
  if ( strchr(mode, 'r') ) mode = "rb";
  if ( strchr(mode, 'w') ) mode = "wb";
  if ( strchr(mode, 'a') ) mode = "ab";
  /* Open file */
  tar->stream = fopen(filename, mode);
  if (!tar->stream) {
    return MTAR_EOPENFAIL;
  }
  /* Read first header to check it is valid if mode is `r` */
  if (*mode == 'r') {
    err = mtar_read_header(tar, &h);
    if (err != MTAR_ESUCCESS) {
      mtar_close(tar);
      return err;
    }
  }

  /* Return ok */
  return MTAR_ESUCCESS;
}


int mtar_close(mtar_t *tar) {
  return tar->close(tar);
}


int mtar_seek(mtar_t *tar, unsigned pos) {
  int err = tar->seek(tar, pos);
  tar->pos = pos;
  return err;
}


int mtar_rewind(mtar_t *tar) {
  tar->remaining_data = 0;
  tar->last_header = 0;
  return mtar_seek(tar, 0);
}


int mtar_next(mtar_t *tar) {
  int err, n;
  mtar_header_t h;
  /* Load header */
  err = mtar_read_header(tar, &h);
  if (err) {
    return err;
  }
  /* Seek to next record */
  n = round_up(h.size, 512) + sizeof(mtar_raw_header_t);
  return mtar_seek(tar, tar->pos + n);
}


int mtar_find(mtar_t *tar, const char *name, mtar_header_t *h) {
  int err;
  mtar_header_t header;
  /* Start at beginning */
  err = mtar_rewind(tar);
  if (err) {
    return err;
  }
  /* Iterate all files until we hit an error or find the file */
  while ( (err = mtar_read_header(tar, &header)) == MTAR_ESUCCESS ) {
    if ( !strcmp(header.name, name) ) {
      if (h) {
        *h = header;
      }
      return MTAR_ESUCCESS;
    }
    mtar_next(tar);
  }
  /* Return error */
  if (err == MTAR_ENULLRECORD) {
    err = MTAR_ENOTFOUND;
  }
  return err;
}


int mtar_read_header(mtar_t *tar, mtar_header_t *h) {
  int err;
  mtar_raw_header_t rh;
  /* Save header position */
  tar->last_header = tar->pos;
  /* Read raw header */
  err = tread(tar, &rh, sizeof(rh));
  if (err) {
    return err;
  }
  /* Seek back to start of header */
  err = mtar_seek(tar, tar->last_header);
  if (err) {
    return err;
  }
  /* Load raw header into header struct and return */
  return raw_to_header(h, &rh);
}


int mtar_read_data(mtar_t *tar, void *ptr, unsigned size) {
  int err;
  /* If we have no remaining data then this is the first read, we get the size,
   * set the remaining data and seek to the beginning of the data */
  if (tar->remaining_data == 0) {
    mtar_header_t h;
    /* Read header */
    err = mtar_read_header(tar, &h);
    if (err) {
      return err;
    }
    /* Seek past header and init remaining data */
    err = mtar_seek(tar, tar->pos + sizeof(mtar_raw_header_t));
    if (err) {
      return err;
    }
    tar->remaining_data = h.size;
  }
  /* Read data */
  err = tread(tar, ptr, size);
  if (err) {
    return err;
  }
  tar->remaining_data -= size;
  /* If there is no remaining data we've finished reading and seek back to the
   * header */
  if (tar->remaining_data == 0) {
    return mtar_seek(tar, tar->last_header);
  }
  return MTAR_ESUCCESS;
}


int mtar_write_header(mtar_t *tar, const mtar_header_t *h) {
  mtar_raw_header_t rh;
  int err;
  /* Build raw header and write */
  err = header_to_raw(&rh, h);
  if (err) {
    return err;
  }
  tar->remaining_data = h->size;
  return twrite(tar, &rh, sizeof(rh));
}


int mtar_write_file_header(mtar_t *tar, const char *name, unsigned size) {
  mtar_header_t h;
  int err;
  /* Build header */
  memset(&h, 0, sizeof(h));
  err = safe_copy(h.name, sizeof(h.name), name);
  if (err) {
    return err;
  }
  h.size = size;
  h.type = MTAR_TREG;
  h.mode = 0664;
  /* Write header */
  return mtar_write_header(tar, &h);
}


int mtar_write_dir_header(mtar_t *tar, const char *name) {
  mtar_header_t h;
  int err;
  /* Build header */
  memset(&h, 0, sizeof(h));
  err = safe_copy(h.name, sizeof(h.name), name);
  if (err) {
    return err;
  }
  h.type = MTAR_TDIR;
  h.mode = 0775;
  /* Write header */
  return mtar_write_header(tar, &h);
}


int mtar_write_data(mtar_t *tar, const void *data, unsigned size) {
  int err;
  /* Write data */
  err = twrite(tar, data, size);
  if (err) {
    return err;
  }
  tar->remaining_data -= size;
  /* Write padding if we've written all the data for this file */
  if (tar->remaining_data == 0) {
    return write_null_bytes(tar, round_up(tar->pos, 512) - tar->pos);
  }
  return MTAR_ESUCCESS;
}


int mtar_finalize(mtar_t *tar) {
  /* Write two NULL records */
  return write_null_bytes(tar, sizeof(mtar_raw_header_t) * 2);
}
