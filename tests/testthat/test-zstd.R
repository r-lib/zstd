test_that("round-trip works for various inputs", {
  cases <- list(
    empty = raw(0),
    small = charToRaw("hello world"),
    text = charToRaw(paste(rep("the quick brown fox ", 500), collapse = "")),
    random = as.raw(sample(0:255, 10000, replace = TRUE))
  )
  for (x in cases) {
    cmp <- zstd_mem_compress(x)
    expect_true(is.raw(cmp))
    expect_identical(zstd_mem_decompress(cmp), x)
  }
})

test_that("compression level affects output and is validated", {
  x <- charToRaw(paste(rep("abcabcabc", 1000), collapse = ""))
  small <- zstd_mem_compress(x, level = zstd_min_clevel())
  big <- zstd_mem_compress(x, level = zstd_max_clevel())
  expect_identical(zstd_mem_decompress(small), x)
  expect_identical(zstd_mem_decompress(big), x)

  expect_error(zstd_mem_compress(x, level = zstd_min_clevel() - 1L))
  expect_error(zstd_mem_compress(x, level = zstd_max_clevel() + 1L))
})

test_that("default level round-trips", {
  x <- charToRaw("some data")
  expect_identical(zstd_mem_decompress(zstd_mem_compress(x)), x)
})

test_that("bad input types are rejected", {
  expect_error(zstd_mem_compress("not raw"))
  expect_error(zstd_mem_decompress("not raw"))
  expect_error(zstd_mem_decompress(list()))
})

test_that("corrupt/truncated input errors out cleanly", {
  x <- charToRaw(paste(rep("hello", 100), collapse = ""))
  cmp <- zstd_mem_compress(x)

  truncated <- cmp[seq_len(length(cmp) - 5)]
  expect_error(zstd_mem_decompress(truncated))

  garbage <- as.raw(sample(0:255, 20, replace = TRUE))
  expect_error(zstd_mem_decompress(garbage))
})

test_that("level bound accessors are sane", {
  expect_true(zstd_min_clevel() <= zstd_default_clevel())
  expect_true(zstd_default_clevel() <= zstd_max_clevel())
})

test_that("zstd_info() reports single-frame info", {
  x <- charToRaw(paste(rep("hello world ", 1000), collapse = ""))
  cmp <- zstd_mem_compress(x)
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeBin(cmp, tmp)

  info <- zstd_info(tmp)
  expect_s3_class(info, "data.frame")
  expect_equal(nrow(info), 1)
  expect_identical(names(info)[1], "path")
  expect_identical(info$path, tmp)
  expect_identical(info$type, "frame")
  expect_identical(info$compressed_size, as.double(length(cmp)))
  expect_identical(info$content_size, as.double(length(x)))
  expect_false(is.na(info$window_size))
})

test_that("zstd_info() handles concatenated (multi-frame) files", {
  x1 <- charToRaw("first chunk of data")
  x2 <- charToRaw(paste(rep("second chunk ", 100), collapse = ""))
  cmp1 <- zstd_mem_compress(x1)
  cmp2 <- zstd_mem_compress(x2)

  tmp <- tempfile()
  on.exit(unlink(tmp))
  con <- file(tmp, "wb")
  writeBin(cmp1, con)
  writeBin(cmp2, con)
  close(con)

  info <- zstd_info(tmp)
  expect_equal(nrow(info), 2)
  expect_identical(info$type, c("frame", "frame"))
  expect_identical(
    info$compressed_size,
    as.double(c(length(cmp1), length(cmp2)))
  )
  expect_identical(info$content_size, as.double(c(length(x1), length(x2))))
  expect_equal(sum(info$compressed_size), file.size(tmp))
})

test_that("zstd_info() validates its argument", {
  expect_error(zstd_info(1L))
  expect_error(zstd_info(c("a", "b")))
  expect_error(zstd_info(NA_character_))
  expect_error(zstd_info(tempfile()))
})

test_that("zstd_info() supports glob patterns over multiple files", {
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  x1 <- charToRaw("first file")
  x2 <- charToRaw(paste(rep("second file ", 50), collapse = ""))
  f1 <- file.path(dir, "a.zst")
  f2 <- file.path(dir, "b.zst")
  writeBin(zstd_mem_compress(x1), f1)
  writeBin(zstd_mem_compress(x2), f2)

  info <- zstd_info(file.path(dir, "*.zst"))
  expect_identical(names(info)[1], "path")
  expect_equal(nrow(info), 2)
  expect_setequal(info$path, c(f1, f2))
  expect_equal(info$content_size[info$path == f1], as.double(length(x1)))
  expect_equal(info$content_size[info$path == f2], as.double(length(x2)))
})

test_that("zstd_info() errors on corrupt data", {
  x <- charToRaw(paste(rep("hello", 100), collapse = ""))
  cmp <- zstd_mem_compress(x)
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeBin(cmp[seq_len(length(cmp) - 5)], tmp)
  expect_error(zstd_info(tmp))
})

test_that("dictionary compression round-trips and helps small inputs", {
  samples <- lapply(1:200, function(i) {
    charToRaw(sprintf('{"id":%d,"name":"sample%d","type":"user"}', i, i %% 5))
  })
  dict <- zstd_train_dict(samples, size = 1000)
  expect_true(is.raw(dict))

  x <- samples[[1]]
  cmp_dict <- zstd_mem_compress(x, dict = dict)
  cmp_plain <- zstd_mem_compress(x)
  expect_identical(zstd_mem_decompress(cmp_dict, dict = dict), x)
  expect_lt(length(cmp_dict), length(cmp_plain))
})

test_that("dictionary mismatch/misuse errors out cleanly", {
  samples <- lapply(1:200, function(i) {
    charToRaw(sprintf('{"id":%d,"name":"sample%d","type":"user"}', i, i %% 5))
  })
  dict <- zstd_train_dict(samples, size = 1000)
  x <- samples[[1]]
  cmp <- zstd_mem_compress(x, dict = dict)

  expect_error(zstd_mem_decompress(cmp))
  expect_error(zstd_mem_compress(x, dict = "not raw"))
  expect_error(zstd_mem_decompress(cmp, dict = "not raw"))
})

test_that("zstd_train_dict() validates its arguments", {
  expect_error(zstd_train_dict("not a list"))
  expect_error(zstd_train_dict(list("not raw")))
  expect_error(zstd_train_dict(list(charToRaw("x")), size = 0))
})

test_that("zstd_compress()/zstd_decompress() round-trip a file", {
  src <- tempfile()
  cmp <- tempfile()
  out <- tempfile()
  on.exit(unlink(c(src, cmp, out)))

  x <- charToRaw(paste(rep("the quick brown fox ", 500), collapse = ""))
  writeBin(x, src)

  expect_identical(zstd_compress(src, cmp), cmp)
  expect_identical(zstd_decompress(cmp, out), out)
  expect_identical(readBin(out, "raw", file.size(out)), x)
})

test_that("zstd_compress() round-trips an empty file", {
  src <- tempfile()
  cmp <- tempfile()
  out <- tempfile()
  on.exit(unlink(c(src, cmp, out)))

  file.create(src)
  zstd_compress(src, cmp)
  zstd_decompress(cmp, out)
  expect_identical(readBin(out, "raw", file.size(out)), raw(0))
})

test_that("zstd_compress() round-trips a multi-chunk file", {
  src <- tempfile()
  cmp <- tempfile()
  out <- tempfile()
  on.exit(unlink(c(src, cmp, out)))

  x <- charToRaw(paste(rep("the quick brown fox ", 5 * 100000), collapse = ""))
  writeBin(x, src)

  zstd_compress(src, cmp, level = 1L)
  zstd_decompress(cmp, out)
  expect_identical(readBin(out, "raw", file.size(out)), x)
  expect_lt(file.size(cmp), file.size(src))
})

test_that("zstd_compress()/zstd_decompress() support dictionaries", {
  samples <- lapply(1:200, function(i) {
    charToRaw(sprintf('{"id":%d,"name":"sample%d","type":"user"}', i, i %% 5))
  })
  dict <- zstd_train_dict(samples, size = 1000)

  src <- tempfile()
  cmp_dict <- tempfile()
  cmp_plain <- tempfile()
  out <- tempfile()
  on.exit(unlink(c(src, cmp_dict, cmp_plain, out)))

  writeBin(samples[[1]], src)
  zstd_compress(src, cmp_dict, dict = dict)
  zstd_compress(src, cmp_plain)
  zstd_decompress(cmp_dict, out, dict = dict)

  expect_identical(readBin(out, "raw", file.size(out)), samples[[1]])
  expect_lt(file.size(cmp_dict), file.size(cmp_plain))
  expect_error(zstd_decompress(cmp_dict, tempfile()))
})

test_that("zstd_compress()/zstd_decompress() validate their arguments", {
  src <- tempfile()
  writeBin(charToRaw("hello"), src)
  on.exit(unlink(src))

  expect_error(zstd_compress(tempfile(), tempfile()))
  expect_error(zstd_compress(src, tempfile(), level = zstd_min_clevel() - 1L))
  expect_error(zstd_compress(src, tempfile(), dict = "not raw"))
  expect_error(zstd_compress(1L, tempfile()))
  expect_error(zstd_compress(src, 1L))

  expect_error(zstd_decompress(tempfile(), tempfile()))
  expect_error(zstd_decompress(src, tempfile(), dict = "not raw"))
  expect_error(zstd_decompress(1L, tempfile()))
  expect_error(zstd_decompress(src, 1L))
})

test_that("zstd_compress()/zstd_decompress() error when output file cannot be opened", {
  src <- tempfile()
  cmp <- tempfile()
  on.exit(unlink(c(src, cmp)))
  writeBin(charToRaw("hello world"), src)
  zstd_compress(src, cmp)

  bad_output <- file.path(tempfile(), "out.zst")
  expect_error(zstd_compress(src, bad_output))
  expect_error(zstd_decompress(cmp, bad_output))
})

test_that("zstd_decompress() errors on corrupt/truncated input", {
  src <- tempfile()
  cmp <- tempfile()
  on.exit(unlink(c(src, cmp)))

  writeBin(charToRaw(paste(rep("hello", 100), collapse = "")), src)
  zstd_compress(src, cmp)

  truncated <- readBin(cmp, "raw", file.size(cmp))
  truncated <- truncated[seq_len(length(truncated) - 5)]
  cmp2 <- tempfile()
  on.exit(unlink(cmp2), add = TRUE)
  writeBin(truncated, cmp2)
  expect_error(zstd_decompress(cmp2, tempfile()))
})

test_that("zstd_tar_compress()/zstd_tar_decompress() round-trip a directory tree", {
  dir <- tempfile()
  dir.create(file.path(dir, "subdir"), recursive = TRUE)
  writeLines("hello", file.path(dir, "a.txt"))
  writeLines(paste(rep("world ", 1000), collapse = ""), file.path(dir, "subdir", "b.txt"))
  file.create(file.path(dir, "empty.txt"))
  on.exit(unlink(dir, recursive = TRUE))

  archive <- tempfile(fileext = ".tar.zst")
  exdir <- tempfile()
  on.exit(unlink(c(archive, exdir), recursive = TRUE), add = TRUE)

  expect_identical(zstd_tar_compress(dir, archive), archive)
  expect_identical(zstd_tar_decompress(archive, exdir), exdir)

  base <- basename(dir)
  expect_identical(
    readLines(file.path(exdir, base, "a.txt")),
    readLines(file.path(dir, "a.txt"))
  )
  expect_identical(
    readLines(file.path(exdir, base, "subdir", "b.txt")),
    readLines(file.path(dir, "subdir", "b.txt"))
  )
  expect_true(file.exists(file.path(exdir, base, "empty.txt")))
  expect_identical(file.size(file.path(exdir, base, "empty.txt")), 0)
})

test_that("zstd_tar_compress() supports relative paths longer than 100 bytes", {
  dir <- tempfile()
  comp <- paste(rep("a", 60), collapse = "")
  nested <- file.path(comp, comp)
  dir.create(file.path(dir, nested), recursive = TRUE)
  writeLines("deep content", file.path(dir, nested, "f.txt"))
  on.exit(unlink(dir, recursive = TRUE))

  archive <- tempfile(fileext = ".tar.zst")
  exdir <- tempfile()
  on.exit(unlink(c(archive, exdir), recursive = TRUE), add = TRUE)

  zstd_tar_compress(dir, archive)
  zstd_tar_decompress(archive, exdir)

  out <- file.path(exdir, basename(dir), nested, "f.txt")
  expect_true(file.exists(out))
  expect_identical(readLines(out), "deep content")
})

test_that("zstd_tar_compress() errors cleanly on paths that can't fit ustar headers", {
  dir <- tempfile()
  comp <- paste(rep("a", 60), collapse = "")
  nested <- do.call(file.path, as.list(rep(comp, 5)))
  dir.create(file.path(dir, dirname(nested)), recursive = TRUE)
  writeLines("x", file.path(dir, nested))
  on.exit(unlink(dir, recursive = TRUE))

  archive <- tempfile(fileext = ".tar.zst")
  on.exit(unlink(archive), add = TRUE)
  expect_error(zstd_tar_compress(dir, archive))
})

test_that("zstd_tar_decompress() rejects a plain (non-tar) zstd file", {
  plain <- tempfile()
  writeLines("just some text, not a tar archive", plain)
  on.exit(unlink(plain))

  cmp <- tempfile(fileext = ".zst")
  on.exit(unlink(cmp), add = TRUE)
  zstd_compress(plain, cmp)

  expect_error(zstd_tar_decompress(cmp, tempfile()))
})

test_that("zstd_tar_decompress() refuses path-traversal entries", {
  make_tar_header <- function(name, size) {
    h <- raw(512)
    raw_name <- charToRaw(name)
    h[seq_along(raw_name)] <- raw_name
    set_field <- function(h, start, value) {
      v <- charToRaw(value)
      h[start:(start + length(v) - 1)] <- v
      h
    }
    h <- set_field(h, 101, sprintf("%07o", 420))
    h <- set_field(h, 109, sprintf("%07o", 0))
    h <- set_field(h, 117, sprintf("%07o", 0))
    h <- set_field(h, 125, sprintf("%011o", size))
    h <- set_field(h, 137, sprintf("%011o", 0))
    h[149:156] <- charToRaw("        ")
    h[157] <- charToRaw("0")
    h <- set_field(h, 258, "ustar")
    h[264:265] <- charToRaw("00")
    chk <- sum(as.integer(h))
    h <- set_field(h, 149, sprintf("%06o", chk))
    h[155] <- as.raw(0)
    h[156] <- charToRaw(" ")
    h
  }

  content <- charToRaw("evil content\n")
  pad_len <- (512 - (length(content) %% 512)) %% 512
  tar_path <- tempfile(fileext = ".tar")
  on.exit(unlink(tar_path))
  con <- file(tar_path, "wb")
  writeBin(make_tar_header("../../evil.txt", length(content)), con)
  writeBin(content, con)
  if (pad_len > 0) writeBin(raw(pad_len), con)
  writeBin(raw(1024), con)
  close(con)

  archive <- tempfile(fileext = ".tar.zst")
  on.exit(unlink(archive), add = TRUE)
  zstd_compress(tar_path, archive)

  expect_error(zstd_tar_decompress(archive, tempfile()))
})

# Shared helpers for the PAX/GNU extended header tests below.

# Builds a single "<len> <text>\n" PAX record, computing `len` (which
# includes its own decimal representation) the same way real tar writers
# do: iterate until the length prefix's own width stops changing the
# total.
pax_raw_record <- function(text) {
  body <- paste0(" ", text, "\n")
  n <- nchar(body) + 1
  repeat {
    reclen <- nchar(as.character(n)) + nchar(body)
    if (reclen == n) return(paste0(n, body))
    n <- reclen
  }
}

pax_record <- function(key, value) pax_raw_record(paste0(key, "=", value))

# The current user's uid/gid, so the tar fixtures below can set a real
# owner instead of 0 (root): chown()ing extracted files to the uid/gid
# that's already running the test succeeds even when not running as
# root, unlike chown(..., 0, 0). Falls back to 0 where `id` isn't
# available (e.g. Windows), where the fixed-owner tests are skipped.
test_uid <- suppressWarnings(as.integer(tryCatch(system("id -u", intern = TRUE), error = function(e) NA)))
test_gid <- suppressWarnings(as.integer(tryCatch(system("id -g", intern = TRUE), error = function(e) NA)))
if (is.na(test_uid)) test_uid <- 0L
if (is.na(test_gid)) test_gid <- 0L

make_tar_header <- function(name, size, type = "0") {
  h <- raw(512)
  raw_name <- charToRaw(name)
  h[seq_along(raw_name)] <- raw_name
  set_field <- function(h, start, value) {
    v <- charToRaw(value)
    h[start:(start + length(v) - 1)] <- v
    h
  }
  h <- set_field(h, 101, sprintf("%07o", 420))
  h <- set_field(h, 109, sprintf("%07o", test_uid))
  h <- set_field(h, 117, sprintf("%07o", test_gid))
  h <- set_field(h, 125, sprintf("%011o", size))
  h <- set_field(h, 137, sprintf("%011o", 0))
  h[149:156] <- charToRaw("        ")
  h <- set_field(h, 157, type)
  h <- set_field(h, 258, "ustar")
  h[264:265] <- charToRaw("00")
  chk <- sum(as.integer(h))
  h <- set_field(h, 149, sprintf("%06o", chk))
  h[155] <- as.raw(0)
  h[156] <- charToRaw(" ")
  h
}

write_tar_entry <- function(con, name, content, type = "0") {
  writeBin(make_tar_header(name, length(content), type = type), con)
  writeBin(content, con)
  pad_len <- (512 - (length(content) %% 512)) %% 512
  if (pad_len > 0) writeBin(raw(pad_len), con)
}

test_that("zstd_tar_decompress() applies a PAX 'x' extended header's path override", {
  # A 'x' typeflag marks a PAX extended header: its "data" is a series of
  # key=value attributes (here, the real long path of the entry that
  # follows), not file content.
  pax_content <- charToRaw(pax_record("path", "some/long/path.txt"))
  file_content <- charToRaw("hello from pax\n")

  tar_path <- tempfile(fileext = ".tar")
  on.exit(unlink(tar_path))
  con <- file(tar_path, "wb")
  write_tar_entry(con, "PaxHeader/entry", pax_content, type = "x")
  write_tar_entry(con, "short-name.txt", file_content)
  writeBin(raw(1024), con)
  close(con)

  archive <- tempfile(fileext = ".tar.zst")
  on.exit(unlink(archive), add = TRUE)
  zstd_compress(tar_path, archive)

  exdir <- tempfile()
  zstd_tar_decompress(archive, exdir)
  expect_true(file.exists(file.path(exdir, "some/long/path.txt")))
  expect_false(file.exists(file.path(exdir, "short-name.txt")))
  expect_identical(readBin(file.path(exdir, "some/long/path.txt"), "raw", length(file_content)), file_content)
})

test_that("zstd_tar_decompress() applies a PAX 'g' global extended header to later entries", {
  # A 'g' typeflag marks a global PAX header: its key=value attributes
  # (here, mtime) apply as defaults to every entry that follows, not just
  # the next one.
  mtime <- 1000000000
  pax_content <- charToRaw(pax_record("mtime", format(mtime, scientific = FALSE)))

  tar_path <- tempfile(fileext = ".tar")
  on.exit(unlink(tar_path))
  con <- file(tar_path, "wb")
  write_tar_entry(con, "PaxHeader/global", pax_content, type = "g")
  write_tar_entry(con, "a.txt", charToRaw("a\n"))
  write_tar_entry(con, "b.txt", charToRaw("b\n"))
  writeBin(raw(1024), con)
  close(con)

  archive <- tempfile(fileext = ".tar.zst")
  on.exit(unlink(archive), add = TRUE)
  zstd_compress(tar_path, archive)

  exdir <- tempfile()
  zstd_tar_decompress(archive, exdir)
  expect_identical(as.numeric(file.info(file.path(exdir, "a.txt"))$mtime), mtime)
  expect_identical(as.numeric(file.info(file.path(exdir, "b.txt"))$mtime), mtime)
})

test_that("zstd_tar_decompress() warns (but still extracts) on a PAX/ustar size mismatch", {
  pax_content <- charToRaw(pax_record("size", "999"))
  file_content <- charToRaw("hello from pax\n")

  tar_path <- tempfile(fileext = ".tar")
  on.exit(unlink(tar_path))
  con <- file(tar_path, "wb")
  write_tar_entry(con, "PaxHeader/entry", pax_content, type = "x")
  write_tar_entry(con, "c.txt", file_content)
  writeBin(raw(1024), con)
  close(con)

  archive <- tempfile(fileext = ".tar.zst")
  on.exit(unlink(archive), add = TRUE)
  zstd_compress(tar_path, archive)

  exdir <- tempfile()
  expect_warning(zstd_tar_decompress(archive, exdir), "size")
  expect_identical(readBin(file.path(exdir, "c.txt"), "raw", length(file_content)), file_content)
})

test_that("zstd_tar_decompress() errors clearly on a malformed PAX record", {
  # missing '=' between key and value
  pax_content <- charToRaw(pax_raw_record("pathXvalue"))

  tar_path <- tempfile(fileext = ".tar")
  on.exit(unlink(tar_path))
  con <- file(tar_path, "wb")
  write_tar_entry(con, "PaxHeader/entry", pax_content, type = "x")
  write_tar_entry(con, "d.txt", charToRaw("d\n"))
  writeBin(raw(1024), con)
  close(con)

  archive <- tempfile(fileext = ".tar.zst")
  on.exit(unlink(archive), add = TRUE)
  zstd_compress(tar_path, archive)

  expect_error(zstd_tar_decompress(archive, tempfile()))
})

test_that("zstd_tar_decompress() still refuses GNU long name/link headers", {
  content <- charToRaw("some/long/gnu/path.txt\n")

  tar_path <- tempfile(fileext = ".tar")
  on.exit(unlink(tar_path))
  con <- file(tar_path, "wb")
  write_tar_entry(con, "./GNU-longname", content, type = "L")
  write_tar_entry(con, "short-name.txt", charToRaw("hello\n"))
  writeBin(raw(1024), con)
  close(con)

  archive <- tempfile(fileext = ".tar.zst")
  on.exit(unlink(archive), add = TRUE)
  zstd_compress(tar_path, archive)

  expect_error(zstd_tar_decompress(archive, tempfile()))
})

test_that("zstd_tar_compress()/zstd_tar_decompress() validate their arguments", {
  expect_error(zstd_tar_compress(character(0), tempfile()))
  expect_error(zstd_tar_compress(tempfile(), tempfile()))
  expect_error(zstd_tar_compress(1L, tempfile()))

  expect_error(zstd_tar_decompress(tempfile()))
})
