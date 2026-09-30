test_that("zstd_tar_compress()/zstd_tar_decompress() round-trip a directory tree", {
  dir <- tempfile()
  dir.create(file.path(dir, "subdir"), recursive = TRUE)
  writeLines("hello", file.path(dir, "a.txt"))
  writeLines(
    paste(rep("world ", 1000), collapse = ""),
    file.path(dir, "subdir", "b.txt")
  )
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

test_that("zstd_tar_compress()/zstd_tar_decompress() handle non-ASCII paths", {
  root <- tempfile()
  dir <- file.path(root, "déjà vu 文字")
  sub <- file.path(dir, "über")
  dir.create(sub, recursive = TRUE)
  fname <- "őrült αβ.txt"
  writeLines("hello", file.path(sub, fname))
  on.exit(unlink(root, recursive = TRUE))

  archive <- file.path(root, "árvíztűrő.tar.zst")
  exdir <- file.path(root, "kiépítés")

  zstd_tar_compress(dir, archive)
  expect_true(file.exists(archive))
  expect_silent(zstd_tar_decompress(archive, exdir))

  out <- file.path(exdir, basename(dir), "über", fname)
  expect_true(file.exists(out))
  expect_identical(readLines(out), "hello")
})

test_that("zstd_tar_compress()/zstd_tar_decompress() keep file mtimes", {
  dir <- tempfile()
  dir.create(file.path(dir, "subdir"), recursive = TRUE)
  writeLines("hello", file.path(dir, "subdir", "a.txt"))
  mtime <- as.POSIXct("2020-01-02 03:04:05", tz = "UTC")
  Sys.setFileTime(file.path(dir, "subdir", "a.txt"), mtime)
  on.exit(unlink(dir, recursive = TRUE))

  archive <- tempfile(fileext = ".tar.zst")
  exdir <- tempfile()
  on.exit(unlink(c(archive, exdir), recursive = TRUE), add = TRUE)

  zstd_tar_compress(dir, archive)
  zstd_tar_decompress(archive, exdir)

  out <- file.path(exdir, basename(dir), "subdir", "a.txt")
  expect_equal(as.numeric(file.mtime(out)), as.numeric(mtime))
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
  # A file name of 100+ bytes can't be split across the ustar 'prefix' and
  # 'name' fields. Keep the full path short enough for Windows' MAX_PATH.
  skip_on_cran()
  dir <- tempfile()
  dir.create(dir)
  long <- paste(rep("a", 120), collapse = "")
  writeLines("x", file.path(dir, long))
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
  if (pad_len > 0) {
    writeBin(raw(pad_len), con)
  }
  writeBin(raw(1024), con)
  close(con)

  archive <- tempfile(fileext = ".tar.zst")
  on.exit(unlink(archive), add = TRUE)
  zstd_compress(tar_path, archive)

  expect_error(zstd_tar_decompress(archive, tempfile()))
})

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
  expect_identical(
    readBin(
      file.path(exdir, "some/long/path.txt"),
      "raw",
      length(file_content)
    ),
    file_content
  )
})

test_that("zstd_tar_decompress() applies a PAX 'g' global extended header to later entries", {
  # A 'g' typeflag marks a global PAX header: its key=value attributes
  # (here, mtime) apply as defaults to every entry that follows, not just
  # the next one.
  mtime <- 1000000000
  pax_content <- charToRaw(pax_record(
    "mtime",
    format(mtime, scientific = FALSE)
  ))

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
  expect_identical(
    as.numeric(file.info(file.path(exdir, "a.txt"))$mtime),
    mtime
  )
  expect_identical(
    as.numeric(file.info(file.path(exdir, "b.txt"))$mtime),
    mtime
  )
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
  expect_identical(
    readBin(file.path(exdir, "c.txt"), "raw", length(file_content)),
    file_content
  )
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

test_that("tar_relative_path()", {
  # base "." leaves paths alone
  expect_equal(tar_relative_path("a.txt", "."), "a.txt")
  expect_equal(tar_relative_path(c("d", "d/x"), "."), c("d", "d/x"))

  # strips base and the following slash
  expect_equal(
    tar_relative_path(c("/tmp/x/d", "/tmp/x/d/sub/a.txt"), "/tmp/x"),
    c("d", "d/sub/a.txt")
  )
  expect_equal(tar_relative_path("rel/d/a.txt", "rel"), "d/a.txt")

  # only strips whole path components
  expect_equal(tar_relative_path("/tmp/xy/a.txt", "/tmp/x"), "/tmp/xy/a.txt")

  # paths not under base are returned unchanged
  expect_equal(tar_relative_path("/other/a.txt", "/tmp/x"), "/other/a.txt")

  # backslashes in paths and base become forward slashes
  expect_equal(
    tar_relative_path("C:\\tmp\\x\\d\\a.txt", "C:\\tmp\\x"),
    "d/a.txt"
  )
  expect_equal(tar_relative_path("d\\a.txt", "."), "d/a.txt")

  expect_equal(tar_relative_path(character(0), "/tmp"), character(0))
})
