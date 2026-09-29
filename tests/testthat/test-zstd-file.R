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

test_that("zstd_compress()/zstd_decompress() expand `~` in paths", {
  skip_on_os("windows")
  home <- tempfile()
  dir.create(home)
  old <- Sys.getenv("HOME")
  on.exit({
    Sys.setenv(HOME = old)
    unlink(home, recursive = TRUE)
  })
  Sys.setenv(HOME = home)

  x <- charToRaw(paste(rep("the quick brown fox ", 500), collapse = ""))
  writeBin(x, file.path(home, "src"))

  expect_identical(
    zstd_compress("~/src", "~/cmp"),
    file.path(path.expand("~"), "cmp")
  )
  zstd_decompress("~/cmp", "~/out")
  out <- file.path(home, "out")
  expect_identical(readBin(out, "raw", file.size(out)), x)
})
