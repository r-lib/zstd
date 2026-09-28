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
