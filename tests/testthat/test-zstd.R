test_that("round-trip works for various inputs", {
  cases <- list(
    empty = raw(0),
    small = charToRaw("hello world"),
    text  = charToRaw(paste(rep("the quick brown fox ", 500), collapse = "")),
    random = as.raw(sample(0:255, 10000, replace = TRUE))
  )
  for (x in cases) {
    cmp <- zstd_compress(x)
    expect_true(is.raw(cmp))
    expect_identical(zstd_decompress(cmp), x)
  }
})

test_that("compression level affects output and is validated", {
  x <- charToRaw(paste(rep("abcabcabc", 1000), collapse = ""))
  small <- zstd_compress(x, level = zstd_min_clevel())
  big   <- zstd_compress(x, level = zstd_max_clevel())
  expect_identical(zstd_decompress(small), x)
  expect_identical(zstd_decompress(big), x)

  expect_error(zstd_compress(x, level = zstd_min_clevel() - 1L))
  expect_error(zstd_compress(x, level = zstd_max_clevel() + 1L))
})

test_that("default level round-trips", {
  x <- charToRaw("some data")
  expect_identical(zstd_decompress(zstd_compress(x)), x)
})

test_that("bad input types are rejected", {
  expect_error(zstd_compress("not raw"))
  expect_error(zstd_decompress("not raw"))
  expect_error(zstd_decompress(list()))
})

test_that("corrupt/truncated input errors out cleanly", {
  x <- charToRaw(paste(rep("hello", 100), collapse = ""))
  cmp <- zstd_compress(x)

  truncated <- cmp[seq_len(length(cmp) - 5)]
  expect_error(zstd_decompress(truncated))

  garbage <- as.raw(sample(0:255, 20, replace = TRUE))
  expect_error(zstd_decompress(garbage))
})

test_that("level bound accessors are sane", {
  expect_true(zstd_min_clevel() <= zstd_default_clevel())
  expect_true(zstd_default_clevel() <= zstd_max_clevel())
})

test_that("zstd_info() reports single-frame info", {
  x <- charToRaw(paste(rep("hello world ", 1000), collapse = ""))
  cmp <- zstd_compress(x)
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeBin(cmp, tmp)

  info <- zstd_info(tmp)
  expect_s3_class(info, "data.frame")
  expect_equal(nrow(info), 1)
  expect_identical(info$type, "frame")
  expect_identical(info$compressed_size, as.double(length(cmp)))
  expect_identical(info$content_size, as.double(length(x)))
  expect_false(is.na(info$window_size))
})

test_that("zstd_info() handles concatenated (multi-frame) files", {
  x1 <- charToRaw("first chunk of data")
  x2 <- charToRaw(paste(rep("second chunk ", 100), collapse = ""))
  cmp1 <- zstd_compress(x1)
  cmp2 <- zstd_compress(x2)

  tmp <- tempfile()
  on.exit(unlink(tmp))
  con <- file(tmp, "wb")
  writeBin(cmp1, con)
  writeBin(cmp2, con)
  close(con)

  info <- zstd_info(tmp)
  expect_equal(nrow(info), 2)
  expect_identical(info$type, c("frame", "frame"))
  expect_identical(info$compressed_size, as.double(c(length(cmp1), length(cmp2))))
  expect_identical(info$content_size, as.double(c(length(x1), length(x2))))
  expect_equal(sum(info$compressed_size), file.size(tmp))
})

test_that("zstd_info() validates its argument", {
  expect_error(zstd_info(1L))
  expect_error(zstd_info(c("a", "b")))
  expect_error(zstd_info(NA_character_))
  expect_error(zstd_info(tempfile()))
})

test_that("zstd_info() errors on corrupt data", {
  x <- charToRaw(paste(rep("hello", 100), collapse = ""))
  cmp <- zstd_compress(x)
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
  cmp_dict <- zstd_compress(x, dict = dict)
  cmp_plain <- zstd_compress(x)
  expect_identical(zstd_decompress(cmp_dict, dict = dict), x)
  expect_lt(length(cmp_dict), length(cmp_plain))
})

test_that("dictionary mismatch/misuse errors out cleanly", {
  samples <- lapply(1:200, function(i) {
    charToRaw(sprintf('{"id":%d,"name":"sample%d","type":"user"}', i, i %% 5))
  })
  dict <- zstd_train_dict(samples, size = 1000)
  x <- samples[[1]]
  cmp <- zstd_compress(x, dict = dict)

  expect_error(zstd_decompress(cmp))
  expect_error(zstd_compress(x, dict = "not raw"))
  expect_error(zstd_decompress(cmp, dict = "not raw"))
})

test_that("zstd_train_dict() validates its arguments", {
  expect_error(zstd_train_dict("not a list"))
  expect_error(zstd_train_dict(list("not raw")))
  expect_error(zstd_train_dict(list(charToRaw("x")), size = 0))
})
