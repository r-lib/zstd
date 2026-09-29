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
