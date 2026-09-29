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
