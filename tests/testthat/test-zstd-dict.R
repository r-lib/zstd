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
