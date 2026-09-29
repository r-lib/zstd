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

test_that("zstd_train_dict() accepts file paths", {
  samples <- lapply(1:200, function(i) {
    charToRaw(sprintf('{"id":%d,"name":"sample%d","type":"user"}', i, i %% 5))
  })
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  paths <- file.path(dir, paste0(seq_along(samples), ".json"))
  for (i in seq_along(samples)) {
    writeBin(samples[[i]], paths[i])
  }

  expect_identical(
    zstd_train_dict(paths, size = 1000),
    zstd_train_dict(samples, size = 1000)
  )
})

test_that("zstd_train_dict() validates its arguments", {
  expect_error(zstd_train_dict("does-not-exist"), "File does not exist")
  expect_error(zstd_train_dict(tempdir()), "not directories")
  expect_error(zstd_train_dict(NA_character_))
  expect_error(zstd_train_dict(1:3))
  expect_error(zstd_train_dict(list("not raw")))
  expect_error(zstd_train_dict(list(charToRaw("x")), size = 0))
})
