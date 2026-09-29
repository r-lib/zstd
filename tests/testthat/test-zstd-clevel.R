test_that("level bound accessors are sane", {
  expect_true(zstd_min_clevel() <= zstd_default_clevel())
  expect_true(zstd_default_clevel() <= zstd_max_clevel())
})
