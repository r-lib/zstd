test_that("zstd_version", {
  v <- zstd_version()
  expect_s3_class(v, "zstd_version")
  expect_named(v, c("type", "version", "path", "static"))
  expect_true(v$type %in% c("system", "bundled"))
  expect_match(v$version, "^[0-9]+[.][0-9]+[.][0-9]+$")
  expect_true(package_version(v$version) >= "1.5.0")
  expect_true(is.character(v$path) && length(v$path) == 1)
  if (v$type == "bundled") {
    expect_true(v$static)
  }
  expect_output(print(v), paste0("zstd ", v$version), fixed = TRUE)
})
