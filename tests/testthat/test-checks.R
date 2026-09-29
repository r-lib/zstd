test_that("as_string", {
  f <- function(x, null = FALSE) as_string(x, null = null)
  expect_equal(f("a"), "a")
  expect_null(f(NULL, null = TRUE))
  expect_snapshot(error = TRUE, {
    f(1L)
    f(NA_character_)
    f(c("a", "b"))
    f(NULL)
  })
})

test_that("as_existing_file", {
  f <- function(x) as_existing_file(x)
  tmp <- tempfile()
  file.create(tmp)
  on.exit(unlink(tmp), add = TRUE)
  expect_equal(f(tmp), tmp)
  expect_snapshot(error = TRUE, {
    f(1L)
    f("does-not-exist")
    f(".")
  })
})

test_that("as_raw", {
  f <- function(x, null = FALSE) as_raw(x, null = null)
  expect_equal(f(as.raw(1:3)), as.raw(1:3))
  expect_null(f(NULL, null = TRUE))
  expect_snapshot(error = TRUE, {
    f("a")
    f(NULL)
    f("a", null = TRUE)
  })
})

test_that("as_flag", {
  f <- function(x, null = FALSE) as_flag(x, null = null)
  expect_true(f(TRUE))
  expect_null(f(NULL, null = TRUE))
  expect_snapshot(error = TRUE, {
    f(NA)
    f(c(TRUE, FALSE))
    f("yes", null = TRUE)
  })
})

test_that("as_count", {
  f <- function(x, positive = FALSE, null = FALSE) {
    as_count(x, positive = positive, null = null)
  }
  expect_identical(f(3), 3L)
  expect_identical(f(0L), 0L)
  expect_null(f(NULL, null = TRUE))
  expect_snapshot(error = TRUE, {
    f(1:2)
    f(NA_integer_)
    f(-1L)
    f(0L, positive = TRUE)
    f("a")
  })
})

test_that("as_clevel", {
  f <- function(x) as_clevel(x)
  expect_identical(f(3), 3L)
  expect_snapshot(error = TRUE, {
    f(zstd_max_clevel() + 1L)
    f(NA)
    f("3")
    f(1:2)
  })
})

test_that("as_choice", {
  f <- function(x, null = TRUE) as_choice(x, c("foo", "bar"), null = null)
  expect_identical(f("bar"), 2L)
  expect_identical(f("FOO"), 1L)
  expect_null(f(NULL))
  expect_snapshot(error = TRUE, {
    f("baz")
    f(1L)
    f(NULL, null = FALSE)
  })
})

test_that("as_files", {
  f <- function(x) as_files(x)
  expect_equal(f(tempdir()), tempdir())
  expect_snapshot(error = TRUE, {
    f(character())
    f(NA_character_)
    f(c("a", NA, "b", NA))
    f(rep(NA_character_, 10))
    f(1L)
    f("does-not-exist")
    f(c(tempdir(), "does-not-exist", "does-not-exist-2"))
    f(paste0("does-not-exist-", 1:10))
  })
})

test_that("as_samples", {
  f <- function(x) as_samples(x)
  s <- list(as.raw(1:3))
  expect_equal(f(s), s)
  tmp <- tempfile()
  writeBin(as.raw(1:3), tmp)
  on.exit(unlink(tmp), add = TRUE)
  expect_equal(f(tmp), s)
  expect_snapshot(error = TRUE, {
    f(NA_character_)
    f(c(tmp, NA, NA))
    f("does-not-exist")
    f(c("does-not-exist", "does-not-exist-2"))
    f(".")
    f(c(".", ".."))
    f(1:3)
    f(list("a"))
    f(list(as.raw(1), 1:3))
    f(list("foo", "bar"))
  })
})

test_that("errors name the user facing function", {
  x <- charToRaw("hello")
  expect_snapshot(error = TRUE, {
    zstd_mem_compress(1:3)
    zstd_mem_compress(x, level = NA)
    zstd_mem_compress(x, strategy = "nope")
    zstd_mem_compress(x, checksum = "yes")
    zstd_mem_decompress(x, dict = "no")
    zstd_compress("does-not-exist", tempfile())
    zstd_info(1L)
    zstd_train_dict(list(x), size = 0)
  })
})
