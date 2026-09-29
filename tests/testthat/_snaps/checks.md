# as_string

    Code
      f(1L)
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be a string scalar, but it is an integer.
    Code
      f(NA_character_)
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be a string scalar, but it is a character `NA`.
    Code
      f(c("a", "b"))
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be a string scalar, but it is a character vector.
    Code
      f(NULL)
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be a string scalar, but it is NULL.

# as_existing_file

    Code
      f(1L)
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be a string scalar, but it is an integer.
    Code
      f("does-not-exist")
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be an existing file. File does not exist: 'does-not-exist'.
    Code
      f(".")
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be a file, but it is a directory: '.'.

# as_raw

    Code
      f("a")
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be a raw vector, but it is a string.
    Code
      f(NULL)
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be a raw vector, but it is NULL.
    Code
      f("a", null = TRUE)
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be a raw vector or NULL, but it is a string.

# as_flag

    Code
      f(NA)
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be a flag (logical scalar), but it is `NA`.
    Code
      f(c(TRUE, FALSE))
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be a flag (logical scalar), but it is a logical vector.
    Code
      f("yes", null = TRUE)
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be a flag (logical scalar) or NULL, but it is a string.

# as_count

    Code
      f(1:2)
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be an integer scalar, not a vector.
    Code
      f(NA_integer_)
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must not be `NA`.
    Code
      f(-1L)
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be non-negative.
    Code
      f(0L, positive = TRUE)
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be positive.
    Code
      f("a")
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be a non-negative integer scalar, but it is a string.

# as_clevel

    Code
      f(zstd_max_clevel() + 1L)
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be between -131072 and 22, but it is 23.
    Code
      f(NA)
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be an integer scalar, but it is `NA`.
    Code
      f("3")
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be an integer scalar, but it is a string.
    Code
      f(1:2)
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be an integer scalar, but it is an integer vector.

# as_choice

    Code
      f("baz")
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be one of 'foo', 'bar', but it is 'baz'.
    Code
      f(1L)
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be a string scalar, one of 'foo', 'bar', but it is an integer.
    Code
      f(NULL, null = FALSE)
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be a string scalar, one of 'foo', 'bar', but it is NULL.

# as_files

    Code
      f(character())
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be a non-empty character vector, but it is an empty character vector.
    Code
      f(NA_character_)
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must not contain `NA` values. It has `NA` at position 1.
    Code
      f(c("a", NA, "b", NA))
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must not contain `NA` values. It has `NA` at positions 2, 4.
    Code
      f(rep(NA_character_, 10))
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must not contain `NA` values. It has `NA` at positions 1, 2, 3, 4, 5 and 5 more.
    Code
      f(1L)
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be a non-empty character vector, but it is an integer.
    Code
      f("does-not-exist")
    Condition
      Error in `f()`:
      ! Invalid argument: all files in `x` must exist. File does not exist: 'does-not-exist'.
    Code
      f(c(tempdir(), "does-not-exist", "does-not-exist-2"))
    Condition
      Error in `f()`:
      ! Invalid argument: all files in `x` must exist. 2 files do not exist: 'does-not-exist', 'does-not-exist-2'.
    Code
      f(paste0("does-not-exist-", 1:10))
    Condition
      Error in `f()`:
      ! Invalid argument: all files in `x` must exist. 10 files do not exist: 'does-not-exist-1', 'does-not-exist-2', 'does-not-exist-3', 'does-not-exist-4', 'does-not-exist-5' and 5 more.

# as_samples

    Code
      f(NA_character_)
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must not contain `NA` values. It has `NA` at position 1.
    Code
      f(c(tmp, NA, NA))
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must not contain `NA` values. It has `NA` at positions 2, 3.
    Code
      f("does-not-exist")
    Condition
      Error in `f()`:
      ! Invalid argument: all files in `x` must exist. File does not exist: 'does-not-exist'.
    Code
      f(c("does-not-exist", "does-not-exist-2"))
    Condition
      Error in `f()`:
      ! Invalid argument: all files in `x` must exist. 2 files do not exist: 'does-not-exist', 'does-not-exist-2'.
    Code
      f(".")
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must contain files, not directories. This is a directory: '.'.
    Code
      f(c(".", ".."))
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must contain files, not directories. These are directories: '.', '..'.
    Code
      f(1:3)
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be a list of raw vectors or a character vector of file paths, but it is an integer vector.
    Code
      f(list("a"))
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be a list of raw vectors, but element 1 is a string.
    Code
      f(list(as.raw(1), 1:3))
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be a list of raw vectors, but element 2 is an integer vector.
    Code
      f(list("foo", "bar"))
    Condition
      Error in `f()`:
      ! Invalid argument: `x` must be a list of raw vectors, but 2 elements are not, at positions 1, 2.

# errors name the user facing function

    Code
      zstd_mem_compress(1:3)
    Condition
      Error in `zstd_mem_compress()`:
      ! Invalid argument: `x` must be a raw vector, but it is an integer vector.
    Code
      zstd_mem_compress(x, level = NA)
    Condition
      Error in `zstd_mem_compress()`:
      ! Invalid argument: `level` must be an integer scalar, but it is `NA`.
    Code
      zstd_mem_compress(x, strategy = "nope")
    Condition
      Error in `zstd_mem_compress()`:
      ! Invalid argument: `strategy` must be one of 'fast', 'dfast', 'greedy', 'lazy', 'lazy2', 'btlazy2', 'btopt', 'btultra', 'btultra2', but it is 'nope'.
    Code
      zstd_mem_compress(x, checksum = "yes")
    Condition
      Error in `zstd_mem_compress()`:
      ! Invalid argument: `checksum` must be a flag (logical scalar), but it is a string.
    Code
      zstd_mem_decompress(x, dict = "no")
    Condition
      Error in `zstd_mem_decompress()`:
      ! Invalid argument: `dict` must be a raw vector or NULL, but it is a string.
    Code
      zstd_compress("does-not-exist", tempfile())
    Condition
      Error in `zstd_compress()`:
      ! Invalid argument: `input` must be an existing file. File does not exist: 'does-not-exist'.
    Code
      zstd_info(1L)
    Condition
      Error in `zstd_info()`:
      ! Invalid argument: `path` must be a string scalar, but it is an integer.
    Code
      zstd_train_dict(list(x), size = 0)
    Condition
      Error in `zstd_train_dict()`:
      ! Invalid argument: `size` must be positive.

