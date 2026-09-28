#' Compress a raw vector with Zstandard
#'
#' @param x A raw vector to compress.
#' @param level Integer compression level. Defaults to
#'   [zstd_default_clevel()]. Valid range is [zstd_min_clevel()] to
#'   [zstd_max_clevel()].
#' @param dict `NULL`, or a raw vector containing a dictionary (as created
#'   by [zstd_train_dict()], or any raw content dictionary). Using a
#'   dictionary can substantially improve the compression ratio of small,
#'   similar inputs. The same dictionary must be passed to
#'   [zstd_mem_decompress()].
#' @return A raw vector: the compressed data.
#' @export
#' @examples
#' x <- charToRaw(paste(rep("hello world ", 1000), collapse = ""))
#' cmp <- zstd_mem_compress(x)
#' identical(zstd_mem_decompress(cmp), x)
zstd_mem_compress <- function(x, level = zstd_default_clevel(), dict = NULL) {
  if (!is.raw(x)) {
    stop("`x` must be a raw vector", call. = FALSE)
  }
  if (!is.null(dict) && !is.raw(dict)) {
    stop("`dict` must be a raw vector or NULL", call. = FALSE)
  }
  level <- as.integer(level)
  if (level < zstd_min_clevel() || level > zstd_max_clevel()) {
    stop(
      "`level` must be between ", zstd_min_clevel(), " and ",
      zstd_max_clevel(),
      call. = FALSE
    )
  }
  .Call(zstd_mem_compress_, x, level, dict)
}

#' Decompress a Zstandard-compressed raw vector
#'
#' @param x A raw vector of Zstandard-compressed data (a single frame).
#' @param dict `NULL`, or a raw vector containing the dictionary that was
#'   used to compress `x`. See [zstd_mem_compress()].
#' @return A raw vector: the decompressed data.
#' @export
#' @examples
#' x <- charToRaw("hello world")
#' zstd_mem_decompress(zstd_mem_compress(x))
zstd_mem_decompress <- function(x, dict = NULL) {
  if (!is.raw(x)) {
    stop("`x` must be a raw vector", call. = FALSE)
  }
  if (!is.null(dict) && !is.raw(dict)) {
    stop("`dict` must be a raw vector or NULL", call. = FALSE)
  }
  .Call(zstd_mem_decompress_, x, dict)
}

#' Train a Zstandard dictionary from sample data
#'
#' Dictionaries improve the compression ratio of many small, similar
#' inputs, such as JSON records that share the same structure. Training
#' needs a few hundred to a few thousand representative samples: aim for
#' total sample size about 100x the target dictionary size.
#'
#' @param samples A list of raw vectors, representative of the data that
#'   will be compressed with the dictionary.
#' @param size Target dictionary size, in bytes. Defaults to 112640 (110KB),
#'   the same default as the `zstd` command line tool.
#' @return A raw vector: the trained dictionary. Pass it as the `dict`
#'   argument of [zstd_mem_compress()] and [zstd_mem_decompress()].
#' @export
#' @examples
#' samples <- lapply(1:100, function(i) {
#'   charToRaw(paste0('{"id":', i, ',"name":"sample"}'))
#' })
#' dict <- zstd_train_dict(samples, size = 1000)
#' cmp <- zstd_mem_compress(samples[[1]], dict = dict)
#' identical(zstd_mem_decompress(cmp, dict = dict), samples[[1]])
zstd_train_dict <- function(samples, size = 112640L) {
  if (!is.list(samples) || !all(vapply(samples, is.raw, logical(1)))) {
    stop("`samples` must be a list of raw vectors", call. = FALSE)
  }
  size <- as.integer(size)
  if (size <= 0) {
    stop("`size` must be a positive integer", call. = FALSE)
  }
  .Call(zstd_train_dict_, samples, size)
}

#' Get information about a Zstandard-compressed file
#'
#' Reads the frame headers of a zstd file without decompressing its
#' content. Handles concatenated files (multiple zstd frames stored
#' back to back, e.g. produced by appending compressed chunks to an
#' existing file), returning one row per frame in the order they
#' appear in the file.
#'
#' @param path Path to a zstd-compressed file.
#' @return A data frame with one row per frame, and columns:
#'   * `type`: `"frame"` for a regular zstd frame, `"skippable"` for a
#'     skippable frame.
#'   * `compressed_size`: size of the frame on disk, in bytes.
#'   * `content_size`: decompressed size of the frame, in bytes, or `NA`
#'     if unknown (streaming frames) or the frame is skippable.
#'   * `window_size`: the decompression window size needed, in bytes, or
#'     `NA` for skippable frames.
#'   * `dict_id`: the dictionary ID used to compress the frame, or `NA`
#'     if none was used or the frame is skippable.
#'   * `checksum`: whether the frame includes a content checksum, or `NA`
#'     for skippable frames.
#' @export
#' @examples
#' tmp <- tempfile()
#' writeBin(zstd_mem_compress(charToRaw("hello world")), tmp)
#' zstd_info(tmp)
zstd_info <- function(path) {
  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    stop("`path` must be a single string", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("File does not exist: ", path, call. = FALSE)
  }
  bin <- readBin(path, "raw", file.size(path))
  as.data.frame(.Call(zstd_info_, bin), stringsAsFactors = FALSE)
}

#' Zstandard compression level bounds
#'
#' @return An integer scalar.
#' @export
#' @rdname zstd_clevel
zstd_min_clevel <- function() {
  .Call(zstd_min_clevel_)
}

#' @export
#' @rdname zstd_clevel
zstd_max_clevel <- function() {
  .Call(zstd_max_clevel_)
}

#' @export
#' @rdname zstd_clevel
zstd_default_clevel <- function() {
  .Call(zstd_default_clevel_)
}
