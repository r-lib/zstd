#' Compress a raw vector with Zstandard
#'
#' @param x A raw vector to compress.
#' @param level Integer compression level. Defaults to
#'   [zstd_default_clevel()]. Valid range is [zstd_min_clevel()] to
#'   [zstd_max_clevel()].
#' @return A raw vector: the compressed data.
#' @export
#' @examples
#' x <- charToRaw(paste(rep("hello world ", 1000), collapse = ""))
#' cmp <- zstd_compress(x)
#' identical(zstd_decompress(cmp), x)
zstd_compress <- function(x, level = zstd_default_clevel()) {
  if (!is.raw(x)) {
    stop("`x` must be a raw vector", call. = FALSE)
  }
  level <- as.integer(level)
  if (level < zstd_min_clevel() || level > zstd_max_clevel()) {
    stop(
      "`level` must be between ", zstd_min_clevel(), " and ",
      zstd_max_clevel(),
      call. = FALSE
    )
  }
  .Call(zstd_compress_, x, level)
}

#' Decompress a Zstandard-compressed raw vector
#'
#' @param x A raw vector of Zstandard-compressed data (a single frame).
#' @return A raw vector: the decompressed data.
#' @export
#' @examples
#' x <- charToRaw("hello world")
#' zstd_decompress(zstd_compress(x))
zstd_decompress <- function(x) {
  if (!is.raw(x)) {
    stop("`x` must be a raw vector", call. = FALSE)
  }
  .Call(zstd_decompress_, x)
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
#' writeBin(zstd_compress(charToRaw("hello world")), tmp)
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
