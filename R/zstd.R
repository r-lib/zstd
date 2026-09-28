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
