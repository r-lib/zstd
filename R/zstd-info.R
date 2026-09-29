#' Get information about a Zstandard-compressed file
#'
#' Reads the frame headers of a zstd file without decompressing its
#' content. Handles concatenated files (multiple zstd frames stored
#' back to back, e.g. produced by appending compressed chunks to an
#' existing file), returning one row per frame in the order they
#' appear in the file.
#'
#' @param path Path to a zstd-compressed file, or a glob pattern (e.g.
#'   `"*.zst"`) matching several files.
#' @return A data frame with one row per frame, and columns:
#'   * `path`: path of the file the frame came from.
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
#' @examplesIf !asNamespace("zstd")$is_rcmd_check()
#' tmp <- tempfile()
#' writeBin(zstd_mem_compress(charToRaw("hello world")), tmp)
#' zstd_info(tmp)
#' unlink(tmp)
zstd_info <- function(path) {
  path <- as_path(path)
  files <- Sys.glob(path)
  if (length(files) == 0) {
    stop(cnd("No files match: '{path}'."))
  }
  info <- lapply(files, function(file) {
    df <- as.data.frame(.Call(zstd_info_, file), stringsAsFactors = FALSE)
    cbind(path = file, df, stringsAsFactors = FALSE)
  })
  do.call(rbind, info)
}
