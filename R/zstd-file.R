#' Compress a file with Zstandard
#'
#' Uses the streaming API, so memory use stays bounded regardless of the
#' size of `input`.
#'
#' @param input Path to the file to compress.
#' @param output Path of the compressed file to create. Overwritten if it
#'   already exists.
#' @param dict `NULL`, or a raw vector containing a dictionary (as created
#'   by [zstd_train_dict()], or any raw content dictionary). The same
#'   dictionary must be passed to [zstd_decompress()].
#' @inheritParams zstd_mem_compress
#' @return `output`, invisibly.
#' @export
#' @examplesIf !asNamespace("zstd")$is_rcmd_check()
#' src <- tempfile()
#' dst <- tempfile()
#' writeLines(paste(rep("hello world", 1000), collapse = " "), src)
#' zstd_compress(src, dst)
#' zstd_decompress(dst, src2 <- tempfile())
#' identical(
#'   readBin(src, "raw", file.size(src)),
#'   readBin(src2, "raw", file.size(src2))
#' )
#' unlink(c(src, dst, src2))
zstd_compress <- function(
  input,
  output,
  level = zstd_default_clevel(),
  dict = NULL,
  window_log = NULL,
  checksum = FALSE,
  strategy = NULL,
  nb_workers = 0L,
  content_size = TRUE,
  dict_id = TRUE,
  long_distance_matching = NULL
) {
  input <- as_existing_file(input)
  output <- as_path(output)
  dict <- as_raw(dict, null = TRUE)
  level <- as_clevel(level)
  p <- as_common_cparams(
    window_log,
    checksum,
    strategy,
    nb_workers,
    content_size,
    dict_id,
    long_distance_matching
  )
  .Call(
    zstd_compress_file_,
    input,
    output,
    level,
    dict,
    p$window_log,
    p$checksum,
    p$strategy,
    p$nb_workers,
    p$content_size,
    p$dict_id,
    p$ldm
  )
  invisible(output)
}

#' Decompress a Zstandard-compressed file
#'
#' Uses the streaming API, so memory use stays bounded regardless of the
#' size of `input`. If the frame carries a content checksum (see the
#' `checksum` argument of [zstd_compress()]), it is verified automatically;
#' an error is raised if the checksum does not match.
#'
#' @param input Path to a Zstandard-compressed file.
#' @param output Path of the decompressed file to create. Overwritten if it
#'   already exists.
#' @param dict `NULL`, or a raw vector containing the dictionary that was
#'   used to compress `input`. See [zstd_compress()].
#' @return `output`, invisibly.
#' @export
#' @examplesIf !asNamespace("zstd")$is_rcmd_check()
#' src <- tempfile()
#' dst <- tempfile()
#' writeLines("hello world", src)
#' zstd_compress(src, dst)
#' zstd_decompress(dst, src2 <- tempfile())
#' identical(
#'   readBin(src, "raw", file.size(src)),
#'   readBin(src2, "raw", file.size(src2))
#' )
#' unlink(c(src, dst, src2))
zstd_decompress <- function(input, output, dict = NULL) {
  input <- as_existing_file(input)
  output <- as_path(output)
  dict <- as_raw(dict, null = TRUE)
  .Call(zstd_decompress_file_, input, output, dict)
  invisible(output)
}
