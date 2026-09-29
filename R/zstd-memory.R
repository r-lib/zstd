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
#' @param window_log `NULL` (library default), or an integer setting the
#'   maximum back-reference distance as a power of two, in bytes. Larger
#'   values can improve the compression ratio of large, redundant inputs,
#'   at the cost of memory use on both the compression and decompression
#'   side.
#' @param checksum Whether to store a checksum of the decompressed content
#'   in the frame. [zstd_mem_decompress()] and [zstd_decompress()] always
#'   verify this checksum automatically when present, and error if it does
#'   not match.
#' @param strategy `NULL` (library default for `level`), or one of
#'   `"fast"`, `"dfast"`, `"greedy"`, `"lazy"`, `"lazy2"`, `"btlazy2"`,
#'   `"btopt"`, `"btultra"`, `"btultra2"`, from fastest to strongest.
#' @param nb_workers Number of compression worker threads. `0` (the
#'   default) compresses on the calling thread. Higher values can speed up
#'   compression of large inputs at some cost to the compression ratio.
#' @param content_size Whether to store the decompressed size in the frame
#'   header, when known. Defaults to `TRUE`.
#' @param dict_id Whether to store the dictionary's ID in the frame header,
#'   when a dictionary is used. Defaults to `TRUE`.
#' @param long_distance_matching `NULL` (library default), or `TRUE`/
#'   `FALSE` to force long distance matching on or off. Improves the
#'   compression ratio of large inputs with repetition far apart.
#' @return A raw vector: the compressed data.
#' @export
#' @examples
#' x <- charToRaw(paste(rep("hello world ", 1000), collapse = ""))
#' cmp <- zstd_mem_compress(x)
#' identical(zstd_mem_decompress(cmp), x)
zstd_mem_compress <- function(
  x,
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
  x <- as_raw(x)
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
    zstd_mem_compress_,
    x,
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
}

#' Decompress a Zstandard-compressed raw vector
#'
#' If the frame carries a content checksum (see the `checksum` argument of
#' [zstd_mem_compress()]), it is verified automatically; an error is raised
#' if the checksum does not match.
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
  x <- as_raw(x)
  dict <- as_raw(dict, null = TRUE)
  .Call(zstd_mem_decompress_, x, dict)
}
