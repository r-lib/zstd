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
