#' Zstandard compression level bounds
#'
#' `zstd_min_clevel()` and `zstd_max_clevel()` return the smallest and
#' largest compression levels that the bundled zstd library accepts.
#' `zstd_default_clevel()` returns the level used when you do not
#' specify one.
#'
#' @details
#' Positive levels trade speed for a better compression ratio. Levels
#' above 19 need a lot more memory, both for compression and
#' decompression.
#'
#' Negative levels are "fast" modes. They are faster than level 1, but
#' compress less. The smallest allowed level is very negative, because
#' zstd uses the level to set its internal target length, and the
#' negative of the largest target length (\eqn{2^{17} = 131072}) is the
#' lower bound. Levels below about -7 give little extra speed, and the
#' output quickly gets close to the size of the input, so they are
#' rarely useful in practice.
#'
#' Level 0 is a special value: zstd treats it as a request for the
#' default level, so compressing with level 0 is the same as compressing
#' with level `r zstd_default_clevel()`. `zstd_default_clevel()` itself
#' never returns 0.
#'
#' @return An integer scalar.
#'
#' @examples
#' zstd_min_clevel()
#' zstd_max_clevel()
#' zstd_default_clevel()
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
