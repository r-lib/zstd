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
