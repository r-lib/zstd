#' Zstandard library version
#'
#' Reports which zstd library the package uses. When the package is
#' installed from source, it uses the system libzstd if a suitable one
#' (version 1.5.0 or later) is found, otherwise it uses the copy of zstd
#' that is bundled with the package. Set the `ZSTD_BUNDLED` environment
#' variable to `true` at installation time to always use the bundled
#' copy.
#'
#' @return A list with class `zstd_version` and entries:
#'   * `type`: `"system"` or `"bundled"`.
#'   * `version`: the version of the zstd library, as a string.
#'   * `path`: path to the file that contains the zstd library code.
#'     For the bundled library, or a system library that was linked
#'     statically, this is the shared library of the zstd R package.
#'   * `static`: `TRUE` if the zstd library is part of the shared
#'     library of the zstd R package, `FALSE` if it is loaded from a
#'     separate shared library.
#'
#' @examples
#' zstd_version()
#' @export
zstd_version <- function() {
  res <- .Call(zstd_version_)
  res$static <- identical(res$path, res$own_path)
  res$own_path <- NULL
  structure(res, class = "zstd_version")
}

#' @export
format.zstd_version <- function(x, ...) {
  c(
    paste0(
      "zstd ",
      x$version,
      " (",
      x$type,
      if (x$type == "system") {
        if (x$static) ", static" else ", dynamic"
      },
      ")"
    ),
    paste0("path: ", x$path)
  )
}

#' @export
print.zstd_version <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}
