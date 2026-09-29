#' Compress files and directories into a Zstandard-compressed tar archive
#'
#' Creates a `.tar.zst` archive: a tar archive of `files`, piped directly
#' into the zstd streaming compressor without ever writing an uncompressed
#' tar file to disk. Uses the streaming API, so memory use stays bounded
#' regardless of the total size of `files`.
#'
#' Entries are stored using ustar-format tar headers. Relative paths up to
#' about 254 bytes are supported (longer paths raise an error); files
#' larger than 4GB are not supported.
#'
#' @param files A character vector of paths to files and/or directories to
#'   archive. Directories are added recursively. Paths are stored in the
#'   archive as given, so use relative paths (e.g. after `setwd()` into the
#'   directory to archive) to avoid embedding local filesystem details in
#'   the archive.
#' @param output Path of the `.tar.zst` file to create. Overwritten if it
#'   already exists.
#' @param dict `NULL`, or a raw vector containing a dictionary (as created
#'   by [zstd_train_dict()], or any raw content dictionary). The same
#'   dictionary must be passed to [zstd_tar_decompress()].
#' @inheritParams zstd_mem_compress
#' @return `output`, invisibly.
#' @export
#' @examplesIf !asNamespace("zstd")$is_rcmd_check()
#' dir <- tempfile()
#' dir.create(file.path(dir, "subdir"), recursive = TRUE)
#' writeLines("hello", file.path(dir, "a.txt"))
#' writeLines("world", file.path(dir, "subdir", "b.txt"))
#' archive <- tempfile(fileext = ".tar.zst")
#' zstd_tar_compress(dir, archive)
#' exdir <- tempfile()
#' zstd_tar_decompress(archive, exdir)
#' readLines(file.path(exdir, basename(dir), "subdir", "b.txt"))
zstd_tar_compress <- function(
  files,
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
  files <- as_files(files)
  info <- file.info(files)
  output <- as_string(output)
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

  paths <- character(0)
  entries <- character(0)
  isdir <- logical(0)
  for (i in seq_along(files)) {
    # Entry names are stored relative to each path's own parent directory
    # (like `tar cf x.tar dir` does), so absolute paths (e.g. from
    # tempfile()) don't leak local filesystem structure into the archive.
    base <- dirname(files[i])

    paths <- c(paths, files[i])
    entries <- c(entries, tar_relative_path(files[i], base))
    isdir <- c(isdir, info$isdir[i])
    if (info$isdir[i]) {
      children <- list.files(
        files[i],
        recursive = TRUE,
        all.files = TRUE,
        include.dirs = TRUE,
        no.. = TRUE,
        full.names = TRUE
      )
      if (length(children) > 0) {
        cinfo <- file.info(children)
        paths <- c(paths, children)
        entries <- c(entries, tar_relative_path(children, base))
        isdir <- c(isdir, cinfo$isdir)
      }
    }
  }

  .Call(
    zstd_tar_compress_,
    paths,
    entries,
    isdir,
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

# Make `paths` relative to the directory `base`, for use as tar entry
# names. Backslashes are converted to forward slashes first. Paths that
# are not under `base` are returned unchanged (apart from the slashes).
tar_relative_path <- function(paths, base) {
  paths <- gsub("\\", "/", paths, fixed = TRUE)
  base <- gsub("\\", "/", base, fixed = TRUE)
  if (identical(base, ".")) {
    return(paths)
  }
  base_len <- nchar(base)
  is_prefixed <- substr(paths, 1, base_len) == base &
    substr(paths, base_len + 1, base_len + 1) == "/"
  paths[is_prefixed] <- substring(paths[is_prefixed], base_len + 2)
  paths
}

#' Extract a Zstandard-compressed tar archive
#'
#' Decompresses `input` to a temporary file and extracts it, using the
#' streaming API for the decompression step, so memory use for that step
#' stays bounded regardless of the total size of the archive.
#'
#' Plain ustar-format entries (as written by [zstd_tar_compress()], with
#' paths up to about 254 bytes) and PAX extended headers (as written by,
#' e.g., macOS's default `tar`, for longer paths or link targets) are
#' supported. `mtime`, and, outside Windows, `uid`/`gid` are applied to
#' extracted files and directories (best-effort: e.g. `chown()` silently
#' has no effect when not running as root). Archives created by other
#' tools that use GNU long name/link headers are not supported, and raise
#' an error rather than being extracted incorrectly.
#'
#' @param input Path to a `.tar.zst` file, as created by
#'   [zstd_tar_compress()].
#' @param exdir Directory to extract files into. Created if it doesn't
#'   already exist.
#' @param dict `NULL`, or a raw vector containing the dictionary that was
#'   used to compress `input`. See [zstd_tar_compress()].
#' @return `exdir`, invisibly.
#' @export
#' @examplesIf !asNamespace("zstd")$is_rcmd_check()
#' dir <- tempfile()
#' dir.create(dir)
#' writeLines("hello world", file.path(dir, "a.txt"))
#' archive <- tempfile(fileext = ".tar.zst")
#' zstd_tar_compress(dir, archive)
#' exdir <- tempfile()
#' zstd_tar_decompress(archive, exdir)
#' readLines(file.path(exdir, basename(dir), "a.txt"))
zstd_tar_decompress <- function(input, exdir = ".", dict = NULL) {
  input <- as_existing_file(input)
  exdir <- as_string(exdir)
  dict <- as_raw(dict, null = TRUE)
  if (!dir.exists(exdir)) {
    dir.create(exdir, recursive = TRUE)
  }
  tmp <- tempfile(fileext = ".tar")
  on.exit(unlink(tmp), add = TRUE)
  .Call(zstd_tar_decompress_, input, exdir, dict, tmp)
  invisible(exdir)
}
