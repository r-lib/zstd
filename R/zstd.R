#' Zstandard compression strategies, from fastest to strongest
#' @noRd
zstd_strategies <- c(
  "fast",
  "dfast",
  "greedy",
  "lazy",
  "lazy2",
  "btlazy2",
  "btopt",
  "btultra",
  "btultra2"
)

#' Convert a strategy name to the integer zstd expects
#' @noRd
zstd_strategy_int <- function(strategy) {
  if (is.null(strategy)) {
    return(NULL)
  }
  if (!is.character(strategy) || length(strategy) != 1 || is.na(strategy)) {
    stop("`strategy` must be a single string or NULL", call. = FALSE)
  }
  idx <- match(strategy, zstd_strategies)
  if (is.na(idx)) {
    stop(
      "`strategy` must be one of ",
      paste(paste0('"', zstd_strategies, '"'), collapse = ", "),
      ", or NULL",
      call. = FALSE
    )
  }
  idx
}

#' Validate the common advanced compression options shared by
#' [zstd_mem_compress()] and [zstd_compress()]
#' @noRd
zstd_check_common_cparams <- function(
  window_log,
  checksum,
  strategy,
  nb_workers,
  content_size,
  dict_id,
  long_distance_matching
) {
  if (!is.null(window_log)) {
    window_log <- as.integer(window_log)
    if (is.na(window_log)) {
      stop("`window_log` must be an integer or NULL", call. = FALSE)
    }
  }
  if (!is.logical(checksum) || length(checksum) != 1 || is.na(checksum)) {
    stop("`checksum` must be `TRUE` or `FALSE`", call. = FALSE)
  }
  strategy <- zstd_strategy_int(strategy)
  if (!is.null(nb_workers)) {
    nb_workers <- as.integer(nb_workers)
    if (is.na(nb_workers) || nb_workers < 0) {
      stop("`nb_workers` must be a non-negative integer", call. = FALSE)
    }
  }
  if (
    !is.logical(content_size) ||
      length(content_size) != 1 ||
      is.na(content_size)
  ) {
    stop("`content_size` must be `TRUE` or `FALSE`", call. = FALSE)
  }
  if (!is.logical(dict_id) || length(dict_id) != 1 || is.na(dict_id)) {
    stop("`dict_id` must be `TRUE` or `FALSE`", call. = FALSE)
  }
  if (
    !is.null(long_distance_matching) &&
      (!is.logical(long_distance_matching) ||
        length(long_distance_matching) != 1 ||
        is.na(long_distance_matching))
  ) {
    stop(
      "`long_distance_matching` must be `TRUE`, `FALSE`, or NULL",
      call. = FALSE
    )
  }
  list(
    window_log = window_log,
    checksum = checksum,
    strategy = strategy,
    nb_workers = nb_workers,
    content_size = content_size,
    dict_id = dict_id,
    ldm = long_distance_matching
  )
}

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
  if (!is.raw(x)) {
    stop("`x` must be a raw vector", call. = FALSE)
  }
  if (!is.null(dict) && !is.raw(dict)) {
    stop("`dict` must be a raw vector or NULL", call. = FALSE)
  }
  level <- as.integer(level)
  if (level < zstd_min_clevel() || level > zstd_max_clevel()) {
    stop(
      "`level` must be between ",
      zstd_min_clevel(),
      " and ",
      zstd_max_clevel(),
      call. = FALSE
    )
  }
  p <- zstd_check_common_cparams(
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
  if (!is.raw(x)) {
    stop("`x` must be a raw vector", call. = FALSE)
  }
  if (!is.null(dict) && !is.raw(dict)) {
    stop("`dict` must be a raw vector or NULL", call. = FALSE)
  }
  .Call(zstd_mem_decompress_, x, dict)
}

#' Compress a file with Zstandard
#'
#' Uses the streaming API, so memory use stays bounded regardless of the
#' size of `input`.
#'
#' @param input Path to the file to compress.
#' @param output Path of the compressed file to create. Overwritten if it
#'   already exists.
#' @param level Integer compression level. Defaults to
#'   [zstd_default_clevel()]. Valid range is [zstd_min_clevel()] to
#'   [zstd_max_clevel()].
#' @param dict `NULL`, or a raw vector containing a dictionary (as created
#'   by [zstd_train_dict()], or any raw content dictionary). The same
#'   dictionary must be passed to [zstd_decompress()].
#' @inheritParams zstd_mem_compress
#' @return `output`, invisibly.
#' @export
#' @examples
#' src <- tempfile()
#' dst <- tempfile()
#' writeLines(paste(rep("hello world", 1000), collapse = " "), src)
#' zstd_compress(src, dst)
#' zstd_decompress(dst, src2 <- tempfile())
#' identical(readBin(src, "raw", file.size(src)), readBin(src2, "raw", file.size(src2)))
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
  if (!is.character(input) || length(input) != 1 || is.na(input)) {
    stop("`input` must be a single string", call. = FALSE)
  }
  if (!is.character(output) || length(output) != 1 || is.na(output)) {
    stop("`output` must be a single string", call. = FALSE)
  }
  if (!file.exists(input)) {
    stop("File does not exist: ", input, call. = FALSE)
  }
  if (!is.null(dict) && !is.raw(dict)) {
    stop("`dict` must be a raw vector or NULL", call. = FALSE)
  }
  level <- as.integer(level)
  if (level < zstd_min_clevel() || level > zstd_max_clevel()) {
    stop(
      "`level` must be between ",
      zstd_min_clevel(),
      " and ",
      zstd_max_clevel(),
      call. = FALSE
    )
  }
  p <- zstd_check_common_cparams(
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
#' @examples
#' src <- tempfile()
#' dst <- tempfile()
#' writeLines("hello world", src)
#' zstd_compress(src, dst)
#' zstd_decompress(dst, src2 <- tempfile())
#' identical(readBin(src, "raw", file.size(src)), readBin(src2, "raw", file.size(src2)))
zstd_decompress <- function(input, output, dict = NULL) {
  if (!is.character(input) || length(input) != 1 || is.na(input)) {
    stop("`input` must be a single string", call. = FALSE)
  }
  if (!is.character(output) || length(output) != 1 || is.na(output)) {
    stop("`output` must be a single string", call. = FALSE)
  }
  if (!file.exists(input)) {
    stop("File does not exist: ", input, call. = FALSE)
  }
  if (!is.null(dict) && !is.raw(dict)) {
    stop("`dict` must be a raw vector or NULL", call. = FALSE)
  }
  .Call(zstd_decompress_file_, input, output, dict)
  invisible(output)
}

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
#' @param level Integer compression level. Defaults to
#'   [zstd_default_clevel()]. Valid range is [zstd_min_clevel()] to
#'   [zstd_max_clevel()].
#' @param dict `NULL`, or a raw vector containing a dictionary (as created
#'   by [zstd_train_dict()], or any raw content dictionary). The same
#'   dictionary must be passed to [zstd_tar_decompress()].
#' @inheritParams zstd_mem_compress
#' @return `output`, invisibly.
#' @export
#' @examples
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
  if (!is.character(files) || length(files) == 0 || anyNA(files)) {
    stop("`files` must be a non-empty character vector", call. = FALSE)
  }
  if (!is.character(output) || length(output) != 1 || is.na(output)) {
    stop("`output` must be a single string", call. = FALSE)
  }
  info <- file.info(files)
  missing <- files[is.na(info$isdir)]
  if (length(missing) > 0) {
    stop("File does not exist: ", missing[1], call. = FALSE)
  }
  if (!is.null(dict) && !is.raw(dict)) {
    stop("`dict` must be a raw vector or NULL", call. = FALSE)
  }
  level <- as.integer(level)
  if (level < zstd_min_clevel() || level > zstd_max_clevel()) {
    stop(
      "`level` must be between ",
      zstd_min_clevel(),
      " and ",
      zstd_max_clevel(),
      call. = FALSE
    )
  }
  p <- zstd_check_common_cparams(
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
    relative_to_base <- function(p) {
      if (identical(base, ".")) {
        return(p)
      }
      base_len <- nchar(base)
      is_prefixed <- substr(p, 1, base_len) == base &
        substr(p, base_len + 1, base_len + 1) %in% c("/", "\\")
      ifelse(is_prefixed, substring(p, base_len + 2), p)
    }

    paths <- c(paths, files[i])
    entries <- c(entries, relative_to_base(files[i]))
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
        entries <- c(entries, relative_to_base(children))
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
#' @examples
#' dir <- tempfile()
#' dir.create(dir)
#' writeLines("hello world", file.path(dir, "a.txt"))
#' archive <- tempfile(fileext = ".tar.zst")
#' zstd_tar_compress(dir, archive)
#' exdir <- tempfile()
#' zstd_tar_decompress(archive, exdir)
#' readLines(file.path(exdir, basename(dir), "a.txt"))
zstd_tar_decompress <- function(input, exdir = ".", dict = NULL) {
  if (!is.character(input) || length(input) != 1 || is.na(input)) {
    stop("`input` must be a single string", call. = FALSE)
  }
  if (!is.character(exdir) || length(exdir) != 1 || is.na(exdir)) {
    stop("`exdir` must be a single string", call. = FALSE)
  }
  if (!file.exists(input)) {
    stop("File does not exist: ", input, call. = FALSE)
  }
  if (!is.null(dict) && !is.raw(dict)) {
    stop("`dict` must be a raw vector or NULL", call. = FALSE)
  }
  if (!dir.exists(exdir)) {
    dir.create(exdir, recursive = TRUE)
  }
  tmp <- tempfile(fileext = ".tar")
  on.exit(unlink(tmp), add = TRUE)
  .Call(zstd_tar_decompress_, input, exdir, dict, tmp)
  invisible(exdir)
}

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
#' @examples
#' tmp <- tempfile()
#' writeBin(zstd_mem_compress(charToRaw("hello world")), tmp)
#' zstd_info(tmp)
zstd_info <- function(path) {
  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    stop("`path` must be a single string", call. = FALSE)
  }
  files <- Sys.glob(path)
  if (length(files) == 0) {
    stop("No files match: ", path, call. = FALSE)
  }
  info <- lapply(files, function(file) {
    df <- as.data.frame(.Call(zstd_info_, file), stringsAsFactors = FALSE)
    cbind(path = file, df, stringsAsFactors = FALSE)
  })
  do.call(rbind, info)
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
