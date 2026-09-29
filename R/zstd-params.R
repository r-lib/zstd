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
    window_log <- suppressWarnings(as.integer(window_log))
    if (length(window_log) != 1 || is.na(window_log)) {
      stop("`window_log` must be an integer or NULL", call. = FALSE)
    }
  }
  if (!is.logical(checksum) || length(checksum) != 1 || is.na(checksum)) {
    stop("`checksum` must be `TRUE` or `FALSE`", call. = FALSE)
  }
  strategy <- zstd_strategy_int(strategy)
  if (!is.null(nb_workers)) {
    nb_workers <- suppressWarnings(as.integer(nb_workers))
    if (length(nb_workers) != 1 || is.na(nb_workers) || nb_workers < 0) {
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
