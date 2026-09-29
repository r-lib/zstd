is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

is_flag <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

is_count <- function(x, positive = FALSE) {
  limit <- if (positive) 1L else 0L
  is.numeric(x) && length(x) == 1 && !is.na(x) && x >= limit
}

as_string <- function(
  x,
  null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (null && is.null(x)) {
    return(x)
  }
  if (is_string(x)) {
    return(x)
  }

  stop(cnd(
    call = call,
    "Invalid argument: `{arg}` must be a string scalar, but it is \\
     {typename(x)}."
  ))
}

as_existing_file <- function(x, arg = caller_arg(x), call = caller_env()) {
  force(arg)
  x <- as_string(x, arg = arg, call = call)
  if (file.exists(x)) {
    return(x)
  }

  stop(cnd(
    call = call,
    "Invalid argument: `{arg}` must be an existing file. \\
     File does not exist: '{x}'."
  ))
}

as_raw <- function(x, null = FALSE, arg = caller_arg(x), call = caller_env()) {
  if (null && is.null(x)) {
    return(x)
  }
  if (is.raw(x)) {
    return(x)
  }

  stop(cnd(
    call = call,
    "Invalid argument: `{arg}` must be a raw vector\\
     {if (null) ' or NULL' else ''}, but it is {typename(x)}."
  ))
}

as_flag <- function(x, null = FALSE, arg = caller_arg(x), call = caller_env()) {
  if (null && is.null(x)) {
    return(x)
  }
  if (is_flag(x)) {
    return(x)
  }

  stop(cnd(
    call = call,
    "Invalid argument: `{arg}` must be a flag (logical scalar)\\
     {if (null) ' or NULL' else ''}, but it is {typename(x)}."
  ))
}

as_count <- function(
  x,
  positive = FALSE,
  null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (null && is.null(x)) {
    return(x)
  }
  if (is_count(x, positive = positive)) {
    return(as.integer(x))
  }

  limit <- if (positive) 1L else 0L
  if (is.numeric(x) && length(x) != 1) {
    stop(cnd(
      call = call,
      "Invalid argument: `{arg}` must be an integer scalar, not a vector."
    ))
  } else if (is.numeric(x) && length(x) == 1 && is.na(x)) {
    stop(cnd(
      call = call,
      "Invalid argument: `{arg}` must not be `NA`."
    ))
  } else if (is.numeric(x) && length(x) == 1 && !is.na(x) && x < limit) {
    stop(cnd(
      call = call,
      "Invalid argument: `{arg}` must be \\
      {if (positive) 'positive' else 'non-negative'}."
    ))
  } else {
    stop(cnd(
      call = call,
      "Invalid argument: `{arg}` must be a \\
      {if (positive) 'positive' else 'non-negative'} integer scalar, \\
      but it is {typename(x)}."
    ))
  }
}

as_clevel <- function(x, arg = caller_arg(x), call = caller_env()) {
  min <- zstd_min_clevel()
  max <- zstd_max_clevel()
  if (is.numeric(x) && length(x) == 1 && !is.na(x)) {
    if (x >= min && x <= max) {
      return(as.integer(x))
    }
    stop(cnd(
      call = call,
      "Invalid argument: `{arg}` must be between {min} and {max}, \\
       but it is {x}."
    ))
  }

  stop(cnd(
    call = call,
    "Invalid argument: `{arg}` must be an integer scalar, but it is \\
     {typename(x)}."
  ))
}

as_choice <- function(
  x,
  choices,
  null = TRUE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (null && is.null(x)) {
    return(x)
  }
  if (is_string(x) && !is.na(mch <- match(tolower(x), choices))) {
    return(mch)
  }

  cchoices <- paste0("'", choices, "'", collapse = ", ")
  if (is_string(x)) {
    stop(cnd(
      call = call,
      "Invalid argument: `{arg}` must be one of {cchoices}, but it is '{x}'."
    ))
  } else {
    stop(cnd(
      call = call,
      "Invalid argument: `{arg}` must be a string scalar, one of \\
       {cchoices}, but it is {typename(x)}."
    ))
  }
}

as_files <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!is.character(x) || length(x) == 0 || anyNA(x)) {
    stop(cnd(
      call = call,
      "Invalid argument: `{arg}` must be a non-empty character vector \\
       without `NA` values, but it is {typename(x)}."
    ))
  }
  missing <- x[is.na(file.info(x)$isdir)]
  if (length(missing) > 0) {
    stop(cnd(
      call = call,
      "Invalid argument: all files in `{arg}` must exist. \\
       File does not exist: '{missing[1]}'."
    ))
  }
  x
}

as_samples <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (is.character(x)) {
    if (anyNA(x)) {
      stop(cnd(
        call = call,
        "Invalid argument: `{arg}` must not contain `NA` file paths."
      ))
    }
    missing <- x[!file.exists(x) | dir.exists(x)]
    if (length(missing) > 0) {
      stop(cnd(
        call = call,
        "Invalid argument: all files in `{arg}` must exist. \\
         File does not exist: '{missing[1]}'."
      ))
    }
    return(lapply(x, function(path) readBin(path, "raw", file.size(path))))
  }
  if (is.list(x) && all(vapply(x, is.raw, logical(1)))) {
    return(x)
  }

  stop(cnd(
    call = call,
    "Invalid argument: `{arg}` must be a list of raw vectors or a character \\
     vector of file paths, but it is {typename(x)}."
  ))
}

as_common_cparams <- function(
  window_log,
  checksum,
  strategy,
  nb_workers,
  content_size,
  dict_id,
  long_distance_matching,
  call = caller_env()
) {
  list(
    window_log = as_count(window_log, null = TRUE, call = call),
    checksum = as_flag(checksum, call = call),
    strategy = as_choice(strategy, zstd_strategies, call = call),
    nb_workers = as_count(nb_workers, null = TRUE, call = call),
    content_size = as_flag(content_size, call = call),
    dict_id = as_flag(dict_id, call = call),
    ldm = as_flag(long_distance_matching, null = TRUE, call = call)
  )
}
