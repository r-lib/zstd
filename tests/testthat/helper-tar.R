# Shared helpers for the PAX/GNU extended header tests in test-zstd-tar.R.

# Builds a single "<len> <text>\n" PAX record, computing `len` (which
# includes its own decimal representation) the same way real tar writers
# do: iterate until the length prefix's own width stops changing the
# total.
pax_raw_record <- function(text) {
  body <- paste0(" ", text, "\n")
  n <- nchar(body) + 1
  repeat {
    reclen <- nchar(as.character(n)) + nchar(body)
    if (reclen == n) {
      return(paste0(n, body))
    }
    n <- reclen
  }
}

pax_record <- function(key, value) pax_raw_record(paste0(key, "=", value))

# The current user's uid/gid, so the tar fixtures below can set a real
# owner instead of 0 (root): chown()ing extracted files to the uid/gid
# that's already running the test succeeds even when not running as
# root, unlike chown(..., 0, 0). Falls back to 0 where `id` isn't
# available (e.g. Windows), where the fixed-owner tests are skipped.
test_uid <- suppressWarnings(as.integer(tryCatch(
  system("id -u", intern = TRUE),
  error = function(e) NA
)))
test_gid <- suppressWarnings(as.integer(tryCatch(
  system("id -g", intern = TRUE),
  error = function(e) NA
)))
if (is.na(test_uid)) {
  test_uid <- 0L
}
if (is.na(test_gid)) {
  test_gid <- 0L
}

make_tar_header <- function(name, size, type = "0") {
  h <- raw(512)
  raw_name <- charToRaw(name)
  h[seq_along(raw_name)] <- raw_name
  set_field <- function(h, start, value) {
    v <- charToRaw(value)
    h[start:(start + length(v) - 1)] <- v
    h
  }
  h <- set_field(h, 101, sprintf("%07o", 420))
  h <- set_field(h, 109, sprintf("%07o", test_uid))
  h <- set_field(h, 117, sprintf("%07o", test_gid))
  h <- set_field(h, 125, sprintf("%011o", size))
  h <- set_field(h, 137, sprintf("%011o", 0))
  h[149:156] <- charToRaw("        ")
  h <- set_field(h, 157, type)
  h <- set_field(h, 258, "ustar")
  h[264:265] <- charToRaw("00")
  chk <- sum(as.integer(h))
  h <- set_field(h, 149, sprintf("%06o", chk))
  h[155] <- as.raw(0)
  h[156] <- charToRaw(" ")
  h
}

write_tar_entry <- function(con, name, content, type = "0") {
  writeBin(make_tar_header(name, length(content), type = type), con)
  writeBin(content, con)
  pad_len <- (512 - (length(content) %% 512)) %% 512
  if (pad_len > 0) writeBin(raw(pad_len), con)
}
