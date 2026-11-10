# Date handling for key start / expiry: detect the user's preferred order
# (day/month/year vs month/day/year vs year/month/day), parse input in any
# common form, and format dates in the preferred order.

# Returns "dmy", "mdy" or "ymd": the date order this user probably expects.
# R's own locale is a poor guide (RStudio and R often run as en_US even on a UK computer), so the order comes from,
# in turn: options(klausmate.date_order=); the computer's region / short-date setting (macOS, Windows); the time
# zone; R's locale; and finally day/month/year.
klausmate_date_order <- function() {
  opt <- getOption("klausmate.date_order")
  if (is.character(opt) && opt %in% c("dmy", "mdy", "ymd")) return(opt)
  for (f in list(km_order_from_os, function() km_order_from_timezone(), km_order_from_r_locale)) {
    o <- tryCatch(f(), error = function(e) NULL)
    if (is.character(o) && length(o) == 1L && o %in% c("dmy", "mdy", "ymd")) return(o)
  }
  "dmy"
}

# Day/month/year unless the country is known to use month/day/year or year/month/day
km_order_from_region <- function(region) {
  region <- toupper(region)
  if (region %in% c("US", "PH", "PR", "GU", "VI", "AS", "MP", "FM", "MH", "PW")) return("mdy")
  if (region %in% c("CN", "JP", "KR", "TW", "HU", "LT", "SE", "MN", "KP", "IR", "ZA")) return("ymd")
  "dmy"
}

# From the computer's own setting: macOS region (e.g. en_GB), Windows short-date pattern (e.g. dd/MM/yyyy)
km_order_from_os <- function() {
  sysname <- Sys.info()[["sysname"]]
  if (sysname == "Darwin") {
    loc <- suppressWarnings(system2("defaults", c("read", "-g", "AppleLocale"), stdout = TRUE, stderr = FALSE))
    if (length(loc) == 1L && grepl("_[A-Za-z]{2}", loc)) {
      region <- sub(".*_([A-Za-z]{2}).*", "\\1", loc)
      return(km_order_from_region(region))
    }
  } else if (sysname == "Windows") {
    out <- suppressWarnings(system2("reg", c("query", shQuote("HKCU\\Control Panel\\International"), "/v", "sShortDate"),
                                    stdout = TRUE, stderr = FALSE))
    line <- grep("sShortDate", out, value = TRUE)
    if (length(line)) return(km_order_from_pattern(sub(".*REG_SZ\\s+", "", line[1])))
  }
  NULL
}

# "dd/MM/yyyy" -> "dmy", "M/d/yyyy" -> "mdy", "yyyy-MM-dd" -> "ymd"
km_order_from_pattern <- function(pat) {
  pos <- c(d = regexpr("d", pat), m = regexpr("M", pat), y = regexpr("y", pat))
  if (any(pos < 0)) return(NULL)
  paste(names(sort(pos)), collapse = "")
}

# From the time zone: US zones mean month/day/year, east-Asian ones year/month/day, other recognised zones day/month/year
km_order_from_timezone <- function(tz = Sys.timezone()) {
  if (is.null(tz) || is.na(tz) || !nzchar(tz)) return(NULL)
  us <- c("New_York", "Chicago", "Denver", "Los_Angeles", "Phoenix", "Anchorage", "Honolulu", "Detroit", "Boise",
          "Indiana/Indianapolis", "Kentucky/Louisville", "Juneau", "Adak", "Menominee", "Nome", "Sitka")
  if (grepl("^US/", tz) || (grepl("^America/", tz) && sub("^America/", "", tz) %in% us)) return("mdy")
  if (tz %in% c("Asia/Tokyo", "Asia/Shanghai", "Asia/Seoul", "Asia/Taipei", "Asia/Pyongyang")) return("ymd")
  if (grepl("^(Europe|Africa|Australia|Atlantic|Indian|Pacific/Auckland|Asia|America|GB|Eire|Etc)", tz)) return("dmy")
  NULL
}

# From R's locale (the date format %x), as a last resort
km_order_from_r_locale <- function() {
  s <- format(as.Date("2031-12-25"), "%x")
  nums <- regmatches(s, gregexpr("[0-9]+", s))[[1]]
  if (length(nums) != 3L) return(NULL)
  key <- vapply(nums, function(n) {
    if (n == "25") "d" else if (n == "12") "m" else if (n %in% c("31", "2031")) "y" else "?"
  }, "")
  ord <- paste(key, collapse = "")
  if (ord %in% c("dmy", "mdy", "ymd")) ord else NULL
}

klausmate_date_pattern <- function(order = klausmate_date_order()) {
  switch(order, mdy = "MM/DD/YYYY", ymd = "YYYY/MM/DD", "DD/MM/YYYY")
}

# Format a Date or POSIXct. with_time = TRUE appends HH:MM.
klausmate_format_date <- function(x, order = klausmate_date_order(),
                                  with_time = inherits(x, "POSIXt")) {
  if (is.null(x) || length(x) == 0L || is.na(x)) return("")
  fmt <- switch(order, mdy = "%m/%d/%Y", ymd = "%Y/%m/%d", "%d/%m/%Y")
  if (with_time) fmt <- paste(fmt, "%H:%M")
  format(x, fmt)
}

# Parse a user-typed date (optionally followed by a time such as 17:30).
# Accepts 2026-12-31, 31/12/2026, 12/31/26, 31.12.26, 31 Dec 2026, Dec 31, 2026.
# Ambiguous numeric dates (both parts <= 12) follow `order`.
# Returns a POSIXct (local time) with attr "has_time", or NA.
klausmate_parse_date <- function(x, order = klausmate_date_order()) {
  na <- structure(as.POSIXct(NA), has_time = FALSE)
  x <- trimws(as.character(x))
  if (length(x) != 1L || is.na(x) || !nzchar(x)) return(na)
  tm <- NULL
  tmr <- regexec("(?<![0-9])([0-9]{1,2}):([0-9]{2})(?::[0-9]{2})?\\s*([AaPp][Mm])?\\s*$", x, perl = TRUE)
  tmm <- regmatches(x, tmr)[[1]]
  if (length(tmm) > 0L) {
    h <- as.integer(tmm[2]); mi <- as.integer(tmm[3]); ap <- tolower(tmm[4])
    if (nzchar(ap)) {
      if (h < 1L || h > 12L) return(na)
      h <- (h %% 12L) + if (ap == "pm") 12L else 0L
    }
    if (h > 23L || mi > 59L) return(na)
    tm <- c(h, mi)
    x <- trimws(sub("\\s*(?<![0-9])[0-9]{1,2}:[0-9]{2}(?::[0-9]{2})?\\s*([AaPp][Mm])?\\s*$", "", x, perl = TRUE))
  }
  mons <- tolower(substr(month.name, 1, 3))
  y <- mth <- d <- NA_integer_
  if (grepl("^[0-9]{4}[-/. ][0-9]{1,2}[-/. ][0-9]{1,2}$", x)) {
    p <- as.integer(strsplit(x, "[-/. ]")[[1]]); y <- p[1]; mth <- p[2]; d <- p[3]
  } else if (grepl("^[0-9]{1,2}[-/. ][0-9]{1,2}[-/. ][0-9]{2}([0-9]{2})?$", x)) {
    p <- strsplit(x, "[-/. ]")[[1]]
    yy <- as.integer(p[3]); if (nchar(p[3]) == 2L) yy <- 2000L + yy
    a <- as.integer(p[1]); b <- as.integer(p[2]); y <- yy
    if (a > 12L && b <= 12L) { d <- a; mth <- b }
    else if (b > 12L && a <= 12L) { mth <- a; d <- b }
    else if (order == "mdy") { mth <- a; d <- b }
    else { d <- a; mth <- b }
  } else {
    w <- strsplit(gsub("[,.]", " ", tolower(x)), "[ -]+")[[1]]
    w <- w[nzchar(w)]
    mi <- which(substr(w, 1, 3) %in% mons & grepl("^[a-z]+$", w))
    if (length(mi) != 1L || length(w) != 3L) return(na)
    mth <- match(substr(w[mi], 1, 3), mons)
    nums <- suppressWarnings(as.integer(w[-mi]))
    if (anyNA(nums)) return(na)
    if (nchar(w[-mi][1]) >= 3L || nums[1] > 31L) { y <- nums[1]; d <- nums[2] }
    else { d <- nums[1]; y <- nums[2] }
    if (y < 100L) y <- 2000L + y
  }
  if (anyNA(c(y, mth, d))) return(na)
  dt <- tryCatch(as.Date(sprintf("%04d-%02d-%02d", y, mth, d)), error = function(e) as.Date(NA))
  if (is.na(dt) || as.integer(format(dt, "%d")) != d) return(na)
  if (is.null(tm)) return(structure(as.POSIXct(format(dt), tz = ""), has_time = FALSE))
  structure(as.POSIXct(sprintf("%s %02d:%02d:00", format(dt), tm[1], tm[2]), tz = ""),
            has_time = TRUE)
}

# Expiry may be a Date (valid through the end of that day) or a POSIXct.
klausmate_is_expired <- function(expiry) {
  if (is.null(expiry) || length(expiry) == 0L || is.na(expiry)) return(FALSE)
  if (inherits(expiry, "POSIXt")) Sys.time() > expiry
  else Sys.Date() > as.Date(expiry)
}

klausmate_not_yet_valid <- function(valid_from) {
  if (is.null(valid_from) || length(valid_from) == 0L || is.na(valid_from)) return(FALSE)
  Sys.time() < as.POSIXct(valid_from)
}


# A period of time typed instead of a date, e.g. "1 day", "2 days", "36 hours", "4 weeks", "1 month", "2 years"
# (also h, d, w, mo, y; singular or plural). Returns list(kind, value): kind "ok" with `value` = `from` plus the period
# (a day is 24 hours, a week 7 days, a month or year a calendar month or year); "short" for a period under one
# hour (minutes and seconds are refused); "none" if `x` is not a period at all (so it can be read as a date).
km_parse_duration <- function(x, from = Sys.time()) {
  m <- regmatches(tolower(trimws(x)),
                  regexec("^([0-9]+(?:\\.[0-9]+)?)\\s*(seconds?|secs?|s|minutes?|mins?|hours?|hrs?|h|days?|d|weeks?|wks?|w|months?|mos?|years?|yrs?|y)$",
                          tolower(trimws(x)), perl = TRUE))[[1]]
  if (length(m) < 3) return(list(kind = "none", value = NULL))
  n    <- as.numeric(m[2])
  unit <- m[3]
  if (grepl("^(s|sec|min)", unit)) return(list(kind = "short", value = NULL))
  from <- as.POSIXct(from)
  value <- if (grepl("^(h)", unit)) from + n * 3600
           else if (grepl("^d", unit)) from + n * 86400
           else if (grepl("^w", unit)) from + n * 7 * 86400
           else {
             if (n != round(n)) return(list(kind = "none", value = NULL))
             k <- if (grepl("^(mo)", unit)) n else 12 * n
             lt <- as.POSIXlt(from); day <- lt$mday; lt$mday <- 1L; lt$mon <- lt$mon + k
             first <- as.POSIXct(lt)                       # the 1st of the target month, same time of day
             mlen  <- as.integer(format(seq(as.Date(first), by = "month", length.out = 2)[2] - 1, "%d"))
             first + (min(day, mlen) - 1L) * 86400         # a 31st becomes the month's last day
           }
  if (is.na(value) || value < from + 3600 - 1e-6) return(list(kind = "short", value = NULL))
  list(kind = "ok", value = value)
}
