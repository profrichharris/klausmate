#' Create a klausmate key file for distribution to students
#'
#' Bundles an Anthropic API key together with per-student usage-limit settings
#' into a single \code{.key} file that can be emailed to students. Students
#' load the file via the \strong{Load key file} button in the app and never see
#' the raw API key.
#'
#' Any of \code{api_key}, \code{reset_period} and \code{final_expiry} left as
#' \code{NULL}, and \code{cost_limit}, \code{max_conversations} or
#' \code{max_lines} left out entirely, is requested interactively at the
#' console. For those last three, passing \code{NULL} (or typing \code{none})
#' means no limit and requires a Y/N confirmation.
#'
#' @param api_key Anthropic API key (string starting with \code{"sk-ant-"}).
#'   If omitted, you are prompted to paste it.
#' @param cost_limit Maximum spend in USD per reset period. Default \code{0.30}.
#'   If omitted, you are prompted to confirm or change the default. Passing
#'   \code{NULL} (or typing \code{none} at the prompt) creates a key with no
#'   spending limit; you are asked to confirm this with Y/N.
#' @param reset_period Duration of the usage window. Default \code{"weekly"}.
#'   If omitted, you are prompted to confirm or change the default. Options:
#'   \itemize{
#'     \item \code{"weekly"}     — resets at midnight each Monday (default)
#'     \item \code{"rolling_24h"} — rolling 24-hour window
#'     \item \code{"rolling_Nh"} — rolling N hours, e.g. \code{"rolling_6h"}
#'     \item \code{"rolling_Nm"} — rolling N minutes, e.g. \code{"rolling_90m"}
#'     \item \code{"hourly"}     — resets at the top of each clock hour
#'     \item \code{"daily"}      — resets at midnight each day
#'     \item \code{"none"}       — never resets: the cost limit is a total for
#'       the life of the key, which stops working once it is used up. If you
#'       do not give \code{final_expiry}, the default expiry is then 24 hours
#'       after the start rather than 15 weeks
#'   }
#' @param valid_from When the key becomes usable: a date (optionally with a
#'   time, e.g. \code{"31/12/2026 09:00"}) or a \code{POSIXct}. Default is now.
#'   Dates typed at the prompt or passed as text are read in either
#'   day/month/year or month/day/year order (your system's preferred order is
#'   used where it is ambiguous), and also as 2026-12-31 or 31 Dec 2026.
#' @param max_conversations Maximum number of conversations per app start, from
#'   1 to 25. Default \code{3}. If omitted, you are prompted to confirm or
#'   change the default. Passing \code{NULL} (or typing \code{none} at the
#'   prompt) removes the limit; you are asked to confirm this with Y/N.
#' @param max_lines Maximum length, in lines, of code the app will accept or
#'   generate. Default \code{100}. If omitted, you are prompted to confirm or
#'   change the default (suggested choices are 100, 250, 500 or \code{none}; any
#'   whole number of lines can be typed). Passing \code{NULL} (or typing \code{none} at the
#'   prompt) removes the limit; you are asked to confirm this with Y/N.
#' @param pin A 6-digit PIN (a string such as \code{"482915"}). The key file is encrypted with
#'   it, so it is of no use without the PIN; students enter the PIN once, when they first load
#'   the key. The PIN is not stored in the file, so tell students the PIN separately. If omitted
#'   you are asked to choose one, or press Enter to have one generated.
#' @param output_file Path for the key file. Default \code{"klausmate.key"} in
#'   the current working directory.
#'
#' @return The normalised path to the key file, invisibly.
#' @export
klausmate_make_key <- function(api_key           = NULL,
                                cost_limit,
                                reset_period      = NULL,
                                final_expiry      = NULL,
                                valid_from        = NULL,
                                max_conversations,
                                max_lines,
                                pin               = NULL,
                                output_file       = "klausmate.key") {

  confirm_unlimited <- function(what) {
    ans <- trimws(readline(paste0(
      "This key will have NO ", what, ". Are you sure? (Y/N): ")))
    if (!tolower(ans) %in% c("y", "yes"))
      stop("Key not created.", call. = FALSE)
    invisible(TRUE)
  }

  # Ask until the answer is usable. Enter accepts `default`. `parse(ans)` returns list(ok = TRUE, value = ...) or
  # list(ok = FALSE, msg = "what was wrong"); a bad answer is explained and the question asked again.
  ask <- function(prompt, default, parse) {
    repeat {
      ans <- trimws(readline(prompt))
      if (!nzchar(ans)) return(default)
      res <- parse(ans)
      if (isTRUE(res$ok)) return(res$value)
      message(res$msg, "\nPlease try again.")
    }
  }
  is_none <- function(x) tolower(x) %in% c("none", "unlimited")

  if (is.null(api_key) || !nzchar(trimws(api_key))) {
    api_key <- trimws(readline("Paste Anthropic API key: "))
    if (!nzchar(api_key)) stop("No API key provided.", call. = FALSE)
  }

  if (is.null(pin)) {
    message("Students will need a 6-digit PIN to use this key;\nit is not stored in the key file.")
    repeat {
      ans <- trimws(readline("Choose a 6-digit PIN [press Enter to generate one]: "))
      if (!nzchar(ans)) { pin <- sprintf("%06d", sample(0L:999999L, 1L)); break }
      if (klausmate_pin_ok(ans)) { pin <- ans; break }
      message("The PIN must be exactly 6 digits.")
    }
  }
  if (!klausmate_pin_ok(pin))
    stop("pin must be exactly 6 digits, for example \"482915\".", call. = FALSE)

  if (missing(cost_limit)) {
    message("Cost limit: the most a student can spend, in USD, per reset period.\nType 'none' for no limit.")
    cost_limit <- ask("Cost limit in USD [press Enter to accept the default: 0.30]: ", 0.30, function(ans) {
      if (is_none(ans)) return(list(ok = TRUE, value = NULL))
      v <- suppressWarnings(as.numeric(ans))
      if (is.na(v) || v < 0) list(ok = FALSE, msg = paste0("'", ans, "' is not a valid cost limit. Enter a number such as 0.30, or 'none'."))
      else list(ok = TRUE, value = v)
    })
  }
  if (is.null(cost_limit)) confirm_unlimited("spending limit")

  if (missing(max_lines)) {
    message("Maximum code length: other choices are 250, 500, 'none' for no limit,\nor any whole number of lines.")
    max_lines <- ask("Maximum code length in lines [press Enter to accept the default: 100]: ", 100L, function(ans) {
      if (is_none(ans)) return(list(ok = TRUE, value = NULL))
      v <- suppressWarnings(as.integer(ans))
      if (is.na(v) || v < 1L) list(ok = FALSE, msg = paste0("'", ans, "' is not a valid code length. Enter a whole number of lines (1 or more), or 'none'."))
      else list(ok = TRUE, value = v)
    })
  }
  if (is.null(max_lines) || (is.numeric(max_lines) && is.infinite(max_lines))) {
    confirm_unlimited("code-length limit")
    max_lines <- Inf
  } else {
    max_lines <- as.integer(max_lines)
    if (is.na(max_lines) || max_lines < 1L)
      stop("max_lines must be a positive whole number, or NULL for no limit.",
           call. = FALSE)
  }

  if (is.null(reset_period)) {
    message("Reset period options: weekly, daily, hourly, rolling_24h,\n",
            "  rolling_Nh (e.g. rolling_6h), rolling_Nm (e.g. rolling_90m),\n",
            "  or none (never resets: the cost limit is a total).")
    reset_period <- ask("Reset period [press Enter to accept the default: weekly]: ", "weekly", function(ans) {
      a <- tolower(ans)
      if (a %in% c("rolling_24h", "hourly", "daily", "weekly", "none") || grepl("^rolling_[0-9]+(h|m)$", a))
        list(ok = TRUE, value = a)
      else list(ok = FALSE, msg = paste0("'", ans, "' is not a reset period. Use weekly, daily, hourly,\n",
                                          "rolling_24h, rolling_Nh (e.g. rolling_6h), rolling_Nm or none."))
    })
  }

  date_order <- klausmate_date_order()
  pattern    <- klausmate_date_pattern(date_order)
  message("Dates are entered as ", pattern, " (taken from your system settings);\n",
          "other forms such as 2026-12-31 or 31 Dec 2026 are also understood.")

  if (is.null(valid_from)) {
    now <- Sys.time()
    message("Start date, with an optional time HH:MM, as ", pattern, ".")
    valid_from <- ask(paste0("Start date [press Enter to accept the default: now (",
                             klausmate_format_date(now, date_order), ")]: "), now, function(ans) {
      parsed <- klausmate_parse_date(ans, date_order)
      if (is.na(parsed)) list(ok = FALSE, msg = paste0("'", ans, "' is not a date I can read. Enter it as ", pattern, ", with an optional time HH:MM."))
      else list(ok = TRUE, value = as.POSIXct(parsed))
    })
  } else if (is.character(valid_from)) {
    p <- klausmate_parse_date(valid_from, date_order)
    if (is.na(p)) stop("Could not understand valid_from: '", valid_from, "'", call. = FALSE)
    valid_from <- as.POSIXct(p)
  } else if (inherits(valid_from, "Date")) {
    valid_from <- as.POSIXct(format(valid_from), tz = "")
  }

  # TRUE if `fe` is a usable final expiry: in the future and after the start
  expiry_ok <- function(fe)
    !(klausmate_is_expired(fe) ||
      (inherits(fe, "Date") && as.POSIXct(format(fe + 1L), tz = "") <= valid_from) ||
      (inherits(fe, "POSIXt") && fe <= valid_from))

  if (is.null(final_expiry)) {
    # A key with a once-only allowance (reset period "none") is meant to be short-lived: 24 hours after it starts.
    # Otherwise the default is 15 weeks after today.
    once_only      <- identical(tolower(trimws(reset_period)), "none")
    default_expiry <- if (once_only) as.POSIXct(valid_from) + 24 * 60 * 60 else Sys.Date() + 7L * 15L
    default_str    <- klausmate_format_date(default_expiry, date_order)
    message("Final expiry date, with an optional time HH:MM, as ", pattern, ".")
    if (once_only)
      message("The reset period is 'none', so the default expiry is 24 hours after the start.")
    repeat {
      final_expiry <- ask(paste0("Final expiry date [press Enter to accept the default: ", default_str, "]: "),
                          default_expiry, function(ans) {
        parsed <- klausmate_parse_date(ans, date_order)
        if (is.na(parsed)) list(ok = FALSE, msg = paste0("'", ans, "' is not a date I can read. Enter it as ", pattern, ", with an optional time HH:MM."))
        else list(ok = TRUE, value = if (isTRUE(attr(parsed, "has_time"))) as.POSIXct(parsed) else as.Date(format(parsed)))
      })
      if (expiry_ok(final_expiry)) break
      message("The final expiry must be in the future and after the start date. Please try again.")
    }
  } else if (is.character(final_expiry)) {
    parsed <- klausmate_parse_date(final_expiry, date_order)
    if (is.na(parsed)) stop("Could not understand final_expiry: '", final_expiry, "'", call. = FALSE)
    final_expiry <- if (isTRUE(attr(parsed, "has_time"))) as.POSIXct(parsed)
                    else as.Date(format(parsed))
  }
  if (!expiry_ok(final_expiry))
    stop("The final expiry is not after the start date.", call. = FALSE)

  if (missing(max_conversations)) {
    message("Max conversations per app start: from 1 to 25, or 'none' for no limit.")
    max_conversations <- ask("Max conversations [press Enter to accept the default: 3]: ", 3L, function(ans) {
      if (is_none(ans) || ans == "0") return(list(ok = TRUE, value = NULL))
      v <- suppressWarnings(as.integer(ans))
      if (is.na(v) || v < 1L) list(ok = FALSE, msg = paste0("'", ans, "' is not valid. Enter a whole number from 1 to 25, or 'none'."))
      else list(ok = TRUE, value = min(v, 25L))
    })
  }
  if (is.null(max_conversations)) {
    confirm_unlimited("limit on conversations")
  } else {
    max_conversations <- min(max(as.integer(max_conversations), 1L), 25L)
  }

  valid_named <- c("rolling_24h", "hourly", "daily", "weekly", "none")
  is_rolling_n <- grepl("^rolling_[0-9]+(h|m)$", reset_period)
  if (!reset_period %in% valid_named && !is_rolling_n)
    stop("Unrecognised reset_period: '", reset_period, "'\n",
         "Use one of: rolling_24h, rolling_Nh, rolling_Nm, hourly, daily, weekly, none",
         call. = FALSE)

  message("Validating API key ...")
  ok <- tryCatch({
    resp <- httr2::request("https://api.anthropic.com/v1/messages") |>
      httr2::req_headers(
        "x-api-key"         = api_key,
        "anthropic-version" = "2023-06-01",
        "content-type"      = "application/json"
      ) |>
      httr2::req_body_json(list(
        model      = "claude-haiku-5-5",
        thinking   = list(type = "disabled"),
        max_tokens = 1L,
        messages   = list(list(role = "user", content = "Hi"))
      )) |>
      httr2::req_perform()
    httr2::resp_status(resp) == 200L
  }, error = function(e) FALSE)

  if (!ok)
    stop("API key could not be validated. Please check the key and your internet connection, then try again.",
         call. = FALSE)

  payload <- list(
    api_key           = api_key,
    key_id            = paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "_",
                               sample(10000L:99999L, 1L)),
    cost_limit        = cost_limit,
    reset_period      = reset_period,
    final_expiry      = final_expiry,
    valid_from        = valid_from,
    max_conversations = max_conversations,
    max_lines         = max_lines,
    created           = Sys.time()
  )
  saveRDS(klausmate_seal_key(payload, pin), output_file)

  out <- normalizePath(output_file)
  message("Key file created: ", out)
  message("  PIN:          ", pin)
  message("                (give this to students separately; it is not stored in the file)")
  if (!is.null(cost_limit))
    message("  Cost limit:   $", cost_limit,
            if (identical(reset_period, "none")) " in total (never resets)" else paste0(" per ", reset_period))
  else
    message("  Cost limit:   none")
  message("  Valid from:   ", klausmate_format_date(valid_from, date_order, with_time = TRUE))
  message("  Final expiry: ", klausmate_format_date(final_expiry, date_order))
  if (!is.null(max_conversations))
    message("  Max conversations per start: ", max_conversations)
  else
    message("  Max conversations per start: unlimited")
  message("  Max code length: ",
          if (is.infinite(max_lines)) "unlimited" else paste(max_lines, "lines"))
  message("Email this file to students: they load it via the\n",
          "'Load key file' button in the app.")
  invisible(out)
}


#' Show the current klausmate configuration
#'
#' Prints the active cost limit and reset period stored in the most recently
#' loaded key file.
#'
#' @return Invisible \code{NULL}.
#' @export
klausmate_config_show <- function() {
  cfg_path <- klausmate_state_path("config")

  cat("klausmate configuration\n")
  if (file.exists(cfg_path)) {
    cfg <- tryCatch(readRDS(cfg_path), error = function(e) NULL)
    if (!is.null(cfg)) {
      cat("  Key file loaded\n")
      cat("  Cost limit:  ", if (is.null(cfg$cost_limit)) "none"
                             else if (identical(cfg$reset_period, "none")) paste0("$", cfg$cost_limit, " in total (never resets)")
                             else paste0("$", cfg$cost_limit, " per ", cfg$reset_period), "\n")
      cat("  Code length: ", if (is.null(cfg$max_lines)) "50 lines (key predates this setting)"
                             else if (is.infinite(cfg$max_lines)) "unlimited"
                             else paste(cfg$max_lines, "lines"), "\n")
      cat("  Key created: ", format(cfg$created, "%Y-%m-%d %H:%M"), "\n")
    }
  } else {
    cat("  No key file loaded\n")
  }
  invisible(NULL)
}


