# ---------------------------------------------------------------------------
# Choice of web browser for klaus()
#
# By default Klausmate opens in the computer's default browser. Only klausmate_browser() changes that: it
# saves the chosen browser in Klausmate's config folder, and klaus() uses it from then on. klausmate_reset()
# does not touch it. If in doubt (nothing saved, the saved browser has gone, the file cannot be read) the
# system default is used.
# ---------------------------------------------------------------------------

KM_BROWSER_DEFAULT <- "System default"

# Browsers that honour the "leave the pointer out" request used by the screenshot shortcut
KM_BROWSER_SHOT_OK <- c("Google Chrome", "Microsoft Edge", "Brave", "Chromium")

km_browser_path <- function() file.path(klausmate_user_dir("config"), "browser.rds")

# Browsers found on this computer: a named list, each element list(kind = "mac"/"exe", target = app name or path)
km_browser_find <- function() {
  found <- list()
  sysname <- Sys.info()[["sysname"]]
  if (sysname == "Darwin") {
    apps <- c("Google Chrome" = "Google Chrome", "Microsoft Edge" = "Microsoft Edge",
              "Brave" = "Brave Browser", "Chromium" = "Chromium", "Firefox" = "Firefox",
              "Safari" = "Safari", "Arc" = "Arc")
    dirs <- c("/Applications", path.expand("~/Applications"))
    for (nm in names(apps)) {
      app <- apps[[nm]]
      hit <- any(file.exists(file.path(dirs, paste0(app, ".app"))))
      if (nm == "Safari") hit <- hit || file.exists("/System/Cryptexes/App/System/Applications/Safari.app")
      if (hit) found[[nm]] <- list(kind = "mac", target = app)
    }
  } else if (sysname == "Windows") {
    roots <- unique(Sys.getenv(c("ProgramFiles", "ProgramFiles(x86)", "LOCALAPPDATA")))
    roots <- roots[nzchar(roots)]
    rel <- c("Google Chrome"  = "Google/Chrome/Application/chrome.exe",
             "Microsoft Edge" = "Microsoft/Edge/Application/msedge.exe",
             "Brave"          = "BraveSoftware/Brave-Browser/Application/brave.exe",
             "Firefox"        = "Mozilla Firefox/firefox.exe")
    for (nm in names(rel)) {
      p <- file.path(roots, rel[[nm]]); p <- p[file.exists(p)]
      if (length(p)) found[[nm]] <- list(kind = "exe", target = normalizePath(p[1]))
    }
  } else {
    cmds <- list("Google Chrome" = c("google-chrome", "google-chrome-stable"),
                 "Microsoft Edge" = "microsoft-edge", "Brave" = "brave-browser",
                 "Chromium" = c("chromium", "chromium-browser"), "Firefox" = "firefox")
    for (nm in names(cmds)) {
      p <- Sys.which(cmds[[nm]]); p <- p[nzchar(p)]
      if (length(p)) found[[nm]] <- list(kind = "exe", target = unname(p[1]))
    }
  }
  found
}

# Open `url` in a browser found by km_browser_find()
km_browser_open <- function(spec, url) {
  if (identical(spec$kind, "mac")) {
    system2("open", c("-a", shQuote(spec$target), shQuote(url)), wait = FALSE)
  } else {
    system2(spec$target, shQuote(url), wait = FALSE)
  }
  invisible(NULL)
}

# The saved choice ("System default" or a browser name), or NULL if none is saved
km_browser_saved <- function() {
  p <- km_browser_path()
  if (!file.exists(p)) return(NULL)
  x <- tryCatch(readRDS(p), error = function(e) NULL)
  if (is.character(x) && length(x) == 1 && nzchar(x)) x else NULL
}

km_browser_save <- function(name) {
  dir.create(dirname(km_browser_path()), showWarnings = FALSE, recursive = TRUE)
  klausmate_save_state(name, km_browser_path())
  invisible(name)
}

# Show the menu of available browsers and return the name chosen, or NULL if the user cancelled.
# `found` is the list from km_browser_find(); `menu_fn` is utils::menu (replaceable for testing).
km_browser_menu <- function(found, menu_fn = utils::menu, why = NULL) {
  nm <- names(found)
  labels <- c(paste0(nm, ifelse(nm %in% KM_BROWSER_SHOT_OK, " (best for screenshots)", "")), KM_BROWSER_DEFAULT)
  title <- paste0(if (!is.null(why)) paste0(why, "\n") else "",
                  "Which web browser would you like Klausmate to open in?\n",
                  "(Your choice is remembered until you change it with klausmate_browser().)")
  i <- menu_fn(labels, title = title)
  if (!is.numeric(i) || length(i) != 1 || is.na(i) || i < 1) return(NULL)
  c(nm, KM_BROWSER_DEFAULT)[i]
}

# What klaus() should pass to shiny::runApp(launch.browser = ): a function that opens the browser chosen with
# klausmate_browser(), or NULL for "use the system default". Never asks anything.
km_browser_launcher <- function(found = km_browser_find()) {
  saved <- km_browser_saved()
  if (is.null(saved) || identical(saved, KM_BROWSER_DEFAULT)) return(NULL)
  if (!saved %in% names(found)) {
    message("Klausmate: the browser you chose (", saved, ") can no longer be found, so your default browser is used. ",
            "Use klausmate_browser() to choose again.")
    return(NULL)
  }
  spec <- found[[saved]]
  function(url) km_browser_open(spec, url)
}

#' Choose the web browser klausmate opens in
#'
#' By default \code{klaus()} opens in your computer's default web browser. Call
#' \code{klausmate_browser()} to choose from the other browsers found on your
#' computer, or name one directly, e.g. \code{klausmate_browser("Firefox")}. The
#' choice is remembered until you change it. Use \code{klausmate_browser("default")}
#' to go back to the computer's default browser. If the chosen browser is no
#' longer installed, the default browser is used. Only \code{klausmate_browser()}
#' changes this setting; \code{klausmate_reset()} leaves it alone.
#'
#' Chrome, Edge and Brave leave the mouse pointer out of Klausmate's screenshots
#' (Cmd/Ctrl+Alt+S); Safari does not.
#'
#' This function will refuse to run while a paused \code{klaus()} session
#' exists: open Klausmate and use \emph{Quit with Code Log} first.
#'
#' @param browser Optional browser name (minor misspellings and lower case are
#'   accepted), or \code{"default"}. If omitted, a menu is shown.
#' @return Invisible name of the browser chosen, or invisible NULL if nothing
#'   was changed.
#' @export
#' @examples
#' \dontrun{
#' klausmate_browser()
#' klausmate_browser("Firefox")
#' }
klausmate_browser <- function(browser = NULL) {
  if (klausmate_paused()) {
    message(
      "klausmate_browser() cannot run while a paused Klausmate session exists.\n",
      "Please open Klausmate and close it with Quit with Code Log first."
    )
    return(invisible(NULL))
  }
  found <- km_browser_find()
  if (is.null(browser)) {
    if (!interactive()) {
      message("klausmate_browser() needs an interactive R session, or a browser name: klausmate_browser(\"Firefox\").")
      return(invisible(NULL))
    }
    saved <- km_browser_saved()
    if (!is.null(saved)) message("Current choice: ", saved)
    choice <- km_browser_menu(found)
    if (is.null(choice)) { message("No change made."); return(invisible(NULL)) }
  } else {
    if (!is.character(browser) || length(browser) != 1 || !nzchar(trimws(browser))) {
      message("Please give a browser name, e.g. klausmate_browser(\"Firefox\"), or \"default\".")
      return(invisible(NULL))
    }
    b <- trimws(browser)
    options <- c(names(found), KM_BROWSER_DEFAULT)
    if (tolower(b) %in% c("default", "system", "system default")) {
      choice <- KM_BROWSER_DEFAULT
    } else {
      exact <- options[tolower(options) == tolower(b)]
      if (length(exact)) {
        choice <- exact[1]
      } else {
        part <- options[grepl(tolower(b), tolower(options), fixed = TRUE)]
        near <- if (length(part)) part else options[which.min(utils::adist(tolower(b), tolower(options))[1, ])]
        d <- if (length(part)) 0 else utils::adist(tolower(b), tolower(near[1]))[1, 1]
        if (length(found) == 0 || d > 3) {
          message("Klausmate could not find a browser called '", browser, "'.",
                  if (length(found)) paste0(" Available: ", paste(names(found), collapse = ", "), ", default.") else "")
          return(invisible(NULL))
        }
        choice <- near[1]
      }
    }
  }
  km_browser_save(choice)
  message("Klausmate will open in: ", choice, ".")
  invisible(choice)
}
