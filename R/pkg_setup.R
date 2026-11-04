# Package set-up for the "Preferred approach for..." choices.
#
# The defaults (tidyverse core packages, ggplot2, sf, tmap) are checked, and installed if missing, the first time
# Klausmate runs on a computer (and again after an R upgrade, which has a new library). A package counts as available
# only if it LOADS in a fresh R process, so a broken dependency chain (sf installed but `proxy` missing) is found.
# Options whose packages failed are removed from Preferences and the default falls back to another option in the same
# group. On later runs, and when Preferences is opened, packages that have since become available are added back
# (the defaults are never changed by that). The record is kept in the config folder, never in the key file.
# Everything the student sees is in inst/app/app.R; the logic that can be tested without the app is here.

# group -> options in the order Preferences lists them; `pkgs` = packages each option needs (none = always
# available); `fallback` = order tried when the default is unavailable; `default` = the built-in default.
km_groups <- function() list(
  coding = list(
    default  = "tidyverse",
    options  = c("tidyverse", "Base R", "data.table"),
    fallback = c("tidyverse", "Base R", "data.table"),
    pkgs     = list(tidyverse = c("dplyr", "tidyr", "readr", "purrr", "tibble", "stringr", "forcats", "lubridate"),
                    data.table = "data.table")),
  plotting = list(
    default  = "ggplot2",
    options  = c("ggplot2", "Base R", "plotly"),
    fallback = c("ggplot2", "plotly", "Base R"),
    pkgs     = list(ggplot2 = "ggplot2", plotly = c("plotly", "ggplot2"))),
  mapping = list(
    default  = "tmap",
    options  = c("tmap", "ggplot2", "leaflet", "mapsf", "Base R"),
    fallback = c("tmap", "ggplot2", "leaflet", "mapsf", "Base R"),
    pkgs     = list(tmap = c("tmap", "sf"), ggplot2 = c("ggplot2", "sf"), leaflet = c("leaflet", "sf"),
                    mapsf = c("mapsf", "sf"))),
  spatial = list(
    default  = "sf",
    options  = c("sf", "terra"),
    fallback = c("sf", "terra"),
    pkgs     = list(sf = "sf", terra = "terra"),
    none     = c("No spatial packages available" = "none")),
  strings = list(
    default  = "Same as Coding",
    options  = c("Same as Coding", "tidyverse", "Base R"),
    fallback = c("Same as Coding", "Base R", "tidyverse"),
    pkgs     = list(tidyverse = c("stringr", "lubridate")))
)

# Packages that must be present for the defaults; installed on first run. Minimum versions where the generated
# code depends on them (the system prompt insists on tmap v4).
km_default_pkgs <- function() unique(c(
  "dplyr", "tidyr", "readr", "purrr", "tibble", "stringr", "forcats", "lubridate", "ggplot2", "sf", "tmap"))
km_min_versions <- function() c(tmap = "4.0")
# Alternatives that are only checked (never installed): if installed they are verified to load.
km_alt_pkgs <- function() setdiff(unique(unlist(lapply(km_groups(), function(g) unlist(g$pkgs)))), km_default_pkgs())

# Bumped when the set-up job is fixed or changed in a way that makes an earlier record untrustworthy (for example one
# made while installs were being cut short): every computer then runs the set-up once more.
km_job_version <- function() 2L

km_rversion <- function() paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1], sep = ".")

km_status_path <- function() file.path(klausmate_user_dir("config"), "package_status.rds")

km_status_load <- function() {
  p <- km_status_path()
  if (!file.exists(p)) return(NULL)
  st <- tryCatch(readRDS(p), error = function(e) NULL)
  if (is.list(st) && !is.null(st$rversion)) st else NULL
}
km_status_save <- function(st) {
  tryCatch(saveRDS(st, km_status_path()), error = function(e) NULL)
  invisible(st)
}

# First time on this computer / R version, or an earlier attempt failed only for lack of a connection.
km_needs_setup <- function(st) {
  is.null(st) || !identical(st$rversion, km_rversion()) || !identical(st$jobver, km_job_version()) ||
    any(unname(st$failed) == "offline")
}

# Does this option's packages all work? `st` NULL = nothing known to be wrong.
km_option_ok <- function(g, option, st) {
  pk <- km_groups()[[g]]$pkgs[[option]]
  length(pk) == 0L || !any(pk %in% names(st$failed))
}

# Choices to offer for a group: every option except those whose packages are known not to work.
km_choices <- function(g, st) {
  gr <- km_groups()[[g]]
  ch <- gr$options[vapply(gr$options, function(o) km_option_ok(g, o, st), logical(1))]
  if (length(ch) == 0L && !is.null(gr$none)) return(gr$none)
  ch
}

# Option to use when the current one is not available: the first in the fallback order that is known to work.
km_fallback <- function(g, st) {
  gr <- km_groups()[[g]]
  for (o in gr$fallback) {
    pk <- gr$pkgs[[o]]
    if (length(pk) == 0L || all(pk %in% st$ok)) return(o)
  }
  if (!is.null(gr$none)) unname(gr$none) else gr$options[1]
}

# Merge the result of a job into the stored record.
km_status_merge <- function(old, res) {
  st <- old %||% list(ok = character(0), failed = character(0))
  bad <- res$failed
  st$ok <- setdiff(union(st$ok, res$ok), names(bad))
  keep <- setdiff(names(st$failed), c(res$ok, names(bad)))
  st$failed <- c(st$failed[keep], bad)
  if (is.null(st$failed)) st$failed <- character(0)
  st$rversion <- km_rversion()
  st$jobver   <- km_job_version()
  st$checked  <- Sys.time()
  st
}

# Packages in the record as failed that are now present in a library (worth checking again).
km_refresh_candidates <- function(st) {
  f <- names(st$failed)
  f[vapply(f, function(p) nzchar(system.file(package = p)), logical(1))]
}

# Packages recorded as working that have gone from every library (library changed): mark as failed.
km_drop_missing <- function(st) {
  gone <- st$ok[!vapply(st$ok, function(p) nzchar(system.file(package = p)), logical(1))]
  if (length(gone) > 0L) {
    st$ok <- setdiff(st$ok, gone)
    st$failed <- c(st$failed, stats::setNames(rep("not installed", length(gone)), gone))
  }
  st
}

# The background job. It is passed to callr with its environment set to globalenv(), so it must use only base, utils
# and tools and call nothing in this package. Prints "KM_INSTALL: a b c" before it starts installing so the app can
# say what is happening.
#   check   : packages to verify (loads in a fresh process)
#   install : packages to install if they do not load (the defaults); others are only verified when installed
#   lib     : where to install (NULL = first writable library; a personal library is created if needed)
km_pkg_job <- function(check, install, minver, repos, lib = NULL, attempt_install = TRUE) {
  loads <- function(p) {
    if (!nzchar(system.file(package = p))) return("not installed")
    tryCatch({
      loadNamespace(p)
      mv <- minver[p]
      if (!is.na(mv) && utils::packageVersion(p) < mv)
        paste0("version ", utils::packageVersion(p), " is older than ", mv)
      else TRUE
    }, error = function(e) conditionMessage(e))
  }
  # The packages are checked in a separate, fresh R process, so that this process never has any of them loaded: on
  # Windows a loaded package cannot be replaced, so installing (or updating a dependency) after loading it in here
  # can fail. If the separate process cannot be started, fall back to loading them in here.
  check_script <- tempfile("km_check_", fileext = ".R")
  writeLines(c(
    "a <- commandArgs(TRUE); pk <- strsplit(a[1], ',', fixed = TRUE)[[1]]",
    "mv <- if (length(a) > 1 && nzchar(a[2])) strsplit(strsplit(a[2], ',', fixed = TRUE)[[1]], '=', fixed = TRUE) else list()",
    "min <- stats::setNames(vapply(mv, `[`, '', 2), vapply(mv, `[`, '', 1))",
    "for (p in pk) {",
    "  r <- if (!nzchar(system.file(package = p))) 'not installed' else tryCatch({",
    "    loadNamespace(p)",
    "    if (!is.na(min[p]) && utils::packageVersion(p) < min[p]) paste0('version ', utils::packageVersion(p), ' is older than ', min[p]) else ''",
    "  }, error = function(e) conditionMessage(e))",
    "  cat('KMRES\t', p, '\t', gsub('[\r\n\t]+', ' ', r), '\n', sep = '')",
    "}"), check_script)
  check_all <- function(pk) {
    pk <- unique(pk)
    if (length(pk) == 0L) return(stats::setNames(character(0), character(0)))
    rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
    old <- Sys.getenv("R_LIBS", NA_character_)
    Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))      # the child sees the same libraries
    on.exit(if (is.na(old)) Sys.unsetenv("R_LIBS") else Sys.setenv(R_LIBS = old), add = TRUE)
    mvs <- paste0(names(minver), "=", unname(minver), collapse = ",")
    out <- tryCatch(suppressWarnings(system2(rscript, c("--vanilla", shQuote(check_script), shQuote(paste(pk, collapse = ",")), shQuote(mvs)),
                                             stdout = TRUE, stderr = FALSE)),
                    error = function(e) character(0))
    ln <- strsplit(grep("^KMRES\t", out, value = TRUE), "\t", fixed = TRUE)
    got <- stats::setNames(vapply(ln, function(x) if (length(x) >= 3L) x[3] else "", ""), vapply(ln, `[`, "", 2))
    if (all(pk %in% names(got))) return(got[pk])
    vapply(pk, function(p) { r <- loads(p); if (isTRUE(r)) "" else r }, "")      # fallback
  }
  reachable <- function() {
    old <- options(timeout = 8); on.exit(options(old))
    tryCatch({ con <- url(paste0(repos[[1]], "/src/contrib/PACKAGES.rds"), "rb"); close(con); TRUE },
             error = function(e) FALSE, warning = function(w) FALSE)
  }
  res <- check_all(check)
  bad <- names(res)[nzchar(res)]
  need <- intersect(bad, install)
  offline <- FALSE
  notes <- character(0)       # what install.packages said, kept to explain a failure
  if (attempt_install && length(need) > 0L) {
    if (!reachable()) {
      offline <- TRUE
    } else {
      if (is.null(lib)) {
        lib <- .libPaths()[1]
        if (file.access(lib, 2) != 0) {
          u <- Sys.getenv("R_LIBS_USER")
          if (nzchar(u)) { dir.create(u, recursive = TRUE, showWarnings = FALSE); .libPaths(c(u, .libPaths())); lib <- u }
        }
      }
      .libPaths(unique(c(lib, .libPaths())))
      for (round in 1:3) {
        stale <- list.files(lib, pattern = "^00LOCK", full.names = TRUE)
        if (length(stale)) unlink(stale, recursive = TRUE)
        cat("KM_INSTALL:", paste(need, collapse = " "), "\n")
        flush(stdout())
        # A warning (a package that is not available, a slow download, an exit status) must not stop the rest from being
        # installed, so warnings are noted and let through; only an error ends the attempt.
        withCallingHandlers(
          tryCatch(utils::install.packages(need, repos = repos, lib = lib, quiet = TRUE),
                   error = function(e) { notes <<- c(notes, conditionMessage(e)); NULL }),
          warning = function(w) { notes <<- c(notes, conditionMessage(w)); invokeRestart("muffleWarning") })
        res[need] <- check_all(need)
        # a package that installs but will not load because one it relies on is missing: install that as well
        dep <- unique(unlist(regmatches(res[need], gregexpr("(?<=there is no package called .)[A-Za-z0-9.]+", res[need], perl = TRUE))))
        dep <- setdiff(dep, need)
        if (length(dep) == 0L) break
        need <- unique(c(need, dep))
        res[dep] <- ""
      }
    }
  }
  bad <- names(res)[nzchar(res)]
  # alternatives that are simply not installed are neither working nor broken
  bad <- bad[!(bad %in% setdiff(check, install) & res[bad] == "not installed")]
  why <- vapply(bad, function(p) {
    if (offline && p %in% install) return("offline")
    r <- res[[p]]
    if (p %in% install && length(notes) > 0L) r <- paste0(r, " (install said: ", substr(paste(unique(notes), collapse = " | "), 1L, 200L), ")")
    r
  }, "")
  list(ok = names(res)[!nzchar(res)], failed = stats::setNames(unname(why), bad),
       installed = if (attempt_install) setdiff(intersect(need, names(res)[!nzchar(res)]), character(0)) else character(0))
}

# The background process for the app. mode "setup": verify everything, install missing defaults. mode "refresh":
# only re-check packages recorded as failed that are present now.
km_start_pkg_job <- function(mode = c("setup", "refresh"), st = NULL, repos = c(CRAN = "https://cloud.r-project.org")) {
  mode <- match.arg(mode)
  extra <- getOption("klausmate.setup_extra", character(0))   # test hook: more packages to treat as defaults
  defaults <- unique(c(km_default_pkgs(), extra))
  if (mode == "setup") {
    check <- unique(c(defaults, km_alt_pkgs()))
  } else {
    check <- km_refresh_candidates(st)
  }
  f <- km_pkg_job
  environment(f) <- globalenv()
  callr::r_bg(f, args = list(check = check, install = defaults, minver = km_min_versions(), repos = repos,
                             lib = getOption("klausmate.setup_lib"), attempt_install = identical(mode, "setup")),
              supervise = TRUE)
}
