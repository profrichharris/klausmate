# Probe: can a Shiny app shown in RStudio's Viewer pane talk to RStudio?
#
# Run this in RStudio (not in a plain R terminal):
#   source("tools/probe_rstudio.R")      # or open the file and press Source
#
# It opens a small app in the Viewer pane. It needs no API key and changes none of your files
# (the "send" button only opens a NEW, UNSAVED script tab). Press each button, then copy the log at
# the bottom of the app (or the console output) and send it back.
#
# What it checks
#   1. Can the app open a new script tab in RStudio holding text?           (documentNew)
#   2. Can it read the script that is in focus in RStudio?                  (getSourceEditorContext)
#   3. Does that follow you when you click other script tabs while the app stays open?  (Watch box)
#   4. Does code the app runs land in your own workspace?                   (globalenv)
#   5. Does the Viewer allow copying to the clipboard, pop-up windows, and how wide is it?
#   6. Is the R console busy while the app runs? (try typing 1+1 in the console)

for (p in c("shiny", "rstudioapi")) {
  if (!requireNamespace(p, quietly = TRUE)) stop("Please install the '", p, "' package first.")
}
library(shiny)

if (!rstudioapi::isAvailable()) {
  message("RStudio was not detected: the RStudio buttons will report errors. ",
          "Run this from inside RStudio for a real test.")
}

launcher <- function(url) {
  if (rstudioapi::isAvailable() && rstudioapi::hasFun("viewer")) {
    rstudioapi::viewer(url)                       # the Viewer pane
  } else {
    utils::browseURL(url)
  }
}

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-size: 13px; }
    button { margin: 2px 2px 2px 0; }
    #log { white-space: pre-wrap; background: #f6f6f6; border: 1px solid #ddd; padding: 6px;
           height: 260px; overflow-y: auto; font-size: 12px; }
  "))),
  tags$h4("Klausmate RStudio probe"),
  tags$p("Press the buttons, then copy the log below."),
  actionButton("send", "1. Send text to a new script"),
  actionButton("grab", "2. Report the script in focus"),
  checkboxInput("watch", "3. Watch the focused script (every 2 s): click other script tabs and see if it follows", FALSE),
  actionButton("run", "4. Run code in my workspace"),
  actionButton("modal", "5a. Open a pop-up window"),
  actionButton("clip", "5b. Copy text to the clipboard"),
  actionButton("size", "5c. Report Viewer size"),
  tags$hr(),
  tags$strong("Log"),
  tags$div(id = "log", textOutput("log")),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('clip', function(m) {
      var done = function(ok, why) { Shiny.setInputValue('clip_result', {ok: ok, why: why || ''}, {priority: 'event'}); };
      try {
        if (navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard.writeText(m.text).then(function() { done(true, 'navigator.clipboard'); },
                                                       function(e) { done(false, 'navigator.clipboard refused: ' + e); });
        } else {
          var t = document.createElement('textarea'); t.value = m.text; document.body.appendChild(t);
          t.select(); var ok = document.execCommand('copy'); document.body.removeChild(t);
          done(ok, 'execCommand');
        }
      } catch (e) { done(false, 'error: ' + e); }
    });
    $(document).on('shiny:connected', function() {
      Shiny.setInputValue('viewer_size', {w: window.innerWidth, h: window.innerHeight}, {priority: 'event'});
    });
    $(document).on('click', '#size', function() {
      Shiny.setInputValue('viewer_size', {w: window.innerWidth, h: window.innerHeight}, {priority: 'event'});
    });
  "))
)

server <- function(input, output, session) {
  lines <- reactiveVal(character(0))
  say <- function(...) {
    msg <- paste0(format(Sys.time(), "%H:%M:%S"), "  ", paste0(..., collapse = ""))
    message("[probe] ", msg)
    lines(c(isolate(lines()), msg))
  }
  output$log <- renderText(paste(lines(), collapse = "\n"))

  say("App started. RStudio available: ", rstudioapi::isAvailable(),
      "; R ", getRversion(), "; RStudio ", tryCatch(as.character(rstudioapi::versionInfo()$version), error = function(e) "?"))

  observeEvent(input$send, {
    r <- tryCatch({
      id <- rstudioapi::documentNew(
        text = "# Klausmate probe: this is a new script made from the app\nx <- 1:5\nmean(x)\n",
        type = "r")
      paste0("OK, new script opened (document id ", id, "). Look for an Untitled tab in RStudio.")
    }, error = function(e) paste0("FAILED: ", conditionMessage(e)))
    say("1. documentNew: ", r)
  })

  report_focus <- function(prefix) {
    r <- tryCatch({
      ctx <- rstudioapi::getSourceEditorContext()
      if (is.null(ctx)) "no source editor is open" else {
        sel <- ctx$selection[[1]]$text
        paste0("id=", ctx$id, "; path=", if (nzchar(ctx$path)) basename(ctx$path) else "(unsaved)",
               "; lines=", length(ctx$contents),
               "; selection=", if (nzchar(sel)) paste0(nchar(sel), " chars") else "none",
               "; first line: ", substr(ctx$contents[1], 1, 60))
      }
    }, error = function(e) paste0("FAILED: ", conditionMessage(e)))
    say(prefix, r)
  }
  observeEvent(input$grab, report_focus("2. getSourceEditorContext: "))

  last_seen <- reactiveVal("")
  observe({
    req(isTRUE(input$watch))
    invalidateLater(2000, session)
    key <- tryCatch({
      ctx <- rstudioapi::getSourceEditorContext()
      if (is.null(ctx)) "none" else paste(ctx$id, if (nzchar(ctx$path)) basename(ctx$path) else "(unsaved)")
    }, error = function(e) paste0("FAILED: ", conditionMessage(e)))
    if (!identical(key, isolate(last_seen()))) {
      last_seen(key)
      say("3. focused script is now: ", key)
    }
  })

  observeEvent(input$run, {
    r <- tryCatch({
      source(textConnection("probe_x <- 42; probe_where <- environmentName(environment())"), local = FALSE)
      paste0("OK. probe_x now exists in your workspace: ", exists("probe_x", envir = globalenv()),
             " (check the Environment pane; remove it with rm(probe_x, probe_where) when done)")
    }, error = function(e) paste0("FAILED: ", conditionMessage(e)))
    say("4. run: ", r)
  })

  observeEvent(input$modal, {
    showModal(modalDialog(title = "Pop-up test", "If you can read this, pop-up windows work in this pane.",
                          easyClose = TRUE, footer = modalButton("Close")))
    say("5a. pop-up window requested (did it appear?)")
  })

  observeEvent(input$clip, {
    session$sendCustomMessage("clip", list(text = "Klausmate probe clipboard test"))
    say("5b. clipboard copy requested; now paste somewhere (e.g. a script) to see if it worked")
  })
  observeEvent(input$clip_result, {
    say("5b. clipboard result: ", if (isTRUE(input$clip_result$ok)) "reported OK" else "FAILED", " (", input$clip_result$why, ")")
  })

  observeEvent(input$viewer_size, {
    say("5c. pane size: ", input$viewer_size$w, " x ", input$viewer_size$h, " pixels")
  })

  session$onSessionEnded(function() {
    message("[probe] session ended")
    stopApp()
  })
}

message("Opening the probe in the Viewer. Press Stop in the Viewer / Esc in the console, or close the tab, to end it.")
runApp(shinyApp(ui, server), launch.browser = launcher)
