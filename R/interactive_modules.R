#' Run an interactive module.
#'
#' @examples
#' run_module(demo)
#' @param module module name.
#' @name run_module
#' @family {interactive modules}
#' @import shiny
#' @export
run_module <- function(module) {
  modules <- list.files(system.file("modules", package = "mverse"))
  modules <- modules[!grepl(".css", modules)]
  module_msg <- paste0(
    "Valid module names are: \n - ",
    paste(modules, collapse = "\n - ")
  )
  if (missing(module) || !nzchar(module) || !module %in% modules)
    stop(
      "Please run `run_module()` with a valid module name.\n",
      module_msg, call. = FALSE)

  app_dir <- system.file("modules", module, package = "mverse")
  runApp(app_dir, display.mode = "normal")
}

#' @import shiny
#' @import shinyjs
#' @import shinyBS
module_ui <- function(
  infoPanel, defPanel, resPanel, maintitle, subtitle = NULL, css = NULL) {
  ui <- fluidPage(
    includeCSS("../styles.css"),
    if(!is.null(css)) includeCSS(css),
    div(
      useShinyjs(),
      h2(maintitle),
      if(!is.null(subtitle)) h4(subtitle),
      div(
        id = "infobox",
        actionButton(
          "infobtnclose",
          NULL,
          icon = icon("times")
        ),
        infoPanel,
        div(class = "flex-center", actionButton("startbtn", "Start Module"))
      ),
      id = "header"
    ),
    actionButton(
      "infobtn",
      NULL,
      icon = icon("info")
    ),
    tabsetPanel(
      id = "mainmenu",
      tabPanel(
        defPanel,
        title = span(id = "define-title", "Define Multiverse"),
        id = "define", value = "define"
      ),
      tabPanel(
        resPanel,
        title = span(id = "display-title", "Display Multiverse"),
        id = "display", value = "display"
      )
    ),
    bsTooltip("infobtn", "Click to view the background information.",
              placement = "left", options = list(container = "body")),
    bsTooltip("display-title", "Click to view the multiverse results",
              placement = "top", options = list(container = "body")),
    bsTooltip("define-title", "Click to define the multiverse.",
              placement = "top", options = list(container = "body"))
  )
}
