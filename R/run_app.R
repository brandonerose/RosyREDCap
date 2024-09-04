#' @import RosyUtils
#' @title run_RosyREDCap
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @export
run_RosyREDCap <- function(
    onStart = NULL,
    options = list(
      launch.browser = T
    ),
    enableBookmarking = NULL,
    uiPattern = "/",
    dev = F,
    ...
) {
  if(dev){
    options <- options(shiny.launch.browser = .rs.invokeShinyPaneViewer)
  }
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
