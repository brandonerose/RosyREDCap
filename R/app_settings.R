#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(...) {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = .packageName
    ),
    # includeCSS(system.file(package="table1", "table1_defaults_1.0/table1_defaults.css")),
    ...
  )
}
#' @title run_RosyREDCap
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @family Shiny Application
#' @return shiny application
#' @export
run_RosyREDCap <- function(
    onStart = NULL,
    enableBookmarking = NULL,
    uiPattern = "/",
    # DB_short_name
    options = list(
      launch.browser = T
    ),
    ...
) {
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
