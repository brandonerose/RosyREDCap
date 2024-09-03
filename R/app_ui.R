#' @import RosyUtils
#' @import shiny
#' @import shinydashboard
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui<- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    includeCSS(system.file(package="table1", "table1_defaults_1.0/table1_defaults.css")),
    # Your application UI logic
    shinydashboardPlus::dashboardPage(
      options = list(
        sidebarExpandOnHover = F
      ),
      header = dbHeader(),
      sidebar = dbSidebar(),
      body = dbBody(),
      controlbar = dbControlbar(),
      footer = TCD_NF(),
      skin = "black"
    )
  )
}
#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = .packageName
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
