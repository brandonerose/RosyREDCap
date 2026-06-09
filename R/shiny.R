#' @noRd
dbSidebar <- function(...) {
  shinydashboardPlus::dashboardSidebar(
    minified = FALSE,
    collapsed = FALSE,
    TCD_SBH(),
    sidebarMenu(id = "sb1", ..., backend_menu_item()),
    TCD_SBF()
  )
}
#' @noRd
backend_menu_item <- function() {
  if (golem::app_prod())
    return(NULL)
  menuItem(text = "Backend",
           tabName = "backend",
           icon = shiny::icon("gear"))
}
#' @noRd
dbBody <- function(...) {
  dashboardBody(tabItems(..., tabItem("backend", fluidRow(
    box(
      title = h1("Input List"),
      width = 12L,
      mod_list_ui("input_list")
    ),
    box(
      title = h1("Values List"),
      width = 12L,
      mod_list_ui("values_list")
    )
  ))))
}
#' @noRd
dbHeader <- function(...) {
  shinydashboardPlus::dashboardHeader(title = tagList(
    span(class = "logo-lg", "RosyREDCap"),
    tags$a(
      href = "https://thecodingdocs.com",
      target = "_blank",
      tags$img(src = "www/logo.png", width = "100%")
    )
  ), ...)
}
#' @noRd
dbControlbar <- function(...) {
  shinydashboardPlus::dashboardControlbar(
    TCD_SBH(),
    div(style = "text-align:center", p(
      paste0("RosyREDCap", " Version: ", "pkg_version")
    )),
    div(style = "text-align:center", p(paste0(
      "Pkg Updated: ", "pkg_date"
    ))),
    ...,
    TCD_SBF(),
    fluidRow(column(
      12L,
      p("This app is still in development."),
      p("Consider donating for more."),
      p("Contact with issues."),
      p("Consider using R package."),
      align = "center"
    ))
  )
}
#' @noRd
mod_list_ui <- function(id) {
  ns <- NS(id)
  tagList(listviewer::jsoneditOutput(ns("values_list")))
}
#' @noRd
mod_list_server <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$values_list <- listviewer::renderJsonedit({
      if (!is_something(values)) {
        return(NULL)
      }
      x <- values |> shiny::reactiveValuesToList() |> listviewer::jsonedit()
      return(x)
    })
  })
}
#' @noRd
TCD_SBH <- function() {
  shinydashboard::sidebarMenu(shiny::div(
    style = "text-align:center",
    tags$a(
      href = "https://thecodingdocs.com",
      target = "_blank",
      tags$img(src = "www/logo.png", width = "50%")
    )
  ))
}
#' @noRd
TCD_SBF <- function() {
  shinydashboard::sidebarMenu(
    shinydashboard::menuItem(
      text = "Donate!",
      icon = shiny::icon("dollar"),
      href = "https://account.venmo.com/u/brandonerose"
    ),
    shinydashboard::menuItem(
      text = "TheCodingDocs.com",
      icon = shiny::icon("stethoscope"),
      href = "https://thecodingdocs.com"
    ),
    shinydashboard::menuItem(
      text = "GitHub Code",
      icon = shiny::icon("github"),
      href = "https://github.com/brandonerose/"
    ),
    shinydashboard::menuItem(
      text = "TheCodingDocs",
      icon = shiny::icon("twitter"),
      href = "https://twitter.com/TheCodingDocs"
    ),
    shinydashboard::menuItem(
      text = "BRoseMDMPH",
      icon = shiny::icon("twitter"),
      href = "https://twitter.com/BRoseMDMPH"
    ),
    shinydashboard::menuItem(
      text = "Brandon Rose, MD, MPH",
      icon = shiny::icon("user-doctor"),
      href = "https://www.thecodingdocs.com/founder"
    )
  )
  # p(paste0('Version: ',pkg_version)) |> shiny::div(style="text-align:center"),
  # p(paste0('Last Update: ',pkg_date)) |> shiny::div(style="text-align:center"),
}
#' @noRd
TCD_NF <- function() {
  shinydashboardPlus::dashboardFooter(left = shiny::div(
    fluidRow(
      shiny::actionButton(
        inputId = "ab1",
        label = "Donate!",
        icon = shiny::icon("dollar"),
        onclick = "window.open('https://account.venmo.com/u/brandonerose', '_blank')"
      ) ,
      shiny::actionButton(
        inputId = "ab2",
        label = "TheCodingDocs.com",
        icon = shiny::icon("stethoscope"),
        onclick = "window.open('https://thecodingdocs.com', '_blank')"
      ) ,
      shiny::actionButton(
        inputId = "ab3",
        label = "GitHub Code",
        icon = shiny::icon("github"),
        onclick = paste0(
          "window.open('https://github.com/brandonerose/",
          "RosyREDCap",
          "', '_blank')"
        )
      ),
      shiny::actionButton(
        inputId = "ab4",
        label = "TheCodingDocs",
        icon = shiny::icon("twitter"),
        onclick = "window.open('https://twitter.com/TheCodingDocs', '_blank')"
      ) ,
      shiny::actionButton(
        inputId = "ab5",
        label = "BRoseMDMPH",
        icon = shiny::icon("square-twitter"),
        onclick = "window.open('https://twitter.com/BRoseMDMPH', '_blank')"
      ) ,
      shiny::actionButton(
        inputId = "ab6",
        label = "Brandon Rose, MD, MPH",
        icon = shiny::icon("user-doctor"),
        onclick = "window.open('https://www.thecodingdocs.com/founder', '_blank')"
      )
    ),
    style = "text-align:center"
  ),
  right = NULL)
}
