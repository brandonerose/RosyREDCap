#' @import RosyUtils
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  output$REDCap_diagram <- DiagrammeR::renderDiagrammeR({REDCap_diagram(DB)})
}
