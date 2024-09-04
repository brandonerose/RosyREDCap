#' @import RosyUtils
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  output$REDCap_diagram_vis <- DiagrammeR::renderGrViz({
    DiagrammeR::grViz(DiagrammeR::generate_dot(REDCap_diagram(DB, type = "DiagrammeR",render = F)))
  })
  output$REDCap_diagram_diag <- visNetwork::renderVisNetwork({
    REDCap_diagram(DB, type = "visNetwork",render = T)
  })
}
