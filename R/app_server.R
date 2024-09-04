#' @import RosyUtils
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  values <- reactiveValues()
  values$DB <- DB
  values$selected_record <- NULL
  output$REDCap_diagram_diag <- DiagrammeR::renderGrViz({
    DiagrammeR::grViz(DiagrammeR::generate_dot(REDCap_diagram(values$DB, type = "DiagrammeR",render = F)))
  })
  output$REDCap_diagram_vis <- visNetwork::renderVisNetwork({
    REDCap_diagram(values$DB, type = "visNetwork",render = T)
  })
  #tables --------
  output$dt_tables_view <- renderUI({
    if (length(values$DB$data_extract) == 0) {
      # If the list is empty, show a message
      return(h3("No tables available to display."))
    } else {
      tabsetPanel(
        id = "tabs",
        do.call(tabsetPanel, c(
          id = "tabs",
          lapply(seq_along(values$DB$data_extract), function(i) {
            table_name <- names(values$DB$data_extract)[i]
            table_name <- values$DB$redcap$instruments$instrument_label[which(values$DB$redcap$instruments$instrument_name==table_name)]
            table_id <- paste0("table__", i)
            tabPanel(
              title = table_name,
              DT::DTOutput(table_id)
            )
          })
        ))
      )
    }
  })
  # Render each DT table
  observe({
    if(!is_something(values$DB$data_extract))return(h3("No tables available to display."))
    lapply(seq_along(values$DB$data_extract), function(i) {
      table_data <- values$DB$data_extract[[i]]
      table_id <- paste0("table__", i)
      output[[table_id]] <- DT::renderDT({table_data %>% make_DT_table(DB = values$DB)})
    }) %>% return()
  })
  #observe --------
  output$vb_selected_record <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = values$selected_record,
      subtitle = "Selected Patient (PSN)",
      width = 12
    )
  })
}
