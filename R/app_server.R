#' @import RosyUtils
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  #values ------------
  values <- reactiveValues()
  values$projects <- get_projects() # get list of cached projects
  values$project <- NULL
  values$DB <- NULL
  values$selected_record <- NULL
  values$last_clicked_record <- NULL
  values$selected_form <- NULL
  values$selected_variable <- NULL
  values$selected_instance <- NULL
  values$active_table_id <- NULL
  values$active_table_rows <- NULL
  values$last_clicked_tab <- NULL
  values$all_records <- NULL
  values$subset_records <- NULL
  # diagrams ----------
  observe({
    if(input$metadata_graph_type == "visNetwork"){
      output$REDCap_diagram <- visNetwork::renderVisNetwork({
        REDCap_diagram(
          values$DB,
          type = input$metadata_graph_type,
          render = T,
          include_vars = input$metadata_graph_include_vars,
          duplicate_forms = input$metadata_graph_duplicate_forms,
          clean_name = input$metadata_graph_clean_name
        )
      })
      output$REDCap_diagram_ui <- renderUI({
        visNetwork::visNetworkOutput("REDCap_diagram")
      })
    }
    if(input$metadata_graph_type == "DiagrammeR"){
      output$REDCap_diagram <- DiagrammeR::renderGrViz({
        DiagrammeR::grViz(DiagrammeR::generate_dot(
          REDCap_diagram(
            values$DB,
            type = input$metadata_graph_type,
            render = F,
            include_vars = input$metadata_graph_include_vars,
            duplicate_forms = input$metadata_graph_duplicate_forms,
            clean_name = input$metadata_graph_clean_name
          )
        )
        )
      })
      output$REDCap_diagram_ui <- renderUI({
        DiagrammeR::grVizOutput("REDCap_diagram")
      })
    }
  })
  #tables --------
  output$dt_tables_view <- renderUI({
    if (length(values$DB[[input$data_choice]]) == 0) {
      # If the list is empty, show a message
      return(h3("No tables available to display."))
    } else {
      tabsetPanel(
        id = "tabs",
        do.call(tabsetPanel, c(
          id = "tabs",
          lapply(seq_along(values$DB[[input$data_choice]]), function(i) {
            table_name_raw <- names(values$DB[[input$data_choice]])[i]
            if(input$data_choice == "data_transform"){
              table_name <- table_name_raw
            }else{
              table_name <- values$DB$redcap$instruments$instrument_label[which(values$DB$redcap$instruments$instrument_name==table_name_raw)]
            }
            table_id <- paste0("table___home__", table_name_raw)
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
    if(!is_something(values$DB[[input$data_choice]]))return(h3("No tables available to display."))
    lapply(names(values$DB[[input$data_choice]]), function(TABLE) {
      table_data <- values$DB[[input$data_choice]][[TABLE]]
      table_id <- paste0("table___home__", TABLE)
      output[[table_id]] <- DT::renderDT({table_data %>% make_DT_table(DB = values$DB)})
    }) %>% return()
  })
  #vb -----------
  # output$vb_selected_record <- shinydashboard::renderValueBox({
  #   shinydashboard::valueBox(
  #     value = values$selected_record,
  #     subtitle = "Selected Patient (PSN)",
  #     width = 12
  #   )
  # })
  # observe ---------------
  observe({
    x <- "redcap"
    y <- "instruments"
    a <- "instrument_label"
    if(input$data_choice == "data_transform") {
      x <- "remap"
      y <- "instruments_new"
      a <- "instrument_name"
    }
    z <- values$DB[[x]][[y]]
    all_forms <- names(values$DB[[input$data_choice]])
    values$selected_form <- z$instrument_name[which(z[[a]] == input$tabs)]
    values$active_table_id <- paste0("table___home__", values$selected_form)
    message("Changed Tabs: ", input$tabs)
    # Track previous tab and reset `last_clicked_record` when switching tabs
    if(is_something(values$selected_form)) {
      overwrite <- F
      selection_update <- T
      # input[[paste0("table___home__", values$selected_form, "_state")]]
      ROW_LC <- input[[paste0(values$active_table_id, "_row_last_clicked")]]
      isolate({
        if(is_something(ROW_LC)) {
          clicked_record <- values$DB[[input$data_choice]][[values$selected_form]][[values$DB$redcap$id_col]][[ROW_LC]]
          overwrite <- TRUE
          message("selected_record: ", values$selected_record)
          message("clicked_record: ", clicked_record)
          message("last_click_record: ", values$last_clicked_record)
          message("last_clicked_tab: ", values$last_clicked_tab)
          message("tabs: ", input$tabs)
          # Only update if there is a new click (ignore old clicks on tab switch)
          if(!is.null(values$selected_record)) {
            if(!is.null(values$last_clicked_tab)) {
              if(values$last_clicked_tab != input$tabs) {
                clicked_record <- values$last_clicked_record
                message("last_clicked_record overwriting clicked")
                values$last_clicked_tab <- input$tabs   # Update the previous tab tracker
              }
            }
            overwrite <- clicked_record != values$selected_record
          }
          if(overwrite) {
            values$selected_record <- values$last_clicked_record <- clicked_record
            values$last_clicked_tab <- input$tabs   # Update the previous tab tracker
            message("overwrite happened!")
            message("selected_record: ", values$selected_record)
            message("clicked_record: ", clicked_record)
            message("last_click_record: ", values$last_clicked_record)
            message("last_clicked_tab: ", values$last_clicked_tab)
            message("tabs: ", input$tabs)
          }
        }
      })
    }
  })
  observe({
    x <- "redcap"
    y <- "instruments"
    a <- "instrument_label"
    if(input$data_choice == "data_transform") {
      x <- "remap"
      y <- "instruments_new"
      a <- "instrument_name"
    }
    z <- values$DB[[x]][[y]]
    all_forms <- names(values$DB[[input$data_choice]])
    values$selected_form <- z$instrument_name[which(z[[a]] == input$tabs)]
    values$active_table_id <- paste0("table___home__", values$selected_form)
    # Track previous tab and reset `last_clicked_record` when switching tabs
    if(is_something(values$selected_form)) {
      if(is_something(values$selected_record)) {
        if(!is.null(input[[paste0("table___home__", values$selected_form, "_state")]])){
          isolate({
            original_tab <- values$last_clicked_tab
            values$last_clicked_tab <- input$tabs   # Update the previous tab tracker
            values$last_clicked_record <- values$selected_record
            for(form in all_forms) {
              if(!is.null(input[[paste0("table___home__", form, "_state")]])){
                ROWS <- which(values$DB[[input$data_choice]][[form]][[values$DB$redcap$id_col]] == values$selected_record)
                PREVIOUS <- input[[paste0(values$active_table_id, "_rows_selected")]]
                run_it <- T
                # if(!is.null(PREVIOUS)){
                #   if(length(PREVIOUS)!=length(ROWS)){
                #     run_it <- T
                #   }else{
                #     run_it <- any(ROWS != PREVIOUS)
                #   }
                # }
                message("Run it? ",run_it," rows = ",ROWS %>% paste0(collapse = ", ")," PREVIOUS = ",PREVIOUS %>% paste0(collapse = ", "))
                if(run_it){
                  proxy <- DT::dataTableProxy(paste0("table___home__", form), deferUntilFlush = FALSE)
                  message("triggered proxy Tabs: ", form, " Row ", ROWS)
                  DT::selectRows(proxy = proxy, selected = ROWS)
                }
              }
            }
          })
        }
      }
    }
  })
  # UI--------
  output$choose_project <- renderUI({
    selectInput(
      inputId = "choose_project_",
      label = "Choose Project",
      choices = NULL
    )
  })
  output$choose_indiv_record <- renderUI({
    selectInput(
      inputId = "choose_indiv_record_",
      label = "Choose Record",
      choices = NULL
    )
  })
  observeEvent(input$choose_project_,{
    if(!is.null(input$choose_project_)){
      ROWS <- which(values$projects$short_name==input$choose_project_)
      if(is_something(ROWS)){
        values$DB <- tryCatch({
          load_DB(values$projects$dir_path[ROWS])
        },error = function(e) {NULL})
      }
      print("choose_indiv_record_ triggered update choose indiv")
    }
  })
  observeEvent(values$DB,{
    if(!is.null(values$DB)){
      values$subset_records <- values$all_records <- values$DB$summary$all_records[[values$DB$redcap$id_col]]
      updateSelectizeInput(session,"choose_indiv_record_" ,selected = NULL,choices = values$subset_records,server = T)
    }
  })
  observeEvent(input$choose_indiv_record_,{
    if(!is.null(input$choose_indiv_record_)){
      values$selected_record <- input$choose_indiv_record_
      print("choose_indiv_record_ triggered update choose indiv")
    }
  })
  observe({
    if(!is.null(values$projects)){
      updateSelectizeInput(session,"choose_project_" ,choices = values$projects$short_name,server = T)
    }
  })
  observeEvent(values$selected_record,{
    if(!is.null(values$selected_record)){
      updateSelectizeInput(session,"choose_indiv_record_" ,selected = values$selected_record)
      print("selected_record triggered update choose indiv")
    }
    # values$variables_to_change_input_list <- NULL
    # if(is_something(values$selected_record)){
    #   if(values$selected_record %in% values$DB$summary$all_records[[values$DB$redcap$id_col]]){
    #     values$variables_to_change_input_list <- values$DB %>%
    #       filter_DB(
    #         records = values$selected_record,
    #         data_choice = "data_extract",
    #         form_names = RosyREDCap:::field_names_to_instruments(values$DB,field_names = values$selected_variable)
    #       ) %>% RosyUtils:::process_df_list()
    #     if(!is_something(values$variables_to_change_input_list)){
    #       values$variables_to_change_input_list <- NULL
    #       # return(h3("No Items available to display."))
    #     }
    #     if(!is_something(values$variables_to_change_input_list[[values$selected_form]])){
    #       values$variables_to_change_input_list <- NULL
    #       # return(h3("No Items available to display."))
    #     }
    #     DF <- values$variables_to_change_input_list[[values$selected_form]]
    #     dynamic_input_ids <- NULL
    #     if(is_something(DF)){
    #       dynamic_input_ids <- paste0("input_dynamic_", seq_len(nrow(DF)))
    #     }
    #     values$dynamic_input_ids <- dynamic_input_ids
    #   }
    # }
  })
  # ab----------
  observeEvent(input$ab_random_record,{
    random_record <- values$DB$summary$all_records[[DB$redcap$id_col]][1:10] %>% sample(1)
    message("Random Record: ", random_record)
    values$selected_record <- random_record
    # input$patient_table_row_last_clicked <- which(values$DB$data_transform[[values$DB$internals$merge_form_name]]$record_id==values$selected_record)
  })
  # redcap links -----
  output$redcap_links <- renderUI({
    if(is_something(values$selected_record)&length(values$selected_record)>0){
      IF <- shinydashboard::menuItem(
        text=paste0("REDCap Record (",values$selected_record,")"),
        icon = shiny::icon("file-lines"),
        href=paste0(values$DB$links$redcap_base,"redcap_v",values$DB$redcap$version,"/DataEntry/record_home.php?pid=",values$DB$redcap$project_id,"&arm=1&id=",values$selected_record)
      )
    }else{
      IF <- NULL
    }
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem(
        text="REDCap Home",
        icon = shiny::icon("home"),
        href=values$DB$links$redcap_home
      ),
      shinydashboard::menuItem(
        text="REDCap Records",
        icon = shiny::icon("database"),
        href=values$DB$links$redcap_record_home
      ),
      IF,
      shinydashboard::menuItem(
        text="REDCap API",
        icon = shiny::icon("key"),
        href=values$DB$links$redcap_API
      ),
      shinydashboard::menuItem(
        text="REDCap API PG",
        icon = shiny::icon("laptop-code"),
        href=values$DB$links$redcap_API_playground
      )
    )
  })
  # html listviewer ----------------
  output$values_list <- listviewer::renderJsonedit({
    x<- values %>% reactiveValuesToList()
    x[["DB"]] <- NULL
    x %>% listviewer::jsonedit() %>% return()
  })
  output$input_list <- listviewer::renderJsonedit({
    input %>% reactiveValuesToList() %>% listviewer::jsonedit()
  })
  # output$output_list <- renderPrint({
  #   names(output$ns)  # Print the structure of the output object
  # })
}
