#' @import RosyUtils
#' @import RosyDB
#' @import RosyApp
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # values ------------
  values <- reactiveValues()
  values$projects <- get_projects() # get list of cached projects
  values$DB <- NULL
  # values$selected_record <- NULL
  values$last_clicked_record <- NULL
  values$selected_form <- NULL
  values$selected_variable <- NULL
  values$selected_instance <- NULL
  values$active_table_id <- NULL
  values$active_table_rows <- NULL
  values$listen_to_click <- NULL
  values$all_records <- NULL
  values$subset_records <- NULL
  values$sbc <- NULL
  values$user_adds_project <- NULL
  values$REDCap_diagram <- NULL
  # user input project -------
  observeEvent(input$user_adds_project_modal, {
    # display a modal dialog with a header, textinput and action buttons
    showModal(modalDialog(
      tags$h2('Please enter your Project Information'),
      textInput('user_adds_project_short_name', 'Short Name'),
      textInput('user_adds_project_api_token', 'API token'),
      textInput('user_adds_project_redcap_base',"Institutional REDCap Link", placeholder = "https://redcap.miami.edu/"),
      textInput('user_adds_project_merged_form_name',"Merged Form Name", placeholder = "merged"),
      #dir
      footer=tagList(
        actionButton('user_adds_project_submit', 'Submit'),
        modalButton('user_adds_project_cancel')
      )
    ))
  })
  # only store the information if the user clicks submit
  observeEvent(input$user_adds_project_submit, {
    removeModal()
    # values$user_adds_project_short_name <- input$name
    # l$state <- input$state
  })
  # setup_DB(
  #   short_name = OUT$short_name,
  #   dir_path = OUT$dir_path,
  #   token_name = OUT$token_name,
  #   redcap_base = "https://redcap.miami.edu/",
  #   force = T,
  #   merge_form_name = "merged"
  # )
  # diagrams ----------
  observe({
    output$REDCap_diagram_test_vis <- visNetwork::renderVisNetwork({
      REDCap_diagram(
        DB = values$DB,
        static = F,
        render = T,
        include_fields = input$metadata_graph_include_vars,
        duplicate_forms = input$metadata_graph_duplicate_forms,
        clean_names = input$metadata_graph_clean_name
      )
    })
    output$REDCap_diagram_test_dia <- DiagrammeR::renderGrViz({
      DiagrammeR::grViz(
        DiagrammeR::generate_dot(
          REDCap_diagram(
            DB = values$DB,
            static = T,
            render = F,
            include_fields = input$metadata_graph_include_vars,
            duplicate_forms = input$metadata_graph_duplicate_forms,
            clean_names = input$metadata_graph_clean_name
          )
        )
      )
    })
  })
  output$REDCap_diagram_ui_test <- renderUI({
    ext <- "REDCap_diagram_test_dia"
    OUT <- DiagrammeR::grVizOutput(ext)
    if(input$metadata_graph_type=="visNetwork"){
      ext <- "REDCap_diagram_test_vis"
      OUT <- visNetwork::visNetworkOutput(ext)
    }
    return(OUT)
  })
  # tables --------
  output$dt_tables_view <- renderUI({
    if (length(values$DB$data) == 0) {
      # If the list is empty, show a message
      return(h3("No tables available to display."))
    } else {
      tabsetPanel(
        id = "tabs",
        do.call(tabsetPanel, c(
          id = "tabs",
          lapply(seq_along(values$DB$data), function(i) {
            table_name_raw <- names(values$DB$data)[i]
            table_name <- values$DB$metadata$forms$form_label[which(values$DB$metadata$forms$form_name==table_name_raw)]
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
  output$forms_transformation <- DT::renderDT({
    cols <- which(colnames(values$editable_forms_transformation_table)%in%c("form_name","form_label","repeating","repeating_via_events"))
    values$editable_forms_transformation_table %>% make_DT_table(editable = list(target = 'cell', disable = list(columns = cols-1)),selection = 'none')
  })
  observeEvent(input$forms_transformation_cell_edit, {
    info <- input$forms_transformation_cell_edit
    message(info$value, " edited!")
    message(info$row, " row!")
    message(info$col, " col!")
    values$editable_forms_transformation_table[info$row, info$col+1] <- info$value # had to add +1 because not showing rownames
  })
  # simple tables ---------
  output$projects_table <- DT::renderDT({
    values$projects %>% make_DT_table()
  })
  output$forms_table <- DT::renderDT({
    values$DB$metadata$forms %>% make_DT_table()
  })
  output$metadata_table <- DT::renderDT({
    values$DB$metadata$fields %>% make_DT_table()
  })
  output$codebook_table <- DT::renderDT({
    values$DB$metadata$choices %>% make_DT_table()
  })
  output$user_table <- DT::renderDT({
    values$DB$redcap$users %>% make_DT_table()
  })
  output$log_table <- DT::renderDT({
    values$DB$redcap$log %>% make_DT_table()
  })
  # Render each DT table ------
  observe({
    if(!is_something(values$DB$data))return(h3("No tables available to display."))
    lapply(names(values$DB$data), function(TABLE) {
      table_data <- values$DB$data[[TABLE]]
      table_id <- paste0("table___home__", TABLE)
      output[[table_id]] <- DT::renderDT({
        table_data %>%
          clean_RC_df_for_DT(values$DB) %>%
          make_DT_table()
      })
    }) %>% return()
  })
  observe({
    if(!is_something(values$DB$data))return(h3("No tables available to display."))
    lapply(names(values$DB$data), function(TABLE) {
      table_data <- values$DB$data[[TABLE]]
      table_id <- paste0("table___home__", TABLE,"_exists")
      values[[table_id]] <- !is.null(input[[paste0("table___home__", TABLE,"_state")]])
    }) %>% return()
  })
  # html ---------------
  output$html_test <- renderUI({
    tags$iframe(
      class = "pubchem-widget",
      src=paste0("https://pubchem.ncbi.nlm.nih.gov/compound/2244#section=2D-Structure&embed=true"),
      style="width: 450px; max-width: 100%; height: 650px;"
    )
  })
  # vb -----------
  # output$vb_selected_record <- shinydashboard::renderValueBox({
  #   shinydashboard::valueBox(
  #     value = values$selected_record,
  #     subtitle = "Selected Patient (PSN)",
  #     width = 12
  #   )
  # })
  # observe ---------------
  # UI--------
  output$transformation_switch_ <- renderUI({
    shinyWidgets::switchInput(
      inputId = "transformation_switch",
      label = "Transformation",
      value = values$DB$internals$is_transformed
    )
  })
  output$choose_project_ <- renderUI({
    selectInput(
      inputId = "choose_project",
      label = "Choose Project",
      choices = NULL
    )
  })
  output$selected_record_ <- renderUI({
    selectInput(
      inputId = "selected_record",
      label = "Choose Record",
      selected = NULL,
      choices = NULL
    )
  })
  output$selected_group_ <- renderUI({
    selectInput(
      inputId = "selected_group",
      label = "Choose Group",
      selected = NULL,
      choices = NULL
    )
  })
  output$choose_split_ <- renderUI({
    selectInput(
      inputId = "choose_split",
      label = "Choose Split",
      selected = NULL,
      choices = NULL
    )
  })
  observeEvent(input$choose_project,{
    if(!is.null(input$choose_project)){
      if(is_something(input$choose_project)){
        values$DB <- tryCatch({
          load_RosyREDCap(short_name=input$choose_project) %>% clean_DB(drop_blanks = F,drop_unknowns = F)
        },error = function(e) {NULL})
        if(is_something(input$choose_project)){
          if(!is.null(input[[paste0("projects_table_state")]])){
            ROW <- which(values$projects == input$choose_project)
            skip <- F
            if(!is.null(input[[paste0("projects_table_rows_selected")]])){
              skip <- identical(ROW,input[[paste0("projects_rows_selected")]])
            }
            if(!skip){
              DT::selectRows(
                proxy = DT::dataTableProxy("projects_table", deferUntilFlush = F),
                selected = ROW
              )
            }
          }
        }
      }
    }
  })
  observe({
    updateSelectizeInput(session,"selected_record",choices = values$subset_records,server = T)
    message("updated selected_record choices")
  })
  observe({
    # if(!is.null(values$sbc)){
    #   if(nrow(values$sbc)>0){
    #     updateSelectizeInput(session,"selected_group",choices =values$sbc$label ,server = T)
    #     message("updated selected_record choices")
    #   }
    # }
  })
  observe({
    updateSelectizeInput(session,"choose_split",choices = values$subset_records,server = T)
    message("updated selected_record choices")
  })
  # observeEvent(values$last_clicked_record,{
  #   if(!is.null(values$last_clicked_record)){
  #     message("values$last_clicked_record changed!")
  #     updateSelectizeInput(session,"selected_record", selected = values$last_clicked_record,choices = values$subset_records,server = T)
  #   }
  # })
  observeEvent(input$transformation_switch,ignoreNULL = T,ignoreInit = T,{
    if(!is.null(values$DB)){
      message("triggered transformation_switch ",input$transformation_switch)
      if(!is.null(values$DB$transformation)){
        if(input$transformation_switch !=values$DB$internals$is_transformed){
          if(input$transformation_switch){
            values$DB <-transform_DB(values$DB)
          }
          if(!input$transformation_switch){
            values$DB <-untransform_DB(values$DB)
          }
        }
      }else{
        message("Nothing to do, no DB$transformation info! ",input$transformation_switch)
        shinyWidgets::updateSwitchInput(
          inputId = "transformation_switch",value = F, label = "Transformation"
        )
      }
    }
  })
  observeEvent(values$DB,{
    message("values$DB changed!")
    if(!is.null(values$DB)){
      values$subset_records <- values$all_records <- values$DB$summary$all_records[[values$DB$redcap$id_col]]
      updateSelectizeInput(session,"selected_record", selected = values$subset_records[1],choices = values$subset_records,server = T)
      # values$sbc <- sidebar_choices(values$DB)
      if(!is.null(values$DB$transformation)){
        values$editable_forms_transformation_table <- values$DB$transformation$forms %>% as.data.frame(stringsAsFactors = FALSE)
      }else{
        # values$editable_forms_transformation_table <- default_forms_transformation(values$DB) %>% as.data.frame(stringsAsFactors = FALSE)
      }
      if(!is.null(values$DB$transformation)){
        if(!is.null(values$DB$internals$is_transformed)){
          if(input$transformation_switch != values$DB$internals$is_transformed){
            shinyWidgets::updateSwitchInput(
              inputId = "transformation_switch",value = values$DB$internals$is_transformed, label = "Transformation"
            )
          }
        }
      }
    }
  })
  observe({
    selected <- input[["projects_table_rows_selected"]]
    # message("selected: ", selected)
    isolate({
      expected <- NULL
      data_col <- values$projects$short_name
      expected <- which(data_col==input$choose_project)
      # message("expected: ", expected)
      if(is_something(selected)){
        if(!identical(selected,expected)){
          selected <- unique(data_col[[selected]])
          message("valid_click: ", selected)
          updateSelectizeInput(session,"choose_project",selected = selected,choices = data_col,server = T)
        }
      }
    })
  })
  observe({
    if(!is.null(input$selected_record)){
      form_name <- "form_label"
      z <- values$DB$metadata$forms
      all_forms <- names(values$DB$data)
      values$DB$data %>% names() %>% lapply(function(form){
        values[[paste0("table___home__", form,"_exists")]]
      })
      values$selected_form <- z$form_name[which(z[[form_name]] == input$tabs)]
      isolate({
        if(is_something(values$selected_form)) {
          values$active_table_id <- paste0("table___home__", values$selected_form)
          starting_record <- input$selected_record
          data_form <- values$DB$data[[values$selected_form]]
          state <- input[[paste0(values$active_table_id, "_state")]]
          for(form in all_forms) {
            if(!is.null(input[[paste0("table___home__", form, "_state")]])){
              ROWS <- which(values$DB$data[[form]][[values$DB$redcap$id_col]] == input$selected_record)
              skip <- F
              if(!is.null(input[[paste0("table___home__", form, "_rows_selected")]])){
                # message("ident ",identical(ROWS,input[[paste0("table___home__", form, "_rows_selected")]]), " ", ROWS, " ", input[[paste0("table___home__", form, "_rows_selected")]])
                skip <- identical(ROWS,input[[paste0("table___home__", form, "_rows_selected")]])
              }
              if(!skip){
                message("triggered proxy Tabs: ", form, " Row ", ROWS)
                DT::selectRows(
                  proxy = DT::dataTableProxy(paste0("table___home__", form), deferUntilFlush = F),
                  selected = ROWS
                )
                if(length(ROWS)>0){
                  page <- as.integer(ROWS[[1]]/state$length)+1
                  message("triggered page: ", page)
                  DT::selectPage(
                    proxy = DT::dataTableProxy(paste0("table___home__", form), deferUntilFlush = F),
                    page = page
                  )
                }
              }
            }
          }
        }
      })
    }
  })
  observe({
    if(!is.null(values$active_table_id)){
      selected <- input[[paste0(values$active_table_id,"_rows_selected")]]
      # message("selected: ", selected)
      isolate({
        if(is_something(values$selected_form)){
          expected <- NULL
          data_col <- values$DB$data[[values$selected_form]][[values$DB$redcap$id_col]]
          expected <- which(data_col==input$selected_record)
          # message("expected: ", expected)
          if(is_something(selected)){
            if(!identical(selected,expected)){
              selected <- unique(data_col[[selected]])
              message("valid_click: ", selected)
              updateSelectizeInput(session,"selected_record",selected = selected,choices = values$subset_records,server = T)
            }
          }
        }
      })
    }
  })
  observe({
    if(!is.null(values$projects)){
      updateSelectizeInput(session,"choose_project" ,choices = values$projects$short_name,server = T)
    }
  })
  observe({
    # updateSelectizeInput(session,"selected_record" ,selected = input$selected_record)
    # values$variables_to_change_input_list <- NULL
    # if(is_something(input$selected_record)){
    #   if(input$selected_record %in% values$DB$summary$all_records[[values$DB$redcap$id_col]]){
    #     values$variables_to_change_input_list <- values$DB %>%
    #       filter_DB(
    #         records = input$selected_record,
    #         data_choice = "data",
    #         form_names = field_names_to_form_names(values$DB,field_names = values$selected_variable)
    #       ) %>% process_df_list()
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
    random_record <- values$subset_records %>% sample1()
    message("Random Record: ", random_record)
    updateSelectizeInput(session,"selected_record",selected = random_record,choices = values$subset_records,server = T)
  })
  observeEvent(input$ab_update_redcap,{
    values$DB <- values$DB %>% update_RosyREDCap()
  })
  observeEvent(input$ab_accept_form_transform,{
    # values$DB$transformation$forms <- values$editable_forms_transformation_table # add check
    if(identical(values$DB$transformation$forms,values$editable_forms_transformation_table)){
      message("values$editable_forms_transformation_table didnt change!")
    }else{
      message("would have accepted values$editable_forms_transformation_table!")
      print.table(values$editable_forms_transformation_table)
    }
  })
  # redcap links -----
  output$redcap_links <- renderUI({
    if(is_something(input$selected_record)&length(input$selected_record)>0){
      IF <- shinydashboard::menuItem(
        text=paste0("REDCap Record (",input$selected_record,")"),
        icon = shiny::icon("file-lines"),
        href=paste0(values$DB$links$redcap_base,"redcap_v",values$DB$redcap$version,"/DataEntry/record_home.php?pid=",values$DB$redcap$project_id,"&arm=1&id=",input$selected_record)
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
  if(golem::app_dev()){
    mod_list_server("input_list",values = input)
    mod_list_server("values_list",values = values)
  }
}
