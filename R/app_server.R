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
  values$selected_form <- NULL
  values$selected_field <- NULL
  values$selected_instance <- NULL
  values$active_table_id <- NULL
  values$all_records <- NULL
  values$subset_records <- NULL
  values$subset_list <- NULL
  values$sbc <- NULL
  values$user_adds_project <- NULL
  values$REDCap_diagram <- NULL
  values$dt_tables_view_list <- NULL
  values$fields_to_change_input_df <- NULL
  values$dynamic_input_ids <- NULL
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
    if(length(values$subset_list) == 0) return(h3("No tables available to display."))
    do.call(tabsetPanel, c(
      id = "tabs",
      lapply(seq_along(values$subset_list), function(i) {
        table_name_raw <- names(values$subset_list)[i]
        table_id <- paste0("table___home__", table_name_raw)
        tabPanel(
          title = table_name_raw %>% form_names_to_form_labels(values$DB),
          DT::DTOutput(table_id)
        )
      })
    )
    )
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
  output$table1 <- renderUI({
    if(is_something(input$choose_form)){
      variables <- unique(
        c(
          ifelse(input$choose_split=="no_choice",NA,input$choose_split),
          input$choose_fields_cat
        ) %>% drop_nas() %>% drop_if("")
      )
      if(length(variables)==0)return()
      DF <- values$subset_list[[input$choose_form]][,variables,drop = F]
      if(is_something(DF)){
        message("input$choose_split: ",input$choose_split)
        message("variables: ",variables %>% as_comma_string())
        # DF %>% head() %>% print()
        html_output <- htmlTable::htmlTable(
          align = "l",
          DF %>% clean_DF(values$DB$metadata$fields,other_drops = other_drops(ignore = input$render_missing)) %>% make_table1(
            group = input$choose_split,
            variables = variables,
            render.missing = input$render_missing
          ),
          css.cell = "width:100%; overflow-x:auto;"  # Ensures width and adds horizontal overflow
        )
        tags$div(
          style = "width:100%; overflow-x:auto;",  # Force containment within the box
          HTML(html_output)
        )
      }
    }
  })
  # dt_tables_view-----------
  # Create a reactive list of DT tables
  output$dt_tables_view_records <- renderUI({
    if (length(values$dt_tables_view_list) == 0) {
      # If the list is empty, show a message
      return(h3("No tables available to display."))
    } else {
      if(input$view_switch_text){
        DF_list_to_text(DF_list = values$dt_tables_view_list, values$DB) %>% HTML()
      }else{
        # Otherwise, generate the list of tables
        lapply(seq_along(values$dt_tables_view_list), function(i) {
          table_name <- names(values$dt_tables_view_list)[i]
          table_id <- paste0("table__dt_view_", i)
          # Create DTOutput for each table
          tagList(
            h3(paste("Table:", table_name)),
            DT::DTOutput(table_id)
          )
        })
      }
    }
  })
  # Render each DT table
  observe({
    if (is_something(input$choose_record)&&is_something(input$choose_fields_view)) {
      if(is_something(values$DB)){
        values$dt_tables_view_list <- values$DB %>% filter_DB(
          filter_field = values$DB$redcap$id_col,
          filter_choices = input$choose_record,
          form_names = field_names_to_form_names(values$DB, field_names = input$choose_fields_view),
          field_names = input$choose_fields_view,
          no_duplicate_cols = T
        ) %>% process_df_list()
        # print(values$dt_tables_view_list)
        # values$dt_tables_view_list <- DB %>% filter_DB(records = subset_list$sarcoma$record_id %>% sample1(), data_choice = get_default_data_choice(values$DB),field_names = "sarc_timeline") %>% process_df_list()
        # values$subset_list$sarcoma %>% dplyr::filter(sarcoma_id%in%values$chosen_group_sarcoma) %>% make_PSDB_table(DB = values$DB)
        if(!is_something(values$dt_tables_view_list))return(h3("No tables available to display."))
        lapply(seq_along(values$dt_tables_view_list), function(i) {
          table_data <- values$dt_tables_view_list[[i]]
          table_id <- paste0("table__dt_view_", i)
          output[[table_id]] <- DT::renderDT({
            table_data %>% clean_DF(fields = values$DB$metadata$fields,other_drops = other_drops(ignore = input$render_missing)) %>% make_DT_table()
          })
        }) %>% return()
      }
    }
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
  output$the_uploading_table <- DT::renderDT({
    values$fields_to_change_input_df %>% make_DT_table()
  })
  # Render each DT table ------
  observe({
    if(!is_something(values$subset_list))return(h3("No tables available to display."))
    lapply(names(values$subset_list), function(TABLE) {
      table_data <- values$subset_list[[TABLE]]
      table_id <- paste0("table___home__", TABLE)
      output[[table_id]] <- DT::renderDT({
        table_data %>%
          # clean_RC_df_for_DT(values$DB) %>%
          make_DT_table()
      })
    }) %>% return()
  })
  observe({
    if(!is_something(values$subset_list))return(h3("No tables available to display."))
    lapply(names(values$subset_list), function(TABLE) {
      table_data <- values$subset_list[[TABLE]]
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
  # output$vb_choose_record <- shinydashboard::renderValueBox({
  #   shinydashboard::valueBox(
  #     value = values$choose_record,
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
  output$filter_switch_ <- renderUI({
    if(is_something(input$choose_group)){
      if (input$choose_group != "All Records") {
        shinyWidgets::switchInput(
          inputId = "filter_switch",
          onLabel = "Strict",
          offLabel = "Records",
          value = T
        )
      }
    }
  })
  output$choose_project_ <- renderUI({
    selectizeInput(
      inputId = "choose_project",
      label = "Choose Project",
      choices = NULL
    )
  })
  output$choose_record_ <- renderUI({
    selectizeInput(
      inputId = "choose_record",
      label = "Choose Record",
      selected = NULL,
      choices = NULL
    )
  })
  output$choose_form_ <- renderUI({
    selectizeInput(
      inputId = "choose_form",
      label = "Choose Form",
      selected = NULL,
      choices = NULL
    )
  })
  output$choose_field_ <- renderUI({
    selectizeInput(
      inputId = "choose_field",
      label = "Choose Field",
      multiple = F,
      selected = NULL,
      choices = NULL
    )
  })
  output$choose_fields_cat_ <- renderUI({
    selectizeInput(
      inputId = "choose_fields_cat",
      label = "Choose Fields",
      multiple = T,
      choices = NULL
    )
  })
  output$choose_fields_view_ <- renderUI({
    selectizeInput(
      inputId = "choose_fields_view",
      label = "Choose Fields",
      multiple = T,
      choices = NULL
    )
  })
  output$choose_fields_change_ <- renderUI({
    selectizeInput(
      inputId = "choose_fields_change",
      label = "Choose Field",
      multiple = T,
      choices = NULL
    )
  })
  output$choose_group_ <- renderUI({
    selectizeInput(
      inputId = "choose_group",
      label = "Choose Group",
      multiple = input$allow_multiple_groups,
      selected = NULL,
      choices = NULL
    )
  })
  output$choose_split_ <- renderUI({
    selectizeInput(
      inputId = "choose_split",
      label = "Choose Split",
      selected = NULL,
      choices = NULL
    )
  })
  observeEvent(values$DB,{
    message("values$DB changed!")
    if(is_something(values$DB)){
      values$selected_form <- NULL
      values$selected_field <- NULL
      values$selected_instance <- NULL
      values$active_table_id <- NULL
      values$all_records <- NULL
      values$subset_records <- NULL
      values$subset_list <- NULL
      values$sbc <- NULL
      values$fields_to_change_input_df <- NULL
      values$dynamic_input_ids <- NULL
      values$subset_records <- values$all_records <- values$DB$summary$all_records[[values$DB$redcap$id_col]]
      values$subset_list <- values$DB$data
      updateSelectizeInput(
        session,
        "choose_record",
        selected = values$subset_records[1],
        choices = values$subset_records,
        server = T
      )
      values$sbc <- sidebar_choices(values$DB)
      if(!is.null(values$DB$transformation)){
        values$editable_forms_transformation_table <- values$DB$transformation$forms %>% as.data.frame(stringsAsFactors = FALSE)
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
      field_names <- values$sbc$field_name %>% unique() %>% vec1_in_vec2(
        values$DB$metadata$fields$field_name[which(values$DB$metadata$fields$field_type_R %in% c("factor", "integer", "numeric"))]
      )
      group_choices <- c(
        "All Records",
        # "Custom Records",
        values$sbc$label[which(values$sbc$field_name %in% field_names)]
      )
      updateSelectizeInput(
        session,"choose_group",
        choices = group_choices,
        server = T
      )
      updateSelectizeInput(
        session = session,
        inputId = "choose_form",
        choices = stats::setNames(
          object =values$DB$metadata$forms$form_name,
          nm = values$DB$metadata$forms$form_label
        )
      )
    }
  })
  observeEvent(input$choose_project,{
    if(!is.null(input$choose_project)){
      if(is_something(input$choose_project)){
        values$DB <- tryCatch({
          load_DB(short_name=input$choose_project) #%>% clean_DB(drop_blanks = F,other_drops = NULL)
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
  observeEvent(input$choose_group,{
    if(is_something(input$choose_group)){
      if(length(input$choose_group) == 1){
        if(input$choose_group == "All Records"){
          values$subset_records <- values$all_records
          values$subset_list <- values$DB$data
        } else {
          # if(input$choose_group == "Custom Records"){
          #   values$subset_records <- values$all_records
          #   values$subset_list <- values$DB$data
          # }
          # if(!input$choose_group %in% c("All Records","Custom Records")){
          x<- values$sbc[which(values$sbc$label==input$choose_group),]
          if(nrow(x)>0){
            DF <- values$DB$data[[x$form_name]]
            filter_field <- values$DB$redcap$id_col
            values$subset_records <- filter_choices <- DF[[values$DB$redcap$id_col]][which(DF[[x$field_name]]==x$name)] %>% unique()
            if(is_something(input$filter_switch)){
              if(input$filter_switch){
                filter_field <- x$field_name
                filter_choices <- x$name
              }
            }
            print(filter_field)
            print(filter_choices)
            values$subset_list <- filter_DB(
              DB = values$DB,
              filter_field = filter_field,
              filter_choices = filter_choices
              # form_names = values$selected_form,
              # field_names = input$choose_fields_cat
            )
          }
        }
      }
      if(length(input$choose_group) > 1){
        # if(input$choose_group == "Custom Records"){
        #   values$subset_records <- values$all_records
        #   values$subset_list <- values$DB$data
        # }
        # if(!input$choose_group %in% c("All Records","Custom Records")){
        x<- values$sbc[which(values$sbc$label%in%input$choose_group),]
        # add observe to remove all records if new is selected
        # if(nrow(x)>0){
        #   DF <- values$DB$data[[x$form_name]]
        #   filter_field <- DB$redcap$id_col
        #   values$subset_records <- filter_choices <- DF[[values$DB$redcap$id_col]][which(DF[[x$field_name]]==x$name)] %>% unique()
        #   if(is_something(input$filter_switch)){
        #     if(input$filter_switch){
        #       filter_field <- x$field_name
        #       filter_choices <- x$name
        #     }
        #   }
        #   print(filter_field)
        #   print(filter_choices)
        #   values$subset_list <- filter_DB(
        #     DB = values$DB,
        #     filter_field = filter_field,
        #     filter_choices = filter_choices
        #     # form_names = values$selected_form,
        #     # field_names = input$choose_fields_cat
        #   )
        # }
      }
    }
  })
  debounced_tabs <- debounce(reactive(input$tabs), 250)  # 250ms delay
  observeEvent(debounced_tabs(), {
    updateSelectizeInput(session, "choose_form", selected = input$tabs %>% form_labels_to_form_names(values$DB))
  }, ignoreInit = TRUE)
  observeEvent(values$subset_records,{
    message("values$subset_records changed!")
    selected <- NULL
    if(is_something(input$choose_record)){
      if(!input$choose_record %in% values$subset_records){
        if(is_something(values$subset_records)){
          selected <- values$subset_records[1]
        }
      }
    }
    updateSelectizeInput(
      session,
      "choose_record",
      selected = selected,
      choices = values$subset_records,
      server = T
    )
  })
  # Update the tabset panel when a new tab is selected in the selectInput
  observe({
    if(is_something(values$DB)){
      if(input$sb1 %in% c("group","record")){
        message("input$sb1:",input$sb1)
        if(is_something(input$choose_form)){
          updateTabsetPanel(session, "tabs", selected = input$choose_form %>% form_names_to_form_labels(values$DB))
        }
        if(is_something(values$subset_list)){
          DF <- values$DB$metadata$fields
          field_names_view <- DF$field_name[which(!DF$field_type %in% c("description"))]
          field_names_cat <- DF$field_name[which(DF$field_type_R %in% c("factor", "integer", "numeric"))]
          field_names_cat <- colnames(values$subset_list[[input$choose_form]]) %>% vec1_in_vec2(field_names_cat)
          field_names_view <- colnames(values$subset_list[[input$choose_form]]) %>% vec1_in_vec2(field_names_view)
          field_labels_cat <- field_names_cat %>% field_names_to_field_labels(values$DB)
          field_labels_view <- field_names_view %>% field_names_to_field_labels(values$DB)
          field_names_change <- DF$field_name[which(
            (!DF$field_type %in% c("description","file")) &
              DF$in_original_redcap &
              DF$form_name == input$choose_form &
              DF$field_name != values$DB$redcap$id_col
          )]
          field_labels_change <- field_names_change %>% field_names_to_field_labels(values$DB)
          if(is_something(field_names_cat)){
            field_choices_cat <- c(
              stats::setNames("no_choice","None"),
              stats::setNames(field_names_cat,field_labels_cat)
            )
            selected <- "no_choice"
            if(is_something(input$choose_split)){
              if(input$choose_split %in% field_names_cat)selected <- input$choose_split
            }
            updateSelectizeInput(
              session = session,
              inputId = "choose_split",
              selected = selected,
              choices = field_choices_cat
            )
            selected <- NULL
            if(is_something(input$choose_fields_cat)){
              if(all(input$choose_fields_cat %in% field_names_cat))selected <- input$choose_fields_cat
            }
            updateSelectizeInput(
              session = session,
              inputId = "choose_fields_cat",
              selected = selected,
              choices = field_choices_cat
            )
          }
          if(is_something(field_names_view)){
            selected <- NULL
            if(is_something(input$choose_fields_view)){
              if(all(input$choose_fields_view %in% field_names_view))selected <- input$choose_fields_view
            }
            updateSelectizeInput(
              session = session,
              inputId = "choose_fields_view",
              selected = selected,
              choices = stats::setNames(field_names_view,field_labels_view)
            )
          }
          if(is_something(field_names_change)){
            selected <- NULL
            if(is_something(input$choose_fields_change)){
              if(all(input$choose_fields_change %in% field_names_change))selected <- input$choose_fields_change
            }
            updateSelectizeInput(
              session = session,
              inputId = "choose_fields_change",
              selected = selected,
              choices =  stats::setNames(field_names_change,field_labels_change)
            )
          }
        }
      }
    }
  })
  # observeEvent(input$choose_form,{
  #   selected <-  input$choose_form %>% form_names_to_form_labels(values$DB)
  #   if(is_something(selected)){
  #     # if(!identical(selected,input$tabs)){
  #     message("updating form2: ",selected)
  #     updateTabsetPanel(
  #       session = session,
  #       inputId = "tabs",
  #       selected = selected
  #     )
  #     # }
  #   }
  # })
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
    if(is_something(input$choose_record)){
      all_forms <- names(values$subset_list)
      values$subset_list %>% names() %>% lapply(function(form){
        values[[paste0("table___home__", form,"_exists")]]
      })
      isolate({
        message("input$tabs : ", input$tabs )
        values$selected_form <- input$tabs %>% form_labels_to_form_names(values$DB)
        message("values$selected_form: ",values$selected_form)
        if(is_something(values$selected_form)) {
          values$active_table_id <- paste0("table___home__", values$selected_form)
          message("values$active_table_id: ",values$active_table_id)
          starting_record <- input$choose_record
          message("values$selected_form: ",values$selected_form)
          state_length <- input[[paste0(values$active_table_id, "_state")]]$length
          message("state_length: ",state_length)
          for(form in all_forms) {
            if(!is.null(input[[paste0("table___home__", form, "_state")]])){
              message("form: ",form)
              ROWS <- which(values$subset_list[[form]][[values$DB$redcap$id_col]] == input$choose_record)
              message("ROWS: ",ROWS %>% as_comma_string())
              skip <- F
              if(!is.null(input[[paste0("table___home__", form, "_rows_selected")]])){
                message("ident ",identical(ROWS,input[[paste0("table___home__", form, "_rows_selected")]]), " ", ROWS, " ", input[[paste0("table___home__", form, "_rows_selected")]])
                skip <- identical(ROWS,input[[paste0("table___home__", form, "_rows_selected")]])
                message("SKIP: ",skip)
              }
              if(!skip){
                message("triggered proxy Tabs: ", form, " Row ", ROWS)
                DT::selectRows(
                  proxy = DT::dataTableProxy(paste0("table___home__", form), deferUntilFlush = F),
                  selected = ROWS
                )
                if(length(ROWS)>0){
                  page <- as.integer(ROWS[[1]]/state_length)+1
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
          data_col <- values$subset_list[[values$selected_form]][[values$DB$redcap$id_col]]
          expected <- which(data_col==input$choose_record)
          # message("expected: ", expected)
          if(is_something(selected)){
            if(!identical(selected,expected)){
              selected <- unique(data_col[[selected]])
              message("valid_click: ", selected)
              updateSelectizeInput(
                session,
                "choose_record",
                selected = selected,
                choices = values$subset_records,
                server = T
              )
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
  # record_changes ---------
  observe({
    running_it <- (
      is_something(input$choose_record) &&
        is_something(input$choose_fields_change) &&
        is_something(input$choose_form)
    )
    input$reset_data_values
    isolate({
      values$fields_to_change_input_df <- NULL
      values$dynamic_input_ids <- NULL
      if(running_it){
        vars <- unique(
          c(
            values$DB$metadata$form_key_cols[[input$choose_form]],
            input$choose_fields_change
          )
        )
        DF <- NULL
        if(is_something(input$choose_form)){
          DF <- values$subset_list[[input$choose_form]]
          rows <-  which(DF[[values$DB$redcap$id_col]]==input$choose_record)
          values$fields_to_change_input_df <- DF[rows,vec1_in_vec2(vars,colnames(DF)),drop = F]
        }
      }
    })
  })
  output$add_input_instance_ui_ <- renderUI({
    if (is_something(input$choose_record)&&is_something(input$choose_fields_change)&&is_something(input$choose_form)) {
      if (values$DB$metadata$forms$repeating[which(values$DB$metadata$forms$form_name==input$choose_form)]) {
        actionButton(
          inputId = "add_input_instance_ui",
          label = "Add Instance"
        )
      }
    }
  })
  observeEvent(input$add_input_instance_ui,ignoreInit = T,{
    if(is_something(input$choose_fields_change)){
      message("clicked add_input_instance_ui")
      DF <- values$fields_to_change_input_df
      blank_df <- DF[0,]
      x <- data.frame(
        record_id = input$choose_record,
        redcap_repeat_instrument = input$choose_form,
        redcap_repeat_instance = "1"
      )
      colnames(x)[which(colnames(x)=="record_id")]<- values$DB$redcap$id_col
      if(is_something(DF)){
        if(nrow(DF)>0){
          x$redcap_repeat_instance <- DF$redcap_repeat_instance %>% as.integer() %>% max() %>% magrittr::add(1) %>% as.character()
        }
      }
      values$fields_to_change_input_df <- DF %>% dplyr::bind_rows(x)
    }
  })
  observeEvent(input$submit_data_values, {
    do_it <- is_something(values$dynamic_input_ids)
    if(do_it){
      DF <- values$fields_to_change_input_df
      any_changes <- F
      if(is_something(DF)){
        dynamic_input_ids <- values$dynamic_input_ids
        for(dynamic_input_id in dynamic_input_ids){
          OUT <- input[[dynamic_input_id]]
          if(!is.null(OUT)){
            ij <- gsub("input_dynamic_","",dynamic_input_id) %>% strsplit("_") %>% unlist()
            i <- ij[1] %>% as.integer()
            j <- ij[2] %>% as.integer()
            if(OUT == "_truly_blank_in_redcap_") OUT <- ""
            message("original ",i," ",j," ",DF[i, j])
            message("new ",i," ",j," ",OUT)
            it_changed <- !identical(unname(DF[i, j]), unname(OUT))
            if(it_changed){
              message("it_changed!")
              DF[i, j] <- OUT
              any_changes <- T
            }
          }
        }
      }
      if(any_changes){
        values$fields_to_change_input_df <- DF
      }
    }
    print("uploaded!")
  })
  output$fields_to_change_dynamic_inputs <- renderUI({
    print("fields_to_change_dynamic_inputs triggered")
    input$choose_fields_change
    values$dynamic_input_ids
    input$choose_record
    input$choose_form
    if (is.null(values$fields_to_change_input_df)) {
      return(h3("No Items available to display."))
    }
    DF <- values$fields_to_change_input_df
    ref_cols <- values$DB$metadata$form_key_cols[[input$choose_form]]
    if(nrow(DF)==0){
      return(h3("No Items available to display."))
    }
    ncols <- ncol(DF)
    nrows <- nrow(DF)
    the_cols <- (1:ncols)[which(!colnames(DF)%in%ref_cols)]
    the_number_of_cols <- ncols-length(ref_cols)
    base_width <- floor(12 / the_number_of_cols)
    remainder <- 12 %% the_number_of_cols
    message("ref_cols: ",ref_cols %>% length())
    message("base_width: ",base_width)
    message("the_number_of_cols: ",the_number_of_cols)
    column_widths <- rep(base_width, the_number_of_cols)
    if (remainder > 0) {
      column_widths[1:remainder] <- column_widths[1:remainder] + 1
    }
    names(column_widths) <- the_cols
    the_rows <- 1:nrows
    input_list <- lapply(the_rows, function(i) {
      lapply(the_cols, function(j) {
        the_col_name <- names(DF)[j]
        if(!the_col_name %in% ref_cols){
          column(
            width = column_widths[[which(names(column_widths) == j)]], # Dynamically adjust column width
            # h4(names(DF)[j], style = "text-align: center;"), # Column name as header
            lapply(i, function(i) {
              input_name <-paste0(DF[i,ref_cols],collapse = "_")
              input_value <- DF[i,j]
              input_id <- paste0("input_dynamic_", i, "_", j)
              if(values$DB$metadata$fields$field_type[which(values$DB$metadata$fields$field_name==the_col_name)]%in%c("radio","dropdown","yesno")){
                codebook_names <- values$DB$metadata$choices$name[which(values$DB$metadata$choices$field_name == the_col_name)]
                missing_codes <- values$DB$metadata$missing_codes
                choice_names <- c("*Truly blank in REDCap*",codebook_names)
                choice_values <- c("_truly_blank_in_redcap_",codebook_names)
                if(is_something(missing_codes)){
                  choice_names <- choice_names %>% append(missing_codes$name)
                  choice_values <- choice_values %>% append(missing_codes$name)
                }
                if(input$sidebar_choice_radio){
                  return(
                    radioButtons(
                      inputId = input_id,
                      label = "",
                      choiceNames = choice_names,
                      choiceValues = choice_values,
                      selected = input_value
                    )
                  )
                }else{
                  choices <- choice_values %>%  as.list()
                  names(choices) <- choice_names
                  return(
                    selectInput(
                      inputId = input_id,
                      label = "",
                      choices = choices,
                      selected = input_value
                    )
                  )
                }
              }else{
                return(
                  textInput(
                    inputId = input_id,
                    label = "",
                    value = input_value
                  )
                )
              }
            })
          )
        }
      })
    })
    values$dynamic_input_ids <- lapply(the_cols, function(j) {
      the_col_name <- names(DF)[j]
      if(!the_col_name %in% ref_cols){
        lapply(1:nrows, function(i) {
          input_id <- paste0("input_dynamic_", i, "_", j)
          return(input_id)
        }) %>% unlist()
      }
    }) %>% unlist()
    do.call(fluidRow, input_list) # Arrange columns in a fluidRow
  })
  # ab----------
  observeEvent(input$ab_random_record,{
    random_record <- values$subset_records %>% sample1()
    message("Random Record: ", random_record)
    updateSelectizeInput(
      session,
      "choose_record",
      selected = random_record,
      choices = values$subset_records,
      server = T
    )
  })
  observeEvent(input$ab_next_record,{
    if(is_something(values$subset_records)&&is_something(input$choose_record)){
      if(length(values$subset_records)>1){
        row <- which(values$subset_records == input$choose_record)
        len <- length(values$subset_records)
        if(row == len){
          next_record_row <- 1
        }else{
          next_record_row <- row + 1
        }
        next_record <- values$subset_records[next_record_row]
        if(is_something(next_record)){
          updateSelectizeInput(
            session,"choose_record",
            selected = next_record,
            choices = values$subset_records,
            server = T
          )
        }
      }
    }
  })
  observeEvent(input$ab_update_redcap,{
    values$DB <- values$DB %>% update_DB()
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
    if(is_something(input$choose_record)&length(input$choose_record)>0){
      IF <- shinydashboard::menuItem(
        text=paste0("REDCap Record (",input$choose_record,")"),
        icon = shiny::icon("file-lines"),
        href=paste0(values$DB$links$redcap_base,"redcap_v",values$DB$redcap$version,"/DataEntry/record_home.php?pid=",values$DB$redcap$project_id,"&arm=1&id=",input$choose_record)
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
  # plotly -----------
  output$parcats <- plotly::renderPlotly({
    if(is_something(input$choose_form))
      DF <- values$subset_list[[input$choose_form]]
    input$shuffle_colors
    # print(input$choose_fields_cat)
    # cols <- vec1_in_vec2(input$choose_fields_cat,colnames(DF))
    # print(cols)
    # # fields_to_forms
    if(length(input$choose_fields_cat)>0){
      cols <- input$choose_fields_cat %>% vec1_in_vec2(colnames(DF))
      DF[,cols, drop = FALSE] %>%
        clean_DF(
          fields = values$DB$metadata,
          drop_blanks = T,
          other_drops = other_drops(ignore = input$render_missing)
        ) %>%
        plotly_parcats(remove_missing = !input$render_missing) %>%
        return()
      # mtcars  %>% plotly_parcats(remove_missing = F) %>% return()
    }
  })
}
