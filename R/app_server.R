#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  # values ------------
  values <- reactiveValues()
  values$projects <- REDCapSync::projects$df() # get list of cached projects
  values$project <- NULL
  values$dataset <- NULL
  values$project_summaries <- NULL
  values$selected_form <- NULL
  values$selected_field <- NULL
  values$selected_instance <- NULL
  values$active_table_id <- NULL
  values$all_records <- NULL
  values$subset_records <- NULL
  values$sbc <- NULL
  values$user_adds_project <- NULL
  values$REDCap_diagram <- NULL
  values$dt_tables_view_list <- NULL
  values$fields_to_change_input_df <- NULL
  values$dynamic_input_ids <- NULL
  values$data_list_form_fields_cat <- NULL
  values$data_list_form_fields_date <- NULL
  values$data_list_form_fields_int <- NULL
  # projects --------------------------------
  observeEvent(input$test_mode, {
    if (input$test_mode) {
      values$projects <- data.frame(project_name = REDCapSync:::TEST_PROJECT_NAMES)
    } else {
      values$projects <- REDCapSync::projects$df() # get list of cached projects
    }
  })
  # files ----------
  shinyFiles::shinyDirChoose(input,
                             "dir",
                             roots = c(home = "~"),
                             filetypes = c(""))
  dir <- reactive(input$dir)
  output$dir <- renderPrint(dir())
  # path
  path <- reactive({
    if (!"path" %in% names(dir())) {
      return(NULL)
    }
    x  <- unlist(dir()$path[-1])
    sanitize_path(file.path(normalizePath("~"), Reduce(file.path, x)))
  })
  # user input project -------
  observeEvent(input$user_adds_project_modal, {
    # display a modal dialog with a header, textinput and action buttons
    showModal(
      modalDialog(
        tags$h2("Please enter your Project Information"),
        textInput(
          inputId = "user_adds_project_short_name",
          label = "Project Name",
          placeholder = "MY_PROJECT"
        ),
        passwordInput("user_adds_project_api_token", "API token"),
        textInput(
          inputId = "user_adds_project_redcap_uri",
          label = "Institutional REDCap URI",
          placeholder = "https://redcap.example.edu/api/"
        ),
        textInput(
          inputId = "user_adds_project_dir_path",
          label = "Directory",
          placeholder = "example/your/directory/"
        ),
        shinyFiles::shinyDirButton("dir", "Chose directory", "Upload"),
        footer = tagList(
          actionButton("user_adds_project_submit", "Submit"),
          modalButton("Cancel")
        )
      )
    )
  })
  # user input project -------
  observeEvent(dir(), {
    updateTextInput(session = session,
                    inputId = "user_adds_project_dir_path",
                    value = path())
  })
  # only store the information if the user clicks submit
  observeEvent(input$user_adds_project_submit, {
    dir_path <- NULL
    if (dir.exists(input$user_adds_project_dir_path)) {
      dir_path <- input$user_adds_project_dir_path
    }
    if (!is_something(input$user_adds_project_api_token)) {
      return()
    }
    if (!is_something(input$user_adds_project_redcap_uri)) {
      return()
    }
    project <- setup_project(
      project_name = toupper(input$user_adds_project_short_name),
      dir_path = dir_path,
      redcap_uri = input$user_adds_project_redcap_uri
    )
    project$print()
    keyring::key_set_with_value(
      service = config$keyring.service(),
      username = project$project_name,
      password = input$user_adds_project_api_token,
      keyring = config$keyring()
    )
    showModal(modalDialog(
      tags$h3("Syncing with REDCap..."),
      tags$p("Please wait while the project is being created."),
      footer = NULL,
      easyClose = FALSE
    ))
    project$sync()
    updateSwitchInput(session = session,
                      inputId = "test_mode",
                      value = FALSE)
    removeModal()
    values$projects <- REDCapSync::projects$df() # get list of cached projects
  })
  # setup_project(
  #   project_name = OUT$project_name,
  #   dir_path = OUT$dir_path,
  #   token_name = OUT$token_name,
  #   redcap_base = "https://redcap.miami.edu/",
  #   force = TRUE,
  #   merge_form_name = "merged"
  # )
  # diagrams ----------
  output$REDCap_diagram_vis <- visNetwork::renderVisNetwork({
    REDCap_diagram(
      project = values$project,
      static = FALSE,
      render = TRUE,
      clean_names = input$metadata_graph_clean_names,
      duplicate_forms = input$metadata_graph_duplicate_forms,
      include_fields = input$metadata_graph_include_fields,
      include_choices = input$metadata_graph_include_choices,
      hierarchical = input$metadata_graph_hierarchical,
      direction = input$metadata_graph_direction,
      zoomView = input$metadata_graph_allow_zoom
    )
  })
  output$REDCap_diagram_dia <- DiagrammeR::renderGrViz({
    DiagrammeR::grViz(DiagrammeR::generate_dot(
      REDCap_diagram(
        project = values$project,
        static = TRUE,
        render = FALSE,
        clean_names = input$metadata_graph_clean_names,
        duplicate_forms = input$metadata_graph_duplicate_forms,
        include_fields = input$metadata_graph_include_fields,
        include_choices = input$metadata_graph_include_choices,
        hierarchical = input$metadata_graph_hierarchical,
        direction = input$metadata_graph_direction,
        zoomView = input$metadata_graph_allow_zoom
      )
    ))
  })
  # tables --------
  output$dt_tables_view <- renderUI({
    if (length(values$dataset$data) == 0L)
      return(h3("No tables available to display."))
    do.call(tabsetPanel, c(id = "tabs", lapply(seq_along(values$dataset$data), function(i) {
      table_name_raw <- names(values$dataset$data)[i]
      table_id <- paste0("table___home__", table_name_raw)
      tabPanel(
        title = table_name_raw |> form_names_to_form_labels_alt(values$dataset$metadata),
        DT::DTOutput(table_id)
      )
    })))
  })
  output$table1 <- renderUI({
    if (is_something(input$choose_form)) {
      variables <- c(
        ifelse(
          input$choose_split == "no_choice",
          NA,
          input$choose_split
        ),
        input$choose_fields_cat
      ) |>
        unique() |>
        drop_nas() |>
        drop_if("")
      if (length(variables) == 0L) {
        return()
      }
      DF <- values$dataset$data[[input$choose_form]][, variables, drop = FALSE]
      if (is_something(DF)) {
        # message_dev("input$choose_split: ",input$choose_split)
        # message_dev("variables: ",variables |> toString())
        # DF |> head() |> print()
        html_output <- htmlTable::htmlTable(
          align = "l",
          #,other_drops = other_drops(ignore = input$render_missing)
          make_table1(
            DF = REDCapSync:::clean_form(
              form = DF,
              fields = values$dataset$metadata$fields,
              labelled = input$labelled
            ),
            group = input$choose_split,
            variables = variables,
            render.missing = input$render_missing
          ),
          css.cell = "width:100%; overflow-x:auto;"  # Ensures width and adds horizontal overflow
        )
        tags$div(
          style = "width:100%; overflow-x:auto;",
          # Force containment within the box
          HTML(html_output)
        )
      }
    }
  })
  # dt_tables_view-----------
  # Create a reactive list of DT tables
  output$dt_tables_view_records <- renderUI({
    if (length(values$dt_tables_view_list) == 0L) {
      # If the list is empty, show a message_dev
      return(h3("No tables available to display."))
    } else {
      if (input$view_switch_text) {
        form_list_to_text(form_list = values$dt_tables_view_list,
                          project = values$project) |> HTML()
      } else {
        # Otherwise, generate the list of tables
        lapply(seq_along(values$dt_tables_view_list), function(i) {
          table_name <- names(values$dt_tables_view_list)[i]
          table_id <- paste0("table__dt_view_", i)
          # Create DTOutput for each table
          tagList(h3(paste("Table:", table_name)), DT::DTOutput(table_id))
        })
      }
    }
  })
  # Render each DT table
  observe({
    if (is_something(input$choose_record) &&
        is_something(input$choose_fields_view)) {
      if (is_something(values$project)) {
        values$dt_tables_view_list <- REDCapSync:::generate_project_dataset(
          project = values$project,
          dataset_name = "RosyREDCap",
          transformation_type = "none",
          filter_field = values$project$metadata$id_col,
          filter_choices = input$choose_record,
          filter_strict = FALSE,
          form_names = REDCapSync:::field_to_form_names(values$project, field_names = input$choose_fields_view),
          field_names = input$choose_fields_view,
          exclude_identifiers = input$deidentify_switch,
          exclude_free_text = input$exclude_free_text_switch,
          date_handling = "none",
          clean = FALSE,
          drop_blanks = FALSE,
          drop_missing_codes = FALSE,
          drop_others = NULL,
          include_metadata = FALSE,
          annotate_from_log = FALSE,
          include_records = FALSE,
          include_users = FALSE,
          include_log = FALSE
        )$data |> process_df_list()
        # message_dev(values$dt_tables_view_list)
        # values$dt_tables_view_list <- project |> generate_project_dataset(records = dataset$data$sarcoma$record_id |> sample1(), data_choice = get_default_data_choice(values$project),field_names = "sarc_timeline") |> process_df_list()
        # values$dataset$data$sarcoma |> dplyr::filter(sarcoma_id%in%values$chosen_group_sarcoma) |> make_PSproject_table(project = values$project)
        if (!is_something(values$dt_tables_view_list))
          return(h3("No tables available to display."))
        x <- lapply(seq_along(values$dt_tables_view_list), function(i) {
          table_data <- values$dt_tables_view_list[[i]]
          table_id <- paste0("table__dt_view_", i)
          output[[table_id]] <- DT::renderDT({
            table_data |>
              REDCapSync:::clean_form(
                fields = values$dataset$metadata$fields,
                labelled = input$labelled
              ) |>
              make_DT_table()
          })
        })
        return(x)
      }
    }
  })
  # simple tables ---------
  output$projects_table <- DT::renderDT({
    values$projects |> make_DT_table()
  })
  output$forms_table <- DT::renderDT({
    values$dataset$metadata$forms |> make_DT_table()
  })
  output$metadata_table <- DT::renderDT({
    values$dataset$metadata$fields |> make_DT_table()
  })
  output$codebook_table <- DT::renderDT({
    values$dataset$metadata$choices |> make_DT_table()
  })
  output$user_table <- DT::renderDT({
    values$project$redcap$users |> make_DT_table()
  })
  output$log_table <- DT::renderDT({
    values$project$redcap$log |> make_DT_table()
  })
  output$the_uploading_table <- DT::renderDT({
    values$fields_to_change_input_df |> make_DT_table()
  })
  # Render each DT table ------
  observe({
    if (!is_something(values$dataset$data))
      return(h3("No tables available to display."))
    x <- lapply(names(values$dataset$data), function(TABLE) {
      table_data <- values$dataset$data[[TABLE]]
      table_id <- paste0("table___home__", TABLE)
      output[[table_id]] <- DT::renderDT({
        table_data |>
          clean_RC_df_for_DT(values$project) |>
          make_DT_table()
      })
    })
    return(x)
  })
  observe({
    if (!is_something(values$dataset$data))
      return(h3("No tables available to display."))
    x <- lapply(names(values$dataset$data), function(TABLE) {
      table_data <- values$dataset$data[[TABLE]]
      table_id <- paste0("table___home__", TABLE, "_exists")
      values[[table_id]] <- !is.null(input[[paste0("table___home__", TABLE, "_state")]])
    })
    return(x)
  })
  # html ---------------
  output$html_test <- renderUI({
    tags$iframe(
      class = "pubchem-widget",
      src = paste0(
        "https://pubchem.ncbi.nlm.nih.gov/compound/2244#section=2D-Structure&embed=true"
      ),
      style = "width: 450px; max-width: 100%; height: 650px;"
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
    selectizeInput(
      inputId = "transformation_switch",
      label = "Transformation",
      multiple = FALSE,
      selected = "default",
      choices = c(
        Default = "default",
        None = "none",
        "Merge-Non-Repeating" = "merge_non_repeating"
      )
    )
  })
  output$filter_switch_ <- renderUI({
    if (is_something(input$choose_group)) {
      if (input$choose_group != "All Records") {
        switchInput(
          inputId = "filter_switch",
          onLabel = "Strict",
          offLabel = "Records",
          value = TRUE
        )
      }
    }
  })
  output$choose_project_ <- renderUI({
    selectizeInput(inputId = "choose_project",
                   label = "Choose Project",
                   choices = NULL)
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
      choices = vec1_in_vec2(
        stats::setNames(
          object = values$dataset$metadata$forms$form_name,
          nm = values$dataset$metadata$forms$form_label
        ),
        names(values$dataset$data)
      )
    )
  })
  output$choose_field_ <- renderUI({
    selectizeInput(
      inputId = "choose_field",
      label = "Choose Field",
      multiple = FALSE,
      selected = NULL,
      choices = NULL
    )
  })
  output$choose_fields_cat_ <- renderUI({
    selectizeInput(
      inputId = "choose_fields_cat",
      label = "Choose Fields",
      multiple = TRUE,
      choices = values$data_list_form_fields_cat
    )
  })
  output$choose_fields_view_ <- renderUI({
    selectizeInput(
      inputId = "choose_fields_view",
      label = "Choose Fields",
      multiple = TRUE,
      choices = values$dataset$metadata$fields$field_name
    )
  })
  output$choose_fields_change_ <- renderUI({
    selectizeInput(
      inputId = "choose_fields_change",
      label = "Choose Field",
      multiple = TRUE,
      choices = values$dataset$data[[input$choose_form]] |>
        colnames() |>
        setdiff(values$dataset$metadata$raw_structure_cols)
      # can remove arm_num and event_name
    )
  })
  output$choose_group_ <- renderUI({
    selectizeInput(
      inputId = "choose_group",
      label = "Choose Group",
      multiple = input$allow_multiple_groups,
      selected = NULL,
      choices = c("All Records", values$sbc$label)
    )
  })
  output$choose_survival_start_col_ <- renderUI({
    selectizeInput(
      inputId = "choose_survival_start_col",
      label = "Start",
      multiple = FALSE,
      selected = NULL,
      choices = values$data_list_form_fields_date,
      width = "100%"
    )
  })
  output$choose_survival_end_col_ <- renderUI({
    selectizeInput(
      inputId = "choose_survival_end_col",
      label = "End",
      multiple = FALSE,
      selected = NULL,
      choices = values$data_list_form_fields_date,
      width = "100%"
    )
  })
  output$choose_survival_status_col_ <- renderUI({
    selectizeInput(
      inputId = "choose_survival_status_col",
      label = "Status Field",
      multiple = FALSE,
      selected = NULL,
      choices = values$data_list_form_fields_cat,
      width = "100%"
    )
  })
  output$choose_survival_status_choice_ <- renderUI({
    choice_df <- values$project$metadata$fields |>
      dplyr::filter(field_name == input$choose_survival_status_col) |>
      REDCapSync:::fields_to_choices()
    selectizeInput(
      inputId = "choose_survival_status_choice",
      label = "Status Choice",
      multiple = FALSE,
      selected = NULL,
      choices = choice_df$name,
      width = "100%"
    )
  })
  output$choose_survival_xlim_ <- renderUI({
    sliderInput(
      inputId = "choose_survival_xlim",
      label = "X Limits",
      min = 0L,
      max = 12L,
      value = c(0L, 12L),
      step = 1L,
      width = "100%"
    )
  })
  output$choose_split_ <- renderUI({
    selectizeInput(
      inputId = "choose_split",
      label = "Choose Split",
      selected = NULL,
      choices = values$data_list_form_fields_cat
    )
  })
  observeEvent(values$project, {
    message_dev("values$project changed!")
    if (is_something(values$project)) {
      values$selected_form <- NULL
      values$selected_field <- NULL
      values$selected_instance <- NULL
      values$active_table_id <- NULL
      values$all_records <- NULL
      values$subset_records <- NULL
      values$dataset <- NULL
      values$sbc <- NULL
      values$fields_to_change_input_df <- NULL
      values$dynamic_input_ids <- NULL
      values$data_list_form_fields_cat <- NULL
      values$data_list_form_fields_date <- NULL
      values$data_list_form_fields_int <- NULL
      values$subset_records <-
        values$all_records <-
        values$project$record_summary[[values$project$metadata$id_col]]
      updateSelectizeInput(
        session = session,
        inputId = "choose_record",
        selected = values$subset_records[1L],
        choices = values$subset_records,
        server = TRUE
      )
      values$dataset <- REDCapSync:::generate_project_dataset(
        project = values$project,
        dataset_name = "RosyREDCap",
        transformation_type = input$transformation_switch,
        labelled = input$labelled,
        exclude_identifiers = input$deidentify_switch,
        exclude_free_text = input$exclude_free_text_switch,
        date_handling = "none",
        clean = FALSE,
        drop_blanks = FALSE,
        drop_missing_codes = FALSE,
        drop_others = NULL,
        include_metadata = FALSE,
        annotate_from_log = FALSE,
        include_records = FALSE,
        include_users = FALSE,
        include_log = FALSE
      )
      values$sbc <- sidebar_choices(values$dataset)
      if (!is.null(values$project$transformation)) {
        values$editable_forms_transformation_table <- values$project$transformation$forms |> as.data.frame(stringsAsFactors = FALSE)
      }
      field_names <- values$sbc$field_name |>
        unique() |>
        vec1_in_vec2(values$dataset$metadata$fields$field_name[which(
          values$dataset$metadata$fields$field_type_r %in% c("factor", "integer", "numeric")
        )])
      updateSelectizeInput(
        session,
        "choose_group",
        choices = c(
          "All Records",
          # "Custom Records",
          values$sbc$label[which(values$sbc$field_name %in% field_names)]
        ),
        server = TRUE
      )
      choices <- stats::setNames(
        object = values$dataset$metadata$forms$form_name,
        nm = values$dataset$metadata$forms$form_label
      ) |> vec1_in_vec2(names(values$dataset$data))
      selected <- choices[1L]
      if ("merged" %in% choices) {
        selected <- "merged"
      }
      updateSelectizeInput(
        session = session,
        inputId = "choose_form",
        selected = selected,
        choices = choices
      )
      values$data_list_form_fields_cat <- get_field_type_from_data_list(
        data_list = values$dataset,
        field_type_r = "factor",
        form_name = input$choose_form
      )
      values$data_list_form_fields_date <- get_field_type_from_data_list(
        data_list = values$dataset,
        field_type_r = "date",
        form_name = input$choose_form
      )
      values$data_list_form_fields_int <- get_field_type_from_data_list(
        data_list = values$dataset,
        field_type_r = "integer",
        form_name = input$choose_form
      )
      # updateSelectizeInput(
      #   session = session,
      #   inputId = "choose_split",
      #   selected = values$data_list_form_fields_cat[1],
      #   choices = values$data_list_form_fields_cat,
      #   server = FALSE
      # )
      # updateSelectizeInput(
      #   session = session,
      #   inputId = "choose_fields_cat",
      #   selected = values$data_list_form_fields_cat[1],
      #   choices = values$data_list_form_fields_cat,
      #   server = FALSE
      # )
      # updateSelectizeInput(
      #   session = session,
      #   inputId = "choose_survival_status_col",
      #   selected = values$data_list_form_fields_cat[1],
      #   choices = values$data_list_form_fields_cat,
      #   server = FALSE
      # )
      # updateSelectizeInput(
      #   session = session,
      #   inputId = "choose_survival_end_col",
      #   selected = values$data_list_form_fields_date[1],
      #   choices = values$data_list_form_fields_date,
      #   server = FALSE
      # )
      # updateSelectizeInput(
      #   session = session,
      #   inputId = "choose_survival_start_col",
      #   selected = values$data_list_form_fields_date[1],
      #   choices = values$data_list_form_fields_date,
      #   server = FALSE
      # )
    }
  })
  observeEvent(input$choose_project, {
    if (!is.null(input$choose_project)) {
      if (is_something(input$choose_project)) {
        values$project <- tryCatch({
          load_project(project_name = input$choose_project)$.internal
        }, error = function(e) {
          NULL
        })
        if (is_something(input$choose_project)) {
          if (!is.null(input[[paste0("projects_table_state")]])) {
            ROW <- which(values$projects$project_name == input$choose_project)
            skip <- FALSE
            if (!is.null(input[[paste0("projects_table_rows_selected")]])) {
              skip <- identical(ROW, input[[paste0("projects_rows_selected")]])
            }
            if (!skip) {
              DT::selectRows(
                proxy = DT::dataTableProxy("projects_table", deferUntilFlush = FALSE),
                selected = ROW
              )
            }
          }
        }
      }
    }
  })
  observe({
    input$deidentify_switch
    input$exclude_free_text_switch
    input$transformation_switch
    input$choose_group
    input$labelled
    message_dev("trigger switch")
    isolate({
      filter_choices <- NULL
      filter_field <- NULL
      if (is_something(values$project)) {
        if (is_something(input$choose_group)) {
          if (is_something(input$transformation_switch)) {
            values$dataset <- REDCapSync:::generate_project_dataset(
              project = values$project,
              dataset_name = "RosyREDCap",
              transformation_type = input$transformation_switch,
              labelled = input$labelled,
              filter_strict = FALSE,
              exclude_identifiers = input$deidentify_switch,
              exclude_free_text = input$exclude_free_text_switch,
              date_handling = "none",
              clean = FALSE,
              drop_blanks = FALSE,
              drop_missing_codes = FALSE,
              drop_others = NULL,
              include_metadata = FALSE,
              annotate_from_log = FALSE,
              include_records = FALSE,
              include_users = FALSE,
              include_log = FALSE
            )
            if (length(input$choose_group) == 1L) {
              sbc <- sidebar_choices(values$dataset)
              if (!identical(sbc, values$sbc)) {
                values$sbc <- sbc
              }
              if (input$choose_group == "All Records") {
                values$subset_records <- values$all_records
              } else {
                x <- values$sbc[which(values$sbc$label == input$choose_group), ]
                if (nrow(x) > 0L) {
                  DF <- values$dataset$data[[x$form_name]]
                  filter_field <- values$project$metadata$id_col
                  values$subset_records <- filter_choices <- DF[[values$project$metadata$id_col]][which(DF[[x$field_name]] ==
                                                                                                          x$name)] |> unique()
                  if (is_something(input$filter_switch)) {
                    if (input$filter_switch) {
                      filter_field <- x$field_name
                      filter_choices <- x$name
                    }
                  }
                }
                if (!is.null(filter_field) &&
                    !is.null(filter_choices)) {
                  values$dataset$data <- values$dataset |>
                    REDCapSync:::filter_data_list(filter_field = filter_field,
                                                  filter_choices = filter_choices)
                }
              }
            }
          }
        }
      }
    })
  })
  debounced_tabs <- debounce(reactive(input$tabs), 250L)  # 250ms delay
  observeEvent(debounced_tabs(), {
    values$selected_form <-
      input$tabs |>
      form_labels_to_form_names_alt(values$dataset$metadata)
    updateSelectizeInput(
      session = session,
      inputId = "choose_form",
      selected = values$selected_form
    )
  }, ignoreInit = TRUE)
  observeEvent(values$subset_records, {
    message_dev("values$subset_records changed!")
    selected <- NULL
    if (is_something(input$choose_record)) {
      if (!input$choose_record %in% values$subset_records) {
        if (is_something(values$subset_records)) {
          selected <- values$subset_records[1L]
        }
      }
    }
    updateSelectizeInput(
      session = session,
      inputId = "choose_record",
      selected = selected,
      choices = values$subset_records,
      server = TRUE
    )
  })
  # observeEvent(values$data_list_form_fields_cat,{
  #   message_dev("values$data_list_form_fields_cat changed!")
  #   selected <- "no_choice"
  #   if(is_something(input$choose_split)){
  #     if(input$choose_split %in% values$data_list_form_fields_cat){
  #       selected <- input$choose_split
  #     }
  #   }
  #   updateSelectizeInput(
  #     session = session,
  #     inputId = "choose_split",
  #     selected = selected,
  #     choices = values$data_list_form_fields_cat,
  #     server = TRUE
  #   )
  # })
  # Update the tabset panel when a new tab is selected in the selectInput
  observeEvent(input$sb1, ignoreNULL = TRUE, {
    if (is_something(values$project)) {
      if (input$sb1 %in% c("group", "record")) {
        message_dev("input$sb1:", input$sb1)
        if (is_something(input$choose_form)) {
          updateTabsetPanel(
            session = session,
            inputId = "tabs",
            selected = input$choose_form |> form_names_to_form_labels_alt(values$dataset$metadata)
          )
        }
      }
    }
  })
  # observe({
  #   if(is_something(values$project)){
  #     if(input$sb1 %in% c("group","record")){
  #       message_dev("input$sb1:",input$sb1)
  #       if(is_something(input$choose_form)){
  #         updateTabsetPanel(
  #           session,
  #           "tabs",
  #           selected = input$choose_form |> form_names_to_form_labels_alt(values$dataset$metadata)
  #         )
  #       }
  #       if(is_something(values$dataset$data)){
  #         field_choices_date <- get_field_type_from_data_list(
  #           data_list = values$dataset,
  #           field_type_r = 'date',
  #           form_name = input$choose_form
  #         )
  #         field_choices_cat <- get_field_type_from_data_list(
  #           data_list = values$dataset,
  #           field_type_r = 'factor',
  #           form_name = input$choose_form
  #         )
  #         fields <- values$dataset$metadata$fields
  #         field_names_view <- fields$field_name[which(!fields$field_type %in% c("description"))]
  #         field_labels_view <- field_names_view |> field_names_to_field_labels_alt(values$dataset$metadata)
  #         field_names_change <- fields$field_name[which(
  #           (!fields$field_type %in% c("description","file")) &
  #             fields$in_original_redcap &
  #             fields$form_name == input$choose_form &
  #             fields$field_name != values$project$metadata$id_col
  #         )]
  #         field_labels_change <- field_names_change |> field_names_to_field_labels_alt(values$dataset$metadata)
  #         # if(is_something(field_choices_cat)){
  #         #   selected <- "no_choice"
  #         #   if(is_something(input$choose_split)){
  #         #     if(input$choose_split %in% field_choices_cat){
  #         #       selected <- input$choose_split
  #         #     }
  #         #   }
  #         #   updateSelectizeInput(
  #         #     session = session,
  #         #     inputId = "choose_split",
  #         #     selected = selected,
  #         #     choices = field_choices_cat
  #         #   )
  #         #   updateSelectizeInput(
  #         #     session = session,
  #         #     inputId = "choose_survival_status_col",
  #         #     selected = selected,
  #         #     choices = field_choices_cat
  #         #   )
  #         #   selected <- NULL
  #         #   if(is_something(input$choose_fields_cat)){
  #         #     if(all(input$choose_fields_cat %in% field_choices_cat)){
  #         #       selected <- input$choose_fields_cat
  #         #     }
  #         #   }
  #         #   updateSelectizeInput(
  #         #     session = session,
  #         #     inputId = "choose_fields_cat",
  #         #     selected = selected,
  #         #     choices = field_choices_cat
  #         #   )
  #         # }
  #         # if(is_something(field_names_view)){
  #         #   selected <- NULL
  #         #   if(is_something(input$choose_fields_view)){
  #         #     if(all(input$choose_fields_view %in% field_names_view)){
  #         #       selected <- input$choose_fields_view
  #         #     }
  #         #   }
  #         #   updateSelectizeInput(
  #         #     session = session,
  #         #     inputId = "choose_fields_view",
  #         #     selected = selected,
  #         #     choices = stats::setNames(field_names_view,field_labels_view)
  #         #   )
  #         # }
  #         # if(is_something(field_names_change)){
  #         #   selected <- NULL
  #         #   if(is_something(input$choose_fields_change)){
  #         #     if(all(input$choose_fields_change %in% field_names_change))selected <- input$choose_fields_change
  #         #   }
  #         #   updateSelectizeInput(
  #         #     session = session,
  #         #     inputId = "choose_fields_change",
  #         #     selected = selected,
  #         #     choices =  stats::setNames(field_names_change,field_labels_change)
  #         #   )
  #         # }
  #         # if(is_something(field_choices_date)){
  #         #   updateSelectizeInput(
  #         #     session = session,
  #         #     inputId = "choose_survival_start_col",
  #         #     # selected = selected,
  #         #     choices = field_choices_date
  #         #   )
  #         #   updateSelectizeInput(
  #         #     session = session,
  #         #     inputId = "choose_survival_end_col",
  #         #     # selected = selected,
  #         #     choices = field_choices_date
  #         #   )
  #         # }
  #       }
  #     }
  #   }
  # })
  observeEvent(input$choose_form, {
    selected <-  input$choose_form |> form_names_to_form_labels_alt(values$dataset$metadata)
    if (is_something(selected)) {
      # if(!identical(selected,input$tabs)){
      message_dev("updating form2: ", selected)
      updateTabsetPanel(session = session,
                        inputId = "tabs",
                        selected = selected)
      values$data_list_form_fields_cat <- get_field_type_from_data_list(
        data_list = values$dataset,
        field_type_r = "factor",
        form_name = input$choose_form
      )
      values$data_list_form_fields_date <- get_field_type_from_data_list(
        data_list = values$dataset,
        field_type_r = "date",
        form_name = input$choose_form
      )
      values$data_list_form_fields_int <- get_field_type_from_data_list(
        data_list = values$dataset,
        field_type_r = "integer",
        form_name = input$choose_form
      )
      # }
    }
  })
  observe({
    selected <- input[["projects_table_rows_selected"]]
    # message_dev("selected: ", selected)
    isolate({
      expected <- NULL
      data_col <- values$projects$project_name
      expected <- which(data_col == input$choose_project)
      # message_dev("expected: ", expected)
      if (is_something(selected)) {
        if (!identical(selected, expected)) {
          selected <- unique(data_col[[selected]])
          message_dev("valid_click: ", selected)
          updateSelectizeInput(
            session = session,
            inputId = "choose_project",
            selected = selected,
            choices = data_col,
            server = TRUE
          )
        }
      }
    })
  })
  observe({
    if (is_something(input$choose_record)) {
      all_forms <- values$dataset$data |> names()
      all_forms |> lapply(function(form) {
        values[[paste0("table___home__", form, "_exists")]]
      })
      message_dev("input$tabs : ", input$tabs)
      values$selected_form <- input$tabs |> form_labels_to_form_names_alt(values$dataset$metadata)
      isolate({
        message_dev("values$selected_form: ", values$selected_form)
        if (is_something(values$selected_form)) {
          values$active_table_id <- paste0("table___home__", values$selected_form)
          message_dev("values$active_table_id: ", values$active_table_id)
          starting_record <- input$choose_record
          message_dev("values$selected_form: ", values$selected_form)
          state_length <- input[[paste0(values$active_table_id, "_state")]]$length
          message_dev("state_length: ", state_length)
          for (form in all_forms) {
            if (!is.null(input[[paste0("table___home__", form, "_state")]])) {
              message_dev("form: ", form)
              ROWS <- which(values$dataset$data[[form]][[values$project$metadata$id_col]] == input$choose_record)
              message_dev("ROWS: ", ROWS |> toString())
              skip <- FALSE
              if (!is.null(input[[paste0("table___home__", form, "_rows_selected")]])) {
                message_dev("ident ",
                            identical(ROWS, input[[paste0("table___home__", form, "_rows_selected")]]),
                            " ",
                            ROWS,
                            " ",
                            input[[paste0("table___home__", form, "_rows_selected")]])
                skip <- identical(ROWS, input[[paste0("table___home__", form, "_rows_selected")]])
                message_dev("SKIP: ", skip)
              }
              if (!skip) {
                message_dev("triggered proxy Tabs: ", form, " Row ", ROWS)
                DT::selectRows(
                  proxy = DT::dataTableProxy(
                    paste0("table___home__", form),
                    deferUntilFlush = FALSE
                  ),
                  selected = ROWS
                )
                if (length(ROWS) > 0L) {
                  page <- as.integer(ROWS[[1L]] / state_length) + 1L
                  message_dev("triggered page: ", page)
                  DT::selectPage(
                    proxy = DT::dataTableProxy(
                      paste0("table___home__", form),
                      deferUntilFlush = FALSE
                    ),
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
    if (!is.null(values$active_table_id)) {
      selected <- input[[paste0(values$active_table_id, "_rows_selected")]]
      # message_dev("selected: ", selected)
      isolate({
        if (is_something(values$selected_form)) {
          expected <- NULL
          data_col <- values$dataset$data[[values$selected_form]][[values$project$metadata$id_col]]
          expected <- which(data_col == input$choose_record)
          # message_dev("expected: ", expected)
          if (is_something(selected)) {
            if (!identical(selected, expected)) {
              selected <- unique(data_col[[selected]])
              message_dev("valid_click: ", selected)
              updateSelectizeInput(
                session,
                "choose_record",
                selected = selected,
                choices = values$subset_records,
                server = TRUE
              )
            }
          }
        }
      })
    }
  })
  observe({
    if (!is.null(values$projects)) {
      updateSelectizeInput(
        session,
        "choose_project",
        choices = values$projects$project_name,
        server = TRUE
      )
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
      if (running_it) {
        vars <- unique(c(
          values$dataset$metadata$form_key_cols[[input$choose_form]],
          input$choose_fields_change
        ))
        DF <- NULL
        if (is_something(input$choose_form)) {
          DF <- values$dataset$data[[input$choose_form]]
          rows <-  which(DF[[values$project$metadata$id_col]] == input$choose_record)
          values$fields_to_change_input_df <- DF[rows, vec1_in_vec2(vars, colnames(DF)), drop = FALSE]
        }
      }
    })
  })
  output$add_input_instance_ui_ <- renderUI({
    if (is_something(input$choose_record) &&
        is_something(input$choose_fields_change) &&
        is_something(input$choose_form)) {
      if (values$dataset$metadata$forms$repeating[which(values$dataset$metadata$forms$form_name ==
                                                        input$choose_form)]) {
        actionButton(inputId = "add_input_instance_ui", label = "Add Instance")
      }
    }
  })
  observeEvent(input$add_input_instance_ui, ignoreInit = TRUE, {
    if (is_something(input$choose_fields_change)) {
      message_dev("clicked add_input_instance_ui")
      DF <- values$fields_to_change_input_df
      # blank_df <- DF[0L, ]
      x <- data.frame(
        record_id = input$choose_record,
        redcap_repeat_instrument = input$choose_form,
        redcap_repeat_instance = "1",
        stringsAsFactors = FALSE
      )
      colnames(x)[which(colnames(x) == "record_id")] <- values$project$metadata$id_col
      if (is_something(DF)) {
        if (nrow(DF) > 0L) {
          new_inst <- DF$redcap_repeat_instance |> as.integer() |> max()
          x$redcap_repeat_instance <- as.character(new_inst + 1L)
        }
      }
      values$fields_to_change_input_df <- DF |> dplyr::bind_rows(x)
    }
  })
  observeEvent(input$submit_data_values, {
    do_it <- is_something(values$dynamic_input_ids)
    if (do_it) {
      DF <- values$fields_to_change_input_df
      any_changes <- FALSE
      if (is_something(DF)) {
        dynamic_input_ids <- values$dynamic_input_ids
        for (dynamic_input_id in dynamic_input_ids) {
          OUT <- input[[dynamic_input_id]]
          if (!is.null(OUT)) {
            ij <- gsub("input_dynamic_", "", dynamic_input_id) |> strsplit("_") |> unlist()
            i <- ij[1L] |> as.integer()
            j <- ij[2L] |> as.integer()
            if (OUT == "_truly_blank_in_redcap_")
              OUT <- ""
            message_dev("original ", i, " ", j, " ", DF[i, j])
            message_dev("new ", i, " ", j, " ", OUT)
            it_changed <- !identical(unname(DF[i, j]), unname(OUT))
            if (it_changed) {
              message_dev("it_changed!")
              DF[i, j] <- OUT
              any_changes <- TRUE
            }
          }
        }
      }
      if (any_changes) {
        values$fields_to_change_input_df <- DF
      }
    }
  })
  observeEvent(input$submit_data_values2, {
    message_dev("uploaded!")
  })
  output$fields_to_change_dynamic_inputs <- renderUI({
    message_dev("fields_to_change_dynamic_inputs triggered")
    input$choose_fields_change
    values$dynamic_input_ids
    input$choose_record
    input$choose_form
    if (is.null(values$fields_to_change_input_df)) {
      return(h3("No Items available to display."))
    }
    DF <- values$fields_to_change_input_df
    ref_cols <- values$dataset$metadata$form_key_cols[[input$choose_form]]
    if (nrow(DF) == 0L) {
      return(h3("No Items available to display."))
    }
    ncols <- ncol(DF)
    nrows <- nrow(DF)
    the_cols <- (1L:ncols)[which(!colnames(DF) %in% ref_cols)]
    the_number_of_cols <- ncols - length(ref_cols)
    base_width <- floor(12L / the_number_of_cols)
    remainder <- 12L %% the_number_of_cols
    message_dev("ref_cols: ", ref_cols |> length())
    message_dev("base_width: ", base_width)
    message_dev("the_number_of_cols: ", the_number_of_cols)
    column_widths <- rep(base_width, the_number_of_cols)
    if (remainder > 0L) {
      column_widths[1L:remainder] <- column_widths[1L:remainder] + 1L
    }
    names(column_widths) <- the_cols
    the_rows <- 1L:nrows
    input_list <- lapply(the_rows, function(i) {
      lapply(the_cols, function(j) {
        the_col_name <- names(DF)[j]
        if (!the_col_name %in% ref_cols) {
          column(
            width = column_widths[[which(names(column_widths) == j)]],
            # Dynamically adjust column width
            # h4(names(DF)[j], style = "text-align: center;"), # Column name as header
            lapply(i, function(i) {
              # input_name <- paste(DF[i, ref_cols], collapse = "_")
              input_value <- DF[i, j]
              input_id <- paste0("input_dynamic_", i, "_", j)
              if (values$dataset$metadata$fields$field_type[which(values$dataset$metadata$fields$field_name ==
                                                                  the_col_name)] %in% c("radio", "dropdown", "yesno")) {
                codebook_names <- values$dataset$metadata$choices$name[which(values$dataset$metadata$choices$field_name == the_col_name)]
                missing_codes <- values$dataset$metadata$missing_codes
                choice_names <- c("*Truly blank in REDCap*", codebook_names)
                choice_values <- c("_truly_blank_in_redcap_", codebook_names)
                if (is_something(missing_codes)) {
                  choice_names <- choice_names |> append(missing_codes$name)
                  choice_values <- choice_values |> append(missing_codes$name)
                }
                if (input$sidebar_choice_radio) {
                  return(
                    radioButtons(
                      inputId = input_id,
                      label = "",
                      choiceNames = choice_names,
                      choiceValues = choice_values,
                      selected = input_value
                    )
                  )
                } else {
                  choices <- choice_values |>  as.list()
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
              } else {
                return(textInput(
                  inputId = input_id,
                  label = "",
                  value = input_value
                ))
              }
            })
          )
        }
      })
    })
    values$dynamic_input_ids <- lapply(the_cols, function(j) {
      the_col_name <- names(DF)[j]
      if (!the_col_name %in% ref_cols) {
        lapply(1L:nrows, function(i) {
          input_id <- paste0("input_dynamic_", i, "_", j)
          input_id
        }) |> unlist()
      }
    }) |>
      unlist()
    do.call(fluidRow, input_list) # Arrange columns in a fluidRow
  })
  # ab----------
  observeEvent(input$ab_random_record, {
    random_record <- values$subset_records |> sample1()
    message_dev("Random Record: ", random_record)
    updateSelectizeInput(
      session,
      "choose_record",
      selected = random_record,
      choices = values$subset_records,
      server = TRUE
    )
  })
  observeEvent(input$ab_next_record, {
    if (is_something(values$subset_records) &&
        is_something(input$choose_record)) {
      if (length(values$subset_records) > 1L) {
        row <- which(values$subset_records == input$choose_record)
        len <- length(values$subset_records)
        if (row == len) {
          next_record_row <- 1L
        } else {
          next_record_row <- row + 1L
        }
        next_record <- values$subset_records[next_record_row]
        if (is_something(next_record)) {
          updateSelectizeInput(
            session,
            "choose_record",
            selected = next_record,
            choices = values$subset_records,
            server = TRUE
          )
        }
      }
    }
  })
  observeEvent(input$ab_update_redcap, {
    # values$project <- values$project$sync() # save_datasets no
    values$project <- tryCatch({
      load_project(project_name = input$choose_project)$sync(save_datasets = FALSE)$save()$.internal
    }, error = function(e) {
      NULL
    })
  })
  # redcap links -----
  output$redcap_links <- renderUI({
    if (is_something(input$choose_record) &&
        (length(input$choose_record) > 0L)) {
      IF <- shinydashboard::menuItem(
        text = paste0("REDCap Record (", input$choose_record, ")"),
        icon = shiny::icon("file-lines"),
        href = paste0(
          values$project$links$redcap_base,
          "redcap_v",
          values$project$redcap$version,
          "/DataEntry/record_home.php?pid=",
          values$project$redcap$project_id,
          "&arm=1&id=",
          input$choose_record
        )
      )
    } else {
      IF <- NULL
    }
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem(
        text = "REDCap Home",
        icon = shiny::icon("home"),
        href = values$project$links$redcap_home
      ),
      shinydashboard::menuItem(
        text = "REDCap Records",
        icon = shiny::icon("database"),
        href = values$project$links$redcap_record_home
      ),
      IF,
      shinydashboard::menuItem(
        text = "REDCap API",
        icon = shiny::icon("key"),
        href = values$project$links$redcap_API
      ),
      shinydashboard::menuItem(
        text = "REDCap API PG",
        icon = shiny::icon("laptop-code"),
        href = values$project$links$redcap_API_playground
      )
    )
  })
  if (golem::app_dev()) {
    mod_list_server("input_list", values = input)
    mod_list_server("values_list", values = values)
  }
  # plotly -----------
  output$parcats <- plotly::renderPlotly({
    if (is_something(input$choose_form)) {
      DF <- values$dataset$data[[input$choose_form]]
    }
    input$shuffle_colors
    # message_dev(input$choose_fields_cat)
    # cols <- vec1_in_vec2(input$choose_fields_cat,colnames(DF))
    # message_dev(cols)
    # # fields_to_forms
    if (length(input$choose_fields_cat) > 0L) {
      cols <- input$choose_fields_cat |> vec1_in_vec2(colnames(DF))
      x <- DF[, cols, drop = FALSE] |>
        REDCapSync:::clean_form(
          fields = values$dataset$metadata$fields,
          labelled = input$labelled,
          drop_blanks = TRUE
        ) |>
        plotly_parcats(remove_missing = !input$render_missing)
      return(x)
    }
  })
  output$survival <- renderPlot({
    if (is_something(input$choose_form)) {
      DF <- values$dataset$data[[input$choose_form]] |>
        REDCapSync:::clean_form(
          fields = values$dataset$metadata$fields,
          labelled = input$labelled,
          drop_blanks = TRUE
        )
    }
    date_fields <- get_field_names_date(values$dataset)
    if (length(date_fields) > 0L) {
      strat_col <- NULL
      if (!is.null(input$choose_split)) {
        if (input$choose_split != "no_choice") {
          strat_col <- input$choose_split
        }
      }
      DF$status_col2 <- as.integer(DF[[input$choose_survival_status_col]] == input$choose_survival_status_choice)
      x <- make_survival(
        DF,
        start_col = input$choose_survival_start_col,
        end_col = input$choose_survival_end_col,
        status_col = "status_col2",
        units = input$survival_units,
        strat_col = strat_col
      )
      return(x)
    }
  })
}
