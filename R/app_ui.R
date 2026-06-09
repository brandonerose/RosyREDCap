#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(includeCSS(
      system.file(package = "table1", "table1_defaults_1.0/table1_defaults.css")
    )),
    tags$script(
      HTML(
        "
    $(document).on('keydown', function(e) {
      var keyCode = e.keyCode || e.which;
      if (keyCode == 13) {  // Enter key code is 13
        $('.dataTable input').blur();  // Trigger input blur to save edit
      }
    });
  "
      )
    ),
    tags$script(
      HTML(
        "
  $(document).on('input', '#user_adds_project_short_name', function() {
    let val = this.value.toUpperCase();
    // keep only valid chars
    val = val.replace(/[^A-Z0-9_]/g, '');
    // enforce: must start with a letter
    if (val.length > 0 && !/^[A-Z]/.test(val)) {
      val = val.replace(/^[^A-Z]+/, '');
    }
    this.value = val;
  });
"
      )
    ),
    tags$script(
      HTML(
        "
  $(document).on('keydown', '#user_adds_project_api_token, #user_adds_project_redcap_uri, #user_adds_project_dir_path', function(e) {
    // block space key
    if (e.which === 32 || e.keyCode === 32) {
      e.preventDefault();
      return false;
    }
  });
"
      )
    ),
    tags$style(
      HTML(
        "
  #user_adds_project_short_name {
    text-transform: uppercase;
  }
"
      )
    ),
    shinydashboardPlus::dashboardPage(
      options = list(sidebarExpandOnHover = FALSE),
      header = dbHeader(),
      sidebar = dbSidebar(
        menuItem(
          text = "Projects",
          tabName = "home",
          icon = shiny::icon("home")
        ),
        conditionalPanel(
          "input.sb1 === 'home'",
          actionBttn("user_adds_project_modal", "Add New Project"),
          switchInput(
            inputId = "test_mode",
            label = "TEST Mode?",
            value = get_golem_options("test_mode")
          )
        ),
        uiOutput("choose_project_"),
        menuItem(
          text = "Project",
          tabName = "project",
          icon = shiny::icon("user-doctor")
        ),
        conditionalPanel(
          "input.sb1 === 'project'",
          actionButton("ab_update_redcap", "Update REDCap!")
        ),
        menuItem(
          text = "Data",
          tabName = "data",
          icon = shiny::icon("chart-bar")
        ),
        conditionalPanel(
          "input.sb1 === 'data' || input.sb1 === 'group' || input.sb1 === 'record'",
          uiOutput("choose_form_"),
          uiOutput("choose_group_"),
          switchInput(
            inputId = "deidentify_switch",
            label = "Exclude Identifiers",
            value = TRUE
          ),
          switchInput(
            inputId = "exclude_free_text_switch",
            label = "Exclude Free Text",
            value = TRUE
          ),
          uiOutput("transformation_switch_"),
          switchInput(
            inputId = "labelled",
            label = "Labelled",
            value = TRUE
          ),
          uiOutput("filter_switch_")
        ),
        menuItem(
          text = "Group",
          tabName = "group",
          icon = shiny::icon("users")
        ),
        menuItem(
          text = "Record",
          tabName = "record",
          icon = shiny::icon("user-large")
        ),
        uiOutput("choose_record_"),
        # conditionalPanel(
        #   "input.sb1 === 'record'",
        #   actionButton("ab_random_record","Random Record!"),
        #   switchInput(
        #     inputId = "view_switch_text",
        #     onLabel = "Text",
        #     offLabel = "Tables",
        #     value = TRUE
        #   )
        # ),
        uiOutput("redcap_links")
      ),
      body = dbBody(
        # home--------
        tabItem("home", fluidRow(
          box(
            title = h1("Home"),
            width = 12L,
            DT::DTOutput("projects_table")
          ),
        )),
        # project--------
        tabItem(
          "project",
          fluidRow(
            shinydashboardPlus::box(
              title = "REDCap Metadata Network",
              closable = FALSE,
              width = 12L,
              height = "800px",
              solidHeader = TRUE,
              collapsible = TRUE,
              sidebar = shinydashboardPlus::boxSidebar(
                id = "mycardsidebar",
                width = 25,
                awesomeCheckbox(
                  inputId = "metadata_graph_duplicate_forms",
                  label = "Duplicate Forms?",
                  value = TRUE
                ),
                awesomeCheckbox(
                  inputId = "metadata_graph_clean_names",
                  label = "Clean Variable Names",
                  value = TRUE
                ),
                awesomeCheckbox(
                  inputId = "metadata_graph_include_fields",
                  label = "Include Fields?",
                  value = FALSE
                ),
                awesomeCheckbox(
                  inputId = "metadata_graph_include_choices",
                  label = "Include Choices?",
                  value = FALSE
                ),
                awesomeCheckbox(
                  inputId = "metadata_graph_hierarchical",
                  label = "Hierarchical?",
                  value = FALSE
                ),
                selectizeInput(
                  inputId = "metadata_graph_direction",
                  label = "Graph Direction (if hierarchical)",
                  choices = c("LR", "UD", "RL", "DU"),
                  selected = "LR"
                ),
                awesomeCheckbox(
                  inputId = "metadata_graph_allow_zoom",
                  label = "Allow Zoom?",
                  value = TRUE
                )
              ),
              tabBox(
                id = "metadata_graph_tabs",
                width = 12L,
                height = "800px",
                tabPanel(
                  "visNetwork",
                  visNetwork::visNetworkOutput("REDCap_diagram_vis", width = "100%", height = "600px")
                ),
                tabPanel(
                  "DiagrammeR",
                  DiagrammeR::grVizOutput("REDCap_diagram_dia")
                )
              )
            ),
            box(
              title = h1("Instruments"),
              width = 12L,
              DT::DTOutput("forms_table")
            ),
            box(
              title = h1("Metadata"),
              width = 12L,
              DT::DTOutput("metadata_table")
            ),
            box(
              title = h1("Codebook"),
              width = 12L,
              DT::DTOutput("codebook_table")
            ),
            box(
              title = h1("Users"),
              width = 12L,
              DT::DTOutput("user_table")
            ),
            box(
              title = h1("Log"),
              width = 12L,
              DT::DTOutput("log_table")
            )
          )
        ),
        # data --------
        tabItem("data", fluidRow(box(
          title = h1("Data Tables"),
          width = 12L,
          uiOutput("dt_tables_view")
        ))),
        # group--------
        tabItem(
          "group",
          fluidRow(box(width = 6L, uiOutput("choose_split_")), box(
            width = 6L, uiOutput("choose_fields_cat_")
          )),
          fluidRow(box(
            title = "Other Options",
            width = 12L,
            awesomeCheckbox(
              inputId = "render_missing",
              label = "Include Missing/Unkown",
              value = FALSE
            )
          )),
          shinydashboardPlus::box(
            title = "Parallel Categories",
            closable = FALSE,
            width = 12L,
            height = "700px",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotly::plotlyOutput("parcats"),
            actionButton("shuffle_colors", "Shuffle Colors")
          ),
          shinydashboardPlus::box(
            title = "Table1",
            closable = FALSE,
            width = 12L,
            solidHeader = TRUE,
            collapsible = TRUE,
            uiOutput("table1"),
            actionButton("table_save_ab", "Save table1")
          ),
          shinydashboardPlus::box(
            title = "Survival Curve",
            closable = FALSE,
            width = 12L,
            height = "800px",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            sidebar = shinydashboardPlus::boxSidebar(
              id = "survival_sidebar",
              width = 25,
              startOpen = TRUE,
              uiOutput("choose_survival_start_col_"),
              uiOutput("choose_survival_end_col_"),
              uiOutput("choose_survival_status_col_"),
              uiOutput("choose_survival_status_choice_"),
              selectInput(
                inputId = "survival_units",
                label = "Units",
                choices = c("days", "months", "years"),
                selected = "years"
              ),
              uiOutput("choose_survival_xlim_")
            ),
            box(
              width = 12L,
              plotOutput("survival", height = "700px"),
              actionButton("survival_save_ab", "Save Plot File")
            )
          ),
          shinydashboardPlus::box(
            title = "Bar Plot",
            closable = FALSE,
            width = 12L,
            height = "700px",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            h2("Bar Plot Placeholder!")
          ),
          shinydashboardPlus::box(
            title = "Scatter Plot",
            closable = FALSE,
            width = 12L,
            height = "700px",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            h2("Scatter Plot Placeholder!")
          ),
          shinydashboardPlus::box(
            title = "Swimmer Plot",
            closable = FALSE,
            width = 12L,
            height = "700px",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            h2("Swimmer Plot Placeholder!")
          )
        ),
        # record--------
        tabItem("record", fluidRow(
          box(
            title = h1("View"),
            width = 4L,
            switchInput(
              inputId = "view_switch_text",
              onLabel = "Text",
              offLabel = "Tables",
              value = TRUE
            ),
            awesomeCheckbox(
              inputId = "sidebar_choice_radio",
              label = "Dropdown instead of radio",
              value = FALSE
            ),
            actionButton("ab_random_record", "Random Record!"),
            actionButton("ab_next_record", "Next Record!"),
            uiOutput("choose_fields_view_"),
            uiOutput("dt_tables_view_records")
          ),
          box(
            width = 8L,
            title = "Change",
            uiOutput("choose_fields_change_"),
            uiOutput("fields_to_change_dynamic_inputs"),
            uiOutput("add_input_instance_ui_"),
            actionButton("reset_data_values", "Reset Data"),
            actionButton("submit_data_values", "Pend For Upload")
          )
        ), fluidRow(
          box(
            width = 12L,
            title = "Upload",
            h3("Below is what will be uploaded to REDCap!"),
            DT::DTOutput("the_uploading_table"),
            actionButton("submit_data_values2", "Upload to REDCap")
          )
        ))
      ),
      controlbar = dbControlbar(
        awesomeCheckbox(
          inputId = "allow_multiple_groups",
          label = "Allow Multiple Groups",
          value = FALSE
        )
      ),
      footer = TCD_NF(),
      skin = "black"
    )
  )
}
