#' @import RosyApp
#' @import shiny
#' @import shinydashboard
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui<- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(
      includeCSS(system.file(package="table1", "table1_defaults_1.0/table1_defaults.css"))
    ),
    tags$script(HTML("
    $(document).on('keydown', function(e) {
      var keyCode = e.keyCode || e.which;
      if (keyCode == 13) {  // Enter key code is 13
        $('.dataTable input').blur();  // Trigger input blur to save edit
      }
    });
  ")),
    shinydashboardPlus::dashboardPage(
      options = list(
        sidebarExpandOnHover = F
      ),
      header = dbHeader(),
      sidebar = dbSidebar(
        menuItem(
          text="Projects",
          tabName = "home",
          icon =shiny::icon("home")
        ),
        conditionalPanel(
          "input.sb1 === 'home'",
          uiOutput("choose_project_"),
        ),
        menuItem(
          text="Project",
          tabName = "project",
          icon =shiny::icon("user-doctor")
        ),
        conditionalPanel(
          "input.sb1 === 'project'",
          selectizeInput(
            "metadata_graph_type",
            label = "Graph Type",
            choices = c("visNetwork","DiagrammeR"),
            selected = "DiagrammeR"
          ),
          actionButton("ab_update_redcap","Update REDCap!"),
          shinyWidgets::awesomeCheckbox(
            inputId = "metadata_graph_include_vars",
            label = "Include Variables?",
            value = F
          ),
          shinyWidgets::awesomeCheckbox(
            inputId = "metadata_graph_duplicate_forms",
            label = "Duplicate Forms?",
            value = T
          )
        ),
        menuItem(
          text="Transformation",
          tabName = "transformation",
          icon =shiny::icon("gear")
        ),
        menuItem(
          text="Data",
          tabName = "data",
          icon =shiny::icon("users")
        ),
        conditionalPanel(
          "input.sb1 === 'data' || input.sb1 === 'group' || input.sb1 === 'record'",
          uiOutput("choose_form_"),
          uiOutput("choose_group_"),
          uiOutput("transformation_switch_"),
          uiOutput("filter_switch_")
        ),
        menuItem(
          text="Group",
          tabName = "group",
          icon =shiny::icon("users")
        ),
        conditionalPanel(
          "input.sb1 === 'group' || input.sb1 === 'record'",
          shinyWidgets::awesomeCheckbox(
            inputId = "render_missing",
            label = "Include Missing/Unkown",
            value = F
          ),
          shinyWidgets::awesomeCheckbox(
            inputId = "sidebar_choice_radio",
            label = "Dropdown instead of radio",
            value = F
          )
        ),
        menuItem(
          text="Plot",
          tabName = "plot",
          icon =shiny::icon("chart-bar")
        ),
        menuItem(
          text="Record",
          tabName = "record",
          icon =shiny::icon("user-large")
        ),
        uiOutput("choose_record_"),
        # conditionalPanel(
        #   "input.sb1 === 'record'",
        #   actionButton("ab_random_record","Random Record!"),
        #   shinyWidgets::switchInput(
        #     inputId = "view_switch_text",
        #     onLabel = "Text",
        #     offLabel = "Tables",
        #     value = T
        #   )
        # ),
        menuItem(
          text="Backend",
          tabName = "backend",
          icon =shiny::icon("gear")
        ),
        uiOutput("redcap_links")
      ),
      body = dbBody(
        # home--------
        tabItem(
          "home",
          fluidRow(
            box(
              title = h1("Home"),
              width = 12,
              DT::DTOutput("projects_table")
            )
          )
        ),
        # project--------
        tabItem(
          "project",
          fluidRow(
            box(
              title = h1("Project"),
              width = 12,
              #more summary stuff
              uiOutput("REDCap_diagram_ui_test"),
            ),
            box(
              title = h1("Instruments"),
              width = 12,
              DT::DTOutput("forms_table")
            ),
            box(
              title = h1("Metadata"),
              width = 12,
              DT::DTOutput("metadata_table")
            ),
            box(
              title = h1("Codebook"),
              width = 12,
              DT::DTOutput("codebook_table")
            ),
            box(
              title = h1("Users"),
              width = 12,
              DT::DTOutput("user_table")
            ),
            box(
              title = h1("Log"),
              width = 12,
              DT::DTOutput("log_table")
            )
          )
        ),
        # data --------
        tabItem(
          "data",
          fluidRow(
            box(
              title = h1("Data Tables"),
              width = 12,
              uiOutput("dt_tables_view")
            )
          )
        ),
        # Transformation--------
        tabItem(
          "transformation",
          fluidRow(
            box(
              title = h1("Forms Transformation"),
              width = 12,
              DT::DTOutput("forms_transformation"),
              actionButton("ab_accept_form_transform","Accept Forms Transformation Edit!")
            )
          )
        ),
        # group--------
        tabItem(
          "group",
          fluidRow(
            box(
              width = 6,
              uiOutput("choose_split_")
            ),
            box(
              width = 6,
              uiOutput("choose_fields_cat_")
            )
          ),
          fluidRow(
            box(
              title = h1("Group"),
              width = 12,
              plotly::plotlyOutput("parcats"),
              actionButton("shuffle_colors", "Shuffle Colors")
            )
          ),
          fluidRow(
            box(
              title = h1("Table1"),
              width = 12,
              uiOutput("table1")
            )
          )
        ),
        # plot--------
        tabItem(
          "plot",
          fluidRow(
            box(
              width = 6
              # uiOutput("choose_split_")
            ),
            box(
              width = 6
              # uiOutput("choose_fields_cat_")
            )
          ),
          fluidRow(
            box(
              title = h1("Plot"),
              width = 12,
              # uiOutput("parcats"),
              actionButton("shuffle_colors2", "Shuffle Colors")
            )
          )
        ),
        # record--------
        tabItem(
          "record",
          fluidRow(
            box(
              title = h1("View"),
              width = 6,
              shinyWidgets::switchInput(
                inputId = "view_switch_text",
                onLabel = "Text",
                offLabel = "Tables",
                value = T
              ),
              actionButton("ab_random_record","Random Record!"),
              actionButton("ab_next_record","Next Record!"),
              uiOutput("choose_fields_view_"),
              uiOutput("dt_tables_view_records")
            ),
            box(
              width = 6,
              tabsetPanel(
                tabPanel(
                  title = "Change",
                  uiOutput("choose_fields_change_"),
                  uiOutput("fields_to_change_dynamic_inputs"),
                  fluidRow(uiOutput("add_input_instance_ui_")),
                  fluidRow(actionButton("reset_data_values", "Reset Data")),
                  fluidRow(actionButton("submit_data_values", "Submit Data"))
                ),
                tabPanel(
                  title = "Upload",
                  h3("Below is what will be uploaded to REDCap!"),
                  DT::DTOutput("the_uploading_table")
                )
              )
            )
          )
        )
      ),
      controlbar = dbControlbar(
        shinyWidgets::awesomeCheckbox(
          inputId = "metadata_graph_clean_name",
          label = "Clean Variable Names",
          value = T
        ),
        shinyWidgets::awesomeCheckbox(
          inputId = "allow_multiple_groups",
          label = "Allow Multiple Groups",
          value = F
        )
      ),
      footer = TCD_NF(),
      skin = "black"
    )
  )
}
