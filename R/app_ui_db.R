#' @import RosyUtils
#' @import shiny
#' @import shinydashboard
dbSidebar<-function(){
  shinydashboardPlus::dashboardSidebar(
    minified = F,
    collapsed = F,
    TCD_SBH(),
    sidebarMenu(
      id="sb1",
      menuItem(
        text="Home",
        tabName = "home",
        icon =shiny::icon("home")
      ),
      uiOutput("choose_project"),
      menuItem(
        text="Data",
        tabName = "data",
        icon =shiny::icon("users")
      ),
      conditionalPanel(
        "input.sb1 === 'data'",
        selectInput(
          "data_choice",
          label = "Data Choice",
          choices = c("data_extract","data_transform"),
          selected = "data_extract"
        )
      ),
      menuItem(
        text="Group",
        tabName = "group",
        icon =shiny::icon("users")
      ),
      # conditionalPanel(
      #   "input.sb1 === 'group'",
      uiOutput("choose_split"),
      shinyWidgets::awesomeCheckbox(
        inputId = "render_missing",
        label = "Missing in Table1",
        value = F
        # )
      ),
      menuItem(
        text="Record",
        tabName = "record",
        icon =shiny::icon("user-large")
      ),
      uiOutput("choose_indiv_record"),
      conditionalPanel(
        "input.sb1 === 'record'",
        shinyWidgets::awesomeCheckbox(
          inputId = "sidebar_choice_radio",
          label = "Dropdown instead of radio",
          value = F
        )
      ),
      menuItem(
        text="Metadata",
        tabName = "metadata",
        icon =shiny::icon("gear")
      ),
      conditionalPanel(
        "input.sb1 === 'metadata'",
        selectInput(
          "metadata_graph_type",
          label = "Graph Type",
          choices = c("visNetwork","DiagrammeR"),
          selected = "visNetwork"
        ),
        shinyWidgets::awesomeCheckbox(
          inputId = "metadata_graph_include_vars",
          label = "Include Variables?",
          value = F
        ),
        shinyWidgets::awesomeCheckbox(
          inputId = "metadata_graph_clean_name",
          label = "Clean Name?",
          value = F
        ),
        shinyWidgets::awesomeCheckbox(
          inputId = "metadata_graph_duplicate_forms",
          label = "Duplicate Forms?",
          value = T
        )
      ),
      menuItem(
        text="Users",
        tabName = "users",
        icon =shiny::icon("user-doctor")
      ),
      fluidRow(
        column(
          12,
          actionButton("ab_update_redcap","Update REDCap!"),
          # verbatimTextOutput("testingtext"),
          valueBoxOutput("vb_selected_record",width = 12),
          actionButton("ab_random_record","Random Record!"),
          align="center")
      )
    ),
    uiOutput("redcap_links"),
    TCD_SBF()
  )
}
dbBody<-function(){
  dashboardBody(
    tabItems(
      #home--------
      tabItem(
        "home",
        fluidRow(
          box(
            title = h1("Records"),
            width = 12,
            listviewer::jsoneditOutput("values_list"),
            listviewer::jsoneditOutput("input_list")
          )
        )
      ),
      tabItem(
        "metadata",
        fluidRow(
          box(
            title = h1("Records"),
            width = 12,
            uiOutput("REDCap_diagram_ui")
          )
        )
      ),
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
      #group--------
      tabItem(
        "group"
        # fluidRow(
        #   box(
        #     title = h1("REDCap Log"),
        #     width = 12,
        #     DT::DTOutput("log_table_chosen")
        #   )
        # )
      ),
      #indiv--------
      tabItem(
        "record",
        fluidRow(
          box(
            title = h1("View"),
            width = 6,
            uiOutput("choose_variables_to_view")
            # uiOutput("dt_tables_view")
          ),
          box(
            title = h1("Change"),
            width = 6,
            uiOutput("choose_variables_to_change"),
            uiOutput("variables_to_change_dynamic_inputs"),
            h1("Below is what will be uploaded to REDCap!"),
            DT::DTOutput("the_uploading_table"),
            uiOutput("add_input_instance_ui"),
            actionButton("submit_data_values", "Submit Data")
            # verbatimTextOutput("output_values_change"),
            # verbatimTextOutput("input_changes")
          )
        )
      ),
      #users--------
      tabItem(
        "users",
        fluidRow(
          box(
            title = h1("Users"),
            width = 12
          )
        )
      )
    )
  )
}
dbHeader<-function(){
  shinydashboardPlus::dashboardHeader(
    title = tagList(
      span(class = "logo-lg", .packageName),
      tags$a(
        href="https://thecodingdocs.com",
        target="_blank",
        tags$img(src = "www/logo.png", width="100%")
      )
    )
  )
}
dbControlbar<-function(){
  shinydashboardPlus::dashboardControlbar(
    TCD_SBH(),
    div(style="text-align:center",p(paste0(pkg_name,' Version: ',pkg_version))),
    div(style="text-align:center",p(paste0('Pkg Updated: ',pkg_date))),
    uiOutput("test"),
    TCD_SBF(),
    fluidRow(
      column(
        12,
        p("This app is still in development."),
        p("Consider donating for more."),
        p("Contact with issues."),
        p("Consider using R package."),
        align="center"
      )
    )
  )
}
TCD_SBH<-function(){
  shinydashboard::sidebarMenu(
    shiny::div(
      style="text-align:center",
      tags$a(
        href="https://thecodingdocs.com",
        target="_blank",
        tags$img(src = "www/logo.png", width="50%")
      )
    )
  )
}
TCD_SBF<-function(){
  shinydashboard::sidebarMenu(
    shinydashboard::menuItem(
      text="Donate!",
      icon = shiny::icon("dollar"),
      href="https://account.venmo.com/u/brandonerose"
    ),
    shinydashboard::menuItem(
      text="TheCodingDocs.com",
      icon = shiny::icon("stethoscope"),
      href="https://thecodingdocs.com"
    ),
    shinydashboard::menuItem(
      text="GitHub Code",
      icon = shiny::icon("github"),
      href="https://github.com/brandonerose/"
    ),
    shinydashboard::menuItem(
      text="TheCodingDocs",
      icon = shiny::icon("twitter"),
      href="https://twitter.com/TheCodingDocs"
    ),
    shinydashboard::menuItem(
      text="BRoseMDMPH",
      icon = shiny::icon("twitter"),
      href="https://twitter.com/BRoseMDMPH"
    ),
    shinydashboard::menuItem(
      text="Brandon Rose, MD, MPH",
      icon = shiny::icon("user-doctor"),
      href="https://www.thecodingdocs.com/founder"
    )
  )
  # p(paste0('Version: ',pkg_version)) %>% shiny::div(style="text-align:center"),
  # p(paste0('Last Update: ',pkg_date)) %>% shiny::div(style="text-align:center"),
}
TCD_NF<-function(){
  shinydashboardPlus::dashboardFooter(
    left = fluidRow(
      shiny::actionButton(
        inputId='ab1',
        label="Donate!",
        icon = shiny::icon("dollar"),
        onclick ="window.open('https://account.venmo.com/u/brandonerose', '_blank')") ,
      shiny::actionButton(
        inputId='ab2',
        label="TheCodingDocs.com",
        icon = shiny::icon("stethoscope"),
        onclick ="window.open('https://thecodingdocs.com', '_blank')") ,
      shiny::actionButton(
        inputId='ab3',
        label="GitHub Code",
        icon = shiny::icon("github"),
        onclick =paste0("window.open('https://github.com/brandonerose/",.packageName,"', '_blank')")
      ),
      shiny::actionButton(
        inputId='ab4',
        label="TheCodingDocs",
        icon = shiny::icon("twitter"),
        onclick ="window.open('https://twitter.com/TheCodingDocs', '_blank')"
      ) ,
      shiny::actionButton(
        inputId='ab5',
        label="BRoseMDMPH",
        icon = shiny::icon("square-twitter"),
        onclick ="window.open('https://twitter.com/BRoseMDMPH', '_blank')"
      ) ,
      shiny::actionButton(
        inputId='ab6',
        label="Brandon Rose, MD, MPH",
        icon = shiny::icon("user-doctor"),
        onclick ="window.open('https://www.thecodingdocs.com/founder', '_blank')"
      )
    ) %>% shiny::div(style="text-align:center"),
    right = NULL
  )
}
TCD_SF<-function(){
  shiny::div(
    class = "sticky_footer",
    fluidRow(
      shiny::actionButton(
        inputId='ab1',
        label="Donate!",
        icon = shiny::icon("dollar"),
        onclick ="window.open('https://account.venmo.com/u/brandonerose', '_blank')") ,
      shiny::actionButton(
        inputId='ab2',
        label="TheCodingDocs.com",
        icon = shiny::icon("stethoscope"),
        onclick ="window.open('https://thecodingdocs.com', '_blank')") ,
      shiny::actionButton(
        inputId='ab3',
        label="GitHub Code",
        icon = shiny::icon("github"),
        onclick =paste0("window.open('https://github.com/brandonerose/",.packageName,"', '_blank')")
      ),
      shiny::actionButton(
        inputId='ab4',
        label="TheCodingDocs",
        icon = shiny::icon("twitter"),
        onclick ="window.open('https://twitter.com/TheCodingDocs', '_blank')"
      ) ,
      shiny::actionButton(
        inputId='ab5',
        label="BRoseMDMPH",
        icon = shiny::icon("square-twitter"),
        onclick ="window.open('https://twitter.com/BRoseMDMPH', '_blank')"
      ) ,
      shiny::actionButton(
        inputId='ab6',
        label="Brandon Rose, MD, MPH",
        icon = shiny::icon("user-doctor"),
        onclick ="window.open('https://www.thecodingdocs.com/founder', '_blank')"
      )
    )
  )
}
