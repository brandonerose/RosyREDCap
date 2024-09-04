#' @import RosyUtils
#' @import shiny
#' @import shinydashboard
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
            htmlwidgets::shinyWidgetOutput("REDCap_diagram")
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
      )
    ),
    uiOutput("redcap_links"),
    TCD_SBF()
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
