library(shiny)
library(shinydashboard)
setwd("~/TELETRAVAIL/github_DAPARforFeatures/Prostar2/dev/prostar_NvCSS/")

ui <- dashboardPage(
  
  
  
  dashboardHeader(title="Prostar",
                  tags$li(class="dropdown",
                          a(href="http://www.prostar-proteomics.org/",
                            img(src="logo.png",
                                title="Prostar website",
                                height="17px"))),
                  tags$li(class="dropdown",
                          a(href="https://github.com/samWieczorek/Prostar2",
                            icon("github"),
                            title="GitHub"))),
  
  dashboardSidebar(
    sidebarMenu(
      br(),
      menuItem("Data Manager", icon = icon("folder"), startExpanded = TRUE,
               menuSubItem("Import Data", tabName = "import", selected = TRUE), #"Open QFeature file","Convert Data","Demo data"
               menuSubItem("Export Results", tabName = "export"),
               menuSubItem("Reload Prostar", tabName = "reload")),
      hr(),
      menuItem("Global Settings", tabName = "globalSettings", icon = icon("cogs")),
      menuItem("Release Notes", tabName = "releaseNotes", icon = icon("clipboard")),
      menuItem("Check for Updates", tabName = "updates", icon = icon("wrench")),
      menuItem("Help", icon = icon("question-circle"),
               menuSubItem("Useful Links", tabName = "usefulLinks"),
               menuSubItem("FAQ", tabName = "faq"),
               menuSubItem("Bug Report", tabName = "bugReport")),
      hr()
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    )
  )
  
)

server <- function(input, output) { }

shinyApp(ui, server)