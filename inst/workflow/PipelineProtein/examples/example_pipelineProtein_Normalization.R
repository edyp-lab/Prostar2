library(omXplore)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinydashboard)
library(shinydashboardPlus)
library(highcharter)
library(MagellanNTK)
library(Prostar2)

options(shiny.fullstacktrace = TRUE)

data(Exp1_R25_prot, package = 'DaparToolshedData')
obj <- Exp1_R25_prot
wf.name <- 'PipelineProtein_Normalization'
wf.path <- system.file('workflow/PipelineProtein', package = 'Prostar2')


# Launch in a standalone mode
shiny::runApp(workflowApp(wf.name, wf.path, dataIn = obj))


# Launch in the Magellan workspace
MagellanNTK(Exp1_R25_prot, wf.path, wf.name)

