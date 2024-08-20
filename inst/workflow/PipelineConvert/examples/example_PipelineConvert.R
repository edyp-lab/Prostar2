library(omXplore)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinydashboard)
library(shinydashboardPlus)
library(highcharter)
library(MagellanNTK)
library(Prostar2)
library(DaparToolshed)


data(Exp1_R25_prot, package = 'DaparToolshedData')
obj <- Exp1_R25_prot
wf.name <- 'PipelineConvert_Convert'
wf.path <- system.file('workflow/PipelineConvert', package = 'Prostar2')



shiny::runApp(workflowApp("PipelineConvert_Convert", wf.path, dataIn = obj))


MagellanNTK(obj, wf.path, wf.name)
