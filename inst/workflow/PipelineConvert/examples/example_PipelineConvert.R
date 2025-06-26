library(omXplore)
library(shiny)
library(shinyjs)
library(shinyBS)
library(highcharter)
library(MagellanNTK)
library(Prostar2)
library(DaparToolshed)
library(bs4Dash)


data(Exp1_R25_prot, package = 'DaparToolshedData')
obj <- Exp1_R25_prot
wf.name <- 'PipelineConvert_Convert'
wf.path <- system.file('workflow/PipelineConvert', package = 'Prostar2')



shiny::runApp(workflowApp("PipelineConvert_Convert", wf.path, dataIn = obj))


MagellanNTK(obj, wf.path, wf.name)
