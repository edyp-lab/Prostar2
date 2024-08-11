library(omXplore)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinydashboard)
library(shinydashboardPlus)
library(highcharter)
library(MagellanNTK)
library(Prostar2)

data(Exp1_R25_prot, package = 'DaparToolshedData')
wf.name <- 'PipelineConvert_Convert'
wf.path <- system.file('workflow/PipelineConvert', package = 'Prostar2')
MagellanNTK(Exp1_R25_prot, wf.path, wf.name)

