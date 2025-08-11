library(omXplore)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinydashboardPlus)
library(highcharter)
library(MagellanNTK)
library(Prostar2)

data(Exp1_R25_prot, package = 'DaparToolshedData')
wf.name <- 'PipelinePeptide'
wf.path <- system.file('workflow/PipelinePeptide', package = 'Prostar2')
MagellanNTK(Exp1_R25_prot, wf.path, wf.name)

