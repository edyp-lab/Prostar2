library(omXplore)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(highcharter)
library(MagellanNTK)
library(Prostar2)

data(Exp1_R25_prot, package = 'DaparToolshedData')
wf.name <- 'PipelinePeptideSC'
wf.path <- system.file('workflow/PipelinePeptideSC', package = 'Prostar2')
MagellanNTK(Exp1_R25_prot, wf.path, wf.name)

