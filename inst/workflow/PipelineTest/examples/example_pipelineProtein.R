library(omXplore)
library(shiny)
library(shinyjs)
library(shinyBS)
library(bs4Dash)
library(highcharter)
library(MagellanNTK)
library(Prostar2)

data(Exp1_R25_prot, package = 'DaparToolshedData')
wf.name <- 'PipelineProtein'
wf.path <- system.file('workflow/PipelineProtein', package = 'Prostar2')
MagellanNTK(Exp1_R25_prot, wf.path, wf.name)

