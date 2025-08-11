library(omXplore)
library(shiny)
library(waiter)
library(shinyBS)
library(highcharter)
library(MagellanNTK)
library(Prostar2)
library(DaparToolshed)
library(bs4Dash)
library(shinyFeedback)
library(bslib)

data(Exp1_R25_prot, package = 'DaparToolshedData')
wf.name <- 'PipelineProtein'
wf.path <- system.file('workflow/PipelineProtein', package = 'Prostar2')
MagellanNTK(Exp1_R25_prot, wf.path, wf.name)

