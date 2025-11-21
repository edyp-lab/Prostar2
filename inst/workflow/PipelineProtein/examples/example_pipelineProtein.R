library(omXplore)
library(shiny)
library(waiter)
library(highcharter)
library(MagellanNTK)
library(Prostar2)
library(DaparToolshed)
library(bs4Dash)
library(shinyFeedback)
library(bslib)

data(Exp1_R25_prot, package = 'DaparToolshedData')
obj <- NULL
wf.name <- 'PipelineProtein'
wf.path <- system.file('workflow/PipelineProtein', package = 'Prostar2')
MagellanNTK(obj, wf.path, wf.name)

