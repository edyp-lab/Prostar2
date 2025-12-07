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

obj <- NULL
wf.name <- 'PipelineConvert'
wf.path <- system.file('workflow/PipelineConvert', package = 'Prostar2')
MagellanNTK(obj, wf.path, wf.name)

