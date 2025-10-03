
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinydashboardPlus)
library(MagellanNTK)
library(Prostar2)
library(DaparToolshed)
options(shiny.fullstacktrace = TRUE)


data(Exp1_R25_prot, package = 'DaparToolshedData')
obj <- Exp1_R25_prot
wf.name <- 'PipelineConvert_Convert'
wf.path <- system.file('workflow/PipelineConvert', package = 'Prostar2')

proc_workflowApp(wf.name, wf.path, dataIn = Exp1_R25_prot)

MagellanNTK(obj, wf.path, wf.name)
