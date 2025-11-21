library(omXplore)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(highcharter)
library(MagellanNTK)
library(Prostar2)
library(QFeatures)

options(shiny.fullstacktrace = TRUE)

data(Exp1_R25_prot, package = 'DaparToolshedData')
obj <- Exp1_R25_prot
wf.name <- 'PipelineProtein_Normalization'
wf.path <- system.file('workflow/PipelineProtein', package = 'Prostar2')


# Launch in a standalone mode
proc_workflowApp(wf.name, wf.path, dataIn = Exp1_R25_prot)


# Launch in the Magellan workspace
MagellanNTK(Exp1_R25_prot, wf.path, wf.name)

