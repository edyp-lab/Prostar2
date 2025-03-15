library(MagellanNTK)
library(highcharter)
library(DaparToolshed)
library(Prostar2)
library(omXplore)
library(DaparToolshed)

history <- list()
data(Exp1_R25_pept, package = "DaparToolshedData")
obj <- Exp1_R25_pept
path <- system.file('workflow/PipelinePeptide', package = 'Prostar2')
shiny::runApp(workflowApp("PipelinePeptide_Imputation", path, dataIn = obj))
