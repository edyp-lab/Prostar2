library(MagellanNTK)
library(highcharter)
library(DaparToolshed)
library(Prostar2)
library(omXplore)
library(DaparToolshed)

path <- system.file('workflow/PipelinePeptide', package = 'Prostar2')
history <- list()
data(Exp1_R25_pept, package = "DaparToolshedData")

obj <- Exp1_R25_pept
shiny::runApp(workflowApp("PipelinePeptide_Imputation", path, dataIn = obj))

filter <- FunctionFilter('qMetacellWholeLine', cmd = 'delete', pattern = c("Missing","Missing POV", "Missing MEC"))
tmp <- filterFeaturesOneSE(obj, 2, 'test', list(filter))
shiny::runApp(workflowApp("PipelinePeptide_Imputation", path, dataIn = tmp))


