library(MagellanNTK)
library(highcharter)
library(DaparToolshed)
library(Prostar2)
library(omXplore)
library(DaparToolshed)

history <- list()
data(Exp1_R25_prot, package = "DaparToolshedData")
obj <- Exp1_R25_prot
# Simulate imputation of missing values
obj <- NAIsZero(obj, 1)
obj <- NAIsZero(obj, 2)
qData <- as.matrix(assay(obj[[2]]))
sTab <- MultiAssayExperiment::colData(obj)
limma <- limmaCompleteTest(qData, sTab, comp.type = "OnevsAll")
df <- cbind(limma$logFC, limma$P_Value)
new.dataset <- obj[[length(obj)]]
HypothesisTest(new.dataset) <- as.data.frame(df)
history[['HypothesisTest_thlogFC']] <- as.numeric(1)
paramshistory(new.dataset) <- history


obj <- Prostar2::addDatasets(obj, new.dataset, 'HypothesisTest')
path <- system.file('workflow/PipelinePeptide', package = 'Prostar2')
shiny::runApp(workflowApp("PipelinePeptide_DA", path, dataIn = obj))
