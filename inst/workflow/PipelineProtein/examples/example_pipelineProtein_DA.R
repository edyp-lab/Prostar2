library(omXplore)
library(shiny)
library(waiter)
library(shinyjs)
library(shinyBS)
library(highcharter)
library(MagellanNTK)
library(Prostar2)
library(DaparToolshed)
library(bs4Dash)
library(shinyFeedback)
library(QFeatures)

options(shiny.fullstacktrace = TRUE)

history <- list()
data(Exp1_R25_prot, package = "DaparToolshedData")
obj <- Exp1_R25_prot
# Simulate imputation of missing values
obj <- NAIsZero(obj, 1)
obj <- NAIsZero(obj, 2)
qData <- as.matrix(assay(obj[[2]]))
sTab <- colData(obj)
limma <- limmaCompleteTest(qData, sTab, comp.type = "OnevsAll")
df <- cbind(limma$logFC, limma$P_Value)
new.dataset <- obj[[length(obj)]]
HypothesisTest(new.dataset) <- as.data.frame(df)
history[['HypothesisTest_thlogFC']] <- as.numeric(1)
paramshistory(new.dataset) <- history


obj <- Prostar2::addDatasets(obj, new.dataset, 'HypothesisTest')
path <- system.file('workflow/PipelineProtein', package = 'Prostar2')

proc_workflowApp("PipelineProtein_DA", path, dataIn = obj)

