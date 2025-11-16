# popover_for_help_ui and popover_for_help_server

A shiny Module.

## Usage

``` r
mod_volcanoplot_ui(id)

mod_volcanoplot_server(
  id,
  dataIn = reactive({
     NULL
 }),
  comparison = reactive({
     NULL
 }),
  group = reactive({
     NULL
 }),
  thlogfc = reactive({
     0
 }),
  thpval = reactive({
     0
 }),
  tooltip = reactive({
     NULL
 }),
  remoteReset = reactive({
     0
 }),
  is.enabled = reactive({
     TRUE
 })
)

volcanoplot(dataIn, comparison, group, thlogfc, thpval, tooltip = NULL)
```

## Arguments

- id:

  xxx

- dataIn:

  An instance of the class `SummarizedExperiment` containing results of
  hypothesis testing stored as a data.frame in its metadata

- comparison:

  xxx

- group:

  xxx

- thlogfc:

  xxxx

- thpval:

  xxx

- tooltip:

  xxx

- remoteReset:

  xxx

- is.enabled:

  xxx

## Examples

``` r
if (interactive()){
library(highcharter)
library(DaparToolshed)
library(SummarizedExperiment)
data(Exp1_R25_prot, package = "DaparToolshedData")
obj <- Exp1_R25_prot
# Simulate imputation of missing values
obj <- DaparToolshed::NAIsZero(obj, 1)
obj <- DaparToolshed::NAIsZero(obj, 2)
qData <- as.matrix(SummarizedExperiment::assay(obj[[2]]))
sTab <- colData(obj)
limma <- limmaCompleteTest(qData, sTab)

df <- data.frame(
  x = limma$logFC[["25fmol_vs_10fmol_logFC"]],
  y = -log10(limma$P_Value[["25fmol_vs_10fmol_pval"]]),
  index = as.character(rownames(obj[[2]]))
)
colnames(df) <- c("x", "y", "index")
tooltipSlot <- c("Fasta_headers", "Sequence_length")
df <- cbind(df, colData(obj[[2]])[, tooltipSlot])
colnames(df) <- gsub(".", "_", colnames(df), fixed = TRUE)
if (ncol(df) > 3) {
  colnames(df)[seq.int(from = 4, to = ncol(df))] <-
    paste("tooltip_", colnames(df)[seq.int(from = 4, to = ncol(df))],
      sep = ""
    )
}
hc_clickFunction <- JS("function(event) {
Shiny.onInputChange('eventPointClicked',
[this.index]+'_'+ [this.series.name]);}")
cond <- c("25fmol", "10fmol")
diffAnaVolcanoplot_rCharts(
  df,
  th_pval = 2.5,
  th_logfc = 1,
  conditions = cond,
  clickFunction = hc_clickFunction
)
shiny::runApp(volcanoplot(xxxx))
}
```
