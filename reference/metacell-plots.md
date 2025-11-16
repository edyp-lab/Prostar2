# Displays a correlation matrix of the quantitative data of a numeric matrix.

xxxx

## Usage

``` r
mod_ds_metacell_Histos_ui(id)

mod_ds_metacell_Histos_server(
  id,
  dataIn = reactive({
     NULL
 }),
  group = reactive({
     NULL
 }),
  pal = reactive({
     NULL
 }),
  pattern = reactive({
     NULL
 }),
  showSelect = reactive({
     TRUE
 }),
  remoteReset = reactive({
     0
 }),
  is.enabled = reactive({
     TRUE
 })
)

mod_ds_metacell_Histos(obj, group, pal, pattern = NULL, showSelect)
```

## Arguments

- id:

  xxx

- dataIn:

  xxx

- group:

  xxx

- pal:

  xxx

- pattern:

  xxx

- showSelect:

  xxx

- remoteReset:

  xxx

- is.enabled:

  xxx

- obj:

  An instance of the class `SummarizedExperiment`

## Value

NA

## Examples

``` r
if (interactive()){
data(Exp1_R25_prot, package = "DaparToolshedData")
grp <- design.qf(Exp1_R25_prot)$Condition
shiny::runApp(mod_ds_metacell_Histos(Exp1_R25_prot[[1]], group = grp))

# Test with pattern already defined
pat <- c("Missing MEC", "Missing POV")
shiny::runApp(mod_ds_metacell_Histos(Exp1_R25_prot[[1]], pattern = pat, group = grp))
}
```
