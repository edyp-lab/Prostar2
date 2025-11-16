# Displays a correlation matrix of the quantitative data of a numeric matrix.

xxxx

## Usage

``` r
mod_metacell_tree_ui(id)

mod_metacell_tree_server(
  id,
  dataIn = reactive({
     NULL
 }),
  remoteReset = reactive({
     0
 }),
  is.enabled = reactive({
     TRUE
 })
)

mod_metacell_tree(
  dataIn,
  remoteReset = reactive({
     0
 })
)
```

## Arguments

- id:

  xxx

- dataIn:

  An instance of the class `SummarizedExperiment`

- remoteReset:

  xxx

- is.enabled:

  xxx

## Value

NA

## Examples

``` r
if (interactive()){
data(Exp1_R25_pept, package = "DaparToolshedData")
shiny::runApp(mod_metacell_tree(Exp1_R25_pept[[1]]))

data(Exp1_R25_prot, package = "DaparToolshedData")
shiny::runApp(mod_metacell_tree(Exp1_R25_prot[[1]]))
}
```
