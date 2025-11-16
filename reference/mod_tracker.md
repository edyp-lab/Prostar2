# Tracking of entities within plots

This shiny module offers a UI to select a subset of a dataset and
superimpose quantitative values of this selection on the complete plot
Three modes of selection are implemented:

- 'Protein list': xxx,

- 'Random': xxx,

- 'Specific column': xxx

## Usage

``` r
mod_tracker_ui(id)

mod_tracker_server(
  id,
  object = reactive({
     NULL
 }),
  remoteReset = reactive({
     0
 }),
  is.enabled = reactive({
     TRUE
 })
)

mod_tracker(dataIn)
```

## Arguments

- id:

  shiny id

- object:

  A instance of the class `SummarizedExperiment`

- remoteReset:

  xxx

- is.enabled:

  xxx

- dataIn:

  xxx

## Value

NA

A [`list()`](https://rdrr.io/r/base/list.html) of integers

## Examples

``` r
if (interactive()){
data(Exp1_R25_prot, package = "DaparToolshedData")
shiny::runApp(mod_tracker(Exp1_R25_prot))
}
```
