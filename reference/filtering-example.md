# Filtering example Shiny module

xxxx

## Usage

``` r
mod_filtering_example_ui(id)

mod_filtering_example_server(
  id,
  dataIn = reactive({
     NULL
 }),
  indices = NULL,
  operation = "keep",
  title = "myTitle",
  remoteReset = reactive({
     0
 }),
  is.enabled = reactive({
     TRUE
 })
)

mod_filtering_example(
  obj,
  indices = NULL,
  operation = "keep",
  title = "myTitle"
)
```

## Arguments

- id:

  xxx

- dataIn:

  xxx

- indices:

  xxx

- operation:

  A character(1) that indicates whether to keep or remove lines
  identified by indices. Available values are 'keep' (default) or
  'delete'

- title:

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
obj <- Exp1_R25_prot[[1]]
indices <- 1:5
operation <- "delete"
shiny::runApp(mod_filtering_example(obj, indices, operation))
}
```
