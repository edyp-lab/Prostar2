# infos_dataset_ui and infos_dataset_server

A shiny Module.

## Usage

``` r
infos_dataset_ui(id)

infos_dataset_server(
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

infos_dataset(obj)
```

## Arguments

- id:

  shiny id

- dataIn:

  An instance of the class `QFeatures`.

## Value

A shiny app

## Examples

``` r
if (interactive()){
data(Exp1_R25_prot, package = "DaparToolshedData")
shiny::runApp(infos_dataset(Exp1_R25_prot))
}
```
