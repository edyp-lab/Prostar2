# mod_open_demo_dataset_ui and mod_open_demo_dataset_server

A shiny Module.

## Usage

``` r
open_demoDataset_ui(id)

open_demoDataset_server(
  id,
  remoteReset = reactive({
     0
 }),
  is.enabled = reactive({
     TRUE
 })
)

open_demoDataset()
```

## Arguments

- id:

  xxx

## Examples

``` r
if (interactive()){
shiny::runApp(open_demoDataset())
}
```
