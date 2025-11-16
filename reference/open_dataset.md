# open_dataset_ui and open_dataset_server

A shiny Module.

## Usage

``` r
open_dataset_ui(id)

open_dataset_server(
  id,
  remoteReset = reactive({
     0
 }),
  is.enabled = reactive({
     TRUE
 })
)

open_dataset()
```

## Arguments

- id:

  xxx

## Examples

``` r
if (interactive()){
shiny::runApp(Prostar2::open_dataset())
}
```
