# Wrapper to Convert pipeline

These functions are inspired by the functions run_workflow() in the
package `MagellanNTK`. They wrap the entire workflow into a single
function

## Usage

``` r
convert_dataset_ui(id)

convert_dataset_server(
  id,
  remoteReset = reactive({
     NULL
 }),
  is.enabled = reactive({
     TRUE
 })
)

convert_dataset()
```

## Arguments

- id:

  xxx

- remoteReset:

  xxx

- is.enabled:

  xxx

## Examples

``` r
if (interactive()){
library(DaparToolshed)
library(Prostar2)
library(spsComps)
shiny::runApp(Prostar2::convert_dataset())
}
```
