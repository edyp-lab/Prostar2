# popover_for_help_ui and popover_for_help_server

A shiny Module.

## Usage

``` r
mod_DetQuantImpValues_ui(id)

mod_DetQuantImpValues_server(
  id,
  dataIn = reactive({
     NULL
 }),
  quant = reactive({
     1
 }),
  factor = reactive({
     1
 }),
  remoteReset = reactive({
     0
 }),
  is.enabled = reactive({
     TRUE
 })
)

mod_DetQuantImpValues(obj)
```

## Arguments

- id:

  xxx

- dataIn:

  xxx

- quant:

  xxx

- factor:

  xxx

- remoteReset:

  xxx

- is.enabled:

  xxx

- obj:

  xxx

## Examples

``` r
if (interactive()){
data(ft_na)
shiny::runApp(mod_DetQuantImpValues(ft_na[[1]]))
}
```
