# Help popover windows

A shiny Module.

## Usage

``` r
mod_inputGroup_ui(id)

mod_inputGroup_server(
  id,
  df = reactive({
     NULL
 }),
  quantCols = reactive({
     NULL
 }),
  remoteReset = reactive({
     0
 }),
  is.enabled = reactive({
     TRUE
 })
)

mod_inputGroup()
```

## Arguments

- id:

  A `character(1)` xxx

- df:

  xxx

- quantCols:

  A vector of

- remoteReset:

  xxx

- is.enabled:

  xxx

## Value

A shiny app

## Examples

``` r
if (interactive()){
shiny::runApp(mod_inputGroup())
}
```
