# xxxx

xxxx

## Usage

``` r
mod_buildDesign_ui(id)

mod_buildDesign_server(
  id,
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

mod_buildDesign(quantCols)
```

## Arguments

- id:

  xxx

- quantCols:

  xxx

- remoteReset:

  xxx

- is.enabled:

  xxx

## Value

NA

## Examples

``` r
if (interactive()){
shiny::runApp(mod_buildDesign(letters[seq(6)]))
}
```
