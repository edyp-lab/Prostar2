# xxxx

xxxx

## Usage

``` r
mod_designExample_ui(id)

mod_designExample_server(
  id,
  n,
  remoteReset = reactive({
     0
 }),
  is.enabled = reactive({
     TRUE
 })
)

mod_designExample(n = 1)
```

## Arguments

- id:

  xxx

- n:

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
shiny::runApp(mod_designExample(2))
}
```
