# dl

A shiny Module.

## Usage

``` r
build_report_ui(id)

build_report_server(
  id,
  dataIn = reactive({
     NULL
 }),
  filename = "myDataset"
)

build_report(dataIn, filename = "myDataset")
```

## Arguments

- id:

  internal

- dataIn:

  internal

- filename:

  xxx

## Value

NA

## Examples

``` r
if (interactive()){
data(sub_R25)
shiny::runApp(build_report(sub_R25))

shiny::runApp(build_report(sub_R25, filename = "myDataset"))
}
```
