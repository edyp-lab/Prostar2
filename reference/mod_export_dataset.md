# Export dataset Shiyny app

A shiny Module.

## Usage

``` r
export_dataset_ui(id)

export_dataset_server(id, dataIn)

export_dataset(dataIn)
```

## Arguments

- id:

  xxx

- dataIn:

  xxx

## Value

A list

## Examples

``` r
if (interactive()){
data(lldata)
shiny::runApp(export_dataset(lldata))
}
```
