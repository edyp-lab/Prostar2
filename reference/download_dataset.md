# dl

A shiny Module.

## Usage

``` r
download_dataset_ui(id)

download_dataset_server(
  id,
  dataIn = reactive({
     NULL
 }),
  extension = c("xlsx", "qf"),
  widget.type = "Link",
  filename = "myDataset",
  excel.style = NULL,
  remoteReset = reactive({
     0
 }),
  is.enabled = reactive({
     TRUE
 })
)

download_dataset(dataIn, filename = "myDataset")
```

## Arguments

- id:

  internal

- dataIn:

  internal

- extension:

  Available values are `csv` (default), `qf` and `Excel`.

- widget.type:

  Available values are `Button` and `Link` (default).

- filename:

  internal

- excel.style:

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
data(Exp1_R25_prot, package = "DaparToolshedData")
shiny::runApp(download_dataset(Exp1_R25_prot))
}
```
