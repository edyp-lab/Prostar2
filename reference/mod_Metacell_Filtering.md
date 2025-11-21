# Filtering Shiny module

This function is a shiny module to create a list of queries (instances
of the class `Filtering` to filter the quantitative metadata of an
instance of the class `SummarizedExperiment`). This function is written
with specifications of the package `MagellanNTK` so as to be easily
integrated into workflfow compliant with `MagellanNTK`.

## Usage

``` r
mod_Metacell_Filtering_ui(id)

mod_Metacell_Filtering_server(
  id,
  dataIn = reactive({
     NULL
 }),
  i = reactive({
     1
 }),
  remoteReset = reactive({
     0
 }),
  is.enabled = reactive({
     TRUE
 })
)

mod_Metacell_Filtering(
  dataIn = NULL,
  i = 1,
  remoteReset = reactive({
     0
 }),
  is.enabled = TRUE
)
```

## Arguments

- id:

  xxx

- dataIn:

  xxx

- i:

  xxx

- remoteReset:

  A `ìnteger(1)` xxxx

- is.enabled:

  A `logical(1)` that indicates whether the module is enabled or
  disabled. This is a remote command.

## Value

As for all modules used with `MagellanNTK`, the return value is a
[`list()`](https://rdrr.io/r/base/list.html) of two items:

- trigger : xxx

- value: In this case, it contains a list() of three slots:

  - ll.var: a list() of instances of the class `Filtering`,

  - ll.query: a list of
    [`character()`](https://rdrr.io/r/base/character.html) which
    describe the queries in natural language,

  - ll.widgets.value: a list of the values of widgets.

## Examples

``` r
if (interactive()){
library(Prostar2)
data(Exp1_R25_prot, package = "DaparToolshedData")
shiny::runApp(mod_Metacell_Filtering(Exp1_R25_prot, 1))


data(Exp1_R25_prot, package = "DaparToolshedData")
fun <- FunctionFilter("qMetacellWholeLine",
  cmd = "delete",
  pattern = c("Missing", "Missing POV", "Missing MEC")
)
}
```
