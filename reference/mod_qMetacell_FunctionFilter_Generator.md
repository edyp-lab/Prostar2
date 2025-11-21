# Build queries for filtering quantitative metadata

This function is a shiny module to create a list of queries (instances
of the class `FunctionFilter` to filter the quantitative metadata of an
instance of the class `SummarizedExperiment`)

## Usage

``` r
mod_qMetacell_FunctionFilter_Generator_ui(id)

mod_qMetacell_FunctionFilter_Generator_server(
  id,
  dataIn = reactive({
     NULL
 }),
  conds,
  keep_vs_remove = reactive({
     setNames(nm = c("delete", "keep"))
 }),
  val_vs_percent = reactive({
     setNames(nm = c("Count", "Percentage"))
 }),
  operator = reactive({
     setNames(nm = SymFilteringOperators())
 }),
  remoteReset = reactive({
     0
 }),
  is.enabled = reactive({
     TRUE
 })
)

mod_qMetacell_FunctionFilter_Generator(
  obj,
  conds,
  keep_vs_remove = setNames(nm = c("delete", "keep")),
  val_vs_percent = setNames(nm = c("Count", "Percentage")),
  operator = setNames(nm = SymFilteringOperators()),
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

  An instance of the class `SummarizedExperiment`

- conds:

  A [`character()`](https://rdrr.io/r/base/character.html) which
  contains the name of the conditions. The length of this vector must be
  equal to the number of samples in the assay (i.e. number of columns in
  assay(obj))

- keep_vs_remove:

  xxx

- val_vs_percent:

  xxx

- operator:

  xxx

- remoteReset:

  A `ìnteger(1)` xxxx

- is.enabled:

  A `logical(1)` that indicates whether the module is enabled or
  disabled. This is a remote command.

- obj:

  xxx

## Value

As for all modules used with `MagellanNTK`, the return value is a
[`list()`](https://rdrr.io/r/base/list.html) of two items:

- trigger : xxx

- value: In this case, it contains a list() of three slots:

  - ll.fun: a list() of instances of the class `FunctionFilter`,

  - ll.query: a list of
    [`character()`](https://rdrr.io/r/base/character.html) which
    describe the queries in natural language,

  - ll.widgets.value: a list of the values of widgets.

## Examples

``` r
if (interactive()){
library(DaparToolshed)
library(SummarizedExperiment)
data(Exp1_R25_prot, package = "DaparToolshedData")
obj <- Exp1_R25_prot
conds <- colData(Exp1_R25_prot)$Condition

shiny::runApp(mod_qMetacell_FunctionFilter_Generator(obj, conds))
shiny::runApp(mod_qMetacell_FunctionFilter_Generator(obj, conds, is.enabled = FALSE))
}
```
