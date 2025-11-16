# Filtering Shiny module

This function is a shiny module to xxx This function is written with
specifications of the package `MagellanNTK` so as to be easily
integrated into workflfow compliant with `MagellanNTK`.

## Usage

``` r
mod_Prot_Normalization_ui(id)

mod_Prot_Normalization_server(
  id,
  dataIn = reactive({
     NULL
 }),
  i = reactive({
     NULL
 }),
  remoteReset = reactive({
     0
 }),
  is.enabled = reactive({
     TRUE
 })
)

mod_Prot_Normalization(dataIn, i)
```

## Arguments

- id:

  xxx

- dataIn:

  An instance of the class `QFeatures`

- i:

  xxx

- remoteReset:

  A `integer(1)` xxxx

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
data(Exp1_R25_prot, package = "DaparToolshedData")
obj <- Exp1_R25_prot[seq_len(100)]
mod_Prot_Normalization(obj, 1)
}
```
