# Pirat shiny app

To be customized

## Usage

``` r
mod_Pirat_ui(id)

mod_Pirat_server(
  id,
  dataIn = reactive({
     NULL
 }),
  remoteReset = reactive({
     0
 }),
  is.enabled = reactive({
     TRUE
 }),
  verbose = FALSE
)

mod_Pirat(dataIn)
```

## Arguments

- id:

  The id of the module

- dataIn:

  An instance of the class `SummarizedExperiment`.

- remoteReset:

  A boolean which indicates whether to reset the widgets or not.

- is.enabled:

  xxx

- verbose:

  A boolean (FALSE as default) which indicates whether to display more
  details on the process

## Value

A shiny app

A shiny app

A shiny app

A shiny app

## Examples

``` r
if (interactive()){
data(subbouyssie)

# Builds the instance of `SummarizedExperiment`
obj.se <- pirat2SE(
  subbouyssie$peptides_ab, subbouyssie$adj,
  subbouyssie$mask_prot_diff, subbouyssie$mask_pep_diff
)

# Launch the app
app <- mod_Pirat(obj.se)
shiny::runApp(app)
}
```
