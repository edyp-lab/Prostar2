# Module agregation

xxxxx

## Usage

``` r
mod_Agregation_ui(id)

mod_Agregation_server(
  id,
  dataIn = reactive({
     NULL
 }),
  steps.enabled = reactive({
     NULL
 }),
  remoteReset = reactive({
     0
 }),
  steps.status = reactive({
     NULL
 }),
  current.pos = reactive({
     1
 }),
  verbose = FALSE
)
```

## Arguments

- id:

  A `character(1)` which is the 'id' of the module.

- dataIn:

  An instance of the class `QFeatures`

- steps.enabled:

  A [`logical()`](https://rdrr.io/r/base/logical.html) which indicates
  whether each step is enabled or disabled in the UI.

- remoteReset:

  A `logical(1)` which acts asa a remote command to reset the module to
  its default values. Default is FALSE.

- steps.status:

  A [`logical()`](https://rdrr.io/r/base/logical.html) which indicates
  the status of each step which can be either 'validated', 'undone' or
  'skipped'. enabled or disabled in the UI.

- current.pos:

  A `interger(1)` which acts as a remote command to make a step active
  in the timeline. Default is 1.

- verbose:

  A `logical(1)` to indicates whether run and debug infos must be
  printed in the console. Default is FALSE.

## Value

NA

## Step 'Description'

xxxxxxx

## Step 'Filter peptides'

xxxxxx

## Step 'Agregation'

xxxxx

## Step 'Save'

xxxxxx

## See also

The user manual of the package `MagellanNTK`.

## Examples

``` r
if (interactive()){
shiny::runApp(workflowApp("Agregation", verbose = TRUE))
}
```
