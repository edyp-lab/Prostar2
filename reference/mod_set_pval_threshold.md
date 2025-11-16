# Module set pval threshold

A shiny Module.

## Usage

``` r
mod_set_pval_threshold_ui(id)

mod_set_pval_threshold_server(
  id,
  pval_init = reactive({
     1
 }),
  fdr = reactive({
     0
 }),
  threshold.type = reactive({
     "logpval"
 }),
  remoteReset = reactive({
     0
 }),
  is.enabled = reactive({
     TRUE
 })
)

mod_set_pval_threshold(pval_init = 1, fdr = 0, threshold.type = "logpval")
```

## Arguments

- id:

  xxx

- pval_init:

  xxx

- fdr:

  xxx

- threshold.type:

  xxx

- remoteReset:

  xxx

- is.enabled:

  xxx

## Examples

``` r
if (interactive()){
shiny::runApp(mod_set_pval_threshold())
}
```
