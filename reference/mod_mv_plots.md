# xxx

xxx

## Usage

``` r
mod_mv_plots_ui(id)

mod_mv_plots_server(
  id,
  data = reactive({
     NULL
 }),
  grp = reactive({
     NULL
 }),
  mytitle = reactive({
     NULL
 }),
  pal = reactive({
     NULL
 }),
  pattern = reactive({
     NULL
 }),
  is.enabled = reactive({
     TRUE
 }),
  remoteReset = reactive({
     0
 })
)

mod_mv_plots(data, grp = NULL, mytitle = NULL, pal = NULL, pattern = NULL)
```

## Arguments

- id:

  xxx

- data:

  xxx

- grp:

  xxx

- mytitle:

  xxx

- pal:

  xxx

- pattern:

  xxx

- is.enabled:

  xxx

- remoteReset:

  xxx

## Examples

``` r
if (interactive()){
library(DaparToolshed)
library(highcharter)
data(Exp1_R25_prot, package = "DaparToolshedData")
pattern <- c("Missing POV")
grp <- design.qf(Exp1_R25_prot)$Condition
shiny::runApp(mod_mv_plots(obj[[4]], pattern = pattern, grp = grp))
}
```
