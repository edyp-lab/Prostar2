% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_mv_plots.R
\name{mod_mv_plots}
\alias{mod_mv_plots}
\alias{mod_mv_plots_ui}
\alias{mod_mv_plots_server}
\title{xxx}
\usage{
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

mod_mv_plots(data, grp = NULL, title = NULL, pal = NULL, pattern = NULL)
}
\arguments{
\item{data}{xxx}

\item{pal}{xxx}

\item{pattern}{xxx}

\item{title}{xxx}

\item{...}{xxx}
}
\description{
xxx
}
\examples{
\dontrun{
library(DaparToolshed)
library(highcharter)
data(Exp1_R25_prot, package = 'DaparToolshedData')
pattern <- c("Missing POV")
grp <- design.qf(Exp1_R25_prot)$Condition
shiny::runApp(mod_mv_plots(obj[[4]], pattern = pattern, grp = grp))
}

}
