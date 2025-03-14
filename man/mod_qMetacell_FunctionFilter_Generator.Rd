% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_qMetacell_FunctionFilter_Generator.R
\name{mod_qMetacell_FunctionFilter_Generator}
\alias{mod_qMetacell_FunctionFilter_Generator}
\alias{mod_qMetacell_FunctionFilter_Generator_ui}
\alias{mod_qMetacell_FunctionFilter_Generator_server}
\title{Build queries for filtering quantitative metadata}
\usage{
mod_qMetacell_FunctionFilter_Generator_ui(id)

mod_qMetacell_FunctionFilter_Generator_server(
  id,
  obj,
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
}
\arguments{
\item{id}{xxx}

\item{obj}{An instance of the class \code{SummarizedExperiment}}

\item{conds}{A \code{character()} which contains the name of the conditions. The
length of this vector must be equal to the number of samples in the assay
(i.e. number of columns in assay(obj))}

\item{keep_vs_remove}{xxx}

\item{val_vs_percent}{xxx}

\item{operator}{xxx}

\item{remoteReset}{A \code{ìnteger(1)} xxxx}

\item{is.enabled}{A \code{logical(1)} that indicates whether the module is
enabled or disabled. This is a remote command.}
}
\value{
As for all modules used with \code{MagellanNTK}, the return value is a
\code{list()} of two items:
\itemize{
\item trigger : xxx
\item value: In this case, it contains a list() of three slots:
\itemize{
\item ll.fun: a list() of instances of the class \code{FunctionFilter},
\item ll.query: a list of \code{character()} which describe the queries in natural
language,
\item ll.widgets.value: a list of the values of widgets.
}
}
}
\description{
This function is a shiny module to create a list of queries (instances of the
class \code{FunctionFilter} to filter the quantitative metadata of an instance
of the class \code{SummarizedExperiment})
}
\examples{
\dontrun{
library(DaparToolshed)
data(Exp1_R25_prot, package ='DaparToolshedData')
obj <- Exp1_R25_prot[[1]]
conds <- colData(Exp1_R25_prot)$Condition

shiny::runApp(mod_qMetacell_FunctionFilter_Generator(obj, conds))
shiny::runApp(mod_qMetacell_FunctionFilter_Generator(obj, conds, is.enabled = FALSE))
}

}
