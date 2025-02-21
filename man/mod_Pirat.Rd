% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_Pirat.R
\name{mod_Pirat}
\alias{mod_Pirat}
\alias{mod_Pirat_ui}
\alias{mod_Pirat_server}
\title{Pirat shiny app}
\usage{
mod_Pirat_ui(id)

mod_Pirat_server(
  id,
  obj = reactive({
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

mod_Pirat(obj)
}
\arguments{
\item{id}{The id of the module}

\item{obj}{An instance of the class \code{SummarizedExperiment}.}

\item{verbose}{A boolean (FALSE as default) which indicates whether to
display more details on the process}

\item{reset}{A boolean which indicates whether to reset the widgets or not.}
}
\value{
A shiny app

A shiny app

A shiny app

A shiny app
}
\description{
\subsection{To be customized}{
}
}
\examples{
\dontrun{
data(subbouyssie)

# Builds the instance of `SummarizedExperiment`
obj.se <- pirat2SE(subbouyssie$peptides_ab, subbouyssie$adj, 
subbouyssie$mask_prot_diff, subbouyssie$mask_pep_diff )

#Launch the app
app <- mod_Pirat(obj.se)
shiny::runApp(app)

}


}
