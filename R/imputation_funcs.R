#' @title Protein POV imputation methods
#' 
#' @description Get imputation methods for proteins for missing POV
#' 
#' @return A vector of character
#' 
#' @examples 
#' GetPOVimputMet()
#' 
#' @export
#' 
GetPOVimputMet <- function(){
  met <- list("slsa" = "slsa", "Det quantile" = "detQuantile", "KNN" = "KNN")
  return(met)
}


#' @title Protein MEC imputation methods
#' 
#' @description Get imputation methods for proteins for missing MEC
#' 
#' @return A vector of character
#' 
#' @examples 
#' GetMECimputMet()
#' 
#' @export
#' 
GetMECimputMet <- function(){
  met <- list("Det quantile" = "detQuantile", "Fixed value" = "fixedValue")
  return(met)
}


#' @title Protein POV imputation
#'
#' @description Perform imputation for proteins for missing POV
#'
#' @param data A `QFeatures`
#' @param method A `character(1)`, imputation method to be used. Must choose
#' between : `slsa`, `detQuantile` and `KNN`
#' @param history The history
#' @param quantile A `numeric(1)`, quantile used as the reference value. 
#' Must be a float between 0 and 100
#' @param factor A `numeric(1)`, multiplying factor to apply to the chosen 
#' quantile value
#' @param n A `numeric(1)`, number of neighbors to account for
#'
#' @return A `list` containing the imputed dataset and the history
#'
#' @examples
#' data(subR25prot, package = "DaparToolshed")
#' imputationProtPOV(subR25prot,
#'                   method = "slsa")
#'
#' @export
#'
imputationProtPOV <- function(data,
                              method,
                              history = NULL,
                              quantile = NULL,
                              factor = NULL,
                              n = NULL) {
  if (is.null(history)){
    history <- MagellanNTK::InitializeHistory()
  }

  try({
    switch(method,
      slsa = {
        .tmp <- DaparToolshed::wrapperImputeSLSA(
          obj = data[[length(data)]],
          design = DaparToolshed::design_qf(data)
        )

        history <- Prostar2::Add2History(history, "Imputation", "POVImputation", "algorithm", method)
      },
      detQuantile = {
        .tmp <- DaparToolshed::wrapperImputeDetQuant(
          obj = data[[length(data)]],
          qval = quantile / 100,
          factor = factor,
          na.type = "Missing POV"
        )

        history <- Prostar2::Add2History(history, "Imputation", "POVImputation", "algorithm", method)
        history <- Prostar2::Add2History(history, "Imputation", "POVImputation", "quantile", quantile)
        history <- Prostar2::Add2History(history, "Imputation", "POVImputation", "factor", factor)
        history <- Prostar2::Add2History(history, "Imputation", "POVImputation", "na.type", "Missing POV")
      },
      KNN = {
        .tmp <- DaparToolshed::wrapperImputeKNN(
          obj = data[[length(data)]],
          grp = DaparToolshed::design_qf(data)$Condition,
          K = n
        )

        history <- Prostar2::Add2History(history, "Imputation", "POVImputation", "algorithm", method)
        history <- Prostar2::Add2History(history, "Imputation", "POVImputation", "K", n)
      }
    )
  })
  
  return(list(data = .tmp, history = history))
}


#' @title Protein MEC imputation
#'
#' @description Perform imputation for proteins for missing MEC
#'
#' @param data A `QFeatures`
#' @param method A `character(1)`, imputation method to be used. Must choose
#' between : `detQuantile` and `fixedValue`
#' @param history The history
#' @param quantile A `numeric(1)`, quantile used as the reference value. 
#' Must be a float between 0 and 100
#' @param factor A `numeric(1)`, multiplying factor to apply to the chosen 
#' quantile value
#' @param fixVal A `numeric(1)`, fixed value used to impute all MEC
#'
#' @return A `list` containing the imputed dataset and the history
#'
#' @examples
#' data(subR25prot, package = "DaparToolshed")
#' imputationProtMEC(subR25prot,
#'                   method = "fixedValue",
#'                   fixVal = 3)
#'
#' @export
#'
imputationProtMEC <- function(data,
                              method,
                              history = NULL,
                              quantile = NULL,
                              factor = NULL,
                              fixVal = NULL) {
  if (is.null(history)){
    history <- MagellanNTK::InitializeHistory()
  }
  
  try({
    switch(method,
           detQuantile = {
             .tmp <- DaparToolshed::wrapperImputeDetQuant(
               obj = data[[length(data)]],
               qval = quantile / 100,
               factor = factor,
               na.type = 'Missing MEC')
             
             history <- Prostar2::Add2History(history, 'Imputation', 'MECImputation', 'algorithm', method)
             history <- Prostar2::Add2History(history, 'Imputation', 'MECImputation', 'quantile', quantile)
             history <- Prostar2::Add2History(history, 'Imputation', 'MECImputation', 'factor', factor)
             history <- Prostar2::Add2History(history, 'Imputation', 'MECImputation', 'na.type', 'Missing MEC')
           },
           
           fixedValue = {
             .tmp <- DaparToolshed::wrapperImputeFixedValue(
               obj = data[[length(data)]],
               fixVal = fixVal,
               na.type = "Missing MEC"
             )
             
             history <- Prostar2::Add2History(history, 'Imputation', 'MECImputation', 'algorithm', method)
             history <- Prostar2::Add2History(history, 'Imputation', 'MECImputation', 'fixVal', fixVal)
             history <- Prostar2::Add2History(history, 'Imputation', 'MECImputation', 'na.type', 'Missing MEC')
           }
    )
  })
  
  return(list(data = .tmp, history = history))
}
