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


#' @title Pirat missingness mechanism plot
#'
#' @description Make the missingness mechanism plot for Pirat
#'
#' @param data A dataset in a Pirat compliant format
#'
#' @return A plot
#'
#' @examples
#' data(subbouyssie, package = "Pirat")
#' missmechPiratPlot(subbouyssie)
#'
#' @export
#'
missmechPiratPlot <- function(data){
  par(mar = c(4,4,1,1))
  mv_rates <- colMeans(is.na(data$peptides_ab))
  mean_abund <- colMeans(data$peptides_ab, na.rm = T)
  mean_abund_sorted <- sort(mean_abund, index.return = T)
  mv_rates_sorted <- mv_rates[mean_abund_sorted$ix]
  kernel_size <- 10
  probs <- rep(0, length(mean_abund) - kernel_size + 1)
  for (i in seq_along(probs)) {
    probs[i] <- mean(mv_rates_sorted[i:(i + kernel_size - 1)])
  }
  not0 <- probs != 0
  m_ab_sorted <- mean_abund_sorted$x[not0]
  probs <- probs[not0]
  res.reg <- lm(log(probs) ~ m_ab_sorted[seq_along(probs)])
  sum.reg.reg <- summary(res.reg)
  
  plot(m_ab_sorted[seq_along(probs)], log(probs),
       ylab="log(p_mis)", 
       xlab="observed mean")
  abline(res.reg, col="red")
  mylabel = bquote(italic(R)^2 == .(format(summary(res.reg)$r.squared, digits = 3)))
  text(x = m_ab_sorted[1]+1, y = (log(probs)[1]+log(probs)[length(log(probs))])/2, labels = mylabel)
}


#' @title Peptide imputation
#'
#' @description Perform imputation for peptides
#'
#' @param data A `QFeatures`
#' @param method A `character(1)`, imputation method to be used. Must choose
#' between : `Pirat`, `impSeq` and `BPCA`
#' @param history The history
#' @param dataPirat The dataset in a Pirat compliant format
#' @param extension A `character(1)`, the extension for Pirat
#' @param alpha.factor A `numeric(1)`, the alpha factor for Pirat
#' @param nPcs A `numeric(1)`, the nPcs for BPCA
#'
#' @return A `list` containing the imputed dataset and the history
#'
#' @examples
#' data(subR25pept, package = "DaparToolshed")
#' imputationPept(subR25pept,
#'                method = "impSeq")
#'
#' @export
#'
imputationPept <- function(data,
                           method,
                           history = NULL,
                           dataPirat = NULL,
                           extension = NULL,
                           alpha.factor = NULL,
                           nPcs = NULL) {
  if (is.null(history)){
    history <- MagellanNTK::InitializeHistory()
  }

  .tmp <- data[[length(data)]] 
  try({
    switch(method,
           Pirat = {
             incProgress(0.5, detail = "Pirat imputation")
             #rv.custom$Pirat_showlog <- TRUE
             Pirat_logtxt <- show_log_console(prefix = "PIRAT", id_notif = "notif_message", console_message = FALSE, {
               Pirat_dataimput <- Pirat::my_pipeline_llkimpute(dataPirat,
                                                               alpha.factor = alpha.factor,
                                                               extension = extension,
                                                               verbose = TRUE)
             })
             if (is.null(Pirat_dataimput$data.imputed)){
               shinyjs::info("Error when imputing with Pirat")
             }
             req(Pirat_dataimput$data.imputed)
             
             history <- Prostar2::Add2History(history, 'Imputation', 'Imputation', 'algorithm', method)
             history <- Prostar2::Add2History(history, 'Imputation', 'Imputation', 'extension', extension)
             history <- Prostar2::Add2History(history, 'Imputation', 'Imputation', 'alpha.factor', alpha.factor)
             
             
             SummarizedExperiment::assay(.tmp, withDimnames=FALSE) <- t(Pirat_dataimput$data.imputed)
           },
           impSeq = {
             incProgress(0.5, detail = "impSeq imputation")
             impSeq_dataimput <- rrcovNA::impSeq(SummarizedExperiment::assay(data[[length(data)]]))
             
             history <- Prostar2::Add2History(history, 'Imputation', 'Imputation', 'algorithm', method)
             
             SummarizedExperiment::assay(.tmp, withDimnames=FALSE) <- impSeq_dataimput
           },
           BPCA = {
             incProgress(0.5, detail = "BPCA imputation")
             BPCA_dataimput <- pcaMethods::pca(SummarizedExperiment::assay(data[[length(data)]]), method = "bpca", nPcs = round(nPcs, 0))
             
             history <- Prostar2::Add2History(history, 'Imputation', 'Imputation', 'algorithm', method)
             history <- Prostar2::Add2History(history, 'Imputation', 'Imputation', 'nPcs', nPcs)
             
             SummarizedExperiment::assay(.tmp, withDimnames=FALSE) <- BPCA_dataimput@completeObs
           }
    )
  })
  
  return(list(data = .tmp, history = history))
}
