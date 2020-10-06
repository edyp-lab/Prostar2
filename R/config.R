pipeline.defs <- list(
  peptidomic = list(Filtering = T,
                  Normalization = F,
                  Imputation = F,
                  HypothesisTest = F),
  protein = list(Filtering=T,
              Normalization=F,
              Imputation=F,
              HypothesisTest=F
            ),
  p2p = list(module_p2p_1=T,
          module_p2p_2=F,
          module_p2p_3=F,
          module_p2p_4=F),
  fullPeptide = list(module_metabo_1=T,
             module_metabo_2=F,
             module_metabo_3=F,
             module_metabo_4=F)
)

