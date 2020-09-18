pipeline.defs <- list(
  peptidomic = c('Filtering',
                  'Normalization',
                  'Imputation',
                  'HypothesisTest'),
  protein = c('Filtering'
            #  'Normalization',
            #  'Imputation',
            #  'HypothesisTest'
            ),
  p2p = c('module_p2p_1',
          'module_p2p_2',
          'module_p2p_3',
          'module_p2p_4'),
  fullPeptide = c('module_metabo_1',
             'module_metabo_2',
             'module_metabo_3',
             'module_metabo_4')
)

