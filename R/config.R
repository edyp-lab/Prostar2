pipeline.defs <- list(
  peptide= c('module_Peptide_Filtering',
             'module_Peptide_Normalization',
             'module_Peptide_Imputation',
             'module_Peptide_HypothesisTest'),
  protein = c('module_Protein_Filtering',
              'module_Protein_Normalization',
              'module_Protein_Imputation'),
  p2p = c('module_p2p_1',
          'module_p2p_2',
          'module_p2p_3',
          'module_p2p_4'),
  metabo = c('module_metabo_1',
             'module_metabo_2',
             'module_metabo_3',
             'module_metabo_4')
)

