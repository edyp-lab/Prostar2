---
title: "Missing values imputation"
abstract: >
  This page describes the computations done with Imputation.

output:
    BiocStyle::html_document:
        highlight: tango
        thme: sandstone
        toc: false
        number_sections: TRUE
        css: style.css
date: 30 mai 2024
link-citations: true
---


This page describes the computations done with Imputation. 
By taking into account the very nature of each missing value.


### Imputation of 'Partially Over Conditions'

### Imputation of 'Missing on Entire Condition'

"<ul><li><strong>imp4p [Ref. 7]</strong>
        a proteomic-specific multiple imputation method that operates on
        peptide-level datasets and which proposes to impute each missing
        value according to its nature (left-censored  or random). To tune
        the number of iterations, let us keep in mind that, the more iterations,
        the more accurate the results, yet the more time-consuming the
        computation.</li> <li><strong>Dummy censored:</strong> each missing
        value is supposed to be a censored value and is replaced by the XXX
        quantile of the corresponding sample abundance distribution <ul><li>
        <strong>KNN </strong>see [Other ref. 2].</li><li><strong>MLE </strong>
        Imputation with the maximum likelihood estimate of the expected
        intensity (see the norm R package).</li></ul></ul>"