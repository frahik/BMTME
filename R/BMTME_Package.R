#' BMTME: Bayesian Multi-Trait Multi-Environment models
#'
#' The Bayesian Multi-Trait Multi-Environment models (BMTME) package was developed to implement...
#'
#'
#' @docType package
#' @name BMTME
NULL

if (getRversion() >= "2.15.1")
  utils::globalVariables(
    c('.',
      'Environment',
      'Trait',
      'Partition',
      'Predicted',
      'Observed',
      'Pearson',
      'MSEP',
      'SE_Pearson',
      'SE_MSEP'
    )

  )