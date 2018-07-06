#' @title Summary.BMTME
#'
#' @description Solo es una prueba
#'
#' @param object \code{BMTME object} Objeto BMTME, resultado de ejecutar BMTME
#' @param ... Further arguments passed to or from other methods.
#' @param information compact, extended, complete
#'
#' @importFrom stats cor
#' @importFrom dplyr summarise group_by select '%>%'
#'
#' @export
# summary.BMTME <- function(object, information = 'compact', ...){
#
#   if (!inherits(object, "BMTME")) Error("This function only works for objects of class 'BMTME'")
#
#   tmp <- paste('--------------------> Summary of data & model <--------------------')
#   cat(tmp,'\n\n')
#
#   tmp <- paste(' Number of phenotypes=', sum(!is.na(object$response)))
#   cat(tmp,'\n')
#
#   cat(' Min (TRN)= ', min(object$response,na.rm = TRUE),'\n')
#   cat(' Max (TRN)= ', max(object$response,na.rm = TRUE),'\n')
#   cat(' Variance of phenotypes (TRN)=', round(var(object$response, na.rm = TRUE),4), '\n')
#   cat(' Residual variance=', round(object$varE, 4), '\n')
#
#   n <- length(object$response)
#
#   if (any(is.na(object$response))) {
#     tst <- which(is.na(object$response))
#     cat(' N-TRN=', n - length(tst), ' N-TST=', length(tst), '\n')
#     cat(' Correlation TRN=', round(cor(object$response[-tst], object$predictions[-tst]), 4),'\n')
#   }else{
#     cat(' N-TRN=',n,'  N-TST=0', '\n\n')
#   }
#   cat('\n------------------------------------------------------------------\n');
# }

summary.BMTME <- function(object, information = 'compact', ...){
  if (!inherits(object, "BMTME")) Stop("This function only works for objects of class 'BMTME'")

  object$results %>%
    group_by(Environment, Trait, Partition) %>%
    summarise(Cor = cor(Predicted, Observed, use = 'pairwise.complete.obs'),
              MSEP = mean((Predicted - Observed)**2, na.rm = T)) %>%
    select(Environment, Trait, Partition, Cor, MSEP) -> presum

  presum %>%  group_by(Environment, Trait) %>%
    summarise(SE_MSEP = sd(MSEP, na.rm = T), MSEP = mean(MSEP, na.rm = T),
              Cor = mean(Cor, na.rm = T), SE_Cor = sqrt((Cor*(1 - Cor))/n())) %>%
    select(Environment, Trait, Cor, SE_Cor, MSEP, SE_MSEP) -> finalSum

  out <- switch(information,
    compact = finalSum,
    complete = presum,
    extended = {
      finalSum$Partition <- 'All'
      presum$Partition <- as.character(presum$Partition)
      presum$SE_Cor <- NA
      presum$SE_MSEP <- NA
      rbind(presum, finalSum)
    }
  )

  return(out)

}

