#' Bayesian Multi Trait Multi Environment Regressor Stacking
#'
#' @param Y phenotipes matrix, with every trait in a independent column.
#' @param ETA eta
#' @param nIter number of iterations
#' @param burnIn number of burning
#' @param thin number of thinning
#' @param testingSet cv object
#' @param digits number of digits of accuracy in the results
#'
#' @return Something cool
#' @export
#'
#' @importFrom IBCF.MTME getMatrixForm
#' @importFrom BGLR BGLR
#' @examples
#' ETA=list(Env=list(X=Z.E,model="BRR"),Gen = list(X = Z.G, model = 'BRR'), EnvGen=list(X=Z.EG,model="BRR"))
#' testingSet <- BMTME::CV.RandomPart(pheno, NPartitions = 10, PTesting = 0.2, set_seed = 123)
BMTMERS <- function(Y = NULL, ETA = NULL, covModel = 'BRR', nIter = 2500, burnIn = 500, thin = 5, progressBar = TRUE, testingSet = NULL, digits = 4) {
  validate.Y(Y)
  time.init <- proc.time()[3]
  nCV <- length(testingSet$CrossValidation_list) #Number of cross-validations
  # Yhat_post <- matrix(NA, ncol = nCV, nrow = nrow(Y)) # save predictions
  nTraits <- dim(Y)[2L]
  YwithCov <- matrix(Y, ncol = 2 * nTraits, nrow = nrow(Y)) # to include covariance data
  results <- data.frame() # save cross-validation results
  pb <- progress::progress_bar$new(format = ':what  [:bar]:percent;  Time elapsed: :elapsed - time left: :eta',
                                   total = 2L*(nCV*nTraits), clear = FALSE, show_after = 0)
  # Covariance
  for (actual_CV in seq_len(nCV)) {
    #########First stage analysis#################################
    for (t in seq_len(nTraits)) {
      if (progressBar) {
        pb$tick(tokens = list(what = paste0('Estimating covariates')))
      }
      y <- Y[, t]
      positionTST <- testingSet$CrossValidation_list[[actual_CV]]
      y[positionTST] <- NA

      fm <- BGLR(y, ETA = ETA, nIter = nIter, burnIn = burnIn, thin = thin, verbose = F)
      YwithCov[, nTraits + t] <- fm$yHat
    }

    XPV <- scale(YwithCov[, (1L + nTraits):(2L*nTraits)])
    ETA1 <- ETA
    ETA1$Cov_PreVal <- list(X = XPV, model = covModel)

    for (t in seq_len(nTraits)) {
      if (progressBar) {
        pb$tick(tokens = list(what = paste0('Fitting the model')))
      }
      y1 <- Y[, t]
      positionTST <- testingSet$CrossValidation_list[[actual_CV]]
      y1[positionTST] <- NA

      fm <- BGLR(y1, ETA = ETA1, nIter = nIter, burnIn = burnIn, thin = thin, verbose = F)

      results <- rbind(results, data.frame(Position = positionTST,
                                           Environment = testingSet$Environments[positionTST],
                                           Trait = colnames(Y)[t],
                                           Partition = actual_CV,
                                           Observed = round(Y[positionTST, t], digits), #$response, digits),
                                           Predicted = round(fm$yHat[positionTST], digits)))
    }

  }
  out <- list(results = results, nIter = nIter, burnIn = burnIn, thin = thin, executionTime = proc.time()[3] - time.init)
  class(out) <- 'BMTMERSCV'
  return(out)
}

validate.Y <- function(matrix){
  if (!is.matrix(matrix) || !is.numeric(matrix)) {
    message('Y must be a phenotypic matrix with numeric data')
    stop(call. = FALSE)
  }
}
