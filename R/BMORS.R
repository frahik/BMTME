#' Bayesian Multi-Output Regression Stacking (BMORS)
#'
#' @param Y \code{(matrix)} Phenotypic respones where each column it's a different trait.
#' @param ETA \code{(matrix)} This is a two-level list used to specify the regression function (or linear predictor).
#' @param covModel \code{(string)} Name of the covariates model to implement (BRR, BayesA, BayesB, BayesC).
#' @param predictor_Sec_complete FALSE by default
#' @param nIter \code{(integer)} Number of iterations to fit the model.
#' @param burnIn \code{(integer)} Number of items to burn at the beginning of the model.
#' @param thin \code{(integer)} Number of items to thin the model.
#' @param progressBar \code{(Logical)} Show the progress bar.
#' @param testingSet \code{(object or vector)} Crossvalidation object or vector with the positions to use like testing in a cross-validation test.
#' @param parallelCores \code{(integer)} Number of cores to use.
#' @param digits \code{(integer)} Number of digits of accuracy in the results.
#'
#' @export
#'
#' @importFrom BGLR BGLR
#' @examples
#' \dontrun{
#' data('wheat')
#' CV.RandomPart(phenoWheat, NPartitions = 10, PTesting = 0.2, set_seed = 123)
#' }
BMORS <- function(Y = NULL, ETA = NULL, covModel = 'BRR', predictor_Sec_complete = FALSE, nIter = 2500, burnIn = 500, thin = 5, progressBar = TRUE, testingSet = NULL, parallelCores = 1, digits = 4) {
  validate.Y(Y)
  parallelCores <- validate.parallelCores(parallelCores)
  time.init <- proc.time()[3]
  nCV <- length(testingSet$CrossValidation_list) #Number of cross-validations
  # Yhat_post <- matrix(NA, ncol = nCV, nrow = nrow(Y)) # save predictions
  nTraits <- dim(Y)[2L]
  YwithCov <- matrix(Y, ncol = 2 * nTraits, nrow = nrow(Y)) # to include covariance data
  results <- data.frame() # save cross-validation results
  pb <- progress::progress_bar$new(format = ':what  [:bar]:percent;  Time elapsed: :elapsed - time left: :eta',
                                   total = 2L*(nCV*nTraits), clear = FALSE, show_after = 0)
  if (parallelCores <= 1 && inherits(testingSet, 'CrossValidation')) {
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
      if (predictor_Sec_complete) {
        ETA1$Cov_PreVal <- list(X = XPV, model = covModel)
      } else {
        ETA1 <- list(Cov_PreVal = list(X = XPV, model = covModel))
      }

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
  } else if(parallelCores > 1 && inherits(testingSet, 'CrossValidation')) {
    cl <- snow::makeCluster(parallelCores)
    doSNOW::registerDoSNOW(cl)
    nCV <- length(testingSet$CrossValidation_list)

    pb <- utils::txtProgressBar(max = nCV, style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    results <- foreach::foreach(actual_CV = seq_len(nCV), .combine = rbind, .packages = 'BMTME', .options.snow = opts) %dopar% {
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
        data.frame(Position = positionTST,
                   Environment = testingSet$Environments[positionTST],
                   Trait = colnames(Y)[t],
                   Partition = actual_CV,
                   Observed = round(Y[positionTST, t], digits),
                   Predicted = round(fm$yHat[positionTST], digits))
      }

    }


  }

  out <- list(results = results, nIter = nIter, burnIn = burnIn, thin = thin, executionTime = proc.time()[3] - time.init)
  class(out) <- 'BMORSCV'
  return(out)
}

validate.Y <- function(matrix){
  if (!is.matrix(matrix) || !is.numeric(matrix)) {
    message('Y must be a phenotypic matrix with numeric data')
    stop(call. = FALSE)
  }
}
