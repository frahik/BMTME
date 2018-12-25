#' Bayesian Multi-Output regression stacking for specific environment estimations
#'
#' @param data \code{(data.frame)} Phenotypic response where each column is a different trait and the first column are the name of the environment where it was evaluated.
#' @param ETA \code{(matrix)} This is a two-level list used to specify the regression function (or linear predictor).
#' @param testingEnv \code{(string)} Name of the Environment to test.
#' @param nIter \code{(integer)} Number of iterations to fit the model.
#' @param burnIn \code{(integer)} Number of items to burn at the beginning of the model.
#' @param thin \code{(integer)} Number of items to thin the model.
#' @param covModel \code{(string)} Name of the covariates model to implement (BRR, BayesA, BayesB, BayesC).
#' @param predictor_Sec_complete \code{(Logical)} FALSE by default.
#' @param progressBar \code{(Logical)} Show the progress bar.
#' @param digits \code{(integer)} Number of digits of accuracy in the results.
#'
#' @export
#'
#' @examples
#' \donttest{
#' data('MaizeToy')
#' phenoMaizeToy <- phenoMaizeToy[order(phenoMaizeToy$Env, phenoMaizeToy$Line),]
#'
#' #Matrix design
#' LG <- cholesky(genoMaizeToy)
#' ZG <- model.matrix(~0 + as.factor(phenoMaizeToy$Line))
#' Z.G <- ZG %*% LG
#' #Linear Predictor
#' ETA <- list(Gen = list(X = Z.G, model = 'BRR'))
#'
#' dataset <- phenoMaizeToy[, 2:5] #Must Include in the first column the environments
#' #Check predictive capacities of the model
#' pm <- BMORS_Env(dataset, testingEnv = 'EBU', ETA = ETA, covModel = 'BRR', nIter = 10000,
#'                 burnIn = 5000, thin = 2, progressBar = FALSE, digits = 3)
#' }
#'
#' @importFrom BGLR BGLR
BMORS_Env <- function(data = NULL, testingEnv = '', ETA = NULL, covModel = 'BRR', predictor_Sec_complete = FALSE, nIter = 2500, burnIn = 500, thin = 5, progressBar = TRUE, digits = 4) {
  time.init <- proc.time()[3]
  Y <- data[, -1]
  YwithCov <-  Y # to include covariable data
  nTraits <- dim(Y)[2]
  nEnvs <- length(testingEnv)
  results <- data.frame() # save cross-validation results
  pb <- progress::progress_bar$new(format = ':what  [:bar]:percent;  Time elapsed: :elapsed - time left: :eta', total = 2L*(nEnvs*nTraits), clear = FALSE, show_after = 0)
  #########First stage analysis#################################
  for (env in testingEnv) {
    Pos_ts <- which(data[, 1] == env)
    for (trait in seq_len(nTraits)) {
      if (progressBar) {
        pb$tick(tokens = list(what = paste0('Estimating covariates')))
      }
      y1 <- Y[, trait]
      y1[Pos_ts] <- NA

      fm <- BGLR(y = y1, ETA = ETA, nIter = nIter, burnIn = burnIn, verbose = FALSE)
      YwithCov[, nTraits + trait] <- fm$yHat
    }

    XPV <- scale(YwithCov[, (1L + nTraits):(2L*nTraits)])
    ETA1 <- ETA
    if (predictor_Sec_complete) {
      ETA1$Cov_PreVal <- list(X = XPV, model = covModel)
    } else {
      ETA1 <- list(Cov_PreVal = list(X = XPV, model = covModel))
    }

    for (t in seq_len(nTraits)) {
      y2 <- Y[, t]
      if (progressBar) {
        pb$tick(tokens = list(what = paste0('Fitting the model')))
      }
      y1 <- y2
      y1[Pos_ts] <- NA
      fm <- BGLR(y1, ETA = ETA1, nIter = nIter, burnIn = burnIn, thin = thin, verbose = FALSE)
      results <- rbind(results, data.frame(Position = Pos_ts,
                                           Environment = data[Pos_ts, 1],
                                           Trait = colnames(Y)[t],
                                           Observed = round(y2[Pos_ts], digits),
                                           Predicted = round(fm$yHat[Pos_ts], digits)))

    }
  }
  out <- list(results = results, nIter = nIter, burnIn = burnIn, thin = thin, executionTime = proc.time()[3] - time.init)
  class(out) <- 'BMORSENV'
  return(out)
}
