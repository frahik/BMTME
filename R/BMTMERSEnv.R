#' Bayesian Multi Trait Multi Environment Regressor Stacking for specific environment estimations
#'
#' @param Y phenotipes matrix, with every trait in a independent column.
#' @param ETA eta
#' @param testingEnv environment to test.
#' @param nIter number of iterations
#' @param burnIn number of burning
#' @param thin number of thinning
#' @param progressBar show the progress bar?
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
BMTMERS_Env <- function(data = NULL, testingEnv = '', ETA = NULL, nIter = 2500, burnIn = 500, thin = 5, progressBar = TRUE, digits = 4) {
  time.init <- proc.time()[3]
  Y <- data[, -1]
  YwithCov <-  Y # to include covariance data
  nTraits <- dim(Y)[2L]
  nEnvs <- length(testingEnv)
  results <- data.frame() # save cross-validation results
  pb <- progress::progress_bar$new(format = ':what  [:bar]:percent;  Time elapsed: :elapsed - time left: :eta', total = 2L*(nEnvs*nTraits), clear = FALSE, show_after = 0)

  #########First stage analysis#################################
  for (trait in seq_len(nTraits)) {
    y2 <- Y[, trait]
    predictions <- list()
    for (env in testingEnv) {
      if (progressBar) {
        pb$tick(tokens = list(what = paste0('Estimating covariance of trait ', colnames(Y)[trait], ' in the environment ', env)))
      }
      y1 <- y2
      Pos_ts <- which(data[, 1] == env)
      y1[Pos_ts] <- NA

      fm <- BGLR(y = y1, ETA = ETA, nIter = nIter, burnIn = burnIn, verbose = F)
      predictions[[env]] <- fm$yHat
    }
    envPred <- matrix(unlist(predictions), ncol = length(testingEnv))
    YwithCov[, nTraits + trait] <- apply(envPred, 1, mean)
  }
  XPV <- scale(YwithCov[, (1L + nTraits):(2L*nTraits)])

  ETA1 <- ETA
  ETA1$Cov_PreVal <- list(X = XPV, model = "BRR")

  for (t in seq_len(nTraits)) {
    y2 <- Y[, t]
    for (env in testingEnv) {
      if (progressBar) {
        pb$tick(tokens = list(what = paste0('Fitting the model in the trait ', colnames(Y)[t], ' and the environment ', env, ' for the global estimations')))
      }
      y1 <- y2
      Pos_ts <- which(data[, 1] == env)
      y1[Pos_ts] <- NA
      fm <- BGLR(y1, ETA = ETA1, nIter = nIter, burnIn = burnIn, thin = thin, verbose = F)

      results <- rbind(results, data.frame(Position = Pos_ts,
                                           Environment = data[Pos_ts, 1],
                                           Trait = colnames(Y)[t],
                                           Observed = round(y2[Pos_ts], digits),
                                           Predicted = round(fm$yHat[Pos_ts], digits)))
    }
  }
  out <- list(results = results, covariance = XPV, nIter = nIter, burnIn = burnIn, thin = thin, executionTime = proc.time()[3] - time.init)
  class(out) <- 'BMTMERSENV'
  return(out)
}
