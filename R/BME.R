#' Bayesian Multi-Environment Model (BME)
#'
#' @param Y \code{(matrix)} Phenotypic response where each column is a different environment.
#' @param Z1 \code{(matrix)} Matrix design for the genetic effects.
#' @param nIter \code{(integer)} Number of iterations to fit the model.
#' @param burnIn \code{(integer)} Number of items to burn at the beginning of the model.
#' @param thin \code{(integer)} Number of items to thin the model.
#' @param bs \code{(integer)} Number of groups.
#' @param parallelCores \code{(integer)} Number of cores to use.
#' @param digits \code{(integer)} Number of digits of accuracy in the results.
#' @param progressBar \code{(Logical)} Show the progress bar.
#' @param testingSet \code{(object or vector)} Crossvalidation object or vector with the positions to use like testing in a cross-validation test.
#'
#' @return If the testingSet is NULL, the function returns the predictions.
#'
#' Else, if the testingSet is not NULL, the function returns the correlation of the predictions of the cross-validation test.
#' @examples
#' \donttest{
#'   data("WheatMadaToy")
#'   phenoMada <- (phenoMada[order(phenoMada$GID),])
#'
#'   #Matrix design
#'   LG <- cholesky(genoMada)
#'   ZG <- model.matrix(~0 + as.factor(phenoMada$GID))
#'   Z.G <- ZG %*% LG
#'
#'   #Pheno data
#'   Y <- as.matrix(phenoMada[, -c(1)])
#'   # Check fitting
#'   fm <- BME(Y = Y, Z1 = Z.G, nIter = 10000, burnIn = 5000, thin = 2, bs = 50)
#'
#'   # Check predictive capacities of the model with CrossValidation object
#'   pheno <- data.frame(GID = phenoMada[, 1], Env = '', Response = phenoMada[, 3])
#'   CrossV <- CV.RandomPart(pheno, NPartitions = 4, PTesting = 0.2, set_seed = 123)
#'
#'   pm <- BME(Y = Y, Z1 = Z.G, nIter = 10000, burnIn = 5000, thin = 2, bs = 50, testingSet = CrossV)
#' }
#'
#' @references
#' Montesinos-Lopez, O.A., Montesinos-Lopez, A., Crossa, J., Toledo, F.H., Perez-Hernandez, O., Eskridge, K.M., … Rutkoski, J. (2016).
#' A Genomic Bayesian Multi-trait and Multi-environment Model. G3: Genes|Genomes|Genetics, 6(9), 2725–2744. \url{https://doi.org/10.1534/g3.116.032359}.
#'
#' @importFrom stats lm rnorm var vcov
#' @importFrom tidyr gather
#' @importFrom foreach %dopar%
#' @useDynLib BMTME
#' @export
BME <- function(Y, Z1, nIter = 1000L, burnIn = 300L, thin = 2L, bs = ceiling(dim(Z1)[2]/6), parallelCores = 1, digits = 4, progressBar = TRUE, testingSet = NULL) {
  time.init <- proc.time()[3]
  parallelCores <- validate.parallelCores(parallelCores)
  if (is.null(testingSet)) {
    out <- coreME(Y, Z1, nIter, burnIn, thin, bs, digits, progressBar, testingSet)
    class(out) <- 'BME'
  } else if (parallelCores <= 1 && inherits(testingSet, 'CrossValidation')) {
    results <- data.frame()
    nCV <- length(testingSet$CrossValidation_list)
    pb <- getProgressBar('Fitting Cross-Validation :what  [:bar] :percent;  Time elapsed: :elapsed', nCV)

    for (actual_CV in seq_len(nCV)) {
      if (progressBar) {
        pb$tick(tokens = list(what = paste0(actual_CV, ' out of ', nCV)))
      }

      positionTST <- testingSet$CrossValidation_list[[actual_CV]]

      results <- rbind(results, CVME(Y, Z1, nIter, burnIn, thin, bs, digits,
                                     positionTST, actual_CV,
                                     testingSet$Environments[positionTST]))
    }

    out <- list(results = results, n_cores = parallelCores, nIter = nIter,
                burnIn = burnIn, thin = thin, executionTime = proc.time()[3] - time.init)
    class(out) <- 'BMECV'

  } else if (parallelCores > 1 && inherits(testingSet, 'CrossValidation')) {
    cl <- snow::makeCluster(parallelCores)
    doSNOW::registerDoSNOW(cl)
    nCV <- length(testingSet$CrossValidation_list)

    progress <- NULL
    if (progressBar) {
      pb <- utils::txtProgressBar(max = nCV, style = 3)
      progress <- function(n) utils::setTxtProgressBar(pb, n)
    }
    opts <- list(progress = progress)
    results <- foreach::foreach(actual_CV = seq_len(nCV), .combine = rbind, .packages = 'BMTME', .options.snow = opts) %dopar% {
      CVME(Y, Z1, nIter, burnIn, thin, bs, digits,
           testingSet$CrossValidation_list[[actual_CV]], actual_CV,
           testingSet$Environments[testingSet$CrossValidation_list[[actual_CV]]])
    }

    parallel::stopCluster(cl)
    out <- list(results = results, n_cores = parallelCores, nIter = nIter,
                burnIn = burnIn, thin = thin, executionTime = proc.time()[3] - time.init)
    class(out) <- 'BMECV'
  } else {
    results <- CVME(Y, Z1, nIter, burnIn, thin, bs, digits, testingSet, 1, NA)

    out <- list(results = results, n_cores = parallelCores, nIter = nIter,
                burnIn = burnIn, thin = thin,
                executionTime = proc.time()[3] - time.init)
    class(out) <- 'BMECV'
  }

  return(out)
}

CVME <- function(Y, Z1, nIter, burnIn, thin, bs, digits, testingSet, iterationNumber, Env){
  fm <- coreME(Y, Z1, nIter, burnIn, thin, bs, digits, FALSE, testingSet)
  observed <- tidyr::gather(as.data.frame(Y[testingSet, ]), 'Trait', 'Observed')
  predicted <- tidyr::gather(as.data.frame(fm$yHat[testingSet, ]), 'Trait', 'Predicted')

  return(data.frame(Position = testingSet,
                    Environment = Env,
                    Trait = rep(colnames(Y), each = length(testingSet)),
                    Partition = iterationNumber,
                    Observed = round(observed$Observed, digits),
                    Predicted = round(predicted$Predicted, digits)))
}

coreME <- function(Y, Z1, nIter, burnIn, thin, bs, digits, progressBar, testingSet){
  Y[testingSet, ] <- NA

  if ((nIter - burnIn - thin) < 0L) {
    stop("nIter must be greater than thin+burnIn")
  }

  pb <- progress::progress_bar$new(format = "Fitting the model [:bar]; Time remaining: :eta",
                                   total = nIter/20L, clear = FALSE, show_after = 0L)
  # ps: blocks size
  rmv_f <- function(ps, c, A, x) {
    A    <- (A + t(A))/2
    p    <- dim(A)[1L]
    k    <- floor(p / ps)#Numbers of blocks
    r1   <- p - k * ps
    ps_1 <- ps + r1 %/% k
    r2   <- p - k * ps_1
    ps_2 <- ps_1 + 1L
    k1   <- k - r2
    tmp  <- 0L
    for (i in seq_len(k1)) {
      tmp1  <- tmp + ps_1
      Pos_i <- (tmp + 1L):tmp1
      tmp   <- tmp1
      A_ii  <- A[Pos_i, Pos_i]
      EigenA <- eigen(A_ii)
      d_A   <- EigenA$values
      V_A   <- EigenA$vectors
      pos_A1 <- which(Re(d_A) > 1e-10)
      if (identical(pos_A1, integer(0))) {
        pos_A <- 1L
      } else {
        pos_A <- pos_A1
      }
      d_A_Star <- d_A[pos_A]
      V_A_Star <- V_A[, pos_A]

      if (length(pos_A) == 1L) {
        d_A_Star_Inv <- 1 / d_A_Star
        V_A_Star_t <- d_A_Star_Inv * t(V_A_Star)
        A_ii_inv <- V_A_Star %*% V_A_Star_t
      } else {
        d_A_Star_Inv <- diag(1 / d_A_Star)
        A_ii_inv <- MatMul(MatMul(V_A_Star, d_A_Star_Inv), t(V_A_Star))
      }
      mu_i <- c(A_ii_inv %*% (c[Pos_i] - A[Pos_i, -Pos_i] %*% x[-Pos_i]))
      x[Pos_i] <- c(mvtnorm::rmvnorm(1, mu_i, A_ii_inv))
    }
    if (r2 != 0) {
      for (i in (k1 + 1L):k) {
        tmp1 <- tmp + ps_2
        Pos_i <- (tmp + 1L):tmp1
        tmp <- tmp1
        A_ii <- A[Pos_i, Pos_i]
        EigenA <- eigen(A_ii)
        d_A <- EigenA$values
        V_A <- EigenA$vectors
        pos_A1 <- which(Re(d_A) > 1e-10)
        if (identical(pos_A1, integer(0))) {
          pos_A <- 1L
        } else {
          pos_A <- pos_A1
        }
        d_A_Star <- d_A[pos_A]
        V_A_Star <- V_A[, pos_A]
        if (length(pos_A) == 1L) {
          d_A_Star_Inv <- 1 / d_A_Star
          V_A_Star_t <- d_A_Star_Inv * t(V_A_Star)
          A_ii_inv <- V_A_Star %*% V_A_Star_t
        } else {
          d_A_Star_Inv <- diag(1 / d_A_Star)
          A_ii_inv <-
            MatMul(MatMul(V_A_Star, d_A_Star_Inv), t(V_A_Star))
        }
        mu_i <- c(A_ii_inv %*% (c[Pos_i] - A[Pos_i, -Pos_i] %*% x[-Pos_i]))
        x[Pos_i] <- c(mvtnorm::rmvnorm(1, mu_i, A_ii_inv))
      }
    }
    x
  }

  n <- nrow(Y)
  nt <- ncol(Y)
  nJ <- ncol(Z1)
  b1 <- matrix(0.1, nrow = nJ, ncol = nt, dimnames = list(NULL, colnames(Y)))
  G_invg <- diag(nJ)

  post_beta <- matrix(nrow = 1L, ncol = nt, 0L, dimnames = list(NULL, colnames(Y)))
  post_beta_2 <- matrix(nrow = 1L, ncol = nt, 0L, dimnames = list(NULL, colnames(Y)))
  post_b1 <- matrix(nrow = nJ, ncol = nt, 0L, dimnames = list(NULL, colnames(Y)))
  post_b1_2 <- matrix(nrow = nJ, ncol = nt, 0L, dimnames = list(NULL, colnames(Y)))
  post_var_b1 <- matrix(nrow = nt, ncol = nt, 0L, dimnames = list(NULL, colnames(Y)))
  post_var_b1_2 <- matrix(nrow = nt, ncol = nt, 0L, dimnames = list(NULL, colnames(Y)))
  post_var_e <- matrix(nrow = nt, ncol = nt, 0L, dimnames = list(NULL, colnames(Y)))
  post_var_e_2 <- matrix(nrow = nt, ncol = nt, 0L, dimnames = list(NULL, colnames(Y)))
  post_yHat <- matrix(nrow = n, ncol = nt, 0L, dimnames = list(NULL, colnames(Y)))
  post_yHat_2 <- matrix(nrow = n, ncol = nt, 0L, dimnames = list(NULL, colnames(Y)))
  # post_logLik <- 0L

  YStar <- Y
  vt <- ve <- 5L
  R2 <- 0.25
  R2e <- 0.5
  my.model <- lm(YStar ~ 1)
  beta0 <- my.model$coefficient
  Cov_Beta_Inv <- vcov(my.model)

  whichNa <- list()
  whichNa$subjects <- which(apply(FUN = any, X = is.na(Y), MARGIN = 1L))
  nNa <- length(whichNa$subjects)
  tst <- c(as.numeric(whichNa$subjects))

  X <- rep(1L, n)
  tX <- t(X)
  tZ1 <- t(Z1)
  tXX <- sum(X)
  tZ1Z1 <- MatMul(tZ1, Z1)
  u_b0 <- X %*% beta0
  u_b1 <- MatMul(Z1, b1)
  betav <- beta0

  VarY <- var(Y, na.rm = TRUE)
  St <- VarY * R2 * (vt + 2L)
  Se <- VarY * (1L - R2e) * (ve + 2L)
  sigmaT <- St / (vt + 2L)
  EigenT <- eigen(sigmaT)
  d_T <- EigenT$values
  V_T <- EigenT$vectors
  pos_T <- which(d_T > 1e-10)
  d_T_Star <- d_T[pos_T]
  V_T_Star <- V_T[, pos_T]
  sigmaT.Inv <- MatMul(MatMul(V_T_Star, diag(1 / d_T_Star)), t(V_T_Star))

  yHat <- (u_b0 + u_b1)
  YStar1 <- YStar
  YStar1[tst, ] <- rep(0L, nt)

  e <- (YStar1 - u_b0)

  Re <- (var(e, na.rm = TRUE) * (1L - R2e)) / 2
  e <- (YStar1 - u_b0)
  EigenRe <- eigen(Re)
  d_Re <- EigenRe$values
  V_Re <- EigenRe$vectors
  pos_Re <- which(d_Re > 1e-10)
  d_Re_Star <- d_Re[pos_Re]
  V_Re_Star <- V_Re[, pos_Re]
  Re.Inv <- MatMul(MatMul(V_Re_Star, diag(1 / d_Re_Star)), t(V_Re_Star))
  W <- YStar1
  nSums <- 0L

  for (t in seq_len(nIter)) {
    # logLik <- 0L
    ##### Linear predictor #####################################
    e <- e + u_b0

    ##### Sample of Betas from  a normal distribution ###########
    sigmaB.Inv <- Cov_Beta_Inv / 1E1
    M <- sigmaB.Inv + Re.Inv * tXX
    mu_ac <- MatMul(sigmaB.Inv, matrixcalc::vec(beta0)) + MatMul(Krone(Re.Inv, tX), matrixcalc::vec(e))
    betav1 <- t(MVnormvv(mu_ac, M))
    betav <- matrix(betav1, ncol = nt, byrow = FALSE)
    u_b0 <- X %*% betav
    e <- e - u_b0

    ##### Sample of b1 from a normal distribution ###############
    e <- e + u_b1
    sigmaTG.Inv <- Krone(sigmaT.Inv, G_invg)
    M1 <- sigmaTG.Inv + Krone(Re.Inv, tZ1Z1)
    mu_b1 <- MatMul(Krone(Re.Inv, tZ1), matrixcalc::vec(e))
    b11 <- rmv_f(ps = bs, c = mu_b1, A = M1, x = b1)
    b1 <- matrix(b11, ncol = nt, byrow = FALSE)
    u_b1 <- MatMul(Z1, b1)
    e <- e - u_b1

    ##### Sample of sigma_Traits#################################
    tb1b1 <- MatMul(t(b1), (G_invg %*% b1))
    sigmaT <- inv_wishart(vt + nJ + nt - 1L,  tb1b1 + St)
    EigenT <- eigen(sigmaT)
    d_T <- EigenT$values
    V_T <- EigenT$vectors
    pos_T <- which(d_T > 1e-10)
    d_T_Star <- d_T[pos_T]
    V_T_Star <- V_T[, pos_T]
    sigmaT.Inv <- MatMul(V_T_Star, MatMul(diag(1 / d_T_Star), t(V_T_Star)))

    ##### Sample of sig.e########################################
    yHat <- W - e
    Re <- inv_wishart(ve + n + nt - 1, MatMul(t(e), e) + Se)
    EigenRe <- eigen(Re)
    d_Re <- EigenRe$values
    V_Re <- EigenRe$vectors
    pos_Re <- which(d_Re > 1e-10)
    d_Re_Star <- d_Re[pos_Re]
    V_Re_Star <- V_Re[, pos_Re]
    Re.Inv <- MatMul(V_Re_Star, MatMul(diag(1 / d_Re_Star), t(V_Re_Star)))
    if ((nNa > 0L)) {
      W[tst, ] <- yHat[tst, ] + mvtnorm::rmvnorm(nNa, mean = rep(0, nt), sigma = Re, method = "chol")
      e[tst, ] <- W[tst, ] - yHat[tst, ]
    }

    ##### Saving output #########################################
    if (progressBar && t %% 20L == 0L) {
      pb$tick()
    }

    if ((t > burnIn) & (t %% thin == 0L)) {
      nSums <- nSums + 1L
      k <- (nSums - 1L) / (nSums)
      post_beta <- post_beta * k + betav / nSums
      post_beta_2 <- post_beta_2 * k + (betav ^ 2) / nSums
      post_b1 <- post_b1 * k + b1 / nSums
      post_b1_2 <- post_b1_2 * k + (b1 ^ 2) / nSums

      post_var_b1 <- post_var_b1 * k + sigmaT / nSums
      post_var_b1_2 <- post_var_b1_2 * k + (sigmaT ^ 2) / nSums

      post_var_e <- post_var_e * k + Re / nSums
      post_var_e_2 <- post_var_e_2 * k + (Re ^ 2) / nSums

      post_yHat <- post_yHat * k + yHat / nSums
      post_yHat_2 <- post_yHat_2 * k + (yHat ^ 2) / nSums

      out <- list(
        Y = round(Y, digits),
        nIter = nIter,
        burnIn = burnIn,
        thin = thin,
        dfe = ve,
        Se = Se,
        yHat = round(post_yHat, digits),
        SD.yHat = round(sqrt(post_yHat_2 - (post_yHat ^ 2)), digits),
        beta = round(post_beta, digits),
        SD.beta = round(sqrt(post_beta_2 - post_beta ^ 2), digits),
        b1 = round(post_b1, digits),
        SD.b1 = round(sqrt(post_b1_2 - post_b1 ^ 2), digits),
        vare = round(post_var_e, digits),
        SD.vare = round(sqrt(post_var_e_2 - post_var_e ^ 2), digits),
        varTrait = round(post_var_b1, digits),
        SD.varTrait = round(sqrt(post_var_b1_2 - post_var_b1 ^ 2), digits),
        NAvalues = nNa)
    }
  }
  return(out)
}
