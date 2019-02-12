#' Bayes Multi-Trait Milti-Environment Model (BMTME)
#'
#' @param Y \code{(matrix)} Phenotypic response where each column is a different trait.
#' @param X \code{(matrix)} Matrix design for the environment effects.
#' @param Z1 \code{(matrix)} Matrix design for the genetic effects.
#' @param Z2 \code{(matrix)} Matrix design for the genetic effects interaction with the environment effects.
#' @param nIter \code{(integer)} Number of iterations to fit the model.
#' @param burnIn \code{(integer)} Number of items to burn at the beginning of the model.
#' @param thin \code{(integer)} Number of items to thin the model.
#' @param bs \code{(integer)} Number of groups.
#' @param parallelCores \code{(integer)} Number of cores to use.
#' @param digits \code{(integer)} Number of digits of accuracy in the results.
#' @param progressBar \code{(Logical)} Show the progress bar.
#' @param testingSet \code{(object or vector)} Crossvalidation object or vector with the positions to use like testing in a cross-validation test.
#'
#' @importFrom stats lm rnorm var vcov
#' @importFrom tidyr gather
#' @importFrom foreach %dopar%
#'
#' @references
#' Montesinos-Lopez, O.A., Montesinos-Lopez, A., Crossa, J., Toledo, F.H., Perez-Hernandez, O., Eskridge, K.M., … Rutkoski, J. (2016).
#' A Genomic Bayesian Multi-trait and Multi-environment Model. G3: Genes|Genomes|Genetics, 6(9), 2725–2744. \url{https://doi.org/10.1534/g3.116.032359}.
#'
#' @examples
#' \donttest{
#'   data("WheatIranianToy")
#'
#'   # Matrix Design
#'   LG <- cholesky(genoIranianToy)
#'   ZG <- model.matrix(~0 + as.factor(phenoIranianToy$GID))
#'   Z.G <- ZG %*% LG
#'   Z.E <- model.matrix(~0 + as.factor(phenoIranianToy$Env))
#'   ZEG <- model.matrix(~0 + as.factor(phenoIranianToy$GID):as.factor(phenoIranianToy$Env))
#'   G2 <- kronecker(diag(length(unique(phenoIranianToy$Env))), data.matrix(genoIranianToy))
#'   LG2 <- cholesky(G2)
#'   Z.EG <- ZEG %*% LG2
#'
#'   #Pheno
#'   Y <- as.matrix(phenoIranianToy[, -c(1, 2)])
#'
#'   #Check fitting
#'   fm <- BMTME(Y = Y, X = Z.E, Z1 = Z.G, Z2 = Z.EG,
#'               nIter = 10000, burnIn = 5000, thin = 2, bs = 50)
#'   fm
#'
#'   # Check predictive capacities of the model
#'   pheno <- data.frame(GID = phenoIranianToy[, 1],
#'                       Env = phenoIranianToy[, 2],
#'                       Response = phenoIranianToy[, 3])
#'   CrossV <- CV.RandomPart(pheno, NPartitions = 4, PTesting = 0.2, set_seed = 123)
#'
#'   pm <- BMTME(Y = Y, X = Z.E, Z1 = Z.G, Z2 = Z.EG,
#'               nIter = 10000, burnIn = 5000, thin = 2,
#'               bs = 50, testingSet = CrossV)
#'   pm
#' }
#'
#' @useDynLib BMTME
#' @export
BMTME <- function(Y, X, Z1, Z2, nIter = 1000L, burnIn = 300L, thin = 2L, bs = ceiling(dim(Z1)[2]/6), parallelCores = 1, digits = 4, progressBar = TRUE, testingSet = NULL) {
  time.init <- proc.time()[3]
  parallelCores <- validate.parallelCores(parallelCores)

  if (is.null(testingSet)) {
    out <- coreMTME(Y, X, Z1, Z2, nIter, burnIn, thin, bs, digits, progressBar, testingSet)
    class(out) <- 'BMTME'
  } else if (parallelCores <= 1 && inherits(testingSet, 'CrossValidation')) {
    results <- data.frame()
    nCV <- length(testingSet$CrossValidation_list)
    pb <- getProgressBar('Fitting Cross-Validation :what  [:bar] :percent;  Time elapsed: :elapsed', nCV)

    for (actual_CV in seq_len(nCV)) {
      if (progressBar) {
        pb$tick(tokens = list(what = paste0(actual_CV, ' out of ', nCV)))
      }

      positionTST <- testingSet$CrossValidation_list[[actual_CV]]

      newtmp <- CVMTME(Y, X, Z1, Z2, nIter, burnIn, thin, bs, digits, positionTST, actual_CV, testingSet$Environments[positionTST])
      results <- rbind(results, newtmp)
    }

    out <- list(results = results, n_cores = parallelCores, nIter = nIter, burnIn = burnIn, thin = thin, executionTime = proc.time()[3] - time.init)
    class(out) <- 'BMTMECV'
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
      CVMTME(Y, X, Z1, Z2, nIter, burnIn, thin, bs, digits,
             testingSet$CrossValidation_list[[actual_CV]], actual_CV,
             testingSet$Environments[testingSet$CrossValidation_list[[actual_CV]]])
    }

    parallel::stopCluster(cl)

    out <- list(results = results,
                n_cores = parallelCores, nIter = nIter, burnIn = burnIn, thin = thin, executionTime = proc.time()[3] - time.init)
    class(out) <- 'BMTMECV'
  } else {
    results <- CVMTME(Y, X, Z1, Z2, nIter, burnIn, thin, bs, digits, testingSet, 1, NA)
    out <- list(results = results, nIter = nIter, burnIn = burnIn, thin = thin, executionTime = proc.time()[3] - time.init)
    class(out) <- 'BMTMECV'
  }
  return(out)
}

CVMTME <- function(Y, X, Z1, Z2, nIter, burnIn, thin, bs, digits, testingSet, iterationNumber, Env){
  fm <- coreMTME(Y, X, Z1, Z2, nIter, burnIn, thin, bs, digits, progressBar = FALSE, testingSet)
  observed <- tidyr::gather(as.data.frame(Y[testingSet, ]), 'Trait', 'Observed')
  predicted <- tidyr::gather(as.data.frame(fm$yHat[testingSet, ]), 'Trait', 'Predicted')
  return(data.frame(Position = testingSet,
                        Environment = Env,
                        Trait = rep(colnames(Y), each = length(testingSet)),
                        Partition = iterationNumber,
                        Observed = round(observed$Observed, digits),
                        Predicted = round(predicted$Predicted, digits)))
}

coreMTME <- function(Y, X, Z1, Z2, nIter, burnIn, thin, bs, digits, progressBar, testingSet) {
  Y[testingSet, ] <- NA

  if ((nIter - burnIn - thin) < 0L) {
    stop("nIter must be greater than thin+burnIn")
  }

  pb <- progress::progress_bar$new(format = "Fitting the model [:bar]; Time remaining: :eta",
                                   total = nIter/20L, clear = FALSE, show_after = 0)

  dMVNorm_i <- function(x_i, SigmaInv, mu, log = TRUE) {
    ## works for a single random draw and requires SigmaInv
    e <- as.matrix(x_i - mu)
    out <- -(length(e)/2 * log(2 * pi)) + log(det(SigmaInv))/2 - (crossprod(e, SigmaInv) %*% e)/2
    if (!log) {
      out <- exp(out)
    }
    return(out)
  }

  #######Function for block sampling of norma-multivariate data#######
  rmv_f <- function(ps, c, A, x) {
    A    <- (A + t(A))/2
    p    <- dim(A)[1L]
    k    <- floor(p / ps) # Numbers of blocks
    r1   <- p - k * ps
    ps_1 <- ps + r1 %/% k
    r2   <- p - k * ps_1
    ps_2 <- ps_1 + 1L
    k1   <- k - r2
    tmp  <- 0L
    for (i in seq_len(k1)) {
      tmp1   <- tmp + ps_1
      Pos_i  <- (tmp + 1L):tmp1
      tmp    <- tmp1
      A_ii   <- A[Pos_i, Pos_i]
      EigenA <- eigen(A_ii)
      d_A    <- EigenA$values
      V_A    <- EigenA$vectors
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

      mu_i     <- c(A_ii_inv %*% (c[Pos_i] - A[Pos_i, -Pos_i] %*% x[-Pos_i]))
      x[Pos_i] <- c(mvtnorm::rmvnorm(1, mu_i, A_ii_inv))
    }

    if (r2 != 0L) {
      for (i in (k1 + 1L):k) {
        tmp1 <- tmp + ps_2
        Pos_i <- (tmp + 1L):tmp1
        tmp <- tmp1
        A_ii  <- A[Pos_i, Pos_i]
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
          A_ii_inv <- MatMul(MatMul(V_A_Star, d_A_Star_Inv), t(V_A_Star))
        }
        mu_i     <- c(A_ii_inv %*% (c[Pos_i] - A[Pos_i, -Pos_i] %*% x[-Pos_i]))
        x[Pos_i] <- c(mvtnorm::rmvnorm(1, mu_i, A_ii_inv))
      }
    }
    return(x)
  }

  ##########Sample of Y values that are missing#####################
  sampleY <- function(R, y, yHat, e, missing, observed, traits) {
    if (length(missing) == traits) {
      e <- crossprod(chol(R), rnorm(traits))
      y <- yHat + e
      logLik <- 0L
    } else {
      Roo <- matrix(R[observed, observed], nrow = length(observed), ncol = length(observed))
      RooInv <- chol2inv(chol(Roo))
      Rmm <- matrix(R[missing, missing], nrow = length(missing), ncol = length(missing))
      Rom <- matrix(R[observed, missing], nrow = length(observed), ncol = length(missing))
      Bmo <- crossprod(Rom, RooInv)

      yHat2 <- as.numeric(Bmo %*% e[observed])
      CondVar <- Rmm - Bmo %*% Rom
      L <- chol(CondVar)
      e <- crossprod(L, rnorm(length(missing)))
      y[missing] <- yHat[missing] + yHat2 + e
      tmp <- (y - yHat)[observed]
      logLik <- dMVNorm_i(x_i = tmp, SigmaInv = RooInv, mu = rep(0L, length(observed)), log = TRUE)
    }

    out <- list(y = y, logLik = logLik)
    return(out)
  }

  #########Size of the required matrice######################
  n  <- nrow(X)
  nt <- ncol(Y)
  nJ <- ncol(Z1)
  nI <- n / nJ
  b1 <- matrix(0.1, nrow = nJ, ncol = nt, dimnames = list(NULL, colnames(Y)))
  b2 <- matrix(0L, nrow = nJ * nI, ncol = nt, dimnames = list(NULL, colnames(Y)))
  G_invg <- diag(nJ)

  envNames <- tryCatch({
    sub(".*)", "", colnames(X))
  }, error = function(e) {
    colnames(X)
  })

  ############For saving the full posteriors#######################
  post_beta <- matrix(nrow = nI, ncol = nt, 0L, dimnames = list(NULL, colnames(Y)))
  post_beta_2 <- matrix(nrow = nI, ncol = nt, 0L, dimnames = list(NULL, colnames(Y)))
  post_b1 <- matrix(nrow = nJ, ncol = nt, 0L, dimnames = list(NULL, colnames(Y)))
  # post_b1_2 <- matrix(nrow = nJ, ncol = nt, 0L, dimnames = list(NULL, colnames(Y)))
  post_b2 <- matrix(nrow = nJ * nI, ncol = nt, 0L, dimnames = list(NULL, colnames(Y)))
  # post_b2_2 <- matrix(nrow = nJ * nI, ncol = nt, 0L, dimnames = list(NULL, colnames(Y)))
  post_var_b1 <- matrix(nrow = nt, ncol = nt, 0L, dimnames = list(NULL, colnames(Y)))
  post_var_b1_2 <- matrix(nrow = nt, ncol = nt, 0L, dimnames = list(NULL, colnames(Y)))
  post_var_b2 <- matrix(nrow = nI, ncol = nI, 0L, dimnames = list(NULL, envNames))
  post_var_b2_2 <- matrix(nrow = nI, ncol = nI, 0L, dimnames = list(NULL, envNames))
  post_var_e <- matrix(nrow = nt, ncol = nt, 0L, dimnames = list(NULL, colnames(Y)))
  post_var_e_2 <- matrix(nrow = nt, ncol = nt, 0L, dimnames = list(NULL, colnames(Y)))
  post_yHat <- matrix(nrow = n, ncol = nt, 0L, dimnames = list(NULL, colnames(Y)))
  post_yHat_2 <- matrix(nrow = n, ncol = nt, 0L, dimnames = list(NULL, colnames(Y)))
  # post_logLik <- 0L

  YStar <- Y
  vt <- vE <- ve <- 5L
  R2 <- 0.25
  R2e <- 0.5
  my.model <- lm(YStar ~ X - 1L)
  beta0 <- my.model$coefficient
  m0 <- apply(beta0, 2L, mean, na.rm = TRUE)
  #vcov(my.model)/(summary(my.model)[[1]]$sigma^2)
  Cov_Beta_Inv <- vcov(my.model)

  whichNa <- list()
  whichNa$subjects <- which(apply(FUN = any, X = is.na(Y), MARGIN = 1L))
  nNa <- length(whichNa$subjects)
  if (nNa > 0L) {
    for (k in seq_len(nNa)) {
      whichNa$traits[[k]] <- which(is.na(Y[whichNa$subjects[[k]],]))
      tmpSubject <- whichNa$subject[[k]]
      tmpTraits <- whichNa$traits[[k]]
      YStar[tmpSubject, tmpTraits] <- m0[tmpTraits]
    }
  }
  # tst <- c(as.numeric(whichNa$subjects))
  tX  <- t(X)
  tZ1 <- t(Z1)
  tZ2 <- t(Z2)
  tXX <- MatMul(tX, X)

  tZ1Z1 <- MatMul(tZ1, Z1)
  tZ2Z2 <- MatMul(tZ2, Z2)
  u_b0 <- MatMul(X, beta0)
  u_b1 <- MatMul(Z1, b1)
  u_b2 <- MatMul(Z2, b2)
  betav <- beta0

  VarY <- var(Y, na.rm = TRUE)
  yyy <- matrix(c(t(Y)), ncol = nI, byrow = FALSE)
  St <- VarY * R2 * (vt + 2L)
  SE <- var(yyy, na.rm = TRUE) * R2 * (vE + 2L)
  Se <- VarY * (1L - R2e) * (ve + 2L)
  sigmaT <- St / (vt + 2L)
  EigenT <- eigen(sigmaT)
  d_T <- EigenT$values
  V_T <- EigenT$vectors
  pos_T <- which(d_T > 1e-10)
  d_T_Star <- d_T[pos_T]
  V_T_Star <- V_T[, pos_T]
  sigmaT.Inv <- MatMul(MatMul(V_T_Star, diag(1 / d_T_Star)), t(V_T_Star))
  sigmaEnv <- SE / (vE + 2L)
  EigenE <- eigen(sigmaEnv)
  d_E <- EigenE$values
  V_E <- EigenE$vectors
  pos_E <- which(d_E > 1e-10)
  d_E_Star <- d_E[pos_E]
  V_E_Star <- V_E[, pos_E]
  sigmaEnv.Inv <- MatMul(MatMul(V_E_Star, diag(1 / d_E_Star)), t(V_E_Star))
  yHat <- (u_b0 + u_b1 + u_b2)
  e <- (YStar - u_b0)
  Re <- (var(e, na.rm = TRUE) * (1L - R2e)) / 2
  EigenRe <- eigen(Re)
  d_Re <- EigenRe$values
  V_Re <- EigenRe$vectors
  pos_Re <- which(d_Re > 1e-10)
  d_Re_Star <- d_Re[pos_Re]
  V_Re_Star <- V_Re[, pos_Re]
  Re.Inv <- MatMul(MatMul(V_Re_Star, diag(1 / d_Re_Star)), t(V_Re_Star))
  W <- YStar

  nSums <- 0L

  for (t in 1:nIter) {
    ##### Linear predictor #########################################################
    e <- e + u_b0
    #####Sample of Betas from  a normal distribution ######################
    sigmaB.Inv <- Cov_Beta_Inv / 1E1
    M <- sigmaB.Inv + Krone(Re.Inv, tXX)
    mu_ac <- MatMul(sigmaB.Inv, matrixcalc::vec(beta0)) + MatMul(Krone(Re.Inv, tX), matrixcalc::vec(e))
    betav1 <- t(MVnormvv(mu_ac, M))
    betav <- matrix(betav1, ncol = nt, byrow = FALSE)
    u_b0 <- MatMul(X, betav)
    e <- e - u_b0

    ##### Sample of b1 from a normal distribution ##################################
    e <- e + u_b1
    sigmaTG.Inv <- Krone(sigmaT.Inv, G_invg)
    M1 <- sigmaTG.Inv + Krone(Re.Inv, tZ1Z1)
    mu_b1 <- MatMul(Krone(Re.Inv, tZ1), matrixcalc::vec(e))
    b11 <- rmv_f(ps = bs, c = mu_b1, A = M1, x = b1)
    b1 <- matrix(b11, ncol = nt, byrow = FALSE)
    u_b1 <- MatMul(Z1, b1)
    e <- e - u_b1

    ##### Sample of sigma_Traits#######################################
    G_invb3 <- Krone(sigmaEnv.Inv, G_invg)
    sigmaT <- inv_wishart(vt + nJ + nt + nJ * nI - 1L, MatMul(t(b1), (G_invg %*% b1)) + (t(b2) %*% MatMul(G_invb3, b2)) + St)
    EigenT <- eigen(sigmaT)
    d_T <- EigenT$values
    V_T <- EigenT$vectors
    pos_T <- which(d_T > 1e-10)
    d_T_Star <- d_T[pos_T]
    V_T_Star <- V_T[, pos_T]
    sigmaT.Inv <- MatMul(V_T_Star, MatMul(diag(1 / d_T_Star), t(V_T_Star)))

    ##### Sample of b2 from a normal distribution#########################
    e <- e + u_b2
    sigmaTE.Inv <- Krone(sigmaT.Inv, sigmaEnv.Inv)
    sigmaTGE.Inv <- Krone(sigmaTE.Inv, G_invg)
    M2 <- sigmaTGE.Inv + Krone(Re.Inv, tZ2Z2)
    mu_b2 <- MatMul(Krone(Re.Inv, tZ2), matrixcalc::vec(e))
    b22 <- rmv_f(ps = bs, c = mu_b2, A = M2, x = b2)
    b2 <- matrix(b22, ncol = nt, byrow = FALSE)
    u_b2 <- MatMul(Z2, b2)
    e <- e - u_b2

    ##### Sample of Environments###########################################
    ME3 <- matrix(matrixcalc::vec(t(b2)), ncol = nI, byrow = FALSE)
    G_invb1 <- Krone(G_invg, sigmaT.Inv)
    MEE <- MatMul(t(ME3), (G_invb1 %*% ME3))
    sigmaEnv <- inv_wishart(vE + nI + nJ * nt - 1L, MEE + SE)
    EigenEnv <- eigen(sigmaEnv)
    d_Env <- EigenEnv$values
    V_Env <- EigenEnv$vectors
    pos_Env <- which(d_Env > 1e-10)
    d_Env_Star <- d_Env[pos_Env]
    V_Env_Star <- V_Env[, pos_Env]
    sigmaEnv.Inv <- MatMul(V_Env_Star, MatMul(diag(1 / d_Env_Star), t(V_Env_Star)))

    ##### Sample of sig.e##################################################
    yHat <- W - e
    Re <- inv_wishart(ve + n + nt - 1, MatMul(t(e), e) + Se)
    EigenRe <- eigen(Re)
    d_Re <- EigenRe$values
    V_Re <- EigenRe$vectors
    pos_Re <- which(d_Re > 1e-10)
    d_Re_Star <- d_Re[pos_Re]
    V_Re_Star <- V_Re[, pos_Re]
    Re.Inv <- MatMul(V_Re_Star, MatMul(diag(1 / d_Re_Star), t(V_Re_Star)))

    #########Sample in case of missing values#################
    for (j in seq_len(nNa)) {
      subject <- whichNa$subject[[j]]
      missing <- whichNa$traits[[j]]
      observed <- seq_len(nt)[-missing]
      tmp <- sampleY(R = Re, y = Y[subject,], yHat = yHat[subject,], e = e[subject,], missing = missing,
                     observed = observed, traits = nt)
      W[subject,] <- tmp$y
      e[subject,] <- W[subject,] - yHat[subject, ]
    }

    ##### Saving output ###################################################
    if (progressBar && t %% 20L == 0L) {
      pb$tick()
    }

    if ((t > burnIn) & (t %% thin == 0L)) {
      nSums <- nSums + 1L
      k <- (nSums - 1L) / (nSums)
      post_beta <- post_beta * k + betav / nSums
      post_beta_2 <- post_beta_2 * k + (betav ^ 2) / nSums
      post_b1 <- post_b1 * k + b1 / nSums
      post_b2 <- post_b2 * k + b2 / nSums
      post_var_b1 <- post_var_b1 * k + sigmaT / nSums
      post_var_b1_2 <- post_var_b1_2 * k + (sigmaT ^ 2) / nSums
      post_var_b2 <- post_var_b2 * k + sigmaEnv / nSums
      post_var_b2_2 <- post_var_b2_2 * k + (sigmaEnv ^ 2) / nSums
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
        b2 = round(post_b2, digits),
        vare = round(post_var_e, digits),
        SD.vare = round(sqrt(post_var_e_2 - post_var_e ^ 2), digits),
        varEnv = round(post_var_b2, digits),
        SD.varEnv = round(sqrt(post_var_b2_2 - post_var_b2 ^ 2), digits),
        varTrait = round(post_var_b1, digits),
        SD.varTrait = round(sqrt(post_var_b1_2 - post_var_b1 ^ 2), digits),
        NAvalues = nNa
      )
    }
  }
  # class(out) <- 'BMTME'
  return(out)
}

