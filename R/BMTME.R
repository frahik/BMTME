#' BMTME
#'
#' @param Y no definition available.
#' @param X no definition available.
#' @param Z1 no definition available.
#' @param Z2 no definition available.
#' @param nIter no definition available.
#' @param burnIn no definition available.
#' @param thin no definition available.
#' @param bs no definition available.
#'
#' @return
#'
#' @importFrom stats lm rnorm var vcov
#'
#' @export
#'
#' @examples
#' @useDynLib BMTME
BMTME <- function(Y, X, Z1, Z2, nIter, burnIn, thin, bs) {
  if ((nIter - burnIn - thin) < 0) {
    stop("nIter must be greater than thin+burnIn")
  }

  dMVNorm_i <- function(x_i, SigmaInv, mu, log = TRUE) {
    ## works for a single random draw and requires SigmaInv
    e <- as.matrix(x_i - mu)
    out <- -(length(e)/2 * log(2 * pi)) + log(det(SigmaInv))/2 - (crossprod(e, SigmaInv) %*%
                                                                    e)/2
    if (!log) {
      out <- exp(out)
    }
    return(out)
  }

  #######Function for block sampling of norma-multivariate data#######
  rmv_f <- function(ps, c, A, x) {
    p    <- dim(A)[1]
    k    <- floor(p / ps) # Numbers of blocks
    r1   <- p - k * ps
    ps_1 <- ps + r1 %/% k
    r2   <- p - k * ps_1
    ps_2 <- ps_1 + 1
    k1   <- k - r2
    tmp  <- 0
    for (i in 1:k1) {
      tmp1   <- tmp + ps_1
      Pos_i  <- (tmp + 1):tmp1
      tmp    <- tmp1
      A_ii   <- A[Pos_i, Pos_i]
      EigenA <- eigen(A_ii)
      d_A    <- EigenA$values
      V_A    <- EigenA$vectors
      pos_A1 <- which(d_A > 1e-10)
      if (identical(pos_A1, integer(0))) {
        pos_A <- 1
      } else {
        pos_A <- pos_A1
      }

      d_A_Star <- d_A[pos_A]
      V_A_Star <- V_A[, pos_A]

      if (length(pos_A) == 1) {
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

    if (r2 != 0) {
      for (i in (k1 + 1):k) {
        tmp1 <- tmp + ps_2
        Pos_i <- (tmp + 1):tmp1
        tmp <- tmp1
        A_ii  <- A[Pos_i, Pos_i]
        EigenA <- eigen(A_ii)
        d_A <- EigenA$values
        V_A <- EigenA$vectors
        pos_A1 <- which(d_A > 1e-10)
        if (identical(pos_A1, integer(0))) {
          pos_A <- 1
        } else {
          pos_A <- pos_A1
        }

        d_A_Star <- d_A[pos_A]
        V_A_Star <- V_A[, pos_A]

        if (length(pos_A) == 1) {
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

    x
  }

  ##########Sample of Y values that are missing#####################
  sampleY <- function(R, y, yHat, e, missing, observed, traits) {
    if (length(missing) == traits) {
      e <- crossprod(chol(R), rnorm(traits))
      y <- yHat + e
      logLik <- 0
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
      logLik <- dMVNorm_i(x_i = tmp, SigmaInv = RooInv, mu = rep(0, length(observed)), log = TRUE)
    }

    out <- list(y = y, logLik = logLik)
    return(out)
  }

  #########Size of the required matrice######################
  n  <- nrow(X)
  nt <- ncol(Y)
  nJ <- ncol(Z1)
  nI <- n / nJ
  b1 <- matrix(0.1, nrow = nJ, ncol = nt)
  b2 <- matrix(0, nrow = nJ * nI, ncol = nt)
  G_invg <- diag(nJ)

  ############For saving the full posteriors#######################
  post_beta <- matrix(nrow = nI, ncol = nt, 0)
  post_beta_2 <- matrix(nrow = nI, ncol = nt, 0)
  post_b1 <- matrix(nrow = nJ, ncol = nt, 0)
  post_b1_2 <- matrix(nrow = nJ, ncol = nt, 0)
  post_b2 <- matrix(nrow = nJ * nI, ncol = nt, 0)
  post_b2_2 <- matrix(nrow = nJ * nI, ncol = nt, 0)
  post_var_b1 <- matrix(nrow = nt, ncol = nt, 0)
  post_var_b1_2 <- matrix(nrow = nt, ncol = nt, 0)
  post_var_b2 <- matrix(nrow = nI, ncol = nI, 0)
  post_var_b2_2 <- matrix(nrow = nI, ncol = nI, 0)
  post_var_e <- matrix(nrow = nt, ncol = nt, 0)
  post_var_e_2 <- matrix(nrow = nt, ncol = nt, 0)
  post_yHat <- matrix(nrow = n, ncol = nt, 0)
  post_yHat_2 <- matrix(nrow = n, ncol = nt, 0)
  post_logLik <- 0

  YStar <- Y
  vt <- vE <- ve <- 5
  R2 <- 0.25
  R2e <- 0.5
  my.model <- lm(YStar ~ X - 1)
  beta0 <- my.model$coefficient
  m0 <- apply(beta0, 2, mean, na.rm = T)
  #vcov(my.model)/(summary(my.model)[[1]]$sigma^2)
  Cov_Beta_Inv <- vcov(my.model)

  whichNa <- list()
  whichNa$subjects <- which(apply(FUN = any, X = is.na(Y), MARGIN = 1))
  nNa <- length(whichNa$subjects)
  if (nNa > 0) {
    for (k in 1:nNa) {
      whichNa$traits[[k]] <- which(is.na(Y[whichNa$subjects[[k]],]))
      tmpSubject <- whichNa$subject[[k]]
      tmpTraits <- whichNa$traits[[k]]
      YStar[tmpSubject, tmpTraits] <- m0[tmpTraits]
    }
  }
  tst <- c(as.numeric(whichNa$subjects))
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

  VarY <- var(Y, na.rm = T)
  yyy <- matrix(c(t(Y)), ncol = nI, byrow = F)
  St <- VarY * R2 * (vt + 2)
  SE <- var(yyy, na.rm = T) * R2 * (vE + 2)
  Se <- VarY * (1 - R2e) * (ve + 2)
  sigmaT <- St / (vt + 2)
  EigenT <- eigen(sigmaT)
  d_T <- EigenT$values
  V_T <- EigenT$vectors
  pos_T <- which(d_T > 1e-10)
  d_T_Star <- d_T[pos_T]
  V_T_Star <- V_T[, pos_T]
  sigmaT.Inv <- MatMul(MatMul(V_T_Star, diag(1 / d_T_Star)), t(V_T_Star))
  sigmaEnv <- SE / (vE + 2)
  EigenE <- eigen(sigmaEnv)
  d_E <- EigenE$values
  V_E <- EigenE$vectors
  pos_E <- which(d_E > 1e-10)
  d_E_Star <- d_E[pos_E]
  V_E_Star <- V_E[, pos_E]
  sigmaEnv.Inv <- MatMul(MatMul(V_E_Star, diag(1 / d_E_Star)), t(V_E_Star))
  yHat <- (u_b0 + u_b1 + u_b2)
  e <- (YStar - u_b0)
  Re <- (var(e, na.rm = TRUE) * (1 - R2e)) / 2
  EigenRe <- eigen(Re)
  d_Re <- EigenRe$values
  V_Re <- EigenRe$vectors
  pos_Re <- which(d_Re > 1e-10)
  d_Re_Star <- d_Re[pos_Re]
  V_Re_Star <- V_Re[, pos_Re]
  Re.Inv <- MatMul(MatMul(V_Re_Star, diag(1 / d_Re_Star)), t(V_Re_Star))
  W <- YStar

  nSums <- 0

  for (t in 1:nIter) {
    ##### Linear predictor #########################################################
    e <- e + u_b0
    #####Sample of Betas from  a normal distribution ######################
    sigmaB.Inv <- Cov_Beta_Inv / 1E1
    M <- sigmaB.Inv + Krone(Re.Inv, tXX)
    mu_ac <- MatMul(sigmaB.Inv, matrixcalc::vec(beta0)) + MatMul(Krone(Re.Inv, tX), matrixcalc::vec(e))
    betav1 <- t(MVnormvv(mu_ac, M))
    betav <- matrix(betav1, ncol = nt, byrow = F)
    u_b0 <- MatMul(X, betav)
    e <- e - u_b0

    ##### Sample of b1 from a normal distribution ##################################
    e <- e + u_b1
    sigmaTG.Inv <- Krone(sigmaT.Inv, G_invg)
    M1 <- sigmaTG.Inv + Krone(Re.Inv, tZ1Z1)
    mu_b1 <- MatMul(Krone(Re.Inv, tZ1), matrixcalc::vec(e))
    b11 <- rmv_f(ps = bs, c = mu_b1, A = M1, x = b1)
    b1 <- matrix(b11, ncol = nt, byrow = F)
    u_b1 <- MatMul(Z1, b1)
    e <- e - u_b1

    ##### Sample of sigma_Traits#######################################
    G_invb3 <- Krone(sigmaEnv.Inv, G_invg)
    sigmaT <- inv_wishart(vt + nJ + nt + nJ * nI - 1, MatMul(t(b1), (G_invg %*% b1)) + (t(b2) %*% MatMul(G_invb3, b2)) + St)
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
    b2 <- matrix(b22, ncol = nt, byrow = F)
    u_b2 <- MatMul(Z2, b2)
    e <- e - u_b2

    ##### Sample of Environments###########################################
    ME3 <- matrix(matrixcalc::vec(t(b2)), ncol = nI, byrow = F)
    G_invb1 <- Krone(G_invg, sigmaT.Inv)
    MEE <- MatMul(t(ME3), (G_invb1 %*% ME3))
    sigmaEnv <- inv_wishart(vE + nI + nJ * nt - 1, MEE + SE)
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
    if ((nNa > 0)) {
      for (j in 1:nNa) {
        subject <- whichNa$subject[[j]]
        missing <- whichNa$traits[[j]]
        observed <- (1:nt)[-missing]
        tmp <- sampleY(R = Re, y = Y[subject,], yHat = yHat[subject,], e = e[subject,], missing = missing,
                       observed = observed, traits = nt)
        W[subject,] <- tmp$y
        e[subject,] <- W[subject,] - yHat[subject, ]
      }
    }
    ##### Saving output ###################################################
    if (t %% 10 == 0) {
      cat('Iter', t, 'Sigmae=', c(Re), '\n')
    }

    if ((t > burnIn) & (t %% thin == 0)) {
      nSums <- nSums + 1
      k <- (nSums - 1) / (nSums)
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
        nIter = nIter,
        burnIn = burnIn,
        thin = thin,
        dfe = ve,
        Se = Se,
        yHat = post_yHat,
        SD.yHat = sqrt(post_yHat_2 - (post_yHat ^ 2)),
        beta = post_beta,
        SD.beta = sqrt(post_beta_2 - post_beta ^ 2),
        b1 = post_b1,
        b2 = post_b2,
        vare = post_var_e,
        SD.vare = sqrt(post_var_e_2 - post_var_e ^ 2),
        varEnv = post_var_b2,
        SD.varEnv = sqrt(post_var_b2_2 - post_var_b2 ^ 2),
        varTrait = post_var_b1,
        SD.varTrait = sqrt(post_var_b1_2 - post_var_b1 ^ 2)
      )
    }
  }
  return(out)
}




