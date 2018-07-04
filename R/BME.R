#' BME
#'
#' @param Y no definition available.
#' @param Z1 no definition available.
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
BME <- function(Y, Z1, nIter, burnIn, thin, bs) {
  if ((nIter - burnIn - thin) < 0) {
    stop("nIter must be greater than thin+burnIn")
  }

  # ps: blocks size
  rmv_f <- function(ps, c, A, x) {
    p    <- dim(A)[1]
    k    <- floor(p / ps)#Numbers of blocks
    r1   <- p - k * ps
    ps_1 <- ps + r1 %/% k
    r2   <- p - k * ps_1
    ps_2 <- ps_1 + 1
    k1   <- k - r2
    tmp  <- 0
    for (i in 1:k1) {
      tmp1  <- tmp + ps_1
      Pos_i <- (tmp + 1):tmp1
      tmp   <- tmp1
      A_ii  <- A[Pos_i, Pos_i]
      EigenA <- eigen(A_ii)
      d_A   <- EigenA$values
      V_A   <- EigenA$vectors
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
      mu_i <- c(A_ii_inv %*% (c[Pos_i] - A[Pos_i, -Pos_i] %*% x[-Pos_i]))
      x[Pos_i] <- c(mvtnorm::rmvnorm(1, mu_i, A_ii_inv))
    }
    if (r2 != 0) {
      for (i in (k1 + 1):k) {
        tmp1 <- tmp + ps_2
        Pos_i <- (tmp + 1):tmp1
        tmp <- tmp1
        A_ii <- A[Pos_i, Pos_i]
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
  b1 <- matrix(0.1, nrow = nJ, ncol = nt)
  G_invg <- diag(nJ)

  post_beta <- matrix(nrow = 1, ncol = nt, 0)
  post_beta_2 <- matrix(nrow = 1, ncol = nt, 0)
  post_b1 <- matrix(nrow = nJ, ncol = nt, 0)
  post_b1_2 <- matrix(nrow = nJ, ncol = nt, 0)
  post_var_b1 <- matrix(nrow = nt, ncol = nt, 0)
  post_var_b1_2 <- matrix(nrow = nt, ncol = nt, 0)
  post_var_e <- matrix(nrow = nt, ncol = nt, 0)
  post_var_e_2 <- matrix(nrow = nt, ncol = nt, 0)
  post_yHat <- matrix(nrow = n, ncol = nt, 0)
  post_yHat_2 <- matrix(nrow = n, ncol = nt, 0)
  post_logLik <- 0

  YStar <- Y
  vt <- vE <- ve <- 5
  R2 <- 0.25
  R2e <- 0.5
  my.model <- lm(YStar ~ 1)
  beta0 <- my.model$coefficient
  Cov_Beta_Inv <- vcov(my.model)

  whichNa <- list()
  whichNa$subjects <- which(apply(FUN = any, X = is.na(Y), MARGIN = 1))
  nNa <- length(whichNa$subjects)
  tst <- c(as.numeric(whichNa$subjects))

  X <- rep(1, n)
  tX <- t(X)
  tZ1 <- t(Z1)
  tXX <- sum(X)
  tZ1Z1 <- MatMul(tZ1, Z1)
  u_b0 <- X %*% beta0
  u_b1 <- MatMul(Z1, b1)
  betav <- beta0

  VarY <- var(Y, na.rm = T)
  St <- VarY * R2 * (vt + 2)
  Se <- VarY * (1 - R2e) * (ve + 2)
  sigmaT <- St / (vt + 2)
  EigenT <- eigen(sigmaT)
  d_T <- EigenT$values
  V_T <- EigenT$vectors
  pos_T <- which(d_T > 1e-10)
  d_T_Star <- d_T[pos_T]
  V_T_Star <- V_T[, pos_T]
  sigmaT.Inv <- MatMul(MatMul(V_T_Star, diag(1 / d_T_Star)), t(V_T_Star))

  yHat <- (u_b0 + u_b1)
  YStar1 <- YStar
  YStar1[tst, ] <- rep(0, nt)

  e <- (YStar1 - u_b0)

  Re <- (var(e, na.rm = TRUE) * (1 - R2e)) / 2
  e <- (YStar1 - u_b0)
  EigenRe <- eigen(Re)
  d_Re <- EigenRe$values
  V_Re <- EigenRe$vectors
  pos_Re <- which(d_Re > 1e-10)
  d_Re_Star <- d_Re[pos_Re]
  V_Re_Star <- V_Re[, pos_Re]
  Re.Inv <- MatMul(MatMul(V_Re_Star, diag(1 / d_Re_Star)), t(V_Re_Star))
  W <- YStar1
  nSums <- 0

  for (t in 1:nIter) {
    logLik <- 0
    ##### Linear predictor #####################################
    e <- e + u_b0

    ##### Sample of Betas from  a normal distribution ###########
    sigmaB.Inv <- Cov_Beta_Inv / 1E1
    M <- sigmaB.Inv + Re.Inv * tXX
    mu_ac <- MatMul(sigmaB.Inv, matrixcalc::vec(beta0)) + MatMul(Krone(Re.Inv, tX), matrixcalc::vec(e))
    betav1 <- t(MVnormvv(mu_ac, M))
    betav <- matrix(betav1, ncol = nt, byrow = F)
    u_b0 <- X %*% betav
    e <- e - u_b0

    ##### Sample of b1 from a normal distribution ###############
    e <- e + u_b1
    sigmaTG.Inv <- Krone(sigmaT.Inv, G_invg)
    M1 <- sigmaTG.Inv + Krone(Re.Inv, tZ1Z1)
    mu_b1 <- MatMul(Krone(Re.Inv, tZ1), matrixcalc::vec(e))
    b11 <- rmv_f(ps = bs, c = mu_b1, A = M1, x = b1)
    b1 <- matrix(b11, ncol = nt, byrow = F)
    u_b1 <- MatMul(Z1, b1)
    e <- e - u_b1

    ##### Sample of sigma_Traits#################################
    tb1b1 <- MatMul(t(b1), (G_invg %*% b1))
    sigmaT <- inv_wishart(vt + nJ + nt - 1,  tb1b1 + St)
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
    if ((nNa > 0)) {
      W[tst, ] = yHat[tst, ] + mvtnorm::rmvnorm(nNa, mean = rep(0, nt), sigma = Re, method = "chol")
      e[tst, ] = W[tst, ] - yHat[tst, ]
    }

    ##### Saving output #########################################
    if (t %% 10 == 0) {
      cat('Iter', t, 'Sigmae=', c(Re), '\n')
    }

    if ((t > burnIn) & (t %% thin == 0)) {
      nSums = nSums + 1
      k = (nSums - 1) / (nSums)
      post_beta = post_beta * k + betav / nSums
      post_beta_2 = post_beta_2 * k + (betav ^ 2) / nSums
      post_b1 = post_b1 * k + b1 / nSums
      post_b1_2 = post_b1_2 * k + (b1 ^ 2) / nSums

      post_var_b1 = post_var_b1 * k + sigmaT / nSums
      post_var_b1_2 = post_var_b1_2 * k + (sigmaT ^ 2) / nSums

      post_var_e = post_var_e * k + Re / nSums
      post_var_e_2 = post_var_e_2 * k + (Re ^ 2) / nSums

      post_yHat = post_yHat * k + yHat / nSums
      post_yHat_2 = post_yHat_2 * k + (yHat ^ 2) / nSums

      out = list(
        nIter = nIter,
        burnIn = burnIn,
        thin = thin,
        dfe = ve,
        Se = Se
      )
      out$yHat = post_yHat
      out$SD.yHat = sqrt(post_yHat_2 - (post_yHat ^ 2))
      out$beta = post_beta
      out$SD.beta = sqrt(post_beta_2 - post_beta ^ 2)
      out$b1 = post_b1
      out$SD.b1 = sqrt(post_b1_2 - post_b1 ^ 2)

      out$vare = post_var_e
      out$SD.vare = sqrt(post_var_e_2 - post_var_e ^ 2)

      out$varTrait = post_var_b1
      out$SD.varTrait = sqrt(post_var_b1_2 - post_var_b1 ^ 2)
    }
  }
  out
}
