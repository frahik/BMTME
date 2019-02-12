#' Cholesky
#'
#' Compute the Cholesky factorization of a non-real symmetric positive-definite square matrix.
#'
#' @param G (numeric - matrix) an object to apply this method, it could be non positive-definite matrices.
#' @param tolerance (\code{double}) Tolerance level, by default is 1e-10.
#'
#' @export
#'
cholesky <- function(G, tolerance = 1e-10) {
  G <- (G + t(G)) / 2
  EigenA <- eigen(G)
  d_A    <- EigenA$values
  V_A    <- EigenA$vectors
  d_A[which(d_A < tolerance)] <- tolerance*100L
  pos_A1 <- which(d_A > tolerance)
  if (identical(pos_A1, integer(0))) {
    pos_A <- 1L
  } else {
    pos_A <- pos_A1
  }
  d_A_Star <- d_A[pos_A]
  V_A_Star <- V_A[, pos_A]

  if (length(pos_A) == 1L) {
    d_A_Star <- 1L / d_A_Star
    LG <- d_A_Star * sqrt(V_A_Star)
  } else {
    d_A_Star <- diag(d_A_Star)
    LG <- (V_A_Star %*% sqrt(d_A_Star))
  }
  return(LG)
}
