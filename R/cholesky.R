#' Cholesky
#'
#' Compute the Cholesky factorization of a non-real symmetric positive-definite square matrix.
#'
#' @param a (numeric - matrix) an object to apply this method, it could be non positive-definite matrices.
#'
#' @export
#'
cholesky <- function(a) {
  n <- dim(a)[1]
  root <- matrix(0, n, n)

  for (i in 1:n) {
    sum2 <- 0
    if (i > 1) {
      sum2 <- sum(root[i, 1:(i - 1)] ^ 2)
    }

    x <- a[i, i] - sum2

    if (x < 0) {
      x <- 0
    }

    root[i, i] <- sqrt(x)

    if (i < n) {
      for (j in (i + 1):n) {
        if (root[i, i] == 0) {
          x <- 0
        }
        else {
          sum2 <- 0
          if (i > 1) {
            sum2 <- root[i, 1:(i - 1)] %*% t(t(root[j, 1:(i - 1)]))
          }
          x <- (a[i, j] - sum2) / root[i, i]
        }
        root[j, i] <- x
      }
    }
  }
  return(root)
}
