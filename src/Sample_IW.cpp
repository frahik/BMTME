// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins("cpp11")]]

#include <RcppArmadillo.h>
#include <algorithm>
#include <iterator>

// [[Rcpp::export]]
arma::mat wishart(unsigned int df, const arma::mat& S ){
  // Dimension of returned wishart
  unsigned int m = S.n_rows;

  arma::mat Z(m, m, arma::fill::zeros);
  arma::mat C(m, m, arma::fill::zeros);
  arma::mat W(m, m, arma::fill::zeros);

  for( unsigned int i = 0; i < m; i++)
    Z(i, i) = sqrt(R::rchisq(df - i));   // filling diagonal with sqrt chisqs

  for( unsigned int j = 0; j < m; j++)
    for( unsigned int i = j+1; i < m; i++)
      Z(i, j) = R::rnorm(0.0, 1.0); // filling lower diagonal with normal(0, 1)

  C = arma::trimatl(Z).t() * arma::chol(S); // lower tri * cholesky
  W = C.t() * C;

  return W;
};

// [[Rcpp::export]]
arma::mat inv_wishart( unsigned int df, const arma::mat& S ) {
  return wishart(df, S.i()).i(); // inverse wishart drawn
};

// [[Rcpp::export]]
arma::rowvec MVnormvv(arma::vec mean, const arma::mat& S ) {
  int ncols = S.n_cols;
  arma::mat LISigmab2(ncols, ncols);
  arma::vec b2std(ncols);
  arma::vec b2Til(ncols);
  //arma::vec b2ps(ncols, arma::fill::zeros);
  arma::mat ISigmab2(ncols, ncols);
  ISigmab2 = S;
  LISigmab2 = arma::chol(ISigmab2);

  // Eq. 9 for b_2 .. idem Eqs. 6
  std::transform(b2std.begin(), b2std.end(), b2std.begin(),
                   [&] (double b2i ) {return(R::rnorm(0.0, 1.0));});

  b2Til = arma::solve(ISigmab2, mean, arma::solve_opts::fast);

  arma::colvec b2ps = (b2Til + arma::solve(arma::trimatu(LISigmab2), b2std));
  return(b2ps.t());
};

// [[Rcpp::export]]
arma::mat  MatMul(arma::mat &X, arma::mat &Y ) {
  arma::mat ans = X * Y;
  return(ans);
};

// [[Rcpp::export]]
arma::mat Krone(const arma::mat & A, const arma::mat & B) {
  return(arma::kron(A, B));
};
