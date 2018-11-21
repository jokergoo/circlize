#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]

//[[Rcpp::export]]
arma::mat matrixInverse(arma::mat S){
	arma::mat InvS=inv(S);
	return(InvS);
}
