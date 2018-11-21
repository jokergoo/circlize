#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector rollSumCpp(NumericVector x, int n) {   
    int vectorSize = x.size();
    NumericVector myRollSum(vectorSize);
    NumericVector x2(vectorSize);
    myRollSum[n-1] = std::accumulate(x.begin(), x.end()-vectorSize+n, 0.0);
    for(int i = n; i < vectorSize; i++) {
       myRollSum[i] = myRollSum[i-1] + x[i] - x[i-n];
    } 
    std::fill(myRollSum.begin(), myRollSum.end()-vectorSize+n-1, NA_REAL); 
    x2 = wrap(na_omit(myRollSum));
    return x2;
}

