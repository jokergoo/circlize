#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix makePanelMatrix(NumericVector T, NumericVector M, NumericVector UID, NumericVector TGD, NumericVector C){
     Rcpp::NumericVector time(T);
     Rcpp::NumericVector minutes(M);
     Rcpp::NumericVector uniqueID(UID);
     Rcpp::NumericVector timeGrid(TGD);
     Rcpp::NumericVector cumLength(C);
     // Get number of rows and columns for from the input matrix
     int nrows = uniqueID.size();
     int ncolumns = timeGrid.size();
     NumericMatrix panelMatrix(nrows,ncolumns);
     std::fill( panelMatrix.begin(), panelMatrix.end(), NumericVector::get_na() ) ;
     IntegerVector sqz = Rcpp::seq(0, cumLength(0));

     for (int z = 0; z < sqz.size()-1; z++) {
     panelMatrix(0, time(sqz(z))-1) = minutes(sqz(z));
     }
     for (int i = 1; i < nrows; i++) {
     IntegerVector sq = Rcpp::seq(cumLength(i-1), cumLength(i));
     for (int z = 0; z < sq.size()-1; z++) {
     panelMatrix(i, time(sq(z))-1) = minutes(sq(z));
     }
     }
     return panelMatrix;
}
