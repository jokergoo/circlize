#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix estep(NumericMatrix A, NumericVector L){
     Rcpp::NumericMatrix Am(A);
     Rcpp::NumericVector lambda(L);
     // Get number of rows and columns for from the input matrix
     int nrows = Am.nrow();
     int ncolumns = Am.ncol();
     // Create a E matrix
     NumericMatrix e(nrows,ncolumns);

     // Just make an integer vector to keep track of stuff
     IntegerVector v = Rcpp::seq(0, ncolumns-1);
     for (int m = 0; m < nrows; m++) {
     IntegerVector end = v[Rcpp::is_finite(Am(m,_))];
     int n = end.size();
     NumericVector start(n);
     NumericVector test(n);
     start(0) = 0;
     for (int i = 1; i < n; i++) {
         start(i) = end(i-1)+1;
     }
     for(int t = 0; t < n; t++){
     // start 0  end  5
     int sqn = end(t)-start(t);
     IntegerVector sqnv = Rcpp::seq(start(t),end(t)); 
     int sqnvn = sqnv.size();
     double mysum = 0;
     for (int g=0; g < sqnvn; g++) {
     mysum = mysum + lambda(sqnv(g));
     }

     if( sqn > 0 ){

     for(int r=0; r < sqnvn; r++){
     e(m,sqnv(r)) = Am(m,sqnv(sqnvn-1))*lambda(sqnv(r))/mysum; 
     }    
     }

     if( sqn == 0 ){
     e(m,sqnv(0)) = Am(m,sqnv(0))*lambda(sqnv(0))/mysum;    
     }
     }
     }
     return e;
}
