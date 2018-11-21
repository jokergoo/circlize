#' @export
#' @importFrom stats printCoefmat pnorm
#' @importFrom methods setClass setMethod
#' @importFrom Rcpp evalCpp
#' @useDynLib acc
#' @method summary aeexfit


summary.aeexfit <- function(object, digits = 3, dig.tst = 2, ...) {
  #if(is.null(digits)) digits <- options()$digits
  #else options(digits = digits)
  cat("\nCall:\n");   print(object$formula)
  cat("\nCoefficients:\n");  
  z <- object$beta/object$betaSE
  pval <- (pnorm(-abs(z)))*2
  # form <- unlist(strsplit(gsub("[[:space:]]", "", as.character(fitted2$formula)), "[~,+]"))
  form <- unlist(strsplit(gsub("[[:space:]]", "", as.character(object$formula)), "[~,+]"))
  mylength <- length(form)
  #betalength <- length(fitted2$beta)
  betalength <- length(object$beta)
  coefnames <- form[(mylength-betalength+1):mylength]
  cmat <- cbind(object$beta,exp(object$beta),object$betaSE,as.vector(z),as.vector(pval))              
  colnames(cmat) <- c("coef","exp(coef)","se(coef)","z","Pr(>|z|)")
  rownames(cmat) <- coefnames
  printCoefmat(cmat, digits, dig.tst,
               signif.stars = TRUE,
               signif.legend = TRUE)
}
