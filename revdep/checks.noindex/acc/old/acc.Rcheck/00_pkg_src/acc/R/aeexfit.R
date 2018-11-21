#' @export
#' @importFrom utils head tail 
#' @importFrom plyr ddply
#' @importFrom nleqslv nleqslv
#' @importFrom stats model.matrix
#' @importFrom methods getClass
#' @importFrom Rcpp evalCpp
#' @useDynLib acc



##############################################################################
# User's Main Function for AEEX
##############################################################################
aeexfit <- function(formula, data, weight=NULL, se="Sandwich", control=list(), boot=NULL) {
  
  method <- "AEEX"
  Call <- match.call()
  fm <- formula
  
  #if (is.null(weight)) {
  #  weight <- rep(1,nrow(data[!duplicated(data[,colnames(data)==formula[[2]]$ID]),]))
  #} 
  
  # A PanelSurv object
  obj <- eval(formula[[2]], data)
  
  # Combine respones data frame and covariate data frame (remove intercept column)
  # Multiple rows per subject
  formula[[2]] <- NULL
  
  if (formula == ~ 1) {
    DF <- cbind(obj$psDF, zero=0)
  } else {
    DF <- cbind(obj$psDF, model.matrix(formula, data))[, -4]
  }
  
  DF <- DF[order(DF$ID, DF$time), ]
  
  # Design matrix, one row per subject
  X <- as.matrix(ddply(DF, "ID", head, n=1)[, -c(1:3)])
  
  # Create an Engine object
  engine.control <- control[names(control) %in% names(attr(getClass(method), "slots"))]
  engine <- do.call("new", c(list(Class=method), engine.control))
  
  if (length(engine@betaInit) == 1 & ncol(X) > 1)
    engine@betaInit <- rep(engine@betaInit, ncol(X))
  if (length(engine@betaInit) > 1 & length(engine@betaInit) != ncol(X))
    stop("Invalid length of initial beta values!")
  
  # Create a StdErr object
  #if(se == "NULL"){
  #  stdErr <- NULL}
  #if(se != "NULL"){
  stdErr.control <- control[names(control) %in% names(attr(getClass(se), "slots"))]
  stdErr <- do.call("new", c(list(Class=se), stdErr.control))
  #}
  
  if(se=="Sandwich"){
    fit <- doPanelFit.AEEX.Sandwich(DF=DF, panelMatrix=obj$panelMatrix, timeGrid=obj$timeGrid,
                                    X=X, engine=engine,weight=weight)
  }
  
  if(se=="Bootstrap"){
    fit <- doPanelFit.AEEX.Bootstrap(DF=DF, panelMatrix=obj$panelMatrix, timeGrid=obj$timeGrid,
                                     X=X, engine=engine,weight=weight, boot)
  }  
  
  ret = list(formula=fm, beta=fit$beta, 
             baseline=fit$baseline,
             timeGrid=fit$timeGrid,
             lambda=fit$lambda,
             convergence=fit$convergence,
             iter=fit$iter,
             betaSE=fit$betaSE,
             betaVar=fit$betaVar,
             baselineSE=fit$baselineSE)
             #U1=fit$U1,U2=fit$U2,U=fit$U,V=fit$V,A=fit$A,B=fit$B,V=fit$V,X=fit$X,
             #panelMatrix=fit$panelMatrix,lambda=fit$lambda)
  
  class(ret) <- "aeefit"
  ret
  
}
