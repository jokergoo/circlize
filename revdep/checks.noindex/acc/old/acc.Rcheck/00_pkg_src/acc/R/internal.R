#' @importFrom utils head tail 
#' @importFrom plyr ddply
#' @importFrom nleqslv nleqslv
#' @importFrom methods setClass setMethod
#' @importFrom stats uniroot stepfun var sd quantile
#' @importFrom Rcpp evalCpp
#' @useDynLib acc


##############################################################################
# Augmented Estimating Equations (AEE)
##############################################################################
doPanelFit.AEE <- function(DF, panelMatrix, timeGrid, X, engine, weight) {
  
  N <- nrow(panelMatrix)
  K <- ncol(panelMatrix)
  
  eStep <- function(lambda) {
    e <- matrix(0, N, K)
    
    for (i in 1:N) {
      end <- which(!is.na(panelMatrix[i, ]))
      start <- c(1, head(end, -1) + 1)
      
      for (j in which(panelMatrix[i, end] > 0)) {
        sq <- seq(start[j], end[j])
        e[i, sq] <- panelMatrix[i, end[j]] * lambda[sq] / sum(lambda[sq])
      }
    }
    e
  }
  
  if (is.null(weight)) {
    weight <- rep(1,nrow(X))
  } 
  
  # ncol(X) dimensional nonlinear equation
  f <- function(beta, e, weight) {
    lambda <- c(colSums(e)) / c(t(r) %*% exp(X %*% beta))
    c(t(X) %*% diag(weight) %*% (rowSums(e) - c(exp(X %*% beta)) * c(r %*% lambda)))
  }
  
  sStep <- function(f, beta, e, weight) {
    if (ncol(X) == 1) {
      beta <- uniroot(f, engine@interval, e=e, weight=weight)$root
    } else {
      beta <- nleqslv(beta, function(x) f(x, e, weight))$x
    }
    
    lambda <- colSums(e) / c(t(r) %*% exp(X %*% beta))
    list(beta=beta,
         lambda=lambda)
  }
  
  ##############################
  # Initialize e and r matrix
  e <- r <- matrix(0, N, K)
  for (i in 1:N) {
    set <- which(!is.na(panelMatrix[i, ]))
    mi <- tail(set, 1)
    dset <- diff(c(0, set))
    
    e[i, 1:mi] <- rep(panelMatrix[i, set] / dset, dset)
    r[i, 1:mi] <- 1
  }
  
  convergence <- 1
  sRes <- sStep(f, engine@betaInit, e, weight)
  
  for (i in 2:engine@maxIter) {
    #e <- eStep(sRes$lambda)
    e <- estep(panelMatrix,sRes$lambda)
    
    betaPre <- sRes$beta
    sRes <- sStep(f, sRes$beta, e, weight)
    s <- sRes$beta - betaPre
    
    if (max(abs(s)) < engine@absTol | max(abs(s / betaPre)) < engine@relTol) {
      convergence <- 'Converged'
      break
    }
  }
  iter <- i
  
  list(beta=sRes$beta,
       baseline=stepfun(timeGrid, cumsum(c(0, sRes$lambda))),
       timeGrid=timeGrid,
       lambda=sRes$lambda,
       convergence=convergence,
       iter=iter)
}


##############################################################################
# Extension of Augmented Estimating Equations (AEEX)
##############################################################################
doPanelFit.AEEX <- function(DF, panelMatrix, timeGrid, X, engine, weight) {
  N <- nrow(panelMatrix)
  K <- ncol(panelMatrix)
  
  eStep <- function(lambda, a) {
    e <- matrix(0, N, K)
    
    for (i in 1:N) {
      end <- which(!is.na(panelMatrix[i, ]))
      start <- c(1, head(end, -1) + 1)
      
      for (j in which(panelMatrix[i, end] > 0)) {
        sq <- seq(start[j], end[j])
        e[i, sq] <- panelMatrix[i, end[j]] * lambda[sq] / sum(lambda[sq])
      }
      
      if (tail(end, 1) < K) {
        sq <- seq(tail(end, 1) + 1, K)
        e[i, sq] <- (sum(panelMatrix[i, end]) + a) * lambda[sq] /
          (sum(lambda[-sq]) + a)
      }
    }
    e
  }
  
  if (is.null(weight)) {
    weight <- rep(1,nrow(X))
  } 
  
  # ncol(X) dimensional nonlinear equation
  f <- function(beta, e, weight) {
    lambda <- c(colSums(e)) / sum(exp(X %*% beta))
    c(t(X) %*% diag(weight) %*% (rowSums(e) - c(exp(X %*% beta)) * sum(lambda)))
  }
  
  sStep <- function(f, beta, e, weight) {
    if (ncol(X) == 1) {
      beta <- uniroot(f, engine@interval, e=e, weight=weight)$root
    } else {
      beta <- nleqslv(beta, function(x) f(x, e, weight))$x
    }
    
    lambda <- colSums(e) / sum(exp(X %*% beta))
    list(beta=beta,
         lambda=lambda)
  }
  
  ##############################
  # Initialize e matrix
  e <- matrix(0, N, K)
  for (i in 1:N) {
    sq <- which(!is.na(panelMatrix[i, ]))
    mi <- tail(sq, 1)
    dsq <- diff(c(0, sq))
    
    e[i, 1:mi] <- rep(panelMatrix[i, sq] / dsq, dsq)
    
    if (mi < K) {
      e[i, (mi + 1):K] <- sum(panelMatrix[i, sq]) / mi
    }
  }
  
  a <- engine@a
  
  # Iteration
  convergence <- 1
  sRes <- sStep(f, engine@betaInit, e, weight)
  for (i in 2:engine@maxIter) {
    e <- estepAEE(panelMatrix,sRes$lambda,a)
    
    betaPre <- sRes$beta
    sRes <- sStep(f, sRes$beta, e, weight)
    s <- sRes$beta - betaPre
    
    if (max(abs(s)) < engine@absTol | max(abs(s / betaPre)) < engine@relTol) {
      convergence <- 'Converged'
      break
    }
  }
  iter <- i
  
  list(beta=sRes$beta,
       baseline=stepfun(timeGrid, cumsum(c(0, sRes$lambda))),
       timeGrid=timeGrid,
       lambda=sRes$lambda,
       convergence=convergence,
       iter=iter)
}



##############################################################################
# Bootstrap variance estimation for AEE
##############################################################################
doPanelFit.AEE.Bootstrap <- function(DF, panelMatrix, timeGrid, X, engine, weight, boot) {
  N <- nrow(panelMatrix)
  K <- ncol(panelMatrix)
  tau <- max(timeGrid)
  
  res <- doPanelFit.AEE(DF, panelMatrix, timeGrid, X, engine, weight)
  engine@betaInit <- res$beta
  
  R <- boot
  betaMatrix <- matrix(0, R, length(res$beta))
  baselineMatrix <- matrix(NA, R, K)
  convergence <- rep(0, R)
  
  uID <- unique(DF$ID)
  
  for (i in 1:R) {
    index <- sort(sample(1:N, size=N, replace=TRUE))
    
    mylist <- apply(matrix(index), 1, function(x) which(DF$ID == uID[x]))
    DF2 <- DF[unlist(mylist), ]
    DF2$ID <- rep(1:N, unlist(lapply(mylist, length)))
    
    panelMatrix2 <- panelMatrix[index, ]
    X2 <- as.matrix(X[index, ])
    subCol <- which(colSums(!is.na(panelMatrix2)) > 0)
    panelMatrix2 <- panelMatrix2[, subCol]
    timeGrid2 <- timeGrid[subCol]
    
    res2 <- doPanelFit.AEE(DF2, panelMatrix2, timeGrid2, X2, engine, weight)
    betaMatrix[i, ] <- res2$beta
    
    tau2 <- max(timeGrid2)
    sq <- which(timeGrid <= tau2)
    if (tau2 < tau)
      baselineMatrix[i, sq] <- res2$baseline(timeGrid[sq])
    else
      baselineMatrix[i, ] <- res2$baseline(timeGrid)
    
    convergence[i] <- res2$convergence
  }
  
  converged <- which(convergence == 0)
  betaVar <- var(betaMatrix[converged, ], na.rm=TRUE)
  betaSE <- sqrt(diag(as.matrix(betaVar)))
  baselineSE <- sd(baselineMatrix[converged, ], na.rm=TRUE)
  
  # 2.5% and 97.5% quantiles of baseline bootstrap estimates, 2*K
  baselineQT <- apply(baselineMatrix[converged, ], 2, quantile,
                      probs=c(0.025, 0.975), na.rm=TRUE, names=FALSE)
  
  c(res, list(betaSE=betaSE, betaVar=betaVar,
              baselineSE=baselineSE, baselineQT=baselineQT, R=length(converged)))
}






##############################################################################
# Bootstrap variance estimation for AEEX
##############################################################################
doPanelFit.AEEX.Bootstrap <- function(DF, panelMatrix, timeGrid, X, engine, weight, boot) {
  N <- nrow(panelMatrix)
  K <- ncol(panelMatrix)
  tau <- max(timeGrid)
  
  res <- doPanelFit.AEEX(DF, panelMatrix, timeGrid, X, engine, weight)
  engine@betaInit <- res$beta
  
  R <- boot
  betaMatrix <- matrix(0, R, length(res$beta))
  baselineMatrix <- matrix(NA, R, K)
  convergence <- rep(0, R)
  
  uID <- unique(DF$ID)
  
  for (i in 1:R) {
    index <- sort(sample(1:N, size=N, replace=TRUE))
    
    mylist <- apply(matrix(index), 1, function(x) which(DF$ID == uID[x]))
    DF2 <- DF[unlist(mylist), ]
    DF2$ID <- rep(1:N, unlist(lapply(mylist, length)))
    
    panelMatrix2 <- panelMatrix[index, ]
    X2 <- as.matrix(X[index, ])
    subCol <- which(colSums(!is.na(panelMatrix2)) > 0)
    panelMatrix2 <- panelMatrix2[, subCol]
    timeGrid2 <- timeGrid[subCol]
    
    res2 <- doPanelFit.AEEX(DF2, panelMatrix2, timeGrid2, X2, engine, weight)
    betaMatrix[i, ] <- res2$beta
    
    tau2 <- max(timeGrid2)
    sq <- which(timeGrid <= tau2)
    if (tau2 < tau)
      baselineMatrix[i, sq] <- res2$baseline(timeGrid[sq])
    else
      baselineMatrix[i, ] <- res2$baseline(timeGrid)
    
    convergence[i] <- res2$convergence
  }
  
  converged <- which(convergence == 0)
  betaVar <- var(betaMatrix[converged, ], na.rm=TRUE)
  betaSE <- sqrt(diag(as.matrix(betaVar)))
  baselineSE <- sd(baselineMatrix[converged, ], na.rm=TRUE)
  
  # 2.5% and 97.5% quantiles of baseline bootstrap estimates, 2*K
  baselineQT <- apply(baselineMatrix[converged, ], 2, quantile,
                      probs=c(0.025, 0.975), na.rm=TRUE, names=FALSE)
  
  c(res, list(betaSE=betaSE, betaVar=betaVar,
              baselineSE=baselineSE, baselineQT=baselineQT, R=length(converged)))
}



##############################################################################
# Observed information matrix based variance estimation for AEE
##############################################################################
doPanelFit.AEE.Sandwich <- function(DF, panelMatrix, timeGrid, X, engine, weight) {
  N <- nrow(panelMatrix)
  K <- ncol(panelMatrix)
  
  res <- doPanelFit.AEE(DF, panelMatrix, timeGrid, X, engine, weight)
  beta <- res$beta
  lambda <- res$lambda
  
  atRiskMatrix <- matrix(0, N, K)
  lastObs <- apply(panelMatrix, 1, function(x) tail(which(!is.na(x)), 1))
  atRiskMatrix[col(atRiskMatrix) <= lastObs] <- 1
  
  # A is the complete information matrix for all subjects
  A11 <- diag(c(t(exp(X %*% beta)) %*% atRiskMatrix))
  A21 <- t(c(exp(X %*% beta)) * X) %*% atRiskMatrix
  A22 <- t(X) %*% (X * c(exp(X %*% beta)) * c(atRiskMatrix %*% lambda))
  A <- rbind(cbind(A11, t(A21) * lambda),
             cbind(A21, A22))
  
  # B is the missing information matrix for all subjects
  B <- matrix(0, K + ncol(X), K + ncol(X))
  for (i in 1:N) {
    sq <- which(!is.na(panelMatrix[i, ]))
    mi <- panelMatrix[i, sq]
    
    if (is.na(panelMatrix[i, K])) {
      sq <- c(sq, K)
      mi <- c(mi, 0)
    }
    
    dsq <- diff(c(0, sq))
    ndsq <- length(dsq)
    
    # normalize lambda, multinomial
    p <- lambda / rep(diff(c(0, cumsum(lambda)[sq])), dsq)
    p[which(p == Inf)] <- 1
    blkp <- p * diag(1, ndsq)[rep(1:ndsq, dsq), ]
    
    # atRisk (rij) is taken care of
    Xi <- X[i, ]
    B11 <- rep(mi, dsq) * (diag(p) - blkp %*% t(blkp))
    B12 <- rowSums(B11) %*% t(Xi)
    B22 <- sum(B11) * Xi %*% t(Xi)
    B <- B + rbind(cbind(B11, B12),
                   cbind(t(B12), B22))
    # matrix.plot(B)
  }
  
  # Inverse of observed information matrix
  V <- matrixInverse(A - B)
  
  # Regularization
  dgV <- diag(V)
  dgV[which(dgV < 0)] <- 0
  diag(V) <- dgV
  
  # Sandwich estimator
  e <- matrix(0, N, K)
  for (i in 1:N) {
    end <- which(!is.na(panelMatrix[i, ]))
    start <- c(1, head(end, -1) + 1)
    
    for (j in which(panelMatrix[i, end] > 0)) {
      sq <- seq(start[j], end[j])
      e[i, sq] <- panelMatrix[i, end[j]] * lambda[sq] / sum(lambda[sq])
    }
  }
  U1 <- (t(e) - outer(lambda, c(exp(X %*% beta)))) * t(atRiskMatrix)
  U2 <- t(colSums(U1) * X)
  U <- rbind(U1, U2)
  V <- V %*% (U %*% t(U)) %*% t(V)
  
  ##
  betaVar <- V[-c(1:K), -c(1:K)]
  betaSE <- sqrt(diag(as.matrix(betaVar)))
  
  lowOne <- matrix(0, K, K)
  lowOne[row(lowOne) >= col(lowOne)] <- 1
  vLambda <- diag(lowOne %*% V[1:K, 1:K] %*% t(lowOne))
  baselineSE <- sqrt(vLambda)
  
  c(res, list(betaSE=betaSE, betaVar=betaVar, baselineSE=baselineSE))
}


##############################################################################
# Observed information matrix based variance estimation for AEEX
##############################################################################
doPanelFit.AEEX.Sandwich <- function(DF, panelMatrix, timeGrid, X, engine, weight) {
  N <- nrow(panelMatrix)
  K <- ncol(panelMatrix)
  
  res <- doPanelFit.AEEX(DF, panelMatrix, timeGrid, X, engine, weight)
  beta <- res$beta
  lambda <- res$lambda
  
  atRiskMatrix <- matrix(1, N, K)
  
  # A is the complete information matrix for all subjects
  A11 <- diag(c(t(exp(X %*% beta)) %*% atRiskMatrix))
  A21 <- t(c(exp(X %*% beta)) * X) %*% atRiskMatrix
  A22 <- t(X) %*% (X * c(exp(X %*% beta)) * c(atRiskMatrix %*% lambda))
  A <- rbind(cbind(A11, t(A21) * lambda),
             cbind(A21, A22))
  
  # B is the missing information matrix for all subjects
  B <- matrix(0, K + ncol(X), K + ncol(X))
  for (i in 1:N) {
    sq <- which(!is.na(panelMatrix[i, ]))
    mi <- panelMatrix[i, sq]
    
    if (is.na(panelMatrix[i, K])) {
      y1 <- sum(mi)
      mu1 <- sum(lambda[seq(1, tail(sq, 1))])
      mu2 <- sum(lambda[seq(tail(sq, 1) + 1, K)])
      sq <- c(sq, K)
      mi <- c(mi, (y1 + engine@a) * mu2 / (mu1 + engine@a))
    }
    
    dsq <- diff(c(0, sq))
    ndsq <- length(dsq)
    
    # normalize lambda, multinomial
    p <- lambda / rep(diff(c(0, cumsum(lambda)[sq])), dsq)
    p[which(p == Inf)] <- 1
    blkp <- p * diag(1, ndsq)[rep(1:ndsq, dsq), ]
    
    Xi <- X[i, ]
    B11 <- rep(mi, dsq) * (diag(p) - blkp %*% t(blkp))
    
    if (is.na(panelMatrix[i, K])) {
      p[seq(1, K - tail(dsq, 1))] <- 0
      B11 <- B11 + outer(p, p) * (y1 + engine@a) *
        mu2 / (mu2 + engine@a) * (1 + mu1 / (mu2 + engine@a))
    }
    
    B12 <- rowSums(B11) %*% t(Xi)
    B22 <- sum(B11) * Xi %*% t(Xi)
    B <- B + rbind(cbind(B11, B12),
                   cbind(t(B12), B22))
    # matrix.plot(B)
  }
  
  # Inverse of observed information matrix
  V <- matrixInverse(A - B)
  
  # Regularization
  dgV <- diag(V)
  dgV[which(dgV < 0)] <- 0
  diag(V) <- dgV
  
  # Sandwich estimator
  e <- matrix(0, N, K)
  for (i in 1:N) {
    end <- which(!is.na(panelMatrix[i, ]))
    start <- c(1, head(end, -1) + 1)
    
    for (j in which(panelMatrix[i, end] > 0)) {
      sq <- seq(start[j], end[j])
      e[i, sq] <- panelMatrix[i, end[j]] * lambda[sq] / sum(lambda[sq])
    }
  }
  U1 <- (t(e) - outer(lambda, c(exp(X %*% beta)))) * t(atRiskMatrix)
  U2 <- t(colSums(U1) * X)
  U <- rbind(U1, U2)
  V <- V %*% (U %*% t(U)) %*% t(V)
  
  ##
  betaVar <- V[-c(1:K), -c(1:K)]
  betaSE <- sqrt(diag(as.matrix(betaVar)))
  
  lowOne <- matrix(0, K, K)
  lowOne[row(lowOne) >= col(lowOne)] <- 1
  vLambda <- diag(lowOne %*% V[1:K, 1:K] %*% t(lowOne))
  baselineSE <- sqrt(vLambda)
  
  c(res, list(betaSE=betaSE, betaVar=betaVar, baselineSE=baselineSE))
  #c(res, list(betaSE=betaSE, betaVar=betaVar, baselineSE=baselineSE,
  #            U1=U1,U2=U2,U=U,V=V,A=A,B=B,V=V,X=X,panelMatrix=panelMatrix,lambda=lambda))
}


##############################################################################
# Class Definition
##############################################################################
setClass("Engine",
         representation(betaInit="numeric", interval="numeric",
                        maxIter="numeric", absTol="numeric", relTol="numeric"),
         prototype(betaInit=0, interval=c(-5, 5),
                   maxIter=150, absTol=1e-6, relTol=1e-6),
         contains="VIRTUAL")

setClass("AEE",
         representation(),
         prototype(),
         contains="Engine")

#setClass("AEEX",
#         representation(a="numeric"),
#         prototype(maxIter=500, a=0.1),
#         contains="Engine")

setClass("AEEX",
         representation(a="numeric"),
         prototype(a=0.1),
         contains="Engine")


setClass("StdErr")

setClass("Sandwich",
         representation(),
         prototype(),
         contains="StdErr")

setClass("Bootstrap",
         representation(R="numeric"),
         prototype(R=30),
         contains="StdErr")
