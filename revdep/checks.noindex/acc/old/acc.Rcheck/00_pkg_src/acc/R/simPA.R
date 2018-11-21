#' @export
#' @importFrom utils head tail 
#' @importFrom stats rbinom rgamma rpois na.omit aggregate
#' @importFrom Rcpp evalCpp
#' @useDynLib acc



simPA <- function(n, type='noninf', beta=1.5, minday, maxday){
  
  if(type=='noninf'){k2.min <- k1.min <- minday}
  if(type=='noninf'){k2.num <- k1.num <- maxday}
  
  if(type=='inf'){k1.min <- minday[1]}
  if(type=='inf'){k2.min <- minday[2]}
  if(type=='inf'){k1.num <- maxday[1]}
  if(type=='inf'){k2.num <- maxday[2]}
  
  ## covariate x1 generated from Bernoulli
  x1=rbinom(n,1,0.5); 
  x=cbind(x1)
  beta1=beta
  nu=10;
  parms=c(beta1) 
  phi=rgamma(n,shape=nu,rate=nu)
  
  ## generating the number of observations for each subject
  k1 <- sample(k1.min:k1.num,n,replace=TRUE) 
  k2 <- sample(k1.min:k2.num,n,replace=TRUE) 
  
  if(k1.min == k1.num){
    k1 <- rep(k1.min,n) 
  }
  
  if(k2.min == k2.num){
    k2 <- rep(k2.min,n) 
  }
  
  k=k1*(ifelse(x==1 & phi<=1,1,0))+  k2*(ifelse(x==1 & phi<=1,0,1))
  K=max(k);
  
  ## generating random time gaps for each subject
  y=matrix(,n,K);
  for (i in 1:n){
    if(x[i]==1 & phi[i]<=1){tg <- sort(sample(1:7,k[i],replace=FALSE))}
    if(!(x[i]==1 & phi[i]<=1)){tg <- sort(sample(1:7,k[i],replace=FALSE))}
    y[i,1:k[i]]=tg}  
  t <- y
  
  ## setting the true baseline mean function
  mu=function(t){6*t}
  ##getting the number of events between time intervals
  z=matrix(,n,K);
  xparms=c();for (s in 1:nrow(x)){xparms[s]<-sum(x[s,]*parms)}
  for (i in 1:n){
    z[i,1]<-rpois(1,mu(t[i,1])*exp(xparms[i])*phi[i])
    if (k[i]>1){
      z[i,2:k[i]]<-rpois(k[i]-1,(mu(t[i,2:k[i]])-mu(t[i,1:(k[i]-1)]))*exp(xparms[i])*phi[i])
    }
  }
  
  TestD<-list(t=t, x=x, z=z, k=k, K=K)
  ID <- NULL
  options(warn=-1)
  simdata <- NULL
  
  for(m in 1:n){
    
    mydata <- data.frame(ID=m,
                         day=as.numeric(na.omit(TestD$t[m,])),
                         x1=TestD$x[m,1],
                         min=as.numeric(na.omit(TestD$z[m,])),
                         z=round(phi[m],2))
    
    mydata <- data.frame(ID=m,aggregate(min~day,data=mydata,FUN=sum),
                         x1=TestD$x[m,1],
                         z=round(phi[m],2))
    
    newdata <- mydata[ which(mydata$min!=0), ]
    
    if(nrow(newdata)==0){
      savedata <- data.frame(ID=m,
                             day=na.omit(TestD$t[m,])[length(na.omit(TestD$t[m,]))],
                             x1=TestD$x[m,1], min=0,
                             z=round(phi[m],2))
    }
    if(nrow(newdata)>0){
      savedata <-    newdata
    }
    simdata <- rbind(simdata,savedata)
  }
  
  rownames(simdata) <- c()
  
  simdata
}