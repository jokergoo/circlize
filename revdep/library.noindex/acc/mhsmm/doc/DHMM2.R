### R code from vignette source 'DHMM2.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: DHMM2.Rnw:28-29
###################################################
options("width"=80)


###################################################
### code chunk number 2: DHMM2.Rnw:51-55
###################################################
load("clsX.RData")
length(cls0)
length(cls1)
library(mhsmm)


###################################################
### code chunk number 3: clsfig1
###################################################
plot(cls1[1:2000], type='l', ylim=c(.8,2))
addStates(cls0[1:2000])


###################################################
### code chunk number 4: DHMM2.Rnw:75-76
###################################################
dpmf <- function(x,j,model) model$parms.emission$pmf[j,x]


###################################################
### code chunk number 5: DHMM2.Rnw:83-90
###################################################
J <- 2
init <- c(.5,.5)
P <- matrix(c(.9,.1,.1,.9),nrow=J)
B <- list(pmf=matrix(.1,ncol=J,nrow=J))
diag(B$pmf) <- .9
init.spec <- hmmspec(init,trans=P,parms.emission=B,dens.emission=dpmf)
init.spec


###################################################
### code chunk number 6: DHMM2.Rnw:97-104
###################################################
mstep.pmf <- function(x,wt) {
  ans <- matrix(ncol=ncol(wt),nrow=ncol(wt))
  for(i in 1:ncol(wt))
    for(j in 1:ncol(wt))
      ans[i,j] <- sum(wt[which(x==j),i])/sum(wt[,i])
  list(pmf=ans)
}


###################################################
### code chunk number 7: DHMM2.Rnw:112-115
###################################################
samp <- 1:2640
train <- list(s=cls0[samp], x=cls1[samp], N=length(cls0[samp]))
valid <- list(x=cls1[-samp], N=length(cls1[-samp]))


###################################################
### code chunk number 8: DHMM2.Rnw:120-122
###################################################
hmm.obj <- hmmfit(train, init.spec,mstep=mstep.pmf)
summary(hmm.obj)


###################################################
### code chunk number 9: DHMM2.Rnw:131-132
###################################################
vit <- predict(hmm.obj, valid)


###################################################
### code chunk number 10: DHMM2.Rnw:137-138
###################################################
smo <- predict(hmm.obj, valid, method="smoothed")


###################################################
### code chunk number 11: DHMM2.Rnw:143-151
###################################################
normtab <- function(tt) round(sweep(tt,1,rowSums(tt),"/"),2)
SS <- cls0[-samp]
XX <- cls1[-samp]
cls2.vit <- vit$s 
cls2.smo <- smo$s 
normtab(table(SS,XX))
normtab(table(SS,cls2.vit))
normtab(table(SS,cls2.smo))


###################################################
### code chunk number 12: clsfig2
###################################################
show <- 1:2000
cls0b <- cls0[-samp]
cls1b <- cls1[-samp]
c(length(SS),length(cls2.vit),length(cls2.smo), length(cls0b), length(cls1b))
plot(cls1b[show], type='l', ylim=c(.8,2))
addStates(list(cls0b[show],cls2.vit[show], cls2.smo[show]))


