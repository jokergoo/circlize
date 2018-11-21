\name{plotAcc}
\alias{plotAcc}
\title{
Plots accelerometer data
}
\description{
Plots accelerometer data. This function receives summary object from function accsummary.
}

\usage{
plotAcc(object,markbouts)
}

\arguments{
  \item{object}{An object returned from either the function accsummary.}
    \item{markbouts}{Whether to mark bouts. If markbout='TRUE' a bar along the time axis will indicate whether the epoch was counted as in bout or not. Default is false. }
}
\value{
A plot is returned.
}

\author{
Jaejoon Song <jjsong2@mdanderson.org>
}

\keyword{ accelerometer }

\examples{
\dontrun{
##
## Example: Simulate a dataset for two days, for an individual with low MVPA level.
##
mvpaLowData <- simAcc(timelength=(60*24*2),paLevel='low')
summary <- accSummary(data=mvpaLowData)
summary$validDates
plotAcc(summary,markbouts='FALSE')

##
## Example: Simulate a dataset for two days, for an individual with moderate MVPA level.
##
mvpaModData <- simAcc(timelength=(60*24*2),paLevel='moderate')
summary <- accSummary(data=mvpaModData, tri='FALSE', axis=NULL,
             spuriousDef=20, nonwearDef=60, minWear=600, 
             patype='MVPA',pacut=c(1952,Inf), boutsize=10, 
             tolerance='TRUE', returnbout='TRUE')
summary$validDates
plotAcc(summary,markbouts='FALSE')

##
## Example: Simulate a dataset for two days, for an individual with high MVPA level.
##
mvpaHighData <- simAcc(timelength=(60*24*2),paLevel='high')
summary <- accSummary(data=mvpaHighData, tri='FALSE', axis=NULL,
             spuriousDef=20, nonwearDef=60, minWear=600, 
             patype='MVPA',pacut=c(1952,Inf), boutsize=10, 
             tolerance='TRUE', returnbout='TRUE')
summary$validDates
plotAcc(summary,markbouts='FALSE')


##
## Example: Simulate a tri-axial dataset for five days.
##
  library(acc)
  library(mhsmm)
  seedset=1234
  minutes=(60*24*5)
  randomTime <- seq(ISOdate(2015,1,1),ISOdate(2020,1,1),"min")
  J <- 3; initial <- rep(1/J, J)
  P <- matrix(rep(NA,9),byrow='TRUE',nrow=J)

  P1 <- matrix(c(0.95, 0.04, 0.01, 
                  0.09, 0.9, 0.01, 
                  0.1, 0.2, 0.7), byrow='TRUE',nrow = J)

  b <- list(mu = c(0, 30, 2500), sigma = c(0, 30, 1000))
  model1 <- hmmspec(init = initial, trans = P1, parms.emis = b,dens.emis = dnorm.hsmm)
  x <- simulate.hmmspec(model1, nsim = (minutes), seed = seedset, rand.emis = rnorm.hsmm)

  seedset=12345
  P2 <- matrix(c(0.95, 0.04, 0.01, 
                  0.09, 0.8, 0.11, 
                  0.1, 0.1, 0.8), byrow='TRUE',nrow = J)
  model2 <- hmmspec(init = initial, trans = P2, parms.emis = b,dens.emis = dnorm.hsmm)
  y <- simulate.hmmspec(model2, nsim = (minutes), seed = seedset, rand.emis = rnorm.hsmm)

  seedset=123456
  P3 <- matrix(c(0.95, 0.04, 0.01, 
                  0.09, 0.8, 0.11, 
                  0.1, 0.1, 0.8), byrow='TRUE',nrow = J)
  model3 <- hmmspec(init = initial, trans = P3, parms.emis = b,dens.emis = dnorm.hsmm)
  z <- simulate.hmmspec(model3, nsim = (minutes), seed = seedset, rand.emis = rnorm.hsmm)

  counts <- data.frame(TimeStamp = randomTime[1:minutes], x=x$x, y=y$x, z=z$x)
  summary <- accSummary(data=counts, tri='TRUE', axis='vm',
                        spuriousDef=20, nonwearDef=60, minWear=600, 
                        patype='MVPA',pacut=c(1952,Inf), boutsize=10, tolerance='TRUE',
                        returnbout='TRUE')
summary$validDates

plotAcc(summary,markbouts='FALSE')
}

}
