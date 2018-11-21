\name{simAcc}
\alias{simAcc}
\title{
Simulates accelerometer data based on a hidden Markov model
}
\description{
Simulates accelerometer data. The simulation function is based on a hidden Markov model, as described in the example for function acc. This function is provided for convenience to generate data from a pre-specified transition probabilities to mimic activity levels of low, moderate and high. To generate data from a specific transition probabilities and distributions, please refer to the example for function acc.
}

\usage{
simAcc(timelength,paLevel,epoch,startDate,endDate,
       mu,sigma,seedset,tpm)
}

\arguments{
  \item{timelength}{Number of observations to be generated.}
  \item{paLevel}{Pre-specified levels of physical activity for convenience. User can specify all parameter as preferred, by stating the option as paLevel=NULL. Default is `moderate'. Options: 1) `low', `moderate', or `high'. Low specifies a hidden markov model with transition probabilities 0.95, 0.04, 0.01, 0.09, 0.9, 0.01, 0.1, 0.2, 0.7, respectively for P11, P12, P13, P21, P22, P23, P31, P32, P33, respectively. Moderate specifies a hidden markov model with transition probabilities 0.95, 0.04, 0.01, 0.09, 0.8, 0.11, 0.1, 0.1, 0.8 respectively for P11, P12, P13, P21, P22, P23, P31, P32, P33, respectively. High specifies a hidden markov model with transition probabilities 0.95, 0.04, 0.01, 0.09, 0.7, 0.21, 0.1, 0.1, 0.8, respectively for P11, P12, P13, P21, P22, P23, P31, P32, P33, respectively. For all levels, it is assumed that the activity intensities are realized from a mixture of two normal distributions (for sedentary activity and mvpa)  and a constant at zero (for non-wear time), with means mu = c(0, 30, 2500) and variance  sigma = c(0, 30, 1000).}
  \item{epoch}{Epoch size. User can specify desired epoch size in units of time larger than seconds. Defaults to 1 minute epoch.}
  \item{startDate}{Start date in ISOdate format. For example ISOdate(2017,1,1,hour=0,min=0,sec=0,tz="GMT").}
  \item{endDate}{End date in ISOdate format. For example ISOdate(2017,1,1,hour=0,min=0,sec=0,tz="GMT").}
  \item{mu}{Mean levels for each activity type.}
  \item{sigma}{Standard deviations for each activity type.}
  \item{seedset}{Sets seed for random data generation. Defaults to 1234.}
  \item{tpm}{Transition probability matrix that specify probability of change from one activity state to another.}
}


\value{
A simulated dataset is returned with two columns:
[TimeStamp, counts] 
}

\author{
Jaejoon Song <jjsong2@mdanderson.org>
}


\keyword{accelerometer}

\examples{
\dontrun{
##
## Example: Simulate a dataset for two days, for an individual with low activity level.
##
mvpaLowData <- simAcc(timelength=(60*24*2),paLevel='low')
summary <- acc(data=mvpaLowData, tri='FALSE', axis=NULL,
                     spuriousDef=20, nonwearDef=60, minWear=600, 
                     patype=c('Sedentary','MVPA'),pacut=c(c(0,99),c(1952,Inf)), 
                     boutsize=c(10,10), tolerance=c('FALSE','TRUE'))
summary

##
## Example: Simulate a dataset for two days, for an individual with moderate activity level.
##
mvpaLowData <- simAcc(timelength=(60*24*2),paLevel='moderate')
summary <- acc(data=mvpaLowData, tri='FALSE', axis=NULL,
                     spuriousDef=20, nonwearDef=60, minWear=600, 
                     patype=c('Sedentary','MVPA'),pacut=c(c(0,99),c(1952,Inf)), 
                     boutsize=c(10,10), tolerance=c('FALSE','TRUE'))
summary

##
## Example: Simulate a dataset for two days, for an individual with high activity level.
##
mvpaLowData <- simAcc(timelength=(60*24*2),paLevel='high')
summary <- acc(data=mvpaLowData, tri='FALSE', axis=NULL,
                     spuriousDef=20, nonwearDef=60, minWear=600, 
                     patype=c('Sedentary','MVPA'),pacut=c(c(0,99),c(1952,Inf)), 
                     boutsize=c(10,10), tolerance=c('FALSE','TRUE'))
summary

}
}