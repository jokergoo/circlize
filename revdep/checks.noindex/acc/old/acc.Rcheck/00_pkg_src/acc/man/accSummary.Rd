\name{accSummary}
\alias{accSummary}
\title{
Summarizes accelerometer data for a single type of physical activity
}
\description{
Summarizes accelerometer data for a single type of physical activity. Functionality is same as function acc, except that this function provides a summary for a single type of physical activity, with more detailed information.
}

\usage{
accSummary(data, tri, axis, spuriousDef, nonwearDef, minWear,
    patype, pacut, epoch, boutsize, tolerance, returnbout)
}



\arguments{
  \item{data}{Data which consists of two columns [TimeStamp,counts] (i.e. raw accelerometer file read in by function readRaw)}
  \item{tri}{Whether the data is from a tri-axial accelerometer. Default is tri=`FALSE'. If tri=`TRUE' then option `axis' should be specified.}
  \item{axis}{This option is only used for the tri-axial accelerometer. Options are `x',`y',`z',`sum', or `vm'. Options `x', `y', or `z' can be spefied to summarize data using only data from a single axis. If the option 'vm' is used, the square root of the squared sum of counts from three axes (i.e. \eqn{\sqrt{{x}^{2}+{y}^{2}+{z}^{2}}} are used for the summary. If the option 'sum' is used, sum of the counts from three axes are used.}
  \item{spuriousDef}{Definition of spurious observation. Defined as minutes of consecutive zeros. For example, if spuriousDef = 20, this means that an observation point will be determined as a spurious observation if there are consequtive counts of at least 20 zeros before and after the single non-zero observation. Default is  spuriousDef = 20.}
  \item{nonwearDef}{Definition of non-wear time. Defined as minutes of consecutive zeros. For example, if nonwearDef=60, this means that a period will be defined as non-wear time if there are at least 60 consecutive zeros. Default is nonwearDef=60. To consider all observations as wear time specify nonwearDef=`Inf'}
  \item{minWear}{Minimum wear time definition. Defined as minutes of wear time. or example, if minWear = 600, this means that a day will be considered valid only if the wear time is at least 600 minutes. Default is minWear = 600. To return summary for all dates in the data, set minWear = 0.}       
  \item{patype}{Type of physical activity for summary. For example, to summarize sedentary activity, use option patype=c(`Sedentary'). To summarize moderate-vigorous physical activities, user specifies patype=c(`MVPA'). This labels the summary accordingly.}
  \item{pacut}{Cut points to be used for the physical activity type. For example, if the user specified patype=c(`Sedentary'), pacut can be specified as pacut=c(c(0,99)). The options requires to have a lower and a upper limit for each activity type (i.e. c(0,99) for sedentary activity). The specified interval includes its lower and upper endpoints (it is a closed inerval).}
  \item{boutsize}{Boutsize to summarize a physical activity. If multiple patype is specified, boutsize should be for each one (e.g., if patype=c(`Sedentary') then one can use  boutsize=c(10)).}
  \item{epoch}{Epoch size. Default is '1 min'. Other epoch size can be specified using this option (e.g., '1 sec')}
  \item{tolerance}{Whether two observations outside the physical activity cut point should be permitted in summarizing a physical activity (e.g. if patype=c(`Sedentary') then one can use tolerance=c(`FALSE')).}
  \item{returnbout}{Whether to return data with bout indicators. If returnbout='FALSE' then only a summary of daily physical activity is returned.}
}

\value{
If returnbout='FALSE', then a summary for each specified physical activity types (number of bouts and minutes of the activity), for valid dates. Defaults to 'TRUE'.

If returnbout='TRUE', then a list of summary object is returned with:
  \item{totalDates}{Number of unique days available in data.}
  \item{validDates}{A summary for each specified physical activity types (number of bouts and minutes of the activity), for valid dates (as defined by minWear).}
  \item{PA}{Data supplied by the user is returned with three additional columns [inPA, nonwear, inboutPA], indicating whether the observation was considered to be in the defined cutpoint of the physical activity (inPA), in the defined cutpoint of nonwear time (nonwear), or in bout (inboutPA), respectively.}
}

\author{
Jaejoon Song <jjsong2@mdanderson.org>
}
\references{Choi, L., Liu, Z., Matthews, C.E. and Buchowski, M.S.  (2011). Validation of Accelerometer Wear and Nonwear Time Classification Algorithm. Med Sci Sports Exerc, 43(2):357-64.

Hall, K. S., Howe, C. A., Rana, S. R., Martin, C. L., and Morey, M. C. (2013). METs and Accelerometry of Walking in Older Adults: Standard versus Measured Energy Cost. Medicine and Science in Sports and Medicine, 45(3). 574-82.

Freedson, P., Melanson, E.,  and Sirard, J. (1998). Calibration of the Computer Sciences and Applications, Inc. accelerometer. Medicine and Science in Sports and Exercercise, 30(5):777-81.

Swartz, A. M., Strath, S. J., Bassett, D. R. Jr., O'Brien, W. L., King, G. A., and Ainsworth, B. E. (2000). Estimation of energy expenditure using CSA accelerometers at hip and wrist sites. Medicine and Science in Sports and Exercercise, 32: S450-456. 

Copeland, J. L., and Esliger, D. W. (2009). Accelerometer assessment of physical activity in active, healthy older adults. J Aging Phys Act, 17: 17-30. }

\keyword{ accelerometer}

\examples{
##
## Example 1: Loading the activity counts data using readCounts function
##      
\dontrun{
library(acc)
infile <- "DataName.dat"
counts <- readCounts(infile)


##
## Example 2: Summarizing accelerometer data for a sedentary individual"  
##        

# For this example, data is generated using a Hidden Markov model
# First, a sequence of time is generated
randomTime <- seq(ISOdate(2015,4,1),ISOdate(2015,4,3),"min")
# Load the mhsmm package to generate data using a Hidden Makov model
library(mhsmm)
# It is assumed that the counts are generated from a Hidden Markov model 
# with three states, being non-wear, sedentary, and moderate-vigorous activity
J <- 3; initial <- rep(1/J, J)
# Set up a transition matrix for the Hidden Markov model.
P <- matrix(c(0.95, 0.04, 0.01, 
             0.09, 0.9, 0.01, 
             0.1, 0.2, 0.7), byrow='TRUE',nrow = J)
# It is assumed that the counts are realized from a mixture of
# two normal distributions (for sedentary activity and mvpa) 
# and a constant at zero (for non-wear time).
b <- list(mu = c(0, 30, 2500), sigma = c(0, 30, 1000))
model <- hmmspec(init = initial, trans = P, parms.emission = b,dens.emission = dnorm.hsmm)
# Generate data!
train <- simulate.hmmspec(model, nsim = (60*24*2), seed = 1234, rand.emis = rnorm.hsmm)
# Now set up a dataset that mimicks the accelerometry data
counts <- data.frame(TimeStamp = randomTime[1:length(train$x)], counts = train$x)
library(acc)
# summarize the data using the acc function.
# Sedentary and moderate-vigorous activity is summarized, using Freedson's cut points by default.
# Option returnbout='TRUE' returns a more detailed information on how the summary was calculated.
summary1 <- accSummary(data=counts, tri='FALSE', axis=NULL,
                     spuriousDef=20, nonwearDef=60, minWear=600, 
                     patype='MVPA',pacut=c(1952,Inf), 
                     boutsize=10, tolerance='TRUE',returnbout='TRUE')
summary1$validDates # This returns the same summary as when returnbout='FALSE'
# summary1$PA # This returns the activity classification and bout information

##
## Example 3: Summarizing accelerometer data for an active individual.
##

randomTime <- seq(ISOdate(2015,4,1),ISOdate(2015,4,3),"min")
library(mhsmm)
J <- 3; initial <- rep(1/J, J)
P <- matrix(c(0.95, 0.04, 0.01, 
             0.09, 0.7, 0.21, 
             0.1, 0.1, 0.8), byrow='TRUE',nrow = J)
b <- list(mu = c(0, 30, 2500), sigma = c(0, 30, 1000))
model <- hmmspec(init = initial, trans = P, parms.emission = b,dens.emission = dnorm.hsmm)
train <- simulate.hmmspec(model, nsim = (60*24*2), seed = 1234, rand.emission = rnorm.hsmm)

counts <- data.frame(TimeStamp = randomTime[1:length(train$x)], counts = train$x)
library(acc)
# Option returnbout='TRUE' returns a more detailed information on how the summary was calculated.
summary2 <- accSummary(data=counts, tri='FALSE', axis=NULL,
                     spuriousDef=20, nonwearDef=60, minWear=600, 
                     patype='MVPA',pacut=c(1952,Inf), 
                     boutsize=10, tolerance='TRUE',returnbout='TRUE')
summary2$validDates # This returns the same summary as when returnbout='FALSE'
# summary2$PA # This returns the activity classification and bout information

}
}