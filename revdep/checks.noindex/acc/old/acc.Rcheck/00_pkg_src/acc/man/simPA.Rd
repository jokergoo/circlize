\name{simPA}
\alias{simPA}
\title{
Simulates minutes of physical activity per day
}
\description{
Simulates minutes of physical activity per day with realistic missing data patterns
}

\usage{
simPA(n, type, beta, minday, maxday)
}

\arguments{
  \item{n}{Number of individuals in the simulated data.}
  \item{type}{Whether to simulate data from informative/non-informative observation/censoring patterns. Options: i) 'inf', ii) 'noninf'.}
  \item{beta}{True coefficient for the binary covariate.}
  \item{minday}{Minimun number of observation days.}
  \item{maxday}{Maximum number of observation days.}
}


\value{
A simulated dataset is returned with four columns:
[ID, day, min, x1, z]. 
}

\author{
Jaejoon Song <jjsong2@mdanderson.org>
}


\keyword{accelerometer}

\examples{
##
## Simulauting data for a single individual 
## with noninformative observation patterns
##
simdata <- simPA(n=1, beta=1.5,type='noninf', minday=6, maxday=7)

}
