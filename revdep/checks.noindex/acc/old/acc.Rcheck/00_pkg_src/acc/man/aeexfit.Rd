\name{aeexfit}
\alias{aeexfit}

\title{
Fits semiparametric regression models robust to informative observation times and censoring
}
\description{
Fits semiparametric regression models robust to informative observation times and censoring
}
\usage{
aeexfit(formula, data, weight, se, control=list(), boot)
}



\arguments{
  \item{formula}{A formula object as returned by aee.}
  \item{data}{A data frame which includes individuals' ID, observation times, and minutes of physical activity since the last observation time.}
  \item{weight}{A vector of sampling weights, for each individual.By default, no sampling weights are applied.}
\item{se}{The method of estimating standard errors can be chosen by the argument se. Two options are available: i) the sandwhich estimation (se = 'Sandwich'), or ii) the bootstrap procedure (se = 'Bootstrap').}
  \item{control}{A list of control parameters. See 'Details'.}
  \item{boot}{The number of resamples generated for the bootstrap procedure.}
}

\details{
The control argument is a list athat can supply any of the following components:
\itemize{
\item betaInit: Initial value for covariate coefficient, default is 0.

\item interval: Initial search interval for solving beta. Default is (-5,5).

\item maxIter: Maximum iterations allowed. Default is 150.

\item absTol: Absolute tolerance. Default is 1e-6.

\item relTol: Relative tolerance. Default is 1e-6.

\item a: A tune parameter. Default is .1. In case of gamma fraility, "a" corresponds to the value of both shape and rate parameters.
}

}

\references{
Wang, X. and Yan, J. (2011). Fitting semiparametric regressions for panel count survival data with an R package spef. Computer Methods and Programs in Biomedicine, 104, 278-285.

Wang, X., Ma, S., and Yan, J. (2013). Augmented estimating equations for semiparametric panel count regression with informative observation times and censoring time. Statistica Sinica, 23, 359-381.
}
\author{
Jaejoon Song <jjsong2@mdanderson.org>
}


\examples{
\dontrun{

data(NHANES)
formula <- aee(ID, Day, mvpaMinutes) ~ VO2max
# Standard errors are obtained using sandwich estimation
fitted <- aeexfit(formula = formula, data = NHANES, se = "Sandwich")
summary(fitted)
}
}
\keyword{ aeefit }
