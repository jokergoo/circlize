\name{aee}
\alias{aee}

\title{
Creates an aee object
}
\description{
Creates an aee object for semiparametric regression with augmented estimating equation. 
}
\usage{
aee(ID, time, minutes)
}

\arguments{
  \item{ID}{Individual identifier}
  \item{time}{Observation time}
  \item{minutes}{Minutes of physical activity}
}

\value{
\item{psDF}{A data frame, part of original input data frame with variable "ID", "time" and "count"}
\item{timeGrid}{Ordered distinct observation times in the set of all observation times}
\item{panelMatrix}{a matrix representation of panel count data, one row per subject, one column per time point in "timeGrid"}
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
# We illustrate the use of function aeexfit
# with the sample data from the National Health and Nutrition Examination Survey (NHANES) 
# to examine the association between the cardiorespiratory function (i.e., VO2max) 
# and daily minutes of moderate to vigorous physical activity (MVPA).

data(NHANES)
formula <- aee(ID, Day, mvpaMinutes) ~ VO2max
# Standard errors are obtained using sandwich estimation
fitted <- aeexfit(formula = formula, data = NHANES, se = "Sandwich")
summary(fitted)
}
}
\keyword{ aee }
