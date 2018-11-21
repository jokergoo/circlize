\name{racePlot}
\alias{racePlot}
\title{
Race plots of minutes of activity per day
}
\description{
Race plots of minutes of activity per day, for several activity types
}

\usage{
racePlot(summary,title,cex.title,cex.text,cex.center,color)
}

\arguments{
  \item{summary}{An object returned from function acc.}
  \item{title}{Title of the plot}
  \item{cex.title}{Font size of the title}
  \item{cex.text}{Font size of the race track labels}
  \item{cex.center}{Font size of the day indicator in the center of the plot}
  \item{color}{Color of the race tracks}
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
library(acc)
##
## Example: Simulate a dataset for seven days, for an individual with low MVPA level.
##
mvpaModData <- simAcc(timelength=(60*24*7),paLevel='moderate')

summary1 <- acc(data=mvpaModData, tri='FALSE', axis='NULL',
spuriousDef=20, nonwearDef=60, minWear=600,epoch=c('1 min','1 min'),
patype=c('Sedentary','MVPA'),pacut=c(c(0,99),c(1952,Inf)),
boutsize=c(10,10), tolerance=c('FALSE','TRUE'))
summary1

racePlot(summary1,title="Summary of Physical Activity Per Day",
cex.title=1,cex.text=1.2)
}
}
