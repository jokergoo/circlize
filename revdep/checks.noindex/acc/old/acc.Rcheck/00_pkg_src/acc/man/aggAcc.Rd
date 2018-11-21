\name{aggAcc}
\alias{aggAcc}
\title{
Aggregates multiple accelerometer summary files
}
\description{
Aggregates multiple accelerometer summary files (e.g. summary files for multiple individuals) by day. This function can be used in sequence with readRawBatch and accBatch.
}

\usage{
aggAcc(path)
}

\arguments{
  \item{path}{Path to accelerometer summary files created by function acc or accBatch}
}


\value{
A folder `aggregate' is created within the specified path. In the folder, aggregate.Rdata file (object `aggregate') consists of columns
[ID, Date, SedentaryMinutes, wearTime, numberOfBoutsSed, mvpaMinutes, numberOfBoutsMVPA], where ID is the filename.
}

\author{
Jaejoon Song <jjsong2@mdanderson.org>
}

\keyword{ accelerometer}

\examples{
##
## Example
##
\dontrun{
mypath <- "C:/Accelerometry files/readfiles/summaryfiles"
aggAcc(mypath)
}

}
