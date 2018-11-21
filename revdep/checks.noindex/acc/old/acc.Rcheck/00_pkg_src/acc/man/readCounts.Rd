\name{readCounts}
\alias{readCounts}
\title{
Reads counts data in .dat, .agd, or .csv format for Actigraph GT1M and GT3X devices
}
\description{
Reads counts data in .dat, .agd, or .csv format for Actigraph GT1M and GT3X devices. Device type and epoch is automatically detected and reported in the console.
}

\usage{
readCounts(filename)
}

\arguments{
  \item{filename}{Speficy full file path and file name. e.g. C:/mydata.dat or C:/mydata.csv}
}


\value{

For uni-axial accelerometer (GT1M), two columns are returned, consisting of: [TimeStamp,Counts]
For tri-axial accelerometer (GT3X), four columns are returned, consisting of: [TimeStamp,x,y,z]
}

\author{
Jaejoon Song <jjsong2@mdanderson.org>
}


\keyword{ accelerometer}

\examples{

##
## A example to read counts data
##
\dontrun{
accData1 <- readCounts("C:/mydata.agd")
accData1 <- readCounts("C:/mydata.dat")
accData2 <- readCounts("C:/mydata.csv")
}


}
