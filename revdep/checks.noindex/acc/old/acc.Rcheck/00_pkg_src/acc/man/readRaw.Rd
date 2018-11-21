\name{readRaw}
\alias{readRaw}
\title{
Reads raw data in .csv format for GT3X devices
}
\description{
Reads raw data in .csv format for Actigraph GT3X devices. Device type and epoch is automatically detected and reported in the console. Marks activity intensity based on two existing methods.
}

\usage{
readRaw(filepath,type,resting)
}


\arguments{
  \item{filepath}{Speficy full file path and file name. (e.g. 'C:/mydata.csv')}
  \item{type}{Specify how activity intensity is identified. Options: i) 'mad' for Vaha-Ypya method, ii) 'ai' for Bai et al.'s method, or iii) 'resting' to calculate resting state intensity level required for Bai's method}
  \item{resting}{Resting state intensity level, required for Bai et al.'s method.}
}


\value{
If the option 'type' is 'mad' or 'ai', five columns are returned, consisting of first four columns [TimeStamp,x,y,z], and the intensity category/level. If the option 'type' is 'resting' then a vector of resting state intensity level is returned.
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
# For Vaha-Ypya et al.'s method
accData1 <- readRaw("C:/mydata.csv",type='mad')
# For Bai et al.'s method with known resting state intensities
accData2 <- readRaw("C:/mydata.csv",type='ai',resting=c(.15,.16,.17))
# Calculating resting state intensity for Bai et al.'s method
restingIntensity <- readRaw("C:/mydata.csv",type='resting')
accData3 <- readRaw("C:/mydata.csv",type='ai',resting=restingIntensity)
}


}
