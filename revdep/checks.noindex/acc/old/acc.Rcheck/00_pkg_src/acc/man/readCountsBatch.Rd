\name{readCountsBatch}
\alias{readCountsBatch}
\title{
Reads multiple accelerometer counts data in a folder
}
\description{
Reads multiple accelerometer counts data in a folder. This is a batch mode of the readCounts function.
}

\usage{
readCountsBatch(path,filetype)
}

\arguments{
  \item{path}{Path to a folder which contains accelerometer counts data in .dat or .csv format.}
  \item{filetype}{Specify whether the data to read is in dat or csv format. Options are either 'dat' or 'csv'. For example if filetype = 'csv' is specified, all csv data will be read and all other types in the same folder will be ignored. By defalt, it is assumed that all files in the specified path are either csv or dat files and are intended to be read.}
}


\value{
A folder `readfiles' is created inside the specified 'path'. In the folder, files are saved by the same filenames as in the raw data.

For uni-axial accelerometer (GT1M), two columns are returned, consisting of: [TimeStamp,Counts]
For tri-axial accelerometer (GT3X), four columns are returned, consisting of: [TimeStamp,x,y,z]
}

\author{
Jaejoon Song <jjsong2@mdanderson.org>
}


\keyword{ accelerometer}

\examples{
##
## Example
##
# filepath to locate the activity counts data files
\dontrun{
mypath <- "C:/Accelerometry files"
readCountsBatch(mypath,filetype='csv')
}
}
