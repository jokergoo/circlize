\name{reverse.circlize}
\alias{reverse.circlize}
\title{
  Return the coordinate in data coordinate system  


}
\description{
  Return the coordinate in data coordinate system  


}
\usage{
reverse.circlize(theta, rou, sector.index = get.current.sector.index(),
    track.index = get.current.track.index())
}
\arguments{
  \item{theta}{measured by degree}
  \item{rou}{distance to the circle center (radius)}
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}

}
\details{
  This is the reverse function of \code{\link{circlize}}. It transform data points from polar coordinate system to data coordinate system.  


}
\value{
  A matrix with two columns (\code{x} and \code{y}) 


}
