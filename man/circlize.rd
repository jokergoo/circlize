\name{circlize}
\alias{circlize}
\title{
  Return the coordinate in polar coordinate system  


}
\description{
  Return the coordinate in polar coordinate system  


}
\usage{
circlize(x, y, sector.index = get.current.sector.index(),
    track.index = get.current.track.index())
}
\arguments{
  \item{x}{Data points on x-axis}
  \item{y}{Data points on y-axis}
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}

}
\details{
  This is the core function in the package. It transform data points from data coordinate system to polar coordinate system.  


}
\value{
  A matrix with two columns (\code{theta} and \code{rou}) 


}
