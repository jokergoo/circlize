\name{circlize}
\alias{circlize}
\title{
  Return the coordinate in polar coordinate system in a specified cell


}
\description{
  Return the coordinate in polar coordinate system in a specified cell


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
  Return the coordinate in polar coordinate system in a specified cell

}
\value{
  A matrix with two columns (\code{theta} and \code{rou})
}