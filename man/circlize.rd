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
  \item{x}{Data points on x-axis}
  \item{y}{Data points on y-axis}
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}

}
\details{
  This is the core function in the package. It transform data points from data coordinate system to polar coordinate system.  


}
\value{
  A matrix with two columns (\code{theta} and \code{rou}) 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
\examples{
\dontrun{
library(circlize)
factors = letters[1:4]
circos.initialize(factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1))
circlize(0.5, 0.5)
circlize(0.5, 0.5, sector.index = "d", track.index = 1)
circos.clear()
}
}
