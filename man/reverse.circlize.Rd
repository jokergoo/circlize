\name{reverse.circlize}
\alias{reverse.circlize}
\title{
Convert to data coordinate system
}
\description{
Convert to data coordinate system
}
\usage{
reverse.circlize(
    x, y,
    sector.index = get.current.sector.index(),
    track.index = get.current.track.index())
}
\arguments{

  \item{x}{degree values. The value can also be a two-column matrix/data frame if you put x and y data points into one variable.}
  \item{y}{distance to the circle center (the radius)}
  \item{sector.index}{Index for the sector where the data coordinate is used}
  \item{track.index}{Index for the track where the data coordinate is used}

}
\details{
This is the reverse function of \code{\link{circlize}}. It transform data points from polar coordinate system to a specified data coordinate system.
}
\value{
A matrix with two columns (\code{x} and \code{y})
}
\examples{
pdf(NULL)
factors = letters[1:4]
circos.initialize(factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1))
reverse.circlize(c(30, 60), c(0.9, 0.8))
reverse.circlize(c(30, 60), c(0.9, 0.8), sector.index = "d", track.index = 1)
reverse.circlize(c(30, 60), c(0.9, 0.8), sector.index = "a", track.index = 1)
circos.clear()
dev.off()
}
