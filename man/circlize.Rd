\name{circlize}
\alias{circlize}
\title{
Convert to polar coordinate system
}
\description{
Convert to polar coordinate system
}
\usage{
circlize(
    x, y,
    sector.index = get.current.sector.index(),
    track.index = get.current.track.index())
}
\arguments{

  \item{x}{Data points on x-axis. The value can also be a two-column matrix/data frame if you put x and y data points into one variable.}
  \item{y}{Data points on y-axis.}
  \item{sector.index}{Index for the sector to convert the coordinates}
  \item{track.index}{Index for the track to convert the coordinates}

}
\details{
This is the core function in the package. It transform data points from data coordinate system (in a specific cell) to the polar coordinate system.
}
\value{
A matrix with two columns (\code{theta} and \code{rou}). \code{rou} is measured in degree.
}
\examples{
pdf(NULL)
factors = c("a", "b")
circos.initialize(factors, xlim = c(0, 1))
circos.track(ylim = c(0, 1))
# x = 0.5, y = 0.5 in sector a and track 1
circlize(0.5, 0.5, sector.index = "a", track.index = 1)
circos.clear()
dev.off()
}
