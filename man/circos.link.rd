\name{circos.link}
\alias{circos.link}
\title{
  Draw links between two points or sections


}
\description{
  Draw links between two points or sections


}
\usage{
circos.link(sector.index1, point1, sector.index2, point2,
    rou = get.track.end.position(get.current.track.index()), top.ratio = 0.5,
    col = "black", lwd = par("lwd"), lty = par("lty"), border = NA)
}
\arguments{
  \item{sector.index1}{Sector index for one sector}
  \item{point1}{A single value or a numeric vector of length 2. If it is a 2-elements vector, then the link would be a belt.}
  \item{sector.index2}{Sector index for the other sector}
  \item{point2}{A single value or a numeric vector of length 2. If it is a 2-elements vector, then the link would be a belt.}
  \item{rou}{The position of the 'root' of the link. It is the percentage of the radius of the unit circle. It would be calculated automatically.}
  \item{top.ratio}{The height of the quadratic curve}
  \item{col}{Color of the link. If the link is a belt, then it is the filled color for the belt.}
  \item{lwd}{Line width}
  \item{lty}{Line style}
  \item{border}{If the link is a belt, then it is the color for the belt border.}

}
\details{
  The link is in fact a quadratic curve.

  Drawing links does not create any track.

  By default you only need to set \code{sector.index1}, \code{point1}, \code{sector.index2} and \code{point2}. Thelink would look nice. However you can also set teh position and the height of the belts by specifying\code{rou} and \code{top.ratio}. See vignette for explaination.


}
\examples{
library(circlize)
par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = "grey",
    bg.border = NA, track.height = 0.05)

circos.link("a", 5, "c", 5)
circos.link("b", 5, "d", c(4, 6))
circos.link("a", c(2, 3), "f", c(4, 6))

circos.clear()
}