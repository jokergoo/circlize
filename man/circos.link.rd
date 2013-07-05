\name{circos.link}
\alias{circos.link}
\title{
  Draw links between points or intervals


}
\description{
  Draw links between points or intervals


}
\usage{
circos.link(sector.index1, point1, sector.index2, point2,
    rou = get.track.end.position(get.current.track.index()), top.ratio = 0.5,
    col = "black", lwd = par("lwd"), lty = par("lty"), border = NA, n = 101,
    top.ratio.low = NULL)
}
\arguments{
  \item{sector.index1}{Sector index for one sector}
  \item{point1}{A single value or a numeric vector of length 2. If it is a 2-elements vector, then the link would be a belt/ribbon.}
  \item{sector.index2}{Sector index for the other sector}
  \item{point2}{A single value or a numeric vector of length 2. If it is a 2-elements vector, then the link would be a belt/ribbon.}
  \item{rou}{The position of the 'root' of the link. It is the percentage of the radius of the unit circle. By default it is the end (bottom) of the most recent track.}
  \item{top.ratio}{Set the height of the quadratic curve.}
  \item{col}{Color of the link. If the link is a belt/ribbon, then it is the filled color for the belt/ribbon.}
  \item{lwd}{Line (or border) width}
  \item{lty}{Line (or border) style}
  \item{border}{If the link is a belt, then it is the color for the belt border.}
  \item{n}{Number of points to represent a quadratic curve. Because currently I don't know how to  calculate the length of a quadratic curve, the number of segmentation of the quadratic curve cannot be calculated now. It should be an odd value because we need the point for the vertex.}
  \item{top.ratio.low}{Adjust the height of the lower border of a link (if it is like a belt/ribbon). The value should be larger than \code{top.ratio}}
}
\details{
  The link is in fact a quadratic curve.

  Drawing links does not create any track. So you can think it is independent of the tracks

  By default you only need to set \code{sector.index1}, \code{point1}, \code{sector.index2} and \code{point2}. The link would look nice. However you can also set the position and the height of links by specifying \code{rou} and \code{top.ratio}. See vignette for detailed explaination.


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