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
    rou = get_most_inside_radius(),
    rou1 = rou, rou2 = rou, h = NULL, w = 1, h2 = h, w2 = w,
    col = "black", lwd = par("lwd"), lty = par("lty"), border = col,
    directional = 0, arr.length = ifelse(arr.type == "big.arrow", 0.02, 0.4),
    arr.width = arr.length/2, arr.type = "triangle", arr.lty = lty,
    arr.lwd = lwd, arr.col = col)
}
\arguments{

  \item{sector.index1}{Index for the first sector}
  \item{point1}{A single value or a numeric vector of length 2. If it is a 2-elements vector, then the link would be a belt/ribbon.}
  \item{sector.index2}{Index for the other sector}
  \item{point2}{A single value or a numeric vector of length 2. If it is a 2-elements vector, then the link would be a belt/ribbon.}
  \item{rou}{The position of the 'root' of the link. It is the percentage of the radius of the unit circle. By default its value is the position of bottom margin of the most inner track.}
  \item{rou1}{The position of root 1 of the link. }
  \item{rou2}{The position of root 2 of the link.}
  \item{h}{Height of the link. }
  \item{w}{Since the link is a Bezier curve, it controls the shape of Bezier curve.}
  \item{h2}{Height of the bottom edge of the link if it is a ribbon.}
  \item{w2}{Shape of the bottom edge of the link if it is a ribbon.}
  \item{col}{Color of the link. If the link is a ribbon, then it is the filled color for the ribbon.}
  \item{lwd}{Line (or border) width}
  \item{lty}{Line (or border) style}
  \item{border}{If the link is a ribbon, then it is the color for the ribbon border.}
  \item{directional}{0 for no direction, 1 for direction from point1 to point2, -1 for direction from point2 to point1. 2 for two directional}
  \item{arr.length}{Length of the arrows, measured in 'cm', pass to \code{\link[shape]{Arrowhead}}. If \code{arr.type} is set to \code{big.arrow}, the value is percent to the radius of the unit circle.}
  \item{arr.width}{Width of the arrows, pass to \code{\link[shape]{Arrowhead}}.}
  \item{arr.type}{Type of the arrows, pass to \code{\link[shape]{Arrowhead}}. Default value is \code{triangle}. There is an additional option that is not passed to \code{\link[shape]{Arrowhead}} (\code{big.arrow}).}
  \item{arr.col}{Color of the arrows, pass to \code{\link[shape]{Arrowhead}}.}
  \item{arr.lwd}{Line width of arrows, pass to \code{\link[shape]{Arrowhead}}.}
  \item{arr.lty}{Line type of arrows, pass to \code{\link[shape]{Arrowhead}}.}

}
\details{
Links are implemented as quadratic Bezier curves.

Drawing links does not create any track. So you can think it is independent of the tracks.

By default you only need to set \code{sector.index1}, \code{point1}, \code{sector.index2} and \code{point2}. The
links would look nice.

See vignette for detailed explanation.
}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.

}
\examples{
\dontrun{

par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = "grey",
    bg.border = NA, track.height = 0.05)
circos.info(plot = TRUE)
#circos.link("a", 5, "c", 5, rou1 = 0.4, rou2 = 0.6, col = "black")
circos.link("a", 5, "g", 5, col = "black", h = 0.5, w = -0.25)
circos.link("c", 10, "d", c(1, 4), col = "#00000040", border = "black")
circos.link("a", c(2, 8), "g", c(4, 4.5), rou1 = 0.9, rou2 = 0.8, 
    col = "#00000040", border = "black")
circos.link("b", c(1, 10), "a", c(1, 10), rou1 = 0.9, rou2 = 0.4,  
    col = "#00000040", border = "black")
circos.clear()

par(mar = c(1, 1, 1, 1))
factors = letters[1:8]
circos.par("canvas.xlim" = c(-2, 2), "canvas.ylim" = c(-2, 2))
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.col = "grey", 
    bg.border = NA, track.height = 0.05)
circos.info(plot = TRUE)
circos.link("a", 5, "b", 5, col = "black", w = 1)
circos.link("b", 5, "c", 5, col = "black", w = 2)
circos.link("c", 5, "d", 5, col = "black", w = 0.25)
circos.link("d", 5, "e", 5, col = "black", w = -0.25)
circos.clear()

}

}
