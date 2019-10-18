\name{circos.link}
\alias{circos.link}
\title{
Draw links between points or/and intervals
}
\description{
Draw links between points or/and intervals
}
\usage{
circos.link(
    sector.index1,
    point1,
    sector.index2,
    point2,
    rou = get_most_inside_radius(),
    rou1 = rou,
    rou2 = rou,
    h = NULL,
    h.ratio = 0.5,
    w = 1,
    h2 = h,
    w2 = w,
    col = "black",
    lwd = par("lwd"),
    lty = par("lty"),
    border = col,
    directional = 0,
    arr.length = ifelse(arr.type == "big.arrow", 0.02, 0.4),
    arr.width = arr.length/2,
    arr.type = "triangle",
    arr.lty = lty,
    arr.lwd = lwd,
    arr.col = col)
}
\arguments{

  \item{sector.index1}{Index for the first sector where one link end locates}
  \item{point1}{A single value or a numeric vector of length 2. If it is a 2-elements vector, then the link would be a belt/ribbon.}
  \item{sector.index2}{Index for the other sector where the other link end locates}
  \item{point2}{A single value or a numeric vector of length 2. If it is a 2-elements vector, then the link would be a belt/ribbon.}
  \item{rou}{The position of the the link ends (if \code{rou1} and \code{rou2} are not set). It is the percentage of the radius of the unit circle. By default its value is the position of bottom margin of the most inner track.}
  \item{rou1}{The position of end 1 of the link. }
  \item{rou2}{The position of end 2 of the link.}
  \item{h}{Height of the link, measured as percent to the radius to the unit circle. By default it is automatically infered.}
  \item{h.ratio}{systematically change the link height. The value is between 0 and 1.}
  \item{w}{Since the link is a Bezier curve, it controls the shape of Bezier curve.}
  \item{h2}{Height of the bottom edge of the link if it is a ribbon.}
  \item{w2}{Shape of the bottom edge of the link if it is a ribbon.}
  \item{col}{Color of the link. If the link is a ribbon, then it is the filled color for the ribbon.}
  \item{lwd}{Line (or border) width}
  \item{lty}{Line (or border) style}
  \item{border}{If the link is a ribbon, then it is the color for the ribbon border.}
  \item{directional}{0 for no direction, 1 for direction from \code{point1} to \code{point2}, -1 for direction from \code{point2} to \code{point1}. 2 for two directional. The direction is important when arrow heads are added.}
  \item{arr.width}{Width of the arrows, pass to \code{\link[shape]{Arrowhead}}.}
  \item{arr.type}{Type of the arrows, pass to \code{\link[shape]{Arrowhead}}. Default value is \code{triangle}. There is an additional option \code{big.arrow}.}
  \item{arr.length}{Length of the arrows, measured in 'cm', pass to \code{\link[shape]{Arrowhead}}. If \code{arr.type} is set to \code{big.arrow}, the value is percent to the radius of the unit circle.}
  \item{arr.col}{Color of the arrows, pass to \code{\link[shape]{Arrowhead}}.}
  \item{arr.lwd}{Line width of arrows, pass to \code{\link[shape]{Arrowhead}}.}
  \item{arr.lty}{Line type of arrows, pass to \code{\link[shape]{Arrowhead}}.}

}
\details{
Links are implemented as quadratic Bezier curves (\url{https://en.wikipedia.org/wiki/B\%C3\%A9zier_curve#Rational_B.C3.A9zier_curves} ).

Drawing links does not create any track. So you can think it is independent of the tracks.

By default you only need to set \code{sector.index1}, \code{point1}, \code{sector.index2} and \code{point2}. The
links would look nice.

Please refer to the vignette for detailed explanation.
}
\seealso{
\url{http://jokergoo.github.io/circlize_book/book/graphics.html#links}
}
\examples{
# There is no example
NULL

}
