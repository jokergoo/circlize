\name{circos.par}
\alias{circos.par}
\title{
  Parameters for circos layout


}
\description{
  Parameters for circos layout


}
\usage{
circos.par(...)
}
\arguments{
  \item{...}{Arguments for the parameters, see "details" section}

}
\details{
  Global parameters for the circos layout. Currently supported parameters are:

\describe{
  \item{start.degree}{The starting degree from which the circle begin to draw. Note this degree is measured in the standard polar coordinate which means it is reverse-clockwise.}
  \item{gap.degree}{Gap between two neighbour sectors.}
  \item{track.margin}{Like \code{margin} in Cascading Style Sheets (CSS), it is the blank area out of the plotting region, also outside of the borders. Since left and right margin are controlled by \code{gap.degree}, only bottom and top margin need to be set. And all cells in a same track share the same margins, and that's why this parameter is called \code{track.margin}. The value for the \code{track.margin} is the percentage according to the radius of the unit circle.}
  \item{unit.circle.segments}{Since curves are simulated by a series of straight lines, this parameter controls the amout of segments to represent a curve. The minimal length of the line segmentation is the length of the unit circle (\code{2pi}) / \code{unit.circoe.segments}.}
  \item{cell.padding}{Padding of the cell. Like \code{padding} in Cascading Style Sheets (CSS), it is the blank area around the plotting regions, but within the borders. The paramter has four values, which controls the bottom, left, top and right padding respectively. The four values are all percentages in which the first and the third padding values are the percentages according to the range of values on y-axis and the second and fourth values are the percentages according to the range of values on x-axis.}
  \item{default.track.height}{The default height of tracks. It is the percentage according to the radius of the unit circle. The height includes the top and bottom cell paddings but not the margins.}
  \item{points.overflow.warning}{Since each cell is in fact not a real plotting region but only an ordinary rectangle, it does not eliminate points that are plotted out of the region. So if some points are out of the plotting region, by default, the  package would continue drawing the points and print warnings. But in some  circumstances, draw something out of the plotting region is useful, such as draw some legend or text. Set this value to \code{FALSE} to turn off the warnings.}
  \item{canvas.xlim}{The coordinate for the canvas. Because the package draw everything (or almost everything) inside the unit circle, so the default \code{canvas.xlim} and \code{canvas.ylim} for the canvas would be all \code{c(-1, 1)}. However, you can set it to a more broad interval if you want to draw other things out of the circle. By choosing proper \code{canvas.xlim} and \code{canvas.ylim}, you can draw part of the circle. E.g. setting \code{canvas.xlim} to \code{c(0, 1)} and \code{canvas.ylim} to \code{c(0, 1)} would only draw circle in the region of (0, pi/2).}
  \item{canvas.ylim}{The coordinate for the canvas. By default it is \code{c(-1, 1)}}
  \item{clock.wise}{The direction of drawing sectors. Default is \code{TRUE}.}
}
  Similar to \code{\link[graphics]{par}}, you can get the values of the parameters by specifying the names of the parameters and you can set the values of the parameters by specifying anamed list which contains the new values.

  \code{gap.degree}, \code{start.degree}, \code{canvas.xlim}, \code{canvas.ylim} and \code{clock.wise} only be set before the initialization of circos layout(i.e. before calling \code{\link{circos.initialize}}) because these values will not be changed afterthe layout of the sectors. The left and right padding for \code{cell.padding} will also beeffectiveless after the initialization because all cells in a sector would share the sameleft and right paddings. 


}
