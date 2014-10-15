\name{circos.par}
\alias{circos.par}
\title{
  Parameters for circos layout  


}
\description{
  Parameters for circos layout  


}
\usage{
circos.par(..., RESET = FALSE, READ.ONLY = NULL)
}
\arguments{
  \item{...}{Arguments for the parameters, see "details" section}
  \item{RESET}{reset to default values}
  \item{READ.ONLY}{whether only return read-only options}

}
\details{
  Global parameters for the circos layout. Currently supported parameters are:  

\describe{
  \item{start.degree}{The starting degree from which the circle begins to draw. Note this degree is measured in the standard polar coordinate which means it is always reverse-clockwise.}
  \item{gap.degree}{Gap between two neighbour sectors. It can be a single value or a vector. If it is a vector, the first value corresponds to the gap after the first sector.}
  \item{track.margin}{Like \code{margin} in Cascading Style Sheets (CSS), it is the blank area out of the plotting region, also outside of the borders. Since left and right margin are controlled by \code{gap.degree}, only bottom and top margin need to be set. And all cells in a same track share the same margins, and that's why this parameter is called \code{track.margin}. The value for the \code{track.margin} is the percentage according to the radius of the unit circle.}
  \item{unit.circle.segments}{Since curves are simulated by a series of straight lines, this parameter controls the amount of segments to represent a curve. The minimal length of the line segmentation is the length of the unit circle (\code{2pi}) divided by \code{unit.circoe.segments}. More segments means better approximation for the curves while larger size if you generate figures as PDF format.}
  \item{cell.padding}{Padding of the cell. Like \code{padding} in Cascading Style Sheets (CSS), it is the blank area around the plotting regions, but within the borders. The parameter has four values, which controls the bottom, left, top and right paddings respectively. The first and the third padding values are the percentages according to the radius of the unit circle and the second and fourth values are degrees.}
  \item{track.height}{The default height of tracks. It is the percentage according to the radius of the unit circle. The height includes the top and bottom cell paddings but not the margins.}
  \item{points.overflow.warning}{Since each cell is in fact not a real plotting region but only an ordinary rectangle, it does not eliminate points that are plotted out of the region. So if some points are out of the plotting region, \code{circlize} would continue drawing the points but print warnings. In some  cases, draw something out of the plotting region is useful, such as draw some legend or text. Set this value to \code{FALSE} to turn off the warnings.}
  \item{canvas.xlim}{The coordinate for the canvas. Because \code{circlize} draws everything (or almost everything) inside the unit circle, the default \code{canvas.xlim} and \code{canvas.ylim} for the canvas would be all \code{c(-1, 1)}. However, you can set it to a more broad interval if you want to draw other things out of the circle. By choosing proper \code{canvas.xlim} and \code{canvas.ylim}, you can draw part of the circle. E.g. setting \code{canvas.xlim} to \code{c(0, 1)} and \code{canvas.ylim} to \code{c(0, 1)} would only draw circle in the region of (0, pi/2).}
  \item{canvas.ylim}{The coordinate for the canvas. By default it is \code{c(-1, 1)}}
  \item{clock.wise}{The direction for adding sectors. Default is \code{TRUE}.}
}
  Similar as \code{\link[graphics]{par}}, you can get the parameter values by specifying the  names of parameters and you can set the parameter values by specifying a named list which contains the new values.  

  \code{gap.degree}, \code{start.degree}, \code{canvas.xlim}, \code{canvas.ylim} and \code{clock.wise}  only be set before the initialization of circos layout (i.e. before calling \code{\link{circos.initialize}}) because these values will not be changed after adding sectors on the circle. The left and right padding for \code{cell.padding} will also be ignored after the initialization because all cells in a sector would share the same left and right paddings.  


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
