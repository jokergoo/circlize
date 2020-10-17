\name{get.cell.meta.data}
\alias{get.cell.meta.data}
\title{
Get the meta data of a cell
}
\description{
Get the meta data of a cell
}
\usage{
get.cell.meta.data(name, sector.index = get.current.sector.index(),
    track.index = get.current.track.index())
}
\arguments{

  \item{name}{Only support one name at a time, see "details" section}
  \item{sector.index}{Index of the sector}
  \item{track.index}{Index of the track}

}
\details{
The following meta information for a cell can be obtained:

\describe{
  \item{\code{sector.index}}{The name (index) for the sector}
  \item{\code{sector.numeric.index}}{Numeric index for the sector}
  \item{\code{track.index}}{Numeric index for the track}
  \item{\code{xlim}}{Minimal and maximal values on the x-axis}
  \item{\code{ylim}}{Minimal and maximal values on the y-axis}
  \item{\code{xrange}}{Range of \code{xlim}. It equals to \code{xlim[2] - xlim[1]}}
  \item{\code{yrange}}{Range of \code{ylim}}
  \item{\code{xcenter}}{Center of x-axis. It equals to \code{(xlim[2] + xlim[1])/2}}
  \item{\code{ycenter}}{Center of y-axis}
  \item{\code{cell.xlim}}{Minimal and maximal values on the x-axis extended by cell paddings}
  \item{\code{cell.ylim}}{Minimal and maximal values on the y-axis extended by cell paddings}
  \item{\code{xplot}}{Degrees for right and left borders of the cell. The values ignore the direction of the circular layout (i.e. whether it is clock wise or not).}
  \item{\code{yplot}}{Radius for top and bottom borders of the cell.}
  \item{\code{cell.width}}{Width of the cell, in degrees.}
  \item{\code{cell.height}}{Height of the cell, simply \code{yplot[2] - yplot[1]}}
  \item{\code{cell.start.degree}}{Same as \code{xplot[1]}}
  \item{\code{cell.end.degree}}{Same as \code{xplot[2]}}
  \item{\code{cell.bottom.radius}}{Same as \code{yplot[1]}}
  \item{\code{cell.top.radius}}{Same as \code{yplot[2]}}
  \item{\code{track.margin}}{Margin for the cell}
  \item{\code{cell.padding}}{Padding for the cell}
}

The function is useful when using \code{panel.fun} in \code{\link{circos.track}} to
get detailed information of the current cell.
}
\seealso{
\code{\link{CELL_META}} is a short version of \code{\link{get.cell.meta.data}}.
}
\examples{
sectors = letters[1:4]
circos.initialize(sectors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
    print(get.cell.meta.data("xlim"))
})
print(get.cell.meta.data("xlim", sector.index = "a", track.index = 1))
circos.clear()
}
