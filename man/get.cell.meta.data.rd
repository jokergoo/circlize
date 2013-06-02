\name{get.cell.meta.data}
\alias{get.cell.meta.data}
\title{
  Get the meta data for a cell


}
\description{
  Get the meta data for a cell


}
\usage{
get.cell.meta.data(name, sector.index = get.current.sector.index(),
    track.index = get.current.track.index())
}
\arguments{
  \item{name}{Only support one name at a time, see "details" section}
  \item{sector.index}{index for the sector}
  \item{track.index}{index for hte track}

}
\details{
  The following meta information for a cell can be obtained:

\describe{
  \item{sector.index}{The name (label) for the sector}
  \item{sector.numeric.index}{Numeric index for the sector}
  \item{track.index}{Numeric index for the track}
  \item{xlim}{Minimal and maximal values on the x-axis}
  \item{ylim}{Minimal and maximal values on the y-axis}
  \item{xrange}{Range of \code{xlim}}
  \item{yrange}{Range of \code{ylim}}
  \item{cell.xlim}{Minimal and maximal values on the x-axis extended by cell paddings}
  \item{cell.ylim}{Minimal and maximal values on the y-axis extended by cell paddings}
  \item{xplot}{Right and left edge degree for the plotting region in the circle. The first element corresponds to the start point of values on x-axis (\code{cell.xlm[1]}) and the second element corresponds to the end point of values on x-axis (\code{cell.xlim[2]}) Since x-axis in data coordinate in cells are always reverse clockwise, \code{xplot[1]} is larger than \code{xplot[2]}.}
  \item{yplot}{Bottom and top radius value for borders of the plotting region. It is the value of radius of arc corresponding to inner border or outer border.}
  \item{cell.start.degree}{Same as \code{xplot[1]}}
  \item{cell.end.degree}{Same as \code{xplot[2]}}
  \item{cell.bottom.radius}{Same as \code{yplot[1]}}
  \item{cell.top.radius}{Same as \code{yplot[2]}}
  \item{track.margin}{Margin for the cell}
  \item{cell.padding}{Padding for the cell}
}
  The function would be useful when you use \code{panel.fun} in \code{\link{circos.trackPlotRegion}} toget detailed information of the current cell.


}
