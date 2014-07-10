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
  \item{name}{Only support one name at a time, see "details" section}
  \item{sector.index}{Index of the sector}
  \item{track.index}{Index of the track}

}
\details{
  The following meta information for a cell can be obtained:  

\describe{
  \item{sector.index}{The name (index) for the sector}
  \item{sector.numeric.index}{Numeric index for the sector. It is the numeric order of levels of \code{factors} in initialization step}
  \item{track.index}{Numeric index for the track}
  \item{xlim}{Minimal and maximal values on the x-axis}
  \item{ylim}{Minimal and maximal values on the y-axis}
  \item{xrange}{Range of \code{xlim}. It equals to \code{xlim[2] - xlim[1]} }
  \item{yrange}{Range of \code{ylim}}
  \item{xcenter}{Center of x-axis. It equals to \code{(xlim[2] + xlim[1])/2} }
  \item{ycenter}{Center of y-axis}
  \item{cell.xlim}{Minimal and maximal values on the x-axis extended by cell paddings}
  \item{cell.ylim}{Minimal and maximal values on the y-axis extended by cell paddings}
  \item{xplot}{Right and left edge degree for the plotting region which are measured in polar coordinate. The first element corresponds to the start point of values on x-axis (\code{cell.xlm[1]}) and the second element corresponds to the end point of values on x-axis (\code{cell.xlim[2]}) Since x-axis in data coordinate in cells are always clockwise, \code{xplot[1]} is larger than \code{xplot[2]}.}
  \item{yplot}{Bottom and top value for the plotting region in polar coordinate. It is the value of radius of arc corresponding to top border or bottom border.}
  \item{cell.start.degree}{Same as \code{xplot[1]}}
  \item{cell.end.degree}{Same as \code{xplot[2]}}
  \item{cell.bottom.radius}{Same as \code{yplot[1]}}
  \item{cell.top.radius}{Same as \code{yplot[2]}}
  \item{track.margin}{Margin for the cell}
  \item{cell.padding}{Padding for the cell}
}
  The function would be useful when you use \code{panel.fun} in \code{\link{circos.trackPlotRegion}} to get detailed information of the current cell. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
