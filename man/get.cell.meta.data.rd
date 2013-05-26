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
  \item{sector.index}{The name for the sector}
  \item{sector.numeric.index}{Numeric index for the sector}
  \item{track.index}{Numeric index for the track}
  \item{xlim}{Minimal and maximal values on the x-axis}
  \item{ylim}{Minimal and maximal values on the y-axis}
  \item{xrange}{Range of the xlim}
  \item{yrange}{Range of the ylim}
  \item{cell.xlim}{Minimal and maximal values on the x-axis extended by cell paddings}
  \item{cell.ylim}{Minimal and maximal values on the y-axis extended by cell paddings}
  \item{xplot}{Right and left edge degree for the plotting region in the canvas}
  \item{yplot}{Bottum and top value for the plotting region in the canvas}
  \item{track.margin}{Margin for the cell}
  \item{cell.padding}{Padding for the cell}
}
  The function would be useful when you use \code{panel.fun} in \code{\link{circos.initialize}} toget the information of the current cell.


}
