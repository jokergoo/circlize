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
  \item{sector.index}{The name (index) for the sector}
  \item{sector.numeric.index}{Numeric index for the sector}
  \item{track.index}{Numeric index for the track}
  \item{xlim}{Minimal and maximal values on the x-axis}
  \item{ylim}{Minimal and maximal values on the y-axis}
  \item{xrange}{Range of \code{xlim}. It equals to \code{xlim[2] - xlim[1]} }
  \item{yrange}{Range of \code{ylim}}
  \item{xcenter}{Center of x-axis. It equals to \code{(xlim[2] + xlim[1])/2} }
  \item{ycenter}{Center of y-axis}
  \item{cell.xlim}{Minimal and maximal values on the x-axis extended by cell paddings}
  \item{cell.ylim}{Minimal and maximal values on the y-axis extended by cell paddings}
  \item{xplot}{Degrees for right and left borders of the cell.}
  \item{yplot}{Radius for top and bottom borders of the cell.}
  \item{cell.start.degree}{Same as \code{xplot[1]}}
  \item{cell.end.degree}{Same as \code{xplot[2]}}
  \item{cell.bottom.radius}{Same as \code{yplot[1]}}
  \item{cell.top.radius}{Same as \code{yplot[2]}}
  \item{track.margin}{Margin for the cell}
  \item{cell.padding}{Padding for the cell}
}
  The function is useful when using \code{panel.fun} in \code{\link{circos.trackPlotRegion}} to get detailed information of the current cell. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
\examples{
\dontrun{
library(circlize)
factors = letters[1:4]
circos.initialize(factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1), panel.fun = function(x, y) {
    print(get.cell.meta.data("xlim"))
})
print(get.cell.meta.data("xlim", sector.index = "a", track.index = 1))
circos.clear()
}
}
