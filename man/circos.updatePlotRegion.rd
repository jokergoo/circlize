\name{circos.updatePlotRegion}
\alias{circos.updatePlotRegion}
\title{
  Update the plotting region in an existed cell  


}
\description{
  Update the plotting region in an existed cell  


}
\usage{
circos.updatePlotRegion(sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"),
    bg.col = NA, bg.border = "black", bg.lty = par("lty"), bg.lwd = par("lwd"))
}
\arguments{
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}
  \item{bg.col}{Background color for the plotting region}
  \item{bg.border}{Color for the border of the plotting region}
  \item{bg.lty}{Line style for the border of the plotting region}
  \item{bg.lwd}{Line width for the border of the plotting region}

}
\details{
  You can update an existed cell by this function by erasing all the graphics. But the \code{xlim} and \code{ylim} inside the cell still remains unchanged.   

  Note if you use \code{\link{circos.trackPlotRegion}} to update an already created track, you can re-define \code{ylim} in these cells. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
