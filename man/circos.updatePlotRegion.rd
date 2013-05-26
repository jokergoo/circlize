\name{circos.updatePlotRegion}
\alias{circos.updatePlotRegion}
\title{
  Update the plotting region in an existed cell


}
\description{
  Update the plotting region in an existed cell


}
\usage{
circos.updatePlotRegion(sector.index = get.current.sector.index(),
    track.index = get.current.track.index(), bg.col = NA, bg.border = "black",
    bg.lty = par("lty"), bg.lwd = par("lwd"))
}
\arguments{
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}
  \item{bg.col}{Background color for the plotting region}
  \item{bg.border}{Color for the boder of the plotting region}
  \item{bg.lty}{Line style for the border of the plotting region}
  \item{bg.lwd}{Line width for the border of the plotting region}

}
\details{
  You can update an existed cell by this function by erasing the contents in the plotting regions


}
