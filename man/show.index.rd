\name{show.index}
\alias{show.index}
\title{
  Label the sector index and the track index of each cell


}
\description{
  Label the sector index and the track index of each cell


}
\usage{
show.index()
}
\details{
  Draw the index of the sector and the track for each cell on the figure.This function can help you to find the coordinates of cells. 


}
\examples{
library(circlize)
par(mar = c(1, 1, 1, 1))
factors = factor(letters[1:10], levels = sample(letters[1:10], 10))
circos.par("cell.padding" = c(0, 0, 0, 0), points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10))
show.index()
circos.clear()
}