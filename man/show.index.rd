\name{show.index}
\alias{show.index}
\title{
  Label the sector index and the track index on each cell  


}
\description{
  Label the sector index and the track index on each cell  


}
\usage{
show.index()
}
\details{
  This function is deprecated, please use \code{\link{circos.info}} instead. 


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
