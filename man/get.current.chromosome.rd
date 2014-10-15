\name{get.current.chromosome}
\alias{get.current.chromosome}
\title{
  Get current chromosome name  


}
\description{
  Get current chromosome name  


}
\usage{
get.current.chromosome()
}
\details{
  The function is a simple wrapper of \code{get.cell.meta.data("sector.index")} and should only be put inside \code{panel.fun} when using \code{\link{circos.genomicTrackPlotRegion}}. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
\examples{
\dontrun{
library(circlize)
circos.initializeWithIdeogram()
circos.genomicTrackPlotRegion(ylim = c(0, 1), panel.fun = function(region, value, ...) {
    print(get.current.chromosome())
})
circos.clear()
}
}
