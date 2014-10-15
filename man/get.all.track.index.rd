\name{get.all.track.index}
\alias{get.all.track.index}
\title{
  Get index for all tracks  


}
\description{
  Get index for all tracks  


}
\usage{
get.all.track.index()
}
\details{
  Simple function returning a vector of all track index. 


}
\examples{
\dontrun{
library(circlize)
factors = letters[1:4]
circos.initialize(factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1))
get.all.track.index()
circos.clear()
}
}
