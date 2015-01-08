\name{get.all.sector.index}
\alias{get.all.sector.index}
\title{
  Get index for all sectors  


}
\description{
  Get index for all sectors  


}
\usage{
get.all.sector.index()
}
\details{
  Simple function returning a vector of all sector index. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
\examples{
\dontrun{
library(circlize)
factors = letters[1:4]
circos.initialize(factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1))
get.all.sector.index()
circos.clear()
}
}
