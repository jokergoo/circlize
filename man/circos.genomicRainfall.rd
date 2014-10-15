\name{circos.genomicRainfall}
\alias{circos.genomicRainfall}
\title{
  Genomic rainfall plot  


}
\description{
  Genomic rainfall plot  


}
\usage{
circos.genomicRainfall(data, ylim = c(0, 9), col = "black", pch = par("pch"),
    cex = par("cex"), ...)
}
\arguments{
  \item{data}{A bed-file-like data frame or a list of data frames}
  \item{ylim}{ylim for rainfall plot track. It's value should be log10(inter-distance+1)}
  \item{col}{Color of points. It should be length of one. If \code{data} is a list, the length of \code{col} can also be the length of the list.}
  \item{pch}{Style of points}
  \item{cex}{Size of points}
  \item{...}{Pass to \code{\link{circos.trackPlotRegion}}}

}
\details{
  This is high-level graphical function, which mean, it will create a new track.  

  Rainfall plot can be used to visualize distribution of regions. On the plot, y-axis corresponds to the distance to neighbour regions (log-based). So if there is a drop-down on the plot, it means there is a cluster of regions at that area.  

  On the plot, y-axis are log10-transformed. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
\examples{
\dontrun{
library(circlize)

par(mar = c(1, 1, 1, 1))

load(paste(system.file(package = "circlize"), "/extdata/DMR.RData", sep=""))

# rainfall
circos.initializeWithIdeogram(plotType = c("axis", "labels"))

bed_list = list(DMR_hyper, DMR_hypo)
circos.genomicRainfall(bed_list, pch = 16, cex = 0.4, col = c("#FF000080", "#0000FF80"))

circos.genomicDensity(bed_list[[1]], col = c("#FF000080"), track.height = 0.1)
circos.genomicDensity(bed_list[[2]], col = c("#0000FF80"), track.height = 0.1)

circos.clear()

}
}
