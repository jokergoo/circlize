\name{circos.genomicRainfall}
\alias{circos.genomicRainfall}
\title{
  Genomic rainfall plot  


}
\description{
  Genomic rainfall plot  


}
\usage{
circos.genomicRainfall(data, col = "black", pch = par("pch"), cex = par("cex"), ...)
}
\arguments{
  \item{data}{A bed-file-like data frame or a \code{GRanges} object. It can also be a list containing data frames and \code{GRanges} objects.}
  \item{col}{color of points. It should be length of one. If \code{data} is a list, the length of \code{col} can also be the length of the list.}
  \item{pch}{style of points}
  \item{cex}{size of points}
  \item{...}{pass to \code{\link{circos.trackPlotRegion}}}

}
\details{
  This is high-level graphical function, which mean, it will create a new track.  

  Rainfall plot can be used to visualize distribution of regions. On the plot, y-axis corresponds to the distance to neighbour regions. So if there is a drop-down on the plot, it means there is a cluster of regions at that area.  

  On the plot, y-axis are log10-transformed. 


}
