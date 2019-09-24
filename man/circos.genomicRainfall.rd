\name{circos.genomicRainfall}
\alias{circos.genomicRainfall}
\title{
Genomic rainfall plot
}
\description{
Genomic rainfall plot
}
\usage{
circos.genomicRainfall(data, mode = "min", ylim = NULL, col = "black",
    pch = par("pch"), cex = par("cex"), normalize_to_width = FALSE, ...)
}
\arguments{

  \item{data}{A bed-file-like data frame or a list of data frames}
  \item{mode}{how to calculate the distance of two neighbouring regions, pass to \code{\link{rainfallTransform}}}
  \item{ylim}{ylim for rainfall plot track. If \code{normalize_to_width} is \code{FALSE}, the value should correspond to log10(dist+1), and if \code{normalize_to_width} is \code{TRUE}, the value should correspond to log2(rel_dist).}
  \item{col}{Color of points. It should be length of one. If \code{data} is a list, the length of \code{col} can also be the length of the list.}
  \item{pch}{Style of points}
  \item{cex}{Size of points}
  \item{normalize_to_width}{If it is \code{TRUE}, the value is the relative distance divided by the width of the region.}
  \item{...}{Pass to \code{\link{circos.trackPlotRegion}}}

}
\details{
This is high-level graphical function, which mean, it will create a new track.

Rainfall plot can be used to visualize distribution of regions. On the plot, y-axis
corresponds to the distance to neighbour regions (log-based). So if there is a drop-down on
the plot, it means there is a cluster of regions at that area.

On the plot, y-axis are log10-transformed.
}
\examples{
# There is no example
NULL
}
