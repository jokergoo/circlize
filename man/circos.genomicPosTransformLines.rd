\name{circos.genomicPosTransformLines}
\alias{circos.genomicPosTransformLines}
\title{
  Add genomic position transformation lines between tracks  


}
\description{
  Add genomic position transformation lines between tracks  


}
\usage{
circos.genomicPosTransformLines(data, track.height = 0.1, posTransform = NULL,
    horizontalLine = c("none", "top", "bottom", "both"), track.margin = c(0, 0),
    direction = c("inside", "outside"), col = "black", lwd = par("lwd"),
    lty = par("lty"), ...)
}
\arguments{
  \item{data}{A data frame containing genomic data}
  \item{track.height}{Height of the track}
  \item{posTransform}{Genomic position transformation function, see \code{\link{posTransform.default}} for an example.}
  \item{horizontalLine}{Whether to draw horizontal lines which indicate region width }
  \item{track.margin}{Margin of tracks}
  \item{direction}{Type of the transformation. \code{inside} means position transformed track are located inside  and \code{outside} means position transformed track are located outside.}
  \item{col}{Color of lines, can be length of one or nrow of \code{data}}
  \item{lwd}{Width of lines}
  \item{lty}{Style of lines}
  \item{...}{pass to \code{\link{circos.trackPlotRegion}}}

}
\details{
  There is one representative situation when such position transformation needs to be applied.  For example, there are two sets of regions in a chromosome in which regions in one set regions are quite densely to each other and regions in other set are far from others. Heatmap or text is going to be drawn on the next track. If there is no position transformation, heatmap or text for those dense regions would be overlapped and hard to identify, also ugly to visualize. Thus, a way to transform original positions to new positions would help for the visualization.  


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
\examples{
\dontrun{
library(circlize)

par(mfrow = c(2, 1))
par(mar = c(1, 1, 1, 1))
### rect matrix
circos.par(cell.padding = c(0, 0, 0, 0))
circos.initializeWithIdeogram()

bed = generateRandomBed(nr = 100, nc = 4)

circos.genomicPosTransformLines(bed, posTransform = posTransform.default,
    horizontalLine = "top", track.height = 0.1)

f = colorRamp2(breaks = c(-1, 0, 1), colors = c("green", "black", "red"))
circos.genomicTrackPlotRegion(bed, stack = TRUE, panel.fun = function(region, value, ...) {
    circos.genomicRect(region, value, col = f(value[[1]]), 
        border = f(value[[1]]), posTransform = posTransform.default, ...)
}, bg.border = NA)

circos.clear()


circos.par(cell.padding = c(0, 0, 0, 0))
circos.initializeWithIdeogram(plotType = NULL)

bed = generateRandomBed(nr = 20, nc = 4)

circos.genomicTrackPlotRegion(bed, ylim = c(0, 1), panel.fun = function(region, value, ...) {
    circos.genomicText(region, value, y = 0, adj = c(1, 0.5),
        labels = "gene", facing = "reverse.clockwise",
        posTransform = posTransform.default)
}, bg.border = NA)

circos.genomicPosTransformLines(bed, posTransform = posTransform.default,
    horizontalLine = "bottom", direction = "outside", track.height = 0.1)

cytoband = read.cytoband()$df
circos.genomicTrackPlotRegion(cytoband, stack = TRUE, panel.fun = function(region, value, ...) {
    circos.genomicRect(region, value, col = cytoband.col(value$V5), border = NA, ...)
}, track.height = 0.05)

circos.clear()
}
}
