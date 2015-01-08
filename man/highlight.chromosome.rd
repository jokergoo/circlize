\name{highlight.chromosome}
\alias{highlight.chromosome}
\title{
  Highlight chromosomes  


}
\description{
  Highlight chromosomes  


}
\usage{
highlight.chromosome(chr, track.index = get.all.track.index(),
    col = "#FF000040", border = NA, lwd = par("lwd"), lty = par("lty"),
    padding = c(0, 0, 0, 0))
}
\arguments{
  \item{chr}{Chromosome names. It should be consistent with the sector index.}
  \item{track.index}{A vector of track index that you want to highlight}
  \item{col}{Color for highlighting. Note the color should be semi-transparent.}
  \item{border}{Border of the highlighted region}
  \item{lwd}{Width of borders}
  \item{lty}{Style of borders}
  \item{padding}{Padding for the highlighted region. It should contain four values representing ratios of the width or height of the highlighted region}

}
\details{
  You can use \code{\link{circos.info}} to find out index for all tracks.  

  The function calls \code{\link{draw.sector}}. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
\examples{
\dontrun{

par(mar = c(1.5, 1.5, 1.5, 1.5))
# highlight 
circos.par("track.height" = 0.1, cell.padding = c(0, 0, 0, 0))
circos.initializeWithIdeogram(plotType = c("axis", "labels"))

bed = generateRandomBed(nr = 100)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
    circos.genomicPoints(region, value, pch = 16, cex = 0.5, ...)
})

bed = generateRandomBed(nr = 100)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
    circos.genomicPoints(region, value, pch = 16, cex = 0.5, ...)
})

bed = generateRandomBed(nr = 100)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
    circos.genomicPoints(region, value, pch = 16, cex = 0.5, ...)
})

bed = generateRandomBed(nr = 100)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
    circos.genomicPoints(region, value, pch = 16, cex = 0.5, ...)
})


highlight.chromosome("chr1", col = "#FF000040", padding = c(0.05, 0.05, 0.15, 0.05))
highlight.chromosome("chr3", col = NA, border = "red", lwd = 2,
    padding = c(0.05, 0.05, 0.15, 0.05))
highlight.chromosome("chr5", col = "#0000FF40", track.index = c(2, 4, 5))
highlight.chromosome("chr7", col = NA, border = "green", lwd = 2,
    track.index = c(2, 4, 5))
circos.clear()

}
}
