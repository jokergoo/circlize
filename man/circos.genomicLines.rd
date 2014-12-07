\name{circos.genomicLines}
\alias{circos.genomicLines}
\title{
  Add lines to a plotting region, specifically for genomic graphics  


}
\description{
  Add lines to a plotting region, specifically for genomic graphics  


}
\usage{
circos.genomicLines(region, value, numeric.column = NULL,
    sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"), posTransform = NULL,
    col = ifelse(area, "grey", "black"), lwd = par("lwd"),
    lty = par("lty"), type = "l",
    area = FALSE, area.baseline = NULL, border = "black", baseline = "bottom",
    pt.col = par("col"), cex = par("cex"), pch = par("pch"), ...)
}
\arguments{
  \item{region}{A data frame contains 2 column which correspond to start position and end position}
  \item{value}{A data frame contains values and other information}
  \item{numeric.column}{Which column in \code{value} data frame should be taken as y-value. If it is not defined, the whole numeric columns in \code{value} will be taken.}
  \item{sector.index}{Pass to \code{\link{circos.lines}}}
  \item{track.index}{Pass to \code{\link{circos.lines}}}
  \item{posTransform}{Self-defined function to transform genomic positions, see \code{\link{posTransform.default}} for explaination}
  \item{col}{col of lines/areas. If there are more than one numeric column, the length of \code{col} can be either one or number of numeric columns. If there is only one numeric column and type is either \code{segment} or \code{h},  the length of \code{col} can be either one or number of rows of \code{region}. pass to \code{\link{circos.lines}}}
  \item{lwd}{Settings are similar as \code{col}. Pass to \code{\link{circos.lines}}}
  \item{lty}{Settings are similar as \code{col}. Pass to \code{\link{circos.lines}}}
  \item{type}{There is an additional option \code{segment} which plot segment lines from start position to end position. Settings are similar as \code{col}. Pass to \code{\link{circos.lines}}. }
  \item{area}{Settings are similar as \code{col}. Pass to \code{\link{circos.lines}}}
  \item{area.baseline}{Deprecated, use \code{baseline} instead.}
  \item{baseline}{Settings are similar as \code{col}. Pass to \code{\link{circos.lines}}}
  \item{border}{Settings are similar as \code{col}. Pass to \code{\link{circos.lines}}}
  \item{pt.col}{Settings are similar as \code{col}. Pass to \code{\link{circos.lines}}}
  \item{cex}{Settings are similar as \code{col}. Pass to \code{\link{circos.lines}}}
  \item{pch}{Settings are similar as \code{col}. Pass to \code{\link{circos.lines}}}
  \item{...}{mysterious parameters}

}
\details{
  The function is a low-level graphical function and usually is put in \code{panel.fun} when using \code{\link{circos.genomicTrackPlotRegion}}. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
\examples{
\dontrun{

par(mar = c(1, 1, 1, 1))


## test line

### test bed
circos.par("track.height" = 0.1)
circos.initializeWithIdeogram(plotType = NULL)

bed = generateRandomBed(nr = 100)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
    circos.genomicLines(region, value, type = "l", ...)
})

bed1 = generateRandomBed(nr = 100)
bed2 = generateRandomBed(nr = 100)
bed_list = list(bed1, bed2)

circos.genomicTrackPlotRegion(bed_list, panel.fun = function(region, value, ...) {
    i = getI(...)
    circos.genomicLines(region, value, col = i, ...)
})

circos.genomicTrackPlotRegion(bed_list, stack = TRUE, 
    panel.fun = function(region, value, ...) {
    i = getI(...)
    circos.genomicLines(region, value, col = i, ...)
})

bed = generateRandomBed(nr = 100, nc = 4)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
    circos.genomicLines(region, value, col = 1:4, ...)
})

circos.genomicTrackPlotRegion(bed, stack = TRUE, panel.fun = function(region, value, ...) {
    i = getI(...)
    circos.genomicLines(region, value, col = i, ...)
})

bed = generateRandomBed(nr = 100)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
    circos.genomicLines(region, value, type = "segment", lwd = 2, ...)
})

circos.clear()

}
}
