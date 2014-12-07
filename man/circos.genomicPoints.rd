\name{circos.genomicPoints}
\alias{circos.genomicPoints}
\title{
  Add points to a plotting region, specifically for genomic graphics  


}
\description{
  Add points to a plotting region, specifically for genomic graphics  


}
\usage{
circos.genomicPoints(region, value, numeric.column = NULL,
    sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"), posTransform = NULL,
    pch = par("pch"), col = par("col"), cex = par("cex"), ...)
}
\arguments{
  \item{region}{A data frame contains 2 columns which correspond to start positions and end positions}
  \item{value}{A data frame contains values and other information}
  \item{numeric.column}{Which column in \code{value} data frame should be taken as y-value. If it is not defined, the whole numeric columns in \code{value} will be taken.}
  \item{sector.index}{Pass to \code{\link{circos.points}}}
  \item{track.index}{Pass to \code{\link{circos.points}}}
  \item{posTransform}{Self-defined function to transform genomic positions, see \code{\link{posTransform.default}} for explanation}
  \item{col}{color of points. If there is only one numeric column, the length of \code{col} can be either one or number of rows of \code{region}. If there are more than one numeric column, the length of \code{col} can be either one or number of numeric columns. Pass to \code{\link{circos.points}}}
  \item{pch}{Type of points. Settings are similar as \code{col}. Pass to \code{\link{circos.points}}}
  \item{cex}{Size of points. Settings are similar as \code{col}. Pass to \code{\link{circos.points}}}
  \item{...}{Mysterious parameters}

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

circos.par("track.height" = 0.1)
circos.initializeWithIdeogram(plotType = NULL)

bed = generateRandomBed(nr = 100)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
    circos.genomicPoints(region, value, pch = 16, cex = 0.5, ...)
})

circos.genomicTrackPlotRegion(bed, stack = TRUE, panel.fun = function(region, value, ...) {
    circos.genomicPoints(region, value, pch = 16, cex = 0.5, ...)
    i = getI(...)
    cell.xlim = get.cell.meta.data("cell.xlim")
    circos.lines(cell.xlim, c(i, i), lty = 2, col = "#00000040")
})

bed1 = generateRandomBed(nr = 100)
bed2 = generateRandomBed(nr = 100)
bed_list = list(bed1, bed2)

# data frame list
circos.genomicTrackPlotRegion(bed_list, panel.fun = function(region, value, ...) {
    cex = (value[[1]] - min(value[[1]]))/(max(value[[1]]) - min(value[[1]]))
    i = getI(...)
    circos.genomicPoints(region, value, cex = cex, pch = 16, col = i, ...)
})

circos.genomicTrackPlotRegion(bed_list, stack = TRUE,
    panel.fun = function(region, value, ...) {
    cex = (value[[1]] - min(value[[1]]))/(max(value[[1]]) - min(value[[1]]))
    i = getI(...)
    circos.genomicPoints(region, value, cex = cex, pch = 16, col = i, ...)
    cell.xlim = get.cell.meta.data("cell.xlim")
    circos.lines(cell.xlim, c(i, i), lty = 2, col = "#00000040")
})


bed = generateRandomBed(nr = 100, nc = 4)
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
    cex = (value[[1]] - min(value[[1]]))/(max(value[[1]]) - min(value[[1]]))
    circos.genomicPoints(region, value, cex = 0.5, pch = 16, col = 1:4, ...)
})

circos.genomicTrackPlotRegion(bed, stack = TRUE, panel.fun = function(region, value, ...) {
    cex = (value[[1]] - min(value[[1]]))/(max(value[[1]]) - min(value[[1]]))
    i = getI(...)
    circos.genomicPoints(region, value, cex = cex, pch = 16, col = i, ...)
    cell.xlim = get.cell.meta.data("cell.xlim")
    circos.lines(cell.xlim, c(i, i), lty = 2, col = "#00000040")
})

circos.clear()

}
}
