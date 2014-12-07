\name{circos.genomicText}
\alias{circos.genomicText}
\title{
  Draw text in a cell, specifically for genomic graphics  


}
\description{
  Draw text in a cell, specifically for genomic graphics  


}
\usage{
circos.genomicText(region, value, y = NULL, labels = NULL, labels.column = NULL,
    numeric.column = NULL, sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"), posTransform = NULL,
    direction = NULL, facing = "inside", niceFacing = FALSE,
    adj = par("adj"), cex = 1, col = "black", font = par("font"), padding = 0, ...)
}
\arguments{
  \item{region}{A data frame contains 2 column which correspond to start position and end position}
  \item{value}{A data frame contains values and other information}
  \item{y}{A vector or a single value indicating position of text.}
  \item{labels}{Labels of text corresponding to each genomic positions}
  \item{labels.column}{If labels are in \code{value}, index of column in \code{value}}
  \item{numeric.column}{Which column in \code{value} data frame should be taken as y-value. If it is not defined, only the first numeric columns in \code{value} will be taken.}
  \item{sector.index}{Pass to \code{\link{circos.rect}}}
  \item{track.index}{Pass to \code{\link{circos.rect}}}
  \item{posTransform}{Self-defined function to transform genomic positions, see \code{\link{posTransform.default}} for explanation}
  \item{facing}{Passing to \code{\link{circos.text}}. Settings are similar as \code{col} }
  \item{niceFacing}{Should the facing of text be adjusted to fit human eyes?}
  \item{direction}{Deprecated, use \code{facing} instead. }
  \item{adj}{Pass to \code{\link{circos.text}}. Settings are similar as \code{col}}
  \item{cex}{Pass to \code{\link{circos.text}}. Settings are similar as \code{col}}
  \item{col}{Pass to \code{\link{circos.text}}. The length of \code{col} can be either one or number of rows of \code{region}.}
  \item{font}{Pass to \code{\link{circos.text}}. Settings are similar as \code{col}}
  \item{padding}{pass to \code{posTransform} if it is set as \code{\link{posTransform.text}}}
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
circos.par("track.height" = 0.1, cell.padding = c(0, 0, 0, 0))
circos.initializeWithIdeogram(plotType = NULL)

bed = generateRandomBed(nr = 20)

circos.genomicTrackPlotRegion(bed, ylim = c(0, 1), panel.fun = function(region, value, ...) {
    circos.genomicText(region, value, y = 0.5, labels = "text", ...)
})

bed = cbind(bed, sample(letters, nrow(bed), replace = TRUE))
circos.genomicTrackPlotRegion(bed, panel.fun = function(region, value, ...) {
    circos.genomicText(region, value, labels.column = 2, ...)
})

circos.clear()

}
}
