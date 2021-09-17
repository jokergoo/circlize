\name{circos.genomicDensity}
\alias{circos.genomicDensity}
\title{
Calculate and add genomic density track
}
\description{
Calculate and add genomic density track
}
\usage{
circos.genomicDensity(
    data,
    ylim.force = FALSE,
    window.size = NULL,
    overlap = TRUE,
    count_by = c("percent", "number"),
    col = ifelse(area, "grey", "black"),
    lwd = par("lwd"),
    lty = par("lty"),
    type = "l",
    area = TRUE,
    area.baseline = NULL,
    baseline = 0,
    border = NA,
    ...)
}
\arguments{

  \item{data}{A bed-file-like data frame or a list of data frames. If the input is a list of data frames. there will be multiple density plot in one same track.}
  \item{ylim.force}{Whether to force upper bound of \code{ylim} to be 1. Ignored if \code{count_by} is set to \code{number}.}
  \item{window.size}{Pass to \code{\link{genomicDensity}}.}
  \item{overlap}{Pass to \code{\link{genomicDensity}}.}
  \item{count_by}{Pass to \code{\link{genomicDensity}}.}
  \item{col}{Colors. It should be length of one. If \code{data} is a list of data frames, the length of \code{col} can also be the length of the list. If multiple sets of genomic regions are visualized in one single track, you should set the colors with transparency to distinguish them.}
  \item{lwd}{Width of lines, the same setting as \code{col} argument.}
  \item{lty}{Style of lines, the same setting as \code{col} argument.}
  \item{type}{Type of lines, see \code{\link{circos.lines}}.}
  \item{area}{See \code{\link{circos.lines}}.}
  \item{area.baseline}{Deprecated, use \code{baseline} instead.}
  \item{baseline}{See \code{\link{circos.lines}}.}
  \item{border}{See \code{\link{circos.lines}}.}
  \item{...}{Pass to \code{\link{circos.trackPlotRegion}}.}

}
\details{
This function is a high-level graphical function, and it will create a new track.

If you have multiple sets of genomic regions, you should make sure the density ranges 
for all sets are similar, or I suggest you should put them into different tracks. One example
can be found in the "Examples" Section where the density range for \code{bed_list[[2]]} is too high
compared to the range for \code{bed_list[[1]]}, thus, it is better to put the two sets of
regions into two separate tracks.
}
\seealso{
\url{https://jokergoo.github.io/circlize_book/book/high-level-genomic-functions.html#genomic-density-and-rainfall-plot}
}
\examples{
load(system.file(package = "circlize", "extdata", "DMR.RData"))

# rainfall
\donttest{
circos.initializeWithIdeogram(plotType = c("axis", "labels"))

bed_list = list(DMR_hyper, DMR_hypo)
circos.genomicRainfall(bed_list, pch = 16, cex = 0.4, col = c("#FF000080", "#0000FF80"))

circos.genomicDensity(bed_list[[1]], col = c("#FF000080"), track.height = 0.1)
circos.genomicDensity(bed_list[[2]], col = c("#0000FF80"), track.height = 0.1)
circos.clear()

############ draw the two densities in one track  #############
circos.initializeWithIdeogram(plotType = c("axis", "labels"))
circos.genomicDensity(bed_list, col = c("#FF000080", "#0000FF80"), track.height = 0.2)
circos.clear()
}
}
