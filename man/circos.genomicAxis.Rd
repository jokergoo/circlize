\name{circos.genomicAxis}
\alias{circos.genomicAxis}
\title{
Add genomic axes
}
\description{
Add genomic axes
}
\usage{
circos.genomicAxis(
    h = "top",
    major.at = NULL,
    labels = NULL,
    major.by = NULL,
    tickLabelsStartFromZero = TRUE,
    labels.cex = 0.4*par("cex"),
    sector.index = get.current.sector.index(),
    track.index = get.current.track.index(),
    ...)
}
\arguments{

  \item{h}{Position of the axes. "top" or "bottom".}
  \item{major.at}{Major breaks. If \code{major.at} is set, \code{major.by} is ignored.}
  \item{labels}{labels corresponding to \code{major.at}. If \code{labels} is set, \code{major.at} must be set.}
  \item{major.by}{Increment of major ticks. It is calculated automatically if the value is not set (about every 10 degrees there is a major tick).}
  \item{tickLabelsStartFromZero}{Whether axis tick labels start from 0? This will only affect the axis labels while not affect x-values in cells.}
  \item{labels.cex}{The font size for the axis tick labels.}
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}
  \item{...}{Other arguments pass to \code{\link{circos.axis}}.}

}
\details{
It assigns proper tick labels under genomic coordinate.
}
\seealso{
\url{https://jokergoo.github.io/circlize_book/book/high-level-genomic-functions.html#genomic-axes}
}
\examples{
circos.initializeWithIdeogram(chromosome.index = paste0("chr", 1:4), plotType = NULL)
circos.track(ylim = c(0, 1), panel.fun = function(x, y) circos.genomicAxis())
circos.track(ylim = c(0, 1), track.height = 0.1)
circos.track(track.index = get.current.track.index(), panel.fun = function(x, y) {
    circos.genomicAxis(h = "bottom", direction = "inside")
})
circos.clear()
}
