\name{circos.genomicAxis}
\alias{circos.genomicAxis}
\title{
Add genomic axes
}
\description{
Add genomic axes
}
\usage{
circos.genomicAxis(h = "top", major.by = NULL, tickLabelsStartFromZero = TRUE,
    labels.cex = 0.4*par("cex"), sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"), ...)
}
\arguments{

  \item{h}{Position of the axes. "top" or "bottom".}
  \item{major.by}{Increment of major ticks. It is calculated automatically if the value is not set (about every 10 degrees there is a major tick).}
  \item{tickLabelsStartFromZero}{Whether axis tick labels start from 0? This will only affect the axis labels while not affect x-values in cells.}
  \item{labels.cex}{the font size for the axis tick labels.}
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}
  \item{...}{Other arguments pass to \code{\link{circos.axis}}.}

}
\details{
It assigns proper tick labels under genomic coordinate.
}
\examples{
circos.initializeWithIdeogram(plotType = NULL)
circos.track(ylim = c(0, 1), panel.fun = function(x, y) circos.genomicAxis())
circos.clear()
}
