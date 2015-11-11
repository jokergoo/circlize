\name{circos.yaxis}
\alias{circos.yaxis}
\title{
Draw y-axis
}
\description{
Draw y-axis
}
\usage{
circos.yaxis(side = c("left", "right"), at = NULL, labels = TRUE, tick = TRUE,
    sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"),
    labels.font = par("font"), labels.cex = par("cex"),
    labels.niceFacing = TRUE,
    tick.length = 0.5, lwd = par("lwd"))
}
\arguments{

  \item{side}{add the y-axis on the left or right of the cell}
  \item{at}{If it is numeric vector, it identifies the positions of the ticks. It can exceed \code{ylim} value and the exceeding part would be trimmed automatically.}
  \item{labels}{labels of the ticks. Also, the exceeding part would be trimmed automatically.}
  \item{tick}{Whether to draw ticks.}
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}
  \item{labels.font}{font style for the axis labels}
  \item{labels.cex}{font size for the axis labels}
  \item{labels.niceFacing}{Should facing of axis labels be human-easy}
  \item{tick.length}{length of the tick, measured by degree}
  \item{lwd}{line width for ticks}

}
\details{
Note, you need to set the gap between sectors manually by \code{\link{circos.par}} to make sure there is enough space
for y-axis.
}
\examples{
# There is no example
NULL

}
