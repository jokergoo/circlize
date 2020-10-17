\name{circos.yaxis}
\alias{circos.yaxis}
\title{
Draw y-axis
}
\description{
Draw y-axis
}
\usage{
circos.yaxis(
    side = c("left", "right"),
    at = NULL,
    labels = TRUE,
    tick = TRUE,
    sector.index = get.current.sector.index(),
    track.index = get.current.track.index(),
    labels.font = par("font"),
    labels.cex = par("cex"),
    labels.niceFacing = TRUE,
    tick.length = convert_x(1, "mm", sector.index, track.index),
    lwd = par("lwd"),
    col = par("col"),
    labels.col = par("col"))
}
\arguments{

  \item{side}{add the y-axis on the left or right of the cell}
  \item{at}{If it is numeric vector, it identifies the positions of the ticks. It can exceed \code{ylim} value and the exceeding part would be trimmed automatically.}
  \item{labels}{labels of the ticks. The exceeding part would be trimmed automatically. The value can also be logical (either an atomic value or a vector) which represents which labels to show.}
  \item{tick}{Whether to draw ticks.}
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}
  \item{labels.font}{font style for the axis labels}
  \item{labels.cex}{font size for the axis labels}
  \item{labels.niceFacing}{Should facing of axis labels be human-easy}
  \item{tick.length}{length of the tick}
  \item{lwd}{line width for ticks}
  \item{col}{color for the axes}
  \item{labels.col}{color for the labels}

}
\details{
Note, you need to set the gap between sectors manually by \code{\link{circos.par}} to make sure there is enough space
for y-axis.
}
\examples{
op = par(no.readonly = TRUE)

sectors = letters[1:8]
circos.par(points.overflow.warning = FALSE)
circos.par(gap.degree = 8)
circos.initialize(sectors, xlim = c(0, 10))
circos.trackPlotRegion(sectors, ylim = c(0, 10), track.height = 0.5)
par(cex = 0.8)
for(a in letters[2:4]) {
  circos.yaxis(side = "left", sector.index = a)
}
for(a in letters[5:7]) {
  circos.yaxis(side = "right", sector.index = a)
}
circos.clear()

par(op)
}
