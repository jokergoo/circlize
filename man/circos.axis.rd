\name{circos.axis}
\alias{circos.axis}
\title{
Draw x-axis
}
\description{
Draw x-axis
}
\usage{
circos.axis(h = "top", major.at = NULL, labels = TRUE, major.tick = TRUE,
    sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"),
    labels.font = par("font"), labels.cex = par("cex"),
    labels.facing = "inside", labels.direction = NULL, labels.niceFacing = TRUE,
    direction = c("outside", "inside"), minor.ticks = 4,
    major.tick.percentage = 0.1, labels.away.percentage = major.tick.percentage/2,
    major.tick.length = convert_y(1, "mm", sector.index, track.index),
    lwd = par("lwd"), col = par("col"), labels.col = par("col"), labels.pos.adjust = TRUE)
}
\arguments{

  \item{h}{Position of the x-axis, can be "top", "bottom" or a numeric value}
  \item{major.at}{If it is numeric vector, it identifies the positions of the major ticks. It can exceed \code{xlim} value and the exceeding part would be trimmed automatically. If it is \code{NULL}, about every 10 degrees there is a major tick.}
  \item{labels}{labels of the major ticks. Also, the exceeding part would be trimmed automatically. The value can also be logical (either an atomic value or a vector) which represents which labels to show.}
  \item{major.tick}{Whether to draw major tick. If it is set to \code{FALSE}, there would be no minor ticks.}
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}
  \item{labels.font}{font style for the axis labels}
  \item{labels.cex}{font size for the axis labels}
  \item{labels.direction}{deprecated, use \code{facing} instead.}
  \item{labels.facing}{facing of labels on axis, passing to \code{\link{circos.text}}}
  \item{labels.niceFacing}{Should facing of axis labels be human-easy}
  \item{direction}{whether the axis ticks point to the outside or inside of the circle.}
  \item{minor.ticks}{Number of minor ticks between two close major ticks.}
  \item{major.tick.percentage}{not used. Length of the major ticks. It is the percentage to the height of the cell.}
  \item{labels.away.percentage}{not used. The distance for the axis labels to the major ticks. It is the percentage to the height of the cell.}
  \item{major.tick.length}{length of the major ticks, measured in "current" data coordinate. \code{\link{convert_y}} can be used to convert an absolute unit to the data coordinate.}
  \item{lwd}{line width for ticks}
  \item{col}{color for the axes}
  \item{labels.col}{color for the labels}
  \item{labels.pos.adjust}{whether to adjust the positions of the first label and the last label. The value can be a vector of length two which correspond to the first label and the last label.}

}
\details{
It can only draw axes on x-direction.
}
\seealso{
\code{\link{circos.yaxis}} draws axes on y-direction.
}
\examples{
# There is no example
NULL
}
