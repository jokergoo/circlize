\name{circos.axis}
\alias{circos.axis}
\title{
Draw x-axis
}
\description{
Draw x-axis
}
\usage{
circos.axis(
    h = "top",
    major.at = NULL,
    labels = TRUE,
    major.tick = TRUE,
    sector.index = get.current.sector.index(),
    track.index = get.current.track.index(),
    labels.font = par("font"),
    labels.cex = par("cex"),
    labels.facing = "inside",
    labels.direction = NULL,
    labels.niceFacing = TRUE,
    direction = c("outside", "inside"),
    minor.ticks = 4,
    major.tick.length = mm_y(1),
    major.tick.percentage = 0.5,
    lwd = par("lwd"),
    col = par("col"),
    labels.col = par("col"),
    labels.pos.adjust = TRUE)
}
\arguments{

  \item{h}{Position of the x-axis, can be "top", "bottom" or a numeric value}
  \item{major.at}{If it is numeric vector, it identifies the positions of the major ticks. It can exceed \code{xlim} value and the exceeding part would be trimmed automatically. If it is \code{NULL}, about every 10 degrees there is a major tick.}
  \item{labels}{labels of the major ticks. Also, the exceeding part would be trimmed automatically. The value can also be logical (either an atomic value or a vector) which represents which labels to show.}
  \item{major.tick}{Whether to draw major tick. If it is set to \code{FALSE}, there will be no minor ticks neither.}
  \item{sector.index}{Index for the sector.}
  \item{track.index}{Index for the track.}
  \item{labels.font}{Font style for the axis labels.}
  \item{labels.cex}{Font size for the axis labels.}
  \item{labels.direction}{Deprecated, use \code{facing} instead.}
  \item{labels.facing}{Facing of labels on axis, passing to \code{\link{circos.text}}}
  \item{labels.niceFacing}{Should facing of axis labels be human-easy.}
  \item{direction}{Whether the axis ticks point to the outside or inside of the circle.}
  \item{minor.ticks}{Number of minor ticks between two close major ticks.}
  \item{major.tick.length}{Length of the major ticks, measured in "current" data coordinate. \code{\link{convert_y}} can be used to convert an absolute unit to the data coordinate.}
  \item{major.tick.percentage}{Not used any more, please directly use \code{major.tick.length}.}
  \item{lwd}{Line width for ticks.}
  \item{col}{Color for the axes.}
  \item{labels.col}{Color for the labels.}
  \item{labels.pos.adjust}{Whether to adjust the positions of the first label and the last label so that the first label  align to its left and the last label align to its right if they exceed the range on axes. The value can be a vector of length two which correspond to the first label and the last label.}

}
\details{
It only draws axes on x-direction.
}
\seealso{
\code{\link{circos.yaxis}} draws axes on y-direction.

\url{https://jokergoo.github.io/circlize_book/book/graphics.html#axes}
}
\examples{
sectors = letters[1:8]
circos.par(points.overflow.warning = FALSE)
circos.initialize(sectors, xlim = c(0, 10))
circos.trackPlotRegion(sectors, ylim = c(0, 10), track.height = 0.1,
    bg.border = NA, panel.fun = function(x, y) {
        circos.text(5, 10, get.cell.meta.data("sector.index"))
})

circos.trackPlotRegion(sectors, ylim = c(0, 10))
circos.axis(sector.index = "a")
circos.axis(sector.index = "b", direction = "inside", labels.facing = "outside")
circos.axis(sector.index = "c", h = "bottom")
circos.axis(sector.index = "d", h = "bottom", direction = "inside",
    labels.facing = "reverse.clockwise")
circos.axis(sector.index = "e", h = 5, major.at = c(1, 3, 5, 7, 9))
circos.axis(sector.index = "f", h = 5, major.at = c(1, 3, 5, 7, 9),
    labels = c("a", "c", "e", "g", "f"), minor.ticks = 0)
circos.axis(sector.index = "g", h = 5, major.at = c(1, 3, 5, 7, 9),
    labels = c("a1", "c1", "e1", "g1", "f1"), major.tick = FALSE,
    labels.facing = "reverse.clockwise")
circos.axis(sector.index = "h", h = 2, major.at = c(1, 3, 5, 7, 9),
    labels = c("a1", "c1", "e1", "g1", "f1"), minor.ticks = 2, 
    major.tick.length = mm_y(5), labels.facing = "clockwise")
circos.clear()

if(FALSE) {

############### real-time clock #################
factors = letters[1]

circos.par("gap.degree" = 0, "cell.padding" = c(0, 0, 0, 0), "start.degree" = 90)
circos.initialize(sectors, xlim = c(0, 12))
circos.trackPlotRegion(sectors, ylim = c(0, 1), bg.border = NA)
circos.axis(sector.index = "a", major.at = 0:12, labels = "",
    direction = "inside", major.tick.length = mm_y(3))
circos.text(1:12, rep(0.5, 12), 1:12, facing = "downward")

while(1) {
    current.time = as.POSIXlt(Sys.time())
    sec = ceiling(current.time$sec)
    min = current.time$min
    hour = current.time$hour

    # erase the clock hands
    draw.sector(rou1 = 0.8, border = "white", col = "white")

    sec.degree = 90 - sec/60 * 360
    arrows(0, 0, cos(sec.degree/180*pi)*0.8, sin(sec.degree/180*pi)*0.8)

    min.degree = 90 - min/60 * 360
    arrows(0, 0, cos(min.degree/180*pi)*0.7, sin(min.degree/180*pi)*0.7, lwd = 2)

    hour.degree = 90 - hour/12 * 360 - min/60 * 360/12
    arrows(0, 0, cos(hour.degree/180*pi)*0.4, sin(hour.degree/180*pi)*0.4, lwd = 2)

    Sys.sleep(1)
}
circos.clear()
}
}
