\name{circos.arrow}
\alias{circos.arrow}
\title{
Draw arrow which is paralle to the circle
}
\description{
Draw arrow which is paralle to the circle
}
\usage{
circos.arrow(
    x1,
    x2,
    y = get.cell.meta.data("ycenter", sector.index, track.index),
    width = get.cell.meta.data("yrange", sector.index, track.index)/2,
    sector.index = get.current.sector.index(),
    track.index = get.current.track.index(),
    arrow.head.length = convert_x(5, "mm", sector.index, track.index),
    arrow.head.width = width*2,
    arrow.position = c("end", "start"),
    tail = c("normal", "point"),
    border = "black",
    col = "white",
    lty = par("lty"),
    ...)
}
\arguments{

  \item{x1}{start position of the arrow on the x-axis.}
  \item{x2}{end position of the arrow on the x-axis.}
  \item{y}{position of the arrow on the y-axis. Note this is the center of the arrow on y-axis.}
  \item{width}{width of the arrow body.}
  \item{sector.index}{index of the sector.}
  \item{track.index}{index of the track.}
  \item{arrow.head.length}{length of the arrow head. Note the value should be smaller than the length of the arrow itself (which is \code{x2 - x1}).}
  \item{arrow.head.width}{width of the arrow head.}
  \item{arrow.position}{where is the arrow head on the arrow.}
  \item{tail}{the shape of the arrow tail (the opposite side of arrow head).}
  \item{border}{border color of the arrow.}
  \item{col}{filled color of the arrow.}
  \item{lty}{line style of the arrow.}
  \item{...}{pass to \code{\link[graphics]{polygon}}.}

}
\details{
Note all position values are measured in the data coordinate (the coordinate in each cell).

If you see points overflow warnings, you can set \code{circos.par(points.overflow.warning = FALSE)} to turn it off.
}
\seealso{
\url{https://jokergoo.github.io/circlize_book/book/graphics.html#circular-arrows}
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
circos.initialize(letters[1:4], xlim = c(0, 1))
circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
  circos.arrow(0, 1, y = 0.5, width = 0.4, arrow.head.length = ux(1, "cm"), 
      col = "red", tail = ifelse(CELL_META$sector.index \%in\% c("a", "c"), 
          "point", "normal"))
}, bg.border = NA, track.height = 0.4)
circos.clear()

########## cell cycle ###########
cell_cycle = data.frame(phase = factor(c("G1", "S", "G2", "M"), 
                                    levels = c("G1", "S", "G2", "M")),
                        hour = c(11, 8, 4, 1))
color = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3")
circos.par(start.degree = 90)
circos.initialize(cell_cycle$phase, xlim = cbind(rep(0, 4), cell_cycle$hour))
circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
  circos.arrow(CELL_META$xlim[1], CELL_META$xlim[2], 
      arrow.head.width = CELL_META$yrange*0.8, arrow.head.length = ux(1, "cm"),
      col = color[CELL_META$sector.numeric.index])
  circos.text(CELL_META$xcenter, CELL_META$ycenter, CELL_META$sector.index, 
      facing = "downward")
}, bg.border = NA, track.height = 0.3)
circos.clear()
}
