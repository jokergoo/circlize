\name{draw.sector}
\alias{draw.sector}
\title{
  Draw sectors or rings in a circle


}
\description{
  Draw sectors or rings in a circle


}
\usage{
draw.sector(center = c(0, 0), start.degree = 0, end.degree = 360, rou1 = 1,
    rou2 = NULL, col = NA, border = "black", lwd = par("lwd"), lty = par("lty"))
}
\arguments{
  \item{center}{Center of the circle}
  \item{start.degree}{start degree for the sector}
  \item{end.degree}{end degree for the sector}
  \item{rou1}{Radius for one of the arc in the sector}
  \item{rou2}{Radius for the other arc in the sector}
  \item{col}{Filled color}
  \item{border}{Border color}
  \item{lwd}{Line width}
  \item{lty}{Line style}

}
\details{
  If the interval between \code{start} and \code{end} (larger or equal to 360 or smaller or equal to -360)it would draw a full circle or ring. If \code{rou2} is set, it would draw part of a ring.


}
\examples{
library(circlize)

factors = letters[1:8]

par(mar = c(1, 1, 1, 1))
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1))
circos.trackPlotRegion(ylim = c(0, 1))

xplot.a1 = get.cell.meta.data("xplot", "a", 1)
yplot.a1 = get.cell.meta.data("yplot", "a", 1)
draw.sector(start.degree = xplot.a1[1], end.degree = xplot.a1[2],
    rou1 = yplot.a1[2], border = NA, col = "#FF000040")

xplot.b2 = get.cell.meta.data("xplot", "b", 2)
yplot.b2 = get.cell.meta.data("yplot", "b", 2)
draw.sector(start.degree = xplot.b2[1], end.degree = xplot.b2[2],
    rou1 = yplot.b2[2], border = NA, col = "#FF00FF40")

draw.sector(start.degree = 0, end.degree = 360, rou1 = yplot.a1[2],
    rou2 = yplot.a1[1], border = NA, col = "#00FF0040")

xplot.c2 = get.cell.meta.data("xplot", "c", 2)
yplot.c2 = get.cell.meta.data("yplot", "c", 2)
xplot.d2 = get.cell.meta.data("xplot", "d", 3)
yplot.d2 = get.cell.meta.data("yplot", "d", 3)
draw.sector(start.degree = xplot.c2[1], end.degree = xplot.d2[2],
    rou1 = yplot.c2[2], rou2 = yplot.c2[1], border = NA, col = "#0000FF40")

xplot.g2 = get.cell.meta.data("xplot", "g", 2)
yplot.g2 = get.cell.meta.data("yplot", "g", 2)
xplot.g3 = get.cell.meta.data("xplot", "g", 3)
yplot.g3 = get.cell.meta.data("yplot", "g", 3)
draw.sector(start.degree = xplot.g2[1], end.degree = xplot.g2[2],
    rou1 = yplot.g2[2], rou2 = yplot.g3[1], border = NA, col = "#00FFFF40")

xplot.e2 = get.cell.meta.data("xplot", "e", 2)
yplot.e2 = get.cell.meta.data("yplot", "e", 2)
xplot.e3 = get.cell.meta.data("xplot", "e", 3)
yplot.e3 = get.cell.meta.data("yplot", "e", 3)
xplot.f2 = get.cell.meta.data("xplot", "f", 2)
yplot.f2 = get.cell.meta.data("yplot", "f", 2)
xplot.f3 = get.cell.meta.data("xplot", "f", 3)
yplot.f3 = get.cell.meta.data("yplot", "f", 3)
draw.sector(start.degree = xplot.e2[1], end.degree = xplot.f2[2],
    rou1 = yplot.e2[2], rou2 = yplot.e3[1], border = NA, col = "#FFFF0040")
show.index()
circos.clear()
}