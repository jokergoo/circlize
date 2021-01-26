\name{circos.connect}
\alias{circos.connect}
\title{
Draw connecting lines/ribons between two sets of points
}
\description{
Draw connecting lines/ribons between two sets of points
}
\usage{
circos.connect(x0, y0, x1, y1,
    sector.index = get.current.sector.index(),
    track.index = get.current.track.index(),
    type = c("normal", "segments", "bezier"),
    segments.ratio = c(1, 1, 1),
    col = par("col"),
    border = "black",
    lwd = par("lwd"),
    lty = par("lty"),
    ...)
}
\arguments{

  \item{x0}{x coordinates for point set 1. The value can also be a two-column matrix.}
  \item{y0}{y coordinates for point set 1.}
  \item{x1}{x coordinates for point set 2. The value can also be a two-column matrix.}
  \item{y1}{y coordinates for point set 2.}
  \item{sector.index}{Index for the sector.}
  \item{track.index}{Index for the track.}
  \item{type}{Which type of connections. Values can be "normal", "segments" and "bezier".}
  \item{segments.ratio}{When \code{type} is set to \code{segments}, each connecting line is segmented into three parts. This argument controls the length of the three parts of sub-segments.}
  \item{col}{Color of the segments.}
  \item{border}{Border color of the links.}
  \item{lwd}{Line width of the segments.}
  \item{lty}{Line type of the segments.}
  \item{...}{Other arguments.}

}
\examples{
\donttest{
circos.initialize(c("a"), xlim = c(0, 1))
circos.track(ylim = c(0, 1), track.height = 0.7, bg.border = NA, 
    panel.fun = function(x, y) {
    circos.lines(CELL_META$cell.xlim, rep(CELL_META$cell.ylim[1], 2), col = "#CCCCCC")
    circos.lines(CELL_META$cell.xlim, rep(CELL_META$cell.ylim[2], 2), col = "#CCCCCC")
    x0 = runif(100)
    x1 = runif(100)

circos.connect(x0, 0, x1, 1, 
        type = "normal", border = NA,
        col = rand_color(100, luminosity = "bright", transparency = 0.75))
})

circos.initialize(c("a"), xlim = c(0, 1))
circos.track(ylim = c(0, 1), track.height = 0.7, bg.border = NA, 
    panel.fun = function(x, y) {
    circos.lines(CELL_META$cell.xlim, rep(CELL_META$cell.ylim[1], 2), col = "#CCCCCC")
    circos.lines(CELL_META$cell.xlim, rep(CELL_META$cell.ylim[2], 2), col = "#CCCCCC")
    x0 = runif(100)
    x1 = runif(100)

circos.connect(x0, 0, x1, 1, 
        type = "bezier", border = NA,
        col = rand_color(100, luminosity = "bright", transparency = 0.75))
})

circos.initialize(c("a"), xlim = c(0, 1))
circos.track(ylim = c(0, 1), track.height = 0.7, bg.border = NA, 
    panel.fun = function(x, y) {
    circos.lines(CELL_META$cell.xlim, rep(CELL_META$cell.ylim[1], 2), col = "#CCCCCC")
    circos.lines(CELL_META$cell.xlim, rep(CELL_META$cell.ylim[2], 2), col = "#CCCCCC")
    x0 = sort(runif(200))
    x0 = matrix(x0, ncol = 2, byrow = TRUE)
    x1 = sort(runif(200))
    x1 = matrix(x1, ncol = 2, byrow = TRUE)

circos.connect(x0, 0, x1, 1, 
        type = "normal", border = NA,
        col = rand_color(100, luminosity = "bright", transparency = 0.5))
})

circos.initialize(c("a"), xlim = c(0, 1))
circos.track(ylim = c(0, 1), track.height = 0.7, bg.border = NA, 
    panel.fun = function(x, y) {
    circos.lines(CELL_META$cell.xlim, rep(CELL_META$cell.ylim[1], 2), col = "#CCCCCC")
    circos.lines(CELL_META$cell.xlim, rep(CELL_META$cell.ylim[2], 2), col = "#CCCCCC")
    x0 = sort(runif(500))
    x0 = matrix(x0, ncol = 2, byrow = TRUE)
    x0 = x0[sample(nrow(x0), nrow(x0)), ]
    x1 = sort(runif(500))
    x1 = matrix(x1, ncol = 2, byrow = TRUE)
    x1 = x1[sample(nrow(x1), nrow(x1)), ]

l = abs(x0[, 1] - x1[, 1]) < 0.5

circos.connect(x0[l ,], 0, x1[l, ], 1, 
        type = "bezier", border = NA,
        col = rand_color(sum(l), luminosity = "bright", transparency = 0.5))
})
}
}
