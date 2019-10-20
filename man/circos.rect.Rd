\name{circos.rect}
\alias{circos.rect}
\title{
Draw rectangle-like grid
}
\description{
Draw rectangle-like grid
}
\usage{
circos.rect(
    xleft, ybottom, xright, ytop,
    sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"),
    rot = 0,
    ...)
}
\arguments{

  \item{xleft}{x for the left bottom points}
  \item{ybottom}{y for the left bottom points}
  \item{xright}{x for the right top points}
  \item{ytop}{y for the right top points}
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}
  \item{rot}{Rotation of the rectangles. The value is measured clockwise in degree. Rotation is relative to the center of the rectangles.}
  \item{...}{pass to \code{\link[graphics]{polygon}}}

}
\details{
The name for this function is \code{\link{circos.rect}}
because if you imagine the plotting region as Cartesian coordinate, then it is rectangle.
in the polar coordinate, the up and bottom edge become two arcs.

This function can be vectorized.
}
\examples{
circos.initialize(fa = c("a", "b", "c", "d"), xlim = c(0, 10))
circos.track(ylim = c(0, 10), panel.fun = function(x, y) {
    for(rot in seq(0, 360, by = 30)) {
        circos.rect(2, 2, 6, 6, rot = rot)
    }
}, track.height = 0.5)
}
