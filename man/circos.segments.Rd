\name{circos.segments}
\alias{circos.segments}
\title{
Draw segments through pairwise of points
}
\description{
Draw segments through pairwise of points
}
\usage{
circos.segments(
    x0, y0, x1, y1,
    sector.index = get.current.sector.index(),
    track.index = get.current.track.index(),
    straight = FALSE,
    col = par("col"),
    lwd = par("lwd"),
    lty = par("lty"),
    ...)
}
\arguments{

  \item{x0}{x coordinates for starting points.}
  \item{y0}{y coordinates for ending points.}
  \item{x1}{x coordinates for starting points.}
  \item{y1}{y coordinates for ending points.}
  \item{sector.index}{Index for the sector.}
  \item{track.index}{Index for the track.}
  \item{straight}{Whether the segment is a straight line.}
  \item{col}{Color of the segments.}
  \item{lwd}{Line width of the segments.}
  \item{lty}{Line type of the segments.}
  \item{...}{Pass to \code{\link[graphics]{lines}}.}

}
\examples{
circos.initialize(letters[1:8], xlim = c(0, 1))
circos.track(ylim = c(0, 1), track.height = 0.3, panel.fun = function(x, y) {
    x = seq(0.2, 0.8, by = 0.2)
    y = seq(0.2, 0.8, by = 0.2)

    circos.segments(x, 0.1, x, 0.9)
    circos.segments(0.1, y, 0.9, y)
})
circos.clear()
}
