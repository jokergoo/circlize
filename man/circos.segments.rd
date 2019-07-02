\name{circos.segments}
\alias{circos.segments}
\title{
Draw segments through pairwise of points
}
\description{
Draw segments through pairwise of points
}
\usage{
circos.segments(x0, y0, x1, y1, sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"), straight = FALSE,
    col = par("col"), lwd = par("lwd"), lty = par("lty"), ...)
}
\arguments{

  \item{x0}{x coordinates for starting points}
  \item{y0}{y coordinates for ending points}
  \item{x1}{x coordinates for starting points}
  \item{y1}{y coordinates for ending points}
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}
  \item{straight}{whether the segment is a straight line}
  \item{col}{color of the segments}
  \item{lwd}{line width of the segments}
  \item{lty}{line type of the segments}
  \item{...}{pass to \code{\link[graphics]{lines}}}

}
\examples{
# There is no example
NULL

}
