\name{circos.polygon}
\alias{circos.polygon}
\title{
Draw polygon
}
\description{
Draw polygon
}
\usage{
circos.polygon(x, y, sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"), ...)
}
\arguments{

  \item{x}{Data points on x-axis}
  \item{y}{Data points on y-axis}
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}
  \item{...}{pass to \code{\link[graphics]{polygon}}}

}
\details{
similar as \code{\link[graphics]{polygon}}.

Note: start point should overlap with the end point,
}
\examples{
# There is no example
NULL
}
