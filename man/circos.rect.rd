\name{circos.rect}
\alias{circos.rect}
\title{
Draw rectangle-like grid
}
\description{
Draw rectangle-like grid
}
\usage{
circos.rect(xleft, ybottom, xright, ytop,
    sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"), ...)
}
\arguments{

  \item{xleft}{x for the left bottom points}
  \item{ybottom}{y for the left bottom points}
  \item{xright}{x for the right top points}
  \item{ytop}{y for the right top points}
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}
  \item{...}{pass to \code{\link[graphics]{polygon}}}

}
\details{
The name for this function is \code{\link{circos.rect}}
because if you imagine the plotting region as Cartesian coordinate, then it is rectangle.
in the polar coordinate, the up and bottom edge become two arcs.

This function can be vectorized.
}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.

}
\examples{
# There is no example
NULL

}
