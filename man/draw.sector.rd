\name{draw.sector}
\alias{draw.sector}
\title{
  Draw sectors in a circle


}
\description{
  Draw sectors in a circle


}
\usage{
draw.sector(center = c(0, 0), start.degree=0, end.degree=360, rou1 = 1,
    rou2 = NULL, col=NA, border = "black")
}
\arguments{
  \item{center}{Center of the circle}
  \item{start.degree}{start degree for the sector}
  \item{end.degree}{end degree for the sector}
  \item{rou1}{Radius for one of the arc in the sector}
  \item{rou2}{Radius for the other arc in the sector}
  \item{col}{Filled color}
  \item{border}{Border color}

}
\details{
  If the interval between \code{start} and \code{end} (larger or equal to 360 or smaller or equal to -360)it would draw a full circle or ring. If \code{rou2} is set, it would draw part of a ring.


}
\examples{
library(circlize)
par(mar = c(1,1,1,1))
plot(0, 1, xlim = c(-1, 1), ylim = c(-1, 1), type = "n")
draw.sector(c(0, 0), start = 0, end = 360, rou1 = 1, col = NA, border = "black")
draw.sector(c(0, 0), start = 30, end = 60, rou1 = 0.8, rou2 = 0.5,
    col = "red", border = "black")
draw.sector(c(0, 0), start = 0, end = 400, rou1 = 0.4, rou2 = 0.3,
    col = "orange", border = "black")
draw.sector(c(0, 0), start = 0, end = -400, rou1 = 0.2,
    col = "green", border = "black")
draw.sector(c(0, 0), start = 120, end = 200, rou1 = 0.9,
    col = "#FF000040", border = "black")
}