\name{draw.sector}
\alias{draw.sector}
\title{
  Draw sectors in a circle


}
\description{
  Draw sectors in a circle


}
\usage{
draw.sector(center = c(0, 0), start.degree=0, end.degree=360, rou1 = 1, rou2 = NULL, col=NA, border = "black")
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
