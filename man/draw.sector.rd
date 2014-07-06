\name{draw.sector}
\alias{draw.sector}
\title{
  Draw sectors or rings in a circle  


}
\description{
  Draw sectors or rings in a circle  


}
\usage{
draw.sector(start.degree = 0, end.degree = 360, rou1 = 1, rou2 = NULL,
    center = c(0, 0), col = NA, border = "black", lwd = par("lwd"), lty = par("lty"))
}
\arguments{
  \item{start.degree}{start degree for the sector}
  \item{end.degree}{end degree for the sector}
  \item{rou1}{Radius for one of the arc in the sector}
  \item{rou2}{Radius for the other arc in the sector}
  \item{center}{Center of the circle}
  \item{col}{Filled color}
  \item{border}{Border color}
  \item{lwd}{Line width}
  \item{lty}{Line style}

}
\details{
  If the interval between \code{start} and \code{end} (larger or equal to 360 or smaller or equal to -360) it would draw a full circle or ring. If \code{rou2} is set, it would draw part of a ring. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
