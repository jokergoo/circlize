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
    track.index = get.cell.meta.data("track.index"),
    col = NA, border = "black", lty = par("lty"), lwd = par("lwd"))
}
\arguments{
  \item{xleft}{x for the left bottom points}
  \item{ybottom}{y for the left bottom points}
  \item{xright}{x for the right top points}
  \item{ytop}{y for the right top points}
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}
  \item{col}{filled color}
  \item{border}{color for the border}
  \item{lty}{line style for the border}
  \item{lwd}{line width for the border}

}
\details{
  Currently, \code{xleft}, \code{ybottom}, \code{xright}, \code{ytop} are all single values, which means you can only draw one rectangle at once. The name for this function is \code{\link{circos.rect}} because if you imagine the plotting region as Cartesian coordinate, then it is rectangle. in the polar coordinate, the up and bottom edge become two arcs.  

  You just need to specify the coordinates of two diagonal points just similar as  \code{\link[graphics]{rect}} does. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
