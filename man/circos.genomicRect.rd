\name{circos.genomicRect}
\alias{circos.genomicRect}
\title{
  Draw rectangle-like grid  


}
\description{
  Draw rectangle-like grid  


}
\usage{
circos.genomicRect(region, value,
    ytop = NULL, ybottom = NULL, ytop.column = NULL, ybottom.column = NULL,
    sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"), posTransform = NULL,
    col = NA, border = "black", lty = par("lty"), lwd = par("lwd"), ...)
}
\arguments{
  \item{region}{a data frame contains 2 column}
  \item{value}{a data frame contains values and other stuff}
  \item{ytop}{a vector of ytop}
  \item{ybottom}{a vector of ybottom}
  \item{ytop.column}{if \code{ytop} is in \code{value}, the index of the column}
  \item{ybottom.column}{if \code{\link{ybottom\code{ is in }value}}`, the index of the column}
  \item{sector.index}{pass to \code{\link{circos.rect}}}
  \item{track.index}{pass to \code{\link{circos.rect}}}
  \item{posTransform}{whether to do position transformation}
  \item{col}{pass to \code{\link{circos.rect}}}
  \item{border}{pass to \code{\link{circos.rect}}}
  \item{lty}{pass to \code{\link{circos.rect}}}
  \item{lwd}{pass to \code{\link{circos.rect}}}
  \item{...}{mysterious parameters}

}
\details{
  sth 


}
