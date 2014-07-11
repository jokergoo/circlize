\name{circos.trackPoints}
\alias{circos.trackPoints}
\title{
  Add points to the plotting regions in a same track  


}
\description{
  Add points to the plotting regions in a same track  


}
\usage{
circos.trackPoints(factors = NULL, x, y, track.index = get.cell.meta.data("track.index"),
    pch = par("pch"), col = par("col"), cex = par("cex"))
}
\arguments{
  \item{factors}{Factors which represent the categories of data}
  \item{x}{Data points on x-axis}
  \item{y}{Data points on y-axis}
  \item{track.index}{Index for the track}
  \item{pch}{Points type}
  \item{col}{Points color}
  \item{cex}{Points size}

}
\details{
  The function adds points in multiple cells by first splitting data into several parts in which each part corresponds to one factor (sector index) and then add points in cells corresponding to the part of data by calling \code{\link{circos.points}}.  

  Length of \code{pch}, \code{col} and \code{cex} can be one, length of levels of the factors and length of  factors. All length will be recycled to the length of factors respectively.  

  This function can be replaced by a \code{for} loop containing \code{\link{circos.points}}. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
