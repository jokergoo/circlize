\name{circos.trackText}
\alias{circos.trackText}
\title{
  Draw text in cells among the whole track  


}
\description{
  Draw text in cells among the whole track  


}
\usage{
circos.trackText(factors, x, y, labels, track.index = get.cell.meta.data("track.index"),
    direction = NULL, facing = c("inside", "outside", "reverse.clockwise", "clockwise",
    "downward", "bending"), niceFacing = FALSE, adj = par("adj"), cex = 1, col = "black",
    font = par("font"))
}
\arguments{
  \item{factors}{Factors which represent the categories of data}
  \item{x}{Data points on x-axis}
  \item{y}{Data points on y-axis}
  \item{labels}{Labels}
  \item{track.index}{Index for the track}
  \item{direction}{deprecated, use \code{facing} instead.}
  \item{facing}{Facing of text}
  \item{niceFacing}{Should the facing of text be adjusted to fit human eyes?}
  \item{adj}{Adjustment for text}
  \item{cex}{Font size}
  \item{col}{Font color}
  \item{font}{Font style}

}
\details{
  The function adds texts in multiple cells by first splitting data into several parts in which each part corresponds to one factor (sector index) and then add texts in cells by calling \code{\link{circos.text}}.  

  This function can be replaced by a \code{for} loop containing \code{\link{circos.text}}. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
