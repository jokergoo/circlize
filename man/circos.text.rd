\name{circos.text}
\alias{circos.text}
\title{
  Draw text in a cell  


}
\description{
  Draw text in a cell  


}
\usage{
circos.text(x, y, labels, sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"), direction = NULL,
    facing = c("inside", "outside", "reverse.clockwise", "clockwise",
    "downward", "bending"), adj = par("adj"), cex = 1, col = "black",
    font = par("font"), ...)
}
\arguments{
  \item{x}{Data points on x-axis}
  \item{y}{Data points on y-axis}
  \item{labels}{Labels for each points}
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}
  \item{direction}{deprecated, use \code{facing} instead.}
  \item{facing}{Facing of text}
  \item{adj}{Adjustment for text}
  \item{cex}{Font size}
  \item{col}{Font color}
  \item{font}{Font style}
  \item{...}{Pass to \code{\link[graphics]{text}}}

}
\details{
  The function is similar to \code{\link[graphics]{text}}. All you need to note is the \code{direction} settings. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
