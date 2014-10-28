\name{circos.trackHist}
\alias{circos.trackHist}
\title{
  Draw histogram in cells among a whole track  


}
\description{
  Draw histogram in cells among a whole track  


}
\usage{
circos.trackHist(factors, x, track.height = circos.par("track.height"),
    track.index = NULL, force.ylim = TRUE, col = ifelse(draw.density, "black", NA),
    border = "black", lty = par("lty"), lwd = par("lwd"),
    bg.col = NA, bg.border = "black", bg.lty = par("lty"), bg.lwd = par("lwd"),
    breaks = "Sturges", include.lowest = TRUE, right = TRUE, draw.density = FALSE)
}
\arguments{
  \item{factors}{Factors which represent the categories of data}
  \item{x}{Data on the x-axis}
  \item{track.index}{Index for the track which is going to be updated. Setting it to \code{NULL} means creating the plotting regions in the next newest track.}
  \item{track.height}{Height of the track. It is the percentage to the radius of the unit circle. If to update a track, this argument is disabled.}
  \item{force.ylim}{Whether to force all cells in the track to share the same \code{ylim}. Btw, \code{ylim} is calculated automatically.}
  \item{col}{Filled color for histogram}
  \item{border}{Border color for histogram}
  \item{lty}{Line style for histogram}
  \item{lwd}{Line width for histogram}
  \item{bg.col}{Background color for the plotting regions}
  \item{bg.border}{Color for the border of the plotting regions}
  \item{bg.lty}{Line style for the border of the plotting regions}
  \item{bg.lwd}{Line width for the border of the plotting regions}
  \item{breaks}{see \code{\link[graphics]{hist}}}
  \item{include.lowest}{see \code{\link[graphics]{hist}}}
  \item{right}{see \code{\link[graphics]{hist}}}
  \item{draw.density}{whether draw density lines instead of histogram bars.}

}
\details{
  It draw histogram in cells among a whole track. It is also an example to show how to add self-defined high-level graphics by this package. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
\examples{
\dontrun{
library(circlize)
par(mar = c(1, 1, 1, 1))
x = rnorm(2600)
factors = sample(letters, 2600, replace = TRUE)
circos.initialize(factors = factors, x = x)
circos.trackHist(factors = factors, x = x, track.height = 0.1,
    col = "#999999", border = "#999999")
circos.trackHist(factors = factors, x = x, force.ylim = FALSE, 
    track.height = 0.1, col = "#999999", border = "#999999")
circos.trackHist(factors = factors, x = x, draw.density = TRUE,
    track.height = 0.1, col = "#999999", border = "#999999")
circos.trackHist(factors = factors, x = x, draw.density = TRUE,
    force.ylim = FALSE, track.height = 0.1, col = "#999999", border = "#999999")

circos.clear()

}
}
