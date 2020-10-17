\name{circos.trackHist}
\alias{circos.trackHist}
\title{
Draw histogram in cells among a whole track
}
\description{
Draw histogram in cells among a whole track
}
\usage{
circos.trackHist(
    sectors,
    x,
    track.height = circos.par("track.height"),
    track.index = NULL,
    ylim = NULL,
    force.ylim = TRUE,
    col = ifelse(draw.density, "black", NA),
    border = "black",
    lty = par("lty"),
    lwd = par("lwd"),
    bg.col = NA,
    bg.border = "black",
    bg.lty = par("lty"),
    bg.lwd = par("lwd"),
    breaks = "Sturges",
    include.lowest = TRUE,
    right = TRUE,
    draw.density = FALSE,
    bin.size = NULL,
    area = FALSE,
    factors = sectors)
}
\arguments{

  \item{sectors}{A \code{\link{factor}} or a character vector which represents the categories of data}
  \item{factors}{The same as \code{sectors}. It will be removed in future versions. }
  \item{x}{Data on the x-axis}
  \item{track.index}{Index for the track which is going to be updated. Setting it to \code{NULL} means creating the plotting regions in the next newest track.}
  \item{track.height}{Height of the track. It is the percentage to the radius of the unit circle. If to update a track, this argument is disabled.}
  \item{ylim}{Ranges on y-direction. By default, \code{ylim} is calculated automatically.}
  \item{force.ylim}{Whether to force all cells in the track to share the same \code{ylim}.}
  \item{col}{Filled color for histogram}
  \item{border}{Border color for histogram}
  \item{lty}{Line style for histogram}
  \item{lwd}{Line width for histogram}
  \item{bg.col}{Background color for the plotting regions}
  \item{bg.border}{Color for the border of the plotting regions}
  \item{bg.lty}{Line style for the border of the plotting regions}
  \item{bg.lwd}{Line width for the border of the plotting regions}
  \item{breaks}{see \code{\link[graphics]{hist}}}
  \item{include.lowest}{see \code{\link[graphics]{hist}}}
  \item{right}{see \code{\link[graphics]{hist}}}
  \item{draw.density}{whether draw density lines instead of histogram bars.}
  \item{area}{whether to fill the area below the density lines. If it is set to \code{TRUE}, \code{col} controls the filled color in the area and \code{border} controls color of the line.}
  \item{bin.size}{size of the bins of the histogram}

}
\details{
It draw histogram in cells among a whole track. It is also an example to show how to add self-defined
high-level graphics by this package.
}
\seealso{
\url{https://jokergoo.github.io/circlize_book/book/high-level-plots.html#histograms}
}
\examples{
\donttest{
x = rnorm(1600)
sectors = sample(letters[1:16], 1600, replace = TRUE)
circos.initialize(sectors, x = x)
circos.trackHist(sectors, x = x, col = "#999999",
  border = "#999999")
circos.trackHist(sectors, x = x, bin.size = 0.1,
  col = "#999999", border = "#999999")
circos.trackHist(sectors, x = x, draw.density = TRUE,
  col = "#999999", border = "#999999")
circos.clear()
}
}
