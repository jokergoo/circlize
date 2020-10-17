\name{circos.barplot}
\alias{circos.barplot}
\title{
Draw barplots
}
\description{
Draw barplots
}
\usage{
circos.barplot(value, pos, bar_width = 0.6,
    col = NA, border = "black", lwd = par("lwd"), lty = par("lty"),
    sector.index = get.current.sector.index(),
    track.index = get.current.track.index())
}
\arguments{

  \item{value}{A numeric vector or a matrix. If it is a matrix, columns correspond to the height of bars.}
  \item{pos}{Positions of the bars.}
  \item{bar_width}{Width of bars. It assumes the bars locating at \code{x = 1, 2, ...}.}
  \item{col}{Filled color of bars.}
  \item{border}{Color for the border.}
  \item{lwd}{Line width.}
  \item{lty}{Line style.}
  \item{sector.index}{Index of sector.}
  \item{track.index}{Index of track.}

}
\details{
If the input variable is a matrix, it draws a stacked barplot.

Please note, the x-values of barplots are normally integer indices. Just be careful
when initializing the circular layout.
}
\examples{
circos.initialize(letters[1:4], xlim = c(0, 10))
circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
    value = runif(10)
    circos.barplot(value, 1:10 - 0.5, col = 1:10)
})
circos.track(ylim = c(-1, 1), panel.fun = function(x, y) {
    value = runif(10, min = -1, max = 1)
    circos.barplot(value, 1:10 - 0.5, col = ifelse(value > 0, 2, 3))
})
circos.clear()

circos.initialize(letters[1:4], xlim = c(0, 10))
circos.track(ylim = c(0, 4), panel.fun = function(x, y) {
    value = matrix(runif(10*4), ncol = 4)
    circos.barplot(value, 1:10 - 0.5, col = 2:5)
})
circos.clear()
}
