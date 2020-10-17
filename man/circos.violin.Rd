\name{circos.violin}
\alias{circos.violin}
\title{
Draw violin plots
}
\description{
Draw violin plots
}
\usage{
circos.violin(value, pos, violin_width = 0.8,
    col = NA, border = "black", lwd = par("lwd"), lty = par("lty"),
    show_quantile = TRUE, pt.col = par("col"), cex = par("cex"), pch = 16,
    max_density = NULL, sector.index = get.current.sector.index(),
    track.index = get.current.track.index())
}
\arguments{

  \item{value}{A numeric vector, a matrix or a list. If it is a matrix, boxplots are made by columns.}
  \item{pos}{Positions of the boxes.}
  \item{violin_width}{Width of violins.}
  \item{col}{Filled color of boxes.}
  \item{border}{Color for the border as well as the quantile lines.}
  \item{lwd}{Line width.}
  \item{lty}{Line style}
  \item{show_quantile}{Whether to show the quantile lines.}
  \item{cex}{Point size.}
  \item{pch}{Point type.}
  \item{pt.col}{Point color}
  \item{max_density}{The maximal density value across several violins. It is used to compare between violins.}
  \item{sector.index}{Index of sector.}
  \item{track.index}{Index of track.}

}
\examples{
\donttest{
circos.initialize(letters[1:4], xlim = c(0, 10))
circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
    for(pos in seq(0.5, 9.5, by = 1)) {
        value = runif(10)
        circos.violin(value, pos)
    }
})
circos.clear()

circos.initialize(letters[1:4], xlim = c(0, 10))
circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
    value = replicate(runif(10), n = 10, simplify = FALSE)
    circos.violin(value, 1:10 - 0.5, col = 1:10)
})
circos.clear()
}
}
