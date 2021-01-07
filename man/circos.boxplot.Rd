\name{circos.boxplot}
\alias{circos.boxplot}
\title{
Draw boxplots
}
\description{
Draw boxplots
}
\usage{
circos.boxplot(value, pos, outline = TRUE, box_width = 0.6,
    col = NA, border = "black", lwd = par("lwd"), lty = par("lty"),
    cex = par("cex"), pch = 1, pt.col = par("col"),
    sector.index = get.current.sector.index(),
    track.index = get.current.track.index())
}
\arguments{

  \item{value}{A numeric vector, a matrix or a list. If it is a matrix, boxplots are made by columns (each column is a box).}
  \item{pos}{Positions of the boxes.}
  \item{outline}{Whether to draw outliers.}
  \item{box_width}{Width of boxes.}
  \item{col}{Filled color of boxes.}
  \item{border}{Color for the border as well as the quantile lines.}
  \item{lwd}{Line width.}
  \item{lty}{Line style}
  \item{cex}{Point size.}
  \item{pch}{Point type.}
  \item{pt.col}{Point color.}
  \item{sector.index}{Index of sector.}
  \item{track.index}{Index of track.}

}
\details{
Please note, the x-values of boxplots are normally integer indices. Just be careful
when initializing the circular layout.
}
\examples{
circos.initialize(letters[1:4], xlim = c(0, 10))
circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
    for(pos in seq(0.5, 9.5, by = 1)) {
        value = runif(10)
        circos.boxplot(value, pos)
    }
})
circos.clear()

circos.initialize(letters[1:4], xlim = c(0, 10))
circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
    value = replicate(runif(10), n = 10, simplify = FALSE)
    circos.boxplot(value, 1:10 - 0.5, col = 1:10)
})
circos.clear()
}
