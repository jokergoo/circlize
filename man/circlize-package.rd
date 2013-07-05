\name{circlize-package}
\alias{circlize-package}
\docType{package}
\title{
  circos layout in R


}
\description{
  circos layout in R


}
\details{
  This package aims to implement circos layout in R.

  Since most of the figures are composed of points, lines and polygon (for filled color), so we just need to implement functions for drawing points, lines and polygon.

  Current there are following functions that can be used for plotting: 

  \itemize{
    \item \code{\link{circos.points}}
    \item \code{\link{circos.lines}}
    \item \code{\link{circos.rect}}
    \item \code{\link{circos.polygon}}
    \item \code{\link{circos.text}}
    \item \code{\link{circos.axis}}
    \item \code{\link{circos.link}}, This maybe the unique feature for circos layout to represent relationships between elements.
  }
  For drawing points, lines and text through the whole track (among several sectors), the following functions are available:

  \itemize{
    \item \code{\link{circos.trackPoints}}
    \item \code{\link{circos.trackLines}}
    \item \code{\link{circos.trackText}}
  }
  Also, the function drawing histograms in the whole track is available:

  \itemize{
    \item \code{\link{circos.trackHist}}
  }
  Functions to arrange the circos layout:

  \itemize{
    \item \code{\link{circos.trackPlotRegion}}
    \item \code{\link{circos.updatePlotRegion}}
    \item \code{\link{circos.par}}
    \item \code{\link{circos.clear}}
  }
  Theoretically, you are able to draw most kinds of circos figures by the above functions.

  For specific use in genomics, a function which draws the ideogram and initializes sectors for chromosomes is supported: \code{\link{circos.initializeWithIdeogram}}.

  Refer to the vignettes to find out how to draw basic and advanced circos figure by this package.


}
\examples{
\dontrun{
set.seed(12345)
n = 10000
a = data.frame(factor = sample(letters[1:8], n, replace = TRUE),
    x = rnorm(n), y = runif(n))
library(circlize)
par(mar = c(1, 1, 1, 1), lwd = 0.1, cex = 0.7)
circos.par("default.track.height" = 0.1)
circos.initialize(factors = a$factor, x = a$x)
bgcol = rep(c("#EFEFEF", "#CCCCCC"), 4)
col = rep(c("#FF000010", "#00FF0010"), 4)
circos.trackPlotRegion(factors = a$factor, y = a$y,
    panel.fun = function(x, y) {
        circos.axis()
})
circos.trackPoints(a$factor, a$x, a$y, col = col,
    pch = 16, cex = 0.5)
circos.text(-1, 0.5, "left", sector.index = "a")
circos.text(1, 0.5, "right", sector.index = "a")
circos.trackHist(a$factor, a$x, bg.col = bgcol, col = NA)
circos.trackPlotRegion(factors = a$factor, x = a$x, y = a$y,
  panel.fun = function(x, y) {
      grey = c("#FFFFFF", "#CCCCCC", "#999999")
      i = get.cell.meta.data("sector.numeric.index")
      circos.updatePlotRegion(bg.col = grey[i \%\% 3 + 1])
      circos.points(x[1:10], y[1:10], col = "red", pch = 16, cex = 0.6)
      circos.points(x[11:20], y[11:20], col = "blue", cex = 0.6)
  })
circos.updatePlotRegion(sector.index = "d", track.index = 2)
circos.points(x = runif(100), y = runif(100))
circos.trackPlotRegion(factors = a$factor, y = a$y)
circos.trackLines(a$factor[1:100], a$x[1:100], a$y[1:100], type = "h")
circos.link("a", 0, "b", 0, top.ratio = 0.9)
circos.link("c", c(-0.5, 0.5), "d", c(-0.5,0.5), col = "red",
    border = "blue", top.ratio = 0.2)
circos.link("e", 0, "g", c(-1,1), col = "green", lwd = 2, lty = 2)
circos.clear()
}
}