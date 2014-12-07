\name{circos.polygon}
\alias{circos.polygon}
\title{
  Draw polygon  


}
\description{
  Draw polygon  


}
\usage{
circos.polygon(x, y, sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"),
    col = NA, border = "black", lty = par("lty"), lwd = par("lwd"))
}
\arguments{
  \item{x}{Data points on x-axis}
  \item{y}{Data points on y-axis}
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}
  \item{col}{filled color}
  \item{border}{color for the border}
  \item{lty}{line style for the border}
  \item{lwd}{line width for the border}

}
\details{
  similar as \code{\link[graphics]{polygon}} 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
\examples{
\dontrun{
library(circlize)
set.seed(123)
par(mar = c(1, 1, 1, 1))
factors = letters[1:4]
circos.initialize(factors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(-3, 3), track.height = 0.4, panel.fun = function(x, y) {
    x1 = runif(20)
    y1 = x1 + rnorm(20)
    or = order(x1)
    x1 = x1[or]
    y1 = y1[or]
    loess.fit = loess(y1 ~ x1)
    loess.predict = predict(loess.fit, x1, se = TRUE)
    d1 = c(x1, rev(x1))
    d2 = c(loess.predict$fit + loess.predict$se.fit,
        rev(loess.predict$fit - loess.predict$se.fit))
    circos.polygon(d1, d2, col = "#CCCCCC", border = NA)
    circos.points(x1, y1, cex = 0.5)
    circos.lines(x1, loess.predict$fit)
})
circos.clear()
}
}
