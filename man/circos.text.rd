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
    "downward", "bending", "bending.inside", "bending.outside"), niceFacing = FALSE,
    adj = par("adj"), cex = 1, col = par("col"), font = par("font"), ...)
}
\arguments{

  \item{x}{Data points on x-axis}
  \item{y}{Data points on y-axis}
  \item{labels}{Labels for each points}
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}
  \item{direction}{deprecated, use \code{facing} instead.}
  \item{facing}{Facing of text. Please refer to vignette for different settings}
  \item{niceFacing}{Should the facing of text be adjusted to fit human eyes?}
  \item{adj}{offset for text. By default the text position adjustment is either horizontal or vertical in the canvas coordinate system. The "circular horizontal" offset can be set as a value in degree unit and the value should be wrapped by \code{\link{degree}}.}
  \item{...}{Pass to \code{\link[graphics]{text}}}
  \item{cex}{Font size}
  \item{col}{Font color}
  \item{font}{Font style}

}
\details{
The function is similar to \code{\link[graphics]{text}}. All you need to note is the \code{facing} settings.
}
\seealso{
\url{http://jokergoo.github.io/circlize_book/book/graphics.html#text}
}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.

}
\examples{
factors = letters[1:4]
circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), 
  track.height = 0.5, panel.fun = function(x, y) {
    circos.text(3, 1, "inside", facing = "inside", cex = 0.8)
    circos.text(7, 1, "outside", facing = "outside", cex = 0.8)
    circos.text(0, 5, "reverse.clockwise", facing = "reverse.clockwise", 
        adj = c(0.5, 0), cex = 0.8)
    circos.text(10, 5, "clockwise", facing = "clockwise", adj = c(0.5, 0), 
        cex = 0.8)
    circos.text(5, 5, "downward", facing = "downward", cex = 0.8)
    circos.text(3, 9, "====bending.inside====", facing = "bending.inside", 
        cex = 0.8)
    circos.text(7, 9, "====bending.outside====", facing = "bending.outside", 
        cex = 0.8)
})
circos.clear()

}
