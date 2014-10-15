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
    "downward", "bending"), niceFacing = FALSE, adj = par("adj"), cex = 1, col = "black",
    font = par("font"), ...)
}
\arguments{
  \item{x}{Data points on x-axis}
  \item{y}{Data points on y-axis}
  \item{labels}{Labels for each points}
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}
  \item{direction}{deprecated, use \code{facing} instead.}
  \item{facing}{Facing of text. Please refer to vignette for different settings }
  \item{niceFacing}{Should the facing of text be adjusted to fit human eyes?}
  \item{adj}{Adjustment for text}
  \item{cex}{Font size}
  \item{col}{Font color}
  \item{font}{Font style}
  \item{...}{Pass to \code{\link[graphics]{text}}}

}
\details{
  The function is similar to \code{\link[graphics]{text}}. All you need to note is the \code{facing} settings. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
\examples{
\dontrun{
library(circlize)
par(mar = c(1, 1, 1, 1), mfrow = c(2, 1))
factors = letters[1:4]
circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.5,
    panel.fun = function(x, y) {
    circos.text(3, 9, "inside", facing = "inside", cex = 0.8)
    circos.text(7, 9, "outside", facing = "outside", cex = 0.8)
    circos.text(0, 5, "reverse.clockwise", facing = "reverse.clockwise", 
        adj = c(0.5, 0), cex = 0.8)
    circos.text(10, 5, "clockwise", facing = "clockwise", adj = c(0.5, 0), cex = 0.8)
    circos.text(5, 5, "downward", facing = "downward", cex = 0.8)
    circos.text(5, 1, "bending", facing = "bending", cex = 0.8)
})
circos.clear()

factors = LETTERS[1:20]
circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 1))
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), track.height = 0.5, 
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        ylim = get.cell.meta.data("ylim")
        theta = mean(get.cell.meta.data("xplot")) \%\% 360
        sector.index = get.cell.meta.data("sector.index")
        if(theta < 90 || theta > 270) {
            text.facing = "clockwise"
            text.adj = c(0, 0.5)
        } else {
            text.facing = "reverse.clockwise"
            text.adj = c(1, 0.5)
        }
        circos.text(mean(xlim), ylim[1],
            labels = paste(rep(sector.index, 8), collapse = ""),
            facing = text.facing, adj = text.adj, cex = 0.8)
})
circos.clear()

}
}
