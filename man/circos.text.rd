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
    track.index = get.cell.meta.data("track.index"), direction = c("default",
    "default2", "vertical_left", "vertical_right", "horizontal", "arc"),
    adj = par("adj"), cex = 1, col = "black", font = par("font"), ...)
}
\arguments{
  \item{x}{Data points on x-axis}
  \item{y}{Data points on y-axis}
  \item{labels}{Labels for each points}
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}
  \item{direction}{Direction of the text, should be one of (\code{default}, \code{default2}, \code{vertical_left}, \code{vertical_right}, \code{horizontal}, \code{arc}). How to choose text direction can be found in the vignette.}
  \item{adj}{Adjustment for texts}
  \item{cex}{Font size}
  \item{col}{Font color}
  \item{font}{Font style}
  \item{...}{Pass to \code{\link[graphics]{text}}}

}
\details{
  The function is similar to \code{\link[graphics]{text}}. All you need to note is the \code{direction} settings. 


}
\examples{
library(circlize)
par(mar = c(1, 1, 1, 1))
factors = letters[1:4]
circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.5,
    panel.fun = function(x, y) {
        circos.text(3, 9, "default", direction = "default")
        circos.text(7, 9, "default2", direction = "default2")
        circos.text(0, 5, "vertical_left", direction = "vertical_left")
        circos.text(10, 5, "vertical_right", direction = "vertical_right")
        circos.text(5, 5, "horizontal", direction = "horizontal")
        circos.text(5, 1, "arc_arc_arc_arc_arc", direction = "arc")
    })
circos.clear()

}
