\name{circos.lines}
\alias{circos.lines}
\title{
  Add lines to the plotting region


}
\description{
  Add lines to the plotting region


}
\usage{
circos.lines(x, y, sector.index = get.current.sector.index(),
    track.index = get.current.track.index(),
    col = ifelse(area, "grey", "black"), lwd = par("lwd"),
    lty = par("lty"), type = "l", straight = FALSE,
    area = FALSE, area.baseline = "bottom", border = "black",
    pt.col = par("col"), cex = par("cex"), pch = par("pch"))
}
\arguments{
  \item{x}{Data points on x-axis}
  \item{y}{Data points on y-axis}
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}
  \item{col}{Line color}
  \item{lwd}{line width}
  \item{lty}{line style}
  \item{type}{line type, similar as \code{type} argument in \code{\link[graphics]{lines}}, but only in \code{c("l", "o", "h", "s")}}
  \item{straight}{whether draw straight lines between points}
  \item{area}{whether to fill the area below the lines. If it is set to \code{TRUE}, \code{col} controls the filled color in the area and \code{border} controls the color of the line.}
  \item{area.baseline}{the base line to draw area below lines, default is the minimal of y-range (most bottom). It can be a string or number. If a string, it should be one of \code{bottom} and \code{top}.}
  \item{border}{color for border of the area}
  \item{pt.col}{if \code{type} is "o", points color}
  \item{cex}{if \code{type} is "o", points size}
  \item{pch}{if \code{type} is "o", points type}

}
\details{
  Normally, straight lines in the Cartesian coordinate have to be transformed into curves in the circos layout.But if you do not want to do such transformation you can use this function just drawing straightlines between points by setting \code{straight} to \code{TRUE}.

  Draw areas below lines can help to identify the direction of y-axis in cells (since it is a circle). This can be fullfilled by specifying\code{area} to \code{TURE}.


}
\examples{
library(circlize)
par(mar = c(1, 1, 1, 1), cex = 0.8)
factors = letters[1:8]
circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.5)
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "a")
circos.text(5, 9, "type = 'l'", sector.index = "a")
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "b", type = "o")
circos.text(5, 9, "type = 'o'", sector.index = "b")
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "c", type = "h")
circos.text(5, 9, "type = 'h'", sector.index = "c")
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "d", type = "s")
circos.text(5, 9, "type = 's'", sector.index = "d")
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "e", area = TRUE)
circos.text(5, 9, "type = 'l', area = TRUE", sector.index = "e")
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "f",
    type = "o", area = TRUE)
circos.text(5, 9, "type = 'o', area = TRUE", sector.index = "f")
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "g",
    type = "s", area = TRUE)
circos.text(5, 9, "type = 's', area = TRUE", sector.index = "g")
circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "h",
    area = TRUE, area.baseline = "top")
circos.text(5, 1, "type = 'l', area = TRUE\narea.baseline = 'top'",
    sector.index = "h")
circos.clear()
par(cex = 1)
}