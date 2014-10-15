\name{circos.lines}
\alias{circos.lines}
\title{
  Add lines to the plotting region  


}
\description{
  Add lines to the plotting region  


}
\usage{
circos.lines(x, y, sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"),
    col = ifelse(area, "grey", "black"), lwd = par("lwd"), lty = par("lty"), type = "l",
    straight = FALSE, area = FALSE, area.baseline = NULL, border = "black",
    baseline = "bottom", pt.col = par("col"), cex = par("cex"), pch = par("pch"))
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
  \item{area}{whether to fill the area below the lines. If it is set to \code{TRUE}, \code{col} controls the filled color in the area and \code{border} controls color of the line.}
  \item{area.baseline}{deprecated, use \code{baseline} instead.}
  \item{baseline}{the base line to draw areas. By default it is the minimal of y-range (bottom). It can be a string or a number. If a string, it should be one of \code{bottom} and \code{top}. This argument also works if \code{type} is set to \code{h}.}
  \item{border}{color for border of the area}
  \item{pt.col}{if \code{type} is "o", point color}
  \item{cex}{if \code{type} is "o", point size}
  \item{pch}{if \code{type} is "o", point type}

}
\details{
  Normally, straight lines in the Cartesian coordinate have to be transformed into curves in the circos layout. But if you do not want to do such transformation you can use this function just drawing straight lines between points by setting \code{straight} to \code{TRUE}.  

  Draw areas below lines can help to identify the direction of y-axis in cells (since it is a circle). This can be done by specifying \code{area} to \code{TURE}. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
\examples{
\dontrun{
library(circlize)
par(mar = c(1, 1, 1, 1), cex = 0.6)
factors = letters[1:9]
circos.par(points.overflow.warning = FALSE)
circos.initialize(factors = factors, xlim = c(0, 10))
circos.trackPlotRegion(factors = factors, ylim = c(0, 10), track.height = 0.5)

circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "a")
circos.text(5, 9, "type = 'l'", sector.index = "a", facing = "outside")

circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "b", type = "o")
circos.text(5, 9, "type = 'o'", sector.index = "b", facing = "outside")

circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "c", type = "h")
circos.text(5, 9, "type = 'h'", sector.index = "c", facing = "outside")

circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "d", type = "h", baseline = 5)
circos.text(5, 9, "type = 'h', baseline = 5", sector.index = "d", facing = "outside")

circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "e", type = "s")
circos.text(5, 9, "type = 's'", sector.index = "e", facing = "outside")

circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "f", area = TRUE)
circos.text(5, 9, "type = 'l', area = TRUE", sector.index = "f")

circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "g", type = "o", area = TRUE)
circos.text(5, 9, "type = 'o', area = TRUE", sector.index = "g")

circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "h", type = "s", area = TRUE)
circos.text(5, 9, "type = 's', area = TRUE", sector.index = "h")

circos.lines(sort(runif(10)*10), runif(10)*8, sector.index = "i", area = TRUE, baseline = "top")
circos.text(5, 9, "type = 'l', area = TRUE\nbaseline = 'top'", sector.index = "i")

circos.clear()
par(cex = 1)

}
}
