\name{circos.lines}
\alias{circos.lines}
\title{
Add lines to the plotting region
}
\description{
Add lines to the plotting region
}
\usage{
circos.lines(
    x, y,
    sector.index = get.current.sector.index(),
    track.index = get.current.track.index(),
    col = ifelse(area, "grey", par("col")),
    lwd = par("lwd"),
    lty = par("lty"),
    type = "l",
    straight = FALSE,
    area = FALSE,
    area.baseline = NULL,
    border = "black",
    baseline = "bottom",
    pt.col = par("col"),
    cex = par("cex"),
    pch = par("pch"))
}
\arguments{

  \item{x}{Data points on x-axis, measured in "current" data coordinate.}
  \item{y}{Data points on y-axis, measured in "current" data coordinate.}
  \item{sector.index}{Index for the sector.}
  \item{track.index}{Index for the track.}
  \item{col}{Line color.}
  \item{lwd}{Line width.}
  \item{lty}{Line style.}
  \item{type}{Line type, similar as \code{type} argument in \code{\link[graphics]{lines}}, but only in \code{c("l", "o", "h", "s")}}
  \item{straight}{Whether draw straight lines between points.}
  \item{area}{Whether to fill the area below the lines. If it is set to \code{TRUE}, \code{col} controls the filled color in the area and \code{border} controls color of the line.}
  \item{area.baseline}{deprecated, use \code{baseline} instead.}
  \item{baseline}{The base line to draw areas. By default it is the minimal of y-range (bottom). It can be a string or a number. If a string, it should be one of \code{bottom} and \code{top}. This argument also works if \code{type} is set to \code{h}.}
  \item{border}{color for border of the area.}
  \item{pt.col}{If \code{type} is "o", point color.}
  \item{cex}{If \code{type} is "o", point size.}
  \item{pch}{If \code{type} is "o", point type.}

}
\details{
Normally, straight lines in the Cartesian coordinate have to be transformed into curves in the circular layout.
But if you do not want to do such transformation you can use this function just drawing straight
lines between points by setting \code{straight} to \code{TRUE}.

Drawing areas below lines can help to identify the direction of y-axis in cells (since it is a circle). This can be done by specifying
\code{area} to \code{TURE}.
}
\examples{
sectors = letters[1:9]
circos.par(points.overflow.warning = FALSE)
circos.initialize(sectors, xlim = c(0, 10))
circos.trackPlotRegion(sectors, ylim = c(0, 10), track.height = 0.5)

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
circos.text(5, 9, "type = 'l', area = TRUE, baseline = 'top'", sector.index = "i")

circos.clear()
}
