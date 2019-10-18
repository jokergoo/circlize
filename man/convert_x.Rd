\name{convert_x}
\alias{convert_x}
\title{
Convert unit on x direction in data coordinate
}
\description{
Convert unit on x direction in data coordinate
}
\usage{
convert_x(
    x,
    unit = c("mm", "cm", "inches"),
    sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"),
    h = get.cell.meta.data("ycenter", sector.index = sector.index,
    track.index = track.index))
}
\arguments{

  \item{x}{a numeric vector}
  \item{unit}{supported units, only "mm", "cm", "inches"}
  \item{sector.index}{index for the sector where the conversion is applied}
  \item{track.index}{index for the track where the conversion is applied}
  \item{h}{since the width of the cell is not identical from the top to the bottom in the cell, the position on y direction needs to be specified. By default it is at the middle point on y-axis}

}
\value{
A vector of numeric values which are measured in the specified data coordinate
}
\seealso{
\code{\link{convert_y}} converts on y direction.

\url{https://jokergoo.github.io/circlize_book/book/circular-layout.html#convert-functions}
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
fa = letters[1:10]
circos.par(cell.padding = c(0, 0, 0, 0), track.margin = c(0, 0))
circos.initialize(fa, xlim = cbind(rep(0, 10), runif(10, 0.5, 1.5)))
circos.track(ylim = c(0, 1), track.height = convert_height(5, "mm"),
    panel.fun = function(x, y) {
        circos.lines(c(0, 0 + convert_x(5, "mm")), c(0.5, 0.5), col = "blue")
    })
circos.par(track.margin = c(0, convert_height(2, "mm")))
circos.track(ylim = c(0, 1), track.height = convert_height(1, "cm"),
    panel.fun = function(x, y) {
        xcenter = get.cell.meta.data("xcenter")
        circos.lines(c(xcenter, xcenter), c(0, convert_y(1, "cm")), col = "red")
    })
circos.par(track.margin = c(0, convert_height(5, "mm")))
circos.track(ylim = c(0, 1), track.height = convert_height(1, "inches"),
    panel.fun = function(x, y) {
        line_length_on_x = convert_x(1*sqrt(2)/2, "cm")
        line_length_on_y = convert_y(1*sqrt(2)/2, "cm")
        circos.lines(c(0, line_length_on_x), c(0, line_length_on_y), col = "orange")
    })
circos.clear()
}
