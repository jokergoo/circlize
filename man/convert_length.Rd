\name{convert_length}
\alias{convert_length}
\title{
Convert units
}
\description{
Convert units
}
\usage{
convert_length(x, unit = c("mm", "cm", "inches"))
}
\arguments{

  \item{x}{a numeric vector}
  \item{unit}{supported units, only "mm", "cm", "inches".}

}
\details{
This function coverts mm/cm/inches units to units measured in the canvas coordinate,
e.g. how much is it in the canvas coordinate for 1 mm/cm/inches.

Since in the circular plot, the aspect ratio is always 1, it does not matter this conversion
is applied on x direction or y direction.

This function is mainly used in the radical direction.
}
\seealso{
\code{\link{convert_x}} and \code{\link{convert_y}} convert absolute units into a data coordinate in a specified cell.

\url{https://jokergoo.github.io/circlize_book/book/circular-layout.html#convert-functions}
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
fa = letters[1:10]
circos.par(cell.padding = c(0, 0, 0, 0), track.margin = c(0, 0))
circos.initialize(fa, xlim = cbind(rep(0, 10), runif(10, 0.5, 1.5)))
circos.track(ylim = c(0, 1), track.height = convert_length(5, "mm"))
circos.par(track.margin = c(0, convert_length(2, "mm")))
circos.track(ylim = c(0, 1), track.height = convert_length(1, "cm"))
circos.par(track.margin = c(0, convert_length(5, "mm")))
circos.track(ylim = c(0, 1), track.height = convert_length(1, "inches"))
circos.clear()
}
