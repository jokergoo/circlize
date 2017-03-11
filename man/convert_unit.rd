\name{convert_unit}
\alias{convert_unit}
\title{
Convert units
}
\description{
Convert units
}
\usage{
convert_unit(x, unit = c("mm", "cm", "inches"))
}
\arguments{

  \item{x}{a numeric vector}
  \item{unit}{supported units, only "mm", "cm", "inches"}

}
\details{
This function coverts mm/cm/inches units to units measured in the data coordinate,
e.g. how much is it in the data coordinate for 1 mm/cm/inches. Since the plotting
region for circular plot is always square, it does not matter whether to convert
units from the first dimension or the second, If the plotting regions is not square
the convertion is applied in the longer dimension.
}
\seealso{
\code{\link{convert_height}}, \code{\link{convert_width}}
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
plot(1, 1)
rect(0.8, 0.8, 1.2, 0.8 + convert_height(10, "mm"), col = "#FF000080")
rect(0.8, 0.8, 0.8 + convert_width(10, "mm"), 1.2, col = "#00FF0080")
}
