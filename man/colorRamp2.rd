\name{colorRamp2}
\alias{colorRamp2}
\title{
Color interpolation
}
\description{
Color interpolation
}
\usage{
colorRamp2(breaks, colors, transparency = 0, space = "LAB")
}
\arguments{

  \item{breaks}{A vector indicating numeric breaks}
  \item{colors}{A vector of colors which correspond to values in \code{breaks}}
  \item{transparency}{A single value in [0, 1]. 0 refers to no transparency and 1 refers to full transparency}
  \item{space}{color space in which colors are interpolated. Value should be one of "RGB", "HSV", "HLS", "LAB", "XYZ", "sRGB", "LUV", see \code{\link[colorspace]{color-class}} for detail.}

}
\details{
Colors are interpolated according to break values and corresponding colors by default through CIE Lab color space (\code{\link[colorspace]{LAB}}). 
Values exceeding breaks will be assigned with corresponding maximum or minimum colors.
}
\value{
It returns a function which accepts a vector of numbers and returns interpolated colors.
}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.

}
\examples{
col_fun = colorRamp2(c(-1, 0, 1), c("green", "white", "red"))
col_fun(c(-2, -1, -0.5, 0, 0.5, 1, 2))
}
