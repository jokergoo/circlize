\name{colorRamp2}
\alias{colorRamp2}
\title{
  Color interpolation  


}
\description{
  Color interpolation  


}
\usage{
colorRamp2(breaks, colors, transparency = 0, ...)
}
\arguments{
  \item{breaks}{a vector indicating breaks of your data}
  \item{colors}{a vector of colors which corresponds to values in \code{breaks}}
  \item{transparency}{a single value in [0, 1]. 0 refers to no transparency and 1 refers to complete transparency}
  \item{...}{pass to \code{\link[grDevices]{colorRamp}}}

}
\details{
  Colors are interpolated according to break values and corresponding colors  


}
\value{
  It returns a function which accepts a vector of numbers and returns interpolated colors. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
\examples{
library(circlize)
col_fun = colorRamp2(c(-1, 0, 1), c("green", "black", "red"))
col_fun(seq(-2, 2, by = 0.5))
}
