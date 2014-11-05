\name{colorRamp2}
\alias{colorRamp2}
\title{
  Color interpolation  


}
\description{
  Color interpolation  


}
\usage{
colorRamp2(breaks, colors, transparency = 0)
}
\arguments{
  \item{breaks}{A vector indicating numeric breaks}
  \item{colors}{A vector of colors which correspond to values in \code{breaks}}
  \item{transparency}{A single value in [0, 1]. 0 refers to no transparency and 1 refers to full transparency}

}
\details{
  Colors are interpolated according to break values and corresponding colors. Values exceeding breaks will be assigned with maximum or minimum colors.  


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

# map colors to p-values
col_fun = colorRamp2(c(log10(0.0001), log10(0.05), log10(1)), c("green", "white", "red"))
col_fun(log10(c(0.000001, 0.0012, 0.012, 0.2)))
}
