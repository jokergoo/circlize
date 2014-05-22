\name{colorRamp2}
\alias{colorRamp2}
\title{
  Color interpolation  


}
\description{
  Color interpolation  


}
\usage{
colorRamp2(breaks, colors, ...)
}
\arguments{
  \item{breaks}{a vector indicating breaks of your data}
  \item{colors}{a vector of colors which corresponds to value in \code{breaks}.}
  \item{...}{pass to \code{\link[grDevices]{colorRamp}}}

}
\details{
  Colors are interpolated according to break values and corresponding colors  


}
\value{
  It returns a function which accepts a vector of numbers and returns interpolated colors. 


}
