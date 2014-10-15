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
  \item{breaks}{A vector indicating numeric breaks}
  \item{colors}{A vector of colors which correspond to values in \code{breaks}}
  \item{transparency}{A single value in [0, 1]. 0 refers to no transparency and 1 refers to full transparency}

}
\details{
  Colors are interpolated according to break values and corresponding colors. Values exceeding breaks will be assigned with maximum or minimum colors.  


}
\value{
  It returns a function which accepts a vector of numbers and returns interpolated colors. 


}
