\name{circos.clear}
\alias{circos.clear}
\title{
  Reset the circos layout parameters  


}
\description{
  Reset the circos layout parameters  


}
\usage{
circos.clear()
}
\details{
  Because there are several parameters for circos plot which can only be set before \code{\link{circos.initialize}}. So before you draw the next circos plot, you need to reset these parameters.  

  If you meet some errors when re-drawing the circos plot, try running this function and it will solve most of the problems. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
