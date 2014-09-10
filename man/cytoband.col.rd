\name{cytoband.col}
\alias{cytoband.col}
\title{
  Assign colors to cytogenetic band (hg19) according to the Giemsa stain results  


}
\description{
  Assign colors to cytogenetic band (hg19) according to the Giemsa stain results  


}
\usage{
cytoband.col(x)
}
\arguments{
  \item{x}{A vector containing the Giemsa stain results}

}
\details{
  The color theme is from \url{http://circos.ca/tutorials/course/slides/session-2.pdf,} page 42. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
\examples{
\dontrun{
cytoband = read.cytoband()
cytoband.col(cytoband$df[[5]])
}
}
