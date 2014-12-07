\name{generateRandomBed}
\alias{generateRandomBed}
\title{
  Generate random genomic data  


}
\description{
  Generate random genomic data  


}
\usage{
generateRandomBed(nr = 10000, nc = 1, fun = function(k) rnorm(k, 0, 0.5),
    species = NULL)
}
\arguments{
  \item{nr}{Number of rows}
  \item{nc}{Number of numeric columns / value columns}
  \item{fun}{Function for generating random values}
  \item{species}{species, pass to \code{\link{read.cytoband}}}

}
\details{
  The function will uniformly sample positions from human genome. Chromosome names start with "chr" and positions are sorted. The final number of rows may not be exactly as same as \code{nr}. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
\examples{
bed = generateRandomBed()
bed = generateRandomBed(nr = 200, nc = 4)
bed = generateRandomBed(fun = function(k) runif(k))
}
