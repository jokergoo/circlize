\name{generateRandomBed}
\alias{generateRandomBed}
\title{
  generate random genomic data  


}
\description{
  generate random genomic data  


}
\usage{
generateRandomBed(nr = 10000, nc = 1, fun = function(k) rnorm(k, 0, 0.5))
}
\arguments{
  \item{nr}{number of rows}
  \item{nc}{number of numeric columns / value columns}
  \item{fun}{function to generate random data}

}
\details{
  The function will sample positions from human genome. Chromosome names start with "chr" and positions are sorted. The final number of rows may not be exactly as same as \code{nr}. 


}
