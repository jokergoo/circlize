\name{calc_gap}
\alias{calc_gap}
\title{
Calculate gap to make two Chord diagram with same scale
}
\description{
Calculate gap to make two Chord diagram with same scale
}
\usage{
calc_gap(x1, x2, big.gap = 10, small.gap = 1)
}
\arguments{

  \item{x1}{The matrix or the data frame for the first Chord diagram.}
  \item{x2}{The matrix or the data frame for the second Chord diagram.}
  \item{big.gap}{\code{big.gap} for the first Chord diagram.}
  \item{small.gap}{\code{small.gap} for both Chord diagrams.}

}
\details{
There should be no overlap between the two sets of sectors.
}
\value{
A numeric value which can be directly set to \code{big.gap} in the second Chord diagram.
}
\examples{
# There is no example
NULL

}
