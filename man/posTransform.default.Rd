\name{posTransform.default}
\alias{posTransform.default}
\title{
Genomic position transformation function
}
\description{
Genomic position transformation function
}
\usage{
posTransform.default(region, ...)
}
\arguments{

  \item{region}{Genomic positions at a single chromosome. It is a data frame with two columns which are start position and end position.}
  \item{...}{other arguments}

}
\details{
The default position transformation functions transforms position to be equally distributed
along the chromosome. If users want to define their own transformation function, the requirement
is that the returned value should be a data frame with two columns: transformed start position
and transformed end position. The returned value should have same number of rows as the input one.

For details why need to use position transformation, please refer to \code{\link{circos.genomicPosTransformLines}}.
}
\examples{
# There is no example
NULL

}
