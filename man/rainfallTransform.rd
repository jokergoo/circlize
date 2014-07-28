\name{rainfallTransform}
\alias{rainfallTransform}
\title{
  Calculate inter-distance of genomic regions  


}
\description{
  Calculate inter-distance of genomic regions  


}
\usage{
rainfallTransform(region, mode = c("min", "max", "mean"))
}
\arguments{
  \item{region}{Genomic positions at a single chromosome. It can be a data frame with two columns which are start positions and end positions.}
  \item{mode}{How to calculate inter-distance. For a region, there is a distance to the  prevous region and also there is a distance to the next region. \code{mode} controls how to merge these two distances into one value.}

}
\value{
  A data frame with three columns: start position, end position and distance 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
\examples{
\dontrun{
bed = generateRandomBed()
bed = subset(bed, chr == "chr1")
rainfallTransform(bed[2:3])
}
}
