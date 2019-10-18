\name{rainfallTransform}
\alias{rainfallTransform}
\title{
Calculate inter-distance of genomic regions
}
\description{
Calculate inter-distance of genomic regions
}
\usage{
rainfallTransform(
    region,
    mode = c("min", "max", "mean", "left", "right"),
    normalize_to_width = FALSE)
}
\arguments{

  \item{region}{Genomic positions. It can be a data frame with two columns which are start positions and end positions on a single chromosome. It can also be a bed-format data frame which contains the chromosome column.}
  \item{mode}{How to calculate inter-distance. For a region, there is a distance to the  prevous region and also there is a distance to the next region. \code{mode} controls how to merge these two distances into one value.}
  \item{normalize_to_width}{If it is \code{TRUE}, the value is the relative distance divided by the width of the region.}

}
\value{
If the input is a two-column data frame, the function returnes a data frame with three columns: start position, end position and distance.
And if the input is a bed-format data frame, there will be the chromosome column added.

The row order of the returned data frame is as same as the input one.
}
\examples{
bed = generateRandomBed()
bed = subset(bed, chr == "chr1")
head(rainfallTransform(bed))
}
