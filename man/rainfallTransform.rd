\name{rainfallTransform}
\alias{rainfallTransform}
\title{
  calculate inter-distance of genomic regions  


}
\description{
  calculate inter-distance of genomic regions  


}
\usage{
rainfallTransform(region, mode = c("min", "max", "mean"))
}
\arguments{
  \item{region}{Genomic positions at a single chromosome. It can be a data frame with two columns which are start positions and end positions or an \code{IRanges} object.}
  \item{mode}{How to calculate inter-distance. For a region, there is a distance to the  prevous region and also there is a distance to the next region. \code{mode} controls how to merge these two distances into one value.}

}
\details{

  rainfall transformation  



}
\{

}
