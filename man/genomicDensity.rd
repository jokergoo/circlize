\name{genomicDensity}
\alias{genomicDensity}
\title{
  Calculate genomic region density  


}
\description{
  Calculate genomic region density  


}
\usage{
genomicDensity(region, window.size = 10000000, overlap = TRUE)
}
\arguments{
  \item{region}{Genomic positions at a single chromosome. It is a data frame with two columns which are start position and end position}
  \item{window.size}{Window size to calculate genomic density}
  \item{overlap}{Whether two neighbouring windows have half overlap}

}
\details{
  It calculate the percent of each genomic windows that is covered by the input regions.  


}
\value{
  a data frame with three columns: start position, end position and percent of overlapping. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
\examples{
\dontrun{
bed = generateRandomBed()
bed = subset(bed, chr == "chr1")
genomicDensity(bed[2:3])
}
}
