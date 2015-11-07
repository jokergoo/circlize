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

  \item{region}{Genomic positions. It can be a data frame with two columns which are start positions and end positions on a single chromosome. It can also be a bed-format data frame which contains the chromosome column.}
  \item{window.size}{Window size to calculate genomic density}
  \item{overlap}{Whether two neighbouring windows have half overlap}

}
\details{
It calculate the percent of each genomic windows that is covered by the input regions.
}
\value{
If the input is a two-column data frame, the function returns a data frame with three columns: 
start position, end position and percent of overlapping. And if the input is a bed-format
data frame, there will be an additionally chromosome name column.
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
