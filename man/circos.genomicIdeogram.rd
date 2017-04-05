\name{circos.genomicIdeogram}
\alias{circos.genomicIdeogram}
\title{
Add an ideogram track
}
\description{
Add an ideogram track
}
\usage{
circos.genomicIdeogram(cytoband = system.file(package = "circlize",
    "extdata", "cytoBand.txt"), species = NULL, track.height = convert_height(2, "mm"))
}
\arguments{

  \item{cytoBand}{a data frame or a file path, pass to \code{\link{read.cytoband}}}
  \item{species}{Abbreviations of species, pass to \code{\link{read.cytoband}}}
  \item{track.height}{height of the ideogram track}

}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL

}
