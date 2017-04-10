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
    "extdata", "cytoBand.txt"), species = NULL, track.height = convert_height(2, "mm"),
    track.margin = circos.par("track.margin"))
}
\arguments{

  \item{cytoband}{a data frame or a file path, pass to \code{\link{read.cytoband}}}
  \item{species}{Abbreviations of species, pass to \code{\link{read.cytoband}}}
  \item{track.height}{height of the ideogram track}
  \item{track.margin}{margins for the track}

}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL

}
