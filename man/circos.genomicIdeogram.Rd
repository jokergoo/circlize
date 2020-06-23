\name{circos.genomicIdeogram}
\alias{circos.genomicIdeogram}
\title{
Add an ideogram track
}
\description{
Add an ideogram track
}
\usage{
circos.genomicIdeogram(
    cytoband = system.file(package = "circlize", "extdata", "cytoBand.txt"),
    species = NULL,
    track.height = mm_h(2),
    track.margin = circos.par("track.margin"))
}
\arguments{

  \item{cytoband}{A data frame or a file path, pass to \code{\link{read.cytoband}}.}
  \item{species}{Abbreviations of the genome, pass to \code{\link{read.cytoband}}.}
  \item{track.height}{Height of the ideogram track.}
  \item{track.margin}{Margins for the track.}

}
\seealso{
\url{https://jokergoo.github.io/circlize_book/book/high-level-genomic-functions.html#ideograms}
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
\donttest{
circos.initializeWithIdeogram(plotType = c("labels", "axis"))
circos.track(ylim = c(0, 1))
circos.genomicIdeogram() # put ideogram as the third track
}
}
