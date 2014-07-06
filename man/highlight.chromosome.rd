\name{highlight.chromosome}
\alias{highlight.chromosome}
\title{
  Highlight a chromosome  


}
\description{
  Highlight a chromosome  


}
\usage{
highlight.chromosome(chr, track.index = seq_len(get.max.track.index()),
    col = "#FF000040", border = NA, lwd = par("lwd"), lty = par("lty"),
    padding = c(0, 0, 0, 0))
}
\arguments{
  \item{chr}{Chromosome name. Only allow single chromosome. It should be consistent with the sector index.}
  \item{track.index}{A vector of track index that you want to highlight}
  \item{col}{Color for highlighting. Note the color should be semi-transparent.}
  \item{border}{Border of the highlighted region}
  \item{lwd}{Width of borders}
  \item{lty}{Style of borders}
  \item{padding}{Padding for the highlighted region. It should contain four values representing ratios of the width or height of the highlighted region}

}
\details{
  You may use \code{\link{circos.info}} to find out index for all tracks.  

  The function calls \code{\link{draw.sector}}. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
