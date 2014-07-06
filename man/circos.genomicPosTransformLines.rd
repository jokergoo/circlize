\name{circos.genomicPosTransformLines}
\alias{circos.genomicPosTransformLines}
\title{
  Add genomic position transformation lines between tracks  


}
\description{
  Add genomic position transformation lines between tracks  


}
\usage{
circos.genomicPosTransformLines(data, track.height = 0.1, posTransform = NULL,
    horizontalLine = c("none", "top", "bottom", "both"), track.margin = c(0, 0),
    type = c("default", "reverse"), col = "black", lwd = par("lwd"), lty = par("lty"))
}
\arguments{
  \item{data}{A data frame containing genomic data}
  \item{track.height}{Height of the track}
  \item{posTransform}{Genomic position transformation function, see \code{\link{posTransform.default}} for an example.}
  \item{horizontalLine}{Whether to draw horizontal lines which indicate width of each region}
  \item{track.margin}{Margin of tracks}
  \item{type}{Type of the transformation. \code{default} means position transformed track are located inside  and \code{reverse} means position transformed track are located outside.}
  \item{col}{Color of lines, can be length of one or length of nrow of \code{data}}
  \item{lwd}{Width of lines}
  \item{lty}{Style of lines}

}
\details{
  There is one representative situation when such position transformation needs to be applied.  For example, there are two sets of regions in a chromosome in which regions in one set regions are quite densely to each other and regions in other set are far from others. Heatmap or text is going to be drawn on the next track. If there is no position transformtion, heatmap or text for those dense regions would be overlapped and hard to identify, also ugly to visualize. Thus, a way to transform original positions to new positions would help for the visualization.  


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
