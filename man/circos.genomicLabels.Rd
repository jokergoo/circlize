\name{circos.genomicLabels}
\alias{circos.genomicLabels}
\title{
Add labels to specified genomic regions
}
\description{
Add labels to specified genomic regions
}
\usage{
circos.genomicLabels(
    bed,
    labels = NULL,
    labels.column = NULL,
    facing = "clockwise",
    niceFacing = TRUE,
    col = par("col"),
    cex = 0.8,
    font = par("font"),
    padding = 0.4,
    connection_height = mm_h(5),
    line_col = par("col"),
    line_lwd = par("lwd"),
    line_lty = par("lty"),
    labels_height = min(c(cm_h(1.5), max(strwidth(labels, cex = cex, font = font)))),
    side = c("inside", "outside"),
    labels.side = side,
    track.margin = circos.par("track.margin"))
}
\arguments{

  \item{bed}{A data frame in bed format.}
  \item{labels}{A vector of labels corresponding to rows in \code{bed}.}
  \item{labels.column}{If the label column is already in \code{bed}, the index for this column in \code{bed}.}
  \item{facing}{fFacing of the labels. The value can only be \code{"clockwise"} or \code{"reverse.clockwise"}.}
  \item{niceFacing}{Whether automatically adjust the facing of the labels.}
  \item{col}{Color for the labels.}
  \item{cex}{Size of the labels.}
  \item{font}{Font of the labels.}
  \item{padding}{Padding of the labels, the value is the ratio to the height of the label.}
  \item{connection_height}{Height of the connection track.}
  \item{line_col}{Color for the connection lines.}
  \item{line_lwd}{Line width for the connection lines.}
  \item{line_lty}{Line type for the connectioin lines.}
  \item{labels_height}{Height of the labels track.}
  \item{side}{Side of the labels track, is it in the inside of the track where the regions are marked?}
  \item{labels.side}{Same as \code{side}. It will replace \code{side} in the future versions.}
  \item{track.margin}{Bottom and top margins.}

}
\details{
The function adds labels for the specified regions. The positions of labels are arranged
so that they are not overlapping to each other.

This function creates two tracks, one for the connection lines and one for the labels.
}
\seealso{
\url{https://jokergoo.github.io/circlize_book/book/high-level-genomic-functions.html#labels}
}
\examples{
\donttest{
circos.initializeWithIdeogram()
bed = generateRandomBed(nr = 50, fun = function(k) sample(letters, k, replace = TRUE))
bed[1, 4] = "aaaaa"
circos.genomicLabels(bed, labels.column = 4, side = "inside")
circos.clear()

circos.initializeWithIdeogram(plotType = NULL)
circos.genomicLabels(bed, labels.column = 4, side = "outside",
    col = as.numeric(factor(bed[[1]])), line_col = as.numeric(factor(bed[[1]])))
circos.genomicIdeogram()
circos.clear()
}
}
