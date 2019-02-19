\name{circos.genomicLabels}
\alias{circos.genomicLabels}
\title{
Add labels to specified genomic regions
}
\description{
Add labels to specified genomic regions
}
\usage{
circos.genomicLabels(bed, labels = NULL, labels.column = NULL,
    facing = "clockwise", niceFacing = TRUE,
    col = par("col"), cex = 0.8, font = par("font"), padding = 0.4,
    connection_height = convert_height(5, "mm"),
    line_col = par("col"), line_lwd = par("lwd"), line_lty = par("lty"),
    labels_height = min(c(convert_height(1.5, "cm"),
    max(strwidth(labels, cex = cex, font = font)))),
    side = c("inside", "outside"), track.margin = circos.par("track.margin"))
}
\arguments{

  \item{bed}{a data frame in bed format}
  \item{labels}{a vector of labels corresponding to rows in \code{bed}}
  \item{labels.column}{if the label column is already in \code{bed}, the index for this column in \code{bed}}
  \item{facing}{facing of the labels. The value can only be 'clockwise' or 'reverse.clockwise'.}
  \item{niceFacing}{whether automatically adjust the facing of the labels.}
  \item{col}{color for the labels}
  \item{cex}{size of the labels}
  \item{font}{font of the labels}
  \item{padding}{padding of the labels, the value is the ratio to the height of the label}
  \item{connection_height}{height of the connection track}
  \item{line_col}{color for the connection lines}
  \item{line_lwd}{line width for the connection lines}
  \item{line_lty}{line type for the connectioin lines}
  \item{labels_height}{height of the labels track}
  \item{side}{side of the labels track, is it in the inside of the track where the regions are marked?}
  \item{track.margin}{bottom and top margins}

}
\details{
The function adds labels for the specified regions. The positions of labels are arranged
so that they are not overlapping to each other.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
\dontrun{
circos.initializeWithIdeogram(plotType = c("labels", "axis"))
bed = generateRandomBed(nr = 100, fun = function(k) sample(letters, k, replace = TRUE))
bed[1, 4] = "aaaaaaaa"
circos.genomicLabels(bed, labels.column = 4, side = "inside",
    col = as.numeric(factor(bed[[1]])))
circos.genomicLabels(bed, labels.column = 4, side = "outside",
    line_col = as.numeric(factor(bed[[1]])))
}
}
