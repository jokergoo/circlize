\name{circos.labels}
\alias{circos.labels}
\title{
Add a label track
}
\description{
Add a label track
}
\usage{
circos.labels(
    sectors, x, labels,
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

  \item{sectors}{A vector of sector names.}
  \item{x}{Positions of the labels.}
  \item{labels}{A vector of labels.}
  \item{facing}{Facing of the labels. The value can only be \code{"clockwise"} or \code{"reverse.clockwise"}.}
  \item{niceFacing}{Whether automatically adjust the facing of the labels.}
  \item{col}{Color for the labels.}
  \item{cex}{Size of the labels.}
  \item{font}{Font of the labels.}
  \item{padding}{Padding of the labels, the value is the ratio to the height of the label.}
  \item{connection_height}{Height of the connection track.}
  \item{line_col}{Color for the connection lines.}
  \item{line_lwd}{Line width for the connection lines.}
  \item{line_lty}{Line type for the connectioin lines.}
  \item{labels_height}{Height of the labels track.}
  \item{side}{Side of the labels track, is it in the inside of the track where the regions are marked?}
  \item{labels.side}{Same as \code{side}. It will replace \code{side} in the future versions.}
  \item{track.margin}{Bottom and top margins.}

}
\details{
This function creates two tracks, one for the connection lines and one for the labels.

If two labels are too close and overlap, this function automatically adjusts the positions of neighouring labels.
}
\examples{
circos.initialize(sectors = letters[1:8], xlim = c(0, 1))
circos.track(ylim = c(0, 1))
circos.labels(c("a", "a", "b", "b"), x = c(0.1, 0.12, 0.4, 0.6), labels = c(0.1, 0.12, 0.4, 0.6))
}
