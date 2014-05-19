\name{circos.genomicText}
\alias{circos.genomicText}
\title{
  add text when drawing genomic graphics  


}
\description{
  add text when drawing genomic graphics  


}
\usage{
circos.genomicText(region, value, labels = NULL, labels.column = NULL, numeric.column = NULL,
    sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"), posTransform = NULL,
    direction = c("default", "default2", "vertical_left",
    "vertical_right", "horizontal", "arc"),
    adj = par("adj"), cex = 1, col = "black", font = par("font"), ...)
}
\arguments{
  \item{region}{a data frame with two columns: start position and end position}
  \item{value}{values corresponding to each genomic positions}
  \item{labels}{labels of text corresponding to each genomic positions}
  \item{labels.column}{index of column of the labels in \code{value}}
  \item{numeric.column}{index of column of the values in \code{value}}
  \item{sector.index}{pass to \code{\link{circos.rect}}}
  \item{track.index}{pass to \code{\link{circos.rect}}}
  \item{posTransform}{whether to do position transformation}
  \item{direction}{passing to \code{\link{circos.text}}}
  \item{adj}{pass to \code{\link{circos.text}}}
  \item{cex}{pass to \code{\link{circos.text}}}
  \item{col}{pass to \code{\link{circos.text}}}
  \item{font}{pass to \code{\link{circos.font}}}
  \item{...}{mysterious parameters}

}
\details{
  sth 


}
