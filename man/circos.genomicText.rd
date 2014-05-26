\name{circos.genomicText}
\alias{circos.genomicText}
\title{
  Draw text in a cell, specifically for genomic graphics  


}
\description{
  Draw text in a cell, specifically for genomic graphics  


}
\usage{
circos.genomicText(region, value, y = NULL, labels = NULL, labels.column = NULL,
    numeric.column = NULL, sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"), posTransform = NULL,
    direction = "default", adj = par("adj"), cex = 1, col = "black",
    font = par("font"), ...)
}
\arguments{
  \item{region}{a data frame contains 2 column which is start position and end position}
  \item{value}{a data frame contains values and other information}
  \item{y}{a vector or a single value indicating position of text.}
  \item{labels}{labels of text corresponding to each genomic positions}
  \item{labels.column}{if labels are in \code{value}, index of column in \code{value}}
  \item{numeric.column}{which column in \code{value} data frame should be taken as y-value. If it is not defined, only the first numeric columns in \code{value} will be taken.}
  \item{sector.index}{pass to \code{\link{circos.rect}}}
  \item{track.index}{pass to \code{\link{circos.rect}}}
  \item{posTransform}{self-defined functions to transform genomic positions, see \code{\link{posTransform.default}} for explaination}
  \item{direction}{passing to \code{\link{circos.text}}. Settings are similar as \code{col} }
  \item{adj}{pass to \code{\link{circos.text}}. Settings are similar as \code{col}}
  \item{cex}{pass to \code{\link{circos.text}}. Settings are similar as \code{col}}
  \item{col}{pass to \code{\link{circos.text}}. The length of \code{col} can be either one or number of rows of \code{region}.}
  \item{font}{pass to \code{\link{circos.text}}. Settings are similar as \code{col}}
  \item{...}{mysterious parameters}

}
\details{
  The function is usually put in \code{panel.fun} when using \code{\link{circos.genomicTrackPlotRegion}}. 


}
