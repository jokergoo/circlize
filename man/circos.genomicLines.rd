\name{circos.genomicLines}
\alias{circos.genomicLines}
\title{
  add lines  


}
\description{
  add lines  


}
\usage{
circos.genomicLines(region, value, numeric.column = NULL,
    sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"), posTransform = NULL,
    col = ifelse(area, "grey", "black"), lwd = par("lwd"),
    lty = par("lty"), type = "l",
    area = FALSE, area.baseline = "bottom", border = "black",
    pt.col = par("col"), cex = par("cex"), pch = par("pch"), ...)
}
\arguments{
  \item{region}{a data frame contains 2 column}
  \item{value}{a data frame contains values and other stuff}
  \item{numeric.column}{which column in \code{value} data frame should be taken as y-value. If it is not defined, the whole numeric columns in \code{value} will be taken.}
  \item{sector.index}{pass to \code{\link{circos.lines}}}
  \item{track.index}{pass to \code{\link{circos.lines}}}
  \item{posTransfrom}{self-defined functions to transform genomic positions}
  \item{col}{col of lines. pass to \code{\link{circos.lines}}}
  \item{lwd}{pass to \code{\link{circos.lines}}}
  \item{lty}{pass to \code{\link{circos.lines}}}
  \item{type}{pass to \code{\link{circos.lines}}. There is an additional option \code{segment}.}
  \item{area}{pass to \code{\link{circos.lines}}}
  \item{area.baseline}{pass to \code{\link{circos.lines}}}
  \item{border}{pass to \code{\link{circos.lines}}}
  \item{pt.col}{pass to \code{\link{circos.lines}}}
  \item{cex}{pass to \code{\link{circos.lines}}}
  \item{pch}{pass to \code{\link{circos.lines}}}
  \item{...}{mysterious parameters}

}
\details{
  sth 


}
