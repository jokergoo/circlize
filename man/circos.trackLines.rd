\name{circos.trackLines}
\alias{circos.trackLines}
\title{
  Add lines to the plotting regions in a same track  


}
\description{
  Add lines to the plotting regions in a same track  


}
\usage{
circos.trackLines(factors, x, y, track.index = get.cell.meta.data("track.index"),
    col = "black", lwd = par("lwd"), lty = par("lty"), type = "l", straight = FALSE,
    area = FALSE, area.baseline = NULL, border = "black", baseline = "bottom",
    pt.col = par("col"), cex = par("cex"), pch = par("pch"))
}
\arguments{
  \item{factors}{Factors which represent the categories of data}
  \item{x}{Data points on x-axis}
  \item{y}{Data points on y-axis}
  \item{track.index}{Index for the track}
  \item{col}{Line color}
  \item{lwd}{line width}
  \item{lty}{line style}
  \item{type}{line type, similar as \code{type} argument in \code{\link[graphics]{lines}}, but only in \code{c("l", "o", "h", "s")}}
  \item{straight}{whether draw straight lines between points}
  \item{area}{whether to fill the area below the lines. If it is set to \code{TRUE}, \code{col} controls the filled color in the area and \code{border} controls the color of the line.}
  \item{area.baseline}{deprecated, use \code{baseline} instead.}
  \item{baseline}{the base line to draw area, pass to \code{\link{circos.lines}}.}
  \item{border}{color for border of the area}
  \item{pt.col}{if \code{type} is "o", points color}
  \item{cex}{if \code{type} is "o", points size}
  \item{pch}{if \code{type} is "o", points type}

}
\details{
  The function adds lines in multiple cells by first splitting data into several parts in which each part corresponds to one factor (sector index) and then add lines in cells by calling \code{\link{circos.lines}}.  

  This function can be replaced by a \code{for} loop containing \code{\link{circos.lines}}. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
