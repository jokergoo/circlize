\name{circos.trackLines}
\alias{circos.trackLines}
\title{
Add lines to the plotting regions in a same track
}
\description{
Add lines to the plotting regions in a same track
}
\usage{
circos.trackLines(
    sectors,
    x, y,
    track.index = get.current.track.index(),
    col = par("col"),
    lwd = par("lwd"),
    lty = par("lty"),
    type = "l",
    straight = FALSE,
    area = FALSE,
    area.baseline = NULL,
    border = "black",
    baseline = "bottom",
    pt.col = par("col"),
    cex = par("cex"),
    pch = par("pch"),
    factors = sectors)
}
\arguments{

  \item{sectors}{A \code{\link{factor}} or a character vector which represents the categories of data.}
  \item{factors}{The same as \code{sectors}. It will be removed in future versions. }
  \item{x}{Data points on x-axis.}
  \item{y}{Data points on y-axis.}
  \item{track.index}{Index for the track.}
  \item{col}{Line color.}
  \item{lwd}{Line width.}
  \item{lty}{Line style.}
  \item{type}{Line type, similar as \code{type} argument in \code{\link[graphics]{lines}}, but only in \code{c("l", "o", "h", "s")}.}
  \item{straight}{Whether draw straight lines between points.}
  \item{area}{Whether to fill the area below the lines. If it is set to \code{TRUE}, \code{col} controls the filled color in the area and \code{border} controls the color of the line.}
  \item{area.baseline}{Deprecated, use \code{baseline} instead.}
  \item{baseline}{The base line to draw area, pass to \code{\link{circos.lines}}.}
  \item{border}{Color for border of the area.}
  \item{pt.col}{If \code{type} is "o", points color.}
  \item{cex}{If \code{type} is "o", points size.}
  \item{pch}{If \code{type} is "o", points type.}

}
\details{
The function adds lines in multiple cells by first splitting data into several parts in which
each part corresponds to one factor (sector index) and then add lines in cells by calling \code{\link{circos.lines}}.

This function can be replaced by a \code{for} loop containing \code{\link{circos.lines}}.
}
\examples{
# There is no example
NULL

}
