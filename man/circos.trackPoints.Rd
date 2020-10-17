\name{circos.trackPoints}
\alias{circos.trackPoints}
\title{
Add points to the plotting regions in a same track
}
\description{
Add points to the plotting regions in a same track
}
\usage{
circos.trackPoints(
    sectors,
    x, y,
    track.index = get.current.track.index(),
    pch = par("pch"),
    col = par("col"),
    cex = par("cex"),
    bg = par("bg"),
    factors = sectors)
}
\arguments{

  \item{sectors}{A \code{\link{factor}} or a character vector which represents the categories of data}
  \item{factors}{The same as \code{sectors}. It will be removed in future versions. }
  \item{x}{Data points on x-axis}
  \item{y}{Data points on y-axis}
  \item{track.index}{Index for the track}
  \item{pch}{Point type}
  \item{col}{Point color}
  \item{cex}{Point size}
  \item{bg}{backgrond color}

}
\details{
The function adds points in multiple cells by first splitting data into several parts in which
each part corresponds to one factor (sector index) and then adding points in each cell by calling \code{\link{circos.points}}.

Length of \code{pch}, \code{col} and \code{cex} can be one, length of levels of the factors or length of
factors.

This function can be replaced by a \code{for} loop containing \code{\link{circos.points}}.
}
\examples{
circos.initialize(letters[1:8], xlim = c(0, 1))
df = data.frame(sectors = sample(letters[1:8], 100, replace = TRUE),
                x = runif(100), y = runif(100))
circos.track(ylim = c(0, 1))
circos.trackPoints(df$sectors, x = df$x, y = df$y, pch = 16, col = as.numeric(factor(df$fa)))
circos.clear()
}
