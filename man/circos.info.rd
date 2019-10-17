\name{circos.info}
\alias{circos.info}
\title{
Get information of the circular plot
}
\description{
Get information of the circular plot
}
\usage{
circos.info(sector.index = NULL, track.index = NULL, plot = FALSE)
}
\arguments{

  \item{sector.index}{Which sectors you want to look at? It can be a vector.}
  \item{track.index}{Which tracks you want to look at? It can be a vector.}
  \item{plot}{Whether to add information on the plot}

}
\details{
It tells you the basic parameters for sectors/tracks/cells. If both \code{sector.index}
and \code{track.index} are set to \code{NULL}, the function would print index for 
all sectors and all tracks. If \code{sector.index} and/or \code{track.index} are set,
the function would print \code{xlim}, \code{ylim}, \code{cell.xlim}, \code{cell.ylim},
\code{xplot}, \code{yplot}, \code{track.margin} and \code{cell.padding} for every cell in specified sectors and tracks.
Also, the function will print index of your current sector and current track.

If \code{plot} is set to \code{TRUE}, the function will plot the index of the sector and the track 
for each cell on the figure.
}
\seealso{
\url{https://jokergoo.github.io/circlize_book/book/circular-layout.html#circos-info-and-circos-clear}
}
\examples{
# There is no example
NULL

}
