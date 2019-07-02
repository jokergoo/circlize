\name{circos.points}
\alias{circos.points}
\title{
Add points to a plotting region
}
\description{
Add points to a plotting region
}
\usage{
circos.points(x, y, sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"),
    pch = par("pch"), col = par("col"), cex = par("cex"), bg = par("bg"))
}
\arguments{

  \item{x}{Data points on x-axis, measured in "current" data coordinate}
  \item{y}{Data points on y-axis, measured in "current" data coordinate}
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}
  \item{pch}{Point type}
  \item{col}{Point color}
  \item{cex}{Point size}
  \item{bg}{backgrond of points}

}
\details{
This function can only add points in one specified cell. Pretending a low-level plotting
function, it can only be applied in plotting region which has been created.

You can think the function similar as the normal \code{\link[graphics]{points}}
function, just adding points in the circular plotting region. The position of
cell is identified by \code{sector.index} and \code{track.index}, if they are not
specified, they are in 'current' sector and 'current' track.

Data points out of the plotting region will also be added, but with warning messages.

Other graphics parameters which are available in the function are \code{pch}, \code{col}
and \code{cex} which have same meaning as those in the \code{\link[graphics]{par}}.

It is recommended to use \code{\link{circos.points}} inside \code{panel.fun} in \code{\link{circos.trackPlotRegion}} so that
it draws points directly on "curent" cell.
}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.

}
\examples{
circos.initialize(letters[1:8], xlim = c(0, 1))
circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
    circos.points(runif(10), runif(10))
})
circos.points(runif(10), runif(10), sector.index = "c", pch = 16, col = "red")
circos.clear()

}
