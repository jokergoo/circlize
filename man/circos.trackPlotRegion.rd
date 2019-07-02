\name{circos.trackPlotRegion}
\alias{circos.trackPlotRegion}
\title{
Create plotting regions for a whole track
}
\description{
Create plotting regions for a whole track
}
\usage{
circos.trackPlotRegion(factors = NULL, x = NULL, y = NULL, ylim = NULL,
    force.ylim = TRUE, track.index = NULL,
    track.height = circos.par("track.height"),
    track.margin = circos.par("track.margin"),
    cell.padding = circos.par("cell.padding"),
    bg.col = NA, bg.border = "black", bg.lty = par("lty"), bg.lwd = par("lwd"),
    panel.fun = function(x, y) {NULL})
}
\arguments{

  \item{factors}{A \code{\link{factor}} or a character vector which represents categories of data, if it is \code{NULL}, then it uses all sector index.}
  \item{x}{Data on x-axis. It is only used if \code{panel.fun} is set.}
  \item{y}{Data on y-axis}
  \item{ylim}{Range of data on y-axis}
  \item{force.ylim}{Whether to force all cells in the track to share the same \code{ylim}. Normally, all cells on a same track should have same \code{ylim}.}
  \item{track.index}{Index for the track which is going to be created/updated. If the specified track has already been created, this function just updated corresponding track with new plot. If the specified track is \code{NULL} or has not been created, this function just creates it. Note the value for this argument should not exceed maximum track index plus 1.}
  \item{track.height}{Height of the track. It is the percentage to the radius of the unit circles. The value can be set by \code{\link{uh}} to an absolute unit. If updating a track (with proper \code{track.index} value), this argument is ignored.}
  \item{track.margin}{only affect current track}
  \item{cell.padding}{only affect current track}
  \item{bg.col}{Background color for the plotting regions. It can be vector which has the same length of sectors.}
  \item{bg.border}{Color for the border of the plotting regions. It can be vector which has the same length of sectors.}
  \item{bg.lty}{Line style for the border of the plotting regions. It can be vector which has the same length of sectors.}
  \item{bg.lwd}{Line width for the border of the plotting regions. It can be vector which has the same length of sectors.}
  \item{panel.fun}{Panel function to add graphics in each cell, see "details" section and vignette for explanation.}

}
\details{
This function tends to be a high-level plotting function, which means,
you must first call this function to create plotting regions, then those
low-level graphic function such as \code{\link{circos.points}}, \code{\link{circos.lines}} can be
applied.

Currently, all the cells that are created in a same track sharing same height, which means,
there is no cell has larger height than others.

Since ranges for values on x-axis has already been defined by \code{\link{circos.initialize}}, only
ranges for values on y-axis should be specified in this function.
There are two ways to identify the ranges for values on y-axes either by \code{y}
or \code{ylim}. If \code{y} is set, it must has the same length as \code{factors} and the \code{ylim} for each cell is calculated
from y values. Also, the ylim can be specified from \code{ylim} which can be a two-element vector or a matrix which
has two columns and the number of rows is the same as the length of the levels of the factors.

If there is no enough space for the new track or the new track overlaps with other tracks,
there will be an error.

If \code{factors} does not cover all sectors, the cells in remaining unselected
sectors would also be created but without drawing anything. The \code{ylim} for these cells
are the same as that in the last created cell.

The function can also update a already-created track if the index for the track
is specified. If updating an existed track, those parameters related to the position (such as track height and track margin)
of the plotting region can not be changed.
}
\section{Panel}{
\code{panel.fun} provides a convenient way to add graphics in each cell when initializing the
tracks. The self-defined function needs two arguments: \code{x} and \code{y} which correspond to the data points
in the current cell. When \code{factors}, \code{x}, and \code{y} are set in \code{\link{circos.trackPlotRegion}}, a subset of \code{x}
and \code{y} are split by \code{factors} and are sent to \code{panel.fun} in the "current" cell.
\code{\link{circos.trackPlotRegion}} creates plotting regions one by one on the track and
\code{panel.fun} adds graphics in the 'current' cell after the plotting region for a certain cell has been
created.

See vignette for examples of how to use this feature.}
\seealso{
\url{http://jokergoo.github.io/circlize_book/book/circular-layout.html}
}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.

}
\examples{
circos.initialize(letters[1:8], xlim = c(0, 1))
set.seed(123)
df = data.frame(fa = sample(letters[1:8], 100, replace = TRUE),
                x = runif(100), y = rnorm(100))
circos.track(ylim = c(0, 1), bg.col = rand_color(8))
circos.track(df$fa, x = df$x, y = df$y, panel.fun = function(x, y) {
    circos.points(x, y)
}, track.height = 0.2, bg.border = rand_color(8))
circos.clear()

}
