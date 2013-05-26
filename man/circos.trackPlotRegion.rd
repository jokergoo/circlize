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
    track.height = circos.par("default.track.height"), bg.col = NA,
    bg.border = "black", bg.lty = par("lty"), bg.lwd = par("lwd"),
    panel.fun = function(x, y) {NULL})
}
\arguments{
  \item{factors}{Factors which represent the categories of data, if is \code{NULL},  then it is the whole sector index.}
  \item{x}{Data on the x-axis}
  \item{y}{Data on the y-axis}
  \item{ylim}{Range of data on the y-axis}
  \item{force.ylim}{Whether to force all cells in the track to share the same \code{ylim}}
  \item{track.index}{Index for the track which is goning to be updated. Setting it to \code{NULL} means creating the plotting regions in the next newest track.}
  \item{track.height}{Height of the track. It is the percentage to the radius of the unit circls. If to update a track, this argument is disabled.}
  \item{bg.col}{Background color for the plotting regions}
  \item{bg.border}{Color for the boder of the plotting regions}
  \item{bg.lty}{Line style for the border of the plotting regions}
  \item{bg.lwd}{Line width for the border of the plotting regions}
  \item{panel.fun}{Panel function to draw figures in each cell, see "details" section and vignette for explaination.}

}
\details{
  This function pretends to be high-level plotting function, which means, you must first call this function to create a plotting region, then thoselow-level-style plotting function such as \code{\link{circos.points}}, \code{\link{circos.lines}} can beapplied.

  It has two different usages. First, it can create a complete track which among severalsectors. Because currently it does not support creating single cell since it wouldmake the layout disordered, this is the only way to create the plotting region.

  Currently, all the cells that are created in a same track sharing same height, which means,there is no cell has longer height than others.

  Since limitation for values on x-axis has already been defined by \code{\link{circos.initialize}}, onlylimitation for values on y-axis should be specified in this function. The \code{x} argument is onlyused if you set \code{panel.fun}. There are two ways to identify the limitation for values on y-axies either by \code{y}or \code{ylim}. If \code{y} is set, it must has the same length as \code{factors} and the ylim or each cell is calculatedfrom y values. Also, the ylim can be specified from \code{ylim} which can be a two-element vector or a matrix whichhas two columns and the number of rows is the same as the length of the levels of the factors.

  If there is no enough space for the new track or the new track has overlap with other tracks,there will be an error.

  \code{panel.fun} provides a convinient way to draw figures in each cell when initializing the track. The self-defined function need two arguments: \code{x} and \code{y} which is the data pointsin the current cell. \code{\link{circos.trackPlotRegion}} creates plotting regions one by one on the track and\code{panel.fun} draw graphs in the 'current' cell after the plotting region of a certain cell has beencreated. See vignette for examples of how to use this feature.

  If \code{factors} does not cover all sectors which is going to be initialized, the cells in remaining unselectedsectors would also be created but without drawing anything. The \code{ylim} for these cellsare the same as that in the latest created cell.

  Second, it can update a already-created plotRegion if the index for the trackis specified. If the index is one bigger than the largest current track index. It in factcreates the new track. If updating an existed track, those parameters related to the positionof the plotting region can not be changed.


}
