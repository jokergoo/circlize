\name{chordDiagramFromDataFrame}
\alias{chordDiagramFromDataFrame}
\title{
Plot Chord Diagram from a data frame

}
\description{
Plot Chord Diagram from a data frame

}
\usage{
chordDiagramFromDataFrame(df, grid.col = NULL, grid.border = NA, transparency = 0.5,
    col = NULL, order = NULL, directional = 0,
    direction.type = "diffHeight", diffHeight = 0.04, reduce = 1e-5, self.link = 2,
    preAllocateTracks = NULL,
    annotationTrack = c("name", "grid", "axis"), annotationTrackHeight = c(0.05, 0.05),
    link.border = NA, link.lwd = par("lwd"), link.lty = par("lty"),
    link.sort = FALSE, link.decreasing = TRUE,
    link.arr.length = ifelse(link.arr.type == "big.arrow", 0.02, 0.4),
    link.arr.width = link.arr.length/2,
    link.arr.type = "triangle", link.arr.lty = par("lty"),
    link.arr.lwd = par("lwd"), link.arr.col = par("col"), ...)}
\arguments{

  \item{df}{A data frame with at least two columns. The first two columns specify the connections and the third column (optional)contains numeric values which are mapped to the width of links as well as the colors if \code{col} is specified as a color mapping function.The sectors in the plot will be \code{union(df[[1]], df[[2]])}.}
  \item{grid.col}{Grid colors which correspond to sectors. The length of the vector should be either 1 or the number of sectors.It's preferred that \code{grid.col} is a named vector of which names correspond to sectors. If it is not a named vector, the order of \code{grid.col} corresponds to order of sectors.}
  \item{grid.border}{border for grids. If it is \code{NULL}, the border color is same as grid color}
  \item{transparency}{Transparency of link colors, 0 means no transparency and 1 means full transparency.If transparency is already set in \code{col} or \code{row.col} or \code{column.col}, this argument will be ignored.\code{NA}also ignores this argument.}
  \item{col}{Colors for links. It can be a vector which corresponds to connections in \code{df}, or a function which generate colors according to values (the third column) in \code{df}, or a single value which means colors for all links are the same. Youmay use \code{\link{colorRamp2}} to generate a function which maps values to colors.}
  \item{order}{Order of sectors. Default order is \code{union(df[[1]], df[[2]])}.}
  \item{directional}{Whether links have directions. 1 means the direction is from the first column in \code{df} to the second column, -1is the reverse and 0 is no direction.}
  \item{direction.type}{type for representing directions. Can be one or two values in "diffHeight" and "arrows". If the value contains "diffHeight",different heights of the links are used to represent the directions for which starting root has long height to give people feelingthat something is comming out. If the value contains "arrows", users can customize arrows with following arguments.}
  \item{diffHeight}{The difference of height between two 'roots' if \code{directional} is set to \code{TRUE}. If the value is set toa positive value, start root is shorter than end root and if it is set to a negative value, start root is longerthan the end root.}
  \item{reduce}{if the ratio of the width of certain grid compared to the whole circle is less than this value, the grid is removed on the plot.Set it to value less than zero if you want to keep all tiny grid.}
  \item{self.link}{if there is a self link in one sector, 1 means the link will be degenerated as a 'mountain' and the width corresponds to the value for this connection.2 means the width of the starting root and the ending root all have the same width that corresponds to the value for the connection.}
  \item{preAllocateTracks}{Pre-allocate empty tracks before drawing Chord diagram. It can be a single number indicatinghow many empty tracks needed to be created or a list containing settings for emptytracks. Please refer to vignette for details.}
  \item{annotationTrack}{Which annotation track should be plotted? By default, a track containing sector names and a trackcontaining grid will be created.}
  \item{annotationTrackHeight}{Track height corresponding to values in \code{annotationTrack}.}
  \item{link.border}{border for links, single scalar or a vector which has the same length as nrows of \code{df}}
  \item{link.lwd}{width for link borders, single scalar or a vector which has the same length as nrows of \code{df}}
  \item{link.lty}{style for link borders, single scalar or a vector which has the same length as nrows of \code{df}}
  \item{link.sort}{whether sort links on every sector based on the width of the links on it.}
  \item{link.decreasing}{for \code{link.sort}}
  \item{link.arr.length}{pass to \code{\link{circos.link}}, same settings as \code{link.lwd}.}
  \item{link.arr.width}{pass to \code{\link[shape]{Arrowhead}}, same settings as \code{link.lwd}.}
  \item{link.arr.type}{pass to \code{\link{circos.link}}, same settings as \code{link.lwd}. Default value is \code{triangle}.}
  \item{link.arr.col}{color or the single line link which is put in the center of the belt, same settings as \code{link.lwd}.}
  \item{link.arr.lwd}{line width ofthe single line link which is put in the center of the belt, same settings as \code{link.lwd}.}
  \item{link.arr.lty}{line type of the single line link which is put in the center of the belt, same settings as \code{link.lwd}.}
  \item{...}{pass to \code{\link{circos.link}}}
}
\details{
...}
