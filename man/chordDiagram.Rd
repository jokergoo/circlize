\name{chordDiagram}
\alias{chordDiagram}
\title{
Plot Chord Diagram
}
\description{
Plot Chord Diagram
}
\usage{
chordDiagram(
    x,
    grid.col = NULL,
    grid.border = NA,
    transparency = 0.5,
    col = NULL,
    row.col = NULL,
    column.col = NULL,
    order = NULL,
    directional = 0,
    xmax = NULL,
    symmetric = FALSE,
    keep.diagonal = FALSE,
    direction.type = "diffHeight",
    diffHeight = convert_height(2, "mm"),
    reduce = 1e-5,
    self.link = 2,
    preAllocateTracks = NULL,
    annotationTrack = c("name", "grid", "axis"),
    annotationTrackHeight = convert_height(c(3, 2), "mm"),
    link.border = NA,
    link.lwd = par("lwd"),
    link.lty = par("lty"),
    link.sort = FALSE,
    link.decreasing = TRUE,
    link.arr.length = ifelse(link.arr.type == "big.arrow", 0.02, 0.4),
    link.arr.width = link.arr.length/2,
    link.arr.type = "triangle",
    link.arr.lty = par("lty"),
    link.arr.lwd = par("lwd"),
    link.arr.col = par("col"),
    link.largest.ontop = FALSE,
    link.visible = TRUE,
    link.rank = NULL,
    link.overlap = FALSE,
    scale = FALSE,
    big.gap = 10,
    small.gap = 1,
    ...)
}
\arguments{

  \item{x}{a matrix or a data frame. The function will pass all argument to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}} depending on the type of \code{x}, also format of other arguments depends of the type of \code{x}. If it is in the form of a matrix, it should be an adjacency matrix. If it is in the form of a data frame, it should be an adjacency list.}
  \item{grid.col}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{grid.border}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{transparency}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{col}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{row.col}{pass to \code{\link{chordDiagramFromMatrix}}}
  \item{column.col}{pass to \code{\link{chordDiagramFromMatrix}}}
  \item{order}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{directional}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{xmax}{maximum value on x-axes, the value should be a named vector.}
  \item{symmetric}{pass to \code{\link{chordDiagramFromMatrix}}}
  \item{keep.diagonal}{pass to \code{\link{chordDiagramFromMatrix}}}
  \item{direction.type}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{diffHeight}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{reduce}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{self.link}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{preAllocateTracks}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{annotationTrack}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{annotationTrackHeight}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{link.border}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{link.lwd}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{link.lty}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{link.sort}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{link.decreasing}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{link.arr.length}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{link.arr.width}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{link.arr.type}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{link.arr.lty}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{link.arr.lwd}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{link.arr.col}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{link.largest.ontop}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{link.visible}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{link.rank}{order to add links to the circle, a large value means to add it later.}
  \item{link.overlap}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{scale}{scale each sector to same width}
  \item{big.gap}{Gap between the two sets of sectors. If the input is a matrix, the two sets are row sectors and column sectors. If the input is a data frame, the two sets correspond to the first column and the second column. It only works when there is no intersection between the two sets.}
  \item{small.gap}{Small gap between sectors.}
  \item{...}{pass to \code{\link{circos.link}}.}

}
\details{
Chord diagram is a way to visualize numeric tables ( \url{http://circos.ca/intro/tabular_visualization/} ), especially useful
when the table represents information of directional relations. This function
visualize tables in a circular way.

This function is flexible and contains some settings that may be a little difficult to understand. 
Please refer to vignette for better explanation.
}
\seealso{
\url{http://jokergoo.github.io/circlize_book/book/the-chorddiagram-function.html}
}
\value{
A data frame which contains positions of links, columns are:

\describe{
  \item{\code{rn}}{sector name corresponding to rows in the adjacency matrix or the first column in the adjacency list}
  \item{\code{cn}}{sector name corresponding to columns in the adjacency matrix or the second column in the adjacency list}
  \item{\code{value}}{value for the interaction or relation}
  \item{\code{o1}}{order of the link on the "from" sector}
  \item{\code{o2}}{order of the link on the "to" sector}
  \item{\code{x1}}{and position of the link on the "from" sector, the interval for the link on the "from" sector is \code{c(x1-abs(value), x1)}}
  \item{\code{x2}}{and position of the link on the "to" sector, the interval for the link on the "from" sector is \code{c(x2-abs(value), x2)}}
}
}
\examples{
set.seed(999)
mat = matrix(sample(18, 18), 3, 6) 
rownames(mat) = paste0("S", 1:3)
colnames(mat) = paste0("E", 1:6)

df = data.frame(from = rep(rownames(mat), times = ncol(mat)),
    to = rep(colnames(mat), each = nrow(mat)),
    value = as.vector(mat),
    stringsAsFactors = FALSE)

chordDiagram(mat)
chordDiagram(df)
circos.clear()
}
