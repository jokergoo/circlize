\name{chordDiagram}
\alias{chordDiagram}
\title{
Plot Chord Diagram
}
\description{
Plot Chord Diagram
}
\usage{
chordDiagram(x, grid.col = NULL, grid.border = NA, transparency = 0.5,
    col = NULL, row.col = NULL, column.col = NULL,
    order = NULL, directional = 0,
    symmetric = FALSE, keep.diagonal = FALSE,
    direction.type = "diffHeight", diffHeight = 0.04, reduce = 1e-5, self.link = 2,
    preAllocateTracks = NULL,
    annotationTrack = c("name", "grid", "axis"), annotationTrackHeight = c(0.05, 0.05),
    link.border = NA, link.lwd = par("lwd"), link.lty = par("lty"),
    link.sort = FALSE, link.decreasing = TRUE,
    link.arr.length = ifelse(link.arr.type == "big.arrow", 0.02, 0.4),
    link.arr.width = link.arr.length/2,
    link.arr.type = "triangle", link.arr.lty = par("lty"),
    link.arr.lwd = par("lwd"), link.arr.col = par("col"), ...)
}
\arguments{

  \item{x}{a matrix or a data frame. The function will pass all argument to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}} depending on the type of \code{x}, also format of other arguments depends of the type of \code{x}.}
  \item{grid.col}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{grid.border}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{transparency}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{col}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{row.col}{pass to \code{\link{chordDiagramFromMatrix}}}
  \item{column.col}{pass to \code{\link{chordDiagramFromMatrix}}}
  \item{order}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
  \item{directional}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}}
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
  \item{...}{pass to \code{\link{circos.link}}.}

}
\details{
Chord diagram is a way to visualize numeric tables ( \url{http://circos.ca/intro/tabular_visualization/} ), especially useful
when the table represents information of directional relations. This function
visualize tables in a circular way.

This function is flexible and contains some settings that may be a little difficult to understand. 
Please refer to vignette for better explanation.
}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.

}
\examples{
\dontrun{

############### example 1 ######################################
set.seed(123)
mat = matrix(sample(1:100, 18, replace = TRUE), 3, 6)
rownames(mat) = letters[1:3]
colnames(mat) = LETTERS[1:6]

### basic settings
par(mfrow = c(3, 2))
par(mar = c(1, 1, 1, 1))

chordDiagram(mat)
circos.clear()

circos.par(gap.degree = c(rep(2, nrow(mat)-1), 10, rep(2, ncol(mat)-1), 10))
chordDiagram(mat)
circos.clear()

circos.par(start.degree = 90)
chordDiagram(mat)
circos.clear()

chordDiagram(mat, order = c("A", "B", "a", "C", "D", "b", "E", "F", "c"))

chordDiagram(mat, directional = TRUE)
chordDiagram(mat, directional = TRUE, diffHeight = 0.06)

circos.clear()

################ example 2 ###############################
set.seed(123)
mat = matrix(sample(1:100, 18, replace = TRUE), 3, 6)
rownames(mat) = letters[1:3]
colnames(mat) = LETTERS[1:6]


### colors settings
rand_color = function(n, alpha = 1) {
    return(rgb(runif(n), runif(n), runif(n), alpha = alpha))
}

par(mfrow = c(3, 3))
par(mar = c(1, 1, 1, 1))
grid.col = NULL
grid.col[letters[1:3]] = c("red", "green", "blue")
grid.col[LETTERS[1:6]] = "grey"
chordDiagram(mat, grid.col = grid.col)
chordDiagram(mat, grid.col = grid.col, transparency = 0.5)
col_mat = rand_color(length(mat), alpha = 0.5)
dim(col_mat) = dim(mat)
chordDiagram(mat, grid.col = grid.col, col = col_mat)
chordDiagram(mat, grid.col = grid.col,
    col = colorRamp2(quantile(mat, seq(0, 1, by = 0.1)),
                     rev(heat.colors(11))), transparency = 0.5)

chordDiagram(mat, grid.col = grid.col, row.col = 1:3, transparency = 0.5)
chordDiagram(mat, grid.col = grid.col, column.col = 1:6, transparency = 0.5)
chordDiagram(mat, grid.col = grid.col, row.col = c("#FF000080", "#00FF0010", "#0000FF10"))
circos.clear()

}

}
