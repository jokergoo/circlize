\name{chordDiagram}
\alias{chordDiagram}
\title{
Plot Chord Diagram

}
\description{
Plot Chord Diagram

}
\usage{
chordDiagram(x, ...)}
\arguments{

  \item{x}{a matrix or a data frame. The function will pass all argument to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}} depending on the type of \code{x}}
  \item{...}{pass to \code{\link{chordDiagramFromMatrix}} or \code{\link{chordDiagramFromDataFrame}}.}
}
\details{
Chord diagram is a way to visualize numeric tables ( \url{http://circos.ca/intro/tabular_visualization/} ), especially useful
when the table represents information of directional relations. This function
visualize tables in a circular way.

This function is flexible and contains some settings that may be a little difficult to understand. 
Please refer to vignette for better explanation.

}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.}
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

}}
