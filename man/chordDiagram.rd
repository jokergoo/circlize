\name{chordDiagram}
\alias{chordDiagram}
\title{
  plot Chord Diagram  


}
\description{
  plot Chord Diagram  


}
\usage{
chordDiagram(mat, grid.col = NULL, transparency = 0.5,
    col = NULL, row.col = NULL, column.col = NULL, directional = FALSE,
    symmetric = FALSE, order = NULL, preAllocateTracks = NULL,
    annotationTrack = c("name", "grid"))
}
\arguments{
  \item{mat}{a table which represents as a numeric matrix}
  \item{grid.col}{colors of grids for elements}
  \item{transparency}{transparency of link/ribbon colors, 0 means no transparency and 1 means complete transparency.}
  \item{col}{colors for links. It can be a matrix which corresponds to \code{mat}, or a function which generate colors  according to values in \code{mat}, or a single value which means colors for all links are the same. You may use \code{\link{colorRamp2}} to generate a function which maps values to colors.}
  \item{row.col}{colors for links. If \code{col} is not set, colors for rownames}
  \item{column.col}{if \code{col} is not set, colors correspond to rownames}
  \item{directional}{whether links have direction. The direction is from rows to columns. If you want the direction from columns to rows, just transpose your \code{mat}.}
  \item{symmetric}{whether the matrix is symmetric. If the value is set to \code{TRUE}, only lower triangular matrix without the diagonal will be used.}
  \item{order}{order of sectors}
  \item{preAllocateTracks}{pre allocate empty tracks before drawing chord diagram}
  \item{annotationTrack}{which annotation track should be plotted?}

}
\details{
  \url{http://circos.ca/intro/tabular_visualization/} 


}
