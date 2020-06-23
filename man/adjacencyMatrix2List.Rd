\name{adjacencyMatrix2List}
\alias{adjacencyMatrix2List}
\title{
Convert adjacency matrix to an adjacency list
}
\description{
Convert adjacency matrix to an adjacency list
}
\usage{
adjacencyMatrix2List(mat, keep.zero = FALSE)
}
\arguments{

  \item{mat}{A numeric matrix.}
  \item{keep.zero}{Whether to keep the interactions with value zero.}

}
\examples{
set.seed(999)
mat = matrix(sample(18, 18), 3, 6) 
rownames(mat) = paste0("S", 1:3)
colnames(mat) = paste0("E", 1:6)
adjacencyMatrix2List(mat)
}
