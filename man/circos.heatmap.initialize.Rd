\name{circos.heatmap.initialize}
\alias{circos.heatmap.initialize}
\title{
Initialize circular heatmaps
}
\description{
Initialize circular heatmaps
}
\usage{
circos.heatmap.initialize(x, split = NULL, cluster = TRUE,
    clustering.method = "complete", distance.method = "euclidean")
}
\arguments{

  \item{x}{A matrix or a list of matrices. If the value is a vector, it is converted into a one-column matrix.}
  \item{split}{A categorical variable. If the \code{x} is a matrix, it splits the matrix into a list of matrices.}
  \item{cluster}{whether to apply clustering on rows.}
  \item{clustering.method}{Clustering method, pass to \code{\link[stats]{hclust}}.}
  \item{distance.method}{Distance method, pass to \code{\link[stats]{dist}}.}

}
\examples{
# There is no example
NULL

}
