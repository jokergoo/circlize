\name{circos.dendrogram}
\alias{circos.dendrogram}
\title{
Add circular dendrograms
}
\description{
Add circular dendrograms
}
\usage{
circos.dendrogram(dend, facing = c("outside", "inside"),
    max_height = NULL, use_x_attr = FALSE)
}
\arguments{

  \item{dend}{A \code{\link[stats]{dendrogram}} object.}
  \item{facing}{Is the dendromgrams facing inside to the circle or outside.}
  \item{max_height}{Maximum height of the dendrogram. This is important if more than one dendrograms are drawn in one track and making them comparable.}
  \item{use_x_attr}{Whether use the \code{x} attribute to determine node positions in the dendrogram, used internally.}

}
\details{
Assuming there are \code{n} nodes in the dendrogram, the positions for leaves on x-axis is \code{0.5, 1.5, ..., n - 0.5}.
So you must be careful with \code{xlim} when you initialize the cirular layout.

You can use the \code{dendextend} package to render the dendrograms.
}
\examples{
# There is no example
NULL
}
