\name{$.CELL_META}
\alias{$.CELL_META}
\title{
Easy to way to get meta data in the current cell
}
\description{
Easy to way to get meta data in the current cell
}
\usage{
\method{$}{CELL_META}(x, name)
}
\arguments{

  \item{x}{name of the variable should be "CELL_META"}
  \item{name}{name of the cell meta name}

}
\details{
The variable \code{\link{CELL_META}} can only be used to get meta data of the "current" cell.
Basically you can simply replace  e.g. \code{get.cell.meta.data("sector.index")} to \code{CELL_META$sector.index}.
}
\seealso{
\code{\link{get.cell.meta.data}}
}
\examples{
# There is no example
NULL

}
