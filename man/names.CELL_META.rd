\name{names.CELL_META}
\alias{names.CELL_META}
\title{
Names of all meta data in the current cell
}
\description{
Names of all meta data in the current cell
}
\usage{
\method{names}{CELL_META}(x)
}
\arguments{

  \item{x}{use \code{\link{CELL_META}}.}

}
\details{
The variable \code{\link{CELL_META}} can only be used to get meta data of the "current" cell.
Basically you can simply replace  e.g. \code{get.cell.meta.data("sector.index")} to \code{CELL_META$sector.index}.

}
\seealso{
\code{\link{get.cell.meta.data}}

}
\examples{
names(CELL_META)
}
