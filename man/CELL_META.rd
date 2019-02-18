\name{CELL_META}
\docType{data}
\alias{CELL_META}
\title{
Easy way to get meta data in the current cell
}
\description{
Easy way to get meta data in the current cell
}
\usage{
CELL_META
}
\details{
The variable \code{\link{CELL_META}} can only be used to get meta data of the "current" cell.
Basically you can simply replace  e.g. \code{get.cell.meta.data("sector.index")} to \code{CELL_META$sector.index}.
}
\seealso{
\code{\link{get.cell.meta.data}}
}
\examples{
pdf(NULL)
circos.initialize("a", xlim = c(0, 1))
circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
	print(CELL_META$sector.index)
	print(CELL_META$xlim)
})
dev.off()
}
