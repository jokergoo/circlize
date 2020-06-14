\name{circos.heatmap.link}
\alias{circos.heatmap.link}
\title{
Draw a link between two matrix rows in the circular heatmap
}
\description{
Draw a link between two matrix rows in the circular heatmap
}
\usage{
circos.heatmap.link(row_from, row_to, ...)
}
\arguments{

  \item{row_from}{The row index where the link starts. The value should be length 1. If you want to draw multiple links, put the function in a \code{for} loop.}
  \item{row_to}{The row index where the link ends.}
  \item{...}{Pass to \code{\link{circos.link}}.}

}
\examples{
\donttest{
set.seed(123)
mat = matrix(rnorm(100*10), nrow = 100)
rownames(mat) = paste0("R", 1:100)
col_fun = colorRamp2(c(-2, 0, 2), c("blue", "white", "red"))
circos.heatmap(mat, col = col_fun, rownames.side = "outside")
circos.heatmap.link(10, 60)
circos.clear()

split = sample(letters[1:5], 100, replace = TRUE)
circos.heatmap(mat, col = col_fun, split = split, 
	rownames.side = "outside")
circos.heatmap.link(10, 60)
circos.clear()
}
}
