\name{set.current.cell}
\alias{set.current.cell}
\title{
Set flag to current cell
}
\description{
Set flag to current cell
}
\usage{
set.current.cell(sector.index, track.index)
}
\arguments{

  \item{sector.index}{sector index}
  \item{track.index}{track index}

}
\details{
After setting the current cell, all functions which need \code{sector.index} and \code{track.index}
arguments and are applied to the current cell do not need to specify the two arguments explicitly.
}
\examples{
pdf(NULL)
circos.initialize(letters[1:8], xlim = c(0, 1))
circos.track(ylim = c(0, 1))
circos.info()
set.current.cell("b", 1)
circos.info()
circos.clear()
dev.off()
}
