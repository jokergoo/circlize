\name{circos.polygon}
\alias{circos.polygon}
\title{
  Draw polygon  


}
\description{
  Draw polygon  


}
\usage{
circos.polygon(x, y, sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"),
    col = NA, border = "black", lty = par("lty"), lwd = par("lwd"))
}
\arguments{
  \item{x}{Data points on x-axis}
  \item{y}{Data points on y-axis}
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}
  \item{col}{filled color}
  \item{border}{color for the border}
  \item{lty}{line style for the border}
  \item{lwd}{line width for the border}

}
\details{
  similar as \code{\link[graphics]{polygon}} 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
