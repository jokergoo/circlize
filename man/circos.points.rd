\name{circos.points}
\alias{circos.points}
\title{
  Add points to a plotting region  


}
\description{
  Add points to a plotting region  


}
\usage{
circos.points(x, y, sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"),
    pch = par("pch"), col = par("col"), cex = par("cex"))
}
\arguments{
  \item{x}{Data points on x-axis}
  \item{y}{Data points on y-axis}
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}
  \item{pch}{Point type}
  \item{col}{Point color}
  \item{cex}{Point size}

}
\details{
  This function can only add points in one specified cell. Pretending a low-level plotting  function, it can only be applied in plotting region which has been created.  

  You can think the function as the normal \code{\link[graphics]{points}} function, just adding points in the plotting region. The position of plotting region is identified by \code{sector.index} and \code{track.index}, if they are not specified, they are in 'current' sector and 'current' track.  

  Data points out of the plotting region will also be added, but with warning messages.  

  Other graphics parameters which are available in the function are \code{pch}, \code{col} and \code{cex} which have same meaning as those in the \code{\link[graphics]{par}}. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
