\name{circos.link}
\alias{circos.link}
\title{
  Draw links between points or intervals  


}
\description{
  Draw links between points or intervals  


}
\usage{
circos.link(sector.index1, point1, sector.index2, point2,
    rou = get.track.end.position(get.current.track.index()),
    rou1 = rou, rou2 = rou, h = NULL, w = 1, h2 = h, w2 = w,
    col = "black", lwd = par("lwd"), lty = par("lty"), border = NA)
}
\arguments{
  \item{sector.index1}{Sector index for one sector}
  \item{point1}{A single value or a numeric vector of length 2. If it is a 2-elements vector, then the link would be a belt/ribbon.}
  \item{sector.index2}{Sector index for the other sector}
  \item{point2}{A single value or a numeric vector of length 2. If it is a 2-elements vector, then the link would be a belt/ribbon.}
  \item{rou}{The position of the 'root' of the link. It is the percentage of the radius of the unit circle. By default it is from the bottom of the most recent track.}
  \item{rou1}{The position of root 1 of the link. }
  \item{rou2}{The position of root 2 of the link.}
  \item{h}{Height of the link. }
  \item{w}{It controls the shape of Bezier curve}
  \item{h2}{Height of the bottom line of the link if it is a ribbon.}
  \item{w2}{Shape of the bottom line of the link}
  \item{col}{Color of the link. If the link is a ribbon, then it is the filled color for the ribbon.}
  \item{lwd}{Line (or border) width}
  \item{lty}{Line (or border) style}
  \item{border}{If the link is a ribbon, then it is the color for the ribbon border.}

}
\details{
  The link is a quadratic Bezier curve.  

  Drawing links does not create any track. So you can think it is independent of the tracks.  

  By default you only need to set \code{sector.index1}, \code{point1}, \code{sector.index2} and \code{point2}. The link would look nice. See vignette for detailed explanation. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
