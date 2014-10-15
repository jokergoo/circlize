\name{circos.axis}
\alias{circos.axis}
\title{
  Draw x-axis  


}
\description{
  Draw x-axis  


}
\usage{
circos.axis(h = "top", major.at = NULL, labels = TRUE, major.tick = TRUE,
    sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"),
    labels.font = par("font"), labels.cex = par("cex"),
    labels.facing = "inside", labels.direction = NULL, labels.niceFacing = TRUE,
    direction = c("outside", "inside"), minor.ticks = 4,
    major.tick.percentage = 0.1, labels.away.percentage = 0.05, lwd = par("lwd"))
}
\arguments{
  \item{h}{Position of the x-axis, can be "top", "bottom" or a numeric value}
  \item{major.at}{If it is numeric vector, it identifies the positions of the major ticks. It can exceed \code{xlim} value and the exceeding part would be trimmed automatically. If it is \code{NULL}, about every 10 degrees there is a major tick.}
  \item{labels}{labels of the major ticks. Also, the exceeding part would be trimmed automatically.}
  \item{major.tick}{Whether to draw major tick. If it is set to \code{FALSE}, there would be no minor ticks.}
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}
  \item{labels.font}{font style for the axis labels}
  \item{labels.cex}{font size for the axis labels}
  \item{labels.direction}{deprecated, use \code{facing} instead.}
  \item{labels.facing}{facing of labels on axis, passing to \code{\link{circos.text}}}
  \item{labels.niceFacing}{Should facing of axis labels be human-easy}
  \item{direction}{whether the axis ticks point to the outside or inside of the circle.}
  \item{minor.ticks}{Number of minor ticks between two close major ticks.}
  \item{major.tick.percentage}{Length of the major ticks. It is the percentage to the height of the cell.}
  \item{labels.away.percentage}{The distance for the axis labels to the major ticks. It is the percentage to the height of the cell.}
  \item{lwd}{line width for ticks}

}
\details{
  It can only draw axes on x-direction.  

  Currently, this package doesn't provide a function to add axes on y-direction. But it is easy to implement by users with \code{\link{circos.lines}} and \code{\link{circos.text}}. 


}
