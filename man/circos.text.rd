\name{circos.text}
\alias{circos.text}
\title{
Draw text in a cell
}
\description{
Draw text in a cell
}
\usage{
circos.text(x, y, labels, sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"), direction = NULL,
    facing = c("inside", "outside", "reverse.clockwise", "clockwise",
    "downward", "bending", "bending.inside", "bending.outside"), niceFacing = FALSE,
    adj = par("adj"), cex = 1, col = par("col"), font = par("font"), ...)
}
\arguments{

  \item{x}{Data points on x-axis}
  \item{y}{Data points on y-axis}
  \item{labels}{Labels for each points}
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}
  \item{direction}{deprecated, use \code{facing} instead.}
  \item{facing}{Facing of text. Please refer to vignette for different settings}
  \item{niceFacing}{Should the facing of text be adjusted to fit human eyes?}
  \item{adj}{offset for text. By default the text position adjustment is either horizontal or vertical in the canvas coordinate system. The "circular horizontal" offset can be set as a value in degree unit and the value should be wrapped by \code{\link{degree}}.}
  \item{...}{Pass to \code{\link[graphics]{text}}}
  \item{cex}{Font size}
  \item{col}{Font color}
  \item{font}{Font style}

}
\details{
The function is similar to \code{\link[graphics]{text}}. All you need to note is the \code{facing} settings.
}
\seealso{
\url{http://jokergoo.github.io/circlize_book/book/graphics.html#text}
}
\examples{
# There is no example
NULL
}
