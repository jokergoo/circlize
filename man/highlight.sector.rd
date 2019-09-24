\name{highlight.sector}
\alias{highlight.sector}
\title{
Highlight sectors and tracks
}
\description{
Highlight sectors and tracks
}
\usage{
highlight.sector(sector.index, track.index = get.all.track.index(),
    col = "#FF000040", border = NA, lwd = par("lwd"), lty = par("lty"),
    padding = c(0, 0, 0, 0), text = NULL, text.col = par("col"),
    text.vjust = 0.5, ...)
}
\arguments{

  \item{sector.index}{A vector of sector index}
  \item{track.index}{A vector of track index that you want to highlight}
  \item{col}{Color for highlighting. Note the color should be semi-transparent.}
  \item{border}{Border of the highlighted region}
  \item{lwd}{Width of borders}
  \item{lty}{Style of borders}
  \item{padding}{Padding for the highlighted region. It should contain four values representing ratios of the width or height of the highlighted region}
  \item{text}{text added in the highlight region, only support plotting one string at a time}
  \item{text.vjust}{adjustment on 'vertical' (radical) direction. Besides to set it as numeric values, the value can also be a string contain absoute unit, e.g. "2.1mm", "-1 inche", but only "mm", "cm", "inches"/"inche" are allowed.}
  \item{text.col}{color for the text}
  \item{...}{pass to \code{\link{circos.text}}}

}
\details{
You can use \code{\link{circos.info}} to find out index for all sectors and all tracks.

The function calls \code{\link{draw.sector}}.
}
\examples{
# There is no example
NULL
}
