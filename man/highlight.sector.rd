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
  \item{padding}{Padding for the highlighted region. It should contain four valuesrepresenting ratios of the width or height of the highlighted region}
  \item{text}{text added in the highlight region, only support plotting one string at a time}
  \item{text.vjust}{adjustment on 'vertical' (radical) direction}
  \item{text.col}{color for the text}
  \item{...}{pass to \code{\link{circos.text}}}

}
\details{
You can use \code{\link{circos.info}} to find out index for all sectors and all tracks.

The function calls \code{\link{draw.sector}}.
}
\examples{
\dontrun{
factors = letters[1:8]
circos.initialize(factors, xlim = c(0, 1))
for(i in 1:4) {
    circos.trackPlotRegion(ylim = c(0, 1))
}
circos.info(plot = TRUE)

highlight.sector(c("a", "h"), track.index = 1)
highlight.sector("c", col = "#00FF0040")
highlight.sector("d", col = NA, border = "red", lwd = 2)
highlight.sector("e", col = "#0000FF40", track.index = c(2, 3))
highlight.sector(c("f", "g"), col = NA, border = "green", 
    lwd = 2, track.index = c(2, 3))
highlight.sector(factors, col = "#FFFF0040", track.index = 4)
circos.clear()
}

}
