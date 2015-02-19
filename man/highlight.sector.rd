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
    padding = c(0, 0, 0, 0))
}
\arguments{

  \item{sector.index}{A vector of sector index}
  \item{track.index}{A vector of track index that you want to highlight}
  \item{col}{Color for highlighting. Note the color should be semi-transparent.}
  \item{border}{Border of the highlighted region}
  \item{lwd}{Width of borders}
  \item{lty}{Style of borders}
  \item{padding}{Padding for the highlighted region. It should contain four values representing ratios of the width or height of the highlighted region}

}
\details{
You can use \code{\link{circos.info}} to find out index for all tracks.  

The function calls \code{\link{draw.sector}}. 


}
