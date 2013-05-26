\name{circos.text}
\alias{circos.text}
\title{
  Draw text in a cell


}
\description{
  Draw text in a cell


}
\usage{
circos.text(x, y, labels, sector.index = get.current.sector.index(),
    track.index = get.current.track.index(), 
    direction = c("default", "vertical_left", "vertical_right", "horizontal", "arc"),
    adj = par("adj"), cex = 1, col = "black", font = par("font"))
}
\arguments{
  \item{x}{Data points on x-axis}
  \item{y}{Data points on y-axis}
  \item{labels}{Labels for each points}
  \item{sector.index}{Index for the sector}
  \item{track.index}{Index for the track}
  \item{direction}{Direction of the text, should be one of (\code{default}, \code{vertical_left}, \code{vertical_right}, \code{horizontal}, \code{arc}). How to choose text direction can be found in the vignette.}
  \item{adj}{Adjustment for texts}
  \item{cex}{Font size}
  \item{col}{Font color}
  \item{font}{Font style}

}
\details{
  The function is similar to \code{\link[graphics]{text}}.


}
