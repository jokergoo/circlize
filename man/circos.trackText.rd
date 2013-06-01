\name{circos.trackText}
\alias{circos.trackText}
\title{
  Draw text in cells among the whole track


}
\description{
  Draw text in cells among the whole track


}
\usage{
circos.trackText(factors, x, y, labels, track.index = get.current.track.index(),
    direction = c("default", "default2", "vertical_left",
                  "vertical_right", "horizontal"),
    adj = par("adj"), cex = 1, col = "black", font = par("font"))
}
\arguments{
  \item{factors}{Factors which represent the categories of data}
  \item{x}{Data points on x-axis}
  \item{y}{Data points on y-axis}
  \item{labels}{Labels}
  \item{track.index}{Index for the track}
  \item{direction}{Text directions, see \code{\link{circos.text}}}
  \item{adj}{Adjustment for texts}
  \item{cex}{Font size}
  \item{col}{Font color}
  \item{font}{Font style}

}
\details{
  The function adds texts in multiple cells by first splitting data into several parts in whicheach part corresponds to one factor (sector index) and then add texts in cells correspondingto the part of data by calling \code{\link{circos.text}}.

  This function can be replaced by a \code{for} loop containing \code{\link{circos.text}}.


}
