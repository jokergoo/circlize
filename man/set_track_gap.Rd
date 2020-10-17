\name{set_track_gap}
\alias{set_track_gap}
\title{
Set gaps between tracks
}
\description{
Set gaps between tracks
}
\usage{
set_track_gap(gap = 0.02)
}
\arguments{

  \item{gap}{Gap between two tracks. Use \code{\link{mm_h}}/\code{\link{cm_h}}/\code{\link{inches_h}} to set in absolute units.}

}
\examples{
circos.initialize(letters[1:10], xlim = c(0, 1))
circos.track(ylim = c(0, 1))
set_track_gap(mm_h(2))
circos.track(ylim = c(0, 1))
circos.clear()
}
