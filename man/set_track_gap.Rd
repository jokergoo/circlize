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

  \item{gap}{Gap between two tracks}

}
\examples{
circos.initialize(fa = letters[1:10], xlim = c(0, 1))
circos.track(ylim = c(0, 1))
set_track_gap(mm_h(2))
circos.track(ylim = c(0, 1))
circos.clear()
}
