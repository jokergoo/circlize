\name{circos.triangle}
\alias{circos.triangle}
\title{
Draw triangles
}
\description{
Draw triangles
}
\usage{
circos.triangle(x1, y1, x2, y2, x3, y3, ...)
}
\arguments{

  \item{x1}{x-coordinates for the first point}
  \item{y1}{y-coordinates for the first point}
  \item{x2}{x-coordinates for the second point}
  \item{y2}{y-coordinates for the second point}
  \item{x3}{x-coordinates for the third point}
  \item{y3}{y-coordinates for the third point}
  \item{...}{pass to \code{\link{circos.polygon}}}

}
\examples{
circos.initialize(fa = c("a", "b", "c", "d"), xlim = c(0, 10))
circos.track(ylim = c(0, 10), panel.fun = function(x, y) {
    circos.triangle(c(2, 2), c(2, 8),
                    c(8, 8), c(2, 8),
                    c(5, 5), c(8, 2))
}, track.height = 0.5)
}
