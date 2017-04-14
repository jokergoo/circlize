\name{circlize-package}
\docType{package}
\alias{circlize-package}
\title{
Circular visualization in R
}
\description{
Circular visualization in R
}
\details{
This package aims to implement circular layout in R.

Since most of the figures are composed of points, lines and polygons, 
we just need to implement low-level functions for drawing points, lines and polygons.

Current there are following low-level graphic functions:

\itemize{
  \item \code{\link{circos.points}}
  \item \code{\link{circos.lines}}
  \item \code{\link{circos.rect}}
  \item \code{\link{circos.polygon}}
  \item \code{\link{circos.segments}}
  \item \code{\link{circos.text}}
  \item \code{\link{circos.axis}}, \code{\link{circos.xaxis}}, \code{\link{circos.yaxis}}
  \item \code{\link{circos.link}}
}

For drawing points, lines and text through the whole track (among several sectors), the following 
functions are available:

\itemize{
  \item \code{\link{circos.trackPoints}}
  \item \code{\link{circos.trackLines}}
  \item \code{\link{circos.trackText}}
}

Functions to arrange circular layout:

\itemize{
  \item \code{\link{circos.initialize}}
  \item \code{\link{circos.track}}
  \item \code{\link{circos.update}}
  \item \code{\link{circos.par}}
  \item \code{\link{circos.info}}
  \item \code{\link{circos.clear}}
}

Theoretically, you are able to draw most kinds of circular plots by the above functions.

For specific use in genomics, we also implement functions which add graphics in genome scale.

Functions to initialize circos plot with genomic coordinates:

\itemize{
  \item \code{\link{circos.initializeWithIdeogram}}
  \item \code{\link{circos.genomicInitialize}}
}

Functions to arrange genomic circular layout:

\itemize{
  \item \code{\link{circos.genomicTrack}}
}

Functions to add basic graphics in genomic scale:

\itemize{
  \item \code{\link{circos.genomicPoints}}
  \item \code{\link{circos.genomicLines}}
  \item \code{\link{circos.genomicText}}
  \item \code{\link{circos.genomicRect}}
  \item \code{\link{circos.genomicLink}}
}

Functions with specific purpose:

\itemize{
  \item \code{\link{circos.genomicDensity}}
  \item \code{\link{circos.genomicRainfall}}
  \item \code{\link{circos.genomicIdeogram}}
  \item \code{\link{circos.genomicHeatmap}}
  \item \code{\link{circos.genomicLabels}}
}

Finally, function that draws Chord diagram:

\itemize{
  \item \code{\link{chordDiagram}}
}

Please refer to the vignettes (\url{http://jokergoo.github.io/circlize_book/book/} ) to find out how to draw basic and advanced circular plots by this package.
}
\examples{
# There is no example
NULL

}
