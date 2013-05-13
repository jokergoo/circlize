\name{circlize}
\alias{circlize-package}
\docType{package}
\title{
  circos layout in R


}
\description{
  circos layout in R


}
\details{
  This package aims to implement circos layout in R.

  Since most of the figures are composed of points, lines and polygon (for filled color), so we just need to implement functions for drawing points, lines and polygon.

  Current there are following functions that can be used for plotting: \code{\link{circos.points}},\code{\link{circos.lines}}, \code{\link{circos.rect}}, \code{\link{circos.polygon}}, \code{\link{circos.text}}, \code{\link{circos.axis}} and \code{\link{circos.link}}. 

  For drawing points, lines and text through the whole track (among several sectors), the followingfunctions are available: \code{\link{circos.trackPoints}}, \code{\link{circos.trackLines}} and \code{\link{circos.trackText}}.

  Also, the function drawing histograms in the whole track is available: \code{\link{circos.trackHist}}.

  Functions to arrange the circos layout: \code{\link{circos.trackPlotRegion}}, \code{\link{circos.updatePlotRegion}},\code{\link{circos.par}} and \code{\link{circos.clear}}.

  Theoretically, you are able to draw most kinds of circos figures by the above functions.

  For specific use in genomics, a function which draws the ideogram and initializes sectorsfor chromosomes is supported: \code{\link{circos.initializeWithIdeogram}}.


}
