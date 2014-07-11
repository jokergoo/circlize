\name{circlize-package}
\alias{circlize-package}
\docType{package}
\title{
  circos layout in R  


}
\description{
  circos layout in R  


}
\details{
  This package aims to implement circos layout( \url{http://circos.ca} ) in R.  

  Since most of the figures are composed of points, lines and polygon (for filled color),  so we just need to implement functions for drawing points, lines and polygon.  

  Current there are following functions that can be used for plotting:   

  \itemize{
    \item \code{\link{circos.points}}
    \item \code{\link{circos.lines}}
    \item \code{\link{circos.rect}}
    \item \code{\link{circos.polygon}}
    \item \code{\link{circos.text}}
    \item \code{\link{circos.axis}}
    \item \code{\link{circos.link}}, This maybe the unique feature for circos layout to represent relationships between elements.
  }
  For drawing points, lines and text through the whole track (among several sectors), the following  functions are available:  

  \itemize{
    \item \code{\link{circos.trackPoints}}
    \item \code{\link{circos.trackLines}}
    \item \code{\link{circos.trackText}}
  }
  Also, the function drawing histograms in the whole track is available:  

  \itemize{
    \item \code{\link{circos.trackHist}}
  }
  Functions to arrange the circos layout:  

  \itemize{
    \item \code{\link{circos.trackPlotRegion}}
    \item \code{\link{circos.updatePlotRegion}}
    \item \code{\link{circos.par}}
    \item \code{\link{circos.info}}
    \item \code{\link{circos.clear}}
  }
  Theoretically, you are able to draw most kinds of circos figures by the above functions.  

  For specific use in genomics, we also implement functions which add graphics in genome scale.  

  Functions to initialize circos plot with genomic coordinates:  

  \itemize{
    \item \code{\link{circos.initializeWithIdeogram}}
    \item \code{\link{circos.genomicInitialize}}
  }
  Functions to arrange genomic circos layout  

  \itemize{
    \item \code{\link{circos.genomicTrackPlotRegion}}
  }
  Functions to add basic genomic graphics  

  \itemize{
    \item \code{\link{circos.genomicPoints}}
    \item \code{\link{circos.genomicLines}}
    \item \code{\link{circos.genomicText}}
    \item \code{\link{circos.genomicRect}}
    \item \code{\link{circos.genomicLink}}
  }
  Functions with specific purpose  

  \itemize{
    \item \code{\link{circos.genomicDensity}}
    \item \code{\link{circos.genomicRainfall}}
  }
  Finally, function that draws chord diagram:  

  \itemize{
    \item \code{\link{chordDiagram}}
  }
  Please refer to the vignettes to find out how to draw basic and advanced circos figures by this package.  


}
