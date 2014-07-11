
# == title (package:circlize)
# circos layout in R
#
# == details
# This package aims to implement circos layout( http://circos.ca ) in R.
#
# Since most of the figures are composed of points, lines and polygon (for filled color), 
# so we just need to implement functions for drawing points, lines and polygon.
#
# Current there are following functions that can be used for plotting: 
# 
# - `circos.points`
# - `circos.lines`
# - `circos.rect`
# - `circos.polygon`
# - `circos.text`
# - `circos.axis`
# - `circos.link`, This maybe the unique feature for circos layout to represent relationships between elements.
# 
# For drawing points, lines and text through the whole track (among several sectors), the following 
# functions are available:
#
# - `circos.trackPoints`
# - `circos.trackLines`
# - `circos.trackText`
#
# Also, the function drawing histograms in the whole track is available:
#
# - `circos.trackHist`
#
# Functions to arrange the circos layout:
#
# - `circos.trackPlotRegion`
# - `circos.updatePlotRegion`
# - `circos.par`
# - `circos.info`
# - `circos.clear`
#
# Theoretically, you are able to draw most kinds of circos figures by the above functions.
#
# For specific use in genomics, we also implement functions which add graphics in genome scale.
#
# Functions to initialize circos plot with genomic coordinates:
# 
# - `circos.initializeWithIdeogram`
# - `circos.genomicInitialize`
#
# Functions to arrange genomic circos layout
#
# - `circos.genomicTrackPlotRegion`
#
# Functions to add basic genomic graphics
#
# - `circos.genomicPoints`
# - `circos.genomicLines`
# - `circos.genomicText`
# - `circos.genomicRect`
# - `circos.genomicLink`
#
# Functions with specific purpose
#
# - `circos.genomicDensity`
# - `circos.genomicRainfall`
#
# Finally, function that draws chord diagram:
#
# - `chordDiagram`
#
# Please refer to the vignettes to find out how to draw basic and advanced circos figures by this package.
#

