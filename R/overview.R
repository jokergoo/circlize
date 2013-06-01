
# == title (package:circlize)
# circos layout in R
#
# == details
# This package aims to implement circos layout in R.
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
# - `circos.clear`
#
# Theoretically, you are able to draw most kinds of circos figures by the above functions.
#
# For specific use in genomics, a function which draws the ideogram and initializes sectors
# for chromosomes is supported: `circos.initializeWithIdeogram`.
#
# Refer to the vignettes to find how to draw basic and advanced circos figure by this package.
#

