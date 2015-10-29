
# == title (package:circlize)
# Circular layout in R
#
# == details
#
# This package aims to implement circular layout in R.
#
# Since most of the figures are composed of points, lines and polygons, 
# we just need to implement low-level functions for drawing points, lines and polygons.
#
# Current there are following low-level graphical functions: 
#
# - `circos.points`
# - `circos.lines`
# - `circos.rect`
# - `circos.polygon`
# - `circos.text`
# - `circos.axis`
# - `circos.link`, This maybe the unique feature for circular layout to represent relationships between elements.
#  
# For drawing points, lines and text through the whole track (among several sectors), the following 
# functions are available:
# 
# - `circos.trackPoints`
# - `circos.trackLines`
# - `circos.trackText`
# 
# Functions to arrange circular layout:
# 
# - `circos.trackPlotRegion`
# - `circos.updatePlotRegion`
# - `circos.par`
# - `circos.info`
# - `circos.clear`
# 
# Theoretically, you are able to draw most kinds of circular plots by the above functions.
# 
# For specific use in genomics, we also implement functions which add graphics in genome scale.
# 
# Functions to initialize circos plot with genomic coordinates:
#  
# - `circos.initializeWithIdeogram`
# - `circos.genomicInitialize`
# 
# Functions to arrange genomic circular layout:
# 
# - `circos.genomicTrackPlotRegion`
# 
# Functions to add basic graphics in genomic scale:
# 
# - `circos.genomicPoints`
# - `circos.genomicLines`
# - `circos.genomicText`
# - `circos.genomicRect`
# - `circos.genomicLink`
# 
# Functions with specific purpose:
# 
# - `circos.genomicDensity`
# - `circos.genomicRainfall`
# 
# Finally, function that draws chord diagram:
# 
# - `chordDiagram`
# - `chordDiagramFromMatrix`
# - `chordDiagramFromDataFrame`
# 
# Please refer to the vignettes to find out how to draw basic and advanced circular plots by this package.
# 