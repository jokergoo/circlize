
# == title (package:circlize)
# Circular visualization in R
#
# == details
#
# This package aims to implement circular layout in R.
#
# Since most of the figures are composed of points, lines and polygons,
# we just need to implement low-level functions for drawing points, lines and polygons.
#
# Current there are following low-level graphic functions:
#
# - `circos.points`
# - `circos.lines`
# - `circos.rect`
# - `circos.polygon`
# - `circos.segments`
# - `circos.text`
# - `circos.axis`, `circos.xaxis`, `circos.yaxis`
# - `circos.barplot`
# - `circos.boxplot`
# - `circos.violin`
# - `circos.link`
#
# For drawing points, lines and text through the whole track (among several sectors), the following
# functions are available:
#
# - `circos.trackPoints`
# - `circos.trackLines`
# - `circos.trackText`
#
# Draw circular heatmaps
#
# - `circos.heatmap`
#
# Functions to arrange circular layout:
#
# - `circos.initialize`
# - `circos.track`
# - `circos.nested`
# - `circos.update`
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
# - `circos.genomicTrack`
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
# - `circos.genomicIdeogram`
# - `circos.genomicHeatmap`
# - `circos.genomicLabels`
#
# Finally, function that draws Chord diagram:
#
# - `chordDiagram`
#
# Please refer to the vignettes (https://jokergoo.github.io/circlize_book/book/ ) to find out how to draw basic and advanced circular plots by this package.
#
