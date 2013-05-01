## the R package

This package aims to implement circos layout in R.

Since most of the figures are composed of points, lines and polygon (for filled color),
so we just need to implement functions for drawing points, lines and polygon.

Current there are following functions that can be used for plotting:
- `circos.points`
- `circos.lines`
- `circos.rect`
- `circos.polygon`
- `circos.text`
- `circos.axis`
- `circos.link`, This maybe the unique feature for circos layout to represent relationships between elements.

For drawing points, lines and text through the whole track (among several sectors), the following functions are available:
- `circos.trackPoints`
- `circos.trackLines`
- `circos.trackText`

Also, the function drawing histograms in the whole track is available:
- `circos.trackHist`

Functions to arrange the circos layout
- `circos.trackPlotRegion`
- `circos.updatPlotRegion`
- `circos.par`

Theoretically, you are able to draw most kinds of circos figures by the above functions.

For how to create or arrange the circos layout, see `example/galary.R`

Several interesting figures can be found in `example/`

More tests and documents will be added, as well as a vignette.


## the Perl module

There is also a Perl module R::Comment2Man to convert comments to documentary files.

Comments are marked as the Markdown-style and it looks more clear than that under the Roxygen package
