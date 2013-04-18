This package aims to implement circos layout in R.

Since most of the figures are composed of points, lines and polygon (for filled color),
so we just need to implement functions for drawing points, lines and polygon.

Current there are following functions that can be used for plotting:
- circos.points
- circos.lines
- circos.rect
- circos.text
- circos.link, This maybe the unique feature for circos layout to represent relationships between elements.

Theoretically, you are able to draw most kinds of circos figures by the above functions.

For how to create or arrange the circos layout, see example/galary.R

Several interesting figures can be found in example/

More tests and documents will be added, as well as a vignette.
