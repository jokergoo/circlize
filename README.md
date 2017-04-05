[![Build Status](https://travis-ci.org/jokergoo/circlize.svg)](https://travis-ci.org/jokergoo/circlize)
[![CRAN](http://www.r-pkg.org/badges/version/circlize)](https://cran.r-project.org/web/packages/circlize/index.html) 
[![CRAN](https://cranlogs.r-pkg.org/badges/grand-total/circlize)](https://cran.r-project.org/web/packages/circlize/index.html) 

## circlize: circular visualization in R

Circular layout is an efficient way for the visualization of huge 
    amounts of information. Here the circlize package provides an implementation 
    of circular layout generation in R as well as an enhancement of available 
    software. The flexibility of this package is based on the usage of low-level 
    graphics functions such that self-defined high-level graphics can be easily 
    implemented by users for specific purposes. Together with the seamless 
    connection between the powerful computational and visual environment in R, 
    circlize givesa users more convenience and freedom to design figures for 
    better understanding complex patterns behind multi-dimensional data.

### Citation

Zuguang Gu, Lei Gu, Roland Eils, Matthias Schlesner, Benedikt Brors, circlize Implements and enhances circular visualization in R. Bioinformatics (Oxford, England) 2014. [PubMed](http://www.ncbi.nlm.nih.gov/pubmed/24930139)

### Basic design

Since most of the figures are composed of points, lines and polygons, 
we just need to implement functions for drawing points, lines and polygons.

Current there are following low-level graphical functions: 

- `circos.points()`
- `circos.lines()`
- `circos.rect()`
- `circos.polygon()`
- `circos.text()`
- `circos.axis()`
- `circos.link()`, This maybe the unique feature for circos layout to represent relationships between elements.
 
For drawing points, lines and text through the whole track (among several sectors), the following 
functions are available:

- `circos.trackPoints()`
- `circos.trackLines()`
- `circos.trackText()`

Functions to arrange circos layout:

- `circos.trackPlotRegion()`
- `circos.updatePlotRegion()`
- `circos.par()`
- `circos.info()`
- `circos.clear()`

Theoretically, you are able to draw most kinds of circos plots by the above functions.

For specific use in genomics, we also implement functions which add graphics in genome scale.

Functions to initialize circos plot with genomic coordinates:
 
- `circos.initializeWithIdeogram()`
- `circos.genomicInitialize()`

Functions to arrange genomic circos layout:

- `circos.genomicTrackPlotRegion()`

Functions to add basic graphics in genomic scale:

- `circos.genomicPoints()`
- `circos.genomicLines()`
- `circos.genomicText()`
- `circos.genomicRect()`
- `circos.genomicLink()`

Functions with specific purpose:

- `circos.genomicDensity()`
- `circos.genomicRainfall()`

Finally, function that draws chord diagram:

- `chordDiagram()`

Please refer to the vignettes to find out how to draw basic and advanced circos plots by this package.

### Install

The package has been submitted to CRAN, so you can install it through:

    install.packages("circlize")

Together with the package there are several vignettes which provide detailed description and examples.

### Example

See http://jokergoo.github.io/circlize.

<img width="408" alt="circlize_example" src="https://cloud.githubusercontent.com/assets/449218/20597169/7a50d29e-b242-11e6-8b2f-ba49a2d3be40.png">

### License

GPL (>= 2)
