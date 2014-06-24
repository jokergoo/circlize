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
 
For drawing points, lines and text through the whole track (among several sectors), the following 
functions are available:

- `circos.trackPoints`
- `circos.trackLines`
- `circos.trackText`

Also, the function drawing histograms in the whole track is available:

- `circos.trackHist`

Functions to arrange the circos layout:

- `circos.trackPlotRegion`
- `circos.updatePlotRegion`
- `circos.par`
- `circos.clear`

Theoretically, you are able to draw most kinds of circos figures by the above functions.

For specific use in genomics, we also implement functions which add graphics in genome scale.

Functions to initialize circos plot with genomic coordinates:
 
- `circos.initializeWithIdeogram`
- `circos.genomicInitialize`

Functions to arrange genomic circos layout

- `circos.genomicTrackPlotRegion`

Functions to add basic genomic graphics

- `circos.genomicPoints`
- `circos.genomicLines`
- `circos.genomicText`
- `circos.genomicRect`
- `circos.genomicLink`

Functions with specific purpose

- `circos.genomicDensity`
- `circos.genomicRainfall`

Please refer to the vignettes to find out how to draw basic and advanced circos figures by this package.

More tests will be added.

### Install

The package has been submitted to CRAN, so you can install it through:

    install.packages("circlize")

Together with the package there are four vignettes which provide detailed description and examples.

## the Perl module

There is also a Perl module `R::Comment2Man` to convert comments to documentary files.

Comments are marked as the Markdown-style and it looks more clear than that under the `Roxygen` package (personal point of view).

It still has a lot of bugs, but it at least works.

The module could be run as:

    perl -Ilib -MR::Comment2Man -e "R::Comment2Man->draft('R/')"

An example of the comment of a function is:

    # == title
    # title of the function
    #
    # == param
    # -x a value returned by `function`
    # -y a value returned by `package::function2`. If ``x`` is a list, then ...
    #
    # == details
    # first line, blablabla...
    #
    # - item1...
    # - item2...
    #
    # -item1 named item1...
    # -item2 named itme2...
    #
    f = function(x, y) {
    }

would be converted to 

    \name{f}
    \alias{f}
    \title{
      title of the function
    }
    \description{
      title of the function
    }
    \usage{
    f(x, y)
    }
    \arguments{
      \item{x}{a value returned by \code{\link{function}}}
      \item{y}{a value returned by \code{\link[package]{function2}}. If \code{x} is a list, then ...}
    }
    \details{
      first line, blablabla...
      \itemize{
        \item item1...
        \item item2...
      }
      \describe{
        \item{item1}{named item1...}
        \item{item2}{named itme2...}
      }
    }

### Examples

For examples, please go to http://jokergoo.github.io/circlize
