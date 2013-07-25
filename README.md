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
- `circos.clear`

Theoretically, you are able to draw most kinds of circos figures by the above functions.

For specific use in genomics, a function which draws the ideogram and initializes sectors for chromosomes is supported:
- `circos.initializeWithIdeogram`

More tests will be added.

### Install

The package has been submitted to CRAN, so you can install it through:

    install.packages("circlize")

Together with the package are four vignettes which provide detailed descriptions and examples.

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

![clock](https://f.cloud.github.com/assets/449218/857241/70e4d6d4-f54d-11e2-89e8-8fa9afadc2a1.png)
![figbagua](https://f.cloud.github.com/assets/449218/857242/71020830-f54d-11e2-9c1e-d551a805f9cb.png)
![figcompare](https://f.cloud.github.com/assets/449218/857243/711becb4-f54d-11e2-8b3f-1bcd9427a2ae.png)
![figcorrelation](https://f.cloud.github.com/assets/449218/857244/7128114c-f54d-11e2-92f3-8f0fcb5817e2.png)
![figdartboard](https://f.cloud.github.com/assets/449218/857245/712aa5b0-f54d-11e2-9e2a-1dccd55a6e91.png)
![figgenomic](https://f.cloud.github.com/assets/449218/857246/713499f8-f54d-11e2-8064-921250bfb95a.png)
![figtwo](https://f.cloud.github.com/assets/449218/857247/7138e328-f54d-11e2-87a6-5da3a52a52f1.png)
![figheatmap](https://f.cloud.github.com/assets/449218/857248/71381010-f54d-11e2-850d-c9bae7ae70de.png)
![test1](https://f.cloud.github.com/assets/449218/857249/713ceacc-f54d-11e2-9721-23eddda06374.jpeg)
