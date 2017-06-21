\name{rand_color}
\alias{rand_color}
\title{
Generate random colors
}
\description{
Generate random colors
}
\usage{
rand_color(n, hue = NULL, luminosity = "random", transparency = 0)
}
\arguments{

  \item{n}{number of colors}
  \item{hue}{the hue of the generated color. You can use following default color name: \code{red}, \code{orange},  \code{yellow}, \code{green}, \code{blue}, \code{purple}, \code{pink} and \code{monochrome}. If the value is a hexidecimal color string such as \code{#00FFFF},  the function will extract its hue value and use that to generate colors.}
  \item{luminosity}{controls the luminosity of the generated color. The value should be a string containing \code{bright}, \code{light} or \code{dark}.}
  \item{transparency}{transparency, numeric value between 0 and 1.}

}
\details{
The code is adapted from randomColor.js (\url{https://github.com/davidmerfield/randomColor} )
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\value{
a vector of colors

}
\examples{
rand_color(10)
rand_color(10, transparency = 0.5)

}
