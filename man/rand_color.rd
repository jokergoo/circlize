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
  \item{luminosity}{controls the luminosity of the generated color. The value should be a string containing \code{bright}, \code{light}, \code{dark} and \code{random}.}
  \item{transparency}{transparency, numeric value between 0 and 1.}

}
\details{
The code is adapted from randomColor.js (\url{https://github.com/davidmerfield/randomColor} ).
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
plot(NULL, xlim = c(1, 10), ylim = c(1, 8), axes = FALSE, ann = FALSE)
points(1:10, rep(1, 10), pch = 16, cex = 5, 
    col = rand_color(10, luminosity = "random"))
points(1:10, rep(2, 10), pch = 16, cex = 5, 
    col = rand_color(10, luminosity = "bright"))
points(1:10, rep(3, 10), pch = 16, cex = 5, 
    col = rand_color(10, luminosity = "light"))
points(1:10, rep(4, 10), pch = 16, cex = 5, 
    col = rand_color(10, luminosity = "dark"))
points(1:10, rep(5, 10), pch = 16, cex = 5, 
    col = rand_color(10, hue = "red", luminosity = "bright"))
points(1:10, rep(6, 10), pch = 16, cex = 5, 
    col = rand_color(10, hue = "green", luminosity = "bright"))
points(1:10, rep(7, 10), pch = 16, cex = 5, 
    col = rand_color(10, hue = "blue", luminosity = "bright"))
points(1:10, rep(8, 10), pch = 16, cex = 5, 
    col = rand_color(10, hue = "monochrome", luminosity = "bright"))
}
