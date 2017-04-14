\name{rand_color}
\alias{rand_color}
\title{
Generate random colors
}
\description{
Generate random colors
}
\usage{
rand_color(n = 1, transparency = 0)
}
\arguments{

  \item{n}{number of colors}
  \item{transparency}{transparency, numeric value between 0 and 1}

}
\details{
Colors are randomly generated from RGB color space through uniform distributions.
}
\value{
a vector of colors
}
\examples{
rand_color(10)
rand_color(10, transparency = 0.5)
}
