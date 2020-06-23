\name{add_transparency}
\alias{add_transparency}
\title{
Add transparency to colors
}
\description{
Add transparency to colors
}
\usage{
add_transparency(col, transparency = 0)
}
\arguments{

  \item{col}{A vector of colors.}
  \item{transparency}{Transparency, numeric value between 0 and 1.}

}
\value{
A vector of colors.
}
\examples{
add_transparency("red", 0.5)
add_transparency(1, 0.5)
add_transparency("#FF000080", 0.2)
}
