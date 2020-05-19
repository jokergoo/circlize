\name{circos.nested}
\alias{circos.nested}
\title{
Nested zooming with two circular plots
}
\description{
Nested zooming with two circular plots
}
\usage{
circos.nested(
    f1,
    f2,
    correspondance,
    connection_height = convert_height(5, "mm"),
    connection_col = NA,
    connection_border = "black",
    connection_lty = par("lty"),
    connection_lwd = par("lwd"),
    adjust_start_degree = TRUE)
}
\arguments{

  \item{f1}{a self-defined function for making the first circular plot. The function should have no argument.}
  \item{f2}{a self-defined function for making the second circular plot. The function should have no argument.}
  \item{correspondance}{a six-column data frame which contains correspondance between the coordinates in two circular plots}
  \item{connection_height}{the height of the connection track, measured as the percent to the radius of the unit circle. The value can be specified by \code{\link{uh}} or \code{\link{convert_height}} with absolute units.}
  \item{connection_col}{filled color of the connection track. The value can be a vector with same length as number of rows of \code{correspondance}}
  \item{connection_border}{border color of the connection track.}
  \item{connection_lty}{line style of the connection track borders}
  \item{connection_lwd}{line width of the connection track borders}
  \item{adjust_start_degree}{If \code{circos.par(start.degree = ...)} is not set in \code{f2()}, the start degree for the second circular plot will be adjusted to make the distance of sectors between the two plots to the minimal.}

}
\details{
The function visualizes zoomings by combining two circular plots into one page where
one is the normal circular plot and the other one only contains regions that need to be zoomed.
This function automatically arranges the two plots to make it easy to correspond between
the original and the zoomed sectors.

Since the function needs to know the information of the two circular plots, please do not call
\code{\link{circos.clear}} in either \code{f1()} or \code{f2()}. It will be called internally in \code{\link{circos.nested}}.

If \code{adjust_start_degree} is set to \code{TRUE}, \code{start.degree} should not be set in \code{f2()}.
Also \code{canvas.xlim} and \code{canvas.ylim} are reset in \code{f2()}, they should not be set in \code{f2()}
either.
}
\seealso{
\url{https://jokergoo.github.io/circlize_book/book/nested-zooming.html}
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL

}
