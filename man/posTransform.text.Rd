\name{posTransform.text}
\alias{posTransform.text}
\title{
Genomic position transformation function specifically for text
}
\description{
Genomic position transformation function specifically for text
}
\usage{
posTransform.text(
    region,
    y,
    labels,
    cex = 1,
    font = par("font"),
    sector.index = get.cell.meta.data("sector.index"),
    track.index = get.cell.meta.data("track.index"),
    padding = 0,
    extend = 0,
    ...)
}
\arguments{

  \item{region}{Genomic positions at a single chromosome. It is a data frame with two columns which are start position and end position.}
  \item{y}{positions of texts}
  \item{labels}{text labels}
  \item{cex}{text size}
  \item{font}{text font style}
  \item{sector.index}{sector index}
  \item{track.index}{track index}
  \item{padding}{padding of text}
  \item{extend}{extend to allow labels to be put in an region which is wider than the current chromosome. The value should be a proportion value and the length is either one or two.}
  \item{...}{other arguments}

}
\details{
This position transformation function is designed specifically for text.
Under the transformation, texts will be as close as possible to the original positions.
}
\examples{
# There is no example
NULL

}
