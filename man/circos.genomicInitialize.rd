\name{circos.genomicInitialize}
\alias{circos.genomicInitialize}
\title{
Initialize circular plot with any genomic data
}
\description{
Initialize circular plot with any genomic data
}
\usage{
circos.genomicInitialize(data, sector.names = NULL, major.by = NULL,
    plotType = c("axis", "labels"), tickLabelsStartFromZero = TRUE,
    axis.labels.cex = 0.4*par("cex"), labels.cex = 0.8*par("cex"),
    track.height = NULL, ...)
}
\arguments{

  \item{data}{A data frame containing genomic data.}
  \item{sector.names}{Labels for each sectors which will be drawn along each sector. It will not modify values of sector index.}
  \item{major.by}{Increment of major ticks. It is calculated automatically if the value is not set (about every 10 degrees there is a major tick).}
  \item{plotType}{If it is not \code{NULL}, there will create a new track containing axis and names for sectors. This argument controls which part should be drawn, \code{axis} for genomic axis and \code{labels} for chromosome names}
  \item{tickLabelsStartFromZero}{Whether axis tick labels start from 0? This will only affect the axis labels while not affect x-values in cells.}
  \item{axis.labels.cex}{the font size for the axis tick labels.}
  \item{labels.cex}{the font size for the labels.}
  \item{track.height}{If \code{PlotType} is not \code{NULL}, height of the annotation track.}
  \item{...}{Pass to \code{\link{circos.initialize}}}

}
\details{
The function will initialize circular plot from genomic data. If \code{plotType} is set with value in \code{axis} or \code{labels}, there will
create a new track.

The order of sectors related to data structure of \code{data}. If the first column in \code{data} is a factor, the order of sectors
is \code{levels(data[[1]])}; If the first column is just a simple vector, the order of sectors is \code{unique(data[[1]]}.

For more details on initializing genomic plot, please refer to the vignettes.
}
\examples{
# There is no example
NULL
}
