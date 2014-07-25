\name{circos.genomicInitialize}
\alias{circos.genomicInitialize}
\title{
  Initialize circos plot with any genomic data  


}
\description{
  Initialize circos plot with any genomic data  


}
\usage{
circos.genomicInitialize(data, sector.names = NULL, major.by = NULL,
    plotType = c("axis", "labels"), tickLabelsStartFromZero = TRUE, ...)
}
\arguments{
  \item{data}{A data frame containing genomic data.}
  \item{sector.names}{Names for each sectors which will be drawn along each sector}
  \item{major.by}{Increment of major ticks. It is calculated automatically if the value is not set.}
  \item{plotType}{Which part should be drawn. \code{axis} for genomic axis and \code{labels} for chromosome names}
  \item{tickLabelsStartFromZero}{whether axis tick labels start from 0? This will only affect the axis labels while not affect x-values in cells.}
  \item{...}{Pass to \code{\link{circos.initialize}}}

}
\details{
  The function will initialize circos plot from genomic data provided. If \code{plotType} is set with value in \code{axis} or \code{labels}, there will create a new track.  

  The order of sectors related to data structure of \code{data}. If the first column in \code{data} is a factor, the order of sectors is \code{levels(data[[1]])}; If the first column is just a simple vector, the order of sectors is \code{unique(data[[1]]}.  

  For more details on initializing genomic plot, please refer to the vignettes. 


}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
\examples{
\dontrun{

df = read.cytoband()$df
circos.genomicInitialize(df)

df = data.frame(name = c("TP53", "TP63", "TP73"),
                 start = c(7565097, 189349205, 3569084),
                 end = c(7590856, 189615068, 3652765),
                 stringsAsFactors = FALSE)
circos.genomicInitialize(df)
circos.clear()

circos.genomicInitialize(df, major.by = 10000)
circos.clear()

circos.genomicInitialize(df, plotType = "labels")
circos.clear()

circos.genomicInitialize(df, sector.names = c("tp53", "tp63", "tp73"))
circos.clear()

circos.genomicInitialize(df, sector.names = c("tp53x", "tp63x", "tp73"))
circos.clear()

df[[1]] = factor(df[[1]], levels = c("TP73", "TP63", "TP53"))
circos.genomicInitialize(df)
circos.clear()

}
}
