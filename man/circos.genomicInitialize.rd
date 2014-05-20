\name{circos.genomicInitialize}
\alias{circos.genomicInitialize}
\title{
  Initialize circos plot with any genomic data  


}
\description{
  Initialize circos plot with any genomic data  


}
\usage{
circos.genomicInitialize(data, sector.names = NULL, major.by = 50000000, plotType = c("axis", "labels"))
}
\arguments{
  \item{data}{a data frame containing genomic data or a \code{GRanges} object.}
  \item{sector.names}{names for each sectors which will be drawn along each sector}
  \item{major.by}{increment of major ticks}
  \item{plotType}{which part should be drawn. \code{axis} for genomic axis and \code{labels} for chromosome names}

}
\details{
  The function will initialize circos plot from genomic data provided. If \code{plotType} is set with value in \code{axis} or \code{labels}, there will create a new track.  

  The order of sectors related to data structure of \code{data}. If it is a data frame and the first column is a factor, the order of sectors is \code{levels(data[[1]])}; If it is a data frame and the first column is just a simple vector, the order of sectors is \code{unique(data[[1]]}; If \code{data} is a \code{GRanges} object, the order of sectors is \code{seqlevels(data)}.  

  For more details on initializing genomic plot, please refer to the vignettes. 


}
