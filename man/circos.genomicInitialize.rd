\name{circos.genomicInitialize}
\alias{circos.genomicInitialize}
\title{
  Initialize circos plot with genomic data  


}
\description{
  Initialize circos plot with genomic data  


}
\usage{
circos.genomicInitialize(data, sector.names = NULL, major.by = 50000000, plotType = c("rect", "axis", "labels"),
    colorMappingColumn = NULL, colorMappingFun = function(x) rep("grey", length(x)))
}
\arguments{
  \item{data}{a data frame containing genomic data or a \code{GRanges} object.}
  \item{sector.names}{names for each sectors which will be drawn along each sector}
  \item{major.by}{increment of major ticks}
  \item{plotType}{which part should be drawn. \code{rect} for the rectangle, \code{axis} for genomic axis and \code{labels} for chromosome names}
  \item{colorMappingColumn}{If you want to draw different color on different genomic regions, which columns should be refered to? If \code{data} is a data frame, the index starts from the first column. If \code{data} is a \code{GRanges} object, the index starts in the meta columns.}
  \item{colorMappingFun}{How to map values in \code{colorMappingColumn} to different colors}

}
\details{
  The function will initialize circos plot from genomic data provided. For values in \code{plotType}, \code{axis} or \code{labels} will create a new track and \code{rect} will create a new track.  

  For more details on initializing genomic plot, please refer to vignettes. 


}
