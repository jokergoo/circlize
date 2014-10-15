\name{circos.initializeWithIdeogram}
\alias{circos.initializeWithIdeogram}
\title{
  Initialize the circos layout with an ideogram  


}
\description{
  Initialize the circos layout with an ideogram  


}
\usage{
circos.initializeWithIdeogram(cytoband = paste(system.file(package = "circlize"),
    "/extdata/cytoBand.txt", sep=""), species = NULL, sort.chr = TRUE,
    chromosome.index = NULL, major.by = NULL,
    plotType = c("ideogram", "axis", "labels"),
    track.height = 0.05, ideogram.height = 0.05, ...)
}
\arguments{
  \item{cytoband}{A path of the cytoband file or a data frame that already contains cytoband data. By default it is cytoband for hg19. Pass to \code{\link{read.cytoband}}.}
  \item{species}{Abbreviations of species. e.g. hg19 for human, mm10 for mouse. If this value is specified, the function will download cytoBand.txt.gz from UCSC website automatically. Pass to \code{\link{read.cytoband}}.}
  \item{sort.chr}{Whether chromosome names should be sorted (first sort by numbers then by letters) when reading cytoband data. Pass to \code{\link{read.cytoband}}.}
  \item{chromosome.index}{Index of chromosomes. The index is used only for subsetting, not for re-ordering.}
  \item{major.by}{Increment of major ticks. Pass to \code{\link{circos.genomicInitialize}}.}
  \item{plotType}{Which tracks should be drawn. \code{rect} for ideogram rectangle, \code{axis} for genomic axis and \code{labels} for chromosome names. If it is set to \code{NULL}, the function just initialize the plot but draw nothing.}
  \item{track.height}{Height of the track which contains "axis" and "labels".}
  \item{ideogram.height}{Height of the ideogram track}
  \item{...}{Pass to \code{\link{circos.initialize}}}

}
\details{
  The function will initialize the circos plot in which each sector corresponds to a chromosome. You can control the order of  chromosomes by set a special format of \code{cytoband} (please refer to \code{\link{read.cytoband}} to find out how to control a proper \code{cytoband}).  

  The function finally pass data to \code{\link{circos.genomicInitialize}} to initialize the circos plot.  

  The style of ideogram is almost fixed, but you can customize it with your self-sefined code. Refer to vignette for demonstration. 


}
