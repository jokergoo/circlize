\name{read.cytoband}
\alias{read.cytoband}
\title{
  Read/parse cytoband data from a data frame / a file / UCSC database  


}
\description{
  Read/parse cytoband data from a data frame / a file / UCSC database  


}
\usage{
read.cytoband(cytoband = paste(system.file(package = "circlize"),
    "/extdata/cytoBand.txt", sep=""), species = NULL, sort.chr = TRUE)
}
\arguments{
  \item{cytoband}{A path of the cytoband file or a data frame that already contains cytoband data}
  \item{species}{Abbrevations of species. e.g. hg19 for human, mm10 for mouse. If this value is specified, the function will download \code{cytoBand.txt.gz} from UCSC website automatically.}
  \item{sort.chr}{Whether chromosome names should be sorted (first sort by numbers then by letters).}

}
\details{
  The function read the cytoband data, sort the chromosome names and calculate the length of each chromosome.  By default, it is human hg19 cytoband data.  

  You can find the data struture for the cytoband data from \url{http://hgdownload.cse.ucsc.edu/goldenpath/hg19/database/cytoBand.txt.gz}  

  If \code{sort.chr} is not set, there would be several circumstances when determining the order of chromosomes.  Assuming \code{chromosome} is the first column in the cytoband data frame, then, if \code{cytoband} is defined as a file path, or \code{species} is set, the order of chromosomes is \code{unique(chromosome)}  which is read from the file; If \code{cytoband} is set as a data frame and the first column is a factor, the order of chromosomes is \code{levels(chromosome)}; If \code{cytoband} is a data frame and the first column is just a character vector, the order of chromosomes is \code{unique(chromosome)}. Please not this concept is really important since the order of chromosomes will be used to control the order of sectors when initializing the circos plot.  


}
\value{
\describe{
  \item{df}{Original data frame for cytoband data}
  \item{chromosome}{Sorted chromosome names}
  \item{chr.len}{Length of chromosomes. Order are same as \code{chromosome}}
}

}
\references{
Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics.
}
