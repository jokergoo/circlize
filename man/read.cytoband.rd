\name{read.cytoband}
\alias{read.cytoband}
\title{
  Read cytoband data  


}
\description{
  Read cytoband data  


}
\usage{
read.cytoband(cytoband = paste(system.file(package = "circlize"), "/extdata/cytoBand.txt", sep=""), species = NULL)
}
\arguments{
  \item{cytoband}{a path of the uncompressed cytoband file or a data frame that already contains cytoband data}
  \item{species}{abbrevations of species. e.g. hg19 for human, mm10 for mouse. If this value is specified, the function will download cytoBand.txt.gz from UCSC website automatically.}

}
\details{
  The function read the cytoband data, sort the chromosome names and calculate the length of each chromosome.  By default, it is human hg19 cytoband data.  


}
\value{
\describe{
  \item{df}{Original data frame for cytoband data}
  \item{chromosome}{sorted chromosome names}
  \item{chr.len}{length of chromosomes. Order are same as \code{chromosome}}
}

}
