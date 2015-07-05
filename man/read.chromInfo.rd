\name{read.chromInfo}
\alias{read.chromInfo}
\title{
Read/parse chromInfo data from a data frame/file/UCSC database

}
\description{
Read/parse chromInfo data from a data frame/file/UCSC database

}
\usage{
read.chromInfo(chromInfo = paste0(system.file(package = "circlize"),
    "/extdata/chromInfo.txt"), species = NULL, chromosome.index = NULL, sort.chr = TRUE)}
\arguments{

  \item{chromInfo}{Path of the chromInfo file or a data frame that already contains chromInfo data}
  \item{species}{Abbreviations of species. e.g. hg19 for human, mm10 for mouse. If thisvalue is specified, the function will download \code{chromInfo.txt.gz} fromUCSC website automatically.}
  \item{chromosome.index}{subset of chromosomes, also used to re-set chromosome orders.}
  \item{sort.chr}{Whether chromosome names should be sorted (first sort by numbers then by letters).If \code{chromosome.index} is set, this argument is enforced to \code{FALSE}#}
}
\details{
The function read the chromInfo data, sort the chromosome names and calculate the length of each chromosome. 
By default, it is human hg19 chromInfo data.

You can find the data structure for the cytoband data from \url{http://hgdownload.cse.ucsc.edu/goldenpath/hg19/database/chromInfo.txt.gz}

If \code{sort.chr} is not set and \code{chromosome.index} is not specified, there would be several circumstances when determining the order of chromosomes. 
Assuming \code{chromosome} is the first column in the chromInfo data frame,
then, if \code{chromInfo} is defined as a file path, or \code{species} is set, the order of chromosomes is \code{unique(chromosome)} 
which is read from the file; If \code{chromInfo}
is set as a data frame and the first column is a factor, the order of chromosomes is \code{levels(chromosome)}; If \code{chromInfo} is a data frame
and the first column is just a character vector, the order of chromosomes is \code{unique(chromosome)}. Please not this concept is really
important since the order of chromosomes will be used to control the order of sectors when initializing the circos plot.

}
\value{
\describe{
  \item{df}{Data frame for chromInfo data (rows are sorted if \code{sort.chr} is set to \code{TRUE})}
  \item{chromosome}{Sorted chromosome names}
  \item{chr.len}{Length of chromosomes. Order are same as \code{chromosome}}
}

}
